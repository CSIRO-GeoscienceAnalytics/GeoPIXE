pro blog_file_da2

; Uses shared memory for the actual image memory. Read DA_accum records from a blog file,
; and process them into DA images. Assumes blog file has new DA records written into it.
;
; This needs to be a slave now, to read n_channels back from buffer_size variables
; in shared memory ppar array. Also, read IP address and Port number too.
;
; N.B. This uses a test on "Connection timed out" sys error message. This will probably
;      change for Linux O/S.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
	if catch_errors_on then begin
	    Catch, ErrorNo
	    if (ErrorNo ne 0) then begin
	       Catch, /cancel
	       on_error, 1
	       help, calls = s
	       n = n_elements(s)
	       c = 'Call stack: '
	       if n gt 2 then c = [c, s[1:n-2]]
	       warning,'blog_file_da2',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return
	    endif
	endif
	tin = systime(/seconds)

; Blog client loads routines from GeoPIXE.sav. 

;................................................................................................
; This code fragmwent will appear at the start of most stand-alone programs
; that need to load "GeoPIXE.sav" as a library. It needs to work when the current
; working directory is: (i) the "geopixe" runtime dir, i.e. for SAV files stored in the
; runtime dir along side GeoPIXE.sav, (ii) a subdir of "geopixe" runtime, such as the
; "maia", "daq", "wizards", e.g. for compiled Maia or Wizard SAV files, and (iii) a 
; project dir during debugging of this program in IDLDE. 
;
; The latter assumes that the runtime dir is named "geopixe" (see notes in "startupp.pro").

	found = 0
	file = 'GeoPIXE.sav'						; current dir is the runtime dir
	if file_test(file) eq 0 then begin
		file = '../GeoPIXE.sav'					; current dir in a subdir of runtime dir
		if file_test(file) eq 0 then begin
			file = '../geopixe/GeoPIXE.sav'		; current dir is another project dir
			if file_test(file) eq 0 then begin
				r = dialog_message(['GeoPIXE library not found.','Failed to restore "GeoPIXE.sav".'],/error)
			endif else found=1
		endif else found=1
	endif else found = 1
	if found then restore, file

;.................................................................................................
; Blog client also loads routines from Maia_Control.sav and all
; device object SAV files, which includes Maia.

  startupp, /maia

	argv = command_line_args( count=argc)
	if argc eq 0 then begin
		maia_prefix = 'Maia_179_'
		conf_file = 'C:\Documents and Settings\rya113\.geopixe\Maia @ CSIRO - kandinski 179 blog blog2.Maia.conf'
	endif else if argc lt 2 then begin
		goto, bad_args
	endif else begin
		maia_prefix = argv[0]
		conf_file = argv[1]
	endelse
;	warning,'blog_file_da2',['arguments:',maia_prefix,conf_file]

  case !version.os_family of
    'MacOS': begin
		timeout_msg = "Resource temporarily unavailable"
       end
    'unix': begin
		timeout_msg = "Resource temporarily unavailable"
       end
    else: begin
		timeout_msg = "Connection timed out"
      end
  endcase

; If the n_buffers or buffer_size pars are changed, reboot.
; Or, uncomment the memory unmapping commands in fin:
; Then shutdown all referencing processes.

	debug = 0							; print debug lines
	prefix = maia_prefix + 'da_'

	psh = shared_memory_buffers( prefix=prefix, error=error, n_buffers=n_buffers, /floatd, n_float=50, n_long=1000)
	if error then goto, bad_shrmem
	buffer_size = (*psh).buffer_size
	nx = buffer_size[0]
	ny = buffer_size[1]
	nel = buffer_size[2]

	ppar = (*psh).ppar
	pdat = (*psh).pdat
	plong = (*psh).plong
	pfloat = (*psh).pfloat
	pbyte = (*psh).pbyte
	pfill = (*psh).pfill
	pvalid = (*psh).pvalid

	define_devices, default_device = device_object_index('MAIA_DEVICE')
	maia = maia_defaults(source='blog_file_da2', conf=conf_file)		; read "Maia.conf" default PVs

;	par array:	0	n_buffers
;				1	free
;				2	reset blog client (and shows status of reset)
;				3	kill blog client (from elsewhere to blog client here)
;				4	kill dependent client (to elsewhere if blog client is going down)
;				5	blog client is up and running
;			  6-12	free
;			 13-15	buffer_size (1-3 dimensions)
;
;	  incoming (from maia_launch ...):     reset kill
;	  outgoing (to maia_launch ...):                 kill running          buffer_size
;	                   0           1         2    3    4    5    6    7  ...   13

	error_mask = 'FFFFFFF000000000'x		; error bits in "dwell/readout"
	error_offset = -40
	duration_mask = '0000000FFFFFFFFF'x		; 32 bits plus 8 bits of next 32-bit word
	scale_offset = 10
	fshift = (2.) ^ (-24)

	timeout_retry = 0
	tseq_last = 0L
	tseq_first = 0L
	first = 1
	ylast = -10

	da_accum_1 = 35US
	mega_bytes_10 = 10000000LL
	mega_bytes_100 = 100000000LL

;	pf	0	% busy				pl	0	blog port		pb	*	blog server ip
;		1	% buffers lost			1	errors
;		2	t_interval				2	N DA in use
;		3	IC rate (from Epics)	3	X compress
;		4	sensitivity				4	Y compress
;		5							5	NN index
;		6							9	activate blog_da_file
;		10+	scale					10+	maia_IC_name (as bytes)

;		5							5	NN index
;		6							6	total used pixels
;		7	energy (from Epics)		7	max X
;		8	total flux				8	max Y
;		9							9	activate blog_da_file
;		10+	scale					10+	maia_IC_name (as bytes)

start:
	pb = pbyte[0]
	pl = plong[0]
	pd = pdat[0]
	pf = pfloat[0]

	n_Dwell = (*pl)[5]
	n_Flux = (*pl)[5] +1
	n_NN = (*pl)[5] +2
	n_NNPU = (*pl)[5] +3
	n_TT = (*pl)[5] +4
;	n_Flux1 = (*pl)[5] +5
;	n_Flux2 = (*pl)[5] +6

	(*pfill[0])[0] = 0
	(*pf)[3] = 12.6							; Mar-2009/Barnes 440 example
	(*pf)[4] = 10.							; 10 nA/V default

	used_mask = bytarr(nx,ny)
	q = where( (*pd)[*,*,n_Flux] ne 0.0, nq)
	if nq eq 0 then begin
		(*pl)[6] = 0						; total used pixels
		(*pl)[7] = 0						; max X
		(*pl)[8] = 0						; max Y
		(*pf)[8] = 0.0						; total flux
	endif else begin
		q_to_xy, q, nx, x,y
		(*pl)[6] = nq
		(*pl)[7] = max(x)
		(*pl)[8] = max(y)
		(*pf)[8] = total((*pd)[n_Flux*nx*ny + q])
		used_mask[q] = 1
	endelse
	(*ppar)[5] = 1							; blog client is running
	(*ppar)[2] = 0							; clear reset flag
	if (*pl)[1] lt 1 then (*pl)[1]=1

	repeat wait,1 until (*pl)[9] eq 1		; wait for a activate flag from Maia_update_da2
											; or simply set break point here and wait.

;	Need to set a break point in the maia_update_da2 routine and set the following by hand ...
;
	file1 = 'C:\NMP\AS\July-2011\Howitt\blog\16344\16344.0'
;	if file1 eq '' then return

	evt_file = select_evt_files( file1, '', 1, '.', '')
	n_files = n_elements(evt_file)

	for i=0L, n_files-1 do begin			; loop over blog files	==========================

		on_ioerror, bad_open
		wait, 1
		openr, unit, evt_file[i], /get_lun
		print, 'process file ',evt_file[i]

		start_time = systime(/seconds)
		tlast = start_time

	;	Start loop, detect (*ppar)[3] = 1 to indicate a remote
	;	kill command from client, etc.
	;	Detect (*ppar)[2] = 1 to indicate a reset command
	;	Use pvalid shared memory to indicate a valid/available buffer

		on_ioerror, bad_read
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		tin = systime(/seconds)

		pr = read_maia( unit, n_buffer=mega_bytes_10, n_actual=n_actual, accept=[da_accum_1])
		if ptr_good(pr) eq 0 then goto, next_file

	;**------------------------------------------------------------------------------------------**

		npr = n_elements(pr)
		for j=0L,npr-1 do begin				; loop over returned pointer list ..................

			p = pr[j]

			n_da_max = ((*p).sub_header.count - 3) / 2
			if debug then print, 'Header1 Byte Length=',l,' Length/4-9=',(l/4)-9,',  Header2 Word Count=',(*p).sub_header.count
			if debug then print, 'N DA=',n_da_max

		;	Dig out DTpp and DA arrays from 'b' array in record ...

			DTpp = ulong((*p).b,0,3)
			swap_bytes, DTpp, /big_endian_data
			DA = long64((*p).b,12,n_da_max)
			swap_bytes, DA, /big_endian_data

		;	n_det = ((*p).header2.count - 16) < n_detectors

			compressX = (*pl)[3] > 1
			compressY = (*pl)[4] > 1
			scale = (*pf)[scale_offset:scale_offset+nel-1]

			x = (*p).sub_header.X / compressX
			y = (*p).sub_header.Y / compressY
			duration = (*p).sub_header.duration and duration_mask
			error_bits = ulong( ishft((*p).sub_header.duration and error_mask, error_offset))
			flux1 = (*p).sub_header.flux1
			flux2 = (*p).sub_header.flux2
			duration = float(duration) / 10000L					; dwell in ms
			if y gt ylast+10 then begin
				print,'   process buffer ', j, ' line Y = ',y, ' duration = ',duration
				wait, 0.1
				ylast = y
			endif

			(*pl)[1] = error_bits								; error bits
			(*pf)[2] = duration									; duration of pixel

			if debug then print, '(*pl)[2], nel-3, n_da_max = ', (*pl)[2],nel-3,n_da_max
			n_da = min( [(*pl)[2],nel-3,n_da_max])				; number of DA elements (less 3 for DT maps)

			if (x ge 0) and (x lt nx) and (y ge 0) and (y lt ny) then begin

				if used_mask[x,y] eq 0 then begin
					used_mask[x,y] = 1
					(*pl)[6] = (*pl)[6] + 1						; total used pixels
				endif
				(*pl)[7] = (*pl)[7] > x							; max X
				(*pl)[8] = (*pl)[8] > y							; max Y

		;		Epics PV value (*pf)[3] from blog_client_epics process (Monitor records),
		;		and sensivitiy range (*pf)[4] from Maia_Launch (maia_update_da2).

				flux_rate = (*pf)[3] * (*pf)[4]					; from blog_client_epics process
				dflux = duration * flux_rate * 0.001
				(*pf)[8] = (*pf)[8] + dflux

		;		Increment the DA images ...

				if debug then print, da[0:n_da-1]

				(*pd)[x,y,0:n_da-1] = (*pd)[x,y,0:n_da-1] + float(DA[0:n_da-1]) * fshift * scale[0:n_da-1]

		;		Increment the DTpp maps for NN, NNPU and TT.
		;		TT as ToT ADC count sum, not correctecd by 'alpha' ...
		;		This order also in maia_launch_update_elements, blog_client_da2, maia_launch (setup), blog_file_da2

				(*pd)[x,y,N_Dwell] = (*pd)[x,y,N_Dwell] + duration
				(*pd)[x,y,N_Flux] = (*pd)[x,y,N_Flux] + dflux
				(*pd)[x,y,N_NN] = (*pd)[x,y,n_NN] + float(DTpp[0])
				(*pd)[x,y,N_NNPU] = (*pd)[x,y,n_NNPU] + float(DTpp[1])
				(*pd)[x,y,N_TT] = (*pd)[x,y,n_TT] + float(DTpp[2])

		;		Should add 'flux1' and 'flux2' maps here too ...

		;		(*pd)[x,y,N_flux1] = (*pd)[x,y,N_flux1] + float(flux1)
		;		(*pd)[x,y,N_flux2] = (*pd)[x,y,N_flux2] + float(flux2)
			endif

			t = systime(/seconds)
			percent = (tin gt tlast) ? 100. * (t-tin)/(tin-tlast) : 1.
			(*pf)[0] = percent
			if debug then print,' Percent busy=', percent
			(*pf)[1] = 0
			tlast = tin
		endfor								; pointer loop .................................

		ptr_free, pr

next_file:
		close_file, unit
	endfor									; file for loop ================================

;-------------------------------------------------------------------------------------------

done:
	if (*ppar)[2] eq 1 then begin			; reset blog client
		print,'Reset blog-client.'
		if ptr_valid(ps) then close_file, unit
		(*pfill[0])[0] = 0
		(*pvalid[0])[0] = 0
		(*ppar)[2] = 0
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, start
	endif
;	if (*ppar)[3] eq 0 then goto, start

fin:
	(*ppar)[5] = 0							; blog client is not running
	(*ppar)[4] = 1							; kill dependent client
	(*pfill[0])[0] = 0
	(*pvalid[0])[0] = 0
	print,'Finish: free shared memory ...'

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-boot.

;	shared_memory_unmap, prefix=prefix, n_buffers=n_buffers
	close_file, unit
	return

bad_open:
	warning,'blog_file_da2','error opening blog file'
	goto, fin
bad_read:
	warning,'blog_file_da2','error reading blog file'
	goto, fin
bad_len:
	warning,'blog_file_da2','illegal very large "len" value'
	goto, fin
bad_da2:
	warning,'blog_file_da2',' bad DA2 record length'
	goto, fin
bad_shrmem:
	warning,'blog_file_da2',' error shrmem allocate'
	goto, fin
bad_args:
	warning,'blog_file_da2',' bad passed arguments. "argc" not 2.'
	return
end

