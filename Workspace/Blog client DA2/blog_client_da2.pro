pro blog_client_da2, args=sargs

; Uses shared memory for the actual image memory. See "blog_client_fake_da", which
; uses shared memory as buffers
;
; This needs to be a slave now, to read n_channels back from buffer_size variables
; in shared memory ppar array. Also, read IP address and Port number too.

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
	       warning,'blog_client_da2',['IDL run-time error caught.', '', $
	          'Fatal Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c,'Terminate process.'], /error
	       MESSAGE, /RESET
	       return
	    endif
	endif
	if n_elements(debug) lt 1 then debug=0						; print debug lines
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
; Blog client also loads routines from Maia_Control.sav and Maia
; device object SAV file.

	startupp, /maia

	if n_elements(sargs) eq 0 then begin
		args = {prefix:'CGR_268_', conf:'C:\Users\rya113\.geopixe\Maia Mapper @ CSIRO Per - kandinski 268 blog mr-05.Maia.conf', server:''}
	endif else begin
		args = unstringify( sargs)
	endelse
	maia_prefix = args.prefix
	conf_file = args.conf
	server = args.server
	debug = args.debug					; (0:off, 1:on, -1:default)

	comms = 0
	if server ne '' then begin
		if strlen(server) gt 3 then begin
			comms = open_comms( server=server, source='blog_client_da2', error=err)
			if err eq 0 then begin
				save_comms_in_common, comms		; save comms for implicit 'log_message' in 'warning', 'alarm_popup'
				log_message, comms, type='INFO', 'blog_client_da2 started, with logging using server = '+server
			endif
;			warning,'blog_client_da2',['arguments:',maia_prefix,conf_file,server]
		endif
	endif

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
; Make them the large sizes BEFORE building a version for Linux.

	prefix = maia_prefix + 'da_'

	psh = shared_memory_buffers( prefix=prefix, error=error, n_buffers=n_buffers, /floatd, n_float=50, n_long=1000)
	if error then goto, bad_shrmem
	buffer_size = (*psh).buffer_size
	nx = buffer_size[0]
	ny = buffer_size[1]
	nel = buffer_size[2]
	nxy = long(nx)*long(ny)

	ppar = (*psh).ppar
	pdat = (*psh).pdat
	plong = (*psh).plong
	pfloat = (*psh).pfloat
	pbyte = (*psh).pbyte
	pfill = (*psh).pfill
	pvalid = (*psh).pvalid

	s = get_login_info()
	clientname = s.machine_name + ':' + prefix
	username = s.user_name
	log_message, comms, type='INFO', 'blog_client_da2, clientname='+clientname+', username='+username

	define_devices
	maia = maia_defaults(source='blog_client_da2', conf=conf_file)			; read "Maia.conf" default PVs
	if debug eq -1 then debug = maia.blog.debug

	if debug then begin
		lfile = extract_path(conf_file) + username + '_' + prefix + 'debug.log'
		openw, dun, lfile, /get_lun
		gprint, active=1
		gprint, out=dun, 'Blog Client DA2 debug log'
	endif

;	par array:	0	n_buffers
;				1	free
;				2	reset blog client (and shows status of reset)
;				3	kill blog client (from elsewhere to blog client here)
;				4	kill dependent client (to elsewhere if blog client is going down)
;				5	blog client is up and running
;			  6-8	Redirect indices for x,y,z axes
;				9	Flip X axis
;				10	Scan X size in pixels
;			 11-12	free
;			 13-15	buffer_size (1-3 dimensions)
;	                                                               Redirection
;	  incoming (from maia_launch ...):   reset kill                 x   y   z   FlipX ScanX
;	  outgoing (to maia_launch ...):                kill running                 9	   10     buffer_size
;	                   0           1       2    3    4     5        6   7   8  				    13-15

	error_mask = 'FFFFFFF000000000'x		; error bits in "dwell/readout"
	error_offset = -40
	duration_mask = '0000000FFFFFFFFF'x		; 32 bits plus 8 bits of next 32-bit word
	scale_offset = 10
	fshift = (2.) ^ (-24)

	timeout_retry = 0
	tseq_last = 0L
	tseq_first = 0L

	sendnext = 7US
	da_accum_1 = 35US
	request_da2 = {aa:'AA'xub, tag:sendnext, bb:'BB'xub, len:2US, desired:[da_accum_1] }

	header = {aa:0B, tag:0US, bb:0B, len:0US, prevlen:0US, $
			seq:0UL, tseq:0UL, tv_sec:0UL, tv_usec:0UL, $
			client:0UL, unused:0UL}

	header2 = {X:0UL,Y:0UL,Z:0UL, trigger:0UL, duration:0ULL, flux1:0UL, flux2:0UL, count:0UL}

	linux_request = request_da2
	swap_bytes, linux_request, /big_endian_data

	setblockqueuedepth = 60US
	nb = 1000UL
	set_blockqueuedepth = {aa:'AA'xub, tag:setblockqueuedepth, bb:'BB'xub, len:4, desired:nb }
	swap_bytes, set_blockqueuedepth, /big_endian_data

	setname = 54US
	b = [byte(clientname),0B]
	nb = strlen(clientname) + 1
	set_clientname = {aa:'AA'xub, tag:setname, bb:'BB'xub, len:uint(nb), desired:b }
	swap_bytes, set_clientname, /big_endian_data

;	pf	0	% busy				pl	0	blog port		pb	*	blog server ip
;		1	% buffers lost			1	errors
;		2	t_interval				2	N DA in use
;		3	IC rate (from Epics)	3	X compress
;		4	sensitivity				4	Y compress
;		5							5	Dwell index
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
	blog_server_ip = string( *pb)
	blog_port = (*pl)[0]
	if (*pl)[1] lt 1 then (*pl)[1]=1

	wait, 1
	sock = open_socket( ip=blog_server_ip, port=blog_port, error=error, $
								read_timeout=3, retries=0, client=clientname )
	if error then goto, bad_open
	ps = ptr_new(sock)

	n_Dwell = (*pl)[5]
	n_Flux = (*pl)[5] +1
	n_NN = (*pl)[5] +2
	n_NNPU = (*pl)[5] +3
	n_TT = (*pl)[5] +4
	n_Flux1 = (*pl)[5] +5
	n_Flux2 = (*pl)[5] +6

	(*pfill[0])[0] = 0
;	(*pf)[3] = 12.6							; Mar-2009/Barnes 440 example
;	(*pf)[4] = 10.							; 10 nA/V default

	used_mask = bytarr(nx,ny)

	(*pl)[6] = 0							; total used pixels
	(*pl)[7] = 0							; max X
	(*pl)[8] = 0							; max Y
	(*pf)[8] = 0.0							; total flux
	(*pd)[*,*,*] = 0.0						; image arrays

;	q = where( (*pd)[*,*,n_Flux] ne 0.0, nq)
;	if nq eq 0 then begin
;		(*pl)[6] = 0						; total used pixels
;		(*pl)[7] = 0						; max X
;		(*pl)[8] = 0						; max Y
;		(*pf)[8] = 0.0						; total flux
;	endif else begin
;		q_to_xy, q, nx, x,y
;		(*pl)[6] = nq
;		(*pl)[7] = max(x)
;		(*pl)[8] = max(y)
;		(*pf)[8] = total((*pd)[n_Flux*nxy + q])
;		used_mask[q] = 1
;	endelse
	tseq_lost = 0L
	loglast_tin = 0.
	loglast_xy = 0.
	good_xy = 0LL
	bad_xy = 0LL
	lost_da = 0LL
	first = 1

	on_ioerror, bad_open2
	writeu, (*ps).unit, set_clientname
	if debug then gprint,'blog_client_da2',' write hostname succeeded'

	on_ioerror, bad_open3
	writeu, (*ps).unit, set_blockqueuedepth
	if debug then gprint,'blog_client_da2',' write blockqueuedepth succeeded'

again:
	start_time = systime(/seconds)
	tlast = start_time
	pre_write = 0
	(*ppar)[5] = 1						; blog client is running

;	Start loop, detect (*ppar)[3] = 1 to indicate a remote
;	kill command from client, etc.
;	Detect (*ppar)[2] = 1 to indicate a reset command
;	Use pvalid shared memory to indicate a valid/available buffer

more:
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

;**------------------------------------------------------------------------------------------**
;	Note from request write must complete both header and payload reads without interruption

	on_ioerror, bad_write
	if pre_write eq 0 then begin
		writeu, (*ps).unit, linux_request
		if debug then gprint,'blog_client_da2',' write succeeded'
	endif else pre_write = 0

reread:
	on_ioerror, bad_read
	readu, (*ps).unit, header

	swap_bytes, header, /big_endian_data

	if debug then gprint, 'Tag=',header.tag,'   Len=',header.len
	l = (header.len)[0]
	if l eq 0 then goto, more
	if first then begin
		tseq_first = header.tseq
		tseq_last = header.tseq
		tseq_lost = 0L
		first = 0
	endif
	dsl = (( ((header.tseq - tseq_last) > 1) - 1) > 0)
	if ((dsl lt 0) or (dsl gt 100000L)) then begin
		log_message, comms, type='WARNING', 'blog_client_da2, Bad "dsl" increment='+str_tidy(dsl)+' (tseq_lost was='+str_tidy(tseq_lost)+', lost_da was='+str_tidy(lost_da)+')'
	endif
	tseq_lost += dsl
	lost_da += dsl
	if ((dsl lt 0) or (dsl gt 100000L)) then begin
		log_message, comms, type='WARNING', 'blog_client_da2, New tseq_lost='+str_tidy(tseq_lost)+', lost_da='+str_tidy(lost_da)+')'
	endif
	tseq_last = header.tseq

	payload = bytarr(header.len)
	if header.tag ne da_accum_1 then goto, reread3

reread2:
	on_ioerror, bad_read2
	readu, (*ps).unit, header2

	swap_bytes, header2, /big_endian_data

	n_da_max = (header2.count - 3) / 2
	if debug then gprint, 'Header1 Byte Length=',l,' Length/4-9=',(l/4)-9,',  Header2 Word Count=',header2.count
	if debug then gprint, 'N DA=',n_da_max

	payload = { DTpp:{ n_events:0UL, n_pileup:0UL, total_time:0UL}, DA:lon64arr(n_da_max) }

;	n_det = (header2.count - 16) < n_detectors

reread3:
	on_ioerror, bad_read3
	readu, (*ps).unit, payload

	tin = systime(/seconds)
	swap_bytes, payload, /big_endian_data

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

	writeu, (*ps).unit, linux_request
	if debug then gprint,'blog_client_da2',' write succeeded'
	pre_write = 1

;**------------------------------------------------------------------------------------------**

	maia_IC_name = string( byte( (*pl)[10:*] ))
	s = strsplit( maia_IC_name, ':.', /extract)
	ns = n_elements(s)
	maia_hw_counter = 0
	maia_counter = 0
	if (ns ge 1) then begin
		if s[0] eq 'Maia' then maia_hw_counter=1		; use Maia h/w flux counters
		if (ns ge 3) then begin
			if s[2] eq 'FC1' then maia_counter=1		; FC1 selected instead of FC0
			if s[2] eq 'time' then maia_counter=2		; dwell time selected instead of FC0
		endif
	endif

	ix = (*ppar)[6]										; redirect X axis
	iy = (*ppar)[7]										; redirect Y axis
	iz = (*ppar)[8]										; redirect Z axis
	flipX = (*ppar)[9]									; flip X axis
	ScanX = (*ppar)[10]									; X axis scan size
	compressX = (*pl)[3] > 1
	compressY = (*pl)[4] > 1
	ScanX = ScanX/compressX
	if ScanX eq 0 then ScanX=nx
	scale = (*pf)[scale_offset:scale_offset+nel-1]		; DA scale factors

	case ix of
		0: x1 = header2.X
		1: x1 = header2.Y
		2: x1 = header2.Z
	endcase
	case iy of
		0: y1 = header2.X
		1: y1 = header2.Y
		2: y1 = header2.Z
	endcase
	case iz of
		0: z1 = header2.X
		1: z1 = header2.Y
		2: z1 = header2.Z
	endcase
	x = x1 / compressX
	y = y1 / compressY

	if FlipX eq 1 then begin
		x = ScanX - x									; flip X axis
	endif

	iduration = header2.duration and duration_mask
	error_bits = ulong( ishft(header2.duration and error_mask, error_offset))
	duration = float(iduration) / 10000L				; dwell in ms (FPGA in 100 ns units)
	flux1 = header2.flux1
	flux2 = header2.flux2
	flux3 = duration
	flux = [flux1, flux2, flux3]
	if debug then gprint, 'x,y (compressed, flipped), flux1, dwell = ', x,y,flux1,duration

	(*pl)[1] = error_bits								; error bits
	(*pf)[2] = duration									; duration of pixel

	n_el_maia = n_elements(special_elements(/maia))
	if debug then gprint, '(*pl)[2], nel-3, n_da_max = ', (*pl)[2],nel-3,n_da_max
	n_da = min( [(*pl)[2]>1,nel-n_el_maia,n_da_max])	; number of DA elements (ignore NN, NNPU, ... maps)

	if (x ge 0) and (x lt nx) and (y ge 0) and (y lt ny) then begin
		good_xy++

		if used_mask[x,y] eq 0 then begin
			used_mask[x,y] = 1
			(*pl)[6] = (*pl)[6] + 1						; total used pixels
		endif											; to maia_update_da2
		(*pl)[7] = (*pl)[7] > x							; max X
		(*pl)[8] = (*pl)[8] > y							; max Y

;		Epics PV value (*pf)[3] from blog_client_epics process (Monitor records),
;		and sensivitiy range (*pf)[4] from Maia_Launch (maia_update_da2).

		if maia_hw_counter then begin
			dflux = flux[maia_counter]					; use Maia h/w Flux counters
			xfs = (maia_counter eq 2) ? 1.0 : (*pf)[4]
			dflux = float(dflux) * xfs					; correction (8/10/13)
		endif else begin
			flux_rate = (*pf)[3] * (*pf)[4]				; from blog_client_epics process
			dflux = duration * flux_rate * 0.001
		endelse
		(*pf)[8] = (*pf)[8] + dflux						; total flux to maia_update_da2

;		Increment the DA images ...

		if debug then gprint, payload.da[0:n_da-1]

		(*pd)[x,y,0:n_da-1] = (*pd)[x,y,0:n_da-1] + float(payload.DA[0:n_da-1]) * fshift * scale[0:n_da-1]

;		Increment the DTpp maps for NN, NNPU and TT.
;		TT as ToT ADC count sum, not correctecd by 'alpha' ...
;		This order also in maia_launch_update_DA_images, blog_client_da2, maia_launch (setup), blog_file_da2

		(*pd)[x,y,N_Dwell] = (*pd)[x,y,N_Dwell] + duration
		(*pd)[x,y,N_Flux] = (*pd)[x,y,N_Flux] + dflux
		(*pd)[x,y,N_NN] = (*pd)[x,y,n_NN] + float(payload.DTpp.n_events)
		(*pd)[x,y,N_NNPU] = (*pd)[x,y,n_NNPU] + float(payload.DTpp.n_pileup)
		(*pd)[x,y,N_TT] = (*pd)[x,y,n_TT] + float(payload.DTpp.total_time)

		(*pd)[x,y,N_flux1] = (*pd)[x,y,N_flux1] + float(flux1)
		(*pd)[x,y,N_flux2] = (*pd)[x,y,N_flux2] + float(flux2)
	endif else bad_xy++

	t = systime(/seconds)
	percent = (tin gt tlast) ? 100. * (t-tin)/(tin-tlast) : 1.
	(*pf)[0] = percent
	if debug then gprint,' Percent busy=', percent
	lost = 100. * tseq_lost/(tseq_last - tseq_first + 1)
	(*pf)[1] = lost
	if (lost gt 80) and ((tseq_last - tseq_first) gt 20) and (t gt loglast_tin + 100.) then begin
		log_message, comms, type='WARNING', 'blog_client_da2, Lost records exceed 30% ('+str_tidy(lost)+')'
		loglast_tin = t
	endif
	if debug then gprint,' Percent lost =', lost
	if ((tseq_last - tseq_first) gt 100) or ((tin-tlast) gt 10) then begin
		tseq_first = tseq_last
		tseq_lost = 0L
	endif
	if (t gt loglast_xy + 100.) then begin
		lostp = 100. * ( float(lost_da) / float((lost_da+good_xy+bad_xy)>1))
		log_message, comms, type='INFO', 'blog_client_da2, Good XY records='+str_tidy(good_xy)+' (bad='+str_tidy(bad_xy)+', lost='+str_tidy(lost_da)+' '+str_tidy(lostp,places=1)+'%)'
		loglast_xy = t
	endif
	tlast = tin
	goto, more

;-------------------------------------------------------------------------------------------

done:
	if (*ppar)[2] eq 1 then begin			; reset blog client
		log_message, comms, type='WARNING', 'blog_client_da2, RESET DA2 blog-client.'
		if debug then gprint,'Reset DA2 blog-client.'
		if ptr_valid(ps) then close_file,(*ps).unit			; close socket
		(*pfill[0])[0] = 0
		(*pvalid[0])[0] = 0
;		wait, 3								; wait for Epics client to see this
		(*ppar)[2] = 0						; clear reset flag
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, start
	endif
	if (*ppar)[3] eq 0 then goto, more

fin:
	if ptr_valid(ps) then close_file,(*ps).unit			; close socket
	(*ppar)[5] = 0							; blog client is not running
	(*ppar)[4] = 1							; kill dependent client
	(*pfill[0])[0] = 0
	(*pvalid[0])[0] = 0
	log_message, comms, type='INFO', 'blog_client_da2, EXIT blog DA2 client.'
	if debug then begin
		gprint,'Exit DA2 blog-client.'
		close_file, dun
	endif

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-boot.

;	shared_memory_unmap, prefix=prefix, n_buffers=n_buffers
	return

bad_open:
	warning,'blog_client_da2','error opening socket'
	goto, fin
bad_open2:
	warning,'blog_client_da2','error writing hostname to socket'
	goto, fin
bad_open3:
	warning,'blog_client_da2','error writing block queue depth to socket'
	goto, fin
bad_len:
	warning,'blog_client_da2','illegal very large "len" value'
	goto, fin
bad_shrmem:
	warning,'blog_client_da2',' error shrmem allocate'
	return
bad_args:
	warning,'blog_client_da2',' bad passed arguments. "argc" not 2.'
	return
	
bad_write:
	if debug then gprint,'blog_client_da2',' error writing request, re-open socket'
	goto, retry
bad_read:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
;		log_message, comms, type='WARNING', 'blog_client_da2, timeout on header read, reread ..'
;		if debug then gprint,'blog_client_da2',' timeout on header read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, reread
	endif
	warning,'blog_client_da2',[' error reading socket 1, re-open socket', $
			'!error_state.sys_msg returned "' + !error_state.sys_msg + '"'], /error
	if debug then gprint,'blog_client_activity',' error reading socket 1, re-open socket'
	goto, retry
bad_read2:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		log_message, comms, type='WARNING', 'blog_client_da2, timeout on header 2 read, reread ..'
		if debug then gprint,'blog_client_da2',' timeout on header2 read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, reread2
	endif
	if debug then gprint,'blog_client_da2',' error reading socket 2, re-open socket'
	goto, retry
bad_read3:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		log_message, comms, type='WARNING', 'blog_client_da2, timeout on payload read, reread ..'
		if debug then gprint,'blog_client_da2',' timeout on payload read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, reread3
	endif
	if debug then gprint,'blog_client_da2',' error reading socket 3, re-open socket'
	goto, retry
bad_da2:
	warning,'blog_client_da2',' bad DA2 record length'
	goto, retry

retry:
	if debug then gprint,'retry socket open ...'
	socket_retry, ps, error=error
	if error then begin
		warning,'blog_da2_activity',['error in Socket retry, exit ...','Client = '+clientname]
		goto, fin
	endif
	goto, again
end


