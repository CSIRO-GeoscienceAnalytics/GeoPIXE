pro daq_client_parameters_slow

; NOTE used in DAQ as yet ...

; Read Klee variables and copy into parameter struct in shared memory.
; Fast update of parameters to shared memory --> read by "daq_update_parameters3.pro"
; Also, read IP address and Port number too.

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
	       warning,'daq_client_parameters_slow',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return
	    endif
	endif
	tin = systime(/seconds)
	tlast = systime(/seconds)-1

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
; Blog client also loads routines from DAQ_Control.sav and all
; device object SAV files, which includes DAQ.

  startupp, /daq

	argv = command_line_args( count=argc)
	if argc eq 0 then begin
		daq_prefix = 'DAQ_173_'
		conf_file = 'C:\Documents and Settings\rya113\.geopixe\CSIRO-shadow-DAQ-Klee-173 blog midgard.DAQ.conf'
		n_detectors = 36
	endif else if argc lt 3 then begin
		goto, bad_args
	endif else begin
		daq_prefix = argv[0]
		n_detectors = long(argv[1])
		conf_file = argv[2]
	endelse
;	warning,'daq_client_parameters_slow',['arguments:',daq_prefix,conf_file]

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

; If the shared struct changed, reboot.
; Or, uncomment the memory unmapping commands in fin:
; Then shutdown all referencing processes.
; Make them the large sizes BEFORE building a version for Linux.
;.....................................................................................

	debug = 0										; print debug lines
	prefix = daq_prefix + 'pars_'					; use same shared memory as "daq_client_parameters"
	template = define(daq_shared1 = n_detectors)	

	psh = shared_memory_struct( prefix=prefix, template=template, error=error )
	if error then goto, bad_shrmem

	ppar = (*psh).ppar
	pdat = (*psh).pdat
	plong = (*psh).plong
	pfloat = (*psh).pfloat
	pbyte = (*psh).pbyte

	s = get_login_info()
	clientname = s.machine_name + ':' + prefix

	define_devices
	daq = daq_defaults(source='daq_client_parameters_slow', conf=conf_file)			; read "DAQ.conf" default PVs

;	par array:	0
;				1
;				2	reset blog client (and shows status of reset)
;				3	kill blog client (from elsewhere to blog client here)
;				4	kill dependent client (to elsewhere if blog client is going down)
;				5	blog client is up and running
;			  6-15	free
;
;	  incoming (from daq_launch ...):     reset kill
;	  outgoing (to daq_launch ...):                 kill running   other running
;	                   0           1         2    3    4    6        5
;.....................................................................................

	timeout_retry = 0
	first = 1

;	pf	0	% busy (other)		pl	0	daq port			pb	*	daq ip
;		1	% buffers lost 			1	errors
;		2	t_interval
;		3	% busy (slow)			2	errors
;		4	% buffers lost
;		5	t_interval

start:
	pf = pfloat
	pb = pbyte
	pl = plong
	pm = pdat
	n_detectors = (*pm).n_detectors
	data = (*pm).layout_data				; from (*play).data
	daq_ip = string( *pb)
	daq_port = (*pl)[0]
	token_daq = '0'
	(*pl)[2] = 0

	wait, 1
	sock = open_socket( ip=daq_ip, port=daq_port, token=token_daq, error=error, $
					read_timeout=10, retries=0, client=clientname )
	if error ne 0 then goto, bad_open
	ps = ptr_new(sock)
	(*ps).n_detectors = n_detectors

again:
	start_time = systime(/seconds)
	tlast = start_time
	errors_last=0
	(*ppar)[6] = 1						; daq client is running (non standard index used here because
										; this process shares this memory with daq_client_parameters)

;	Start loop, detect (*ppar)[3] = 1 to indicate a remote kill command from client, etc.
;	Detect (*ppar)[2] = 1 to indicate a reset command

more:
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	tin = systime(/seconds)

;-------------------------------------------------------------------------------------------
;	Now read parameters. Note that socket_command_get will do a socket retry if
;	there is a transmission error or lost connction.

; Update DAQ rates ----------------------------------------

	v  = socket_command_get( ps, 'rate', class='event', error=err)
	if err eq 0 then begin
		(*pm).control.status.event_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'discard.rate', class='event', error=err)
	if err eq 0 then begin
		(*pm).control.status.discard_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'rate', class='photon', error=err)
	if err eq 0 then begin
		(*pm).control.status.photon_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'rate', class='blog', /quiet, error=err)
	if err eq 0 then begin
		(*pm).control.status.blog_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'rate', class='charge', error=err)
	if err eq 0 then begin
		(*pm).control.status.charge_rate = v[0]
	endif else (*pl)[1]++


;------------------------------------------------------------------------------------
; Other large things that change only occassionally ...

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 2.

	v  = socket_command_get( ps, 'connected', class='blog', error=err)
	if err eq 0 then begin
		(*pm).control.status.blog = v[0]
	endif
	
	v  = socket_command_get( ps, 'beam.energy', class='metadata', error=err)
	if err eq 0 then begin
		if v[0] gt 0. then (*pm).run.energy = v[0] * 1.0e-6
	endif

	v = socket_command_get( ps, 'coeff', class='charge', /float, error=err)
	if err eq 0 then begin
		(*pm).beam.charge.scale = v[0]
		(*pm).IC.pv.val = v[0]
	endif
	v = socket_command_get( ps, 'unit', class='charge', chip=0, /string, error=err)
	if err eq 0 then begin
		(*pm).beam.charge.unit = v[0]
		sens = charge_sensitivity( (*pm).IC.pv.val, v[0], /ionbeam)
		val = charge_gain_units( sens, unit=vunit)
		(*pm).IC.pv.val = val
		(*pm).IC.pv.unit = vunit
	endif

; gaintrim parameters.

;	v  = socket_command_get( ps, 'ecoeff', channel=0, class='gaintrim.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
;	if (err eq 0) and (n_elements((*pm).channel.trim.E.b) eq n_elements(v)) then begin
;		(*pm).channel.trim.E.b = v[data.index]
;	endif else (*pl)[2]++
;	v  = socket_command_get( ps, 'ecoeff', channel=1, class='gaintrim.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
;	if (err eq 0) and (n_elements((*pm).channel.trim.E.a) eq n_elements(v)) then begin
;		(*pm).channel.trim.E.a = v[data.index]
;	endif else (*pl)[2]++
;	v  = socket_command_get( ps, 'tcoeff', channel=0, class='gaintrim.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
;	if (err eq 0) and (n_elements((*pm).channel.trim.T.b) eq n_elements(v)) then begin
;		(*pm).channel.trim.T.b = v[data.index]
;	endif else (*pl)[2]++
;	v  = socket_command_get( ps, 'tcoeff', channel=1, class='gaintrim.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
;	if (err eq 0) and (n_elements((*pm).channel.trim.T.a) eq n_elements(v)) then begin
;		(*pm).channel.trim.T.a = v[data.index]
;	endif else (*pl)[2]++
;
;	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
;	wait, 2.

; Update DA rGamma array here cos it's large ... (rather than in 'daq_launch_read_da')

;	v = socket_command_get( ps, 'enable', class='da', error=err)
;	if err eq 0 then begin
;		(*pm).DA.on = v[0]
;	endif else (*pl)[2]++
;
;	if (*pm).DA.on and ((*pm).version.software ge 5411) then begin
;		v = socket_command_get( ps, 'number', class='da.element', error=err)
;		if err eq 0 then begin
;			(*pm).DA.N = v[0]
;		endif else (*pl)[2]++
;		if (*pm).DA.N eq 0 then (*pm).DA.on=0
;
;		if (*pm).DA.on and ((*pm).DA.N gt 0) then begin
;			v = socket_command_get( ps, 'name', class='da.element', chip=-1, n_chips=(*pm).number.da, /string, error=err)
;			for i=0,min([n_elements(v),(*pm).DA.N])-1 do begin
;				b = [byte(v[i]),0B]
;				n = n_elements(b) < n_elements((*pm).DA.bname[*,0])
;				if err eq 0 then begin
;					(*pm).DA.bname[0:n-1,i] = b[0:n-1]
;				endif else (*pl)[2]++
;			endfor
;		
;			v = socket_command_get( ps, 'scale', class='da.element', chip=-1, n_chips=(*pm).number.da, /float, error=err)
;			if err eq 0 then begin
;				(*pm).DA.scale[0:(*pm).DA.N-1] = v[0:(*pm).DA.N-1]
;			endif else (*pl)[2]++
;
;			rGamma = fltarr(n_elements((*pm).DA.rGamma[*,0]),n_elements((*pm).DA.rGamma[0,*]))
;			for j=0L,(*pm).DA.N-1 do begin
;				v = socket_command_get( ps, 'geometry', class='da.element', chip=j, channel=-1, n_channels=n_detectors, /float, error=err)
;				if err eq 0 then begin
;					rGamma[0:n_detectors-1,j] = v[0:n_detectors-1]
;				endif else (*pl)[2]++
;			endfor
;			(*pm).DA.rGamma = rGamma
;		endif else begin
;			(*pm).DA.rGamma[*] = 0.0
;		endelse
;	endif

;	v = socket_command_get( ps, 'enable', class='deadtime', error=err)
;	if err eq 0 then begin
;		(*pm).enable.deadtime = v[0]
;	endif else (*pl)[2]++
;
;	if (*pm).enable.deadtime then begin	
;		if (*pm).version.software ge 4737 then begin
;			v = socket_command_get( ps, 'coeff', class='deadtime.time', channel=-1, n_channels=2, /float, error=err)
;			if err eq 0 then begin
;				(*pm).deadtime.cal.b = v[0]
;				(*pm).deadtime.cal.a = v[1]
;			endif else (*pl)[2]++
;		endif
;	endif

	v = socket_command_get( ps, 'enable', class='pixel', error=err)
	if err eq 0 then begin
		(*pm).enable.pixel = v[0]
	endif else (*pl)[2]++

	if (*pm).enable.pixel then begin
		v = socket_command_get( ps, 'coord.extent', class='pixel.dim', n_channels=6, channel=-1, error=err)
		if err eq 0 then begin
			(*pm).scan.extent = v
		endif else (*pl)[2]++

		v = socket_command_get( ps, 'origin', class='pixel.dim', n_chips=6, chip=-1, error=err)
		if err eq 0 then begin
			(*pm).scan.origin = v
		endif else (*pl)[2]++

		v = socket_command_get( ps, 'pitch', class='pixel.dim', n_chips=6, chip=-1, error=err)
		if err eq 0 then begin
			(*pm).scan.pitch = v
		endif else (*pl)[2]++
	endif 

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 2.

	t = systime(/seconds)
	dt = t-tin -3*2.
	(*pf)[3] = 100.*dt/(t-tlast) > 0.1
	(*pf)[5] = t-tlast
	tlast = t
	if (*pl)[1] gt errors_last then begin
		print,'DAQ_parameters_slow, more errors ...', (*pl)[2]
	endif
	errors_last = (*pl)[2]
	goto, more

;-------------------------------------------------------------------------------------------

done:
	if (*ppar)[2] eq 1 then begin						; reset daq client
		print,'Reset Parameters daq-client.'
		if ptr_valid(ps) then close_file,(*ps).unit		; close socket
		(*ppar)[2] = 0
		(*pf)[3] = 0.0
		(*pf)[4] = 0.0
		(*pl)[2] = 0
		goto, start
	endif
	if (*ppar)[3] eq 0 then goto, more

fin:													; kill daq client
	if ptr_valid(ps) then close_file,(*ps).unit			; close socket
	(*ppar)[6] = 0										; daq client is not running (alt flag!)
	(*ppar)[4] = 1										; kill dependent client
	print,'Finish: free shared memory ...'

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-start.

;	shared_memory_unmap, prefix=prefix
	return

bad_open:
	warning,'daq_client_parameters_slow','error opening socket'
	goto, fin
bad_shrmem:
	warning,'daq_client_parameters_slow',' error shrmem allocate'
	return
bad_args:
	warning,'daq_client_parameters_slow',' bad passed arguments. "argc" not 3.'
	return
end

