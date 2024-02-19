pro maia_client_parameters_slow, args=sargs

; Read Kandinski variables and copy into parameter struct in shared memory.
; Read by "maia_update_parameters3.
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
	       warning,'maia_client_parameters_slow',['IDL run-time error caught.', '', $
	          'Fatal Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c,'Terminate process.'], /error
	       MESSAGE, /RESET
	       return
	    endif
	endif
	tin = systime(/seconds)
	tlast = systime(/seconds)-1

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

; Kandinski client also loads routines from Maia_Control.sav and Maia
; device object SAV file.

	startupp, /maia

	if n_elements(sargs) eq 0 then begin
		args = {prefix:'CGR_268_', conf:'C:\Users\rya113\.geopixe\Maia Mapper @ CSIRO Per - kandinski 268 blog mr-05.Maia.conf', $
					server:'', n_detectors:384}
	endif else begin
		args = unstringify( sargs)
	endelse
	maia_prefix = args.prefix
	conf_file = args.conf
	server = args.server
	n_detectors = args.n_detectors
	debug = args.debug					; (0:off, 1:on, -1:default)

	comms = 0
	if server ne '' then begin
		if strlen(server) gt 3 then begin
			comms = open_comms( server=server, source='maia_client_parameters_slow', error=err)
			if err eq 0 then begin
				save_comms_in_common, comms		; save comms for implicit 'log_message' in 'warning', 'alarm_popup'
				log_message, comms, type='INFO', 'maia_client_parameters_slow started, with logging using server = '+server
			endif
;			warning,'maia_client_parameters_slow',['arguments:',maia_prefix,conf_file,server]
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

; If the shared struct changed, reboot.
; Or, uncomment the memory unmapping commands in fin:
; Then shutdown all referencing processes.
; Make them the large sizes BEFORE building a version for Linux.
;.....................................................................................

	prefix = maia_prefix + 'pars_'					; use same shared memory as "maia_client_parameters"
	template = define(maia_shared1 = n_detectors)	

	psh = shared_memory_struct( prefix=prefix, template=template, error=error )
	if error then goto, bad_shrmem

	ppar = (*psh).ppar
	pdat = (*psh).pdat
	plong = (*psh).plong
	pfloat = (*psh).pfloat
	pbyte = (*psh).pbyte

	s = get_login_info()
	clientname = s.machine_name + ':' + prefix
	username = s.user_name
	log_message, comms, type='INFO', 'maia_client_parameters_slow, clientname='+clientname+', username='+username

	define_devices
	maia = maia_defaults(source='maia_client_parameters_slow', conf=conf_file)			; read "Maia.conf" default PVs
	if debug eq -1 then debug = maia.blog.debug

	if debug then begin
		lfile = extract_path(conf_file) + username + '_' + prefix + 'debug.log'
;		print,'Open Maia Client parameters slow debug file: '+lfile
		openw, dun, lfile, /get_lun
		gprint, active=1
		gprint, out=dun, 'Maia Client parameters slow debug log'
	endif

;	par array:	0
;				1
;				2	reset blog client (and shows status of reset)
;				3	kill blog client (from elsewhere to blog client here)
;				4	kill dependent client (to elsewhere if blog client is going down)
;				5	blog client is up and running
;			  6-15	free
;
;	  incoming (from maia_launch ...):     reset kill
;	  outgoing (to maia_launch ...):                 kill running   other running
;	                   0           1         2    3    4    6        5
;.....................................................................................

	timeout_retry = 0
	first = 1

;	pf	0	% busy (other)		pl	0	maia port			pb	*	maia ip
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
	maia_ip = string( *pb)
	maia_port = (*pl)[0]
	token_maia = '0'
	(*pl)[2] = 0

	wait, 1
	sock = open_socket( ip=maia_ip, port=maia_port, token=token_maia, error=error, $
					read_timeout=3, retries=0, client=clientname )
	if error ne 0 then goto, bad_open
	ps = ptr_new(sock)
	(*ps).n_detectors = n_detectors
	loglast_tin = 0.

again:
	start_time = systime(/seconds)
	tlast = start_time
	errors_last=0
	(*ppar)[6] = 1						; maia client is running (non standard index used here because
										; this process shares this memory with maia_client_parameters)

;	Start loop, detect (*ppar)[3] = 1 to indicate a remote kill command from client, etc.
;	Detect (*ppar)[2] = 1 to indicate a reset command

more:
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	tin = systime(/seconds)

	socket_command_mode, ddm_down = 1 - clip((*pm).control.status.link, 0,1)

;-------------------------------------------------------------------------------------------
;	Now read parameters. Note that socket_command_get will do a socket retry if
;	there is a transmission error or lost connction.

	; Update detector monitor ----------------------------------------

	if (*pm).control.status.link then begin
	
		v  = socket_command_get( ps, 'bias.voltage', class='detector', error=err)
		if err eq 0 then begin
			(*pm).control.bias = v[0]
		endif else (*pl)[2]++
	
		v = socket_command_get( ps, 'peltier.current', class='detector', error=err)
		if err eq 0 then begin
			(*pm).control.peltier = v[0]
		endif else (*pl)[2]++
	
		v = socket_command_get( ps, 'bpinterlock', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.interlock = v[0]
		endif else (*pl)[2]++
	
		; Update Maia rates ----------------------------------------
	
		v  = socket_command_get( ps, 'link.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.link_rate = v[0]
		endif else (*pl)[2]++
	
		v  = socket_command_get( ps, 'event.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.event_rate = v[0]
		endif else (*pl)[2]++
	
		v  = socket_command_get( ps, 'bpinterlock.uptime', class='status', /double, error=err)
		if err eq 0 then begin
			(*pm).control.status.bpinterlock_uptime = v[0]
		endif else (*pl)[2]++
	
		v  = socket_command_get( ps, 'bpinterlock.downtime', class='status', /double, error=err)
		if err eq 0 then begin
			(*pm).control.status.bpinterlock_downtime = v[0]
		endif else (*pl)[2]++
	endif
	
;------------------------------------------------------------------------------------
; Other large things that change only occassionally ...

; Throttle factors, written to shared memory in maia_update_ET_spectra

	v  = socket_command_get( ps, 'factor', class='throttle.energy', chip=-1, n_chips=n_elements((*pm).throttle.factors), error=err)
	if (err eq 0) and (n_elements((*pm).throttle.factors) eq n_elements(v)) then begin
		(*pm).throttle.factors = v
	endif else (*pl)[2]++

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 2.

; gaintrim parameters.

	if n_elements((*pm).channel.trim.E.b) ne n_elements(data.index) then begin
		log_message, comms, type='ERROR', 'maia_client_parameters_slow, "data.index" does not match "trim.E.b". N(index)= '+ str_tidy( n_elements(data.index))
		gprint,'Maia_parameters_slow, "data.index" does not match "trim.E.b". N(index)= '+ str_tidy( n_elements(data.index))
		(*pl)[2]++
	endif else begin
		v  = socket_command_get( ps, 'ecoeff', channel=0, class='gaintrim.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
		if (err eq 0) and (n_elements((*pm).channel.trim.E.b) eq n_elements(v)) then begin
			(*pm).channel.trim.E.b = v[data.index]
		endif else (*pl)[2]++
		v  = socket_command_get( ps, 'ecoeff', channel=1, class='gaintrim.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
		if (err eq 0) and (n_elements((*pm).channel.trim.E.a) eq n_elements(v)) then begin
			(*pm).channel.trim.E.a = v[data.index]
		endif else (*pl)[2]++
		v  = socket_command_get( ps, 'tcoeff', channel=0, class='gaintrim.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
		if (err eq 0) and (n_elements((*pm).channel.trim.T.b) eq n_elements(v)) then begin
			(*pm).channel.trim.T.b = v[data.index]
		endif else (*pl)[2]++
		v  = socket_command_get( ps, 'tcoeff', channel=1, class='gaintrim.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
		if (err eq 0) and (n_elements((*pm).channel.trim.T.a) eq n_elements(v)) then begin
			(*pm).channel.trim.T.a = v[data.index]
		endif else (*pl)[2]++
	endelse

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 2.

; Update DA rGamma array here cos it's large ... (rather than in 'maia_launch_read_da')

	v = socket_command_get( ps, 'enable', class='da', error=err)
	if err eq 0 then begin
		(*pm).DA.on = v[0]
	endif else (*pl)[2]++

	if (*pm).DA.on and ((*pm).version.software ge 5411) then begin
		v = socket_command_get( ps, 'number', class='da.element', error=err)
		if err eq 0 then begin
			(*pm).DA.N = v[0]
		endif else (*pl)[2]++
		if (*pm).DA.N eq 0 then (*pm).DA.on=0

		if (*pm).DA.on and ((*pm).DA.N gt 0) then begin
			v = socket_command_get( ps, 'name', class='da.element', chip=-1, n_chips=(*pm).number.da, /string, error=err)
			if (err eq 0) and ((*pm).DA.N ge 1) and (n_elements(v) ge 1) then begin
				for i=0,min([n_elements(v),(*pm).DA.N])-1 do begin
					b = [byte(v[i]),0B]
					n = n_elements(b) < n_elements((*pm).DA.bname[*,0])
					if err eq 0 then begin
						(*pm).DA.bname[0:n-1,i] = b[0:n-1]
					endif else (*pl)[2]++
				endfor
			endif else (*pl)[2]++

		
			v = socket_command_get( ps, 'scale', class='da.element', chip=-1, n_chips=(*pm).number.da, /float, error=err)
			if err eq 0 then begin
				(*pm).DA.scale[0:(*pm).DA.N-1] = v[0:(*pm).DA.N-1]
			endif else (*pl)[2]++

			rGamma = fltarr(n_elements((*pm).DA.rGamma[*,0]),n_elements((*pm).DA.rGamma[0,*]))
			for j=0L,(*pm).DA.N-1 do begin
				v = socket_command_get( ps, 'geometry', class='da.element', chip=j, channel=-1, n_channels=n_detectors, /float, error=err)
				if (err eq 0) and (n_elements(rGamma) ge n_detectors) and (n_elements(v) ge n_detectors) then begin
					rGamma[0:n_detectors-1,j] = v[0:n_detectors-1]
				endif else (*pl)[2]++
			endfor
			(*pm).DA.rGamma = rGamma
		endif else begin
			(*pm).DA.rGamma[*] = 0.0
		endelse
	endif

;	Read any flux variable info relating to built-in Maia FC0, FC1 ...
;	Non-blank 'unit' indicates that they are in use, and are flagged remote=1.
;	The current selection and Epics PVs are read in 'maia_client_parameters'.

	if ((*pm).version.software ge 6646) then begin
		v1 = socket_command_get( ps, 'unit', class='flux.chan', n_chips=2, chip=-1, /string, error=err)
		if err eq 0 then begin
			if v1[0] ne '' then begin				; Maia:scaler.FC0
				(*pm).IC0.remote = 0
				v = socket_command_get( ps, 'coeff', class='flux.chan', chip=0, /float, error=err)
				if err eq 0 then begin
					(*pm).IC0.remote = 1
					(*pm).IC0.pv.val = v[0]
				endif else (*pl)[2]++
				v = socket_command_get( ps, 'name', class='flux.chan', chip=0, /string, error=err)
				if err eq 0 then begin
					b = [byte(v[0]),0B]
					n = n_elements(b) < n_elements((*pm).IC0.pv.bname)
					(*pm).IC0.pv.bname[0:n-1] = b[0:n-1]
				endif else (*pl)[2]++
				v = socket_command_get( ps, 'unit', class='flux.chan', chip=0, /string, error=err)
				if err eq 0 then begin
					sens = charge_sensitivity( (*pm).IC0.pv.val, v[0])
					val = charge_gain_units( sens, unit=vunit)
					(*pm).IC0.pv.val = val
					(*pm).IC0.pv.unit = vunit
				endif else (*pl)[2]++
			endif
			if v1[1] ne '' then begin				; Maia:scaler.FC1
				(*pm).IC1.remote = 0
				v = socket_command_get( ps, 'coeff', class='flux.chan', chip=1, /float, error=err)
				if err eq 0 then begin
					(*pm).IC1.remote = 1
					(*pm).IC1.pv.val = v[0]
				endif else (*pl)[2]++
				v = socket_command_get( ps, 'name', class='flux.chan', chip=1, /string, error=err)
				if err eq 0 then begin
					b = [byte(v[0]),0B]
					n = n_elements(b) < n_elements((*pm).IC1.pv.bname)
					(*pm).IC1.pv.bname[0:n-1] = b[0:n-1]
				endif else (*pl)[2]++
				v = socket_command_get( ps, 'unit', class='flux.chan', chip=1, /string, error=err)
				if err eq 0 then begin
					sens = charge_sensitivity( (*pm).IC1.pv.val, v[0])
					val = charge_gain_units( sens, unit=vunit)
					(*pm).IC1.pv.val = val
					(*pm).IC1.pv.unit = vunit
				endif else (*pl)[2]++
			endif
		endif else (*pl)[2]++
	endif
	
	v = socket_command_get( ps, 'enable', class='deadtime', error=err)
	if err eq 0 then begin
		(*pm).enable.deadtime = v[0]
	endif else (*pl)[2]++

	if (*pm).enable.deadtime then begin	
		if (*pm).version.software ge 4737 then begin
			v = socket_command_get( ps, 'coeff', class='deadtime.time', channel=-1, n_channels=2, /float, error=err)
			if (err eq 0) and (n_elements(v) eq 2) then begin
				(*pm).deadtime.cal.b = v[0]
				(*pm).deadtime.cal.a = v[1]
			endif else (*pl)[2]++
		endif
	endif

	v = socket_command_get( ps, 'enable', class='pixel', error=err)
	if err eq 0 then begin
		(*pm).enable.pixel = v[0]
	endif else (*pl)[2]++

	if (*pm).enable.pixel then begin
		v = socket_command_get( ps, 'coord.extent', class='pixel.dim', chip=-1, n_chips=3, error=err)
		if (err eq 0) and (n_elements(v) ge 2) then begin
			(*pm).scan.X = v[0]
			(*pm).scan.Y = v[1]
		endif else (*pl)[2]++
		v = socket_command_get( ps, 'dwell', class='scan', error=err)
		if err eq 0 then begin
			(*pm).scan.dwell.val = 1000. * v[0]
			(*pm).scan.dwell.on = 1
		endif else (*pl)[2]++
	
		v = socket_command_get( ps, 'origin', class='pixel.dim', n_chips=3, chip=-1, error=err)
		if (err eq 0) and (n_elements(v) ge 2) then begin
			(*pm).scan.origin.X = v[0]
			(*pm).scan.origin.Y = v[1]
		endif else (*pl)[2]++
		v = socket_command_get( ps, 'unit', class='position.dim', n_chips=3, chip=-1, error=err)
		if (err eq 0) and (n_elements(v) ge 2) then begin
			b = [byte(v[0]),0B]
			n = n_elements(b) < n_elements((*pm).scan.origin.bXunit)
			(*pm).scan.origin.bXunit[0:n-1] = b[0:n-1]
			(*pm).scan.pitch.bXunit[0:n-1] = b[0:n-1]
			b = [byte(v[1]),0B]
			n = n_elements(b) < n_elements((*pm).scan.origin.bYunit)
			(*pm).scan.origin.bYunit[0:n-1] = b[0:n-1]
			(*pm).scan.pitch.bYunit[0:n-1] = b[0:n-1]
		endif else (*pl)[2]++

		v = socket_command_get( ps, 'pitch', class='pixel.dim', n_chips=3, chip=-1, error=err)
		if (err eq 0) and (n_elements(v) ge 2) then begin
			(*pm).scan.pitch.X = v[0]
			(*pm).scan.pitch.Y = v[1]
		endif else (*pl)[2]++
;		v = socket_command_get( ps, 'unit', class='position.dim', n_chips=3, chip=-1, error=err)
;		if (err eq 0) and (n_elements(v) ge 2) then begin
;			b = [byte(v[0]),0B]
;			n = n_elements(b) < n_elements((*pm).scan.pitch.bXunit)
;			(*pm).scan.pitch.bXunit[0:n-1] = b[0:n-1]
;			b = [byte(v[1]),0B]
;			n = n_elements(b) < n_elements((*pm).scan.pitch.bYunit)
;			(*pm).scan.pitch.bYunit[0:n-1] = b[0:n-1]
;		endif else (*pl)[2]++
	endif 

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 2.

; Update pileup limits table ...

	v = socket_command_get( ps, 'trange', class='pileup.energy', chip=-1, n_chips=n_elements((*pm).pileup.limits.low), channel=0, error=err)
	if (err eq 0) and (n_elements((*pm).pileup.limits.low) eq n_elements(v)) then begin
		(*pm).pileup.limits.low = v
	endif else (*pl)[2]++
	v = socket_command_get( ps, 'trange', class='pileup.energy', chip=-1, n_chips=n_elements((*pm).pileup.limits.high), channel=1, error=err)
	if (err eq 0) and (n_elements((*pm).pileup.limits.high) eq n_elements(v)) then begin
		(*pm).pileup.limits.high = v
	endif else (*pl)[2]++

	t = systime(/seconds)
	dt = t-tin -3*2.
	(*pf)[3] = 100.*dt/(t-tlast) > 0.1
	(*pf)[5] = t-tlast
	tlast = t
	if (*pl)[2] gt errors_last then begin
		log_message, comms, type='WARNING', 'maia_client_parameters_slow, more Kandinski read errors: '+ str_tidy( (*pl)[2])
		gprint,'Maia_parameters_slow, more errors ...', (*pl)[2]
		wait, 10
	endif
	errors_last = (*pl)[2]
	goto, more

;-------------------------------------------------------------------------------------------

done:
	if (*ppar)[2] eq 1 then begin						; reset maia client
		log_message, comms, type='WARNING', 'maia_client_parameters_slow, Reset Parameters maia-client.'
		if debug then gprint,'Reset Parameters maia-client.'
		if ptr_valid(ps) then close_file,(*ps).unit		; close socket
		(*ppar)[2] = 0
		(*pf)[3] = 0.0
		(*pf)[4] = 0.0
		(*pl)[2] = 0
		goto, start
	endif
	if (*ppar)[3] eq 0 then goto, more

fin:													; kill maia client
	if ptr_valid(ps) then close_file,(*ps).unit			; close socket
	(*ppar)[6] = 0										; maia client is not running
	(*ppar)[4] = 1										; kill dependent client
	log_message, comms, type='INFO', 'maia_client_parameters_slow, Exit Maia parameters Kandinski-client.'
	if debug then begin
		gprint,'Exit Maia parameters slow Kandinski-client.'
		close_file, dun
	endif

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-start.

;	gprint,'Finish: free shared memory ...'
;	shared_memory_unmap, prefix=prefix
	return

bad_open:
	warning,'maia_client_parameters_slow','error opening socket'
	goto, fin
bad_shrmem:
	warning,'maia_client_parameters_slow',' error shrmem allocate'
	return
bad_args:
	warning,'maia_client_parameters_slow',' bad passed arguments. "argc" not 3.'
	return
end

