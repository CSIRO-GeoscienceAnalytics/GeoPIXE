pro maia_client_parameters, args=sargs

; Read Kandinski variables and copy into parameter struct in shared memory.
; To be read by "maia_update_parameters".
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
	       warning,'maia_client_parameters',['IDL fatal error caught.', '', $
	          'Fatal Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c,'Terminate process.'], /error
	       MESSAGE, /RESET
	       return
	    endif
	endif

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

@maia_scratch.def

	comms = 0
	if server ne '' then begin
		if strlen(server) gt 3 then begin
			comms = open_comms( server=server, source='maia_client_parameters', error=err)
			if err eq 0 then begin
				save_comms_in_common, comms		; save comms for implicit 'log_message' in 'warning', 'alarm_popup'
				log_message, comms, type='INFO', 'maia_client_parameters started, with logging using server = '+server
			endif
;			warning,'maia_client_parameters',['arguments:',maia_prefix,conf_file,server]
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

	prefix = maia_prefix + 'pars_'
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
	log_message, comms, type='INFO', 'maia_client_parameters, clientname='+clientname+', username='+username

	define_devices
	maia = maia_defaults(source='maia_client_parameters', conf=conf_file)			; read "Maia.conf" default PVs
	if debug eq -1 then debug = maia.blog.debug

	if debug then begin
		lfile = extract_path(conf_file) + username + '_' + prefix + 'debug.log'
;		print,'Open Maia Client parameters debug file: '+lfile
		openw, dun, lfile, /get_lun
		gprint, active=1
		gprint, out=dun, 'Maia Client parameters debug log'
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
;	  outgoing (to maia_launch ...):                 kill running
;	                   0           1         2    3    4    5
;.....................................................................................

	timeout_retry = 0
	first = 1

;	pf	0	% busy (here)		pl	0	maia port			pb	*	maia ip
;		1	% buffers lost 			1	errors
;		2	t_interval

start:
	pf = pfloat
	pb = pbyte
	pl = plong
	pm = pdat
	data = (*pm).layout_data			; from (*play).data
	maia_ip = string( *pb)
	maia_port = (*pl)[0]
	token_maia = '0'
	(*pl)[1] = 0

	wait, 1
	sock = open_socket( ip=maia_ip, port=maia_port, token=token_maia, error=error, $
					read_timeout=3, retries=0, client=clientname )
	if error ne 0 then goto, bad_open
	ps = ptr_new(sock)
	(*ps).n_detectors = n_detectors

again:
	start_time = systime(/seconds)
	tlast = start_time
	errors_last = 0
	loglast_tin = 0.
	(*ppar)[5] = 1						; maia client is running

;	Start loop, detect (*ppar)[3] = 1 to indicate a remote kill command from client, etc.
;	Detect (*ppar)[2] = 1 to indicate a reset command

more:
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	tin = systime(/seconds)

	v  = socket_command_get( ps, 'identity', class='config.dam', /string, error=err)
	if err eq 0 then begin
		b = [byte(v),0B]
		n = n_elements(b) < n_elements((*pm).identity.dam)
		(*pm).identity.dam[0:n-1] = b[0:n-1]
	endif else (*pl)[1]++

	ver = socket_command_get( ps, 'version', class='config.ps', /long, error=error)
	if error eq 0 then (*pm).version.software = ver[0]
	
	if (*pm).version.software lt 4737 then begin
		warning,'maia_client_parameters',['Kandinski version [' +str_tidy((*pm).version.software)+ '] is too old.','Missing "scratch.datum" variables will cause problems.']
	endif

	v  = socket_command_get( ps, 'link', class='status', error=err)
	if err eq 0 then begin
		(*pm).control.status.link = v[0]
	endif else (*pl)[1]++
	socket_command_mode, ddm_down = 1 - clip((*pm).control.status.link, 0,1)

;	File-system size and available logging time ...

	if ((*pm).version.software ge 7021) and ((*pm).control.status.blog eq 1) then begin
		v1  = socket_command_get( ps, 'fs.size', class='status.blog', /UL64, error=err)
		if err eq 0 then begin
			(*pm).control.status.fs_size = v1
		endif else (*pl)[1]++
		v1  = socket_command_get( ps, 'fs.free', class='status.blog', /UL64, error=err)
		if err eq 0 then begin
			(*pm).control.status.fs_free = v1
		endif else (*pl)[1]++
		v1  = socket_command_get( ps, 'fs.time', class='status.blog', /long, error=err)
		if err eq 0 then begin
			(*pm).control.status.fs_time = v1
		endif else (*pl)[1]++
		v1  = socket_command_get( ps, 'error', class='status.blog', /string, error=err)
		if err eq 0 then begin
			b = [byte(v1),0B]
			n = n_elements(b) < n_elements((*pm).status.blog_error)
			(*pm).status.blog_error[0:n-1] = b[0:n-1]
		endif else (*pl)[1]++
	endif 

;-------------------------------------------------------------------------------------------
;	Now read parameters. Note that socket_command_get will do a socket retry if
;	there is a transmission error or lost connction.

; This (repeated) block of parameters are linked to maia_update_parameters2, for fast update ...

	if (*pm).control.status.link then begin
	
		v  = socket_command_get( ps, 'link.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.link_rate = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'event.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.event_rate = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'event.discard.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.discard_rate = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'bpinterlock', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.interlock = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'peltier.current', class='detector', error=err)
		if err eq 0 then begin
			(*pm).control.peltier = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'bias.voltage', class='detector', error=err)
		if err eq 0 then begin
			(*pm).control.bias = v[0]
		endif else (*pl)[1]++

	endif

; This block of parameters are linked to maia_update_parameters, for update every few seconds ...

	v  = socket_command_get( ps, 'link', class='status', error=err)
	if err eq 0 then begin
		(*pm).control.status.link = v[0]
	endif else (*pl)[1]++
	socket_command_mode, ddm_down = 1 - clip((*pm).control.status.link, 0,1)

	v  = socket_command_get( ps, 'blog.connected', class='status', error=err)
	if err eq 0 then begin
		(*pm).control.status.blog = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'link.uptime', class='status', error=err)
	if err eq 0 then begin
		(*pm).control.status.link_uptime = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'link.erate', class='status', channel=-1, n_channel=4, error=err)
	if err eq 0 then begin
		(*pm).control.status.link_erate = v
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'main.uptime', class='status', error=err)
	if err eq 0 then begin
		(*pm).control.status.main_uptime = v[0]
	endif else (*pl)[1]++

	; Update detector monitor ----------------------------------------

	if (*pm).control.status.link then begin
	
		v  = socket_command_get( ps, 'bias.voltage', class='status.detector', error=err)
		if err eq 0 then begin
			(*pm).control.bias_monitor = v[0]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'bias.current', class='status.detector', error=err)
		if err eq 0 then begin
			(*pm).control.leakage = v[0]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'temp', class='status.detector', error=err)
		if err eq 0 then begin
			(*pm).control.temp.detector = v[0]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'peltier.current', class='status.detector', error=err)
		if err eq 0 then begin
			(*pm).control.peltier_monitor = v[0]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'peltier.supply.voltage', class='status.detector', error=err)
		if err eq 0 then begin
			(*pm).control.peltier_supply = v[0]
		endif else (*pl)[1]++
	
		if (*pm).version.dbpm ge 2 then begin
			was = (*pm).control.status.bake
			v = socket_command_get( ps, 'bake.enable', class='status.detector', error=err)
			if err eq 0 then begin
				(*pm).control.status.bake = v[0]
			endif else (*pl)[1]++
		endif
	
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		wait, 0.5
	
		if ((*pm).n_detectors ne 96) or ((*pm).version.dam ge 3) then begin
			v  = socket_command_get( ps, 'guard.voltage', class='detector', error=err)
			if err eq 0 then begin
				(*pm).control.guard = v[0]
			endif else (*pl)[1]++
	
			v = socket_command_get( ps, 'temp', class='status.hermes', error=err)
			if err eq 0 then begin
				(*pm).control.temp.hermes = v[0]
			endif else (*pl)[1]++
	
			v = socket_command_get( ps, 'temp', class='status.water', error=err)
			if err eq 0 then begin
				(*pm).control.temp.water = v[0]
			endif else (*pl)[1]++
	
			v = socket_command_get( ps, 'temp', class='status.coldtrap', error=err)
			if err eq 0 then begin
				(*pm).control.temp.coldtrap = v[0]
			endif else (*pl)[1]++
	
			v = socket_command_get( ps, 'oan', class='status.hermes', error=err)
			if err eq 0 then begin
				(*pm).control.oan = v
			endif else (*pl)[1]++

			v = socket_command_get( ps, 'aux', class='status.scepter', error=err)
			if err eq 0 then begin
				(*pm).control.aux = v
			endif else (*pl)[1]++

			v = socket_command_get( ps, 'monitor.voltage', class='status.dam', error=err)
			if err eq 0 then begin
				(*pm).control.monitor = v
			endif else (*pl)[1]++
		endif
	
		v = socket_command_get( ps, 'temp', class='status.mosfet', error=err)
		if err eq 0 then begin
			(*pm).control.temp.mosfet = v[0]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'temp', class='status.fpga', error=err)
		if err eq 0 then begin
			(*pm).control.temp.fpga = v[0]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'temp', class='status.hymod.fpga', error=err)
		if err eq 0 then begin
			(*pm).control.temp.hymod_fpga = v[0]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'temp', class='status.hymod.cpu', error=err)
		if err eq 0 then begin
			(*pm).control.temp.hymod_cpu = v[0]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'vcc', class='status.ddm', error=err)
		if err eq 0 then begin
			(*pm).control.vcc = v[0]
		endif else (*pl)[1]++
	endif
	
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 0.5

; This (repeated) block of parameters are lnked to maia_update_parameters2, for fast update ...

	if (*pm).control.status.link then begin
	
		v  = socket_command_get( ps, 'link.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.link_rate = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'event.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.event_rate = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'event.discard.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.discard_rate = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'bpinterlock', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.interlock = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'peltier.current', class='detector', error=err)
		if err eq 0 then begin
			(*pm).control.peltier = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'bias.voltage', class='detector', error=err)
		if err eq 0 then begin
			(*pm).control.bias = v[0]
		endif else (*pl)[1]++
	endif
	
;	v  = socket_command_get( ps, 'pixel', channel=-1, n_channel=3, error=err)
	v  = socket_command_get( ps, 'coord', class='pixel.dim', chip=-1, n_chips=3, error=err)
	if err eq 0 then begin
		(*pm).run.X = v[0]
		(*pm).run.Y = v[1]
		(*pm).run.Z = v[2]
	endif else (*pl)[1]++

	; Update local copy of all Hermes, Scepter parameters ----------------------------------------

	if (*pm).control.status.link then begin
	
		v = socket_command_get( ps, 'GAIN', class='hermes', chip=-1 , error=err)
		if (err eq 0) and (n_elements((*pm).channel.hermes.gain) eq n_elements(v[data.hermes])) then begin
			(*pm).channel.hermes.gain = v[data.hermes]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'TIME', class='hermes', chip=-1 , error=err)
		if (err eq 0) and (n_elements((*pm).channel.hermes.time) eq n_elements(v[data.hermes])) then begin
			(*pm).channel.hermes.time = v[data.hermes]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'EBLK', class='hermes', chip=-1 , error=err)
		if (err eq 0) and (n_elements((*pm).channel.hermes.eblk) eq n_elements(v[data.hermes])) then begin
			(*pm).channel.hermes.eblk = v[data.hermes]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'ELK', class='hermes', chip=-1 , channel=-1, error=err)
		if (err eq 0) and (n_elements((*pm).channel.hermes.elk) eq n_elements(v[data.hermes])) then begin
			(*pm).channel.hermes.elk = v[data.index]
		endif else (*pl)[1]++
	
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		wait, 0.5
	
		v = socket_command_get( ps, 'TDM', class='scepter', chip=-1 , error=err)
		if (err eq 0) and (n_elements((*pm).channel.scepter.tdm) eq n_elements(v[data.hermes])) then begin
			(*pm).channel.scepter.tdm = v[data.hermes]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'TDS', class='scepter', chip=-1 , error=err)
		if (err eq 0) and (n_elements((*pm).channel.scepter.tds) eq n_elements(v[data.hermes])) then begin
			(*pm).channel.scepter.tds = v[data.hermes]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'TOS', class='scepter', chip=-1 , error=err)
		if (err eq 0) and (n_elements((*pm).channel.scepter.tos) eq n_elements(v[data.hermes])) then begin
			(*pm).channel.scepter.tos = v[data.hermes]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'THRESH', class='scepter', chip=-1 , error=err)
		if (err eq 0) and (n_elements((*pm).channel.scepter.thresh) eq n_elements(v[data.hermes])) then begin
			(*pm).channel.scepter.thresh = v[data.hermes]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'TRK', class='scepter', chip=-1 , error=err)
		if (err eq 0) and (n_elements((*pm).channel.scepter.trk) eq n_elements(v[data.hermes])) then begin
			(*pm).channel.scepter.trk = v[data.hermes]
		endif else (*pl)[1]++
	endif
	
	v = socket_command_get( ps, 'enable', class='linearise', error=err)
	if (err eq 0) then begin
		(*pm).enable.linear = v[0]
	endif else (*pl)[1]++
	if ((*pm).version.software ge 9542) then begin
		v2 = socket_command_get( ps, 'enable', class='linearise2', error=err2)
		if (err2 eq 0) and (err eq 0) then begin
			(*pm).enable.linear = v2[0] or v[0]
		endif else (*pl)[1]++
	endif

	v = socket_command_get( ps, 'enable', class='gaintrim', error=err)
	if (err eq 0) then begin
		(*pm).enable.gaintrim = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='pileup', error=err)
	if (err eq 0) then begin
		(*pm).enable.pileup = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='throttle', error=err)
	if (err eq 0) then begin
		(*pm).enable.throttle = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='pixel', error=err)
	if (err eq 0) then begin
		(*pm).enable.pixel = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='deadtime', error=err)
	if (err eq 0) then begin
		(*pm).enable.deadtime = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='da', error=err)
	if (err eq 0) then begin
		(*pm).da.on = v[0]
	endif else (*pl)[1]++

	if (*pm).number.roi gt 0 then begin
		v = socket_command_get( ps, 'enable', class='roi', error=err)
		if (err eq 0) then begin
			(*pm).enable.roi = v[0]
		endif else (*pl)[1]++
	endif

	v = socket_command_get( ps, 'enable', class='spectrum', chip=-1, n_chips=(*pm).number.spectra, error=err)
	if (err eq 0) then begin
		(*pm).enable.groups = v[0]
	endif else (*pl)[1]++
	
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 0.5

; This (repeated) block of parameters are lnked to maia_update_parameters2, for fast update ...

	if (*pm).control.status.link then begin
	
		v  = socket_command_get( ps, 'link.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.link_rate = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'event.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.event_rate = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'event.discard.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.discard_rate = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'bpinterlock', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.interlock = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'peltier.current', class='detector', error=err)
		if err eq 0 then begin
			(*pm).control.peltier = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'bias.voltage', class='detector', error=err)
		if err eq 0 then begin
			(*pm).control.bias = v[0]
		endif else (*pl)[1]++
	endif
	
; More Scepter parameters ...

	if (*pm).control.status.link then begin
	
		v = socket_command_get( ps, 'TRKE', class='scepter', chip=-1 , error=err)
		if (err eq 0) and (n_elements((*pm).channel.scepter.trke) eq n_elements(v[data.hermes])) then begin
			(*pm).channel.scepter.trke = v[data.hermes]
		endif else (*pl)[1]++
	
		if (*pm).version.scepter ge 7 then begin
			v = socket_command_get( ps, 'THPD', class='scepter', chip=-1 , error=err)
			if (err eq 0) and (n_elements((*pm).channel.scepter.thpd) eq n_elements(v[data.hermes])) then begin
				(*pm).channel.scepter.thpd = abs(v[data.hermes])
			endif else (*pl)[1]++
		endif
	
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		wait, 0.5
	
		if (*pm).version.scepter ge 6 then begin
			v = socket_command_get( ps, 'TRIM', class='scepter', chip=-1 , channel=-1, error=err)
			if (err eq 0) and (n_elements((*pm).channel.scepter.trim) eq n_elements(v[data.hermes])) then begin
				(*pm).channel.scepter.trim = abs(v[data.index])
			endif else (*pl)[1]++
	
			v = socket_command_get( ps, 'FILT', class='scepter', chip=-1 , error=err)
			if (err eq 0) and (n_elements((*pm).channel.scepter.filt) eq n_elements(v[data.hermes])) then begin
				(*pm).channel.scepter.filt = v[data.hermes]
			endif else (*pl)[1]++
	
			v = socket_command_get( ps, 'TCM', class='scepter', chip=-1 , error=err)
			if (err eq 0) and (n_elements((*pm).channel.scepter.tcm) eq n_elements(v[data.hermes])) then begin
				(*pm).channel.scepter.tcm = v[data.hermes]
			endif else (*pl)[1]++
		endif
	endif
	
; Update local copy of certain global parameters ----------------------------------------

	v = socket_command_get( ps, 'clock.rate', class='readout', error=err)
	if (err eq 0) then begin
		(*pm).channel.scepter.clock = v[0] * 1.0e-6
	endif else (*pl)[1]++

; Update enables

	v = socket_command_get(  ps, 'enable', class='synth', error=err)
	if (err eq 0) then begin
		(*pm).enable.synth = v[0]
	endif else (*pl)[1]++

	v = socket_command_get(  ps, 'enable', class='pulser', error=err)
	if (err eq 0) then begin
		(*pm).enable.pulser = v[0]
	endif else (*pl)[1]++

	if (*pm).control.status.link then begin
	
		v = socket_command_get( ps, 'EAN', class='hermes', chip=-1, channel=-1, error=err)
		if (err eq 0) and (n_elements((*pm).enable.EAN) eq n_elements(v)) then begin
			(*pm).enable.EAN = v[data.index]
		endif else (*pl)[1]++
	
		v = socket_command_get( ps, 'LOCK', class='scepter', chip=-1, error=err)
		if (err eq 0) and (n_elements((*pm).enable.LOCK) eq n_elements(v)) then begin
			(*pm).enable.LOCK = v
		endif else (*pl)[1]++
	endif
	
	v  = socket_command_get( ps, 'enable', class='event', error=err)
	if (err eq 0) then begin
		(*pm).readout.event = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='photon', error=err)
	if (err eq 0) then begin
		(*pm).readout.photon = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='accum', error=err)
	if (err eq 0) then begin
		(*pm).readout.accum = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='activity', error=err)
	if (err eq 0) then begin
		(*pm).readout.activity = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='scepter', error=err)
	if (err eq 0) then begin
		(*pm).readout.scepter = v[0]
	endif else (*pl)[1]++

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 0.5

	v = socket_command_get( ps, 'enable', class='readout.clock', chip=-1, n_chips=3, error=err)
	if (err eq 0) and (n_elements((*pm).readout.rr_enable) eq n_elements(v)) then begin
		(*pm).readout.rr_enable = v
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='readout.quad', chip=-1, n_chips=4, error=err)
	if (err eq 0) and (n_elements((*pm).readout.quad_enable) eq n_elements(v)) then begin
		(*pm).readout.quad_enable = v
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='linearise', error=err)
	if (err eq 0) then begin
		(*pm).enable.linear = v[0]
	endif else (*pl)[1]++
	if ((*pm).version.software ge 9542) then begin
		v2 = socket_command_get( ps, 'enable', class='linearise2', error=err2)
		if (err2 eq 0) and (err eq 0) then begin
			(*pm).enable.linear = v2[0] or v[0]
		endif else (*pl)[1]++
	endif

	v = socket_command_get( ps, 'enable', class='gaintrim', error=err)
	if (err eq 0) then begin
		(*pm).enable.gaintrim = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='pileup', error=err)
	if (err eq 0) then begin
		(*pm).enable.pileup = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='throttle', error=err)
	if (err eq 0) then begin
		(*pm).enable.throttle = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='pixel', error=err)
	if (err eq 0) then begin
		(*pm).enable.pixel = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='deadtime', error=err)
	if (err eq 0) then begin
		(*pm).enable.deadtime = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='da', error=err)
	if (err eq 0) then begin
		(*pm).da.on = v[0]
	endif else (*pl)[1]++

	if (*pm).number.roi gt 0 then begin
		v = socket_command_get( ps, 'enable', class='roi', error=err)
		if (err eq 0) then begin
			(*pm).enable.roi = v[0]
		endif else (*pl)[1]++
	endif

	v = socket_command_get( ps, 'enable', class='spectrum', chip=-1, n_chips=(*pm).number.spectra, error=err)
	if (err eq 0) then begin
		(*pm).enable.groups = v[0]
	endif else (*pl)[1]++

	if (*pm).control.status.link then begin
	
		v = socket_command_get( ps, 'ECH', class='hermes', chip=-1, channel=-1, error=err)
		if (err eq 0) and (n_elements((*pm).enable.ech) eq n_elements(v)) then begin
			(*pm).enable.ech = v[data.index]
		endif else (*pl)[1]++
	endif
	
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 0.5

; This (repeated) block of parameters are lnked to maia_update_parameters2, for fast update ...

	if (*pm).control.status.link then begin
	
		v  = socket_command_get( ps, 'link.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.link_rate = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'event.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.event_rate = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'event.discard.rate', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.status.discard_rate = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'bpinterlock', class='status', error=err)
		if err eq 0 then begin
			(*pm).control.interlock = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'peltier.current', class='detector', error=err)
		if err eq 0 then begin
			(*pm).control.peltier = v[0]
		endif else (*pl)[1]++
	
		v  = socket_command_get( ps, 'bias.voltage', class='detector', error=err)
		if err eq 0 then begin
			(*pm).control.bias = v[0]
		endif else (*pl)[1]++
	endif

;	v  = socket_command_get( ps, 'pixel', channel=-1, n_channel=3, error=err)
	v  = socket_command_get( ps, 'coord', class='pixel.dim', chip=-1, n_chips=3, error=err)
	if err eq 0 then begin
		(*pm).run.X = v[0]
		(*pm).run.Y = v[1]
		(*pm).run.Z = v[2]
	endif else (*pl)[1]++

; Update info strings, stored as BYTE arrays in shared memory ...

	if (*pm).version.software ge 4737 then begin
		if (*pm).number.roi gt 0 then begin
;			v  = socket_command_get( ps, 'info', class='roi', /string, error=err)
			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_ROI, /string, error=err)
			b = [byte(v[0]),0B]
			n = n_elements(b) < n_elements((*pm).info.roi)
			if err eq 0 then begin
				(*pm).info.roi[0:n-1] = b[0:n-1]
			endif else (*pl)[1]++
		endif

;		v  = socket_command_get( ps, 'info', class='deadtime', /string, error=err)
		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_deadtime, /string, error=err)
		b = [byte(v[0]),0B]
		n = n_elements(b) < n_elements((*pm).info.deadtime)
		if err eq 0 then begin
			(*pm).info.deadtime[0:n-1] = b[0:n-1]
		endif else (*pl)[1]++

;		v  = socket_command_get( ps, 'info', class='cal', /string, error=err)
		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_cal, /string, error=err)
		b = [byte(v[0]),0B]
		n = n_elements(b) < n_elements((*pm).info.cal)
		if err eq 0 then begin
			(*pm).info.cal[0:n-1] = b[0:n-1]
		endif else (*pl)[1]++

;		v  = socket_command_get( ps, 'info', class='gaintrim', /string, error=err)
		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_gaintrim, /string, error=err)
		b = [byte(v[0]),0B]
		n = n_elements(b) < n_elements((*pm).info.gaintrim)
		if err eq 0 then begin
			(*pm).info.gaintrim[0:n-1] = b[0:n-1]
		endif else (*pl)[1]++

;		v  = socket_command_get( ps, 'info', class='linearise', /string, error=err)
		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_linearise, /string, error=err)
		b = [byte(v[0]),0B]
		n = n_elements(b) < n_elements((*pm).info.linear)
		if err eq 0 then begin
			(*pm).info.linear[0:n-1] = b[0:n-1]
		endif else (*pl)[1]++

;		v  = socket_command_get( ps, 'info', class='pileup', /string, error=err)
		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_pileup, /string, error=err)
		b = [byte(v[0]),0B]
		n = n_elements(b) < n_elements((*pm).info.pileup)
		if err eq 0 then begin
			(*pm).info.pileup[0:n-1] = b[0:n-1]
		endif else (*pl)[1]++

;		v  = socket_command_get( ps, 'info', class='throttle', /string, error=err)
		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_throttle, /string, error=err)
		b = [byte(v[0]),0B]
		n = n_elements(b) < n_elements((*pm).info.throttle)
		if err eq 0 then begin
			(*pm).info.throttle[0:n-1] = b[0:n-1]
		endif else (*pl)[1]++
	endif

	if (*pm).version.software ge 4813 then begin
;		v  = socket_command_get( ps, 'info', class='group', /string, error=err)
		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_group, /string, error=err)
		b = [byte(v[0]),0B]
		n = n_elements(b) < n_elements((*pm).info.group)
		if err eq 0 then begin
			(*pm).info.group[0:n-1] = b[0:n-1]
		endif else (*pl)[1]++
	endif

;	DA info here, which shows current PV selection. The h/w FC0, FC1 ones are
;	read in 'maia_client_parameters_slow'.

	if (*pm).version.software ge 4737 then begin
;		v  = socket_command_get( ps, 'info', class='da', /string, error=err)
		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_DA, /string, error=err)
		b = [byte(v[0]),0B]
		n = n_elements(b) < n_elements((*pm).info.da)
		if err eq 0 then begin
			(*pm).info.da[0:n-1] = b[0:n-1]
		endif else (*pl)[1]++
	endif

	t = systime(/seconds)
	dt = t-tin -7*0.5
	(*pf)[0] = 100.*dt/(t-tlast) > 0.1
	(*pf)[2] = t-tlast
	tlast = t
	if (*pl)[1] gt errors_last then begin
		log_message, comms, type='WARNING', 'maia_client_parameters, more Kandinski read errors: '+ str_tidy( (*pl)[1])
		gprint,'Maia_parameters, more Kandinski read errors ...', (*pl)[1]
		wait, 10
	endif
	errors_last = (*pl)[1]
	goto, more

;-------------------------------------------------------------------------------------------

done:
	if (*ppar)[2] eq 1 then begin						; reset maia client
		log_message, comms, type='WARNING', 'maia_client_parameters, Reset Parameters maia-client.'
		if debug then gprint,'Reset Parameters maia-client.'
		if ptr_valid(ps) then close_file,(*ps).unit		; close socket
		(*ppar)[2] = 0
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		(*pl)[1] = 0
		goto, start
	endif
	if (*ppar)[3] eq 0 then goto, more

fin:													; kill maia client
	if ptr_valid(ps) then close_file,(*ps).unit			; close socket
	(*ppar)[5] = 0										; maia client is not running
	(*ppar)[4] = 1										; kill dependent client
	log_message, comms, type='INFO', 'maia_client_parameters, Exit Maia parameters Kandinski-client.'
	if debug then begin
		gprint,'Exit Maia parameters Kandinski-client.'
		close_file, dun
	endif

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-start.

;	gprint,'Finish: free shared memory ...'
;	shared_memory_unmap, prefix=prefix
	return

bad_open:
	warning,'maia_client_parameters',['error opening socket','Client = '+clientname]
	goto, fin
bad_shrmem:
	warning,'maia_client_parameters',['error shrmem allocate','Client = '+clientname]
	return
bad_args:
	warning,'maia_client_parameters',[' bad passed arguments. "argc" not 3.','Client = '+clientname]
	return
end
