pro daq_client_parameters

; Read Klee variables and copy into parameter struct in shared memory.
; Fast update of parameters to shared memory --> read by "daq_update_parameters2.pro"
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
	       warning,'daq_client_parameters',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return
	    endif
	endif

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
; Klee client also loads routines from DAQ_Control.sav and all
; device object SAV files, which includes DAQ.

  startupp, /daq

@daq_scratch.def

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
;	warning,'daq_client_parameters',['arguments:',daq_prefix,conf_file]

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

	prefix = daq_prefix + 'pars_'
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
	username = s.user_name

	define_devices
	daq = daq_defaults(source='daq_client_parameters', conf=conf_file)			; read "daq.conf" default PVs
	debug = daq.daq.debug

	if debug then begin
		lfile = extract_path(conf_file) + username + '_' + prefix + 'debug.log'
		print,'Open DAQ Client parameters debug file: '+lfile
		openw, dun, lfile, /get_lun
		gprint, active=1
		gprint, out=dun, 'DAQ Client parameters debug log'
	endif

;	par array:	0
;				1
;				2	reset blog client (and shows status of reset)
;				3	kill blog client (from elsewhere to blog client here)
;				4	kill dependent client (to elsewhere if blog client is going down)
;				5	blog client is up and running
;			  6-15	free
;
;	  incoming (from daq_launch ...):     reset kill
;	  outgoing (to daq_launch ...):                 kill running
;	                   0           1         2    3    4    5
;.....................................................................................

	timeout_retry = 0
	first = 1

;	pf	0	% busy (here)		pl	0	daq port			pb	*	daq ip
;		1	% buffers lost 			1	errors
;		2	t_interval

start:
	pf = pfloat
	pb = pbyte
	pl = plong
	pm = pdat
	data = (*pm).layout_data			; from (*play).data
	daq_ip = string( *pb)
	daq_port = (*pl)[0]
	token_daq = '0'
	(*pl)[1] = 0

	wait, 1
	sock = open_socket( ip=daq_ip, port=daq_port, token=token_daq, error=error, $
					read_timeout=10, retries=0, client=clientname )
	if error ne 0 then goto, bad_open
	ps = ptr_new(sock)
	(*ps).n_detectors = n_detectors

again:
	start_time = systime(/seconds)
	tlast = start_time
	errors_last = 0
	(*ppar)[5] = 1						; daq client is running

;	Start loop, detect (*ppar)[3] = 1 to indicate a remote kill command from client, etc.
;	Detect (*ppar)[2] = 1 to indicate a reset command

more:
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	tin = systime(/seconds)

;-------------------------------------------------------------------------------------------
;	Now read parameters. Note that socket_command_get will do a socket retry if
;	there is a transmission error or lost connction.

; This (repeated) block of parameters are linked to daq_update_parameters2, for fast update ...

	v  = socket_command_get( ps, 'rate', class='event', error=err)
	if err eq 0 then begin
		(*pm).control.status.event_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'discard.rate', class='event', error=err)
	if err eq 0 then begin
		(*pm).control.status.discard_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'discard', class='blog', /quiet, error=err)
	if err eq 0 then begin
		(*pm).control.status.discard = v[0]
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

	if (*pm).version.software gt 6260 then begin
		v1  = socket_command_get( ps, 'position', class='position.dim', chip=-1, n_chip=6, error=err)
		if err eq 0 then begin
			(*pm).run.position = v1
		endif
	endif else begin
		v1  = socket_command_get( ps, 'position', class='deflect.axis', chip=-1, n_chip=2, error=err)
		if err eq 0 then begin
			v2  = socket_command_get( ps, 'position', class='stage.axis', chip=-1, n_chip=3, error=err)
			if err eq 0 then begin
				(*pm).run.position = [v1,v2]
			endif
		endif
	endelse
	if (*pm).version.software gt 6880 then begin
		v1  = socket_command_get( ps, 'coord', class='pixel.dim', chip=-1, n_chip=6, error=err)
		if err eq 0 then begin
			(*pm).run.pixel = v1
		endif
;		v1  = socket_command_get( ps, 'velocity', class='stage.axis', chip=-1, n_chip=4, error=err)
		v1  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_velocity, /string, error=err)
		if err eq 0 then begin
			velocity = unstringify(v1[0])
			if n_elements(velocity) lt 4 then velocity = [2.,2.,2.,2.]
			(*pm).stage.velocity = velocity
		endif
	endif 
	
; This block of parameters are linked to daq_update_parameters, for update every few seconds ...

	v  = socket_command_get( ps, 'connected', class='blog', error=err)
	if err eq 0 then begin
		(*pm).control.status.blog = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'uptime', class='main', error=err)
	if err eq 0 then begin
		(*pm).control.status.main_uptime = v[0]
	endif else (*pl)[1]++

	; Update temp monitor ----------------------------------------

	v = socket_command_get( ps, 'temp', class='hymod.cpu', error=err)
	if err eq 0 then begin
		(*pm).control.temp.cpu = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'temp', class='hymod.fpga', error=err)
	if err eq 0 then begin
		(*pm).control.temp.fpga = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'temp', class='hymod.board', error=err)
	if err eq 0 then begin
		(*pm).control.temp.board = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'temp', class='hymod.phy_eth0', error=err)
	if err eq 0 then begin
		(*pm).control.temp.eth0 = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'temp', class='hymod.phy_eth1', error=err)
	if err eq 0 then begin
		(*pm).control.temp.eth1 = v[0]
	endif else (*pl)[1]++

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 0.5

	; Update local copy of all Scepter parameters ----------------------------------------

	v = socket_command_get( ps, 'TDM', class='scepter', error=err)
	if (err eq 0) then begin
		(*pm).channel.scepter.tdm = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'TDS', class='scepter', error=err)
	if (err eq 0) then begin
		(*pm).channel.scepter.tds = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'TOS', class='scepter', error=err)
	if (err eq 0) then begin
		(*pm).channel.scepter.tos = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'THRESH', class='scepter', error=err)
	if (err eq 0) then begin
		(*pm).channel.scepter.thresh = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'TRK', class='scepter', error=err)
	if (err eq 0) then begin
		(*pm).channel.scepter.trk = v[0]
	endif else (*pl)[1]++

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 0.5

; This (repeated) block of parameters are lnked to daq_update_parameters2, for fast update ...

	v  = socket_command_get( ps, 'rate', class='event', error=err)
	if err eq 0 then begin
		(*pm).control.status.event_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'discard.rate', class='event', error=err)
	if err eq 0 then begin
		(*pm).control.status.discard_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'discard', class='blog', /quiet, error=err)
	if err eq 0 then begin
		(*pm).control.status.discard = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'rate', class='photon', error=err)
	if err eq 0 then begin
		(*pm).control.status.photon_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'rate', class='charge', error=err)
	if err eq 0 then begin
		(*pm).control.status.charge_rate = v[0]
	endif else (*pl)[1]++

	if (*pm).version.software gt 6260 then begin
		v1  = socket_command_get( ps, 'position', class='position.dim', chip=-1, n_chip=6, error=err)
		if err eq 0 then begin
			(*pm).run.position = v1
		endif
	endif else begin
		v1  = socket_command_get( ps, 'position', class='deflect.axis', chip=-1, n_chip=2, error=err)
		if err eq 0 then begin
			v2  = socket_command_get( ps, 'position', class='stage.axis', chip=-1, n_chip=3, error=err)
			if err eq 0 then begin
				(*pm).run.position = [v1,v2]
			endif
		endif
	endelse
	if (*pm).version.software gt 6880 then begin
		v1  = socket_command_get( ps, 'coord', class='pixel.dim', chip=-1, n_chip=6, error=err)
		if err eq 0 then begin
			(*pm).run.pixel = v1
		endif
;		v1  = socket_command_get( ps, 'velocity', class='stage.axis', chip=-1, n_chip=4, error=err)
		v1  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_velocity, /string, error=err)
		if err eq 0 then begin
			velocity = unstringify(v1[0])
			if n_elements(velocity) lt 4 then velocity = [2.,2.,2.,2.]
			(*pm).stage.velocity = velocity
		endif
	endif 


; More Scepter parameters ...

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 0.5

	v = socket_command_get( ps, 'TRKE', class='scepter', error=err)
	if (err eq 0) then begin
		(*pm).channel.scepter.trke = v[0]
	endif else (*pl)[1]++

	if (*pm).version.scepter ge 7 then begin
		v = socket_command_get( ps, 'THPD', class='scepter' , error=err)
		if (err eq 0) then begin
			(*pm).channel.scepter.thpd = abs(v[0])
		endif else (*pl)[1]++
	endif

	if (*pm).version.scepter ge 6 then begin
		v = socket_command_get( ps, 'TRIM', class='scepter', channel=-1, n_channels=32, error=err)
		if (err eq 0) and (n_elements(v) eq 32) then begin
			v2 = v[0:31]
			(*pm).channel[0:31].scepter.trim = abs(v2[data[0:31].index])
		endif else (*pl)[1]++

		v = socket_command_get( ps, 'FILT', class='scepter',  error=err)
		if (err eq 0) then begin
			(*pm).channel.scepter.filt = v[0]
		endif else (*pl)[1]++

		v = socket_command_get( ps, 'TCM', class='scepter',  error=err)
		if (err eq 0) then begin
			(*pm).channel.scepter.tcm = v[0]
		endif else (*pl)[1]++
	endif

; Update local copy of certain global parameters ----------------------------------------

	v = socket_command_get( ps, 'clock.rate', class='scepter', error=err)
	if (err eq 0) then begin
		(*pm).channel.scepter.clock = v[0] * 1.0e-6
	endif else (*pl)[1]++

; Update enables

	v = socket_command_get(  ps, 'enable', class='synth', error=err)
	if (err eq 0) then begin
		(*pm).enable.synth = v[0]
	endif else (*pl)[1]++

;	v = socket_command_get(  ps, 'enable', class='pulser', error=err)
;	if (err eq 0) then begin
;		(*pm).enable.pulser = v[0]
;	endif else (*pl)[1]++

	v = socket_command_get( ps, 'LOCK', class='scepter',  error=err)
	if (err eq 0) then begin
		(*pm).enable.LOCK = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'enable', class='event.blog', error=err)
	if (err eq 0) then begin
		(*pm).readout.event = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='photon', error=err)
	if (err eq 0) then begin
		(*pm).readout.photon = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='activity.blog', error=err)
	if (err eq 0) then begin
		(*pm).readout.activity = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='scepter', error=err)
	if (err eq 0) then begin
		(*pm).readout.scepter = v[0]
	endif else (*pl)[1]++

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
	wait, 0.5

	v = socket_command_get( ps, 'enable', class='scepter.clock', error=err)
	if (err eq 0) then begin
		(*pm).readout.quad_enable[0] = v[0]
	endif else (*pl)[1]++

	v = socket_command_get( ps, 'enable', class='pixel', error=err)
	if (err eq 0) then begin
		(*pm).enable.pixel = v[0]
	endif else (*pl)[1]++

;	v = socket_command_get( ps, 'enable', class='da', error=err)
;	if (err eq 0) then begin
;		(*pm).da.on = v[0]
;	endif else (*pl)[1]++
;
;	if (*pm).number.roi gt 0 then begin
;		v = socket_command_get( ps, 'enable', class='roi', error=err)
;		if (err eq 0) then begin
;			(*pm).enable.roi = v[0]
;		endif else (*pl)[1]++
;	endif

	v = socket_command_get( ps, 'enable', class='photon.chan', chip=-1, n_chips=n_detectors, error=err)
	if (err eq 0) and (n_elements((*pm).enable.ech) eq n_elements(v)) then begin
		(*pm).enable.ech = 1-v[data.index]
	endif else (*pl)[1]++

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

; This (repeated) block of parameters are lnked to daq_update_parameters2, for fast update ...

	v  = socket_command_get( ps, 'rate', class='event', error=err)
	if err eq 0 then begin
		(*pm).control.status.event_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'discard.rate', class='event', error=err)
	if err eq 0 then begin
		(*pm).control.status.discard_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'discard', class='blog', /quiet, error=err)
	if err eq 0 then begin
		(*pm).control.status.discard = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'rate', class='photon', error=err)
	if err eq 0 then begin
		(*pm).control.status.photon_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'rate', class='charge', error=err)
	if err eq 0 then begin
		(*pm).control.status.charge_rate = v[0]
	endif else (*pl)[1]++

	v  = socket_command_get( ps, 'rate', class='blog', /quiet, error=err)
	if err eq 0 then begin
		(*pm).control.status.blog_rate = v[0]
	endif else (*pl)[1]++

	if (*pm).version.software gt 6260 then begin
		v1  = socket_command_get( ps, 'position', class='position.dim', chip=-1, n_chip=6, error=err)
		if err eq 0 then begin
			(*pm).run.position = v1
		endif
	endif else begin
		v1  = socket_command_get( ps, 'position', class='deflect.axis', chip=-1, n_chip=2, error=err)
		if err eq 0 then begin
			v2  = socket_command_get( ps, 'position', class='stage.axis', chip=-1, n_chip=3, error=err)
			if err eq 0 then begin
				(*pm).run.position = [v1,v2]
			endif
		endif
	endelse
	if (*pm).version.software gt 6880 then begin
		v1  = socket_command_get( ps, 'coord', class='pixel.dim', chip=-1, n_chip=6, error=err)
		if err eq 0 then begin
			(*pm).run.pixel = v1
		endif
;		v1  = socket_command_get( ps, 'velocity', class='stage.axis', chip=-1, n_chip=4, error=err)
		v1  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_velocity, /string, error=err)
		if err eq 0 then begin
			velocity = unstringify(v1[0])
			if n_elements(velocity) lt 4 then velocity = [2.,2.,2.,2.]
			(*pm).stage.velocity = velocity
		endif
	endif 

; Update scan parameters (that were in parameters_slow) ...

	v = socket_command_get( ps, 'enable', class='pixel', error=err)
	if err eq 0 then begin
		(*pm).enable.pixel = v[0]
	endif else (*pl)[2]++

	if (*pm).enable.pixel then begin
		v = socket_command_get( ps, 'coord.extent', class='pixel.dim', n_chips=6, chip=-1, error=err)
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

		v = socket_command_get( ps, 'info', class='metadata.scan', /string, error=err)
		if err eq 0 then begin
			b = [byte(v[0]),0B]
			n = n_elements(b) < n_elements((*pm).scan.info)
			(*pm).scan.info = b[0:n-1]
		endif else (*pl)[2]++
	endif 

; Update info strings, stored as BYTE arrays in shared memory ...

	if (*pm).version.software ge 7000 then begin
;		if (*pm).number.roi gt 0 then begin
;			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_ROI, /string, error=err)
;			b = [byte(v[0]),0B]
;			n = n_elements(b) < n_elements((*pm).info.roi)
;			if err eq 0 then begin
;				(*pm).info.roi[0:n-1] = b[0:n-1]
;			endif else (*pl)[1]++
;		endif

;		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_deadtime, /string, error=err)
;		b = [byte(v[0]),0B]
;		n = n_elements(b) < n_elements((*pm).info.deadtime)
;		if err eq 0 then begin
;			(*pm).info.deadtime[0:n-1] = b[0:n-1]
;		endif else (*pl)[1]++

;		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_cal, /string, error=err)
;		b = [byte(v[0]),0B]
;		n = n_elements(b) < n_elements((*pm).info.cal)
;		if err eq 0 then begin
;			(*pm).info.cal[0:n-1] = b[0:n-1]
;		endif else (*pl)[1]++
;
;		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_gaintrim, /string, error=err)
;		b = [byte(v[0]),0B]
;		n = n_elements(b) < n_elements((*pm).info.gaintrim)
;		if err eq 0 then begin
;			(*pm).info.gaintrim[0:n-1] = b[0:n-1]
;		endif else (*pl)[1]++
;
;		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_linearise, /string, error=err)
;		b = [byte(v[0]),0B]
;		n = n_elements(b) < n_elements((*pm).info.linear)
;		if err eq 0 then begin
;			(*pm).info.linear[0:n-1] = b[0:n-1]
;		endif else (*pl)[1]++
;
;		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_pileup, /string, error=err)
;		b = [byte(v[0]),0B]
;		n = n_elements(b) < n_elements((*pm).info.pileup)
;		if err eq 0 then begin
;			(*pm).info.pileup[0:n-1] = b[0:n-1]
;		endif else (*pl)[1]++
	endif

;	DA info here, which shows current PV selection. The h/w FC0, FC1 ones are
;	read in 'maia_client_parameters_slow'.

;	if (*pm).version.software ge 4737 then begin
;		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_DA, /string, error=err)
;		b = [byte(v[0]),0B]
;		n = n_elements(b) < n_elements((*pm).info.da)
;		if err eq 0 then begin
;			(*pm).info.da[0:n-1] = b[0:n-1]
;		endif else (*pl)[1]++
;	endif

	t = systime(/seconds)
	dt = t-tin -6*0.5
	(*pf)[0] = 100.*dt/(t-tlast) > 0.1
	(*pf)[2] = t-tlast
	tlast = t
	if (*pl)[1] gt errors_last then begin
		gprint,'daq_parameters, more errors ...', (*pl)[1]
	endif
	errors_last = (*pl)[1]
	goto, more

;-------------------------------------------------------------------------------------------

done:
	if (*ppar)[2] eq 1 then begin						; reset daq client
		if debug then gprint,'Reset Parameters daq-client.'
		if ptr_valid(ps) then close_file,(*ps).unit		; close socket
		(*ppar)[2] = 0
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		(*pl)[1] = 0
		goto, start
	endif
	if (*ppar)[3] eq 0 then goto, more

fin:													; kill daq client
	if ptr_valid(ps) then close_file,(*ps).unit			; close socket
	(*ppar)[5] = 0										; daq client is not running
	(*ppar)[4] = 1										; kill dependent client
	if debug then begin
		gprint,'Exit DAQ parameters Klee-client.'
		close_file, dun
	endif

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-start.

;	gprint,'Finish: free shared memory ...'
;	shared_memory_unmap, prefix=prefix
	return

bad_open:
	warning,'daq_client_parameters','error opening socket'
	goto, fin
bad_shrmem:
	warning,'daq_client_parameters',' error shrmem allocate'
	return
bad_args:
	warning,'daq_client_parameters',' bad passed arguments. "argc" not 3.'
	return
end


