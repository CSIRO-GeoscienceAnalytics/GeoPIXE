pro blog_client_epics, args=sargs

; Reads Epics PV info in Monitor records to pick up flux changes.
;
; Uses shared memory for the DA imaging. Takes care not to play with flags used by DA2.
; Uses a new flag ppar[6] to signify that Epics client is up, rather than ppar[5] used in DA2.
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
	       warning,'blog_client_epics',['IDL run-time error caught.', '', $
	          'Fatal Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c,'Terminate process.'], /error
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

;	warning,'blog_client_epics',['Prefix = '+maia_prefix, 'Conf = '+conf_file, 'Server = '+server,  $
;					'Debug = '+str_tidy(debug)]
	comms = 0
	if server ne '' then begin
		if strlen(server) gt 3 then begin
			comms = open_comms( server=server, source='blog_client_epics', error=err)
			if err eq 0 then begin
				save_comms_in_common, comms		; save comms for implicit 'log_message' in 'warning', 'alarm_popup'
				log_message, comms, type='INFO', 'blog_client_epics started, with logging using server = '+server
			endif
;			warning,'blog_client_epics',['arguments:',maia_prefix,conf_file,server]
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

;	This uses the DA shared memory, but does not set any flow control
;	parameters used in the DA2 client. It uses an extra ppar[6] par
;	to flag that this process is running.

; If the n_buffers or buffer_size pars are changed, reboot.
; Or, uncomment the memory unmapping commands in fin:
; Then shutdown all referencing processes.
; Make them the large sizes BEFORE building a version for Linux.

	prefix = maia_prefix + 'da_'

	psh = shared_memory_buffers( prefix=prefix, error=error, n_buffers=n_buffers, /floatd, n_float=50, n_long=300)
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

	s = get_login_info()
	clientname = s.machine_name + ':' + prefix
	username = s.user_name
	log_message, comms, type='INFO', 'blog_client_epics, clientname='+clientname+', username='+username

	define_devices
	maia = maia_defaults(source='blog_client_epics', conf=conf_file)		; read "Maia.conf" default PVs
	if debug eq -1 then debug = maia.blog.debug

	maia_energy_pv = maia.epics.energy
	if maia_energy_pv eq '' then maia_energy_pv='ENERGY'

	if debug then begin
		lfile = extract_path(conf_file) + username + '_' + prefix + 'debug.log'
;		warning,'blog_client_epics',['Lfile = '+lfile]
		openw, dun, lfile, /get_lun
		gprint, active=1
		gprint, out=dun, 'Blog Client Epics debug log'
	endif

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
;	  outgoing (to maia_launch ...):                 kill     running      buffer_size
;	                   0           1         2    3    4    5    6    7  ...   13

	timeout_retry = 0
	tseq_last = 0L
	tseq_first = 0L
	first = 1

	sendnext = 7US
	monitor = 26US
	request = {aa:'AA'xub, tag:sendnext, bb:'BB'xub, len:2US, desired:[monitor] }

	header = {aa:0B, tag:0US, bb:0B, len:0US, prevlen:0US, $
			seq:0UL, tseq:0UL, tv_sec:0UL, tv_usec:0UL, $
			client:0UL, unused:0UL}

	linux_request = request
	swap_bytes, linux_request, /big_endian_data

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
;		5							5	NN index
;		6							6	total used pixels
;		7	energy (from Epics)		7	max X
;		8	total flux				8	max Y
;		9							9	activate blog_da_file
;		10+	scale					10+	maia_IC_name (as bytes)

start:
	pb = pbyte[0]
	pl = plong[0]
	blog_server_ip = string( *pb)
	blog_port = (*pl)[0]

	wait, 1
	sock = open_socket( ip=blog_server_ip, port=blog_port, error=error, $
							read_timeout=3, retries=0, client=clientname )
	if error then goto, bad_open
	ps = ptr_new(sock)

	pd = pdat[0]
	pf = pfloat[0]
	tseq_lost = 0L
	loglast_tin = 0.

	on_ioerror, bad_open2
	writeu, (*ps).unit, set_clientname
	if debug then gprint,'blog_client_epics',' write hostname succeeded'

again:
	start_time = systime(/seconds)
	tlast = start_time
	(*ppar)[6] = 1						; blog client is running (note non-standard index)

;	Start loop, detect (*ppar)[3] = 1 to indicate a remote
;	kill command from client, etc.
;	Detect (*ppar)[2] = 1 to indicate a reset command
;	Use pvalid shared memory to indicate a valid/available buffer

more:
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

;**------------------------------------------------------------------------------------------**
;	Note from request write must complete both header and payload reads without interruption

	on_ioerror, bad_write
	writeu, (*ps).unit, linux_request
	if debug then gprint,'blog_client_epics',' write succeeded'

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
	tseq_lost = tseq_lost + ((( ((header.tseq - tseq_last) > 1) ) - 1) > 0)
	tseq_last = header.tseq

	payload = bytarr(header.len)

reread3:
	on_ioerror, bad_read3
	readu, (*ps).unit, payload

	tin = systime(/seconds)
;	swap_bytes, payload, /big_endian_data

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

;**------------------------------------------------------------------------------------------**

	maia_IC_name = string( byte( (*pl)[10:*] ))

	s = strsplit( string(payload), string([10B,13B]), /extract)
	ns = n_elements(s)

	if ns gt 0 then begin
		for i=0,ns-1 do begin
			s2 = strsplit( s[i], '	 ', /extract)
			ns2 = n_elements(s2)
			if (s2[0] eq maia_IC_name) and (ns2 ge 4) then begin
				(*pf)[3] = float2(s2[3])
			endif else if (locate(strlowcase(maia_energy_pv), strlowcase(s2[0])) ge 0) and (ns2 ge 4) then begin
				(*pf)[7] = float2(s2[3]) * 0.001
			endif
		endfor
	endif

;	Don't set *pf[0] and [1] here as they are used in DA2 ...

	t = systime(/seconds)
	percent = (tin gt tlast) ? 100. * (t-tin)/(tin-tlast) : 1.
;	(*pf)[0] = percent
	if debug then gprint,' Percent busy=', percent
	lost = 100. * tseq_lost/(tseq_last - tseq_first + 1)
;	(*pf)[1] = lost
	if debug then gprint,' Percent lost =', lost
	if ((tseq_last - tseq_first) gt 20) or ((tin-tlast) gt 20) then begin
		tseq_first = tseq_last
		tseq_lost = 0L
	endif
	tlast = tin
	goto, more

;-------------------------------------------------------------------------------------------

done:
	if (*ppar)[2] eq 1 then begin			; reset blog client
		log_message, comms, type='INFO', 'blog_client_epics, RESET, restart.'
		if debug then gprint,'Reset Epics blog-client.'
		if ptr_valid(ps) then close_file,(*ps).unit			; close socket
		; re-read conf file to get PV string list ?
		tseq_first = tseq_last
		tseq_lost = 0L
		goto, start
	endif
	if (*ppar)[3] eq 0 then goto, more

fin:
	log_message, comms, type='INFO', 'blog_client_epics, EXIT, close socket and exit.'
	if ptr_valid(ps) then close_file,(*ps).unit			; close socket
	if debug then begin
		gprint,'Exit Epics blog-client.'
		close_file, dun
	endif

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-boot.

;	shared_memory_unmap, prefix=prefix, n_buffers=n_buffers
	return

bad_open:
	warning,'blog_client_epics','error opening socket'
	goto, fin
bad_open2:
	warning,'blog_client_epics','error writing hostname to socket'
	goto, fin
bad_len:
	warning,'blog_client_epics','illegal very large "len" value'
	goto, fin
bad_shrmem:
	warning,'blog_client_epics',' error shrmem allocate'
	return
bad_args:
	warning,'blog_client_epics',' bad passed arguments. "argc" not 2.'
	return
	
bad_write:
	if debug then gprint,'blog_client_epics',' error writing request, re-open socket'
	goto, retry
bad_read:
	if !error_state.sys_msg eq timeout_msg then begin		; could also use test on:
;		message, /reset										; !error_state.sys_code[0] eq 10060 instead?
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
;		log_message, comms, type='WARNING', 'blog_client_epics, timeout on header read, reread ..'
;		if debug then gprint,'blog_client_epics',' timeout on header read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		goto, reread
	endif
	warning,'blog_client_epics',[' error reading socket 1, re-open socket', $
			'!error_state.sys_msg returned "' + !error_state.sys_msg + '"'], /error
;	if debug then gprint,'blog_client_activity',' error reading socket 1, re-open socket'
	goto, retry
bad_read3:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		log_message, comms, type='WARNING', 'blog_client_epics, timeout on payload read, reread ..'
		if debug then gprint,'blog_client_epics',' timeout on payload read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		goto, reread3
	endif
	if debug then gprint,'blog_client_epics',' error reading socket 3, re-open socket'
	goto, retry
bad_string:
	warning,'blog_client_epics',' bad string record length'
	goto, retry

retry:
	if debug then gprint,'retry socket open ...'
	socket_retry, ps, error=error
	if error then begin
		warning,'blog_epics_activity',['error in Socket retry, exit ...','Client = '+clientname]
		goto, fin
	endif
	goto, again
end

