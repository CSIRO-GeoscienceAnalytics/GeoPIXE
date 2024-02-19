pro blog_client_activity, args=sargs

; Read Blog Activity records and copy into an activity array in shared memory.
; Uses a pair of shared memory blocks (n_buffers=2). Buffer_size is a 2D array
; for [n_detectors,2].
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
	       warning,'blog_client_activity',['IDL run-time error caught.', '', $
	          'Fatal Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c,'Terminating process.'], /error
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

;.................................................................................................
; Blog client also loads routines from Maia_Control.sav and all
; device object SAV files, which includes Maia.

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
			comms = open_comms( server=server, source='blog_client_activity', error=err)
			if err eq 0 then begin
				save_comms_in_common, comms		; save comms for implicit 'log_message' in 'warning', 'alarm_popup'
				log_message, comms, type='INFO', 'blog_client_activity started, with logging using server = '+server
			endif
;			warning,'blog_client_activity',['arguments:',maia_prefix,conf_file,server]
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
;
; Shared memory areas referenced here:
;	1.	blog_client_activity		for activity rates by detector, group
;
;.....................................................................................

	prefix = maia_prefix + 'activity_'

	psh = shared_memory_buffers( prefix=prefix, error=error, n_buffers=n_buffers )
	if error then goto, bad_shrmem
	buffer_size = (*psh).buffer_size
	n_detectors = buffer_size[0]

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
	log_message, comms, type='INFO', 'blog_client_activity, clientname='+clientname+', username='+username

	define_devices
	maia = maia_defaults(source='blog_client_activity', conf=conf_file)			; read "Maia.conf" default PVs
	if debug eq -1 then debug = maia.blog.debug

	if debug then begin
		lfile = extract_path(conf_file) + username + '_' + prefix + 'debug.log'
		openw, dun, lfile, /get_lun
		gprint, active=1
		gprint, out=dun, 'Maia Blog Client Activity debug log'
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
;	  outgoing (to maia_launch ...):                 kill running          buffer_size
;	                   0           1         2    3    4    5    6    7  ...   13
;.....................................................................................

	timeout_retry = 0
	first = 1
	tseq_last = 0L
	tseq_first = 0L
	tlong6 = ulonarr(6)
	error_mask = 'FFFFFFF000000000'x		; error bits in "dwell/readout"
	error_offset = -40
	duration_mask = '0000000FFFFFFFFF'x		; 32 bits plus 8 bits of next 32-bit word
	group = ''
	project = ''
	group_next = ''
	project_next = ''

	activ = 39US				; Maia activity accumulator
	summary2 = 32US
	summary3 = 53US
	summary4 = 56US
	setgroup = 33US
	setproject = 51US
	sendnext = 7US
	sendprev = 23US
	request_any = {aa:'AA'xub, tag:sendnext, bb:'BB'xub, len:0US }
	request_activ = {aa:'AA'xub, tag:sendnext, bb:'BB'xub, len:2US, desired:activ }
	request_summary2 = {aa:'AA'xub, tag:sendprev, bb:'BB'xub, len:2US, desired:summary2 }
	request_summary3 = {aa:'AA'xub, tag:sendprev, bb:'BB'xub, len:2US, desired:summary3 }
	request_summary4 = {aa:'AA'xub, tag:sendprev, bb:'BB'xub, len:2US, desired:summary4 }
	request_setgroup = {aa:'AA'xub, tag:sendprev, bb:'BB'xub, len:2US, desired:setgroup }
	request_setproject = {aa:'AA'xub, tag:sendprev, bb:'BB'xub, len:2US, desired:setproject }

;	Loops on all these now. Later will NOT request setgroup and setproject ...
;	linux_request = [request_summary2,request_summary3,request_setgroup,request_setproject,request_activ]

	linux_request = [request_summary2, request_summary3, request_summary4, request_activ]
	name_request = ['summary2', 'summary3', 'summary4', 'active']
	swap_bytes, linux_request, /big_endian_data

	setname = 54US
	b = [byte(clientname),0B]
	nb = strlen(clientname) + 1
	set_clientname = {aa:'AA'xub, tag:setname, bb:'BB'xub, len:uint(nb), desired:b }
	swap_bytes, set_clientname, /big_endian_data

	header = {aa:0B, tag:0US, bb:0B, len:0US, prevlen:0US, $
			seq:0UL, tseq:0UL, tv_sec:0UL, tv_usec:0UL, client:0UL, unused:0UL}

	header2 = {X:0UL,Y:0UL,Z:0UL, trigger:0UL, duration:0ULL, flux1:0UL, flux2:0UL, count:0UL}

;	pf	0	% busy				pl	0	blog port			pb	*	blog server ip
;		1	% buffers lost			1	errors
;		2	t_interval				2	run number			pb1	*	group/project dir name (current)
;		3	flux0					3	segment #
;		4	flux1					4	run time (sec)
;									5	# blocks written
;									6	bytes/sec
;									7	discard mode

start:
	pf = pfloat[0]
	pb = pbyte[0]
	pb1 = pbyte[1]
	pl = plong[0]
	blog_server_ip = string( *pb)
	blog_port = (*pl)[0]
	k = -1

	wait, 1
	sock = open_socket( ip=blog_server_ip, port=blog_port, error=error, $
						read_timeout=3, retries=0, client=clientname )
	if error ne 0 then goto, bad_open
	ps = ptr_new(sock)
	tseq_lost = 0L
	loglast_tin = 0.
	if debug then gprint,'blog_client_activity',' socket open succeeded, clientname:' + clientname

again:
	start_time = systime(/seconds)
	tlast = start_time
	(*ppar)[5] = 1						; blog client is running

	on_ioerror, bad_open2
	writeu, (*ps).unit, set_clientname
	if debug then gprint,'blog_client_activity',' write hostname succeeded.'

;	Start loop, detect (*ppar)[3] = 1 to indicate a remote
;	kill command from client, etc.
;	Detect (*ppar)[2] = 1 to indicate a reset command

more:
	k = k+1
	if k ge n_elements(linux_request) then k=0
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

;-------------------------------------------------------------------------------------------
;	Note: from request write must complete both header and payload reads without interruption.
;		  Any format errors need to force a socket retry and start again.

	on_ioerror, bad_write
	writeu, (*ps).unit, linux_request[k]
	if debug then gprint,'blog_client_activity',' write request "'+name_request[k]+'" succeeded'

reread:
	on_ioerror, bad_read
	header.tag = 0 &  header.len = 0
	header.aa = 0  &  header.bb = 0
	
	readu, (*ps).unit, header
	swap_bytes, header, /big_endian_data

	if debug then begin
		gprint,'blog_client_activity',' read header succeeded'
		gprint, '	Tag=',header.tag,'   Len=',header.len
	endif
	l = (header.len)[0]
	
;	A bug in blogd means that testing for "len" zero and looping does not work.
;	With bug fixed, only test for AA, BB here, and reinstate len=0 test below to loop.

;	if (header.aa ne 'AA'xub) or (header.bb ne 'BB'xub) or (header.len eq 0) then begin
	if (header.aa ne 'AA'xub) or (header.bb ne 'BB'xub) then begin
		gprint, '	Header format error, Re-open socket ...'
		goto, retry
	endif
;	if header.tag eq 0 then goto, more						; never do this, let it skip through case below
	if l eq 0 then goto, more								; this should work, but doesn't with blogd bug
	payload = bytarr(header.len)
	if header.tag ne activ then goto, reread3				; "activ" is an accumulator, read sub-header next

reread2:													; only for an accumulator record
	on_ioerror, bad_read2
	header2.count = 0

	readu, (*ps).unit, header2
	swap_bytes, header2, /big_endian_data

	if debug then begin
		gprint,'blog_client_activity',' read header2 succeeded'
		gprint, '	Count=',header2.count
	endif
	if (header2.count eq 0) then begin
		gprint, '	Header2 format error, Re-open socket ...'
		goto, retry
	endif
	payload = ulonarr(header2.count)
	n_group = 16
	n_det = (header2.count - 16) < n_detectors

	if debug then gprint, '	Header1 Byte Length=',l,' Length/4-9=',(l/4)-9,',  Header2 Word Count=',header2.count

reread3:
	on_ioerror, bad_read3
	readu, (*ps).unit, payload

	tin = systime(/seconds)
	swap_bytes, payload, /big_endian_data

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

;-------------------------------------------------------------------------------------------

	case header.tag of
		activ: begin
			if first then begin
				tseq_first = header.tseq
				tseq_last = header.tseq
				tseq_lost = 0L
				first = 0
			endif
			tseq_lost = tseq_lost + ((( ((header.tseq - tseq_last) > 1) ) - 1) > 0)
			tseq_last = header.tseq

			(*pdat[0])[0:n_det-1] = payload[0:n_det-1]
			(*pdat[1])[0:15] = payload[header2.count-16:header2.count-1]
			duration = header2.duration and duration_mask
			flux0 = float(header2.flux1) / (float(duration) / 10000000L)
			flux1 = float(header2.flux2) / (float(duration) / 10000000L)
			error_bits = ulong( ishft(header2.duration and error_mask, error_offset))
			(*pl)[1] = error_bits
			(*pf)[2] = float(duration) / 10000000L
			(*pf)[3] = flux0
			(*pf)[4] = flux1
			if debug then begin
				gprint,'Activity record:'
				gprint,'	Error=',error_bits, ', Duration=', duration
				gprint,'	flux0, flux1=',flux0, flux1
				gprint,'	Activity=',(*pdat[0])[0:n_det-1]
;				wait,0.5
			endif

			t = systime(/seconds)
			percent = (tin gt tlast) ? 100. * (t-tin)/(tin-tlast) : 1.
			(*pf)[0] = percent
			if debug then gprint,' Percent busy=', percent
			lost = 100. * tseq_lost/(tseq_last - tseq_first + 1)
			(*pf)[1] = lost
			if debug then gprint,' Percent lost =', lost
			if ((tseq_last - tseq_first) gt 20) or ((tin-tlast) gt 20) then begin
				tseq_first = tseq_last
				tseq_lost = 0L
			endif
			tlast = tin
			end

		summary2: begin
			tlong6[0] = ulong(payload,0,1)				; run number			(*pl)[2]
			tlong6[1] = ulong(payload,4,1)				; segment number		(*pl)[3]
;			tlong6[2] = ulong(payload,12,1)				; run time (sec)		(*pl)[4]
;			tlong6[3] = ulong(payload,24,1)				; # blocks written		(*pl)[5]
			tlong6[4] = ulong(payload,40,1)				; bytes/sec rate		(*pl)[6]
			tlong6[5] = ulong(payload,8,1)				; discard mode flag		(*pl)[7]
			swap_bytes, tlong6, /big_endian_data

			(*pl)[2:7] = tlong6

			group_next = string(payload[44:*])			; set group string
			g = {project:{current:project,next:project_next}, group:{current:group,next:group_next}}
			s = stringify(g)
			ns = strlen(s) < 256
			b = byte(s)
			(*pb1)[*] = 0B
			(*pb1)[0:ns-1] = b[0:ns-1]					; set group/project byte-string
			if debug then begin
				gprint,'Summary 2 record:'
				gprint,'	Run=',(*pl)[2],' segment=',(*pl)[3],' rate=',(*pl)[6]
				gprint,'	Set Group/Project=', s
			endif
			end

		summary3: begin
			tlong6[0] = ulong(payload,0,1)				; run number			(*pl)[2]
			tlong6[1] = ulong(payload,4,1)				; segment number		(*pl)[3]
;			tlong6[2] = ulong(payload,12,1)				; run time (sec)		(*pl)[4]
;			tlong6[3] = ulong(payload,24,1)				; # blocks written		(*pl)[5]
			tlong6[4] = ulong(payload,40,1)				; bytes/sec rate		(*pl)[6]
			tlong6[5] = ulong(payload,8,1)				; discard mode flag		(*pl)[7]
			swap_bytes, tlong6, /big_endian_data

			(*pl)[2:7] = tlong6

			bg = payload[44:*]
			q = where(bg eq 0B, nq)
			if nq gt 0 then bg[q]=127B
			str = strsplit( string(bg), string(127B), /extract, /preserve_null)
			ns = n_elements(str)
			group_next = str[0]
			group = ''
			project_next = ''
			project = ''
			if ns ge 4 then begin
				group = str[1]
				project_next = str[2]
				project = str[3]
			endif
			g = {project:{current:project,next:project_next}, group:{current:group,next:group_next}}
			s = stringify(g)
			ns = strlen(s) < 256
			b = byte(s)
			(*pb1)[*] = 0B
			(*pb1)[0:ns-1] = b[0:ns-1]					; set group/project byte-string
			if debug then begin
				gprint,'Summary 3 record:'
				gprint,'	Run=',(*pl)[2],' segment=',(*pl)[3],' rate=',(*pl)[6]
				gprint,'	Set Group/Project=', s
			endif
			end

		summary4: begin
			tlong6[0] = ulong(payload,0,1)				; run number			(*pl)[2]
			tlong6[1] = ulong(payload,4,1)				; segment number		(*pl)[3]
;			tlong6[2] = ulong(payload,12,1)				; run time (sec)		(*pl)[4]
;			tlong6[3] = ulong(payload,24,1)				; # blocks written		(*pl)[5]
			tlong6[4] = ulong(payload,40,1)				; bytes/sec rate		(*pl)[6]
			tlong6[5] = ulong(payload,8,1)				; discard mode flag		(*pl)[7]
			swap_bytes, tlong6, /big_endian_data

			(*pl)[2:7] = tlong6

			bg = payload[44:*]
			q = where(bg eq 0B, nq)
			if nq gt 0 then bg[q]=127B
			str = strsplit( string(bg), string(127B), /extract, /preserve_null)
			ns = n_elements(str)
			group_next = str[0]
			group = ''
			project_next = ''
			project = ''
			if ns ge 5 then begin
				group = str[1]
				project_next = str[2]
				project = str[3]
				errmess = str[4]
			endif
			g = {project:{current:project,next:project_next}, group:{current:group,next:group_next}}
			s = stringify(g)
			ns = strlen(s) < 256
			b = byte(s)
			(*pb1)[*] = 0B
			(*pb1)[0:ns-1] = b[0:ns-1]					; set group/project byte-string
			if debug then begin
				gprint,'Summary 4 record:'
				gprint,'	Run=',(*pl)[2],' segment=',(*pl)[3],' rate=',(*pl)[6]
				gprint,'	Set Group/Project=', s
			endif
			end

		setgroup: begin
			n = min( [header.len, 256])
			group_next = string(payload[0:n-1])			; set group string
			g = {project:{current:project,next:project_next}, group:{current:group,next:group_next}}
			s = stringify(g)
			ns = strlen(s)
			b = byte(s)
			(*pb1)[*] = 0B
			(*pb1)[0:ns-1] = b[0:ns-1]					; set group/project byte-string
			if debug then begin
				gprint,'Setgroup record:'
				gprint,'	Set Group/Project=', s
			endif
			end

		setproject: begin
			n = min( [header.len, 256])
			project_next = string(payload[0:n-1])			; set project string
			g = {project:{current:project,next:project_next}, group:{current:group,next:group_next}}
			s = stringify(g)
			ns = strlen(s)
			b = byte(s)
			(*pb1)[*] = 0B
			(*pb1)[0:ns-1] = b[0:ns-1]					; set group/project byte-string
			if debug then begin
				gprint,'Setproject record:'
				gprint,'	Set Group/Project=', s
			endif
			end

		else:
	endcase

	goto, more

;-------------------------------------------------------------------------------------------

done:
	if (*ppar)[2] eq 1 then begin						; reset blog client
		log_message, comms, type='WARNING', 'blog_client_activity, Reset Activity blog-client.'
		if debug then gprint,'Reset Activity blog-client.'
		if ptr_valid(ps) then close_file,(*ps).unit		; close socket
		(*pfill[0])[0] = 0
		(*pvalid[0])[0] = 0
		(*ppar)[2] = 0
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, start
	endif
	if (*ppar)[3] eq 0 then goto, more

fin:													; kill blog client
	gprint,'Finish: close unit ...'
	if ptr_valid(ps) then close_file,(*ps).unit			; close socket
	(*ppar)[5] = 0										; blog client is not running
	(*ppar)[4] = 1										; kill dependent client
	(*pfill[0])[0] = 0
	(*pvalid[0])[0] = 0
	log_message, comms, type='INFO', 'blog_client_activity, Exit blog activity client.'
	if debug then begin
		gprint,'Exit Activity blog-client.'
		close_file, dun
	endif
	
;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-start.

;	print,'Finish: free shared memory ...'
;	shared_memory_unmap, prefix=prefix, n_buffers=n_buffers
	return

bad_open:
	warning,'blog_client_activity',['error opening socket','Client = '+clientname], /error
	goto, fin
bad_open2:
	warning,'blog_client_activity',['error writing hostname to socket','Client = '+clientname], /error
	goto, fin
bad_len:
	warning,'blog_client_activity',['illegal very large "len" value','Client = '+clientname], /error
	goto, fin
bad_shrmem:
	warning,'blog_client_activity',['error shrmem allocate','Client = '+clientname], /error
	return
bad_args:
	warning,'blog_client_activity',['bad passed arguments. "argc" not 2.','Client = '+clientname], /error
	return

bad_write:
	if debug then gprint,'blog_client_activity: error writing request, re-open socket'
	goto, retry
bad_read:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
;		log_message, comms, type='WARNING', 'blog_client_activity, timeout on header read, reread ..'
;		if debug then gprint,'blog_client_activity',' timeout on header read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		(*pdat[0])[*] = 0
		(*pdat[1])[*] = 0
		goto, reread
	endif
	warning,'blog_client_activity',[' error reading socket 1, re-open socket', $
			'!error_state.sys_msg returned "' + !error_state.sys_msg + '"','Client = '+clientname], /error
	if debug then gprint,'blog_client_activity',' error reading socket 1, re-open socket'
	goto, retry
bad_read2:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		log_message, comms, type='WARNING', 'blog_client_activity, timeout on header 2 read, reread ..'
		if debug then gprint,'blog_client_activity',' timeout on header2 read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		(*pdat[0])[*] = 0
		(*pdat[1])[*] = 0
		goto, reread2
	endif
	if debug then gprint,'blog_client_activity',' error reading socket 2, re-open socket'
	goto, retry
bad_read3:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		log_message, comms, type='WARNING', 'blog_client_activity, timeout on payload read, reread ..'
		if debug then gprint,'blog_client_activity',' timeout on payload read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		(*pdat[0])[*] = 0
		(*pdat[1])[*] = 0
		goto, reread3
	endif
	if debug then gprint,'blog_client_activity',' error reading socket 3, re-open socket'
	goto, retry
bad_hist:
	warning,'blog_client_activity',['error in Fortran Histogram, try more ...','Client = '+clientname]
	goto, more

retry:
	if debug then gprint,'retry socket open ...'
	socket_retry, ps, error=error
	if error then begin
		warning,'blog_client_activity',['error in Socket retry, exit ...','Client = '+clientname]
		goto, fin
	endif
	goto, again
end

