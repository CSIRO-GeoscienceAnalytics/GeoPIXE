pro blog_client_group_spectra, args=sargs

; Read Blog Spectrum_Accum records and copy into the Group Spectra arrays in shared memory.
; Uses a series of shared memory blocks (n_buffers). Buffer_size is about 4K.
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
	       warning,'blog_client_group_spectra',['IDL run-time error caught.', '', $
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
			comms = open_comms( server=server, source='blog_client_group_spectra', error=err)
			if err eq 0 then begin
				save_comms_in_common, comms		; save comms for implicit 'log_message' in 'warning', 'alarm_popup'
				log_message, comms, type='INFO', 'blog_client_group_spectra started, with logging using server = '+server
			endif
;			warning,'blog_client_group_spectra',['arguments:',maia_prefix,conf_file,server]
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
;	1.	blog_client_groups			for activity rates by detector, group
;
;.....................................................................................

	prefix = maia_prefix + 'groups_'

	psh = shared_memory_buffers( prefix=prefix, error=error, n_buffers=n_buffers )
	if error then goto, bad_shrmem
	buffer_size = (*psh).buffer_size
	n_channels = buffer_size[0]

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
	log_message, comms, type='INFO', 'blog_client_group_spectra, clientname='+clientname+', username='+username

	define_devices
	maia = maia_defaults(source='blog_client_group_spectra', conf=conf_file)		; read "Maia.conf" default PVs
	if debug eq -1 then debug = maia.blog.debug

	if debug then begin
		lfile = extract_path(conf_file) + username + '_' + prefix + 'debug.log'
		openw, dun, lfile, /get_lun
		gprint, active=1
		gprint, out=dun, 'Blog Client Groups debug log'
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
	tseq_lost = 0L

	index_mask = '000000E0'x
	index_offset = -5
	error_mask = 'FFFFFFF000000000'x		; error bits in "dwell/readout"
	error_offset = -40
	duration_mask = '0000000FFFFFFFFF'x		; 32 bits plus 8 bits of next 32-bit word

	spectrum_accum = 40US
	time_accum = 43US
	sendnext = 7US
	request_any = {aa:'AA'xub, tag:sendnext, bb:'BB'xub, len:0US }
	request_accum = {aa:'AA'xub, tag:sendnext, bb:'BB'xub, len:4US, desired:[spectrum_accum,time_accum] }

	header = {aa:0B, tag:0US, bb:0B, len:0US, prevlen:0US, $
			seq:0UL, tseq:0UL, tv_sec:0UL, tv_usec:0UL, $
			client:0UL, unused:0UL}

	header2 = {X:0UL,Y:0UL,Z:0UL, trigger:0UL, duration:0ULL, flux1:0UL, flux2:0UL, count:0UL}

	linux_request = request_accum
	swap_bytes, linux_request, /big_endian_data

	setblockqueuedepth = 60US
	nb = 12UL
	set_blockqueuedepth = {aa:'AA'xub, tag:setblockqueuedepth, bb:'BB'xub, len:4, desired:nb }
	swap_bytes, set_blockqueuedepth, /big_endian_data

	setname = 54US
	b = [byte(clientname),0B]
	nb = strlen(clientname) + 1
	set_clientname = {aa:'AA'xub, tag:setname, bb:'BB'xub, len:uint(nb), desired:b }
	swap_bytes, set_clientname, /big_endian_data

;	pf	0	% busy				pl	0	blog port		pb	*	blog server ip
;		1	% buffers lost			1	errors
;		2	t_interval
;		3	flux1
;		4	flux2

start:
	log_message, comms, type='INFO', 'blog_client_group_spectra, START, open socket.'
	pf = pfloat[0]
	pb = pbyte[0]
	pl = plong[0]
	blog_server_ip = string( *pb)
	blog_port = (*pl)[0]

	wait, 1
	sock = open_socket( ip=blog_server_ip, port=blog_port, error=error, $
						read_timeout=3, retries=0, client=clientname )
	if error ne 0 then goto, bad_open
	ps = ptr_new(sock)
	tseq_lost = 0L
	loglast_tin = 0.

	on_ioerror, bad_open2
	writeu, (*ps).unit, set_clientname
	if debug then gprint,'blog_client_group_spectra',' write hostname succeeded'

	on_ioerror, bad_open3
	writeu, (*ps).unit, set_blockqueuedepth
	if debug then gprint,'blog_client_group_spectra',' write blockqueuedepth succeeded'

again:
	start_time = systime(/seconds)
	tlast = start_time
	(*ppar)[5] = 1						; blog client is running

;	Start loop, detect (*ppar)[3] = 1 to indicate a remote
;	kill command from client, etc.
;	Detect (*ppar)[2] = 1 to indicate a reset command
;	Use pvalid shared memory to indicate a valid/available buffer

more:
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

;-------------------------------------------------------------------------------------------
;	Note: from request write must complete both header and payload reads without interruption

	on_ioerror, bad_write
	writeu, (*ps).unit, linux_request
	if debug then gprint,'blog_client_group_spectra',' write succeeded'

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
	if (header.tag ne spectrum_accum) and (header.tag ne time_accum) then goto, reread3

reread2:
	on_ioerror, bad_read2
	readu, (*ps).unit, header2

	swap_bytes, header2, /big_endian_data

	payload = ulonarr(header2.count)
	siz = header2.count < n_channels
	index = ishft( header2.trigger and index_mask, index_offset)

	if debug then gprint, 'Header1 Byte Length=',l,' Length/4-9=',(l/4)-9,',  Header2 Word Count=',header2.count

reread3:
	on_ioerror, bad_read3
	readu, (*ps).unit, payload

	tin = systime(/seconds)
	swap_bytes, payload, /big_endian_data

	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

;-------------------------------------------------------------------------------------------

	if (index ge 0) and (index lt n_buffers) then begin
		(*pdat[index])[0:siz-1] = (*pdat[index])[0:siz-1] + payload[0:siz-1]
	endif else gprint,'blog_client_group_spectra: illegal "index" ',index

	duration = header2.duration and duration_mask
	error_bits = ulong( ishft(header2.duration and error_mask, error_offset))
	(*pl)[1] = error_bits
	(*pf)[2] = (*pf)[2] + float(duration) / 10000000L	; assumes 100 ns clock
	(*pf)[3] = (*pf)[3] + float(header2.flux1)
	(*pf)[4] = (*pf)[4] + float(header2.flux2)			; only needed in pixel records

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
	goto, more

done:
	if (*ppar)[2] eq 1 then begin						; reset blog client
		log_message, comms, type='INFO', 'blog_client_group_spectra, RESET, clear spectra shared memory.'
		if debug then gprint,'Reset Group Spectra blog-client.'
		if ptr_valid(ps) then close_file,(*ps).unit			; close socket
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
	log_message, comms, type='INFO', 'blog_client_group_spectra, EXIT, close socket and exit.'
	if ptr_valid(ps) then close_file,(*ps).unit			; close socket
	(*ppar)[5] = 0										; blog client is not running
	(*ppar)[4] = 1										; kill dependent client
	(*pfill[0])[0] = 0
	(*pvalid[0])[0] = 0
	if debug then begin
		gprint,'Exit Groups blog-client.'
		close_file, dun
	endif

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-start.

;	shared_memory_unmap, prefix=prefix, n_buffers=n_buffers
	return

bad_open:
	warning,'blog_client_group_spectra','error opening socket'
	goto, fin
bad_open2:
	warning,'blog_client_group_spectra','error writing hostname to socket'
	goto, fin
bad_open3:
	warning,'blog_client_group_spectra','error writing block queue depth to socket'
	goto, fin
bad_len:
	warning,'blog_client_group_spectra','illegal very large "len" value'
	goto, fin
bad_shrmem:
	warning,'blog_client_group_spectra',' error shrmem allocate'
	return
bad_args:
	warning,'blog_client_group_spectra',' bad passed arguments. "argc" not 2.'
	return
	
bad_write:
	if debug then gprint,'blog_client_group_spectra',' error writing request, re-open socket'
	goto, retry
bad_read:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
;		log_message, comms, type='WARNING', 'blog_client_group_spectra, timeout on header read, reread ..'
;		if debug then gprint,'blog_client_group_spectra',' timeout on header read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, reread
	endif
	warning,'blog_client_group_spectra',[' error reading socket 1, re-open socket', $
			'!error_state.sys_msg returned "' + !error_state.sys_msg + '"'], /error
	if debug then gprint,'blog_client_group_spectra',' error reading socket 1, re-open socket'
	goto, retry
bad_read2:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		log_message, comms, type='WARNING', 'blog_client_group_spectra, timeout on header 2 read, reread ..'
		if debug then gprint,'blog_client_group_spectra',' timeout on header2 read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, reread2
	endif
	if debug then gprint,'blog_client_group_spectra',' error reading socket 2, re-open socket'
	goto, retry
bad_read3:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		log_message, comms, type='WARNING', 'blog_client_group_spectra, timeout on payload read, reread ..'
		if debug then gprint,'blog_client_group_spectra',' timeout on payload read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, reread3
	endif
	if debug then gprint,'blog_client_group_spectra',' error reading socket 3, re-open socket'
	goto, retry
	
retry:
	if debug then gprint,'retry socket open ...'
	socket_retry, ps, error=error
	if error then begin
		warning,'blog_groups_activity',['error in Socket retry, exit ...','Client = '+clientname]
		goto, fin
	endif
	goto, again
end

