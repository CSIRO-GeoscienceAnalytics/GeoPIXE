pro blog_client_et2_spectra, args=sargs

; Read Blog ET2 records and increment into a 2D spectra array in shared memory.
; Uses a single 2D shared memory block (n_buffers=1). Buffer_size is a 2D array
; for [n_channels,n_detectors].
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
	       warning,'blog_client_et2_spectra',['IDL run-time error caught.', '', $
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
			comms = open_comms( server=server, source='blog_client_et2_spectra', error=err)
			if err eq 0 then begin
				save_comms_in_common, comms		; save comms for implicit 'log_message' in 'warning', 'alarm_popup'
				log_message, comms, type='INFO', 'blog_client_et2_spectra started, with logging using server = '+server
			endif
;			warning,'blog_client_et2_spectra',['arguments:',maia_prefix,conf_file,server]
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
; Use 3 shared memory areas referenced here:
;	1.	blog_client_et2_spectra		for individual E detector spectra
;	2.	blog_client_et2_spectraT	for individual T detector spectra
;	3.	blog_client_et2d			for ET 2D 'images' for All, detectors: 0, 1, 2, ...
;
; Only the first one will use hand-shaking flags. The 2nd will only
; use the data buffer and the size parameters in ppar2.
;.....................................................................................

	prefix = maia_prefix + 'et_spec_'

	psh = shared_memory_buffers( prefix=prefix, error=error, n_buffers=n_buffers )
	if error then goto, bad_shrmem
	buffer_size = (*psh).buffer_size
	n_channels = buffer_size[0]
	n_detectors = buffer_size[1]

	ppar = (*psh).ppar
	pdat = (*psh).pdat							; E spectra
	plong = (*psh).plong
	pfloat = (*psh).pfloat
	pbyte = (*psh).pbyte
	pfill = (*psh).pfill
	pvalid = (*psh).pvalid

	s = get_login_info()
	clientname = s.machine_name + ':' + prefix
	username = s.user_name
	log_message, comms, type='INFO', 'blog_client_et2_spectra, clientname='+clientname+', username='+username

	define_devices
	maia = maia_defaults(source='blog_client_et2_spectra', conf=conf_file)		; read "Maia.conf" default PVs
	if debug eq -1 then debug = maia.blog.debug

	if debug then begin
		lfile = extract_path(conf_file) + username + '_' + prefix + 'debug.log'
		openw, dun, lfile, /get_lun
		gprint, active=1
		gprint, out=dun, 'Blog Client ET2 debug log'
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

	prefix4 = maia_prefix + 'et_specT_'

	psh4 = shared_memory_buffers( prefix=prefix4, error=error, n_buffers=n_buffers4 )
	if error then goto, bad_shrmem
	buffer_size4 = (*psh4).buffer_size
	n_channels4 = buffer_size4[0]
	n_detectors4 = buffer_size4[1]

	ppar4 = (*psh4).ppar
	pdat4 = (*psh4).pdat						; T spectra
	pbyte4 = (*psh4).pbyte						; throttle spectrum storage

;	par array:	0	n_buffers
;				1	free
;			  6-12	free
;			 13-15	buffer_size (1-3 dimensions)
;
;	  incoming (from maia_launch ...):
;	  outgoing (to maia_launch ...):                                      buffer_size
;	                   0           1         2    3    4    5    6    7  ...   13
;.....................................................................................

	prefix2 = maia_prefix + 'et2d_'

	psh2 = shared_memory_buffers( prefix=prefix2, error=error2, n_buffers=n_buffers2 )
	if error2 then goto, bad_shrmem
	buffer_size2 = (*psh2).buffer_size
	n_energy = buffer_size2[0]
	n_time = buffer_size2[1]					; new 3D image requires GeoPIXE 7.5y at least
	n_et2d = buffer_size2[2]					; and Fortran lib 41

	ecompress = (4096L/n_energy) > 1			; for 12 bit E
	tcompress = (1024L/n_time) > 1				; for 10 bit T

	ppar2 = (*psh2).ppar
	pdat2 = (*psh2).pdat						; ET 2D map (may contain 3rd dimension: n_dets+1)
;
;	par array:	0	n_buffers
;				1	free
;			  6-12	free
;			 13-15	buffer_size (1-3 dimensions)
;
;	  outgoing (to maia_launch ...):                                        buffer_size
;	                   0           1         2    3    4    5    6    7  ...   13
;.....................................................................................

	timeout_retry = 0
	channel_on = intarr(n_detectors)
	channel_on[*] = 1
	found = lonarr(n_detectors)

	nmax = 64*1024L/4							; max length of ET2 payload (longs)
	e = uintarr(nmax)
	tot = uintarr(nmax)
	x = uintarr(nmax)
	y = uintarr(nmax)
	z = uintarr(nmax)
	ste = uintarr(nmax)
	x0 = 0
	y0 = 0
	z0 = 0
	swap = (big_endian() ne 1)					; is local platform big-endian?
	specx = lonarr(n_channels, n_detectors)
	specy = lonarr(n_channels, n_detectors)
	first = 1
	tseq_last = 0L
	tseq_first = 0L

	et2 = 25US
	et3 = 31US
	et4 = 34US
	monitor = 26US
	sendnext = 7US
	request_any = {aa:'AA'xub, tag:sendnext, bb:'BB'xub, len:0US }
	request_et = {aa:'AA'xub, tag:sendnext, bb:'BB'xub, len:6US, desired:[et2,et3,et4] }
	request_monitor = {aa:'AA'xub, tag:sendnext, bb:'BB'xub, len:2US, desired:monitor }

	header = {aa:0B, tag:0US, bb:0B, len:0US, prevlen:0US, $
			seq:0UL, tseq:0UL, tv_sec:0UL, tv_usec:0UL, $
			client:0UL, unused:0UL}

	linux_request = request_et
	swap_bytes, linux_request, /big_endian_data

	setname = 54US
	b = [byte(clientname),0B]
	nb = strlen(clientname) + 1
	set_clientname = {aa:'AA'xub, tag:setname, bb:'BB'xub, len:uint(nb), desired:b }
	swap_bytes, set_clientname, /big_endian_data

;	pf	0	% busy				pl	0	blog port		pb	*	blog server ip
;		1	% buffers lost			1					pb4	*	throttle spectrum
;		2	ROI fraction			2	ROI enable
;									3,4	ROI limits

start:
	log_message, comms, type='INFO', 'blog_client_et2, START, open socket.'
	pf = pfloat[0]
	pb = pbyte[0]
	pb4 = pbyte4[0]
	pl = plong[0]
	blog_server_ip = string( *pb)
	blog_port = (*pl)[0]

	wait, 1
	sock = open_socket( ip=blog_server_ip, port=blog_port, error=error, $
							read_timeout=3, retries=0, client=clientname )
	if error ne 0 then goto, bad_open
	ps = ptr_new(sock)

	q = where( *pb4 lt 1, nq)			; check zeroes in Throttle vector
	if nq gt 0 then (*pb4)[q] = 1B
	tseq_lost = 0L
	loglast_tin = 0.
	loglast_et = 0.
	good_et = 0LL
	bad_et = 0LL
	lost_et = 0LL
	timeout_count = 0LL

	on_ioerror, bad_open2
	writeu, (*ps).unit, set_clientname
	if debug then begin
		gprint,'blog_client_activity',' write hostname succeeded'
		gprint,'blog_client_activity',' n_detectors, n_channels ='+str_tidy(n_detectors)+','+str_tidy(n_channels)
	endif

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

;-------------------------------------------------------------------------------------------
;	Note from request write must complete both header and payload reads without interruption

	on_ioerror, bad_write
	if pre_write eq 0 then begin
		writeu, (*ps).unit, linux_request
		if debug then gprint,'blog_client_et2_spectra',' write succeeded'
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
	dsl = ((( ((header.tseq - tseq_last) > 1) ) - 1) > 0)
	tseq_lost += dsl
	lost_et += dsl
	tseq_last = header.tseq

	payload = bytarr(header.len)

reread2:
	on_ioerror, bad_read2
	readu, (*ps).unit, payload
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

	writeu, (*ps).unit, linux_request
	if debug then gprint,'blog_client_et2_spectra',' write succeeded'
	pre_write = 1

;-------------------------------------------------------------------------------------------

;	(*pdat2[0])[1,1,0] = 1234.

	tin = systime(/seconds)
	n = 0L
	tag3 = where( header.tag eq [et2,et3,et4])

;	x,y spectra are not used here, so we don't need the updated 'maia_et2_events2' routine with re-directed axes, including 'z'.

;	err = maia_et2_events2( payload, header.len, channel_on,n_detectors, e,tot,x,y,z,n,ste, swap, x0,y0,z0, tag=tag3)
	err = maia_et2_events( payload, header.len, channel_on,n_detectors, e,tot,x,y,n,ste, swap, x0,y0, tag=tag3)

	if err then goto, bad_read3
	if debug then gprint, '    Tag Seq = ',header.tseq,'  N events = ',n

;	x,y spectra are ignored for now, so we don't need to worry about the redirection ...
;	case self.sort_options.source.x of
;		0: x1 = x[0:n-1]
;		1: x1 = y[0:n-1]
;		2: x1 = z[0:n-1]
;	endcase
;	case self.sort_options.source.y of
;		0: y1 = x[0:n-1]
;		1: y1 = y[0:n-1]
;		2: y1 = z[0:n-1]
;	endcase
;	case self.sort_options.source.z of
;		0: z1 = x[0:n-1]
;		1: z1 = y[0:n-1]
;		2: z1 = z[0:n-1]
;	endcase

	if n ge 1 then begin
	    err = hist_accumulate( e,x,y,ste,n, *pdat[0],specx,specy, n_channels,n_detectors,found, $
	    					tot=tot, multiple=long((*pb4)[e]), et2d=*pdat2[0], n_energy=n_energy,n_time=n_time, $
	    					ecompress=ecompress,tcompress=tcompress, spect=*pdat4[0], nt=n_channels4 )
	    if err ne 0 then goto, bad_hist
		(*pvalid[0])[0] = 1
		(*pfill[0])[0] = (*pfill[0])[0] + n

		ROI_enable = (*pl)[2]
		if ROI_enable then begin
			low = (*pl)[3]
			high = (*pl)[4]
			q = where( (e ge low) and (e le high), nq)
;			nr = (nq ge 1) ? total( (*pb4)[e[q]]) : 0			; won't work for throttle factors
			fraction = float( nq) / float(n_elements(e))
			(*pf)[2] = fraction
		endif
	endif else begin
		if debug then gprint,'Found nothing.'
	endelse
	good_et++

	t = systime(/seconds)
	percent = (tin gt tlast) ? 100. * (t-tin)/(tin-tlast) : 1.
	(*pf)[0] = percent
	if debug then gprint,' Percent busy=', percent
	lost = 100. * tseq_lost/(tseq_last - tseq_first + 1)
	(*pf)[1] = lost
	if (lost gt 80) and ((tseq_last - tseq_first) gt 20) and (t gt loglast_tin + 100.) then begin
		log_message, comms, type='WARNING', 'blog_client_et2, Lost ET records exceeds 80% ('+str_tidy(lost,places=1)+')'
		loglast_tin = t
	endif
	if debug then gprint,' Percent lost =', lost
	if ((tseq_last - tseq_first) gt 100) or ((tin-tlast) gt 10) then begin
		tseq_first = tseq_last
		tseq_lost = 0L
	endif
	if (t gt loglast_et + 100.) then begin
		log_message, comms, type='INFO', 'blog_client_et2, Good ET records='+str_tidy(good_et)+' (bad='+str_tidy(bad_et)+', lost='+str_tidy(lost_et)+')'
		loglast_et = t
	endif
	tlast = tin
	goto, more

done:
	if (*ppar)[2] eq 1 then begin							; reset blog client
		log_message, comms, type='INFO', 'blog_client_et2, RESET, clear spectra shared memory.'
		if debug then gprint,'Reset ET2 Spectra blog-client.'
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
	log_message, comms, type='INFO', 'blog_client_et2, EXIT, close socket and exit.'
	if ptr_valid(ps) then close_file,(*ps).unit			; close socket
	(*ppar)[5] = 0										; blog client is not running
	(*ppar)[4] = 1										; kill dependent client
	(*pfill[0])[0] = 0
	(*pvalid[0])[0] = 0
	if debug then begin
		gprint,'Exit ET2 blog-client.'
		close_file, dun
	endif

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-start.

;	shared_memory_unmap, prefix=prefix, n_buffers=n_buffers
	return

bad_open:
	warning,'blog_client_et2_spectra','error opening socket'
	goto, fin
bad_open2:
	warning,'blog_client_et2_spectra','error writing hostname to socket'
	goto, fin
bad_len:
	warning,'blog_client_et2_spectra','illegal very large "len" value'
	goto, fin
bad_shrmem:
	warning,'blog_client_et2_spectra',' error shrmem allocate'
	return
bad_args:
	warning,'blog_client_et2_spectra',' bad passed arguments.'
	return
	
bad_write:
	warning,'blog_client_et2_spectra',[' error writing request, re-open socket', $
			'!error_state.sys_msg returned "' + !error_state.sys_msg + '"'], /error
	if debug then gprint,'blog_client_et2_spectra',' error writing request, re-open socket'
	goto, retry
bad_read:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		if timeout_count++ gt 100 then begin
			log_message, comms, type='WARNING', 'blog_client_et2_spectra, 100 consecutive timeouts on header read, reread ..'
			timeout_count = 0LL
		endif
		if debug then gprint,'blog_client_et2_spectra',' timeout on header read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, reread
	endif
	warning,'blog_client_et2_spectra',[' error reading socket 1, re-open socket', $
			'!error_state.sys_msg returned "' + !error_state.sys_msg + '"'], /error
	if debug then gprint,'blog_client_et2_spectra',' error reading socket 1, re-open socket'
	goto, retry
bad_read2:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		log_message, comms, type='WARNING', 'blog_client_et2_spectra, timeout on payload read, reread ..'
		if debug then gprint,'blog_client_et2_spectra',' timeout on payload read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, reread2
	endif
	if debug then gprint,'blog_client_et2_spectra',' error reading socket 2, re-open socket'
	goto, retry
bad_read3:
	warning,'blog_client_et2_spectra',' error in Fortran decode 3, try more ...'
	bad_et++
	goto, more
bad_hist:
	warning,'blog_client_et2_spectra',' error in Fortran Histogram, try more ...'
	bad_et++
	goto, more
	
retry:
	if debug then gprint,'retry socket open ...'
	socket_retry, ps, error=error
	if error then goto, fin
	goto, again
end

