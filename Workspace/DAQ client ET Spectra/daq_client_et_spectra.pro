pro daq_client_et_spectra

; Read Blog DAQ ET records and increment into a 2D spectra array in shared memory.
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
	       warning,'daq_client_et_spectra',['IDL run-time error caught.', '', $
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
; Blog client also loads routines from Maia_control.sav and DAQ_Control.sav and all
; device object SAV files, which includes DAQ.

  	startupp, /daq

	argv = command_line_args( count=argc)
	if argc eq 0 then begin
		daq_prefix = 'DAQ_173_'
		conf_file = 'C:\Documents and Settings\rya113\.geopixe\CSIRO-shadow-DAQ-Klee-173.DAQ.conf'
	endif else if argc lt 2 then begin
		goto, bad_args
	endif else begin
		daq_prefix = argv[0]
		conf_file = argv[1]
	endelse
;	warning,'daq_client_et_spectra',['arguments:',daq_prefix,conf_file]

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
;	1.	daq_et_spectra		for individual E detector spectra
;	2.	daq_et_spectraT		for individual T detector spectra
;	3.	daq_et2d			for a single ET 2D 'image'
;
; Only the first one will use hand-shaking flags. The 2nd will only
; use the data buffer and the size parameters in ppar2.
;.....................................................................................

	prefix = daq_prefix + 'et_spec_'
;	warning,'daq_client_et_spectra',['prefix:',prefix,conf_file]

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

	define_devices
	daq = daq_defaults(source='daq_client_et_spectra', conf=conf_file)		; read "DAQ.conf" defaults
	debug = daq.blog.debug

	if debug then begin
		lfile = extract_path(conf_file) + username + '_' + prefix + 'debug.log'
		openw, dun, lfile, /get_lun
		gprint, active=1
		gprint, out=dun, 'DAQ Blog Client ET spectra debug log'
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
;	  incoming (from daq_launch ...):     reset kill
;	  outgoing (to daq_launch ...):                 kill running          buffer_size
;	                   0           1         2    3    4    5    6    7  ...   13
;.....................................................................................

	prefix4 = daq_prefix + 'et_specT_'

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
;	  incoming (from daq_launch ...):
;	  outgoing (to daq_launch ...):                                      buffer_size
;	                   0           1         2    3    4    5    6    7  ...   13
;.....................................................................................

	prefix2 = daq_prefix + 'et2d_'

	psh2 = shared_memory_buffers( prefix=prefix2, error=error2, n_buffers=n_buffers2 )
	if error2 then goto, bad_shrmem
	buffer_size2 = (*psh2).buffer_size
	n_energy = buffer_size2[0]
	n_time = buffer_size2[1]

	ecompress = (8192L/n_energy) > 1			; for 13 bit E
	tcompress = (2048L/n_time) > 1				; for 11 bit T

	ppar2 = (*psh2).ppar
	pdat2 = (*psh2).pdat						; ET 2D map
;
;	par array:	0	n_buffers
;				1	free
;			  6-12	free
;			 13-15	buffer_size (1-3 dimensions)
;
;	  outgoing (to daq_launch ...):                                      buffer_size
;	                   0           1         2    3    4    5    6    7  ...   13
;.....................................................................................

	timeout_retry = 0
	channel_on = intarr(n_detectors)
	channel_on[*] = 1
	found = lonarr(n_detectors)

	nmax = 64*1024L/4						; max length of ET payload (longs)
	e = uintarr(nmax)
	tot = uintarr(nmax)
	x = uintarr(nmax)
	y = uintarr(nmax)
	ste = uintarr(nmax)
	time = ulonarr(nmax)
	x0 = 0
	y0 = 0
	swap = (big_endian() ne 1)
	specx = lonarr(n_channels, n_detectors)
	specy = lonarr(n_channels, n_detectors)
	seen = intarr(n_detectors)
	first = 1
	tseq_last = 0L
	tseq_first = 0L

	daq_et = 48US
	daq_et_nots = 49US
	monitor = 26US
	sendnext = 7US
	request_any = {aa:'AA'xub, tag:sendnext, bb:'BB'xub, len:0US }
	request_et = {aa:'AA'xub, tag:sendnext, bb:'BB'xub, len:4US, desired:[daq_et,daq_et_nots] }
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

start:
	pf = pfloat[0]
	pb = pbyte[0]
	pb4 = pbyte4[0]
	pl = plong[0]
	blog_server_ip = string( *pb)
	blog_port = (*pl)[0]

	wait, 1
	sock = open_socket( ip=blog_server_ip, port=blog_port, error=error, $
									retries=0, read_timeout=10, client=clientname )
	if error ne 0 then goto, bad_open
	ps = ptr_new(sock)

	q = where( *pb4 lt 1, nq)
	if nq gt 0 then (*pb4)[q] = 1B
	tseq_lost = 0L

	on_ioerror, bad_open2
	writeu, (*ps).unit, set_clientname
	if debug then gprint,'daq_client_et_spectra',' write hostname succeeded'

again:
	start_time = systime(/seconds)
	tlast = start_time
	(*ppar)[5] = 1						; blog client is running
	(*pb4)[*] = (*pb4)[*] > 1			; Throttle multiplicity at least 1

;	Start loop, detect (*ppar)[3] = 1 to indicate a remote
;	kill command from client, etc.
;	Detect (*ppar)[2] = 1 to indicate a reset command
;	Use pvalid shared memory to indicate a valid/available buffer (old/redundant?)

more:
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

;-------------------------------------------------------------------------------------------
;	Note from request write must complete both header and payload reads without interruption

	on_ioerror, bad_write
	writeu, (*ps).unit, linux_request
	if debug then gprint,'daq_client_et_spectra',' write succeeded'

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

reread2:
	on_ioerror, bad_read2
	readu, (*ps).unit, payload
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

;-------------------------------------------------------------------------------------------

;	(*pdat2[0])[1,1] = 1234.

	tin = systime(/seconds)
	n = 0L

	tag3 = where( header.tag eq [daq_et,daq_et_nots])
	err = daq_et_events( payload, header.len, channel_on,n_detectors, e,tot,x,y,time,n,ste, swap, x0,y0, tag3=tag3)
	if err then goto, bad_read3

	seen[*]=0
	if n gt 0 then seen[ste] = 1
	if debug then gprint,'Data, n=',n,'  seen=',where(seen ne 0)

	if n ge 1 then begin
	    err = hist_accumulate( e,x,y,ste,n, *pdat[0],specx,specy, n_channels,n_detectors,found, $
	    					tot=tot, multiple=long((*pb4)[e]), et2d=*pdat2[0], n_energy=n_energy,n_time=n_time, $
	    					ecompress=ecompress,tcompress=tcompress, spect=*pdat4[0], nt=n_channels4 )
	    if err ne 0 then goto, bad_hist
		(*pvalid[0])[0] = 1
		(*pfill[0])[0] = (*pfill[0])[0] + n

;		Do DA/ROI processing here too, or in a parallel process (daq_client_da)?

	endif else if debug then gprint,'Found nothing.'

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
		gprint,'Reset ET Spectra blog-client.'
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
	if ptr_valid(ps) then close_file,(*ps).unit			; close socket
	(*ppar)[5] = 0										; blog client is not running
	(*ppar)[4] = 1										; kill dependent client
	(*pfill[0])[0] = 0
	(*pvalid[0])[0] = 0
	if debug then begin
		gprint,'Exit DAQ ET blog-client.'
		close_file, dun
	endif

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-start.

;	gprint,'Finish: free shared memory ...'
;	shared_memory_unmap, prefix=prefix, n_buffers=n_buffers
	return

bad_open:
	warning,'daq_client_et_spectra','error opening socket'
	goto, fin
bad_open2:
	warning,'daq_client_et_spectra','error writing hostname to socket'
	goto, fin
bad_len:
	warning,'daq_client_et_spectra','illegal very large "len" value'
	goto, fin
bad_write:
	gprint,'daq_client_et_spectra',' error writing request, re-open socket'
	goto, retry
bad_read:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		if debug then gprint,'daq_client_et_spectra',' timeout on header read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, reread
	endif
	warning,'daq_client_et_spectra',[' error reading socket 1, re-open socket', $
			'!error_state.sys_msg returned "' + !error_state.sys_msg + '"'], /error
	gprint,'daq_client_et_spectra',' error reading socket 1, re-open socket'
	goto, retry
bad_read2:
	if !error_state.sys_msg eq timeout_msg then begin
;		message, /reset
		if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done
		if debug then gprint,'daq_client_et_spectra',' timeout on payload read, reread ...'
		tseq_first = tseq_last
		tseq_lost = 0L
		(*pf)[0] = 0.0
		(*pf)[1] = 0.0
		goto, reread2
	endif
	gprint,'daq_client_et_spectra',' error reading socket 2, re-open socket'
	goto, retry
bad_read3:
	warning,'daq_client_et_spectra',' error in Fortran decode 3, try more ...'
	goto, more
bad_hist:
	warning,'daq_client_et_spectra',' error in Fortran Histogram, try more ...'
	goto, more
bad_shrmem:
	warning,'daq_client_et_spectra',' error shrmem allocate'
	return
bad_args:
	warning,'daq_client_et_spectra',' bad passed arguments. "argc" not 2.'
	return
retry:
	if debug then gprint,'retry socket open ...'
	socket_retry, ps, error=error
	if error then goto, fin
	goto, again
end

