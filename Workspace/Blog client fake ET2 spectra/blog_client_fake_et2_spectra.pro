pro blog_client_fake_et2_spectra

; Uses shared memory for the actual spectra memory. See "blog_client_fake_spectra",
; which uses shared memory as buffers.
;
; Fake a read from blog socket on blog2 using Monte Spectrum approach
; write to shared memory (e.g. to be read by "test_shrmem_blog_client4")

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
	       warning,'blog_client_fake',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !Error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return
	    endif
	endif
	tin = systime(/seconds)

; If the n_buffers or buffer_size pars are changed, reboot.
; Or, uncomment the memory unmapping commands in fin:
; Then shutdown all referencing processes.
; Make them the large sizes BEFORE building a version for Linux.

	debug = 0								; print debug lines
	delay = 0.5								; delay between buffers
	prefix = 'blog_client_et2_spectra_'

	psh = shared_memory_buffers( prefix=prefix, error=error, n_buffers=n_buffers )
	if error then goto, bad_shrmem
	buffer_size = (*psh).buffer_size
	n_detectors = buffer_size[1]
	n_channels = buffer_size[0]

	ppar = (*psh).ppar
	pdat = (*psh).pdat
	plong = (*psh).plong
	pfloat = (*psh).pfloat
	pbyte = (*psh).pbyte
	pfill = (*psh).pfill
	pvalid = (*psh).pvalid

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

;	Load a spectrum to use Monte carlo methods with ...

	ps = read_spec('C:\software\IDL\GeoPIXE-source\Develop\monte-test.spec')

	cumm = fltarr((*ps).size)
	cumm[0] = (*(*ps).data)[0]
	for k=1,(*ps).size-1 do begin
	    cumm[k] = cumm[k-1] + (*(*ps).data)[k]
	endfor
	cumm = float(cumm) * float((*ps).size-1)/cumm[(*ps).size-1]

	on_ioerror, null

start:
	tlast = tin
	tin = systime(/seconds)
	loop = 0
	(*ppar)[5] = 1							; blog client is running

;	Start loop, detect (*ppar)[3] = 1 to indicate a remote
;	kill command from client, etc.
;	Detect (*ppar)[2] = 1 to indicate a reset command
;	Use pvalid shared memory to indicate a valid/available buffer

more:
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

    r = (*ps).size * randomu( seed, 1000L)
    m = binary_search2( cumm, r)
	h = histogram( m, binsize=1, min=0,max=n_channels-1)
	g = (n_detectors*randomu( seed,1) < (n_detectors-1))[0]

	pd = pdat[0]
	pe = plong[0]
	pf = pfloat[0]

	(*pd)[0:n_channels-1,g] = temporary((*pd)[0:n_channels-1,g]) + h	; data
	(*pe)[0] = long(g)													; group in extra long 0
	(*pfill[0])[0] = n_channels											; fill
	(*pvalid[0])[0] = 1													; valid

	loop = loop+1
	if loop ge 16 then begin
		t = systime(/seconds)
		percent = 100. * (t-tin)/(tin-tlast)
		(*pf)[0] = percent
		if debug then print,' Percent=', percent
		wait, delay
		goto, start
	endif
	goto, more

done:
	if (*ppar)[2] eq 1 then begin			; reset blog client
		print,'Reset blog-client.'
		for i=0,n_buffers-1 do begin
			(*pfill[i])[0] = 0
			(*pvalid[i])[0] = 0
		endfor
		(*ppar)[2] = 0
		goto, start
	endif
	if (*ppar)[3] eq 0 then goto, more

fin:
	(*ppar)[5] = 0							; blog client is not running
	(*ppar)[4] = 1							; kill dependent client
	for i=0,n_buffers-1 do begin
		(*pfill[i])[0] = 0
		(*pvalid[i])[0] = 0
	endfor
	print,'Finish: free shared memory ...'

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-boot.

;	shared_memory_unmap, prefix=prefix, n_buffers=n_buffers
	return

bad_len:
	warning,'blog_client','illegal very large "len" value'
	goto, fin
bad_shrmem:
	print,'blog_client',' error shrmem allocate'
	goto, fin
end

