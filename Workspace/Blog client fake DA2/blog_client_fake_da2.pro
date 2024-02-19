pro blog_client_fake_da2

; Uses shared memory for the actual image memory. See "blog_client_fake_da", which
; uses shared memory as buffers
;
; Fake a read from blog socket on blog2 using a stored image file
; write to shared memory (e.g. to be read by "maia_launch")
; Save using project build, which uses make_blog_client_fake_da2 (or use IDL 6.3).

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
	       warning,'blog_client_fake_da2',['IDL run-time error caught.', '', $
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

	debug = 0							; print debug lines
	delay = 0.05						; delay between buffers
	prefix = 'blog_client_da2_'

	psh = shared_memory_buffers( prefix=prefix, error=error, n_buffers=n_buffers, /floatd)
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

;	Load an image ...

	path = 'maia' + slash()
	ps = read_geopixe_image(path + 'c4-x-3-m.dai', error=err)
	if err then goto, bad_file

	pd = pdat[0]
	pe = plong[0]
	pf = pfloat[0]
	pv = pvalid[0]
	(*pfill[0])[0] = 0

	nx = min([(*ps).xsize, nx])
	ny = min([(*ps).ysize, ny])
	nel = min([long((*ps).n_el),nel])
	els = (*(*ps).el)[0:nel-1]

	y = 0L
	on_ioerror, null

start:
	(*ppar)[5] = 1							; blog client is running

;	Start loop, detect (*ppar)[3] = 1 to indicate a remote
;	kill command from client, etc.
;	Detect (*ppar)[2] = 1 to indicate a reset command
;	Use pvalid shared memory to indicate a valid/available buffer

more:
	tlast = tin
	tin = systime(/seconds)
	if ((*ppar)[3] eq 1) or ((*ppar)[2] eq 1) then goto, done

	(*pe)[0:4] = [nel,nx,ny,(*ps).n_el,0]

;	Need compression here. In which case, need to use Fortran 'image_accumulate'.
;	Later, this will use DA records (see original maia_update_da.pro), and increment
;	these into *pd using  'image_accumulate'. TAKE CARE: Need to pass the actual
;	dimensions of the *pd array to Fortran.

	for i=0,15 do begin
		(*pd)[0:nx-1,y,0:nel-1] = temporary((*pd)[0:nx-1,y,0:nel-1]) +  $
									(*(*ps).image)[0:nx-1,y,0:nel-1]
	endfor
	(*pv)[0] = 1								; valid

	y = y+1
	if y ge ny then begin
		y = 0L
	endif

;	For real DA, need to add code for lost buffers, as in spectra3 ...

;	wait,0.1
	t = systime(/seconds)
	(*pf)[0] = (*pf)[0] + (t-tin)
	if debug then print,' Time busy=', (*pf)[0]

	wait, delay
	goto, more

done:
	if (*ppar)[2] eq 1 then begin			; reset blog client
		print,'Reset blog-client.'
		(*pfill[0])[0] = 0
		(*pv)[0] = 0
		y = 0L
		(*ppar)[2] = 0
		goto, start
	endif
	if (*ppar)[3] eq 0 then goto, more

fin:
	(*ppar)[5] = 0							; blog client is not running
	(*ppar)[4] = 1							; kill dependent client
	(*pvalid[0])[0] = 0
	print,'Finish: free shared memory ...'

;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-boot.

;	shared_memory_unmap, prefix=prefix, n_buffers=n_buffers
	return

bad_file:
	warning,'blog_client_fake_da2','failed to open fake DA image file'
	goto, fin
bad_len:
	warning,'blog_client_fake_da2','illegal very large "len" value'
	goto, fin
bad_shrmem:
	print,'blog_client_fake_da2',' error shrmem allocate'
	goto, fin
end

