pro hist_xy, x,y,z, pimage, nx,ny,neli, hist,nhist, clip_zero=clip_zero

; Project x,y onto z in 'image' arrays, and accumulate the result
; in 'hist' arrays. 'hist' is created to suit max(z). 'pimage' is
; a pointer to the image array.
;
; The external F95 routine in 'image_dll' DLL file expects these
; arguments and data types:
;
;	x,y		int*4	vectors of x,y coords in image array
;	z		int*4	corresponding projected coordinate in hist array
;	n		int*4	number of x,y,z elements
;
;	image	float*4	image array
;	nx,ny	int*4	x,y dimensions of image array(nx,ny,nel)
;	nel		int*4	number of element planes in image array
;
;	hist	float*4	histogram array to accumulate
;	nhist	int*4	histogram frequency to accumulate
;	nh		nt*4	size if hist array
;
; To pass the image array, while avoiding IDL making copies all
; the time, we pass the pointer to image, by value, but use it by
; reference in the Fortran.

; Error returns:

	COMPILE_OPT STRICTARR
	nel = neli
	if n_elements(clip_zero) eq 0 then clip_zero=0

	err = 7
	serr = ['all OK', $								; 0
		'wrong number of arguments', $				; 1
		'bad type for a matrix or vector', $		; 2
		'no data in x array', $						; 3
		'nx,ny,nel dimensions missing', $			; 4
		'z array contains no valid data', $			; 5
		'too few data in y or z vectors', $			; 6
		'ILLEGAL ERROR'	 ]							; 7

;............................................................................
;
; Start by checking data types, and return an error if incorrect
; A recast is NOT done as this would mean wasteful temporary variables
; and time lost. Better to just get calling code right.

	if size(x,/tname) ne 'LONG' then begin
		err=2L
		goto, bad
	endif
	if size(y,/tname) ne 'LONG' then begin
		err=2L
		goto, bad
	endif
	if size(z,/tname) ne 'LONG' then begin
		err=2L
		goto, bad
	endif

; if size(*pimage,/tname) ne 'FLOAT' then err=2L & goto, bad

	n = n_elements(x)
	nh = max(z+1)
	
	if n lt 1 then begin
		err=3L
		goto, bad
	endif
	if nx lt 2 then begin
		err=4L
		goto, bad
	endif
	if ny lt 2 then begin
		err=4L
		goto, bad
	endif
	if nel lt 1 then begin
		err=4L
		goto, bad
	endif
	if nh lt 2 then begin
		err=5L
		goto, bad
	endif
	if n_elements(y) lt n then begin
		err=6L
		goto, bad
	endif
	if n_elements(z) lt n then begin
		err=6L
		goto, bad
	endif
	
	nel = nel < n_elements((*pimage)[0,0,*])
	
	print,'n=',n,' nx,ny,nel=',nx,ny,nel,' nh=',nh

;............................................................................

	hist = fltarr(nh,nel)
	nhist = lonarr(nh)
	
	for j=0L,n-1 do begin
		if ((z[j] ge 0) and (z[j] lt nh)) then begin
			if clip_zero then begin
				img = (*pimage)[x[j],y[j],0:nel-1] > 0.0
			endif else begin
				img = (*pimage)[x[j],y[j],0:nel-1]
			endelse
			hist[z[j],0:nel-1] = hist[z[j],0:nel-1] + img
			nhist[z[j]] = nhist[z[j]] + 1
		endif
	endfor
	
	return

;............................................................................

	value = bytarr(11)
	value[*] = 0B			; pass all by reference
	value[4] = 1B			; except 'pimage'
	err = 0L
	
	err = call_external( geopixe_library(), geolib_name( 'hist_xy'), cdecl=geolib_cdecl(), $
				x,y,z,long(n), pimage,long(nx),long(ny),long(nel), $
				hist, nhist, long(nh), value=value )
	
	if err ne 0L then goto, bad
	return

;............................................................................

bad:
	print,'hist_xy: error - ',serr[err]
	hist = -1
	nhist = -1
	return
end
