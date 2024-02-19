function do_translate_region, p, ixoffset=ixoffset, iyoffset=iyoffset, ixcompress=ixcompress, $
		iycompress=iycompress, ixsize=ixsize, error=error

; Translate from old region coords in 'p' to the local image coords parameters and create a new region
; output 'p2' suitable for the local image state (compressions and offsets).

	if n_elements(ixoffset) eq 0 then ixoffset = 0
	if n_elements(iyoffset) eq 0 then iyoffset = 0
	if n_elements(ixcompress) eq 0 then ixcompress = 1
	if n_elements(iycompress) eq 0 then iycompress = 1
	error = 1

	copy_pointer_data, p, p2, /init

	if n_elements(ixsize) eq 0 then return, p2
	if ixsize eq 0 then return, p2

; Only translate pmark for mode=0. For mode=1 the pmark is in element-element
; space in Corr window.
; First find x,y in global sample coords ...

	if (*p).mode eq 0 then begin
		if ptr_good((*p).pmark[0]) then begin								; 3/4/23  @4-23
			x0 = (*p).xoffset + (*(*p).pmark[0]).x * (*p).xcompress
			y0 = (*p).yoffset + (*(*p).pmark[0]).y * (*p).ycompress
		
;			Translate into local image coords ...

			(*(*p2).pmark[0]).x = (x0 - ixoffset) / ixcompress
			(*(*p2).pmark[0]).y = (y0 - iyoffset) / iycompress
		endif

;		If the alt shape is defined, translate those too ...

		if (*p).analyze_type[1] gt 0 then begin
			if ptr_good((*p).pmark[1]) then begin								; 3/4/23
				x1 = (*p).xoffset + (*(*p).pmark[1]).x * (*p).xcompress
				y1 = (*p).yoffset + (*(*p).pmark[1]).y * (*p).ycompress
				(*(*p2).pmark[1]).x = (x1 - ixoffset) / ixcompress
				(*(*p2).pmark[1]).y = (y1 - iyoffset) / iycompress
			endif 
		endif
	endif

; Translate the 'q' index array. First find its original local coords
; (offset to non-zero entries to save space), form a mask array and resize
; it using the original compress.

	q_to_xy, *(*p).q, (*p).nx, x,y
	mx = min(x)
	my = min(y)
	nx = max(x)-mx+1
	ny = max(y)-my+1
	b0 = bytarr(nx,ny)
	q = where( (x-mx ge 0) and (x-mx lt nx) and (y-my ge 0) and (y-my lt ny), nq)
	if nq gt 0 then b0[x[q]-mx,y[q]-my] = 1
	if ((*p).xcompress gt 1) or ((*p).xcompress gt 1 ) then begin
		mx = mx * (*p).xcompress
		my = my * (*p).ycompress
		nx = nx * (*p).xcompress
		ny = ny * (*p).ycompress
		b0 = rebin(b0, nx, ny, /sample)
	endif 
	
; Find the uncompressed true coords 'xl', 'yl' ...

	q0 = where( b0 eq 1, nq0)
	if nq0 eq 0 then goto, err
	q_to_xy, q0, nx, x,y
	x2 = x + mx + (*p).xoffset
	y2 = y + my + (*p).yoffset
	xl = (x2 - ixoffset) / ixcompress
	yl = (y2 - iyoffset) / iycompress
	
; Now form the new 'q' array, pass back as ptr 'pq' ...

	nx2 = ixsize
	ny2 = max(yl)+1
	if (nx2 lt 1) or (ny2 lt 1) then goto, err
	b2 = bytarr(nx2,ny2)
	q = where( (xl ge 0) and (xl lt nx2) and (yl ge 0) and (yl lt ny2), nq)
	if nq gt 0 then b2[xl[q],yl[q]] = 1
	
; Now the final q index array ...

	q1 = where( b2 eq 1, nq1)	
	if nq1 eq 0 then goto, err
	if ptr_valid( (*p2).q) then ptr_free, (*p2).q
	(*p2).q = ptr_new( q1, /no_copy)
	error = 0
	return, p2
	
err:
	print,'translate_region: no valid region pixels.'
	(*p2).q = ptr_new( -1, /no_copy)						; 2/8/19
	error = 0												; 2/8/19
	return, p2
end

;-----------------------------------------------------------------------------------------

function translate_region, p, pstate, error=error

; Translate from old region coords in 'p'
; to the local image coords, as in (*pstate).p

; Translate region pmark x,y arrays and q pointer array from a set
; read into the region window 'p' to the local image coordinates as
; in '(*pstate).p' and return as a ptr to a p-like struct 'p2'. 
; Also find the new 'q' array translated from the old in 'p' and return
; in (*p2).q
;
; This is to allow a region set to be applied and used on Windowed or compressed
; image version of the original images, or vica versa.

	error = 1
	if n_elements(p) eq 0 then goto, bad
	if ptr_good(p[0]) eq 0 then goto, bad
	pimg = (*pstate).p
	if ptr_good(pimg) eq 0 then goto, bad

	p2 = do_translate_region( p, ixoffset=(*pimg).xoffset, iyoffset=(*pimg).yoffset, ixcompress=(*pimg).xcompress, $
		iycompress=(*pimg).ycompress, ixsize=(*pimg).xsize, error=error)

	return, p2

bad:
	print,'translate_region: invalid pointer.'
	return, ptr_new()
end
