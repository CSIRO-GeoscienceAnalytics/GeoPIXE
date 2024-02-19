function shift_image_rows, img, dx, x=x, y=y, odd=odd, even=even

; Shift alternate rows of an image by dx in X or Y
;	/x		shift in X (default)
;	/y		shift in Y
;	/odd	shift odd rows
;	/even	shift even rows (default)

	if n_elements(x) lt 1 then x=0
	if n_elements(y) lt 1 then y=0
	if n_elements(odd) lt 1 then odd=0
	if n_elements(even) lt 1 then even=0
	if y eq 0 then x=1
	if odd eq 0 then even=1

	sx = n_elements(img[*,0])
	sy = n_elements(img[0,*])

	if y then goto, y_shift
	
	nx = floor(dx)				; whole pixel parts (fix() later, when move fractional part too)
	fx = dx - nx				; fractional shifts (should be always positive)
	
	if even then begin
		q = 2*indgen((sy+1)/2)
	endif else begin
		q = 1 + 2*indgen(sy/2)
	endelse
	
	img1 = img
	img2 = img
	img1[*,q] = shift( img[*,q], nx,0) 
	if nx gt 0 then begin
		for i=0,nx-1 do img1[i,q] = img1[nx,q]
	endif else if nx lt 0 then begin
		for i=sx+nx,sx-1 do img1[i,q] = img1[sx+nx-1,q]
	endif
	nx2 = nx+1
	img2[*,q] = shift( img[*,q], nx2,0) 
	if nx2 gt 0 then begin
		for i=0,nx2-1 do img2[i,q] = img2[nx2,q]
	endif else if nx2 lt 0 then begin
		for i=sx+nx2,sx-1 do img2[i,q] = img2[sx+nx2-1,q]
	endif
	img1[*,q] = ( img1[*,q]*(1.-fx) + img2[*,q]*fx)	
	return, img1
	
y_shift:
	ny = floor(dx)				; whole pixel parts (fix() later, when move fractional part too)
	fy = dx - ny				; fractional shifts (should be always positive)
	
	if even then begin
		q = 2*indgen((sx+1)/2)
	endif else begin
		q = 1 + 2*indgen(sx/2)
	endelse
	
	img1 = img
	img2 = img
	img1[q,*] = shift( img[q,*], 0,ny) 
	if ny gt 0 then begin
		for i=0,ny-1 do img1[q,i] = img1[q,ny]
	endif else if ny lt 0 then begin
		for i=sy+ny,sy-1 do img1[q,i] = img1[q,sy+ny-1]
	endif
	ny2 = ny+1
	img2[q,*] = shift( img[q,*], 0,ny2) 
	if ny2 gt 0 then begin
		for i=0,ny2-1 do img2[q,i] = img2[q,ny2]
	endif else if ny2 lt 0 then begin
		for i=sy+ny2,sy-1 do img2[q,i] = img2[q,sy+ny2-1]
	endif
	img1[q,*] = ( img1[q,*]*(1.-fy) + img2[q,*]*fy)	
	return, img1
end
