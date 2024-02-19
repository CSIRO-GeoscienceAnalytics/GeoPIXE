function gsmooth, imgi, w

; Doesn;t seem to be used.
; Does weird stuff for a 1D array.
; Use the IDL 'gauss_smooth' instead.

COMPILE_OPT STRICTARR
	img = reform(imgi)
	s = size(img)
	if (s[0] ne 2) and (s[0] ne 1) then return, img
	nx = s[1]
	ny = (s[0] eq 2) ? s[2]: 1
	n = 3 * fix(w + 0.5)
	n = 2*(n/2) + 1
	n = n > 3
	nx2 = nx + 2*n
	ny2 = ny + 2*n

;	Extend image on all edges

	img2 = extend_image( img, n)

;	Smooth using Gaussian kernel

	off = n/2
	kernel = fltarr(n,n)

	sum = 0.0
	for k=0L,n-1 do begin
	    for j=0L,n-1 do begin
	       term = exp( -0.693 * float((k-off)*(k-off) + (j-off)*(j-off)) / (0.25*w*w) )
	       kernel[k,j] = term
	       sum = sum + term
	    endfor
	endfor
	kernel = kernel/sum
	
;	Strip off extended edges

	img3 = convol( img2, kernel, /edge_truncate)
	img = img3[n:nx2-n-1,n:ny2-n-1]
	return, img
end




