function gaussian_kernel, w

; Form round convolution kernel of width 'w'

COMPILE_OPT STRICTARR

	n = round(w) > 1
	n = 2*(n/2) + 1
	n = n > 3
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

	return, kernel
end