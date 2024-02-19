function round_kernel, w

; Form round convolution kernel of width 'w'

COMPILE_OPT STRICTARR

	n = round( long(w) ) > 1
	sz = n
	b = bytarr(sz,sz)

	q = indgen(sz*sz)
	q_to_xy, q, sz, x,y
	
	c = float(sz-1)/2.
	q = where( (x-c)^2 + (y-c)^2 lt float(sz/2.)^2, nq)
	b[q] = 1B
	
	return, b
end
