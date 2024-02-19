function mode, y, threshold=threshold

;	Statistical 'mode', ignoring anything below 'threshold'

	if n_elements(threshold) lt 1 then threshold = 0.01*mean(y)
	
	h = histogram(y, nbins=200, reverse_indices=index, locations=x, omin=omin, omax=omax)
	
	q = where( x gt threshold, nq)
	if nq eq 0 then return, 0.

	q2 = reverse(sort(h[q]))
	h2 = h[q[q2]]
	x2 = x[q[q2]]
	
	return, x2[0]
end
