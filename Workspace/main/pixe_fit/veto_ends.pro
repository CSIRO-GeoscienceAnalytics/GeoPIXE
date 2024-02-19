pro veto_ends, spec, low, high, error=error

;	Move the end limits in if there are zero channels out there

	COMPILE_OPT STRICTARR
	n = n_elements(spec)
	if n lt 2 then begin
		low = 0
		high = 0
		return
	endif
	if finite(low) eq 0 then low = 2
	if finite(high) eq 0 then high = n-2
	low = (low > 5) < (n-1)
	high = (high < (n-5)) > low

	q = where( spec[low:high] gt 0.1)
	if q[0] eq -1 then begin
		error = 1								; no non-zero channels
		return
	endif

	low = low > (low + min(q))
	high = high < (low + max(q))

	if low gt high-30 then begin
		error = 1								; spectrum too short or null
		return
	endif
	error = 0
	return
	end
