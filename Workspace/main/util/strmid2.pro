function strmid2, f, start, length
;
; Call 'strmid' but treat case of (equal) vectors, which it stuffs up!
; This call permits 'f' and 'length' as vectors of the same length.

COMPILE_OPT STRICTARR

	n = n_elements(f)
	if n lt 1 then return,''
	s = strarr(n)
	ns = n_elements(start)
	if ns eq 0 then begin
		start = 0
		ns = 1
	endif
	start2 = start
	if ns lt n then start2 = replicate( start[0],n)
	nl = n_elements(length)

	if nl ge 1 then begin
		length2 = length
		if nl lt n then length2 = replicate( length[0],n)

		for i=0,n-1 do begin
			s[i] = strmid( f[i], start2[i],length2[i])
		endfor
	endif else begin
		for i=0,n-1 do begin
			s[i] = strmid( f[i], start2[i])
		endfor
	endelse

	return, s
end



