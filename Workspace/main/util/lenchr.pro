function lenchr, s

	n = n_elements(s)
	if n lt 1 then return, 0
	l = lonarr(n)

	for j=0L,n-1 do begin
		str = s[j]
		ns = strlen(str)
		ns = ns[0]
		if ns lt 1 then l[j] = 0
		b = byte(str)

		for i=ns-1,0,-1 do begin
			if b[i] gt 32 then begin
				l[j] = i+1
				goto, more
			endif
		endfor
		l[j] = 0
more:
	endfor

finish:
	if n eq 1 then l=l[0]
	return, l
	end