function str_remove, sub, str

;	Remove string(s) 'sub' from string(s) 'str'

COMPILE_OPT STRICTARR

	ns = n_elements(str)
	nb = n_elements(sub)
	if ns eq 0 then return, ''
	if nb eq 0 then return, str

	lb = strlen(sub)
	ls = strlen(str)
	s = str

	for j=0,nb-1 do begin
		if lb[j] eq 0 then continue

		for i=0,ns-1 do begin
			p = locate(sub[j], s[i])
			while p ge 0 do begin
				if p[i] gt 0 then begin
					s1 = strmid( s[i], 0, p[i])
				endif else s1 = ''
		
				if p[i]+lb[j]-1 le ls[i]-1 then begin
					s2 = strmid( s[i], p[i]+lb[j], ls[i]-p[i]-lb[j]+1)
				endif else s2 = ''
		
				s[i] = s1+s2
				p = locate(sub[j], s[i])
			endwhile 
		endfor
	endfor

	return, s
end

