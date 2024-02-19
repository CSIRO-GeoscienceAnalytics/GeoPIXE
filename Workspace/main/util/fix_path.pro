function fix_path, path, force=force

; Make sure a path has a valid separator at the end.
; Also accept '\' and '/' as separators for all o/s.
; If 'force' present, then use this as final path separator.

COMPILE_OPT STRICTARR

	if n_elements(path) eq 0 then return, ''
	ps = reform(byte([path_sep(),'/','\']))
	new = ps[0]
	if n_elements(force) gt 0 then new=(byte(force))[0]

	np = n_elements(path)
	if np gt 1 then begin
		f = ''
		for i=0,np-1 do begin
			f = [f,fix_path(path[i])]
		endfor
		return, f[1:*]
	endif

	s = strtrim(path,2)
	b = byte(s)
	n = n_elements(b)
	if n ge 1 then begin					; ensure path has proper ending
		q = where( b[n-1] eq ps, nq)
		if nq eq 0 then begin
			b = [b, new]
		endif
		n = n_elements(b)
		for i=0,n-1 do begin
			q = where( b[i] eq ps, nq)		; replace all separators w/ norm for this platform
			if nq ne 0 then begin
				b[i] = new
			endif
		endfor
		path = string(b)
	endif
	
	return, path
end
