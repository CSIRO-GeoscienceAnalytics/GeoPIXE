function where_tokens, tokens, list, nq, nocase=nocase, q=old_q, exclude=nott

; Search for the existence of the string fragments 'tokens' in strings 'list'.
; /nocase	for case-insensitive.
; /exclude	return indices for NOT tokens
; if 'q' provided, merge these indices with q result.

	nq = 0L
	if n_elements(tokens) lt 1 then return, -1L
	if n_elements(list) lt 1 then return, -1L
	if n_elements(nocase) lt 1 then nocase=0
	if n_elements(nott) lt 1 then nott=0
	
	mask = intarr(n_elements(list))
	items = nocase ? strlowcase(list) : list
	
	for i=0L,n_elements(tokens)-1 do begin
		token = nocase ? strlowcase(tokens[i]) : tokens[i]
	
		pos = strpos( items, token)
		q1 = where( pos ge 0, nq1)
		if nq1 gt 0 then mask[q1]=1
	endfor
	
	q = where( mask eq (nott eq 0), nq)

	if n_elements(old_q) gt 0 then begin
		q = [q, old_q]
		qq = where( q ne -1, nqq)
		if nqq gt 0 then begin
			q2 = sort_unique(q[qq])
			q = q[qq[q2]]
			nq = n_elements(q)
		endif else begin
			q = -1L
			nq = 0L
		endelse
	endif
	return, q
end
