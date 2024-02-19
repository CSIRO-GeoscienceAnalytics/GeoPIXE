function include_shell, zi, shelli, e_min, e_max, list=list, $
						min_rel=min_rel, e=e, rel=rel, photo=photo

; 	returns a logical true (1) if any of the lines of the shell of element
;	of atomic number 'z' are in the range e_min to e_max keV.
;	Don't include a shell if the major line is excluded.

;	The list of indices for all lines in the shell can be returned
;	in 'list'. Also 'e' and 'rel'.
;	N.B. This only returns the list (and e,rel) for the first (z,shell).
;	List only those above 'min_rel'.

	if n_elements(min_rel) lt 1 then min_rel = 0.0001
	if n_elements(shelli) lt 1 then shelli = 1
	if n_elements(e_min) lt 1 then e_min = 1.0
	if n_elements(e_max) lt 1 then e_max = 100.0
	z = zi
	shell = shelli

	nz = n_elements(z)
	ns = n_elements(shell)
	nn = nz

	if (nz lt 1) or (ns lt 1) then return, 0.0
	if nz eq 1 then begin
		nn = ns
		z = replicate(Z,nn)
	endif else if ns eq 1 then begin
		nn = nz
		shell = replicate(shell,nn)
	endif else if nz ne ns then begin
		print,'include_shell: Z,Shell can only both be vectors if they have same length.'
		inc = intarr(nn)
		goto, done
	endif
	inc = intarr(nn)

	for i=nn-1,0,-1 do begin
		list = list_line_index( z[i], shell[i], photo=photo )	; descending rel-int order
		e = e_line( z[i], list)
		rel = relative_intensity( z[i], list, photo=photo)

		q = where((e ge e_min) and (e le e_max) and (rel gt min_rel))
		if q[0] eq -1 then begin
			inc[i] = 0
			e = 0.0
			rel = 0.0
			list = 0
		endif else begin
			inc[i] = 1
			e = e[q]
			rel = rel[q]
			list = list[q]
		endelse
		q = where( major_line(z[i],shell[i]) eq list )
		if q[0] eq -1 then begin
			inc[i] = 0
			e = 0.0
			rel = 0.0
			list = 0
		endif
	endfor

done:
 	s = size(inc)
	if s[0] gt 0 then inc = reform(inc)
	if n_elements(inc) eq 1 then inc = inc[0]
	return, inc
end
