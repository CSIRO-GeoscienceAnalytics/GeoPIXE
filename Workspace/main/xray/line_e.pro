function line_e, z, line
;
;	Return energy of X-ray line, where 'line' is a Siegban line mnemonic
;	(see 'line_id' for mnemonics)

common c_line_list_1, element_start, line_list, n_line_list
common c_atomic_1, periodic_table, n_periodic

	if n_elements(n_periodic) lt 1 then t = atomic_number('He')
	if n_elements(n_line_list) lt 1 then n_line_list = 0
	if n_line_list lt 1 then build_line_list, 0.0001

	n = n_elements(z)
	if n lt 1 then return, 0.0
	e = fltarr(n)
	q = where((z gt 0) and (z lt n_periodic), nq)
	if q[0] eq -1 then return, e
	
	for j=0L,nq-1 do begin
		i = element_start(z[q[j]])
		
		if i ge 0 then begin
			while (line_list.z[i] eq z[q[j]]) and (i lt n_line_list) do begin
				if line eq line_list.line[i] then begin
					e[q[j]] = line_list.e[i]
					goto, out
				endif
				if i ge n_line_list-1 then goto, out
				i = i+1
			endwhile
out:
		endif
	endfor
			
	if n eq 1 then e=e[0]
	return, e
end
