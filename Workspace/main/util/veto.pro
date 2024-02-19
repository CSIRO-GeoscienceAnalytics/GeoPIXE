	function veto, q2, q
;
;	Veto indices 'q2' in 'q'
;
;	Go through 'q' and veto any indices that are also in 'q2'
;
COMPILE_OPT STRICTARR

	if n_elements(q) lt 1 then return, -1L
	if n_elements(q2) lt 1 then return, q
	if q2[0] eq -1 then return, q

	n = max(q)+1
	if n lt 1 then return, -1L

	mask = bytarr(n)
	mask[q] = 1
	q3 = where(q2 lt n, nq3)
	if nq3 gt 0 then mask[q2[q3]]=0

	q4 = where(mask eq 1)
	return, q4
	end
