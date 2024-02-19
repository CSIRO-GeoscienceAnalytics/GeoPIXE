pro check_widget_vector, p

; Check widget IDs and eliminate bad ones

COMPILE_OPT STRICTARR
if n_elements(p) eq 0 then return
if ptr_valid(p) eq 0 then return

	n = n_elements(*p)
	if n eq 0 then return
	mask = intarr(n)
	for i=0,n-1 do begin
		mask[i] = widget_info( (*p)[i], /valid)
	endfor
	q = where(mask eq 1, nq)
	if nq eq 0 then begin
		*p = 0L
	endif else begin
		*p = (*p)[q]
	endelse
	return
end
