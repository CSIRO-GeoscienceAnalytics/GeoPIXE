
;	Convert list (or list of lists) to simple data-type arrays.

function List_To_Normal, h
	COMPILE_OPT STRICTARR	

	if is_a_list(h) eq 0 then return,h
	if n_elements(h) eq 0 then return,['']
	flip = 0

	for i=0,n_elements(h)-1 do begin
		if is_a_list(h[i]) then begin
			d = transpose( list_to_normal( h[i]))
			flip = 1
		endif else begin
			d = h[i]
		endelse
		if n_elements(d) gt 0 then begin
			if i eq 0 then begin
				s = d
			endif else begin
				s = [s,d]
			endelse
		endif else print, 'list_to_normal:  bad element ="' + string(d) + '"'
	endfor
	if flip then s = transpose(s)
	return, s
end
