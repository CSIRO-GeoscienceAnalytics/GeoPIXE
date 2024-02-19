pro add_widget_vector, p, wid, error=err

; Add another widget ID to a pointer to widget ID vector
; Check valid for each, and eliminate bad widgets.

COMPILE_OPT STRICTARR
err = 1
if n_elements(p) eq 0 then return
if n_elements(wid) eq 0 then return
if ptr_valid(p) eq 0 then return

	n = n_elements( *p)
	if n ge 1 then begin
		*p = [*p, wid]
	endif else begin
		*p = wid
	endelse
	
	check_widget_vector, p
	err = 0
	return
end
