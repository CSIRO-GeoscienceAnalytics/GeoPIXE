pro widget_control_vector, p, first=first, error=err, _ref_extra=extra

; Use 'widget_control' on members of a vector of widget IDs
; pointed to by 'p'.
; /first	only apply to first in vector list

COMPILE_OPT STRICTARR
err = 1
if n_elements(p) eq 0 then return
if ptr_valid(p) eq 0 then return
if n_elements(*p) eq 0 then return
if n_elements(first) eq 0 then first=0
		
	check_widget_vector, p
	
	for i=0,n_elements(*p)-1 do begin
		if widget_info( (*p)[i], /valid) then begin
			widget_control, (*p)[i], _strict_extra=extra
			err = 0
			if first then break
		endif
	endfor
	return
end
