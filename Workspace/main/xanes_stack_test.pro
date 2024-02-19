pro xanes_stack_test, p, xanes, n_el, el, el_xanes, z_found=z_found

; Test struct 'p' for a XANES tag. If there, this is an image stack
; if not, it is an image struct. Return 'n_el' and 'el' for stack from
; z_coords array. Take care for empty (but otherwise valid) image structs.
; Return at least one 'el' otherwise element droplists crash out.
;
; Call using:
;	xanes_stack_test, p, xanes, n_el, el, el_xanes, z_found=z_found

n_el = 1L
el = [' ']
el_xanes = ''
z_found = 0
stack_type = 0
if ptr_good( p) eq 0 then return

xanes = tag_present('XANES', *p)
if xanes then begin
	n_el = (*p).zsize
	if ptr_good((*p).pz_coords) then begin
		z_found = 1
		el = str_tidy(*(*p).pz_coords)		 	;	+ ' ' + (*p).z_coord_units
	endif else begin
		el = str_tidy(indgen((*p).zsize))
	endelse
	el_xanes = (*p).el
	stack_type = (*p).stack_type
endif else begin
	n_el = (*p).n_el > 1
	el = ptr_good((*p).el) ? *(*p).el : [' ']
endelse

return
end