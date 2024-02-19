pro image_flux_flatten_stack, pimg, flux, base=base

;	Flatten images according to variation from the average of the flux,
;	ignoring any missing (or wildly divergent) pixels.
;	Check if stack is 4D (for tomo, stack_type=1), and apply same flatten to all element 3D's:
;	/tomo	indicates a different plane order for tomo processing efficiency.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'image_flux_flatten_stack',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if ptr_good( pimg, /struct) eq 0 then return
	if n_elements(flux) eq 0 then return
		
;	For a tomo stack, norm each element's 3D to same flux scale.

	case (*pimg).stack_type of
		0: begin
			n_energy = n_elements((*(*pimg).image)[0,0,*])
			n_el = 1
			end
		1: begin
			n_energy = n_elements((*(*pimg).image)[0,0,0,*])
			n_el = n_elements((*(*pimg).image)[0,0,*,0])
			end
		2: begin
			n_energy = n_elements((*(*pimg).image)[0,0,*])
			n_el = 1
			end
		else:
	endcase

	if n_elements(flux[0,0,*]) ne n_energy then begin
		warning,'image_flux_flatten_stack','Flux and image arrays inconsistent depths.'
		return
	endif
	
;	Need to respect different memory organization for 'image' in XANES and Tomo modes ...

	scale = flux_flatten( flux, base=base)
	for i=0, n_el-1 do begin
		case (*pimg).stack_type of
			0: begin
				(*(*pimg).image)[*,*,*,i] = (*(*pimg).image)[*,*,*,i] * scale
				end
			1: begin
				(*(*pimg).image)[*,*,i,*] = (*(*pimg).image)[*,*,i,*] * scale
				end
			2: begin
				(*(*pimg).image)[*,*,*,i] = (*(*pimg).image)[*,*,*,i] * scale
				end
			else:
		endcase
	endfor
	flux = flux * scale	
	return
end
