pro image_correct_flux_stack, pimg, flux, IC, pv_list, charge=charge, flatten=flatten, random_subset=random_subset, $
							accept_fraction=accept_fraction

; Correct images for variation in flux, if /flatten is set.
; Flux is 3D over energy planes.
; Store flux array after normalization in pimg struct, along with IC details
; Calculate charge and place in img struct.
; Check if stack is 4D (for tomo, stack_type=1), and apply same flatten to all element 3D's:
; Done in 'image_flux_flatten_stack'.

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
		warning,'image_correct_flux_stack',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if ptr_good( pimg, /struct) eq 0 then return
	if n_elements(flux) eq 0 then return
	if n_elements(IC) eq 0 then return
	if n_elements(pv_list) eq 0 then return
	if n_elements(flatten) eq 0 then flatten=1
	if n_elements(charge) eq 0 then charge = 0.0
	if n_elements(random_subset) eq 0 then random_subset=0
	if n_elements(accept_fraction) eq 0 then random_subset=0
		
	total_flux = total(flux[*,*,*])
	(*pimg).has_flux = 0
	if ptr_valid((*pimg).flux) then ptr_free, (*pimg).flux
	
	(*pimg).IC.mode = IC.mode
	(*pimg).IC.conversion = (IC.mode eq 0) ? 0.0 : IC.conversion
	if tag_present('use_dwell',IC) then begin			; called from DA_evt_stack
		(*pimg).dwell.on = IC.use_dwell
		(*pimg).dwell.val = IC.dwell
		(*pimg).IC.pv.name = IC.pv
		(*pimg).IC.pv.val = IC.val
		(*pimg).IC.pv.unit = IC.unit
	endif else begin									; called from Maia_Launch (not likely?)
		(*pimg).IC.pv = IC.pv							; called from parallel gather /flatten
	endelse
	if ptr_valid((*pimg).plist) then begin
		*(*pimg).plist = pv_list
	endif else begin
		(*pimg).plist = ptr_new(pv_list)
	endelse

;	If there is a flux map, then it is charge (IC mode = 0) or needs conversion to charge.
;	Define here that total flux and charge is per energy plane (after normalization).
	
	if total_flux gt 1.0e-10 then begin
		if flatten then image_flux_flatten_stack, pimg, flux
		if IC.mode eq 0 then begin
			charge = total(flux[*,*,*])
		endif else begin
			if (*pimg).IC.conversion eq 0.0 then goto, bad_conv
			charge = total(flux[*,*,*]) * (*pimg).IC.conversion 
		endelse
		(*pimg).has_flux = 1
		(*pimg).flux = ptr_new(flux)
	endif
	
	if random_subset then begin
		charge = charge * accept_fraction
		if (*pimg).has_flux then begin
			*(*pimg).flux = *(*pimg).flux * accept_fraction
		endif
	endif
	(*pimg).charge = charge
	(*pimg).flatten = flatten
	return

bad_conv:
	warning,'image_correct_flux_stack','No (or zero) flux to charge conversion factor supplied.'
	return
end
