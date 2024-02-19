pro image_correct_flux, pimg, flux, IC, pv_list, charge=charge, flatten=flatten, random_subset=random_subset, $
							raw_flux=raw_flux, accept_fraction=accept_fraction, cluster_pass=cluster_pass

; Correct images for variation in flux, if /flatten is set
; Store flux array in pimg struct, along with IC details
;
; Calculate charge (unless no flux, then used input charge) and place in pimg struct
; If 'flux' is 3 plane, then extra planes only get normalized to raw_flux.
;
;	/cluster_pass	for second pass after cluster image reconstruction
;					which assumes all was flattened in stripes before

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
		warning,'image_correct_flux',['IDL run-time error caught.', '', $
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
	if n_elements(cluster_pass) eq 0 then cluster_pass=0
		
	total_flux = total(flux[*,*,0])
	(*pimg).has_flux = 0
	if ptr_valid((*pimg).flux) then ptr_free, (*pimg).flux
	if ptr_valid((*pimg).raw_flux) then ptr_free, (*pimg).raw_flux
	
	(*pimg).IC.mode = IC.mode
	(*pimg).IC.conversion = (IC.mode eq 0) ? 0.0 : IC.conversion
	if tag_present('use_dwell',IC) then begin			; called from DA_evt
		(*pimg).dwell.on = IC.use_dwell
		(*pimg).dwell.val = IC.dwell
		(*pimg).IC.pv.name = IC.pv
		(*pimg).IC.pv.val = IC.val
		(*pimg).IC.pv.unit = IC.unit
	endif else begin									; called from Maia_Launch
		(*pimg).IC.pv = IC.pv							; called from parallel gather /flatten
	endelse
	if ptr_valid((*pimg).plist) then begin
		*(*pimg).plist = pv_list
	endif else begin
		(*pimg).plist = ptr_new(pv_list)
	endelse

;	Look up the added PVs added to the PV list for the added 'attributes' image planes.
;	The added 'attributes' are always at the front of the 'plist' after 'check_pv_list'.
;	If the selected PV is one of these, then pass the FC index to the flatten routines,
;	so that the second pass post-cluster reconstruction can use this plane (already flattened
;	in stripes) to do final post-cluster flatten.

	use_FC_index = -1
	pv_list = *(*pimg).plist
	(*pimg).DevObj->check_pv_list, pv_list
	q = where( (*pimg).IC.pv.name eq pv_list, nq)
	if (nq gt 0) and (q[0] lt (*pimg).n_attributes) then use_FC_index = q[0]

;	If there is a flux map, then it is charge (IC mode = 0) or needs conversion to charge.
	
	if total_flux gt 1.0e-10 then begin
		if flatten then image_flux_flatten, pimg, flux, raw=raw_flux, scale=scale, cluster_pass=cluster_pass, use_FC_index=use_FC_index
		if IC.mode eq 0 then begin
			charge = total(flux[*,*,0])
		endif else begin
			if (*pimg).IC.conversion eq 0.0 then goto, bad_conv
			charge = total(flux[*,*,0]) * (*pimg).IC.conversion
		endelse
		(*pimg).has_flux = 1
		(*pimg).flux = ptr_new(flux[*,*,0])
		(*pimg).raw_flux = ptr_new(raw_flux)
;		(*pimg).scale = ptr_new(scale)
	endif
	
	if random_subset then begin
		charge = charge * accept_fraction
		if (*pimg).has_flux then begin
			*(*pimg).flux = *(*pimg).flux * accept_fraction
			*(*pimg).raw_flux = *(*pimg).raw_flux * accept_fraction
		endif
	endif
	(*pimg).charge = charge
	(*pimg).flatten = flatten
	return

bad_conv:
	warning,'image_correct_flux','No (or zero) flux to charge conversion factor supplied.'
	return
end
