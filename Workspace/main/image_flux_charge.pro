pro image_flux_charge, p, xanes=xanes, charge=charge, pixels=pixels, $
					conv=conv, flux=flux, RT_mode=RT_mode, error=error

; Update temp struct in 'image' struct *p to reflect flux, conv, charge
; if (*p).temp.valid eq 0, else just return total 'charge', number of 'pixels'
; and total 'flux' and current conversion factor 'conv'.
; 
; (*p).temp struct:
; 	valid			temp struct is up to date
; 	charge_map		charge map (excludes bounds, if valid)
; 	total_pixels	total number of used (flux ne 0) pixels
; 	total_flux		total flux
; 	
; Take care if 'p' is a XANES stack and not an image.
; Do Not use this in every RT image update, as it is unecessary overhead.
; For PIXE using a flux map, need to make sure that IC.conversion=1.0

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
		warning,'image_flux_charge',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	error = 1
	RT_mode = 0
	if (*p).charge eq 0.0 then RT_mode=1
	if ptr_good(p,/struct) eq 0 then return
	if n_elements(xanes) eq 0 then xanes=0
	tconv = (((*p).IC.mode eq 0) ? 1.0 : (*p).IC.conversion)
	if (*p).temp.valid then begin
		charge = (*p).temp.charge
		pixels = (*p).temp.total_pixels
		conv = (*p).IC.conversion
		flux = (*p).temp.total_flux
		error = 0
		return
	endif
	nx = (*p).xsize
	ny = (*p).ysize
	
	if (*p).IC.conversion eq 0.0 then begin				; PIXE defaults
		(*p).IC.mode = 0
		tconv = 1.0
	endif
	
; Total_pixels refers to main 'image' array, not 'error'.
; For a stack, charge is for a single frame in stack.
; '(*p).charge' zero indicates RT mode, which does not support stacks.

	if ptr_valid((*p).temp.charge_map) then ptr_free, (*p).temp.charge_map
	(*p).temp.charge_map = ptr_new( fltarr(nx,ny))
	if ptr_valid((*p).temp.flux_map) then ptr_free, (*p).temp.flux_map
	(*p).temp.flux_map = ptr_new( fltarr(nx,ny))

;	print, 'Image_flux_charge: Bounded pixels = ', long((*p).bounds.xmax - (*p).bounds.xmin +1)* long((*p).bounds.ymax - (*p).bounds.ymin +1) 	
;	help, (*p).bounds, /str
	
	if ((*p).bounds.valid eq 1) then begin
		max_pixels = long((*p).bounds.xmax - (*p).bounds.xmin +1)* long((*p).bounds.ymax - (*p).bounds.ymin +1) 	
		if max_pixels le 0 then goto, no_points
	endif else begin
		max_pixels = long(nx)* long(ny) 
	endelse
	
	if (*p).charge eq 0.0 then begin								; Maia/DAQ RT mode only
;		print, '   (*p).charge zero, RT mode ...'
		RT_mode = 1
		if xanes then goto, bad_xanes
		q1 = where( *(*p).el eq 'Flux', nq1)
		q2 = where( *(*p).el eq 'Charge', nq2)
		if (nq1 gt 0) and (n_elements(*(*p).image) gt 1) then begin										; realtime SXRF by flux
			q3 = where( (*(*p).image)[*,*,q1[0]] ne 0.0, nq3)
;			print, '   Non-zero "Flux" map pixels = ',nq3
			if nq3 gt 0 then begin
				nxy = long(nx)*long(ny)
				(*p).temp.total_flux = total( (*(*p).image)[ q1[0]*nxy + q3] )
				(*p).temp.total_pixels = nq3
				*(*p).temp.charge_map = (*(*p).image)[ *,*, q1[0]] * tconv
				*(*p).temp.flux_map = (*(*p).image)[ *,*, q1[0]]
				(*p).temp.charge = total(*(*p).temp.charge_map) 
			endif else begin
				(*p).temp.total_flux = 0.0
				(*p).temp.total_pixels = max_pixels
				(*p).temp.charge = 0.0
			endelse
			charge = (*p).temp.charge
			pixels = (*p).temp.total_pixels
			conv = (*p).IC.conversion
			flux = (*p).temp.total_flux
		endif else if nq2 gt 0 then begin							; realtime PIXE by charge map
			(*p).IC.mode = 0
			(*p).IC.conversion = 0.0
			q3 = where( (*(*p).image)[*,*,q2[0]] ne 0.0, nq3)
			if nq3 gt 0 then begin
				nxy = long(nx)*long(ny)
				(*p).temp.total_flux = total( (*(*p).image)[ q2[0]*nxy + q3] )
				(*p).temp.total_pixels = nq3
				*(*p).temp.charge_map = (*(*p).image)[ *,*, q2[0]] 
				*(*p).temp.flux_map = (*(*p).image)[ *,*, q2[0]] 
				(*p).temp.charge = total(*(*p).temp.charge_map) 
			endif else begin
				(*p).temp.total_flux = 0.0
				(*p).temp.total_pixels = max_pixels
				(*p).temp.charge = 0.0
			endelse
			charge = (*p).temp.charge
			pixels = (*p).temp.total_pixels
			conv = (*p).IC.conversion
			flux = (*p).temp.total_flux
		endif else if (*p).has_flux then begin					
			q = where( *(*p).flux ne 0.0, nq)
			if nq gt 0 then begin
				(*p).temp.total_flux = total( (*(*p).flux)[q] )
				(*p).temp.total_pixels = nq
				*(*p).temp.charge_map = *(*p).flux * tconv
				*(*p).temp.flux_map = *(*p).flux 
				(*p).temp.charge = total(*(*p).temp.charge_map) 
			endif else begin
				(*p).temp.total_flux = 0.0
				(*p).temp.total_pixels = max_pixels
				(*p).temp.charge = 0.0
			endelse
			charge = (*p).temp.charge
			pixels = (*p).temp.total_pixels
			conv = (*p).IC.conversion
			flux = (*p).temp.total_flux
		endif else begin
			(*p).temp.total_pixels = max_pixels
			charge = 0.0
			pixels = (*p).temp.total_pixels
			conv = 0.0
			flux = 0.0
		endelse
	endif else begin												; normal offline mode
		if (*p).has_flux then begin
			iz = xanes ? (((*p).zsize/2) < ((n_elements( (*(*p).flux)[0,0,*])-1)>0)) : 0
			q = where( (*(*p).flux)[*,*,iz] gt 0.0, nq)
			if nq gt 0 then begin
				(*p).temp.total_flux = total( (*(*p).flux)[q] )
				(*p).temp.total_pixels = nq
				*(*p).temp.charge_map = (*(*p).flux)[*,*,iz] * tconv
				*(*p).temp.flux_map = (*(*p).flux)[*,*,iz]
				(*p).temp.charge = total(*(*p).temp.charge_map) 
			endif else begin
				(*p).temp.total_flux = 0.0
				(*p).temp.total_pixels = max_pixels
				(*p).temp.charge = 0.0
			endelse
			charge = (*p).temp.charge
			pixels = (*p).temp.total_pixels
			conv = (*p).IC.conversion
			flux = (*p).temp.total_flux
		endif else begin
			charge = (*p).charge
			if (*p).bounds.valid then begin
				(*(*p).temp.charge_map)[(*p).bounds.xmin-(*p).xoffset:(*p).bounds.xmax-(*p).xoffset, $
							(*p).bounds.ymin-(*p).yoffset:(*p).bounds.ymax-(*p).yoffset] = charge/float(max_pixels)
			endif else begin
				(*(*p).temp.charge_map)[*,*] = charge/float(max_pixels)
			endelse
			(*p).temp.charge = total(*(*p).temp.charge_map) 
			(*p).temp.total_pixels = max_pixels 
			(*(*p).temp.flux_map)[*] = 0.0
			(*p).temp.total_flux = 0.0
			pixels = (*p).temp.total_pixels
			conv = (*p).IC.conversion
			flux = (*p).temp.total_flux
			charge = (*p).temp.charge
		endelse
	endelse
;	help, charge, pixels, conv, flux, /str
	(*p).temp.valid = 1
	error = 0

	return

bad_xanes:
	warning,'image_flux_charge','"Charge" should not be zero for a XANES stack.'
	return
no_points:
	warning,'image_flux_charge','"Bounds" excludes all points.'
	return
end
