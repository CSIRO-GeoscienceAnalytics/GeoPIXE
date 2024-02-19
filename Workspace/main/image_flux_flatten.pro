pro image_flux_flatten, pimg, flux, raw=raw, scale=scale, cluster_pass=cluster_pass, use_FC_index=use_FC_index

;	Flatten images according to variation from the average of the flux,
;	ignoring any missing (or wildly divergent) pixels.
;
;	if 'flux' is 3D, then flatten elements to flux[*,*,0] and the extra
;	elements to raw.
;	scale			Return the scale factor map here

;	/cluster_pass	For second pass after cluster image reconstruction
;					which assumes all was flattened in stripes before.
;	use_FC_index	If not -1, indicates an attribute plane is being used as flux.
;					So use indexed attribute image to do secondary flatten
;					after cluster reconstruction.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'image_flux_flatten',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if ptr_good( pimg, /struct) eq 0 then return
	if n_elements(flux) eq 0 then return
	if n_elements(cluster_pass) eq 0 then cluster_pass=0
	if n_elements(use_FC_index) eq 0 then use_FC_index=-1
	
	if tag_present('n_attributes', *pimg) then begin
		n = (*pimg).n_attributes
		xanes_stack_test, pimg, xanes, n_el, el, el_xanes
		if xanes then return
	endif else begin
		n = 0
		n_el = n_elements((*(*pimg).image)[0,0,*])
	endelse
	
;	Flatten each element image (not 'attributes'), based on flux array,
;	which includes DT/PU effects.
			
	scale = flux_flatten( flux[*,*,0])
	for i=0L,n_el-1-n do begin
		(*(*pimg).image)[*,*,i] = (*(*pimg).image)[*,*,i] * scale
	endfor

;	Flatten 'flux' image the same way ...

	flux[*,*,0] = flux[*,*,0] * scale
	
;	Flatten extra image planes (attributes) ...  We don't want these to have DT/PU
;	effects in them, just flattened (normalized) to flux. Hence, use 'raw' unless
;	this is a second pass post-cluster flatten, in which case use the indexed attribute plane,
;	as stored in image array, which has been flattened already in stripes.
;	** See notes in 'image_correct_flux'.
			
	if n eq 0 then return
	if cluster_pass eq 0 then begin
		if n_elements(raw) eq 0 then begin
			warning,'image_flux_flatten','Extra planes, but Raw flux array missing.'
			return
		endif
		scale2 = flux_flatten( raw)
	endif else begin
		if use_FC_index ge 0 then begin
			scale2 = flux_flatten( (*(*pimg).image)[*,*,n_el-n + use_FC_index])
		endif else begin
			scale2 = scale
		endelse
	endelse
	for i=n_el-n,n_el-1 do begin
		(*(*pimg).image)[*,*,i] = (*(*pimg).image)[*,*,i] * scale2
	endfor
	
	return
end
