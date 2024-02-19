function detector_efficiency, pdetector, playout, e, effective=aEff, $
				solid_angle=tOmega

; Calculate the effective intrinsic efficiency, total efficiency (relative
; to 4.pi) and total solid-angle of the array at energy 'e', based on weighted
; average of all pads, using solid-angle of each as weights.
;
; This return for solid-angle differs from "solid_angle" routine, in that no
; 'vetoes' are applied here.

	aEff = 1.0
	tOmega = 0.0
	TotalEff = 0.0
	cleanup_layout = 0

	if n_elements(e) lt 1 then goto, done
	nen = n_elements(e)
	aEff = e
	TotalEff = e
	aEff[*] = 1.0
	TotalEff[*] = 0.0

	if n_elements(pdetector) eq 0 then goto, done
	if ptr_valid(pdetector) eq 0 then goto, done
	if (n_elements(*pdetector) eq 0) then begin
		warning,'detector_efficiency','detector pointer empty.',/error
		goto, bad
	endif

	array = (*pdetector).array

; Calculate the	generic detector element (e.g. one pad in centre of array) solid-angle ...

	omega = solid_angle( (*pdetector).distance, (*pdetector).diameter, tilt=(*pdetector).tilt, $
							array=0, shape=(*pdetector).shape)

; Single detector ...

	if (array eq 0) then begin

		aEff = det_eff( pdetector, e, gamma=(*pdetector).pige)
		TotalEff = aEff
		tOmega = omega

		if (*pdetector).pige eq 0 then begin
			TotalEff = aEff * omega / (4000.*!pi)
		endif

		return, TotalEff
	endif

; Array detector ...

	get_layout = 0
	if (n_elements(playout) eq 0) then begin
		get_layout = 1
	endif else begin
		if ptr_valid( playout) eq 0 then begin
			get_layout = 1
		endif else begin
			if (n_elements(*playout) eq 0) then get_layout=1
			if size(*playout,/tname) ne 'STRUCT' then get_layout=1
		endelse
	endelse
	if get_layout then begin
		if (strlen((*pdetector).layout) gt 0) then begin
			d = read_detector_layout((*pdetector).layout, error=error)
			if error eq 0 then begin
				playout = ptr_new(d)
				cleanup_playout = 1
			endif else begin
				warning,'detector_efficiency','error reading layout: '+(*pdetector).layout
				goto, bad
			endelse
		endif
	endif
	if ptr_good(playout,/struct) eq 0 then goto, bad
	n_det = (*playout).N

; Build array geometry details ...

	g = detector_geometry( playout, (*pdetector).distance, 90., $
								0., tilt=(*pdetector).tilt, error=error)
	if error then begin
		warning,'detector_efficiency','error from "detector_geometry"'
		goto, bad
	endif

; Solid-angle for individual pads rOmega ...

	rOmega = fltarr(n_det)
	darea = (*playout).data.width * (*playout).data.height
	if (*pdetector).shape eq 0 then darea = darea * !pi/4.

	rOmega = 1000. * (darea*cos(g.tilt/!radeg)/(g.R*g.R))

; Efficiency ratios ...

;	This does follow the detector contours, and uses the local tilt - in 'g'.
;	Need to calculate these for local tilt. Do this by copying detector struct, and modifying
;	tilt and distance (weak effect on some geometry corrections).

	Eff = fltarr(nen,n_det)
	det = *pdetector
	for i=0L,n_det-1 do begin
		det.distance = g[i].R
		det.tilt = g[i].tilt
		Eff[*,i] = det_eff( det, e)
;		if nen gt 1 then begin
;			print, '.'
;		endif
	endfor

; Form weighted average of efficiencies, and total efficiency ...

	tOmega = 0.0
	aEff[*] = 0.0
	for i=0L,n_det-1 do begin
		aEff = aEff + Eff[*,i] * rOmega[i]
		tOmega = tOmega + rOmega[i]
	endfor

; Total efficiency, relative to 4 pi ...

	TotalEff = aEff
	aEff = TotalEff / tOmega

	TotalEff = TotalEff / (4000. * !pi)

done:
	if cleanup_layout then ptr_free, playout
	return, TotalEff

bad:
	tOmega = 0.0
	aEff = 0.0
	return, 0.0
end
