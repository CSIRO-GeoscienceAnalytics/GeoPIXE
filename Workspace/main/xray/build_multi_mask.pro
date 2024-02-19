function build_multi_mask, Thick=T, pitch=pitch, gap=gap, R=R, w=w, Mo=Mo, $
				tune=tune, grow=grow, nmask=nmask, safety=safety, $
				holes=holes, glue=glue, no_tube=no_tube, nx=nx, ny=ny

; Build a multi-layered mask
; returns		mask array
; 	holes		holes array
; 
; This version knows about the new gap model, which asymptotes to
; an effective gap of 30 microns on the front face.
;
;	Thick		thickness of the detector wafer (mm)
;	pitch		pitch between detector pads (mm)
;	nx			number of pads in X (half of full nx)
;	ny			number of pads in Y (half of full ny)
;	gap			gap between pads (mm)
;	R			distance from beam spot to detector wafer
;				(N.B. Mask always made for R=10 mm)
;	w			thickness of each mask layer
;	nmask		number of mask layers
;				Note that "calc_charge_sharing" is limited to 6 layers of mask.
;	glue		thickness (mm) of glue layer between mask layers and between masks and wafer
;	Mo			material composition (pure element) of mask
;	tune		scale the mask offset on trailing edge of mask
;	grow		scale the mask offset on leading edge
;	safety		safety margin distance (mm) for leading/trailing edges
;	/no_tube		do not veto central detectors for a Mo tube

compile_opt strictarr
if n_elements(T) lt 1 then T=0.4
if n_elements(pitch) lt 1 then pitch=1.0
if n_elements(gap) lt 1 then gap=0.075
if n_elements(R) lt 1 then R=10.0
if n_elements(w) lt 1 then w=0.1
if n_elements(nmask) lt 1 then nmask=3
if n_elements(Mo) lt 1 then Mo='Mo'
if n_elements(glue) lt 1 then glue=0.025
if n_elements(tune) lt 1 then tune=1.03
if n_elements(grow) lt 1 then grow=0.892
if n_elements(safety) lt 1 then safety=0.01
if n_elements(no_tube) lt 1 then no_tube=0						; 1 for Maia 96
if n_elements(nx) lt 1 then nx=10								; 4 for Maia 96
if n_elements(ny) lt 1 then ny=10								; 6 for Maia 96
if nmask gt 6 then warning,'test_charge_sharing','mask layers exceed 6.'

offset = glue
mask = build_mask2( Thick=T, pitch=pitch, gap=gap, R=R, w=w, Mo=Mo, tune=tune, grow=grow, $
		safety=safety, aperture=holes, offset=offset, level=0L, no_tube=no_tube, $
		nx=nx, ny=ny)
if nmask gt 1 then begin
	for i=1L,nmask-1 do begin
		offset = offset + w + glue
		mask2 = build_mask2( Thick=T, pitch=pitch, gap=gap, R=R, w=w, offset=offset, Mo=Mo, $
				tune=tune, grow=grow, safety=safety, aperture=holes2, level=i, $
				no_tube=no_tube, nx=nx, ny=ny )
		mask = [mask, mask2]
		holes = [holes, holes2]
	endfor
endif

return, mask
end
