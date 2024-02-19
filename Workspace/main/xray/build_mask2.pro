function build_mask2, Thick=T, pitch=pitch, gap=gap, R=R, w=w, Mo=Mo, level=level, $
					tune=tune, grow=grow, aperture=aperture, offset=offset, safety=safety, $
					no_tube=no_tube, nx=nx, ny=ny

; Build an array of mask 'boxes' for detector mask volumes
; Only build a quadrant, and veto inner pads for Mo tube unless /no_tube set.
;
;	Thick		thickness of the detector wafer (mm)
;	pitch		pitch between detector pads (mm)
;	nx			number of pads in X (half of full nx)
;	ny			number of pads in Y (half of full ny)
;	gap			gap between pads (mm)
;	R			distance from beam spot to detector wafer
;	w			thickness of mask
;	offset		offset in Z for 'suspended' mask, which is part of a
;				stack of multiple mask layers
;	level		number label for a mask in a stack
;	Mo			material composition (pure element) of mask
;	tune		scale the mask offset on trailing edge of mask
;	grow		scale the mask offset on leading edge of mask
;	safety		safety margin distance (mm) for leading/trailing edge
;		
;	The leading edge leads to signal in gap after passage through
;	pad, which only is significant at high energies.
;	The trailing edge needs to be tuned lower to allow a detector
;	built for one R (e.g. 10) to be used at larger R (e.g. 15).
;	
; Output:
;	return		array of boxes covering mask volume
;	aperture	array of low,high extent, in X,Y, of the apertures in the mask
;
; Format of each output mask element:
;	box = { x:[-3.,-1.], y:[-2.,2.], z:[-1.,0.], material:'Si', IDx:0, IDy:0, order:0.0 }
;
;	x,y,z		give dimensions of box on x,y,z axes
;	material	is the composition of the box
;	IDx, IDy	are the x,y indices for a detector in array
;	order		is a priority order, giving the radial distance from origin

compile_opt strictarr
if n_elements(T) lt 1 then T=0.4
if n_elements(pitch) lt 1 then pitch=1.0
if n_elements(gap) lt 1 then gap=0.075
if n_elements(R) lt 1 then R=10.0
if n_elements(w) lt 1 then w=0.1
if n_elements(Mo) lt 1 then Mo='Mo'
if n_elements(tune) lt 1 then tune=1.03
if n_elements(grow) lt 1 then grow=0.9
if n_elements(offset) lt 1 then offset=0.0
if n_elements(level) lt 1 then level=round(offset / w)
if n_elements(safety) lt 1 then safety=0.0
if n_elements(no_tube) lt 1 then no_tube=0					; 1 for Maia 96
if n_elements(nx) lt 1 then nx=10							; 4 for Maia 96
if n_elements(ny) lt 1 then ny=10							; 6 for Maia 96

veto = no_tube ? -100 : 1									; index to veto for central pads
width = pitch - gap											; width of each active volume
z1 = offset													; offset towards sample
z2 = offset + w
first = 1
xymin = -0.5*pitch
aperture = {x:[0.0,0.0], y:[0.0,0.0], IDx:0L, IDy:0L, level:level}

for i=-1L,nx-1 do begin
	for j=-1L,ny-1 do begin
	  if (i ge veto) or (j ge veto) then begin
		if i eq -1 then begin
			no_box2 = 0
			x1 = -pitch*0.5									; start of pad in X
			x2 = 0.5*gap +safety							; end of pad in X
			x3 = x1											; start of pad in X
			x4 = x2											; end of pad in X
			x2 = x2 - tune*(w+offset) * x2/R
			x4 = x4 - tune*(w+offset) * x4/R
		endif else begin
			no_box2 = 0
			x1 = pitch*float(i+1)-0.5*gap -safety			; start of pad in X for box_1
			x2 = x1 + gap +2*safety							; end of pad in X
			x3 = pitch*float(i)+0.5*gap +safety				; start of pad in X for box_2
			x4 = x2											; end of pad in X
			x1 = x1 - grow*(T+offset) * x1/R
			x2 = x2 - tune*(w+offset) * x2/R
			x3 = x3 - tune*(w+offset) * x3/R
			x4 = x4 - tune*(w+offset) * x4/R
			
			xl = x3											; mask aperture
			xh = x1
			if i eq nx-1 then begin
				x2 = pitch*float(i+1) + pitch*0.5
				x4 = x2
			endif
		endelse

		if j eq -1 then begin
			x1 = x3
			x2 = x4
			y1 = -pitch	* 0.5								; start of pad in Y
			y2 = 0.5*gap +safety							; end of pad in Y
			y2 = y2 - tune*(w+offset) * y2/R
			no_box2 = 1
		endif else begin
			no_box2 = 0
			y1 = pitch*float(j) + 0.5*gap +safety			; start of pad in Y for box_1
			y2 = pitch*float(j+1) - 0.5*gap -safety			; end of pad in Y
			y3 = y2											; start of pad in Y for box_2
			y4 = y3 + gap +2*safety							; end of pad in Y
			y1 = y1 - tune*(w+offset) * y1/R
			y2 = y2 - grow*(T+offset) * y2/R
			y3 = y2
			y4 = y4 - tune*(w+offset) * y4/R
			
			yl = y1											; mask aperture
			yh = y2
			if j eq ny-1 then begin
				y4 = pitch*float(j+1) + pitch*0.5
			endif
		endelse

		if i eq veto and j lt veto then begin
			x1 = (float(i)+0.5)*pitch
			x3 = x1
		endif
;		if j eq veto and i lt veto and i ge 0 then begin
		if j eq veto and i lt veto then begin
			no_box2 = 1
			x1 = x3
			x2 = x4
			y1 = (float(j)+0.5)*pitch
			y2 = y4
		endif
		if i eq veto and j eq veto then begin
			x1 = (float(i)+0.5)*pitch
			y3 = (float(j)+0.5)*pitch
			y2 = y3
		endif
		
		order = 1 + long(i)*long(i) + long(j)*long(j)
		box1 = { x:[x1,x2]>xymin, y:[y1,y2]>xymin, z:[z1,z2], material:Mo, IDx:i, IDy:j, order:order, level:level }
		if first then begin
			mask = box1
			first = 0
		endif else begin
			mask = [mask, box1]
		endelse
		if (i ge 0 and j ge 0) and (i ge (veto+1) or j ge (veto+1)) then begin
			aperture1 = {x:[xl,xh], y:[yl,yh], IDx:i, IDy:j, level:level}
			aperture = [aperture, aperture1]
		endif
		if no_box2 eq 0 then begin
			box2 = { x:[x3,x4]>xymin, y:[y3,y4]>xymin, z:[z1,z2], material:Mo, IDx:i, IDy:j, order:order, level:level }
			mask = [mask, box2]
		endif
	  endif
	endfor
endfor
aperture = aperture[1:*]

return, mask
end
