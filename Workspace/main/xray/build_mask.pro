function build_mask, Thick=T, pitch=pitch, gap=gap, R=R, w=w, Mo=Mo, $
						tune=tune, grow=grow, aperture=aperture

; Build an array of mask 'boxes' for detector mask volumes
; Only build a quadrant, and veto inner pads for Mo tube.
;
;	Thick		thickness of the detector wafer (mm)
;	pitch		pitch between detector pads (mm)
;	gap			gap between pads (mm)
;	R			distance from beam spot to detector wafer
;	w			thickness of mask
;	Mo			material composition (pure element) of mask
;	tune		scale the mask offset on trailing edge of mask
;	grow		scale the mask offset on leading edge of mask
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
if n_elements(w) lt 1 then w=0.100
if n_elements(Mo) lt 1 then Mo='Mo'
if n_elements(tune) lt 1 then tune=0.5
if n_elements(grow) lt 1 then grow=0.85

nx = 10
ny = 10
veto = 1
width = pitch - gap
z1 = 0.0
z2 = w
first = 1
xymin = -0.5*gap
aperture = {x:[0.0,0.0], y:[0.0,0.0]}

for i=-1L,nx-1 do begin
	for j=-1L,ny-1 do begin
	  if (i ge veto) or (j ge veto) then begin
		if i eq -1 and j eq -1 then begin
			x1 = 0.0										; start of pad in X
			x2 = 0.5*gap									; end of pad in X
			x2 = x2 - tune*w * x2/R
			no_box2 = 1
		endif else begin
			no_box2 = 0
			x1 = pitch*float(i+1)-0.5*gap					; start of pad in X
			x2 = x1 + gap									; end of pad in X
			x3 = pitch*float(i)+0.5*gap						; start of pad in X
			x4 = x3 + pitch									; end of pad in X
			x1 = x1 - grow*T * x1/R
			x2 = x2 - tune*w * x2/R
			x3 = x3 - tune*w * x3/R
			x4 = x4 - tune*w * x4/R
			
			xl = pitch*float(i)+0.5*gap						; mask aperture
			xl = xl - tune*w * xl/R
			xh = x1
		endelse

		if j eq -1 then begin
			x1 = x3
			x2 = x4
			y1 = -0.5*gap									; start of pad in Y
			y2 = 0.5*gap									; end of pad in Y
			y2 = y2 - tune*w * y2/R
			no_box2 = 1
		endif else begin
			no_box2 = 0
			y1 = pitch*float(j)+0.5*gap						; start of pad in Y
			y2 = y1 + pitch - gap							; end of pad in Y
			y3 = y2											; start of pad in Y
			y4 = y3 + gap									; end of pad in Y
			y1 = y1 - tune*w * y1/R
			y2 = y2 - grow*T * y2/R
			y3 = y2
			y4 = y4 - tune*w * y4/R
			
			yl = pitch*float(j)+0.5*gap
			yl = yl - tune*w * yl/R
			yh = y3
		endelse

		if i eq veto and j lt veto then begin
			x1 = (float(i+1)*pitch-0.5*gap - T*(float(i+1)*pitch-0.5*gap)/R)
			x3 = x1
		endif
		if j eq veto and i lt veto then begin
			no_box2 = 1
			x1 = x3
			x2 = x4
			y1 = pitch*float(j+1)-0.5*gap
			y1 = y1 - T*y1/R
			y2 = y4
		endif
		if i eq veto and j eq veto then begin
			x1 = (float(i+1)*pitch-0.5*gap - T*(float(i+1)*pitch-0.5*gap)/R)
			y3 = pitch*float(j+1)-0.5*gap
			y3 = y3 - T*y3/R
			y2 = y3
		endif
		
		order = 1 + long(i)*long(i) + long(j)*long(j)
		box1 = { x:[x1,x2]>xymin, y:[y1,y2]>xymin, z:[z1,z2], material:Mo, IDx:i, IDy:j, order:order }
		if first then begin
			mask = box1
			first = 0
		endif else begin
			mask = [mask, box1]
		endelse
		if (i ge 0 and j ge 0) and (i ge 2 or j ge 2) then begin
			aperture1 = {x:[xl,xh], y:[yl,yh]}
			aperture = [aperture, aperture1]
		endif
		if no_box2 eq 0 then begin
			box2 = { x:[x3,x4]>xymin, y:[y3,y4]>xymin, z:[z1,z2], material:Mo, IDx:i, IDy:j, order:order }
			mask = [mask, box2]
		endif
	  endif
	endfor
endfor
aperture = aperture[1:*]

return, mask
end
