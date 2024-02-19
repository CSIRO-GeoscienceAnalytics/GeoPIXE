function build_gaps, T=T, pitch=pitch, gap=gap

; Build an array of gap 'boxes' for detector gap volumes
; Only build a quadrant, and veto inner pads for Mo tube.
;
;	T			thickness of the detector wafer (mm)
;	pitch		pitch between detector pads (mm)
;	gap			gap between pads (mm)
;	
; Format of each element:
;	box = { x:[-3.,-1.], y:[-2.,2.], z:[-1.,0.], material:'Si', IDx:0, IDy:0, order:0.0 }
;
;	x,y,z		give dimensions of box on x,y,z axes
;	material	is the composition of the box
;	IDx, IDy	are the x,y indices for a detector in array
;	order		is a priority order, giving the radial distance from origin

compile_opt strictarr
if n_elements(T) lt 1 then T=0.4
if n_elements(pitch) lt 1 then pitch=1.0
if n_elements(gap) lt 1 then gap=0.05

nx = 10
ny = 10
veto = 1
width = pitch - gap
z1 = -T
z2 = 0.0
first = 1
xymin = -0.5*gap

for i=-1L,nx-1 do begin
	for j=-1L,ny-1 do begin
	  if (i ge veto) or (j ge veto) then begin
		if i eq -1 and j eq -1 then begin
			x1 = 0.0										; start of pad in X
			x2 = 0.5*gap									; end of pad in X
			no_box2 = 1
		endif else begin
			no_box2 = 0
			x1 = pitch*float(i+1)-0.5*gap					; start of pad in X
			x2 = x1 + gap									; end of pad in X
			x3 = pitch*float(i)+0.5*gap						; start of pad in X
			x4 = x3 + pitch									; end of pad in X
		endelse

		if j eq -1 then begin
			x1 = x3
			x2 = x4
			y1 = -0.5*gap										; start of pad in Y
			y2 = 0.5*gap									; end of pad in Y
			no_box2 = 1
		endif else begin
			no_box2 = 0
			y1 = pitch*float(j)+0.5*gap						; start of pad in Y
			y2 = y1 + pitch - gap							; end of pad in Y
			y3 = y2											; start of pad in Y
			y4 = y3 + gap									; end of pad in Y
		endelse

		if i eq veto and j lt veto then begin
			x1 = x1 > (float(i+1)*pitch-0.5*gap)
			x3 = x3 > (float(i+1)*pitch-0.5*gap)
		endif
		if j eq veto and i lt veto then begin
			no_box2 = 1
			x1 = x3
			x2 = x4
			y1 = y3
			y2 = y4
		endif
		
		order = 1 + long(i)*long(i) + long(j)*long(j)
		box1 = { x:[x1,x2]>xymin, y:[y1,y2]>xymin, z:[z1,z2], material:'Si', IDx:i, IDy:j, order:order }
		if first then begin
			gaps = box1
			first = 0
		endif else gaps = [gaps, box1]
		if no_box2 eq 0 then begin
			box2 = { x:[x3,x4]>xymin, y:[y3,y4]>xymin, z:[z1,z2], material:'Si', IDx:i, IDy:j, order:order }
			gaps = [gaps, box2]
		endif
	  endif
	endfor
endfor

return, gaps
end
