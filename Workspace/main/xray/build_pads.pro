function build_pads, T=T, pitch=pitch, gap=gap

; Build an array of pad 'boxes' for detector active volumes
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
veto = 2
width = pitch - gap
z1 = -T
z2 = 0.0
first = 1

for i=0L,nx-1 do begin
	for j=0L,ny-1 do begin
	  if (i ge veto) or (j ge veto) then begin
		x1 = 0.5*gap + pitch*float(i)					; start of pad in X
		x2 = x1 + width									; end of pad in X

		y1 = 0.5*gap + pitch*float(j)					; start of pad in Y
		y2 = y1 + width									; end of pad in Y
		
		order = 1 + long(i)*long(i) + long(j)*long(j)
		box = { x:[x1,x2], y:[y1,y2], z:[z1,z2], material:'Si', IDx:i, IDy:j, order:order }
		if first then begin
			pads = box
			first = 0
		endif else pads = [pads, box]
	  endif
	endfor
endfor

return, pads
end
