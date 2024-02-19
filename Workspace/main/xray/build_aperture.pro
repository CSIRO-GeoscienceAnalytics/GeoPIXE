function build_aperture, aperture=aperture, thick=thick

; Build an array of pad 'boxes' for detector square aperture
; Only build a quadrant, and veto inner pads for Mo tube.
;
; Format of each element:
;	box = { x:[-3.,-1.], y:[-2.,2.], z:[-1.,0.], material:'Si', IDx:0, IDy:0, order:0.0 }
;
;	x,y,z		give dimensions of box on x,y,z axes
;	material	is the composition of the box
;	IDx, IDy	are the x,y indices for a detector in array
;	order		is a priority order, giving the radial distance from origin

compile_opt strictarr
if n_elements(aperture) lt 1 then aperture=10.4
if n_elements(thick) lt 1 then thick=1.0

mat = 'Fe'
offset = 4.90

dt = thick/10.
first = 1
for i=0L,9 do begin
	x1 = aperture/2. + float(i)*dt
	x2 = 15.
	y1 = -2.
	y2 = x1
	z1 = offset - dt*float(i)
	z2 = z1-dt
	box = { x:[x1,x2], y:[y1,y2], z:[z1,z2], material:mat, IDx:0, IDy:0, order:0L }
	if first then begin
		frame = box
		first = 0
	endif else frame = [frame,box]

	x1 = -2.
	x2 = 15.
	y1 = aperture/2. + float(i)*dt
	y2 = 15.
	z1 = offset - dt*float(i)
	z2 = z1-dt
	frame = [frame, { x:[x1,x2], y:[y1,y2], z:[z1,z2], material:mat, IDx:0, IDy:1, order:1L }]
endfor

return, frame
end
