function x_transparent, x, trailing=trailing, R=R, designR=R0, $
						w=w, Mo=Mo, nmask=nmask, glue=glue, E=E

; x			distance from origin of grid bar (mm) in Maia-384
; R			distance from face to sample/beam-spot
; designR	design distance (mm)
; w			thickness of Mo absorber (mm)
; Mo		element of mask (default 'Mo')
; glue		glue layers (mm)
; nmask		number of mask layers
; E			X-ray energy (keV)
;
; xsi		distance for 0.5 transmission in Mo

if n_elements(R) lt 1 then R=10.0
if n_elements(R0) lt 1 then R0=10.0
if n_elements(w) lt 1 then w=0.1
if n_elements(Mo) lt 1 then Mo='Mo'
if n_elements(nmask) lt 1 then nmask=3
if n_elements(glue) lt 1 then glue=0.025
if n_elements(E) lt 1 then E=10.
if n_elements(trailing) lt 1 then trailing=0

; Calculate distance in Mo (mm) for an attenuation of 2 ...

layer = make_filter( Mo, 1000*w, /microns, error=err)
if err then return, 0.0
mu = 1000. * atten( layer, E)
xsi = 10. * alog(2.) / (mu * layer.density)

nx = 100
p = [0.,0.,R]
ray = { p:p, a:fltarr(3), x:0.0, y:0.0, ID:0L}
rays = replicate(ray, nx)
length = fltarr(nx)

delta = x*(w+glue)/R0

if trailing then begin
	d = -delta
	base = glue
	for i=0L,nmask-1 do begin
		b = { x:[-100.,d]+x, y:[-1.,1.], z:[0.,w]+base, material:Mo, IDx:0, IDy:0, order:0, level:0}
		if i eq 0 then box=b else box=[box,b]
		d = d-delta
		base = base + w+glue
	endfor
	if R gt R0 then begin
		xmax = x - delta + x*(w+glue)/R
		xmin = x - (nmask+1)*delta + nmask*x*(w+glue)/R
	endif else begin
		xmin = x - delta
		xmax = x - nmask*delta + nmask*x*(w+glue)/R
	endelse

	ox = xmin
	dx = (xmax-xmin)/(nx-1)
	xr = dx * findgen(nx) + ox
endif else begin
	d = 0.
	base = glue
	for i=0L,nmask-1 do begin
		b = { x:[d,100.]+x, y:[-1.,1.], z:[0.,w]+base, material:Mo, IDx:0, IDy:0, order:0, level:0}
		if i eq 0 then box=b else box=[box,b]
		d = d-delta
		base = base + w+glue
	endfor
	if R gt R0 then begin
		xmax = x + x*(w+glue)/R
		xmin = x - (nmask-1)*delta + (nmask-1)*x*(w+glue)/R
	endif else begin
		xmin = x
		xmax = x - nmask*delta + nmask*x*(w+glue)/R
	endelse

	ox = xmin
	dx = (xmax-xmin)/(nx-1)
	xr = dx * findgen(nx) + ox
endelse

k = 0
for i=0L,nx-1 do begin
	a = [xr[i],0.,-R]
	a = a/sqrt(a[0]^2+a[1]^2+a[2]^2)
	rays[k].a = a							; ray unit vector to this point
	rays[k].x = xr[i]
	rays[k].y = 0.
	rays[k].id = k
	k = k+1
endfor

for i=0L,nmask-1 do begin
	l = intersection( box[i], rays, hit=hit, entry=enter)
	q = where( hit eq 1, nq)
	if nq ge 1 then length[q] = length[q] + l[q]
endfor

if xsi lt min(length) then begin
	newx = x
endif else if xsi gt max(length) then begin
	newx = trailing ? min(xr) : max(xr)
endif else begin
	newx = interpol( xr, length, xsi)
endelse

return, newx
end
