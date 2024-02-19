pro time_amp_shape, xi,yi, x,y

; Make time amp curve of (xi,yi).

if n_elements(init) lt 1 then init=0
n = n_elements(xi)
if n lt 7 then begin
	x = 0.0
	y = 0.0
	return
endif
mnx = min(xi)
mxx = max(xi)
mny = min(yi)
mxy = max(yi)
if ((mxx lt 1.0e-3) and (mxy lt 1.0e-3)) or $
		((mnx eq mxx) and (mny eq mxy)) then begin
	x = xi
	y = yi
	return
endif

dx = xi[1]-xi[n-1]
dy = yi[1]-yi[n-1]
tan0 = [dx,dy] / sqrt(dx*dx+dy*dy)

spline_p2, xi[0:4]+(xi[6]-xi[2]),yi[0:4]+(yi[6]-yi[2]), x1,y1	;, tan0=tan0, tan1=tan0
spline_p2, xi[0:4]+(xi[5]-xi[2]),yi[0:4]+(yi[5]-yi[2]), x2,y2	;, tan0=tan0, tan1=tan0
x = [x1,reverse(x2),x1[0]]
y = [y1,reverse(y2),y1[0]]

return
end
