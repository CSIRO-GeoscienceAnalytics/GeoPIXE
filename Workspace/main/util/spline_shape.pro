pro spline_shape, xi,yi, x,y

; Make closed spline shape of (xi,yi).
; Assume that last and first points are NOT the same,
; and so they will be joined.

n = n_elements(xi)
if n lt 2 then begin
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

spline_p2, [xi,xi[0]],[yi,yi[0]], x,y, tan0=tan0, tan1=tan0

return
end
