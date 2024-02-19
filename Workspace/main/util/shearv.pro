pro shearv, x,y, xc,yc, angle, xr,yr, degrees=degrees

; Shear vectors (x,y) about centre (xc,yc) by theta (radians)

theta = angle
if n_elements(degrees) lt 1 then degrees=0
if degrees then theta = angle * !pi / 180.0

xr = x + (y-yc) * tan(theta)
yr = y

return
end
