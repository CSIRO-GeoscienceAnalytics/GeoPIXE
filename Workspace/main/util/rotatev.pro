pro rotatev, x,y, xc,yc, angle, xr,yr, degrees=degrees

; Rotate vectors (x,y) about centre (xc,yc) by theta (radians)

theta = angle
if n_elements(degrees) lt 1 then degrees=0
if degrees then theta = angle * !pi / 180.0

xr = cos(theta)*float(x-xc) - sin(theta)*float(y-yc) + float(xc)
yr = sin(theta)*float(x-xc) + cos(theta)*float(y-yc) + float(yc)

return
end
