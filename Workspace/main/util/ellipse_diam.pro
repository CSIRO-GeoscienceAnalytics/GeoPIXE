	pro ellipse_diam, x1,y1, x2,y2, x3,y3, x4,y4, x,y
;
;	Make a vector of x,y coords for an ellipse marked on the
;	major diameter by x1,y1 and x2,y2, and on the minor diameter by
;	x3,y3 and x4,y4.
;

	theta = angle_lines( x1,y1, x2,y1, x2,y2)
	xc = x1
	yc = y1
	x0 = [ x1, x2, x3, x4]						; initial points
	y0 = [ y1, y2, y3, y4]
	rotatev, x0,y0, xc,yc, -theta, xr,yr		; rotate ellipse control points onto X axis

	mx = 0.5*(xr[0]+xr[1])
	my = 0.5*(yr[0]+yr[1])
	r = 0.5 * sqrt( float(xr[1]-xr[0])*float(xr[1]-xr[0]) + float(yr[1]-yr[0])*float(yr[1]-yr[0]) )
	n = 24
	xe = fltarr(n+1)
	ye = fltarr(n+1)
	dt = 2.*!pi/n

	yscale = abs((yr[3]-yr[2])/(xr[1]-xr[0]))	; Y/X aspect ratio

	for i=0L,n do begin							; build ellipse on X axis
	    xe(i) = mx + r*cos(i*dt)
	    ye(i) = my + yscale * r*sin(i*dt)
	endfor

	rotatev, xe,ye, xc,yc, theta, x,y			; rotate ellipse back again
	return
	end
