	pro circle_diam, x1,y1, x2,y2, x,y, n=n, aspect=aspect
;
;	Make a vector of x,y coords for a circle marked on the diameter
;	by x1,y1 and x2,y2.
;
	if n_elements(n) lt 1 then n = 24
	if n_elements(aspect) lt 1 then aspect = 1.0
	mx = 0.5*(x1+x2)
	my = 0.5*(y1+y2)
	r = 0.5 * sqrt( float(x2-x1)*float(x2-x1) + float(y2-y1)*float(y2-y1) )
	x = fltarr(n+1)
	y = fltarr(n+1)
	dt = 2.*!pi/n
	torg = atan( y1-my, x1-mx)
	asp = sqrt(aspect)

	t = torg
	for i=0L,n do begin
	    x(i) = mx + r*cos(t) / asp
	    y(i) = my + r*sin(t) * asp
		t = t+dt
	endfor
	return
	end
