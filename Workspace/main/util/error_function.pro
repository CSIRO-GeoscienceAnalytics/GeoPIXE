
pro error_function_initial, fname, pd, ps, mark, a

; Initial parameters values for the CURVEFIT function defined by 'fname'
;	pd	points to spectrum data
;	ps	points to spectrum struct
;	mark	is X0-X5 marker array (assumed to be checked)

a = dblarr(4)
a[0] = mean( (*pd)[mark[0]:mark[1]] )
a[1] = mean( (*pd)[mark[4]:mark[5]] )
a[2] = (*ps).cal.poly[1] * mean( [mark[2],mark[3]] ) + (*ps).cal.poly[0]
a[3] = (*ps).cal.poly[1] * (mark[3]-mark[2])/3.

return
end

;---------------------------------------------------------------------------

pro error_function, x, a, f, pder

;	Error function, where	a[0] = base		a[1] = top
;							a[2] = centre	a[3] = FWHM of source Gaussian
;
;	For use with CURVEFIT function.

if n_params() lt 3 then return

c1 = 2.3548
c2 = 1.0 / sqrt(!pi)
n = n_elements(x)

z = c1 * (x - a[2]) / a[3]
g = 0.5*( ErrorF(z) + 1.0)
d = a[1]-a[0]

f = a[0] + g*d

if n_params() ge 4 then begin

	p0 = 1.0 - g
	p1 = g

	h = -d * c2 * exp(-z*z) / a[3]
	p2 = h * c1
	p3 = h * z

	pder = replicate( a[0], n,4)
	pder[*,0] = p0
	pder[*,1] = p1
	pder[*,2] = p2
	pder[*,3] = p3
endif

return
end
