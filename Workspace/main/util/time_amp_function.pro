pro time_amp_function, x, a, f, pder

;	Time-Amp function, where	a[0] = scaling		a[1] = curvature
;								a[2] = offset
;
;	For use with CURVEFIT function.

if n_params() lt 3 then return
n = n_elements(x)

lx = alog10(x)
f = a[0]*( lx + a[1]/lx + a[2])

if n_params() ge 4 then begin

	pder = replicate( a[0], n,3)
	pder[*,0] = f/a[0]
	pder[*,1] = a[0]/lx
	pder[*,2] = a[0]
endif

return
end
