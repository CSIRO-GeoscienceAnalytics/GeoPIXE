function analyze_cut, pstate, error=error

;	Integrate cut channels --> sum

COMPILE_OPT STRICTARR

	if ptr_valid( (*pstate).p) eq 0 then goto, bad
	p = *((*pstate).p)
	if ptr_valid( p[0]) eq 0 then goto, bad

	i = current_plot( pstate)						; current spectrum
	ps = p[i]										; pointer to current spectrum struct
	if ptr_valid( (*ps).data) eq 0 then goto, bad

	error = 1
	types = [0,1,0,0,0,0]							; 0:Cut, 1:X0-X5
	type = types[(*pstate).mark_set]

	if ((*pstate).mark_set eq 4) then begin				; Cut 0,1

		x2 = round( (*pstate).cmark[0,4])
		x3 = round( (*pstate).cmark[1,4]) > x2
		x = ([0,0,x2,x3,0,0] > 0) < ((*ps).size-1)

		s23 = total( (*(*ps).data)[x[2]:x[3]] )

		back = 0.0
		z1 = 0.0
		z2 = 0.0
		dleft = 0.0
		dright = 0.0

		area = s23
		err = sqrt(abs(s23))

	endif else if ((*pstate).mark_set eq 1) then begin	; X0-X5

		x0 = round( (*pstate).cmark[0,1])
		x1 = round( (*pstate).cmark[1,1])
		x2 = round( (*pstate).cmark[2,1])
		x3 = round( (*pstate).cmark[3,1])
		x4 = round( (*pstate).cmark[4,1])
		x5 = round( (*pstate).cmark[5,1])
		if (x2 lt x0) or (x2 gt x5) then x2=x1
		if (x1 lt x0) or (x1 gt x5) then x1=x2
		if (x4 lt x0) or (x4 gt x5) then x4=x3
		if (x3 lt x0) or (x3 gt x5) then x3=x4
		x = [x0,x1,x2,x3,x4,x5]

		if (x0 lt 0.1) and (x1 lt 0.1) then begin	; old
			type = 2

			ds = x3+1.-x2
			db = x5+1.-x4

			s23 = total( (*(*ps).data)[x2:x3] )
			zb = total( (*(*ps).data)[x4:x5] )  /db
			z1 = 0.0
			z2 = zb
			dleft = 0.0
			dright = -ds/db

			es23 = s23

			back = zb * ds
			area = s23 - back

			varb = abs(zb*db *(ds/db)*(ds/db))
			var0 = abs(es23)

			var = var0 + varb
			err = sqrt(abs(var))
		endif else begin							; X0-X5
			d1 = x1+1.-x0
			d0 = x3+1.-x2
			d2 = x5+1.-x4
			c1 = (x0+x1)/2.
			c0 = (x2+x3)/2.
			c2 = (x4+x5)/2.

			s23 = total( (*(*ps).data)[x2:x3] )
			z1 = total( (*(*ps).data)[x0:x1] )  /d1
			z2 = total( (*(*ps).data)[x4:x5] )  /d2

			es23 = s23
			ez1 = z1
			ez2 = z2
			dz1 = z1/d1
			dz2 = z2/d2

			a = (z2-z1)/(c2-c1)
			b = z1-a*c1
			da = (dz2+dz1)/((c2-c1)*(c2-c1))
			db = dz1+da*c1*c1

			dleft = -d0*((c2-c0)/(c2-c1))/d1
			dright = -d0*((c0-c1)/(c2-c1))/d2

;			back = d0*(z2*(c0-c1)+z1*(c2-c0))/(c2-c1)
;			back = -dright*z2*d1 -dleft*z1*d2

			back = d0*(z2-z1)*(c0-c1)/(c2-c1) + d0*z1
			area = s23-back

			b1 = (c2-c0)*(c2-c0)/d1
			b2 = (c0-c1)*(c0-c1)/d2
			b0 = d0/(c2-c1)

			var1 = abs(b1*ez1*b0*b0)
			var2 = abs(b2*ez2*b0*b0)
			var0 = abs(es23)

			var = var0+var1+var2
			err = sqrt(abs(var))
		endelse
	endif else begin
		goto, bad
	endelse

	e = (*ps).cal.poly[0] + (*ps).cal.poly[1] * x
	results = define(/cut)

	results.type =	type					; type of markers (0:Cut, 1:X0-X5, 2:old)
	results.x = 	x						; X markers (0-5) (channel)
	results.e = 	e						; energy of markers
	results.units =	(*ps).cal.units			; cal units
	results.cal_a =	(*ps).cal.poly[1]		; cal A
	results.cal_b =	(*ps).cal.poly[0]		; cal B

	results.dleft =	dleft					; left lever
	results.dright =	dright				; right lever

	results.left =	z1						; left background sum (X0-X1)
	results.right =	z2						; right background sum (X4-X5)
	results.sum =	s23						; total sum (X2-X3)
	results.back =	back					; background counts (X2-X3)
	results.area =	area					; back subtracted sum (X2-X3)
	results.error =	err						; area uncertainty

	error = 0
	return, results

bad:
	error = 1
	return, 0
end
