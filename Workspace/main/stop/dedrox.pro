	FUNCTION dedrox, Z1,A1, layer, E

;	stopping power (MeV / mg/cm^2) of particle 'Z1,A1' in composition
;	'layer' at energy 'E' (MeV).
;
;	Layer takes the decode_formula form {N:n, Z:z, F:f}

	de_dx = 0.0
	nt = n_elements(e)
	if nt lt 1 then goto, done
	de_dx = fltarr(nt)
	if layer.N lt 1 then goto, done

	a2t = mass( layer.Z[0:layer.n-1] )

	if nt eq 1 then begin
		mtot = layer.F[0:layer.n-1] * a2t[0:layer.n-1]
		de_dx = total( mtot * dedx(Z1,A1,layer.Z[0:layer.n-1],E) ) / total(mtot)
	endif else begin
		msum = 0.0
		sum = fltarr(nt)
		for i=0L,layer.n-1 do begin
			msum = msum + layer.F[i] * a2t[i]
			sum = sum + layer.F[i] * a2t[i] * dedx(Z1,A1,layer.Z[i],E)
		endfor
		de_dx = sum / msum
	endelse

done:
	s = size(de_dx)
	if s[0] gt 0 then de_dx = reform(de_dx)
	if n_elements(de_dx) eq 1 then de_dx = de_dx[0]
	return, de_dx
	end

