	pro decode_formula, expression, n, zn, fn, weight=weight, verbose=verbose, atoms=t

; Decode a chemical formula string with groups (e.g. "(Fe2O3)12.3(Al2O3)23.1")
; into:
;	n		number of valid elements found
;	z		atomic number vector
;	f		atomic fraction for each z vector
;
; The weights (outside of brackets) can be atomic weights (default) or
; weight % weights (if weight=1).
;
;	Based on SET_FORM.FOR.
;	Later: extend valence to here too.
;
;-------------------------------------------------------------------------------

COMPILE_OPT STRICTARR
	n = 0
	f = fltarr(93)
	if n_elements(weight) lt 1 then weight=0
	if n_elements(verbose) lt 1 then verbose=0

	radical = radical_split( expression)
	if verbose then print,'radicals = ',radical

	nr = n_elements(radical)
	if nr lt 1 then goto, done
	if lenchr(radical[0]) lt 1 then goto, done

	for i=0L,nr-1 do begin
		chop_radical, radical[i], formula, x
		decode_radical, formula, ni, zi, fi, number=(1-weight)
		if verbose then print,'  chop radical = ',radical[i],' --> formula =',formula,'  x=',x
		if verbose then print,'      n=',ni,' z=',zi,' f=',fi
		if ni lt 1 then begin
			print,'decode_formula: bad radical - "'+radical[i]+'"'
		endif else begin
			if weight then begin
				w = total( fi * mass(zi))
				if w lt 1.0e-10 then begin
					print,'decode_formula: zero weight radical - ['+radical[i]+']'
				endif else begin
					f[zi] = f[zi] + fi*x/w
				endelse
			endif else begin
				f[zi] = f[zi] + fi*x
			endelse
		endelse
	endfor

	z = where( f[1:*] gt 1.0e-10) + 1
	if z[0] eq -1 then begin
		print,'decode_formula: no valid elements'
		goto, done
	endif
	t = total( f[z])
	if verbose then print,' total atom count = ',t
	if t lt 1.0e-10 then begin
		print,'decode_formula: total formula count zero'
		goto, done
	endif

	f = f[z] / t
	n = n_elements(z)

done:
	if n eq 1 then begin
		z = z[0]
		f = f[0]
	endif
	if n eq 0 then begin
		z = 0
		f = 0.0
	endif

	zn = intarr(32)
	fn = fltarr(32)
	if n ge 1 then begin
		zn[0:n-1] = z
		fn[0:n-1] = f
	endif

	return
	end
