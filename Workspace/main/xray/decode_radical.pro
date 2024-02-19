pro decode_radical, formulai, n, z, f, order=order, valence=valence, $
						number=number

; Decode a chemical formula string (e.g. "Fe\3\2O3")
; into:
;	n		number of valid elements found
;	z		atomic number vector
;	f		atomic fraction (or formula number) for each z vector
;	order	order of element in formula
;	valence	structure of valence info: {highest: , fraction: }
;			highest:	highest valence state specified
;			fraction:	fraction in the high valence
;
;	If /number is set, then 'f' returns the formula numbers.
;
;	Based on DECODE_FORMULA.FOR.
;
;-------------------------------------------------------------------------------

COMPILE_OPT STRICTARR
	formula = formulai
;	formula = geo_formula(formula)		; check for a end-member formula

	n = 0
	f = fltarr(93)
	v = intarr(93)
	fraction = fltarr(93)
	order = replicate(-1,93)
	if n_elements(number) lt 1 then number=0

	code = formula_split( formula)

	ns = n_elements(code)
	if ns lt 1 then goto, done
	if lenchr( code[0]) lt 1 then goto, done

	for i=0L,ns-1 do begin
		chop_element, code[i], zi, xi, v=vx
		if zi lt 1 then begin
			print,'decode_radical: bad element - "'+code[i]+'"'
		endif

		if order[zi] lt 1 then order[zi] = i+1
		f[zi] = f[zi] + xi

		if vx gt 0 then begin
			if vx gt v[zi] then begin
				v[zi] = vx
				fraction[zi] = xi
			endif else if vx eq v[zi] then begin
				fraction[zi] = fraction[zi] + xi
			endif
		endif
	endfor

	z = where(f[1:*] gt 1.0e-10) + 1
	if z[0] eq -1 then begin
		print,'decode_radical: no valid elements'
		goto, done
	endif
	t = total( f[z])
	if t lt 1.0e-10 then begin
		print,'decode_radical: total formula count zero'
		goto, done
	endif

	fraction = fraction[z] / f[z]
	f = f[z]
	if number eq 0 then f = f / t
	order = order[z]
	valence = { highest:v[z], fraction:fraction }
	n = n_elements(z)

done:
	if n eq 1 then begin
		z = z[0]
		f = f[0]
		order = order[0]
		valence = valence[0]
	endif
	if n eq 0 then begin
		z = 0
		f = 0.0
		order = 0
		valence = {highest:0, fraction:0.0}
	endif
	return
	end
