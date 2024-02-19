	pro chop_radical, radical, formula, x

; chop a radical expression into a chemical formula part
; and a weighting factor part.
;
; e.g. (Al2O3)13.1  -->  formula="Al2O3", x=13.1

	formula = ''
	X = 0.0

	lr = lenchr(radical)
	cb = locate(')',radical)
	s = strmid( radical, 0,1)
	if s eq '(' then begin
		L=1
		M = cb - 1
		if M lt L then begin
			print,'chop_radical: unbalanced paranthethese'
			return
		endif
		no_x = 0
		if M eq lr-2 then no_x = 1
	endif else if cb ge 0 then begin
		L = 0
		M = cb - 1
		no_x = 0
		if M eq lr-2 then no_x = 1
	endif else begin
		L = 0
		M = lr-1
		no_x = 1
	endelse

	formula = extract( radical, L, M)
	if no_x then begin
		X = 1.0
	endif else begin
		X = float( extract( radical, M+2, lr-1))
	endelse

	return
	end
