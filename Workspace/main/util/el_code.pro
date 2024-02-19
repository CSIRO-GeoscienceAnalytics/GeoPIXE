function single_letter_element, el

;	Is 'el' a single letter element mneumonic (not 'C')?

	singles = ['H','B','N','O','F','P','S','K','V','Y','I','W','U']

	q = where( el eq singles, count)
	if count eq 0 then return, 0
	return, 1
	end

;-----------------------------------------------------------------

pro el_code, list, el, z, shell, bad, error

;	Decode element mneumonics with optional 'L', 'M' appended
;	to indicate shell.

	error = 1
	n = n_elements(list)
	if n lt 1 then return
	bad = intarr(n)
	el = strarr(n)
	shell = intarr(n)
	shells = ['?','K','L','M']

	for i=0L,n-1 do begin
		name = strupcase(list[i])
		l = lenchr(name)

		case l of
			1: begin
				shell[i] = 1
				el[i] = extract(name,0,0)
				if single_letter_element(el[i]) or (el[i] eq 'C') then begin
					bad[i] = 0
				endif else begin
					bad[i] = 1
				endelse
				end
			3: begin
				nam3 = extract(name,2,2)
				el[i] = extract(list[i],0,1)
				q = where( nam3 eq shells)
				shell[i] = q[0]
				if q[0] le 0 then bad[i]=1
				end
			2: begin
				nam1 = extract(name,0,0)
				nam2 = extract(name,1,1)
				if single_letter_element(nam1) and (nam2 eq 'L') then begin
					shell[i] = 2
					el[i] = nam1
				endif else if single_letter_element(nam1) and (nam2 eq 'K') then begin
					shell[i] = 1
					el[i] = nam1
				endif else if single_letter_element(nam1) and (nam2 eq 'M') and $
							(nam1 ne 'P') and (nam1 ne 'S') then begin
					shell[i] = 3
					el[i] = nam1
				endif else begin
					shell[i] = 1
					el[i] = extract(list[i],0,1)
				endelse
				end
			else: begin
				shell[i] = 0
				el[i] = ''
				bad[i] = 1
				end
		endcase
	endfor

	z = atomic_number(el)
	q = where(z eq 0, count)
	if count ne 0 then bad[q]=1
	if n_elements(z) eq 1 then z=z[0]
	if n_elements(bad) eq 1 then bad=bad[0]
	if n_elements(el) eq 1 then el=el[0]
	error = 0
	return
end