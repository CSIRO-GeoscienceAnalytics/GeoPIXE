function value_fill, arrayi, small=small, default=default, error=err

; Fill any missing (zero) entries in vector 'array' with previous non-zero
; value or 'default' if no previous exists.

COMPILE_OPT STRICTARR
if n_elements(small) lt 1 then small=0.0001
err = 0

	n = n_elements(arrayi)
	if n lt 1 then begin
		array = 0.0
		goto, bad
	endif
	array = arrayi
	if n_elements(default) lt 1 then default=array[0]
	
	qz = where( abs(array) lt small, nqz)
	if nqz eq 0 then return, array
	
	qnz = where( abs(array) ge small, nqnz)
	if nqnz eq 0 then begin
		array[*] = default
		return, array
	endif
	
	for i=0, nqz-1 do begin
		q = where( qnz lt qz[i], nq)
		if nq eq 0 then begin
			array[qz[i]] = default
		endif else begin
			array[qz[i]] = array[qnz[q[nq-1]]]
		endelse
	endfor
	return, array
	
bad:
	err = 1
	return, array
end