function hex_jam, str

; Convert the Hex values in string (array) 'str' to an integer value (array).
; Assume they are padded to correct byte length.

	COMPILE_OPT STRICTARR

	ns = n_elements(str)

	b = byte(str[0])
	n = n_elements(b)

	if n le 2 then begin
		x = bytarr(ns)
	endif else if n le 4 then begin
		x = uintarr(ns)
	endif else if n le 8 then begin
		x = ulonarr(ns)
	endif else begin
		x = ulon64arr(ns)
	endelse

	f = '(Z'+str_tidy(nb)+')'
	reads, str, x, format=f

	return, x
end
