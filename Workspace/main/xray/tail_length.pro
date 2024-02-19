function tail_length, detector, E

;	Length of the peak tail

	n = n_elements(e)
	if n lt 1 then return, 0.0
	len = fltarr(n)
	if ptr_valid( detector) eq 0 then return, len

	len = ((*detector).tail.L + (*detector).tail.S * (E - 6.4)) > 0.0

	return, len
	end
