function tail_amplitude, detector, E

;	Amplitude of the peak tail
;	Assumes Si(Li) detector for now ...

	n = n_elements(e)
	if n lt 1 then return, 0.0
	amp = fltarr(n)
	if ptr_valid( detector) eq 0 then return, amp

	small = 1.0e-10
	if (*detector).tail.amp lt small then return, amp

;	Assume detector 'thick' in mg/cm**2 and F,B in microns

	D = (*detector).crystal.thick				; mg/cm**2
	F = (*detector).tail.F * (*detector).density/10. < D
	B = (*detector).tail.B * (*detector).density/10. < (D-F)

	U = replicate(10000.,n)
	q = where(e gt 1.0)
	if q[0] ne -1 then U[q] = atten( (*detector).crystal, E[q])			; cm**2/mg

	total = 1. - exp(-U*D)
	front = 1. - exp(-U*F)
	back = exp(-U*(D-B)) * (1. - exp(-U*B))

	tail = (front + back) / total
	tail = tail < 1.0
	q = where(finite(tail) eq 0)
	if q[0] ne -1 then tail[q] = 0.0

	amp = (*detector).tail.amp * tail
	if n_elements(amp) eq 1 then amp=amp[0]
	return, amp
	end
