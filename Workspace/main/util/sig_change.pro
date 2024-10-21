function sig_change, old, new, error=err, tol=tol

	; Flag a significant change in 'new' compared to 'old'
	; Assumes quantities should be positive.
	; 'tol' tolerance, defaults to "significance" of 1 ppm.

	err = 1
	if n_elements(old) eq 0 then return, 1
	if n_elements(tol) eq 0 then tol=1.0e-6

	sig = fix( old)
	sig[*] = 1
	if n_elements(new) ne n_elements(old) then return, sig

	err = 0
	f = (new-old)/(old > 1.0e-35)
	sig[*] = abs(f) gt tol
	return, sig
end
