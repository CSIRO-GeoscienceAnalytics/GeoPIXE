function jump_ratio, zi, shelli, photo=photo

; absorption sec fluor jump ratio correction for each shell
;
;	Note: This is the sec fluor yield factor, not the absorption
;		jump ratio itself.
;
;    shell = 1  K
;            2  L
;			 3  M
;
;	Note that this parameterization for PIXE seems to only calculate
;	the secondary fluorescence of the L3 subshell, not all L subshells,
;	although the jump_ratio_xrf routine does all. May need to retire
;	this routine and use the XRF one for both PIXE and XRF.

if n_elements(photo) lt 1 then photo=0
if photo then return, jump_ratio_xrf( Zi, shelli)

	z = zi
	shell = shelli

	nz = n_elements(z)
	ns = n_elements(shell)
	nn = nz

	if (nz lt 1) or (ns lt 1) then return, 0.0
	if nz eq 1 then begin
		nn = ns
		z = replicate(Z,nn)
	endif else if ns eq 1 then begin
		nn = nz
		shell = replicate(shell,nn)
	endif else if nz ne ns then begin
		print,'jump_ratio: Z,Shell can only both be vectors if they have same length.'
		jump = fltarr(nn)
		goto, done
	endif
	jump = fltarr(nn)

	bad = where( ((Z gt 94) OR (Z LE 0)) or $
				((shell lt 1) or (shell gt 16)) )	; reject these
	more = replicate(1,nn)							; continue calculating
	if bad[0] ne -1 then more[bad] = 0
	test = intarr(nn)

	q = where( (shell eq 1) and more)						; K shell
	if q[0] ne -1 then begin
		test[*] = 0
		test[q] = 1
		more[q] = 0

		q = where( test)
		if q[0] ne -1 then begin
			jump[q] = (0.924 - 0.00144*float(z[q]))
		endif
	endif

	q = where( ((shell ge 2) and (shell le 3)) and more)	; L,M shell
	if q[0] ne -1 then begin
		test[*] = 0
		test[q] = 1
		more[q] = 0

		q = where( test)
		if q[0] ne -1 then begin
			jump[q] = (0.548 - 0.00231*float(z[q]))
		endif
	endif

 done:
 	jump = reform(jump)
	if n_elements(jump) eq 1 then jump = jump[0]
	RETURN, jump
	END