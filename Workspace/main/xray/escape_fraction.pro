	FUNCTION ESCAPE_FRACTION, detectori, E, beta=beta, tilt=tilt

;	the Si, Ge escape fraction.

COMPILE_OPT STRICTARR
	if n_elements(tilt) lt 1 then tilt=0.0
	n = n_elements(E)
	if n gt 1 then begin
		ESCAPE_FRACTION = fltarr(n)
	endif else begin
		ESCAPE_FRACTION = [0.0]
	endelse
	if n_elements(beta) lt 1 then beta=0

	detector = detectori
	if size(detector,/tname) eq 'POINTER' then detector = *detector
	if size(detector,/tname) ne 'STRUCT' then goto, done

	if detector.crystal.Z[0] eq 0 then goto, done

	ct = cos( tilt / !radeg)

	e_loss = escape_energy( detector, beta=beta)
	if e_loss lt 0.01 then goto, done
	ba = 1.0
	if beta then begin
		ba = 0.0
;		if (detector.crystal.N eq 1) and (detector.crystal.Z[0] eq 32) then ba=0.174
		if (detector.crystal.N eq 1) and (detector.crystal.Z[0] eq 32) then ba=0.133
	endif
	q = where( (E gt edge(detector.crystal.Z[0],1)) and (E lt 100.0))
	if q[0] eq -1 then goto, done

	MU_DET = ABSCO( detector.crystal.Z[0], e_loss)				; cm**2/g
	MU_E = ABSCO( detector.crystal.Z[0], E[q])
	D = detector.crystal.thick * 0.001 / ct

	EF = (1.-MU_DET*ALOG(1.+MU_E/MU_DET)/MU_E) / TANH(0.5*MU_E*D)

	ESCAPE_FRACTION[q] = detector.GAMMA * EF * ba

done:
	escape_fraction = reform(escape_fraction)
	if n_elements(escape_fraction) eq 1 then escape_fraction = escape_fraction[0]
	return, escape_fraction
	END

