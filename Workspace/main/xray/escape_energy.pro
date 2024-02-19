	FUNCTION ESCAPE_ENERGY, detectori, beta=beta

;	the Si, Ge escape energy
;		if /beta, return Kb escape energy offset

	if n_elements(beta) lt 1 then beta=0
	detector = detectori
	if size(detector,/tname) eq 'POINTER' then detector = *detector
	if size(detector,/tname) ne 'STRUCT' then return, escape_fraction

	if (detector.crystal.N eq 1) and (detector.crystal.Z[0] eq 32) then begin
		if beta then begin
		    escape_energy = line_e( atomic_number('Ge'), 'Kb1')
		endif else begin
		    escape_energy = line_e( atomic_number('Ge'), 'Ka1')
		endelse
	endif else if (detector.crystal.N eq 1) and (detector.crystal.Z[0] eq 14) then begin
		if beta then begin
		    escape_energy = 0.0
		endif else begin
		    escape_energy = line_e( atomic_number('Si'), 'Ka_')
		endelse
	endif else begin
	    escape_energy = 0.0
	endelse

	return, escape_energy
	END

