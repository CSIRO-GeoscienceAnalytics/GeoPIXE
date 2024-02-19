	function ion_xsect, Z1, A1, Z2, Shell, E, gamma=gamma

;	Return cross-section (cm^2) for element 'Z2',
;	shell 'Shell' and at energies (MeV) 'E'.
;
;	Only E can be a vector (for Spline efficiency)

	if n_elements(gamma) eq 0 then gamma=0

	if gamma then begin
		return, ion_xsect_pige( Z1, A1, Z2, E)

	endif else begin
		return, ion_xsect_pixe( Z1, A1, Z2, Shell, E)

	endelse

	return, 0.0
	end

