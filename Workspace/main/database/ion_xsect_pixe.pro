	function ion_xsect_pixe, Z1, A1i, Zi, Shelli, Ei

;	Return PIXE cross-section (cm^2) for element 'Zi',
;	shell 'Shelli' and at energies (MeV) 'Ei'.
;
;	Only Ei can be a vector (for Spline efficiency)
;
;	The cross-section tables are based on 1 AMU.
;	Need to divide by A1 before interpolating.

	common c_xsect_k1, z2_k, index_k, e_k, sig_k, xsect_K_OK
	common c_xsect_L1, z2_L, index_L, e_L, sig_L, xsect_L_OK
	common c_xsect_m1, z2_m, index_m, e_m, sig_m, xsect_m_OK
;
	z = zi
	shell = shelli
	e = ei
	a1 = round(a1i) + excess(z1,a1i)/931.481

	nz = n_elements(z)
	ns = n_elements(shell)
	nen = n_elements(e)
	nt = nen
	xsect = fltarr(nt)

	if (nz lt 1) or (ns lt 1) or (nen lt 1) then return, 0.0
	if (nz gt 1) or (ns gt 1) then begin
		print,'Ion_xsect: Z,Shell can not be vectors.'
	endif

	if( n_elements(xsect_K_OK) eq 0) then xsect_K_OK = 0
	if( n_elements(xsect_L_OK) eq 0) then xsect_L_OK = 0
	if( n_elements(xsect_M_OK) eq 0) then xsect_M_OK = 0
	if( xsect_K_OK ne 1) then init_xsect
	if( xsect_K_OK ne 1) then return, xsect
	if( xsect_L_OK ne 1) then return, xsect
	if( xsect_M_OK ne 1) then return, xsect

;	Now interpolate the log ...

	q = sort(e)
	b = indgen(n_elements(e))
	b = b[q]							; back pointers to reverse sort
	e = e[q]

	if shell[0] eq 1 then begin
		if index_K[z[0]] ge 0 then $
			xsect = exp( spline(e_K, alog(sig_K[*,index_K[z[0]]] > 1.0e-37), e/A1)) * z1^2
	endif else	if shell[0] eq 2 then begin
		if index_L[z[0]] ge 0 then $
			xsect = exp( spline(e_L, alog(sig_L[*,index_L[z[0]]] > 1.0e-37), e/A1)) * z1^2
	endif else	if shell[0] eq 3 then begin
		if index_M[z[0]] ge 0 then $
			xsect = exp( spline(e_M, alog(sig_M[*,index_M[z[0]]] > 1.0e-37), e/A1)) * z1^2
	endif

	q = sort(b)
	xsect = xsect(q)					; reverse sort E effect

done:
	s = size(xsect)
	if s[0] gt 0 then xsect = reform(xsect)
	if n_elements(xsect) eq 1 then xsect = xsect[0]
	return, xsect
	end