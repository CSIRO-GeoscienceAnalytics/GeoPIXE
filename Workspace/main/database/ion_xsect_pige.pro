	function ion_xsect_pige, Z1, A1, Zi, Ei

;	Return PIGE cross-section (cm^2) for element 'Zi',
;	at energies (MeV) 'Ei'. Scale these by the abundance of the
;	reaction isotope.
;
;	Only Ei can be a vector (for Spline efficiency)

	common c_xsect_F19, e_F19, sig_F19, xsect_F19_OK, xsect_pige_done
	common c_xsect_Na23, e_Na23, sig_Na23, xsect_Na23_OK
;
	z = zi
	e = ei

	nz = n_elements(z)
	nen = n_elements(e)
	nt = nen
	xsect = fltarr(nt)

	if (nz lt 1) or (nen lt 1) then return, 0.0
	if (nz gt 1) then begin
		print,'Ion_xsect_pige: Z can not be a vector.'
	endif

	if( n_elements(xsect_pige_done) eq 0) then xsect_pige_done = 0
	if( xsect_pige_done ne 1) then init_xsect_pige
	if( xsect_pige_done eq 0) then return, xsect

;	Only for protons for now ...

	if (z1 ne 1) or (round(a1) ne 1) then return, xsect

	q = sort(e)
	b = indgen(nen)
	b = b[q]								; back pointers to reverse sort
	e = e[q]

	case z of
		9: begin							; F19  (197 keV, interpolate log plot)

			ok = where( (e ge 0.509) and (e le 4.302))
			if (ok[0] eq -1) or (xsect_F19_OK eq 0) then goto, done

			xsect[ok] = abundance_za(9,19)*0.01 *   $
						exp( spline(e_F19, alog(sig_F19), e[ok], 10.))
			end

		11: begin							; Na23  (440 keV, interpolate log plot)

			ok = where( (e ge 1.019) and (e le 4.065))
			if (ok[0] eq -1) or (xsect_Na23_OK eq 0) then goto, done

			cross = sig_Na23
			q = where(e_Na23 lt 1.67)
			if q[0] ne -1 then cross[q] = 0.062 * cross[q]

			xsect[ok] = abundance_za(11,23)*0.01 *   $
						spline(e_Na23, cross, e[ok], 10.)

			end
		else:
	endcase

	q = sort(b)
	xsect = xsect(q)						; reverse sort E effect

done:
	s = size(xsect)
	if s[0] gt 0 then xsect = reform(xsect)
	if n_elements(xsect) eq 1 then xsect = xsect[0]
	return, xsect
	end
