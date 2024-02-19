function energy_compton, e, thetad, multiple_scattering=multiple

; Return energy of Compton scattering of 'e' keV into angle 'thetad' (degrees)
; Return for every theta, unless multiple scattering, in which case calculate
; multiple scattering, for sequence of scattering angles theta, for each e.

COMPILE_OPT STRICTARR

if n_elements(multiple) lt 1 then multiple=0

theta = thetad/!radeg							;convert degree to radian!!

;510.996 = mc^2 = rest mass energy for electron

if multiple then begin
	ecom = 1.
	for i=0L,n_elements(theta)-1 do begin
		ecom = ecom * 510.996/(510.996 + e*(1-cos(theta[i])))
	endfor
endif else begin
	ecom = 510.996/(510.996 + e*(1-cos(theta)))
endelse


return, ecom*e
end
