function atten, layer, e, _extra=extra

; Return mass-attenuation coefficient (cm^2/mg) for
; 'layer' and energy 'e' (keV).
;
; layer must have the structure {N:int, Z:intarr(), F:fltarr() },
;
; where 'Z' is atomic number, and 'F' is atomic fraction
;	and 'N' is the number of Z,F that are active/used,
;
; or layer can be an array of these structures.
;
; N.B the size of layer.Z and layer.F must be the same,
; and the length of Z,F will be the same in all elements of layer.

	nen = n_elements(E)
	nl = n_elements(layer)
	nn = nen

	if (nen lt 1) or (nl lt 1) then return, 0.0
	if nen eq 1 then begin
		nn = nl										; single E, many layers
		atten = fltarr(nn)

		for i=0L,nl-1 do begin
			if layer[i].N ge 1 then begin
				ml = mass(layer[i].Z[0:layer[i].N-1]) * layer[i].F[0:layer[i].N-1]
				a = absco( layer[i].Z[0:layer[i].N-1], E, _extra=extra)
				sa = total( ml * a)
				s = total( ml)
				atten[i] = (sa/s) / 1000.0
			endif else begin
				atten[i] = 1.0
			endelse
		endfor

	endif else if nl eq 1 then begin				; single layer, many E
		nn = nen
		atten = fltarr(nen)
		if layer.N lt 1 then goto, done

		sa = fltarr(nen)
		s = 0.0
		for i=0L, layer.N-1 do begin
			ml = mass( layer.Z[i]) * layer.F[i]
			ma = ml * absco( layer.Z[i], E, _extra=extra)
			sa = sa + ma
			s = s + ml
		endfor
		atten = (sa/s) / 1000.0

	endif else if nen ne nl then begin
		print,'atten: Layer,E can only both be vectors if they have same length.'
		atten = fltarr(nn)
		goto, done
	endif

done:
	atten = reform(atten)
	if nn eq 1 then atten = atten[0]
	RETURN, atten
END
