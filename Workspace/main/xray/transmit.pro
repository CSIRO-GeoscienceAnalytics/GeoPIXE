function transmit, filteri, Ei, tilt=tilt, pressure=pressurei, temp=tempi, _extra=extra

; Calculate the transmission through all the 'filter' at
; energy 'e' (keV).
;
; If 'presssure' and 'temp' present, this indicates a gas filter.
;	In this case assume thick is mg/cm2 NPT (1013.25 mbar, 20 C) and scale this for new PT.
;	This is only for normal filters, not pinholes.
;	These are scalars, for ambient conditions.
;
; NOTE: Transmit does NOT use the .microns parameter; all thick
; must be mg/cm^2.
;
; Unlike other routines, if there are many 'e' then the result
; is a vector, with the transmission through all filters at each 'e'.
;
; 'filter' is an array of filters, or a pointer to an array.
;
; 'filter' must have the structure :
;	{N:int, Z:intarr(), F:fltarr(), thick:float,
;			pinhole:int, pinratio: float },
;
; where 'Z' is atomic number, and 'F' is atomic fraction,
;	thick is the thickness in mg/cm^2, and pinratio is the
;	pinhole solid-angle ratio and pinhole flags a pinhole filter,
;	and 'N' is the number of Z,F that are active/used,
;
; N.B the size of layer.Z and layer.F must be the same,
; and the length of Z,F will be the same in all elements of layer.

	if n_elements(tilt) lt 1 then tilt=0.0
	if n_elements(ei) lt 1 then return, 0.0
	e = ei
	nen = n_elements(E)
	if (nen lt 1) then return, 0.0
	transmit = E
	transmit[*] = 1.0									; has same dimensions as E

	if n_elements(filteri) lt 1 then goto, done
	filter = filteri
	if size(filter,/tname) eq 'POINTER' then begin
		if ptr_valid(filter) eq 0 then goto, done
		filter = *filter
	endif
	if size(filter,/tname) ne 'STRUCT' then goto, done

	nl = n_elements(filter)
	if (nl lt 1) then goto, done
	gas_on = 0
	if (n_elements(pressurei) ge 1) and (n_elements(tempi) ge 1) then begin
		gas_on = 1
		if pressurei lt 0.001 then gas_on=0
	endif

	ct = cos( double(tilt) / !radeg)

	tags = tag_names(filter)
	found_bragg = 0
	q = where( tags eq 'BRAGG')
	if q[0] ne -1 then found_bragg = 1
	found_pinhole = 0
	q = where( tags eq 'PINHOLE')						; are there Pinhole tags in Struct?

	if q[0] ne -1 then begin
		found_pinhole = 1								; which are normal pinhole filters
		pinhole = filter.pinhole
		q = where( (filter.pinhole eq 1) and (filter.pinratio gt 1.0), nq)

		if nq gt 0 then begin
			q2 = reverse(sort(filter[q].pinratio))
			h = [filter[q[q2]].pinratio,1.0]			; hole ratios, and 1.0 for one more index
			f = 1.0
			for i=0L,nq-1 do begin
				a = atten(filter[q[q2[i]]], E, _extra=extra)
				f = f * exp(double(-a*filter[q[q2[i]]].thick)/ct)

				transmit = f*(h[i] - h[i+1])/h[i] + transmit*(h[i+1]/h[i])
			endfor
		endif
	endif

	if found_pinhole then begin
		q = where( filter.pinhole ne 1, nq)				; which are not pinhole filters
	endif else begin
		pinhole = replicate(0, nl)
		q = indgen(nl)
		nq = nl
	endelse

	if gas_on then begin
		pressure = replicate( 1013.25, nl)				; detect gas filters as low density
		temp = replicate( 20., nl)						; default to NPT conditions

		for i=0L,nq-1 do begin
			if (filter[q[i]].density gt 0.) and (filter[q[i]].density lt 0.03) then begin
				pressure[q[i]] = pressurei
				temp[q[i]] = tempi
			endif
		endfor
	endif

	if nq gt 0 then begin
		for i=0L,nq-1 do begin
			a = atten(filter[q[i]], E, _extra=extra)
			t = filter[q[i]].thick

			case pinhole[q[i]] of
				0: begin								; normal filter
					scale = 1.0
					if gas_on then begin
						scale = scale * (pressure[q[i]] > 0.001)/1013.25
					endif
					if gas_on  then begin
						scale = scale * 293.15/(temp[q[i]] + 273.15)
					endif

					transmit = transmit * exp(double(-a*scale*t/ct))
					end
				2: begin								; Bragg filter plus foil

					transmit = transmit * exp(double(-a*t/ct))
					if found_bragg then transmit = transmit * hopg_function( filter[q[i]].Bragg, E)
					end
			endcase
		endfor
	endif

done:
	transmit = reform(transmit)
	if n_elements(transmit) eq 1 then transmit = transmit[0]
	RETURN, transmit
END
