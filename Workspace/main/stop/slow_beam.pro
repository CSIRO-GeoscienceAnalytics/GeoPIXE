function rk4_de_dx, x, e

; Call dedrox, but veto z,A = 0,0

common c_rk4_de_dx, cde_z1, cde_a1, cde_slices, cde_islice

if (cde_z1 ne 0) and (cde_a1 ne 0) then begin
	return, -dedrox(cde_z1, cde_a1, cde_slices[cde_islice], e)
endif else begin
	return, replicate(0.0, n_elements(e))
endelse
end

;----------------------------------------------------------------

function slow_beam, z1, a1, e0, slices, last=last, distance=xhalf, $
					normal=normal, cos_beam=cos_beam, dydx=dydx

;	Apply dedrox to slow beam through the slices array 'slices',
;	and return the energy at each slice CENTRE. 
;	'Cos_beam' gives the path increase factor for target tilt.
;	No energy loss for z1=0 or a1=0.0 (photons).

;	General: slices can vary in thickness within a layer.
;
;	Returns:
;	'last'			is the last slice at range, or the end of 'slices'.
;	'distance'		along beam, or normal to surface if /normal.
;	'dydx'			stopping power in half-slice steps (starting at front).

	COMPILE_OPT STRICTARR
	ErrorNo = 0
	common c_errors_1, catch_errors_on
	if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
	if catch_errors_on then begin
		Catch, ErrorNo
		if (ErrorNo ne 0) then begin
			Catch, /cancel
			on_error, 1
			help, calls = s
			n = n_elements(s)
			c = 'Call stack: '
			if n gt 2 then c = [c, s[1:n-2]]
			warning,'slow_beam',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			error = 1
			return, 0.0
		endif
	endif
	common c_rk4_de_dx, cde_z1, cde_a1, cde_slices, cde_islice

	if n_elements(cos_beam) lt 1 then cos_beam = 1.0
	if n_elements(normal) lt 1 then normal = 0

	x = 0.0
	e = e0
	cde_slices = slices
	cde_z1 = z1
	cde_a1 = a1
	ns = n_elements(slices)
	ehalf = fltarr(2*ns+1)
	xhalf = fltarr(2*ns+1)
	dydx = fltarr(2*ns+1)

	ehalf[0] = e0
	k = 1
	n = 1
	for j=0L,ns-1 do begin
		cde_islice = j
		s = 0.5* slices[j].thick / cos_beam

		dydx[k] = rk4_de_dx(x,e)					; first half of slice
		e = rk4( e, dydx[k], x, s, 'rk4_de_dx')
		if finite(e) eq 0 then e=0.0
		x = x+s
		ehalf[k] = e								; mid point of slice
		xhalf[k] = x
		k = k+1
		if e lt 0.03 then goto, done

		dydx[k] = rk4_de_dx(x,e)					; second half of slice
		e = rk4( e, dydx[k], x, s, 'rk4_de_dx')
		if finite(e) eq 0 then e=0.0
		x = x+s
		ehalf[k] = e								; end of slice
		xhalf[k] = x
		k = k+1
		if e lt 0.03 then goto, done
		n = k
	endfor

done:
	dydx[0] = dydx[1]
	last = (n-1)/2 - 1
	xhalf = xhalf[0:n-1]
	ehalf = ehalf[0:n-1]
	dydx = -dydx[0:n-1]
	if normal then xhalf = xhalf*cos_beam
	if n lt 3 then begin
		warning,'slow_beam','stopped soon ...'
	endif
	return, ehalf
	end


