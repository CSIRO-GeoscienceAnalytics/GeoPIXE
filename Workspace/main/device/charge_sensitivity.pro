function charge_sensitivity, val, unit, ionbeam=ionbeam, time=time

; Convert a 'val' multiplier and a 'unit' string into a sensitivity value.
; If a time unit, return time=1.
; Set /ionbeam for ionbeam use.

COMPILE_OPT STRICTARR
	sens = 0.
	if n_elements(ionbeam) eq 0 then ionbeam=0
	if n_elements(val) eq 0 then return, sens
	if n_elements(unit) eq 0 then return, sens
	if (size( val, /tname) ne 'FLOAT') and (size( val, /tname) ne 'DOUBLE') then begin
		warning,'charge_sensitivity','Type for "val" should be "FLOAT".'
	endif
	if (size( unit, /tname) ne 'STRING') then begin
		warning,'charge_sensitivity','Type for "unit" should be "STRING".'
	endif
	
	charge_gain_unit_lists, vals, units, vunits, alt_units=alt_units, times=times, ionbeam=ionbeam

	time = 0
	q = where( strlowcase(unit) eq strlowcase(units), nq)
	if nq eq 0 then begin
		q = where( strlowcase(unit) eq strlowcase(alt_units), nq)
		if nq eq 0 then return, val
	endif
	time = times[q[0]]
	
	return, val * vunits[q[0]]
end
