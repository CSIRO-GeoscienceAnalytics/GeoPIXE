function charge_gain_units, sensitivity, units=gain_units

; Convert 'sensitivity' to 'gain' in the range 1-500 and a 'units' scaling range

COMPILE_OPT STRICTARR

	gain_units = 0.
	if n_elements(sensitivity) eq 0 then return, 0
	if sensitivity lt 1.0e-12 then return, 0.
	
	gain_value = sensitivity
	gain_units = 1.0
	while gain_value lt 0.95 do begin
		gain_value = gain_value * 1000.
		gain_units = gain_units / 1000.
	endwhile
	while gain_value ge 950. do begin
		gain_value = gain_value / 1000.
		gain_units = gain_units * 1000.
	endwhile
	return, gain_value
end
