pro charge_gain_unit_lists, vals, units, vunit, alt_units=alt_units, ionbeam=ionbeam, times=times

; Return standard list if flux/charge gains and units for droplists and conversions

COMPILE_OPT STRICTARR
if n_elements(ionbeam) eq 0 then ionbeam=0

	vals = [1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.,200.,300.,400.,500.,600.,700.,800.,900.]
	if ionbeam then begin
		units = ['  -  ', 'fC', 'pC', 'nC', 'uC', 'mC', 'C', 'ms']
		alt_units = ['  -  ', 'fC', 'pC', 'nC', 'uC', 'mC', 'C', 'ms']
		vunit = [0., 1.0e-12, 1.0e-9, 1.0e-6, 0.001, 1.0, 1000.0, 1.0]
	endif else begin
		units = ['  -  ', 'aA/V', 'fA/V', 'pA/V', 'nA/V', 'uA/V', 'mA/V', 'A/V', 'kA/V', 'ms']
		alt_units = ['  -  ', 'aA/Hz', 'fA/Hz', 'pA/Hz', 'nA/Hz', 'uA/Hz', 'mA/Hz', 'A/Hz','kA/Hz', 'ms']
		vunit = [0., 1.0e-9, 1.0e-6, 0.001, 1.0, 1000.0, 1000000.0, 1.0e+9, 1.0e+12, 1.0]
	endelse
	times = replicate( 0, n_elements(units))
	times[ n_elements(units)-1] = 1					; last one is a time
	return
end
