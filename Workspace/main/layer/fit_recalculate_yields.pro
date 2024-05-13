function fit_recalculate_yields, p, peaks=peaks, gamma=gamma, pressure=pressure, temp=temp, error=error

;	If changed ambient conditions detected in new spectrum data, recalculate yields
;	passing the new ambient P,T to the source model.
;
;	Based on execution of 'geo_array_yield' (not 1D or 2D options), as used in 'layer_setup'.
;	Keep additions here in tune with those in 'calculate-button' event in 'layer_setup_event'.
;
;	p			'fit_setup' pars struct pointer
;	peaks		pointer to struct for yields of ALL elements
;
;	/gamma		flags gamma mode

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'fit_recalculate_yields',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0
	endif
endif
error = 1

	py = (*p).yields						; full yields struct from 'layer_setup' peaks.
	pdetector = (*p).detector				; detector pointer
	playout = (*p).playout					; detector layout
	psource = ptr_new( (*py).beam)			; source struct

;	Note for non continuum beams these return passively without 'error' ...

	case (*py).beam.model of
		1: begin
			source_calculate, psource, convert=0, pressure=pressure, temp=temp, error=error
			end
		2: begin
			pink_calculate, psource, convert=0, pressure=pressure, temp=temp, error=error
			end
		else:
	endcase
	if error then goto, bad_source

	yield = geo_array_yield( (*py).formula, (*py).thick, microns=(*py).microns, density=(*py).density, weight=(*py).weight, $
				energy=(*py).e_beam, theta=(*py).theta, phi=(*py).phi, beam=*psource, $
				alpha=(*py).alpha, beta=(*py).beta, unknown=(*py).unknown, gamma=gamma, $
				array=(*py).array, detector=pdetector, layout=playout, z1=(*py).z1, a1=(*py).a1, state=(*py).state, $
				e_min=(*py).emin, e_max=(*py).emax, /progress, error=error, $

				intensity=intensity, lines=lines, n_lines=n_lines, z2=z2, shell=shell, $
				layers=layers, sec_yield=sec_yield, mu_zero=mu_zero, e_lines=e_lines, $
				ratio_yield=ratio_yield, ratio_intensity=ratio_intensity )

	print,'Done geo_array_yield.'
	if error then goto, bad_yield

	peaks = make_peaks( z2=z2, shell=shell, n_lines=n_lines, lines=lines, intensity=intensity, yield=yield, $
				layers=layers, unknown=(*py).unknown, e_beam=(*py).e_beam, theta=(*py).theta, $
				phi=(*py).phi, alpha=(*py).alpha, beta=(*py).beta, $
				title=(*py).title, formula=(*py).formula, thick=(*py).thick, microns=(*py).microns, $
				density=(*py).density, weight=(*py).weight, z1=(*py).z1, a1=(*py).a1, $
				state=(*py).state, e_min=(*py).emin, e_max=(*py).emax, mu_zero=mu_zero, $
				e_lines=e_lines, ratio_yield=ratio_yield, array=(*py).array, detector_file=(*pdetector).file, $
				ratio_intensity=ratio_intensity, beam=*psource, /pointer )
	
	error = 0
	return, yield

bad_source:
	warning,'fit_recalculate_yields','bad source calculation'
	return, 0
bad_yield:
	warning,'fit_recalculate_yields','bad yields calculation'
	return, 0
end
