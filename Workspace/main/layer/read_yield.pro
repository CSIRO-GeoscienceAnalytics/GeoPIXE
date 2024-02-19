function read_yield, F, error=error, many=many

; Read yield definitions from 'F', return pointer to struct(s).
; Returns 'p' pointer to yields struct, or array of yields structs
; if /many is set.
; The array form is used either for a series of yield calculation
; with layer thicknesses varying (1D or 2D grid for one or two layers)
; or multiple beam energies(?).

COMPILE_OPT STRICTARR
error = 1
if n_params() lt 1 then return, 0
if lenchr(F) lt 1 then return, 0
if n_elements(many) lt 1 then many=0
error = 0

	on_ioerror, bad_io
	close, 1
	openr, 1, F, /XDR

	valid_versions = [-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]

	version = 0L
	readu,1, version
	q = where( version eq valid_versions)
	if q[0] eq -1 then goto, bad_version

	ny = 1L
	if version le -6 then begin
		readu,1, ny
	endif
	first = 1
	i = 0

more:
	n = 0
	nk = 0
	nl = 0
	readu,1, n, nk, nl
	if (n le 0) or (n gt 300) then goto, bad_io
	if (nk le 0) or (nk gt 100) then goto, bad_io
	if (nl le 0) or (nl gt 100) then goto, bad_io

	title = ''
	readu,1, title

	z2 = intarr(n)
	shell = intarr(n)
	n_lines = intarr(n)
	lines = intarr(nk,n)
	intensity = fltarr(nk,n)
	e = fltarr(nk,n)
	yield = fltarr(n,nl)
	layers = replicate( make_layer('Si',1.0), nl)
	unknown = 0
	e_beam = 0.0
	theta = 0.0
	phi = 0.0
	alpha = 0.0
	beta = 0.0
	free = replicate(1,n)
	z1 = 1
	a1 = 1
	state = 1.0
	emin = 0.0
	emax = 0.0
	array = 0
	ratio_yield = 0
	detector_file = ''

	if nl gt 1 then begin
		formula = strarr(nl)
		thick = fltarr(nl)
		weight = intarr(nl)
		microns = intarr(nl)
		density = fltarr(nl)
	endif else begin
		formula = ''
		thick = 0.0
		weight = 0
		microns = 0
		density = 0.0
	endelse

	readu,1, z2, shell, n_lines
	readu,1, lines
	readu,1, intensity, e
	readu,1, yield
	readu,1, layers
	readu,1, unknown, e_beam
	readu,1, theta, phi, alpha, beta

	if version eq -1 then begin
		if nl gt 1 then begin
			formula = strarr(nl)
			weight = replicate(0,nl)
			microns = replicate(0, nl)
			density = replicate(0.0, nl)
		endif else begin
			formula = ''
			weight = 0
			microns = 0
			density = 0.0
		endelse
		thick = layers.thick
		for j=0L,nl-1 do begin
			Nj = layers[j].N
			formula[j] = strjoin( '(' + element_name(layers[j].Z[0:Nj-1]) + ')' +  $
										strip_trail_zero(layers[j].F[0:Nj-1]) )
		endfor
	endif

	if version le -2 then begin
		readu,1, formula
		readu,1, weight, thick
		readu,1, microns, density
		readu,1, z1, a1, state
	endif

	if version le -3 then begin
		readu,1, emin, emax
	endif

	got_ratio_intensity = 0L
	ratio_intensity = 0L
	if version le -5 then begin
		readu,1, array
		if array then begin
			n_det = 0L
			readu,1, n_det
			ratio_yield = fltarr(n_det,n)
			readu,1, ratio_yield
			readu,1, detector_file

			ratio_intensity = replicate( 1.0, n_det, nk, n)
			if version le -7 then begin
				readu,1, got_ratio_intensity
				if got_ratio_intensity then begin
					readu,1, ratio_intensity
				endif
			endif
		endif
	endif

	mu_zero = 0.0
	if version le -4 then begin
		use_mu_zero = 0L
		readu,1, use_mu_zero
		if use_mu_zero then begin
			mu_zero = fltarr(nk,n)
			readu,1, mu_zero
		endif
	endif

	beam = define(/source)
	beam1 = define(/old_source2)
	if version le -10 then begin
		beam0 = beam
	endif else if version le -9 then begin
		beam0 = beam1
	endif else begin
		beam0 = define(/old_source1)
	endelse
	if version le -8 then begin
		use_beam = 0L 
		readu,1, use_beam
		if use_beam then begin
			readu,1, beam0
		endif
	endif
	struct_assign, beam0, beam, /nozero
	if version eq -9 then begin
		ep = 0.33 * findgen(300)							; must be 300 elements
		trans = xos_transmission(ep) / xos_transmission(beam.poly.energy)
		beam.poly.etrans = ep								; transmission table energies (keV)
		beam.poly.trans = trans								; transmissions
	endif

;	N.B. This has to match the form in 'calc_da_merge_scatter' ('correct_da_loop'), 'correct_lines',
;	'make_peaks', 'read_yield', 'select_element_lines', 'sum_peaks', 'append_escapes', 'pixe_initial'

	y = make_peaks( z2=z2, shell=shell, n_lines=n_lines, lines=lines, intensity=intensity, yield=yield, $
				layers=layers, unknown=unknown, e_beam=e_beam, file=F, free=free, $
				theta=theta, phi=phi, alpha=alpha, beta=beta, $
				e_lines=e, title=title, detector_file=detector_file, array=array, $
				formula=formula, thick=thick, microns=microns, $
				density=density, weight=weight, $
				z1=z1, a1=a1, state=state, e_min=emin, e_max=emax, mu_zero=mu_zero, $
				ratio_yield=ratio_yield, ratio_intensity=ratio_intensity, $
				beam=beam )
		
	if i eq 0 then begin
		peaks = y
	endif else begin
		peaks = [peaks,y]
	endelse
	if many and (i+1 lt ny) then begin
		i = i+1
		goto, more
	endif
	close, 1

p = ptr_new( peaks, /no_copy)
return, p

bad_io:
	if arg_present(error) eq 0 then warning, 'read_yield', 'Read_yield: bad yield I/O'
	error = 1
	return, 0
bad_version:
	if arg_present(error) eq 0 then warning, 'read_yield', 'Read_yield: bad file version'
	error = 1
	return, 0
bad_source:
	if arg_present(error) eq 0 then warning, 'read_yield', 'Read_yield: bad source file'
	error = 1
	return, 0
end

