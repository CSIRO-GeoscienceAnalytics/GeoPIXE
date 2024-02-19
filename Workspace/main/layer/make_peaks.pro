function make_peaks, z2=z2, shell=shell, n_lines=n_lines, lines=lines, intensity=intensity, yield=yield, $
		layers=layers, unknown=unknown, e_beam=e_beam, theta=theta, phi=phi, alpha=alpha, beta=beta, $
		title=title, formula=formula, thick=thicki, microns=micronsi, file=file, free=free, $
		density=densityi, weight=weighti, z1=z1, a1=a1, state=state, e_min=emin, e_max=emax, $
		mu_zero=mu_zero, e_lines=e, ratio_yield=ratio_yield, array=array, detector_file=afile, $
		ratio_intensity=ratio_intensity, beam=beam, default=default, pointer=pointer, error=error

;	Build the peaks struct

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
		warning,'make_peaks',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return, 0
	endif
endif
error = 1

if n_elements(pointer) lt 1 then pointer=0
if n_elements(default) eq 1 then begin
	if size(default,/tname) eq 'POINTER' then begin
		pdefault = default
		use_default = 1
	endif else if size(default,/tname) eq 'STRUCT' then begin
		pdefault = ptr_new( default)
		use_default = 1
	endif else begin
		warning,'make_peaks','Illegal "defaults" supplied.'
		return, 0L
	endelse
endif else use_default=0

if use_default then begin
	if n_elements(title) lt 1 then title=(*pdefault).title
	if n_elements(z1) lt 1 then z1=(*pdefault).z1
	if n_elements(a1) lt 1 then a1=(*pdefault).a1
	if n_elements(state) lt 1 then state=(*pdefault).state
	if n_elements(emin) lt 1 then emin=(*pdefault).emin
	if n_elements(emax) lt 1 then emax=(*pdefault).emax
	if n_elements(mu_zero) lt 1 then mu_zero=(*pdefault).mu_zero

	if n_elements(micronsi) lt 1 then micronsi = (*pdefault).microns
	if n_elements(weighti) lt 1 then weighti = (*pdefault).weight
	if n_elements(densityi) lt 1 then densityi = (*pdefault).density
	if n_elements(thicki) lt 1 then thicki = (*pdefault).thick

	if n_elements(theta) lt 1 then theta = (*pdefault).theta
	if n_elements(phi) lt 1 then phi = (*pdefault).phi
	if n_elements(alpha) lt 1 then alpha = (*pdefault).alpha
	if n_elements(beta) lt 1 then beta = (*pdefault).beta

	if n_elements(e_beam) lt 1 then e_beam = (*pdefault).e_beam
	if n_elements(unknown) lt 1 then unknown = (*pdefault).unknown
	if n_elements(array) lt 1 then array = (*pdefault).array
	if n_elements(ratio_yield) lt 1 then ratio_yield = (*pdefault).ratio_yield
	if n_elements(ratio_intensity) lt 1 then ratio_intensity = (*pdefault).ratio_intensity
	if n_elements(afile) lt 1 then afile = (*pdefault).detector_file
	if n_elements(file) lt 1 then file = (*pdefault).file
	if n_elements(beam) eq 0 then beam = (*pdefault).beam

	if n_elements(layers) lt 1 then layers = (*pdefault).layers
	if n_elements(yield) lt 1 then yield = (*pdefault).yield
	if n_elements(intensity) lt 1 then intensity = (*pdefault).intensity
	if n_elements(lines) lt 1 then lines = (*pdefault).lines
	if n_elements(n_lines) lt 1 then n_lines = (*pdefault).n_lines
	if n_elements(shell) lt 1 then shell = (*pdefault).shell
	if n_elements(z2) lt 1 then z2 = (*pdefault).z
	
	if n_elements(formula) lt 1 then formula = (*pdefault).formula
	if n_elements(e) lt 1 then e = (*pdefault).e
	if n_elements(free) lt 1 then free = (*pdefault).free
	
	n = n_elements(z2)
	nl = n_elements(yield[0,*])
	weight = weighti
	microns = micronsi
	thick = thicki
	density = densityi

endif else begin
	if n_elements(layers) eq 0 then return,0
	if n_elements(title) lt 1 then title=layers[0].name
	if n_elements(z1) lt 1 then z1=1
	if n_elements(a1) lt 1 then a1=1
	if n_elements(state) lt 1 then state=1.0
	if n_elements(emin) lt 1 then emin=0.0
	if n_elements(emax) lt 1 then emax=0.0
	if n_elements(mu_zero) lt 1 then mu_zero=0.0

	n = n_elements(z2)
	nl = n_elements(yield[0,*])

	if n_elements(micronsi) lt 1 then micronsi = replicate(0,nl)
	if n_elements(weighti) lt 1 then weighti = replicate(0,nl)
	if n_elements(densityi) lt 1 then densityi = -1.0

	if n_elements(theta) lt 1 then theta = 135.0
	if n_elements(phi) lt 1 then phi = 0.0
	if n_elements(alpha) lt 1 then alpha = 0.0
	if n_elements(beta) lt 1 then beta = 0.0

	if n_elements(e_beam) lt 1 then e_beam = 3.0
	if n_elements(unknown) lt 1 then unknown = 1
	if n_elements(array) lt 1 then array = 0
	if n_elements(ratio_yield) lt 1 then begin
		ratio_yield = 0
		array = 0
	endif
	if n_elements(ratio_intensity) lt 1 then ratio_intensity = 0
	if n_elements(file) lt 1 then file=''
	if n_elements(afile) lt 1 then begin
		afile = ''
		array = 0
	endif
	if n_elements(beam) eq 0 then begin
		beam = define(/source)
		beam.continuum = 0
		beam.energy = e_beam
	endif
	
	if n_elements(thicki) eq 0 then begin
		thick = layers.thick
	endif else begin
		thick = thicki
	endelse
	if (n_elements(weighti) ge 1) then begin
		weight = weighti
	endif else density = replicate(0, nl)
	if (n_elements(micronsi) ge 1) then begin
		microns = micronsi
	endif else microns = replicate(0, nl)
	if (n_elements(densityi) ge 1) then begin
		density = densityi
	endif else density = replicate(0.0, nl)

	if n_elements(formula) lt 1 then begin
		formula = strarr(nl)
		weight = replicate(0,nl)
		for i=0L,nl-1 do begin
			formula[i] = strjoin( '(' + element_name(layers[i].Z[0:layers[i].N-1]) + ')' +  strip_trail_zero(layers[i].F[0:layers[i].N-1]))
		endfor
	endif

	linmax = n_elements(intensity[*,0])

	if n_elements(e) lt 1 then begin
		e = fltarr(linmax,n)
		for i=0L,n-1 do begin
			for k=0L,n_lines[i]-1 do begin
				e[k,i] = e_line( z2[i], lines[k,i])
			endfor
		endfor
	endif
	free = replicate(1,n)
endelse

;	N.B. This has to match the form in 'calc_da_merge_scatter' ('correct_da_loop'), 'correct_lines',
;	'make_peaks', 'read_yield', 'select_element_lines', 'sum_peaks', 'append_escapes', 'pixe_initial'

	peaks = {n_els:n, z:z2, shell:shell, n_lines:n_lines, lines:lines, $
			e:e, intensity:intensity, n_layers:nl, yield:yield, $
			layers:layers, unknown:unknown, e_beam:e_beam, $
			theta:theta, phi:phi, alpha:alpha, beta:beta, $
			title:title, file:file, free:free, $
			formula:formula, weight:weight, thick:thick, $
			microns:microns, density:density, $
			z1:z1, a1:a1, state:state, emin:emin, emax:emax, $
			mu_zero:mu_zero, ratio_yield:ratio_yield, array:array, detector_file:afile, $
			ratio_intensity:ratio_intensity, beam:beam }

	error = 0
	p = peaks
	if pointer then p = ptr_new(peaks, /no_copy)
	return, p
	end
