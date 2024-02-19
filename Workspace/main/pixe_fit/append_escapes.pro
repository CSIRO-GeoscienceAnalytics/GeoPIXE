function append_escapes, pyield, pcuts, e_low, e_high, a, org, mask, note, detector

;	Append escape peaks to the list.
;
;	THIS ROUTINE IS CURRENTLY NOT USED IN PIXE_FIT.
;	ALL ESCAPE PEAKS ARE ADDED IN CORRECT_LINES.

	if ptr_valid(pyield) eq 0 then return, 0
	small = 1.0e-6

	n_els = (*pyield).n_els
	n_layers = n_elements((*pyield).yield[0,*])
	nk = n_elements((*pyield).intensity[*,0])
	array = (*pyield).array
	ratio_intensity = 1.0
	if (*pyield).array then begin
		n_det = n_elements((*pyield).ratio_yield[*,0])
		if tag_present('ratio_intensity', *pyield) eq 0 then begin
			ratio_intensity = replicate( 1.0, n_det, nk, n_els)
		endif else begin
			ratio_intensity =(*pyield).ratio_intensity
		endelse
	endif

	e = (*pyield).e
	intensity = (*pyield).intensity
	lines = (*pyield).lines
	n_lines = (*pyield).n_lines
	use_mu_zero = 0
	mu_zero = 0.0
	if n_elements( (*pyield).mu_zero) gt 1 then begin
		use_mu_zero = 1
		mu_zero = (*pyield).mu_zero
	endif

;	Append escape peaks here (after transmission correction, before cutting lines).
;	Label each using the index for 'esc'.

	do_alpha = 0
	do_beta = 0
	m = 1
	esc = escape_energy( detector)
	if esc gt 0.1 then begin
		do_alpha = 1
		m = 2
		alpha_e = e - esc
		alpha_lines = lines
		alpha_lines[*] = line_index('esc')
		q = where(lines eq line_index('Compton'))
		if q[0] ne -1 then alpha_lines[q] = lines[q]
		alpha_intensity = intensity * escape_fraction( detector, e)

		esc = escape_energy( detector, /beta)
		if esc gt 0.1 then begin
			do_beta = 1
			m = 3
			beta_e = e - esc
			beta_lines = lines
			beta_lines[*] = line_index('esc')
			q = where(lines eq line_index('Compton'))
			if q[0] ne -1 then beta_lines[q] = lines[q]
			beta_intensity = intensity * escape_fraction( detector, e, /beta)
		endif
	endif
	if do_alpha or do_beta then begin
		new_e = fltarr( m*nk, n_els)
		new_lines = intarr( m*nk, n_els)
		new_intensity = fltarr( m*nk, n_els)
		if array then new_ratio_intensity = replicate( 1.0, n_det, m*nk, n_els)
		if use_mu_zero then new_mu_zero = fltarr( m*nk, n_els)

		new_e[0:nk-1,*] = e
		new_lines[0:nk-1,*] = lines
		new_intensity[0:nk-1,*] = intensity
		if array then new_ratio_intensity[*,0:nk-1,*] = ratio_intensity
		if use_mu_zero then new_mu_zero[0:nk-1,*] = mu_zero

		for i=0L,n_els-1 do begin
			new_n_lines = n_lines[i]
			if do_alpha and (n_lines[i] gt 0) then begin
				q = where( (alpha_e[0:n_lines[i]-1,i] ge e_low) and $
						(alpha_intensity[0:n_lines[i]-1,i] gt small))
				if q[0] ne -1 then begin
					nq = n_elements(q)
					new_e[new_n_lines:new_n_lines+nq-1,i] = alpha_e[q,i]
					new_lines[new_n_lines:new_n_lines+nq-1,i] = alpha_lines[q,i]
					new_intensity[new_n_lines:new_n_lines+nq-1,i] = alpha_intensity[q,i]
					new_n_lines = new_n_lines + nq
				endif
			endif
			if do_beta and (n_lines[i] gt 0) then begin
				q = where( (beta_e[0:n_lines[i]-1,i] ge e_low) and $
						(beta_intensity[0:n_lines[i]-1,i] gt small))
				if q[0] ne -1 then begin
					nq = n_elements(q)
					new_e[new_n_lines:new_n_lines+nq-1,i] = beta_e[q,i]
					new_intensity[new_n_lines:new_n_lines+nq-1,i] = beta_intensity[q,i]
					new_lines[new_n_lines:new_n_lines+nq-1,i] = beta_lines[q,i]
					new_n_lines = new_n_lines + nq
				endif
			endif
			n_lines[i] = new_n_lines
		endfor
		nk = max(n_lines) > 1
		e = new_e[0:nk-1,*]
		intensity = new_intensity[0:nk-1,*]
		if array then ratio_intensity = new_ratio_intensity[*,0:nk-1,*]
		if use_mu_zero then mu_zero = new_mu_zero[0:nk-1,*]
		lines = new_lines[0:nk-1,*]
	endif

;	N.B. This has to match the form in 'calc_da_merge_scatter' ('correct_da_loop'), 'correct_lines',
;	'make_peaks', 'read_yield', 'select_element_lines', 'sum_peaks', 'append_escapes', 'pixe_initial'

;	xrays = {n_els:n_els, z:(*pyield).z, shell:(*pyield).shell, n_lines:n_lines, lines:lines, $
;			e:e, intensity:intensity, n_layers:n_layers, yield:(*pyield).yield, $
;			layers:(*pyield).layers, unknown:(*pyield).unknown, $
;			e_beam:(*pyield).e_beam, theta:(*pyield).theta, phi:(*pyield).phi, $
;			alpha:(*pyield).alpha, beta:(*pyield).beta, $
;			title:(*pyield).title, file:(*pyield).file, free:(*pyield).free, $
;			formula:(*pyield).formula, weight:(*pyield).weight, thick:(*pyield).thick, $
;			microns:(*pyield).microns, density:(*pyield).density, $
;			z1:(*pyield).z1, a1:(*pyield).a1, state:(*pyield).state, $
;			emin:(*pyield).emin, emax:(*pyield).emax, mu_zero:mu_zero, $
;			ratio_yield:(*pyield).ratio_yield, array:(*pyield).array, detector_file:(*pyield).detector_file, $
;			ratio_intensity:ratio_intensity  }
;
;	peaks = ptr_new( xrays, /no_copy)

	peaks = make_peaks( n_lines=n_lines, lines=lines, intensity=intensity, $
				e_lines=e, mu_zero=mu_zero, ratio_intensity=ratio_intensity, $
				default=pyield, /pointer )
		
	return, peaks
	end
