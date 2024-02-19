function select_element_lines, pyield, z, shelli, el_free, use_m=use_m, $
			no_extras=no_extras

;	Prune down the full 'pyield' list to only include elements given by 'z'
;	and 'shelli'. Add extra lines for XRF for Elastic and Compton.
;	Pass on 'el_free' to the peaks.free parameters.
;
;	use_m =	0		Detect shell=3 to mean 'K+L' (both shell=1 and 2), for 4 state buttons.
;			1		Detect shell=4,5 as 'K+L' (both 1,2), 'L+M' (both 2,3) for 6 state buttons.
;					Set this up where 'periodic_table' is made in 'fit_setup'.
;
;	/no_extras		Veto adding Compton and elastic (used in sim geopixe).

	if n_elements(use_m) lt 1 then use_m=0
	if ptr_good(pyield) eq 0 then begin
		warning, 'select_element_lines', 'Invalid "pyield" supplied.'
		return, ptr_new()
	endif
	if n_elements(z) lt 1 then begin
		warning, 'select_element_lines', 'No element selections "z" supplied.'
		return, ptr_new()
	endif
	if n_elements(z) ne n_elements(shelli) then begin
		warning, 'select_element_lines', 'Element selections "z" supplied do not match "shell".'
		return, ptr_new()
	endif
	n_els = (*pyield).n_els
	n_layers = n_elements((*pyield).yield[0,*])							; number of layers
	nk = n_elements((*pyield).intensity[*,0])							; max number of lines
	if n_elements(el_free) lt 1 then el_free = replicate(1,n_els)
	use_mu_zero = 0
	mu_zero = 0.0
	if n_elements( (*pyield).mu_zero) gt 1 then use_mu_zero=1
	if n_elements( no_extras) eq 0 then no_extras=0

;	Find elements in list, label fit/mdl using free

	good = intarr(n_els)
	free = intarr(n_els)
	for i=0L, n_elements(z)-1 do begin
		if use_m then begin
			if shelli[i] eq 4 then begin
				q = where( (z[i] eq (*pyield).z) and (((*pyield).shell eq 1) or ((*pyield).shell eq 2)) )
			endif else if shelli[i] eq 5 then begin
				q = where( (z[i] eq (*pyield).z) and (((*pyield).shell eq 2) or ((*pyield).shell eq 3)) )
			endif else begin
				q = where( (z[i] eq (*pyield).z) and (shelli[i] eq (*pyield).shell) )
			endelse
		endif else begin
			if shelli[i] eq 3 then begin
				q = where( (z[i] eq (*pyield).z) and (((*pyield).shell eq 1) or ((*pyield).shell eq 2)) )
			endif else begin
				q = where( (z[i] eq (*pyield).z) and (shelli[i] eq (*pyield).shell) )
			endelse
		endelse
		if q[0] ne -1 then begin
			good[q] = 1
			free[q] = el_free[i]
		endif
	endfor
	q = where( good eq 1)
	if q[0] eq -1 then return, ptr_new()

;	Reduce arrays to only these selected elements

	z2 = (*pyield).z[q]
	n_els = n_elements(z2)
	shell = (*pyield).shell[q]
	free = free[q]
	n_lines = (*pyield).n_lines[q]
	lines = (*pyield).lines[*,q]
	e = (*pyield).e[*,q]
	intensity = (*pyield).intensity[*,q]
	if use_mu_zero then mu_zero = (*pyield).mu_zero[*,q]
	yield = (*pyield).yield[q,*]

	ratio_yield = 0.
	array = (*pyield).array
	ratio_intensity = 0.
	if array then begin
		n_det = n_elements((*pyield).ratio_yield[*,0])				; number of detectors (for array)
		ratio_yield = (*pyield).ratio_yield[*,q]
		if tag_present('ratio_intensity', *pyield) then ratio_intensity = (*pyield).ratio_intensity[*,*,q]
	endif

;goto, cont
	photo = 0
	if (*pyield).z1 eq 0 and (*pyield).a1 eq 0 then photo=1
	
;	For SXRF want to model Rayleigh & Compton peaks - add in as fake elements ...
;	For continuum source, add all significant anode lines as both Compton and elastic "elements" ...
;	Make sure any selected elements, that are the same as the anode elements, are disabled.

	if (photo eq 1) and (no_extras eq 0) then begin
		add_lines = 1
		if (*pyield).beam.continuum then begin
			qc = where( (*pyield).beam.lines.z ne 0, nqc)
			if nqc gt 0 then begin
				nlc = n_elements( (*pyield).beam.lines.e[*,0]) < nk

;				z2c = intarr(2*nqc)
;;				z2c = [(*pyield).beam.lines.z[qc],(*pyield).beam.lines.z[qc]]
;				shellc = [(*pyield).beam.lines.shell[qc],(*pyield).beam.lines.shell[qc]]
;				freec = replicate(1,2*nqc)
;				n_linesc = [(*pyield).beam.lines.n_lines[qc],(*pyield).beam.lines.n_lines[qc]]
;				nk2 = n_elements((*pyield).beam.lines.e[*,0])
;				linesc = transpose([replicate(line_index('Compton'),nqc,nlc),replicate(line_index('elastic'),nqc,nlc)])
;				ec = transpose([transpose(energy_compton( (*pyield).beam.lines.e[0:nlc-1,qc], (*pyield).theta)),transpose((*pyield).beam.lines.e[0:nlc-1,qc])])
;				intensityc = transpose([transpose((*pyield).beam.lines.rel[0:nlc-1,qc]),transpose((*pyield).beam.lines.rel[0:nlc-1,qc])])
;				if (*pyield).array then begin
;					ratio_yieldc = replicate( 1.0, n_det, 2*nqc)
;					if tag_present('ratio_intensity', *pyield) then begin
;						ratio_intensityc = replicate( 1.0, n_det, nk, 2*nqc)
;					endif
;				endif
;				if use_mu_zero then begin
;					mu_zeroc = replicate(0.1, nk, 2*nqc)
;				endif	
;				if n_layers gt 1 then begin
;					yieldc = replicate(1.0, 2*nqc,n_layers)
;				endif else begin
;					yieldc = replicate(1.0, 2*nqc)
;				endelse	

;				Add only the Compton lines for now ...

				z2c = intarr(nqc)
;				z2c = [(*pyield).beam.lines.z[qc]]
				shellc = [(*pyield).beam.lines.shell[qc]]
				freec = replicate(1,nqc)
				n_linesc = [(*pyield).beam.lines.n_lines[qc]]
				nk2 = n_elements((*pyield).beam.lines.e[*,0])
				linesc = transpose([replicate(line_index('Compton'),nqc,nlc)])
				ec = transpose([transpose(energy_compton( (*pyield).beam.lines.e[0:nlc-1,qc], (*pyield).theta))])
				intensityc = transpose([transpose((*pyield).beam.lines.rel[0:nlc-1,qc])])

				if (*pyield).beam.poly.mode eq 1 then begin
					if (*pyield).beam.poly.model eq 'XOS default' then begin
;						intensityc = intensityc * xos_transmission(ec)				; @3-23
						q1 = where( (ec le max((*pyield).beam.poly.etrans)) and (ec ge min((*pyield).beam.poly.etrans)) and (ec gt 0.), nq1)
						q2 = where( (*pyield).beam.poly.etrans gt 0., nq2)
						if (nq1 eq 0) or (nq2 eq 0) then begin
							warning,'select_element_lines', ['Polycapillary table does not overlap with desired energies.', $
											'Table spans energies: '+str_tidy(min((*pyield).beam.poly.etrans))+' to '+str_tidy(max((*pyield).beam.poly.etrans)), $
											'Desired energies: '+str_tidy(min(ec))+' to '+str_tidy(max(ec))]
						endif else begin
							intensityc[q1] = intensityc[q1] * interpol( (*pyield).beam.poly.trans[q2], (*pyield).beam.poly.etrans[q2], ec[q1])
						endelse
					endif
				endif

				if (*pyield).array then begin
					ratio_yieldc = replicate( 1.0, n_det, nqc)
					if tag_present('ratio_intensity', *pyield) then begin
						ratio_intensityc = replicate( 1.0, n_det, nk, nqc)
					endif
				endif
				if use_mu_zero then begin
					mu_zeroc = replicate(0.1, nk, nqc)
				endif	
				if n_layers gt 1 then begin
					yieldc = replicate(1.0, nqc,n_layers)
				endif else begin
					yieldc = replicate(1.0, nqc)
				endelse	

				for i=0,nqc-1 do begin												; disable any fitting elements that
					qe = where( (z2 eq z2c[i]) and (shell eq shellc[i]), nqe)		; are also anode elements
					if nqe gt 0 then free[qe] = 0									; (does not work with z2c set to ZERO above)
				endfor
			endif else add_lines=0
		endif else begin
			z2c = [0,0]
			shellc = [2,3]
			freec = [1,1]
			n_linesc = [1,1]
			linesc = intarr( nk, 2)
			linesc[0,0] = line_index('Compton')
			linesc[0,1] = line_index('elastic')
			nk2 = nk
			ec = fltarr( nk, 2)
			ec[0,0] = energy_compton( (*pyield).e_beam, (*pyield).theta)
			ec[0,1] = (*pyield).e_beam
			intensityc = fltarr( nk, 2)
			intensityc[0,0] = 1.0
			intensityc[0,1] = 1.0
			if (*pyield).array then begin
				ratio_yieldc = replicate( 1.0, n_det,2)
				if tag_present('ratio_intensity', *pyield) then begin
					ratio_intensityc = replicate( 1.0, n_det, nk, 2)
				endif
			endif
			if use_mu_zero then begin
				mu_zeroc = replicate(0.1, nk, 2)
			endif
			if n_layers gt 1 then begin
				yieldc = replicate(1.0, 2,n_layers)
			endif else begin
				yieldc = replicate(1.0, 2)
			endelse
		endelse

		if add_lines then begin
			n1 = n_elements(z2)
			n2 = n_elements(z2c)
			z2 = [z2, z2c]
			n_els = n1+n2
			shell = [shell, shellc]
			free = [free,freec]
			n_lines = [n_lines, n_linesc]
			nlm = nk < nk2
			lines = intarr(nk,n_els)
			lines[*,0:n1-1] = (*pyield).lines[*,q]
			lines[0:nlm-1,n1:n1+n2-1] = linesc[0:nlm-1,*]
			e = fltarr(nk,n_els)
			e[*,0:n1-1] = (*pyield).e[*,q]
			e[0:nlm-1,n1:n1+n2-1] = ec[0:nlm-1,*]
			intensity = fltarr(nk,n_els)
			intensity[*,0:n1-1] = (*pyield).intensity[*,q]
			intensity[0:nlm-1,n1:n1+n2-1] = intensityc[0:nlm-1,*]
			if (*pyield).array then begin
				ratio_yield = fltarr(n_det,n_els)
				ratio_yield[*,0:n1-1] = (*pyield).ratio_yield[*,q]
				ratio_yield[*,n1:n1+n2-1] = ratio_yieldc[*,*]
				if tag_present('ratio_intensity', *pyield) then begin
					ratio_intensity = fltarr( n_det, nk, n_els)
					ratio_intensity[*,*,0:n1-1] = (*pyield).ratio_intensity[*,*,q]
					ratio_intensity[*,0:nlm-1,n1:n1+n2-1] = ratio_intensityc[*,0:nlm-1,*]
				endif
			endif
			if use_mu_zero then begin
				mu_zero = fltarr(nk,n_els)
				mu_zero[*,0:n1-1] = (*pyield).mu_zero[*,q]
				mu_zero[0:nlm-1,n1:n1+n2-1] = mu_zeroc[0:nlm-1,*]
			endif
			yield = [yield,yieldc]		
		endif
	endif

;	N.B. This has to match the form in 'calc_da_merge_scatter' ('correct_da_loop'), 'correct_lines',
;	'make_peaks', 'read_yield', 'select_element_lines', 'sum_peaks', 'append_escapes', 'pixe_initial'

cont:

	p = make_peaks( z2=z2, shell=shell, n_lines=n_lines, lines=lines, intensity=intensity, yield=yield, $
					e_lines=e, mu_zero=mu_zero, ratio_yield=ratio_yield, ratio_intensity=ratio_intensity, $
					free=free, default=pyield, /pointer )
		
	return, p
	end
