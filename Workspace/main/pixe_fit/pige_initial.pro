	pro pige_initial, peaks, dynamic, e_low, e_high, cal_a, cal_b, $
			n_els, mask, a, na, org, rorg, name, note, do_step, spectrum, $
			x_valid=x_valid, major_e=major_e

;	Routine to set inital starting values for peaks.

COMPILE_OPT STRICTARR

	if n_elements(peaks) lt 1 then return
	if n_elements(dynamic) lt 1 then dynamic=0
	if ptr_valid(peaks) eq 0 then goto, done
	if n_elements(spectrum) lt 1 then goto, done
	use_mu_zero = 0
	mu_zero = 0.0
	if n_elements( (*peaks).mu_zero) gt 1 then use_mu_zero=1

	aw = 1.0645			; convert height*FWHM to Gaussian peak area
	small_peak = 0.2	; discard elements less than this count
	too_narrow = 0.5	;

	r_peak = 0.5		; significant peak/backround ratio
	cal_peak = 100.		; significant peak area for energy calibration
	fwhm_peak = 300.	; significant peak area for FWHM fitting
	back_peak = 10.		; increase in significance with background

	step_amp = 1000.	; significant peak area for step amplitude
	step_eterm = 10000.	; significant peak area for step amplitude energy term
	step_e = 1500.		; mimimum energy spread to vary step energy term

	small = 1.0e-10

;---------------------------------------------------------------------------------

	el_cal = 10000.
	eh_cal = 0.
	n_cal = 0
	el_fwhm = 10000.
	eh_fwhm = 0.
	n_fwhm = 0
	el_step1 = 10000.
	eh_step1 = 0.
	n_step1 = 0
	el_step2 = 10000.
	eh_step2 = 0.
	n_step2 = 0

	x_low = (e_low - cal_b)/cal_a
	x_high = (e_high - cal_b)/cal_a
	e_origin = e_low + 0.3 * (e_high - e_low)
	fwhm_origin = e_low
	n_channels = n_elements(spectrum)

	x_ok = intarr(n_channels)
	if n_elements(x_valid) ge 1 then begin
		x_ok[ clip(x_valid,0,n_channels-1)] = 1
	endif else begin
		x_ok[x_low:x_high] = 1
	endelse

;---- background scaling parameter ----------------------------------------------------

;	a[7] = 1.0

;---- Initial sum peak lines -------------------------------------------------------------

;	sum_peaks, a, mask, name, note, do_step, org, na, peaks, /new

;---- peak area initial values -----------------------------------------------------------

	for k=0L,n_els-1 do begin
		do_step[k] = 0
		a[org+k] = 0.0
		if name[org+k] ne 'sum' then begin
			name[org+k] = element_name( (*peaks).z[k])
			name[org+n_els+k] = 'Back '+element_name( (*peaks).z[k])
		endif
		nl = (*peaks).n_lines[k]
		if nl gt 0 then begin
			order = reverse( sort( (*peaks).intensity[0:nl-1] ))
			no = n_elements(order)
	;		order = order[0:no-1 < 4]										; 5 most intense
			e = (*peaks).e[order,k]
			rel = (*peaks).intensity[order,k]
			x = (e-cal_b)/cal_a
			q = where( (x ge x_low) and (x le x_high) and (rel gt 0.01))
			if q[0] eq -1 then begin
				mask[org+k] = 0												; outside fit range
				mask[org+n_els+k] = 0
			endif
		endif else begin
			mask[org+k] = 0
			mask[org+n_els+k] = 0
		endelse

;		Initial peak areas:

		if mask[org+k] then begin
			order = order[q]
			no = n_elements(order)
			ap = fltarr(no)
			for i=0L,no-1 do begin
				e = (*peaks).e[order[i],k]
				x = round((e-cal_b)/cal_a)
				d = sqrt( A[0]*A[0] + A[1]*A[1] * (e-fwhm_origin))			; FWHM channels
				j = round(d/2.) > 1
				i1 = (x-j) > 0
				i2 = (x+j) < (n_channels-1)
				i3 = (x+5*j) < (n_channels-1)
				i4 = (x+8*j) < (n_channels-1)
				tot = total( spectrum[i1:i2])
				back = total( spectrum[i3:i4])
				s = tot-back
				back = back/float(i4-i3+1)									; back height
				s = s/float(i2-i1+1)										; peak height
				ap[i] = s * aw * d / (*peaks).intensity[order[i],k]			; peak area estimate
			endfor

			q = where( ap gt 0.0)
			if q[0] eq -1 then begin
				ap = 1.
				a[org+k] = 1.
				a[org+n_els+k] = back
;				mask[org+k] = 0
;				mask[org+n_els+k] = 0
			endif else begin
				ap = min(ap[q])
				a[org+k] = ap
				a[org+n_els+k] = back
			endelse

;			Check the significance of the lines for use in fitting
;			various things ...

			if s gt r_peak*back then begin
				b = back_peak * sqrt( abs(back * aw*d))
				if ap gt cal_peak+b then begin
					n_cal = n_cal+1
					if e lt el_cal then el_cal = e
					if e gt eh_cal then eh_cal = e
				endif
				if ap gt fwhm_peak+2.*b then begin
					n_fwhm = n_fwhm+1
					if e lt el_fwhm then el_fwhm = e
					if e gt eh_fwhm then eh_fwhm = e
				endif

				if ap gt step_amp+2.*b then begin
					n_step1 = n_step1+1
					do_step[k] = 1
					note[org+k] = 'Steps included in fit'
				endif
				if ap gt step_eterm+2.*b then begin
					n_step2 = n_step2+1
					do_step[k] = 1
					note[org+k] = 'Steps included in fit'
					if e lt el_step2 then el_step2 = e
					if e gt eh_step2 then eh_step2 = e
				endif
			endif

;			Discard any too weak peaks,
;			but only if dynamic is NOT on (dynamic=0)

;			if ap lt small_peak then begin						; insignificant element
;				if dynamic eq 0 then begin
;					a[org+k] = 0.0
;					mask[org+k] = 0
;					note[org+k] = 'Too small, masked off in fit'
;				endif else begin
;					note[org+k] = 'Very small, but keep for Dynamic Analysis'
;				endelse
;			endif
		endif
	endfor

;---- check masks to vary other parameters, based on significant peaks ----------------

;	Is spectrum too narrow for these parameters?

	if (e_high-e_low)/e_high lt too_narrow then begin
		mask[3] = 0
		mask[1] = 0
		note[3] = 'Narrow spectrum, Gain fixed in fit'
		note[1] = 'Narrow spectrum, Fano term fixed to theory'
	endif

;	Does spectrum have enough large well-spaced peaks to determine
;	both energy calibration parameters?

	if mask[2] or mask[3] then begin
		wide_enough = eh_cal gt 2.*el_cal
		if n_cal lt 1 then begin
			mask[2] = 0
			mask[3] = 0
			note[2] = 'Peaks too small, calibration fixed in fit'
			note[3] = ' '
		endif else if (n_cal eq 1) or (wide_enough eq 0) then begin
			mask[3] = 0
			note[3] = 'Too few peaks, Gain fixed in fit'
		endif
	endif

;	Does spectrum have enough large well-spaced peaks to determine
;	both FWHM parameters?

	if mask[1] or mask[0] then begin
		wide_enough = eh_fwhm gt 2.*el_fwhm
		if n_fwhm lt 1 then begin
			mask[0] = 0
			mask[1] = 0
			note[0] = 'Peaks too small, FWHM variation fixed in fit'
			note[1] = ' '
		endif else if (n_fwhm eq 1) or (wide_enough eq 0) then begin
			mask[1] = 0
			note[1] = 'Too few peaks, Fano term fixed to theory'
		endif
	endif

;	Does spectrum have enough large well-spaced peaks to determine
;	both step parameters?

	if mask[5] or mask[6] then begin
		if n_step1 lt 1 then begin
			mask[5] = 0
			mask[6] = 0
			note[5] = 'No large peaks, step variation fixed in fit'
			note[6] = ' '
		endif else begin
			if n_step2 lt 1 then begin
				mask[6] = 0
				note[6] = 'No very large peaks to adjust step lengths'
			endif else if (eh_step2-el_step2) lt step_e then begin
				mask[6] = 0
				note[6] = 'Only limited energy peaks, fix step energy term'
			endif
		endelse
	endif

;---- Initial sum peak areas -------------------------------------------------------------

;	sum_peaks, a, mask, name, note, do_step, org, na, peaks

;---- weed out week line, and those CUT out or out of E range ---------

	n_els = (*peaks).n_els
	n_layers = (*peaks).n_layers
	e = (*peaks).e
	intensity = (*peaks).intensity
	if use_mu_zero then mu_zero = (*peaks).mu_zero
	lines = (*peaks).lines
	n_lines = (*peaks).n_lines
	nk = n_elements( e[*,0])

	major_e = reform(e[0,*])		; keep original major line energies (prior to cut)

	ok = intarr(nk,n_els)
	for i=0L,n_els-1 do begin
		if n_lines[i] gt 0 then begin
			ok[0:n_lines[i]-1,i] = 1
		endif
	endfor
	x = clip((e-cal_b)/cal_a, 0,n_channels-1)
	q = where( x_ok[x] eq 0)
	if q[0] ne -1 then ok[q] = 0

	for i=0L,n_els-1 do begin
		if n_lines[i] gt 0 then begin
			q = where( (e[0:n_lines[i]-1,i] lt e_low) or (e[0:n_lines[i]-1,i] gt e_high))
			if q[0] ne -1 then ok[q,i] = 0

			q = where( A[org+i]*intensity[0:n_lines[i]-1,i] lt 0.3 )
			if q[0] ne -1 then ok[q,i] = 0
		endif
		q = where( ok[*,i] eq 1)
		nq = n_elements(q)
		if q[0] eq -1 then begin
			n_lines[i] = 0
			mask[org+i] = 0
			mask[org+n_els+i] = 0
			note[org+i] = 'No significant lines in fit range'
		endif else begin
			n_lines[i] = nq
			lines[0:nq-1,i] = lines[q,i]
			if nq lt nk then lines[nq:nk-1,i] = 0
			e[0:nq-1,i] = e[q,i]
			intensity[0:nq-1,i] = intensity[q,i]
			if use_mu_zero then mu_zero[0:nq-1,i] = mu_zero[q,i]
		endelse
	endfor

;	grays = {n_els:n_els, z:(*peaks).z, shell:(*peaks).shell, n_lines:n_lines, lines:lines, $
;			e:e, intensity:intensity, n_layers:n_layers, yield:(*peaks).yield, $
;			layers:(*peaks).layers, unknown:(*peaks).unknown, $
;			e_beam:(*peaks).e_beam, theta:(*peaks).theta, phi:(*peaks).phi, $
;			alpha:(*peaks).alpha, beta:(*peaks).beta, $
;			title:(*peaks).title, file:(*peaks).file, free:(*peaks).free, $
;			formula:(*peaks).formula, weight:(*peaks).weight, thick:(*peaks).thick, $
;			microns:(*peaks).microns, density:(*peaks).density, $
;			z1:(*peaks).z1, a1:(*peaks).a1, state:(*peaks).state, $
;			emin:(*peaks).emin, emax:(*peaks).emax, mu_zero:mu_zero, $
;			ratio_yield:(*peaks).ratio_yield, array:(*peaks).array, detector_file:(*peaks).detector_file }

	grays = make_peaks( n_lines=n_lines, lines=lines, intensity=intensity, $
				e_lines=e, mu_zero=mu_zero, ratio_intensity=ratio_intensity, $
				default=peaks )	

	ptr_free, peaks
	peaks = ptr_new( grays, /no_copy)

done:
	return
	end
