	pro pixe_initial, peaks, dynamic, e_low, e_high, cal_a, cal_b, $
			n_els, mask, a, na, org, rorg, name, note, do_tail, spectrum, background, $
			x_valid=x_valid, pileup_mode=pileup_mode, major_e=major_e, tweak_par=tweak_par, $
			tweek_el=tweek_el, tweek_lines=tweek_lines, sum_deficit=sum_deficit, use_last=use_last

;	Routine to set inital starting values for peaks.
;	x_valid are the valid channel numbers.

;	Parameters:	0 Noise		2 cal B		4 pileup	5 tail amp		7  backgnd 1	8 Compton tail amp
;				1 Fano		3 cal A					6 tail length	10 backgnd 2	9 Compton tail length

COMPILE_OPT STRICTARR

	if n_elements(peaks) lt 1 then return
	if n_elements(dynamic) lt 1 then dynamic=0
	if n_elements(pileup_mode) lt 1 then pileup_mode=0
	if ptr_valid(peaks) eq 0 then goto, done
	if n_elements(spectrum) lt 1 then goto, done
	if n_elements(background) lt 1 then goto, done
	if n_elements(tweek_el) lt 1 then tweek_el=-1
	if n_elements(tweak_par) lt 1 then tweak_par=0

	use_mu_zero = 0
	mu_zero = 0.0
	if n_elements( (*peaks).mu_zero) gt 1 then use_mu_zero=1
	if n_elements( use_last) eq 0 then use_last=0

	aw = 1.0645			; convert height*FWHM to Gaussian peak area
	small_peak = 0.2	; discard elements less than this count
	too_narrow = 0.35	;

	r_peak = 0.5		; significant peak/backround ratio
	cal_peak = 100.		; significant peak area for energy calibration
	fwhm_peak = 300.	; significant peak area for FWHM fitting
	back_peak = 10.		; increase in significance with background

	comp_amp = 500.		; significant peak area for Compton tail peaks
	comp_len = 3000.	; significant peak area for Compton tail lengths

	tail_amp = 50000.	; significant peak area for tail peaks
	tail_len = 1500000.	; significant peak area for tail lengths
						; these are modified by the EEf factor (more with energy).
	tail_e = 12.		; mimimum energy line to vary tail lengths

	small = 1.0e-10

;---------------------------------------------------------------------------------

	el_cal = 10000.
	eh_cal = 0.
	n_cal = 0
	el_fwhm = 10000.
	eh_fwhm = 0.
	n_fwhm = 0
	el_tail1 = 10000.
	eh_tail1 = 0.
	n_tail1 = 0
	el_tail2 = 10000.
	eh_tail2 = 0.
	n_tail2 = 0
	el_tailc = 10000.
	eh_tailc = 0.
	n_tail1c = 0
	n_tail2c = 0

	n_channels = n_elements(spectrum)
	x_low = clip( (e_low - cal_b)/cal_a, 0,n_channels-1)
	x_high = clip( (e_high - cal_b)/cal_a, 0,n_channels-1)
	e_origin = e_low + 0.3 * (e_high - e_low)
	fwhm_origin = e_low

	x_ok = intarr(n_channels)
	if n_elements(x_valid) ge 1 then begin
		x_ok[ clip(x_valid,0,n_channels-1)] = 1
	endif else begin
		x_ok[x_low:x_high] = 1
	endelse

;---- Names of elements -----------------------------------------------------------------

;		Use negative Z for the added Comptons from continuum sources.

	shells = ['','K','L','M']
	n_els = (*peaks).n_els
	for k=0L,n_els-1 do begin
		if (*peaks).lines[0,k] eq line_index('elastic') then begin
			name[org+k] = 'elastic'
		endif else if (*peaks).lines[0,k] eq line_index('Compton') then begin
			if (*peaks).z[k] lt 0 then begin									;@10-24
				name[org+k] = 'c' + element_name( -(*peaks).z[k])				;@10-24
			endif else begin
				name[org+k] = 'Compton'
			endelse
		endif else begin
			name[org+k] = element_name( (*peaks).z[k]) + ' ' + shells[ (*peaks).shell[k]]
		endelse
	endfor

;---- background scaling parameter ------------------------------------------------------

	a[7] = 1.0
	a[10] = 1.0

;---- Initial sum peak lines -------------------------------------------------------------

	if pileup_mode eq 0 then sum_peaks, a, mask, name, note, do_tail, org, na, peaks, $
										/new, e_high=e_high, sum_deficit=sum_deficit
	n_els = (*peaks).n_els

;---- peak area initial values -----------------------------------------------------------

	for k=0L,n_els-1 do begin
		do_tail[k] = 0
		if tweak_par eq 0 then a[org+k] = 0.0
		nl = (*peaks).n_lines[k]
		if nl gt 0 then begin
			order = reverse( sort( (*peaks).intensity[0:nl-1] ))
			no = n_elements(order)
	;		order = order[0:no-1 < 4]											; 5 most intense
			e = (*peaks).e[order,k]
			rel = (*peaks).intensity[order,k]
			x = (e-cal_b)/cal_a

;			Keep lines to examine for area estimate to main ones, but mindful of situation where major
;			lines have been excluded due to energy range/ cuts.

;			q = where( x_ok[[x]] and (rel gt 0.003))
			q = where( x_ok[[x]] and (rel gt 0.01))								; lines to use for area estimate
			if q[0] eq -1 then begin
				mask[org+k] = 0													; outside fit range
				note[org+k] = 'No valid lines in fit range, masked off in fit'
			endif
			q2 = where( (*peaks).lines[order,k] eq line_index('Compton'))
			if q2[0] ne -1 then do_tail[k]=1									; always do Compton tails
			q2 = where( (*peaks).lines[order,k] eq line_index('Compton2'))
			if q2[0] ne -1 then do_tail[k]=1
		endif else begin
			mask[org+k] = 0
			note[org+k] = 'No valid lines, masked off in fit'
		endelse

;		Initial peak areas:

		force = 0						; force all elements to be included

		if mask[org+k] then begin
			order = order[q]
			no = n_elements(order)
			ap = fltarr(no)
			for i=0L,no-1 do begin
				e = (*peaks).e[order[i],k]
				x = (e-cal_b)/cal_a
				d = sqrt( A[0]*A[0] + A[1]*A[1] * (e-fwhm_origin))				; FWHM channels
				j = fix(d/2.) > 1
				i1 = (x-j) > 0
				if x lt (n_channels-1) then begin
					i2 = ((x+j) > 0 )< (n_channels-1)
					tot = total( spectrum[i1:i2])
					b = total( background[i1:i2])
					s = tot-b
					s = s/float(2*j+1)											; peak height
					ap[i] = s * aw * d / (*peaks).intensity[order[i],k]			; peak area estimate
				
;					Check the significance of the lines for use in fitting
;					various things ...

					if s gt r_peak*background[(i1+i2)/2] then begin
						b = back_peak * sqrt( abs(background[(i1+i2)/2] * aw*d))
						if ap[i] gt cal_peak+b then begin
							n_cal = n_cal+1
							if e lt el_cal then el_cal = e
							if e gt eh_cal then eh_cal = e
						endif
						if ap[i] gt fwhm_peak+2.*b then begin
							n_fwhm = n_fwhm+1
							if e lt el_fwhm then el_fwhm = e
							if e gt eh_fwhm then eh_fwhm = e
						endif
		
						EEf = (e/6.)*(e/6.)
						if ap[i]*EEf gt tail_amp+2.*b then begin
							n_tail1 = n_tail1+1
							do_tail[k] = 1
							note[org+k] = 'Tail amps included in fit'
						endif
						if ap[i]*EEf gt tail_len+2.*b then begin
							n_tail2 = n_tail2+1
							do_tail[k] = 1
							note[org+k] = 'Tail lengths included in fit'
							if e lt el_tail2 then el_tail2 = e
							if e gt eh_tail2 then eh_tail2 = e
						endif
		
						if ((*peaks).lines[0,k] eq line_index('Compton')) or ((*peaks).lines[0,k] eq line_index('Compton2')) then begin
							if ap[i] gt comp_amp+2.*b then begin
								n_tail1c = n_tail1c+1
								do_tail[k] = 1
								note[org+k] = 'Compton tail amps included in fit'
							endif
							if ap[i] gt comp_len+2.*b then begin
								n_tail2c = n_tail2c+1
								do_tail[k] = 1
								note[org+k] = 'Compton tail lengths included in fit'
							endif
						endif
					endif				
				endif
			endfor

;			q = where( ap gt 0.0)
;			if (q[0] eq -1) then begin
;				if (force eq 0) then begin
;					ap = 0.0
;					mask[org+k] = 0
;				endif else begin
;					ap = 3.
;					a[org+k] = ap
;				endelse
;			endif else begin

;				Using "min" can cause problems if the background cuts high and some minor line
;				goes negative here. But "mean()" here causes more problems as lots of minor 
;				lines overlapping with major elements tend to set high initial areas.

;				ap = mean(ap[q] > 0.) > 3.
				q2 = where( ap[q] gt 0.1, nq2)
				if nq2 gt 0 then begin
					ap = min(ap[q[q2]]) > 3.
				endif else ap = min(ap[q]) > 3.
				if tweak_par eq 0 then a[org+k] = ap
;			endelse

;			Discard any too weak peaks,
;			but only if dynamic is NOT on (dynamic=0)

			if max(ap) lt small_peak then begin						; insignificant element
				if (dynamic eq 0) and (use_last eq 0) then begin
					if tweak_par eq 0 then a[org+k] = 0.0
					mask[org+k] = 0
					note[org+k] = 'Too small, masked off in fit'
				endif else begin
					note[org+k] = 'Very small, but keep for Dynamic Analysis or "use_last" loop'
				endelse
			endif
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
;		wide_enough = eh_cal gt 2.*el_cal
		wide_enough = ((eh_cal - el_cal) gt 2.2) or (eh_cal gt 2.*el_cal)
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
;	both tail parameters?

	if mask[5] or mask[6] then begin
		if n_tail1 lt 1 then begin
			mask[5] = 0
			mask[6] = 0
			note[5] = 'No large peaks, Tail variation fixed in fit'
			note[6] = ' '
		endif else begin
			if n_tail2 lt 1 then begin
				mask[6] = 0
				note[6] = 'No very large peaks to adjust Tail lengths'
			endif else if eh_tail2 lt tail_e then begin
				mask[6] = 0
				note[6] = 'Only low energy peaks, fix Tail lengths'
			endif
		endelse
	endif

	if mask[8] or mask[9] then begin
		if n_tail1c lt 1 then begin
			mask[8] = 0
			mask[9] = 0
			note[8] = 'No large peaks, Compton tail variation fixed in fit'
			note[9] = ' '
		endif else begin
			if n_tail2c lt 1 then begin
				mask[8] = 0
				note[9] = 'No very large peaks to adjust Compton tail lengths'
			endif
		endelse
	endif
;	print,'Compton tail masks[8:9]=', mask[8:9]

;---- Initial sum peak areas -------------------------------------------------------------

	if pileup_mode eq 0 then sum_peaks, a, mask, name, note, do_tail, org, na, peaks, $
								sum_deficit=sum_deficit, e_high=e_high

;---- weed out week lines, and those CUT out or out of E range ---------------------------

	n_els = (*peaks).n_els
	n_layers = (*peaks).n_layers
	nk = n_elements((*peaks).intensity[*,0])
	e = (*peaks).e
	intensity = (*peaks).intensity
	array = (*peaks).array
	ratio_intensity = 1.0
	if (*peaks).array then begin
		n_det = n_elements((*peaks).ratio_yield[*,0])
		if tag_present('ratio_intensity', *peaks) eq 0 then begin
			ratio_intensity = replicate( 1.0, n_det, nk, n_els)
		endif else begin
			ratio_intensity =(*peaks).ratio_intensity
		endelse
	endif
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

;			Keep the line intensity threshold conservative in case the initial area estimate was set a bit low.

;			q = where( A[org+i]*intensity[0:n_lines[i]-1,i] lt 0.3 )
			q = where( A[org+i]*intensity[0:n_lines[i]-1,i] lt 0.1 )			; line area/intensity threshold
			if q[0] ne -1 then ok[q,i] = 0
		endif
		q = where( ok[*,i] eq 1)
		nq = n_elements(q)
		if q[0] eq -1 then begin
			n_lines[i] = 0
			mask[org+i] = 0
			note[org+i] = 'No significant lines in fit range'

			if tweek_el eq i then begin
				tweek_el = -1
				warning,'pixe_initial','All lines of "tweek" element masked off.'
			endif
		endif else begin
			n_lines[i] = nq
			lines[0:nq-1,i] = lines[q,i]
			if nq lt nk then lines[nq:nk-1,i] = 0
			e[0:nq-1,i] = e[q,i]
			intensity[0:nq-1,i] = intensity[q,i]
			if array then ratio_intensity[*,0:nq-1,i] = ratio_intensity[*,q,i]
			
			if use_mu_zero then mu_zero[0:nq-1,i] = mu_zero[q,i]

			if tweek_el eq i then begin
				nt = n_elements(tweek_lines)
				if nk gt nt then tweek_lines = [tweek_lines,replicate(-1,nk-nt)]
				tweek_lines[0:nq-1] = tweek_lines[q]
				if nq lt nk then tweek_lines[nq:nk-1] = -1
			endif
		endelse
	endfor

;	N.B. This has to match the form in 'calc_da_merge_scatter' ('correct_da_loop'), 'correct_lines',
;	'make_peaks', 'read_yield', 'select_element_lines', 'sum_peaks', 'append_escapes', 'pixe_initial'

	xrays = make_peaks( n_lines=n_lines, lines=lines, intensity=intensity, $
				e_lines=e, mu_zero=mu_zero, ratio_intensity=ratio_intensity, $
				default=peaks )	

	ptr_free, peaks
	peaks = ptr_new( xrays, /no_copy)

done:
	return
	end
