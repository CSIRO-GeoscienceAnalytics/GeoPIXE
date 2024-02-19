pro build_mask_phases, mask, org, mask_phase, n_phases, mask_legend, mask_title, mask_kill, $
						gamma=gamma

;	Construct phases of the fit using staged mask settings.

	if n_elements(gamma) lt 1 then gamma=0
	if gamma then begin
		tail = 'step'
		tail_amp = 'step amplitude'
		stail_length = 'step E term'
	endif else begin
		tail = 'tail'
		tail_amp = 'tail amplitude'
		stail_length = 'tail lengths'
	endelse

	na = n_elements(mask)
	mask_phase = intarr(na,5)
	mask_legend = strarr(5)
	mask_title = ' '
	for j=0L,4 do begin
		mask_phase[*,j] = mask
	endfor

	if mask[5] then begin				; tail/step amplitude is free
		n_phases = 4
		mask_phase[0,0] = 0
		mask_phase[1,0] = 0
		mask_phase[2,0] = 0				; phase 1
		mask_phase[3,0] = 0
		mask_phase[5,0] = 0
		mask_phase[6,0] = 0
		mask_phase[8,0] = 0
		mask_phase[9,0] = 0
		mask_legend[0] = 'Vary linear peak area parameters only'

		mask_phase[5,1] = 0				; phase 2
		mask_phase[6,1] = 0
		mask_phase[8,1] = 0
		mask_phase[9,1] = 0
		mask_legend[1] = 'Vary peak areas, energy calibration and peak-width parameters'

		mask_phase[0,2] = 0				; phase 3
		mask_phase[1,2] = 0
		mask_phase[6,2] = 0
		mask_phase[9,2] = 0
		mask_legend[2] = 'Vary peak areas, energy calibration and '+tail_amp+', but not peak widths and '+stail_length

		mask_phase[6,3] = 0				; phase 4
;		mask_phase[9,3] = 0
		mask_legend[3] = 'Vary peak areas, energy calibration, '+tail_amp+' and peak widths, but not '+stail_length

		if mask[6] then begin			; phase 5
			n_phases = 5
			mask_legend[4] = 'Vary '+stail_length+' now too'
			mask_title = 'Vary '+tail_amp+' and '+stail_length+' in a 5 phase fit'
		endif else begin
			mask_title = 'Vary '+tail_amp+' in a 4 phase fit'
		endelse
	endif else begin
		n_phases = 3
		mask_phase[2,0] = 0				; phase 1
		mask_phase[3,0] = 0
		mask_phase[0,0] = 0
		mask_phase[1,0] = 0
		mask_phase[6,0] = 0
		mask_phase[9,0] = 0
		mask_legend[0] = 'Vary linear peak area parameters only'

		mask_phase[0,1] = 0				; phase 2
		mask_phase[1,1] = 0
		mask_phase[6,1] = 0
		mask_phase[9,1] = 0
		mask_legend[1] = 'Vary peak areas and energy calibration parameters'

		mask_phase[6,2] = 0				; phase 3
;		mask_phase[9,2] = 0
		mask_legend[2] = 'Vary peak areas, energy calibration and peak widths parameters'

		if mask[6] then begin			; phase 5
			n_phases = 4
			mask_legend[3] = 'Vary '+stail_length+', but not '+tail_amp
			mask_title = 'Vary '+stail_length+' in a non-standard 4 phase fit'
		endif else begin
			mask_title = 'Perform a standard 3 phase fit with no '+tail+' variation'
		endelse
	endelse

;----  Kill certain parameters if fit becomes unstable ----------------------------------

	mask_kill = replicate(1,na)

	mask_kill[1] = 0								; tail Fano term in FWHM
	mask_kill[3] = 0								; tail energy calibration gain
	mask_kill[5] = 0								; tail amplitudes
	mask_kill[6] = 0								; tail lengths
	mask_kill[8] = 0								; Compton tail amplitudes
	mask_kill[9] = 0								; Compton tail lengths

	return
	end
