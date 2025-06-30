pro file_pcm_preview, file, preview=preview

; Provide preview of PCM file details for 'file_requester' from 'file'.
; Return preview data in 'preview' struct.

COMPILE_OPT STRICTARR
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
		warning,'file_pcm_preview',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
define_devices

	preview = 0L
	if n_elements(file) lt 1 then return
	
	on_ioerror, bad
	close, 2
	openr, 2, file, /xdr

	on_ioerror, bad

	path = extract_path(file)
	s1 = ''
	readu,2, s1
	version = 0
	gamma = 0
	detector_file = ''
	if inumeric(s1) then begin
		version = fix(s1)
		if version le -2 then begin
			readu,2, gamma
		endif
		if version lt 0 then begin
			readu,2, s1
		endif else version = 0
	endif
	filter_file = s1

	valid = [0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12]
	q = where( version eq valid)
	if q[0] eq -1 then begin
		warning,'load_pcm_parameters',['Error in PCM file.','Bad version number.']
		return
	endif

	readu,2, s1
	detector_file = path + strip_path(s1)
	
	readu,2, s1
	cuts_file = s1

	readu,2, s1
	yield_file = s1
	
	n_el = 0L
	readu,2, n_el
	if n_el gt 0 then begin
		zs = replicate( {element, Z:0, Shell:0}, n_el)
		readu,2, zs		
	endif

	n_off = 0L
	readu,2, n_off
	if n_off gt 0 then begin
		zs_off = replicate( {element, Z:0, Shell:0}, n_off)
		readu,2, zs_off
	endif

	if version eq 0 then begin
		free = [0,0,0]
		readu,2, free
		free = [free, 0]
	endif else begin
		free = [0,0,0,0]				; cal, fwhm, tail, no-tail	flags
		readu,2, free
	endelse
	tail_mode = (free[2] eq 0)
	if free[3] eq 1 then begin
		tail_mode = 3
		free[2] = 0
	endif
	
	width_mode = (free[1] eq 0)

	width = 170.
	if (free[1] eq 0) and (version le -8) then begin
		readu,2, width
	endif

	free_fano = 0
	if (version le -9) then begin
		readu,2, free_fano
	endif

	cal_mode = (free[0] eq 0)

	free_gain = 1
	if (version le -10) then begin
		readu,2, free_gain
	endif

	back = 0
	boost = 0
	pileup_mode = 0
	sum_deficit = 0.1
	background2 = 0
	back2_split_energy = 10.0
	if version lt 0 then begin
		readu,2, back
		readu,2, boost
	endif else begin
		readu,2, back
		if back gt 0 then begin
			back = 0
			boost = 1
		endif
	endelse
	if version le -12 then begin
		readu,2, background2, back2_split_energy
	endif
	if version le -3 then begin
		readu,2, pileup_mode
	endif
	if version le -9 then begin
		readu,2, sum_deficit
	endif

	el = 0.0
	eh = 0.0
	readu,2, el, eh

	tweek =  {el:-1, lines: replicate(-1,20), a:replicate(1.0,10)}
	if version le -4 then readu,2, tweek

	c = 1.0003
	seb = 0
	passes = 8
	if version le -5 then begin
		readu,2, c, seb, passes

		if back ge 1 then begin
			s1 = ''
			readu,2, s1
		endif
	endif
	curve = c
	trim_seb = seb

	compton_shift = -0.006
	compton_spread = 1.0
	if version le -6 then begin
		readu,2, compton_shift, compton_spread
	endif

	compton_tail_amp = 1.0
	compton_tail_len = 1.0
	if version le -7 then begin
		readu,2, compton_tail_amp, compton_tail_len
	endif

	mpda_mode = 0
	correct_file = ''
	if version le -11 then begin
		readu,2, mpda_mode
		if mpda_mode ge 1 then begin
			readu,2, correct_file
		endif
	endif

	close, 2

;-----------------------------------------------------------------------------------------
; Details string array:

	list = ['File: ' + strip_path(file), '']
	list = [list, 'Detector: ' + detector_file]
	list = [list, 'Filter: ' + filter_file]
	if cuts_file ne '' then list = [list, 'Cuts: ' + cuts_file]
	list = [list, 'Yields: ' + yield_file, '']

	if n_el ge 1 then begin
		line = 'Fit: '
		q = sort( zs.Z)
		for i=0,n_el-1 do begin
			shell = ''
			if zs[q[i]].shell eq 2 then shell = 'L'
			if zs[q[i]].shell eq 3 then shell = 'M'
			line = line + element_name(zs[q[i]].Z) + shell + ','
		endfor
		list = [list, line]
	endif else begin
		list = [list, 'Fit: none selected']
	endelse

	if n_off ge 1 then begin
		line = 'MDL: '
		q = sort( zs_off.Z)
		for i=0,n_off-1 do begin
			shell = ''
			if zs_off[q[i]].shell eq 2 then shell = 'L'
			if zs_off[q[i]].shell eq 3 then shell = 'M'
			line = line + element_name(zs_off[q[i]].Z) + shell + ','
		endfor
		list = [list, line]
	endif else begin
		list = [list, 'MDL: none']
	endelse

	list = [list, '', 'Energy range: ' + str_tidy(el) + ' - ' + str_tidy(eh), '']

	list = [list, 'Free Cal: ' + str_tidy(free[0]) + ', Free gain: ' + str_tidy(free_gain)]
	list = [list, 'Free Width: ' + str_tidy(free[1]) + ', width = '+ str_tidy(width) + ', Free Fano: ' + str_tidy(free_fano)]
	list = [list, 'Free Tail: ' + str_tidy(free[2]), '']
	
	list = [list, 'Back: ' + str_tidy(back) + ' Boost: ' + str_tidy(boost)]
	list = [list, 'Back2: ' + str_tidy(background2) + (background2 ? ', Split energy: ' + str_tidy(back2_split_energy) : '')]

	list = [list, '', 'Pileup mode: ' + str_tidy(pileup_mode) + ', Sum deficit: ' + str_tidy(sum_deficit), '']

	list = [list, 'SNIP passes: ' + str_tidy(passes) + ', curvature: ' + str_tidy(curve)]
	list = [list, '', 'Compton Tail Amp: ' + str_tidy(compton_tail_amp) + ', Length: ' + str_tidy(compton_tail_len)]
	list = [list, 'Compton Shift: ' + str_tidy(compton_shift) + ', Spread: ' + str_tidy(compton_spread)]
	
;------------------------------------------------------------

	preview = {details:list}
	return

bad:
	return
end