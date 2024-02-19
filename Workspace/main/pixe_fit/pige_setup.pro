	pro pige_setup, e_low, e_high, n_channels, cal_a, cal_b, w0, w1, $
		el_fix, fix_fano, fix_cal, fix_width, fix_step, no_step, n_els, $
		mask, a, na, org, rorg, name, note, do_step, detector

COMPILE_OPT STRICTARR

	org = 20
	rorg = 10
	na = org + 2*n_els
	mask = intarr(na)
	name = strarr(na)
	note = strarr(na)
	a = fltarr(na)
	do_step = intarr(n_els)

	mask[org:org+n_els-1] = 1-el_fix
	mask[org+n_els:*] = 1-el_fix

	e_origin = e_low + 0.3 * (e_high - e_low)
	fwhm_origin = e_low

;----	Initial cal, peak width --------------------------------------

	mask[3] = 1
	mask[2] = 1
	a[3] = 1./cal_a
	a[2] = a[3]*(e_origin - cal_b)
	name[3] = 'Cal Gain'
	name[2] = 'Cal Offset'
	note[3] = 'Free'
	note[2] = 'Free'

;	name[7] = 'Background scaling'

	mask[0] = 1
	mask[1] = 1
	a[1] = sqrt( w1) * a[3]
	a[0] = sqrt( w0 + w1*fwhm_origin ) * a[3]
	name[0] = 'Width Noise'
	name[1] = 'Width Fano factor'
	note[0] = "Free'
	note[1] = 'Free'

	if fix_cal then begin
		mask[2] = 0
		mask[3] = 0
		note[2] = 'Fixed energy calibration'
		note[3] = ' '
	endif

	if fix_fano then begin
		mask[1] = 0
		note[1] = 'Fano factor fixed to theory'
	endif

	if fix_width then begin
		mask[0] = 0
		mask[1] = 0
		note[0] = 'Peak width variation fixed'
		note[1] = ' '
	endif

;----	Steps ------------------------------------------------------

	name[5] = 'Step amplitude'
	name[6] = 'Step energy term'
	if no_step then begin
		mask[5] = 0
		mask[6] = 0
		note[5] = 'No steps'
		note[6] = ' '
	endif else begin
		a[5] = 0.002
		a[6] = 0.00005
		mask[5] = 1
		mask[6] = 1
		note[5] = 'Free'
		note[6] = 'Free'
		if fix_step then begin
			mask[5] = 0
			mask[6] = 0
			note[5] = 'Steps fixed'
			note[6] = ' '
		endif
	endelse

	return
	end
