	pro pixe_setup, e_low, e_high, n_channels, cal_a, cal_b, w0, w1, $
		el_fix, fix_fano, fix_cal, fix_width, fix_tail, no_tail, fix_gain, n_els, $
		mask, a, na, org, rorg, name, note, do_tail, detector, $
		pileup_mode=pileup_mode, tweek_el=tweek_el, tweek_lines=tweek_lines, $
		old_a=old_a, tweak_par=tweak_par

COMPILE_OPT STRICTARR
	common c_fit_memory_1, fit_memory_on
	if n_elements(fit_memory_on) lt 1 then fit_memory_on=0		; enable fit parameter memory

	if n_elements(pileup_mode) lt 1 then pileup_mode=0
	if n_elements(Camp) lt 1 then Camp = 0.5
	if n_elements(Clen) lt 1 then Clen = 1.0
	if n_elements(tweek_el) lt 1 then tweek_el=-1
	if n_elements(tweak_par) lt 1 then tweak_par=0

;	Note: The legal values of ORG and RORG are just (10,0) or (20,10).
;	This is to be able to read PFR files of mixed versions together.

;	Parameters:	0 Noise		2 cal B		4 pileup	5 tail amp		7 backgnd 1		8 Compton tail amp
;				1 Fano		3 cal A					6 tail length	10 backgnd 2	9 Compton tail length
;
;	(ORG - RORG) should always to 10
;
;	ORG = 20						; start index for element intensity parameters
;	RORG = 10						; start index for line intensity tweaks

	ORG = 21						; start index for element intensity parameters	;@9-19
	RORG = 11						; start index for line intensity tweaks
	
	if tweak_par and n_elements(na) gt 0 then return

	na = org + n_els
	mask = intarr(na)
	name = strarr(na)
	note = strarr(na)
	a = fltarr(na)
	if n_elements(old_a) ne rorg then old_a=fltarr(rorg)
	do_tail = intarr(n_els)

	mask[org:*] = 1-el_fix

	e_origin = e_low + 0.3 * (e_high - e_low)
	fwhm_origin = e_low

;----   Initial line intensity tweeks --------------------------------

	mask[rorg:org-1] = 0
	a[rorg:org-1] = 1.0
	if tweek_el ge 0 then begin
;		mask[org+tweek_el] = 0
		q = where( tweek_lines ge 0)
		if q[0] ne -1 then mask[rorg+tweek_lines[q]] = 1
	endif

;----	Initial cal, peak width --------------------------------------

	mask[3] = 1
	mask[2] = 1
;	if fit_memory_on then begin
;		a[3] = old_a[3]
;		a[2] = old_a[2]
;	;	if (fix_cal eq 0) or (abs(a[3]-1.0) lt 1.0e-6) or (abs(a[3]) lt 1.0e-6) then begin
;
;	;	if (fix_cal eq 1) or (abs(a[3]-1.0) lt 1.0e-6) or (abs(a[3]) lt 1.0e-6) or  $
;	;				abs(a[3] - 1./cal_a) gt 0.01*a[3] then begin
;			a[3] = 1./cal_a
;			a[2] = a[3]*(e_origin - cal_b)
;	;	endif
;	endif else begin
		a[3] = 1./cal_a
		a[2] = a[3]*(e_origin - cal_b)
;	endelse

	name[3] = 'Cal Gain'
	name[2] = 'Cal Offset'
	note[3] = 'Free'
	note[2] = 'Free'

	if fix_gain then begin
		mask[3] = 0
		note[3] = 'Fixed energy gain'
	endif
	if fix_cal then begin
		mask[2] = 0
		mask[3] = 0
		note[2] = 'Fixed energy calibration'
		note[3] = 'Fixed energy calibration'
	endif

	name[7] = 'Background 1 scaling'
	name[10] = 'Background 2 scaling'					;@9-19

	mask[0] = 1
	mask[1] = 1
;	if fit_memory_on then begin
;		a[0] = old_a[0]
;		a[1] = old_a[1]
;		if (fix_width eq 0) or (a[0] lt 1.0e-6) then begin
;			a[0] = sqrt( w0 + w1*fwhm_origin ) * a[3]
;		endif
;	;	if (fix_fano eq 0) or (a[1] lt 1.0e-6) then begin
;			a[1] = sqrt( w1) * a[3]
;	;	endif
;	endif else begin
		a[0] = sqrt( w0 + w1*fwhm_origin ) * a[3]
		a[1] = sqrt( w1) * a[3]
;	endelse

	name[0] = 'Width Noise'
	name[1] = 'Width Fano factor'
	note[0] = 'Free'
	note[1] = 'Free'

	if fix_fano then begin
		mask[1] = 0
		note[1] = 'Fano factor fixed to theory'
	endif

	if fix_width then begin
		mask[0] = 0
		mask[1] = 0
		note[0] = 'Peak width variation fixed'
		note[1] = 'Peak width variation fixed'
	endif

;----	Tailing ------------------------------------------------------

	name[5] = 'Tail amplitude'
	name[6] = 'Tail lengths'
	if no_tail then begin
		mask[5] = 0
		mask[6] = 0
		note[5] = 'No tails'
		note[6] = 'No tails'
	endif else begin
		ta = tail_amplitude( detector, 6.4)
		tl = tail_length( detector, 6.4)
		if (ta lt 1.0e-10) or (tl lt 1.0e-10) then begin
			mask[5] = 0
			mask[6] = 0
			note[5] = 'No tail parameters for this detector'
			note[6] = 'No tail parameters for this detector'
		endif else begin
			a[5] = tail_alpha_to_a( detector, 6.4, ta)
;			a[5] = tail_alpha_to_a( detector, 6.4, ta/3.)
			a[6] = tail_gamma_to_a( detector, 6.4, tl)
			mask[5] = 1
			mask[6] = 1
			note[5] = 'Free'
			note[6] = 'Free'
			if fix_tail then begin
				mask[5] = 0
				mask[6] = 0
				note[5] = 'Tails fixed'
				note[6] = 'Tails fixed'
			endif
		endelse
	endelse

;----	Compton Tailing scaling ------------------------------------------------

	name[8] = 'Compton tail amplitude'
	name[9] = 'Compton tail lengths'
	if no_tail then begin
		mask[8] = 0
		mask[9] = 0
		note[8] = 'No Compton tails'
		note[9] = 'No Compton tails'
	endif else begin
		rho_zero = 0.2
		eta_zero = 0.1
		a[8] = sqrt(1. - rho_zero)
		a[9] = sqrt(1. - eta_zero)
		mask[8] = 1
		mask[9] = 1
		note[8] = 'Free'
		note[9] = 'Free'
		if fix_tail then begin
			mask[8] = 0
			mask[9] = 0
			note[8] = 'Compton Tails fixed'
			note[9] = 'Compton Tails fixed'
		endif
	endelse

;----	Pileup scaling ------------------------------------------------------

	if pileup_mode eq 1 then begin
		name[4] = 'sum'
		mask[4] = 1
		a[4] = 1.0
	endif

	return
	end
