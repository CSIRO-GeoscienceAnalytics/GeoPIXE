function pixe_fit, p, pspec, pback, ppileup=ppileup, $
		fix_cal=fix_cal, fix_fano=fix_fano, fix_width=fix_width, fix_gain=fix_gain, $
		fix_tail=fix_tail, no_tail=no_tail, dynamic=dynamic, back_mode=back_mode, $
		pileup_mode=pileup_mode, sum_deficit=sum_deficit, gamma=gamma, use_m=use_m, $
		do_split=do_split, emid=emid, $
		
		progress=do_progressi, initial=initiali, refit=refit, use_last=use_last,$
		message=message, silent=silent, pcm=pcm, old=old, last_a=old_a, tweak_par=tweak_par, $
		mp_loop=mp_loop0,  correct=correct, pressure=pressure, temp=temp, $
		
		show_df=show_df, scale_df=scale_df, tweek_el=tweek_el, tweek_lines=tweek_lines, $
		results=results, da_pars=da_pars, cancel=cancel, python=python, error=error

;	Perform least-squares fit to a spectrum using an adaptation of the algorithms of Bevington (69)
;	with the modified weights of Awaya (78) and couched in a vector/matrix arithmetic form, initially
;	to permit using array processor hardware and later the vector language features of IDL. If called
;	with /initial, it will simply estimate all initial parameters and determine the background, which
;	is used to plot an initial estimate of the spectrum background and fit.
;
;	Returns a pointer to the fitted spectrum struct, or a null pointer on error. 
;	Also returns these keyword values:
;	'da_pars'		returns a pointer to the DA matrix parameters for a call to 'calc_da_matrix'.
;	'results'		returns the results struct.
;
;	In /gamma mode, all "tail" arguments refer to peak "step" instead.
;
; Inputs:
;	'p'				is a pointer to the pars struct in fit_setup.
;	'pspec'			pointer to the target spectrum struct (as defined in 'define.pro')
;		data		pointer to spectrum array
;		size		number of channels
;		cal.poly	calibration polynomial
;	'pback'			pointer to the background spectrum struct (PIXE only)
;	'ppileup'		pointer to the pileup spectrum struct (pileup_mode=1 only)
;
;	'fix_fano'		fixes Fano term in peak widths (default 1)
;	'fix_width'		fix all width parameters (default 0)
;	'fix_cal'		fix both energy calibration parameters (default 0)
;	'fix_gain'		fix gain energy calibration parameter (default 0)
;	'fix_tail'		fix both tail parameters (default 0), (step in /gamma)
;	'no_tail'		zero all tails, and don't vary parameters (default 0), (ignore in /gamma)
;	'dynamic'		Dynamic Analysis mode (2 default)
;	'back_mode'		background mode (0=SNIP, 1,2...=background plugins, e.g. SNIP & Bayer)
;
;	'/initial'		indicates to only construct initial values, don't fit.
;	'/tweak_par'	indicates a small change in one parameter to display function.
;	
;	mp_loop=n		indicates to iterate (n times) on composition using phase information struct 'correct'
;	'correct'		pointer to composition matrix struct (used for multiphase loop)
;	
;	pressure		indicates fitting a spectrum with ambient conditions specified, with pressure (mbar) and
;	temp			temperature (C). Pass these on to 'transmit' in filters and 'det-eff' (in 'array_yield').
;
;	/gamma			for gamma-ray PIGE fitting
;	pileup_mode = 0	normal pileup element calculated from spectrum
;				1	use "Image Pileup" spectrum supplied in 'ppileup'
;	sum_deficit		% deficit in sum peak amplitudes due to finite time resolution
;	pcm				PCM file-name
;	
;	use_m = 0		shell=3" means to fit BOTH K and L lines
;			1		"shell=4,5" means fit BOTH K+L or L+M
;
;	dynamic = 0		no dynamic analysis
;			  1		background subtraction mode (Fortran, spectra only); not implemented in IDL yet.
;			  2		linear L.S. for all and background (imaging)
;
;	'show_df'		return DF[n] (analytical and finite difference) instead of fit for debugging.
;
;	tweek_el=n		Select number of element to tweek line intensities for (n=-1 means OFF)
;	tweek_lines=tweek_lines	Array over lines; each element selects tweek parameter for a line (-1 means OFF).
;
;	'refit'	= 1		redo fit, using old results pointed to by 'old'
;	'old'			old results, used with /refit.
;	last_a			'A' parameter array from previous fit
;	/python			Just assemble all lines parameters, etc and but instead of fitting export all lines data, 
;					spectrum, background, pileup, etc. to files for a Python program.
;
;	The 'p' pointer contains POINTERS to:
;
;	'peaks'			a struct of X-ray lines and yields (cut down list)
;	'detector'		struct of the selected detector parameters
;	'filter'		struct of filter(s) details.
;	'cuts'			struct/array of cuts
;	'yields'		a struct of X-ray lines and yields (full list)
;
;	And the user selected parameters:
;
;	'e_low'			low energy of fit range
;	'e_high'		high energy
;	'passes'		number of passes through SNIP, etc. (default = 8)
;	'curve'			low-E background curvature (default = 1.0003)
;
;	'peaks'			the 'peaks' struct contains ...
;		n_els		number of elements (total)
;		z			Z vector
;		n_lines		number of X-ray lines for each element
;		lines		line IDs for each element (0 = major)
;		e			energy (keV) for each line
;		shell		shell number (1=K, 2=L, 3=M, 0=sum)
;		intensity	relative intensities (major = 1.0)
;		yield		PIXE/SXRF yield for each element (and layer?)
;		free		flags it as a free variable
;		formula		layer formulae
;		weight		flags wt%
;		thick		layer thicknesses (microns, mg/cm^2)
;		microns		flags microns
;		density		layer densities
;		ratio_yields		yield ratios across detectors
;		ratio_intensity		relative intensity ratio across detectors
;
;--------------------------------------------------------------------------------------
;
;	See Method/software log 8, page 260.
;
;	Parameters:	0 Noise		2 cal B		4 pileup	5 tail amp		7  backgnd 1	8 Compton tail amp
;				1 Fano		3 cal A					6 tail length	10 backgnd 2	9 Compton tail length
;
;	(ORG - RORG) should always be 10 (for line Tweeks)
;
;	ORG			start index for element intensity parameters
;	RORG		start index for line intensity tweaks
;				These are set in 'pixe_setup'.
;
;	for RORG=11, ORG=21 (setup in 'pixe_setup')
;						0-10	parameters (see above)
;						11-20	tweek lines (fit adjustments to line intensities)
;						21-*	element peak area (major line for each)
;
;	Note: The legal values of ORG and RORG are such that ORG-RORG equals 10.
;
;--------------------------------------------------------------------------------------

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_debug_2, pdebug2
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
		m = ['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c]
		warning,'PIXE_fit',m, /error
		MESSAGE, /RESET
		s = strjoin( m, ",")
		message = s
		goto, bad_exit
	endif
endif

common c_fit_model_6,  a, org, rorg, na, cpeaks, n_channels, x, cdetector, mask, name, note, $
			e_low, e_high, do_tail, ccompton
common c_fit_model_7, new_sxrf_mode

	linear_fit_initial = 0				; do one iteration of linear fit for initial

	error = 0
	cancel = 0
	message = 'success'
	progress_tlb = 0L
	pars = ptr_new()

	if n_elements(fix_cal) lt 1 then fix_cal=0
	if n_elements(fix_gain) lt 1 then fix_gain=0
	if n_elements(fix_fano) lt 1 then fix_fano=1
	if n_elements(fix_width) lt 1 then fix_width=0
	if n_elements(fix_tail) lt 1 then fix_tail=0
	if n_elements(no_tail) lt 1 then no_tail=0
	if n_elements(dynamic) lt 1 then dynamic=2
	if n_elements(pcm) lt 1 then pcm=''
	if n_elements(back_mode) lt 1 then back_mode=0
	if n_elements(do_split) lt 1 then do_split=0
	if n_elements(emid) lt 1 then emid=10.0
	if n_elements(pileup_mode) lt 1 then pileup_mode=0
	if n_elements(do_progressi) lt 1 then do_progressi=0
	if n_elements(refit) lt 1 then refit=0
	if n_elements(use_last) lt 1 then use_last=0
	if n_elements(silent) lt 1 then silent=0
	if n_elements(gamma) lt 1 then gamma=0
	if n_elements(use_m) lt 1 then use_m=1
	if n_elements(mp_loop0) lt 1 then mp_loop0=0
	if n_elements(correct) lt 1 then correct=0L
	if n_elements(sum_deficit) lt 1 then sum_deficit=0.5
	if n_elements(tweek_el) lt 1 then tweek_el=-1
	if n_elements(tweek_lines) ne 20 then begin
		tweek_lines = replicate(-1,20)
		tweek_el = -1
	endif
	if (tweek_el ge 0) and (n_elements(tweek_lines) ne 20) then begin
		message = 'Illegal length for tweek_lines array.'
		warning,'pixe_fit',message
	endif
	if n_elements(initiali) lt 1 then initiali=0
	if n_elements(show_df) lt 1 then show_df=-1
	if n_elements(scale_df) lt 1 then scale_df=1.0
	if n_elements(python) lt 1 then python=0
	initial = initiali
	do_progress = do_progressi
	if initial then do_progress=0
	mp_loop = mp_loop0
	if n_elements(tweak_par) lt 1 then tweak_par=0
	if tweak_par then initial=1

	if n_elements(p) lt 1 then goto, bad
	if n_elements(pspec) lt 1 then goto, bad
	if ptr_valid(p) eq 0 then goto, bad_ptr
	if ptr_valid(pspec) eq 0 then goto, bad_ptr
	if ptr_valid((*pspec).data) eq 0 then goto, bad_ptr
	if ptr_valid((*p).peaks) eq 0 then begin
		message = '"peaks" yields struct not defined'
		goto, bad_exit
	endif
	if ptr_valid((*p).detector) eq 0 then goto, bad_ptr
	if ptr_valid((*p).filter) eq 0 then goto, bad_ptr

	if gamma eq 0 then begin
		if n_elements(pback) lt 1 then goto, bad
		if ptr_valid(pback) eq 0 then begin
			message = 'Invalid "pback" pointer'
			goto, bad_exit
		endif
		if ptr_valid( (*pback)[0] ) eq 0 then begin
			message = 'Invalid "(*pback)[0]" pointer'
			goto, bad_exit
		endif
		if ptr_valid((*(*pback)[0]).data) eq 0 then begin
			message = 'Spec "data" pointer not defined'
			goto, bad_exit
		endif
		background1 = *(*(*pback)[0]).data										;@9-19
		background2 = background1
		background2[*] = 0.0
		if n_elements(*pback) gt 1 then begin
			if ptr_valid((*(*pback)[1]).data) then begin
				background2 = *(*(*pback)[1]).data
			endif
		endif
		background = background1 + background2
	endif else begin
		background = 0.0
	endelse
	back2_non_zero = (total(background2) gt 0.)									;@9-19

	n_channels = (*pspec).size
	spectrum = *(*pspec).data
	charge = (*pspec).charge													;@9-21 (how to deal with scalar charge, vector dt_corr?)
	dt_corr = (*pspec).deadtime_correction
	cal_ab, (*pspec).cal, cal_a, cal_b, cal_units, error=error
	if error then goto, bad_cal

	if pileup_mode eq 1 then begin
		if ptr_valid(ppileup) eq 0 then goto, bad_ptr
		if ptr_valid((*ppileup).data) eq 0 then goto, bad_ptr

		cal2 = (*pspec).cal
		cal2.poly *= (1. -sum_deficit/100. +0.2/100.)		; remap for pileup deficit (less -0.2% assumed in modelling)
															; i.e. correct for -0.2% assumed in 'Spectrum_Mark_Pileup_Image'

		pileup = map_spec( (*ppileup).data, cal2, (*pspec).cal, error=err)
	endif else pileup = 0.0

	sxrf = 0
	sxrf_mono = 0
	if ptr_valid((*p).yields) then begin
		sxrf = (((*(*p).yields).z1 eq 0) and ((*(*p).yields).a1 eq 0))
		sxrf_mono = 1
		if typevar( (*(*p).yields).beam) eq 'STRUCT' then begin
			if sxrf and ((*(*p).yields).beam.continuum eq 1) then sxrf_mono = 0
		endif
	endif

	if (*pspec).type eq 0 then begin
		if ((*(*p).peaks).Z1 eq 0) and ((*(*p).peaks).A1 eq 0) then (*pspec).type=7			; SXRF
	endif
	if gamma then (*pspec).type = 1

	e_min = cal_b
	e_max = (n_channels-1)*cal_a + cal_b
;	e_low = (*p).e_low > ((gamma eq 1) ? 50.0 : 0.603)		; 0.6 keV minimum for PIXE (also in strip_clip)
;	e_low = (*p).e_low > ((gamma eq 1) ? 50.0 : 0.3)		; 0.3 keV minimum for PIXE (also in strip_clip)
	e_low = (*p).e_low > ((gamma eq 1) ? 50.0 : 0.15)		; 0.15 keV minimum for PIXE (also in strip_clip)
	e_low = (e_low > e_min) < e_max
	e_high = ((*p).e_high > e_min) < e_max
	if e_low ge e_high then goto, bad_energy
	if (n_channels lt 50) or ((e_high-e_low)/cal_a lt 50) then begin
		message = 'Too few spectrum channels (<50)'
		goto, bad_exit
	endif

	if sxrf_mono and (fix_cal eq 0) and new_sxrf_mode then begin
		e_high = e_high < 0.85 * (*(*p).yields).e_beam
	endif

	if size(*(*p).detector,/tname) ne 'STRUCT' then goto, bad
	w0 = (*(*p).detector).w0
	w1 = (*(*p).detector).w1
	if gamma then begin
		FWHM = 1000.*sqrt(abs( w1 * 1332. + w0 ))			; FWHM @ Co 60 1.332 MeV (eV)
	endif else begin
		FWHM = 1000.*sqrt(abs( w1 * 5.898 + w0 ))			; FWHM @ Mn Ka (eV)
	endelse
	if initial eq 0 then begin
		print,' Initial cal: E =',cal_a,' * x +',cal_b,' keV'
		print,' Initial FWHM: w^2 =',w1,' * E +',w0,' keV^2'
		print,' '
	endif
;	p0 = ptr_new( *(*p).peaks)								; just for debugging
	
start:
	pyield = (*p).peaks										; cut down list of yields, intensities, etc.	
	el_fix = 1 - (*pyield).free								; (from 'fit_setup_trim_list')
	n_els = (*pyield).n_els
	if n_els lt 1 then begin
		fit = background
		goto, done
	endif
	pileup_ratio = 0.0

	if do_progress then begin
;		warning,'pixe_fit','debug: start fit.'
		progress, tlb=progress_tlb, title='GeoPIXE: Spectrum Fit'
	endif

;---- Set-up masks, a arrays, etc. ---------------------------------------------------------------------

	if gamma then begin			; PIGE setup ...

		pige_setup, e_low, e_high, n_channels, cal_a, cal_b, w0, w1, $
			el_fix, fix_fano, fix_cal, fix_width, fix_tail, no_tail, $
			n_els, mask, a, na, org, rorg, name, note, do_tail, (*p).detector

;---- Correct lines for filters, detector absorption, add escapes? --------------------------------------

;		peaks = correct_lines( pyield, e_low, e_high, a, org, mask, note, $
;						(*p).filter, (*p).detector)
		peaks = ptr_new( *pyield)

;---- Determine the x values, channel numbers, for fit, excluding cuts ----------------------------------

		x = select_pige_x( peaks, e_low, e_high, a, n_channels, (*p).cuts)

;---- Determine starting values for parameters A, and weed out weak lines -------------------------------

		pige_initial, peaks, dynamic, e_low, e_high, cal_a, cal_b, $
				n_els, mask, a, na, org, rorg, name, note, do_tail, spectrum, $
				x_valid=x, major_e=major_e

	endif else begin			; PIXE setup ...

		pixe_setup, e_low, e_high, n_channels, cal_a, cal_b, w0, w1, $
			el_fix, fix_fano, fix_cal, fix_width, fix_tail, no_tail, fix_gain, $
			n_els, mask, a, na, org, rorg, name, note, do_tail, (*p).detector, $
			pileup_mode=pileup_mode, tweek_el=tweek_el, tweek_lines=tweek_lines, $
			old_a=old_a, tweak_par=tweak_par

;---- Correct lines for filters, 'generic' detector absorption, add escapes ------------------------------

		peaks = correct_lines( pyield, e_low, e_high, a, org, mask, note, $
						(*p).filter, (*p).detector)

;---- Determine the x values, channel numbers, for fit, excluding cuts -----------------------------------

		x = select_x( e_low, e_high, cal_a, cal_b, n_channels, (*p).cuts)

;---- Determine starting values for parameters A, and weed out weak lines --------------------------------

		pixe_initial, peaks, dynamic, e_low, e_high, cal_a, cal_b, $
				n_els, mask, a, na, org, rorg, name, note, do_tail, spectrum, background, $
				x_valid=x, pileup_mode=pileup_mode, major_e=major_e, tweak_par=tweak_par, $
				tweek_el=tweek_el, tweek_lines=tweek_lines, use_last=use_last
	endelse

	if initial then goto, start_fit
	
;---- Yields and detector arrays --------------------------------------------------------------------------

; If /refit, then have possibility of different element lists. Build qz as a pointer back into old element list.
; This will be needed to access Yields, as well as 'ratio_yields'.

	if refit then begin						; Refit: re-order elements if need be
		array = (*old).array.on
		n_det = (*old).array.n_det
		Yyield = fltarr((*peaks).n_els)
		qz = intarr((*peaks).n_els)
		for i=0L,(*peaks).n_els-1 do begin
			qz[i] = where( ((*peaks).z[i] eq (*old).el.z) and ((*peaks).shell[i] eq (*old).el.shell))
			if qz[i] ne -1 then Yyield[i] = (*old).yield.yield[qz[i],(*peaks).unknown-1]
		endfor
	endif else begin
		array = (*peaks).array
		n_det = n_elements((*peaks).ratio_yield[*,0])
		Yyield = (*peaks).yield[*,(*peaks).unknown-1]
	endelse

	if ptr_valid( (*pspec).pactive) then begin
		active = *(*pspec).pactive
	endif else begin
		active = (*pspec).station-1
	endelse

; All array code assumes vectors in detector number order. Hence, (*peaks).ratio_yields,
; (*peaks).ratio_intensity and rGamma should all be in this order.

	nk = max( (*peaks).n_lines) > 1
	if array then begin
		if do_progress then begin
			progress, /running, progress_tlb, title='Detector array: calculate relative intensities ...'
		endif
		rY = (*peaks).ratio_yield[*,*]
		if refit then begin
			rGamma = fltarr(n_det,(*peaks).n_els)
			cIntensity = replicate( 1.0, nk,(*peaks).n_els)			; del n_det dimension 6/4/16
			nk1 = nk < (*old).array.nk
			for i=0L,(*peaks).n_els-1 do begin
				if qz[i] ne -1 then begin
					rGamma[*,i] = (*old).array.rGamma[*,qz[i]]
					cIntensity[0:nk1-1,i] = (*old).array.cIntensity[0:nk1-1,qz[i]]
				endif
			endfor
		endif
	endif else begin
		rGamma = 1.0
		cIntensity = 1.0
	endelse
	e = reform(major_e)
		
;	Use ‘detector_geometry’ to provide the effective geometry (theta, phi) of every detector
;	in the array that has been selected, given the global angles and tilt of the array as a whole.
		
	yield = array_yield( pdetector=(*p).detector, playout=(*p).playout, pfilter=(*p).filter, $
				Energy=e, charge=charge, array=array, active=active, $
				n_det=n_det, multiplicity=(*pspec).multiplicity, n_lines=(*peaks).n_lines, $ 
				E_line=(*peaks).e, ratio_intensity=(*peaks).ratio_intensity, $
				Y=Yyield, ratio_yield=rY, n_els=(*peaks).n_els, theta=(*peaks).theta, phi=(*peaks).phi, $
				refit=refit, use_last=use_last, pressure=pressure, temp=temp, $
				
				rGamma=rGamma, counts_per_ppm_uc=counts_per_ppm_uc, correct_intensity=cIntensity, $
				totGamma=totGamma, error=error)
	if error then goto, bad_yield

; Correct relative intensities (for central detector) by ratios to central corrected for rEff, rOmega
; rFilt averaged over all detectors.

	if array then begin
		(*peaks).intensity[0:nk-1,*] = (*peaks).intensity[0:nk-1,*] * cIntensity[0:nk-1,*]
	endif
		
;---- Parameters to vary for Dynamic, last iteration ----------------------------------

;	back_on = 0		Background is varied in linear L.S.
;					Just use matrix multiply (imaging).
;			  1		A separate background row is generated.
;					Calculate background using this row and
;					subtract background before matrix multiply (spectra only).

	if do_progress then begin
		progress, /running, progress_tlb, title='Setup for fit ...'
	endif

;---- The Fit ---------------------------------------------------------------------

start_fit:
	back_on = 0									; normal mode for fit and "dynamic=2" mode.
	if dynamic eq 1 then back_on = 1			; background subtraction (old Fortran option, not used in IDL yet)
;	mask[7] = 1									; re-enable back varying for testing
;												; when is this used? (what about mask[10]?)
	
;---- Mask phases --------------------------------------------------------------------

	build_mask_phases, mask, org, mask_phase, n_phases, mask_legend, mask_title, $
				mask_kill, gamma=gamma

;---- Initial function evaluation ----------------------------------------------------

	nit = 0
	max_its = 20
	done_kill = 0
	cancel = 0
	keep_cal = 0
	np = 0
	show_df = show_df < (n_elements(mask)-1)
	if initial eq 0 then begin
		mask = mask_phase[*,0]
	endif else begin
		if show_df ge 0 then mask[show_df] = 1
	endelse
	q = where( mask eq 1)
	nq = n_elements(q)
	chi = 0.0
	err = fltarr(na)
	f_chi = fltarr(n_channels)

	if linear_fit_initial and initial and (show_df lt 0) then begin
		initial = 0
		max_its = 1
	endif

	if gamma then begin
		fit = pige( a, org, peaks, n_channels, x, (*p).detector, $
			dfa=dfa, mask=mask, do_step=do_tail, e_low=e_low, e_high=e_high, $
			initial=initial, show_df=show_df, scale_df=scale_df, dff=dff )
	endif else begin
		fit = pixe( a, org, rorg, peaks, n_channels, x, background1, background2, (*p).detector, $
			dfa=dfa, mask=mask, do_tail=do_tail, e_low=e_low, e_high=e_high, $
			initial=initial, show_df=show_df, scale_df=scale_df, dff=dff, $
			pileup_mode=pileup_mode, pileup_spec=pileup, $
			tweek_el=tweek_el, tweek_lines=tweek_lines, compton=(*p).compton )		;@9-19
	endelse

	if initial or (show_df ge 0) then goto, done

	debug_monte = 0												; for Monte Carlo
	if debug_monte then begin
		if ptr_good( cpeaks) then ptr_free, cpeaks
		if ptr_good( ccompton) then ptr_free, ccompton
		if ptr_good( cdetector) then ptr_free, cdetector
		copy_pointer_data, peaks, cpeaks, /init					; populate the common for Monte Carlo
		copy_pointer_data, (*p).compton, ccompton, /init		; in "test_monte_phase_spectra"
		copy_pointer_data, (*p).detector, cdetector, /init	
	endif
	
	if (nq lt 1) or (q[0] eq -1) then goto, bad_mask
	y = spectrum[x]
	ni = n_elements(x)

	chi_old = chi_squared( fit[x], y, ni-nq)
	grad = (chi_old gt 1000.) ? 0.5 : 0.1
	grad_zero = 0

;---- if /python, export all data to files ------------------------

	if python then begin
		on_ioerror, bad_pyio
		openw, lun, geopixe_environment()+'fit-parameters.txt', /get_lun

		printf, lun, '# Assume for now:'
		printf, lun, '#	1. Single layer samples'
		printf, lun, '#	2. Single element detectors'
		printf, lun, '#'
		printf, lun, '# Flags and options'
		printf, lun, fix_cal, fix_gain, fix_width, fix_fano, fix_tail, no_tail
		printf, lun, org, rorg
		printf, lun, '#'
		printf, lun, '# Fit phases'
		printf, lun, mask_phase
		printf, lun, '#'
		printf, lun, '# Array dimensions'
		printf, lun, (*peaks).n_els, n_elements((*peaks).lines[*,0]), (*peaks).n_layers
		printf, lun, '#'
		printf, lun, '# Element, line data'
		printf, lun, '# Z'
		printf, lun, (*peaks).z
		printf, lun, '# shell'
		printf, lun, (*peaks).shell
		printf, lun, '# do_tail'
		printf, lun, do_tail
		printf, lun, '# n_lines'
		printf, lun, (*peaks).n_lines
		printf, lun, '# E'
		printf, lun, (*peaks).e
		printf, lun, '# lines (line index per line)'
		printf, lun, (*peaks).lines
		printf, lun, '# Intensity'
		printf, lun, (*peaks).intensity
		printf, lun, '# yield'
		printf, lun, yield
		printf, lun, '#'
		printf, lun, '# Detector parameters'
		printf, lun, array
		printf, lun, cal_b, cal_a
		printf, lun, w0, w1
		printf, lun, (*(*p).detector).tail.amp, (*(*p).detector).tail.L, (*(*p).detector).tail.S
		printf, lun, '#'
		printf, lun, '# Compton parameters'
		printf, lun, (*(*p).compton).tail.amp, (*(*p).compton).tail.len, (*(*p).compton).shift, (*(*p).compton).spread
		close_file, lun
	endif

;---- the fitting loop -----------------------------------------------------------------

	repeat begin

;....... the fit matrix equations, only for varied pars ................................

		t = dfa[x,*]
		df = t[*,q]
		f = fit[x]

		beta = dblarr(ni,nq)
		for k=0L,nq-1 do begin
			beta[*,k] = df[*,k] / (f > 1.0)			; Awaya 'f' weighting (T. Awaya, Nucl. Instr. Meth. 165 (1978), 449)
;			beta[*,k] = df[*,k] / (1.0)				; unit weighting (E. Clayton and C.G. Ryan, 1990, "Weighting Measures in Fitting PIXE Spectra", Nucl. Instr. and Meth. B49, 161-165)
		endfor

		alpha = df ## transpose(beta)
		
;		diagonal = (nq+1) * indgen(nq)
;		alpha[diagonal] = alpha[diagonal] * (1. + grad)

		diagonal = (nq+1) * indgen(nq)
		tad = alpha[diagonal]						; rather than scale up the diagonals to force
		alpha = alpha / (1. + grad)					; gradient search, scale down all the off-diagonals.
		alpha[diagonal] = tad						; this stops biasing results when grad is still not 0.
		
		print,'grad = ',grad
		inverse = invert( alpha, status)
		if status eq 2 then print,'Possible poor accuracy in matrix inversion'
		if status eq 1 then goto, bad_matrix

		gamma_matrix = inverse ## beta

		da = gamma_matrix ## transpose( y-f)
		a0 = a
		a[q] = a[q] + reform(da)
		
;		print,''
;		print,'mask=',mask
;		print,'A=',a0
;		print,'dA=',reform(da)
;		print,' A=',a
		if a[5] lt 0.0 then a[5]=a0[5]				; veto changes that go negative
		if a[6] lt 0.0 then a[6]=a0[6]
		if a[8] lt 0.0 then a[8]=a0[8]
		if a[9] lt 0.0 then a[9]=a0[9]
		
		nit = nit+1
		err[q] = sqrt( inverse[diagonal] * (1.+grad))

;..................................................................................

		np = nit < (n_phases-1)
		if done_kill eq 0 then begin
			if (nit gt (max_its+2)/2) and (nit gt n_phases+3) then begin
				mask = mask * mask_kill
				done_kill = 1
			endif else begin
				mask = mask_phase[*,np]
			endelse
		endif

		q = where( mask eq 1)
		nq = n_elements(q)
		if (nq lt 1) or (q[0] eq -1) then goto, bad_mask

		print,' loop=',np,'  mask-phase[8:9]=', mask[8:9]

		if gamma then begin
			pileup_ratio = 0.0
			fit = pige( a, org, peaks, n_channels, x, (*p).detector, $
						dfa=dfa, mask=mask, do_step=do_tail, e_low=e_low, e_high=e_high )
		endif else begin
			if pileup_mode eq 0 then begin
				sum_peaks, a, mask, name, note, do_tail, org, na, peaks, $
						pileup_ratio=pileup_ratio, e_high=e_high, sum_deficit=sum_deficit
			endif

			fit = pixe( a, org, rorg, peaks, n_channels, x, background1, background2, (*p).detector, $
					dfa=dfa, mask=mask, do_tail=do_tail, e_low=e_low, e_high=e_high, $
					pileup_mode=pileup_mode, pileup_spec=pileup, $
					tweek_el=tweek_el, tweek_lines=tweek_lines, compton=(*p).compton )		;@9-19
		endelse
;		(*pdebug2).fit = fit

		chi = chi_squared( fit[x], y, ni-nq, f_chi=fc)
		f_chi[x] = fc

		if do_progress then begin
			progress, /update, progress_tlb, {unit:0, value:0, current:nit<max_its, size:max_its}, cancel=cancel
		endif
		if cancel then begin
			message = 'Fit loop "cancelled" by user'
			goto, bad_exit
		endif

		if grad_zero eq 0 then begin
			if (grad lt 0.002) then begin
				grad = 0.0
				grad_zero = 1
			endif
			if chi lt 1.01*chi_old then begin
				grad = grad * 0.1
			endif else if chi gt 1.1*chi_old then begin
				grad = grad * 10.0
			endif
		endif

		ok = (nit ge n_phases) and $
			( ((abs(chi-chi_old) lt 0.01*chi) and $
			   (chi le chi_old+1.) and (grad lt 0.01)) or (chi lt 0.01) )
		done = (ok or (nit ge max_its) )
;ok=1
;done=1
		if done and (ok eq 0) then begin
			keep_cal=1
			if do_progress then begin
				warning, 'pixe_fit', ['Maximum number of iterations exceeded ('+string(max_its)+').', $
					'Spectrum label: ' + (*pspec).label, '', $
					'Fit is slow to converge or may be unstable.', $
					'Try reducing free parameters, or element list.','', $
					'Look for combinations of overlapping elements,', $
					'that may represent unresolvable ambiguity.'], cancel=cancel
				if cancel then begin
					message = 'Fit loop "cancelled" by user'
					goto, bad_exit
				endif
			endif
			message = 'exceed maximum iterations =' + string(max_its)
		endif

		chi_old = chi

	endrep until done

;---- cleanup, scale errors, determine MDL --------------------------------------------------------------------------

	if gamma then begin

		pige_cleanup, peaks, e_low, e_high, cal_a, cal_b, w0, w1, dcal_a, dcal_b, dw0, dw1, $
			na, org, mask, a, err, name, note, n_els, aname, area, aerr, mdl, $
			fit, x, f_chi, chi, rms

		FWHM = 1000.*sqrt(abs( w1 * 1332. + w0 ))			; FWHM @ Co 60 1.332 MeV (eV)
	endif else begin

;		Update this pileup_ratio value for info, but it will not be used for further
;		image correction, etc. in image pileup mode.

		if pileup_mode eq 1 then begin
			pileup_ratio = sqrt( (*pspec).pileup_ratio) * a[4]
		endif
		isilent = silent

		pixe_cleanup, peaks, e_low, e_high, cal_a, cal_b, w0, w1, dcal_a, dcal_b, dw0, dw1, $
			na, org, rorg, mask, a, err, name, note, n_els, aname, area, aerr, mdl, $
			pileup_ratio, background, fit, x, f_chi, chi, rms, pileup_mode=pileup_mode, $
			detector=(*p).detector, compton=(*p).compton, silent=isilent
		if (do_progressi eq 0) and (isilent eq 1) then silent=1

		FWHM = 1000.*sqrt(abs( w1 * 5.898 + w0 ))			; FWHM @ Mn Ka (eV)
	endelse

	qt = where( (finite(dt_corr) eq 0) or (dt_corr lt 0.2) or (dt_corr gt 5.), nqt)
	if nqt gt 0 then dt_corr[qt] = 1.0

	conc = float(area / yield) * dt_corr
	cerr = float(aerr / yield)
	cmdl = float(mdl / yield)

;---- Loop for multiphase iteration on composition --------------------------------------------------------------

;	Determine new weighted yields and intensities and store these in (*p).yields (so they get saved later).
;	Then cut-down list to selected elements and save in (*p).peaks (also saved), and loop to "start".

	if mp_loop gt 0 then begin
		mp_loop = mp_loop-1

		fit_setup_phase_yields, p, conc, phase=phase, minerals=minerals, use_m=use_m, error=err
		if err then goto, bad_list
		
		if do_progress then begin
			progress, /complete, progress_tlb, 'PIXE Fit completed, loop='+str_tidy(mp_loop+1)
			progress, /ending, progress_tlb
		endif

		use_last = 1				; to re-use detector array variation in 'array_yields'.
		goto, start
	endif
	
;---- Build final results struct --------------------------------------------------------------------------------

	if arg_present(da_pars) or arg_present(results) then begin

		el = fit_setup_elements( p, free=el_free, use_m=use_m, OK=OK)
		print,format='(/A,(T7,20A11))',' el=',el
		print,format='(A,(T10,20(G10.3,1x)))',' e=',e
		print,format='(A,(T10,20(G10.3,1x)))',' area=',area
		print,format='(A,(T10,20(G10.3,1x)))',' err=',aerr
		print,format='(A,(T10,20(G10.3,1x)))',' amdl=',mdl
		print,format='(A,(T10,20(G10.3,1x)))',' c/ppm*Q=',counts_per_ppm_uc * charge
		print,format='(A,(T10,20(G10.3,1x)))',' yield=',yield
		print,format='(/A,(T7,20A11))',' name=',aname
		print,format='(A,(T10,20(G10.3,1x)))',' conc=',conc
		print,format='(A,(T10,20(G10.3,1x)))',' cerr=',cerr
		print,format='(A,(T10,20(G10.3,1x)))',' cmdl=',cmdl
		print,format='(A,(T10,20(G10.3,1x)))',' DT_corr=',dt_corr
		if array then begin
			print,format='(A,(T10,20(G10.3,1x)))',' totGamma=', totGamma
			print,' cIntensity (relative intensity correction due to array) ='
			for i=0,n_els-1 do begin
				if (*peaks).n_lines[i] gt 0 then print,format='(I,(T10,10(G10.3,1x)))', i, cIntensity[0:((*peaks).n_lines[i]<nk)-1,i]
			endfor
		endif

;---- if /python, export all data and results to files ------------------------

		if python then begin
			on_ioerror, bad_pyio
			openw, lun, geopixe_environment()+'fit-spectrum.txt', /get_lun
			printf, lun, '# X spectrum channels to fit:'
			printf, lun, x
			printf, lun, '# Y spectrum counts to fit:'
			printf, lun, y
			printf, lun, '# Background:'
			printf, lun, background[x]
			printf, lun, '# fit results:'
			printf, lun, fit[x]
			printf, lun, '# chi per channel results:'
			printf, lun, f_chi[x]
			printf, lun, '# chi-squared:'
			printf, lun, chi
			close_file, lun

			openw, lun, geopixe_environment()+'fit-results.txt', /get_lun
			printf, lun, '# fit A, Err parameters result:'
			printf, lun, a
			printf, lun, err
			printf, lun, '# fit Area, Aerr result:'
			printf, lun, area
			printf, lun, aerr
			printf, lun, '# counts_per_ppm_uc * charge:'
			printf, lun, counts_per_ppm_uc * charge
			printf, lun, '# fit Conc, Err, MDL result:'
			printf, lun, conc
			printf, lun, cerr
			printf, lun, cmdl
			close_file, lun
		endif

;----------------------------------------------------------------------

		cfile = ''
		if ptr_valid((*p).cuts) then cfile = (*(*p).cuts)[0].file

		mode = 0
		mid = 0.0
		n_layers = n_elements((*peaks).thick)
		if n_layers eq 3 then begin
			mode = 1
			mid = (*peaks).thick[0] + 0.5*(*peaks).thick[1]
		endif

;		If /refit then reuse parameters from existing fit results.
;		NOTE: This has a problem if the element list has changed.
;		Detect this here, and pop up a warning message.

		if mp_loop0 gt 0 then begin
			rphase = {	mode:		mp_loop0, $			; multiphase loop mode (extra iterations)
				correct:	(*p).correct_file, $		; corrections filename
				minerals:	minerals, $					; end-members used for multiphase fit loop
				phase:		phase}						; phase fraction
		endif else begin
			rphase = {	mode:		0}					; single fit mode (no extra iterations)
		endelse

		if refit then begin
			if (*old).correct ne 0 then goto, bad_correct
			owarn = 0
			rforce = 0
			if (*old).n_els ne n_els then begin
				owarn = 1
			endif else begin
				q = where( ((*old).el.z ne (*peaks).z) or ((*old).el.shell ne (*peaks).shell) )
				if q[0] ne -1 then owarn=1
			endelse
			if (*old).cuts.file ne cfile then owarn=1
			if owarn then begin
				warning,'pixe_fit',['Some properies have changed (e.g. element list) since', $
					'the last fit that effect the calculation of geometry factors,', $
					'such as modelled for melt and fluid inclusions.', $
					'In those cases, you will need to go to the "Results Properties" window', $
					'and hit "apply" to force the recalculation of geometry normalization factors.']
				rforce = 1
			endif
			rscale = (*old).scale
			rdetector = (*old).detector
			rfilter = (*old).filter
			rnorm = (*old).inclusion.norm[qz]
			rstim = (*old).stim

			conc = rscale * rnorm * conc
			cerr = rscale * rnorm * cerr
			cmdl = rscale * rnorm * cmdl

			rinclusion = { X:		(*old).inclusion.x, $			; fluid inclusion length (microns)
						Y:			(*old).inclusion.y, $			; width (microns)
						m:			(*old).inclusion.m, $			; mid-plane depth (microns)
						option:		(*old).inclusion.option, $		; density options
						density:	(*old).inclusion.density, $		; fluid density (not same as (*peaks).density[1])
						type:		(*old).inclusion.type, $		; fluid type
						bubble:		(*old).inclusion.bubble, $		; bubble diameter (microns, optional)
						beam:		(*old).inclusion.beam, $		; beam details
						norm:		rnorm }							; shape norm factors (calc in results-properties)

			ryield = { title:		(*peaks).title, $		; yield title
						file:		(*peaks).file, $		; file name
						yield:		(*old).yield.yield[qz,*], $	; yield for elements present
						emin:		(*peaks).emin, $		; min X-ray energy
						emax:		(*peaks).emax, $		; max X-ray energy
						z1:			(*peaks).z1, $			; beam Z1
						a1:			(*peaks).a1, $			; beam A1
						e_beam:		(*peaks).e_beam, $		; beam energy
						state:		(*peaks).state, $		; beam charge state
						theta:		(*peaks).theta, $		; detector angle (in plane)
						phi:		(*peaks).phi, $			; detector angle (out of plane)
						alpha:		(*peaks).alpha, $		; target rotation
						beta:		(*peaks).beta, $		; target tilt
						unknown:	(*peaks).unknown, $		; unknown layer
						formula:	(*old).yield.formula, $	; layer formulae
						weight:		(*peaks).weight, $		; layer wt% flags
						thick:		(*old).yield.thick, $	; layer thickness (microns, mg/cm^2)
						microns:	(*peaks).microns, $		; layer microns flags
						density:	(*old).yield.density }	; layer densities

			rbeam =		(*peaks).beam						; source struct

 		endif else begin
			rforce = 0
			rscale = 1.0
			rdetector =  { name:	(*(*p).detector).crystal.name, $		; detector name
						file:		(*(*p).detector).file }					; file name
			rfilter = { name:		(*(*p).filter)[0].name, $				; filter names
						file:		(*(*p).filter_list)[(*p).filter_mode] }	; filter file
			rstim =   {	OK:			0, $					; flags a completed STIM correction
						E0:			0.0, $					; beam energy used
						Emean:		0.0, $					; mean STIM energy
						x:			0.0 }					; stim thickness

			beam_x = 0.0
			beam_y = 0.0
			beam_shape = 1									; Default shape is ellipse

			case (*pspec).shape.type of
				1: begin									; Box
					beam_shape = 0
					beam_x = max([(*pspec).shape.x,(*pspec).shape.y])
					beam_y = min([(*pspec).shape.x,(*pspec).shape.y])
					end
				2: begin									; Circle
					beam_shape = 1
					beam_x = (*pspec).shape.x
					beam_y = (*pspec).shape.y
					end
				5: begin									; Ellipse
					beam_shape = 1
					beam_x = (*pspec).shape.x
					beam_y = (*pspec).shape.y
					end
				else:
			endcase

			rinclusion = { X:		0.0, $					; fluid inclusion length (microns)
						Y:			0.0, $					; width (microns)
						m:			mid, $					; mid-plane depth (microns)
						option:		0, $					; density options
						density:	1.0, $					; fluid density (not same as (*peaks).density[1])
						type:		0, $					; fluid type
						bubble:		0.0, $					; bubble diameter (microns, optional)
						beam: { X:	beam_x, $				; beam X size (microns)
							Y:		beam_y, $				; Y size
							shape:	beam_shape }, $			; beam shape options
						norm:		replicate(1.0,n_els) }	; shape norm factors (calc in results-properties)

;			order in struct to match that in 'read_fit_results' ...

			ryield = { title:		(*peaks).title, $		; yield title
						file:		(*peaks).file, $		; file name
						yield:		(*peaks).yield, $		; yield for elements present (after multiphase loops)
						emin:		(*peaks).emin, $		; min X-ray energy
						emax:		(*peaks).emax, $		; max X-ray energy
						z1:			(*peaks).z1, $			; beam Z1
						a1:			(*peaks).a1, $			; beam A1
						e_beam:		(*peaks).e_beam, $		; beam energy
						state:		(*peaks).state, $		; beam charge state
						theta:		(*peaks).theta, $		; detector angle (in plane)
						phi:		(*peaks).phi, $			; detector angle (out of plane)
						alpha:		(*peaks).alpha, $		; target rotation
						beta:		(*peaks).beta, $		; target tilt
						unknown:	(*peaks).unknown, $		; unknown layer
						formula:	(*peaks).formula, $		; layer formulae
						weight:		(*peaks).weight, $		; layer wt% flags
						thick:		(*peaks).thick, $		; layer thickness (microns, mg/cm^2)
						microns:	(*peaks).microns, $		; layer microns flags
						density:	(*peaks).density }		; layer densities
					
			rbeam =		(*peaks).beam						; source struct
		endelse

		if rorg eq 0 then begin
			n_pars = org
			atweeks = replicate(1.0,10)						; these are line tweeks, not par tweeks
		endif else begin
			n_pars = rorg
			atweeks = a[rorg:org-1]							; org - rorg should always to 10
		endelse
		tweek_id = strarr(20)
		if tweek_el ge 0 then begin
			nt = min([(*peaks).n_lines[tweek_el],20])
			tweek_id[0:nt-1] = line_id((*peaks).lines[0:nt-1,tweek_el])
		endif
		if n_elements(tweek_lines ne 20) then tweek_lines=replicate(-1,20)
		if ptr_valid( (*p).compton) eq 0 then begin
			rcompton = { Tail: {Amp:1.0, $					; Compton tail amplitude
								Len:1.0}, $					; Compton tail length
						Shift: -0.006, $					; Compton peak shift adjustment for e-momentum
						Spread: 1.0 }						; Compton peak spread adjustment
		endif else begin
			rcompton = *(*p).compton
		endelse
		if gamma then begin
			rtail = { amp:	0.0, $							; Step amplitude
						L:	0.0, $							; Step E term
						S:	0.0 }							; ?
		endif else begin
			rtail = { amp:	(*(*p).detector).tail.amp, $		; Tail amplitude
						L:	(*(*p).detector).tail.L, $			; Tail length L parameter
						S:	(*(*p).detector).tail.S }			; Tail length S parameter
		endelse

;	Changes here have to be matched by 'load_fit_results' in 'fit_results'
;	NOTE: n_pars is implcit in ORG,RORG

		results = {	n_els:			n_els, $				; number of elements
				conc:				conc, $					; concentrations (ppm)
				error:				cerr, $					; conc error (1 sigma)
				mdl:				cmdl, $					; MDL (99% confidence)
				area:				area, $					; major line peaks areas
				aerror:				aerr, $					; area error
				amdl:				mdl, $					; area MDL (counts)
				scale:				rscale, $				; scale factor
				mode:				mode, $					; analysis mode (0:Thick, 1:fluid inc)
				org:				org, $					; offset to start of element pars
				rorg:				rorg, $					; offset over nlinear to line tweeks
				type:				(*pspec).type, $		; type of detector data (0:PIXE, 1:PIGE)
				veto:				intarr(n_els), $		; veto an element in tables, etc.

				el: { 	Z:			(*peaks).Z, $					; atomic numbers of elements fitted
						shell:		(*peaks).Shell, $				; shells of elememts
						name:		name[org:org+n_els-1], $		; names of elements
						note:		note[org:org+n_els-1], $		; fitting notes for element
						line:		line_id((*peaks).lines[0,*]), $ ; line IDs of remaining major line
						e:			reform((*peaks).e[0,*]), $		; energy of major lines
						mask:		mask[org:org+n_els-1] }, $		; fitting mask
				setup: { pcm:		pcm, $					; PCM parameter file name
						elow:		e_low, $				; E low
						ehigh:		e_high}, $				; E high
				fit: {	n_its:		nit, $					; number of iterations
						phases:		mask_title, $			; phases of fit
						chi:		chi, $					; reduced chi-squared
						rms:		rms}, $					; RMS error

				nlinear: { free: { cal:	(fix_cal eq 0), $	; free Cal parameters
						FWHM:		(fix_width eq 0), $		; free FWHM parameters
						Fano:		(fix_fano eq 0), $		; free Fano term in width
						Tail:		(fix_tail eq 0) }, $	; free Tail/Step parameters
						no_Tail:	no_tail, $				; no Tails/Steps included
						A:			A[0:n_pars-1], $		; non-linear parameters
						mask:		mask[0:n_pars-1], $		;   " mask
						name:		name[0:n_pars-1], $		; names of non-linear pars
						note:		note[0:n_pars-1], $		; fitting notes for pars
						pileup:		pileup_ratio }, $		; pileup ratio
				tweek: { el:		tweek_el, $				; element to tweek line intensities
						lines:		tweek_lines[0:19], $	; indices into tweek pars for each line (MUST be 20 elements)
						id:			tweek_id, $ 			; line IDs of remaining major line
						a:			atweeks }, $			; tweek parameter [10] values
				compton:			rcompton, $				; compton parameters
				cuts:	 { file:	cfile}, $				; cuts file
				detector:	 		rdetector, $			; detector
				filter: 			rfilter, $				; filter

				spectrum: { label:	(*pspec).label, $		; spectrum label
					file:			(*pspec).file, $		; spectrum file name
					charge:			charge, $				; charge (uC)
					multiplicity:	(*pspec).multiplicity, $	; detector array multiplicity
					cal: { a:		cal_a, $				; Cal A
							b:		cal_b, $				; Cal B
							da:		dcal_a, $				; error in Cal A
							db:		dcal_b, $				; error in Cal B
							units:	'keV'}, $				; Cal units
					FWHM: { w0:		w0, $					; FWHM w0 term
							dw0:	dw0, $					; error in FWHM w0
							w1:		w1, $					; FWHM w1 term
							dw1:	dw1 }, $				; error in FWHM w1 term
					Tail: 	rtail }, $						; tail / step parameters
				flux:				(*pspec).IC_total, $	; total IC count for spectrum
				deadtime_correction:	dt_corr, $			; deadtime correction
				
				array: { 	on:		array, $				; array detector mode used in fit
							N_det:	n_det, $				; number of detectors
							active:	active, $				; list of channels active in spectrum sum
							rGamma:	rGamma, $				; rGamma factors
							nk:		nk, $					; maximum number of lines for elements
							cIntensity: cIntensity }, $		; relative intensity correction factors

				background: { mode:	back_mode, $			; background mode (0=SNIP)
							scale:	a[7] }, $				; background scaling
				back_split: { mode:	fix(do_split), $		; background2 split mode (0=no, 1=yes)
							emid:	emid, $					; background2 split mid E
							scale:	a[10] }, $				; background2 scaling
				yield: 				ryield, $				; layer and yield details
				multiphase:			rphase, $				; multiphase fit mode and phase results
				beam:				rbeam, $				; source beam struct
				inclusion: 			rinclusion, $			; inclusion geometry, norm factors, etc.
				stim: 				rSTIM, $				; STIM correction of thickness parameters
				correct:			0, $					; correction type (0=none)
				force:				rforce, $				; force Apply update in results_properties
				counts_per_ppm_uc:	counts_per_ppm_uc $		; final yield factors
			}
	endif

;---- Build DA matrix parameters for 'calc_da_matrix2' ------------------------------------

	if arg_present(da_pars) then begin

		q = where( mask[org:*] eq 1)

;	N.B. This has to match the form in 'calc_da_merge_scatter' ('correct_da_loop'), 'pixe_fit'

		pars = { $
			mask:				mask, $							; parameter mask
			a:					a, $							; A
			name:				name, $							; names of parameters
			org:				org, $							; org: origin of elements
			rorg:				rorg, $							; rorg: origin of tweeks
			peaks:				ptr_new(*peaks,/no_copy), $		; peaks
			x:					x, $							; x
			do_tail:			do_tail, $						; do_tail / step
			e_low:				e_low, $						; low energy range
			e_high:				e_high, $						; high energy range
			cal_a:				cal_a, $						; Cal A
			cal_b:				cal_b, $						; Cal B
			counts_per_ppm_uc:	counts_per_ppm_uc, $			; yield factors
			array: { 	on:		array, $						; array detector mode used in fit
						N_det:	n_det, $						; number of detectors
						rGamma:	rGamma }, $						; yield ratios
			cmdl:				cmdl }							; MDL (ppm)

		da_pars = ptr_new( pars, /no_copy)
	endif

	error = 0								; indicates that results and pars are valid

;---- All done, return the fit spectrum ---------------------------------------------------

;	NOTE 'peaks' is not valid from here on

done:
	if do_progress then begin
		progress, /complete, progress_tlb, 'PIXE Fit completed.'
	endif
	if python then begin
		message = 'Operations cut short for Python output'
		goto, bad_exit
	endif
	if initial eq 0 then begin
		print,' Iteration ',nit,', chi-squared = ',chi
		print,format='(/A,(T10,20(I10,1x)))',' index = ',indgen(n_elements(A))
		print,format='(A,(T10,20(G10.3,1x)))',' A = ',a
		print,format='(A,(T10,20(G10.3,1x)))',' Peaks = ', a[org:org+n_els-1]
		if gamma then print,format='(A,(T10,20(G10.3,1x)))',' Back = ', a[org+n_els:org+2*n_els-1]
		print, ' '
		print,' Final  cal:   E =',cal_a,' * x +',cal_b,' keV'
		print,' Final FWHM: w^2 =',w1,' * x +',w0,' keV^2'
	endif

	if keep_cal eq 0 then begin						; update cal to new fitted values
		tcal = (*pspec).cal
		cal_ab, tcal, cal_a, cal_b, cal_units, /set
		(*pspec).cal = tcal
		if gamma eq 0 then (*(*pback)[0]).cal = tcal
	endif

	if pileup_mode eq 1 then begin					; update pileup spectrum scaling factor
		(*pspec).pileup_A4 = a[4]
		print,'Pileup_ratio = ',(*pspec).pileup_ratio,'  A4=',a[4]
	endif
	(*pspec).FWHM = FWHM
;	print,' FWHM (Mn K) = ',FWHM

	if gamma then begin
		gname = 'PIGE_Fit'
	endif else begin
		gname = 'PIXE_Fit'
	endelse
	sfit = define(/spectrum)
	sfit.source = (*pspec).source
	sfit.cal = (*pspec).cal
	sfit.label = gname+' (GeoPIXE)'
	sfit.comment = gname+' least squares fit to spectrum'
	sfit.size = (*pspec).size
	show_df = show_df < (n_elements(dfa[0,*])-1)

	if show_df ge 0 then begin
		sfit.label = 'DF '+str_tidy(show_df)+' by analytical formulae'
		sfit.comment = 'Analytical derivative'
		sfit.data = ptr_new( dfa[*,show_df])

		dfit = define(/spectrum)
		dfit.source = (*pspec).source
		dfit.label = 'DF '+str_tidy(show_df)+' by finite difference'
		dfit.cal = (*pspec).cal
		dfit.comment = 'Finite difference comparison'

		dfit.size = (*pspec).size
		dfit.data = ptr_new( dff, /no_copy)

		sfit.n_fit = 1
		sfit.fit[0] = ptr_new( dfit, /no_copy)
	endif else begin
		sfit.data = ptr_new( fit, /no_copy)
	endelse
	if do_progress then progress, /ending, progress_tlb

	return, ptr_new( sfit, /no_copy)

bad:
	message = 'Missing pointer parameters'
	warning, 'pixe_fit', message, /error, cancel=cancel
	goto, bad_exit
bad_ptr:
	message = 'Invalid pointers'
	warning, 'pixe_fit', message, /error, cancel=cancel
	goto, bad_exit
bad_energy:
	message = 'Illegal energy range'
	warning,' pixe_fit', message, /error, cancel=cancel
	goto, bad_exit
bad_mask:
	message = ['All parameters masked off in fit.','Spectrum label: ' + (*pspec).label]
	warning, 'pixe_fit', message, cancel=cancel
	goto, bad_exit
bad_yield:
	message = 'Error return from "array_yield"'
	warning, 'pixe_fit', message, cancel=cancel
	goto, bad_exit
bad_det:
	message = ['All array detectors invalid, or not enabled in fit.','Spectrum label: ' + (*pspec).label]
	warning, 'pixe_fit', message, cancel=cancel
	goto, bad_exit
bad_filter:
	message = ['Pin-hole "filters" are illegal with detector arrays.', $
				'However, they can be built into the detector "absorbers".']
	warning, 'pixe_fit', message, cancel=cancel
	goto, bad_exit
bad_cal:
	message = 'Spectrum is not calibrated correctly'
	warning,'pixe_fit',[message+',', $
				'or calibration units are "CHANNELS".','Spectrum label: ' + (*pspec).label, $
				'','Calibrate spectrum and try again.'], cancel=cancel
	goto, bad_exit
bad_matrix:
	message = 'Ill-conditioned transform matrix'
	warning, 'pixe_fit', [message+'.', $
			'Try reducing free parameters, or element list.','Spectrum label: ' + (*pspec).label,'', $
			'Look for combinations of overlapping elements,', $
			'that may represent unresolvable ambiguity.'], cancel=cancel
	goto, bad_exit
bad_correct:
	message = 'Cannot refit data with a correction applied.'
	warning, 'pixe_fit', [message+'.', $
			'Return to the original fit and separate background results.', $
			'Refit the original data, and reapply the correction.'], cancel=cancel
	goto, bad_exit
bad_list:
	message = 'Failed to trim new element and yields lists for loop.'
	warning,' pixe_fit', message, /error, cancel=cancel
	goto, bad_exit
bad_pyio:
	message = ['Bad write to Python files.','Check permissions for user ".geopixe" dir.']
	close_file, lun
	warning,' pixe_fit', message, /error, cancel=cancel
	goto, bad_exit

bad_exit:
	if do_progress and widget_info(progress_tlb,/valid) then progress, /ending, progress_tlb
	error = 1
	return, ptr_new()
	end
