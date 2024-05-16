function read_fit_results, F, error=error

; Read the fit results from 'F'

;--------------------------------------------------------------------------------------
;
;	See Method/software log 8, page 260.
;
;	Parameters:	0 Noise		2 cal B		4 pileup	5 tail amp		7  backgnd 1	8 Compton tail amp
;				1 Fano		3 cal A					6 tail length	10 backgnd 2	9 Compton tail length
;
;	(ORG - RORG) should always to 10 (for line Tweeks)
;
;	ORG			start index for element intensity parameters
;	RORG		start index for line intensity tweaks
;				These are set in 'pixe_setup'.
;
;	for RORG=10, ORG=20
;						10-19	tweek lines
;						20-*	element peak area
;
;	Note: The legal values of ORG and RORG are such that ORG-RORG equals 10.
;
;--------------------------------------------------------------------------------------

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
if n_elements(geopixe_root) lt 1 then geopixe_root=''
filter_bug_retry = 1
filter_array_bug = 0
error = 1
presults = 0L

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		if filter_bug_retry then begin
			print,'read_fit_results: filter bug retry ...'
			filter_array_bug = 1
			filter_bug_retry = 0
			goto, retry
		endif
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'read_fit_results',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		close, 1
		return, 0L
	endif
endif

	if lenchr(F) lt 1 then return, 0L

retry:
	on_ioerror, bad_io
	close, 1
	openr, 1, F, /XDR

cont:
	valid_versions = [-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16]

	version = 0L
	readu,1, version
	q = where( version eq valid_versions)
	if q[0] eq -1 then goto, bad_version

	n = 0
	readu,1, n
	if (n le 0) or (n gt 10000) then goto, bad_io

	for i=0L,n-1 do begin
		n_els = 0
		n_layers = 0
		org = 0
		rorg = 0
		type = 0
		readu,1, n_els, n_layers, org
		if version le -4 then readu,1, rorg
		if version le -2 then readu,1, type

		n_pars = (rorg eq 0) ? org : rorg		; ORG-RORG = 10
		atweeks = replicate(1.0,10)				; fixed! this MUST always be 10
		idtweek = strarr(20)

		if (n_els le 0) or (n_els gt 300) then goto, bad_io
		if (n_layers le 0) or (n_layers gt 1000) then goto, bad_io
		if (org le 0) or (org gt 50) then goto, bad_io

		conc = fltarr(n_els)
		err = fltarr(n_els)
		mdl = fltarr(n_els)
		area = fltarr(n_els)
		aerror = fltarr(n_els)
		amdl = fltarr(n_els)
		readu,1, conc, err, mdl, area, aerror, amdl

		scale = 1.0
		mode = 0
		readu,1, scale, mode

		el = { z:intarr(n_els), shell:intarr(n_els), name:strarr(n_els), $
				note:strarr(n_els), line:strarr(n_els), e:fltarr(n_els), $
				mask:intarr(n_els) }
		readu,1, el

		setup = { pcm:'', elow:0.0, ehigh:0.0 }
		fit = { n_its:0, phases:'', chi:0.0, rms:0.0 }
		nlinear = { free:{cal:0B, FWHM:0B, Fano:0B, Tail:0B}, no_tail:0, $
				a:fltarr(n_pars), mask:intarr(n_pars), name:strarr(n_pars), note:strarr(n_pars), $
				pileup:0.0 }
		readu,1, setup, fit, nlinear

		cuts = { file:'' }
		detector = { name:'', file:'' }
		if filter_array_bug then begin
			filter= { name:strarr(2), file:'' }
		endif else begin
			filter= { name:'', file:'' }
		endelse
		readu,1, cuts, detector, filter
		if filter_array_bug then filter = { name:filter.name[0], file:filter.file }
		filter.file = strip_path( filter.file)
		detector.file = strip_path( detector.file)

		if version le -7 then begin
			spectrum = { label:'', file:'', charge:0.0, multiplicity:1, cal:{a:0.0, b:0.0, da:0.0, db:0.0, units:''}, $
					FWHM:{w0:0.0, dw0:0.0, w1:0.0, dw1:0.0}, tail:{amp:0.0, L:0.0, S:0.0} }
			background = { mode:0, scale:0.0 }
			readu,1, spectrum, background
		endif else if version le -5 then begin
			spectrum1 = { label:'', file:'', charge:0.0, multiplicity:1, cal:{a:0.0, b:0.0, da:0.0, db:0.0, units:''}, $
					FWHM:{w0:0.0, dw0:0.0, w1:0.0, dw1:0.0}, tail:{amp:0.0, length:0.0} }
			background = { mode:0, scale:0.0 }
			readu,1, spectrum1, background
			spectrum = { label:'', file:'', charge:0.0, multiplicity:1, cal:{a:0.0, b:0.0, da:0.0, db:0.0, units:''}, $
					FWHM:{w0:0.0, dw0:0.0, w1:0.0, dw1:0.0}, tail:{amp:0.0, L:0.0, S:0.0} }

			spectrum.label = spectrum1.label
			spectrum.file = spectrum1.file
			spectrum.charge = spectrum1.charge
			spectrum.cal = spectrum1.cal
			spectrum.FWHM = spectrum1.FWHM
			spectrum.tail.amp = spectrum1.tail.amp
			spectrum.tail.L = spectrum1.tail.length
		endif else begin
			spectrum1 = { label:'', file:'', charge:0.0, cal:{a:0.0, b:0.0, da:0.0, db:0.0, units:''}, $
					FWHM:{w0:0.0, dw0:0.0, w1:0.0, dw1:0.0}, tail:{amp:0.0, length:0.0} }
			background = { mode:0, scale:0.0 }
			readu,1, spectrum1, background
			spectrum = { label:'', file:'', charge:0.0, multiplicity:1, cal:{a:0.0, b:0.0, da:0.0, db:0.0, units:''}, $
					FWHM:{w0:0.0, dw0:0.0, w1:0.0, dw1:0.0}, tail:{amp:0.0, L:0.0, S:0.0} }

			spectrum.label = spectrum1.label
			spectrum.file = spectrum1.file
			spectrum.charge = spectrum1.charge
			spectrum.cal = spectrum1.cal
			spectrum.FWHM = spectrum1.FWHM
			spectrum.tail.amp = spectrum1.tail.amp
			spectrum.tail.L = spectrum1.tail.length
		endelse

		back_split = { mode:	0, $				; background2 split mode (0=no, 1=yes)
						emid:	10.0, $				; background2 split mid E
						scale:	1.0 }				; background2 scaling
		if version le -15 then begin
			readu,1, back_split
		endif

		flux = 0.0
		if version le -11 then begin
			readu,1, flux
		endif

		yield= { title:		'', $					; yield title
				file:		'', $					; file name
				yield:		fltarr(n_els,n_layers), $	; yield for elements present
				emin:		0.0, $					; min X-ray energy
				emax:		0.0, $					; max X-ray energy
				z1:			0, $					; beam Z1
				a1:			0, $					; beam A1
				e_beam:		0.0, $					; beam energy
				state:		0.0, $					; beam charge state
				theta:		0.0, $					; detector angle (in plane)
				phi:		0.0, $					; detector angle (out of plane)
				alpha:		0.0, $					; target rotation
				beta:		0.0, $					; target tilt
				unknown:	0, $					; unknown layer
				formula:	strarr(n_layers), $		; layer formulae
				weight:		intarr(n_layers), $		; layer wt% flags
				thick:		fltarr(n_layers), $		; layer thickness (microns, mg/cm^2)
				microns:	intarr(n_layers), $		; layer microns flags
				density:	fltarr(n_layers) }		; layer densities

		readu,1, yield

		inclusion= { X:		0.0, $					; fluid inclusion length (microns)
				Y:			0.0, $					; width (microns)
				m:			0.0, $					; mid-plane depth (microns)
				option:		0, $					; density options
				density:	0.0, $					; fluid density (not same as (*peaks).density[1])
				type:		0, $					; fluid type
				bubble:		0.0, $					; bubble diameter (microns, optional)
				beam: { X:	0.0, $					; beam X size (microns)
					Y:		0.0, $					; Y size
					shape:	0 }, $					; beam shape options
				norm:		fltarr(n_els) } 		; shape norm factors (calc in results-properties)

		readu,1, inclusion

		veto = intarr(n_els)
		if version le -3 then readu,1, veto			; veto elements in tables

		tweek = { el: -1, lines:replicate(-1,20), id:idtweek, A:atweeks}
		if version le -4 then readu,1, tweek		; tweek line intensities

		counts_per_ppm_uc = fltarr(n_els)
		if version le -5 then readu,1, counts_per_ppm_uc

		compton = { Tail: {Amp:1.0, $				; Compton tail amplitude
							Len:1.0}, $				; Compton tail length
					Shift: -0.006, $				; Compton peak shift adjustment for e-momentum
					Spread: 1.0 }					; Compton peak spread adjustment
		if version le -7 then readu,1, compton

		on = 0
		n_det = 0
		na = 0
		active = 0
		rGamma = 0.0
		cIntensity = 1.0
		nk = 40L
		cIntensity = replicate( 1.0, nk,n_els)
		if version le -8 then begin
			readu,1, on
			if on then begin
				readu,1, n_det, na
				active = intarr(na)
				rGamma = fltarr(n_det,n_els)
				readu,1, active
				readu,1, rGamma
				if version le -12 then begin
					readu,1, nk
					cIntensity = replicate( 1.0, nk,n_els)
					readu,1, cIntensity
				endif
			endif
		endif
		array = {on:on, n_det:n_det, active:active, rGamma:rGamma, nk:nk, cIntensity:cIntensity }

		stim =   {	OK:			0, $				; flags a completed STIM correction
					E0:			0.0, $				; beam energy used
					Emean:		0.0, $				; mean STIM energy
					x:			0.0 }				; stim thickness
		if version le -9 then begin
			readu,1, stim
		endif

		correct = 0									; type of correction (0= none)
		if version le -10 then begin
			readu,1, correct
		endif
		dt_corr = 1.0								; deadtime crrection
		if version le -13 then begin
			readu,1, dt_corr
		endif

		continuum = 0
		model = 0
		if version le -16 then begin
			readu,1, continuum
			use_beam = 0L
			if continuum ge 1 then use_beam=1L
			if use_beam then begin
				readu,1, model
				case model of
					1: begin						; source beam struct (for continuum sources)
						beam = read_source( unit=1, error=err2)
						if err2 then goto, bad_beam
						end
					2: begin						; pink beam struct (for continuum sources)
						beam = read_pink( unit=1, error=err2)
						if err2 then goto, bad_beam
						end
					else: begin
						beam = define(/source)
						end
				endcase 
			endif else begin
				beam = define(/source)
			endelse
		endif else begin
			beam = define(/source)					; source beam struct (for continuum sources)
			if version le -14 then begin
				beam = read_source( unit=1, error=err2)
				if err2 then goto, bad_beam
			endif
		endelse

;	Changes here have to be matched by 'pixe_fit'

		results = { n_els:	n_els, $				; number of elements
				conc:		conc, $					; concentrations (ppm)
				error:		err, $					; conc error (1 sigma)
				mdl:		mdl, $					; MDL (99% confidence)
				area:		area, $					; major line peaks areas
				aerror:		aerror, $				; area error
				amdl:		amdl, $					; area MDL (counts)
				scale:		scale, $				; scale factor
				mode:		mode, $					; analysis mode (0:Thick, 1:fluid inc)
				org:		org, $
				rorg:		rorg, $
				type:		type, $					; type of data (0:PIXE, 1:PIGE)
				veto:		veto, $					; veto an element in tables, etc.

				el:			el, $					; element details
				setup:		setup, $				; fit setup parameters
				fit:		fit, $					; fit stats
				nlinear:	nlinear, $				; non-linear parameters
				tweek:		tweek, $				; line intensity tweeks
				compton:	compton, $				; compton parameters
				cuts:		cuts, $					; cut file details
				detector:	detector, $				; detector
				filter:		filter, $				; filter
				spectrum:	spectrum, $				; source spectrum details
				flux:		flux, $					; total IC count for spectrum
				deadtime_correction:	dt_corr, $	; deadtime correction
				array:		array, $				; array detector parameters
				background:	background, $			; background options
				back_split:	back_split, $			; background2 split options
				yield:		yield, $				; PIXE yields
				beam:		beam, $					; beam source struct
				inclusion:	inclusion, $			; inclusion parameters
				stim: 		STIM, $					; STIM correction of thickness parameters
				correct:	correct, $				; correction applied
				force:		0, $					; force Results_Properties Apply
				counts_per_ppm_uc:	counts_per_ppm_uc $		; final yield factors
			}

		if i eq 0 then begin
			presults = ptr_new( results, /no_copy)
		endif else begin
			presults = [presults, ptr_new( results, /no_copy)]
		endelse
	endfor
	error = 0

finish:
	close_file,1
	return, presults

bad_ptr:
	warning,'read_fit_results','Bad initial results pointer',/error
	return, 0L
bad_beam:
	warning,'read_fit_results','Error reading results beam struct',/error
	goto, finish
bad_io:
	warning,'read_fit_results','Error reading results file',/error
	goto, finish
bad_version:
	warning, 'read_fit_results', 'Bad file version',/error
	goto, finish
end
