pro source_calculate, p, Energy=E2, spec=spec2, convert=convert, pressure=pressure, temp=temp, error=error

;	Calculate the full spectrum (E2, Spec2) to be drawn,
;	and also a compressed form to go in the source struct.
;
;	/convert	convert local 'pink_setup' filter parameters to standard mg/cm2 thick.
;
; If 'presssure' and 'temp' present, this indicates a gas filter.
;	In this case assume thick is mg/cm2 NPT (1013.25 HPa, 20 C) and scale this for new PT.
;	These are scalars, for ambient conditions.
;	Called from 'fit_recalculate_yields'.

	COMPILE_OPT STRICTARR
	error = 0
	if n_elements(convert) eq 0 then convert=0
	if ptr_valid(p) eq 0 then return
	if size(*p,/tname) ne 'STRUCT' then return
	if (*p).continuum eq 0 then return
	error = 1

	try_compton = 0										; test a Compton component

	bin = 0.02											; energy bin (keV) for 0.1% bandwidth?
	if (*p).mono.mode eq 0 then (*p).modata.mono[0] = 0.0
	trans_thick = (*p).beam.thick
	if (*p).beam.mode eq 0 then trans_thick = 0.0

	if (*p).n_filters ge 1 then begin
		if convert then begin
			f = source_convert_filter_to_standard( (*p).filters[0:(*p).n_filters-1], error=error)
		endif else begin
			f = (*p).filters[0:(*p).n_filters-1]
		endelse
	endif else begin
		f = make_filter('Be',0.0,name='null')
	endelse
	
	if (*p).mono.mode eq 1 then begin
		mono = (*p).modata.mono
	endif else begin
		mono = 0
	endelse

	spec = source_tube_spectrum( (*p).modata.volts, (*p).modata.power, bin=bin, Energy=E, eps=(*p).modata.eps, $
					phi=(*p).modata.phi, formula=(*p).modata.anode.formula, weight=(*p).modata.anode.weight, $
					filter=f, proportion=proportion, char=char, lines=lines, mono=mono, $
					transmission=trans_thick, pressure=pressure, temp=temp, error=error)
	if error then return
	
	if (*p).poly.mode eq 1 then begin
;		poly_rel = xos_transmission( E) / xos_transmission( (*p).poly.energy)		; polycapillary transmission, relative to 17.4 KeV
		poly_rel = E																; @3-23
		poly_rel[*] = 0.
		q = where( (E le max((*p).poly.etrans)) and (E ge min((*p).poly.etrans)) and (E gt 0.), nq)
		q2 = where( (*p).poly.etrans gt 0., nq2)
		if (nq eq 0) or (nq2 eq 0) then begin
			warning,'source_calculate', ['Polycapillary table does not overlap with desired energies.', $
							'Table spans energies: '+str_tidy(min((*p).poly.etrans))+' to '+str_tidy(max((*p).poly.etrans)), $
							'Desired energies: '+str_tidy(min(E))+' to '+str_tidy(max(E))]
			return
		endif
		poly_rel[q] = interpol( (*p).poly.trans[q2], (*p).poly.etrans[q2], E[q])	; don't trust 'interpol' beyond E range
		gain = (*p).poly.gain * poly_rel											; flux gain of polycapillary versus E
		solid = 1.0e+3 * !pi * ((*p).poly.pinhole/2.)^2 / (*p).poly.distance^2		; solid-angle of pin-hole (msr)

		spec = 1.0e-3 * solid * spec * gain				; for 'ph/s/0.1%bandwidth' units
		char = 1.0e-3 * solid * char * gain
	endif else begin
		solid = (*p).acceptance							; acceptance output solid-angle (msr) of system	
		spec = 1.0e-3 * solid * spec					; for 'ph/s/0.1%bandwidth' units
		char = 1.0e-3 * solid * char
	endelse
	
	if try_compton then begin							; see what Compton scattered component looks like
		compton = spec
		compton_rayliegh = 3.							; ratio of Compton to elastic (e.g. light matrix)
		noff = fix(3.0 / bin)							; but source anode will be heavy Z usually
		nc = n_elements(spec)
		compton[0:nc-1-noff] = compton[noff:nc-1]
		compton[nc-noff:*] = 0.
		spec = 0.5 * (spec + compton_rayliegh * gauss_smooth(compton,noff))
	endif

	spec2 = spec										; keep original at 20 eV 0.1% bandwidth bin
	E2 = E
	
	source = define(/source)
	N1 = n_elements(spec)
	N2 = n_elements(source.spectrum.data)
	if N1 gt N2 then begin								; remap onto short 300 step spectrum for storage
		f = float(N1)/float(N2)
		spec = f * smart_congrid( spec, N2)
		proportion = smart_congrid( proportion, N2)
		char = f * smart_congrid( char, N2)
		E = smart_congrid( E, N2)
		N = N2
	endif else N=N1
	
	source.continuum = 1
	source.energy = (*p).modata.volts
	source.model = 1
	source.spectrum.N = N
	source.spectrum.data[0:N-1] = spec[0:N-1] 
	source.spectrum.E[0:N-1] = E[0:N-1] 
	source.spectrum.proportion[0:N-1] = proportion[0:N-1]
	source.spectrum.char[0:N-1] = char[0:N-1]
	source.spectrum.cal.B = E[0]
	source.spectrum.cal.A = E[1]-E[0]
	source.file = (*p).file
	source.title = (*p).title
	source.modata.volts = (*p).modata.volts
	source.modata.power = (*p).modata.power
	source.modata.anode.name = (*p).modata.anode.name
	source.modata.anode.formula = (*p).modata.anode.formula
	source.modata.anode.weight = (*p).modata.anode.weight
	source.modata.spot = (*p).modata.spot
	source.modata.phi = (*p).modata.phi
	source.modata.eps = (*p).modata.eps
	source.modata.bin = bin
	source.modata.mono = (*p).modata.mono
	source.acceptance = (*p).acceptance
	source.n_filters = (*p).n_filters
	source.filters[*].thick = 0.0
	if (*p).n_filters gt 0 then source.filters[0:(*p).n_filters-1] = (*p).filters[0:(*p).n_filters-1]
	
	if size( lines, /tname) eq 'STRUCT' then begin
		nz = n_elements(lines.z) < n_elements(source.lines.z)
		nl = n_elements(lines.line[*,0]) < n_elements(source.lines.line[*,0])
		source.lines.n_lines[0:nz-1] = lines.n_lines[0:nz-1]
		source.lines.z[0:nz-1] = lines.z[0:nz-1]
		source.lines.shell[0:nz-1] = lines.shell[0:nz-1]
		source.lines.line[0:nl-1,0:nz-1] = lines.line[0:nl-1,0:nz-1]
		source.lines.e[0:nl-1,0:nz-1] = lines.e[0:nl-1,0:nz-1]
		source.lines.rel[0:nl-1,0:nz-1] = lines.rel[0:nl-1,0:nz-1]
	endif
	
	source.beam.mode = (*p).beam.mode
	source.beam.thick = trans_thick
	source.mono.mode = (*p).mono.mode
	source.mono.z = (*p).mono.z
	source.poly = (*p).poly
	
	*p = source
	error = 0
	return
end
