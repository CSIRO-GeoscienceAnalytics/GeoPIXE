pro pink_calculate, p, Energy=E2, spec=spec2, convert=convert, pressure=pressure, temp=temp, error=error

;	Calculate the full spectrum (E2, Spec2) to be drawn,
;	and also a compressed form to go in the pink struct.
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

	try_compton = 0

	bin = (*p).modata.bin								; energy bin (keV) for 0.1% bandwidth?
	if (*p).mono.mode eq 0 then (*p).modata.mono[0] = 0.0

	if (*p).n_filters ge 1 then begin
		if convert then begin
			f = pink_convert_filter_to_standard( (*p).filters[0:(*p).n_filters-1], error=err)
		endif else begin
			f = (*p).filters[0:(*p).n_filters-1]
		endelse
	endif else begin
		f = make_filter('Be',0.0,name='null')
	endelse
	
	E = 0.5 + bin * findgen(2000)

	if (*p).fe_spectrum_file eq '' then return
	d = read_beam( (*p).fe_spectrum_file, count=n, skip=0, remap_energy=E, error=error)
	if error then return

	E = d.energy
	t = transmit( f, E, pressure=pressure, temp=temp)

	use_mono = 1
	if (*p).mono.mode ne 1 then use_mono = 0
	if use_mono then begin
		monE = (*p).modata.mono[0]
		if monE gt 0.5 then begin
			if n_elements( (*p).modata.mono) lt 2 then begin
				bw = 0.03
			endif else begin
				bw = (*p).modata.mono[1] / 100.
			endelse
			if n_elements( (*p).modata.mono) lt 3 then begin
				meff = 0.5
			endif else begin
				meff = (*p).modata.mono[2]
			endelse
		endif else use_mono = 0
	endif
	if use_mono then begin
		dE = monE * bw / 2.
		q = where( (E lt monE-dE) or (E gt monE+dE), nq)
		if nq gt 0 then t[q] = 0. 
		t = t * meff
	endif
	spec = d.spec * t

	q = where( (E ge 2.0) and (E lt 100.), nq)
	spec = spec[q]
	E = E[q]
	
	for i=0,(*p).n_mirrors-1 do begin
		dm = read_beam( (*p).mirrors[i].file, count=n, skip=2, remap_energy=E, error=error)
		if error then return

		spec = spec * dm.spec
	endfor

	if (*p).poly.mode eq 1 then begin
;		poly_rel = xos_transmission( E) / xos_transmission( (*p).poly.energy)		; polycapillary transmission, relative to 17.4 KeV
		poly_rel = E																; @3-23
		poly_rel[*] = 0.
		q = where( (E le max((*p).poly.etrans)) and (E ge min((*p).poly.etrans)) and (E gt 0.), nq)
		q2 = where( (*p).poly.etrans gt 0., nq2)
		if (nq eq 0) or (nq2 eq 0) then begin
			warning,'pink_calculate', ['Polycapillary table does not overlap with desired energies.', $
							'Table spans energies: '+str_tidy(min((*p).poly.etrans))+' to '+str_tidy(max((*p).poly.etrans)), $
							'Desired energies: '+str_tidy(min(E))+' to '+str_tidy(max(E))]
			return
		endif
		poly_rel[q] = interpol( (*p).poly.trans[q2], (*p).poly.etrans[q2], E[q])	; don't trust 'interpol' beyond E range
		gain = (*p).poly.gain * poly_rel											; flux gain of polycapillary versus E
		solid = 1.0e+3 * !pi * ((*p).poly.pinhole/2.)^2 / (*p).poly.distance^2		; solid-angle of pin-hole (msr)

		spec = 1.0e-3 * solid * spec * gain				; for 'ph/s/0.1%bandwidth' units
	endif else begin
;		solid = (*p).acceptance							; acceptance output solid-angle (msr) of system	
;		spec = 1.0e-3 * solid * spec					; for 'ph/s/0.1%bandwidth' units
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
	
	pink = define(/pink)
	N1 = n_elements(spec)
	N2 = n_elements(pink.spectrum.data)
	if N1 gt N2 then begin								; remap onto short 300 step spectrum for storage
		f = float(N1)/float(N2)
		spec = f * smart_congrid( spec, N2)
		proportion = smart_congrid( proportion, N2)
		E = smart_congrid( E, N2)
		N = N2
	endif else N=N1
	
	pink.continuum = 1
	pink.energy = E[max(where(spec gt 1.0e-10))]
	pink.model = 2
	pink.spectrum.N = N
	pink.spectrum.data[0:N-1] = spec[0:N-1] 
	pink.spectrum.E[0:N-1] = E[0:N-1] 
	pink.spectrum.cal.B = E[0]
	pink.spectrum.cal.A = E[1]-E[0]
	pink.file = (*p).file
	pink.title = (*p).title
	pink.fe_spectrum_file = (*p).fe_spectrum_file
	pink.n_mirrors = (*p).n_mirrors
	if (*p).n_mirrors gt 0 then pink.mirrors[0:(*p).n_mirrors-1] = (*p).mirrors[0:(*p).n_mirrors-1]

	pink.modata.spot = (*p).modata.spot
	pink.modata.bin = bin
	pink.modata.mono = (*p).modata.mono
	pink.acceptance = (*p).acceptance
	pink.n_filters = (*p).n_filters
	pink.filters[*].thick = 0.0
	if (*p).n_filters gt 0 then pink.filters[0:(*p).n_filters-1] = (*p).filters[0:(*p).n_filters-1]
	
	pink.mono.mode = (*p).mono.mode
	pink.mono.z = (*p).mono.z
	pink.poly = (*p).poly
	
	*p = pink
	pointer_display, p
	error = 0
	return
end
