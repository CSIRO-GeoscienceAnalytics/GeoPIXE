function read_source, F, unit=unit, error=error

; Read the X-ray Source details from unit 'unit', or open file 'F'.
; Only close 'unit' if opened here to file 'F'.

ErrorNo = 0
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
		warning,'read_source',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
error = 1
source = 0

	if (n_elements(F) lt 1) and (n_elements(unit) lt 1) then return, 0
	
	if n_elements(unit) ne 0 then begin
		stat = fstat(unit)
		if stat.open eq 0 then goto, bad_stat
		F = stat.name
		open_file = 0
	endif else begin
		F = strip_file_ext(F) + '.source'
		open_file = 1
		on_ioerror, bad_file
		openr, unit, F, /XDR, /get_lun
	endelse

	valid = [-1,-2,-3,-4,-5,-6,-7]			; valid version numbers

;	Assume that filters are stored in standard form (thick in mg/cm2 even if /microns set),
;	and returns them in std form.

	on_ioerror, bad_io
	version = 0L
	readu,unit, version
	q = where( version eq valid)
	if q[0] eq -1 then return, 0

	source = define(/source)

	continuum = source.continuum
	energy = source.energy
	model = source.model
	file = source.file
	title = source.title
	
	readu,unit, continuum, energy, model, file, title
	source.continuum = continuum
	source.energy = energy
	source.model = model
	source.file = F
	source.title = title

	n = 0L
	readu,unit, n
	if n ne n_elements(source.spectrum.data) then goto, bad_data
	
	spec = source.spectrum
	readu,unit, spec
	source.spectrum = spec
	
	modata = source.modata
	readu,unit, modata
	source.modata = modata

	n_filters = source.n_filters
	readu,unit, n_filters, n
	if n ne n_elements(source.filters) then goto, bad_filters
	source.n_filters = n_filters

	filters = source.filters
	readu,unit, filters
	source.filters = filters

	if version le -2 then begin
		lines = source.lines
		readu,unit, lines
		source.lines = lines
	endif

	if version le -3 then begin
		beam = source.beam
		readu,unit, beam
		source.beam = beam
		if source.beam.mode eq 0 then source.beam.thick = 0.0
	endif

	if version le -4 then begin
		mono = source.mono
		readu,unit, mono
		source.mono = mono
		if source.mono.mode eq 0 then source.modata.mono[0] = 0.0
	endif

	if version le -5 then begin
		acceptance = source.acceptance
		readu,unit, acceptance
		if acceptance lt 0.001 then acceptance = 0.001
		source.acceptance = acceptance
	endif

	if version le -6 then begin							; @3-23
		if version le -7 then begin
			poly = source.poly
			readu,unit, poly
			source.poly = poly
		endif else begin
			pold = {	mode:	0, $					; poly mode (0=off, 1=on)
						gain:	21000., $				; flux gain
						energy:	17.4, $					; energy of this flux gain
						model:	'XOS default', $		; name of polycapillary transmission function model
						diameter:	2.0, $				; diameter of beam at exit (mm)
						focus:		14.0, $				; focal distance (mm)
						spot:	0.03, $					; focus spot size
						pinhole: 0.025, $				; pinhole diameter (at 'distance' mm) as flux gain reference
						distance: 100.}					; distance of pinhole
			readu,unit, pold
			e = 0.33 * findgen(300)						; must be 300 elements
			trans = xos_transmission(e) / xos_transmission(pold.energy)

			pnew = source.poly
			struct_assign, pold, pnew
			source.poly = pnew
			source.poly.etrans = e						; transmission table energies (keV)
			source.poly.trans =	trans					; transmissions
		endelse
	endif
	error = 0
	
finish:
	if open_file then close_file, unit
	
;	p = ptr_new( source, /no_copy)
	return, source

bad_file:
	warning,'read_source','error opening file: '+F
	goto, finish
bad_stat:
	warning,'read_source','error connecting with unit'
	goto, finish
bad_io:
	warning,'read_source','bad source I/O'
	goto, finish
bad_data:
	warning,'read_source','bad size of spectra read'
	goto, finish
bad_filters:
	warning,'read_source','bad size of filter array read'
	goto, finish
end

	
	