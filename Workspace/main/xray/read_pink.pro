function read_pink, F, unit=unit, error=error

; Read the X-ray Pink Beam details from unit 'unit', or open file 'F'.
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
		warning,'read_pink',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
error = 1
source = 0

	if (n_elements(F) lt 1) and (n_elements(unit) lt 1) then return, 0
	
	valid = [-1]			; valid version numbers
	on_ioerror, bad_io

	if n_elements(unit) ne 0 then begin
		stat = fstat(unit)
		if stat.open eq 0 then goto, bad_stat
		F = stat.name
		open_file = 0
	endif else begin
		F = strip_file_ext(F) + '.pink'
		open_file = 1
		on_ioerror, bad_file
		openr, unit, F, /XDR, /get_lun
	endelse

	version = 0L
	readu,unit, version
	q = where( version eq valid)
	if q[0] eq -1 then return, 0

;	Assume that filters are stored in standard form (thick in mg/cm2 even if /microns set),
;	and returns them in std form.

	pink = define(/pink)

	continuum = pink.continuum
	energy = pink.energy
	model = pink.model
	file = pink.file
	title = pink.title
	
	readu,unit, continuum, energy, model, file, title
	pink.continuum = continuum
	pink.energy = energy
	pink.model = model
	pink.file = F
	pink.title = title

	n = 0L
	readu,unit, n
	if n ne n_elements(pink.spectrum.data) then goto, bad_data
	
	spec = pink.spectrum
	readu,unit, spec
	pink.spectrum = spec
	
	modata = pink.modata
	readu,unit, modata
	pink.modata = modata

	n_filters = pink.n_filters
	readu,unit, n_filters, n
	if n ne n_elements(pink.filters) then goto, bad_filters
	pink.n_filters = n_filters

	filters = pink.filters
	readu,unit, filters
	pink.filters = filters

	mono = pink.mono
	readu,unit, mono
	pink.mono = mono
	if pink.mono.mode eq 0 then pink.modata.mono[0] = 0.0

	acceptance = pink.acceptance
	readu,unit, acceptance
	if acceptance lt 0.001 then acceptance = 0.001
	pink.acceptance = acceptance

	poly = pink.poly
	readu,unit, poly
	pink.poly = poly

	file = pink.fe_spectrum_file
	readu,unit, file
	pink.fe_spectrum_file = file

	n_mirrors = pink.n_mirrors
	readu,unit, n_mirrors, n
	if n ne n_elements(pink.mirrors) then goto, bad_mirrors
	pink.n_mirrors = n_mirrors

	mirrors = pink.mirrors
	readu,unit, mirrors
	pink.mirrors = mirrors
	error = 0
	
finish:
	if open_file then close_file, unit
	
;	p = ptr_new( pink, /no_copy)
	return, pink

bad_file:
	warning,'read_pink','error opening file: '+F
	goto, finish
bad_stat:
	warning,'read_pink','error connecting with unit'
	goto, finish
bad_io:
	warning,'read_pink','bad pink beam I/O'
	goto, finish
bad_data:
	warning,'read_pink','bad size of spectra read'
	goto, finish
bad_filters:
	warning,'read_pink','bad size of filter array read'
	goto, finish
bad_mirrors:
	warning,'read_pink','bad size of mirror array read'
	goto, finish
end

	
	