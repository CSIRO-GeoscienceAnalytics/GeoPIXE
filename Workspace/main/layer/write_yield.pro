pro write_yield, p, F

; Write the yield definitions 'p' to file 'F'
; 'p' can be a yield struct, or pointer to yields,
; and yields can be a scalar or array of yields struct. 
; The array form is used either for a series of yield calculation
; with layer thicknesses varying (1D or 2D grid for one or two layers)
; or multiple beam energies.

COMPILE_OPT STRICTARR
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
		warning,'write_yield',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

	if n_params() lt 2 then begin
		print,'write_yields: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return
	if size(p,/tname) eq 'POINTER' then begin
		if ptr_good(p,/struct) eq 0 then goto, bad_data
		y = *p
	endif else begin
		y = p
	endelse
	if size(y,/tname) ne 'STRUCT' then goto, bad_data

	version = -11L					; .yield version number
	on_ioerror, bad_io
	close, 1
	openw, 1, F, /XDR
	i = 0
	ny = n_elements(y)

	writeu,1, version
	writeu,1, ny

more:
	peaks = y[i]

	nk = n_elements( peaks.intensity[*,0])
	writeu,1, peaks.n_els, nk, peaks.n_layers
	writeu,1, peaks.title
	writeu,1, peaks.z, peaks.shell, peaks.n_lines
	writeu,1, peaks.lines
	writeu,1, float(peaks.intensity), peaks.e
	writeu,1, float(peaks.yield)
	writeu,1, peaks.layers
	writeu,1, peaks.unknown, peaks.e_beam
	writeu,1, peaks.theta, peaks.phi, peaks.alpha, peaks.beta

	writeu,1, peaks.formula
	writeu,1, fix(peaks.weight), peaks.thick
	writeu,1, fix(peaks.microns), peaks.density
	writeu,1, fix(peaks.z1), fix(peaks.a1), peaks.state

	writeu,1, peaks.emin, peaks.emax
	writeu,1, peaks.array
	if peaks.array then begin
		writeu,1, n_elements(peaks.ratio_yield[*,0])			; n_det
		writeu,1, peaks.ratio_yield
		writeu,1, peaks.detector_file

		if n_elements( peaks.ratio_intensity) gt 1 then begin
			writeu,1, 1L
			writeu,1, peaks.ratio_intensity
		endif else begin
			writeu,1, 0L
		endelse
	endif

	use_mu_zero = 0L
	if n_elements(peaks.mu_zero) gt 1 then use_mu_zero=1L
	writeu,1, use_mu_zero
	if use_mu_zero then writeu,1, peaks.mu_zero

	use_beam = 0L
	if peaks.beam.continuum ge 1 then use_beam=1L
	writeu,1, use_beam
	if use_beam then begin
		writeu,1, peaks.beam.model
		writeu,1, peaks.beam
	endif

	if i+1 lt ny then begin
		i = i+1
		goto, more
	endif
	close,1
return

bad_io:
	print,'Write_yield: bad I/O'
	return
bad_data:
	print,'Write_yield: bad yield struct'
	return
end
