pro write_detector, detectori, F, error=error

; Write the Detector definitions to 'F'

	error = 1
	if n_params() lt 2 then goto, bad_pars
	if lenchr(F) lt 1 then goto, bad_pars

	det = detectori
	if size(det,/tname) eq 'POINTER' then det = *det
	if size(det,/tname) ne 'STRUCT' then goto, bad_det

	F = strip_file_ext(F) + '.detector'

	version = -9L					; .detector version number
	on_ioerror, bad_io
	close, 1
	openw, 1, F, /XDR

	writeu,1, version
	writeu,1, det.pige

	if det.pige eq 0 then begin
		if size(det.absorbers,/tname) ne 'STRUCT' then begin
			writeu,1, 0
		endif else begin
			n = n_elements(det.absorbers)
			writeu,1, n
			writeu,1, det.absorbers[0:n-1]
		endelse

		writeu,1, det.crystal
		writeu,1, det.diameter, det.density, det.distance, det.source
		writeu,1, det.gamma, det.w0, det.w1, det.resolution
		writeu,1, det.aeff, det.beff
		writeu,1, det.tail
		writeu,1, det.tilt
		writeu,1, det.use_special1, det.use_special2
		if det.use_special1 then writeu,1, det.special1
		if det.use_special2 then writeu,1, det.special2
		writeu,1, det.cohen
		writeu,1, det.layout, det.array, det.shape
		writeu,1, det.correct_solid_angle

	endif else begin
		writeu,1, det.crystal
		writeu,1, det.a
		writeu,1, det.e_low, det.e_high
	endelse

	error = 0
	close,1

return

bad_io:
	warning,'Write_detector','error writing detector file: '+F
	return
bad_det:
	warning,'Write_detector','error in detector structure'
	return
bad_pars:
	warning,'write_detector','missing arguments in call'
	return
end
