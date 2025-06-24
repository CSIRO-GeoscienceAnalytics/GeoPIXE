pro file_lcm_preview, file, preview=preview

; Provide preview of LCM file details for 'file_requester' from 'file'.
; Return preview data in 'preview' struct.

COMPILE_OPT STRICTARR
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
		warning,'file_lcm_preview',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
define_devices

	preview = 0L
	if n_elements(file) lt 1 then return
	
	on_ioerror, bad
	close, 2
	openr, 2, file, /xdr

	on_ioerror, bad
	s = ''
	readu,2, s
	version = 0
	gamma = 0
	if inumeric(s) then begin
		version = fix(s)
		if version le -2 then begin
			readu,2, gamma
		endif
		if version lt 0 then begin
			readu,2, s
		endif else version = 0
	endif

	valid = [0,-1,-2,-3,-4,-5]
	q = where( version eq valid)
	if q[0] eq -1 then return

	output_file = s
	readu,2, s
	title = s

	beam = {beam, mode:0, z1:0, a1:0, state:0.0, state_factor:0.0, e_factor:0.0, energy:0.0}
	readu,2, beam
	
	source_file = ''
	if (beam.mode eq 6) or (beam.mode eq 7) then readu,2, source_file
	
	mono = (beam.mode le 5)

	detector = {detector2, theta:0.0, phi:0.0}
	readu,2, detector

	target = {target, alpha:0.0, beta:0.0}
	readu,2, target

	emin = 0.0
	emax = 0.0
	readu,2, emin, emax

	array = 0
	dfile = ''
	if version le -3 then begin
		readu,2, array
		if array then readu,2, dfile
	endif

	unknown = 0L
	n = 0L
	readu,2, unknown, n

	if n gt 0 then begin
		layer = replicate( {layer2, thick:0.0, microns:0, density:0.0, formula_mode:0, weight:0, $
							formula:'', oxides:fltarr(16) }, n)
		many = intarr(n)
		tmin = fltarr(n)
		tmax = fltarr(n)
		tstep = fltarr(n)

		readu,2, layer
		if version le -4 then begin
			readu,2, many
			readu,2, tmin
			readu,2, tmax
			readu,2, tstep
		endif
	endif

	close, 2

;------------------------------------------------------------
; Details string array:

	list = ['File: ' + strip_path(file),'Title: '+ title,'']
	if (beam.z1 eq 0) and (beam.a1 eq 0) then begin
		if mono then begin
			list = [list, 'Monochromatic X-rays']
			list = [list, 'Energy: ' + str_tidy(beam.energy)]
		endif else begin
			list = [list, 'Continuum source file: '+source_file]
		endelse
	endif else begin
		list = [list, 'Beam: Z1 = ' + str_tidy(beam.z1) + ', A1 = ' + str_tidy(beam.a1)]
		list = [list, 'Energy: ' + str_tidy(beam.energy)]
	endelse
	list = [list, '', 'Theta: ' + str_tidy(detector.theta) + ', Phi: ' + str_tidy(detector.phi)]
	list = [list, 'Alpha: ' + str_tidy(target.alpha) + ', Beta: ' + str_tidy(target.beta),'']

	if array eq 1 then begin
		list = [list, 'Array detector: ' + dfile, '']
	endif

	list = [list, 'Number of layers: '  + str_tidy(n)] 
	for i=0,n-1 do begin
		list = [list, '    ' + layer[i].formula] 
		units = layer[i].microns ? '(microns)' : '(mg/cm^2)'
		list = [list, '        Thickness: ' + str_tidy(layer[i].thick) + ' ' + units] 
	endfor

;------------------------------------------------------------

	preview = {details:list}
	return

bad:
	return
end