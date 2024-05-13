function read_beam, F, count=nq, skip=ns, remap_energy=mape, error=error

; Read ASCII files of energy versus spectral/transmission data.
;
; Skip 'ns' rows. Detect eV and convert to keV. 
; Interpolate and return with new energy values given by 'mape' (keV).

COMPILE_OPT STRICTARR
error = 1
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
		warning,'read_beam',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	if n_elements(ns) lt 1 then ns=0
	if n_elements(bin) lt 1 then bin=0.02
	if lenchr(F) lt 1 then goto, bad_file

	on_ioerror, bad_file
	openr,unit, F, /get_lun
	on_ioerror,err

	s = ''
	n_rows = 10000L
	while( ns gt 0) do begin			; skip
		readf, unit, s
		ns--
	endwhile
	
	data = replicate({energy:0.0, spec:0.0}, n_rows)

	on_ioerror, cont
	readf, unit, data

cont:
	close_file, unit

	q = where( data.spec ne 0.0, nq)
	if nq eq 0 then goto, bad_data
	data = data[q]

	kev = data[nq-1].energy lt 1000.
	if kev eq 0 then begin
		data.energy = data.energy/1000.
	endif

	if n_elements(mape) ne 0 then begin
		b = mape[0]
		a = mape[1]-mape[0]
		e = mape
		n = n_elements(mape)

		y = interpol( data.spec, data.energy, e) > 0.0

		error = 0
		return, {energy:e, spec:y, cal:{B:b, A:a}}
	endif else begin

		error = 0
		return, {energy:data.energy, spec:data.spec, cal:{B:data[0].energy, A:(data[1].energy-data[0].energy)}}
	endelse

err:
	warning, 'read_beam', 'Error reading file: '+F
	close_file, unit
	return, 0
bad_file:
	warning, 'read_beam', 'Error opening file: '+F
	close_file, unit
	return, 0
bad_data:
	warning, 'read_beam', 'Bad data in file: '+F
	close_file, unit
	return, 0
	end

	