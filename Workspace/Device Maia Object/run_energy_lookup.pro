function run_energy_lookup, run, path=path, error=error

; Lookup the run 'run' in a selected CSV file and return the energy
; in column 1. All subsequent columns can contain run numbers.

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
		warning,'run_energy_lookup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0.0
	endif
endif

common c_run_energy_lookup_1, runs, energies
if n_elements(path) eq 0 then path=''

	get_file = 0
	if n_elements(runs) eq 0 then get_file = 1
	try = 0
	r = 0.0
	line = ''
	file = ''
	env = geopixe_environment() + 'energy-run-lookup.conf'
	save_back = 0

Loop:
	if try gt 3 then goto, done

	if get_file eq 0 then begin
		q = where( runs eq run, nq)		; is run found in lookup table?
		if nq eq 0 then get_file = 1
	endif

	if get_file then begin
		if try eq 0 then begin			; try lookup file in conf in .geopixe first
			on_ioerror, more
			openr, lun, env, /get_lun
			on_ioerror, more
			readf, lun, file
more:
			close_file, Lun
			if file eq '' then begin
				try = try+1
				goto, loop
			endif
			save_back = 0
		endif else begin				; prompt user for a new lookup table file

			file = file_requester( filter='*.csv', path=path, title='Select new energy-run lookup file')
			if file eq '' then goto, done
			save_back = 1
		endelse

		if save_back then begin
			on_ioerror, bad_file2		; save new lookup table filename to conf
			openw, lun, env, /get_lun
			on_ioerror, bad_write
			printf, lun, file
			close_file, Lun
		endif
	
		energies = fltarr(10000)
		runs = lonarr(10000)
		k = 0							; read energy-run lookup table from file
		on_ioerror, bad_file
		openr, lun, file, /get_lun
		while (EOF(lun) eq 0) and (k lt 10000) do begin
			on_ioerror, bad_read
			readf, lun, line
			if strmid( line, 0,1) ne '#' then begin
				s = strsplit( line, ' 	,', /extract, count=n)
				if n ge 2 then begin
					energies[k:k+n-2] = float2(s[0])
					runs[k:k+n-2] = long2(s[1:n-1])
					k = k+(n-1)
				endif
			endif
		endwhile
		energies = energies[0:k-1]
		runs = runs[0:k-1]
		get_file = 0
		try = try+1
		goto, loop
	endif

	r = energies[q[0]]
	error = 0

done:
	close_file, lun
	return, r

bad_file:
	warning,'run_energy_lookup','Bad energy-run lookup table file open.'
	goto, done
bad_read:
	warning,'run_energy_lookup','Bad energy-run lookup table file read.'
	goto, done
bad_file2:
	warning,'run_energy_lookup','Bad energy-run conf open.'
	goto, done
bad_write:
	warning,'run_energy_lookup','Bad energy-run conf write.'
	goto, done
end
