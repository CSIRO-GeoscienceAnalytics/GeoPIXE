pro save_plot_options, file, plot_options, error=error

	COMPILE_OPT STRICTARR
	ErrorNo = 0
	error = 1
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
			warning,'save_plot_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif
	if n_elements(file) eq 0 then goto, missing_pars
	if n_elements(plot_options) eq 0 then goto, missing_pars

	file = strip_file_ext(file) + '.plot'
	on_ioerror, bad_open
	openw, lun, file, /xdr, /get_lun

	on_ioerror, bad_io
	version = -6
	writeu,lun, version
	writeu,lun, plot_options
	error = 0

done:
	close_file, lun
	return

missing_pars:
	warning,'save_plot_options','missing argument(s).'
	goto, done
bad_open:
	warning,'save_plot_options','error opening file: '+file
	goto, done
bad_io:
	warning,'save_plot_options','error writing file: '+file
	goto, done
end	