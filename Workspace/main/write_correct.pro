pro write_correct, p, F, mode=mode, error=error

; Write the Correct_Yield parameters to 'F'
; 'p' is a pointer to the parameters struct.
; mode=	1 	for image correction matrix data
;		2	for simple composition correction matrix

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
		warning,'Write_correct',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		close, 1
		return
	endif
endif

	error = 1
	if n_params() lt 2 then begin
		print,'write_correct: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return
	if ptr_good(p) eq 0 then return
	if n_elements(mode) eq 0 then mode=1
	
	F = strip_file_ext(F) + (mode ? '.correct' : '.comat')

	version = -2L								; .correct version number
	on_ioerror, bad_io
	close, 1
	openw, 1, F, /XDR

	writeu,1, version
	writeu,1, (*p).n_comp, (*p).max_comp
	writeu,1, (*p).comp
	writeu,1, (*p).minerals
	writeu,1, (*p).original
	writeu,1, (*p).rest
	writeu,1, (*p).files
	writeu,1, (*p).R
	close_file,1
	error = 0
return

bad_io:
	print,'Write_correct: bad I/O'
	return
end
