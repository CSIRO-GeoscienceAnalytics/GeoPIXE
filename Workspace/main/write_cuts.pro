pro write_cuts, p, F

; Write the CUT definitions to 'F'
; 'p' is a pointer to the cuts array.

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
		warning,'Write_cuts',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_io
	endif
endif

	if n_params() lt 2 then begin
		print,'write_cuts: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return
	if ptr_valid(p) eq 0 then return
	n = n_elements(*p)
	F = strip_file_ext(F) + '.cuts'

	version = -2L					; .cut version number
	on_ioerror, bad_io
	close, 1
	openw, 1, F, /XDR

	writeu,1, version
	writeu,1, n
	writeu,1, (*p)[0:n-1]
	close,1
return

bad_io:
	close, 1
	print,'Write_cuts: bad I/O'
	return
end
