pro log_message, comm, messagei, type=itype, error=error

; Log a message to the MMlog server setup above

COMPILE_OPT STRICTARR
ErrorNo = 0
error = 1
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'log_message',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif

	if typename(comm) ne 'PYTHON' then return
	if typename(messagei) ne 'STRING' then return
	if n_elements(itype) eq 0 then itype = 'ERROR'
	type = strupcase( itype)
	message = messagei
	if strlen(message) gt 60 then message = strmid( message, 0, 200)

	gprint,level=2,'Log '+type+': '+message

	case type of
		'ERROR': begin
			r = comm.logger.error( message)
			end
		'WARNING': begin
			r = comm.logger.warning( message)
			end
		'INFO': begin
			r = comm.logger.info( message)
			end
		else:
	endcase

	error = 0
	return
end
