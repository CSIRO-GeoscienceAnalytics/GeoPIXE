pro release_sems, sem_name

; release semaphores for n_buffers

COMPILE_OPT STRICTARR
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'release_sems',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif
if n_elements(sem_name) lt 1 then return
if strlen(sem_name[0]) lt 1 then return

	for i=0L,n_elements(sem_name)-1 do begin
		sem_release, sem_name[i]
;		sem_delete, sem_name[i]
	endfor
	return
end
