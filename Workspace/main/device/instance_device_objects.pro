function instance_device_objects, names, error=error

; Return an instance of all listed device names

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
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
		warning,'instance_device_objects',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c,'','While creating '+now+' Object.'], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif
error = 1

	n = n_elements( names)
	if n lt 1 then return, 0L

	obj = objarr(n)
	for i=0,n-1 do begin
		obj[i] = obj_new(names[i])
	endfor

	error = 0
	return, obj
end
