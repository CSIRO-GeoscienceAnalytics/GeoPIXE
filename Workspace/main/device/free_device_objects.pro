pro free_device_objects, obji

; Free selected device objects

COMPILE_OPT STRICTARR
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if n_elements(force) lt 1 then force=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'free_device_objects',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if ptr_valid(obji) then begin
		obj = *obji
	endif else begin
		obj = obji
	endelse

	n = n_elements(obj)
	if n eq 0 then return

	for i=0,n-1 do begin
		if obj_valid(obj[i]) then obj_destroy, obj[i]
	endfor

	return
end
