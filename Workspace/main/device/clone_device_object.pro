function clone_device_object, sObj, error=err

; Clone a device object, and inherit all its 
; display 'options'.

COMPILE_OPT STRICTARR
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
		warning,'clone_device_object',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif

	err = 1
	if obj_valid(sObj) eq 0L then begin
		print,'clone_device_object: Source Device Object ref not valid.'
		return, 0L
	endif

	name = sObj->name()
	if strlen(name) eq 0L then begin
		print,'clone_device_object: Source Device Object name not valid.'
		return, 0L
	endif

	dObj = obj_new( name)
	options = sObj->get_options( error=err)
	if err then begin
		print,'clone_device_object: Failed to get source Device Object options.'
		return, dObj
	endif
	
	dObj->set_options, options
	
	err = 0
	return, dObj
end
