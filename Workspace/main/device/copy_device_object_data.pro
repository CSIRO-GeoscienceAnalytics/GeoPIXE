pro copy_device_object_data, sObj, dObj, error=err

; Copy a device object data/ options.

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
		warning,'copy_device_object_data',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	err = 1
	if obj_valid(sObj) eq 0L then begin
		print,'copy_device_object_data: Source Device Object ref not valid.'
		return
	endif
	if obj_valid(dObj) eq 0L then begin
		print,'copy_device_object_data: Dest Device Object ref not valid.'
		return
	endif

	name = obj_class( sObj)
	if strpos( name, '_DEVICE') lt 0 then begin
		print,'copy_device_object_data: sObj NOT a Device Object.'
		return
	endif
	name = obj_class( dObj)
	if strpos( name, '_DEVICE') lt 0 then begin
		print,'copy_device_object_data: dObj NOT a Device Object.'
		return
	endif

	options = sObj->get_options( error=err)
	if err then begin
		print,'copy_device_object_data: Failed to get source Device Object options.'
		return
	endif
	doptions = dObj->get_options( error=err)
	if err then begin
		print,'copy_device_object_data: Failed to get dest Device Object options.'
		return
	endif
	t1 = ptr_new(options)
	t2 = ptr_new(doptions)
	copy_pointer_data, t1, t2
	doptions = *t2
	ptr_free, t1,t2
	
	dObj->set_options, options	
	err = 0
	return
end
