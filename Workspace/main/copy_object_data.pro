pro copy_object_data, sObj, dObj, init=init, error=err

; Copy an object's data.
; 	/init		create dObj first

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
		warning,'copy_object_data',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(init) lt 1 then init=0

	err = 1
	if obj_valid(sObj) eq 0L then begin
		if obj_null( sObj) eq 0 then begin
			print,'copy_object_data: Source Object ref not valid.'
			return
		endif
	endif
	
	if init then begin
		dObj = clone_object( sObj, error=err)
		if obj_null( dObj) then begin
			err = 0
			return
		endif
		if err then return
	endif
	if obj_valid(dObj) eq 0L then begin
		print,'copy_object_data: Dest Object ref not valid.'
		return
	endif
	
	name = obj_class( sObj)
	if strlen(name) eq 0L then begin
		print,'copy_object_data: Source Object name not valid.'
		return
	endif
	
	if strpos( name, '_DEVICE') gt 0 then begin
		copy_device_object_data, sObj, dObj, error=err
	endif
	return
end
