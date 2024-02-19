function clone_object, sObj, error=err

; Clone an object.
; If it is a device object, then call 'clone_device_object' to
; also copy device data.

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
		warning,'clone_object',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif

	err = 1
	if obj_valid(sObj) eq 0L then begin
		if obj_null( sObj) then begin
			err = 0
			return, obj_new()
		endif
		print,'clone_object: Source Object ref not valid.'
		return, 0L
	endif

	name = obj_class( sObj)
	if strlen(name) eq 0L then begin
		print,'clone_object: Source Object name not valid.'
		return, 0L
	endif

	if strpos( name, '_DEVICE') gt 0 then begin
		dObj = clone_device_object( sObj, error=err)
		return, dObj
	endif else begin
		dObj = obj_new( name)
		err = 0
		return, dObj
	endelse
end
