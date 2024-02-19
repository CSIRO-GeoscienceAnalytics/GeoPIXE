function new_device_object, name, options=options, header=header, error=err

; Make a new device object, and inherit its display 'options' and 'header' from
; previous object instances.

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
		warning,'new_device_object',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif

	err = 1
	if n_elements(name) eq 0 then return, err
	if name eq '' then return, err

	dObj = obj_new( name)
	if size( options, /tname) eq 'STRUCT' then begin
		dObj->set_options, options
	endif
	if size( header, /tname) eq 'STRUCT' then begin
		dObj->set_header, header
	endif
	
	err = 0
	return, dObj
end
