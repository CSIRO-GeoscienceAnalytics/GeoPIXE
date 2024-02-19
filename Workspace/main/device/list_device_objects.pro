function list_device_objects, title=title, beam_type=beam_type, include_generic=include_generic

; Return the device object list vector from common
; Use "define_devices" to set this up first.

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
		warning,'list_device_objects',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, ''
	endif
endif
common c_device_objects_3, device_obj_names, device_obj_titles, device_obj_beam_type
common c_device_objects_4, device_import_list, device_import_valid
if n_elements(device_obj_names) eq 0 then define_devices
if n_elements(include_generic) eq 0 then include_generic=0

	title = device_obj_titles
	beam_type = device_obj_beam_type
	names = device_obj_names
	if include_generic then begin
		names = [names, 'GENERIC_DEVICE']
		title = [title, 'Generic device']
	endif
	return, names
end