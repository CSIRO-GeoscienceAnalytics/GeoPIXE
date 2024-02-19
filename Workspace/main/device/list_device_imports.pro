function list_device_imports, error=err, find=find

; Return the vector of structs specifying the import data formats
; Use "define_devices" to set this up first.
; If 'find' set, then search for a title to match (not case sensitive).

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
		warning,'list_device_imports',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, ''
	endif
endif
common c_device_objects_3, device_obj_names, device_obj_titles, device_obj_beam_type
common c_device_objects_4, device_import_list, device_import_valid
if n_elements(device_obj_names) eq 0 then define_devices

	err = (device_import_valid ne 1)
	list = device_import_list
	
	if keyword_set(find) then begin
		q = where( strlowcase(find) eq strlowcase(list.name), nq)
		if nq eq 0 then return, 0L
		return, list[q[0]]
	endif else begin
		return, list
	endelse
end