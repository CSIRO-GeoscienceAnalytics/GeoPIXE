pro define_devices, force=force, titles=titles, names=names

; Search for object device define SAV files, and initialize the device lists.

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
		warning,'define_devices',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_device_objects_3, device_obj_names, device_obj_titles, device_obj_beam_type
common c_device_objects_4, device_import_list, device_import_valid

	base_device__define			; to make sure it is resolved before devices loaded
	
; Search for all "XXX_DEVICE" objects and open them ...
; (exclude "BASE_DEVICE" and "GENERIC_DEVICE" from list)

	if force or (n_elements(device_obj_names) lt 1) then begin
		device_import_valid = 0
		device_import_list = 0L
		
		obj = open_device_objects( error=error, name=name, title=title, beam_type=beam_type)
		if error then goto, bad_obj_list
		device_obj_names = name
		device_obj_titles = title
		device_obj_beam_type = beam_type
		
		first = 1
		n_obj = n_elements(obj)
		for i=0L,n_obj-1 do begin
			if obj_valid( obj[i]) then begin
				opt = obj[i]->get_import_list( error=err)
				if err eq 0 then begin
					if first then begin
						device_import_list = opt
						first = 0
					endif else begin
						device_import_list = [device_import_list, opt]
					endelse
				endif
			endif
		endfor
		gobj = obj_new('GENERIC_DEVICE')
		if obj_valid( gobj) then begin
			opt = gobj->get_import_list( error=err)
			if err eq 0 then begin
				if first then begin
					device_import_list = opt
					first = 0
				endif else begin
					device_import_list = [device_import_list, opt]
				endelse
			endif
		endif
		device_import_valid = 1-first

;		Free device objects

		for i=0,n_elements(obj)-1 do begin
			if obj_valid( obj[i]) then obj_destroy, obj[i]
		endfor
		if obj_valid( gobj) then obj_destroy, gobj
	endif

;	Output arrays

	titles = device_obj_titles
	names = device_obj_names

finish:
	return
	
bad_obj_list:
	warning,'define_devices','Failed to set-up Device object list.'
	goto, finish
end
