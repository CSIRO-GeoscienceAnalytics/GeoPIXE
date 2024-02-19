function open_device_objects, error=error, name=name, title=title, beam_type=beam_type

; Search for all XXX__define.pro (or XXX__define.sav) files
; and probe them to build a list of device names and object definitions

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
		warning,'open_device_objects',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c,'','While creating '+now+' Object.'], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif

error = 1
title = ''
name = ''
now = ''
if n_elements(geopixe_root) lt 1 then startupp

f = find_device_objects( error=error, files=files, generic_file=gfile)
;print, files
if error then return, 0L
n = n_elements(f)
if (n eq 1) and (f[0] eq '') then return, 0L

obj = objarr(n)
name = strarr(n)
title = strarr(n)
beam_type = intarr(n)
for i=0L,n-1 do begin
	restore, files[i]						; restore define and method routines
	now = f[i]
	obj[i] = obj_new( f[i])
	name[i] = obj[i]->name()
	title[i] = obj[i]->title()
	beam_type[i] = obj[i]->beam_type()
endfor
if n_elements(gfile) gt 0 then restore, gfile

error = 0
return, obj

bad_list:
	warning,'find_device_objects',['No device objects found,','with file-names: "XXX_device__define".']
	goto, err
	
err:
	error = 1
	return, 0L
end