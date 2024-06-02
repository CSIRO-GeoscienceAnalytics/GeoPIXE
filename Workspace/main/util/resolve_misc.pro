pro resolve_misc

; A place to add routines that may get used in plugins, but that have NOT
; been used elsewhere in GeoPIXE, and hence are not in GeoPIXE.sav.
;
; Note: you can't simply add IDL or external pro and functions here using:
;		resolve_routine, 'libpro'
;		resolve_routine, 'libfun', /is_function
; This seems logical, but IDL does NOT resolve these.
;
; Hence, you need to add these as dummy calls.

COMPILE_OPT STRICTARR

	t = lmfit(0)

	return
end
