function get_default_reference, kvs, kvs_prefix, finder=finder, ruler=ruler, error=error

; Get the default translation reference for either:
; /finder or /ruler

COMPILE_OPT STRICTARR
error = 1
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'get_default_reference',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif
if n_elements(finder) eq 0 then finder=0
if n_elements(ruler) eq 0 then ruler=0
if finder eq 0 then ruler=1
	ref = 0L
	error = 1

	check_kvs, kvs, error=error
	if error then goto, kvs_fail

	if ruler then begin
		tag = 'ruler'
	endif else begin
		tag = 'finder'
	endelse

	kref = kvs_prefix + 'SL.default.' + tag + '.reference'

	template1 = define(/maia_frame_spec)
	ref = get_kvs( kvs, kref, template=template1, error=error)

done:
	return, ref

kvs_fail:
	warning,'get_default_reference',['KVS not reachable.','Abort KVS Load.'],/error
	goto, done
end

