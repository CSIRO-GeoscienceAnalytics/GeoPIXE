pro set_default_reference, frame, finder=finder, ruler=ruler, perth=perth

; Set the default frame translation references for either:
; /finder or /ruler
; to current frame number 'frame'.

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
		warning,'set_default_reference',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(finder) eq 0 then finder=0
if n_elements(ruler) eq 0 then ruler=0
if n_elements(frame) eq 0 then frame=1
if n_elements(perth) eq 0 then perth=0
if finder eq 0 then ruler=1

	kvs = open_kvs()
	if perth then begin
		kref = 'MM.Per.SL.frame' + strtrim(string(frame),2) + '.reference'
	endif else begin
		kref = 'MM.Mel.SL.frame' + strtrim(string(frame),2) + '.reference'
	endelse
	print,'Retrieve from KVS: ', kref

	template1 = define(/maia_frame_spec)
	ref = get_kvs( kvs, kref, template=template1, error=error)
	if error then goto, bad
	pointer_display, ref

	if ruler then begin
		tag = 'ruler'
	endif else begin
		tag = 'finder'
	endelse

	if perth then begin
		name = 'MM.Per.SL.default.' + tag + '.reference'
	endif else begin
		name = 'MM.Mel.SL.default.' + tag + '.reference'
	endelse
	print,'Write back into KVS: ', name

	set_kvs, kvs, name, ref, error=error

done:
	close_kvs, kvs
	return
bad:
	warning,'set_default_reference','error ...'
	goto, done
end

