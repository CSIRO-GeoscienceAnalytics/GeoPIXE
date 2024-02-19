function throttle_q, e,multiple,n_buffer, throttle=throttle, tbuff=tbuff

;  This assumes:
;				e:			uintarr(n_buffer)
;				multiple:	lonarr(n_buffer) or -1
;
;				throttle	uintarr(nt)
;				tbuff		uintarr(nt)

COMPILE_OPT STRICTARR
ErrorNo = 0
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
		warning,'throttle_q',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, -1L
	endif
endif
if n_elements(multiple) gt 1 then begin
	warning,'throttle_q','Cant handle "multiple" events here yet.'
	err = -1L
	return, err
endif
if n_elements(throttle) lt 1 then begin
	throttle = replicate(1US, 4096)
	tbuff = throttle
endif
nt = n_elements(throttle)
if n_elements(tbuff) ne nt then tbuff = uintarr(nt)
mask = intarr(n_buffer)

value = bytarr(7)
value[*] = 0					; pass all by reference
err = 0L

if size(e,/tname) ne 'UINT' then goto, bad
if size(mask,/tname) ne 'INT' then goto, bad
if size(throttle,/tname) ne 'UINT' then goto, bad
if size(tbuff,/tname) ne 'UINT' then goto, bad
if size(multiple,/tname) ne 'LONG' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'throttle_q'), cdecl=geolib_cdecl(), $
			e, multiple, long(n_buffer), throttle,tbuff,nt, mask, value=value )

if err ne 0 then return, -1L

q = where(mask eq 1)
return, q

bad:
	print,'throttle_q: Error - bad type for argument.'
	err = -1L
	return, err
end
