function bd12_events, ev,n_buffer, x0,y0, channel_on,nc, e,x,y,n,ste
;
;  This assumes ev is:		uintarr(n_buffer)
;				x,y,e,ste:	uintarr(n_buffer)
;				channel_on	intarr(nc)
;				-all else-	long
;
;				x0, y0		are the current, X,Y position

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'bd12_events',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

value = bytarr(11)
value[*] = 0					; pass all by reference
err = 0L
n = 0L

if size(ev,/tname) ne 'UINT' then goto, bad
if size(ste,/tname) ne 'UINT' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(channel_on,/tname) ne 'INT' then goto, bad
if size(x0,/tname) ne 'UINT' then goto, bad
if size(y0,/tname) ne 'UINT' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'bd12_events'), cdecl=geolib_cdecl(), $
			ev,long(n_buffer), x0,y0, channel_on,long(nc), e,x,y,n,ste, $
			value=value )

return, err

bad:
	print,' bd12_events: Error - bad type for argument.'
	err = 1L
	return, err
end
