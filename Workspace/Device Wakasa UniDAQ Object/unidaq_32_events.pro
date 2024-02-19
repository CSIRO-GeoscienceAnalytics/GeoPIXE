function unidaq_32_events, ev,n_buffer, channel_on,nc, e,x,y,n,ste, $
			ibranch,swap, ecount,length,skip, x0,y0, bad,debug
;
;  This assumes ev 					bytarr(n_buffer)
;				x,y,e,ste			uintarr(n_buffer)
;				channel_on			intarr(nc)
;				ibranch				int			internal branch computed goto (internal)
;				ecount				int			counts events in event block (internal)
;				length				int			block payload data length (internal)
;				skip				int			skip long words in data (internal)
;				x0,y0				int			current X,Y pixel position (internal)
;				-all else-			long

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on = 1

if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'unidaq_32_events',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

value = bytarr(18)
value[*] = 0					; pass all by reference
err = 0L
n = 0L

if size(ev,/tname) ne 'LONG' then goto, bad
if size(channel_on,/tname) ne 'INT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(n,/tname) ne 'LONG' then goto, bad
if size(ste,/tname) ne 'UINT' then goto, bad
if size(ibranch,/tname) ne 'INT' then goto, bad
if size(ecount,/tname) ne 'INT' then goto, bad
if size(length,/tname) ne 'INT' then goto, bad
if size(skip,/tname) ne 'INT' then goto, bad
if size(y0,/tname) ne 'INT' then goto, bad
if size(y0,/tname) ne 'INT' then goto, bad
if size(bad,/tname) ne 'LONG' then goto, bad
if size(debug,/tname) ne 'LONG' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'unidaq_32_events'), cdecl=geolib_cdecl(), $
			ev,long(n_buffer), channel_on,long(nc), e,x,y,n,ste, $
			ibranch, long(swap), ecount,length,skip, x0,y0, bad,debug, $
			value=value )

return, err

bad:
	print,' unidaq_32_events: Error - bad type for argument.'
	err = 1L
	return, err
end
