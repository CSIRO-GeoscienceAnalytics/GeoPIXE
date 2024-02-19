function mpa4_events2, ev, n_buffer,n_actual, e,x,y,ste, $
				veto,fx,n_fx, n_max,n, x0,y0, charge_adc, channel_on,nc, $
				j_mpa,ibranch, bad_xy

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
		warning,'mpa4_events2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

value = bytarr(20)
value[*] = 0					; pass all by reference
err = 0L
n = 0L

if size(ev,/tname) ne 'ULONG64' then goto, bad
if size(ste,/tname) ne 'UINT' then goto, bad
if size(x,/tname) ne 'LONG' then goto, bad
if size(y,/tname) ne 'LONG' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(veto,/tname) ne 'UINT' then goto, bad
if size(fx,/tname) ne 'LONG' then goto, bad
if size(channel_on,/tname) ne 'INT' then goto, bad

if size(x0,/tname) ne 'LONG' then goto, bad
if size(y0,/tname) ne 'LONG' then goto, bad

if size(n_buffer,/tname) ne 'LONG' then goto, bad
if size(n_actual,/tname) ne 'LONG' then goto, bad
if size(n,/tname) ne 'LONG' then goto, bad
if size(n_max,/tname) ne 'LONG' then goto, bad

if size(j_mpa,/tname) ne 'LONG' then goto, bad
if size(ibranch,/tname) ne 'LONG' then goto, bad
if size(bad_xy,/tname) ne 'LONG' then goto, bad

;Print, 'Before call to mpa4_events2 ...'
;help, geopixe_library(), geolib_name( 'mpa4_events2'), geolib_cdecl(), $
;	ev, n_buffer,n_actual, e,x,y,ste, veto,fx,long(n_fx), n_max,n, x0,y0, long(charge_adc), $
;	channel_on, long(nc), j_mpa,ibranch, bad_xy
;wait, 3.0

err = call_external( geopixe_library(), geolib_name( 'mpa4_events2'), cdecl=geolib_cdecl(), $
			ev, n_buffer,n_actual, e,x,y,ste, veto,fx,long(n_fx), n_max,n, x0,y0, long(charge_adc), $
			channel_on, long(nc), j_mpa,ibranch, bad_xy, $
			value=value )

return, err

bad:
	print,' mpa4_events2: Error - bad type for argument.'
	err = 1L
	return, err
end
