function mpa4_events, ev, n_buffer,n_actual, $
				e,x,y,ste, veto,weight, n,n_max, j_mpa,ibranch, bad_xy

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
		warning,'mpa4_events',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

value = bytarr(14)
value[*] = 0					; pass all by reference
err = 0L
n = 0L

if size(ev,/tname) ne 'ULONG64' then goto, bad
if size(ste,/tname) ne 'UINT' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(veto,/tname) ne 'UINT' then goto, bad
if size(weight,/tname) ne 'UINT' then goto, bad

if size(j_mpa,/tname) ne 'LONG' then goto, bad
if size(ibranch,/tname) ne 'LONG' then goto, bad
if size(n_buffer,/tname) ne 'LONG' then goto, bad
if size(n_actual,/tname) ne 'LONG' then goto, bad
if size(n,/tname) ne 'LONG' then goto, bad
if size(n_max,/tname) ne 'LONG' then goto, bad
if size(bad_xy,/tname) ne 'LONG' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'mpa4_events'), cdecl=geolib_cdecl(), $
			ev, n_buffer,n_actual, e,x,y,ste, veto,weight, n,n_max, j_mpa,ibranch, bad_xy, $
			value=value )

return, err

bad:
	print,' mpa4_events: Error - bad type for argument.'
	err = 1L
	return, err
end
