function tohoku_events, ev,j_mpa,ibranch,n_buffer,n_actual, $
				e,x,y,ste, n,n_max, mpa_x_adc,mpa_y_adc, $
				word, ADCindex, ADCpntr, adc,tag, k_adc, adcs, bad_xy

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
		warning,'tohoku_events',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

value = bytarr(21)
value[*] = 0					; pass all by reference
err = 0L
n = 0L

if size(ev,/tname) ne 'UINT' then goto, bad
if size(ste,/tname) ne 'UINT' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad

if size(word,/tname) ne 'UINT' then goto, bad
if size(ADCindex,/tname) ne 'UINT' then goto, bad
if size(ADCpntr,/tname) ne 'UINT' then goto, bad
if size(adc,/tname) ne 'UINT' then goto, bad
if size(tag,/tname) ne 'UINT' then goto, bad

if size(k_adc,/tname) ne 'LONG' then goto, bad
if size(j_mpa,/tname) ne 'LONG' then goto, bad
if size(ibranch,/tname) ne 'LONG' then goto, bad
if size(n_buffer,/tname) ne 'LONG' then goto, bad
if size(n_actual,/tname) ne 'LONG' then goto, bad
if size(n,/tname) ne 'LONG' then goto, bad
if size(n_max,/tname) ne 'LONG' then goto, bad
if size(adcs,/tname) ne 'LONG' then goto, bad
if size(bad_xy,/tname) ne 'LONG' then goto, bad
if size(mpa_x_adc,/tname) ne 'INT' then goto, bad
if size(mpa_y_adc,/tname) ne 'INT' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'tohoku_events'), cdecl=geolib_cdecl(), $
			ev,j_mpa,ibranch,n_buffer,n_actual, $
			e,x,y,ste, n,n_max, mpa_x_adc,mpa_y_adc, $
			word, ADCindex, ADCpntr, adc,tag, k_adc, adcs, bad_xy, $
			value=value )

return, err

bad:
	print,' tohoku_events: Error - bad type for argument.'
	err = 1L
	return, err
end
