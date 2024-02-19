function low_stats_filter, spec1,spec2, n_spec, low,high, AF,BF

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
		warning,'low_stats_filter',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

value = bytarr(7)
value[*] = 0						; pass all by reference
err = 0L

if size(spec1,/tname) ne 'FLOAT' then goto, bad
if size(spec2,/tname) ne 'FLOAT' then goto, bad
if size(AF,/tname) ne 'FLOAT' then goto, bad
if size(BF,/tname) ne 'FLOAT' then goto, bad
if n_spec le 2 then goto, bad

err = call_external( geopixe_library(), geolib_name( 'low_stats_filter'), cdecl=geolib_cdecl(), $
			spec1,spec2, long(n_spec), long(low),long(high), AF,BF, $
			value=value )

return, err

bad:
	print,'low_stats_filter: Error - bad type for argument.'
	err = 1L
	return, err
end
