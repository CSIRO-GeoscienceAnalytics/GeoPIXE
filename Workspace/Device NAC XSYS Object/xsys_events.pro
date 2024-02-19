function xsys_events, ev,i,n,n_buffer, channel_on,nc,e,x,y,ste,  $
				run_num, title_start,title_end, unknown
;
;  This assumes ev is:		uintarr(n_buffer)
;				x,y,e,ste:	uintarr(n_buffer)
;				channel_on	intarr(nc)
;				-all else-	long

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
		warning,'xsys_events',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

value = bytarr(14)
value[*] = 0					; pass all by reference
err = 0L

if size(ev,/tname) ne 'UINT' then goto, bad
if size(ste,/tname) ne 'UINT' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(i,/tname) ne 'LONG' then goto, bad
if size(n,/tname) ne 'LONG' then goto, bad
if size(run_num,/tname) ne 'LONG' then goto, bad
if size(title_start,/tname) ne 'LONG' then goto, bad
if size(title_end,/tname) ne 'LONG' then goto, bad
if size(unknown,/tname) ne 'LONG' then goto, bad
if size(channel_on,/tname) ne 'INT' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'xsys_events'), cdecl=geolib_cdecl(), $
			ev,i,n,long(n_buffer), channel_on,long(nc),e,x,y,ste,  $
			run_num, title_start,title_end, unknown, value=value )

return, err

bad:
print,' xsys_events: Error - bad type for argument.'
err = 1L
return, err
end
