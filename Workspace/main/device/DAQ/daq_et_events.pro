function daq_et_events, ev,n_buffer, channel_on,nc, e,t,x,y,time,n,ste, swap, x0,y0, tag3=tag3
;
;  Called from background DAQ processes to decode ET records.
;  (don't confuse this with daq_36_events used for data file processing)
;  
;  This assumes ev 				bytarr(n_buffer)
;				x,y,e,t,ste		uintarr(n_buffer)
;				time			ulonarr(n_buffer)
;				channel_on		intarr(nc)
;				x0,y0			int		current X,Y pixel position (internal)
;				-all else-		long
;		tag3	process a DAQ ET event [0=with, 1=without time-stamps]

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
		warning,'daq_et_events',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif
if n_elements(tag3) lt 1 then tag3=0

value = bytarr(15)
value[*] = 0					; pass all by reference
err = 0L
n = 0L

if size(ev,/tname) ne 'BYTE' then goto, bad
if size(channel_on,/tname) ne 'INT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(t,/tname) ne 'UINT' then goto, bad
if size(time,/tname) ne 'ULONG' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(n,/tname) ne 'LONG' then goto, bad
if size(ste,/tname) ne 'UINT' then goto, bad
if size(y0,/tname) ne 'INT' then goto, bad
if size(y0,/tname) ne 'INT' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'daq_et_events'), cdecl=geolib_cdecl(), $
			ev,long(n_buffer), channel_on,long(nc), e,t,x,y,time,n,ste, $
			long(swap), x0,y0, long(tag3), value=value )

return, err

bad:
	print,' daq_et_events: Error - bad type for argument.'
	err = 1L
	return, err
end
