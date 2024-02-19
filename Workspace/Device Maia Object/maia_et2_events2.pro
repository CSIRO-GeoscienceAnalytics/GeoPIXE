function maia_et2_events2, ev,n_buffer, channel_on,nc, e,t,x,y,z,n,ste, swap, x0,y0,z0, tag=tag
;
;  Called from background Maia processes to decode ET records.
;  (don't confuse this with maia_384_events used for data file processing)
;  
;  This assumes ev 				bytarr(n_buffer)
;				x,y,z,e,t,ste	uintarr(n_buffer)
;				channel_on		intarr(nc)
;				x0,y0,z0		int		current X,Y,Z pixel position (internal)
;				-all else-		long
;		tag		ET event tag index (0=ET2, 1=ET3, 2=ET4)

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
		warning,'maia_et2_events2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif
if n_elements(tag) lt 1 then tag=2

value = bytarr(16)
value[*] = 0					; pass all by reference
err = 0L
n = 0L

if size(ev,/tname) ne 'BYTE' then begin sb='ev' & goto, bad & endif
if size(channel_on,/tname) ne 'INT' then begin sb='channel_on' & goto, bad & endif
if size(e,/tname) ne 'UINT' then begin sb='e' & goto, bad & endif
if size(t,/tname) ne 'UINT' then begin sb='t' & goto, bad & endif
if size(x,/tname) ne 'UINT' then begin sb='x' & goto, bad & endif
if size(y,/tname) ne 'UINT' then begin sb='y' & goto, bad & endif
if size(z,/tname) ne 'UINT' then begin sb='z' & goto, bad & endif
if size(n,/tname) ne 'LONG' then begin sb='n' & goto, bad & endif
if size(ste,/tname) ne 'UINT' then begin sb='ste' & goto, bad & endif
if size(y0,/tname) ne 'INT' then begin sb='x0' & goto, bad & endif
if size(y0,/tname) ne 'INT' then begin sb='y0' & goto, bad & endif
if size(z0,/tname) ne 'INT' then begin sb='z0' & goto, bad & endif

err = call_external( geopixe_library(), geolib_name( 'maia_et2_events2'), cdecl=geolib_cdecl(), $
			ev,long(n_buffer), channel_on,long(nc), e,t,x,y,z,n,ste, $
			long(swap), x0,y0,z0, long(tag), value=value )

return, err

bad:
	gprint,'maia_et2_events2: Error - bad type for argument: '+sb
	err = 1L
	return, err
end
