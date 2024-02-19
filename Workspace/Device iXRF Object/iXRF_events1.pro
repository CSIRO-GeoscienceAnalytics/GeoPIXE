function iXRF_events1, ev,n_buffer, channel_on,nc, e,count,x,y,ste,veto,tags,n_events,n, $
			fx,n_fx, x0,y0, ibranch,swap, tag,length,skip, bad,debug
;
;  Called from data file processing to decode ET records.
;  (don't confuse this with maia_et2_events used for background Maia processes)
;  Now uses long 'skip' for records longer than 32K.
;
;  This assumes ev 			bytarr(n_buffer)
;				e,count,ste	E,count,n for events uintarr(n_events)
;				x,y,z		X,Y,Z for events intarr(n_events) - accepts negatives
;				veto,tags	veto (=1) to suppress uintarr(n_events)
;				fx			float(4,n_events) [selected flux,FC1,FC2,Dwell]
;				n			long		returned number of events
;				channel_on	intarr(nc)
;
;				ibranch		int		internal branch computed goto (internal)
;				tag			int		block tag (internal)
;				length		long	block payload data length (internal)
;				skip		long	skip long words in data (internal)
;				x0,y0		int		current X,Y pixel position (internal)

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
		warning,'iXRF_events1',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

value = bytarr(24)
value[*] = 0					; pass all by reference
err = 0L
n = 0L

if size(ev,/tname) ne 'BYTE' then begin sb='ev' & goto, bad & endif
if size(channel_on,/tname) ne 'INT' then begin sb='channel_on' & goto, bad & endif
if size(e,/tname) ne 'UINT' then begin sb='e' & goto, bad & endif
if size(count,/tname) ne 'UINT' then begin sb='t' & goto, bad & endif
if size(x,/tname) ne 'INT' then begin sb='x' & goto, bad & endif
if size(y,/tname) ne 'INT' then begin sb='y' & goto, bad & endif
if size(ste,/tname) ne 'UINT' then begin sb='ste' & goto, bad & endif
if size(veto,/tname) ne 'UINT' then begin sb='veto' & goto, bad & endif
if size(tags,/tname) ne 'UINT' then begin sb='tags' & goto, bad & endif
if size(n,/tname) ne 'LONG' then begin sb='n' & goto, bad & endif
if size(fx,/tname) ne 'FLOAT' then begin sb='fx' & goto, bad & endif
if n_elements(fx[*,0]) ne 5 then begin sb='fx size' & goto, bad & endif
if size(x0,/tname) ne 'INT' then begin sb='x0' & goto, bad & endif
if size(y0,/tname) ne 'INT' then begin sb='y0' & goto, bad & endif
if size(ibranch,/tname) ne 'INT' then begin sb='ibranch' & goto, bad & endif
if size(tag,/tname) ne 'INT' then begin sb='tag' & goto, bad & endif
if size(length,/tname) ne 'LONG' then begin sb='length' & goto, bad & endif
if size(skip,/tname) ne 'LONG' then begin sb='skip' & goto, bad & endif
if size(bad,/tname) ne 'LONG' then begin sb='bad' & goto, bad & endif
if size(debug,/tname) ne 'LONG' then begin sb='debug' & goto, bad & endif

err = call_external( geopixe_library(), geolib_name( 'ixrf_events1'), cdecl=geolib_cdecl(), $
			ev,long(n_buffer), channel_on,long(nc), e,count,x,y,ste,veto,tags,long(n_events),n, $
			fx,long(n_fx), x0,y0, ibranch, long(swap), tag,length,skip, bad,debug, $ 
			value=value )

if err then begin
	print, 'iXRF_events1 Fortran error, too few arguments'
	print,'		tag, length, ibranch, skip =',tag, length, ibranch, skip
endif
if bad then begin
	print, 'iXRF_events1 Fortran error, BAD flagged (negative skip).'
	print,'		tag, length, ibranch, skip =',tag, length, ibranch, skip
	print,'		format error at byte pointer (debug) offset past JSON header =',debug
	err = 1L
endif
return, err

bad:
	gprint,'iXRF_events1: Error - bad type for argument: '+sb
	err = 1L
	return, err
end
