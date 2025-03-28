function falconx_events2, ev,n_buffer, channel_on,nc, e,t,x,y,z,u,v,w,ste,veto,tags,n_events,n, $
			fx,n_fx, flux_mode, x0,y0,z0,u0,v0,w0, ibranch,swap, tag,length,skip, bad,debug
;
;  Called from data file processing to decode ET records.
;  (don't confuse this with ???_et_events used for background Fx processes)
;  Now uses long 'skip' for records longer than 32K.
;
;  This assumes ev 			bytarr(n_buffer)
;				e,t,ste		E,T,n for events uintarr(n_events)
;				x,y,z,u,v,w		X,Y,Z,U,V,W for events intarr(n_events) - accepts negatives
;				veto,tags	veto (=1) to suppress uintarr(n_events)
;				fx			float(5,n_events) [selected flux,FC1,FC2,Dwell,DT]
;				flux_mode	flux selection (0=PV (not used?), 1,2=FC0,1)
;				n			long		returned number of events
;				channel_on	intarr(nc)
;
;				ibranch		int		internal branch computed goto (internal)
;				tag			int		block tag (internal)
;				length		long	block payload data length (internal)
;				skip		long	skip long words in data (internal)
;				x0,y0,z0,u0,v0,w0	int		current X,Y,Z,U,V,W pixel position (internal)

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
		warning,'falconx_events2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

value = bytarr(33)
value[*] = 0					; pass all by reference
err = 0L
n = 0L

if size(ev,/tname) ne 'BYTE' then begin sb='ev' & goto, bad & endif
if size(channel_on,/tname) ne 'INT' then begin sb='channel_on' & goto, bad & endif
if size(e,/tname) ne 'UINT' then begin sb='e' & goto, bad & endif
if size(t,/tname) ne 'UINT' then begin sb='t' & goto, bad & endif
if size(x,/tname) ne 'INT' then begin sb='x' & goto, bad & endif
if size(y,/tname) ne 'INT' then begin sb='y' & goto, bad & endif
if size(z,/tname) ne 'INT' then begin sb='z' & goto, bad & endif
if size(u,/tname) ne 'INT' then begin sb='u' & goto, bad & endif
if size(v,/tname) ne 'INT' then begin sb='v' & goto, bad & endif
if size(w,/tname) ne 'INT' then begin sb='w' & goto, bad & endif
if size(ste,/tname) ne 'UINT' then begin sb='ste' & goto, bad & endif
if size(veto,/tname) ne 'UINT' then begin sb='veto' & goto, bad & endif
if size(tags,/tname) ne 'UINT' then begin sb='tags' & goto, bad & endif
if size(n,/tname) ne 'LONG' then begin sb='n' & goto, bad & endif
if size(fx,/tname) ne 'FLOAT' then begin sb='fx' & goto, bad & endif
if n_elements(fx[*,0]) ne 5 then begin sb='fx size' & goto, bad & endif
if size(x0,/tname) ne 'INT' then begin sb='x0' & goto, bad & endif
if size(y0,/tname) ne 'INT' then begin sb='y0' & goto, bad & endif
if size(z0,/tname) ne 'INT' then begin sb='z0' & goto, bad & endif
if size(u0,/tname) ne 'INT' then begin sb='u0' & goto, bad & endif
if size(v0,/tname) ne 'INT' then begin sb='v0' & goto, bad & endif
if size(w0,/tname) ne 'INT' then begin sb='w0' & goto, bad & endif
if size(ibranch,/tname) ne 'INT' then begin sb='ibranch' & goto, bad & endif
if size(tag,/tname) ne 'INT' then begin sb='tag' & goto, bad & endif
if size(length,/tname) ne 'LONG' then begin sb='length' & goto, bad & endif
if size(skip,/tname) ne 'LONG' then begin sb='skip' & goto, bad & endif
if size(bad,/tname) ne 'LONG' then begin sb='bad' & goto, bad & endif
if size(debug,/tname) ne 'LONG' then begin sb='debug' & goto, bad & endif

err = call_external( geopixe_library(), geolib_name( 'falconx_events2'), cdecl=geolib_cdecl(), $
			ev,long(n_buffer), channel_on,long(nc), e,t,x,y,z,u,v,w,ste,veto,tags,long(n_events),n, $
			fx,long(n_fx), long(flux_mode), x0,y0,z0,u0,v0,w0, ibranch, long(swap), tag,length,skip, bad,debug, $ 
			value=value )

if err then begin
	print, 'falconx_events2 Fortran error'
	print,'		tag, length, ibranch, skip =',tag, length, ibranch, skip
endif
return, err

bad:
	gprint,'falconx_events2: Error - bad type for argument: '+sb
	err = 1L
	return, err
end
