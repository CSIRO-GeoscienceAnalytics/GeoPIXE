function falconx_events4, ev,n_buffer, channel_on,nc, e,t,x,y,z,u,v,w,ste,veto,tags,n_events,n, $
			fx,n_fx, flux_mode, swap_icr_raw, x0,y0,z0,u0,v0,w0, ibranch,swap, tag,length,skip, bad,debug, version1,tick1
;
;  Called from data file processing to decode ET records.
;  (don't confuse this with ???_et_events used for background Fx processes)
;  Now uses long 'skip' for records longer than 32K.
;
;  This assumes ev 			bytarr(n_buffer)
;				e,t,ste		E,T,n for events uintarr(n_events)
;				x,y,z,u,v,w		X,Y,Z,U,V,W for events intarr(n_events) - accepts negatives
;				veto,tags	veto (=1) to suppress uintarr(n_events)
;				fx			float(6,n_events) [selected flux,FC1,FC2,Dwell,DT,raw]
;				flux_mode	flux selection (0=PV (not used?), 1,2,3,4=FC0,1,2,3)
;				swap_icr_raw enable smart swap of ICR and RAW if RAW found to be larger.
;				n			long		returned number of events
;				channel_on	intarr(nc)
;				version		long	FalconX version number
;				tick		float	clock time (ms)  (default to tick from header read)
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
common c_falconx_20, version, tick
if n_elements(version) lt 1 then version=0
if n_elements(tick) eq 0 then tick = 4.0e-6
if n_elements(version1) eq 1 then version=version1
if n_elements(tick1) eq 1 then tick=tick1

if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'falconx_events4',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

value = bytarr(36)
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
if n_elements(fx[*,0]) ne 6 then begin sb='fx size' & goto, bad & endif
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
if size(version,/tname) ne 'LONG' then begin sb='version' & goto, bad & endif
if size(tick,/tname) ne 'FLOAT' then begin sb='tick' & goto, bad & endif

err = call_external( geopixe_library(), geolib_name( 'falconx_events4'), cdecl=geolib_cdecl(), $
			ev,long(n_buffer), channel_on,long(nc), e,t,x,y,z,u,v,w,ste,veto,tags,long(n_events),n, $
			fx,long(n_fx), long(flux_mode), long(swap_icr_raw), x0,y0,z0,u0,v0,w0, ibranch, long(swap), tag,length,skip, bad,debug, $ 
			version, tick, value=value )

if err then begin
	print, 'falconx_events4 Fortran error'
	print,'		tag, length, ibranch, skip =',tag, length, ibranch, skip
endif
return, err

bad:
	gprint,'falconx_events4: Error - bad type for argument: '+sb
	err = 1L
	return, err
end
