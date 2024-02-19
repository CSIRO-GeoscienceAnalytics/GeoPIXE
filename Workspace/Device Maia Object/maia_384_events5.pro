function maia_384_events5, ev,n_buffer, channel_on,nc, e,t,x,y,ste,veto,tags,n_events,n, $
			fx,n_fx, flux_mode, x0,y0, monitor,n_monitor,nm, $
			energy_string, beam_energy, flux_string, flux_pv, time_pv, time_first, $
			y_mode,xmargin,width, ibranch,swap, tag,length,skip, bad,debug
;
;  Called from data file processing to decode ET records.
;  (don't confuse this with maia_et2_events used for background Maia processes)
;  Now uses long 'skip' for records longer than 32K.
;
;  This assumes ev 			bytarr(n_buffer)
;				e,t,x,y,ste	E,T,X,Y,n for events uintarr(n_events)
;				veto,tags	veto (=1) to suppress uintarr(n_events)
;				fx			float(4,n_events) [selected flux,FC1,FC2,Dwell]
;				n			long		returned number of events
;				channel_on	intarr(nc)
;				monitor		bytarr(n_monitor)
;
;				flux_mode			0=EpicsPV, 1=FC1, 2=FC2
;				energy_string  		key part of energy PV string (case sensitive)
;				flux_string			flux PV string (case sensitive)
;				beam_energy	float	current beam energy found
;				flux_pv		float	current Epics flux PV found
;				time_pv		double	timestamp for flux_pv
;				time_first	double	first timestamp in buffer
;				
;				monitor		returns all monitor event strings
;				nm			number of characters in monitor
;				
;				y_mode		1 new Y encoder mode, 0 normal PA advance
;				xmargin		width of X margin for Y encoder mode
;				width		width of scan for Y encoder mode
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
		warning,'maia_384_events5',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

value = bytarr(39)
value[*] = 0					; pass all by reference
err = 0L
n = 0L

if size(ev,/tname) ne 'BYTE' then begin sb='ev' & goto, bad & endif
if size(channel_on,/tname) ne 'INT' then begin sb='channel_on' & goto, bad & endif
if size(e,/tname) ne 'UINT' then begin sb='e' & goto, bad & endif
if size(t,/tname) ne 'UINT' then begin sb='t' & goto, bad & endif
if size(x,/tname) ne 'UINT' then begin sb='x' & goto, bad & endif
if size(y,/tname) ne 'UINT' then begin sb='y' & goto, bad & endif
if size(ste,/tname) ne 'UINT' then begin sb='ste' & goto, bad & endif
if size(veto,/tname) ne 'UINT' then begin sb='veto' & goto, bad & endif
if size(tags,/tname) ne 'UINT' then begin sb='tags' & goto, bad & endif
if size(n,/tname) ne 'LONG' then begin sb='n' & goto, bad & endif
if size(fx,/tname) ne 'FLOAT' then begin sb='fx' & goto, bad & endif
if n_elements(fx[*,0]) ne 4 then begin sb='fx size' & goto, bad & endif
if size(x0,/tname) ne 'INT' then begin sb='x0' & goto, bad & endif
if size(y0,/tname) ne 'INT' then begin sb='y0' & goto, bad & endif
if size(monitor,/tname) ne 'BYTE' then begin sb='monitor' & goto, bad & endif
if size(beam_energy,/tname) ne 'FLOAT' then begin sb='beam_energy' & goto, bad & endif
if size(flux_pv,/tname) ne 'FLOAT' then begin sb='flux_pv' & goto, bad & endif
if size(time_pv,/tname) ne 'DOUBLE' then begin sb='time_pv' & goto, bad & endif
if size(time_first,/tname) ne 'DOUBLE' then begin sb='time_first' & goto, bad & endif
if size(nm,/tname) ne 'LONG' then begin sb='nm' & goto, bad & endif
if size(ibranch,/tname) ne 'INT' then begin sb='ibranch' & goto, bad & endif
if size(tag,/tname) ne 'INT' then begin sb='tag' & goto, bad & endif
if size(length,/tname) ne 'LONG' then begin sb='length' & goto, bad & endif
if size(skip,/tname) ne 'LONG' then begin sb='skip' & goto, bad & endif
if size(bad,/tname) ne 'LONG' then begin sb='bad' & goto, bad & endif
if size(debug,/tname) ne 'LONG' then begin sb='debug' & goto, bad & endif

if n_elements(energy_string) ge 1 then begin
	energyb = byte(energy_string)
	nb = n_elements(energyb)
endif else begin
	energyb = 0B
	nb = 0L
endelse
if n_elements(flux_string) ge 1 then begin
	fluxb = byte(flux_string)
	nf = n_elements(fluxb)
endif else begin
	fluxb = 0B
	nf = 0L
endelse
time_first = 0.0D+0

err = call_external( geopixe_library(), geolib_name( 'maia_384_events5'), cdecl=geolib_cdecl(), $
			ev,long(n_buffer), channel_on,long(nc), e,t,x,y,ste,veto,tags,long(n_events),n, $
			fx,long(n_fx), long(flux_mode), x0,y0, monitor,long(n_monitor),nm, $
			energyb,nb, beam_energy, fluxb,nf, flux_pv,time_pv, time_first, $
			long(y_mode),long(xmargin),long(width), ibranch, long(swap), tag,length,skip, bad,debug, $ 
			value=value )

if err then begin
	print, 'maia_384_events5 Fortran error'
	print,'		tag, length, ibranch, skip =',tag, length, ibranch, skip
endif
return, err

bad:
	gprint,'maia_384_events5: Error - bad type for argument: '+sb
	err = 1L
	return, err
end
