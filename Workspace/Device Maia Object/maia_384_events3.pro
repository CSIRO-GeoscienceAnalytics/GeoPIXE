function maia_384_events3, ev,n_buffer, channel_on,nc, e,t,x,y,n,ste,tags, $
			ibranch,swap, tag,length,skip, x0,y0, monitor,n_monitor,nm, $
			time,beam,flux,n_times,nt, bad,debug, time_sec=time_sec, $
			energy_string=energy_string, flux_string=flux_string, $
			index=index, y_mode=y_mode, xmargin=xmargin, width=width, height=height, $
			fc1=fc1, fc2=fc2, dwell=dwell, tt=tt
;
;  Called from data file processing to decode ET records.
;  (don't confuse this with maia_et2_events used for background Maia processes)
;
;;  This assumes ev 			bytarr(n_buffer)
;				x,y,e,t,ste,tags	uintarr(n_buffer)
;				channel_on	intarr(nc)
;				monitor		bytarr(n_monitor)
;				time		double(n_times) - return only
;				ibranch		int		internal branch computed goto (internal)
;				tag			int		block tag (internal)
;				length		int		block payload data length (internal)
;				skip		int		skip long words in data (internal)
;				x0,y0		int		current X,Y pixel position (internal)
;				-all else-	long
;
;				energy_string  		key part of energy PV string (case sensitive)
;				flux_string			flux PV string
;
;				monitor		returns all monitor event strings
;				nm			number of characters in monitor
;				
;				time		returns time (double) of monitor events
;				beam		returns beam energy monitor values found
;				flux		returns selected IC values found
;				nt			number of monitor events (number of 'time' values)
;				n_times		maximum length of time, beam, flux
;				
;				index		cross-reference index index[x,dy]=nt index
;							(e.g. flux @ 'x' is flux[index[x,dy]] with Y offset by initial y0)
;				dwell		dwell partial map (x,dy) (ms)
;				fc1			Flux 1  partial map (x,dy)
;				fc2			Flux 2  partial map (x,dy)
;				tt			TT sum  partial map (x,dy)
;
;				y_mode		1 new Y mode, 0 old PA advance
;				xmargin		width of X margin for Y advances
;				width		width of X scan (dims for index)
;				height		height of Y scan (dims for index)

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
		warning,'maia_384_events3',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

time_sec = lonarr(n_times)
time_micro = lonarr(n_times)
beam = fltarr(n_times)
flux = fltarr(n_times)

if n_elements(y_mode) lt 1 then y_mode = 1
if n_elements(xmargin) lt 1 then xmargin = 10
if n_elements(width) lt 1 then width = 1000
if n_elements(height) lt 1 then height = 1000

value = bytarr(42)
value[*] = 0					; pass all by reference
err = 0L
n = 0L

if size(ev,/tname) ne 'BYTE' then goto, bad
if size(channel_on,/tname) ne 'INT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(t,/tname) ne 'UINT' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(n,/tname) ne 'LONG' then goto, bad
if size(ste,/tname) ne 'UINT' then goto, bad
if size(tags,/tname) ne 'UINT' then goto, bad
if size(ibranch,/tname) ne 'INT' then goto, bad
if size(tag,/tname) ne 'INT' then goto, bad
if size(length,/tname) ne 'LONG' then goto, bad
if size(skip,/tname) ne 'INT' then goto, bad
if size(y0,/tname) ne 'INT' then goto, bad
if size(y0,/tname) ne 'INT' then goto, bad
if size(bad,/tname) ne 'LONG' then goto, bad
if size(debug,/tname) ne 'LONG' then goto, bad
if size(monitor,/tname) ne 'BYTE' then goto, bad
if size(nm,/tname) ne 'LONG' then goto, bad
if size(nt,/tname) ne 'LONG' then goto, bad
if size(index,/tname) ne 'LONG' then goto, bad
if n_elements(energy_string) ge 1 then begin
	energyb = byte(energy_string)
	nb = n_elements(energyb)
endif else begin
	energyb = 0B
	nb = 0
endelse
if n_elements(flux_string) ge 1 then begin
	fluxb = byte(flux_string)
	nf = n_elements(fluxb)
endif else begin
	fluxb = 0B
	nf = 0
endelse
if size(fc1,/tname) ne 'LONG' then goto, bad
if size(fc2,/tname) ne 'LONG' then goto, bad
if size(dwell,/tname) ne 'FLOAT' then goto, bad
if size(tt,/tname) ne 'FLOAT' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'maia_384_events3'), cdecl=geolib_cdecl(), $
			ev,long(n_buffer), channel_on,long(nc), e,t,x,y,n,ste,tags, $
			ibranch, long(swap), tag,length,skip, x0,y0, monitor,long(n_monitor),nm, $
			time_sec,time_micro,beam,flux,n_times,nt, bad,debug, $
			energyb, nb, fluxb, nf, index, $
			long(y_mode), long(xmargin), long(width), long(height), $
			fc1, fc2, dwell, tt, value=value )

if err then begin
	print, 'maia_384_events3 error'
endif
if err eq 0 then begin
	time = double(time_sec) + double(time_micro)/1000000.
endif
if err then print,'Err: tag, length, ibranch, time =',tag, length, ibranch, time_sec[0]
;q = where( (n_record gt 1000L) and (time_sec - 1266481679L gt 100000L), nq)
;if nq gt 0 then begin
;	print, 'debug'
;endif
return, err

bad:
	print,'maia_384_events3: Error - bad type for argument.'
	err = 1L
	return, err
end
