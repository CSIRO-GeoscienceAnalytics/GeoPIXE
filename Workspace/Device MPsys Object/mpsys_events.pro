function mpsys_events, ev,n_buffer,channel_on,nc, e,x,y,n,ste, unknown, $
				step_toggle,step_events,step_count, toggle_adc,toggle_bit, $
				ystep=ystep
;
;  This assumes ev is:		uintarr(3,n_buffer)
;				x,y,e,ste:	uintarr(n_buffer)
;				channel_on	intarr(nc)
;				-all else-	long
;
;	ystep = 0	X step mode
;			1	Y step mode
;
;	for X,Y stepping:
;				xstep =		1
;				xmax		maximum X (or Y)
;				toggle_adc	ADC with toggle bit
;				toggle_bit	bit #
;
;	in common:
;				xlast		current/last X (or Y)	(must be UINT)
;				toggle_last	last one				(must be UINT)
;				xcount		cummulative xcounts		(must be LONG)

COMPILE_OPT STRICTARR
common c_mpsys_5c, xlast,xmax,xcount, toggle_last
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
		warning,'mpsys_events',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif
if n_elements(ystep) lt 1 then ystep = 0L

value = bytarr(19)
value[*] = 0					; pass all by reference
err = 0L
n = 0L

if size(ev,/tname) ne 'UINT' then goto, bad
if size(ste,/tname) ne 'UINT' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(unknown,/tname) ne 'LONG' then goto, bad
if size(xlast,/tname) ne 'UINT' then goto, bad
if size(toggle_last,/tname) ne 'UINT' then goto, bad
if size(xcount,/tname) ne 'LONG' then goto, bad
if size(channel_on,/tname) ne 'INT' then goto, bad

; In mpsys_events routine:
;
; ystep = 	0	X step mode
;			1	Y step mode
;
; xstep =	0	normal XY scan
;			1	X,Y step mode using toggle bit using /step_toggle
;			2	X,Y step using step_count in toggle_adc
;			3	X,Y step using /step_events (step_count in PIXE ADC)
;
; xlast		cummulative X (or Y) position
; xcount	cummulative X (or Y) count

if step_toggle eq 1 then begin
	xstep = 1
endif else begin
	if step_events eq 1 then begin
		xstep = 3
	endif else if step_count gt 0 then begin
		xstep = 2
	endif else begin
		xstep = 0
	endelse
endelse

err = call_external( geopixe_library(), geolib_name( 'mpsys_events'), cdecl=geolib_cdecl(), $
			ev,long(n_buffer), channel_on,long(nc), e,x,y,n,ste,  $
			unknown, long(xstep),xlast,long(xmax),long(step_count), xcount, $
			long(toggle_adc), long(toggle_bit),toggle_last, long(ystep), $
			value=value )

return, err

bad:
	print,' mpsys_events: Error - bad type for argument.'
	err = 1L
	return, err
end
