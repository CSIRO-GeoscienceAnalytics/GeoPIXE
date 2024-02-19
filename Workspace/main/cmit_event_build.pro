function cmit_event_build, ste, e,x,y,multiple,n_buffer, out, n_out_max, n_out, $
					x0,y0, time=time, throttle=throttle, tbuff=tbuff, maia=maia

;  Inputs:
;				ste,x,y,e:	uintarr(n_buffer)
;				multiple:	lonarr(n_buffer) (optional or -1)
;
;				n_out		long
;				time		uintarr(n_buffer) (optional)
;				throttle	uintarr(nt)		(optional)
;				tbuff		uintarr(nt)		(optional)
; Outputs:
;				x0,y0		int
;
;				n_out		long	
;	maia mode	out			bytarr(n_out_max)
;	non maia	out			uintarr(4,n_out_max)
;	with time	out			uintarr(5,n_out_max)
;

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
		warning,'cmit_event_build',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1L
	endif
endif
if n_elements(maia) lt 1 then maia=0
if n_elements(multiple) lt 1 then multiple=-1L
do_time = 0L
n_out_dim = 4L
if n_elements(time) eq n_buffer then begin
	do_time = 1L
	n_out_dim = 5L
endif
if n_elements(throttle) lt 1 then begin
	throttle = replicate(1US, 4096)
	tbuff = throttle
endif
nt = n_elements(throttle)
if n_elements(tbuff) ne nt then goto, bad

value = bytarr(17)
value[*] = 0					; pass all by reference
err = 0L

if size(ste,/tname) ne 'UINT' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(throttle,/tname) ne 'UINT' then goto, bad
if size(tbuff,/tname) ne 'UINT' then goto, bad
if size(x0,/tname) ne 'INT' then goto, bad
if size(y0,/tname) ne 'INT' then goto, bad
if n_elements(time) gt 0 then begin
	if size(time,/tname) ne 'UINT' then goto, bad
endif else time = uint(0)
if size(multiple,/tname) ne 'LONG' then goto, bad
if maia then begin
	if size(out,/tname) ne 'BYTE' then goto, bad
endif else begin
	if size(out,/tname) ne 'UINT' then goto, bad
endelse

if maia then begin
	err = call_external( geopixe_library(), geolib_name( 'cmit_event_maia'), cdecl=geolib_cdecl(), $
		ste, e,time, x,y, multiple,long(n_buffer), do_time, out, $
		long(n_out_max),n_out_dim, n_out, x0,y0, value=value )
endif else begin
	err = call_external( geopixe_library(), geolib_name( 'cmit_event_build'), cdecl=geolib_cdecl(), $
		ste, e,time, x,y, multiple,long(n_buffer), do_time, out, $
		long(n_out_max),n_out_dim, n_out, $
		throttle, tbuff, nt, value=value[0:14] )
endelse

return, err

bad:
	print,'cmit_event_build: Error - bad type for argument.'
	err = 1L
	return, err
end
