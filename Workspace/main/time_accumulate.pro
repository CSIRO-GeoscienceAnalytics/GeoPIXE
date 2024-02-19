function time_accumulate, e,x,stn,n, time_stn, spec,ns,found,nq
;
;  This assumes spec is:	lonarr(ns,nq)
;				found		lonarr(nq)

COMPILE_OPT STRICTARR

value = bytarr(9)
value[*] = 0			; pass all by reference
err = 0L

if size(spec,/tname) ne 'LONG' then goto, bad
if size(found,/tname) ne 'LONG' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(stn,/tname) ne 'UINT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'time_accumulate'), cdecl=geolib_cdecl(), $
			e,x,stn,long(n), long(time_stn), spec,long(ns),found, $
			long(nq), value=value )

return, err

bad:
	print,' time_accumulate: Error - bad type for argument.'
	err = 1
	return, err
end
