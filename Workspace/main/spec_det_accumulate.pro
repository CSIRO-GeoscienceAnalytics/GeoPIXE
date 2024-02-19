function spec_det_accumulate, e,x,y,ste,n, spec,ns,nd,found, mask,nx,ny, $
								multiple=multiple
;
;  This assumes spec is:	lonarr(ns,nq)
;				found		lonarr(nq)
;				mask:		bytarr(nx,ny,nq)
;				x,y,ste,e:	uintarr(n)
;				multiple	lonarr(n) or -1L

COMPILE_OPT STRICTARR
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
		warning,'Spec_det_accumulate',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif
if n_elements(multiple) lt 1 then multiple=-1L

value = bytarr(13)
value[*] = 0					; pass all by reference
err = 0L

if size(spec,/tname) ne 'LONG' then goto, bad
if size(found,/tname) ne 'LONG' then goto, bad
if size(mask,/tname) ne 'BYTE' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(ste,/tname) ne 'UINT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(multiple,/tname) ne 'LONG' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'spec_det_accumulate'), cdecl=geolib_cdecl(), $
			e,x,y,ste,long(n), spec,long(ns),long(nd),found, $
			mask,long(nx),long(ny), multiple, value=value )

return, err

bad:
	print,' spec_det_accumulate: Error - bad type for argument.'
	err = 1L
	return, err
end
