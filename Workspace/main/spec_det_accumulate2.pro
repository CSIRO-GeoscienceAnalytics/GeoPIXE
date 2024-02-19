function spec_det_accumulate2, e,x,y,ste,pu,n, spec,ns,nd,found, nnpu,nn, mask,nx,ny, $
								multiple=multiple
;
;  This assumes spec is:	lonarr(ns,nd)
;				found		lonarr(nd)
;				nnpu, nn	lonarr(nd)
;				mask:		bytarr(nx,ny)
;				x,y,ste,pu,e:	uintarr(n)
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
		warning,'Spec_det_accumulate2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif
if n_elements(multiple) lt 1 then multiple=-1L

value = bytarr(16)
value[*] = 0					; pass all by reference
err = 0L

if size(spec,/tname) ne 'LONG' then goto, bad
if size(found,/tname) ne 'LONG' then goto, bad
if size(mask,/tname) ne 'BYTE' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(ste,/tname) ne 'UINT' then goto, bad
if size(pu,/tname) ne 'UINT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(multiple,/tname) ne 'LONG' then goto, bad
if size(nnpu,/tname) ne 'LONG' then goto, bad
if size(nn,/tname) ne 'LONG' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'spec_det_accumulate2'), cdecl=geolib_cdecl(), $
			e,x,y,ste,pu,long(n), spec,long(ns),long(nd),found, nnpu,nn, $
			mask,long(nx),long(ny), multiple, value=value )

return, err

bad:
	print,' spec_det_accumulate2: Error - bad type for argument.'
	err = 1L
	return, err
end
