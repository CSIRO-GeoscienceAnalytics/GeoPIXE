function spec_accumulate4, e,x,y,pu,veto,n, spec,ns,found, mask,nq,nx,ny,yoff, $
								multiple=multiple, master=master
;
;  This assumes spec is:	lonarr(ns,nq)
;				found		lonarr(nq)
;				mask:		bytarr(nq,nx,ny)		reordered
;				x,y,e,pu,veto:	uintarr(n)
;				yoff:		long Y offset into mask
;				multiple:	lonarr(n) or -1L
;				master:		bytarr(nx,ny)

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
		warning,'spec_accumulate4',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif
if n_elements(multiple) lt 1 then multiple=-1L
if n_elements(mask) lt long64(nq)*long64(nx)*long64(ny) then begin
	print,' spec_accumulate4: Error - bad size for mask.'
	err = 1L
	return, err
endif
if n_elements(master) lt 1 then begin
	use_master = 0
	master = 0B
endif else begin
	if n_elements(master) lt long(nx)*long(ny) then begin
		print,' spec_accumulate4: Error - bad size for master.'
		err = 1L
		return, err
	endif
	use_master = 1
endelse

value = bytarr(17)
value[*] = 0					; pass all by reference
err = 0L

if size(spec,/tname) ne 'LONG' then goto, bad
if size(found,/tname) ne 'LONG' then goto, bad
if size(mask,/tname) ne 'BYTE' then goto, bad
if size(x,/tname) ne 'UINT' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(pu,/tname) ne 'UINT' then goto, bad
if size(veto,/tname) ne 'UINT' then goto, bad
if size(multiple,/tname) ne 'LONG' then goto, bad
if size(master,/tname) ne 'BYTE' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'spec_accumulate4'), cdecl=geolib_cdecl(), $
			e,x,y,pu,veto,long(n), spec,long(ns),found, $
			mask,long(nq),long(nx),long(ny),long(yoff), multiple, long(use_master), master, value=value )

return, err

bad:
	print,' spec_accumulate4: Error - bad type for argument.'
	err = 1L
	return, err
end
