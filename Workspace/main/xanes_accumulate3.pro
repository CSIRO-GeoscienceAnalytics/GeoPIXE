function xanes_accumulate3, x,y,e,n, image,nx,ny, image_error,nxe,nye, $
						nel, matrix, size, multiple=multiple
;
;  This assumes image is:	fltarr(nx,ny,nel)
;			image_error:	fltarr(nxe,nye,nel)
;				matrix:		fltarr(size,nel)
;				y,e:		uintarr(n)
;				x			long			for long record counts
;				multiple:	lonarr(n) or -1
;
;	and that error coords are normal x,y divided by 2.
;	except if ny=1, nye=1, as in traverse.

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
		warning,'xanes_accumulate3',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1L
	endif
endif
if n_elements(multiple) lt 1 then multiple=-1L

value = bytarr(14)
value[*] = 0					; pass all by reference
err = 0L

if size(x,/tname) ne 'LONG' then goto, bad
if size(y,/tname) ne 'UINT' then goto, bad
if size(e,/tname) ne 'UINT' then goto, bad
if size(image,/tname) ne 'FLOAT' then goto, bad
if size(image_error,/tname) ne 'FLOAT' then goto, bad
if size(matrix,/tname) ne 'FLOAT' then goto, bad
if size(multiple,/tname) ne 'LONG' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'xanes_accumulate3'), cdecl=geolib_cdecl(), $
			x,y,e,long(n), image,long(nx),long(ny), image_error,long(nxe),long(nye), $
			long(nel), matrix, long(size), multiple, value=value )

return, err

bad:
	print,' xanes_accumulate3: Error - bad type for argument.'
	err = 1L
	return, err
end
