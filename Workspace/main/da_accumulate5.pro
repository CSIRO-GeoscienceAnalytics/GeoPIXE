function da_accumulate5, x,y,e,pu,veto,n,good, image,nx,ny, nnpu,nn, image_error,nxe,nye, $
						nel, matrix, size, multiple=multiple
;
;  This assumes image is:	fltarr(nx,ny,nel)
;			image_error:	fltarr(nxe,nye,nel)
;				nnpu:		lonarr(nxe,nye)
;				nn:			lonarr(nxe,nye)
;				matrix:		fltarr(size,nel)
;				x,y,e,pu:	uintarr(n)
;				veto:		uintarr(n), valid=0, veto=1
;				good		number of good events in n
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
		warning,'da_accumulate5',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1L
	endif
endif
if n_elements(multiple) lt 1 then multiple=-1L

value = bytarr(19)
value[*] = 0					; pass all by reference
err = 0L

if size(x,/tname) ne 'UINT' then begin sb='x' & goto, bad & endif
if size(y,/tname) ne 'UINT' then begin sb='y' & goto, bad & endif
if size(e,/tname) ne 'UINT' then begin sb='e' & goto, bad & endif
if size(pu,/tname) ne 'UINT' then begin sb='pu' & goto, bad & endif
if size(veto,/tname) ne 'UINT' then begin sb='veto' & goto, bad & endif
if size(image,/tname) ne 'FLOAT' then begin sb='image' & goto, bad & endif
if size(image_error,/tname) ne 'FLOAT' then begin sb='image_error' & goto, bad & endif
if size(nnpu,/tname) ne 'LONG' then begin sb='nnpu' & goto, bad & endif
if size(nn,/tname) ne 'LONG' then begin sb='nn' & goto, bad & endif
if size(matrix,/tname) ne 'FLOAT' then begin sb='matrix' & goto, bad & endif
if size(multiple,/tname) ne 'LONG' then begin sb='multiple' & goto, bad & endif

err = call_external( geopixe_library(), geolib_name( 'da_accumulate5'), cdecl=geolib_cdecl(), $
		x,y,e,pu,veto,long(n),long(good), image,long(nx),long(ny), nnpu,nn, image_error,long(nxe),long(nye), $
		long(nel), matrix, long(size), multiple, value=value )

return, err

bad:
	gprint,' da_accumulate5: Error - bad type for argument: '+sb
	err = 1L
	return, err
end
