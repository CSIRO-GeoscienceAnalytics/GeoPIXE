function da_accumulate_stack, x,y,z,z2,e,pu,veto,n,good, image,nx,ny,nz, nnpu,nn, $
				image_error,nxe,nye,nze, matrix,size,nee, multiple=multiple, compress=compress
;
;  This assumes image is:	fltarr(nx,ny,nz)
;			image_error:	fltarr(nxe,nye,nz)
;				nnpu:		lonarr(nxe,nye)
;				nn:			lonarr(nxe,nye)
;				matrix:		fltarr(size,nee)
;				x,y,z,e,pu:	uintarr(n)
;				z2			uintarr(n), re-mapped 'z' onto 'matrix' columns
;				veto:		uintarr(n), valid=0, veto=1
;				good		number of good events in n
;				multiple:	lonarr(n) or -1
;				/compress	compress z down onto z2 value set
;							(this is now done in 'da_evt_stack', not usually here)
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
		warning,'da_accumulate_stack',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1L
	endif
endif
if n_elements(multiple) lt 1 then multiple=-1L
if n_elements(compress) lt 1 then compress=0

value = bytarr(24)
value[*] = 0					; pass all by reference
err = 0L

if size(x,/tname) ne 'UINT' then begin sb='x' & goto, bad & endif
if size(y,/tname) ne 'UINT' then begin sb='y' & goto, bad & endif
if size(z,/tname) ne 'UINT' then begin sb='z' & goto, bad & endif
if size(z2,/tname) ne 'UINT' then begin sb='z2' & goto, bad & endif
if size(e,/tname) ne 'UINT' then begin sb='e' & goto, bad & endif
if size(pu,/tname) ne 'UINT' then begin sb='pu' & goto, bad & endif
if size(veto,/tname) ne 'UINT' then begin sb='veto' & goto, bad & endif
if size(image,/tname) ne 'FLOAT' then begin sb='image' & goto, bad & endif
if size(image_error,/tname) ne 'FLOAT' then begin sb='image_error' & goto, bad & endif
if size(nnpu,/tname) ne 'LONG' then begin sb='nnpu' & goto, bad & endif
if size(nn,/tname) ne 'LONG' then begin sb='nn' & goto, bad & endif
if size(matrix,/tname) ne 'FLOAT' then begin sb='matrix' & goto, bad & endif
if size(multiple,/tname) ne 'LONG' then begin sb='multiple' & goto, bad & endif

;err = call_external( geopixe_library(), geolib_name( 'da_accumulate_stack'), cdecl=geolib_cdecl(), $
;		x,y,z,e,pu,veto,long(n),long(good), image,long(nx),long(ny),long(nz), nnpu,nn, image_error,long(nxe),long(nye),long(nze), $
;		matrix, long(size), long(nee), multiple, value=value )
err = call_external( geopixe_library(), geolib_name( 'da_accumulate_stack'), cdecl=geolib_cdecl(), $
		x,y,z,z2,e,pu,veto,long(n),long(good), image,long(nx),long(ny),long(nz), nnpu,nn, image_error,long(nxe),long(nye),long(nze), $
		matrix, long(size), long(nee), multiple, long(compress), value=value )

return, err

bad:
	gprint,' da_accumulate_stack: Error - bad type for argument: '+sb
	err = 1L
	return, err
end
