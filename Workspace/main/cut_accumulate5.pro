function cut_accumulate5, x,y,e,pu,veto, n,good, image,nx,ny, nnpu,nn, error,nxe,nye, $
						cut,type,dleft,dright,nel, multiple=multiple, $
						stim_mean, image_count, error_count
;
;  This assumes image is:	fltarr(nx,ny,nel)
;  				image_count	long(nx,ny,nel)
;				error 		fltarr(nxe,nye,nel)
;				error_count	long(nxe,nye,nel)
;				nnpu:		lonarr(nxe,nye)
;				nn:			lonarr(nxe,nye)
;				e:			fltarr(n)
;				x,y,pu,veto:	uintarr(n)
;				cut:		fltarr(6,nel)
;				type:		uintarr(nel)
;				dleft,dright:	fltarr(nel)
;				multiple:	lonarr(n)

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
		warning,'cut_accumulate5',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1L
	endif
endif
if n_elements(multiple) lt 1 then multiple=-1L

value = bytarr(24)
value[*] = 0					; pass all by reference
err = 0L

if size(x,/tname) ne 'UINT' then begin sb='x' & goto, bad & endif
if size(y,/tname) ne 'UINT' then begin sb='y' & goto, bad & endif
if size(e,/tname) ne 'FLOAT' then begin sb='e' & goto, bad & endif
if size(pu,/tname) ne 'UINT' then begin sb='pu' & goto, bad & endif
if size(veto,/tname) ne 'UINT' then begin sb='veto' & goto, bad & endif
if size(image,/tname) ne 'FLOAT' then begin sb='image' & goto, bad & endif
if size(error,/tname) ne 'FLOAT' then begin sb='error' & goto, bad & endif
if size(nnpu,/tname) ne 'LONG' then begin sb='nnpu' & goto, bad & endif
if size(nn,/tname) ne 'LONG' then begin sb='nn' & goto, bad & endif
if size(cut,/tname) ne 'FLOAT' then begin sb='cut' & goto, bad & endif
if size(type,/tname) ne 'UINT' then begin sb='type' & goto, bad & endif
if size(dleft,/tname) ne 'FLOAT' then begin sb='dleft' & goto, bad & endif
if size(dright,/tname) ne 'FLOAT' then begin sb='dright' & goto, bad & endif
if size(multiple,/tname) ne 'LONG' then begin sb='multiplt' & goto, bad & endif
if stim_mean then begin
	if size(image_count,/tname) ne 'LONG' then begin sb='image_count' & goto, bad & endif
	if size(error_count,/tname) ne 'LONG' then begin sb='error_count' & goto, bad & endif
endif

err = call_external( geopixe_library(), geolib_name( 'cut_accumulate5'), cdecl=geolib_cdecl(), $
			x,y,e,pu,veto, long(n),long(good), image,long(nx),long(ny), nnpu,nn, $
			error,long(nxe),long(nye), cut,type,dleft,dright,long(nel), multiple, long(stim_mean), $
			image_count, error_count, value=value )
return, err

bad:
	gprint,' cut_accumulate5: Error - bad type for argument: '+sb
	err = 1L
	return, err
end
