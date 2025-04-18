function da_accumulate10, x,y,e,pu,veto,n,good, image,nx,ny,nel2, nnpu,nn, image_error,nxe,nye, $
						nel, matrix, size, phase, ncomp, inv_yield, invy, fny, multiple=multiple
;
; Version of da_accumulate5 for multi-phase DA matrix
; 
;  This assumes image:		fltarr(nx,ny,nel2)
;			image_error:	fltarr(nxe,nye,nel2)
;				nnpu:		lonarr(nx,ny)
;				nn:			lonarr(nx,ny)
;				matrix:		fltarr(size,nel,ncomp)						; multiphase DA matrices
;				phase:		fltarr(nx,ny,nel)							; phase table
;				inv_yield	fltarr(nel,ncomp)							; inverse yields from DA matrix file
;				invy		fltarr(nxe,nye,nel)							; 1/Y image accumulation
;				fny			fltarr(nxe,nye,nel)							; accumulation count
;				x,y,e,pu:	uintarr(n)
;				veto:		uintarr(n)									; valid=0, veto=1
;				good													; number of good events in n
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
		warning,'da_accumulate10',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1L
	endif
endif
if n_elements(multiple) lt 1 then multiple=-1L

if n_elements(image[*,0,0]) ne nx then begin sb='image X' & goto, bad_array & endif
if n_elements(image[0,*,0]) ne ny then begin sb='image Y' & goto, bad_array & endif
if n_elements(image[0,0,*]) ne nel2 then begin sb='image Nel' & goto, bad_array & endif
if n_elements(image_error[*,0,0]) ne nxe then begin sb='error X' & goto, bad_array & endif
if n_elements(image_error[0,*,0]) ne nye then begin sb='error Y' & goto, bad_array & endif
if n_elements(image_error[0,0,*]) ne nel2 then begin sb='error Nel' & goto, bad_array & endif
if n_elements(matrix[*,0,0]) ne size then begin sb='matrix Ne' & goto, bad_array & endif
if n_elements(matrix[0,*,0]) ne nel then begin sb='matrix Nel' & goto, bad_array & endif
if n_elements(matrix[0,0,*]) ne ncomp then begin sb='matrix Ncomp' & goto, bad_array & endif
if n_elements(nnpu[*,0,0]) ne nx then begin sb='nnpu X' & goto, bad_array & endif
if n_elements(nnpu[0,*,0]) ne ny then begin sb='nnpu Y' & goto, bad_array & endif
if n_elements(nn[*,0,0]) ne nx then begin sb='nn X' & goto, bad_array & endif
if n_elements(nn[0,*,0]) ne ny then begin sb='nn Y' & goto, bad_array & endif
if n_elements(phase[*,0,0]) ne nx then begin sb='phase X' & goto, bad_array & endif
if n_elements(phase[0,*,0]) ne ny then begin sb='phase Y' & goto, bad_array & endif
if n_elements(phase[0,0,*]) ne ncomp then begin sb='phase Ncomp' & goto, bad_array & endif
if n_elements(inv_yield[*,0]) ne nel then begin sb='inv_yield Nel' & goto, bad_array & endif
if n_elements(inv_yield[0,*]) ne ncomp then begin sb='inv_yield Ncomp' & goto, bad_array & endif
if n_elements(invy[*,0,0]) ne nxe then begin sb='invy X' & goto, bad_array & endif
if n_elements(invy[0,*,0]) ne nye then begin sb='invy Y' & goto, bad_array & endif
if n_elements(invy[0,0,*]) ne nel then begin sb='invy Nel' & goto, bad_array & endif
if n_elements(fny[*,0,0]) ne nxe then begin sb='fny X' & goto, bad_array & endif
if n_elements(fny[0,*,0]) ne nye then begin sb='fny Y' & goto, bad_array & endif
if n_elements(fny[0,0,*]) ne nel then begin sb='fny Nel' & goto, bad_array & endif

value = bytarr(25)
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
if size(phase,/tname) ne 'FLOAT' then begin sb='table' & goto, bad & endif
if size(multiple,/tname) ne 'LONG' then begin sb='multiple' & goto, bad & endif
if size(inv_yield,/tname) ne 'FLOAT' then begin sb='inv_yield' & goto, bad & endif
if size(invy,/tname) ne 'FLOAT' then begin sb='invy' & goto, bad & endif
if size(fny,/tname) ne 'FLOAT' then begin sb='fny' & goto, bad & endif

err = call_external( geopixe_library(), geolib_name( 'da_accumulate10'), cdecl=geolib_cdecl(), $
		x,y,e,pu,veto,long(n),long(good), image,long(nx),long(ny),long(nel2), nnpu,nn, image_error,long(nxe),long(nye), $
		long(nel), matrix, long(size), phase,ncomp, inv_yield, invy, fny, multiple, value=value )

return, err

bad:
	gprint,' da_accumulate10: Error - Bad type for argument: '+sb
	err = 1L
	return, err
bad_array:
	gprint,' da_accumulate10: Error - Bad array dimensions for argument: '+sb
	err = 1L
	return, err
end
