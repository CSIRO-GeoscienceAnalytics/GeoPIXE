function image_accumulate, x,y,e,d,n, image,nx,ny,nel
;
;  This assumes 	pimage:		fltarr(nx,ny,nel)
;					d:			fltarr(n)
;					x,y,e:		lonarr(n)

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
		warning,'image_accumulate',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1L
	endif
endif
if n_elements(multiple) lt 1 then multiple=-1L

value = bytarr(9)
value[*] = 0					; pass all by reference
err = 0L

if (size(x,/tname) ne 'ULONG') and (size(x,/tname) ne 'LONG') then goto, bad
if (size(y,/tname) ne 'ULONG') and (size(y,/tname) ne 'LONG') then goto, bad
if (size(e,/tname) ne 'ULONG') and (size(e,/tname) ne 'LONG') then goto, bad
if size(image,/tname) ne 'FLOAT' then goto, bad
if size(d,/tname) ne 'FLOAT' then goto, bad

err = call_external( geopixe_library(), geolib_name( 'image_accumulate'), cdecl=geolib_cdecl(), $
			x,y,e,d,long(n), image,long(nx),long(ny),long(nel), value=value )

return, err

bad:
	print,' image_accumulate: Error - bad type for argument.'
	err = 1L
	return, err
end
