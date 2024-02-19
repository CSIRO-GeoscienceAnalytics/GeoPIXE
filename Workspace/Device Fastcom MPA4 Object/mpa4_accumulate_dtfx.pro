pro mpa4_accumulate_dtfx, x,y,ste,veto,n, fx,n_fx, channel_on,nc, xrange=xrange,yrange=yrange, $
			dead=dead, flux=flux, spectrum=spectrum, image=image, pseudo=pseudo, weight=weight, error=err
;
;  This assumes x,y,veto,pseudo	uintarr(n) (pseudo only used for /image)
;				fx			fltarr(n_fx,n)
;				dead		deadtime image (image mode), versus detector (sopectrum mode)
;				weight		weight image (image mode), versus detector (spectrum mode)
;				flux		flux array (image mode only)
;			xrange,yrange	dimensions for image mode flux array
;			xrange,1		dimensions for dead spectrum mode

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
		warning,'mpa4_accumulate_dtfx',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		err = 1
		return
	endif
endif
if n_elements(spectrum) lt 1 then spectrum=0L
if n_elements(image) lt 1 then image=0L
if n_elements(xcompress) lt 1 then xcompress=1L
if n_elements(ycompress) lt 1 then ycompress=1L
if n_elements(flux) lt 1 then flux=0.0
if n_elements(pseudo) lt 1 then pseudo=0US
if spectrum eq 0 then image=1L

value = bytarr(16)
value[*] = 0					; pass all by reference
err = 0L

if size(x,/tname) ne 'UINT' then begin sb='x' & goto, bad & endif
if size(y,/tname) ne 'UINT' then begin sb='y' & goto, bad & endif
if size(ste,/tname) ne 'UINT' then begin sb='ste' & goto, bad & endif
if size(veto,/tname) ne 'UINT' then begin sb='veto' & goto, bad & endif
if size(pseudo,/tname) ne 'UINT' then begin sb='pseudo' & goto, bad & endif
if size(fx,/tname) ne 'LONG' then begin sb='fx' & goto, bad & endif
if size(channel_on,/tname) ne 'INT' then begin sb='channel_on' & goto, bad & endif
if size(flux,/tname) ne 'FLOAT' then begin sb='flux' & goto, bad & endif
if size(dead,/tname) ne 'FLOAT' then begin sb='dead' & goto, bad & endif
if size(weight,/tname) ne 'FLOAT' then begin sb='weight' & goto, bad & endif

;Print, 'Before call to mpa4_accumulate_dtfx ...'
;wait,3.0

;glib = geopixe_library()
;gname = geolib_name( 'mpa4_accumulate_dtfx')
;cdecl = geolib_cdecl()

;err = call_external( glib, gname, cdecl=cdecl, $
err = call_external( geopixe_library(), geolib_name( 'mpa4_accumulate_dtfx'), cdecl=geolib_cdecl(), $
		long(image), x,y,ste,veto,pseudo,long(n), fx,long(n_fx), channel_on,long(nc), $
		long(xrange),long(yrange), flux, dead, weight, value=value )
if err then gprint,' mpa4_accumulate_dtfx: Error in Fortran - bad number of arguments'
return

bad:
	gprint,' mpa4_accumulate_dtfx: Error - bad type for argument: '+sb
	err = 1L
	return
end
