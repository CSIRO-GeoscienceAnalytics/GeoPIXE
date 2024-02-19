pro mdaq2_accumulate_dtfx, x,y,ste,veto,fx,n,n_fx, flux_mode, flux_scale, xrange,yrange,n_flux, $
			dta, dead, flux=flux, dwell=dwell, xcompress=xcompress, ycompress=ycompress, $
			spectrum=spectrum, image=image, pseudo=pseudo, weight=weight, error=err
;
;  This assumes x,y,veto,pseudo	uintarr(n) (pseudo only used for /image)
;				fx			fltarr(n_fx,n)
;				flux_mode	0=Epics PV, 1=FC0, 2=FC1
;				flux_scale	units for flux scaling
;				dead		deadtime image (image mode), versus detector (spectrum mode)
;				weight		weight image (image mode), versus detector (spectrum mode)
;				flux		flux array (image mode only)
;				dwell		dwell array (image mode only)
;	xrange,yrange,n_flux	dimensions for image mode flux array
;				xrange,1	dimensions for dead spectrum mode

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
		warning,'mdaq2_accumulate_dtfx',['IDL run-time error caught.', '', $
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
if n_elements(dwell) lt 1 then dwell=0.0
if n_elements(pseudo) lt 1 then pseudo=0US
if spectrum eq 0 then image=1L

value = bytarr(21)
value[*] = 0					; pass all by reference
err = 0L

if size(x,/tname) ne 'UINT' then begin sb='x' & goto, bad & endif
if size(y,/tname) ne 'UINT' then begin sb='y' & goto, bad & endif
if size(ste,/tname) ne 'UINT' then begin sb='ste' & goto, bad & endif
if size(veto,/tname) ne 'UINT' then begin sb='veto' & goto, bad & endif
if size(pseudo,/tname) ne 'UINT' then begin sb='pseudo' & goto, bad & endif
if size(fx,/tname) ne 'FLOAT' then begin sb='fx' & goto, bad & endif
if size(flux_scale,/tname) ne 'FLOAT' then begin sb='flux_scale' & goto, bad & endif
if size(flux,/tname) ne 'FLOAT' then begin sb='flux' & goto, bad & endif
if size(dead,/tname) ne 'FLOAT' then begin sb='dead' & goto, bad & endif
if size(weight,/tname) ne 'FLOAT' then begin sb='weight' & goto, bad & endif
if size(dwell,/tname) ne 'FLOAT' then begin sb='dwell' & goto, bad & endif

err = call_external( geopixe_library(), geolib_name( 'mdaq2_accumulate_dtfx'), cdecl=geolib_cdecl(), $
		image, x,y,ste,veto,pseudo,fx,long(n),long(n_fx), long(flux_mode), flux_scale, $
		long(xcompress),long(ycompress), long(xrange),long(yrange),long(n_flux), float(dta), $
		flux, dead, weight, dwell, value=value )
if err then gprint,' mdaq2_accumulate_dtfx: Error in Fortran - bad number of arguments'
return

bad:
	gprint,' mdaq2_accumulate_dtfx: Error - bad type for argument: '+sb
	err = 1L
	return
end
