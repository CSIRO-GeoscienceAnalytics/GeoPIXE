function da_build_mpda_table, table, spec, phase, nx, ny, n_comp, n_energy
;
; Construct 'table' for multi-phase DA matrix
; 
;  This assumes table:		fltarr(n_energy,n_comp,nx,ny)
;				spec:		fltarr(n_energy,n_comp)
;				phase:		fltarr(nx,ny,n_comp)
;				

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
		warning,'da_build_mpda_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1L
	endif
endif

value = bytarr(7)
value[*] = 0					; pass all by reference
err = 0L

if size(phase,/tname) ne 'FLOAT' then begin sb='image' & goto, bad & endif
if size(spec,/tname) ne 'FLOAT' then begin sb='matrix' & goto, bad & endif
if size(table,/tname) ne 'FLOAT' then begin sb='table' & goto, bad & endif

err = call_external( geopixe_library(), geolib_name( 'da_build_mpda_table'), cdecl=geolib_cdecl(), $
		table, spec, phase, long(nx), long(ny), long(n_comp), long(n_energy), value=value )

return, err

bad:
	gprint,' da_build_mpda_table: Error - bad type for argument: '+sb
	err = 1L
	return, err
end
