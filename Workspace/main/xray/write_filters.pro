pro write_filters, p, F, error=error

; Write the Filter definitions to 'F'
; 'p' is a pointer to the filters array.

;	Filters are stored with thick in mg/cm^2 units always.
;	The /microns flag set in 'p' just says that the filter WAS specified in microns.
;	But the thick is still stored in mg/cm^2.
;
;	For /microns, make sure to convert thick
;	to mg/cm^2 in 'p' before write using density (t[g/cm^2] = density * t[microns] / 10).
;	This is usually done with a call to "make_filter".

	error = 1
	if n_params() lt 2 then goto, bad_pars
	if lenchr(F) lt 1 then goto, bad_pars
	if ptr_valid(p) eq 0 then goto, bad_filters

	F = strip_file_ext(F) + '.filter'
	n = n_elements(*p)

	version = -3L					; .filter version number
	on_ioerror, bad_io
	close, 1
	openw, 1, F, /XDR

	writeu,1, version
	writeu,1, n
	writeu,1, (*p)[0:n-1]
	close,1

	error = 0
return

bad_io:
	warning,'Write_filters','error writing filters file: '+F
	return
bad_filters:
	warning,'Write_filters','error in filters structure array'
	return
bad_pars:
	warning,'write_filters','missing arguments in call'
	return
end
