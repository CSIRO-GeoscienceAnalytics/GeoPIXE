pro write_da, dain, F

;	Write the Dynamic Analysis matrix to 'F'
;	'dain' is a pointer to the matrix struct, or the
;	struct itself.

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
		warning,'Write_DA',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_io
	endif
endif

	if n_params() lt 2 then begin
		print,'write_da: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return
;	F = strip_file_ext(F) + '.dam'

	da = dain
	if size(da,/tname) eq 'POINTER' then begin
		if ptr_valid(da) eq 0 then goto, bad_da
		da = *da
	endif
	if size(da,/tname) ne 'STRUCT' then goto, bad_da

	da0 = da
	nda_extra = 0L
	if ptr_good( da.pmore) then begin
		if ptr_good( (*da.pmore)[0]) then begin
			nda_extra = n_elements( *da.pmore)
		endif
	endif
	
	version = -9L
	on_ioerror, bad_io
	openw, lun, F, /XDR, /get_lun
	writeu,lun, version
	writeu,lun, nda_extra

	j = -1
loop:
	writeu,lun, da.label
	writeu,lun, long(da.n_el)
	writeu,lun, da.cal_orig, da.cal, da.charge
	writeu,lun, da.el
	writeu,lun, long(da.ecompress)
	writeu,lun, float(da.mdl)
	writeu,lun, float(da.yield)
	writeu,lun, long(da.size)
	writeu,lun, da.matrix
	writeu,lun, long(da.station)

	writeu,lun, da.density0
	writeu,lun, da.thick

	use_mu_zero = 0L
	if n_elements( da.mu_zero) gt 1 then use_mu_zero=1L
	writeu,lun, use_mu_zero
	if use_mu_zero then writeu,lun, da.mu_zero

	writeu,lun, da.n_pure
	if da.n_pure gt 0 then writeu,lun, da.pure

	writeu,lun, da.array.on
	if da.array.on gt 0 then begin
		writeu,lun, da.array.n_det
		if da.array.n_det gt 0 then writeu,lun, da.array.rGamma
	endif

	writeu,lun, da.E_beam
	
	j = j+1
	if j lt nda_extra then begin
		da = *(*da0.pmore)[j]
		goto, loop
	endif
	close_file,lun
return

bad_da:
	warning,'Write_DA','bad DA matrix'
	return
bad_io:
	warning,'Write_DA','bad I/O'
	close_file, lun
	return
end
