pro write_pink, p1, F, unit=unit, error=error

; Write the X-ray Pink Beam details to unit 'unit', or open file 'F'.
; Only close 'unit' if opened here to file 'F'.
; 
; 'p1' is a pointer to the pink struct, or the struct itself.

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
		warning,'write_pink',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
error = 1

	if n_elements(p1) lt 1 then return
	if (n_elements(F) lt 1) and (n_elements(unit) lt 1) then return

	if n_elements(unit) ne 0 then begin
		stat = fstat(unit)
		if stat.open eq 0 then goto, bad_stat
		F = stat.name
		open_file = 0
	endif else begin
		F = strip_file_ext(F) + '.pink'
		open_file = 1
		on_ioerror, bad_file
		openw, unit, F, /XDR, /get_lun
	endelse
	
	if size(p1,/tname) eq 'POINTER' then begin
		if ptr_good(p1) eq 0 then goto, bad_ptr
		(*p1).file = F
		p = p1
	endif else if size(p1,/tname) eq 'STRUCT' then begin
		p1.file = F
		p = ptr_new( p1)
	endif else return

	version = -1L									; .source version number	;@3-23

;	Assume that filters are in standard form (thick in mg/cm2 even if /microns set) already.

	on_ioerror, bad_io
	writeu,unit, version

	writeu,unit, (*p).continuum, (*p).energy, (*p).model, F, (*p).title
	writeu,unit, n_elements( (*p).spectrum.data) 
	writeu,unit, (*p).spectrum
	writeu,unit, (*p).modata
	writeu,unit, (*p).n_filters, n_elements( (*p).filters)
	writeu,unit, (*p).filters
	writeu,unit, (*p).mono
	writeu,unit, (*p).acceptance
	writeu,unit, (*p).poly
	writeu,unit, (*p).fe_spectrum_file
	writeu,unit, (*p).n_mirrors, n_elements( (*p).mirrors)
	writeu,unit, (*p).mirrors

finish:
	if open_file then close_file, unit
	error = 0
	return

bad_file:
	print,'write_pink: error opening file: '+F
	goto, finish
bad_stat:
	print,'write_pink: error connecting with unit'
	goto, finish
bad_io:
	print,'write_pink: bad I/O'
	goto, finish
bad_ptr:
	print,'write_pink: bad pointer supplied'
	goto, finish
end

