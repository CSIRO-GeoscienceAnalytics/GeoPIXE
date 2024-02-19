pro write_spec, p, file, trav=trav, error=error
;
;	Write the spectra pointed to by 'p' to a .spec
;	spectrum file 'file'.
;
;	'p' is a pointer (or pointer array) pointing to
;	spectrum structs, containing the spectrum details
;	and data.
;
;	/trav - write a .trav file instead

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
		warning,'Write_spec',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_io
	endif
endif

; From version -11 onwards, use a /XDR binary format

version = -26								; .spec version number
error = 1
if n_elements(file) lt 1 then goto, bad_file
if n_elements(trav) lt 1 then trav=0
n = n_elements(p)
if n lt 1 then goto, bad_p
if ptr_good(p[0]) eq 0 then goto, bad_ptr
if var_type(*p[0]) ne 8 then goto, bad_ptr

on_ioerror, null
;on_ioerror, bad_io
close, 1
;if trav then begin
;	s = strip_file_ext( file) + '.trav'
;endif else begin
;	s = strip_file_ext( file) + '.spec'
;endelse
s = file
print,'write_spec: file= ',s
openw, 1, s, /XDR

writeu, 1, version

writeu, 1, (*p[0]).ystep
writeu, 1, (*p[0]).processed, (*p[0]).valid, (*p[0]).bad_xy, (*p[0]).clipped
writeu, 1, (*p[0]).xstep_on, (*p[0]).xstep, (*p[0]).step_events
writeu, 1, (*p[0]).step_toggle, (*p[0]).toggle_bit, (*p[0]).toggle_station
writeu, 1, (*p[0]).events
writeu, 1, (*p[0]).channel, (*p[0]).type, (*p[0]).detector, (*p[0]).xcompress
writeu, 1, (*p[0]).ycompress, (*p[0]).nx, (*p[0]).ny
writeu, 1, (*p[0]).microns, (*p[0]).ecal.poly[1], (*p[0]).ecal.poly[0]

writeu, 1, (*p[0]).matrix.file
writeu, 1, (*p[0]).matrix.charge
if ptr_valid((*p[0]).matrix.mdl) then begin
	writeu, 1, n_elements(*(*p[0]).matrix.mdl)
	if n_elements(*(*p[0]).matrix.mdl) ge 1 then writeu, 1, *(*p[0]).matrix.mdl
endif else begin
	writeu, 1, 0
endelse
device_name = (*p[0]).DevObj->name()
writeu, 1, device_name
writeu, 1, (*p[0]).source2
writeu, 1, (*p[0]).throttle
writeu, 1, (*p[0]).pileup
writeu, 1, (*p[0]).linearize

writeu, 1, (*p[0]).IC.mode
if (*p[0]).IC.mode ne 0 then begin
	writeu, 1, (*p[0]).IC
endif
np = 0
if ptr_valid((*p[0]).plist) then np = n_elements(*(*p[0]).plist)
writeu, 1, np
if np gt 0 then begin
	writeu, 1, *(*p[0]).plist
endif
writeu, 1, (*p[0]).dwell

writeu, 1, (*p[0]).max_detectors
writeu, 1, (*p[0]).has_pileup
if (*p[0]).has_pileup then begin
	writeu, 1, *(*p[0]).pileup_loss_det
endif
writeu, 1, (*p[0]).has_dead
if (*p[0]).has_dead then begin
	writeu, 1, *(*p[0]).deadtime_det
endif

q = where( ptr_valid( p), count)
writeu, 1, count
if count ge 1 then begin
	for i=0L,count-1 do begin
		if put_spec( 1, p[q[i]]) eq 1 then goto, bad_io
	endfor
endif
error = 0

finish:
	close,1

	f = file_search( file+'.*')							; delete .spec.0, .spec.1, ... temp files
	if f[0] ne '' then file_delete, f, /quiet

	return

bad_io:
	warning,'write_spec','error writing spectrum to file'
	goto, finish
bad_file:
	warning,'write_spec','no file name supplied'
	goto, usage
bad_ptr:
	warning,'write_spec','bad spectrum pointer'
	goto, usage
bad_p:
	warning,'write_spec','no spectrum pointer'
	goto, usage

usage:
	print,'write_spec: Usage: write_spec, p, file'
	print,'		where "p" is pointer to spectrum struct(s)'
	print,'		and "file" is the name of the output file'
	return
end
