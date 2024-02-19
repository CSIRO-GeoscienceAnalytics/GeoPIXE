function put_spec, u, p
;
;	Write the spectrum pointed to by 'p' to the .spec
;	spectrum file open on unit 'u'. Called from 'write_spec'.
;
;	'p' is a pointer to a
;	spectrum struct, containing the spectrum details and data.
;
;	return 0 = OK, 1 = error.

COMPILE_OPT STRICTARR
common c_null_spec_1, max_history, max_cal,  max_fit

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
		warning,'Put_spec',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, error
	endif
endif

n = n_elements(p)
if n lt 1 then goto, bad_p
if ptr_valid(p[0]) eq 0 then goto, bad_ptr

on_ioerror, null
;on_ioerror, bad_io

writeu, u, (*p).source
writeu, u, (*p).label
writeu, u, (*p).sample
writeu, u, (*p).grain
writeu, u, (*p).comment

n = min([ max_history, (*p).n_history])
writeu, u, n
if n gt 0 then begin
	writeu, u, (*p).history[0:n-1]
endif

writeu, u, (*p).cal

writeu, u, (*p).charge
writeu, u, (*p).IC_total
writeu, u, (*p).multiplicity
writeu, u, (*p).deadtime_correction, (*p).energy
writeu, u, (*p).x, (*p).y, (*p).z, (*p).theta, (*p).phi
writeu, u, (*p).scan.x, (*p).scan.y
writeu, u, (*p).shape.type, (*p).shape.x, (*p).shape.y
writeu, u, (*p).filter
writeu, u, (*p).station
writeu, u, (*p).sequence.num, (*p).sequence.sub

if ptr_valid((*p).error) eq 0 then (*p).has_errors = 0
writeu, u, (*p).size
writeu, u, (*p).has_errors
writeu, u, (*p).show_back
writeu, u, (*p).ecompress
writeu, u, (*p).pileup_ratio, (*p).pileup_A4
writeu, u, (*p).FWHM

writeu, u, (*p).array
if (*p).array then begin
	writeu, u, n_elements(*(*p).pactive)
	writeu, u, *(*p).pactive
endif

nxc = 0L
if ptr_good((*p).px_coords) then nxc = min( [(*p).size, n_elements(*(*p).px_coords)])
writeu,1, nxc
if nxc gt 1 then writeu,1, (*(*p).px_coords)[0:nxc-1]
writeu,1, (*p).x_coord_units

writeu, u, (*p).ambient
writeu, u, (*p).tube

if (*p).size gt 0 then begin
	na = n_elements( *(*p).data)
	writeu, u, na
	if na gt 0 then writeu, u, float(*(*p).data)
endif

if (*p).has_errors and ((*p).size gt 0) then begin
	writeu, u, n_elements( *(*p).error)
	writeu, u, float(*(*p).error)
endif

count = min([ max_fit, (*p).n_fit])
if count gt 0 then q = where( ptr_valid((*p).fit[0:count-1]), count)
writeu, u, count
if count ge 1 then begin
	for i=0L,count-1 do begin
		if put_spec( u, (*p).fit[q[i]]) eq 1 then goto, bad_fit
	endfor
endif

return, 0

bad_io:
	print,'put_spec: I/O error'
	goto, error
bad_fit:
	print,'put_spec: error on recursive put_spec of fit'
	goto, error
bad_ptr:
	print,'put_spec: bad spectrum pointer'
	goto, error
bad_p:
	print,'put_spec: no spectrum pointer'
	goto, error

error:
	return, 1
end
