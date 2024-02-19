function get_text_spec, u, header=header, find=find, version=version
;
;	Read the spectrum in the .spec file
;	on unit 'u'.
;
;	return 'p', a pointer to a
;	spectrum struct, containing the spectrum details
;	and data.
;
;	If /header, read header. If header shows station='find' then
;	stop (find = 0 means accept any). Else continue reading spectrum.
;
;	return p = OK, 0 = error.

ErrorNo = 0
common c_null_spec_1, max_history, max_cal,  max_fit
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
		warning,'get_text_spec',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, error
	endif
endif

if n_elements(header) lt 1 then header=0
if n_elements(find) lt 1 then find=0
if n_elements(version) lt 1 then version=-4

null_spectrum = define(/spectrum)
spec = null_spectrum

on_ioerror, bad_io

s = ' '
readf, u, s
spec.source = s
readf, u, s
spec.label = s
readf, u, s
spec.sample = s
readf, u, s
spec.grain = s
readf, u, s
spec.comment = s

n = 0
readf, u, n
spec.n_history = n
if n gt max_history then print,'get_text_spec: exceeded max_history'
if n gt 0 then begin
	for i=0L,n-1 do begin
		readf, u, s
		if i lt max_history then spec.history[i] = s
	endfor
endif

readf, u, n
spec.cal.order = n
if n gt max_cal then print,'get_text_spec: exceeded max_cal'
if n gt 0 then begin
	readf, u, s
	spec.cal.units = s
	xn = fltarr(n+1)
	readf, u, xn
	for i=0L,n do begin
		if i le max_cal then spec.cal.poly[i] = xn[i]
	endfor
endif

x = 0.0
readf, u, x
spec.charge = x
xn = fltarr(5)
readf, u, xn
spec.x = xn[0]
spec.y = xn[1]
spec.z = xn[2]
spec.theta = xn[3]
spec.phi = xn[4]
xn = fltarr(2)
readf, u, xn
spec.scan.x = xn[0]
spec.scan.y = xn[1]
readf, u, n
spec.filter = n
readf, u, n
spec.station = n
nn = intarr(2)
readf, u, nn
spec.sequence.num = nn[0]
spec.sequence.sub = nn[1]

readf, u, n
spec.size = n

spec.has_errors = 0
if version le -5 then begin
	readf, u, n
	spec.has_errors = n
endif

spec.show_back = 0
if version le -7 then begin
	readf, u, n
	spec.show_back = n
endif

spec.ecompress = 1
if version le -10 then begin
	readf, u, n
	spec.ecompress = n
endif


if header and ((spec.station eq 0) or (spec.station eq find) or (find eq 0)) then goto, done

if (spec.size gt 0) then begin
	xn = fltarr(spec.size)
	readf, u, xn
	spec.data = ptr_new( xn, /no_copy)
endif

if (spec.size gt 0) and (spec.has_errors eq 1)then begin
	en = fltarr(spec.size)
	readf, u, en
	spec.error = ptr_new( en, /no_copy)
endif

readf, u, n
spec.n_fit = n
if n gt 0 then begin
	for j=0L,n-1 do begin
		pj = get_text_spec( u, version=version)
		if ptr_valid(pj) eq 0 then goto, bad_fit
		if j lt max_fit then begin
			spec.fit[j] = pj
		endif else begin
			free_spectrum, pj
		endelse
	endfor
endif

done:
p = ptr_new( spec, /no_copy)
return, p

bad_io:
	print,'get_text_spec: I/O error'
	goto, error
bad_fit:
	print,'get_text_spec: error on recursive get_text_spec of fit'
	goto, error

error:
	return, 0
end
