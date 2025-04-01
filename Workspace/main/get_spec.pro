function get_spec, u, header=header, find=find, version=version
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
		warning,'Get_spec',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, error
	endif
endif

if n_elements(header) lt 1 then header=0
if n_elements(find) lt 1 then find=0
if n_elements(version) lt 1 then version=-11

null_spectrum = define(/spectrum)
spec = null_spectrum

on_ioerror, bad_io

s = ' '
readu, u, s
spec.source = s
readu, u, s
spec.label = s
readu, u, s
spec.sample = s
readu, u, s
spec.grain = s
readu, u, s
spec.comment = s

n = 0
readu, u, n
if n gt max_history then print,'get_spec: exceeded max_history'
spec.n_history = n < max_history
if n gt 0 then begin
	s = strarr(n)
	readu, u, s
	spec.history[0:spec.n_history-1] = s[0:spec.n_history-1]
endif

cal = spec.cal
readu, u, cal
spec.cal = cal

x = 0.0
readu, u, x
spec.charge = x

if version le -20 then begin
	readu, u, x
	spec.IC_total = x
endif
if version le -15 then begin
	readu, u, n
	spec.multiplicity = n
endif
if version le -24 then begin
	xn = fltarr(2)
	readu, u, xn
	spec.deadtime_correction = xn[0]
	spec.energy = xn[1]
endif

xn = fltarr(5)
readu, u, xn
spec.x = xn[0]
spec.y = xn[1]
spec.z = xn[2]
spec.theta = xn[3]
spec.phi = xn[4]

xn = fltarr(2)
readu, u, xn
spec.scan.x = xn[0]
spec.scan.y = xn[1]

if version le -14 then begin
	readu, u, n, xn
	spec.shape.type = n
	spec.shape.x = xn[0]
	spec.shape.y = xn[1]
endif

readu, u, n
spec.filter = n
readu, u, n
spec.station = n
;print,'station=',n
nn = intarr(2)
readu, u, nn
spec.sequence.num = nn[0]
spec.sequence.sub = nn[1]

readu, u, n
spec.size = n

spec.has_errors = 0
readu, u, n
spec.has_errors = n

spec.show_back = 0
readu, u, n
spec.show_back = n

spec.ecompress = 1
readu, u, n
spec.ecompress = n

spec.pileup_ratio = 0.0
spec.pileup_A4 = 1.0
spec.FWHM = 0.0
if version le -13 then begin
	r1 = 0.0
	r2 = 0.0
	readu, u, r1,r2
	spec.pileup_ratio = r1
	spec.pileup_A4 = r2
endif
if version le -18 then begin
	f = 0.0
	readu, u, f
	spec.FWHM = f
endif

spec.array = 0
if version le -16 then begin
	readu, u, n
	spec.array = n
	if spec.array then begin
		na = 0L
		readu, u, na
		if na gt 0 then begin
			active = intarr(na)
			readu, u, active
			spec.pactive = ptr_new(active)
		endif
	endif
endif

if version le -22 then begin
	nxc = 0L
	readu,1, nxc
	if nxc gt 1 then begin
		x_coords = dblarr(nxc)
		readu,1, x_coords
		spec.px_coords = ptr_new(x_coords)
	endif
	sx = ''
	readu,1,sx
	spec.x_coord_units = sx
;	print,'version -22: X coords=',nxc,', units=',sx[0]
endif

if version le -26 then begin
	ambient = {	on:		0, $				; ambient conditions detected in spectrum data
				P:		0.0, $				; pressure (mbar)
				T:		0.0 }				; temperature (C)
	tube = {	volts:		0.0, $			; lab source tube voltage (kV)
				current:	0.0, $			; lab source tube current (uA)
				time:		0.0 }			; acquisition "real" time (use with 'deadtime_correction')

	readu,1,ambient
	readu,1,tube
	spec.ambient = ambient
	spec.tube = tube
endif

if header and ((spec.station eq 0) or (spec.station eq find) or (find le 0)) then goto, done

if (spec.size gt 0) then begin
	readu, u, n
	xn = fltarr(n)
	spec.size = spec.size < n					; should be same as above, but not sometimes
	readu, u, xn
	spec.data = ptr_new( xn[0:spec.size-1], /no_copy)
endif

if (spec.size gt 0) and (spec.has_errors eq 1)then begin
	readu, u, n
	en = fltarr(n)
	readu, u, en
	spec.error = ptr_new( en[0:(spec.size < n)-1], /no_copy)
endif

readu, u, n
spec.n_fit = n
if n gt 0 then begin
	for j=0L,n-1 do begin
		pj = get_spec( u, version=version)
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
	print,'get_spec: I/O error'
	goto, error
bad_fit:
	print,'get_spec: error on recursive get_spec of fit'
	goto, error

error:
	return, 0
end
