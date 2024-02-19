pro append_DA_fit, p, pmatrix, pregion, charge, multiplicity, throttle=throttle
;
;	Estimate the total fit to extracted spectra 'p'
;	using the DA matrix 'pmatrix' and the concentration
;	'pconc' and the charge 'charge'.
;
;	'p' is a pointer to a spectrum struct as in 'spectrum_display' or 'read_spec'.
;
;	'pregion' points to the region results
;
;	Use the passed 'charge' rather than the region's fractional charge
;	as in the spectrum header because the spectra may have been projected
;	without a valid charge.

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
		warning,'Append_DA_Fit',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

pel = (*pregion).el
pconc = (*pregion).conc

if ptr_valid(p) eq 0 then return
if ptr_valid(pmatrix) eq 0 then return
if ptr_valid(pconc) eq 0 then return
if ptr_valid(pel) eq 0 then return
if charge lt 1.0e-10 then return
if multiplicity lt 1 then multiplicity=1 
if (*pmatrix).n_pure lt 1 then goto, old_matrix
if n_elements(throttle) lt 1 then throttle=0

cal = (*p).cal
if abs( cal.poly[1]-1.0) lt 0.001 then return				; uncalibrated spectrum, abort

cal_ab, cal, (*pmatrix).cal.a,(*pmatrix).cal.b,'keV', /set

spec = map_spec( *(*p).data, (*p).cal, cal, error=err)	
if err then return

for i=0L,(*p).n_fit-1 do begin
	if ptr_valid( (*p).fit[i] ) then free_spectrum, (*p).fit[i]
endfor
(*p).n_fit = 0

;.....................................................................................

optimize_back = (n_elements( (*pmatrix).matrix[0,0,*]) gt 1) or ((*pmatrix).array.on eq 1)
;optimize_back = 0

if optimize_back then begin
	t1 = spec & t2 = spec & tb = spec
	x = (findgen(41)/10.)-2.
	r = 10.^x
	bias = 0.25 * (x^2)
	
	nr = n_elements(r)
	chisq = fltarr(nr)
	for i=0,nr-1 do begin
		t = build_da_fit( p, pmatrix, pregion, charge, multiplicity, bscale=r[i], back=b, /suppress, error=err)
		if err then return
	
		d = spec-t
		t1[*] = 0 & t2[*] = 0 & tb[*]=0
		q = where( spec lt t, nq, complement=qc, ncomplement=nqc)
		if nqc gt 0 then begin
			t1[qc] = d[qc]^2			
		endif
		if nq gt 0 then begin
			t2[q] = abs(d[q])^2		;2.3
		endif
		qb = where( spec lt (b-3.), nqb)			; is this correct?
		if nqb gt 0 then begin
			tb[qb] = (b[qb]-spec[qb])^2
		endif

;		Bias chisq in favour of Back scaling near 1.0 ...

		chisq[i] = bias[i] + total( (t1+t2+tb)/((spec>10.)^1))/n_elements(spec)
	endfor
	q = sort(chisq)
	rf = r[q[0]]
endif else rf=1.0
print,'append_da_fit: scale back = ',rf

;.....................................................................................

t = build_da_fit( p, pmatrix, pregion, charge, multiplicity, show=show, back=b, bscale=rf, error=err)
if err then return

null_spectrum = define(/spectrum)

if throttle then begin
	cal2 = (*p).cal
	ptr_free, (*p).data
	(*p).size = n_elements(t)
	(*p).data = ptr_new(t, /no_copy)
	(*p).cal = cal
	map_to_cal, p, cal2						; should switch to use 'map_spec' ??
	(*p).label = 'Throttle basis spectrum - peak elements'
	(*p).comment = 'Maximum feature spectrum for Throttle'
endif else begin
	if show then begin
		spec = null_spectrum
		spec.source = (*p).source
		spec.label = 'DA background for ' + (*p).label
		spec.cal.poly[0] = cal.poly[0]
		spec.cal.poly[1] = cal.poly[1]
		spec.cal.units = cal.units
		spec.cal.order = 1
		spec.comment = 'DA background estimate'
		spec.charge = charge
		spec.multiplicity = multiplicity
		spec.size = n_elements(b)
		spec.data = ptr_new(b, /no_copy)

		(*p).fit[0] = ptr_new(spec, /no_copy)
		(*p).n_fit = 1
	endif

	spec = null_spectrum
	spec.source = (*p).source
	spec.label = 'DA spectrum fit for ' + (*p).label
	; spec.cal.poly[0] = cal.poly[0] + cal.poly[1]				; old VAX offset?
	spec.cal.poly[0] = cal.poly[0]
	spec.cal.poly[1] = cal.poly[1]
	spec.cal.units = cal.units
	spec.cal.order = 1
	spec.comment = 'DA spectrum fit using pure elements'
	spec.charge = charge
	spec.multiplicity = multiplicity

	spec.size = n_elements(t)
	spec.data = ptr_new(t, /no_copy)

	(*p).fit[(*p).n_fit] = ptr_new(spec, /no_copy)
	(*p).n_fit = (*p).n_fit+1
endelse

return

old_matrix:
	print,'append_DA_fits: old matrix without pure element spectra'
	return
end

