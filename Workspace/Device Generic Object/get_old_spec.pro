	pro get_old_spec, p, name, fit=fit
;
;	Read in a spectrum
;
	if n_elements(fit) lt 1 then fit=0

	obj = obj_new('GENERIC_DEVICE')
	if obj_valid(obj) eq 0 then return

	buff = fltarr(2048)
;
	spec = {spectrum, file:' ', label:' ', story1:' ', story2:' ', $
			story3:' ', size:0, order:0, units:' ', a:0.0, b:0.0, $
			q:0.0, x:buff, y:buff }
;
	on_ioerror,more
	openr,unit,name,/get_lun
;
	a = 1.0
	b = 0.0
	x = 0.0
	n = 0
	q = 0.0
	s = string(0)
;
	n_actual = 0
	readf,unit,n_actual
	if n_actual lt 1 then begin
		print,'	Error: No spectra in file.'
		return
	endif
;
	if fit then begin
		p = ptrarr(1)
	endif else begin
		p = ptrarr(n_actual)
	endelse
	for i=0L,n_actual-1 do begin
		on_ioerror,more
		readf,unit,s
		spec.file = s
		readf,unit,s
		spec.label = s
		readf,unit,s
		spec.story1 = s
		readf,unit,s
		spec.story2 = s
		readf,unit,s
		spec.story3 = s
;
		readf,unit,n
		spec.size = n
;
		readf,unit,n
		spec.order = n
		readf,unit,s
		spec.units = s
		readf,unit, b, a
		spec.a = a
		spec.b = b
;
		readf,unit,q
		spec.q = q
;
		n = spec.size
		if n lt 1 then return
;
		on_ioerror, NULL
		x = findgen(n)
		y = fltarr(n)
		readf,unit,y
;
		x = a*x +b
		n = min([2047,n-1])
		spec.x[0:n] = x[0:n]
		spec.y[0:n] = y[0:n]

		spec2 = define(/spectrum)
		spec2.DevObj = clone_device_object(obj)

		spec2.cal.order = 1
		spec2.cal.units = spec.units
		spec2.cal.poly[1] = spec.a
		spec2.cal.poly[0] = spec.b
		spec2.file = spec.file
		spec2.source = spec.label
		spec2.label = spec.label
		spec2.charge = spec.q

		if strpos(spec.story2,'Sr') ge 0 then begin
	;		help,spec,/struct
		endif

		pure = 0
		if strpos(spec.story2,'pure') ge 0 then begin
			if (strpos(spec.story2,'Back') eq -1) and (strpos(spec.story2,'Sum') eq -1) then begin
				spec2.label = spec2.label + ' pure '
				pure = 1
				spec.y = spec.y * 1000.
			endif
		endif

		spec2.size = n
		spec2.data = ptr_new(spec.y[0:n])

		if (i gt 0) and fit then begin
			(*p[0]).fit[i-1] = ptr_new( spec2, /no_copy)
			(*p[0]).n_fit = i
		endif else begin
			p[i] = ptr_new( spec2, /no_copy)
		endelse
	endfor

;
  more:	close,unit
	free_lun,unit
	return
	end

