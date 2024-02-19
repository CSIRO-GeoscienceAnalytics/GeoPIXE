function read_detector, F, error=error

;	Read detector definitions from 'F'
;	return as a pointer
;
;	Note that this reads thick (det.crystal.thick) in mg/cm^2

	error = 1
	if n_params() lt 1 then return, 0
	if lenchr(F) lt 1 then return, 0

	valid = [-1,-2,-3,-4,-5,-6,-7,-8,-9]

	on_ioerror, bad_io
	close, 1
	openr, 1, F, /XDR

	version = 0L
	readu,1, version
	q = where( version eq valid)
	if q[0] eq -1 then return, 0

	filters = define(/filter)
	crystal = make_layer('Si',1.0)
	diameter = 0.0
	density = 0.0
	distance = 0.0
	source = 0.0
	gamma_factor = 0.0
	w0 = 0.0
	w1 = 0.0
	resolution = 0.0
	tilt = 0.0
	aeff = 0.0
	beff = 0.0
	tail = {F:0.0, B:0.0, amp:0.0, L:0.0, S:0.0}
	use1 = 0
	use2 = 0
	special1 = {Tb:0.01, db:0.01, Rest:0.1}
	special2 = {c:1.0, w:0.1, h:0.1, c2:0.1, w2:0.1, h2:0.1}
	cohen = 1
	gamma = 0
	poly = fltarr(6)
	e_low = 0.1
	e_high = 4.0
	layout = ''
	array = 0
	shape = 0
	correct_solid_angle = 1

	if version le -5 then begin
		readu,1,gamma
		if gamma eq 1 then begin
			readu,1, crystal
			readu,1, poly
			readu,1, e_low, e_high

			if crystal.name eq 'Si' then begin
				w1 = 0.0033
				w0 = (1.9*1.9 - w1 * 1332.0) > 0.001
			endif else if crystal.name eq 'Ge' then begin
				w1 = 0.002089
				w0 = (1.9*1.9 - w1 * 1332.0) > 0.001
			endif else begin
				w1 = 0.0033
				w0 = (1.9*1.9 - w1 * 1332.0) > 0.001
			endelse

			error = 0
			close,1
			goto, ok
		endif
	endif

	max_n = 100
	n = 0L
	readu,1, n
	if (n lt 0) or (n gt max_n) then goto, bad_io

	if n ge 1 then begin
		if version le -7 then begin
			absorber = define(/filter)
			filters = replicate( absorber, n)
			readu,1, filters
		endif else if version le -2 then begin
			absorber = define(/old_filter2)
			rfilters = replicate( absorber, n)
			readu,1, rfilters
		endif else begin
			absorber = define(/old_filter1)
			rfilters = replicate( absorber, n)
			readu,1, rfilters
		endelse

		if version ge -6 then begin
			filter = define(/filter)
			filters = replicate( filter, n)

			if version le -2 then begin
				filters.microns = rfilters.microns
				filters.density = rfilters.density
				filters.formula = rfilters.formula
				filters.weight = rfilters.weight
			endif

			filters.n = rfilters.n
			filters.z = rfilters.z
			filters.f = rfilters.f
			filters.thick = rfilters.thick
			filters.pinhole = rfilters.pinhole
			filters.pinratio = rfilters.pinratio
			filters.name = rfilters.name

			for i=0L,n-1 do begin
				s = ''
				for j=0L,filters[i].n-1 do begin
					s = s + element_name(filters[i].z[j])
					if abs(filters[i].f[j]-1.0) gt 0.005 then begin
						s = s + string(filters[i].f[j])
					endif
				endfor
				filters[i].formula = strcompress(s,/remove_all)
			endfor
		endif
	endif

;	Note that this reads thick in mg/cm^2

	readu,1, crystal
	readu,1, diameter, density, distance, source
	readu,1, gamma_factor, w0,w1, resolution
	readu,1, aeff, beff
	readu,1, tail
	if version le -6 then begin
		readu,1, tilt
	endif

	readu,1, use1, use2
	if use1 then readu,1, special1
	if use2 then readu,1, special2

	if version le -3 then begin
		readu,1, cohen
	endif

	if version le -8 then begin
		readu,1, layout, array, shape
		if lenchr(layout) ne 0 then begin
			if file_test(layout) eq 0 then begin
				t = extract_path(f)+strip_path(layout)
				if file_test(t) eq 1 then begin
					layout = t
				endif
			endif
		endif
	endif

	if version le -9 then begin
		readu,1, correct_solid_angle
	endif

	error = 0
	close,1

OK:
	det = {absorbers:filters, crystal:crystal, diameter:diameter, $
			density:density, distance:distance, source:source, aeff:aeff, $
			beff:beff, gamma:gamma_factor, w0:w0, w1:w1, tail:tail, resolution:resolution, $
			tilt:tilt, use_special1:use1, use_special2:use2, correct_solid_angle:correct_solid_angle, $
			special1:special1, special2:special2, cohen:cohen, pige:gamma, a:poly, $
			e_low:e_low, e_high:e_high, file:F, layout:layout, array:array, shape:shape }

	p = ptr_new( det, /no_copy)
	return, p

bad_io:
	print,'Read_detector: bad detector I/O'
	error = 1
	return, 0
end

