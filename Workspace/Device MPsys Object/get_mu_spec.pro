	pro get_mu_spec, p, name
;
;	Read in a Melbourne Uni ascii spectrum
;
	on_ioerror,err
	openr,unit,name,/get_lun

	obj = obj_new('MPSYS_DEVICE')
	if obj_valid(obj) eq 0 then goto, err

	on_ioerror,cont
	title = ''
	readf, unit, title

	parameters = fltarr(10)
	readf, unit, parameters

	siz = round(parameters[0])					; # channels
	first = round(parameters[1])				; first channel
	ca = parameters[2]							; cal a
	cb = parameters[3]							; cal b
	frac = parameters[4]						; 1/fraction of area
	bmass = parameters[5]						; beam mass
	e_beam = parameters[6]
	theta = parameters[7]
	omega = parameters[8]
	q = parameters[9]							; integrated charge (nC?)

	q = q/frac

	buff = fltarr(siz)
	readf,unit,buff
;
cont:
	spec2 = define(/spectrum)
	spec2.file = name
	spec2.source = name
	spec2.DevObj = clone_device_object(obj)
	spec2.label = strtrim(title,2)

	spec2.cal.order = 1
	spec2.cal.units = 'keV'
	spec2.cal.poly[0] = cb
	spec2.cal.poly[1] = ca

	spec2.charge = q							; uC

	spec2.size = siz
	spec2.data = ptr_new(buff)

	p = ptr_new( spec2)

more:	close,unit
	free_lun,unit
	return

err:
	warning,'get_mu_spec','error in MU file or OBJ.'
	return
	end

