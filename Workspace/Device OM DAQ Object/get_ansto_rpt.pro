	pro get_ansto_rpt, p, name
;
;	Read in a ANSTO RPT spectrum file
;
COMPILE_OPT STRICTARR
	on_ioerror,err
	openr,unit,name,/get_lun

	on_ioerror,cont
	title = ''
	readf, unit, title

	parameters = intarr(2)
	readf, unit, parameters

	siz = round(parameters[1])					; # channels

	buff = fltarr(siz)
	readf,unit,buff

	obj = obj_new('OM_DAQ_DEVICE')
	if obj_valid(obj) eq 0 then goto, err

cont:
	spec2 = define(/spectrum)
	spec2.file = name
	spec2.source = name
	spec2.DevObj = clone_device_object(obj)
	spec2.label = strtrim(title,2)

	spec2.cal.order = 1
	spec2.cal.units = 'channel'
	spec2.cal.poly[0] = 0.0
	spec2.cal.poly[1] = 1.0

	spec2.size = siz
	spec2.data = ptr_new(buff)

	p = ptr_new( spec2)

more:	
	close_file, unit
	return

err:
	warning,'get_ansto_rpt','error in RPT file.'
	goto, more
	end

