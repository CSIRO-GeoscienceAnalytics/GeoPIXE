	pro get_lund_spec, p, name, group=group
;
;	Read in a Lund KMax (MAC) binary spectrum
;
COMPILE_OPT STRICTARR
	p = 0L
	
	obj = obj_new('LUND_KMAX_DEVICE')
	if obj_valid(obj) eq 0 then goto, err

	
	on_ioerror,err
	openr,unit,name,/get_lun
	on_ioerror,cont

	lund_header,header
	READU, unit, header

	swap_bytes, header, /big_endian_data	; swap bytes on a little endian machine
;	help,header,/struct

	q = where((header.btitle ge 32) and (header.btitle le 126))
	title = string(header.btitle[q])
;	print,'btitle=',header.btitle
;	print,'title=',title

	if header.DataYLength ge 2 then goto, err_2D

	spectrum = lonarr(header.DataXLength)
	readu,unit,spectrum

	swap_bytes, spectrum, /big_endian_data	; swap bytes on a little endian machine

cont:
	spec2 = define(/spectrum)
	spec2.file = name
	spec2.source = name
	spec2.DevObj = clone_device_object(obj)
	spec2.label = strtrim(title,2)

	spec2.cal.order = 1
	spec2.cal.units = string(header.XUnits)
	spec2.cal.poly[0] = header.XOffset
	spec2.cal.poly[1] = header.XGain

	lq = lund_charge( path=extract_path(name), group_leader=group)				; uC
	spec2.charge = lq.charge
	spec2.ecompress = lq.ecompress
	spectrum[0:5] = 0.0

	spec2.size = header.DataXLength
	spec2.data = ptr_new(float(spectrum))

	p = ptr_new( spec2)

more:	close,unit
	free_lun,unit
	return

err:
	warning,'get_lund_spec','error in MU file or bad obj.'
	goto, more
err_2D:
	warning,'get_lund_spec',['HIST file is 2D.','Not a spectrum file.']
	goto, more
	end

