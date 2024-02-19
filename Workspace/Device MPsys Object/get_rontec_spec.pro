	pro get_rontec_spec, p, name
;
;	Read in a spectrum, from the Rontec system

	obj = obj_new('MPSYS_DEVICE')
	if obj_valid(obj) eq 0 then goto, err

	on_ioerror,err
	openr,unit,name,/get_lun

	line = ''
more:
	readf, unit, line
	print,line
	if strmid(line,0,5) ne 'Kanal' then goto, more
	print,'found Kanal'
	readf, unit, line

	t = replicate( {channel:-100L, count:0L, energy:0.0}, 8192)

	on_ioerror, process
	readf, unit, t

process:
	close, unit
	free_lun,unit
	on_ioerror, null

	q = where( t.channel ne -100)
	siz = max(q)+1
    ex = fix(alog( siz>1)/alog(2) + 1.00001)
	siz = (2^ex) < 8192
	e = t[0:siz-1].energy
	c = t[0:siz-1].count

	spec2 = define(/spectrum)
	spec2.file = name
	spec2.source = name
	spec2.DevObj = clone_device_object(obj)
	spec2.label = name
	spec2.cal.order = 1
	spec2.cal.units = 'keV'
	spec2.cal.poly[0] = e[0]
	spec2.cal.poly[1] = (e[100] - e[0]) / 100.

	spec2.size = siz
	spec2.data = ptr_new( float(c) )

	p = ptr_new( spec2)
	return

err:
	warning,'get_Rontec_spec','error opening file: '+name
	p = 0L
	return
	end

