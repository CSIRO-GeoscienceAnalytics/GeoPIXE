	pro get_root_ascii_spec, p, name, group=group
;
;	Read in a ROOT dumped ascii spectrum
;
	COMPILE_OPT STRICTARR
	on_ioerror,err
	openr,unit,name,/get_lun

	obj = obj_new('NAC_XSYS_DEVICE')
	if obj_valid(obj) eq 0 then goto, err

	on_ioerror, cont
	title = ''
	data = lonarr(8192)
	line = ''
	readf, unit, title

	while NOT EOF(unit) do begin
		readf, unit, line
		str = strsplit( line, ' 	[]=,', /extract)
		ns = n_elements(str)
		i = fix2(str[1])
		if ns ge 3 and (i ge 0) and (i lt 8192) then begin
			data[ i] = long2(str[2])
		endif
	endwhile

cont:
	q = where( data gt 0, nq)
	if nq eq 0 then goto, err	
	siz = q[nq-1]+1
	data = data[0:siz-1]
	
	spec2 = define(/spectrum)
	spec2.file = name
	spec2.source = name
	spec2.DevObj = clone_device_object(obj)
	spec2.label = strtrim(title,2)

;	spec2.cal.order = 1
;	spec2.cal.units = 'keV'
;	spec2.cal.poly[0] = cb
;	spec2.cal.poly[1] = ca

;	spec2.charge = q							; uC

	spec2.size = siz
	spec2.data = ptr_new(data)

	p = ptr_new( spec2)

more:	close, unit
	free_lun, unit
	return

err:
	warning,'get_root_ascii_spec','error in MU file or OBJ.'
	return
	end

