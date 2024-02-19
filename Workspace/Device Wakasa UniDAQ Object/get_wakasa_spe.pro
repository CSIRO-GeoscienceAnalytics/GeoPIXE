	pro get_wakasa_spe, p, name
;
;	Read in a Wakasa Ortec Maestro SPE ascii spectrum
;
	on_ioerror,err
	openr,unit,name,/get_lun

	charge = 0.0
	ca = 1.0
	cb = 0.0
	cq = 0.0
	live_time = 0.0
	real_time = 0.0
	nd = 1									; what about multiple detectors?
	n = 2048
	date = ''
	time = ''
	id = ''

	on_ioerror,cont
	line = ''

more:
	readf, unit, line
	if strmid(line,0,1) eq "!" then goto, more
	c0 = strmid(line,0,1)
	if c0 ne '$' then goto, more
new:
	set_separators, ' :'
	chop_string, line, s, ns
	if ns lt 1 then goto, more

	case s[0] of
		'$SPEC_ID': begin
			readf, unit, line
			id = strtrim(line,2)
			end
		'$SPEC_REM': begin
			repeat begin
				readf, unit, line
				c0 = strmid(line,0,1)
				if c0 ne '$' then begin
					rem = (n_elements(rem) lt 1) ? strtrim(line,2) : rem + ', ' + strtrim(line,2)
				endif
			endrep until c0 eq '$'
			goto, new
			end
		'$DATE_MEA': begin
			readf, unit, line
			set_separators, ' '
			chop_string, line, s, ns
			if ns lt 2 then goto, more
			date = strtrim(s[0],2)
			time = strtrim(s[1],2)
			end
		'$MEAS_TIM': begin
			readf, unit, line
			chop_string, line, s, ns
			if ns lt 2 then goto, more
			live_time = float(s[0])
			real_time = float(s[1])
			end
		'$DATA': begin
			readf, unit, line
			chop_string, line, s, ns
			if ns lt 2 then goto, more
			n = long(s[1])
			buff = fltarr(nd,n)						; what about multiple detectors?
			readf, unit, buff
			end
		'$MCA_CAL': begin
			readf, unit, line
			chop_string, line, s, ns
			if ns lt 1 then goto, more
			nc = long(s[0])
			readf, unit, line
			chop_string, line, s, ns
			if ns lt (nc>2) then goto, more
			cb = float(s[0])
			ca = float(s[1])
			if ns ge 3 then cq = float(s[2])
			end
		else:
	endcase
	goto, more

cont:
	nt = intarr(nd)
	for i=0L,nd-1 do begin
		q = where(buff[i,*] ne 0)
		nt[i] = (max(q)+1) > 32
	endfor
	q = where(nt gt 1)
	nd = 0
	if q[0] ne -1 then nd=n_elements(q)
	if nd lt 1 then goto, err

;	charge = float(ic_count)

	obj = obj_new('WAKASA_UNIDAQ_DEVICE')
	if obj_valid(obj) eq 0 then goto, err

	spec2 = define(/spectrum)
	if nd gt 1 then spec2 = replicate(spec2,nd)
	p = ptrarr(nd)

	for i=0L,nd-1 do begin
		spec2[i].file = name
		spec2[i].source = name
		spec2[i].DevObj = clone_device_object(obj)
		spec2[i].label = strip_path(name)
		spec2[i].channel = q[i]
		spec2[i].station = q[i]+1
		spec2[i].comment = date + ' ' + time + ': ' + rem
		spec2[i].sample = id

		spec2[i].cal.order = 1
		spec2[i].cal.units = 'keV'
		spec2[i].cal.poly[0] = cb
		spec2[i].cal.poly[1] = ca
		if abs(cq) gt 1.0e-10 then begin
			spec2[i].cal.order = 2
			spec2[i].cal.poly[2] = cq
		endif
		if live_time gt 1.0e-6 then begin
			spec2[i].deadtime_correction = real_time/live_time
		endif
		spec2[i].charge = charge * spec2[i].deadtime_correction

		spec2[i].size = nt[i]
		spec2[i].data = ptr_new(reform(buff[0:nt[i]-1]))

		p[i] = ptr_new( spec2[i])
	endfor

finish:
	close_file,unit
	return

err:
	warning,'get_wakasa_spe','error in SPE file.'
	p = 0L
	goto, finish
	end

