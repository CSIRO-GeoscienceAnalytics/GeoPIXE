	pro get_hasylab_fio, p, name
;
;	Read in a Hasylab FIO ascii spectrum
;
	on_ioerror,err
	openr,unit,name,/get_lun

	IC_total = 0.0
	ca = 1.0
	cb = 0.0
	cq = 0.0
	live_time = 0.0
	real_time = 0.0
	nd = 1
	n = 2048
	date = ''
	time = ''
	a1_val = 0.0
	a1_unit = 'na/v'
	sgain2 = ''
	sgain2 = ''
	sgain3 = ''
	ic_count = 0L
	ic_count2 = 0L

	units = ['pa/v','na/v','ua/v','ma/v']
	vals = [0.001,1.0,1000.0,1000000.0]

	on_ioerror,cont
	line = ''

more:
	readf, unit, line
	line = strlowcase(line)
	if strmid(line,0,1) eq "!" then goto, more
	set_separators, '	 ='
	chop_string, line, s, ns

	if ns lt 1 then goto, more
	if s[0] eq '%d' then goto, read_data
	if ns lt 2 then goto, more

	case s[0] of
		'live': begin
			live_time = float(s[1:1+nd-1])
			end
		'real': begin
			real_time = float(s[1:1+nd-1])
			end
;		'cal_offset': begin
;			cb = float(s[1:1+nd-1])
;			end
;		'cal_slope': begin
;			ca = float(s[1:1+nd-1])
;			end
;		'channels': begin
;			n = fix(s[1])
;			end
		'date': begin
			date = s[1]
			end
		'time': begin
			time = s[1]
			end
		'monitor_counts': begin
			ic_count = long(s[1:1+nd-1])
			end
		'monitor_counts2': begin
			ic_count2 = long(s[1:1+nd-1])
			end
		'gain': begin
			case s[1] of
				'keithley1': begin
					sgain1 = s[2]
					end
				'keithley2': begin
					sgain2 = s[2]
					end
				'keithley3': begin
					sgain3 = s[2]
					end
				else:
			endcase
			end
		else:
	endcase
	goto, more

read_data:
	readf, unit, line
	if (n lt 1) or (nd lt 1) then goto, cont
	buff = fltarr(nd,n)
	readf, unit, buff

cont:
	IC_total = float(ic_count)			; monitor_counts
;	IC_total = float(ic_count2)			; monitor_counts2

	nt = intarr(nd)
	for i=0L,nd-1 do begin
		q = where(buff[i,*] ne 0)
		nt[i] = (max(q)+1) > 32
	endfor
	q = where(nt gt 1)
	nd = 0
	if q[0] ne -1 then nd=n_elements(q)
	if nd lt 1 then goto, err

	spec2 = define(/spectrum)
	obj = obj_new('HASYLAB_FIO_DEVICE')
	if nd gt 1 then spec2 = replicate(spec2,nd)
	p = ptrarr(nd)

	for i=0L,nd-1 do begin
		spec2.file = name
		spec2[i].source = name
		spec2[i].DevObj = clone_device_object(obj)
		spec2[i].label = strip_path(name)
		spec2[i].channel = q[i]
		spec2[i].station = q[i]+1

		spec2[i].cal.order = 1
		spec2[i].cal.units = 'keV'
		spec2[i].cal.poly[0] = cb
		spec2[i].cal.poly[1] = ca
;		spec2[i].cal.poly[0] = cb[q[i]]
;		spec2[i].cal.poly[1] = ca[q[i]]
;		if abs(cq[q[i]]) gt 1.0e-10 then begin
;			spec2[i].cal.order = 2
;			spec2[i].cal.poly[2] = cq[q[i]]
;		endif
		if live_time[q[i]] gt 1.0e-6 then begin
			spec2[i].deadtime_correction = real_time[q[i]]/live_time[q[i]]
		endif
		spec2[i].IC_total = IC_total[q[i]]

		spec2[i].size = nt[i]
		spec2[i].data = ptr_new(reform(buff[q[i],0:nt[i]-1]))

		p[i] = ptr_new( spec2[i])
	endfor

finish:	close,unit
	free_lun,unit
	return

err:
	warning,'get_hasylab_fio','error in FIO file.'
	p = 0L
	goto, finish
	end

