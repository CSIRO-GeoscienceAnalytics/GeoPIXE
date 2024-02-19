pro get_ems_ascii_spec, p, name

;   Read in an ascii EMS spectrum file
;
;	Cater for:
;		1.	UQ iXRF EMS uses 2 columns with energy but NCOLUMNS=1
;		2.	Rio (MLA) EMS uses 4 columns with NCOLUMNS=4 for a single spectrum, no energy column

	COMPILE_OPT STRICTARR

    on_ioerror,err
    openr,unit,name,/get_lun

    str = strarr(16384)					; strarr(8192)
    on_ioerror, process
    readf, unit, str
    close_file, unit

process:
    on_ioerror, null
    q = where( str ne '', nq)

	title = ''
	siz = 0L
	ncolumns = 1
	cal_unit = ''
	cal_a = 0.
	cal_b = 0.
	live = 0.
	real = 0.
	version = '0.0'
	date = ''
	owner = ''
	title = ''
	datatype = 'X'
	
	has_energy = 0
    first = 1
	idata = -1
	done = 0
    j = 0
	while (strmid(str[j],0,1) eq '#') and (done eq 0) do begin
		s = strsplit( str[j], '#,: ', /extract)

		case s[0] of
			'VERSION': begin
				version = s[1]
				end
			'DATE': begin
				date = strjoin( s[1:*], ' ')
				end
			'OWNER': begin
				owner = strjoin( s[1:*], ' ')
				end
			'TITLE': begin
				title = strjoin( s[1:*], ' ')
				end
			'NPOINTS': begin
				siz = long2( float2(s[1]))
				end
			'NCOLUMNS': begin
				ncolumns = long2( float2(s[1]))
				end
			'DATATYPE': begin
				datatype = s[1]
				end
			'OFFSET': begin
				cal_b = float2(s[1])
				end
			'XPERCHAN': begin
				cal_a = float2(s[1])
				end
			'XUNITS': begin
				cal_unit = s[1]
				end
			'REALTIME': begin
				real = float2(s[1])
				end
			'LIVETIME': begin
				live = float2(s[1])
				end
			'SPECTRUM': begin
				done = 1
				idata = j+1
				end
			else:
		endcase
		j = j+1
		if j ge nq then goto, no_data
	endwhile
    if idata eq -1 then goto, no_data


    spec2 = define(/spectrum)
    spec2.file = name
    spec2.source = name
	spec2.label = title

	scale = 1.
	if cal_unit eq 'eV' then begin
		scale = 0.001
		cal_unit = 'keV'
	endif
	spec2.cal.units = cal_unit
	spec2.cal.order = 1
	spec2.cal.poly[0] = cal_b * scale
	spec2.cal.poly[1] = cal_a * scale
	spec2.ecal.units = cal_unit
	spec2.ecal.order = 1
	spec2.ecal.poly[0] = cal_b * scale
	spec2.ecal.poly[1] = cal_a * scale

;	spec2.filter = filter[i]
;	spec2.energy = energy[i]

;	spec2.ambient = ambient[i]
;	spec2.tube.volts = energy[i]
;	spec2.tube.current = current[i]
;	spec2.tube.time = real[i]

;	spec2.IC_total = real[i] * energy[i] * current[i] * 1.0e-3		; use model @ 1 W power
;	spec2.IC_total = real[i] * current[i]
;	spec2.IC_total = real[i]
	spec2.IC.mode = 2

;	spec2.sample = label[i]

	spec2.comment = date + ' ' + owner
	spec2.deadtime_correction = (live gt 0.) ? real/live : 1.0
	spec2.channel = 0
	spec2.station = 1

	n_channels = (nq-idata)*ncolumns + 1
	d = fltarr(n_channels)
	e = fltarr(n_channels)
	j = 0

	case datatype of
		'X': begin
			has_energy = 0
			end
		'XY': begin
			has_energy = 1
			end
		else:
	endcase

	for i=idata,16384-1 do begin
		if str[i] eq '' then goto, done

		s = strsplit( str[i], ', ', /extract, count=ns)
		if j eq 0 then begin
;			spec2.cal.poly[0] = float2(s[0]) * 1000.				; actually shown in keV
			
			if ns ne ncolumns+1 then begin
				print,'get_ems_ascii_spec: inconsistent number of columns.'
			endif
		endif

		if has_energy then e[j] = float2( s[0])
	
		for k=has_energy,ns-1 do begin
			d[j+k] = float2( s[k])
		endfor
		j = j + ncolumns
		if j ge n_channels-1 then break
	endfor

done:
	spec2.size = j
	spec2.data = ptr_new( d, /no_copy )
	p = ptr_new( spec2)

more:
    close_file,unit
    return

err:
    warning,'get_ems_ascii_spec','error opening file: '+name
    p = 0L
	goto, more
no_data:
    warning,'get_ems_ascii_spec','no data found'
    p = 0L
	goto, more
   end
