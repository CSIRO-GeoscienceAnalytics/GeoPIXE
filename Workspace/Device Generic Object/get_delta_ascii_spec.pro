    pro get_delta_ascii_spec, p, name

;   Read in an ascii CSV Delta spectrum file

	COMPILE_OPT STRICTARR
    on_ioerror,err
    openr,unit,name,/get_lun
    if n_elements(energy) lt 1 then energy=0
    if n_elements(channel) lt 1 then channel=0

    str = strarr(16384)					; strarr(8192)
    close, unit
    on_ioerror, process
    openr,unit,name
    readf,unit,str
    close_file,unit

process:
    on_ioerror, null
    q = where( str ne '')
    siz = max(q)+1

    head = strsplit( str[0], ',', /extract)
    nh = n_elements(head)-1
    label = strarr(nh)
    time = strarr(nh)
    offset = fltarr(nh)
	slope = fltarr(nh)
	live = fltarr(nh)
	real = fltarr(nh)
	energy = fltarr(nh)
	filter = intarr(nh)
	ambient = replicate( {on:0, P:0.0, T:0.0}, nh)
	p_found = intarr(nh)
	t_found = intarr(nh)
	current = fltarr(nh)

    first = 1
	done = 0
    j = 0
	while (strmid(str[j],0,1) ne ',') and (done eq 0) do begin
		s = strsplit( str[j], ',', /extract)
		if inumeric(s[0]) then begin
			done = 1
			continue
		endif
		case s[0] of
			'TestID': begin
				label = s[1:*]
				end
			'Offset': begin
				offset = float2(s[1:*])
				end
			'Slope': begin
				slope = float2(s[1:*])
				end
			'Livetime': begin
				live = float2(s[1:*])
				end
			'Realtime': begin
				real = float2(s[1:*])
				end
			'LiveTime': begin
				live = float2(s[1:*])
				end
			'RealTime': begin
				real = float2(s[1:*])
				end
			'TubeCurrentMon': begin
				current = float2(s[1:*])
				end
			'FilterPosition': begin
				filter = fix2(s[1:*])
				end
			'TimeStamp': begin
				time = s[1:*]
				end
			'TubeVoltageSet': begin
				energy = float2(s[1:*])
				end
			'VacPressure': begin
				ambient.P = float2(s[1:*])
				p_found = ambient.P gt 0.001
				end
			'ProbeTemp': begin
				ambient.T = float2(s[1:*])
				t_found = abs(ambient.T) gt 0.001
				end
			else:
		endcase
		j = j+1
		if j ge siz then goto, no_data
	endwhile

	ambient.on = t_found and p_found

	j0 = j
	buff = fltarr(nh,siz-j0)
	while j lt siz do begin
		s = strsplit( str[j], ',', /extract, /preserve_null)
		buff[*,j-j0] = float2(s[1:nh])
		j = j+1
	endwhile
	
    spec2 = define(/spectrum)
    spec2.file = name
    spec2.source = name

    p = ptrarr(nh)
    for i=0,nh-1 do begin
		spec2.cal.units = 'keV'
		spec2.cal.order = 1
		spec2.cal.poly[0] = offset[i]
		spec2.cal.poly[1] = slope[i]
		spec2.label = label[i]
		spec2.filter = filter[i]
		spec2.energy = energy[i]

		spec2.ambient = ambient[i]
		spec2.tube.volts = energy[i]
		spec2.tube.current = current[i]
		spec2.tube.time = real[i]

		spec2.IC_total = real[i] * energy[i] * current[i] * 1.0e-3		; use model @ 1 W power
;		spec2.IC_total = real[i] * current[i]
;		spec2.IC_total = real[i]
		spec2.IC.mode = 2

		spec2.sample = label[i]
		spec2.comment = str_tidy(energy[i]) + ' kV - ' + time[i]
		spec2.deadtime_correction = (live[i] gt 0.) ? real[i]/live[i] : 1.0
		
		d = reform(buff[i,*])
		spec2.size = n_elements(d)
		spec2.data = ptr_new( d, /no_copy )
		p[i] = ptr_new( spec2)
    endfor

more:
    close_file,unit
    return

err:
    warning,'get_delta_ascii_spec','error opening file: '+name
    p = 0L
	goto, more
no_data:
    warning,'get_delta_ascii_spec','no data found'
    p = 0L
	goto, more
    end

