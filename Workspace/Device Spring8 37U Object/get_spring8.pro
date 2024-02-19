	function get_spring8, file1, error=error
;
;	Read in mdaq info file for run details.
;	If not found, then return error=1.
;
	COMPILE_OPT STRICTARR
	common c_geopixe_adcs, geopixe_max_adcs
	if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=8

	error = 1
	mp = -1
	if n_elements(file1) lt 1 then return,0
	if lenchr(file1) lt 1 then return,0

	file = strip_file_ext( file1)
	file = strip_file_m( file, ending = ['_ch0','_ch1','_ch2','_ch3','_ch4'])
	file = strip_file_m( file, ending = '_mca')

	F = file + '.mdaq'
	on_ioerror,more
	openr, unit, F, /get_lun

	mp = 0

;	These details need to match those in get_header_info

	id = ''
	date = ''
	time = ''
	name = ''
	comment = ''
	user = ''
	charge = 0.0
	base_charge = 0.1
	clock = 80.0
	deadtime_mode = ''
	n_energy = 0
	
;	scan: origin:x,y, size(mm):x_mm,y_mm, pixels:x_pixels,y_pixels

    scan = {on:0, x_pixels:0, y_pixels:0, z_pixels:0, x:0.0, y:0.0, z:0.0, x_mm:0.0, y_mm:0.0, z_mm:0.0, $
          mode:0, n_steps:0, step_size:0.0, sort_mode:0, dwell:0.0, clock:0.0, use_ylut:0, $
          pYlut:ptr_new(/allocate_heap) }
	detector_name = strarr(geopixe_max_adcs)
	detector = replicate(-1,geopixe_max_adcs)
	cal = replicate( {on:0, a:1.0, b:0.0, units:'channel'}, geopixe_max_adcs)
	
	line = ''

	on_ioerror,err
	while EOF(unit) eq 0 do begin
		readf, unit, line
		if (lenchr(line) gt 0) and (extract(line,0,0) ne '#') then begin

			str = strsplit( line, ' 	', /extract)
			n_str = n_elements(str)
			
			if (str[0] eq 'set') and (n_str gt 2) then begin

				str2 = strsplit( str[1], '.', /extract)
				n_str2 = n_elements(str2)

				case str2[0] of
					'run': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'id': begin
;									id = fix2(str[2])
									id = str[2]
									end
								'name': begin
									name = str[2]
									end
								'date': begin
									date = strcompress(strjoin(str[2:*],' '))
									end
								'comment': begin
									comment = strcompress(strjoin(str[2:*],' '))
									end
								'user': begin
									user = str[2]
									end
								else:
							endcase
						endif
						end
					'integrated_charge': begin
						charge = float(str[2])
						end
					'fpga_clock': begin
						clock = float(str[2])
						end
					'base_charge': begin
						base_charge = float(str[2])
						end
					'scan': begin
						if n_str2 ge 2 then begin
							scan.on = 1
							case str2[1] of
								'xpixels': begin
									scan.x_pixels = fix2(str[2])
									end
								'ypixels': begin
									scan.y_pixels = fix2(str[2])
									end
								'zpixels': begin
									scan.z_pixels = fix2(str[2])
									end
								'width': begin
									scan.x_mm = float2(str[2]) * 0.001
									end
								'height': begin
									scan.y_mm = float2(str[2]) * 0.001
									end
								'depth': begin
									scan.z_mm = float2(str[2]) * 0.001
									end
								'xorigin': begin
									scan.x = float2(str[2]) * 0.001
									end
								'yorigin': begin
									scan.y = float2(str[2]) * 0.001
									end
								'zorigin': begin
									scan.z = float2(str[2]) * 0.001
									end
								else:
							endcase
						endif
						end
					'st0': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[0] = str[2]
								end
								'energy_cal_a': begin
									cal[0].on = 1
									cal[0].units = 'keV'
									cal[0].a = float2(str[2])
								end
								'energy_cal_b': begin
									cal[0].on = 1
									cal[0].b = float2(str[2])
								end
								'channels': begin
									n_energy = long2(str[2])
									end
								else:
							endcase
						endif
						end
					'st1': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[1] = str[2]
									end
								'energy_cal_a': begin
									cal[1].on = 1
									cal[1].units = 'keV'
									cal[1].a = float2(str[2])
									end
								'energy_cal_b': begin
									cal[1].on = 1
									cal[1].b = float2(str[2])
									end
								'channels': begin
									n_energy = long2(str[2])
									end
								else:
							endcase
						endif
						end
					'st2': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[2] = str[2]
									end
								'energy_cal_a': begin
									cal[2].on = 1
									cal[2].units = 'keV'
									cal[2].a = float2(str[2])
									end
								'energy_cal_b': begin
									cal[2].on = 1
									cal[2].b = float2(str[2])
									end
								else:
							endcase
						endif
						end
					'st3': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[3] = str[2]
									end
								'energy_cal_a': begin
									cal[3].on = 1
									cal[3].units = 'keV'
									cal[3].a = float2(str[2])
									end
								'energy_cal_b': begin
									cal[3].on = 1
									cal[3].b = float2(str[2])
									end
								else:
							endcase
						endif
						end
					'st4': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[4] = str[2]
									end
								'energy_cal_a': begin
									cal[4].on = 1
									cal[4].units = 'keV'
									cal[4].a = float2(str[2])
									end
								'energy_cal_b': begin
									cal[4].on = 1
									cal[4].b = float2(str[2])
									end
								else:
							endcase
						endif
						end
					'st5': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[5] = str[2]
									end
								'energy_cal_a': begin
									cal[5].on = 1
									cal[5].units = 'keV'
									cal[5].a = float2(str[2])
									end
								'energy_cal_b': begin
									cal[5].on = 1
									cal[5].b = float2(str[2])
									end
								else:
							endcase
						endif
						end
					'st6': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[6] = str[2]
									end
								'energy_cal_a': begin
									cal[6].on = 1
									cal[6].units = 'keV'
									cal[6].a = float2(str[2])
									end
								'energy_cal_b': begin
									cal[6].on = 1
									cal[6].b = float2(str[2])
									end
								else:
							endcase
						endif
						end
					'st7': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[7] = str[2]
									end
								'energy_cal_a': begin
									cal[7].on = 1
									cal[7].units = 'keV'
									cal[7].a = float2(str[2])
									end
								'energy_cal_b': begin
									cal[7].on = 1
									cal[7].b = float2(str[2])
									end
								else:
							endcase
						endif
						end
					'st8': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[8] = str[2]
									end
								'energy_cal_a': begin
									cal[8].on = 1
									cal[8].units = 'keV'
									cal[8].a = float2(str[2])
									end
								'energy_cal_b': begin
									cal[8].on = 1
									cal[8].b = float2(str[2])
									end
								else:
							endcase
						endif
						end
					else:
				endcase
			endif
		endif
	endwhile
	on_ioerror,null

	if lenchr(comment) eq 0 then begin
		if (lenchr(name) gt 0) then begin
			comment = name + ' (' + str_tidy(id) + '), ' + date + ' ' + time
		endif else if (lenchr(id) gt 0) then begin
			comment = str_tidy(id) + ':' + date + ' ' + time
		endif else begin
			comment = date + ' ' + time
		endelse
	endif

	det_types = strupcase(detector_types())
	for i=0L,geopixe_max_adcs-1 do begin
		q = where( strupcase(detector_name[i]) eq det_types, nq)
		if nq gt 0 then detector[i] = q[0]
	endfor

;	These details need to match those in get_header_info

	error = 0

	mp = {	comment:	comment, $			; comment string
			id:			id, $				; ID number
			date:		date, $				; date
			user:		user, $				; user name
			name:		name, $				; run name, point name
			scan:		scan, $				; scan parameters
			deadtime:	deadtime_mode, $	; deadtime mode
			charge:		charge, $			; integrated charge (uC)
			clock:		clock, $			; FPGA clock (MHz)
			charge_tick: base_charge, $		; charge per count (pC)
			detector:	detector, $			; detector type codes
			cal:		cal, $				; station E cal parameters
			n_energy:	n_energy, $			; number of E channels
			error:		error $				; error return
	}

more:
	close_file, unit
	return, mp

err:
	warning, 'get_mdaq2', ['Serious error in MDAQ file.','Ignoring MDAQ file settings.']
	goto, more
	end

