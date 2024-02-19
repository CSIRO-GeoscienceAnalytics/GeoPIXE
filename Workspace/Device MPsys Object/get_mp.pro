	function get_mp, file, error=error
;
;	Read in an MPsys MP file for run details.
;	If not found, then return error=1.
;
	COMPILE_OPT STRICTARR
	common c_prefs_scan, prefs_XY_scan, prefs_X_step, prefs_Y_step, prefs_Resolution
	common c_geopixe_adcs, geopixe_max_adcs
	if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=8

	if n_elements(prefs_XY_scan) lt 1 then prefs_XY_scan = {X:100.0, Y:100.0}
	if n_elements(prefs_X_step) lt 1 then prefs_X_step = {Y:2000.0}
	if n_elements(prefs_Y_step) lt 1 then prefs_Y_step = {X:640.0}
	if n_elements(prefs_Resolution) lt 1 then prefs_Resolution = {X:0.635, Y:0.08333333}

	error = 1
	mp = 0
	if n_elements(file) lt 1 then return,0
	if lenchr(file) lt 1 then return,0

	F = strip_file_ext(file) + '.mp'
	on_ioerror,more
	openr,unit,F,/get_lun

	mp = 0

;	These details need to match those in get_header_info

	id = ''
	date = ''
	time = ''
	name = ''
	comment = ''
	charge = 0.0
	deadtime_mode = ''
    scan = {on:0, x:0.0, y:0.0, scale:0.0, x_pixels:0, y_pixels:0, x_mm:0.0, y_mm:0.0, $
          mode:0, n_steps:0, step_size:0.0, sort_mode:0, dwell:0.0, $
          pYlut:ptr_new(/allocate_heap) }
	detector_name = strarr(geopixe_max_adcs)
	detector = replicate(-1,geopixe_max_adcs)
	cal = replicate( {on:0, a:0.0, b:0.0, units:''}, geopixe_max_adcs)

	line = ''

	on_ioerror,err
	while EOF(unit) eq 0 do begin
		readf, unit, line
		if (lenchr(line) gt 0) and (extract(line,0,0) ne '#') then begin

			set_separators, ' 	'
			chop_string, line, str, n_str

			if (str[0] eq 'set') and (n_str gt 2) then begin

				set_separators, '.'
				chop_string, str[1], str2, n_str2

				case str2[0] of
					'run_id': begin
						id = str[2]
						end
					'run_date': begin
						date = strcompress(strjoin(str[2:*],' '))
						end
					'run_time': begin
						time = str[2]
						end
					'run_name': begin
						name = str[2]
						end
					'integrated_charge': begin
						charge = float(str[2])
						end
					'deadtime': begin
						if (n_str2 ge 2) then begin
							case str2[1] of
								'mode': begin
									deadtime_mode = str[2]
									end
								else:
							endcase
						endif
						end
					'scan': begin
						if n_str2 ge 2 then begin
							scan.on = 1
							case str2[1] of
								'x_resolution': begin
									scan.x = fix(str[2])
									end
								'y_resolution': begin
									scan.y = fix(str[2])
									end
								'scale': begin
									scan.scale = float(str[2])
									end
								else:
							endcase
						endif
						end
					'st1': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[0] = str[2]
									end
								'calibrated_e': begin
									cal[0].on = 1
									end
								'energy_cal_a': begin
									cal[0].a = float(str[2])
									end
								'energy_cal_b': begin
									cal[0].b = float(str[2])
									end
								'energy_cal_label': begin
									cal[0].units = str[2]
									end
								else:
							endcase
						endif
						end
					'st2': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[1] = str[2]
									end
								'calibrated_e': begin
									cal[1].on = 1
									end
								'energy_cal_a': begin
									cal[1].a = float(str[2])
									end
								'energy_cal_b': begin
									cal[1].b = float(str[2])
									end
								'energy_cal_label': begin
									cal[1].units = str[2]
									end
								else:
							endcase
						endif
						end
					'st3': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[2] = str[2]
									end
								'calibrated_e': begin
									cal[2].on = 1
									end
								'energy_cal_a': begin
									cal[2].a = float(str[2])
									end
								'energy_cal_b': begin
									cal[2].b = float(str[2])
									end
								'energy_cal_label': begin
									cal[2].units = str[2]
									end
								else:
							endcase
						endif
						end
					'st4': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[3] = str[2]
									end
								'calibrated_e': begin
									cal[3].on = 1
									end
								'energy_cal_a': begin
									cal[3].a = float(str[2])
									end
								'energy_cal_b': begin
									cal[3].b = float(str[2])
									end
								'energy_cal_label': begin
									cal[3].units = str[2]
									end
								else:
							endcase
						endif
						end
					'st5': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[4] = str[2]
									end
								'calibrated_e': begin
									cal[4].on = 1
									end
								'energy_cal_a': begin
									cal[4].a = float(str[2])
									end
								'energy_cal_b': begin
									cal[4].b = float(str[2])
									end
								'energy_cal_label': begin
									cal[4].units = str[2]
									end
								else:
							endcase
						endif
						end
					'st6': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[5] = str[2]
									end
								'calibrated_e': begin
									cal[5].on = 1
									end
								'energy_cal_a': begin
									cal[5].a = float(str[2])
									end
								'energy_cal_b': begin
									cal[5].b = float(str[2])
									end
								'energy_cal_label': begin
									cal[5].units = str[2]
									end
								else:
							endcase
						endif
						end
					'st7': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[6] = str[2]
									end
								'calibrated_e': begin
									cal[6].on = 1
									end
								'energy_cal_a': begin
									cal[6].a = float(str[2])
									end
								'energy_cal_b': begin
									cal[6].b = float(str[2])
									end
								'energy_cal_label': begin
									cal[6].units = str[2]
									end
								else:
							endcase
						endif
						end
					'st8': begin
						if n_str2 ge 2 then begin
							case str2[1] of
								'detector_name': begin
									detector_name[7] = str[2]
									end
								'calibrated_e': begin
									cal[7].on = 1
									end
								'energy_cal_a': begin
									cal[7].a = float(str[2])
									end
								'energy_cal_b': begin
									cal[7].b = float(str[2])
									end
								'energy_cal_label': begin
									cal[7].units = str[2]
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

	if (lenchr(name) gt 0) then begin
		comment = name + ' (' + id + '), ' + date + ' ' + time
	endif else if (lenchr(id) gt 0) then begin
		comment = id + ':' + date + ' ' + time
	endif else begin
		comment = date + ' ' + time
	endelse
	if scan.on then begin
		if ((scan.x eq 1) and (scan.y eq 1)) or (scan.scale lt 0.01) then begin
			scan.sort_mode = 1									; Traverse
		endif else begin
			if scan.x eq 1 then begin							; X step
				scan.mode = 1

;				Use the preferences value for the max Y scan in X-step mode here instead of 2.0.
;				and the value of size of single step instead of 0.635.

				scan.y_mm = scan.scale * prefs_X_step.Y / 1000.
				scan.y_pixels = scan.y
				scan.step_size = float( round(scan.y_mm * 1000. / (float(scan.y_pixels)*prefs_resolution.X)) )*prefs_resolution.X
				scan.n_steps = round(scan.step_size / prefs_resolution.X)
				scan.y_mm = scan.y_pixels * scan.step_size/1000.
				scan.x_mm = scan.y_mm
				scan.x_pixels = scan.y_pixels
			endif else if scan.y eq 1 then begin				; Y step
				scan.mode = 3

;				Use the preferences value for the max X scan in Y-step mode here instead of 0.64.
;				and the value of size of single step instead of 1./12.

				scan.x_mm = scan.scale * prefs_Y_step.X / 1000.
				scan.x_pixels = scan.x
				scan.step_size = float( round(scan.x_mm * 1000. / (float(scan.x_pixels)*prefs_resolution.Y)) )*prefs_resolution.Y
				scan.n_steps = round(scan.step_size / prefs_resolution.Y)
				scan.x_mm = scan.x_pixels * scan.step_size/1000.
				scan.y_mm = scan.x_mm
				scan.y_pixels = scan.x_pixels
			endif else begin									; XY raster
				scan.mode = 0
				scan.step_size = 0.0
				scan.n_steps = 0

;				Use the preferences value for the max X,Y scans in XY scan mode here instead of 0.1.

				if scan.x ge scan.y then begin
					scan.x_mm = scan.scale * prefs_XY_scan.X / 1000.
					scan.y_mm = scan.x_mm * float(scan.y)/float(scan.x)
				endif else begin
					scan.y_mm = scan.scale * prefs_XY_scan.Y / 1000.
					scan.x_mm = scan.y_mm * float(scan.x)/float(scan.y)
				endelse
				scan.x_pixels = scan.x
				scan.y_pixels = scan.y
			endelse
		endelse
	endif

	det_types = strupcase(detector_types())
	for i=0L,geopixe_max_adcs-1 do begin
		q = where( strupcase(detector_name[i]) eq det_types)
		if q[0] ne -1 then detector[i] = q[0]
	endfor

;	These details need to match those in get_header_info

	mp = {	comment:	comment, $			; comment string
			id:			fix(id), $			; ID number
			scan:		scan, $				; scan parameters
			deadtime:	deadtime_mode, $	; deadtime mode
			charge:		charge, $			; integrated charge (uC)
			detector:	detector, $			; detector type codes
			cal:		cal, $				; station E cal parameters
			error:		error $				; error return
	}

	error = 0

more:
	close_file, unit
	return, mp

err:
	warning, 'get_mp', ['Serious error in MP file.','Ignoring settings.','',$
			'Check file format, and be sure to','use ASCII mode for FTP transfers.']
	goto, more
	end

