pro aps_to_list, filei, group=group

;	Read an APS .mda data cube file and write it out
;	as a .lst list-mode style file
;
;	For multi-file select, assumes that all use same IC, sensitivity, ELT settings

COMPILE_OPT STRICTARR
common c_aps_last, last
common c_errors_1, catch_errors_on

ErrorNo = 0
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'aps_to_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(filei) lt 1 then filei=''
	file = filei

	if (strlen(file[0]) lt 1) then begin
		if n_elements(last) ge 1 then begin
			path = extract_path(last[0])
		endif else begin
			path = ''
		endelse

		file = file_requester( /read, path=path, $
				title='Select MDA file(s) to translate', filter='*.mda', $
				/fix_filter, /multiple_files)
	endif else begin
		path = extract_path( file[0])
		if path eq file[0] then begin
			file = file_requester( /read, path=path, file=file[0], $
				title='Select MDA file(s) to translate', filter='*.mda', $
				/fix_filter, /multiple_files)
		endif
	endelse
	if strlen(file[0]) lt 1 then return
	last = file[0]
	jfile = 0
	nfile = n_elements(file)
	flux_scale = 1.
	if n_elements(group) lt 1 then begin
		local_group = 1
		group = widget_base(xsize=1,ysize=1)
	endif else local_group=0

loop:
	on_ioerror, bad_file
	close, 1
	openw, 1, strip_file_ext(file[jfile]) + '.lst'				; should this use /XDR (or is this slow)?

	filename = strip_path(file[jfile])
	path = extract_path(file[jfile])

;	First read MDA only for header info ...

	widget_control, /hour

    sv_read_scan2, $
               filename = filename, $							; Inputs
               output_directory = path, $
               input_directory = path, $
;               /show_extra_pvs, $
               show_extra_pvs=0, $

               scan_name = scan_name, $							; Outputs
               scan_time_stamp = scan_time_stamp, $
               mca_calib_arr = mca_calib_arr, $
               mca_calib_name_arr = mca_calib_name_arr, $
               mca_calib_description_arr = mca_calib_description_arr, $
               y_coord_arr = y_coord_arr, $
               x_coord_arr = x_coord_arr, $
               x_coord_units = x_coord_units, $
               y_coord_units = y_coord_units, $
               y_pixels = y_pixels, $
               x_pixels = x_pixels, $
               detector_arr = detector_arr, $
               detector_name_arr = detector_name_arr, $
               detector_description_arr = detector_description_arr, $
               invalid_file = invalid_file, $
               one_d_info = one_d_info, $
               one_d_positioner = one_d_positioner, $
               two_d_info = two_d_info, $
               extra_pv = extra_pv, $
               two_d_positioner = two_d_positioner, $
               add_scan_s_regular = add_scan_s_regular, $
               pointer_extra_pvs = pointer_extra_pvs, $
               n_energy = n_energy, $
               n_mca = n_mca, $
			   check_size = check, $
			   /equal_size

	if invalid_file then begin
		warning,'aps_to_list','Invalid File error, abort.'
		return
	endif

	on_ioerror, bad_io
	version = -4
	writeu, 1, version

	n_x = x_pixels
	n_y = y_pixels

	if n_mca gt 8 then begin
		warning,'aps_to_list',['Number of MCA channels exceeds 8.', $
						'Problems may be encountered with LST read.']
	endif

	writeu, 1, long(n_x), long(n_y), long(n_energy), long(n_mca)
	writeu, 1, x_coord_arr, y_coord_arr								; dbl
	
	bx = byte(x_coord_units)
	by = byte(y_coord_units)
	nbx = n_elements(bx)
	nby = n_elements(by)
	writeu, 1, long(nbx), long(nby)
	if nbx ge 1 then writeu, 1, bx
	if nby ge 1 then writeu, 1, by
		
	cal = fltarr(3,n_mca)
	cal[1,*] = 1.0
	if n_elements(mca_calib_arr) ge 3*n_mca then begin
		for i=0L,n_mca-1 do begin
			cal[0,i] = mca_calib_arr[3*i]				; b
			cal[1,i] = mca_calib_arr[1+3*i]				; a
			cal[2,i] = mca_calib_arr[2+3*i]				; q
		endfor
	endif

	writeu, 1, cal

	xsize = abs(float(x_coord_arr[x_pixels-1] - x_coord_arr[0]))		; mm on sector 2
	ysize = abs(float(y_coord_arr[y_pixels-1] - y_coord_arr[0]))		; micron on sector 20

;	Now in version -2, write flux array (IC US).
;	First work out IC count scale factor.

	if jfile eq 0 then begin
		a1_val = 0.0
		a1_unit = 1.0
		vals = [1., 2., 5., 10., 20., 50., 100., 200., 500.]
		units = ['pA/V', 'nA/V', 'uA/V', 'mA/V']
		vunit = [0.001,1.0,1000.0,1000000.0]

;		Ion chamber PV selection options, or ring current, etc.

		qa = where_tokens( [':scaler',':userCalc',':ION',':CURRENT',':HVSUM_MON',':EH'], detector_name_arr)
		q = where_tokens( ['Ion','IC','I0','Ring'], detector_description_arr, nq, q=qa)

;		Ion chamber preamp sensitivity value

		q2a = where_tokens( ['sens_num.VAL'], mca_calib_name_arr)
		q2 = where_tokens( ['sensitivity'], mca_calib_description_arr, nq2, q=q2a)

;		Ion chamber preamp sensitivity units

		q3a = where_tokens( ['sens_unit.VAL'], mca_calib_name_arr)
		q3 = where_tokens( ['sensitivity'], mca_calib_description_arr, nq3, q=q3a)

;		Live time PV selection ...

		time_mode = 0
		q4 = where_tokens( ['.ELTM','.ERTM'], detector_name_arr, nq4)
		if nq4 gt 0 then begin
			time_mode = 1
			livetime_title = 'Select Live Time:'
		endif else begin
			q4a = where_tokens( [':userCalc'], detector_name_arr)
			q4 = where_tokens( ['dead'], detector_description_arr, nq4, q=q4a)
			if nq4 gt 0 then begin
				time_mode = 2
				livetime_title = 'Select Dead-time fraction:'
			endIF
		endelse
		if nq4 ne 0 then begin
			times = ['--- Disable ---',detector_name_arr[q4]]
		endif else begin
			times = ['--- none found ---']
		endelse
	endif

	if (q[0] ne -1) then begin
		if (q2[0] ne -1) and (q3[0] ne -1) then begin
			if jfile eq 0 then select = generic_flux_select( group, detector_name_arr[q], mca_calib_name_arr[q2], $
						livetime_title=livetime_title, mca_calib_name_arr[q3], times, error=error)
		endif else begin
			if jfile eq 0 then select = generic_flux_select( group, detector_name_arr[q], string(vals), units, $
						livetime_title=livetime_title, times, error=error)
			xsize = 0.001 * xsize
			ysize = 0.001 * ysize							; correct sector 20 micron back to mm
		endelse
	endif else error=1
	if local_group then widget_control, group, /destroy

	if error then begin
		n_flux = -1
		flux = 0.0
	endif else begin
		if jfile eq 0 then begin
			n_flux = q[select.flux]
			if (nq2 ne 0) and (nq3 ne 0) then begin
				a1_val = vals[ fix(mca_calib_arr[q2[select.sense_num]]) ]
				a1_unit = vunit[ fix(mca_calib_arr[q3[select.sense_unit]]) ]
			endif else begin
				a1_val = vals[ select.sense_num ]
				a1_unit = vunit[ select.sense_unit ]
			endelse
			flux_scale = a1_val * a1_unit										; scaling of counts for nA/V units
		endif

;	Flux should be counts per dwell in each pixel. Here we scale count-rate by dwell to get counts.
;	And, we scale flux to correct for dead-time losses to get a 'live flux'.

		flux = detector_arr[*,*,n_flux]

		if (nq4 ne 0) and (select.live_time ge 1) then begin
			case time_mode of
				1: begin
					flux = flux * detector_arr[*,*,q4[select.live_time-1]]		; scale c/s by live time --> counts
					end
				2: begin
					pos = strpos( detector_name_arr, '.ERTM')
					q5 = where(pos ge 0, nq5)									; scale c/s by realtime*(1-DeadTimeFrac)
					if nq5 ne 0 then begin
						live = detector_arr[*,*,q5[0]] * (1.- detector_arr[*,*,q4[select.live_time-1]])	
						flux = flux * live		
					endif
					end
				else:
			endcase
		endif
		flux = reform(flux)
	endelse

	print,'Total flux: ', flux_scale*total(flux)
	
;	wset, 0
;	tv, 16B + bytscl(flux,top=99)
;	wset,1
;	tv, 16B + bytscl(detector_arr[*,*,q4[select.live_time]],top=99)

	writeu, 1, xsize, ysize

;	Now save the data types of the parts of the event struct.
;	These will be used in 'read_aps_lst_header' to determine the type
;	and total length (bytes) of each event.

	writeu, 1, size(check.channel,/type),size(check.n,/type),size(check.x,/type),size(check.y,/type),size(check.e,/type)

	if n_flux eq -1 then begin
		writeu, 1, -1L
	endif else begin
		writeu, 1, n_flux
		writeu, 1, flux_scale
		writeu, 1, flux
	endelse

;	Now, read it again.
;	This time set 'dump_unit' to force it to write direct to the LST file ...

	widget_control, /hour

    sv_read_scan2, $
               filename = filename, $
               output_directory = path, $
               input_directory = path, $
			   dump_unit = 1, $						; dump LST data to unit 1
			   use_size = check

	if invalid_file then begin
		warning,'aps_to_list','Invalid File error, abort.'
		return
	endif

	flush, 1
	close, 1

	jfile = jfile+1
	if jfile lt nfile then goto, loop
	print,'All done.'
	return

bad_file:
	warning,'aps_to_list','Bad file - '+file[jfile]
	return
bad_io:
	warning,'aps_to_list','Bad I/O.'
	return
	end