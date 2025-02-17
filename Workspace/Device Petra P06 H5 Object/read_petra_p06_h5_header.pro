function read_petra_p06_h5_header, unit, error=error

; Draft attempt at reading Nexus map scan 'header' data from P06
; This is actually spread over many Nexus files in the dir tree, not in a single H5 file.
; Will just use info in main map histogram H5 file for now.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'read_petra_p06_h5_header',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   return, 0
   endif
endif
common c_petra_1, min_x, step_x, min_y, pixel_x, pixel_y
error = 1

	stat = fstat( unit)
	
; File points to actual detector data (e.g. "Sativa_hydroponics\scan_00088\xspress3_mini_189\scan_00088_00000.nxs")
; These will be extracted elsewhere from the open xspress3 file in the 'read_buffer' method ...
; 	entry/instrument/detector/sequence_number
; 	entry/instrument/xspress3/channel00/histogram/
; 	entry/instrument/xspress3/channel00/scaler/deadtimeticks, totalticks
;
; Will search here for related H5 files for:
; Trigger files:						(e.g. "Sativa_hydroponics\scan_00088\pilctriggergenerator_03\scan_00088_00000.nxs")
; 	entry/data/encoder1, encoder2
;	entry/data/position_trigger_start, position_trigger_step_size, position_trigger_stop
;
; ADC files:							(e.g. "Sativa_hydroponics\scan_00088\adc_01\scan_00088_00000.nxs")
;	/entry/data/exposuretime
;	/entry/data/value1, value2, ...		flux counters?
;
; QBPM files:							(e.g. "Sativa_hydroponics\scan_00088\qbpm_02\scan_00088_00000.nxs")
;	/entry/data/value1, value2, ...		flux counters?
;
; Context file:							(e.g. "Sativa_hydroponics\scan_00088.nxs")
;	/scan/data/energy
;	/scan/instrument/name
;	/scan/instrument/keithley1, keithley2, ...
;	/scan/sample/name
;

	file_id = H5F_OPEN(stat.name)

	version = '0.0.0'									; can't read attribute below!
	use_x = 0
	use_y = 0
	dwell = 0.0
	n_adcs = 1					; 8
	pv_names = ''
	cal = replicate( {cal_devicespec, on:0, a:0.0, b:0.0, units:''}, n_adcs)

;	timebase = 1000L			; for ms?
	mp = 0
	error = 1

;	Find start, stop, step and encoder1, encoder2

	path = dir_up( extract_path( stat.name))
	file = strip_path( stat.name)
	dirs = find_file2( path + '*')
	q = where( strmid( strip_path( dirs),0,strlen('pilctriggergenerator_')) eq 'pilctriggergenerator_', nq)
	if nq eq 0 then begin
		warning,'read_petra_p06_h5_header','No "pilctriggergenerator_..." sub directory found.'
		goto, finish
	endif
	base = strmid( file, 0, locate_last( '_', file))
	afile = fix_path(dirs[q]) + base + '_*.nxs'
	afiles = find_file2( afile)
	na = n_elements(afiles)
	if (na eq 0) or (afiles[0] eq '') then begin
		warning,'read_petra_p06_h5_header','No "'+afile+'" files found.'
		goto, finish
	endif
	
	for i=0,na-1 do begin
		afile_id = H5F_OPEN( afiles[i])

		nm = H5G_get_nmembers( afile_id,'entry/data')
		name7 = strarr(nm)
		for j=0,nm-1 do begin
			name7[j] = H5G_get_member_name(afile_id,'entry/data',j)
		endfor

		if i eq 0 then begin
			q = where( name7 eq 'position_trigger_start', nq)
			if nq eq 0 then begin
				warning,'read_petra_p06_h5_header','No "entry/data/position_trigger_start" dataset found.'
				H5F_close, afile_id
				goto, find_encoder
			endif
			rec_id = H5D_OPEN(afile_id,'entry/data/position_trigger_start')
			start = H5D_read(rec_id)
			H5D_close, rec_id
	
			q = where( name7 eq 'position_trigger_stop', nq)
			if nq eq 0 then begin
				warning,'read_petra_p06_h5_header','No "entry/data/position_trigger_stop" dataset found.'
				H5F_close, afile_id
				goto, find_encoder
			endif
			rec_id = H5D_OPEN(afile_id,'entry/data/position_trigger_stop')
			stop = H5D_read(rec_id)
			H5D_close, rec_id
			
			q = where( name7 eq 'position_trigger_step_size', nq)
			if nq eq 0 then begin
				warning,'read_petra_p06_h5_header','No "entry/data/position_trigger_step_size" dataset found.'
				H5F_close, afile_id
				goto, find_encoder
			endif
			rec_id = H5D_OPEN(afile_id,'entry/data/position_trigger_step_size')
			step = H5D_read(rec_id)
			H5D_close, rec_id
		endif

find_encoder:
		q = where( name7 eq 'encoder_1', nq)
		if nq eq 0 then begin
			warning,'read_petra_p06_h5_header','No "entry/data/encoder_1" dataset found.'
			H5F_close, afile_id
			goto, finish
		endif
		rec_id = H5D_OPEN(afile_id,'entry/data/encoder_1')

		vals = H5D_read(rec_id)
		encoder1 = (i eq 0) ? vals : [encoder1, vals]
		H5D_close, rec_id

		q = where( name7 eq 'encoder_2', nq)
		if nq eq 0 then begin
			warning,'read_petra_p06_h5_header','No "entry/data/encoder_2" dataset found.'
			H5F_close, afile_id
			goto, finish
		endif
		rec_id = H5D_OPEN(afile_id,'entry/data/encoder_2')

		vals = H5D_read(rec_id)
		encoder2 = (i eq 0) ? vals : [encoder2, vals]
		H5D_close, rec_id

		H5F_close, afile_id
	endfor
	
	pixel_x = round( (encoder1 - min(encoder1)) / abs(step) )
	pixel_y = round( (encoder2 - min(encoder2)) / abs(step) )
	step_x = step
	min_x = min( pixel_x)
	min_y = min( pixel_y)
	nx = max(pixel_x) + 1
	ny = max(pixel_y) + 1
				
;	Find exposure time
;	 
	path = dir_up( extract_path( stat.name))
	file = strip_path( stat.name)
	dirs = find_file2( path + '*')
	q = where( strmid( strip_path( dirs),0,strlen('adc_')) eq 'adc_', nq)
	name_adc = strip_path(dirs[q[0]])
	if nq eq 0 then begin
		warning,'read_petra_p06_h5_header','No "adc_..." sub directory found.'
		goto, find_sample
	endif
	base = strmid( file, 0, locate_last( '_', file))
	afile = fix_path(dirs[q[0]]) + base + '_*.nxs'
	afiles = find_file2( afile)
	na = n_elements(afiles)
	if (na eq 0) or (afiles[0] eq '') then begin
		warning,'read_petra_p06_h5_header','No "'+afile+'" files found.'
		goto, find_sample
	endif

	for i=0,na-1 do begin
		afile_id = H5F_OPEN( afiles[i])

		nm = H5G_get_nmembers( afile_id,'entry/data')
		name8 = strarr(nm)
		for j=0,nm-1 do begin
			name8[j] = H5G_get_member_name(afile_id,'entry/data',j)
		endfor

		q = where( strmid( strip_path( name8),0,strlen('value')) eq 'value', nq)
		if (i eq 0) and (nq gt 0) then begin
			for j=0,nq-1 do pv_names = [pv_names, name_adc+'/'+name8[q[j]]]
		endif

		q = where( name8 eq 'exposuretime', nq)
		if nq eq 0 then begin
			warning,'read_petra_p06_h5_header','No "entry/data/exposuretime" dataset found.'
			H5F_close, afile_id
			goto, find_sample
		endif
		rec_id = H5D_OPEN(afile_id,'entry/data/exposuretime')
		vals = H5D_read(rec_id)
		dwell_array = (i eq 0) ? vals : [dwell_array, vals]
		H5D_close, rec_id

		H5F_close, afile_id
	endfor

	h = histogram( dwell_array, /NaN, locations=tx)
	q2 = reverse(sort(h))
	common_dwell = tx[q2[0]]

;	Find energy, sample, instrument name

find_sample:
	path = dir_up( path)
	cfile = base + '.nxs'
	cfiles = find_file2( path + cfile)
	na = n_elements(cfiles)
	if (na eq 0) or (cfiles[0] eq '') then begin
		warning,'read_petra_p06_h5_header','No "'+cfile+'" file found.'
		goto, finish
	endif

	cfile_id = H5F_OPEN( cfiles[0])

	nm = H5G_get_nmembers( cfile_id,'scan/data')
	name9 = strarr(nm)
	for j=0,nm-1 do begin
		name9[j] = H5G_get_member_name(cfile_id,'scan/data',j)
	endfor

	q = where( name9 eq 'energy', nq)
	if nq eq 0 then begin
		warning,'read_petra_p06_h5_header','No "scan/data/energy" dataset found.'
		H5F_close, cfile_id
		goto, finish
	endif
	rec_id = H5D_OPEN(cfile_id,'scan/data/energy')
	energy = H5D_read(rec_id)
	H5D_close, rec_id
	
	nm = H5G_get_nmembers( cfile_id,'scan/sample')
	name10 = strarr(nm)
	for j=0,nm-1 do begin
		name10[j] = H5G_get_member_name(cfile_id,'scan/sample',j)
	endfor

	q = where( name10 eq 'name', nq)
	if nq eq 0 then begin
		warning,'read_petra_p06_h5_header','No "scan/sample/name" dataset found.'
		H5F_close, cfile_id
		goto, finish
	endif
	rec_id = H5D_OPEN(cfile_id,'scan/sample/name')
	sample = H5D_read(rec_id)
	H5D_close, rec_id

	nm = H5G_get_nmembers( cfile_id,'scan/instrument')
	name11 = strarr(nm)
	for j=0,nm-1 do begin
		name11[j] = H5G_get_member_name(cfile_id,'scan/instrument',j)
	endfor

	q = where( name11 eq 'name', nq)
	if nq eq 0 then begin
		warning,'read_petra_p06_h5_header','No "scan/instrument/name" dataset found.'
		H5F_close, cfile_id
		goto, finish
	endif
	rec_id = H5D_OPEN(cfile_id,'scan/instrument/name')
	endstation = H5D_read(rec_id)
	H5D_close, rec_id

	H5F_close, cfile_id

;	Find other 'value1, ...'
;
	path = dir_up( extract_path( stat.name))
	file = strip_path( stat.name)
	dirs = find_file2( path + '*')
	q = where( strmid( strip_path( dirs),0,strlen('qbpm_')) eq 'qbpm_', nq)
	name_qbpm = strip_path(dirs[q[0]])
	if nq eq 0 then begin
		warning,'read_petra_p06_h5_header','No "qbpm_..." sub directory found.'
		goto, find_cals
	endif
	base = strmid( file, 0, locate_last( '_', file))
	qfile = fix_path(dirs[q[0]]) + base + '_*.nxs'
	qfiles = find_file2( qfile)
	na = n_elements(qfiles)
	if (na eq 0) or (qfiles[0] eq '') then begin
		warning,'read_petra_p06_h5_header','No "'+qfile+'" files found.'
		goto, find_cals
	endif

	qfile_id = H5F_OPEN( qfiles[0])

	nm = H5G_get_nmembers( qfile_id,'entry/data')
	name12 = strarr(nm)
	for j=0,nm-1 do begin
		name12[j] = H5G_get_member_name(qfile_id,'entry/data',j)
	endfor

	q = where( strmid( strip_path( name12),0,strlen('value')) eq 'value', nq)
	if (nq gt 0) then begin
		for j=0,nq-1 do pv_names = [pv_names, name_qbpm+'/'+name12[q[j]]]
	endif

	H5F_close, qfile_id

;	We don't have any embedded energy calibration so far

find_cals:
	for i=0,n_adcs-1 do begin
		cal[i].a = 0.0					; dslope[i]
		cal[i].b = 0.0					; doff[i]
		if cal[i].a gt 1.0e-10 then begin
			cal[i].on = 1
			cal[i].units = 'keV'
		endif
	endfor

	pv_names = pv_names[1:*]
	error = 0

;	Setting the nominal pixel dwell to the most common (dwell map) below. 
;
;	Missing from header info at the moment is (also see other possibles in 'update_header_info'):
;		cal					energy calibration
;		comment				comment string for this run
;		IC_name				ion chamber PV name (e.g. from "Detectors")
;		IC_sensitivity		ion chamber preamp sensitivity (relative to 1.0 = nA/V). Is this "/scan/instrument/keithley1, keithley2, ..."

	mp = {	version:	version, $					; version (float)
;			timebase:	timebase, $					; timebase count?
;			headings:	headings, $				`	; "Detector" names
			nx:			nx, $						; X pixels
			ny:			ny, $						; Y pixels
			axisx:		encoder1, $					; X coords
			axisy:		encoder2, $					; Y coords
			dwell:		common_dwell, $				; mean dwell 
			energy:		energy[0], $				; beam energy
			sample:		sample, $					; sample name
			facility:	'Petra III', $
			endstation:	endstation, $				; endstation
			cal:		cal, $						; energy calibrations
			pv_names:	pv_names $					; PV names
			}
	error = 0

finish:
	H5F_close, file_id
	return, mp
end
