function read_as_mex_h5_header, unit, error=error

; Draft attempt at reading HDF map scan 'header' data from MEX

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
       warning,'read_as_mex_h5_header',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   return, 0
   endif
endif
common c_petra_1, min_x, step_x, min_y, pixel_x, pixel_y
common c_maia_13, maia_dwell
error = 1

	stat = fstat( unit)
	
;	Data is organized as spectra (4096) for each of detector channel (4), for each sequence step.
;	The sequence steps have an 'x' and 'y' each, which do not necessarily follow a perfect rectangle.

	file_id = H5F_OPEN(stat.name)

	version = '0.0.0'									; can't read attribute below!
	use_x = 0
	use_y = 0
	dwell = 0.0
	common_dwell = 0.0
	sample = 'unknown'
	n_adcs = 1					; 8
	pv_names = ''
	cal = replicate( {cal_devicespec, on:0, a:0.0, b:0.0, units:''}, n_adcs)

;	timebase = 1000L			; for ms?
	mp = 0
	error = 1

	nm = H5G_get_num_objs( file_id)
	name7 = strarr(nm)
	for j=0,nm-1 do begin
		name7[j] = H5G_get_obj_name_by_idx(file_id, j)
	endfor
	
	q = where( name7 eq 'abs_x', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "abs_x" dataset found.'
		goto, finish
	endif else begin	
		rec_id = H5D_OPEN(file_id, 'abs_x')
		abs_x = H5D_read(rec_id)
		H5D_close, rec_id
	endelse

	q = where( name7 eq 'abs_y', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "abs_y" dataset found.'
		goto, finish
	endif else begin
		rec_id = H5D_OPEN(file_id, 'abs_y')
		abs_y = H5D_read(rec_id)
		H5D_close, rec_id
	endelse

	q = where( name7 eq 'x', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "x" dataset found.'
		goto, finish
	endif else begin	
		rec_id = H5D_OPEN(file_id, 'x')
		x = H5D_read(rec_id)
		H5D_close, rec_id
	endelse

	q = where( name7 eq 'y', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "y" dataset found.'
		goto, finish
	endif else begin
		rec_id = H5D_OPEN(file_id, 'y')
		y = H5D_read(rec_id)
		H5D_close, rec_id
	endelse
	nxy = n_elements(y)
	
	step_x = x - shift(x,1)
	step_x[0] = step_x[1]
	step_x = mean( median( abs(step_x[nxy/10:*]),5))
	step_y = step_x

	pixel_x = round( (x - min(x)) / step_x )
	pixel_y = round( (y - min(y)) / step_y )
	min_x = min( pixel_x)
	min_y = min( pixel_y)
	nx = max(pixel_x) + 1
	ny = max(pixel_y) + 1
				
;	Find exposure time

	q = where( name7 eq 'x_ts', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "x_ts" (dwell) dataset found.'
		goto, finish
	endif else begin
		rec_id = H5D_OPEN(file_id, 'x_ts')
		dwell_array = H5D_read(rec_id)
		dwell_array = dwell_array * 1.0e-9		; s
		H5D_close, rec_id
	endelse

	maia_dwell = fltarr(nx,ny)
	maia_dwell[pixel_x,pixel_y] = dwell_array * 1000.			; ms dwell
	
	h = histogram( maia_dwell *300./(max(maia_dwell)>0.001), /NaN, locations=tx)
	q2 = reverse(sort(h))
	common_dwell = tx[q2[0]] *(max(maia_dwell)>0.001)/300.

;	Find energy, sample, instrument name

	q = where( name7 eq 'dcm_energy_ev', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "dcm_energy_ev" dataset found.'
		goto, finish
	endif else begin
		rec_id = H5D_OPEN(file_id, 'dcm_energy_ev')
		energy = H5D_read(rec_id)
		H5D_close, rec_id
	endelse

;find_sample:
;	path = dir_up( path)
;	cfile = base + '.nxs'
;	cfiles = find_file2( path + cfile)
;	na = n_elements(cfiles)
;	if (na eq 0) or (cfiles[0] eq '') then begin
;		warning,'read_as_mex_h5_header','No "'+cfile+'" file found.'
;		goto, finish
;	endif
;
;	cfile_id = H5F_OPEN( cfiles[0])
;
;	nm = H5G_get_nmembers( cfile_id,'scan/data')
;	name9 = strarr(nm)
;	for j=0,nm-1 do begin
;		name9[j] = H5G_get_member_name(cfile_id,'scan/data',j)
;	endfor
;
;	q = where( name9 eq 'energy', nq)
;	if nq eq 0 then begin
;		warning,'read_as_mex_h5_header','No "scan/data/energy" dataset found.'
;		H5F_close, cfile_id
;		goto, finish
;	endif
;	rec_id = H5D_OPEN(cfile_id,'scan/data/energy')
;	energy = H5D_read(rec_id)
;	H5D_close, rec_id
;	
;	nm = H5G_get_nmembers( cfile_id,'scan/sample')
;	name10 = strarr(nm)
;	for j=0,nm-1 do begin
;		name10[j] = H5G_get_member_name(cfile_id,'scan/sample',j)
;	endfor
;
;	q = where( name10 eq 'name', nq)
;	if nq eq 0 then begin
;		warning,'read_as_mex_h5_header','No "scan/sample/name" dataset found.'
;		H5F_close, cfile_id
;		goto, finish
;	endif
;	rec_id = H5D_OPEN(cfile_id,'scan/sample/name')
;	sample = H5D_read(rec_id)
;	H5D_close, rec_id
;
;	nm = H5G_get_nmembers( cfile_id,'scan/instrument')
;	name11 = strarr(nm)
;	for j=0,nm-1 do begin
;		name11[j] = H5G_get_member_name(cfile_id,'scan/instrument',j)
;	endfor
;
;	q = where( name11 eq 'name', nq)
;	if nq eq 0 then begin
;		warning,'read_as_mex_h5_header','No "scan/instrument/name" dataset found.'
;		H5F_close, cfile_id
;		goto, finish
;	endif
;	rec_id = H5D_OPEN(cfile_id,'scan/instrument/name')
;	endstation = H5D_read(rec_id)
;	H5D_close, rec_id
;
;	H5F_close, cfile_id

;	Find other flux PVs

	q = where( name7 eq 'i0', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "i0" dataset found.'
		goto, finish
	endif else begin
		pv_names = 'i0'
;		rec_id = H5D_OPEN(file_id, 'i0')
;		i0 = H5D_read(rec_id)
;		H5D_close, rec_id
	endelse
	q = where( name7 eq 'i1', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "i1" dataset found.'
	endif else begin
		pv_names = n_elements(pv_names) gt 0 ? [pv_names,'i1'] : 'i1'
;		rec_id = H5D_OPEN(file_id, 'i1')
;		i1 = H5D_read(rec_id)
;		H5D_close, rec_id
	endelse
	q = where( name7 eq 'i2', nq)
	if nq eq 0 then begin
		warning,'read_as_mex_h5_header','No "i2" dataset found.'
	endif else begin
		pv_names = n_elements(pv_names) gt 0 ? [pv_names,'i2'] : 'i2'
;		rec_id = H5D_OPEN(file_id, 'i2')
;		i2 = H5D_read(rec_id)
;		H5D_close, rec_id
	endelse

;	path = dir_up( extract_path( stat.name))
;	file = strip_path( stat.name)
;	dirs = find_file2( path + '*')
;	q = where( strmid( strip_path( dirs),0,strlen('qbpm_')) eq 'qbpm_', nq)
;	name_qbpm = strip_path(dirs[q[0]])
;	if nq eq 0 then begin
;		warning,'read_as_mex_h5_header','No "qbpm_..." sub directory found.'
;		goto, find_cals
;	endif
;	base = strmid( file, 0, locate_last( '_', file))
;	qfile = fix_path(dirs[q[0]]) + base + '_*.nxs'
;	qfiles = find_file2( qfile)
;	na = n_elements(qfiles)
;	if (na eq 0) or (qfiles[0] eq '') then begin
;		warning,'read_as_mex_h5_header','No "'+qfile+'" files found.'
;		goto, find_cals
;	endif
;
;	qfile_id = H5F_OPEN( qfiles[0])
;
;	nm = H5G_get_nmembers( qfile_id,'entry/data')
;	name12 = strarr(nm)
;	for j=0,nm-1 do begin
;		name12[j] = H5G_get_member_name(qfile_id,'entry/data',j)
;	endfor
;
;	q = where( strmid( strip_path( name12),0,strlen('value')) eq 'value', nq)
;	if (nq gt 0) then begin
;		for j=0,nq-1 do pv_names = [pv_names, name_qbpm+'/'+name12[q[j]]]
;	endif
;
;	H5F_close, qfile_id

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

	error = 0

;	Setting the nominal pixel dwell to the most common (dwell map) below. 
;
;	Missing from header info at the moment is (also see other possibles in 'update_header_info'):
;		cal					energy calibration
;		comment				comment string for this run
;		sample				sample name
;		IC_sensitivity		ion chamber preamp sensitivity (relative to 1.0 = nA/V). Is this "/scan/instrument/keithley1, keithley2, ..."

	mp = {	version:	version, $					; version (float)
;			timebase:	timebase, $					; timebase count?
;			headings:	headings, $				`	; "Detector" names
			nx:			nx, $						; X pixels
			ny:			ny, $						; Y pixels
			axisx:		abs_x, $					; X coords
			axisy:		abs_y, $					; Y coords
			dwell:		common_dwell, $				; mean dwell 
			energy:		energy[0], $				; beam energy
			sample:		sample, $					; sample name
			facility:	'AS', $
;			endstation:	endstation, $				; endstation
			endstation:	'MEX', $					; endstation
			cal:		cal, $						; energy calibrations
			pv_names:	pv_names $					; PV names
			}
	error = 0

finish:
	H5F_close, file_id
	return, mp
end
