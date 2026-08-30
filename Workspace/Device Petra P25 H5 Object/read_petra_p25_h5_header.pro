function read_petra_p25_h5_header, unit, error=error

; Draft attempt at reading Nexus map scan 'header' data from P25
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
       warning,'read_petra_p25_h5_header',['IDL run-time error caught.', '', $
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
	file_id = H5F_OPEN(stat.name)

	version = '0.0.0'									; can't read attribute below!
	use_x = 0
	use_y = 0
	dwell = 0.0
	common_dwell = 0.0
	n_adcs = 1					; 8
	pv_names = ''
	cal = replicate( {cal_devicespec, on:0, a:0.0, b:0.0, units:''}, n_adcs)
	sample = ''
	grain = ''
	comment = ''

	endstation = 'P25'
	mp = 0
	error = 1

;	Find scan dimensions

	nm = H5G_get_nmembers( file_id,'entry/data')
	name = strarr(nm)
	for j=0,nm-1 do begin
		name[j] = H5G_get_member_name( file_id,'entry/data',j)
	endfor
	q = where( name eq 'scan_grid_dimension', nq)
	if nq eq 0 then begin
		warning,'read_petra_p25_h5_header','No "entry/data/scan_grid_dimension" dataset found.'
		H5F_close, file_id
		return, 0
	endif
	rec_id = H5D_OPEN( file_id,'entry/data/scan_grid_dimension')
	dimensions = H5D_read(rec_id)
	nx = dimensions[0]
	ny = dimensions[1]
	total_pixels = nx * ny
	pixel_x = lonarr(total_pixels)
	pixel_y = lonarr(total_pixels)
	encoder_x = fltarr(total_pixels)
	encoder_y = fltarr(total_pixels)
	energy = fltarr(total_pixels)
	dwell_array = fltarr(total_pixels)
	H5D_close, rec_id

;	Find sequence numbers

	q = where( name eq 'sequence_number', nq)
	if nq eq 0 then begin
		warning,'read_petra_p25_h5_header','No "entry/data/sequence_number" dataset found.'
		H5F_close, file_id
		return, 0
	endif
	rec_id = H5D_OPEN( file_id,'entry/data/sequence_number')
	sequence = H5D_read(rec_id)

;	Find pixel indices and encoder values

	q = where( name eq 'index_0', nq)
	if nq eq 0 then begin
		warning,'read_petra_p25_h5_header','No "entry/data/index_0" dataset found.'
		H5F_close, file_id
		return, 0
	endif
	rec_id = H5D_OPEN( file_id,'entry/data/index_0')
	t = H5D_read(rec_id)
	pixel_x[sequence] = t
	H5D_close, rec_id

	q = where( name eq 'index_1', nq)
	if nq eq 0 then begin
		warning,'read_petra_p25_h5_header','No "entry/data/index_1" dataset found.'
		H5F_close, file_id
		return, 0
	endif
	rec_id = H5D_OPEN( file_id,'entry/data/index_1')
	t = H5D_read(rec_id)
	pixel_y[sequence] = t
	H5D_close, rec_id

	q = where( name eq 'sample_0', nq)
	if nq eq 0 then begin
		warning,'read_petra_p25_h5_header','No "entry/data/sample_0" dataset found.'
		H5F_close, file_id
		return, 0
	endif
	rec_id = H5D_OPEN( file_id,'entry/data/sample_0')
	t = H5D_read(rec_id)
	encoder_x[sequence] = t
	H5D_close, rec_id

	q = where( name eq 'sample_1', nq)
	if nq eq 0 then begin
		warning,'read_petra_p25_h5_header','No "entry/data/sample_1" dataset found.'
		H5F_close, file_id
		return, 0
	endif
	rec_id = H5D_OPEN( file_id,'entry/data/sample_1')
	t = H5D_read(rec_id)
	encoder_y[sequence] = t
	H5D_close, rec_id

;	Find energies, i0, i1

	q = where( name eq 'energy', nq)
	if nq eq 0 then begin
		warning,'read_petra_p25_h5_header','No "entry/data/energy" dataset found.'
		H5F_close, file_id
		return, 0
	endif
	rec_id = H5D_OPEN( file_id,'entry/data/energy')
	t = H5D_read(rec_id)
	energy[sequence] = t
	H5D_close, rec_id

	q = where( name eq 'i0', nq)
	if nq ne 0 then pv_names = [pv_names, 'i0']
	q = where( name eq 'i1', nq)
	if nq ne 0 then pv_names = [pv_names, 'i1']
	q = where( pv_names ne '', nq)
	if nq ne 0 then pv_names = pv_names[q]
				
;	Find dwell times
 
	nm = H5G_get_nmembers( file_id,'entry/instrument')
	name1 = strarr(nm)
	for j=0,nm-1 do begin
		name1[j] = H5G_get_member_name( file_id,'entry/instrument',j)
	endfor
	q = where( name1 eq 'detector', nq)
	if nq eq 0 then begin
		warning,'read_petra_p25_h5_header','No "entry/instrument/detector" dataset found.'
		H5F_close, file_id
		return, 0
	endif

	nm = H5G_get_nmembers( file_id,'entry/instrument/detector')
	name2 = strarr(nm)
	for j=0,nm-1 do begin
		name2[j] = H5G_get_member_name( file_id,'entry/instrument/detector',j)
	endfor

	nm = H5G_get_nmembers( file_id,'entry/instrument/detector/'+name2[0])
	name3 = strarr(nm)
	for j=0,nm-1 do begin
		name3[j] = H5G_get_member_name( file_id,'entry/instrument/detector/'+name2[0],j)
	endfor

	q = where( name3 eq 'dwell_time', nq)
	if nq eq 0 then begin
		warning,'read_petra_p25_h5_header','No "entry/instrument/detector/'+name2[0]+'" dataset found.'
		H5F_close, file_id
		return, 0
	endif
	rec_id = H5D_OPEN( file_id,'entry/instrument/detector/' + name2[0] + '/dwell_time' )
	t = H5D_read(rec_id)
	H5D_close, rec_id

	maia_dwell = fltarr(nx,ny)
	maia_dwell[pixel_x[sequence],pixel_y[sequence]] = t * 1000.			; ms dwell
	
	h = histogram( maia_dwell *300./(max(maia_dwell)>0.001), /NaN, locations=tx)
	q2 = reverse(sort(h))
	common_dwell = tx[q2[0]] *(max(maia_dwell)>0.001)/300.

;	Find sample, instrument name



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
;		IC_name				ion chamber PV name (e.g. from "Detectors")
;		IC_sensitivity		ion chamber preamp sensitivity (relative to 1.0 = nA/V). Is this "/scan/instrument/keithley1, keithley2, ..."

	mp = {	version:	version, $					; version (float)
;			timebase:	timebase, $					; timebase count?
			headings:	name2, $				`	; "Detector" names
			nx:			nx, $						; X pixels
			ny:			ny, $						; Y pixels
			axisx:		encoder_x[uniq(pixel_x,sort(pixel_x))], $		; X coords
			axisy:		encoder_y[uniq(pixel_y,sort(pixel_y))], $		; Y coords
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
