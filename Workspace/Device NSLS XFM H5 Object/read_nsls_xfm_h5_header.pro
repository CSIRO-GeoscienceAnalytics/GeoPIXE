function read_nsls_xfm_h5_header, unit, error=error


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
       warning,'read_nsls_xfm_h5_header',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   return, 0
   endif
endif
error = 1

	stat = fstat( unit)
	file_id = H5F_OPEN(stat.name)

	version = '2.1.0'									; can't read attribute below!
	use_x = 0
	use_y = 0
	dwell = 0.0
	n_adcs = 8
	pv_names = ''

	timebase = 1000L			; for ms
	mp = 0
	error = 1

	base_id = H5G_OPEN(file_id,'xrmmap')
	n = H5A_get_num_attrs( base_id)
	for i=0,n-1 do begin
		attr_id = H5A_open_idx(base_id, i)
		case H5A_get_name( attr_id) of
			'Version': begin
;				attr = H5A_read( attr_id)				; errors?
;				version = attr
				end
			else:
		endcase
		H5A_close, attr_id
	endfor
	H5G_close, base_id

	nm = H5G_get_nmembers(file_id,'xrmmap')	
	name = strarr(nm)
	for i=0,nm-1 do begin
		name[i] = H5G_get_member_name(file_id,'xrmmap',i)
	endfor

	q = where( name eq 'scalars', nq)
	if nq eq 0 then begin
		warning,'read_nsls_xfm_h5_header','No "xrmmap/scalars" group found.'
		goto, finish
	endif

	nm = H5G_get_nmembers(file_id,'xrmmap/scalars')	
	headings = strarr(nm)
	for i=0,nm-1 do begin
		headings[i] = H5G_get_member_name(file_id,'xrmmap/scalars',i)
	endfor
	pv_names = headings

	q = where( name eq 'mca1', nq)
	if nq eq 0 then begin
		warning,'read_nsls_xfm_h5_header','No "xrmmap/mca1" dataset found.'
		goto, finish
	endif

	nm = H5G_get_nmembers(file_id,'xrmmap/mca1')	
	name2 = strarr(nm)
	for i=0,nm-1 do begin
		name2[i] = H5G_get_member_name(file_id,'xrmmap/mca1',i)
	endfor

	q = where( name2 eq 'realtime', nq)
	if nq eq 0 then begin
		warning,'read_nsls_xfm_h5_header','No "xrmmap/mca1/realtime" data found.'
		goto, finish
	endif
	det_id = H5D_OPEN(file_id,'xrmmap/mca1/realtime')
	dspace = H5D_get_space(det_id)
	dims = H5S_get_simple_extent_dims(dspace)
	H5S_close, dspace
	
	nx = dims[0]
	ny = dims[1]

	det = H5D_read(det_id)
	dwell = det[*,*] / float(timebase)		; for ms ???
	H5D_close, det_id

	q = where( name eq 'positions', nq)
	if nq eq 0 then begin
		warning,'read_nsls_xfm_h5_header','No "xrmmap/positions" group found.'
		goto, finish
	endif 

	nm = H5G_get_nmembers(file_id,'xrmmap/positions')	
	name2 = strarr(nm)
	for i=0,nm-1 do begin
		name2[i] = H5G_get_member_name(file_id,'xrmmap/positions',i)
	endfor

	q = where( name2 eq 'pos', nq)
	if nq eq 0 then begin
		warning,'read_nsls_xfm_h5_header','No "xrmmap/positions/pos" data found.'
		goto, finish
	endif

	axis_id = H5D_OPEN(file_id,'xrmmap/positions/pos')
	pos = H5D_read(axis_id)
	axisx = reform(pos[0,*,0])					; assumes was mm
	use_x = 1
	axisy = reform(pos[1,0,*])					; assumes was mm
	use_y = 1
	H5D_close, axis_id			

	cal = replicate( {cal_devicespec, on:0, a:0.0, b:0.0, units:''}, n_adcs)

	q = where( name eq 'config', nq)
	if nq eq 0 then begin
		warning,'read_nsls_xfm_h5_header','No "xrmmap/config" group found.'
		goto, finish
	endif

	nm = H5G_get_nmembers(file_id,'xrmmap/config')	
	name2 = strarr(nm)
	for i=0,nm-1 do begin
		name2[i] = H5G_get_member_name(file_id,'xrmmap/config',i)
	endfor

	q = where( name2 eq 'mca_calib', nq)
	if nq eq 0 then begin
		warning,'read_nsls_xfm_h5_header','No "xrmmap/config/mca_calib" group found.'
		goto, finish
	endif

	nm = H5G_get_nmembers(file_id,'xrmmap/config/mca_calib')	
	name3 = strarr(nm)
	for i=0,nm-1 do begin
		name3[i] = H5G_get_member_name(file_id,'xrmmap/config/mca_calib',i)
	endfor

	q = where( name3 eq 'offset', nq)
	if nq eq 0 then begin
		warning,'read_nsls_xfm_h5_header','No "xrmmap/config/mca_calib/offset" data found.'
		goto, finish
	endif

	axis_id = H5D_OPEN(file_id,'xrmmap/config/mca_calib/offset')
	doff = H5D_read(axis_id)
	H5D_close, axis_id			

	q = where( name3 eq 'slope', nq)
	if nq eq 0 then begin
		warning,'read_nsls_xfm_h5_header','No "xrmmap/config/mca_calib/slope" data found.'
		goto, finish
	endif

	axis_id = H5D_OPEN(file_id,'xrmmap/config/mca_calib/slope')
	dslope = H5D_read(axis_id)
	H5D_close, axis_id			

	for i=0,n_adcs-1 do begin
		cal[i].a = dslope[i]
		cal[i].b = doff[i]
		if cal[i].a gt 1.0e-10 then begin
			cal[i].on = 1
			cal[i].units = 'keV'
		endif
	endfor

	error = 0

;	Setting the nominal pixel dwell to the mean(dwell map) below. If there are long dwell
;	pixels, you might need to exclude them, or set nominal dwell another way.
;
;	Missing from header info at the moment is (also see other possibles in 'update_header_info'):
;		energy				beam energy
;		comment				comment string for this run
;		IC_name				ion chamber PV name (e.g. from "Detectors")
;		IC_sensitivity		ion chamber preamp sensitivity (relative to 1.0 = nA/V)

	h = histogram( dwell, /NaN, locations=x)
	q2 = reverse(sort(h))
	maia_fixed_dwell = x[q2[0]]

	mp = {	version:	version, $					; version (float)
			timebase:	timebase, $					; timebase count?
			headings:	headings, $				`	; "Detector" names
			nx:			nx, $						; X pixels
			ny:			ny, $						; Y pixels
			axisx:		axisx, $					; X coords
			axisy:		axisy, $					; Y coords
			dwell:		maia_fixed_dwell, $			; mean dwell 
			cal:		cal, $						; energy calibrations
			pv_names:	pv_names $					; PV names
			}
	error = 0

finish:
	H5F_close, file_id
	return, mp
end
