function read_pnc_cat_hdf_header, unit, error=error


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
       warning,'read_pnc_cat_hdf_header',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   return, 0
   endif
endif
error = 1

	stat = fstat( unit)
	file_id = H5F_OPEN(stat.name)

	version = 3.0
	timebase = 2000000L
	icr_channel = 9
	timer_channel = 1
	autodt = 'YES'
	headings = strarr(17)
	use_x = 0
	use_y = 0
	dwell = 0.0
	mp = 0
	error = 1

	base_id = H5G_OPEN(file_id,'2D Scan')
	n = H5A_get_num_attrs( base_id)
	for i=0,n-1 do begin
		attr_id = H5A_open_idx(base_id, i)
		attr = H5A_read( attr_id)
		case H5A_get_name( attr_id) of
			'VERSION': begin
				version = attr
				end
			else:
		endcase
		H5A_close, attr_id
	endfor
	H5G_close, base_id

	nm = H5G_get_nmembers(file_id,'2D Scan')	
	name = strarr(nm)
	for i=0,nm-1 do begin
		name[i] = H5G_get_member_name(file_id,'2D Scan',i)
	endfor

	q = where( name eq 'Detectors', nq)
	if nq eq 0 then begin
		warning,'read_pnc_cat_hdf_header','No "Detectors" dataset found.'
		goto, finish
	endif else begin
		det_id = H5D_OPEN(file_id,'2D Scan/Detectors')

		n = H5A_get_num_attrs( det_id)
		for i=0,n-1 do begin
			attr_id = H5A_open_idx(det_id, i)
			attr = H5A_read( attr_id)
			case H5A_get_name( attr_id) of
				'TIMER_CHANNEL': begin
					timer_channel = attr
					end
				'Detector Names': begin
					headings = attr
					end
				'TIMEBASE': begin
					timebase = attr
					end
				'ICR_CHANNEL': begin
					icr_channel = attr
					end
				'AUTODTCORR': begin
					autodt = attr
					end
				else:
			endcase
			H5A_close, attr_id
		endfor

		dspace = H5D_get_space(det_id)
		dims = H5S_get_simple_extent_dims(dspace)
		H5S_close, dspace

		nx = dims[1]
		ny = dims[2]
		axisx = fltarr(nx)
		axisy = fltarr(ny)
		pv_names = headings[ timer_channel: (icr_channel-2)>timer_channel]

		det = H5D_read(det_id)
		dwell = 1000. * reform( det[timer_channel-1,*,*] / timebase)
		H5D_close, det_id
	endelse

	qx = where( name eq 'X Positions', nqx)
	qy = where( name eq 'Y Positions', nqy)
	if nqx gt 0 then begin
		axis_id = H5D_OPEN(file_id,'2D Scan/'+name[qx[0]])
		axisx = H5D_read(axis_id)
		axisx = 0.001 * reform(axisx[0,*,0])					; assumes was microns
		use_x = 1
		H5D_close, axis_id			
	endif else axisx = 0.0
	if nqy gt 0 then begin
		axis_id = H5D_OPEN(file_id,'2D Scan/'+name[qy[0]])
		axisy = 0.001 * reform( H5D_read(axis_id))				; assumes was microns
		use_y = 1
		H5D_close, axis_id			
	endif else axisy = 0.0

	error = 0

;	Setting the nominal pixel dwell to the mean(dwell map) below. If there are long dwell
;	pixels, you might need to exclude them, or set nominal dwell another way.
;
;	Missing from header info at the moment is (also see other possibles in 'update_header_info'):
;		cal					detector channel energy calibrations
;		cal = replicate( {cal_devicespec, on:0, a:0.0, b:0.0, units:''}, max_adcs)

;		energy				beam energy
;		comment				comment string for this run
;		IC_name				ion chamber PV name (e.g. from "Detectors")
;		IC_sensitivity		ion chamber preamp sensitivity (relative to 1.0 = nA/V)

	mp = {	version:	version, $					; version (float)
			timebase:	timebase, $					; timebase count
			icr_channel: icr_channel, $				; ICR channel (offset in det array to first ICR)
			timer_channel: timer_channel, $			; offset to timer
			autodt:		autodt, $					; auto DT mode
			headings:	headings, $				`	; "Detector" names
			nx:			nx, $						; X pixels
			ny:			ny, $						; Y pixels
			axisx:		axisx, $					; X coords
			axisy:		axisy, $					; Y coords
			dwell:		mean(dwell), $				; mean dwell 
			pv_names:	pv_names $					; PV names
			}
	error = 0

finish:
	H5F_close, file_id
	return, mp
end
