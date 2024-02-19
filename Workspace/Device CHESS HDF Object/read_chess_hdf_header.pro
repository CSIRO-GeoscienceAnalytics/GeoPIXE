function read_chess_hdf_header, unit, error=error


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
       warning,'read_chess_hdf_header',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   return, 0
   endif
endif
error = 1

	stat = fstat( unit)
	file_id = H5F_OPEN(stat.name)

	version = 1
	head = 0
	mp = get_mp2( stat.name, error=error)
	if error then return, head

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

	head = {	version:	version, $				; version (float)
			comment:	mp.comment, $				; comment (incl. date, user)
			run:		mp.name, $					; run number
			nx:			mp.scan.x_pixels, $			; X pixels
			ny:			mp.scan.y_pixels, $			; Y pixels
			sx:			mp.scan.x_mm, $				; X size (mm)
			sy:			mp.scan.y_mm, $				; Y size (mm)
			orgx:		mp.scan.x, $				; X origin (mm)
			orgy:		mp.scan.y, $				; Y origin (mm)
			axisx:		mp.xs, $					; X coords
			axisy:		mp.ys, $					; Y coords
			dwell:		mp.scan.dwell, $			; mean dwell
			flux:		mp.flux $					; flux array
;			pv_names:	pv_names $					; PV names
			}
	error = 0

finish:
	H5F_close, file_id
	return, head
end
