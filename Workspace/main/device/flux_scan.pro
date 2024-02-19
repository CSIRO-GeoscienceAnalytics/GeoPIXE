pro flux_scan, obj, file, PV_list, IC_name, IC_val, IC_vunit, dwell=dwell, $
			image_mode=image_mode, group=group, suppress=suppress, $
			no_pv=no_pv, use_dwell=use_dwell, dir_mode=dir_mode, error=error

; Scan raw data files for flux IC PV information
; 
; image_mode	scan data for PVs for an image, with dwell
; First			first file in multi-file (or first dir if dir_mode)
; Suppress		suppress pop-ups

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
       warning,'flux_scan',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, done
    endif
endif

	if n_elements(first) lt 1 then first = 1
	if n_elements(suppress) lt 1 then suppress = 0
	if n_elements(image_mode) lt 1 then image_mode = 1
	if n_elements(nsls_debug) lt 1 then nsls_debug = 0
	if n_elements(dir_mode) lt 1 then dir_mode = 0
	
	error = 1
	PV_list = 'none'
	IC_name = ''
	IC_val = 1.
	IC_vunit = 0.
	dwell = 0.
	no_pv = 1
	use_dwell = 0
	
;	This hack for XANES dirs assumes that dir/*, with numerical extensions, collects all raw data files.
;	This works for Maia, but not all data formats.

	if dir_mode then begin
		f = file
		n = strlen(f)
		t = strmid( f, n-1,1)
		ps = path_sep()
		if t ne ps then f = f+ps
		f = find_file2(f + '*', /extension_numeric)
		evt_file = f[0]
	endif else evt_file=file
	
	on_ioerror, bad_file
	openr, unit, evt_file, /get_lun

	obj->flux_scan, unit, evt_file, PV_list, IC_name, IC_val, IC_vunit, dwell=dwell, $
			image_mode=image_mode, group=group, suppress=suppress, $
			no_pv=no_pv, use_dwell=use_dwell, error=error
	obj->check_pv_list, PV_list

done:
	close_file, unit
	return

bad_file:
	warning,'flux_scan','error opening file: '+evt_file
	error=1
	goto, done
end
