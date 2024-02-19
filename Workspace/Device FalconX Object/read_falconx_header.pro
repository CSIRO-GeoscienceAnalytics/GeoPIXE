function read_falconx_header, unit, error=err, no_scan=no_scan, silent=silent

; /no_scan to suppress scanning for X range if a scan record is not found.
; Now that we store the last header until file changes, we don't need /no_scan.

COMPILE_OPT STRICTARR
common c_falconx_10, last_file, last_head
common c_falconx_20, version, tick
if n_elements(version) lt 1 then version=0
if n_elements(tick) eq 0 then tick = 4.0e-6
if n_elements(last_file) lt 1 then last_file=''
if n_elements(last_head) lt 1 then last_head=ptr_new(/allocate_heap)

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
       warning,'read_falconx_header',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       free_record, pp
       err = 1
       return, 0L
    endif
endif

@falconx_listmode.def

	err = 1
	if n_elements(no_scan) lt 1 then no_scan=0
	if n_elements(silent) lt 1 then silent=0
	n_buffer = 50000L		; 2000000L
	
	stat = fstat( unit)
	file = stat.name
;	if file eq last_file then begin
;		err = 0
;		d = *last_head
;		return, d
;	endif

	json = read_falconx_json( unit, n_header=njson, error=err)
	if err then return,0
	if njson gt 0 then begin
		ver = falconx_version( json, tick=tick)
		if ver gt 0 then begin
			version = ver[0]
		endif else begin
			warning,'read_falconx_header','JSON found, but Version zero!'
			return, 0
		endelse
	endif else begin
		if version eq 0 then begin
			gprint,'read_falconx_header: Version zero, so look for 0 segment file.'
			file2 = file
			t = strip_file_ext(file2)
			k = locate_last('_',t)
			f = strmid(t,0,k+1) + '0.' + extract_extension(file2)
			if file_test(f) and (f ne file2) then begin
				file2 = f
				json2 = read_falconx_json( file=file2, n_header=njson2, error=err)
				ver = falconx_version( json2, tick=tick)
				if ver gt 0 then begin
					version = ver[0]
				endif else begin
					warning,'read_falconx_header','Version zero. JSON not found in first segment.'
;					return, 0
				endelse
			endif else begin
				warning,'read_falconx_segments','Version zero. Error reading first segment?'
;				return, 0
			endelse
		endif
	endelse
	
;	NOTE: 'normal_accept' comes from 'falconx_listmode.def'

	info = get_falconx_info( file, error=err)

	pr = read_falconx_segments( file[0], n_buffer=n_buffer, unit=unit, header=json, version=version, $
						accept=normal_accept, njson=njson, /veto_progress)
	pp = ptr_new( pr)
	d = get_falconx_details( pp, header=json, njson=njson, info=info, $
						silent=silent, version=version, tick=tick, error=err)

if (err eq 0) then begin
	last_file = file
	*last_head = d
	if (d.scan_dwell eq 0.) and (no_scan eq 0) then begin
		gprint,level=2,'read_falconx_header: Dwell not found. Scan further ...'

		point_lun, unit, 0							; rewind unit
		json = read_falconx_json( unit, n_header=njson, error=err)
		if err then return,0

		n_buffer = 2000000L
		free_fx_record, pp
		pr = read_falconx_segments( file, n_buffer=n_buffer, unit=unit, header=json, version=version, $
						accept=normal_accept, njson=njson, /veto_progress)
		pp = ptr_new( pr)
		d = get_falconx_details( pp, header=json, njson=njson, info=info, $
						version=version, tick=tick, error=err)

		if err eq 0 then begin
			if (d.scan_dwell eq 0.) then begin
				gprint,level=2,'read_falconx_header: No dwell found on re-try!'
			endif
			last_file = file
			*last_head = d
		endif
	endif
endif

free_fx_record, pp
if err then return, 0L

err = 0
return, d
end

							