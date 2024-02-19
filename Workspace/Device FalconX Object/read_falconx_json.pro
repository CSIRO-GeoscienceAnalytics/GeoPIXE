function read_falconx_json, unit, n_header=njson, file=file, error=error

; Read the new JSON object text header

COMPILE_OPT STRICTARR
error = 1
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
       warning,'read_falconx_json',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   njson = 0
       return, ''
    endif
endif

	if n_elements(unit) eq 0 then begin
		if n_elements(file) eq 0 then begin
			warning,'read_falconx_json','No "unit" and no "file" provided."
			njson = 0
			error = 1
			return, ''
		endif
		local_open = 1
		openr, unit, file, /get_lun
	endif else local_open=0

more:
	njson = 0
	headerSize = 0L
	line = ''
    on_ioerror, bad
	readf, unit, line
	if line eq 'SiToro_List_Mode' then begin
		print,'read_falconx_json: found header "SiToro_List_Mode".
	endif else begin
		goto, rewindit
	endelse

	readf, unit, line
	s = strsplit( line, ' 	', /extract)
	ns = n_elements(s)
	if ns lt 2 then return,0

	headerSize = long(s[1])
	if (headerSize eq 0) then begin
		print,'???  read_falconx_json: headerSize=0, weird ...'
	    stat = fstat(unit)
	    njson = stat.cur_ptr
		header = ''
		goto, done
	endif
	b = bytarr(headerSize)
	readu, unit, b
	header = string(b)

    stat = fstat(unit)
    njson = stat.cur_ptr
	print,'read_falconx_json: read JSON, size=',njson
	error = 0
	goto, done

rewindit:
;	print,'???  read_falconx_json: rewind unit.'
	point_lun, unit, 0							; rewind unit
	njson = 0
	header = ''
	error = 0
	goto, done

bad:
	njson = 0
	header = ''
	error = 1
	goto, done
	
done:
	if local_open then close_file, unit
	return, header
end