function read_iXRF_json, unit, njson=njson, file=file, error=error

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
       warning,'read_iXRF_json',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   njson = 0
       return, ''
    endif
endif

	if n_elements(unit) eq 0 then begin
		if n_elements(file) eq 0 then begin
			warning,'read_iXRF_json','No "unit" and no "file" provided."
			njson = 0
			error = 1
			return, ''
		endif
		local_open = 1
		openr, unit, file, /get_lun
	endif else local_open=0

more:
	njson = 0
	headerSize = 0US
	line = ''
    on_ioerror, bad
	readu, unit, headerSize
	if (headerSize eq 0) then begin
		print,'???  read_iXRF_json: headerSize=0, weird ...'
	    stat = fstat(unit)
	    njson = stat.cur_ptr
		header = ''
		goto, done
	endif
	b = bytarr(headerSize)
	readu, unit, b

;	b2 = bytarr(2,headerSize/2)				; hack for 16-bit characters, change l;ater for UTF-8
;	b2[*] = b
;	b = reform( b2[0,*])

	header = json_parse( string(b))			;, /tostruct)

    stat = fstat(unit)
    njson = stat.cur_ptr
	print,'read_iXRF_json: read JSON, size=',njson
	error = 0
	goto, done

rewindit:
;	print,'???  read_iXRF_json: rewind unit.'
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