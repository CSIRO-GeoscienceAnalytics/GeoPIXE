function get_depth_curve, file, error=error

; Read depth curve data

COMPILE_OPT STRICTARR
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
		warning,'get_depth_curve',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_file
	endif
endif
if n_elements(file) lt 1 then goto, bad_file
if strlen(file) lt 1 then goto, bad_file

	on_ioerror, bad_file
	error = 1
	openr, lun, file, /get_lun
	on_ioerror, bad_read
	line = ''
	readf, lun, line
	for i=0,2 do begin
		readf, lun, line
		s = strsplit( line, ', 	',/extract)
		case strlowcase(s[0]) of
			'z': begin
				z = fix2(s[1:*])
				end
			'shell': begin
				shell = fix2(s[1:*])
				end
			'element': begin
				element = s[1:*]
				end
		endcase
	endfor
	n = n_elements(z)+1
	data = fltarr(n,1000)
	on_ioerror, cont
	readf, lun, data

cont:
	q = where( data[0,*] gt 0, nd)
	if nd eq 0 then goto, bad_data
	data = data[*,0:nd-1]
	
	details = {n_el:n, n_depth:nd, z:z, shell:shell, element:element, $
				data:data}
	error = 0
	return, details

bad_file:
	warning,'get_depth_curve','File not found ='+file
	close_file, lun
	return, -1
bad_read:
	warning,'get_depth_curve','Bad read from file ='+file
	close_file, lun
	return, -1
bad_data:
	warning,'get_depth_curve','Bad data in file ='+file
	close_file, lun
	return, -1
end
