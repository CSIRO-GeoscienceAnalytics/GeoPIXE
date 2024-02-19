function get_daq_ylut, file, output=output, strip=strip, error=error

; Read a Maia Y lookup table file that lists the first Y value
; for each blog data file.
; Use the 'output' file path by default, else base it on the input 'file'.
; if /strip, then strip off trailing index number for cluster mode.

COMPILE_OPT STRICTARR
error = 1
if (n_elements(output) lt 1) then begin
	if (n_elements(file) lt 1) then return, 0L
	if (strlen(file) lt 1) then return, 0L
endif
if n_elements(strip) lt 1 then strip=0

; Look for an existing Y lookup table ...
; If 'output' has an index suffix (cluster) then append this to ylut
; file name too.

if n_elements(output) gt 0 then begin
	if n_elements(file) gt 0 then begin
		f0 = extract_path(output) + strip_file_ext( strip_path(file)) + '.ylut'
	endif else begin
		f0 = strip_file_ext( output, double=strip)+'.ylut'
	endelse
endif else begin
	f0 = strip_file_ext( file)+'.ylut'
endelse
print,'get_daq_ylut: try YLUT = ',f0
if file_test(f0) then begin
	on_ioerror, bad
	openr, lun, f0, /get_lun
	n = 0L
	readf, lun, n
	if n lt 1 then return, 0L
	ylut = lonarr(n)
	readf, lun, ylut
	close_file, lun
	error = 0
	return, ylut
endif else print,'get_daq_ylut: YLUT not found = ',f0
return, 0L

bad:
	close_file, lun
	print,'get_daq_ylut: error reading YLUT = ',f0
	return, 0L
end
	
