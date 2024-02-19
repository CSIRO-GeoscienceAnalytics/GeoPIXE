function get_falconx_ylut, file, output=output, strip=strip, error=error, embed_detector=embed_detector

; Read a falconx Y lookup table file that lists the first Y value
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
if n_elements(embed_detector) eq 0 then embed_detector=0

; Look for an existing Y lookup table ...
; If 'output' has an index suffix (cluster) then append this to ylut
; file name too.

if n_elements(output) gt 0 then begin
	path = extract_path(output)
endif else begin
	path = extract_path(file)
endelse
if n_elements(file) gt 0 then begin
	f0 = strip_file_ext( strip_path(file[0]))
endif else begin
	f0 = strip_file_ext( strip_path(output[0]), double=strip)
endelse

i = locate_last('_',f0)
if i gt 0 then f0=strmid(f0,0,i)

if embed_detector then begin
	m = locate_last( '_', f0)				; "_" before detector number
	if m gt 0 then begin
		f0 = strmid( f0,0,m)
	endif
endif

f0 = path + f0 + '.ylut'

ylut = 0L
print,'get_falconx_ylut: try YLUT = ',f0
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
endif else print,'get_falconx_ylut: YLUT not found = ',f0
return, 0L

bad:
	close_file, lun
	print,'get_falconx_ylut: error reading YLUT = ',f0
	return, 0L
end
	
