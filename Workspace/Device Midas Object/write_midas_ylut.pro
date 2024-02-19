pro write_midas_ylut, ylut, file, output=output, strip=strip, error=error

; Write out a Y lookup table that lists the first Y value
; in each segment file, indexed by segment file number (extension).
; Use the 'output' file path by default, else base it on the input 'file'.
; If output has a 'nn' after DAI, then append this too.
; /strip remove trailing index after ".ylut" for cluster output files.

error = 1
if (n_elements(output) lt 1) then begin
	if (n_elements(file) lt 1) then return
	if (strlen(file) lt 1) then return
endif
if n_elements(ylut) lt 1 then return
if n_elements(strip) lt 1 then strip=0

; Write ylut file. If 'output' has index (cluster) suffix, then
; append it to ylut file name too, unless /strip is used.

if n_elements(output) gt 0 then begin
	if n_elements(file) gt 0 then begin
		f0 = extract_path(output) + strip_file_ext( strip_path(file)) + '.ylut'
	endif else begin
		f0 = strip_file_ext( output, double=strip)+'.ylut'
	endelse
endif else begin
	f0 = strip_file_ext( file)+'.ylut'
endelse

on_ioerror, bad_write
openw, lun, f0, /get_lun

printf, lun, n_elements(ylut)
printf, lun, ylut
close_file, lun

error = 0
return

bad_write:
	warning,'write_midas_ylut','error writing Y lookup table.',/error
	close_file, lun
	return
end
