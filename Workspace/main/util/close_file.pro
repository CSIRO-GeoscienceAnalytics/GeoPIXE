pro close_file, lun, keep=keep, _extra=extra

COMPILE_OPT STRICTARR
ErrorNo = 0
Catch, ErrorNo
if (ErrorNo ne 0) then begin
	Catch, /cancel
	return
endif
if n_elements(keep) lt 1 then keep=0

if n_elements(lun) lt 1 then return
if lun eq 0 then return

flush, lun
if keep then begin
	close, lun, _extra=extra
endif else begin
	free_lun, lun, _extra=extra
endelse
return
end