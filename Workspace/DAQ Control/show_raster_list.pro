pro show_raster_list, step, com, arg, type

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'show_raster_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	n = n_elements(step)
	if n lt 1 then return

	print,'Raster list:'
	print, format='(2x,"i",2x,"step",11x,"command",6x,"argument",2x,"type")'
	lstep = 0
	for i=0,n-1 do begin
		if strlen(com[i]) gt 0 then begin
			if step[i] ne lstep then print,' '
			print,i,step[i],com[i],arg[i],type[i], format='(I3,2x,I3,2x,A17,2x,A12,2x,"(",A3,")")'
			lstep = step[i]
		endif
	endfor
	return
end
