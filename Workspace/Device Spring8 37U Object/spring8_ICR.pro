function spring8_ICR, file2, error=error

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
       warning,'spring8_ICR',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
	   return, 0
   endif
endif
error = 1

	file = strip_file_ext( file2)
	file = strip_file_m( file, ending = ['_ch0','_ch1','_ch2','_ch3','_ch4'])
	file = strip_file_m( file, ending = '_mca')
	file1 = file + '.txt'

	file5 = strip_file_ext( file1) + '.ICR'

	version = 0L
	xsize = 0L
	ysize = 0L

	on_ioerror, bad_ICR
	openr, unit5, file5, /get_lun, /XDR

	on_ioerror, bad_ICR2
	readu, unit5, version
	readu, unit5, xsize, ysize

	ICR = fltarr(xsize,ysize)
	readu, unit5, ICR
	close_file, unit5

	error = 0
	return, ICR

bad_ICR:
	print,'spring8_ICR: bad open of ICR file: '+file5
	close_file, unit5
	return, 0
bad_ICR2:
	print,'spring8_ICR: bad read from ICR file: '+file5
	close_file, unit5
	return, 0
end
