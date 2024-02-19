function read_midas_header, unit, error=err, no_scan=no_scan

; /no_scan to suppress scanning for X range if a scan record is not found.
; Now that we store the last header until file changes, we don't need /no_scan.

COMPILE_OPT STRICTARR
common c_midas_10, last_file, last_head
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
       warning,'read_midas_header',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       free_midas_record, pp
       err = 1
       return, 0L
    endif
endif

	err = 1
	if n_elements(no_scan) lt 1 then no_scan=0
;	n_buffer = (no_scan eq 0) ? 2000000L : 500000L
	n_buffer = 2000000L
	print,'read_midas_header: n_buffer = ',n_buffer
	
	stat = fstat( unit)
	file = stat.name
	if file eq last_file then begin
		err = 0
		return, *last_head
	endif

	pr = read_midas_segments( file, n_buffer=n_buffer, unit=unit, /veto_progress)
	pp = ptr_new( pr)
	d = get_midas_details( pp, error=err)

	if ptr_good(pp) then free_midas_record, pp
	if err then return, 0L

	err = 0
	return, d
end

							