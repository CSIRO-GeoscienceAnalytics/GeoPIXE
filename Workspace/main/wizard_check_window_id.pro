pro wizard_check_window_id, open=windows_open, needed=windows_needed, name=name, id=id, count=count, error=error

;	Check the window 'id' with name 'name' against those stored for this window 'name'
;	in 'windows_open' ptr array indexed against names list in 'windows_needed'..

COMPILE_OPT STRICTARR
error = 1
count = 0
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
		warning,'wizard_check_window_id',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif
if n_elements(name) eq 0 then return
if n_elements(id) eq 0 then return

	if widget_info( id, /valid) eq 0 then return
	q = where( name eq windows_needed, nq)				; index to needed windows
	if nq eq 0 then return

	current = *windows_open[q[0]]						; current open IDs
	if n_elements(current) eq 0 then begin
		current = id
	endif else begin
		current = [current, id]
	endelse
	current = current[ uniq( current, sort(current))]	; eliminate duplicates

	nc = n_elements(current)
	good = intarr(nc)
	for i=0,nc-1 do begin
		good[i] = widget_info( current[i], /valid)		; check all to be valid
	endfor
	q1 = where( good eq 1, nq1)
	if nq1 ge 1 then begin
		*windows_open[q[0]] = current[q1]
	endif else begin
		*windows_open[q[0]] = null
	endelse

	count = n_elements(*windows_open[q[0]])				; current unique open count

;	print,'name,id,current=',name,'  ',id,'  ',*windows_open[q[0]]
	error = 0
	return
end
