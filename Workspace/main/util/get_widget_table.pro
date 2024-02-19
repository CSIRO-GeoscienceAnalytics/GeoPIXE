pro get_widget_table, table, view=view, rows=rows, columns=columns

; Get certain parameters from a table widget.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'get_widget_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	view = [0,0]
	rows = 0
	columns = 0
	if widget_info( table, /valid) eq 0 then return

	view = widget_info( table, /table_view)
	widget_control, table, get_value=t
	columns = n_elements( t[*,0])
	rows = n_elements( t[0,*])
	return
end
	