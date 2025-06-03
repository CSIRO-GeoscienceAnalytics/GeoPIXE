pro image_DA_preview, file, preview=preview

; Provide preview of DA matrix details for 'file_requester' from 'file'.
; Return preview data in 'preview' struct.

COMPILE_OPT STRICTARR
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
		warning,'image_DA_preview',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
define_devices

	preview = 0L
	if n_elements(file) lt 1 then return
	
	p = ptr_new( read_da( file, error=error, mpda=mpda))
	if error ne 0 then return
	if ptr_valid(p) eq 0 then return

;------------------------------------------------------------
; Details string array:

	list = 'File: ' + strip_path(file)

	List = [list, '# elements: ' + str_tidy((*p).n_el), '' ]

	n = n_elements((*p).el)
	i = 0
	repeat begin
		list = [list, strjoin((*p).el[i:(i+9)<(n-1)],' ') ]
		i = i+10
	endrep until i gt n-1

;------------------------------------------------------------

	free_DA, p

	preview = {details:list}
	return
end