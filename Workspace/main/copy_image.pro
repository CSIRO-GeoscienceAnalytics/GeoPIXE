function copy_image, p1, no_image=no_image

; Copy the image pointed to by 'p1'

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
		warning,'Copy_image',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

if n_elements(p1) lt 1 then goto, bad
if ptr_valid(p1) eq 0 then goto, bad
if n_elements(no_image) lt 1 then no_image=0

	if no_image then begin
		copy_pointer_data, p1, p2, notag=['image','error'], /init
		
;		The 'notag' skips setting up a duplicate 'image', so it is still
;		pointing at the old ptr data. Hence, we clear that ptr in p2 copy here ...

		(*p2).image = ptr_new()
	endif else begin
		copy_pointer_data, p1, p2, /init
	endelse
	return, p2
	
bad:
	return, ptr_new()
	end