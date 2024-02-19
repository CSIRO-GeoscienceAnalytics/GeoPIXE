pro allocate_pixmap, width, height, new_wid=wid, old_wid=old, error=error, $
				_EXTRA=_VWBExtra_

; Allocate a new pixmap area. 
; Return window ID in 'wid' if not error.
; Deallocate 'old' if new succeeds.

error = 1
wid = -1
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
		warning,'allocate_pixmap',['IDL run-time error caught.', 'Error allocating image pixmap.','', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		print,'Error allocating new pixmap, w,h=',width,height
		return
	endif
endif
if n_elements(width) lt 1 then goto, bad
if n_elements(height) lt 1 then goto, bad

	present = 0
	if n_elements(old) eq 1 then begin
		wset,old
		present = (!d.window eq -1) ? 0 : 1 
		if (!d.x_size eq width) and (!d.y_size eq height) and (!d.window eq old) then begin
			print,'no change to pixmap'
			wid = old
			error = 0
			return
		endif
	endif
	
	; if break here on debug error, set ERROR=1 and RETURN.
	window, /free, xsize=width>1, ysize=height>1, /pixmap, retain=0, _EXTRA=_VWBExtra_
	wid = !d.window
	wset, wid
	print,'allocate new pixmap ',wid,', w,h=',width,height
	
	if present then wdelete, old
	error = 0
	return

Bad:
	warning,'allocate_pixmap','Missing arguments.'
	return
end
