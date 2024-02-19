pro warning, routine, message, error=error, info=info, cancel=cancel, output=output, timeout=timeout

COMPILE_OPT STRICTARR
common c_debug_warnings, enable_warning_popup
if n_elements(enable_warning_popup) lt 1 then enable_warning_popup=1
common c_debug_linux_bug, first						;@8-18
if n_elements(first) lt 1 then first=3				;@8-18

if n_elements(routine) lt 1 then routine='Unknown'
if n_elements(error) lt 1 then error=0
if n_elements(info) lt 1 then info=0
if n_elements(timeout) lt 1 then timeout=0.
cancel = 0

log_warning, routine, message, error=error, info=info

if timeout gt 0. then begin
	warning_popup, routine, message, error=error, info=info, cancel=cancel, timeout=timeout, output=output
	return
endif

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		ErrArray = ['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c]
		if enable_warning_popup then begin
			a = dialog_message(ErrArray, /error)
		endif else begin
			gprint,level=2, output=output, '====== Error Catch in Warning =======', ErrArray
		endelse

;		Due to an odd bug with very first pop-up, we try again ...		;@8-18

		if first gt 0 then begin						;@8-18
loop:
			first = (first-1) >0
			Catch, ErrorNo
			if (ErrorNo ne 0) then begin
				Catch, /cancel
				help, calls = s
				n = n_elements(s)
				c = 'Call stack: '
				if n gt 2 then c = [c, s[1:n-2]]
				ErrArray = ['IDL run-time error caught.', '', $
						'Error:  '+strtrim(!error_state.name,2), $
						!Error_state.msg,'',c]
				if enable_warning_popup then begin
					a = dialog_message(ErrArray, /error)
				endif else begin
					gprint,level=2, output=output, '====== Error Catch in Warning =======', ErrArray
				endelse
				if first gt 0 then goto, loop
				return
			endif
		endif else return
	endif
endif

	r = string(routine)
	
	if n_params() lt 1 then return
	serr = 'WARNING in'
	if error then serr = 'ERROR in'
	if info then serr = 'Information from'
	
	if n_params() lt 2 then begin
		ErrArray = [serr+' routine '+r+': ', 'Error Number:'+strtrim(!error,2), !Err_String]
	endif else begin
		ErrArray = [serr+' routine '+r+': ', string(message)]
	endelse

;	This test to reduce print traffic in background processes that fill up stdout buffer ...
	if n_elements(output) ne 0 then gprint,level=2, output=output, '=========================', ErrArray

	if enable_warning_popup then begin
		if error then begin
			a = dialog_message(ErrArray, /error, cancel=arg_present(cancel))
		endif else if info then begin
			a = dialog_message(ErrArray, /information, cancel=arg_present(cancel))
		endif else begin
			a = dialog_message(ErrArray, cancel=arg_present(cancel))
		endelse
		if a eq 'Cancel' then cancel=1
	endif else begin
;		if n_params() lt 2 then begin
;			gprint,level=2, output=output, '========================= '+serr+' routine: ',r,', error Number:',strtrim(!error,2),!Err_String
;		endif else begin
;			gprint,level=2, output=output, '========================= '+serr+' routine: ',r, ', ', string(message)
;		endelse
	endelse

	return
end
