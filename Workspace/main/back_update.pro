pro back_update, list=back_list, title=back_title, $
					present=present, new=new, file=file, count=count, error=error

; Check for new back plugin files, and update local lists, and the
; current droplist settings.
;
; list		the new back plugin file list found (no paths)
; title		the new back plugin title strings
; count		total number found
;
; present	the name of the current back file list member
; new		the new 'index' to the present back within the new list returned
; file		return full filename with path too

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
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
	       warning,'back_update',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
		   back_list = ''
		   back_title = 'No "title" return'
	       return
	    endif
	endif
	error = 1

	read_filts = 0
	count = 0
	if arg_present(back_title) then read_filts=1

	plugin_path = geopixe_root+'plugins'+slash()
	back_list = find_file2(plugin_path+'*_back_plugin.sav')
	
	back_title = back_list
	back_title[*] = ''
	nf = n_elements( back_list)
	count = nf
	ok = replicate( 1, nf)
	
;	Catch, ErrorNo
;	if (ErrorNo ne 0) then begin
;		Catch, /cancel
;		warning,'GeoPIXE',['Errors detected in Back plugins.', $
;				'Check Back plugin SAV files.','','Make sure version is less than or', $
;				'equal to current IDL session,', $
;				'and at least v6.0 for VM mode.']
;
;		if catch_errors_on then begin
;			Catch, ErrorNo
;			if (ErrorNo ne 0) then begin
;				Catch, /cancel
;				on_error, 1
;				help, calls = s
;				n = n_elements(s)
;				c = 'Call stack: '
;				if n gt 2 then c = [c, s[1:n-2]]
;				warning,'back_update',['IDL run-time error caught.', '', $
;						'Error:  '+strtrim(!error_state.name,2), $
;						!error_state.msg,'',c,'','Check plugins for errors.'], /error
;				MESSAGE, /RESET
;				return
;			endif
;		endif
;		return
;	endif

	if back_list[0] ne '' and read_filts then begin
		print,'back_update:  process ',nf,' back plugin files ...'
		for i=0L,nf-1 do begin
			print,'Back: restore plugin: ', back_list[i]
			restore, back_list[i], /verbose
			back_list[i] = strip_path( back_list[i])
			back_list[i] = strip_file_ext( back_list[i])
			t = call_function( back_list[i], title=title)
			if n_elements(title) lt 1 then begin
				back_title[i] = 'No "title" return'
				ok[i] = 0
			endif else begin
				back_title[i] = title
				ok[i] = 1
			endelse
			print,'back_update: register plugin: ', back_title[i]
		endfor
	endif
	back_files = back_list
	back_list = strip_path( back_list)

	q = where( (ok eq 1) and (back_list ne ''))
	if q[0] eq -1 then begin
		back_title = ['-- none --']
		back_list = ''
		back_files = ''
		count = 0
	endif else begin
		q2 = sort_unique( back_list[q], ns, /last)
		back_title = back_title[q[q2]]
		back_list = back_list[q[q2]]
		back_files = back_files[q[q2]]
		count = ns
	endelse

	new = -1
	if size( present, /tname) eq 'STRING' then begin
		q = where( back_list eq present)			; assumes filename without path
		if q[0] ne -1 then begin
			new = q[0]
			file = back_files[q[0]]
		endif else begin
			q = where( back_title eq present)		; else compare titles
			if q[0] ne -1 then begin
				new = q[0]
				file = back_files[q[0]]
			endif
		endelse
	endif
	error = 0
	return
end
