function Wizard_Load_plugins, error=error

	COMPILE_OPT STRICTARR
	ErrorNo = 0
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
	       warning,'Wizard_Load_plugins',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return, 0L
	    endif
	endif
	error = 1
	
	add_plugins = 0
	plugin_path = geopixe_root+'wizard'+slash()
	plugins = ptr_new()
    plugin_list = find_file2(plugin_path+'wizard_*.sav')
	if plugin_list[0] eq '' then begin
		plugin_title = ['-- none --']
		print,'Wizard: no Wizards found.'
	endif else begin
		nf = n_elements(plugin_list)
		print,'Wizard:  process ',nf,' plugin files ...'
		plugin_title = strarr(nf)

;		if catch_errors_on then begin
;			Catch, ErrorNo
;			if (ErrorNo ne 0) then begin
;				Catch, /cancel
;				warning,'Wizard_Load_plugins',['Errors detected in Wizards.', $
;						'Check Wizard SAV files.','','Make sure version is less than or', $
;						'equal to current IDL session,', $
;						'and at least v6.0 for VM mode.']
;
;				Catch, ErrorNo
;				if (ErrorNo ne 0) then begin
;					Catch, /cancel
;					on_error, 1
;					help, calls = s
;					n = n_elements(s)
;					c = 'Call stack: '
;					if n gt 2 then c = [c, s[1:n-2]]
;					warning,'Wizard_Load_plugins',['IDL run-time error caught.', '', $
;						'Error:  '+strtrim(!error_state.name,2), $
;						!error_state.msg,'',c,'','Check wizards for errors.'], /error
;					MESSAGE, /RESET
;					return, plugins
;				endif
;				return, plugins
;			endif
;		endif

		for i=0L,nf-1 do begin
			title = strip_path( plugin_list[i])
			title = strip_file_ext( title)
			plugin_title[i] = title
			print,'Wizard: register valid plugin: ', plugin_title[i]
		endfor
		add_plugins = 1
		plugins = ptr_new( {list:plugin_list, title:plugin_title})
		error = 0
	endelse
	return, plugins
end
