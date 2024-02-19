pro ftp_update, error=error, quiet=quiet

  	compile_opt strictarr
	now = fix_path(file_expand_path('.'))
	error = 1
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_update',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		cd, now
		return
	endif
	startupp, error=0
	if n_elements(quiet) eq 0 then quiet=1
	
	dir = file_requester(title='Select GeoPIXE download target directory',/dir)
	if dir eq '' then goto, done
	file_mkdir, dir
	cd, dir
	count = 0L
	
	label = ['Username:','Password:','FTP path:']
	text = ['NSLS','?%?)YS','/update/6.3']
	string_edit, title='Select FTP update path:', label=label, text=text, error=err
	if err then goto, done
	
	u = ftp_connect(dir=text[2], user=text[0],pass=text[1], quiet=quiet, error=err)
	if err then goto, bad
	
;	ignore = ['license.sav','geopixe.conf','Maia.conf','*.filter','Maia_*.parameters.csv']
	ignore = ['license.sav']

	ftp_dir_copy, u, count=count, quiet=quiet, ignore=ignore, error=error
	if error then goto, bad
	error = 0
	print,'   Updated ',strtrim(string(count),2),' files.'
	warning,'ftp_update','Updated file count = '+strtrim(string(count),2), /info
	
done:
	cd, now
	ftp_close, u
	return
	
bad:
	print,'    Update aborted. Updated ',strtrim(string(count),2),' files.'
	warning,'ftp_update',['Update interrupted by error.','Updated file count = '+strtrim(string(count),2)]
	goto, done
end
