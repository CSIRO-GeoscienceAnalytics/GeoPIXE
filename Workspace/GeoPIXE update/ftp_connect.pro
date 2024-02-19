
pro ftp_post, u, cmd, res, out=out, count=count, quiet=quiet, error=error

;	Post a command 'cmd' to FTP port unit 'u'
;	res		return string lines
;	Out		is regex spec for termination return string.
;	Count	is count of return lines
;	error	1=error, 0=OK

  	compile_opt strictarr
	error = 0
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_post',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
	if n_elements(quiet) eq 0 then quiet=0
	
	; On some Unix systems the formatted print with only LF casues problems
	; so we add an explicit CR to make it conform to the FTP standard CR-LF.
	
	cr = string(13B)
	lf = string(10B)
	case !version.os_family of
		'unix': begin
			cmd0 = cmd + cr
			end
		else: begin
			cmd0 = cmd
			end
	endcase
	
	if (cmd ne '') then begin
		printf, u, cmd0, format='(a)'
		if quiet eq 0 then ftp_print, '>'+cmd
	endif
	if (size(out,/type) eq 0) then out='2?? *'

	res = ''
	catch, err
	if (err ne 0) then return
	line=''
	count=0
	while arg_present(res) do begin
		readf, u, line
		if count eq 0 then res=line else res=[res,line]
		count=count+1
    	if quiet eq 0 then ftp_print, '<'+line
		if strmatch(line,out) then break
		if strmatch(line,'4?? *') then goto, bad
		if strmatch(line,'5?? *') then goto, bad
	endwhile
	return

bad:
	error = 1
	return
end

;------------------------------------------------------------------------------

function ftp_help, u, name, list=list, quiet=quiet

;	Check 'name' against a set of recognized commands returned by HELP.
;	Return whether 'name' is valid and the command 'list' too.

  	compile_opt strictarr
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_help',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
	if n_elements(quiet) eq 0 then quiet=0
	
	ftp_post, u,'HELP', res, out='2?? *', count=count, quiet=quiet, error=err
	if err then goto, bad
	list = ''
	if count ge 3 then begin
		for i=1,count-2 do begin
			s = strsplit( res[i], ' 	', /extract)
			if i eq 1 then list=s else list=[list,s]
		endfor
	endif
	ok = 0
	q = where( list eq strupcase(name), nq)
	if nq ge 1 then ok=1
	return, ok
	
bad:
	error = 1
	return, 0
end

;------------------------------------------------------------------------------

function ftp_ignore, name, ignore=ignore

;	Check 'name' against a set of 'ignore' specs
;	If there is a match, ignore this file copy.

  	compile_opt strictarr
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_ignore',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
	ok = 0
	if n_elements(ignore) eq 0 then return, 0

	for i=0,n_elements(ignore)-1 do begin
		if strmatch( name, ignore[i]) then ok=1
	endfor
	return, ok
end

;------------------------------------------------------------------------------

pro ftp_parse_pasv, text, host, port, quiet=quiet

; 	Parse 'text' for PASV mode host IP and port details

  	compile_opt strictarr
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_parse_pasv',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
	if n_elements(quiet) eq 0 then quiet=0
	host = ''
	port = 0

	t = strtrim(text,2)
	ind = where(strcmp(t,'227',3))
	i = ind[0]
	if (i ne -1) then begin
		sub = stregex(t[i],'\([0-9,]*\)',/extract)
		p = strsplit(strmid(sub,1,strlen(sub)-2),',',/extract)
		p = strtrim(p,2)
		host = p[0]+'.'+p[1]+'.'+p[2]+'.'+p[3]
		port = 256*long(p[4])+long(p[5])
	endif
;	if quiet eq 0 then ftp_print, '         pasv: host='+host+', port='+string(port)
	return
end

;------------------------------------------------------------------------------

function ftp_file_time, u, file, seconds=seconds, proxy=proxy, modify=modify, error=error

;	Read the file 'file' MTIME
;	return time string unless: /seconds	and then return as UTC
;	/proxy		used via a proxy server (then use passed 'modify' time)
;	error	1=error, 0=OK

  	compile_opt strictarr
	socket_open = 0
	error = 0
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_file_time',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
	if n_elements(seconds) eq 0 then seconds=0
	if n_elements(proxy) eq 0 then proxy=0
	if n_elements(modify) eq 0 then modify=''
	if n_elements(file) eq 0 then return, 0LL
	if file eq '' then return, 0LL
	error = 1
	
	if proxy then begin
		error = 0
		s = modify
		if seconds then begin
			if s ne '' then begin
				year = long64(strmid( s,0,4))
				month = long64(strmid( s,4,2))
				day = long64(strmid( s,6,2))
				hour = long64(strmid( s,8,2))
				mins = long64(strmid( s,10,2))
				secs = long64(strmid( s,12,2))
				t1 = julday(month, day, year, hour, mins, secs)
				t0 = julday(1, 1, 1970, 0, 0, 0)
				t = long64(t1-t0)*3600*24
				return, t								; return UTC seconds
			endif else begin
				warning,'ftp_file_time','failed to get file time via passed LIST modify time.'
				return, 0LL
			endelse
		endif	
	endif else begin
		if modify eq '' then begin
			ftp_post, u, 'MDTM '+file, s, out='213 *', /quiet, error=err
			if err or (n_elements(s) eq 0) then begin
				warning,'ftp_file_time','failed to get file time via MDTM'
				return, 0LL
			endif
			s = (strlen(s) gt 4) ? strmid(s,4) : ''
			msg = 'MDTM #2'
		endif else begin
			s = modify
			msg = 'MLSD list'
		endelse
		error = 0
		if seconds then begin
			if s ne '' then begin
				year = long64(strmid( s,0,4))
				month = long64(strmid( s,4,2))
				day = long64(strmid( s,6,2))
				hour = long64(strmid( s,8,2))
				mins = long64(strmid( s,10,2))
				secs = long64(strmid( s,12,2))
				t1 = julday(month, day, year, hour, mins, secs)
				t0 = julday(1, 1, 1970, 0, 0, 0)
				t = long64(t1-t0)*3600*24
				return, t								; return UTC seconds
			endif else begin
				warning,'ftp_file_time','failed to get file time via '+msg
				return, 0LL
			endelse
		endif
	endelse
	return, s
end

;------------------------------------------------------------------------------

function ftp_file_size, u, file, quiet=quiet, error=error

;	Read the file 'file' size
;	error	1=error, 0=OK

  	compile_opt strictarr
	socket_open = 0
	error = 1
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_file_size',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
	if n_elements(quiet) eq 0 then quiet=0
	if n_elements(file) eq 0 then return, 0L
	if file eq '' then return, 0L
	
	ftp_post, u, 'SIZE '+file, res, out='213 *', quiet=quiet, error=error
	if error eq 0 then begin
		sz = long64(strmid(res[n_elements(res)-1],4))
	endif else sz=0L
	return, sz
end

;------------------------------------------------------------------------------

pro ftp_get_file, u, file, force=force, count=count, quiet=quiet, ignore=ignore, $
				maia_veto=maia_veto, proxy=proxy, modify=modify, error=error, cancel=cancel

;	Read the file 'file' and copy into the local current working dir
;	/quiet		suppress most print statements
;	/maia_veto	veto copying files in 'maia' dirs (except '*Maia.conf')
;	/force		force copy (ignore times)
;	/proxy		used via a proxy server (then use passed 'modify' time)
;	count		returns number transferred
;	cancel		1=cancel button pressed --> abort
;	error		1=error, 0=OK

  	compile_opt strictarr
	socket_open = 0
	error = 0
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_get_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		close_file, v
		ftp_post, u, '', res
		goto, done
	endif
	if n_elements(force) eq 0 then force=0
	if n_elements(count) eq 0 then count=0L
	if n_elements(quiet) eq 0 then quiet=0
	if n_elements(maia_veto) eq 0 then maia_veto=0
	if n_elements(proxy) eq 0 then proxy=0
	if n_elements(modify) eq 0 then modify=''
	if maia_veto and (file ne 'template.Maia.conf') then return
	file2 = file
	suppress = 0
	day_secs = 24L * 3600L
	
;	Check modify times. Only transfer older files if /force in effect ...
	
	info = file_info( file)
	if force eq 0 then begin
		if info.exists then begin
			tftp = ftp_file_time( u, file, /seconds, proxy=proxy, modify=modify, error=error)
			if error then return
			tfile = info.mtime
;			ftp_print,'ftp_get_file: Test file= '+file
;			ftp_print,'              Time local='+str_tidy(tfile)+' remote='+str_tidy(tftp)
			if tfile ge tftp + day_secs then begin
;				if file eq 'maia_control.sav' then begin
;				if file eq 'maia_client_parameters.sav' then begin
;					ftp_print,'ftp_get_file: '+file+' not new; do not copy. Time local='+str_tidy(tfile)+' remote='+str_tidy(tftp)
;				endif
				return
			endif
		endif
	endif

;	Check against 'ignore' list ...

	if info.exists and (n_elements(ignore) ge 1) then begin
		if ftp_ignore( file, ignore=ignore) then begin
			suppress = 1
			ftp_print,'      Ignore file= '+file, cancel=cancel
			if cancel then goto, done
			file2 = file + '.new'
			ftp_print,'             (transferred and saved locally as "'+file2+'")'
		endif
	endif
	socket_open = 0
	error = 1
	
;	Copy the file ...

	ftp_print,'      Copy file= '+file, cancel=cancel
	if cancel then goto, done
	sz = ftp_file_size( u, file, quiet=quiet, error=err)
	if (sz eq 0) then begin
		error = 0					; ***** try this?
		goto, done
	endif
	if err then begin
		ftp_print,'             (error retrieving file SIZE "'+file+'")'
		return
	endif
	ftp_post, u, 'PASV', res, quiet=quiet
	ftp_parse_pasv, res, host, port, quiet=quiet
	if quiet eq 0 then ftp_print,'   ... PASV using host='+host+', port='+port
	socket, v, host, port, connect_timeout=5, read_timeout=5, /get_lun, /rawio
	socket_open = 1
	ftp_post, u, 'RETR '+file, res, out='1?? *', quiet=quiet

	temp = file + '.part'
	openw, w, temp, /get_lun
    bufsize = (1024 * 1024LL) < sz
    buffer = bytarr(bufsize)
	
	catch, err
	if (err ne 0) then begin
		goto, done
	endif
	tc = 0LL
	while (tc lt sz) do begin
		if (sz-tc lt bufsize) then begin
			bufsize = sz-tc
			if bufsize eq 0 then goto, fin
			buffer = bytarr(bufsize)
		endif  
        readu, v, buffer, transfer_count=dtc
        if dtc lt bufsize then begin
;			ftp_print,'             (incomplete buffer read, Bufsize = '+str_tidy(bufsize)+', Transfer count = '+str_tidy(dtc)+')'
;			warning,'ftp_get_file',['incomplete buffer read for file:',file, $
;						'Bufsize = '+str_tidy(bufsize), $
;						'Transfer count read = '+str_tidy(dtc)]
;			goto, done
        endif
		if dtc gt 0 then writeu, w, buffer[0:dtc-1]
		tc = tc+dtc
	endwhile
fin:
	close_file, w
	file_move, temp, file2, /overwrite, verbose=(quiet eq 0)
	if suppress eq 0 then count = count+1

;	Check that transferred file has same size as remote file ...
	
	info = file_info( file2)
	if sz ne info.size then begin
		ftp_print,'             (inconsistent size, Local = '+str_tidy(info.size)+', Remote = '+str_tidy(sz)+')'
	endif

;	Correct scripts and .TXT files for end of line format ...
	
	ftp_test_script, file2
	error = 0

done:
	if socket_open then begin
		close_file, v
		ftp_post, u, '', res, quiet=quiet
	endif
	close_file, w
	return
end

;------------------------------------------------------------------------------

pro ftp_dir_copy, u, dir, force=force, count=count, quiet=quiet, error=error, $
						maia=maia, proxy=proxy, ignore=ignore, cancel=cancel

;	Copy the relative dir 'dir' to the same relative dir
;	location on local system.
;	Assumes FTP site is Unix based, and paths end in "/".
;
;	/force		force copy all (ignore times)
;	/quiet		suppress most messages
;	/maia		copy all 'maia' dir files, not just 'Maia.conf'
;	/proxy		used via a proxy server, so dumb it down to minimum
;	ignore		list of file specs to ignore (copy to .new)
;	count		copy file count (not counting .new ignore files)
;	error		=1 error return
;	cancel		=1 cancel button pressed return

  	compile_opt strictarr
	common ftp_common_1, pstate
	now = file_expand_path('.')
	cwd_up = 0
	error = 1
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_dir_copy',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, done
	endif
	if n_elements(dir) eq 0 then dir=''
	if n_elements(force) eq 0 then force=0
	if n_elements(count) eq 0 then count=0L
	if n_elements(quiet) eq 0 then quiet=0
	if n_elements(maia) eq 0 then maia=1
	if n_elements(proxy) eq 0 then proxy=0
	
	ftp_print,'Scan dir= ' + ((dir eq '') ? '<top level>' : dir), cancel=cancel
	if cancel then return
	maia_veto = 0
	if dir ne '' then begin
		if (maia eq 0) and (dir eq 'maia') then maia_veto=1
		safe_file_mkdir, dir, error=error
		if error then begin
			warning,'ftp_dir_copy',['failed to make local dir:',dir,'or it exists and is not writeable.']
			goto, done
		endif
		cd, dir
		ftp_cd, u, fix_path(dir,force='/'), quiet=quiet, error=error
		if error then begin
			warning,'ftp_dir_copy',['failed to remote CWD to:',dir]
			goto, done
		endif
		cwd_up = 1
	endif
	list = ftp_dir_list( u, proxy=proxy, quiet=quiet)		; current dir listing
	if list.n eq 0 then begin
;		warning,'ftp_dir_copy','Null dir list returned for "'+dir+'"'
		goto, done
	endif
	
	q = where( list.type eq 'file', nq)
	if nq gt 0 then begin
		for i=0L, nq-1 do begin
;			if list.name[q[i]] eq 'maia_client_parameters.sav' then begin
;				ftp_print, 'maia_client_parameters.sav: FTP modify='+list.modify[q[i]]
;			endif
			ftp_get_file, u, list.name[q[i]], force=force, ignore=ignore, count=count, quiet=quiet, $
									maia_veto=maia_veto, proxy=proxy, modify=list.modify[q[i]], error=error, cancel=cancel
			if cancel then goto, done
			if error then begin
				warning,'ftp_dir_copy',['failed to copy '+list.type[q[i]],list.name[q[i]]]
				goto, done
			endif
			if strlowcase(list.name[q[i]]) eq 'news.txt' then begin
				if ptr_good(pstate,/struct) then begin
					news, list.name[q[i]]
				endif
			endif
		endfor
	endif

	q = where( list.type eq 'dir', nq)
	if (nq gt 0) and (maia_veto eq 0) then begin
		for i=0L, nq-1 do begin
			ftp_dir_copy, u, list.name[q[i]], force=force, ignore=ignore, count=count, quiet=quiet, $
									maia=maia, proxy=proxy, error=error, cancel=cancel
			if cancel then goto, done
			if error then begin
				warning,'ftp_dir_copy',['failed to copy '+list.type[q[i]],list.name[q[i]]]
				goto, done
			endif
		endfor
	endif
	error = 0
	
done:
	if cwd_up then begin
		ftp_cd_up, u, quiet=quiet, error=err
		if err then begin
			warning,'ftp_dir_copy',['failed to CWD up a level from:',dir]
			error = 1
		endif
	endif
	cd, now
	return
end

;------------------------------------------------------------------------------

function ftp_dir_list, u, dir, count=count, proxy=proxy, quiet=quiet, error=err

;	Get dir list for directory name 'dir'
;	/quiet		suppress most messages
;	/proxy		used via a proxy server, so dumb it down to minimum
;	return		{n:nl, name:name, modify:modify, type:type}
;	count		count of returns, same as {n}

  	compile_opt strictarr
	Catch, ErrorNo
	err = 1
	host = '?'
	port = 21
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_dir_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'','Host = '+host[0], '',c], /error
		MESSAGE, /RESET
		close_file, v
		ftp_post, u, '', res
		return, {n:0}
	endif
	if n_elements(dir) eq 0 then dir=''
	if n_elements(quiet) eq 0 then quiet=0
	if n_elements(proxy) eq 0 then proxy=1

;	First check whether the FTP server can handle MLSD commands.
;	If not, dumb it down, as for proxy server.

	advanced = ftp_help( u, 'MLSD', quiet=quiet, list=list)
	if advanced eq 0 then proxy=1
	
	ftp_post, u, 'PASV', res, quiet=quiet
	ftp_parse_pasv, res, host, port, quiet=quiet
	if quiet eq 0 then ftp_print,'   ... PASV using host='+host+', port='+string(port)
	socket, v, host[0], port, connect_timeout=5, read_timeout=5, /get_lun
	com = proxy ? 'LIST '+dir : 'MLSD '+dir
	ftp_post, u, com, res, out='1?? *', quiet=quiet, error=err
	if err then return, {n:0}
	
	list = ''
	line = ''
	count = 0L
	catch, err
	if (err ne 0) then begin
		Catch, /cancel
		goto, done
	endif
	repeat begin
		readf, v, line
		if count eq 0 then list=line else list=[list,line]
		count = count+1
	endrep until 0
done:
	close_file, v
	ftp_post, u, '', res, quiet=quiet

	err = 1
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_dir_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, {n:0}
	endif
	
	nl = count
	if (nl eq 0) then return, {n:0}
	if (nl eq 1) then if (list[0] eq '') then return, {n:0}

; If use "LIST" (/proxy) then have a format like this:
;		-rw-r--r--   1 rya113   ftp          1992 Feb 23 22:36 APS Vortex SDD.detector 
;		drwxr-xr-x   7 rya113   ftp          1536 Apr 16 04:58 Help
;		-rw-r--r--   1 rya113   ftp          1992 Feb 23 2004  Old.detector 
; from privateftp.csiro.au:		
;		-rw-r--r--    1 1174     600          1992 Dec 05 02:23 APS Vortex SDD.detector
;    NOTE: if old year, then no time, so we need to fiddle that below ...
;
; If "MLSD" (proxy=0) then have a format like this:
;		modify=20120223223631;perm=adfr;size=1992;type=file;unique=780040U26BCE;UNIX.group=99;UNIX.mode=0644;UNIX.owner=1116; APS Vortex SDD.detector
;		modify=20120416045813;perm=fle;type=dir;unique=780040U2664D;UNIX.group=99;UNIX.mode=0755;UNIX.owner=1116; Help
	
	name = strarr(nl)
	type = strarr(nl)
	modify = strarr(nl)
	for i=0,nl-1 do begin
		if proxy then begin															; /proxy LIST command
			str = strsplit( list[i], ' 	', /extract, count=n)
			if n ge 9 then begin
				s = strmid(str[0],0,1)
				type[i] = (s eq 'd') ? 'dir' : 'file'
				name[i] = strjoin(str[8:*],' ')
				stfile = 'Mon ' + str[5] + ' ' + str[6]								; seems to ignore bogus day of week
				if strpos(str[7],':') ge 0 then begin								; date has time, no year
					st0 = systime()
					tar0 = bin_date( st0)
					tsys = julday( tar0[1], tar0[2], tar0[0], tar0[3], tar0[4], tar0[5])
					stfile = stfile + ' ' + str[7] + ':00 '
					tar = bin_date( stfile + strtrim(string(tar0[0]),2))
					t1 = julday( tar[1], tar[2], tar[0], tar[3], tar[4], tar[5])
					if t1 gt tsys+1 then begin
						tar = bin_date( stfile + strtrim(string(tar0[0]-1),2))		; must be previous year
					endif
				endif else begin													; date has year, no time
					stfile = stfile + ' 23:59:00 ' + str[7]							; default to a late hour
					tar = bin_date( stfile )
				endelse
				year = strtrim(string(tar[0]),2)
				month = strtrim(string(tar[1]),2)
				if strlen(month) eq 1 then month = '0'+month
				day = strtrim(string(tar[2]),2)
				if strlen(day) eq 1 then day = '0'+day
				hour = strtrim(string(tar[3]),2)
				if strlen(hour) eq 1 then hour = '0'+hour
				min = strtrim(string(tar[4]),2)
				if strlen(min) eq 1 then min = '0'+min
				sec = strtrim(string(tar[5]),2)
				if strlen(sec) eq 1 then sec = '0'+sec

				modify[i] = year + month + day + hour + min + sec
;				print,list[i]+' --> '+modify[i]
			endif
		endif else begin															; MLSD command return
			str = strsplit( list[i], ';', /extract, count=n)
			if n ge 1 then begin
				for j=0,n-1 do begin
					s = strsplit( str[j], '=', /extract, count=ns)
					if ns eq 1 then begin
						name[i] = strtrim(s[0],2)
					endif else begin
						case s[0] of
							'modify': begin
								modify[i] = s[1]
								end
							'type': begin
								type[i] = s[1]
								end
							else:
						endcase
					endelse
				endfor
			endif
		endelse
	endfor
	err = 0
	return, {n:nl, name:name, modify:modify, type:type}
end

;------------------------------------------------------------------------------

pro ftp_close, u

  	compile_opt strictarr
	close_file, u
	return
end

;------------------------------------------------------------------------------

pro ftp_cd, u, dir, quiet=quiet, error=error

	compile_opt strictarr
	error = 1
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_cd',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		ftp_post, u, '', res
		return
	endif
	if n_elements(dir) eq 0 then dir=''
	if n_elements(quiet) eq 0 then quiet=0
	if dir eq '' then return

	ftp_post, u, 'CWD '+dir, res, quiet=quiet, error=error
	return
end

;------------------------------------------------------------------------------

pro ftp_cd_up, u, quiet=quiet, error=error

	compile_opt strictarr
	error = 1
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_cd_up',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		ftp_post, u, '', res
		return
	endif
	if n_elements(quiet) eq 0 then quiet=0

	ftp_post, u, 'CDUP', res, quiet=quiet, error=error
	return
end

;------------------------------------------------------------------------------

pro ftp_print, s, cancel=cancel

;	Print a line to console and also back in FTP progress window (if pstate valid)
;	Cancel=1 indicates that the cancel button has been pressed.

	compile_opt strictarr
	common ftp_common_1, pstate
	error = 1
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_print',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
	if n_elements(s) eq 0 then return
	print, s
	
	if ptr_good(pstate,/struct) then begin
		ftp_update_progress, pstate, s, cancel=cancel
	endif else cancel=0
	return
end

;------------------------------------------------------------------------------

pro ftp_test_script, file

;	Test local file 'file' and if it is a shell script, then
;	1.	strip out <cr> from <cr><lf> pairs
;	2.	chmod a+x on it.
;	Implemented by formatted read/write to native text on local system.

	compile_opt strictarr
	common ftp_common_1, pstate
	error = 1
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_test_script',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, cont
	endif
	if n_elements(file) eq 0 then return
	if file eq '' then return

;	Check for likely script file (no extension) ...

	script = 0
	if locate( '.', file) eq -1 then script=1

;	Check for a text file by extension ...

	ascii = ['txt','html','htm','bat','mp','dmt','c','h','f','f90','csv','conf', $
				'pro','var','paths','ylut']
	text = 0
	ext = strlowcase( extract_extension( file))
	q = where( ext eq ascii, nq)
	if nq ge 1 then text=1
	if (text eq 0) and (script eq 0) then return
	
	on_ioerror, cont
	if script then begin
		openr, lun, file, /get_lun
		b = bytarr(10)
		readu, lun, b
		close_file, lun
		s = string(b[0:1])
		if s ne '#!' then return
	endif
	
	file2 = file+'.part'
	openr, lun, file, /get_lun
	openw, lun2, file2, /get_lun
	s = ''
	on_ioerror, cont
	while not EOF(lun) do begin
		readf, lun, s
		printf, lun2, s
	endwhile
	close_file, lun2
	close_file, lun

	file_move, file2, file, /overwrite, verbose=0
	if script then begin
		file_chmod, file, /a_execute
		ftp_print,'             (check script and chmod file= "'+file+'")'
	endif
	if text then begin
		ftp_print,'             (check text format file= "'+file+'")'
	endif
	
cont:
	close_file, lun2
	close_file, lun
	return
end

;------------------------------------------------------------------------------

function ftp_connect, site, port=port, dir=dir, user=user, pass=pass, $
		pstate=ps, quiet=quiet, proxy=proxy, pport=pport, error=error
             
;	Connect to FTP site 'site', defaults to port 'ftp'
;	Set remote working dir to 'dir', else top level dir on connection
;	Use 'user', 'pass' for authentication.
;	
;	pstate		pstate ptr in geopixe_update (used for 'printing' messages back there)
;	/quiet		suppress verbose messages
;	proxy		proxy server, else leave blank
;	pport		proxy port, else blank
;	error		1=error, 0=OK returnuser

	compile_opt strictarr
	common ftp_common_1, pstate
	pstate = 0L
	error = 1
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_connect',['Error conncting to FTP socket.','IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), !error_state.msg,'',c], /error
		MESSAGE, /RESET
		close_file, u
		return, 0L
	endif
	if n_elements(quiet) eq 0 then quiet=0
	if n_elements(ps) ne 0 then pstate=ps
	if n_elements(proxy) eq 0 then proxy=''
	if n_elements(pport) eq 0 then pport=''

	if n_elements(site) eq 0 then site='privateftp.csiro.au'
	if strcmp(site,'ftp://',6) then host=strmid(site,6) else host=site
	pos = strpos(host,'/')
	if pos ge 0 then begin
		dir = strmid(host,pos)
		host = strmid(host,0,pos)
	endif
	if (size(user,/type) eq 0) then goto, bad1
	if (size(pass,/type) eq 0) then goto, bad1

	if lenchr(proxy) gt 0 then begin
		user = strtrim(user,2) + '@' + strtrim(host,2)
		if n_elements(port) ne 0 then user = user + ':' + strtrim(string(port),2)
		host = proxy
		port = 21
		if lenchr(pport) gt 0 then port=long(pport)
	endif else begin
;		if n_elements(port) eq 0 then port='ftp'
		if n_elements(port) eq 0 then port=21
	endelse
	
	if quiet eq 0 then ftp_print, '  ftp connect: host='+host+', port='+string(port)
	socket, u, host[0], port[0], connect_timeout=30, read_timeout=30, write_timeout=30, /get_lun

	ftp_post, u, 'PASV', res, quiet=quiet		; ?? cos it works on Linux at DESY!
												; may be blocking port 20 sometimes?

	ftp_post, u, '', res, quiet=quiet
	ftp_post, u, 'USER '+user, res, out='3?? *', quiet=quiet, error=error
	if error then goto, bad1
	ftp_post, u, 'PASS '+pass, res, quiet=quiet, error=error
	if error then goto, bad1
	ftp_post, u, 'TYPE I', res, quiet=quiet, error=error
	if error then goto, bad3
	if (size(dir,/type) ne 0) then ftp_post, u, 'CWD '+dir, res, quiet=quiet, error=error
	if error then goto, bad4
	ftp_print,'FTP connection successful.'
	error = 0
	return, u
	
bad1:
	ftp_print,'Invalid username/password.'
	goto, bad
bad3:
	ftp_print,'Error setting FTP transfer TYPE.'
	goto, bad
bad4:
	ftp_print,'Failed to CWD to dir: '+dir
	goto, bad
	
bad:
	close_file, u
	return, 0L
end
