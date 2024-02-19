;
;	Update GeoPIXE and optionally Maia files and directories to a target system.
;
;	Uses config file 'geopixe-update.conf' in home .geopixe dir for settings.
;	These are saved on completion of a transfer and on exit.
;	
;	Uses a set of 'ignore' file specifications to ignore during transfer.
;	These files are protected on target system. However, a new file of this name will be
;	transferred to a file with a '.new' suffix. Check these for new fearures.
;	
;	/maia	update Maia files in any 'maia' directory (will always update '*Maia.conf').
;			have renamed 'Maia' dir in 'setup' so it is unaffected by this test.
;
pro geopixe_update_event, event

	COMPILE_OPT STRICTARR
	ErrorNo = 0
	now = fix_path(file_expand_path('.'))
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
			warning,'geopixe_update_event',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			cd, now
			return
		endif
	endif
	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate	
	if ptr_good(pstate,/struct) eq 0 then goto, bad_state
  
	case tag_names( event,/structure) of
		'WIDGET_TRACKING': begin
			if (*pstate).tracking eq 0 then return
			widget_control, event.id, get_uvalue=s
			if size(s,/tname) eq 'STRING' then begin
				if event.enter eq 1 then begin
					widget_control, (*pstate).help, set_value=s
				endif else begin
					widget_control, (*pstate).help, set_value='Select FTP site details and your local GeoPIXE directory to update and press "Start". ' + $
							'Files matching the "Ignore" list specs will not be transferred if a matching file exists locally.
				endelse
			endif
			return
			end
		else:
	endcase

  uname = widget_info( event.id, /uname)
  case uname of

	'geopixe-update-tlb': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_KILL_REQUEST': begin
				goto, kill
				end
			'WIDGET_BASE': begin
				x = (event.x > 365) - (*pstate).xoffset
				y = ((event.y > 580) - (*pstate).yoffset) > 10
				widget_control, (*pstate).progress_list, scr_xsize=x, scr_ysize=y
				end
			else:
		endcase
		end
	'host-text': begin
		widget_control, event.id, get_value=s
		(*pstate).host = s
		end
	'user-text': begin
		widget_control, event.id, get_value=s
		(*pstate).user = s
		end
	'pass-text': begin
		widget_control, event.id, get_value=s
		(*pstate).pass = s
		end
	'path-text': begin
		widget_control, event.id, get_value=s
		(*pstate).path = s
		end
	'proxy-text': begin
		widget_control, event.id, get_value=s
		(*pstate).proxy = s
		end
	'port-text': begin
		widget_control, event.id, get_value=s
		(*pstate).port = s
		end

	'query': begin
		ftp_update_strings, pstate
		*(*pstate).pprogress = ''
		error = 1

		u = ftp_connect( (*pstate).host, dir='/update', user=(*pstate).user, pass=(*pstate).pass, $
								pstate=pstate, /quiet, error=error)
		if error then begin
			ftp_update_progress, pstate, ['Error connecting to FTP server.','Abort transfer.']
			goto, done
		endif
		list = ftp_dir_list( u, proxy=(*pstate).use_proxy, error=error, count=count, /quiet)
		if error or (count eq 0) then begin
			ftp_update_progress, pstate, ['Error retrieving update version list.','Abort transfer.']
			goto, done
		endif
		ftp_update_progress, pstate, 'Available versions on FTP site "update" dir:'
		for i=0,count-1 do begin
			if list.type[i] eq 'dir' then ftp_update_progress, pstate, '    ' + list.name[i]
		endfor
		end

	'dir-text': begin
		widget_control, event.id, get_value=s
		(*pstate).dir = s
		safe_file_mkdir, s, error=error
		if error then begin
			warning,'geopixe_update_event','Failed to create/open directory "'+s
		endif
		end
	'load-button': begin
		f = file_requester(path=(*pstate).dir, title='Select local GeoPIXE directory to update',/dir)
		if f[0] eq '' then return
		(*pstate).dir = F[0]
		widget_control, (*pstate).dir_text, set_value=(*pstate).dir
		end

	'ignore-text': begin
		widget_control, (*pstate).ignore_text, get_value=s
		if n_elements(*(*pstate).pignore) gt 0 then begin
			*(*pstate).pignore = [*(*pstate).pignore, s]
		endif else begin
			*(*pstate).pignore = (*pstate).pignore
		endelse
		widget_control, (*pstate).ignore_list, set_value=*(*pstate).pignore
		end
	'add-button': begin
		widget_control, (*pstate).ignore_text, get_value=s
		if n_elements(*(*pstate).pignore) gt 0 then begin
			*(*pstate).pignore = [*(*pstate).pignore, s]
		endif else begin
			*(*pstate).pignore = s
		endelse
		widget_control, (*pstate).ignore_list, set_value=*(*pstate).pignore
		end
	'del-button': begin
		i = widget_info( (*pstate).ignore_list, /list_select)
		if i[0] ge 0 then begin
			(*(*pstate).pignore)[i] = ''
			q = where( *(*pstate).pignore ne '', nq)
			if nq ge 1 then begin
				*(*pstate).pignore = (*(*pstate).pignore)[q]
				widget_control, (*pstate).ignore_list, set_value=*(*pstate).pignore
			endif else begin
				*(*pstate).pignore = ''
				widget_control, (*pstate).ignore_list, set_value=''
			endelse
		endif
		end
	'save-button': begin
		on_ioerror, finish_save
		openw, lun, geopixe_environment() + 'ftp-update.ignore', /get_lun
		for i=0L,n_elements(*(*pstate).pignore)-1 do begin
			printf, lun, (*(*pstate).pignore)[i]
		endfor
finish_save:
		close_file, lun
		on_ioerror, null
		end

	'ignore-list': begin
		if n_elements( *(*pstate).pignore) gt event.index then begin
			widget_control, (*pstate).ignore_text, set_value=(*(*pstate).pignore)[event.index]
		endif
		end
	'progress-list': begin
		end

	'force': begin
		case event.value of
			0: begin
				(*pstate).force = event.select
				end
			1: begin
				(*pstate).debug = event.select
				end
		endcase
		end

	'start-button': begin
		ftp_update_strings, pstate
		*(*pstate).pprogress = ''

		error = 1
		if (*pstate).dir ne '' then begin
			safe_file_mkdir, (*pstate).dir, error=error
			if error then begin
				ftp_update_progress, pstate, ['Failed to make local dir: "'+(*pstate).dir+'"', $
												'or, no write access to directory.','Abort transfer.']
				goto, done
			endif
			cd, (*pstate).dir
		endif
		count = 0L
		quiet = (*pstate).debug eq 0

		u = ftp_connect( (*pstate).host, dir=(*pstate).path, user=(*pstate).user, pass=(*pstate).pass, $
								proxy=(*pstate).proxy, pport=(*pstate).port, pstate=pstate, quiet=quiet, error=error)
		if error then begin
			ftp_update_progress, pstate, ['Error connecting to FTP server.','Abort transfer.']
			goto, done
		endif

		if (*pstate).force then begin
			ftp_update_progress, pstate, 'Force copy of all in effect ...'
		endif
		
		cancel = 0
		ftp_dir_copy, u, count=count, quiet=quiet, ignore=*(*pstate).pignore, proxy=(*pstate).use_proxy, $
								maia=(*pstate).maia, force=(*pstate).force, error=error, cancel=cancel
		if cancel then begin
			ftp_update_progress, pstate, ['Cancel transfer.']
			goto, done
		endif
		if error then begin
			ftp_update_progress, pstate, ['Error copying directory.','Terminate transfer.']
			goto, done
		endif
		error = 0
		ftp_update_progress, pstate, ['Updated file count = '+strtrim(string(count),2), $
					'Transfer complete.','Save settings to .geopixe home directory.']
		print,'   Updated ',strtrim(string(count),2),' files.'
		ftp_update_save, pstate	
done:
		cd, now
		ftp_close, u
		end

	'config-button': begin
		ftp_update_save, pstate	
		end
		
	'help-button': begin
		news, 'Help/GeoPIXE-update.txt', title='GeoPIXE Update Help', group=event.top
		end
		
	'exit-button': begin
		ftp_update_save, pstate	
		goto, kill
		end
	else:
  endcase
  return
  
bad_state:
	warning,'geopixe_update_event',['STATE variable has become ill-defined.','Abort Fit Setup.'],/error
	goto, kill
bad_ptr:
	warning,'geopixe_update_event',['Parameter structure variable has become ill-defined.','Abort Fit Setup.'],/error
	goto, kill

kill:
	widget_control, hourglass=0
;	cancel_notify, event.top
	
	; if ptr_valid((*pstate).pstuff) then ptr_free, (*pstate).pstuff
	
	widget_control, event.top, /destroy
	return
end

;--------------------------------------------------------------------------

pro ftp_update_strings, pstate

	COMPILE_OPT STRICTARR
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_update_strings',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif

	widget_control, (*pstate).host_text, get_value=s
	(*pstate).host = s
	widget_control, (*pstate).proxy_text, get_value=s
	(*pstate).proxy = s
	(*pstate).use_proxy = strlen(s) gt 0 
	widget_control, (*pstate).port_text, get_value=s
	(*pstate).port = s
	widget_control, (*pstate).user_text, get_value=s
	(*pstate).user = s
	widget_control, (*pstate).pass_text, get_value=s
	(*pstate).pass = s
	widget_control, (*pstate).path_text, get_value=s
	(*pstate).path = s
	widget_control, (*pstate).dir_text, get_value=s
	(*pstate).dir = s
	return
end

;--------------------------------------------------------------------------

pro ftp_update_save, pstate

	COMPILE_OPT STRICTARR
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_update_save',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish_save
	endif

	ftp_update_strings, pstate

	root_dir = geopixe_environment()					; user .geopixe dir
	geopixe_update_conf = root_dir + 'geopixe-update.conf'

	on_ioerror, finish_save
	openw, lun, geopixe_update_conf, /get_lun
	printf, lun, '#'
	printf, lun, '#	GeoPIXE FTP Update config file.'
	printf, lun, '#'
	printf, lun, '	host	'+(*pstate).host
	printf, lun, '	user	'+(*pstate).user
	printf, lun, '	pass	'+(*pstate).pass
	printf, lun, '	path	"'+(*pstate).path+'"'
	printf, lun, '	dir	"'+(*pstate).dir+'"'
	printf, lun, '#'
	printf, lun, '#	Proxy settings: Leave these blank if no proxy is needed.'
	printf, lun, '#'
	printf, lun, '	proxy	'+(*pstate).proxy
	printf, lun, '	port	'+(*pstate).port
	printf, lun, '#'
	printf, lun, '#	See also ignore specifications, in file "ftp-update.ignore".'
	printf, lun, '#'
	
finish_save:
	close_file, lun
	on_ioerror, null
	return
end

;--------------------------------------------------------------------------

pro ftp_update_progress, pstate, s, cancel=cancel

	COMPILE_OPT STRICTARR
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_update_progress',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
	if n_elements(s) eq 0 then return
	if size(s, /tname) ne 'STRING' then return
	if ptr_good(pstate,/struct) eq 0 then return
	
	cancel_event = widget_event( (*pstate).cancel, /nowait)
	cancel = 0
	if tag_names( cancel_event, /structure_name) eq 'WIDGET_BUTTON' then cancel = 1
	
	if n_elements(*(*pstate).pprogress) gt 0 then begin
		*(*pstate).pprogress = [*(*pstate).pprogress, s]
	endif else begin
		*(*pstate).pprogress = s
	endelse
	n = n_elements(*(*pstate).pprogress)
;	widget_control, (*pstate).progress_list, update=0
	widget_control, (*pstate).progress_list, set_value=*(*pstate).pprogress
	nview = widget_info( (*pstate).progress_list, /list_num_visible)
	widget_control, (*pstate).progress_list, set_list_top=(n-nview)>0
;	widget_control, (*pstate).progress_list, update=1
	return
end

;--------------------------------------------------------------------------

pro ftp_update_load, pstate

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
	if n_elements(geopixe_root) lt 1 then geopixe_root=''
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
			warning,'ftp_update_load',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	i = 0
	root_dir = geopixe_environment()
	geopixe_paths = root_dir + 'ftp-update.ignore'						; user .geopixe dir
	if file_test(geopixe_paths, /read) eq 0 then begin
		geopixe_paths = 'ftp-update.ignore'								; same dir as geopixe_update.sav
		if file_test(geopixe_paths, /read) eq 0 then begin
			root_dir = geopixe_root
			geopixe_paths = root_dir + 'ftp-update.ignore'				; geopixe dir
			if file_test(geopixe_paths, /read) eq 0 then goto, cont
		endif
	endif
	on_ioerror, cont
	openr, lun, geopixe_paths, /get_lun
	s = ''
	str = strarr(1000)
	on_ioerror, cont
	while not EOF(lun) do begin
		readf, lun, s
		str[i] = s
		i = i+1
		if i eq 1000 then goto, cont
	endwhile

cont:
	close_file, lun
	if i ge 1 then begin
		*(*pstate).pignore = str[0:i-1]
	endif
	return
end

;-----------------------------------------------------------------

pro OnRealize_update_ignore_list, wWidget

	COMPILE_OPT STRICTARR
	top = tlb_id(wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate	
	if ptr_good(pstate,/struct) eq 0 then return
	
	ftp_update_load, pstate
	widget_control, wWidget, set_value=*(*pstate).pignore
	return
end

;-----------------------------------------------------------------

pro OnRealize_update_progress_list, wWidget

	COMPILE_OPT STRICTARR
	top = tlb_id(wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate	
	if ptr_good(pstate,/struct) eq 0 then return
	
	geom = widget_info( wWidget, /geometry)
	tlb_geom = widget_info( top, /geometry)
	(*pstate).yoffset = tlb_geom.ysize - geom.scr_ysize
	(*pstate).xoffset = tlb_geom.xsize - geom.scr_xsize
	return
end

;-------------------------------------------------------------------------------------------------------

pro maia_update_nogui, debug=debug

; A no GUI version of Maia-Update, that assumes all parameters are
; in the conf file already (e.g. from a previous GUI maia_update).

	COMPILE_OPT STRICTARR
	common c_errors_1, catch_errors_on
	common c_debug_warnings, enable_warning_popup
	common c_working_dir, geopixe_root
	if n_elements(geopixe_root) lt 1 then geopixe_root=''
	if n_elements(catch_errors_on) eq 0 then catch_errors_on = 1
	if n_elements(enable_warning_popup) lt 1 then enable_warning_popup=0
	if n_elements(debug) eq 0 then debug = 0
	enable_warning_popup = 0
	catch_errors_on = 1
	if debug then catch_errors_on = 0
	if catch_errors_on then begin
		Catch, ErrorNo
		if (ErrorNo ne 0) then begin
			Catch, /cancel
			on_error, 1
			help, calls = s
			n = n_elements(s)
			c = 'Call stack: '
			if n gt 2 then c = [c, s[1:n-2]]
			warning,'geopixe_update',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	version = '8.6o'
	now = fix_path(file_expand_path('.'))
;	startupp
	
;	Command line arguments, if used ...

	apath = ''
	adir = ''
	aforce = ''
	argv = command_line_args( count=argc)			; command line args
	if argc gt 0 then begin
		for i=0,argc-1 do begin
			s = strsplit(argv[i],'=',/extract)
			if n_elements(s) ge 2 then begin
				case strlowcase(s[0]) of
					'path': begin
						apath = s[1]
						end
					'dir': begin
						adir = s[1]
						end
					'force': begin
						aforce = s[1]
						end
					else:
				endcase
			endif
		endfor
;		warning,'geopixe_update',['Found these args:',argv,'', $
;			'current dir = "'+now+'"','', $
;			'path = '+apath,'dir = '+adir], /info
	endif
	
;	FTP arameters, from conf file ...

	def = ftp_defaults( error=err)					; conf file from .geopixe home dir
	force = 0
	if err then begin
		host = 'pftp.csiro.au'
		user = ''
		pass = ''
		path = '/update/8.7'
		dir = ''
		proxy = ''
		port = ''
	endif else begin
		host = def.host
		user = def.user
		pass = def.pass
		path = def.path
		dir = def.dir
		proxy = def.proxy
		port = def.port
	endelse
	if apath ne '' then path=apath					; command line args override .geopixe file
	if adir ne '' then dir=adir
	if aforce ne '' then force=long(aforce)
	if dir eq '' then dir=now
	use_proxy = strlen(proxy) gt 0 
	
;	Ignore file specs, from 'ftp-update.ignore' file ...

	ignore = []
	i = 0
	root_dir = geopixe_environment()
	geopixe_paths = root_dir + 'ftp-update.ignore'						; user .geopixe dir
	if file_test(geopixe_paths, /read) eq 0 then begin
		geopixe_paths = 'ftp-update.ignore'								; same dir as geopixe_update.sav
		if file_test(geopixe_paths, /read) eq 0 then begin
			root_dir = geopixe_root
			geopixe_paths = root_dir + 'ftp-update.ignore'				; geopixe dir
			if file_test(geopixe_paths, /read) eq 0 then goto, cont
		endif
	endif
	on_ioerror, cont
	openr, lun, geopixe_paths, /get_lun
	s = ''
	str = strarr(1000)
	on_ioerror, cont
	while not EOF(lun) do begin
		readf, lun, s
		str[i] = s
		i = i+1
		if i eq 1000 then goto, cont
	endwhile

cont:
	close_file, lun
	if i ge 1 then begin
		ignore = str[0:i-1]
	endif

;	Now do the transfers ...

	error = 1
	if dir ne '' then begin
		safe_file_mkdir, dir, error=error
		if error then begin
			print, 'geopixe_update_nogui: Failed to make local dir: "'+dir+'" or, no write access to directory. Abort transfer.'
			goto, done
		endif
		cd, dir
	endif
	count = 0L
	quiet = 1

	u = ftp_connect( host, dir=path, user=user, pass=pass, $
							proxy=proxy, pport=port, quiet=quiet, error=error)
	if error then begin
		print, 'geopixe_update_nogui: Error connecting to FTP server; Abort transfer.'
		goto, done
	endif

	if force then begin
		print, 'geopixe_update_nogui: Force copy of all in effect ...'
	endif
	
	cancel = 0
	ftp_dir_copy, u, count=count, quiet=quiet, ignore=ignore, proxy=use_proxy, $
							maia=maia, force=force, error=error
	if error then begin
		print, 'geopixe_update_nogui: Error copying directory. Terminate transfer.'
		goto, done
	endif
	error = 0
	print, 'Updated file count = '+strtrim(string(count),2)
	print, 'Transfer complete.'

done:
	cd, now
	ftp_close, u
	return
end

;-------------------------------------------------------------------------------------------------------

pro geopixe_update, GROUP_LEADER=wGroup, TLB=tlb, xoffset=xoffset, yoffset=yoffset, maia=maia

;	Update GeoPIXE and optionally Maia files and directories to a target system.
;
;	Uses config file 'geopixe-update.conf' in home .geopixe dir for settings.
;	These are saved on completion of a transfer and on exit.
;	
;	Uses a set of 'ignore' file specifications to ignore during transfer.
;	These files are protected on target system. However, a new file of this name will be
;	transferred to a file with a '.new' suffix. Check these for new fearures.
;	
;	/maia	update Maia files in any 'maia' directory (will always update '*Maia.conf').
;			have renamed 'Maia' dir in 'setup' so it is unaffected by this test.
;
;	If spawned as a sub-process, then get args path='path' and dir='dir', which override
;	internal values (and from .geopixe dir) if present.

	COMPILE_OPT STRICTARR
	ErrorNo = 0
	common c_errors_1, catch_errors_on
	common c_debug_warnings, enable_warning_popup
	if n_elements(catch_errors_on) eq 0 then catch_errors_on = 1
	if n_elements(enable_warning_popup) lt 1 then enable_warning_popup=1
	if catch_errors_on then begin
		Catch, ErrorNo
		if (ErrorNo ne 0) then begin
			Catch, /cancel
			on_error, 1
			help, calls = s
			n = n_elements(s)
			c = 'Call stack: '
			if n gt 2 then c = [c, s[1:n-2]]
			warning,'geopixe_update',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif
	if n_elements(maia) eq 0 then maia=1
	
	version = '8.7o'
	
	text_xsize = 430
	text_xsize2 = 385
	text_xsize4 = 330
	label_xsize = 43
	progress_ysize = 150
	ignore_ysize = 60
	case !version.os_family of
		'Windows': begin
			text_xsize3 = 38
			list_xsize = 488
			help_xsize = 502
			end
		'unix': begin
			text_xsize3 = 42
			list_xsize = 482
			help_xsize = 493
			end
		else: begin
			warning,'geopixe_update',['Unknown O/S referenced: '+!version.os_family]
			end
	endcase
	tracking = 1
	
	if n_elements(wGroup) lt 1 then wGroup=0L
	if n_elements(xoffset) lt 1 then begin
		screen = get_screen_size()
		xoffset = (0.3*screen[0] - help_xsize/2) > 0
	endif
	if n_elements(yoffset) lt 1 then begin
		screen = get_screen_size()
		yoffset = (screen[1]/2 - 800/2) > 0
	endif
	
	cd, current=now
	apath = ''
	adir = ''
	argv = command_line_args( count=argc)			; command line args
	if argc gt 0 then begin
		for i=0,argc-1 do begin
			s = strsplit(argv[i],'=',/extract)
			if n_elements(s) ge 2 then begin
				case strlowcase(s[0]) of
					'path': begin
						apath = s[1]
						end
					'dir': begin
						adir = s[1]
						end
					else:
				endcase
			endif
		endfor
;		warning,'geopixe_update',['Found these args:',argv,'', $
;			'current dir = "'+now+'"','', $
;			'path = '+apath,'dir = '+adir], /info
	endif
	
	def = ftp_defaults( error=err)					; conf file from .geopixe home dir
	if err then begin
		host = 'pftp.csiro.au'
		user = ''
		pass = ''
		path = '/update/8.7'
		dir = ''
		proxy = ''
		port = ''
	endif else begin
		host = def.host
		user = def.user
		pass = def.pass
		path = def.path
		dir = def.dir
		proxy = def.proxy
		port = def.port
	endelse
	if apath ne '' then path=apath					; command line args override .geopixe file
	if adir ne '' then dir=adir
	if dir eq '' then dir=now
	
	ignore = []
	
	title = maia ? 'GeoPIXE and Maia Update '+version : 'GeoPIXE Update '+version 
	
	tlb = Widget_Base( GROUP_LEADER=wGroup, UNAME='geopixe-update-tlb',  $
	       /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset,  $
	      /TLB_SIZE_EVENTS ,TITLE=title ,SPACE=3 ,XPAD=3,  $
	      YPAD=3 ,COLUMN=1 ,/BASE_ALIGN_CENTER)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
	
	fbase = widget_base( tbase, /column, xpad=1, ypad=1, space=2, /base_align_center, /align_center, /frame)
	label = widget_label( fbase, value='CSIRO GeoPIXE FTP Server')
	
	rbase = widget_base( fbase, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
	label = widget_label( rbase, value='Host:', scr_xsize=label_xsize)
	host_text = widget_text( rbase, value=host, uname='host-text', tracking=tracking, /editable, scr_xsize=text_xsize, $
						uvalue='Enter the CSIRO GeoPIXE server IP address.')
	ubase = widget_base( fbase, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
	label = widget_label( ubase, value='User:', scr_xsize=label_xsize)
	user_text = widget_text( ubase, value=user, uname='user-text', tracking=tracking, /editable, scr_xsize=text_xsize, $
						uvalue='Enter your Username on the CSIRO GeoPIXE server.')
	pbase = widget_base( fbase, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
	label = widget_label( pbase, value='Pass:', scr_xsize=label_xsize)
	pass_text = widget_text( pbase, value=pass, uname='pass-text', tracking=tracking, /editable, scr_xsize=text_xsize, $
						uvalue='Enter your Password on the CSIRO GeoPIXE server.')
	vbase = widget_base( fbase, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
	label = widget_label( vbase, value='Path:', scr_xsize=label_xsize)
	path_text = widget_text( vbase, value=path, uname='path-text', tracking=tracking, /editable, scr_xsize=text_xsize2, $
						uvalue='Enter the remote path to the GeoPIXE code directory to update from. Click on "?" to see available update directories.')
	button = widget_button( vbase, value='?', uname='query', tracking=tracking, scr_xsize=text_xsize3, $	
						uvalue='Retrieve available versions from FTP site "update" dir.')
	
	f2base = widget_base( tbase, /column, xpad=1, ypad=1, space=2, /base_align_center, /align_center, /frame)
	label = widget_label( f2base, value='Your Proxy Server (if needed)')
	pxbase = widget_base( f2base, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
	label = widget_label( pxbase, value='Proxy:', scr_xsize=label_xsize)
	proxy_text = widget_text( pxbase, value=proxy, uname='proxy-text', tracking=tracking, /editable, scr_xsize=text_xsize4, $
						uvalue='If you must use a proxy server, enter the proxy server IP address and port number. Else, leave these blank.')
	label = widget_label( pxbase, value='Port:', scr_xsize=label_xsize)
	port_text = widget_text( pxbase, value=port, uname='port-text', tracking=tracking, /editable, scr_xsize=label_xsize, $
						uvalue='If you must use a proxy server, enter the proxy server IP address and port number. Else, leave these blank.')
	
	lbase = widget_base( tbase, /column, xpad=1, ypad=1, space=2, /base_align_center, /align_center, /frame)
	label = widget_label( lbase, value='Your Local GeoPIXE run-time directory')
	dbase = widget_base( lbase, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
	label = widget_label( dbase, value='Dir:', scr_xsize=label_xsize)
	dir_text = widget_text( dbase, value=dir, uname='dir-text', tracking=tracking, /editable, scr_xsize=text_xsize2, $
						uvalue='Enter the local path to your GeoPIXE run-time directory to update.')
	button = widget_button( dbase, value='Load', uname='load-button', tracking=tracking, scr_xsize=text_xsize3, $	
						uvalue='Open file requester to select local GeoPIXE update "Dir".')
	
	ibase = widget_base( tbase, /column, xpad=1, ypad=1, space=2, /base_align_center, /align_center, /frame)
	label = widget_label( ibase, value='Local File Specifications to Ignore')
	
	ignore_list = Widget_List( ibase, UNAME='ignore-list', value=ignore, $
			NOTIFY_REALIZE='OnRealize_update_ignore_list', $
			scr_xsize=list_xsize ,scr_ysize=ignore_ysize, tracking=tracking, $
			uvalue='Local files to ignore (if present). Use single "?" and multiple-character "*" wildcards.')
	
	iibase = widget_base( ibase, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
	label = widget_label( iibase, value='Ignore:', scr_xsize=label_xsize)
	ignore_text = widget_text( iibase, value='', uname='ignore-text', tracking=tracking, /editable, $
						uvalue='Enter a file specification to ignore. Use single ("?") and multiple ("*") character wildcards. ' + $
						'Hit <return> or "Add" to add to ignore list.', scr_xsize=text_xsize)
	
	bibase = widget_base( ibase, /row, /base_align_center, xpad = 0, ypad=0, space=20)
	button = widget_button( bibase, value='Add', uname='add-button', tracking=tracking, $	
						uvalue='Add the current "Ignore" specification to the ignore list.')
	button = widget_button( bibase, value='Save', uname='save-button', tracking=tracking, $	
						uvalue='Save ignore list to disk to be available for future use. These are stored as "ftp-update.ignore" in your home .geopixe directory.')
	button = widget_button( bibase, value='Del', uname='del-button', tracking=tracking, $	
						uvalue='Delete the selected "Ignore" specification from the ignore list. Click on an item to select it first.')

	gbase = widget_base( tbase, /column, xpad=1, ypad=1, space=2, /base_align_center, /align_center, /frame)
	label = widget_label( gbase, value='FTP Transfer Progress Messages')
	
	progress_list = Widget_List( gbase, UNAME='progress-list', value='', $
			NOTIFY_REALIZE='OnRealize_update_progress_list', $
			scr_xsize=list_xsize ,scr_ysize=progress_ysize, tracking=tracking, $
			uvalue='FTP transfer progress messages.')
	
	bbase = widget_base( tbase, /row, /base_align_center, xpad = 0, ypad=0, space=10)
	button = widget_button( bbase, value='Start Update', uname='start-button', tracking=tracking, $	
						uvalue='Start the update transfer from the FTP site.')
	force_id = cw_bgroup2( bbase, ['Force','Debug'], /row, set_value=[0,0], /return_index, /tracking, $
  						uname='force', /nonexclusive, xpad=0, ypad=0, uvalue=['Force copy of all files (ignore modify times). "Ignore" files still saved with ".new" suffix.', $
						'Enable Debug diagnostics'])
	cancel = widget_button( bbase, value='Cancel', uname='cancel-button', tracking=tracking, $	
						uvalue='Cancel transfer (after a delay).')
	label = widget_label( bbase, value='     ')
	button = widget_button( bbase, value='Help', uname='help-button', tracking=tracking, $	
						uvalue='Display the help file "GeoPIXE-update.txt".')
	button = widget_button( bbase, value='Exit', uname='exit-button', tracking=tracking, $	
						uvalue='Do nothing further, save settings and exit.')
;	button = widget_button( bbase, value='Save', uname='config-button', tracking=tracking, $	
;						uvalue='Comment this out later!')

	help = widget_text( tbase, scr_xsize=help_xsize, ysize=3, /wrap, uname='help', tracking=tracking, $
					uvalue='Context sensitive help. Pass cursor over widgets above to get tips on them.', frame=0)
	
	state = {	$
				dir_text:			dir_text, $				; local dir text ID
				host_text:			host_text, $			; host IP text ID
				proxy_text:			proxy_text, $			; proxy IP text ID
				port_text:			port_text, $			; proxy port text ID
				user_text:			user_text, $			; user text ID
				pass_text:			pass_text, $			; password text ID
				path_text:			path_text, $			; remote path text ID
				ignore_list:		ignore_list, $			; Ignore List ID
				ignore_text:		ignore_text, $			; ignore text ID
				progress_list:		progress_list, $		; Progress List ID
				cancel:				cancel, $				; cancel button ID
				help:				help, $					; help text ID
	
				pignore:			ptr_new(ignore), $		; ptr to list of ignore specs
				pprogress:			ptr_new(/alloc), $		; ptr to progress messages list
				host:				host, $					; host IP
				proxy:				proxy, $				; proxy IP (if used, else blank)
				use_proxy:			(strlen(proxy) gt 0), $	; flags use of proxy
				port:				port, $					; proxy port number
				user:				user, $					; user name
				pass:				pass, $					; password
				path:				path, $					; remote update path
				dir:				dir, $					; local geopixe dir
				tracking:			tracking, $				; tracking enable
				maia:				maia, $					; enable Maia s/w too.
				force:				0, $					; force copy all
				debug:				0, $					; enable debug output
				xoffset:			0, $					; offset in xsize for resize
				yoffset:			0 }						; offset in ysize for resize
	
	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize
	
	XManager, 'geopixe_update', tlb ,/no_block
	return
end
