;
; Alternative to 'dialog_pickfile' with more uniform behaviour across platforms
; Most options like in "dialog_pickfile", plus a few more (see below).
; 
; If 'file' is passed with a path included, and it is found, then that path will be used.
; Else, the 'path' will be used with the 'file' stripped of any path. If 'path' is blank,
; then the current working directory will be assumed.
;
; The order of file and path testing is as follows:
; 	file			test input file assumed to also have a remote path
; 	path			test input file-only on local alternative supplied path 'path'
; 	local updir		look for file-only updir from local 'path'
; 	remote updir	look for file-only updir from remote path (stripped from input filename)
; 	translate		look for a match translating path roots using table, then for each match:
; 		file		look for file-only match in translated dir
; 		updir		also look for file-only match updir (and searching down from there)
;
; The variable controlling the number of dirs allowed (before using "...") is
; in the 'find_file2()' function.
;
; file				initial filename (needs a path included to use /translate)
; path				initial path ( will be tried if path included in file does not work)
; title				window title
; multiple_files	multiple select active
; dialog_parent		parent of modal pop-up
; filter			file filter string array (e.g. '*' or ['*.spec','*.trav'] )
; /fix_filter		fix the filter
; /numeric			show only numeric file extensions
; /latest			show latest files first
; /directory		select and return a path name only
; 					select in tree, unless /multiple, and then in list
; get_path			return the final path
;
; /read				For file read, will cause a dir search for a match, updir and translate
; /write			For file write, no searching.
;					If both /read and /write are missing, then no search, updir or translate.
;					Set /write if require write of file, to be able to create new dirs.
;					Set /read to force search for file name provided (and updir, translate).
;
; new options:
; 'preview_routine'	string - name of preview routine
; /image			show image/spectrum preview first
; /ignore			pass 'ignore nulls' option onto read_geopixe_image (in preview)
; 
; /skip_if_exists	skip opening the requester if the file specified already exists
; 					on the path specified, or after translation or updir.
; /skip_if_null		skip opening the requester if the file specified is null/blank
; 					Need to pass a valid 'file' for this to be an effective test.
; translate=0		If file is passed with a full path included and /translate set (default), then
; 					if it is not found, or not in the selected path, then use translation tables 
; 					to try alternate path roots if the file path root matches one in the table.
; 					Use translate=0 to suppress auto-translation.
; updir=n			Also check up to 'n' dir level up for a match to the input file name.
;
; Preview routines:
; Input:	file		name of file
; Output:	result		structure of form {image:image, details:details}
; 
; where		image		preview image array
;			details		string array of various parameters and text
;						either part of result struct can be missing to indicate no return.
;
;	author:		C.G. Ryan, CSIRO							2010
;				added /skip_if_exists or null				2012	C.G. Ryan
;				added translation tables					2014	C.G. Ryan
;				refine search order							2016	C.G. Ryan
;				fixed offset jumping in Linux				2019	C.G. Ryan
;				used file_search2 to allow cancel			2019	C.G. Ryan
;
;----------------------------------------------------------------------------------------------------------

pro file_requester_event, event

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
		warning,'file_requester_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
common c_file_requester, find_path, exclude, pattern
widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
		if (*pstate).tracking eq 0 then goto, finish
		widget_control, event.id, get_uvalue=s
		if size(s,/tname) eq 'STRING' then begin
			if event.enter eq 1 then begin
				widget_control, (*pstate).help, set_value=s
			endif else begin
				widget_control, (*pstate).help, set_value='Select a path from the directory tree on the left, or by using the "path" field, and select files to open in the file list on the right, or using the "files" field. ' + $
						'Open a new directory tree by specifying a new "Root" or by selecting a new "bookmark" from the list on the right.
			endelse
		endif
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
;		print,'Kill request batch_sort ...'
		goto, kill
		end
	'WIDGET_BASE': begin
		uname = widget_info( event.id, /uname)
		case uname of
			'file-requester-tlb': begin
				case !version.os_family of
					'MacOS': begin
						xoff = 33
						yoff1a = 156
						yoff1b = 118
						yoff2 = 286
						end
					'unix': begin
						xoff = 33
						yoff1a = 156
						yoff1b = 118
						yoff2 = 286
						end
					else: begin
						xoff = 33
						yoff1a = 156
						yoff1b = 118
						yoff2 = (*pstate).dir ? 175 : 286
						end
				endcase
				x1 = 0.5*((event.x > 440) - xoff)
				if (*pstate).preview and (*pstate).view_preview then begin
					y1 = (event.y > 480) - yoff1a
					(*pstate).xsize = x1-6
					(*pstate).ysize = 0.47*y1
					widget_control, (*pstate).draw, scr_xsize=x1-6, scr_ysize=0.47*y1
					widget_control, (*pstate).details_list, scr_xsize=x1-6, scr_ysize=0.47*y1
					widget_control, (*pstate).tree, scr_xsize=x1, scr_ysize=0.53*y1
;					print,'tree: size =',x1,0.53*y1
;					print,'draw: size =',x1-6,0.47*y1
				endif else begin
					y1 = (event.y > 480) - yoff1b
					widget_control, (*pstate).tree, scr_xsize=x1, scr_ysize=y1
;					print,'tree: size =',x1,y1
				endelse
				widget_control, (*pstate).root_text, scr_xsize=x1-50
				x2 = 0.5*((event.x > 440) - xoff)
				y2 = (event.y > 480) - yoff2
				widget_control, (*pstate).path_list, scr_xsize=x2, scr_ysize=(*pstate).dir ? y2 : y2/3
				widget_control, (*pstate).path_text, scr_xsize=x2-50
				widget_control, (*pstate).trans_list, scr_xsize=x2, scr_ysize=(*pstate).dir ? y2 : y2/3
				widget_control, (*pstate).from_text, scr_xsize=x2-50
				widget_control, (*pstate).to_text, scr_xsize=x2-50
				if (*pstate).show_list then begin
					widget_control, (*pstate).file_list, scr_xsize=x2, scr_ysize=2*y2/3
					widget_control, (*pstate).file_text, scr_xsize=x2-50
					widget_control, (*pstate).filter_list, scr_xsize=x2-50
;					print,'files: size =',x2,2*y2/3
				endif
;				print,'paths: size =',x2,y2/3
;				print,'text: length =',x2-50
				widget_control, (*pstate).help, scr_xsize=x1+x2+22
				if (*pstate).preview_base ne 0L then begin
					geo = widget_info( (*pstate).preview_base, /geometry)
					(*pstate).preview_ysize = geo.ysize
				endif
				geo = widget_info( (*pstate).tree, /geometry)
				(*pstate).tree_ysize = geo.ysize
				goto, finish
				end
			else:
		endcase
		end
	else:
endcase

;widget_control, event.top, update=0
;help,event
uname = widget_info( event.id, /uname)
case uname of

	'tree': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_TREE_SEL': begin
				widget_control, event.id, get_uvalue=uvalue
				if locate('...', uvalue) eq -1 then (*pstate).path = uvalue else (*pstate).path=''
				print,'Select Tree node: uvalue = ', uvalue
				widget_control, hourglass=1
				widget_control_update, event.top, update=0
				grow_tree, event.id, uvalue, uname='tree', /expanded
				set_widget_text, (*pstate).path_text, (*pstate).path

				if (*pstate).show_list then begin
					file_requester_update_list, pstate
					widget_control, event.id, set_tree_select=1
				endif
				widget_control_update, event.top, update=1
				end
			else:
		endcase
		end

	'root-text': begin
		widget_control, event.id, get_value=s
		(*pstate).root = s
		n = strlen(s)
		t = strmid( s, n-1,1)
		ps = path_sep()
		if t ne ps then s = s+ps
		widget_control_update, event.top, update=0
		set_widget_text, event.id, s

		widget_control, hourglass=1
		widget_control, (*pstate).tree_root, /destroy
		(*pstate).tree_root = widget_tree((*pstate).tree, value=(*pstate).root, uvalue=(*pstate).root, uname='tree', /folder, /expanded)
		grow_tree, (*pstate).tree_root, (*pstate).root, uname='tree', /expanded
		widget_control_update, event.top, update=1
		end

	'preview': begin
		print,'hit preview draw ...'
		end

	'preview-toggle': begin
		(*pstate).view_preview = event.select
		if (*pstate).view_preview then begin
			widget_control, (*pstate).tree, ysize=(*pstate).tree_ysize
			widget_control, (*pstate).preview_base, map=1, scr_ysize=(*pstate).preview_ysize
		endif else begin
			widget_control, (*pstate).preview_base, map=0, scr_ysize=1
			widget_control, (*pstate).tree, ysize=(*pstate).tree_ysize + (*pstate).preview_ysize
		endelse
		end
		
	'path-list': begin
		(*pstate).current_path = event.index
		if event.clicks eq 2 then begin
			s1 = (*(*pstate).ppaths)[event.index]
			if strupcase(s1) eq '$HOME' then begin
				s2 = geopixe_environment()
			endif else begin
				n = strlen(s1)
				if n gt 0 then begin
					t = strmid( s1, n-1,1)
					ps = path_sep()
					if t ne ps then s1 = s1+ps
				endif else goto, finish
				s2 = s1
			endelse
;			safe_file_mkdir, s2, /verbose, error=error
;			if error then begin
;				warning,'file_requester',['Illegal directory name, write protected parent dir,','or error creating directory:', s2, '', $
;						'Check ownership and file protection bits for parent dir:', dir_up(s2)]
;				goto, finish
;			endif
			widget_control_update, event.top, update=0
			(*pstate).path = s2
			set_widget_text, (*pstate).path_text, (*pstate).path

			id = find_leaf( (*pstate).tree_root, (*pstate).path)
			widget_control, hourglass=1
			if id gt 0 then begin
				grow_tree, id, (*pstate).path, uname='tree', /expanded
				widget_control, id, /set_tree_select, /set_tree_expanded, /set_tree_visible
			endif else begin
				(*pstate).root = (*pstate).path
				widget_control, (*pstate).tree_root, /destroy
				(*pstate).tree_root = widget_tree((*pstate).tree, value=(*pstate).root, uvalue=(*pstate).root, uname='tree', /folder, /expanded)
				grow_tree, (*pstate).tree_root, (*pstate).root, uname='tree', /expanded
			endelse
			widget_control, (*pstate).root_text, set_value=(*pstate).root

			if (*pstate).show_list then file_requester_update_list, pstate
			widget_control_update, event.top, update=1
		endif
		end

	'path-text': begin
		widget_control, event.id, get_value=s1
		if strupcase(s1) eq '$HOME' then begin
			s2 = geopixe_environment()
		endif else begin
			n = strlen(s1)
			if n gt 0 then begin
				s1 = fix_path(s1)
			endif else goto, finish
			s2 = s1
		endelse
		if (*pstate).write then begin
			safe_file_mkdir, s2, /verbose, error=error
			if error then begin
				warning,'file_requester',['Illegal directory name, write protected parent dir,','or error creating directory:', s2, '', $
						'Check ownership and file protection bits for parent dir:', dir_up(s2)]
				goto, finish
			endif
		endif
		widget_control_update, event.top, update=0
		(*pstate).path = s2
		set_widget_text, event.id, s2
		if (*pstate).dir then goto, done
		use_parent = 0

;		If create a new dir in Path, then id will not be found, instead
;		find the parent node and call grow_tree to add the new leaf.

		id = find_leaf( (*pstate).tree_root, (*pstate).path)
		if id eq 0 then begin
			id = find_leaf( (*pstate).tree_root, file_dirname((*pstate).path,/mark))
			if id ne 0 then begin
				use_parent = 1
				old_path = (*pstate).path 
				(*pstate).path = file_dirname((*pstate).path,/mark)
			endif
		endif
		
;		If id not found anywhere, then open a new root ...

		widget_control, hourglass=1
		if id gt 0 then begin
			grow_tree, id, (*pstate).path, uname='tree', /expanded
			widget_control, id, /set_tree_select, /set_tree_expanded, /set_tree_visible
		endif else begin
			(*pstate).root = (*pstate).path
			widget_control, (*pstate).tree_root, /destroy
			(*pstate).tree_root = widget_tree((*pstate).tree, value=(*pstate).root, uvalue=(*pstate).root, uname='tree', /folder, /expanded)
			grow_tree, (*pstate).tree_root, (*pstate).root, uname='tree', /expanded
		endelse
		widget_control, (*pstate).root_text, set_value=(*pstate).root

;		This code occurs to open a new leaf added above for a new dir.

		if use_parent then begin
			id = find_leaf( (*pstate).tree_root, old_path)
			if id gt 0 then begin
				(*pstate).path = old_path 
				grow_tree, id, (*pstate).path, uname='tree', /expanded
				widget_control, id, /set_tree_select, /set_tree_expanded, /set_tree_visible
			endif
		endif

		if (*pstate).show_list then file_requester_update_list, pstate
		widget_control_update, event.top, update=1
		end

	'find-text': begin
		widget_control, event.id, get_value=pattern
		if pattern eq '' then goto, finish
		find_path = (*pstate).path
		exclude = ''
		File_requester_find, pstate, find_path, pattern, exclude, event.top
		widget_control_update, event.top, update=1
		end

	'find-more': begin
		if pattern eq '' then goto, finish
		File_requester_find, pstate, find_path, pattern, exclude, event.top
		widget_control_update, event.top, update=1
		end

	'load-button': begin
		file_requester_load, pstate
		widget_control, (*pstate).path_list, set_value=*(*pstate).ppaths
		end

	'save-button': begin
		on_ioerror, finish_save
		openw, lun, geopixe_environment() + 'file-requester.paths', /get_lun
		for i=0L,n_elements(*(*pstate).ppaths)-1 do begin
			printf, lun, (*(*pstate).ppaths)[i]
		endfor
finish_save:
		close_file, lun
		on_ioerror, null
		end

	'add-button': begin
		widget_control, (*pstate).path_text, get_value=s
		if n_elements(*(*pstate).ppaths) gt 0 then begin
			*(*pstate).ppaths = [*(*pstate).ppaths, s]
		endif else begin
			*(*pstate).ppaths = s
		endelse
		widget_control, (*pstate).path_list, set_value=*(*pstate).ppaths
		end

	'del-button': begin
		i = widget_info( (*pstate).path_list, /list_select)
		if i[0] ge 0 then begin
			(*(*pstate).ppaths)[i] = ''
			q = where( *(*pstate).ppaths ne '', nq)
			if nq ge 1 then begin
				*(*pstate).ppaths = (*(*pstate).ppaths)[q]
				widget_control, (*pstate).path_list, set_value=*(*pstate).ppaths
			endif else begin
				*(*pstate).ppaths = ''
				widget_control, (*pstate).path_list, set_value=''
			endelse
		endif
		end

	'go-button': begin
		s1 = (*(*pstate).ppaths)[(*pstate).current_path]
		if strupcase(s1) eq '$HOME' then begin
			s2 = geopixe_environment()
		endif else begin
			n = strlen(s1)
			if n gt 0 then begin
				t = strmid( s1, n-1,1)
				ps = path_sep()
				if t ne ps then s1 = s1+ps
			endif else goto, finish
			s2 = s1
		endelse
		widget_control_update, event.top, update=0
		(*pstate).path = s2
		set_widget_text, (*pstate).path_text, (*pstate).path

		id = find_leaf( (*pstate).tree_root, (*pstate).path)
		widget_control, hourglass=1
		if id gt 0 then begin
			grow_tree, id, (*pstate).path, uname='tree', /expanded
			widget_control, id, /set_tree_select, /set_tree_expanded, /set_tree_visible
		endif else begin
			(*pstate).root = (*pstate).path
			widget_control, (*pstate).tree_root, /destroy
			(*pstate).tree_root = widget_tree((*pstate).tree, value=(*pstate).root, uvalue=(*pstate).root, uname='tree', /folder, /expanded)
			grow_tree, (*pstate).tree_root, (*pstate).root, uname='tree', /expanded
		endelse
		widget_control, (*pstate).root_text, set_value=(*pstate).root

		if (*pstate).show_list then file_requester_update_list, pstate
		widget_control_update, event.top, update=1
		end

	'up-button': begin
		s = strmid( (*pstate).path, 0, strlen((*pstate).path)-1)
		i = locate_last( slash(), s)
		if i ge 1 then (*pstate).path = strmid( (*pstate).path, 0, i+1)
		widget_control_update, event.top, update=0
		set_widget_text, (*pstate).path_text, (*pstate).path

		id = find_leaf( (*pstate).tree_root, (*pstate).path)
		widget_control, hourglass=1
		if id gt 0 then begin
			grow_tree, id, (*pstate).path, uname='tree', /expanded
			widget_control, id, /set_tree_select, /set_tree_expanded, /set_tree_visible
		endif else begin
			(*pstate).root = (*pstate).path
			widget_control, (*pstate).tree_root, /destroy
			(*pstate).tree_root = widget_tree((*pstate).tree, value=(*pstate).root, uvalue=(*pstate).root, uname='tree', /folder, /expanded)
			grow_tree, (*pstate).tree_root, (*pstate).root, uname='tree', /expanded
		endelse
		widget_control, (*pstate).root_text, set_value=(*pstate).root

		if (*pstate).show_list then file_requester_update_list, pstate
		widget_control_update, event.top, update=1
		end

	'translate-list': begin
		i = widget_info( (*pstate).trans_list, /list_select)
		if i[0] ge 0 then begin
			j = i/2
			print,' Set: from=',(*(*pstate).ptrans)[j].from, ' to=', (*(*pstate).ptrans)[j].to
			set_widget_text, (*pstate).from_text, (*(*pstate).ptrans)[j].from
			set_widget_text, (*pstate).to_text, (*(*pstate).ptrans)[j].to 
		endif
		end

	'load-trans-button': begin
		file_requester_load_trans, pstate
		file_requester_draw_trans, pstate
		end

	'save-trans-button': begin
		on_ioerror, finish_save2
		openw, lun, geopixe_environment() + 'file-requester.translate', /get_lun
		for i=0L,n_elements( (*(*pstate).ptrans)[*])-1 do begin
			printf, lun, (*(*pstate).ptrans)[i].from
			printf, lun, (*(*pstate).ptrans)[i].to
		endfor
finish_save2:
		close_file, lun
		on_ioerror, null
		end

	'from-text': begin
		widget_control, event.id, get_value=s1
		(*pstate).from = s1
		set_widget_text, event.id, s1
		end

	'to-text': begin
		widget_control, event.id, get_value=s1
		n = strlen(s1)
		if n gt 0 then begin
			t = strmid( s1, n-1,1)
			ps = path_sep()
			if t ne ps then s1 = s1+ps
		endif else goto, finish
		s2 = s1
;		if (*pstate).write then begin
;			safe_file_mkdir, s2, /verbose, error=error
;			if error then begin
;				warning,'file_requester',['Illegal directory name,','or error creating directory:', s2]
;				goto, finish
;			endif
;		endif
		(*pstate).to = s2
		set_widget_text, event.id, s2
		if (*pstate).dir then goto, done
		use_parent = 0

;		If create a new dir in To, then id will not be found, instead
;		find the parent node and call grow_tree to add the new leaf.

		id = find_leaf( (*pstate).tree_root, (*pstate).to)
		if id eq 0 then begin
			id = find_leaf( (*pstate).tree_root, file_dirname((*pstate).to,/mark))
			if id ne 0 then begin
				use_parent = 1
				old_path = (*pstate).to 
				(*pstate).to = file_dirname((*pstate).to,/mark)
			endif
		endif
		
;		If id not found anywhere, then open a new root ...

		widget_control, hourglass=1
		widget_control_update, event.top, update=0
		if id gt 0 then begin
			grow_tree, id, (*pstate).to, uname='tree', /expanded
			widget_control, id, /set_tree_select, /set_tree_expanded, /set_tree_visible
		endif else begin
			(*pstate).root = (*pstate).to
			widget_control, (*pstate).tree_root, /destroy
			(*pstate).tree_root = widget_tree((*pstate).tree, value=(*pstate).root, uvalue=(*pstate).root, uname='tree', /folder, /expanded)
			grow_tree, (*pstate).tree_root, (*pstate).root, uname='tree', /expanded
		endelse
		widget_control, (*pstate).root_text, set_value=(*pstate).root

;		This code occurs to open a new leaf added above for a new dir.

		if use_parent then begin
			id = find_leaf( (*pstate).tree_root, old_path)
			if id gt 0 then begin
				(*pstate).to = old_path 
				grow_tree, id, (*pstate).to, uname='tree', /expanded
				widget_control, id, /set_tree_select, /set_tree_expanded, /set_tree_visible
			endif
		endif
		widget_control_update, event.top, update=1
		end

	'add-trans-button': begin
		widget_control, (*pstate).from_text, get_value=s1
		widget_control, (*pstate).to_text, get_value=s2
		if n_elements(*(*pstate).ptrans) gt 0 then begin
			*(*pstate).ptrans = [*(*pstate).ptrans, {from:s1,to:s2}]
		endif else begin
			*(*pstate).ptrans = {from:s1,to:s2}
		endelse
		file_requester_draw_trans, pstate
		end

	'del-trans-button': begin
		i = widget_info( (*pstate).trans_list, /list_select)
		if i[0] ge 0 then begin
			j = i/2
			(*(*pstate).ptrans)[j].from = ''
			(*(*pstate).ptrans)[j].to = ''
			file_requester_draw_trans, pstate
		endif
		end

	'trans-shift-up': begin
		if event.select eq 1 then begin
			i = widget_info( (*pstate).trans_list, /list_select)
			if i[0] ge 0 then begin
				j = i/2
				q = indgen( n_elements( *(*pstate).ptrans))
				if j ge 1 then begin
					t = q[j-1]
					q[j-1] = q[j]
					q[j] = t
					qs = sort(q)
					*(*pstate).ptrans = (*(*pstate).ptrans)[qs]
					file_requester_draw_trans, pstate
				endif
			endif
		endif
		end
		
	'trans-shift-down': begin
		if event.select eq 1 then begin
			i = widget_info( (*pstate).trans_list, /list_select)
			if i[0] ge 0 then begin
				j = i/2
				nq = n_elements( *(*pstate).ptrans)
				q = indgen( nq)
				if j lt (nq-1) then begin
					t = q[j+1]
					q[j+1] = q[j]
					q[j] = t
					qs = sort(q)
					*(*pstate).ptrans = (*(*pstate).ptrans)[qs]
					file_requester_draw_trans, pstate
				endif
			endif
		endif
		end
		
	'file-list': begin
;		widget_control, event.top, update=0
		n = widget_info( (*pstate).file_list, /list_select)
		if n[0] ne -1 then begin
			(*pstate).file = strjoin( '"'+(*(*pstate).pfiles)[n]+'"', ' + ')
			*(*(*pstate).p).pfile = (*(*pstate).pfiles)[n]
			set_widget_text, (*pstate).file_text, (*pstate).file
			file_requester_preview, pstate
		endif
;		widget_control, event.top, update=1
		if event.clicks eq 2 then goto, done
		end

	'options': begin
		case event.value of
			0: begin
				(*pstate).numeric_ext = event.select
				if (*pstate).show_list then file_requester_update_list, pstate
				end
			1: begin
				(*pstate).date = event.select
				if (*pstate).show_list then file_requester_update_list, pstate
				end
			2: begin
				(*pstate).numeric_part = event.select
				if (*pstate).show_list then file_requester_update_list, pstate
				end
			else:
		endcase
		end

	'file-text': begin
		widget_control, event.id, get_value=s
		(*pstate).file = s
		str = strsplit(s,',',/extract)
		*(*(*pstate).p).pfile = str
;		widget_control, event.top, update=0
		set_widget_text, (*pstate).file_text, (*pstate).file
		file_requester_preview, pstate
;		widget_control, event.top, update=1
		goto, done
		end

	'filter-mode': begin
;		widget_control, (*pstate).path_text, get_value=s
;		n = strlen(s)
;		t = strmid( s, n-1,1)
;		ps = path_sep()
;		if t ne ps then s = s+ps
;		(*pstate).path = s
;		set_widget_text, (*pstate).path_text, s

		s = strcompress(event.str,/remove_all)
		if s eq '' then s='*'
		*(*pstate).pfilter = s
		if event.index lt 0 then begin
			q = where(s eq *(*pstate).pfilters, nq)
			if nq eq 0 then begin
				*(*pstate).pfilters = [*(*pstate).pfilters, s]
				widget_control, event.id, combobox_additem=s
			endif
		endif

		if (*pstate).show_list then file_requester_update_list, pstate
		end

	'delete-file-button': begin
		n = n_elements( *(*(*pstate).p).pfile)
		if (n gt 0) then begin
			for i=0L,n-1 do begin
				F = (*pstate).path + (*(*(*pstate).p).pfile)[i]
				if strlen(F) gt 0 then begin
					ok = dialog_message(['Delete the file:',F,'Are you sure?'], /question)
					if ok eq 'Yes' then file_delete, F
				endif
			endfor

			if (*pstate).show_list then file_requester_update_list, pstate
		endif
		end
		
	'cancel-button': begin
;		print,'Close fit batch ...'
		*(*(*pstate).p).pfile = ''
		(*(*pstate).p).error = 1
		(*(*pstate).p).cancel = 1
		goto, kill
		end

	'open-button': begin
		nle = locate('...', (*pstate).path)
		if nle lt 0 then goto, done
		warning,'file_reqeuster','Expand "..." node to select a valid directory.'
		end
	else:
endcase

finish:
	widget_control, hourglass=0
;#	widget_control, event.top, update=1
	close_file, lun
	return

done:
	p = (*pstate).p
	widget_control, (*pstate).path_text, get_value=s
	(*p).path = s
	if (*pstate).write then begin
		if locate('...', (*p).path) eq -1 then begin
			if file_test((*p).path,/dir) eq 0 then begin
				safe_file_mkdir, (*p).path, /verbose, error=error
				if error then begin
					warning,'file_requester',['Illegal directory name,','or error creating directory:', (*p).path]
				endif
			endif
		endif
	endif
;#	widget_control, event.top, update=1
	if (*pstate).show_list then begin
		widget_control, (*pstate).file_text, get_value=s

;		Allow filenames to contain '+', which means hiding them.
;		The restore '+' after splitting.
;		Also remove the enclosing "".

		s1 = hide_embedded(s,'+')
		str = strsplit( s1, '+', /extract)
		str1 = hide_embedded(str,'+',/unhide)
		for i=0,n_elements(str1)-1 do str1[i] = str_remove('"',str1[i])
		*(*p).pfile = strtrim( str1,2)
	endif
	(*p).error = 0
	goto, kill

bad_state:
	warning,'batch_sort_event',['STATE variable has become ill-defined.','Abort Batch Sort.'],/error
	goto, kill

kill:
	close_file, lun

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	if (*pstate).local_group then widget_control, (*pstate).group, /destroy
	return
end

;--------------------------------------------------------------------------

pro File_requester_draw_trans, pstate

COMPILE_OPT STRICTARR

	q = where( (*(*pstate).ptrans)[*].from ne '', nq)
	if nq ge 1 then begin
		*(*pstate).ptrans = (*(*pstate).ptrans)[q]
		t = strarr(2,nq)
		t[0,*] = (*(*pstate).ptrans)[*].from
		t[1,*] = '--> ' + (*(*pstate).ptrans)[*].to
		widget_control, (*pstate).trans_list, set_value=t
	endif else begin
		*(*pstate).ptrans = {from:'',to:''}
		widget_control, (*pstate).trans_list, set_value=''
	endelse
	return
end

;--------------------------------------------------------------------------

pro File_requester_find, pstate, find_path, pattern, exclude, top

COMPILE_OPT STRICTARR

	f = file_search2( find_path, pattern, cancel=cancel, /progress, all=0, exclude=exclude, group_leader=top)	;, /debug)
	if cancel then begin
		print,'file_requester: cancel from file_search'
		return
	endif
	if f[0] eq '' then begin
		warning,'file_requester','File "'+pattern+'" not found.',/info
		return
	endif
	s = extract_path(f[0])
	
	n = strlen(s)
	if n gt 0 then begin
		t = strmid( s, n-1,1)
		ps = path_sep()
		if t ne ps then s = s+ps
	endif else return
	s2 = s
	widget_control_update, top, update=0
	(*pstate).path = s2
	set_widget_text, (*pstate).path_text, s
	use_parent = 0

;	If create a new dir in Path, then id will not be found, instead
;	find the parent node and call grow_tree to add the new leaf.

	id = find_leaf( (*pstate).tree_root, (*pstate).path)
	if id eq 0 then begin
		id = find_leaf( (*pstate).tree_root, file_dirname((*pstate).path, /mark))
		if id ne 0 then begin
			use_parent = 1
			old_path = (*pstate).path 
			(*pstate).path = file_dirname((*pstate).path,/mark)
		endif
	endif
		
;	If id not found anywhere, then open a new root ...

	widget_control, hourglass=1
	if id gt 0 then begin
		grow_tree, id, (*pstate).path, uname='tree', /expanded
		widget_control, id, /set_tree_select, /set_tree_expanded, /set_tree_visible
	endif else begin
		(*pstate).root = (*pstate).path
		widget_control, (*pstate).tree_root, /destroy
		(*pstate).tree_root = widget_tree((*pstate).tree, value=(*pstate).root, uvalue=(*pstate).root, uname='tree', /folder, /expanded)
		grow_tree, (*pstate).tree_root, (*pstate).root, uname='tree', /expanded
	endelse
	widget_control, (*pstate).root_text, set_value=(*pstate).root

;	This code occurs to open a new leaf added above for a new dir.

	if use_parent then begin
		id = find_leaf( (*pstate).tree_root, old_path)
		if id gt 0 then begin
			(*pstate).path = old_path 
			grow_tree, id, (*pstate).path, uname='tree', /expanded
			widget_control, id, /set_tree_select, /set_tree_expanded, /set_tree_visible
		endif
	endif

	if (*pstate).show_list then file_requester_update_list, pstate
	return
end

;--------------------------------------------------------------------------

pro file_requester_update_list, pstate

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
		warning,'file_requester_update_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if (*pstate).dir then begin
	extension_numeric=0
	name_numeric = (*pstate).numeric_ext
	numeric_part = (*pstate).numeric_part
endif else begin
	extension_numeric=(*pstate).numeric_ext
	numeric_part = (*pstate).numeric_part
	numeric_ext = 0
endelse
nle = locate('...', (*pstate).path)
if (nle ge 1) or ((*pstate).path eq '') then begin
	*(*pstate).pfiles = ''
	widget_control, (*pstate).file_list, set_value=''
	return
endif

s = *(*pstate).pfilter
if s eq '' then s='*'
str = strsplit( strtrim(s,2),' ",',/extract)
n = n_elements(str)
files = ''
if n gt 0 then begin
	for i=0L,n-1 do begin
		f = find_file2( (*pstate).path + str[i], test_regular=1-(*pstate).dir, $
				extension_numeric=extension_numeric, name_numeric=name_numeric, part_numeric=numeric_part, $
				test_directory=(*pstate).dir)
		files = [files, strip_path( f, keep=(*pstate).dir)]
	endfor
endif
q = where( files ne '',nq)
if nq gt 0 then files = files[q]
if nq eq 0 then files = ''

; Sort by numeric (name or extension) or by creation date.
; If numeric and date (latest), then show reverse of numeric order

if (*pstate).date and ((*pstate).numeric_ext eq 0) then begin	; date and no numeric, 
	info = file_info( (*pstate).path + files)					; sort by reverse creation date
	date = info.mtime
	q = reverse(sort(date))
	files = files[q]
endif else if (*pstate).numeric_ext then begin					; numeric sort, name or extension
	q = indgen(n_elements(files))
	if (*pstate).date then q=reverse(q)							; if latest, then reverse order
	files = files[q]
endif else if (*pstate).numeric_part then begin					; numeric sort, name or extension
	q = indgen(n_elements(files))
	if (*pstate).date then q=reverse(q)							; if latest, then reverse order
	files = files[q]
endif else begin												; just alphabetical order
	q = sort(files)
	files = files[q]
endelse
*(*pstate).pfiles = files
widget_control, (*pstate).file_list, set_value=*(*pstate).pfiles
return
end

;--------------------------------------------------------------------------
				
pro file_requester_load, pstate

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
		warning,'file_requester_load',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	i = 0
	root_dir = geopixe_environment()
	geopixe_paths = root_dir + 'file-requester.paths'
	if file_test(geopixe_paths, /read) eq 0 then begin
		root_dir = geopixe_root
		geopixe_paths = root_dir + 'file-requester.paths'
		if file_test(geopixe_paths, /read) eq 0 then goto, cont
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
		q = sort(str[0:i-1])
		*(*pstate).ppaths = str[q]
	endif else begin
		*(*pstate).ppaths = ''
	endelse
	return
end

;--------------------------------------------------------------------------
				
pro file_requester_load_trans, pstate

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
		warning,'file_requester_load_trans',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	table = file_requester_get_trans( error=err)
	*(*pstate).ptrans = table
	return
end

;--------------------------------------------------------------------------
				
function file_requester_get_trans, error=err

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
		warning,'file_requester_get_trans',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, {from:'',to:''}
	endif
endif

	err = 1
	i = 0
	root_dir = geopixe_environment()
	geopixe_paths = root_dir + 'file-requester.translate'
	if file_test(geopixe_paths, /read) eq 0 then begin
		root_dir = geopixe_root
		geopixe_paths = root_dir + 'file-requester.translate'
		if file_test(geopixe_paths, /read) eq 0 then goto, cont
	endif
	on_ioerror, cont
	openr, lun, geopixe_paths, /get_lun
	table = replicate({from:'',to:''},1000)
	j = 0
	s = ''
	on_ioerror, cont
	while not EOF(lun) do begin
		readf, lun, s
		b = byte(s)
		q = where(b ge 32, nq)
		s = (nq gt 0) ? string(b[q]) : ''
		if j eq 0 then begin
			table[i].from = s
			j = 1
		endif else begin
			table[i].to = fix_path(s)
			i = i+1
			j = 0
		endelse
		if i eq 1000 then goto, cont
	endwhile

cont:
	close_file, lun
	if i ge 1 then begin
		q = where( table[0:i-1].from ne '', nq)
		if nq gt 0 then begin
			table = table[q]
			err = 0
		endif
	endif
	if err then table = {from:'',to:''}
	return, table
end

;--------------------------------------------------------------------------

pro file_requester_preview, pstate

COMPILE_OPT STRICTARR
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
		warning,'file_requester_preview',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if widget_info( (*pstate).details_list, /valid_id) eq 0 then return
if (*pstate).preview_routine eq '' then goto, bad
;files = (*pstate).file
files = *(*(*pstate).p).pfile
if n_elements(files) eq 0 then goto, bad
;file = strsplit(files, '+', /extract)
if files[0] eq '' then goto, bad
file = (*pstate).path + strtrim( files[0],2)

if (*pstate).ignore then begin
	call_procedure, (*pstate).preview_routine, file, preview=preview, /ignore
endif else begin
	call_procedure, (*pstate).preview_routine, file, preview=preview
endelse

if n_elements(preview) lt 1 then goto, bad
if size(preview, /tname) ne 'STRUCT' then goto, bad

;#widget_control, (*pstate).tlb, update=0
if tag_present('details',preview) then begin
	widget_control, (*pstate).details_list, set_value = preview.details
endif

wset, (*pstate).wid
erase
if tag_present('image',preview) then begin
	s = size(preview.image)
	if s[0] eq 2 then begin								; image
		nxp = n_elements(preview.image[*,0])
		nyp = n_elements(preview.image[0,*])
		scale = max([float(nxp)/(*pstate).xsize,float(nyp)/(*pstate).ysize])
		nx = long(nxp/scale)
		ny = long(nyp/scale)
		if tag_present('R',preview) then begin
			bb = congrid(preview.image,nx,ny)
			tvlct, r,g,b, /get
		  	tvlct, preview.r,preview.g,preview.b
			tv, bb
			tvlct, r,g,b
		endif else begin
			bb = bytscl( congrid(preview.image,nx,ny), top=99B) + 16B
			tv, bb
		endelse
	endif else if s[0] eq 3 then begin
		nxp = n_elements(preview.image[0,*,0])
		nyp = n_elements(preview.image[0,0,*])
		scale = max([float(nxp)/(*pstate).xsize,float(nyp)/(*pstate).ysize])
		nx = long(nxp/scale)
		ny = long(nyp/scale)
;		b = bytscl( congrid(preview.image,3,nx,ny), top=99B) + 16B
		b = congrid(preview.image,3,nx,ny)
		device, decomposed=1
		tv, b, true=1
		device, decomposed=0
	endif
	
endif else if tag_present('spectrum',preview) then begin
	wset, (*pstate).wid
	plot, preview.spectrum.x, preview.spectrum.y, yrange=[0.5,max(preview.spectrum.y)],/ylog, $
			position=[0.2,0.2,0.95,0.85], /ystyle,/xstyle, /nodata
	oplot, preview.spectrum.x, preview.spectrum.y, color=spec_colour('green')
endif else begin
	xyouts, 0.5,0.5,'no preview',align=0.5,/norm
endelse
;#widget_control, (*pstate).tlb, update=1
return

bad:
	widget_control, (*pstate).details_list, set_value = ''
	return
end

;--------------------------------------------------------------------------

pro OnRealize_File_requester_paths, wWidget

COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	file_requester_load, pstate
	widget_control, (*pstate).path_list, set_value=*(*pstate).ppaths
	return
end

;--------------------------------------------------------------------------

pro OnRealize_File_requester_translate, wWidget

COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	file_requester_load_trans, pstate
	widget_control, (*pstate).from_text, set_value=(*(*pstate).ptrans)[0].from
	widget_control, (*pstate).to_text, set_value=(*(*pstate).ptrans)[0].to
	file_requester_draw_trans, pstate
	return
end

;--------------------------------------------------------------------------

pro OnRealize_File_requester_list, wWidget

COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	file_requester_update_list, pstate
	return
end

;--------------------------------------------------------------------------

pro OnRealize_File_requester_root, wWidget

COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	set_widget_text, (*pstate).root_text, (*pstate).root
	return
end

;--------------------------------------------------------------------------

pro OnRealize_File_requester_preview_tab, wWidget

COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	geo = widget_info( (*pstate).preview_base, /geometry)
	(*pstate).preview_ysize = geo.ysize

	widget_control, wWidget, set_tab_current=(1-(*pstate).image)
	return
end

;--------------------------------------------------------------------------

pro OnRealize_File_requester_tree, wWidget

COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	file_requester_load, pstate
	path = strtrim((*pstate).path,2)
	np = strlen(path)
	bookmarks = strtrim(*(*pstate).ppaths,2)
	nmarks = n_elements(bookmarks)
	if (np lt 1) then begin
		if (nmarks gt 0) then begin
			tree_path = (*(*pstate).ppaths)[0]
			(*pstate).path = tree_path
		endif else begin
			tree_path = ''
		endelse
		goto, build
	endif else begin
		if nmarks eq 0 then begin
			tree_path = (*pstate).path
			goto, build
		endif
	endelse

	mask = intarr(nmarks)
	for i=0L,nmarks-1 do begin
		nb = strlen(bookmarks[i])
		if nb le np then begin
			test = strmid(path,0,nb)
			if file_lowcase(test) eq file_lowcase(bookmarks[i]) then mask[i]=1
		endif
	endfor
	q = where(mask eq 1,nq)
	if nq gt 0 then begin
		q2 = sort(strlen(bookmarks[q]))
		tree_path = (*(*pstate).ppaths)[q[q2[0]]]
	endif else begin
		tree_path = path
	endelse

build:
  	widget_control_update, top, update=0
	(*pstate).tree_root = widget_tree((*pstate).tree, value=tree_path, uvalue=tree_path, uname='tree', /folder, /expanded)
	(*pstate).root = tree_path
	set_widget_text, (*pstate).root_text, tree_path
	set_widget_text, (*pstate).path_text, (*pstate).path
	grow_tree, (*pstate).tree_root, tree_path, uname='tree'
	widget_control, (*pstate).tree_root, /set_tree_expanded

	id = find_leaf( (*pstate).tree_root, (*pstate).path)
	if id gt 0 then begin
		grow_tree, id, (*pstate).path, uname='tree'
		widget_control, id, /set_tree_select, /set_tree_expanded, /set_tree_visible
	endif
  	widget_control_update, top, update=1
	return
end

;--------------------------------------------------------------------------

pro OnRealize_File_requester_draw, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=wid
(*pstate).wid = wid

return
end

;--------------------------------------------------------------------------

function file_requester, title=title, path=pathi, file=filei, multiple_files=multiple, $
		dialog_parent=group, filter=filteri, fix_filter=fix_filter, group=group2, $
		get_path=get_path, preview_routine=preview_routinei, image=image, debug=debug, $
		read=readm, write=writem, must_exist=must_exist, directory=dir, $
		numeric=numeric, latest=latest, ignore=ignore, cancel=cancel, $
		skip_if_exists=skip_if_exists, skip_if_null=skip_if_nulli, translate=translate, $
		updir=updir, within_modal=within_modal, $
		
		noconfirm=noconfirm			; obsolete (only here for compatibility with v6.6)

; Alternative to 'dialog_pickfile' with more uniform behaviour across platforms
; Most options like in "dialog_pickfile", plus a few more (see below).
; 
; If 'file' is passed with a path included, and it is found, then that path will be used.
; Else, the 'path' will be used with the 'file' stripped of any path. If 'path' is blank,
; then the current working directory will be assumed.
;
; The order of file and path testing is as follows:
; 	file			test input file assumed to also have a remote path
; 	path			test input file-only on local alternative supplied path 'path'
; 	local updir		look for file-only updir from local 'path'
; 	remote updir	look for file-only updir from remote path (stripped from input filename)
; 	translate		look for a match translating path roots using table, then for each match:
; 		file		look for file-only match in translated dir
; 		updir		also look for file-only match updir (and searching down from there)
;
; The variable controlling the number of dirs allowed (before using "...") is
; in the 'find_file2()' function.
;
; file				initial filename (needs a path included to use /translate)
; path				initial path (will be tried if path included in file does not work)
; title				window title
; multiple_files	multiple select active
; dialog_parent		parent of modal pop-up
; within_modal		flags being called from a modal widget, which needs forcing /modal
; filter			file filter string array (e.g. '*' or ['*.spec','*.trav'] )
; /fix_filter		fix the filter
; /numeric			show only numeric file extensions
; /latest			show latest files first
; /directory		select and return a path name only
; 					select in tree, unless /multiple, and then in list
; get_path			return the final path
;
; /read				For file read, will cause a dir search for a match, updir and translate
; /write			For file write, no searching.
;					If both /read and /write are missing, then no search, updir or translate.
;					Set /write if require write of file, to be able to create new dirs.
;					Set /read to force search for file name provided (and updir, translate).
;
; new options:
; 'preview_routine'	string - name of preview routine
; /image			show image/spectrum preview first
; /ignore			pass 'ignore nulls' option onto read_geopixe_image (in preview)
; 
; /skip_if_exists	skip opening the requester if the file specified already exists
; 					on the path specified, or after translation or updir.
; /skip_if_null		skip opening the requester if the file specified is null/blank
; 					Need to pass a valid 'file' for this to be an effective test.
; translate=0		If file is passed with a full path included and /translate set (default), then
; 					if it is not found, or not in the selected path, then use translation tables 
; 					to try alternate path roots if the file path root matches one in the table.
; 					Use translate=0 to suppress auto-translation.
; updir=n			Also check up to 'n' dir level up for a match to the input file name.
;
; Preview routines:
; Input:	file		name of file
; Output:	result		structure of form {image:image, details:details}
; 
; where		image		preview image array
;			details		string array of various parameters and text
;						either part of result struct can be missing to indicate no return.
;
;	author:		C.G. Ryan, CSIRO							2010
;				added /skip_if_exists or null				2012	C.G. Ryan
;				added translation tables					2014	C.G. Ryan
;				refine search order							2016	C.G. Ryan
;				fixed offset jumping in Linux				2019	C.G. Ryan
;				used file_search2 to allow cancel			2019	C.G. Ryan
;				added "more" with file search				2022	C.G. Ryan
;				added 'within_modal' option					2024	C.G. Ryan

	COMPILE_OPT STRICTARR
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
			warning,'file_requester',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, ''
		endif
	endif
	common c_working_dir, geopixe_root
	if n_elements(geopixe_root) eq 0 then startupp
	
	cancel = 0
	default = file_expand_path('.')
	default = fix_path( default)
	get_path = ''
	if n_elements(writem) lt 1 then writem=0
	if n_elements(readm) lt 1 then readm=0
	if n_elements(within_modal) lt 1 then within_modal=0
	if n_elements(skip_if_exists) lt 1 then skip_if_exists=0
	if n_elements(skip_if_nulli) lt 1 then skip_if_null=0 else skip_if_null=skip_if_nulli
	if skip_if_exists and (writem eq 0) then readm=1
	if writem then begin
		title2 = 'Select File to Write'
		ok = 'Save'
	endif else if readm then begin
		title2 = 'Select File to Open'
		ok = 'Open'
	endif else begin
		title2 = 'Select File'
		ok = 'OK'
	endelse
;	if readm eq 0 then writem = 1					; can have both read=0, write=0, means no searching
	if writem eq 1 then readm = 0
	if n_elements(debug) eq 0 then debug=0			; use to disable /modal, /floating, /no_block=0
	if n_elements(ignore) lt 1 then ignore=0
	if n_elements(filei) lt 1 then begin
		filei = ''
		skip_if_null = 0
	endif
	file = filei
	if skip_if_null and (file eq '') then return, ''
	if (n_elements(pathi) lt 1) then path=default else path=pathi		;@3-20
	if ptr_valid(path[0]) then begin
		if ptr_good(path) then begin
			path = *path
		endif
	endif
	if n_elements(filteri) lt 1 then filteri='*'
	if n_elements(fix_filter) lt 1 then fix_filter=0
	if n_elements(group) lt 1 then group=0L
	if (n_elements(group2) ne 0) and (group eq 0) then group=group2
	if n_elements(multiple) lt 1 then multiple=0
	if n_elements(dir) lt 1 then dir=0
	if n_elements(numeric) lt 1 then numeric=0
	if n_elements(latest) lt 1 then latest=0
	if n_elements(image) lt 1 then image=0
	if dir then begin
		title2='Select Directory Path'
		if writem then title2='Select Directory Path to write'
	endif
	if n_elements(title) lt 1 then title = title2
	if n_elements(translate) lt 1 then translate=0
	if n_elements(updir) lt 1 then updir=0
	if ((updir ge 1) or translate) and (writem eq 0) then readm=1
	
	final_file = file
	preview = 0
	if n_elements(preview_routinei) gt 0 then preview=1
	if n_elements(preview_routinei) eq 0 then begin
		preview_routine = ''
	endif else begin
		preview_routine = preview_routinei
	endelse
	if preview_routine eq '' then preview=0
	if image and (preview eq 0) then begin
		preview = 1
		preview_routine = 'image_preview'
	endif
	tracking = 1

;	In order for file_requester() to use the 'progress' bar,
;	it must use the combination modal=0 and no_block=0. 
;
;	If Modal=1 is used, it would force no_block=0 anyway, so this works the same, in effect.
;	But this way (modal=0 and no_block=0) allows 'progress' to receive its events properly.
;
;	However, if called from a modal widget, then use /within_modal to force /modal here.
;
; History notes:
;	Originally, ‘file_requester’ was setup as a normal modal widget, which blocks until is closes.
;	It’s also floating, which means it sits in front of other windows.
;	
;	Blocking is similar, keeping control in xmanager near the end of the main file requester program. 
;	We don’t want to set no_block=1 on xmanager. However, we should *NEVER* set no_block=0, which does 
;	some weird stuff, like disable debugging. Simply, omit the no_block keyword to xmanager to achieve 
;	the same result. See ‘file_requester code.
;	
;	Then file_requester was changed to permit a progress bar popup while searching for a file match 
;	using “Find”. i.e. It was found that you could use modal=0, floating=1 on the top base widget and 
;	omit no_block on xmanager.
;	
;	However, then file_requester could not work properly if called from another modal widget (e.g. 
;	‘flux_select’). In that case, we use another option ‘/within_modal’ to remind file_requester to 
;	use /modal (and disable the Find). Only use ‘/within_modal’ when called from a modal widget.

	if debug then begin
		modal = 0
		floating = 0
		no_block = 1
	endif else if within_modal then begin
		modal = 1							; need this if 'file_requester' called from a modal
		floating = 1						; program like 'flux_scan'.
		no_block = 0						; But this means "Find" will not work properly (so search box disabled)
	endif else begin
		modal = 0							; was modal=1 before file search "Find" option added June,2022
		floating = 1
		no_block = 0
	endelse
	image_preview
	view_preview = preview
	
	filter = ''
	for i=0L,n_elements(filteri)-1 do begin
		filter = [filter, strsplit( strtrim(filteri[i],2), ' ",', /extract)]	
	endfor
	filter = filter[1:*]
	
	print,'File_requester -------------------------------------------------------------'
	path = fix_path( path)
	if file eq '' then begin								; blank filename
		if path[0] eq '' then path = default
		goto, cont
	endif
	file0 = file
	path0 = path[0]
	found_path = 0
	found_file = 0

;	Try the file with remote and local paths without searching first ...

	remote = fix_path( extract_path( file))	
	print,'    File_req: input file= '+file+', path= '+remote
	print,'    File_req: (paths= '+strjoin(path,', ')+')'

	f = strip_path( file)									; test using passed 'path'
	t = file_search( remote+f)	
	if t[0] ne '' then begin
		file = t[0]
		print,'    File_req: found file remote: '+file
		path = extract_path(file)
		goto, cont
	endif else if dir eq 1 then begin
;		safe_file_mkdir, remote, /verbose, error=error		; this may be a good idea?
;		if error then begin
;			warning,'file_requester',['Illegal directory name, write protected parent dir,','or error creating directory:', remote, '', $
;					'Check ownership and file protection bits for parent dir:', dir_up(remote)]
;			goto, finish
;		endif
		goto, cont
	endif

	n_path = n_elements(path)
	for i=0,n_path-1 do begin
		if (i gt 0) and (path[i] eq path[0]) then continue
		t = file_search( path[i]+f)							; test using alternate(s) 'path[]'
		if t[0] ne '' then begin			
			file = t[0]
			print,'    File_req: found file local: '+file
			path = extract_path(file)
			goto, cont
		endif
	endfor
	
;	Go searching for files only in /read mode

	if readm then begin
		paths = fix_path([remote,path])
		n_paths = n_elements(paths)
		f = strip_path( file)										; test using (each) passed 'path[]'
		k = 0
		exclude = ''												; maintain a list of dirs tested (and not to be redone)

;		Search down from current dir (only for /read mode) ...
;		Then go up dir levels (if updir > 0) from 'path[]' and search down from there ...

		repeat begin
			for i=0,n_paths-1 do begin
				if paths[i] eq '' then continue
				t = file_search2( paths[i], f, /progress, cancel=cancel, exclude=exclude, title='find: '+f+' in '+paths[i])	;, /debug)
				if cancel then goto, cont
				if t[0] ne '' then begin		
					file = t[0]
					print,'    File_req: found (deep/updir): file= '+file
					path = extract_path(file)
					goto, cont
				endif
			endfor
			last = paths
			paths = fix_path( dir_up(paths))
			k = k+1
		endrep until (k gt updir) or ( total(last eq paths, /integer) eq n_paths)
	
;		Try using the translation tables ...

		if translate then begin
			table = file_requester_get_trans( error=err)
			if err then goto, cont
			file = remote + strip_path( file)
							
			for i=0,n_elements(table)-1 do begin
				print,'....File_req: translate: test new path: ' + table[i].from 
				n = strlen(table[i].from)
				n2 = strlen(file)
				if (n eq 0) or (n2 lt n) then continue
				s = strmid( file,0,n)
				from = fix_path(table[i].from)								; 15/7/19
				if geopixe_case_sensitive() eq 0 then begin
					s = strlowcase(s)
					from = strlowcase(from)
				endif
				if s eq from then begin
					s1 = fix_path( table[i].to)
					t = s1 + strmid( file,n,n2-n)
					s1 = extract_path(t)
					print,'    File_req: try path match: path= '+s1
					if file_test( s1, /dir) then begin						; test dir using translated path
						file1 = t
						path1 = s1
						found_path = 1
						print,'    File_req: found path match: path= '+s1
						if file_test(t) then begin
							file2 = t
							path2 = s1
							print,'    File_req: found translated file: file= '+file2
							found_file = 1
							break
						endif
						if (found_file eq 0) and (updir gt 0) then begin	; try up a few dir levels
							f = strip_path( file)
							s1 = fix_path( dir_up(s1))
							k = 0
							repeat begin
								print,'    File_req: file not found translated, try updir: '+s1
								t = file_search2( s1, f, /progress, cancel=cancel, exclude=exclude, title='find: '+f+' in '+s1)
								if cancel then goto, cont
								if t2[0] ne '' then begin					; test up another level, but searching down from there
									file2 = t[0]
									path2 = s1
									print,'    File_req: found translated file updir: file= '+file2
									s1 = extract_path(file2)
									found_file = 1
									break
								endif
								last = s1
								s1 = fix_path( dir_up(s1))
								k = k+1
							endrep until (k ge updir) or (s1 eq last)
						endif
					endif
				endif
			endfor
		endif
		if found_file then begin
			file = file2
			path = path2
		endif else if found_path then begin
			file = file1
			path = path1
		endif else begin
			file = file0
			path = path0
		endelse
	endif

;	Now proceed to the file requester pop-up ...

cont:
	path = path[0]
	if (dir eq 0) and file_test(file) and (file ne '') then begin
		if skip_if_exists then begin
			get_path = extract_path(file)
			final_file = file
			print,'    file_req: return dir/file= ', file
			goto, finish
		endif
	endif

	if writem then begin
		path1 = extract_path(file)
		if file_test( path1) then begin
			path = path1
		endif
	endif
	print,'    File_req: start with file= '+file+' path='+path

	if (dir eq 0) or (writem eq 0) then begin
		if (file_test(path,/dir) eq 0) then path=extract_path(file)
		if (strlen(path) lt 1) or (file_test(path,/dir) eq 0) then path=default
		file = strip_path(file)
	endif
	
	if (dir eq 0) and file_test(path+file) and (file ne '') then begin
		if skip_if_exists then begin
			get_path = path
			final_file = path+file
			goto, finish
		endif
	endif

case !version.os_family of
	'MacOS': begin
		left_xsize = 270
		tree_ysize = preview ? 266 : ((dir and not multiple) ? 345 : 504)
		draw_ysize = 199
		right_xsize = 300
		path_ysize = (dir and not multiple) ? 270 : 132
		file_ysize = 170
		text_xsize = 230
		text_xsize2 = 250
		retain = 2
		help_trim = 20
		end
	'unix': begin
		left_xsize = 270
		tree_ysize = preview ? 266 : ((dir and not multiple) ? 345 : 504)
		draw_ysize = 199
		right_xsize = 300
		path_ysize = (dir and not multiple) ? 270 : 132
		file_ysize = 170
		text_xsize = 230
		text_xsize2 = 250
		retain = 2
		help_trim = 20
		end
	else: begin
		left_xsize = 270
		tree_ysize = preview ? 266 : ((dir and not multiple) ? 345 : 504)
		draw_ysize = 199
		right_xsize = 300
		path_ysize = (dir and not multiple) ? 270 : 132
		file_ysize = 175
		text_xsize = 230
		text_xsize2 = 250
		retain = 1
		help_trim = 20
		end
endcase

w = 0
h = 0
xoff = 0
yoff = 0
if widget_info( Group, /valid) then begin
	geom = widget_info( Group, /geometry)
	w = geom.scr_xsize
	h = geom.scr_ysize
	xoff = geom.xoffset
	yoff = geom.yoffset
	local_group = 0
endif else begin
	group = widget_base(scr_xsize=1, scr_ysize=1)
	local_group = 1
endelse
screen = get_screen_size()
if n_elements(xoffset) lt 1 then begin
	xoffset = ((xoff + w) < (screen[0]- (left_xsize+right_xsize+help_trim+20))) > 0
	if xoffset lt (xoff + w) then begin
		t = xoff - (left_xsize+right_xsize+help_trim+20)
		if t ge 0 then xoffset=t
	endif
endif
if n_elements(yoffset) lt 1 then begin
	yoffset = ((yoff) < (screen[1]-28 - 200)) > 0
endif

; 	top-level base

tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, modal=modal, floating=floating, $
					group_leader=group, uname='file-requester-tlb', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2 ,xoffset=xoffset, $
					yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

middlebase = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
lbase = widget_base( middlebase, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

ttbase = widget_base( lbase, /column, xpad=1, ypad=1, space=2, /base_align_center, /align_center, /frame)
label = widget_label( ttbase, value='Directory Tree')

tree_base = widget_base( ttbase, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

if dir then begin
	list_title = 'Directory Files'
	query_title = ['Numeric', 'Latest first', 'Numeric part']
	tree_help = 'Tree: Click on a path in the directory tree to select the path. Click "+", to expand a directory, ' + $
		'and "-" to collapse it; "Double click" toggles expanded state. Use "Root" to select a new directory tree, or select a new path from the bookmark list'
	book_help = 'Bookmarks: "Double click" on a path to open it in the directory Tree and select the path. ' + $
		'Single click paths to use "Del" to delete. "Add" will bookmark the current "Path". Use "Save" to save bookmarks for next time. ' + $
		'Enter a Path of "$HOME" to specify your home directory.'
	path_help = 'Path: Enter a new directory path. Hit <return> to activate it and open it in the directory Tree. ' + $
		'Use this field to also create a new directory. Remember to hit <return> to create it. ' + $
		'Enter a Path of "$HOME" to specify your home directory.'
	trans_help = 'Translation table: Convert original path stubs referenced in data files to local path stubs.' + $
		'Single click path pairs to use "Del" to delete entry. "Add" will include the current "From/To" path pair in the table. ' + $
		'Use the arrows to re-order, and "Save" to save the translation table for next time. '
	open_help = 'Open the selected path.'
	help_help = 'Select a path from the directory tree on the left, or by using the "path" field. ' + $
		'Open a new directory tree by specifying a new "Root" or by selecting a new "bookmark" from the list on the right.'
endif else begin
	list_title = 'Files satisfying filter'
	query_title = ['Numeric ext', 'Latest first', 'Numeric part']
	tree_help = 'Tree: Click on a path in the directory tree to view its files. Click "+", to expand a directory, ' + $
		'and "-" to collapse it; "Double click" toggles expanded state. Use "Root" to select a new directory tree, or select a new path from the bookmark list'
	book_help = 'Bookmarks: "Double click" on a path to open it in the directory Tree and view its files. ' + $
		'Single click paths to use "Del" to delete. "Add" will bookmark the current "Path". Use "Save" to save bookmarks for next time. ' + $
		'Enter a Path of "$HOME" to specify your home directory.'
	path_help = 'Path: Enter a new directory path. Hit <return> to activate it and open it in the directory Tree. ' + $
		'Use this field to also create a new directory. Remember to hit <return> to create it. ' + $
		'Enter a Path of "$HOME" to specify your home directory.'
	trans_help = 'Translation table: Convert original path stubs referenced in data files to local path stubs.' + $
		'Single click path pairs to use "Del" to delete entry. "Add" will include the current "From/To" path pair in the table. ' + $
		'Use the arrows to re-order, and "Save" to save the translation table for next time. '
	open_help = 'Open the selected file(s).'
	help_help = 'Select a path from the directory tree on the left, or by using the "path" field, and select files to open in the file list on the right, or using the "files" field. ' + $
		'Open a new directory tree by specifying a new "Root" or by selecting a new "bookmark" from the list on the right.'
endelse

tree = widget_tree( tree_base, xsize=left_xsize, ysize=tree_ysize, tracking=tracking, uvalue=tree_help, NOTIFY_REALIZE='OnRealize_file_requester_tree')

rbase = widget_base( ttbase, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
label = widget_label( rbase, value='Root:')
root_text = widget_text( rbase, value=path, uname='root-text', tracking=tracking, /editable, $
					NOTIFY_REALIZE='OnRealize_file_requester_root', scr_xsize=text_xsize, $
					uvalue='Root: Enter a new root absolute directory path to build a new directory tree. Hit <return> to activate and open the new tree.')

if preview and (dir eq 0) then begin
	preview_base = widget_base( lbase, /column, /base_align_center, /align_center, xpad = 0, ypad=0, space=0, map=1)
	tab_panel = widget_tab( preview_base, location=0, /align_center, uname='preview-tab-panel', $
					NOTIFY_REALIZE='OnRealize_file_requester_preview_tab')

	image_base = widget_base( tab_panel, title=' Preview ', /column, xpad=1, ypad=1, space=1, $
					/align_center, /base_align_center)		;, scr_xsize=left_xsize)

	draw = widget_draw( image_base, uname='preview', scr_xsize=left_xsize-5, scr_ysize=draw_ysize, tracking=tracking, $
					NOTIFY_REALIZE='OnRealize_file_requester_draw', retain=retain, $
					uvalue='Image: Preview of certain image and spectra file data types.')

	details_base = widget_base( tab_panel, title=' Details ', /column, xpad=1, ypad=1, space=1, $
					/align_center, /base_align_center)		;, scr_xsize=left_xsize)

	details_list = Widget_List( details_base, UNAME='details-list', value='', $
;		NOTIFY_REALIZE='OnRealize_File_requester_details', $
		scr_xsize=left_xsize-5 ,scr_ysize=draw_ysize, tracking=tracking, $
		uvalue='Details: parameters and details returned from selected file.')

endif else begin
	draw = 0L
	details_list = 0L
	preview_base = 0L
endelse

rbase = widget_base( middlebase, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

tab_panel = widget_tab( rbase, location=0, /align_center, uname='tab-panel')
tab_names = [' Bookmarked Paths ',' Translation Table ']

pbase = widget_base( tab_panel, title=tab_names[0], /column, xpad=1, ypad=1, space=2, /base_align_center, /align_center);, /frame)

path_list = Widget_List( pbase, UNAME='path-list', value='', $
		NOTIFY_REALIZE='OnRealize_File_requester_paths', $
		scr_xsize=right_xsize-4 ,scr_ysize=path_ysize, tracking=tracking, uvalue=book_help)
f0base = widget_base( pbase, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
label = widget_label( f0base, value='Path:')
path_text = widget_text( f0base, value=path, uname='path-text', tracking=tracking, /editable, $
					uvalue=path_help, scr_xsize=text_xsize2)
f1base = widget_base( pbase, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
label = widget_label( f1base, value='Find:')
find_text = widget_text( f1base, value='', uname='find-text', tracking=tracking, /editable, sensitive=(within_modal eq 0), $
					uvalue='Find a match:  Will set "Path" to the location of a file specified by a search pattern. ' + $
					'First, select start for search in "Path", pattern to search for in "Find" and hit <return> to start search. ' + $
					'Returns first match(es). Click "More" for next matches.', scr_xsize=text_xsize2-35-5)
button = widget_button( f1base, value='More', uname='find-more', tracking=tracking, scr_xsize=35, $
					uvalue='More: Find the next matches to "Find" pattern.')

b1base = widget_base( pbase, /row, /base_align_center, xpad = 0, ypad=0, space=5)
button = widget_button( b1base, value='Add', uname='add-button', tracking=tracking, $		;, scr_xsize=35, $
					uvalue='Add: Add the current "Path" to the list of bookmarks.')
button = widget_button( b1base, value='Del', uname='del-button', tracking=tracking, $		;, scr_xsize=35, $
					uvalue='Del: Delete the selected bookmark. Single click to select a bookmark.')
button = widget_button( b1base, value='Save', uname='save-button', tracking=tracking, $		;, scr_xsize=35, $
					uvalue='Save: Save bookmarks to disk to be available for future use of the file-requester.')
label = widget_label( b1base, value=' ', scr_xsize=10)
button = widget_button( b1base, value='Go', uname='go-button', tracking=tracking, $		;, scr_xsize=35, $
					uvalue='Go: Set the current path to the selected bookmark and locate the path in the directory tree and open it. ' + $
					'Alternatively, "double click" on a path in the bookmark list to select it and open it.')
button = widget_button( b1base, value='Up', uname='up-button', tracking=tracking, $		;, scr_xsize=35, $
					uvalue='Up: Move one directory UP in the current path tree.')


p2base = widget_base( tab_panel, title=tab_names[1], /column, xpad=1, ypad=1, space=2, /base_align_center, /align_center);, /frame)

trans_list = Widget_List( p2base, UNAME='translate-list', value='', $
		NOTIFY_REALIZE='OnRealize_File_requester_translate', $
		scr_xsize=right_xsize-4 ,scr_ysize=path_ysize, tracking=tracking, uvalue=trans_help)
t0base = widget_base( p2base, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
label = widget_label( t0base, value='From:')
from_text = widget_text( t0base, value='', uname='from-text', tracking=tracking, /editable, $
					uvalue='From: Set an original path root "From" to translate to the "To" path below. ' + $
					'Then select "Add" to add it to the translation table. Also remember to "Save" these definitions.', scr_xsize=text_xsize2)
t1base = widget_base( p2base, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
label = widget_label( t1base, value='To:')
to_text = widget_text( t1base, value='', uname='to-text', tracking=tracking, /editable, $
					uvalue='To: Set a translation path root target to translate the "From" path above to this path. ' + $
					'Then select "Add" to add it to the translation table. Also remember to "Save" these definitions.', scr_xsize=text_xsize2)

b2base = widget_base( p2base, /row, /base_align_center, xpad = 0, ypad=0, space=5)
button = widget_button( b2base, value='Add', uname='add-trans-button', tracking=tracking, $		;, scr_xsize=35, $
					uvalue='Add: Add the current "From" --> "To" path pair to the translation list. Also remember to "Save" these definitions.')
button = widget_button( b2base, value='Del', uname='del-trans-button', tracking=tracking, $		;, scr_xsize=35, $
					uvalue='Del: Delete the selected translation path pair. Single click to select an entry. Also remember to "Save" these definitions.')
label = widget_label( b2base, value='    ')

arrow = picture_button( b2base, geopixe_root + 'images/up-16x14.jpeg', uname='trans-shift-up', $
			/tracking, uvalue='Move this entry up one in translation table. Table is scanned from top to bottom, so put more general paths (shorter root stubs) at top and more specific ones at bottom.', /pushbutton_events)
arrow = picture_button( b2base, geopixe_root + 'images/down-16x14.jpeg', uname='trans-shift-down', $
			/tracking, uvalue='Move this entry down one in translation table. Table is scanned from top to bottom, so put more general paths (shorter root stubs) at top and more specific ones at bottom.', /pushbutton_events)

label = widget_label( b2base, value='    ')
button = widget_button( b2base, value='Save', uname='save-trans-button', tracking=tracking, $		;, scr_xsize=35, $
					uvalue='Save: Save translation table to your home .geopixe directory to be available for future use of the file-requester.')


files = strip_path( file_search( path + filter, /test_regular))

show_list = (dir eq 0) or multiple
if show_list then begin
	fbase = widget_base( rbase, /column, xpad=1, ypad=1, space=2, /base_align_center, /align_center, /frame)
	label = widget_label( fbase, value=list_title)
	if multiple then begin
		fhelp = 'File List: Select one or more files to open. "Double click" the file to open it immediately. Multiple select using "shift" and "ctrl" keys ' + $
			'together with a left-mouse click. Or, click and drag while holding down one of these keys.'
		fxhelp = 'File(s): Enter one or more file name(s), or multiply select files from the list above. Hit <return> to open the file immediately. ' + $
						'Multiple select using "shift" and "ctrl" keys together with a left-mouse click. ' + $
						'Or, click and drag while holding down one of these keys.'
		fbtext = dir ? 'Dir(s):' : 'File(s):'
	endif else begin
		fhelp = 'File List: Select a file to open. "Double click" the file to open it immediately. Filter files using a "Filter". ' + $
			'To sort files with numeric extensions or numeric final part of name check the "Numeric ext" or "Numeric part" check-boxes.'
		fxhelp = 'File: Enter a file name, or select a file from the list above. ' + $
						'Hit <return> to open the file immediately. For many files a preview of its contexts is displayed in the preview window.'
		fbtext = 'File:'
	endelse
	file_list = Widget_List( fbase, UNAME='file-list', value = files, multiple=multiple, $
				NOTIFY_REALIZE='OnRealize_File_requester_list', $
				tracking=tracking, uvalue=fhelp, scr_xsize=right_xsize ,scr_ysize=file_ysize)

	query = cw_bgroup2( fbase, query_title, /row, xpad=0, ypad=0, space=0, /return_index, /tracking, $
						uname='options', set_value=[numeric,latest,0], /nonexclusive, $
						uvalue=['Display files with numeric file extensions, or numeric directories, only, and sort the files numerically.', $
							'Sort files by date to show files with recent modification dates first.', 'Sort files by a numeric final part of the file-name (e.g. "_123").'])

	f1base = widget_base( fbase, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
	label = widget_label( f1base, value=fbtext)
	file_text = widget_text( f1base, value=file, uname='file-text', tracking=tracking, /editable, $
						uvalue=fxhelp, scr_xsize=text_xsize2)

	f2base = widget_base( fbase, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
	label = widget_label( f2base, value='Filter:')
	tfilt = filter
	q = where((filter ne "*") and (filter ne "*.*"), nq)
	if nq ne 0 then begin
		tfilt = tfilt[q]
		if nq gt 1 then begin
			filters = [ strjoin(tfilt,", "), tfilt, "*.*"]
		endif else begin
			filters = [tfilt, "*.*"]
		endelse
	endif else begin
		filters = filter 
	endelse
	filter = filters[0]
	filter_list = widget_combobox( f2base, value=filters, editable=(fix_filter eq 0), $
					uname='filter-mode', tracking=tracking, scr_xsize=text_xsize2, $
					uvalue='Filter: Enter a file filter (e.g. "*.spec") or a list of filter specifications separated by commas (e.g. "*-m.dai,*-x.dai"), or select one from the list.')

endif else begin
	file_list = 0L
	file_text = 0L
	filter_list = 0L
	filters = '*'
	query = 0L
endelse

b2base = widget_base( rbase, /row, /base_align_center, xpad = 0, ypad=0, space=5)
if preview then begin
	toggle_preview = cw_bgroup2( b2base, 'Preview', /row, xpad=0, ypad=0, space=0, /return_index, /tracking, $
						uname='preview-toggle', set_value=[1], /nonexclusive, $
						uvalue=['Toggle the visibility of the preview area.'])
endif else toggle_preview=0L
if show_list then begin
	button = widget_button( b2base, value='Delete', uname='delete-file-button', tracking=tracking, $		;, scr_xsize=35, $
					uvalue='Delete: Delete the selected file(s). Use with caution.')
endif else label = widget_label( b2base, value=' ', scr_xsize=15)
label = widget_label( b2base, value=' ', scr_xsize=15)
button = widget_button( b2base, value='Cancel', uname='cancel-button', tracking=tracking, $		;, scr_xsize=35, $
					uvalue='Cancel: Close requester quietly without opening any files.')
button = widget_button( b2base, value=ok, uname='open-button', tracking=tracking, $		;, scr_xsize=35, $
					uvalue=open_help)

help = widget_text( tbase, scr_xsize=left_xsize+right_xsize+help_trim, ysize=3, /wrap, uname='help', tracking=tracking, $
				uvalue=help_help, frame=0)

p = ptr_new( {pfile:ptr_new(file), path:path, dir:dir, cancel:0, error:0})

state = { $
		p:				p, $					; parameters
		within_modal:	within_modal, $			; flags special case of called from a modal popup
		
		group:			group, $				; group_leader
		local_group:	local_group, $			; fake group, need to close later
		tlb:			tlb, $					; top level base ID
		tree:			tree, $					; tree widget anchor
		tree_ysize:		tree_ysize, $			; tree Y size
		tree_root:		0L, $					; tree widget root		
		root_text:		root_text, $			; tree root text ID
		draw:			draw, $					; draw widget ID
		details_list:	details_list, $			; details list ID
		path_list:		path_list, $			; path list ID
		path_text:		path_text, $			; path text ID
		trans_list:		trans_list, $			; translation table list ID
		from_text:		from_text, $			; from text widget ID
		to_text:		to_text, $				; to text widget ID
		file_list:		file_list, $			; file list ID
		file_text:		file_text, $			; file text ID
		filter_list:	filter_list, $			; filter text ID
		find_text:		find_text, $			; find pattern text ID

		tracking:		tracking, $				; tracking enable
		dir:			dir, $					; directory mode
		numeric_ext:	numeric, $				; numeric file extensions
		date:			latest, $				; sort by latest date
		numeric_part:	0, $					; sort by numeric final part of name
		multiple:		multiple, $				; multiple file/dir selection
		show_list:		show_list, $			; show the list
		preview:		preview, $				; preview ON
		view_preview:	view_preview, $			; enable viewing of preview panel
		image:			image, $				; show image preview
		preview_routine: preview_routine, $		; name of preview routine
		toggle_preview:	toggle_preview, $		; toggle preview ID	
		ignore:			ignore, $				; pass 'ignore' to read_geopixe_image
		preview_base:	preview_base, $			; preview map base ID
		preview_ysize:	0, $					; preview area base Y size for mapping
		wid:			0L, $					; draw wid
		xsize:			left_xsize-5, $			; draw xsize
		ysize:			draw_ysize, $			; draw ysize
		pfilter:		ptr_new(filter), $		; filter text as input
		pfilters:		ptr_new(filters), $		; filter text list
		file:			file, $					; current file
		write:			writem, $				; /write file mode
		pfiles:			ptr_new(files), $
		current_path:	0, $					; current path list index
		path:			path, $					; current path
		ppaths:			ptr_new(/allocate_heap), $	; path list
		from:			'', $					; current 'from' path
		to:				'', $					; current 'to' path
		ptrans:			ptr_new(/allocate_heap), $	; translate table
		root:			path, $					; root path
		help:			help $					; help widget ID
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

; Never use no_block=0 on xmanager, it will disable debugging.
; Don't seem to need this now. The important point is to NOT use no_block=0 on xmanager
; probably ever, especially with the /modal base, where it is automatic.

if no_block then begin
	xmanager, 'file_requester', tlb, /no_block
endif else begin
	xmanager, 'file_requester', tlb
endelse

;print,'  done xmanager ...'
if (*p).error eq 0 and writem then begin
	if locate('...', (*p).path) eq -1 then begin
		if file_test((*p).path,/dir) eq 0 then begin
			safe_file_mkdir, (*p).path, /verbose, error=error
			if error then begin
				warning,'file_requester',['Illegal directory name,','or error creating path directory:', (*p).path]
			endif
		endif
	endif
endif
cancel = (*p).cancel

if show_list then begin
	file = *(*p).pfile
	if n_elements(file) eq 1 then file=file[0]
	if (*p).error or (file[0] eq '') then begin
		final_file = ''
		goto, finish
	endif
	get_path = (*p).path
	print,'    file_req: return dir/file= ', get_path + file
	final_file = get_path + file
endif else begin
	if (*p).error then begin
		final_file = ''
		goto, finish
	endif
	get_path = (*p).path
	print,'    file_req: return path= ', get_path
	final_file = get_path
endelse

finish:
	print,'End File_requester -------------------------------------------------------------'
	return, final_file
end
