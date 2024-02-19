;
;	Simple pop-up blocking widget to gather entry of parameters, modes, files, etc.
;	
;	r = options_popup( group, title=title, drop=drop, file=file, text=text, check=check, $
;		map_drop=map_drop, map_file=map_file, map_text=map_text, map_check=map_check, $
;		initial_text=initial_text, exclusive=exclusive, $
;		help_drop=help_drop, help_file=help_file, help_text=help_text, help_check=help_check, $
;		path=path, filter=filter, multiple=multiple, dir=dir, debug=debug, error=error)
;
;	Popup a modal options window to collect the following optional elements:
;	explanation	explanatory text (optional), appears in new 'explanation' field
;	drop		droplists: drop[m,n]=strings for 'm' droplists op[tions for each of 'n' droplists (ignore blanks)
;	file		file widgets: file[n]=strings for 'n' file button labels and file text fields
;	text		text widgets: text[n]=strings for labels to 'n' text edit widgets
;	check		checkbox widgets: check[n]=strings for 'n' checkboxes
;	/exclusive	for exclusive checkbox/radio buttons
;
;	map_...		map value for each element of first drop-list, to map on/off selected widgets
;	help_...	help text for each drop-list, file, text, checkbox
;	filter		vector of file filters for each file widget
;	multiple	vector of multiple file-select for file requesters
;	dir			dir mode for file widgets
;	
;	group		group leader (optional)
;	/debug		debug mode, not modal for testing, but returns straight away.
;	error		error return: 1=bad, 0=OK
;
; Droplist text: The visibility of other widgets (and extra droplists) can be ciontrolled with
; the map_... keyword vectors (with one value for each element of the first droplist)
;
; Optionally, map visibility of each drop, file, text, all checkboxes based on droplist 1.
; Each vector has a value to see (1) or hide (0) each widget ...
;
; Optionally, provide a context-sensitive help string for each member of each widget class.
;
; If no parameters are provided, it defaults to a 2 text field entry pop-up.
; 
; Multiple files are returned in a single string, with the full path in the first, and added
; files appended following "+". To separate these into a file vector, use the function:
; "options_multiple_files()".
;
;----------------------------------------------------------------------

pro options_popup_event, Event

COMPILE_OPT STRICTARR

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	p = (*pstate).p

	uname = widget_info( event.id, /uname)

	case tag_names( event,/structure) of
		'WIDGET_TRACKING': begin
			if widget_info( (*pstate).help, /valid) eq 0 then goto, finish
			widget_control, event.id, get_uvalue=s
			if event.enter eq 1 then begin
				if size(s,/tname) eq 'STRING' then begin
					widget_control, (*pstate).help, set_value=s
				endif else if size(s,/tname) eq 'STRUCT' then begin
					if tag_present('HELP',s) then begin
						if size(s.Help,/tname) eq 'STRING' then begin
							widget_control, (*pstate).help, set_value=s.Help
						endif
					endif
				endif
			endif else begin
				widget_control, (*pstate).help, set_value=(*pstate).help_default
			endelse
			goto, finish
			end
		'WIDGET_TIMER': begin
		;	print,' got a timer event; update text reads ...'
			end
		else:
	endcase

	case uname of

		'options_popup_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				(*p).error = 1
				widget_control, event.top, /destroy
				return
			endif
			end

		'drop-list': begin
			widget_control, event.id, get_uvalue=u
			i = u.n
			(*p).drop[i] = event.index
			if (i eq 0) then begin
				if n_elements((*pstate).drop_base) gt 1 then begin
					for j=1,n_elements((*pstate).drop_base)-1 do begin
						if widget_info( (*pstate).drop_base[j], /valid) then begin
							widget_control, (*pstate).drop_base[j], map=(*pstate).map_drop[(*p).drop[i]]
							widget_control, (*pstate).drop_base[j], scr_ysize=(*pstate).map_drop[(*p).drop[i]] ? (*pstate).drop_base_ysize : 0
						endif
					endfor
				endif
				for j=0,n_elements((*pstate).file_base)-1 do begin
					if widget_info( (*pstate).file_base[j], /valid) then begin
						widget_control, (*pstate).file_base[j], map=(*pstate).map_file[(*p).drop[i]]
						widget_control, (*pstate).file_base[j], scr_ysize=(*pstate).map_file[(*p).drop[i]] ? (*pstate).file_base_ysize : 0
					endif
				endfor
				for j=0,n_elements((*pstate).text_base)-1 do begin
					if widget_info( (*pstate).text_base[j], /valid) then begin
						widget_control, (*pstate).text_base[j], map=(*pstate).map_text[(*p).drop[i]]
						widget_control, (*pstate).text_base[j], scr_ysize=(*pstate).map_text[(*p).drop[i]] ? (*pstate).text_base_ysize : 0
					endif
				endfor
				if widget_info( (*pstate).check_base, /valid) then begin
					widget_control, (*pstate).check_base, map=(*pstate).map_check[(*p).drop[i]]
					widget_control, (*pstate).check_base, scr_ysize=(*pstate).map_check[(*p).drop[i]] ? (*pstate).check_base_ysize : 0
				endif
			endif
			end
			
		'file-button': begin
			widget_control, event.id, get_uvalue=u
			i = u.n
			F = file_requester( /read, title='Select ' + (*pstate).file[i], path=(*pstate).path, multiple=(*pstate).multiple[i], $
								file=(*p).file[i], filter=(*pstate).filter[i], group=event.top, dir=(*pstate).dir )
			if F[0] ne '' then begin
				(*p).file[i] = F[0]
				(*pstate).path = extract_path(F[0])
				if n_elements(F) gt 1 then begin
					for j=1,n_elements(F)-1 do (*p).file[i] = (*p).file[i] + '+' + strip_path(F[j]) 
				endif
				set_widget_text, (*pstate).file_widget[i], (*p).file[i]
			endif
			end
			
		'file-string': begin
			widget_control, event.id, get_uvalue=u
			i = u.n
			widget_control, (*pstate).file_widget[i], get_value=s
			(*p).file[i] = s
			end
			
		'text-string': begin
			widget_control, event.id, get_uvalue=u
			i = u.n
			widget_control, (*pstate).text_widget[i], get_value=s
			(*p).text[i] = s
			end
			
		'check': begin
			i = event.value
			(*p).check[i] = event.select			
			end
			
		'ok': begin
			if n_elements((*pstate).text_widget) ge 1 then begin
				for i=0,n_elements((*pstate).text_widget)-1 do begin
					if widget_info( (*pstate).text_widget[i], /valid) then begin
						widget_control, (*pstate).text_widget[i], get_value=s
						(*p).text[i] = s
					endif
				endfor
			endif
			if n_elements((*pstate).file_widget) ge 1 then begin
				for i=0,n_elements((*pstate).file_widget)-1 do begin
					if widget_info( (*pstate).file_widget[i], /valid) then begin
						widget_control, (*pstate).file_widget[i], get_value=s
						(*p).file[i] = s
					endif
				endfor
			endif
			(*p).error = 0
			widget_control, event.top, /destroy
			if (*pstate).local then widget_control, (*pstate).group, /destroy
			return
			end
			
		'cancel': begin
			(*p).error = 1
			widget_control, event.top, /destroy
			if (*pstate).local then widget_control, (*pstate).group, /destroy
			return
			end
		else:
	endcase

finish:
	return
end

;-----------------------------------------------------------------

pro OnRealize_options_popup_droplist, wWidget

	COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	widget_control, wWidget, get_uvalue=u
	i = u.n	
	geo = widget_info( (*pstate).drop_base[i], /geometry)
	(*pstate).drop_base_ysize = geo.ysize

	if (i ne 0) and ((*pstate).map_drop[0] eq 0) then begin
		widget_control, (*pstate).drop_base[i], map=0, scr_ysize=1
	endif
	return
end

;-----------------------------------------------------------------

pro OnRealize_options_popup_file, wWidget

	COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate
	p = (*pstate).p

	widget_control, wWidget, get_uvalue=u
	i = u.n
	geo = widget_info( (*pstate).file_base[i], /geometry)
	(*pstate).file_base_ysize = geo.ysize
	set_widget_text, wWidget, (*p).file[i]	

	if (*pstate).map_file[0] eq 0 then begin
		widget_control, (*pstate).file_base[i], map=0, scr_ysize=1
	endif
	return
end

;-----------------------------------------------------------------

pro OnRealize_options_popup_text, wWidget

	COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	widget_control, wWidget, get_uvalue=u
	i = u.n
	geo = widget_info( (*pstate).text_base[i], /geometry)
	(*pstate).text_base_ysize = geo.ysize

	if (*pstate).map_text[0] eq 0 then begin
		widget_control, (*pstate).text_base[i], map=0, scr_ysize=1
	endif
	return
end

;-----------------------------------------------------------------

pro OnRealize_options_popup_check, wWidget

	COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	geo = widget_info( (*pstate).check_base, /geometry)
	(*pstate).check_base_ysize = geo.ysize

	if (*pstate).map_check[0] eq 0 then begin
		widget_control, (*pstate).check_base, map=0, scr_ysize=1
	endif
	return
end

;-----------------------------------------------------------------

function options_multiple_files, F

;	Separate concantenated file strings in 'F', adding path in first file to all.
;	Return as a string array.

	COMPILE_OPT STRICTARR

	s = strsplit( F, '+', /extract)
	ns = n_elements(s)
	r = strarr(ns)
	r[0] = s[0]
	path = extract_path(s[0])
	for i=1,ns-1 do begin
		r[i] = path + s[i]
	endfor

	return, r
end

;-----------------------------------------------------------------

function options_popup, group, title=title, drop=drop, file=file, text=text, check=check, $
		map_drop=map_drop, map_file=map_file, map_text=map_text, map_check=map_check, $
		help_drop=help_drop, help_file=help_file, help_text=help_text, help_check=help_check, $
		initial_text=initial_text, initial_file=initial_file, initial_check=initial_check, multiple=multiple, $
		help_default=help_default, min_xsize=min_xsize, explanation=explanation, columns_check=columns_check, $
		path=path, filter=filter, dir=dir, debug=debug, exclusive=exclusive, error=error

;	Popup a modal options window to collect the following optional elements:
;	explanation	explanatory text (optional), appears in new 'explanation' field
;	drop		droplists: drop[m,n]=strings for 'm' droplist options for each of 'n' droplists (ignore blanks)
;	file		file widgets: file[n]=strings for 'n' file button labels and file text fields
;	text		text widgets: text[n]=strings for labels to 'n' text edit widgets
;	check		checkbox widgets: check[n]=strings for 'n' checkboxes
;	/exclusive	for exclusive checkbox/radio buttons
;	columns_check	number of columns to use for check marks
;
;	initial_... inital values for text, file, check
;	map_...	 	map value for each element of first drop-list, to map on/off selected widgets
;
;	help_... 	help text for each drop-list, file, text, checkbox
;	help_default  default help string to be displayed on entry and between other helps.
;
;	filter		file filter for all file widgets
;	multiple 	vector of multiple file-select for file requesters
;	dir			dir mode for file widgets
;	
;	group		group leader (optional)
;	min_xsize	guide to minimum xsize for display
;	/debug		debug mode, not modal for testing, but returns straight away.
;	error		error return: 1=bad, 0=OK
;
; Droplist text: The visibility of other widgets (and extra droplists) can be ciontrolled with
; the map_... keyword vectors (with one value for each element of the first droplist)
;
; Optionally, map visibility of each drop, file, text, all checkboxes based on droplist 1.
; Each vector has a value to see (1) or hide (0) each widget ...
;
; Optionally, provide a context-sensitive help string for each member of each widget class.
;
; Multiple files are returned in a single string, with the full path in the first, and added
; files appended following "+". To separate these into a file vector, use the function:
; "options_multiple_files()".


COMPILE_OPT STRICTARR
	error = 1
	if n_elements(title) lt 1 then title='Edit text strings'
	if n_elements(debug) lt 1 then debug=0
	if n_elements(help_default) lt 1 then help_default='Move cursor over widget to see help on it ...'
	if n_elements(path) lt 1 then path=''
	if n_elements(dir) lt 1 then dir=0
	if n_elements(exclusive) lt 1 then exclusive=0
	if n_elements(group) lt 1 then group=0L
	if n_elements(min_xsize) lt 1 then min_xsize=100
	if n_elements(columns_check) lt 1 then columns_check=2
	if widget_info(group,/valid) eq 0 then begin
		group = widget_base( scr_xsize=1, scr_ysize=1)
		local = 1
	endif else local=0
	if debug then begin
		modal = 0
		floating = 0
	endif else begin
		modal = 1
		floating = 1
	endelse
	tracking = 1
	if (n_elements(help_drop) eq 0) and (n_elements(help_file) eq 0) and (n_elements(help_text) eq 0) and (n_elements(help_check) eq 0) then tracking=0 
	
	s = size(drop, /struct)
	case s.n_dimensions of
		0: n_drop = (s.n_elements eq 1) ? 1 : 0
		1: n_drop = 1
		2: n_drop = s.dimensions[1]
		else: begin
			warning,'options_popup','Only 1D and 2D "drop" string argument lists allowed.'
			return,0
			end
	endcase
	if n_elements(map_drop) eq 0 then begin
		case s.n_dimensions of
			1: map_drop = replicate(1B,s.dimensions[0])
			2: map_drop = replicate(1B,s.dimensions[0])
			else: map_drop = 1B
		endcase
	endif else if n_elements(map_drop) ne n_elements(drop[*,0]) then begin
		warning,'options_popup','Dimensions of "map_drop" must match "drop[*,0]".'
	endif
	if n_elements(map_file) eq 0 then begin
		case s.n_dimensions of
			1: map_file = replicate(1B,s.dimensions[0])
			2: map_file = replicate(1B,s.dimensions[0])
			else: map_file = 1B
		endcase
	endif else if n_elements(map_file) ne n_elements(drop[*,0]) then begin
		warning,'options_popup','Dimensions of "map_file" must match "drop[*,0]".'
	endif
	if n_elements(map_text) eq 0 then begin
		case s.n_dimensions of
			1: map_text = replicate(1B,s.dimensions[0])
			2: map_text = replicate(1B,s.dimensions[0])
			else: map_text = 1B
		endcase
	endif else if n_elements(map_text) ne n_elements(drop[*,0]) then begin
		warning,'options_popup','Dimensions of "map_text" must match "drop[*,0]".'
	endif
	if n_elements(map_check) eq 0 then begin
		case s.n_dimensions of
			1: map_check = replicate(1B,s.dimensions[0])
			2: map_check = replicate(1B,s.dimensions[0])
			else: map_check = 1B
		endcase
	endif else if n_elements(map_check) ne n_elements(drop[*,0]) then begin
		warning,'options_popup','Dimensions of "map_check" must match "drop[*,0]".'
	endif
	
	n_file = n_elements(file)
	n_text = n_elements(text)
	n_check = n_elements(check)
	if n_file eq 0 then file=''
	if (n_elements(filter) lt 1) then begin
		filter = replicate('*', (n_file>1))
	endif
	if (n_elements(multiple) lt 1) then begin
		multiple = replicate(0, (n_file>1))
	endif
	if (n_drop eq 0) and (n_file eq 0) and (n_text eq 0) and (n_check eq 0) then begin
		text = ['Option 1','Option 2']
		n_text = n_elements(text)
	endif
	if n_elements(help_check) eq 0 then help_check=''

	case !version.os_family of
		'MacOS': begin
			schar = 1.2 * !d.x_ch_size
			end
		'unix': begin
			schar = 1.3 * !d.x_ch_size
			end
		else: begin
			schar = 1.0 * !d.x_ch_size
			end
	endcase

	nchar = 10								
	if n_file gt 0 then nchar = nchar > max(strlen(file))
	if n_text gt 0 then nchar = nchar > max(strlen(text))
;	if n_check gt 0 then nchar = nchar > total(strlen(check))
	xchar1 = (( nchar < 100) * schar) > 50					; size of labels

	nchar = 20							
	if n_file gt 0 then begin
		if n_elements(initial_file) gt 0 then begin
			nchar = nchar > max(strlen(initial_file))
		endif
	endif
	if n_text gt 0 then begin
		if n_elements(initial_text) gt 0 then begin
			nchar = nchar > max(strlen(initial_text))
		endif
	endif
	xchar2 = (( nchar < 100) * schar) > 100					; size of fields

	nchar = 10
	if n_drop gt 0 then nchar = nchar > max(strlen(drop))
	xchar3 = (xchar1 + xchar2) > (( nchar < 100) * schar)	; size of drops
	
	case !version.os_family of
		'MacOS': begin
			button_xsize = xchar1
			field_xsize = xchar2
			base_xsize = xchar3 > min_xsize
			file_xsize = field_xsize > (min_xsize-button_xsize)
			drop_xsize = xchar3 > min_xsize
			text_xsize = field_xsize > (min_xsize-80)
			check_xsize = base_xsize			; xchar3
			help_xsize = base_xsize
			fly = 1.2
			end
		'unix': begin
			button_xsize = xchar1
			field_xsize = xchar2
			base_xsize = xchar3 > min_xsize
			file_xsize = field_xsize > (min_xsize-button_xsize)
			drop_xsize = xchar3 > min_xsize
			text_xsize = field_xsize > (min_xsize-80)
			check_xsize = base_xsize			; xchar3
			help_xsize = base_xsize
			fly = 1.2
			end
		else: begin
			button_xsize = xchar1
			field_xsize = xchar2
			base_xsize = xchar3 > min_xsize
			file_xsize = field_xsize > (min_xsize-button_xsize)
			drop_xsize = xchar3 > min_xsize
			text_xsize = field_xsize > (min_xsize-80)
			check_xsize = base_xsize			; xchar3
			help_xsize = base_xsize
			fly = 1.1
			end
	endcase
	
	xsize = help_xsize
	ysize = 40*(1+n_drop+n_file+n_text+n_check/2)
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])
	
	tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='options_popup_TLB', /base_align_center, $
					modal=modal, floating=floating, xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=1, /base_align_right)

	if n_elements(explanation) gt 0 then begin
		nl = 0
		for k=0,n_elements(explanation)-1 do begin
			nl = nl + ceil((fly * (strlen( explanation[k])+1) * !d.x_ch_size) / help_xsize)
		endfor
		explain = widget_text( tbase, scr_xsize=help_xsize, ysize=nl, /wrap, uname='options-explanation', /tracking, $
				value=explanation, uvalue='Explanation of the role of this Options popup.', frame=1)
	endif

	if n_drop ge 1 then begin
		drop_base = lonarr(n_drop)
		drop_widget = lonarr(n_drop)
		for i=0,n_drop-1 do begin
			if drop[0,i] ne '' then begin
				q = where( drop[*,i] ne '', nq)
				if nq gt 0 then begin
					drop_base[i] = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_right, map=(i ne 0) ? map_drop[0] : 1)
					htxt = (n_elements(help_drop) gt i) ? help_drop[i] : ''
					drop_widget[i] = widget_combobox( drop_base[i], value=reform(drop[q,i]), uname='drop-list', tracking=tracking, xsize=drop_xsize, $
							uvalue={n:i, help:htxt}, Notify_Realize='OnRealize_options_popup_droplist' )
				endif
			endif
		endfor
	endif else begin
		drop_widget = 0L
		drop_base = 0L
	endelse
	
	if n_file ge 1 then begin
		file_base = lonarr(n_file)
		file_widget = lonarr(n_file)
		if n_elements(initial_file) ne n_file then initial_file=strarr(n_file)		
		for i=0,n_file-1 do begin
			file_base[i] = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_right, map=map_file[0])
			htxt = (n_elements(help_file) gt i) ? help_file[i] : ''
			button = widget_button( file_base[i], value=file[i]+':', uname='file-button', tracking=tracking, $
						uvalue={n:i, help:'File selection button: '+htxt}, scr_xsize=button_xsize )
			file_widget[i] = widget_text( file_base[i], uname='file-string', value=initial_file[i], tracking=tracking, $
						uvalue={n:i, help:'File edit text string: '+htxt}, scr_xsize=file_xsize, /edit, Notify_Realize='OnRealize_options_popup_file')
		endfor
	endif else begin
		file_widget = 0L
		file_base = 0L
		initial_file = ''
	endelse
	
	if n_text ge 1 then begin
		text_base = lonarr(n_text)
		text_widget = lonarr(n_text)
		if n_elements(initial_text) ne n_text then initial_text=strarr(n_text)
		for i=0,n_text-1 do begin
			text_base[i] = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_right, map=map_text[0])
			lab = widget_label( text_base[i], value=text[i]+':')
			htxt = (n_elements(help_text) gt i) ? help_text[i] : ''
			text_widget[i] = widget_text( text_base[i], uname='text-string', value=initial_text[i], tracking=tracking, $
						uvalue={n:i, help:htxt}, scr_xsize=text_xsize, /edit, Notify_Realize='OnRealize_options_popup_text')
		endfor
	endif else begin
		text_widget = 0L
		text_base = 0L
		initial_text = ''
	endelse
	
	if n_check ge 1 then begin
		icheck = bytarr(n_check)
		if exclusive then begin
			cval = 0
			if n_elements( initial_check) gt 0 then cval=initial_check
			icheck[cval] = 1
		endif else begin
			cval = replicate(0,n_elements(check))
			if n_elements( initial_check) gt 0 then cval=initial_check
			icheck[*] = cval
		endelse
		check_base = widget_base( tbase, /column, xpad=0, ypad=0, space=2, /base_align_right, /align_right, map=map_check[0], $
								Notify_Realize='OnRealize_options_popup_check')
		check_widget = cw_bgroup2( check_base, check, set_value=cval, column=columns_check, xsize=check_xsize, $
						/return_index, uname='check', exclusive=exclusive, nonexclusive=1-exclusive, tracking=tracking, $
						uvalue=help_check, ypad=0)
	endif else begin
		icheck = bytarr(n_check>1)
		check_widget = 0L
		check_base = 0L
	endelse
	
	bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	button = widget_button( bbase, value='Cancel', uname='cancel',scr_xsize=120, tracking=tracking, uvalue='Ignore parameters entered and return.')
	button = widget_button( bbase, value='OK', uname='ok',scr_xsize=120, tracking=tracking, uvalue='Accept parameters entered and proceed with operation.')

	if tracking then begin
		help = widget_text( tbase, scr_xsize=help_xsize, ysize=4, /wrap, uname='HELP', tracking=tracking, $
				value=help_default, frame=0)
	endif else help=0L
	
	p = ptr_new(  {	drop:			intarr(n_drop>1), $				; drop indices
					file:			initial_file, $					; file names
					text:			initial_text, $					; text strings
					check:			icheck, $						; checkbox states
					error:			0 $								; error flag
					})
					
	state = {	$
				p:						p, $						; pointer to selection
				group:					group, $					; group leader window
				local:					local, $					; group is local, so close later
				path:					path, $						; default path
				drop_base_ysize:		0L, $						; drop base Y size geometry
				file_base_ysize:		0L, $						; file base Y size geometry
				text_base_ysize:		0L, $						; text base Y size geometry
				check_base_ysize:		0L, $						; check base Y size geometry
				file:					file, $						; file labels
				filter:					filter, $					; file filters
				multiple:				multiple, $					; multiple select
				dir:					dir, $						; dir mode
				help_default:			help_default, $				; default help string
				map_drop:				map_drop, $					; map drop bases (except #0) on/off based on droplist 0
				map_file:				map_file, $					; map file bases on/off based on droplist 0
				map_text:				map_text, $					; map text bases on/off based on droplist 0
				map_check:				map_check, $				; map check base on/off based on droplist 0
				drop_widget:			drop_widget, $				; widget ID of drop lists
				drop_base:				drop_base, $				; widget ID of drop bases
				file_widget:			file_widget, $				; widget ID of file strings
				file_base:				file_base, $				; widget ID of file bases
				text_widget:			text_widget, $				; widget ID of text strings
				text_base:				text_base, $				; widget ID of text bases
				check_widget:			check_widget, $				; widget ID of check box states
				check_base:				check_base, $				; widget ID of check-box base
				help:					help $						; widget ID of help text
			}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	if debug then begin
		xmanager, 'options_popup', tlb, /no_block
	endif else begin
		xmanager, 'options_popup', tlb
	endelse

	results = *p
	error = (*p).error
	return, results
end
