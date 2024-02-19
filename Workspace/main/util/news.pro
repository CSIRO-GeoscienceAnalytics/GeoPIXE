;
; Display a news 'file' in a window.
;
;	Pass 'file' to display the file immediately, or
;	Send notify events to the news TLB, or globally if news does not have a parent,
;	to specify either 'file' to display a file path, or 'text' to display a string array.

pro news_event, event

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
			warning,'news_event',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif
	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate	
	if ptr_good(pstate,/struct) eq 0 then goto, bad_state

	case !version.os_family of
		'Windows': begin
			xoff = 12
			yoff = 12
			end
		'unix': begin
			xoff = 12
			yoff = 12
			end
		else: begin
			warning,'news',['Unknown O/S referenced: '+!version.os_family]
			end
	endcase

	case tag_names( event,/structure) of
		'NOTIFY': begin
			case event.tag of
				'news-file': begin
					if ptr_good( event.pointer) then begin
						file = *event.pointer
						list = news_read_file( file, error=err)
						if err then return
						widget_control, (*pstate).news_list, set_value=list
					endif
					end
				'news-text': begin
					if ptr_good( event.pointer) then begin
						list = *event.pointer
						widget_control, (*pstate).news_list, set_value=list
					endif
					end
				else:
			endcase
			end
		else:
	endcase

	uname = widget_info( event.id, /uname)
	case uname of
		'news-tlb': begin
			case Tag_Names(Event, /STRUCTURE_NAME) of
				'WIDGET_KILL_REQUEST': begin
					goto, kill
					end
				'WIDGET_BASE': begin
					x = (event.x > 300) - xoff
					y = ((event.y > 200) - yoff)
					widget_control, (*pstate).news_list, scr_xsize=x, scr_ysize=y
					end
				else:
			endcase
			end
		'news-list': begin
			end
		else:
	endcase
	return
  
bad_state:
	warning,'news',['STATE variable has become ill-defined.','Abort.'],/error
	goto, kill
bad_ptr:
	warning,'news',['Parameter structure variable has become ill-defined.','Abort.'],/error
	goto, kill

kill:
	widget_control, hourglass=0
	widget_control, event.top, /destroy
	return
end

;----------------------------------------------------------------------

function news_read_file, file, error=err
	
	COMPILE_OPT STRICTARR
	err = 1
	if n_elements(file) eq 0 then return, ''
	on_ioerror, bad_file
	openr, lun, file, /get_lun
	line = ''
	while eof(lun) eq 0 do begin
		readf, lun, line
		b = byte(line)
		q = where(b eq 9, nq)
		if nq gt 0 then b[q]=32B
		line = string(b)
		if n_elements(list) eq 0 then begin
			list = line
		endif else begin
			list = [list,line]
		endelse
	endwhile
	close_file, lun
	err = 0
	return, list
	
bad_file:
	warning,'News_read_file','Failed to open News file '+file
	return, ''
end

;----------------------------------------------------------------------

pro news, file, group=wgroup, TLB=tlb, title=title, $
				xoffset=xoffset, yoffset=yoffset

; Display a news 'file' in a window.

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
			warning,'news',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

;	if n_elements(file) eq 0 then return
	list = news_read_file( file, error=err)
;	if err then return
	
	if n_elements(wGroup) lt 1 then wGroup=0L
	if n_elements(title) lt 1 then title='GeoPIXE News'
	if n_elements(xoffset) lt 1 then begin
		screen = get_screen_size()
		xoffset = (0.5*screen[0]) > 0
	endif
	if n_elements(yoffset) lt 1 then begin
		screen = get_screen_size()
		yoffset = (screen[1]/2 - 800/2) > 0
	endif
	
	case !version.os_family of
		'Windows': begin
			list_xsize = 500
			list_ysize = 600
			end
		'unix': begin
			list_xsize = 600
			list_ysize = 600
			end
		else: begin
			warning,'news',['Unknown O/S referenced: '+!version.os_family]
			end
	endcase

	tlb = Widget_Base( GROUP_LEADER=wGroup, UNAME='news-tlb',  $
	       /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset,  $
	      /TLB_SIZE_EVENTS ,TITLE=title ,SPACE=3 ,XPAD=3,  $
	      YPAD=3 ,COLUMN=1 ,/BASE_ALIGN_CENTER)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
	
	news_list = Widget_List( tbase, UNAME='news-list', value=list, $
;			NOTIFY_REALIZE='OnRealize_news_news_list', $
			scr_xsize=list_xsize ,scr_ysize=list_ysize)
	
	state = {	$
				news_list:			news_list, $			; News List ID
				xoffset:			0, $					; offset in xsize for resize
				yoffset:			0 }						; offset in ysize for resize
	
	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize
	
	if widget_info( wGroup, /valid) then begin
		register_notify, tlb, ['news-file','news-text'], from=wGroup
	endif else begin
		register_notify, tlb, ['news-file','news-text']
	endelse
	
	XManager, 'news', tlb ,/no_block
	return	
end
	