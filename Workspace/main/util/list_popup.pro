;
;	Simple popup window for selection from a list.
;
;	title		optionsl window title
;	list		list strarr()
;	initial		initial value for result text widget.
;	group		group leader (creates its own if not present)
;	xoffset, yoffset	offset on screen
;
;	/debug	run in debug mode (not blocking or modal, enables IDL debugging easier)
;
;	Select on item in the list, which appears in the result text widget. Edit this text widget
;	to create a new item, not in list. Click "OK" to return this result string.
;
;	OK			Returns selected list row, or edited result in result text widget
;	Cancel		returns ''

pro list_popup_event, event

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
		warning,'list_popup_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
p = (*pstate).p

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
			'list-popup-tlb': begin
				case !version.os_family of
					'MacOS': begin
						xoff = 10
						yoff = 50
						end
					'unix': begin
						xoff = 10
						yoff = 50
						end
					else: begin
						xoff = 10
						yoff = 35
						end
				endcase
				widget_control, (*pstate).list, scr_xsize=event.x - xoff, scr_ysize=event.y-yoff
				goto, finish
				end
			else:
		endcase
		end
	else:
endcase

;#widget_control, event.top, update=0
uname = widget_info( event.id, /uname)
case uname of

	'popup-list': begin
		(*p).select = event.index
		(*p).error = 0
		item = (*p).list[ (*p).select]
		(*p).item = item
		widget_control, (*pstate).item_text, set_value = item
		end

	'item-text': begin
		widget_control, (*pstate).item_text, get_value=s
		(*p).item = s
		end

	'ok-button': begin
		widget_control, (*pstate).item_text, get_value=s
		(*p).item = s
		goto, kill
		end

	'cancel-button': begin
		(*p).item = ''
		goto, kill
		end

	else:
endcase

finish:
	widget_control, hourglass=0
;#	widget_control, event.top, update=1
	return

bad_state:
	warning,'list_popup_event',['STATE variable has become ill-defined.','Abort List Popup.'],/error
	goto, kill

kill:

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	if (*pstate).local_group then widget_control, (*pstate).group, /destroy
	ptr_free, pstate
	return
end

;--------------------------------------------------------------------------
;
;	Simple popup window for selection from a list.
;
;	title		optionsl window title
;	list		list strarr()
;	initial		initial value for result text widget.
;	group		group leader (creates its own if not present)
;	xoffset, yoffset	offset on screen
;
;	/debug	run in debug mode (not blocking or modal, enables IDL debugging easier)
;
;	Select on item in the list, which appears in the result text widget. Edit this text widget
;	to create a new item, not in list. Click "OK" to return this result string.
;
;	OK			Returns selected list row, or edited result in result text widget
;	Cancel		returns ''

function list_popup, title=title, list=listi, debug=debug, group=group, initial=initial, $
			xoffset=xoffset, yoffset=yoffset, sort=sort

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
			warning,'list_popup',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, ''
		endif
	endif
	
	if n_elements(title) lt 1 then title = 'Select from List'
	if n_elements(debug) lt 1 then debug=0
	if n_elements(sort) lt 1 then sort=0
	if n_elements(group) lt 1 then group=0L
	if n_elements(initial) lt 1 then initial=''
	if n_elements(listi) lt 1 then return, ''
	if sort then begin
		list = listi[ sort(listi) ]
	endif else list=listi

	case !version.os_family of
		'MacOS': begin
			list_xsize = 300
			list_ysize = 500
			end
		'unix': begin
			list_xsize = 300
			list_ysize = 500
			end
		else: begin
			list_xsize = 300
			list_ysize = 500
			end
	endcase

	tracking = 0
	if debug then begin
		modal = 0
		floating = 0
		no_block = 1
	endif else begin
		modal = 1
		floating = 1
		no_block = 0
	endelse

	if widget_info( Group, /valid) then begin
		local_group = 0
	endif else begin
		group = widget_base(scr_xsize=1, scr_ysize=1)
		local_group = 1
	endelse

	screen = get_screen_size()
	if n_elements(xoffset) lt 1 then begin
		xoffset = screen[0]/2 - 100
	endif
	if n_elements(yoffset) lt 1 then begin
		yoffset = (screen[1]/2 - 300 ) > 0
	endif

	tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, modal=modal, floating=floating, $
					group_leader=group, uname='list-popup-tlb', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2 ,xoffset=xoffset, $
					yoffset=yoffset, /base_align_center)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

	popup_list = Widget_List( tbase, UNAME='popup-list', value=list, $
			scr_xsize=list_xsize ,scr_ysize=list_ysize, tracking=tracking, $
			uvalue='Details: parameters and details returned from selected file.')

	rbase = widget_base( tbase, /row, /base_align_center, /align_right, xpad = 1, ypad=0, space=5)
	item_text = widget_text( rbase, value=initial, uname='item-text', tracking=tracking, /editable, $
					uvalue='Select item from list above or enter a new one here.', scr_xsize=list_xsize-132)

	button = widget_button( rbase, value='Cancel', uname='cancel-button', tracking=tracking, scr_xsize=75, $
					uvalue='Cancel selection.')
	button = widget_button( rbase, value='OK', uname='ok-button', tracking=tracking, scr_xsize=45, $
					uvalue='Accept selection.')

	p = ptr_new( {list:list, select:0L, item:'', error:0} )

	state = { $
			p:			p, $							; list data
			debug:		debug, $						; debug flag
			tracking:	tracking, $						; tracking flag
			group:		group, $						; parent group
			local_group: local_group, $					; flags local group base to tidy up later
			item_text:	item_text, $					; selected item text ID
			list:		popup_list $					; popup list ID
		}
	
	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

;	Never use no_block=0 on xmanager, it will disable debugging.
;	Don't seem to need this now. The important point is to NOT use no_block=0 on xmanager
;	probably ever, especially with the /modal base, where it is automatic.

	if no_block then begin
		xmanager, 'list_popup', tlb, /no_block
	endif else begin
		xmanager, 'list_popup', tlb
	endelse

	item = (*p).item
	ptr_free, p
	return, item
end
