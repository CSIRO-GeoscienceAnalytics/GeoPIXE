;
;	Edit supplied 'text' strings, and label each using 'label'.
;	Both can be arrays for an array of strings
;	/numeric	make sure text is a valid numeric value.
;
;----------------------------------------------------------------------

pro string_edit_event, Event

COMPILE_OPT STRICTARR

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	p = (*pstate).p
	
	uname = widget_info( event.id, /uname)

	case tag_names( event,/structure) of
		'NOTIFY': begin
;			case event.tag of
;
;				else:
;			endcase
			end
		'WIDGET_TRACKING': begin
			widget_control, event.id, get_uvalue=s
			if event.enter eq 1 then begin
				if size(s,/tname) eq 'STRING' then begin
					widget_control, (*pstate).help, set_value=s
				endif else if size(s,/tname) eq 'STRUCT' then begin
					t = tag_names( s)
					q = where( t eq 'HELP')
					if q[0] ne -1 then begin
						if size(s.Help,/tname) eq 'STRING' then begin
							widget_control, (*pstate).help, set_value=s.Help
						endif
					endif
				endif
			endif else begin
				if abs((*p).new.charge - (*p).new.conv * (*p).flux) gt 0.001 then begin
					s = 'NOTE: New "Charge" is not consistent with the new "Conv" and "Flux" (IC count) values.'
				endif else s=''
				widget_control, (*pstate).help, set_value=s
			endelse
			goto, finish
			end
		'WIDGET_TIMER': begin
		;	print,' got a timer event; update text reads ...'
			end
		else:
	endcase

	case uname of

		'string_edit_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				(*p).error = 1
				widget_control, event.top, /destroy
				return
			endif
			end
			
		'string': begin
			widget_control, event.id, get_uvalue=i
			widget_control, (*pstate).text_widget[i], get_value=s
			if (*pstate).numeric then begin
				if gnumeric(s) eq 0 then begin
					warning,'string_edit',['Not a valid numeric string.','Try again ...']
					goto, finish
				endif
			endif
			(*p).text[i] = s
			end
			
		'ok': begin
			error = 0
			for i=0,n_elements((*p).text)-1 do begin
				widget_control, (*pstate).text_widget[i], get_value=s
				if (*pstate).numeric then begin
					if gnumeric(s) eq 0 then begin
						warning,'string_edit',['Not a valid numeric string.','Try again ...']
						goto, finish
					endif
				endif
				(*p).text[i] = s
			endfor
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

pro string_edit, group, title=title, label=label, text=text, debug=debug, $
		numeric=numeric, error=error

;	Edit supplied 'text' strings, and label each using 'label'.
;	Both can be arrays for an array of strings
;	/numeric	make sure text is a valid numeric value.

COMPILE_OPT STRICTARR
	n = n_elements(text)
	if n lt 1 then return
	if n_elements(label) ne n then label='String '+str_tidy(indgen(n_elements(text)))
	if n_elements(title) lt 1 then title='Edit text strings'
	if n_elements(debug) lt 1 then debug=0
	if n_elements(group) lt 1 then group=0L
	if widget_info(group,/valid) eq 0 then begin
		group = widget_base( scr_xsize=1, scr_ysize=1)
		local = 1
	endif else local=0
	if n_elements(numeric) lt 1 then numeric=0

	if debug then begin
		modal = 0
		floating = 0
	endif else begin
		modal = 1
		floating = 1
	endelse
	
	case !version.os_family of
		'MacOS': begin
;			symbol = 'SYMBOL*12'
;			large_font = 'Arial*12'
;			widget_control, default_font='Geneva*10'		; set font for all windows
			button_xsize = 100
			help_xsize = 265
			left_xsize = 60
			end
		'unix': begin
;			symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
;			large_font = '10x20'
;			widget_control, default_font='6x13'				; set font for all windows
			button_xsize = 100
			help_xsize = 265
			left_xsize = 60
			end
		else: begin
;			symbol = 'SYMBOL*BOLD*14'
;			large_font = 'COURIER*BOLD*10'
		;	widget_control, default_font='Arial*14'			; set font for all windows
			button_xsize = 100
			help_xsize = 265
			left_xsize = 60
			end
	endcase
	mscale = 3
	if numeric then mscale=1
	
	xsize = 120
	ysize = 80
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])
	text_widget = lonarr(n)
	
	tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='string_edit_TLB', /base_align_center, $
					modal=modal, floating=floating, xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_right)

	for i=0,n-1 do begin
		r1base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
		lab = widget_label( r1base, value=label[i]+':')
		text_widget[i] = widget_text( r1base, uname='string', value=text[i], $
						uvalue=i, scr_xsize=mscale*button_xsize, /edit)
	endfor
	
	bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	button = widget_button( bbase, value='Cancel', uname='cancel',scr_xsize=button_xsize)
;	lab = widget_label( bbase, value='     ')
	button = widget_button( bbase, value='OK', uname='ok',scr_xsize=button_xsize)

	p = ptr_new(  {	text:			text, $							; text strings
					error:			0 $								; error flag
					})
					
	state = {	$
				p:						p, $						; pointer to selection
				group:					group, $					; group leader window
				local:					local, $					; group is local, so close later
				numeric:				numeric, $					; flags numeric text strings
				text_widget:			text_widget $				; widget ID of text strings
			}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	if debug then begin
		xmanager, 'string_edit', tlb, /no_block
	endif else begin
		xmanager, 'string_edit', tlb
	endelse

	text = (*p).text
	error = (*p).error
	return
end
