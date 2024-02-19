
;	Fit Results table.

pro stage_edit_event, event

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
		warning,'stage_edit_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

no_list = 0
if ptr_good(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
p = (*pstate).plist
if ptr_valid(p) eq 0 then goto, bad_ptr
if size(*p,/tname) ne 'POINTER' then begin
	no_list = 1
endif else begin
	if ptr_valid( (*p)[0] ) eq 0 then no_list=1
	if no_list eq 0 then if size(*(*p)[0],/tname) ne 'STRUCT' then no_list=1
endelse

plist = (*pstate).plist
ps = (*pstate).ps
pm = (*pstate).pm
p = (*pstate).pitem
overscans = (*pstate).overscans

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'scan-edit-select': begin
				(*pstate).sel = (*event.pointer).top
				if ptr_good( plist) eq 0 then goto, finish
				n = n_elements( *plist)
				(*pstate).sel = clip( (*pstate).sel, 0, n-1) 
;				print,'Notify "scan-select", sel = ', (*pstate).sel
				*p = *(*plist)[ (*pstate).sel]
				
				stage_edit_update, pstate, /update_display
				widget_control, (*pstate).charge_min_text, sensitive=(*p).raster.step_scan
				widget_control, (*pstate).charge_max_text, sensitive=(*p).raster.step_scan
				widget_control, (*pstate).photons_min_text, sensitive=(*p).raster.step_scan
				widget_control, (*pstate).photons_max_text, sensitive=(*p).raster.step_scan
				end
			else:
		endcase
		goto, finish
		end
	'WIDGET_TRACKING': begin
		if (*pstate).tracking eq 0 then goto, finish
		widget_control, event.id, get_uvalue=s
		if size(s,/tname) eq 'STRING' then begin
			if event.enter eq 1 then begin
				widget_control, (*pstate).help, set_value=s
			endif else begin
				widget_control, (*pstate).help, set_value=(*pstate).current_alert
			endelse
		endif
		goto, finish
		end
	'WIDGET_TIMER': begin
		u = widget_info( event.id, /uname)
		case u of
			'left-button': begin							; timer to do initial refresh of widgets
				stage_edit_update, pstate, /update_display
				widget_control, (*pstate).charge_min_text, sensitive=(*p).raster.step_scan
				widget_control, (*pstate).charge_max_text, sensitive=(*p).raster.step_scan
				widget_control, (*pstate).photons_min_text, sensitive=(*p).raster.step_scan
				widget_control, (*pstate).photons_max_text, sensitive=(*p).raster.step_scan
				end
			'right-button': begin
				if (*pstate).extent.left eq 2 then begin
					(*pstate).extent.left = 0
					widget_control, (*pstate).left_button, set_value={select:(*pstate).extent.left}
				endif
				if (*pstate).extent.right eq 2 then begin
					(*pstate).extent.right = 0
					widget_control, (*pstate).right_button, set_value={select:(*pstate).extent.right}
				endif
				if (*pstate).extent.top eq 2 then begin
					(*pstate).extent.top = 0
					widget_control, (*pstate).top_button, set_value={select:(*pstate).extent.top}
				endif
				if (*pstate).extent.bottom eq 2 then begin
					(*pstate).extent.bottom = 0
					widget_control, (*pstate).bottom_button, set_value={select:(*pstate).extent.bottom}
				endif
				end
			else:
		endcase
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request stage_edit ...'
		goto, kill
		end
	else:
endcase

stage_edit_update, pstate, /from_panel

uname = widget_info( event.id, /uname)
case uname of

	'stage_edit_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				x = event.x > 570
				y = event.y > 350
				widget_control, event.id, scr_xsize=x
				widget_control, (*pstate).help, scr_xsize=x-20
				end
			else:
		endcase
		end

	'clear-button': begin
		(*pstate).extent.left = 0
		(*pstate).extent.right = 0
		(*pstate).extent.top = 0
		(*pstate).extent.bottom = 0
		widget_control, (*pstate).left_button, set_value={select:0}
		widget_control, (*pstate).right_button, set_value={select:0}
		widget_control, (*pstate).top_button, set_value={select:0}
		widget_control, (*pstate).bottom_button, set_value={select:0}
		stage_edit_update, pstate, /update_display
		end
		
	'left-button': begin
		stage_edit_position, pstate
		(*pstate).mark.left = (*pstate).position.x
		(*pstate).extent.left = ((*pstate).extent.right ge 1) ? 2 : 1
		widget_control, (*pstate).left_button, set_value={select:(*pstate).extent.left}
		if (*pstate).extent.left eq 2 then begin
			(*pstate).extent.right = 2
			widget_control, (*pstate).right_button, set_value={select:(*pstate).extent.right}
			
			(*p).origin.x = (*pstate).mark.left
			(*p).raster.size.x = (*pstate).mark.right - (*pstate).mark.left
			(*p).origin.z = (*pstate).position.z
			stage_edit_update, pstate, /update_display
			widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		endif
		end
		
	'right-button': begin
		stage_edit_position, pstate
		(*pstate).mark.right = (*pstate).position.x
		(*pstate).extent.right = ((*pstate).extent.left ge 1) ? 2 : 1
		widget_control, (*pstate).right_button, set_value={select:(*pstate).extent.right}
		if (*pstate).extent.right eq 2 then begin
			(*pstate).extent.left = 2
			widget_control, (*pstate).left_button, set_value={select:(*pstate).extent.left}
			
			(*p).origin.x = (*pstate).mark.left
			(*p).raster.size.x = (*pstate).mark.right - (*pstate).mark.left
			(*p).origin.z = (*pstate).position.z
			stage_edit_update, pstate, /update_display
			widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		endif
		end
		
	'top-button': begin
		stage_edit_position, pstate
		(*pstate).mark.top = (*pstate).position.y
		(*pstate).extent.top = ((*pstate).extent.bottom ge 1) ? 2 : 1
		widget_control, (*pstate).top_button, set_value={select:(*pstate).extent.top}
		if (*pstate).extent.top eq 2 then begin
			(*pstate).extent.bottom = 2
			widget_control, (*pstate).bottom_button, set_value={select:(*pstate).extent.bottom}
			
			(*p).origin.y = (*pstate).mark.bottom
			(*p).raster.size.y = (*pstate).mark.top - (*pstate).mark.bottom
			(*p).origin.z = (*pstate).position.z
			stage_edit_update, pstate, /update_display
			widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		endif
		end
		
	'bottom-button': begin
		stage_edit_position, pstate
		(*pstate).mark.bottom = (*pstate).position.y
		(*pstate).extent.bottom = ((*pstate).extent.bottom ge 1) ? 2 : 1
		widget_control, (*pstate).bottom_button, set_value={select:(*pstate).extent.bottom}
		if (*pstate).extent.bottom eq 2 then begin
			(*pstate).extent.top = 2
			widget_control, (*pstate).top_button, set_value={select:(*pstate).extent.top}
			
			(*p).origin.y = (*pstate).mark.bottom
			(*p).raster.size.y = (*pstate).mark.top - (*pstate).mark.bottom
			(*p).origin.z = (*pstate).position.z
			stage_edit_update, pstate, /update_display
			widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		endif
		end
		
	'bottom-left-button': begin
		stage_edit_position, pstate
		(*pstate).mark.bottom = (*pstate).position.y
		(*pstate).extent.bottom = ((*pstate).extent.bottom ge 1) ? 2 : 1
		widget_control, (*pstate).bottom_button, set_value={select:(*pstate).extent.bottom}
		if (*pstate).extent.bottom eq 2 then begin
			(*pstate).extent.top = 2
			widget_control, (*pstate).top_button, set_value={select:(*pstate).extent.top}
			
			(*p).origin.y = (*pstate).mark.bottom
			(*p).raster.size.y = (*pstate).mark.top - (*pstate).mark.bottom
			(*p).origin.z = (*pstate).position.z
		endif
		(*pstate).mark.left = (*pstate).position.x
		(*pstate).extent.left = ((*pstate).extent.right ge 1) ? 2 : 1
		widget_control, (*pstate).left_button, set_value={select:(*pstate).extent.left}
		if (*pstate).extent.left eq 2 then begin
			(*pstate).extent.right = 2
			widget_control, (*pstate).right_button, set_value={select:(*pstate).extent.right}
			
			(*p).origin.x = (*pstate).mark.left
			(*p).raster.size.x = (*pstate).mark.right - (*pstate).mark.left
			(*p).origin.z = (*pstate).position.z
		endif
		stage_edit_update, pstate, /update_display
		widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		end
		
	'top-right-button': begin
		stage_edit_position, pstate
		(*pstate).mark.top = (*pstate).position.y
		(*pstate).extent.top = ((*pstate).extent.bottom ge 1) ? 2 : 1
		widget_control, (*pstate).top_button, set_value={select:(*pstate).extent.top}
		if (*pstate).extent.top eq 2 then begin
			(*pstate).extent.bottom = 2
			widget_control, (*pstate).bottom_button, set_value={select:(*pstate).extent.bottom}
			
			(*p).origin.y = (*pstate).mark.bottom
			(*p).raster.size.y = (*pstate).mark.top - (*pstate).mark.bottom
			(*p).origin.z = (*pstate).position.z
		endif
		(*pstate).mark.right = (*pstate).position.x
		(*pstate).extent.right = ((*pstate).extent.left ge 1) ? 2 : 1
		widget_control, (*pstate).right_button, set_value={select:(*pstate).extent.right}
		if (*pstate).extent.right eq 2 then begin
			(*pstate).extent.left = 2
			widget_control, (*pstate).left_button, set_value={select:(*pstate).extent.left}
			
			(*p).origin.x = (*pstate).mark.left
			(*p).raster.size.x = (*pstate).mark.right - (*pstate).mark.left
			(*p).origin.z = (*pstate).position.z
		endif
		stage_edit_update, pstate, /update_display
		widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		end
		
	'centre-button': begin
		stage_edit_position, pstate
		(*pstate).mark.left = (*pstate).position.x - (*p).raster.size.x /2
		(*pstate).mark.right = (*pstate).position.x + (*p).raster.size.x /2
		(*pstate).mark.top = (*pstate).position.y + (*p).raster.size.y /2
		(*pstate).mark.bottom = (*pstate).position.y - (*p).raster.size.y /2
		(*pstate).extent.left = 2
		(*pstate).extent.right = 2
		(*pstate).extent.top = 2
		(*pstate).extent.bottom = 2
		widget_control, (*pstate).left_button, set_value={select:(*pstate).extent.left}
		widget_control, (*pstate).right_button, set_value={select:(*pstate).extent.right}
		widget_control, (*pstate).top_button, set_value={select:(*pstate).extent.top}
		widget_control, (*pstate).bottom_button, set_value={select:(*pstate).extent.bottom}
		(*p).origin.x = (*pstate).mark.left
		(*p).raster.size.x = (*pstate).mark.right - (*pstate).mark.left
		(*p).origin.y = (*pstate).mark.bottom
		(*p).raster.size.y = (*pstate).mark.top - (*pstate).mark.bottom
		(*p).origin.z = (*pstate).position.z
		stage_edit_update, pstate, /update_display
		widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		end
	
	'grab-z-button': begin
		stage_edit_position, pstate
		(*p).origin.z = (*pstate).position.z
		stage_edit_update, pstate, /update_display
		end

	'beam-text': begin
		stage_edit_update, pstate, /update_display
		end

	'axis-x-mode': begin
		(*p).raster.axis.x = event.index
		widget_control, (*pstate).scale_x_text, sensitive=((*p).raster.axis.x eq 0)
		end

	'axis-y-mode': begin
		(*p).raster.axis.y = event.index
		widget_control, (*pstate).scale_y_text, sensitive=((*p).raster.axis.y eq 1)
		end

	'size-x-text': begin
		stage_edit_update, pstate, /update_display
		end

	'size-y-text': begin
		stage_edit_update, pstate, /update_display
		end

	'npixels-x-text': begin
		widget_control, (*pstate).npixels_x_text, get_value=s
		nx = float2(s)
		(*p).raster.pixel.x = (*p).raster.size.x / nx
		stage_edit_update, pstate, /update_display
		end

	'npixels-y-text': begin
		widget_control, (*pstate).npixels_y_text, get_value=s
		ny = float2(s)
		(*p).raster.pixel.y = (*p).raster.size.y / ny
		stage_edit_update, pstate, /update_display
		end

	'pixel-x-text': begin
		stage_edit_update, pstate, /update_display
		end

	'pixel-y-text': begin
		stage_edit_update, pstate, /update_display
		end
		
	'scale-x-text': begin
		widget_control, (*pstate).scale_x_text, get_value=s
		socket_command_set, ps, 'scale', float2(s), class='deflect.axis', chip=0, error=error
		socket_command_set, ps, 'unit', 'mm', class='deflect.axis', chip=0
		stage_edit_update, pstate, /update_display
		end

	'scale-y-text': begin
		widget_control, (*pstate).scale_y_text, get_value=s
		socket_command_set, ps, 'scale', float2(s), class='deflect.axis', chip=1, error=error
		socket_command_set, ps, 'unit', 'mm', class='deflect.axis', chip=1
		stage_edit_update, pstate, /update_display
		end

	'shape-mode': begin
		if event.index ge 1 then begin
			warning,'stage_edit','May not be sufficient Klee resources for a non-rectangular raster.'
		endif
		(*p).raster.shape = event.index < 0
		widget_control, (*pstate).shape_mode, set_combobox_select=(*p).raster.shape
		stage_edit_update, pstate, /update_display
		end

	'overscan-mode': begin
		(*p).raster.overscan = overscans[ event.index < (n_elements(overscans)-1)]
		stage_edit_update, pstate, /update_display
		end

	'fly-step-toggle': begin
		(*p).raster.step_scan = event.value
		help,event,/str
		widget_control, (*pstate).charge_min_text, sensitive=(*p).raster.step_scan
		widget_control, (*pstate).charge_max_text, sensitive=(*p).raster.step_scan
		widget_control, (*pstate).photons_min_text, sensitive=(*p).raster.step_scan
		widget_control, (*pstate).photons_max_text, sensitive=(*p).raster.step_scan
		if (*p).raster.step_scan eq 0 then begin
			widget_control, (*pstate).charge_min_text, set_value='0.'
			widget_control, (*pstate).charge_max_text, set_value='0.'
			widget_control, (*pstate).photons_min_text, set_value='0.'
			widget_control, (*pstate).photons_max_text, set_value='0.'
		endif
		stage_edit_update, pstate, /update_display
		end

	'interlace-check': begin
		case event.value of
			0: begin
				(*p).raster.interlace = event.select
				end
			else:
		endcase
		stage_edit_update, pstate, /update_display
		end

	'charge-min-text': begin
		stage_edit_update, pstate, /update_display
		end

	'charge-max-text': begin
		stage_edit_update, pstate, /update_display
		end

	'photons-min-text': begin
		stage_edit_update, pstate, /update_display
		end

	'photons-max-text': begin
		stage_edit_update, pstate, /update_display
		end

	'time-min-text': begin
		stage_edit_update, pstate, /update_display
		end

	'time-max-text': begin
		stage_edit_update, pstate, /update_display
		end

	'origin-x-text': begin
		end

	'origin-y-text': begin
		end

	'origin-z-text': begin
		end

	'sample-text': begin
		end

	'grain-text': begin
		end

	'comment-text': begin
		end

	'check-button': begin
		stage_list_check_bounds, pm, p, ps, silent=0, error=error
		yellow = (error) ? 1 : 0
		widget_control, (*pstate).check_button, set_value={select:yellow}
		end

	'apply-button': begin
		n = n_elements( *plist)
		row = (*pstate).sel
		row = clip( row, 0, n-1) 
		*(*plist)[ row] = *p
		(*pstate).sel = row
		*(*pstate).pselect = (*pstate).sel
		notify, 'scan-list-update', from=event.top
		notify, 'scan-list-select', (*pstate).pselect, from=event.top
		end

	'append-button': begin
		if ptr_good( (*plist)[0]) then begin
			*plist = [*plist, ptr_new(*p)]
		endif else begin
			*plist = [ptr_new(*p)]
		endelse
		n = n_elements( *plist)
		(*(*plist)[n-1]).active = 0
		(*pstate).sel = n-1
		*(*pstate).pselect = (*pstate).sel
		notify, 'scan-list-update', from=event.top
		notify, 'scan-list-select', (*pstate).pselect, from=event.top
		end

	'insert-button': begin
		n = n_elements(*plist)
		i = clip((*pstate).sel,0,n-1)
		if n lt 1 then goto, finish
		if i ge 1 then begin
			plist1 = (*plist)[0:i-1]
		endif else begin
			plist1 = 0L
		endelse
		plist2 = (*plist)[i:n-1]
		if ptr_valid(plist1[0]) then begin
			pt = [plist1,ptr_new(*p),plist2]
		endif else begin
			pt = [ptr_new(*p),plist2]
		endelse
		*plist = pt
		(*pstate).sel = i
		*(*pstate).pselect = (*pstate).sel
		notify, 'scan-list-update', from=event.top
		notify, 'scan-list-select', (*pstate).pselect, from=event.top
		end

	'close-button': begin
		print,'Close fit results ...'
		goto, kill
		end
	else:
endcase

finish:
	widget_control, hourglass=0
	close, 1
	return

bad_state:
	warning,'stage_edit_event',['STATE variable has become ill-defined.','Abort Fit Results.'],/error
	goto, kill
bad_ptr:
	warning,'stage_edit_event',['Parameter structure variable has become ill-defined.','Abort Fit Results.'],/error
	goto, kill

kill:
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;------------------------------------------------------------------------------------------

pro stage_edit_position, pstate

; Get stage position

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
		warning,'stage_edit_position',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	plist = (*pstate).plist
	ps = (*pstate).ps
	pm = (*pstate).pm
	p = (*pstate).pitem

	v  = socket_command_get( ps, 'position', class='stage.axis', chip=-1, n_chips=3, /float, error=err)
	if err eq 0 then begin
		(*pstate).position.x = v[0]
		(*pstate).position.y = v[1]
		(*pstate).position.z = v[2]
	endif
	return
end

;------------------------------------------------------------------------------------------

pro stage_edit_update, pstate, update_display=update_display, from_panel=from_panel

; Optionally (if /from_panel), read text widgets to update struct values.
; Update (if /update_display) stage_edit widgets.

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
		warning,'stage_edit_update',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(update_display) eq 0 then update_display=0
if n_elements(from_panel) eq 0 then from_panel=0

	if ptr_valid( pstate) eq 0 then return
	if size( *pstate, /tname) ne 'STRUCT' then return
	plist = (*pstate).plist
	ps = (*pstate).ps
	pm = (*pstate).pm
	p = (*pstate).pitem
	overscans = (*pstate).overscans

if from_panel then begin

	widget_control, (*pstate).beam_text, get_value=s
	*(*pstate).pbeam = float2(s)
	widget_control, (*pstate).size_x_text, get_value=s
	(*p).raster.size.x = float2(s)
	widget_control, (*pstate).size_y_text, get_value=s
	(*p).raster.size.y = float2(s)
	widget_control, (*pstate).pixel_x_text, get_value=s
	(*p).raster.pixel.x = float2(s)
	widget_control, (*pstate).pixel_y_text, get_value=s
	(*p).raster.pixel.y = float2(s)
	widget_control, (*pstate).scale_x_text, get_value=s
	(*pm).deflect.scale.x = float2(s)
	widget_control, (*pstate).scale_y_text, get_value=s
	(*pm).deflect.scale.y = float2(s)
	widget_control, (*pstate).origin_x_text, get_value=s
	(*p).origin.x = float2(s)
	widget_control, (*pstate).origin_y_text, get_value=s
	(*p).origin.y = float2(s)
	widget_control, (*pstate).origin_z_text, get_value=s
	(*p).origin.z = float2(s)
	widget_control, (*pstate).charge_min_text, get_value=s
	(*p).raster.charge.min = (*pstate).charge_scale * float2(s)
	widget_control, (*pstate).charge_max_text, get_value=s
	(*p).raster.charge.max = (*pstate).charge_scale * float2(s)
	widget_control, (*pstate).photons_min_text, get_value=s
	(*p).raster.photons.min = float2(s)
	widget_control, (*pstate).photons_max_text, get_value=s
	(*p).raster.photons.max = float2(s)
	widget_control, (*pstate).time_min_text, get_value=s
	(*p).raster.time.min = (*pstate).dwell_scale * float2(s)
	widget_control, (*pstate).time_max_text, get_value=s
	(*p).raster.time.max = (*pstate).dwell_scale * float2(s)
	widget_control, (*pstate).sample_text, get_value=s
	(*p).sample = s
	widget_control, (*pstate).grain_text, get_value=s
	(*p).grain = s
	widget_control, (*pstate).comment_text, get_value=s
	(*p).comment = s
endif

if update_display then begin

;	Check any dwell entries against maximum velocity ...
	
	if (*p).raster.axis.x ge 2 then begin
		tvmin = (*p).raster.pixel.x / (*pm).stage.max.velocity[(*p).raster.axis.x - 2]
		if (*p).raster.time.min gt 0. then begin
			(*p).raster.time.min = (*p).raster.time.min > tvmin
		endif
		if (*p).raster.time.max gt 0. then begin
			(*p).raster.time.max = ((*p).raster.time.max > tvmin) > (*p).raster.time.min
		endif
	endif 
	
	nx = round( float((*p).raster.size.x) / float((*p).raster.pixel.x)) > 1
	ny = round( float((*p).raster.size.y) / float((*p).raster.pixel.y)) > 1

	time_tot = stage_list_time( p, *(*pstate).pbeam, suffix=suffix)
	stime = stage_list_time_string( time_tot) + '   ' + suffix

	widget_control, (*pstate).size_x_text, set_value=str_tidy( (*p).raster.size.x) 
	widget_control, (*pstate).size_y_text, set_value=str_tidy( (*p).raster.size.y) 
	widget_control, (*pstate).npixels_x_text, set_value=str_tidy( nx) 
	widget_control, (*pstate).npixels_y_text, set_value=str_tidy( ny) 

	widget_control, (*pstate).charge_min_text, set_value=str_tidy( (*p).raster.charge.min / (*pstate).charge_scale) 
	widget_control, (*pstate).charge_max_text, set_value=str_tidy( (*p).raster.charge.max / (*pstate).charge_scale) 
	widget_control, (*pstate).photons_min_text, set_value=str_tidy( (*p).raster.photons.min) 
	widget_control, (*pstate).photons_max_text, set_value=str_tidy( (*p).raster.photons.max) 
	widget_control, (*pstate).time_min_text, set_value=str_tidy( (*p).raster.time.min / (*pstate).dwell_scale) 
	widget_control, (*pstate).time_max_text, set_value=str_tidy( (*p).raster.time.max / (*pstate).dwell_scale) 

	widget_control, (*pstate).origin_x_text, set_value=str_tidy( (*p).origin.x) 
	widget_control, (*pstate).origin_y_text, set_value=str_tidy( (*p).origin.y) 
	widget_control, (*pstate).origin_z_text, set_value=str_tidy( (*p).origin.z) 

	widget_control, (*pstate).pixel_x_text, set_value=str_tidy( (*p).raster.pixel.x) 
	widget_control, (*pstate).pixel_y_text, set_value=str_tidy( (*p).raster.pixel.y) 

	widget_control, (*pstate).sample_text, set_value=str_tidy( (*p).sample) 
	widget_control, (*pstate).grain_text, set_value=str_tidy( (*p).grain) 
	widget_control, (*pstate).comment_text, set_value=str_tidy( (*p).comment) 
	widget_control, (*pstate).time_text, set_value=stime
	
	widget_control, (*pstate).axis_x_mode, set_combobox_select=(*p).raster.axis.x
	widget_control, (*pstate).axis_y_mode, set_combobox_select=(*p).raster.axis.y
	widget_control, (*pstate).shape_mode, set_combobox_select=(*p).raster.shape
	widget_control, (*pstate).fly_step_toggle, set_value=(*p).raster.step_scan
	q = where( (*p).raster.overscan le overscans, nq)
	if nq gt 0 then widget_control, (*pstate).overscan_mode, set_combobox_select=q[0]
	widget_control, (*pstate).interlace_check, set_value=[(*p).raster.interlace]

	v = socket_command_get( ps, 'scale', class='deflect.axis', chip=0, error=error)
	if error eq 0 then begin
		(*pm).deflect.scale.x = v[0]
	endif
	v = socket_command_get( ps, 'scale', class='deflect.axis', chip=1, error=error)
	if error eq 0 then begin
		(*pm).deflect.scale.y = v[0]
	endif
	widget_control, (*pstate).scale_x_text, set_value=str_tidy((*pm).deflect.scale.x)
	widget_control, (*pstate).scale_y_text, set_value=str_tidy((*pm).deflect.scale.y)
	widget_control, (*pstate).scale_x_text, sensitive=((*p).raster.axis.x eq 0)
	widget_control, (*pstate).scale_y_text, sensitive=((*p).raster.axis.y eq 1)
endif

	stage_list_check_bounds, pm, p, ps, /silent, error=error
	yellow = (error) ? 1 : 0
	widget_control, (*pstate).check_button, set_value={select:yellow}
	return
end

;------------------------------------------------------------------------------------------

pro stage_edit, group_leader=group, TLB=tlb, data=plist, row=rowi, path=path, $
				debug=debug, daq=daq_pars, port=daq_port, pbeam=pbeam

COMPILE_OPT STRICTARR

common c_working_dir, geopixe_root

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
		warning,'stage_edit',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''
if n_elements(debug) lt 1 then debug=0
if n_elements(rowi) lt 1 then rowi=0
if size(*plist,/tname) ne 'POINTER' then begin
	no_list = 1
endif else begin
	no_list = 0
	if ptr_valid( (*plist)[0] ) eq 0 then no_list=1
	if no_list eq 0 then if size(*(*plist)[0],/tname) ne 'STRUCT' then no_list=1
endelse
if no_list then begin
	*plist = [ptr_new(define(/scan_spec))]
	(*(*plist)[0]).active = 1							; set this one to 'OFF'
endif
if ptr_good(pbeam) eq 0 then pbeam=ptr_new(/allocate_heap)
row = rowi

n = n_elements( *plist)
print,'Stage-edit: row, n = ', row, n
row = clip( row, 0, n-1) 
pitem = (*plist)[ row]
overscans = [0, 2, 4, 6, 10, 20, 30, 50, 100, 200, 300, 500, 1000]			; must be even

charge_scale = 0.001			; display in fC, store in struct as pC.
charge_units = 'fC'
dwell_scale = 0.001				; display in ms, store in struct as s.
dwell_units = 'ms'

; DAQ control socket parameters struct

ps = bad_pars_struct( daq_port, make_pars=make_ps)
if make_ps then begin
	ps = ptr_new( define(/maia_port))
;	warning,'stage_edit','No open DAQ control socket.'
;	return
endif

; Default DAQ control parameters struct

pm = bad_pars_struct( daq_pars, make_pars=make_pm)
if make_pm then begin
	pm = ptr_new( define(daq_struct=36))
;	warning,'stage_edit','No DAQ control struct.'
;	return
endif

; Default detector layout struct parameters from file "DAQ_36.csv"

prefix = 'DAQ_36'
letters = ['','A','B','C','D','E','F','G','H','I','J','K','L','M']
prefix = prefix + letters[ (*pm).version.daq ]

case !version.os_family of
	'MacOS': begin
		yw = 220
		text_xsize = 90
		origin_xsize = 65
		sample_xsize = 160
		button_ysize = 27
		button_xsize = 60
		mode_xsize = 130
		help_xsize = 560
		end
	'unix': begin
		yw = 252
		text_xsize = 90
		origin_xsize = 65
		sample_xsize = 160
		button_ysize = 27
		button_xsize = 60
		mode_xsize = 130
		help_xsize = 560
		end
	else: begin
		yw = 219
		text_xsize = 90
		origin_xsize = 65
		sample_xsize = 160
		button_xsize = 60
		button_ysize = 23
		mode_xsize = 130
		help_xsize = 550
		end
endcase

tracking = 1					; later have context-sensitive help window
grey = spec_colour('l.grey')
green = spec_colour('green')
yellow = spec_colour('yellow')
lblue = spec_colour('l.blue')
red = spec_colour('red')
orange = spec_colour('orange')
dgreen = spec_colour('d.green')
blue = spec_colour('blue')
violet = spec_colour('violet')
pink = spec_colour('pink')
black = spec_colour('black')
white = spec_colour('white')

;	  select: 0     1      2      3
colours = [ grey, yellow, green, violet, violet]		; for ??

active = ['ON','OFF','START','STOP','REF1','REF2']
axes = ['DAC X','DAC Y','Stage X','Stage Y','Stage Z','Stage A']
shapes = ['Rectangular scan','Elliptical scan']

; 	top-level base

tlb = widget_base( /column, title='Scan Edit', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='stage_edit_TLB', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2, /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)

r1base = widget_base( tbase, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

; Left button stack

lbase = widget_base( r1base, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
label = widget_label( lbase, value='Set Extent')

button = state_button( lbase, value='Clear', uname='clear-button', tracking=tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Clears the extent flags in preparation for setting "left", '+ $
					'"right", "top" and "bottom" positions to set scan size and origin. These clear automatically once matching ' + $
					'(left-right, top-bottom or centre) are set.')

left_button = state_button( lbase, value='Left', uname='left-button', tracking=tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the left extent of the scan. "Yellow" indicates left-right pending. Complete by setting "right" position. ' + $
					'"Green" indicates left-right completed. Use "Clear" to start again.')

right_button = state_button( lbase, value='Right', uname='right-button', tracking=tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the right extent of the scan. "Yellow" indicates left-right pending. Complete by setting "left" position. ' + $
					'"Green" indicates left-right completed. Use "Clear" to start again.')

top_button = state_button( lbase, value='Top', uname='top-button', tracking=tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the top extent of the scan. "Yellow" indicates bottom-top pending. Complete by setting "bottom" position. ' + $
					'"Green" indicates bottom-top completed. Use "Clear" to start again.')

bottom_button = state_button( lbase, value='Bottom', uname='bottom-button', tracking=tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the bottom extent of the scan. "Yellow" indicates bottom-top pending. Complete by setting "top" position. ' + $
					'"Green" indicates bottom-top completed. Use "Clear" to start again.')

button = state_button( lbase, value='Bot-Left', uname='bottom-left-button', tracking=tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the bottom, left extent of the scan. "Yellow" indicates bottom, left pending. Complete by setting "top, right" positions. ' + $
					'"Green" indicates those completed. Use "Clear" to start again.')

button = state_button( lbase, value='Top-Right', uname='top-right-button', tracking=tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the top, right extent of the scan. "Yellow" indicates top, right pending. Complete by setting "bottom, left" positions. ' + $
					'"Green" indicates those completed. Use "Clear" to start again.')

button = state_button( lbase, value='Centre', uname='centre-button', tracking=tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the centre of the scan. Uses the present XY scan size and this centre to set the new stage origin. ' + $
					'"Green" indicates those completed. Use this with DAC XY scan to set scan around present stage XY position.')

button = state_button( lbase, value='Grab Z', uname='grab-z-button', tracking=tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the scan origin Z value to the current stage Z position.')

; Main base area

r2base = widget_base( r1base, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

;...................................................................................................
; Axes and Min/max

ambase = widget_base( r2base, /row, xpad=0, ypad=0, space=1, /base_align_top, /align_center)

; Axes

am1base = widget_base( ambase, /column, xpad=0, ypad=0, space=1, /base_align_right, /align_center)

am1base0 = widget_base( am1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am1base0, value='           X', scr_xsize=text_xsize)
label = widget_label( am1base0, value='           Y', scr_xsize=text_xsize)

am1base1 = widget_base( am1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am1base1, value='Axis:')
axis_x_mode = widget_combobox( am1base1, value=axes, uname='axis-x-mode', /tracking, scr_xsize=text_xsize, $
		;	notify_realize='OnRealize_stage_edit_x_mode', $
			uvalue='Select horizontal (X) image axis source mode. Select between DAC (X,Y) and Stage (X,Y,Z,A) axes. ')
axis_y_mode = widget_combobox( am1base1, value=axes, uname='axis-y-mode', /tracking, scr_xsize=text_xsize, $
		;	notify_realize='OnRealize_stage_edit_y_mode', $
			uvalue='Select horizontal (Y) image axis source mode. Select between DAC (X,Y) and Stage (X,Y,Z,A) axes.')

am1base2 = widget_base( am1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am1base2, value='Size:')
size_x_text = widget_text( am1base2, value='', uname='size-x-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter horizontal (X) image axis size (mm). Hit <return> to calculate #pixels and time.')
size_y_text = widget_text( am1base2, value='', uname='size-y-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter vertical (Y) image axis size (mm). Hit <return> to calculate #pixels and time.')

am1base3 = widget_base( am1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am1base3, value='# pixels:')
npixels_x_text = widget_text( am1base3, value='', uname='npixels-x-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter horizontal (X) image axis size in pixels. Hit <return> to calculate single pixel size. Note that X,Y pixel size should be constrained to be equal.')
npixels_y_text = widget_text( am1base3, value='', uname='npixels-y-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter vertical (Y) image axis size in pixels. Hit <return> to calculate single pixel size. Note that X,Y pixel size should be constrained to be equal')

am1base4 = widget_base( am1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am1base4, value='Pixel:')
pixel_x_text = widget_text( am1base4, value='', uname='pixel-x-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter or show the X pixel size (mm) per pixel. Pixel size should be constrained to be equal in X and Y. Hit <return> to calculate the # pixels in X.')
pixel_y_text = widget_text( am1base4, value='', uname='pixel-y-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter or show the Y pixel size (mm) per pixel. Pixel size should be constrained to be equal in X and Y. Hit <return> to calculate the # pixels in Y.')

am1base5 = widget_base( am1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am1base5, value='Scale:')
scale_x_text = widget_text( am1base5, value='', uname='scale-x-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter or show the X DAC scale (mm/V). Scan X size (when using DAC) should be constrained to be within 3.0*scale.')
scale_y_text = widget_text( am1base5, value='', uname='scale-y-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter or show the Y DAC scale (mm/V). Scan Y size (when using DAC) should be constrained to be within 3.0*scale.')

; Min/max

am2base = widget_base( ambase, /column, xpad=0, ypad=0, space=1, /base_align_top, /align_center)

am2base0 = widget_base( am2base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am2base0, value='          Min', scr_xsize=text_xsize)
label = widget_label( am2base0, value='          Max', scr_xsize=text_xsize)

am2base3 = widget_base( am2base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am2base3, value='Time:')
time_min_text = widget_text( am2base3, value='', uname='time-min-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter the minimum time ('+dwell_units+') to acquire each pixel. Set "0.0" to ignore min time. ' + $
			'All (non-zero) "Mins" must be satisfied for pixel advance.')
time_max_text = widget_text( am2base3, value='', uname='time-max-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter the maximum time ('+dwell_units+') to acquire each pixel to force pixel advance. Set "0.0" to ignore max time. ' + $
			'The least (non-zero) "Max" satisfied will trigger pixel advance.')

am2base1 = widget_base( am2base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am2base1, value='Charge:')
charge_min_text = widget_text( am2base1, value='', uname='charge-min-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter the minimum charge ('+charge_units+') to be acquired for each pixel. Set "0.0" to ignore min charge. ' + $
			'All (non-zero) "Mins" must be satisfied for pixel advance. Set a likely beam current to show a scan time estimate.', sensitive=0)
charge_max_text = widget_text( am2base1, value='', uname='charge-max-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter the maximum charge ('+charge_units+') to be acquired in a each pixel to force pixel advance. Set "0.0" to ignore max charge. ' + $
			'The least (non-zero) "Max" satisfied will trigger pixel advance. Set a likely beam current to show a scan time estimate.', sensitive=0)

am2base2 = widget_base( am2base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am2base2, value='Photons:')
photons_min_text = widget_text( am2base2, value='', uname='photons-min-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter the minimum number of photon events to be acquired for each pixel. Set "0" to ignore min photons. ' + $
			'All (non-zero) "Mins" must be satisfied for pixel advance.', sensitive=0)
photons_max_text = widget_text( am2base2, value='', uname='photons-max-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter the maximum number of photon events to be acquired in a each pixel to force pixel advance. Set "0" to ignore max photons. ' + $
			'The least (non-zero) "Max" satisfied will trigger pixel advance.', sensitive=0)

;am2base4 = widget_base( am2base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)
;label = widget_label( am2base4, value='Charge Units:')
;charge_units_mode = widget_combobox( am2base4, value=str_tidy(overscans), uname='charge-units-mode', /tracking, scr_xsize=text_xsize, $
;	;	notify_realize='OnRealize_stage_edit_charge_units_mode', $
;	uvalue='Select the units for the Charge counts integrated using the Counter input (FC0) to Hymod.', /align_right)

;...................................................................................................
; origin and sample/comment

osbase = widget_base( r2base, /row, xpad=0, ypad=0, space=1, /base_align_top, /align_center)

; Origin

os1base = widget_base( osbase, /column, xpad=0, ypad=0, space=1, /base_align_top, /align_center)

as1base0 = widget_base( os1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( as1base0, value='        X', scr_xsize=origin_xsize)
label = widget_label( as1base0, value='        Y', scr_xsize=origin_xsize)
label = widget_label( as1base0, value='        Z', scr_xsize=origin_xsize)

os1base1 = widget_base( os1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( os1base1, value='  Origin:')
origin_x_text = widget_text( os1base1, value='', uname='origin-x-text', /tracking, /editable, scr_xsize=origin_xsize, $
			uvalue='Enter or show the X stage origin (left extent) for the scan (mm). X is horizontal stage axis.')
origin_y_text = widget_text( os1base1, value='', uname='origin-y-text', /tracking, /editable, scr_xsize=origin_xsize, $
			uvalue='Enter or show the Y stage origin (bottom extent) for the scan (mm). Y is vertical stage axis. ')
origin_z_text = widget_text( os1base1, value='', uname='origin-z-text', /tracking, /editable, scr_xsize=origin_xsize, $
			uvalue='Enter or show the Z stage origin (focus) for the scan (mm). Z is the stage axis along the beam. ')

; sample/comment

os2base = widget_base( osbase, /column, xpad=0, ypad=0, space=1, /base_align_top, /align_center)

os2base1 = widget_base( os2base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( os2base1, value='Sample:')
sample_text = widget_text( os2base1, value='', uname='sample-text', /tracking, /editable, scr_xsize=sample_xsize, $
			uvalue='Enter the sample name for this scan. New scans will continue to inherit this sample name for the previous scan.')

os2base2 = widget_base( os2base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( os2base2, value='Grain:')
grain_text = widget_text( os2base2, value='', uname='grain-text', /tracking, /editable, scr_xsize=sample_xsize, $
			uvalue='Enter an optional "grain" name or point/scan identifier for this scan. New scans will continue to inherit this grain name for the previous scan.')

os2base3 = widget_base( os2base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( os2base3, value='Comment:')
comment_text = widget_text( os2base3, value='', uname='comment-text', /tracking, /editable, scr_xsize=sample_xsize, $
			uvalue='Enter an optional comment for this particular scan.')

;...................................................................................................
; shape, beam, time

dpbase = widget_base( r2base, /row, xpad=0, ypad=1, space=2, /base_align_center, /align_center)

dplbase = widget_base( dpbase, /column, xpad=1, ypad=0, space=2, /align_center, /base_align_center)

dpbase1 = widget_base( dplbase, /row, xpad=1, ypad=0, space=20, /align_center, /base_align_center)
dpbase1b = widget_base( dpbase1, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)
label = widget_label( dpbase1b, value='Shape:')
shape_mode = widget_combobox( dpbase1b, value=shapes, uname='shape-mode', /tracking, scr_xsize=mode_xsize, $
		;	notify_realize='OnRealize_stage_edit_shape_mode', $
			uvalue='Select scan shape mode: Rectangular raster, elliptical filled area, ... ' + $
				'Note that limited Klee resources may not permit a non-Rectangular raster.', /align_right)

fly_step_toggle = cw_bgroup2( dpbase1, ['Fly','Step'], /row, xpad=0, ypad=0, space=0, /exclusive, /no_release, $
					uname='fly-step-toggle', set_value=[0], /return_index, /tracking, $
					uvalue='Select the type of pixel advance between (i) "Fly" for fly-scan at constant velocity and time per pixel (use the Time Max setting), ' + $
					'and (ii) "Step" for step-scan with more options for adaptive pixel advance (can combine Time, Charge and/or Photon Min/Max settings).')

dpbase4 = widget_base( dplbase, /row, xpad=1, ypad=0, space=40, /align_center, /base_align_center)
dpbase4b = widget_base( dpbase4, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)
label = widget_label( dpbase4b, value='Overscan:')
overscan_mode = widget_combobox( dpbase4b, value=str_tidy(overscans), uname='overscan-mode', /tracking, scr_xsize=text_xsize, $
		;	notify_realize='OnRealize_stage_edit_overscan_mode', $
			uvalue='Select the number of "overscans". The entire frame is overscanned, unless one axis uses a DAC and one a stage, then the overscan is only along the fast axis.', /align_right)

interlace_check = cw_bgroup2( dpbase4, ['Interlaced Scan'], /row, set_value=[0], $
				/return_index, uname='interlace-check',/ nonexclusive, /tracking, $
				uvalue='Enable optional "interlace" mode, for ' + $
				'16 interlaced frames of interspersed spaced Y lines.', xpad=0, ypad=0, space=5)

dprbase = widget_base( dpbase, /column, xpad=1, ypad=0, space=2, /align_center, /base_align_right)

dpbase2 = widget_base( dprbase, /row, xpad=1, ypad=0, space=2, /align_right, /base_align_center)
label = widget_label( dpbase2, value='Beam:')
beam_text = widget_text( dpbase2, value='1.0', uname='beam-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter an estimate of beam current (nA). This is used with Charge min/max controls to estimate time. Hit <return> to calculate the total time.')

dpbase3 = widget_base( dprbase, /row, xpad=1, ypad=0, space=2, /align_right, /base_align_center)
label = widget_label( dpbase3, value='Total time:')
time_text = widget_text( dpbase3, value='', uname='time-text', /tracking, editable=0, scr_xsize=text_xsize, $
			uvalue='Shows the total time for the scan, estimated from beam current and min/max pixel time and charge (and the beam current for charge modes). ' + $
			'Time qualifier shows total Time determined by Time min/max "(T)" or Charge min/max "(Q)". ')

;...................................................................................................
; Buttons

bbase = widget_base( tbase, /row, /base_align_center, xpad = 0, ypad=0, space=30)

check_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
check_button = state_button( check_base, value='Check', uname='check-button', tracking=tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Check scan against stage and DAC bounds, etc. Yellow button indicates a problem to check.')

add_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
button = widget_button( add_base, value='Append', uname='append-button', tracking=tracking, $
					uvalue='Append the current settings in this edit window to the bottom of the scan list.')
button = widget_button( add_base, value='Insert', uname='insert-button', tracking=tracking, $
					uvalue='Insert the current settings in this edit window above the selected row in the scan list. Use this to duplicate (and edit) a scan list entry.')

apply_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
button = widget_button( apply_base, value='Apply', uname='apply-button', tracking=tracking, $
					uvalue='Apply the current changes to the selected row in the scan list.')

close_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
button = widget_button( close_base, value='Close', uname='close-button', tracking=tracking, $
					uvalue='Close the Edit window. The data remains stored in the parent DAQ Control window')

;...................................................................................................
; Help

help = widget_text( tbase, scr_xsize=help_xsize, ysize=3, /wrap, uname='help', tracking=tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

state = { $
		path:			ptr_new(path), $		; pointer to current path
		plist:			plist, $				; pointer to array of pointers to scan detail structs
		pitem:			ptr_new(*pitem), $		; pointer to local copy of present list row
		ps:				ps, $					; pointer to Klee port struct
		pm:				pm, $					; pointer to DAQ parameters struct
		pselect:		ptr_new(/allocate_heap), $	; pointer to select for notify to ??
		pbeam:			pbeam, $					; beam current estimate (nA)

		extent: {	left:	0, $				; flags setting extents
					right:	0, $				;
					top:	0, $				;
					bottom: 0}, $				;
		mark:	 {	left:	0.0, $				; mark the extent positions
					right:	0.0, $				;
					top:	0.0, $				;
					bottom: 0.0}, $				;

		position: {	x:		0.0, $				; present X stage position			
					y:		0.0, $				; present Y stage position
					z:		0.0}, $				; present Z stage position
		sel:			row, $					; row in list selected
;		beam:			1.0, $					; beam current estimate (nA)
		charge_units:	0.01, $					; charge units per counter count (pC)
		charge_scale:	charge_scale, $			; charge display units (pC)
		dwell_scale:	dwell_scale, $			; dwell time display units (s)
		overscans:		overscans, $			; overscan counts
		
		tracking:		tracking, $				; is tracking enabled
		axes:			axes, $					; axis list
		shapes:			shapes, $				; shapes list
		active:			active, $				; activation modes for items in list
		current_alert:	'', $					; current/last error alert mnessage


		left_button:	left_button, $			; left button ID
		right_button:	right_button, $			; right button ID
		top_button:		top_button, $			; top button ID
		bottom_button:	bottom_button, $		; bottom button ID
		axis_x_mode:	axis_x_mode, $			; X axis selection mode ID
		axis_y_mode:	axis_y_mode, $			; Y axis selection mode ID
		size_x_text:	size_x_text, $			; X axis size text ID
		size_y_text:	size_y_text, $			; Y axis size text ID
		npixels_x_text:	npixels_x_text, $		; X axis # pixels text ID
		npixels_y_text:	npixels_y_text, $		; Y axis # pixels text ID
		shape_mode:		shape_mode, $			; shape selection mode ID
		charge_min_text: charge_min_text, $		; charge min text ID
		charge_max_text: charge_max_text, $		; charge max text ID
		photons_min_text: photons_min_text, $	; photons min text ID
		photons_max_text: photons_max_text, $	; photons max text ID
		time_min_text: 	time_min_text, $		; time min text ID
		time_max_text: 	time_max_text, $		; time max text ID
;		charge_units_mode: charge_units_mode, $	; charge units droplist ID
		origin_x_text:	origin_x_text, $		; origin X text ID
		origin_y_text:	origin_y_text, $		; origin Y text ID
		origin_z_text:	origin_z_text, $		; origin Z text ID
		sample_text:	sample_text, $			; sample text ID
		grain_text:		grain_text, $			; grain text ID
		comment_text:	comment_text, $			; comment text ID
		pixel_x_text:	pixel_x_text, $			; pixel X size text ID
		pixel_y_text:	pixel_y_text, $			; pixel X size text ID
		scale_x_text:	scale_x_text, $			; DAC X scale widget text ID
		scale_y_text:	scale_y_text, $			; DAC Y scale widget text ID
		time_text:		time_text, $			; time text ID
		beam_text:		beam_text, $			; beam current ID
		fly_step_toggle: fly_step_toggle, $		; fly/scan toggle bgroup2 ID
		overscan_mode:	overscan_mode, $		; overscan droplist mode ID
		interlace_check: interlace_check, $		; interlace bgroup2 ID
		check_button:	check_button, $			; check bounds button ID
		
		help:			help $					; ID of help text
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

widget_control, left_button, timer=0.1				; set timer to do initial refresh of widgets

register_notify, tlb, ['path', $					; new path
					'scan-new', $					; a new scan details message
					'scan-edit-select' $			; scan list row select
					], from=group

xmanager, 'stage_edit', tlb, /no_block

return
end
