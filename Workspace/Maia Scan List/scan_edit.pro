
;	Scan Edit table.

pro scan_edit_event, event

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
		warning,'scan_edit_event',['IDL run-time error caught.', '', $
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
pindex = (*pstate).pindex
pframe = (*pstate).pframe				; frame details
psample = (*pstate).psample				; sample details
ps = (*pstate).ps
pm = (*pstate).pm
p = (*pstate).pitem
interlaces = [1, 2, 4, 8, 16, 32]
coords = ['finder','mapper1','mapper2','mapper1','mapper2']

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'scan-edit-select': begin							; update all edit fields for new row select
				(*pstate).sel = (*event.pointer).top
				if ptr_good( plist) eq 0 then goto, finish
				n = n_elements( *pindex)
				(*pstate).sel = clip( (*pstate).sel, 0, n-1) 
				j = (*pindex)[ (*pstate).sel]
				*p = *(*(*plist)[j]).pval
;				print,'Notify "scan-select", sel = ', (*pstate).sel, ' j=',j
				
				scan_edit_update, pstate, /update_display
				end
			'scan-edit-coords': begin
				(*pstate).coords_src = (*event.pointer)[0]
				(*pstate).coords_tgt = (*event.pointer)[1]
				scan_edit_update, pstate, /update_display
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
				widget_control, (*pstate).help, set_value=' '
			endelse
		endif
		goto, finish
		end
	'WIDGET_TIMER': begin
		u = widget_info( event.id, /uname)
		case u of
			'append-button': begin							; timer to do initial refresh of widgets
				scan_edit_update, pstate, /update_display
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
		print,'Kill request scan_edit ...'
		goto, kill
		end
	else:
endcase

scan_edit_update, pstate, /from_panel, origin_changed=changed
if changed then begin
	(*p).coordinates = coords[(*pstate).coords_src]
	scan_edit_update, pstate, /update_display
endif

uname = widget_info( event.id, /uname)
case uname of

	'scan_edit_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				x = event.x > 470
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
		scan_edit_update, pstate, /update_display
		end
		
	'left-button': begin
		scan_edit_position, pstate
		(*pstate).mark.left = (*pstate).position.x
		(*pstate).extent.left = ((*pstate).extent.right ge 1) ? 2 : 1
		widget_control, (*pstate).left_button, set_value={select:(*pstate).extent.left}
		if (*pstate).extent.left eq 2 then begin
			(*pstate).extent.right = 2
			widget_control, (*pstate).right_button, set_value={select:(*pstate).extent.right}
			
			(*p).origin.x = min([(*pstate).mark.left, (*pstate).mark.right])
			(*p).raster.size.x = abs((*pstate).mark.right - (*pstate).mark.left)
			scan_edit_update, pstate, /update_display
			widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		endif
		end
		
	'right-button': begin
		scan_edit_position, pstate
		(*pstate).mark.right = (*pstate).position.x
		(*pstate).extent.right = ((*pstate).extent.left ge 1) ? 2 : 1
		widget_control, (*pstate).right_button, set_value={select:(*pstate).extent.right}
		if (*pstate).extent.right eq 2 then begin
			(*pstate).extent.left = 2
			widget_control, (*pstate).left_button, set_value={select:(*pstate).extent.left}
			
			(*p).origin.x = min([(*pstate).mark.left, (*pstate).mark.right])
			(*p).raster.size.x = abs((*pstate).mark.right - (*pstate).mark.left)
			scan_edit_update, pstate, /update_display
			widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		endif
		end
		
	'top-button': begin
		scan_edit_position, pstate
		(*pstate).mark.top = (*pstate).position.y
		(*pstate).extent.top = ((*pstate).extent.bottom ge 1) ? 2 : 1
		widget_control, (*pstate).top_button, set_value={select:(*pstate).extent.top}
		if (*pstate).extent.top eq 2 then begin
			(*pstate).extent.bottom = 2
			widget_control, (*pstate).bottom_button, set_value={select:(*pstate).extent.bottom}
			
			(*p).origin.y = min([(*pstate).mark.bottom, (*pstate).mark.top])
			(*p).raster.size.y = abs((*pstate).mark.top - (*pstate).mark.bottom)
			scan_edit_update, pstate, /update_display
			widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		endif
		end
		
	'bottom-button': begin
		scan_edit_position, pstate
		(*pstate).mark.bottom = (*pstate).position.y
		(*pstate).extent.bottom = ((*pstate).extent.top ge 1) ? 2 : 1
		widget_control, (*pstate).bottom_button, set_value={select:(*pstate).extent.bottom}
		if (*pstate).extent.bottom eq 2 then begin
			(*pstate).extent.top = 2
			widget_control, (*pstate).top_button, set_value={select:(*pstate).extent.top}
			
			(*p).origin.y = min([(*pstate).mark.bottom, (*pstate).mark.top])
			(*p).raster.size.y = abs((*pstate).mark.top - (*pstate).mark.bottom)
			scan_edit_update, pstate, /update_display
			widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		endif
		end
		
	'bottom-left-button': begin
		scan_edit_position, pstate
		(*pstate).mark.bottom = (*pstate).position.y
		(*pstate).extent.bottom = ((*pstate).extent.top ge 1) ? 2 : 1
		widget_control, (*pstate).bottom_button, set_value={select:(*pstate).extent.bottom}
		if (*pstate).extent.bottom eq 2 then begin
			(*pstate).extent.top = 2
			widget_control, (*pstate).top_button, set_value={select:(*pstate).extent.top}
			
			(*p).origin.y = min([(*pstate).mark.bottom, (*pstate).mark.top])
			(*p).raster.size.y = abs((*pstate).mark.top - (*pstate).mark.bottom)
		endif
		(*pstate).mark.left = (*pstate).position.x
		(*pstate).extent.left = ((*pstate).extent.right ge 1) ? 2 : 1
		widget_control, (*pstate).left_button, set_value={select:(*pstate).extent.left}
		if (*pstate).extent.left eq 2 then begin
			(*pstate).extent.right = 2
			widget_control, (*pstate).right_button, set_value={select:(*pstate).extent.right}
			
			(*p).origin.x = min([(*pstate).mark.left, (*pstate).mark.right])
			(*p).raster.size.x = abs((*pstate).mark.right - (*pstate).mark.left)
		endif
		scan_edit_update, pstate, /update_display
		widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		end
		
	'top-right-button': begin
		scan_edit_position, pstate
		(*pstate).mark.top = (*pstate).position.y
		(*pstate).extent.top = ((*pstate).extent.bottom ge 1) ? 2 : 1
		widget_control, (*pstate).top_button, set_value={select:(*pstate).extent.top}
		if (*pstate).extent.top eq 2 then begin
			(*pstate).extent.bottom = 2
			widget_control, (*pstate).bottom_button, set_value={select:(*pstate).extent.bottom}
			
			(*p).origin.y = min([(*pstate).mark.bottom, (*pstate).mark.top])
			(*p).raster.size.y = abs((*pstate).mark.top - (*pstate).mark.bottom)
		endif
		(*pstate).mark.right = (*pstate).position.x
		(*pstate).extent.right = ((*pstate).extent.left ge 1) ? 2 : 1
		widget_control, (*pstate).right_button, set_value={select:(*pstate).extent.right}
		if (*pstate).extent.right eq 2 then begin
			(*pstate).extent.left = 2
			widget_control, (*pstate).left_button, set_value={select:(*pstate).extent.left}
			
			(*p).origin.x = min([(*pstate).mark.left, (*pstate).mark.right])
			(*p).raster.size.x = abs((*pstate).mark.right - (*pstate).mark.left)
		endif
		scan_edit_update, pstate, /update_display
		widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		end
		
	'centre-button': begin
		scan_edit_position, pstate
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
		(*p).origin.x = min([(*pstate).mark.left, (*pstate).mark.right])
		(*p).raster.size.x = abs((*pstate).mark.right - (*pstate).mark.left)
		(*p).origin.y = min([(*pstate).mark.bottom, (*pstate).mark.top])
		(*p).raster.size.y = abs((*pstate).mark.top - (*pstate).mark.bottom)
		scan_edit_update, pstate, /update_display
		widget_control, (*pstate).right_button, timer=2.		; book a timer to clear the green buttons
		end
	
	'ref1-finder-button': begin
		(*pframe).finder.ref1 = (*pm).finder.position
		scan_edit_update, pstate, /update_display
		end
	'ref2-finder-button': begin
		(*pframe).finder.ref2 = (*pm).finder.position
		scan_edit_update, pstate, /update_display
		end

	'ref1-finder-x-text': begin
		widget_control, event.id, get_value=s
		(*pframe).finder.ref1.x = float2(s)
		end
	'ref1-finder-y-text': begin
		widget_control, event.id, get_value=s
		(*pframe).finder.ref1.y = float2(s)
		end
	'ref1-finder-z-text': begin
		widget_control, event.id, get_value=s
		(*pframe).finder.ref1.z = float2(s)
		end

	'ref2-finder-x-text': begin
		widget_control, event.id, get_value=s
		(*pframe).finder.ref2.x = float2(s)
		end
	'ref2-finder-y-text': begin
		widget_control, event.id, get_value=s
		(*pframe).finder.ref2.y = float2(s)
		end
	'ref2-finder-z-text': begin
		widget_control, event.id, get_value=s
		(*pframe).finder.ref2.z = float2(s)
		end

;	Here we use 'coords_tgt' to select between GeoPIXE coords, assuming an image
;	acquired on that endstation.

	'ref1-mapper-button': begin
		if ((*pstate).coords_tgt eq 0) then begin 
			(*pframe).mapper1.ref1 = (*pm).geopixe1.position	
			(*pframe).mapper1.ref1.z = (*pm).mapper1.position.z
		endif else if ((*pstate).coords_tgt eq 1) then begin 
			(*pframe).mapper2.ref1 = (*pm).geopixe2.position	
			(*pframe).mapper2.ref1.z = (*pm).mapper2.position.z
		endif
		scan_edit_update, pstate, /update_display
		end
	'ref2-mapper-button': begin
		if ((*pstate).coords_tgt eq 0) then begin 
			(*pframe).mapper1.ref2 = (*pm).geopixe1.position	
			(*pframe).mapper1.ref2.z = (*pm).mapper1.position.z
		endif else if ((*pstate).coords_tgt eq 1) then begin 
			(*pframe).mapper2.ref2 = (*pm).geopixe2.position	
			(*pframe).mapper2.ref2.z = (*pm).mapper2.position.z
		endif
		scan_edit_update, pstate, /update_display
		end

	'ref1-mapper-x-text': begin
		widget_control, event.id, get_value=s
		if ((*pstate).coords_tgt eq 0) then begin 
			(*pframe).mapper1.ref1.x = float2(s)
		endif else begin
			(*pframe).mapper2.ref1.x = float2(s)
		endelse
		end
	'ref1-mapper-y-text': begin
		widget_control, event.id, get_value=s
		if ((*pstate).coords_tgt eq 0) then begin 
			(*pframe).mapper1.ref1.y = float2(s)
		endif else begin
			(*pframe).mapper2.ref1.y = float2(s)
		endelse
		end
	'ref1-mapper-z-text': begin
		widget_control, event.id, get_value=s
		if ((*pstate).coords_tgt eq 0) then begin 
			(*pframe).mapper1.ref1.z = float2(s)
		endif else begin
			(*pframe).mapper2.ref1.z = float2(s)
		endelse
		end

	'ref2-mapper-x-text': begin
		widget_control, event.id, get_value=s
		if ((*pstate).coords_tgt eq 0) then begin 
			(*pframe).mapper1.ref2.x = float2(s)
		endif else begin
			(*pframe).mapper2.ref2.x = float2(s)
		endelse
		end
	'ref2-mapper-y-text': begin
		widget_control, event.id, get_value=s
		if ((*pstate).coords_tgt eq 0) then begin 
			(*pframe).mapper1.ref2.y = float2(s)
		endif else begin
			(*pframe).mapper2.ref2.y = float2(s)
		endelse
		end
	'ref2-mapper-z-text': begin
		widget_control, event.id, get_value=s
		if ((*pstate).coords_tgt eq 0) then begin 
			(*pframe).mapper1.ref2.z = float2(s)
		endif else begin
			(*pframe).mapper2.ref2.z = float2(s)
		endelse
		end

	'ref1-sample-button': begin
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref1 = (*pm).current.position
			(*psample)[q[0]].coordinates = coords[(*pstate).coords_src]
		endif
		scan_edit_update, pstate, /update_display
		end
	'ref2-sample-button': begin
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref2 = (*pm).current.position
			(*psample)[q[0]].coordinates = coords[(*pstate).coords_src]
		endif
		scan_edit_update, pstate, /update_display
		end

	'ref1-sample-x-text': begin
		widget_control, event.id, get_value=s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref1.x = float2(s)
		endif
		end
	'ref1-sample-y-text': begin
		widget_control, event.id, get_value=s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref1.y = float2(s)
		endif
		end
	'ref1-sample-z-text': begin
		widget_control, event.id, get_value=s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref1.z = float2(s)
		endif
		end

	'ref2-sample-x-text': begin
		widget_control, event.id, get_value=s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref2.x = float2(s)
		endif
		end
	'ref2-sample-y-text': begin
		widget_control, event.id, get_value=s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref2.y = float2(s)
		endif
		end
	'ref2-sample-z-text': begin
		widget_control, event.id, get_value=s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref2.z = float2(s)
		endif
		end

	'get-ref-mode': begin
		(*pstate).get_ref = event.index
		end

	'get-ref-button': begin
		scan_edit_get_reference, pstate
		scan_edit_update, pstate, /update_display
		end

	'sample-coords-mode': begin
;		widget_control, event.id, set_combobox_select = (*pstate).sample_coords 
		(*pstate).sample_coords = event.index
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].coordinates = coords[event.index]
		endif
		end
	'scan-coords-mode': begin
;		widget_control, event.id, set_combobox_select = (*pstate).scan_coords 
		(*pstate).scan_coords = event.index
		(*p).coordinates = coords[event.index]
		end

	'sample-name-text': begin
		scan_edit_new_sample, pstate, /search
		scan_edit_update, pstate, /update_display
		end

	'sample-query-button': begin
		scan_edit_new_sample, pstate, /all, /search
		scan_edit_update, pstate, /update_display
		end

	'sample-igsn-text': begin
		scan_edit_new_sample, pstate, /search, /local_igsn
		scan_edit_update, pstate, /update_display
		end

	'project-name-text': begin
;		scan_edit_new_project, pstate
;		scan_edit_update, pstate, /update_display
		end

	'project-query-button': begin
		scan_edit_new_project, pstate
		scan_edit_update, pstate, /update_display
		end

	'size-x-text': begin
		scan_edit_update, pstate, /update_display
		end

	'size-y-text': begin
		scan_edit_update, pstate, /update_display
		end

	'npixels-x-text': begin
		widget_control, (*pstate).npixels_x_text, get_value=s
		nx = round(float2(s))
		(*p).raster.pixel.x = (*p).raster.size.x / nx
		scan_edit_update, pstate, /update_display
		end

	'npixels-y-text': begin
		widget_control, (*pstate).npixels_y_text, get_value=s
		ny = round(float2(s))
		(*p).raster.pixel.y = (*p).raster.size.y / ny
		scan_edit_update, pstate, /update_display
		end

	'pixel-x-text': begin
		scan_edit_update, pstate, /update_display
		end

	'pixel-y-text': begin
		scan_edit_update, pstate, /update_display
		end
		
	'interlace-mode': begin
		(*p).raster.interlace = (*pstate).enable_interlace ? interlaces[ event.index < (n_elements(interlaces)-1)] : 1
		scan_edit_update, pstate, /update_display
		end

	'dwell-text': begin
		scan_edit_update, pstate, /update_display
		end

	'velocity-text': begin
		widget_control, (*pstate).velocity_text, get_value=s
		v = float2(s)
		(*p).raster.dwell = 1000. * (*p).raster.pixel.x / v
		scan_edit_update, pstate, /update_display
		end

	'origin-x-text': begin
		end

	'origin-y-text': begin
		end

	'origin-z-text': begin
		end

	'set-origin-button': begin
		text = ['X origin (ruler)','Y origin (ruler)']
		initial_text = ['','']
		help_text = ['Enter the X origin coordinates (mm) as measured from the aluminium target (left edge of target) at the left end of the sample frame (as viewed looking at the face of samples).', $
					'Enter the Y origin coordinates (mm) as measured up from the aluminium base-plate (top surface).']
		Help_default = 'Enter the X,Y origin coordinates (mm) as measured from the aluminium target (left edge) and the aluminium base-plate (top surface), as viewed looking at the face of samples.'
		r = options_popup( title='Origin as measured using ruler', text=text, initial_text=initial_text, help_text=help_text, $
				help_default=help_default, error=error)
		if error then return

		(*p).origin.X = float2(r.text[0]) + 4.07
		(*p).origin.Y = float2(r.text[1]) - 6.68
		scan_edit_update, pstate, /update_display, silent=0
		end

	'sample-text': begin
		end

	'grain-text': begin
		end

	'comment-text': begin
		end

	'check-button': begin
		scan_list_check_bounds, pm, p, ps, silent=0, error=error
		yellow = (error) ? 1 : 0
		widget_control, (*pstate).check_button, set_value={select:yellow}
		end

	'capture-button': begin
		(*p).origin = (*pm).current.newscan.origin
		(*p).raster.size = (*pm).current.newscan.size
		(*p).Zsurface = (*pm).current.newscan.Zsurface
		(*p).coordinates = coords[(*pstate).coords_src]
		scan_edit_update, pstate, /update_display
		end

	'apply-button': begin
		if no_list then begin
			warning,'',['No valid scans in Scan List table.','Use "Append" to add a new one.']
			goto, finish
		endif
		scan_edit_read, pstate

		n = n_elements( *plist)
		row = (*pstate).sel
		row = clip( row, 0, n-1) 
		j = (*pindex)[ row]
		pitem = (*(*plist)[j]).pval
		*pitem = *p
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq eq 0 then begin
			if (n_elements(*psample) le 1) and ((*psample)[0].sample eq '') then begin
				*psample = define(/maia_sample_spec)
				(*psample).sample = (*p).sample
				(*psample).coordinates = coords[(*pstate).coords_src]
			endif else begin
				ts = define(/maia_sample_spec)
				ts.sample = (*p).sample
				ts.coordinates = coords[(*pstate).coords_src]
				*psample = [ *psample, ts]
			endelse
		endif
		(*pstate).sel = row
		*(*pstate).pselect = (*pstate).sel
		notify, 'scan-list-update', from=event.top
		notify, 'scan-list-select', (*pstate).pselect, from=event.top
		end

	'append-button': begin
		scan_edit_read, pstate
		pt = ptr_new( {on:1, key:'', pval:ptr_new()} )				; append new scan to list
		(*p).active = 0
		if changed then (*p).coordinates = coords[(*pstate).coords_src]
		(*pt).pval = ptr_new( *p)
		if ptr_good( (*plist)[0]) then begin
			*plist = [*plist, pt]
			n = n_elements( *plist)
			*pindex = [*pindex, n-1]
		endif else begin
			*plist = [pt]
			*pindex = [0]
		endelse

		(*pstate).sel = n_elements(*pindex)-1
		*(*pstate).pselect = (*pstate).sel
		notify, 'scan-list-update', from=event.top
		notify, 'scan-list-select', (*pstate).pselect, from=event.top
		end

	'insert-button': begin
		if no_list then begin
			warning,'',['No valid scans in Scan List table.','Use "Append" to add a new one.']
			goto, finish
		endif
		scan_edit_read, pstate
		pt = ptr_new( {on:1, key:'', pval:ptr_new()} )				; insert new scan in list
		(*p).active = 0
		if changed then (*p).coordinates = coords[(*pstate).coords_src]
		(*pt).pval = ptr_new( *p)
		if ptr_good( (*plist)[0]) then begin
			*plist = [*plist, pt]
		endif else begin
			*plist = [pt]
		endelse
		n = n_elements( *plist)

		ni = n_elements(*pindex)									; insert index to scan into index
		i = clip((*pstate).sel,0,ni-1)
		if (*pindex)[0] eq -1 then ni=0
		if (ni lt 1) then i=0
		if i ge 1 then begin
			i1 = (*pindex)[0:i-1]
		endif else begin
			i1 = -1
		endelse
		if (n gt i) then begin
			i2 = (*pindex)[i:ni-1]
		endif else begin
			i2 = -1
		endelse
		if i1[0] ge 0 then begin
			i3 = [i1,(n-1)]
		endif else begin
			i3 = [(n-1)]
		endelse
		if i2[0] ge 0 then begin
			i3 = [i3,i2]
		endif
		*pindex = i3

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
	warning,'scan_edit_event',['STATE variable has become ill-defined.','Abort Fit Results.'],/error
	goto, kill
bad_ptr:
	warning,'scan_edit_event',['Parameter structure variable has become ill-defined.','Abort Fit Results.'],/error
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

pro scan_edit_get_reference, pstate

; Get a selected reference default to set the reference struct for current frame

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
		warning,'scan_edit_get_reference',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
	
	ref = get_default_reference( (*pstate).kvs, (*pstate).kvs_prefix, finder=(*pstate).get_ref, error=error)
	if error then begin
		warning,'scan_edit_get_reference','Failed to get default reference.'
		return
	endif

	pframe = (*pstate).pframe				; frame details
	*pframe = ref

	return
end

;------------------------------------------------------------------------------------------

pro scan_edit_position, pstate

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
		warning,'scan_edit_position',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	pm = (*pstate).pm

	(*pstate).position = (*pm).current.position
	(*pstate).newscan = (*pm).current.newscan
	return
end

;------------------------------------------------------------------------------------------

pro scan_edit_read, pstate, origin_changed=changed

; Read all text widgets before Apply

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
		warning,'scan_edit_read',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	plist = (*pstate).plist					; sample list
	pindex = (*pstate).pindex				; index of those not deleted
	pframe = (*pstate).pframe				; frame details
	psample = (*pstate).psample				; sample details
	ps = (*pstate).ps						; Maia port
	pm = (*pstate).pm						; local variables
	p = (*pstate).pitem						; current scan edit item
	changed = 0								; flags change in origin

	widget_control, (*pstate).ref1_finder_x_text, get_value=s
	(*pframe).finder.ref1.x = float2(s)
	widget_control, (*pstate).ref1_finder_y_text, get_value=s
	(*pframe).finder.ref1.y = float2(s)
	widget_control, (*pstate).ref1_finder_z_text, get_value=s
	(*pframe).finder.ref1.z = float2(s)

	widget_control, (*pstate).ref2_finder_x_text, get_value=s
	(*pframe).finder.ref2.x = float2(s)
	widget_control, (*pstate).ref2_finder_y_text, get_value=s
	(*pframe).finder.ref2.y = float2(s)
	widget_control, (*pstate).ref2_finder_z_text, get_value=s
	(*pframe).finder.ref2.z = float2(s)

	widget_control, (*pstate).ref1_mapper_x_text, get_value=s
	if ((*pstate).coords_tgt eq 0) then begin 
		(*pframe).mapper1.ref1.x = float2(s)
	endif else begin
		(*pframe).mapper2.ref1.x = float2(s)
	endelse
	widget_control, (*pstate).ref1_mapper_y_text, get_value=s
	if ((*pstate).coords_tgt eq 0) then begin 
		(*pframe).mapper1.ref1.y = float2(s)
	endif else begin
		(*pframe).mapper2.ref1.y = float2(s)
	endelse
	widget_control, (*pstate).ref1_mapper_z_text, get_value=s
	if ((*pstate).coords_tgt eq 0) then begin 
		(*pframe).mapper1.ref1.z = float2(s)
	endif else begin
		(*pframe).mapper2.ref1.z = float2(s)
	endelse

	widget_control, (*pstate).ref2_mapper_x_text, get_value=s
	if ((*pstate).coords_tgt eq 0) then begin 
		(*pframe).mapper1.ref2.x = float2(s)
	endif else begin
		(*pframe).mapper2.ref2.x = float2(s)
	endelse
	widget_control, (*pstate).ref2_mapper_y_text, get_value=s
	if ((*pstate).coords_tgt eq 0) then begin 
		(*pframe).mapper1.ref2.y = float2(s)
	endif else begin
		(*pframe).mapper2.ref2.y = float2(s)
	endelse
	widget_control, (*pstate).ref2_mapper_z_text, get_value=s
	if ((*pstate).coords_tgt eq 0) then begin 
		(*pframe).mapper1.ref2.z = float2(s)
	endif else begin
		(*pframe).mapper2.ref2.z = float2(s)
	endelse

	if widget_info( (*pstate).ref1_x_text, /valid) then begin
		widget_control, (*pstate).ref1_x_text, get_value=s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref1.x = float2(s)
		endif
	endif
	if widget_info( (*pstate).ref1_y_text, /valid) then begin
		widget_control, (*pstate).ref1_y_text, get_value=s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref1.y = float2(s)
		endif
	endif
	if widget_info( (*pstate).ref1_z_text, /valid) then begin
		widget_control, (*pstate).ref1_z_text, get_value=s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref1.z = float2(s)
		endif
	endif

	if widget_info( (*pstate).ref2_x_text, /valid) then begin
		widget_control, (*pstate).ref2_x_text, get_value=s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref2.x = float2(s)
		endif
	endif
	if widget_info( (*pstate).ref2_y_text, /valid) then begin
		widget_control, (*pstate).ref2_y_text, get_value=s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref2.y = float2(s)
		endif
	endif
	if widget_info( (*pstate).ref2_z_text, /valid) then begin
		widget_control, (*pstate).ref2_z_text, get_value=s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq gt 0 then begin
			(*psample)[q[0]].reference.ref2.z = float2(s)
		endif
	endif

	scan_edit_new_sample, pstate, /local_igsn

	widget_control, (*pstate).size_x_text, get_value=s
	(*p).raster.size.x = float2(s)
	widget_control, (*pstate).size_y_text, get_value=s
	(*p).raster.size.y = float2(s)
	widget_control, (*pstate).pixel_x_text, get_value=s
	(*p).raster.pixel.x = float2(s)
	widget_control, (*pstate).pixel_y_text, get_value=s
	(*p).raster.pixel.y = float2(s)
	widget_control, (*pstate).origin_x_text, get_value=s
	t = float2(s)
	if abs( t - (*p).origin.x) gt 0.001 then changed=1
	(*p).origin.x = t
	widget_control, (*pstate).origin_y_text, get_value=s
	t = float2(s)
	if abs( t - (*p).origin.y) gt 0.001 then changed=1
	(*p).origin.y = t
	widget_control, (*pstate).origin_z_text, get_value=s
	(*p).origin.z = float2(s)
	widget_control, (*pstate).dwell_text, get_value=s	
	(*p).raster.dwell = float2(s)
	widget_control, (*pstate).grain_text, get_value=s
	(*p).region = s
	widget_control, (*pstate).comment_text, get_value=s
	(*p).comment = s

	return
end

;------------------------------------------------------------------------------------------

pro scan_edit_new_project, pstate

; Test the project string to see if it is a new project
; If 'select' passed, search for these ...

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
		warning,'scan_edit_new_project',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
coords = ['finder','mapper1','mapper2','mapper1','mapper2']

	plist = (*pstate).plist					; sample list
	pindex = (*pstate).pindex				; index of those not deleted
	pframe = (*pstate).pframe				; frame details
	psample = (*pstate).psample				; sample details
	ps = (*pstate).ps						; Maia port
	pm = (*pstate).pm						; local variables
	p = (*pstate).pitem						; current scan edit item

	widget_control, (*pstate).project_text, get_value=s0
	s0 = strcompress( s0, /remove_all)

;	Get current project list from KVS ...

	list = get_kvs( (*pstate).kvs, 'MM.project.list', /list, error=err)
	nl = n_elements(list)
	project_list = strarr(nl)
	for i=0,nl-1 do project_list[i] = strmid( string(list[i]), 8)

;	See if any new projects found in directory, add these to KVS ...

	if (*pstate).prefs.path.projects ne '' then begin
		f = strip_path(file_search( fix_path((*pstate).prefs.path.projects) +'*', /test_directory)) 
		nf = n_elements(f)
		for i=0,nf-1 do begin
			if f[i] ne '' then begin
				q = where( strlowcase(f[i]) eq strlowcase(project_list), nq)
				if (nq eq 0) then begin
					print, "Append to MM.project.list: project."+f[i]
					append_kvs, (*pstate).kvs, 'MM.project.list', 'project.'+f[i], error=err
					if err then print,'Scan_Edit: failed to append project "'+f[i]+'" to KVS.'
					project_list = [project_list, F[i]]
				endif
			endif
		endfor
	endif
	q = where( project_list ne '', nq)
	if nq ge 1 then project_list = project_list[q]

	s = list_popup( title='Select Project or Define New One', list=project_list, initial=s0, /sort)
	q = where( strlowcase(s) eq strlowcase(project_list), nq)
	if (nq eq 0) and (s ne '') then begin
		append_kvs, (*pstate).kvs, 'MM.project.list', 'project.'+s, error=err
	endif
;	if s ne '' then begin
		(*p).project = s
;	endif
	return
end

;------------------------------------------------------------------------------------------

pro scan_edit_new_sample, pstate, all=all, search=ssearch, local_igsn=do_igsn

; Test the sample string to see if it is a new sample
; If 'select' passed, search for these ...

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
		warning,'scan_edit_new_sample',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
coords = ['finder','mapper1','mapper2','mapper1','mapper2']

	if n_elements(ssearch) eq 0 then ssearch=0
	if n_elements(all) eq 0 then all=0
	if n_elements(do_igsn) eq 0 then do_igsn=0

	plist = (*pstate).plist					; sample list
	pindex = (*pstate).pindex				; index of those not deleted
	pframe = (*pstate).pframe				; frame details
	psample = (*pstate).psample				; sample details
	ps = (*pstate).ps						; Maia port
	pm = (*pstate).pm						; local variables
	p = (*pstate).pitem						; current scan edit item

	widget_control, (*pstate).igsn_text, get_value=igsn
	use_igsn = 0
	widget_control, (*pstate).sample_text, get_value=s
	s = strcompress( s, /remove_all)

	list = get_kvs( (*pstate).kvs, 'MM.sample.list', /list, error=err)
	nl = n_elements(list)
	sample_list = strarr(nl)
	for i=0,nl-1 do sample_list[i] = strmid( string(list[i]), 7)
	template3 = define(/maia_sample_spec)

	if ssearch then begin
		if all then begin
			q = indgen(nl)
			nq = nl
		endif else begin
			s2 = strlowcase(s[0])
			ns = strlen(s2)
			q = where( s2 eq strmid(strlowcase(sample_list),0,ns), nq)
		endelse
	endif else begin
		q = where( (strlowcase(s))[0] eq strlowcase(sample_list), nq)
	endelse
	if (nq gt 0) then begin
		list = sample_list[q]
	endif else list=sample_list
	if (nq eq 0) or ssearch then begin
		s = list_popup( title='Select Sample or Define New One', list=list, initial=s, /sort)
		q = where( strlowcase(s) eq strlowcase(sample_list), nq)
		if (nq eq 0) and (s ne '') then begin
			append_kvs, (*pstate).kvs, 'MM.sample.list', 'sample.'+s, error=err
			use_igsn = 1
		endif
	endif
	if s ne '' then begin
		(*p).sample = s
		q = where( (*psample).sample eq (*p).sample, nq)
		if nq eq 0 then begin
			if (n_elements(*psample) le 1) and ((*psample)[0].sample eq '') then begin
				*psample = define(/maia_sample_spec)
				(*psample).sample = (*p).sample
				if use_igsn then (*psample).igsn = igsn
				(*psample).coordinates = coords[(*pstate).coords_src]
			endif else begin
				ts = define(/maia_sample_spec)
				ts.sample = (*p).sample
				if use_igsn then ts.igsn = igsn
				ts.coordinates = coords[(*pstate).coords_src]
				*psample = [ *psample, ts]
			endelse
		endif else begin
			if do_igsn then begin
				(*psample)[q[0]].igsn = igsn
			endif else begin
				l = locate('.scan.', (*(*plist)[0]).key)
				if l ge 0 then begin
					key =  strmid( (*(*plist)[0]).key, 0, l) + '.sample.'+(*p).sample
					s = get_kvs( (*pstate).kvs, key, template=template3, error=err)
					if err eq 0 then (*psample)[q[0]].igsn = s.igsn
				endif
			endelse
		endelse
	endif
	return
end

;------------------------------------------------------------------------------------------

pro scan_edit_update, pstate, update_display=update_display, from_panel=from_panel, $
				silent=silent, origin_changed=changed

; Optionally (if /from_panel), read text widgets to update struct values.
; Update (if /update_display) scan_edit widgets.

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
		warning,'scan_edit_update',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(update_display) eq 0 then update_display=0
if n_elements(from_panel) eq 0 then from_panel=0
if n_elements(silent) eq 0 then silent=1
changed = 0											; flags change in origin

	if ptr_valid( pstate) eq 0 then return
	if size( *pstate, /tname) ne 'STRUCT' then return
	plist = (*pstate).plist
	ps = (*pstate).ps
	pm = (*pstate).pm
	p = (*pstate).pitem
	interlaces = [1, 2, 4, 8, 16, 32]
	pframe = (*pstate).pframe				; frame details
	psample = (*pstate).psample				; sample details
	coords = ['finder','mapper1','mapper2','mapper1','mapper2']

if from_panel then begin

	widget_control, (*pstate).size_x_text, get_value=s
	(*p).raster.size.x = float2(s)
	widget_control, (*pstate).size_y_text, get_value=s
	(*p).raster.size.y = float2(s)
	widget_control, (*pstate).pixel_x_text, get_value=s
	(*p).raster.pixel.x = float2(s)
	widget_control, (*pstate).pixel_y_text, get_value=s
	(*p).raster.pixel.y = float2(s)
	widget_control, (*pstate).origin_x_text, get_value=s
	t = float2(s)
	if abs( t - (*p).origin.x) gt 0.001 then changed=1
	(*p).origin.x = t
	widget_control, (*pstate).origin_y_text, get_value=s
	t = float2(s)
	if abs( t - (*p).origin.y) gt 0.001 then changed=1
	(*p).origin.y = t
	widget_control, (*pstate).origin_z_text, get_value=s
	(*p).origin.z = float2(s)
	widget_control, (*pstate).dwell_text, get_value=s	
	(*p).raster.dwell = float2(s)
	widget_control, (*pstate).sample_text, get_value=s
	(*p).sample = s
	widget_control, (*pstate).grain_text, get_value=s
	(*p).region = s
	widget_control, (*pstate).comment_text, get_value=s
	(*p).comment = s
	widget_control, (*pstate).project_text, get_value=s
	(*p).project = s
endif

if update_display then begin

;	Frame

	widget_control, (*pstate).ref1_finder_x_text, set_value=str_tidy( (*pframe).finder.ref1.x) 
	widget_control, (*pstate).ref1_finder_y_text, set_value=str_tidy( (*pframe).finder.ref1.y) 
	widget_control, (*pstate).ref1_finder_z_text, set_value=str_tidy( (*pframe).finder.ref1.z) 
	widget_control, (*pstate).ref2_finder_x_text, set_value=str_tidy( (*pframe).finder.ref2.x) 
	widget_control, (*pstate).ref2_finder_y_text, set_value=str_tidy( (*pframe).finder.ref2.y) 
	widget_control, (*pstate).ref2_finder_z_text, set_value=str_tidy( (*pframe).finder.ref2.z) 

	widget_control, (*pstate).ref1_mapper_x_text, set_value=str_tidy( ((*pstate).coords_tgt eq 0) ? (*pframe).mapper1.ref1.x : (*pframe).mapper2.ref1.x) 
	widget_control, (*pstate).ref1_mapper_y_text, set_value=str_tidy( ((*pstate).coords_tgt eq 0) ? (*pframe).mapper1.ref1.y : (*pframe).mapper2.ref1.y) 
	widget_control, (*pstate).ref1_mapper_z_text, set_value=str_tidy( ((*pstate).coords_tgt eq 0) ? (*pframe).mapper1.ref1.z : (*pframe).mapper2.ref1.z) 
	widget_control, (*pstate).ref2_mapper_x_text, set_value=str_tidy( ((*pstate).coords_tgt eq 0) ? (*pframe).mapper1.ref2.x : (*pframe).mapper2.ref2.x) 
	widget_control, (*pstate).ref2_mapper_y_text, set_value=str_tidy( ((*pstate).coords_tgt eq 0) ? (*pframe).mapper1.ref2.y : (*pframe).mapper2.ref2.y) 
	widget_control, (*pstate).ref2_mapper_z_text, set_value=str_tidy( ((*pstate).coords_tgt eq 0) ? (*pframe).mapper1.ref2.z : (*pframe).mapper2.ref2.z) 

;	Sample

	sample = (*p).sample
	widget_control, (*pstate).sample_text, set_value=sample
	widget_control, (*pstate).project_text, set_value=(*p).project

	q = where( (*psample).sample eq sample, nq)
	if widget_info( (*pstate).ref1_x_text, /valid) then widget_control, (*pstate).ref1_x_text, set_value=(nq ge 1) ? str_tidy( (*psample)[q[0]].reference.ref1.x)  : '?'
	if widget_info( (*pstate).ref1_y_text, /valid) then widget_control, (*pstate).ref1_y_text, set_value=(nq ge 1) ? str_tidy( (*psample)[q[0]].reference.ref1.y)  : '?'
	if widget_info( (*pstate).ref1_z_text, /valid) then widget_control, (*pstate).ref1_z_text, set_value=(nq ge 1) ? str_tidy( (*psample)[q[0]].reference.ref1.z)  : '?'
	if widget_info( (*pstate).ref2_x_text, /valid) then widget_control, (*pstate).ref2_x_text, set_value=(nq ge 1) ? str_tidy( (*psample)[q[0]].reference.ref2.x)  : '?'
	if widget_info( (*pstate).ref2_y_text, /valid) then widget_control, (*pstate).ref2_y_text, set_value=(nq ge 1) ? str_tidy( (*psample)[q[0]].reference.ref2.y)  : '?'
	if widget_info( (*pstate).ref2_z_text, /valid) then widget_control, (*pstate).ref2_z_text, set_value=(nq ge 1) ? str_tidy( (*psample)[q[0]].reference.ref2.z)  : '?'

	widget_control, (*pstate).igsn_text, set_value=(*psample)[q[0]].igsn

	if widget_info( (*pstate).sample_coords_mode, /valid) then begin
		if nq eq 0 then begin
			widget_control, (*pstate).sample_coords_mode, set_combobox_select=(*pstate).coords_src
		endif else begin
			q1 = where( (*psample)[q[0]].coordinates eq coords[0:2], nq1)
			if nq1 gt 0 then begin
				widget_control, (*pstate).sample_coords_mode, set_combobox_select=q1[0]
			endif else print,'Bad coords tag for: '+ (*psample)[q[0]].sample
		endelse
	endif

	q1 = where( (*p).coordinates eq coords[0:2], nq1)
	if nq1 gt 0 then begin
		widget_control, (*pstate).scan_coords_mode, set_combobox_select=q1[0]
	endif else print,'Bad coords tag for: '+ (*p).region

;	Scan: Check any dwell entries against maximum velocity ...
	
	vel = clip( (*pm).stage.speed.x, 1., 1000.)

	tvmin = (*p).raster.pixel.x / vel[0]
	if (*p).raster.dwell gt 0. then begin
		(*p).raster.dwell = (*p).raster.dwell > 1000. * tvmin
	endif
	
	nx = round( float((*p).raster.size.x) / float((*p).raster.pixel.x)) > 1
	ny = round( float((*p).raster.size.y) / float((*p).raster.pixel.y)) > 1
	
	time_tot = scan_list_time( p)
	stime = time_legend( time_tot)
	
	widget_control, (*pstate).size_x_text, set_value=str_tidy( (*p).raster.size.x) 
	widget_control, (*pstate).size_y_text, set_value=str_tidy( (*p).raster.size.y) 
	widget_control, (*pstate).npixels_x_text, set_value=str_tidy( nx) 
	widget_control, (*pstate).npixels_y_text, set_value=str_tidy( ny) 

	widget_control, (*pstate).dwell_text, set_value=str_tidy( (*p).raster.dwell) 
	widget_control, (*pstate).velocity_text, set_value=str_tidy( (*p).raster.pixel.x / (0.001 * (*p).raster.dwell)) 

	widget_control, (*pstate).origin_x_text, set_value=str_tidy( (*p).origin.x) 
	widget_control, (*pstate).origin_y_text, set_value=str_tidy( (*p).origin.y) 
	widget_control, (*pstate).origin_z_text, set_value=str_tidy( (*p).origin.z) 

	widget_control, (*pstate).pixel_x_text, set_value=str_tidy( (*p).raster.pixel.x) 
	widget_control, (*pstate).pixel_y_text, set_value=str_tidy( (*p).raster.pixel.y) 

	widget_control, (*pstate).sample_text, set_value=str_tidy( (*p).sample) 
	widget_control, (*pstate).grain_text, set_value=str_tidy( (*p).region) 
	widget_control, (*pstate).comment_text, set_value=str_tidy( (*p).comment) 
	widget_control, (*pstate).time_text, set_value=stime
	
	q = where( (*p).raster.interlace le interlaces, nq)
	if nq gt 0 then widget_control, (*pstate).interlace_mode, set_combobox_select= (*pstate).enable_interlace ? q[0] : 0
endif

	scan_list_check_bounds, pm, p, ps, silent=silent, error=error
	yellow = (error) ? 1 : 0
	widget_control, (*pstate).check_button, set_value={select:yellow}
	return
end

;------------------------------------------------------------------------------------------

pro scan_edit, group_leader=group, TLB=tlb, data=plist, index=pindex, row=rowi, path=path, $
				debug=debug, daq=pars, port=ps, sample=psample, frame=pframe, coords=lcoords, kvs_port=kvs, $
				enable_interlace=enable_interlace, kvs_prefix=kvs_prefix, prefs=prefs

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
		warning,'scan_edit',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''
if n_elements(kvs) eq 0 then kvs = obj_new()
if n_elements(debug) lt 1 then debug=0
if n_elements(rowi) lt 1 then rowi=0
if n_elements(lcoords) lt 1 then lcoords=[0,0]
if n_elements(enable_interlace) lt 1 then enable_interlace=1
if n_elements(kvs_prefix) lt 1 then kvs_prefix=''
if n_elements(prefs) lt 1 then begin
	prefs = geopixe_defaults( error=err, source='scan_edit')
endif

if size(*plist,/tname) ne 'POINTER' then begin
	no_list = 1
endif else begin
	no_list = 0
	if ptr_valid( (*plist)[0] ) eq 0 then no_list=1
	if no_list eq 0 then if size(*(*plist)[0],/tname) ne 'STRUCT' then no_list=1
endelse
if no_list then begin
;	pt = ptr_new( {on:1, key:'', pval:ptr_new(define(/maia_scan_spec))} )		; new scan to list
;	*plist = [pt]
;	(*(*(*plist)[0]).pval).active = 1											; set this one to 'OFF'
	*plist = [ptr_new()]
	*pindex = [-1]
endif
if ptr_good(psample) eq 0 then *psample=define(/maia_sample_spec)
if ptr_good(pframe) eq 0 then *pframe=define(/maia_frame_spec)
row = rowi

interlaces = [1, 2, 4, 8, 16, 32]
src_coords = ['finder','mapper1','mapper2','mapper1','mapper2']
tgt_coords = ['mapper1','mapper2']

n = n_elements( *pindex)
if (n eq 1) and ((*pindex)[0] eq -1) then begin
	pitem = ptr_new( define(/maia_scan_spec))
	(*pitem).coordinates = src_coords[lcoords[0]]
endif else begin
	print,'scan-edit: row, n = ', row, n
	row = clip( row, 0, n-1) 
	j = (*pindex)[ row]
	pitem = (*(*plist)[j]).pval
endelse

q = where( (*psample).sample eq (*pitem).sample, nq)
if (nq gt 0) and ((*pitem).sample ne '') then begin
	s = (*psample)[q[0]].coordinates
	q1 = where( src_coords eq s, nq1)
	sample_coords = q1[0] > 0
endif else begin
	sample_coords = lcoords[0]
endelse
s = (*pitem).coordinates
q1 = where( src_coords eq s, nq1)
if (nq1 gt 0) and (s ne '') then begin
	scan_coords = q1[0] > 0
endif else begin
	scan_coords = lcoords[0]
endelse

; Scan List parameters

pm = pars

case !version.os_family of
	'MacOS': begin
		yw = 219
		text_xsize = 85
		ref_xsize = 65
		origin_xsize = 95
		sample_xsize = 185
		number_xsize = 140
		comment_xsize = 343
		button_xsize = 50
		button2_xsize = 70
		button2_ysize = 22
		mode_xsize = 130
		right_xsize2 = 265
		right_xsize = 2*right_xsize2 + 8
		sspace1 = 6
		espace1 = 3
		space10 = 30
		help_lines = 5
		help_xsize = right_xsize+8
		padding = ' '
		padding2 = ''
		end
	'unix': begin
		yw = 219
		text_xsize = 85
		ref_xsize = 65
		origin_xsize = 95
		sample_xsize = 185
		number_xsize = 140
		comment_xsize = 343
		button_xsize = 50
		button2_xsize = 70
		button2_ysize = 22
		mode_xsize = 130
		right_xsize2 = 265
		right_xsize = 2*right_xsize2 + 8
		sspace1 = 6
		espace1 = 3
		space10 = 30
		help_lines = 5
		help_xsize = right_xsize+8
		padding = ' '
		padding2 = ''
		end
	else: begin
		yw = 219
		text_xsize = 85
		ref_xsize = 60
		origin_xsize = 90
		sample_xsize = 170
		number_xsize = 140
		comment_xsize = 320
		button_xsize = 50
		button2_xsize = 70
		button2_ysize = 20
		mode_xsize = 130
		right_xsize2 = 250
		right_xsize = 2*right_xsize2 + 5
		sspace1 = 1
		espace1 = 1
		space10 = 10
		help_lines = 4
		help_xsize = right_xsize
		padding = '        '
		padding2 = '      '
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
;axes = ['DAC X','DAC Y','Stage X','Stage Y','Stage Z','Stage A']
;shapes = ['Rectangular Snake','Elliptical scan']

; 	top-level base

tlb = widget_base( /column, title='Scan Edit', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='scan_edit_TLB', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2, /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)

r1base = widget_base( tbase, /row, xpad=0, ypad=0, space=8, /base_align_center, /align_center)

; Main base area

r2base = widget_base( r1base, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

; Top panel

r1base = widget_base( r2base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_bottom)

;...................................................................................................

; Sample details 

s0base = widget_base( r1base, /column, xpad=0, ypad=0, space=sspace1, /base_align_center)

sbase = widget_base( s0base, /column, xpad=2, ypad=1, space=1, /frame, /base_align_center, scr_xsize=right_xsize2)
label = widget_label( sbase, value='Sample Specification')
s1base = widget_base( sbase, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
label = widget_label( s1base, value='Name:')
sample_text = widget_text( s1base, value='', uname='sample-name-text', /tracking, /editable, scr_xsize=sample_xsize, $
	uvalue='Sample name. Hit <return> to add a new sample (and IGSN); will search database for any partial matches. Use "?" to display all existing samples in the database.')
button = widget_button( s1base, value='?', uname='sample-query-button', tracking=tracking, scr_xsize=22, $
	uvalue='Open sample database. Alternatively, enter a partial string in the "Name" field and hit <return> to search for partial matches.')
s1baseb = widget_base( sbase, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
label = widget_label( s1baseb, value='IGSN:')
igsn_text = widget_text( s1baseb, value='', uname='sample-igsn-text', /tracking, /editable, scr_xsize=sample_xsize, $
	uvalue='Enter Sample IGSN code for a new sample.')
label = widget_label( s1baseb, value=' ', scr_xsize=22)

;s1base2 = widget_base( s1base, /row, xpad=0, ypad=0, space=1, /align_center, /base_align_center)
;label = widget_label( s1base2, value='IGSN:')
;sample_igsn_text = widget_text( s1base2, value='CSA2 12345.j7', uname='sample-igsn-text', /tracking, /editable, scr_xsize=number_xsize, $
;	uvalue='IGSN sample identifier. Use "?" to search database.')
;button = widget_button( s1base2, value='?', uname='igsn-query-button', tracking=tracking, $
;	uvalue='Search sample IGSN database.')

; Project selection

sbase4 = widget_base( s0base, /column, xpad=2, ypad=1, space=1, /frame, /base_align_center, scr_xsize=right_xsize2)
label = widget_label( sbase4, value='Project Selection')
s4base = widget_base( sbase4, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
label = widget_label( s4base, value='Name:')
project_text = widget_text( s4base, value='', uname='project-name-text', /tracking, scr_xsize=sample_xsize, $		;, /editable
	uvalue='Project name. Use "?" to display all existing projects in the database and select one.')
button = widget_button( s4base, value='?', uname='project-query-button', tracking=tracking, scr_xsize=22, $
	uvalue='Open project database to select a project.' )

; Ref1,2

;s2base = widget_base( sbase, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)
;
;s2base1 = widget_base( s2base, /row, xpad=1, ypad=0, space=2, /align_right, /base_align_center)
;label = widget_label( s2base1, value=padding+'X', scr_xsize=ref_xsize)
;label = widget_label( s2base1, value=padding+'Y', scr_xsize=ref_xsize)
;label = widget_label( s2base1, value=padding+'Z', scr_xsize=ref_xsize)
;
;s2base2 = widget_base( s2base, /row, xpad=1, ypad=0, space=2, /align_right, /base_align_center)
;;label = widget_label( s2base2, value='  Ref1:')
;button = widget_button( s2base2, value='Ref1:', uname='ref1-sample-button', tracking=tracking, $
;	uvalue='Source: Position cursor on Ref #1 for this sample and capture its coordinates. ' + $
;		'Set "Source" using droplist on Scan List window. For "Finder" source, set Cursor on Camera View; for "GeoPIXE" source use Single Pixel shape.', scr_xsize=button_xsize)
;ref1_x_text = widget_text( s2base2, value='', uname='ref1-sample-x-text', /tracking, /editable, scr_xsize=ref_xsize, $
;	uvalue='Enter or show the source X cooordinate for Ref #1 (mm). X is horizontal scan axis.')
;ref1_y_text = widget_text( s2base2, value='', uname='ref1-sample-y-text', /tracking, /editable, scr_xsize=ref_xsize, $
;	uvalue='Enter or show the source Y cooordinate for Ref #1 (mm). Y is vertical scan axis. ')
;ref1_z_text = widget_text( s2base2, value='', uname='ref1-sample-z-text', /tracking, /editable, scr_xsize=ref_xsize, $
;	uvalue='Enter or show the source Z cooordinate for Ref #1 (mm). Z is the scan axis along the beam. ')
;
;s2base3 = widget_base( s2base, /row, xpad=1, ypad=0, space=2, /align_right, /base_align_center)
;;label = widget_label( s2base3, value='  Ref2:')
;button = widget_button( s2base3, value='Ref2:', uname='ref2-sample-button', tracking=tracking, $
;	uvalue='Source: Position cursor on Ref #2 for this sample and capture its coordinates. ' + $
;		'Set "Source" using droplist on Scan List window. For "Finder" source, set Cursor on Camera View; for "GeoPIXE" source use Single Pixel shape.', scr_xsize=button_xsize)
;ref2_x_text = widget_text( s2base3, value='', uname='ref2-sample-x-text', /tracking, /editable, scr_xsize=ref_xsize, $
;	uvalue='Enter or show the source X cooordinate for Ref #2 (mm). X is horizontal scan axis.')
;ref2_y_text = widget_text( s2base3, value='', uname='ref2-sample-y-text', /tracking, /editable, scr_xsize=ref_xsize, $
;	uvalue='Enter or show the source Y cooordinate for Ref #2 (mm). Y is vertical scan axis. ')
;ref2_z_text = widget_text( s2base3, value='', uname='ref2-sample-z-text', /tracking, /editable, scr_xsize=ref_xsize, $
;	uvalue='Enter or show the source Z cooordinate for Ref #2 (mm). Z is the scan axis along the beam. ')

ref1_x_text = 0L
ref1_y_text = 0L
ref1_z_text = 0L
ref2_x_text = 0L
ref2_y_text = 0L
ref2_z_text = 0L

; Coords 

s3base = widget_base( s0base, /column, xpad=2, ypad=1, space=1, /frame, /base_align_center, /align_center, scr_xsize=right_xsize2)
;label = widget_label( s3base, value='Sample & Scan Coordinates')
label = widget_label( s3base, value='Scan Coordinates')

s2base4 = widget_base( s3base, /column, xpad=0, ypad=0, space=1, /base_align_right, /align_center)
s2base5 = widget_base( s2base4, /row, xpad=0, ypad=0, space=5, /base_align_center)

;sample_coords_label = widget_label( s2base5, value='Sample:')
;sample_coords_mode = widget_combobox( s2base5, value=str_tidy(src_coords[0:2]), uname='sample-coords-mode', /tracking, scr_xsize=sample_xsize, $
;			uvalue='Shows the coordinate system for the Sample Ref marks. The initial setting is set by the "Source" droplist on the Scan List window. NOTE: The change from "finder" to "mapper" coordinates is ' + $
;			'usually handled by the "Translate" function.', /align_right)
sample_coords_mode = 0L

s2base6 = widget_base( s2base4, /row, xpad=0, ypad=0, space=5, /base_align_center)
scan_coords_label = widget_label( s2base6, value='Scan:')
scan_coords_mode = widget_combobox( s2base6, value=str_tidy(src_coords[0:2]), uname='scan-coords-mode', /tracking, scr_xsize=sample_xsize, $
			uvalue='Shows the coordinate system for the Scan origin and extent. The initial setting is set by the "Source" droplist on the Scan List window. NOTE: The change from "finder" to "mapper" coordinates is ' + $
			'usually handled by the "Translate" function.', /align_right)

;button = widget_button( s2base4, value='Translate', uname='translate-button', tracking=tracking, $
;			uvalue='Transform all Sample & Scan coordinates to "Mapper" based on Frame Finder to Mapper References. ' + $
;			'Both Finder and Mapper Reference points must be defined for Frame first.', scr_xsize=1.75*button_xsize)

;...................................................................................................

; Frame details - Finder coords

r0base = widget_base( r1base, /column, xpad=0, ypad=0, space=1, /base_align_center)
fbase = widget_base( r0base, /column, xpad=2, ypad=1, space=1, /frame, /base_align_center, scr_xsize=right_xsize2)
label = widget_label( fbase, value='Frame Reference (Finder Coords)')

; Ref1,2

f2base = widget_base( fbase, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)

f2base1 = widget_base( f2base, /row, xpad=1, ypad=0, space=2, /align_right, /base_align_center)
label = widget_label( f2base1, value=padding+'X', scr_xsize=ref_xsize)
label = widget_label( f2base1, value=padding+'Y', scr_xsize=ref_xsize)
label = widget_label( f2base1, value=padding+'Z', scr_xsize=ref_xsize)

f2base2 = widget_base( f2base, /row, xpad=1, ypad=0, space=2, /align_right, /base_align_center)
;label = widget_label( f2base2, value='  Ref1:')
button = widget_button( f2base2, value='Ref1:', uname='ref1-finder-button', tracking=tracking, $
	uvalue='Position Finder cursor on Ref #1 for this Frame and capture its coordinates. ' + $
		'For "Finder", set Cursor on Camera View.', scr_xsize=button_xsize)
ref1_finder_x_text = widget_text( f2base2, value='', uname='ref1-finder-x-text', /tracking, /editable, scr_xsize=ref_xsize, $
	uvalue='Enter or show the Finder X cooordinate for Ref #1 (mm). X is horizontal scan axis.')
ref1_finder_y_text = widget_text( f2base2, value='', uname='ref1-finder-y-text', /tracking, /editable, scr_xsize=ref_xsize, $
	uvalue='Enter or show the Finder Y cooordinate for Ref #1 (mm). Y is vertical scan axis. ')
ref1_finder_z_text = widget_text( f2base2, value='', uname='ref1-finder-z-text', /tracking, /editable, scr_xsize=ref_xsize, $
	uvalue='Enter or show the Finder Z cooordinate for Ref #1 (mm). Z is the scan axis along the beam. ')

f2base3 = widget_base( f2base, /row, xpad=1, ypad=0, space=2, /align_right, /base_align_center)
;label = widget_label( f2base3, value='  Ref2:')
button = widget_button( f2base3, value='Ref2:', uname='ref2-finder-button', tracking=tracking, $
	uvalue='Position Finder cursor on Ref #2 for this Frame and capture its coordinates. ' + $
		'For "Finder", set Cursor on Camera View.', scr_xsize=button_xsize)
ref2_finder_x_text = widget_text( f2base3, value='', uname='ref2-finder-x-text', /tracking, /editable, scr_xsize=ref_xsize, $
	uvalue='Enter or show the Finder X cooordinate for Ref #2 (mm). X is horizontal scan axis.')
ref2_finder_y_text = widget_text( f2base3, value='', uname='ref2-finder-y-text', /tracking, /editable, scr_xsize=ref_xsize, $
	uvalue='Enter or show the Finder Y cooordinate for Ref #2 (mm). Y is vertical scan axis. ')
ref2_finder_z_text = widget_text( f2base3, value='', uname='ref2-finder-z-text', /tracking, /editable, scr_xsize=ref_xsize, $
	uvalue='Enter or show the Finder Z cooordinate for Ref #2 (mm). Z is the scan axis along the beam. ')

; Mapper coords

fbase3 = widget_base( r0base, /column, xpad=2, ypad=1, space=1, /frame, /base_align_center, /align_center, scr_xsize=right_xsize2)
label = widget_label( fbase3, value='Frame Reference (Mapper Coords)')

; Ref1,2

f3base = widget_base( fbase3, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)

f3base1 = widget_base( f3base, /row, xpad=1, ypad=0, space=2, /align_right, /base_align_center)
label = widget_label( f3base1, value=padding+'X', scr_xsize=ref_xsize)
label = widget_label( f3base1, value=padding+'Y', scr_xsize=ref_xsize)
label = widget_label( f3base1, value=padding+'Z', scr_xsize=ref_xsize)

f3base2 = widget_base( f3base, /row, xpad=1, ypad=0, space=2, /align_right, /base_align_center)
;label = widget_label( f3base2, value='  Ref1:')
button = widget_button( f3base2, value='Ref1:', uname='ref1-mapper-button', tracking=tracking, $
	uvalue='Position mapper cursor on Ref #1 for this Frame and capture its coordinates. ' + $
		'Set "Target" mapper coords using droplist on Scan List window. For "mapper #1" or "mapper #2" use "GeoPIXE" Single Pixel shape on frame image from this end-station.', scr_xsize=button_xsize)
ref1_mapper_x_text = widget_text( f3base2, value='', uname='ref1-mapper-x-text', /tracking, /editable, scr_xsize=ref_xsize, $
	uvalue='Enter or show the target X cooordinate for Ref #1 (mm). X is horizontal scan axis.')
ref1_mapper_y_text = widget_text( f3base2, value='', uname='ref1-mapper-y-text', /tracking, /editable, scr_xsize=ref_xsize, $
	uvalue='Enter or show the target Y cooordinate for Ref #1 (mm). Y is vertical scan axis. ')
ref1_mapper_z_text = widget_text( f3base2, value='', uname='ref1-mapper-z-text', /tracking, /editable, scr_xsize=ref_xsize, $
	uvalue='Enter or show the target Z cooordinate for Ref #1 (mm). Z is the scan axis along the beam. ')

f3base3 = widget_base( f3base, /row, xpad=1, ypad=0, space=2, /align_right, /base_align_center)
;label = widget_label( f3base3, value='  Ref2:')
button = widget_button( f3base3, value='Ref2:', uname='ref2-mapper-button', tracking=tracking, $
	uvalue='Position mapper cursor on Ref #2 for this Frame and capture its coordinates. ' + $
		'Set "Target" mapper coords using droplist on Scan List window. For "mapper #1" or "mapper #2" use "GeoPIXE" Single Pixel shape on frame image from this end-station.', scr_xsize=button_xsize)
ref2_mapper_x_text = widget_text( f3base3, value='', uname='ref2-mapper-x-text', /tracking, /editable, scr_xsize=ref_xsize, $
	uvalue='Enter or show the target mapper X cooordinate for Ref #2 (mm). X is horizontal scan axis.')
ref2_mapper_y_text = widget_text( f3base3, value='', uname='ref2-mapper-y-text', /tracking, /editable, scr_xsize=ref_xsize, $
	uvalue='Enter or show the target mapper Y cooordinate for Ref #2 (mm). Y is vertical scan axis. ')
ref2_mapper_z_text = widget_text( f3base3, value='', uname='ref2-mapper-z-text', /tracking, /editable, scr_xsize=ref_xsize, $
	uvalue='Enter or show the target mapper Z cooordinate for Ref #2 (mm). Z is the scan axis along the beam. ')

f4base = widget_base( r0base, /row, xpad=1, ypad=1, space=2, /align_right, /base_align_center)
get_ref_mode = widget_combobox( f4base, value=['Retrieve RULER reference defaults','Retrieve FINDER reference defaults'], uname='get-ref-mode', /tracking, scr_xsize=sample_xsize+25, $
			uvalue='Select a set of default references for coordinate translation. Use "RULER" for ruler local coordinates or "FINDER" using the tiled image Finder. ' + $
			'Use the "Get" button to load the selected defaults.', /align_right)
button = widget_button( f4base, value='Get', uname='get-ref-button', tracking=tracking, $
			uvalue='Get the selected reference defaults for coordinate translation. Select a set of default references for coordinate translation ("ruler" for ruler local coordinates or "finder" using the tiled image Finder) ' + $
			'using the droplist.', scr_xsize=button_xsize)


;...................................................................................................

; Scan details

dbase = widget_base( r2base, /column, xpad=2, ypad=2, space=1, /frame, /base_align_center, /align_center, scr_xsize=right_xsize)
drbase1 = widget_base( dbase, /row, xpad=1, ypad=0, space=space10, /align_right, /base_align_center)

; Left button stack

lbase = widget_base( drbase1, /column, xpad=0, ypad=0, space=espace1, /base_align_center, /align_top)
label = widget_label( lbase, value='Set Extent')

button = state_button( lbase, value='Clear', uname='clear-button', tracking=tracking, xsize=button2_xsize, ysize=button2_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Clears the pending flags (yellow buttons) in preparation for setting "left", '+ $
					'"right", "top" and "bottom" positions to set scan size and origin. These clear automatically once matching ' + $
					'(left-right, top-bottom or centre) are set.')

label = widget_label( lbase, value=' ')

left_button = state_button( lbase, value='Left', uname='left-button', tracking=tracking, xsize=button2_xsize, ysize=button2_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the left extent of the scan to the current "Source" position (select "source" on Scan List window). "Yellow" indicates left-right pending. Complete by setting "right" position. ' + $
					'Momentary "Green" indicates completion, which sets X origin and size. Use "Clear" to cancel pending Yellow.')

right_button = state_button( lbase, value='Right', uname='right-button', tracking=tracking, xsize=button2_xsize, ysize=button2_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the right extent of the scan to the current "Source" position (select "source" on Scan List window). "Yellow" indicates left-right pending. Complete by setting "left" position. ' + $
					'Momentary "Green" indicates completion, which sets X origin and size. Use "Clear" to cancel pending Yellow.')

top_button = state_button( lbase, value='Top', uname='top-button', tracking=tracking, xsize=button2_xsize, ysize=button2_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the top extent of the scan to the current "Source" position (select "source" on Scan List window). "Yellow" indicates bottom-top pending. Complete by setting "bottom" position. ' + $
					'Momentary "Green" indicates completion, which sets Y origin and size. Use "Clear" to cancel pending Yellow.')

bottom_button = state_button( lbase, value='Bottom', uname='bottom-button', tracking=tracking, xsize=button2_xsize, ysize=button2_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the bottom extent of the scan to the current "Source" position (select "source" on Scan List window). "Yellow" indicates bottom-top pending. Complete by setting "top" position. ' + $
					'Momentary "Green" indicates completion, which sets Y origin and size. Use "Clear" to cancel pending Yellow.')

label = widget_label( lbase, value=' ')

button = state_button( lbase, value='Bot-Left', uname='bottom-left-button', tracking=tracking, xsize=button2_xsize, ysize=button2_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the bottom, left extent of the scan to the current "Source" position (select "source" on Scan List window). "Yellow" indicates bottom, left pending. Complete by setting "top, right" positions. ' + $
					'Momentary "Green" indicates completion, which sets XY origin and size. Use "Clear" to cancel pending Yellow.')

button = state_button( lbase, value='Top-Right', uname='top-right-button', tracking=tracking, xsize=button2_xsize, ysize=button2_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the top, right extent of the scan to the current "Source" position (select "source" on Scan List window). "Yellow" indicates top, right pending. Complete by setting "bottom, left" positions. ' + $
					'Momentary "Green" indicates completion, which sets XY origin and size. Use "Clear" to cancel pending Yellow.')

button = state_button( lbase, value='Centre', uname='centre-button', tracking=tracking, xsize=button2_xsize, ysize=button2_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Set the centre of the scan to the current "Source" position (select "source" on Scan List window). Uses the present XY scan size and this centre to set the new scan origin. ' + $
					'Momentary "Green" indicates completion.')

dcbase2 = widget_base( drbase1, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_top)
label = widget_label( dcbase2, value='Scan Details')
d1base = widget_base( dcbase2, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)

d1base1 = widget_base( d1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( d1base1, value='Region:')
grain_text = widget_text( d1base1, value='', uname='grain-text', /tracking, /editable, scr_xsize=sample_xsize, $
			uvalue='Enter an optional "region" name descriptor for this scan region. ')
label = widget_label( d1base1, value=' ', scr_xsize=comment_xsize-sample_xsize-7)
d1base2 = widget_base( d1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( d1base2, value='Comment:')
comment_text = widget_text( d1base2, value='', uname='comment-text', /tracking, /editable, scr_xsize=comment_xsize, $
			uvalue='Enter an optional comment for this particular scan region.')

; Axes and Time

ambase = widget_base( dcbase2, /row, xpad=0, ypad=0, space=1, /base_align_top, /align_center)

; Axes

am1base = widget_base( ambase, /column, xpad=0, ypad=0, space=1, /base_align_right, /align_center)

am1base0 = widget_base( am1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am1base0, value=padding+'  X', scr_xsize=text_xsize)
label = widget_label( am1base0, value=padding+'  Y', scr_xsize=text_xsize)

am1base1 = widget_base( am1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
;label = widget_label( am1base1, value='Axis:')
;axis_x_mode = widget_combobox( am1base1, value=axes, uname='axis-x-mode', /tracking, scr_xsize=text_xsize, $
;	;	notify_realize='OnRealize_scan_edit_x_mode', $
;	uvalue='Select horizontal (X) image axis source mode. Select between DAC (X,Y) and scan (X,Y,Z,A) axes. ')
;axis_y_mode = widget_combobox( am1base1, value=axes, uname='axis-y-mode', /tracking, scr_xsize=text_xsize, $
;	;	notify_realize='OnRealize_scan_edit_y_mode', $
;	uvalue='Select horizontal (Y) image axis source mode. Select between DAC (X,Y) and scan (X,Y,Z,A) axes.')

am1base2 = widget_base( am1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am1base2, value='Size:')
size_x_text = widget_text( am1base2, value='', uname='size-x-text', /tracking, /editable, scr_xsize=text_xsize, $
	uvalue='Enter horizontal (X) image axis size (mm) in "Source" coordinates. Hit <return> to calculate #pixels and time.')
size_y_text = widget_text( am1base2, value='', uname='size-y-text', /tracking, /editable, scr_xsize=text_xsize, $
	uvalue='Enter vertical (Y) image axis size (mm) in "Source" coordinates. Hit <return> to calculate #pixels and time.')

am1base4 = widget_base( am1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am1base4, value='Pixel:')
pixel_x_text = widget_text( am1base4, value='', uname='pixel-x-text', /tracking, /editable, scr_xsize=text_xsize, $
	uvalue='Enter the X pixel pitch (mm) per pixel. Pixel pitch should be constrained to be equal in X and Y. Hit <return> to calculate the # pixels in X.')
pixel_y_text = widget_text( am1base4, value='', uname='pixel-y-text', /tracking, /editable, scr_xsize=text_xsize, $
	uvalue='Enter the Y pixel pitch (mm) per pixel. Pixel pitch should be constrained to be equal in X and Y. Hit <return> to calculate the # pixels in Y.')

am1base3 = widget_base( am1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am1base3, value='# pixels:')
npixels_x_text = widget_text( am1base3, value='', uname='npixels-x-text', /tracking, /editable, scr_xsize=text_xsize, $
	uvalue='Enter horizontal (X) image axis size in pixels. Hit <return> to calculate single pixel pitch. Note that X,Y pixel pitch should be constrained to be equal.')
npixels_y_text = widget_text( am1base3, value='', uname='npixels-y-text', /tracking, /editable, scr_xsize=text_xsize, $
	uvalue='Enter vertical (Y) image axis size in pixels. Hit <return> to calculate single pixel pitch. Note that X,Y pixel pitch should be constrained to be equal')

; Time, dwell, velocity

am2base = widget_base( ambase, /column, xpad=0, ypad=0, space=1, /base_align_top, /align_center)

am2base0 = widget_base( am2base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am2base0, value=padding2+'Time', scr_xsize=text_xsize)

am2base3 = widget_base( am2base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( am2base3, value='Dwell:')
dwell_text = widget_text( am2base3, value='', uname='dwell-text', /tracking, /editable, scr_xsize=text_xsize, $
	uvalue='Enter the dwell time (ms) to acquire each pixel in fly scan mode. Hit <return> to show the new Velocity.')

dpbase4 = widget_base( am2base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( dpbase4, value='Velocity:')
velocity_text = widget_text( dpbase4, value='', uname='velocity-text', /tracking, /editable, scr_xsize=text_xsize, $
			uvalue='Enter the desired velocity (mm/s) for X scan. Hit <return> to set Dwell to pitch/velocity.')

dpbase3 = widget_base( am2base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( dpbase3, value='Total time:')
time_text = widget_text( dpbase3, value='', uname='time-text', /tracking, editable=0, scr_xsize=text_xsize, $
			uvalue='Shows the total time for the scan, estimated from pixel dwell time and total number of pixels. ')

; Interlace

am4base = widget_base( am2base, /column, xpad=0, ypad=3, space=5, /align_right, /base_align_center)
dpbase4b = widget_base( am4base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( dpbase4b, value='Interlace:')
interlace_mode = widget_combobox( dpbase4b, value='    '+str_tidy(interlaces), uname='interlace-mode', /tracking, scr_xsize=text_xsize, $
		;	notify_realize='OnRealize_scan_edit_interlace_mode', $
			uvalue='Select either "interlace"=1 (off; scan each line), or "interlace"=n to move "n" lines on the slow axis ' + $
			'and fill in the intervening lines over "n" scan frames.', /align_right, sensitive=enable_interlace)

; Origin

os1base = widget_base( dcbase2, /column, xpad=0, ypad=0, space=1, /base_align_top, /align_center)

as1base0 = widget_base( os1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( as1base0, value=padding+'    X', scr_xsize=origin_xsize)
label = widget_label( as1base0, value=padding+'    Y', scr_xsize=origin_xsize)
label = widget_label( as1base0, value=padding+'    Z', scr_xsize=origin_xsize)
label = widget_label( as1base0, value=padding+'    ')

os1base1 = widget_base( os1base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( os1base1, value='  Origin:')
origin_x_text = widget_text( os1base1, value='', uname='origin-x-text', /tracking, /editable, scr_xsize=origin_xsize, $
		uvalue='Enter or show the X scan origin (left extent) for the scan (mm) in "Source" coordinates (select "source" on Scan List window). X is horizontal scan axis.')
origin_y_text = widget_text( os1base1, value='', uname='origin-y-text', /tracking, /editable, scr_xsize=origin_xsize, $
		uvalue='Enter or show the Y scan origin (bottom extent) for the scan (mm) in "Source" coordinates (select "source" on Scan List window). Y is vertical scan axis. ')
origin_z_text = widget_text( os1base1, value='', uname='origin-z-text', /tracking, /editable, scr_xsize=origin_xsize, $
		uvalue='Enter or show the Z scan origin (focus) for the scan (mm). Z is the scan axis along the beam. ')

;button = widget_button( os1base1, value='Set', uname='set-origin-button', tracking=tracking, $
;	uvalue='Open a requester to enter X,Y origin in sample frame "Ruler" coordinates. These are measured using a ruler. X is from the left edge of the aluminium target at the left end of the stage.' + $
;			'Y is measured from the top surface of the black aluminium base plate.')

;...................................................................................................
; Buttons

bbase = widget_base( r2base, /row, /base_align_center, xpad = 0, ypad=1, space=30)

check_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
check_button = state_button( check_base, value='Check', uname='check-button', tracking=tracking, xsize=button_xsize, ysize=button2_ysize, $
					/freeze, select=0, colours=colours, n_states=3, alt=0, $
					uvalue='Check scan against stage bounds, etc. Yellow button indicates a problem to check. ' + $
					'NOTE: These bounds checks are only significant for final "mapper" coordinates after "Translate".')

button = widget_button( bbase, value='Capture', uname='capture-button', tracking=tracking, $
		uvalue='Capture a Scan area as selected on the "Source" image display and append it as a new scan. The source is selected using "Source" on the Scan List window (e.g. Finder, Mapper or GeoPIXE for end-stations 1 or 2). ' + $
		'Select regions on GeoPIXE images using the "Box" shape.')
add_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
append_button = widget_button( add_base, value='Append', uname='append-button', tracking=tracking, $
					uvalue='Append the current settings in this edit window to the bottom of the scan list as a new scan.')
button = widget_button( add_base, value='Insert', uname='insert-button', tracking=tracking, $
					uvalue='Insert the current settings in this edit window above the selected row in the scan list. Use this to duplicate (and edit) a scan list entry.')

apply_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
button = widget_button( apply_base, value='Apply', uname='apply-button', tracking=tracking, $
					uvalue='Apply the current changes to the selected row in the scan list.')

;close_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
;button = widget_button( close_base, value='Close', uname='close-button', tracking=tracking, $
;					uvalue='Close the Edit window. The data remains stored in the parent DAQ Control window')

;...................................................................................................
; Help

help = widget_text( tbase, scr_xsize=help_xsize, ysize=help_lines, /wrap, uname='help', tracking=tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

state = { $
		path:			ptr_new(path), $		; pointer to current path
		plist:			plist, $				; pointer to array of pointers to scan detail structs
		pindex:			pindex, $				; pointer to display index
		pframe:			pframe, $				; pointer to frame details
		psample:		psample, $				; pionter to sample ref array
		pitem:			ptr_new(*pitem), $		; pointer to local copy of present list row
		kvs:			kvs, $					; KVS object
		kvs_prefix:		kvs_prefix, $			; KVS key prefix (e.g. 'MM.Mel.')
		ps:				ps, $					; pointer to Klee port struct
		pm:				pm, $					; pointer to DAQ parameters struct
		pselect:		ptr_new(/allocate_heap), $	; pointer to select for notify to ??
		prefs:			prefs, $				; geopixe defaults

		extent: {	left:	0, $				; flags setting extents
					right:	0, $				;
					top:	0, $				;
					bottom: 0}, $				;
		mark:	 {	left:	0.0, $				; mark the extent positions
					right:	0.0, $				;
					top:	0.0, $				;
					bottom: 0.0}, $				;

		position: {	x:		0.0, $				; present X scan position			
					y:		0.0, $				; present Y scan position
					z:		0.0}, $				; present Z scan position
		newscan:  {	new: 0, $					; newscan
					origin: {	x: 0.0, $		; origin X
								y: 0.0, $		; Y
								z: 0.0}, $		; Z
					size: {		x: 0.0, $		; scan X size
								y: 0.0}}, $		; Y
		sel:			row, $					; row in list selected
		beam:			1.0, $					; beam current estimate (nA)

		coords_src:		lcoords[0], $			; source coords from scan-list (0=finder, 1=mapper1, 2=mapper2, 3=geopixe1, 4=geopixe2)
		coords_tgt:		lcoords[1], $			; tgt coords from scan-list (0=mapper1, 1=mapper2)

		scan_coords:	scan_coords, $			; coords in current scan
		sample_coords:	sample_coords, $		; coords in current sample
		get_ref:		0, $					; get reference mode (0=ruler, 1=finder)

		tracking:		tracking, $				; is tracking enabled
;		shapes:			shapes, $				; shapes list
		active:			active, $				; activation modes for items in list
		enable_interlace:	enable_interlace, $ ; enable interlace > 1

		left_button:	left_button, $			; left button ID
		right_button:	right_button, $			; right button ID
		top_button:		top_button, $			; top button ID
		bottom_button:	bottom_button, $		; bottom button ID
		size_x_text:	size_x_text, $			; X axis size text ID
		size_y_text:	size_y_text, $			; Y axis size text ID
		npixels_x_text:	npixels_x_text, $		; X axis # pixels text ID
		npixels_y_text:	npixels_y_text, $		; Y axis # pixels text ID
		dwell_text: 	dwell_text, $			; dwell text ID
		velocity_text:	velocity_text, $		; velocity text ID
		origin_x_text:	origin_x_text, $		; origin X text ID
		origin_y_text:	origin_y_text, $		; origin Y text ID
		origin_z_text:	origin_z_text, $		; origin Z text ID
		sample_text:	sample_text, $			; sample text ID
		project_text:	project_text, $			; project text ID
		igsn_text:		igsn_text, $			; IGSN text ID
		grain_text:		grain_text, $			; grain text ID
		comment_text:	comment_text, $			; comment text ID
		pixel_x_text:	pixel_x_text, $			; pixel X size text ID
		pixel_y_text:	pixel_y_text, $			; pixel X size text ID
		time_text:		time_text, $			; time text ID
		interlace_mode:	interlace_mode, $		; interlace droplist mode ID
		check_button:	check_button, $			; check bounds button ID

		sample_coords_mode: sample_coords_mode, $	; sample coords mode ID
		scan_coords_mode: scan_coords_mode, $	; scan coords mode ID
		
		ref1_x_text:	ref1_x_text, $			; sample ref1 X text ID
		ref1_y_text:	ref1_y_text, $			; sample ref1 Y text ID
		ref1_z_text:	ref1_z_text, $			; sample ref1 Z text ID
		ref2_x_text:	ref2_x_text, $			; sample ref2 X text ID
		ref2_y_text:	ref2_y_text, $			; sample ref2 Y text ID
		ref2_z_text:	ref2_z_text, $			; sample ref2 Z text ID

		ref1_finder_x_text:	ref1_finder_x_text, $	; frame (finder) ref1 X text ID
		ref1_finder_y_text:	ref1_finder_y_text, $	; frame (finder) ref1 Y text ID
		ref1_finder_z_text:	ref1_finder_z_text, $	; frame (finder) ref1 Z text ID
		ref2_finder_x_text:	ref2_finder_x_text, $	; frame (finder) ref2 X text ID
		ref2_finder_y_text:	ref2_finder_y_text, $	; frame (finder) ref2 Y text ID
		ref2_finder_z_text:	ref2_finder_z_text, $	; frame (finder) ref2 Z text ID

		ref1_mapper_x_text:	ref1_mapper_x_text, $	; frame (mapper) ref1 X text ID
		ref1_mapper_y_text:	ref1_mapper_y_text, $	; frame (mapper) ref1 Y text ID
		ref1_mapper_z_text:	ref1_mapper_z_text, $	; frame (mapper) ref1 Z text ID
		ref2_mapper_x_text:	ref2_mapper_x_text, $	; frame (mapper) ref2 X text ID
		ref2_mapper_y_text:	ref2_mapper_y_text, $	; frame (mapper) ref2 Y text ID
		ref2_mapper_z_text:	ref2_mapper_z_text, $	; frame (mapper) ref2 Z text ID

		help:			help $					; ID of help text
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

widget_control, append_button, timer=0.1				; set timer to do initial refresh of widgets

register_notify, tlb, ['path', $					; new path
					'scan-new', $					; a new scan details message
					'scan-edit-select', $			; scan edit update w/ new row select
					'scan-edit-coords' $			; scan list coords select
					], from=group

xmanager, 'scan_edit', tlb, /no_block

return
end
