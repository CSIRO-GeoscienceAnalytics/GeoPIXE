
; Wizard to determine the 'conv' calibration factors for standards foils.
; 
;	For a selected raw dir, find all raw files (of 'standard_type' = "standard"),
;	and calculate the IC conversion calibration factor 'conv' by using a
;	region over most of the foil image.

pro wizard_batch_event, event

;	Event routine to process widget events and "Notify" events from other windows,
;	such as the "wizard-return" events from GeoPIXE windows, which are replies to 
;	completed "wizard-action" requests sent using the global Notify mechanism.
;	These events contain a 'pointer' that points to a struct of this form:
;	
;	{	wizard:		'', $				; originating wizard name
;		window:		'', $				; specific name label for destination window
;		command:	'', $				; command to be executed by window
;		qual1:		'', $				; some extra qualifier1
;		qual2:		'', $				; some extra qualifier2
;		error:		0, $				; error return code, on reply
;		top:		0L, $				; return 'event.top' in case of 'open-test'
;		pdata:		ptr_new(), $		; general data to be transferred
;		local:		1, $				; indicates 'pdata' managed (e.g. freed) by Wizard
;		callback:	'', $				; name of callback routine
;		pnext:		ptr_new() }			; next action in linked list

;	The event routine responds to a "wizard-return" Notify event directed to its
;	wizard name and checks for a callback routine name. If present the callback is executed
;	to perform any cleanup or apply returned data. It then checks for a valid 'pnext'
;	pointer to further Notify event(s) and sends a further 'Notify, "wizard-action", pnext'
;	to sequentially perform actions in the linked list.
;
;	See further notes with main routine below ...

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

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
		warning,'wizard_batch_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		(*pstate).busy = 0
		return						; goto, kill
	endif
endif
widget_control, hourglass=0
if widget_info( event.top, /valid) eq 0 then goto, finish	; stop timer events after window closed?

; Get the pointer 'pstate' to the state data, store in first child ...

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if ptr_good(pstate, /struct) eq 0 then goto, bad_state

no_data = 1
p = (*pstate).presults
if ptr_good(p) eq 1 then begin
	if size( (*p)[0],/tname) eq 'STRUCT' then begin
		no_data = 0
	endif
endif
obj = *(*pstate).pDevObj

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
		if (*pstate).tracking eq 0 then goto, finish
		s = ''
		widget_control, event.id, get_uvalue=uv
		if size(uv,/tname) eq 'STRING' then begin
			s = uv
		endif else if size(uv,/tname) eq 'STRUCT' then begin
			if tag_present('HELP',uv) then s = uv.help
		endif
		if event.enter eq 1 then begin
			widget_control, (*pstate).help, set_value=s
		endif else begin
			widget_control, (*pstate).help, set_value='Complete entries and operations on the current tab before hitting "Next" to go to the next one. ' + $
						'Go back to previous tab using "Back" or select specific tabs on the left.'
		endelse
		goto, finish
		end
	'WIDGET_TIMER': begin
		if *(*pstate).message.pwizard ne !null then begin
			warning, 'Batch Wizard', *(*pstate).message.pwizard
			*(*pstate).message.pwizard  = !null
		endif
		if *(*pstate).message.pwindow ne !null then begin
			warning, 'Batch Wizard', *(*pstate).message.pwindow
			*(*pstate).message.pwindow  = !null
		endif
		wizard_test_windows, 'batch', pstate					; periodically check which GeoPIXE windows are
		widget_control, event.id, timer=20.0	
		goto, finish
		end

	'NOTIFY': begin												; events from other GeoPIXE windows
		case event.tag of
;			'path': begin
;				if ptr_valid( event.pointer) then begin
;					*(*pstate).path = (*event.pointer)
;				endif
;				goto, finish
;				end
			'new-detectors': begin
				present = (*(*pstate).detector_list)[(*pstate).detector_mode]
				detector_update, list=list, title=title, present=present, new=new
				list = ['',list]
				title = [' ',title]
				widget_control, (*pstate).detector_mode_id, set_value=title, set_combobox_select=new+1
				widget_control, (*pstate).detector_new_mode_id, set_value=title, set_combobox_select=new+1
				*(*pstate).detector_list = list
				(*pstate).detector_mode = new+1
				(*pstate).detector_new_mode = new+1
				goto, finish
				end

;			These are the events returned from GeoPIXE windows after 'wizard-action' operations.
;			We check for an error return, and for callback operations to perform, and then if there
;			is a next 'wizard-action' event to send.

			'wizard-return': begin	
				if ptr_valid( event.pointer) then begin
					pw = event.pointer

					if (*pw).wizard ne 'batch' then begin
						print,'Wizard Batch: found another Wizard: '+(*pw).wizard
						*(*pstate).message.pwizard = 	['Another Wizard appears to be open ("'+(*pw).wizard+'").', '', $
								'This will cause many problems.','Only open one Wizard at a time.','Please close other Wizards.']
						goto, finish
					endif

;					A reply from a window to 'open-test' shows that it is open ('top' returns 'event.top' of sending window) ...
;					Accumulate a list of all open and valid IDs to test if unwanted duplicates are open.

					if (*pw).command eq 'open-test' then begin
						wizard_check_window_id, needed=(*pstate).windows_needed, open=(*pstate).windows_open, name=(*pw).window, id=(*pw).top, count=count, error=error
						if error then goto, finish

						if count gt 1 then begin
							print,'Wizard Batch: found duplicate windows.'
							*(*pstate).message.pwindow = ['Multiple windows of types "'+strjoin((*pstate).windows_needed,', ')+'" may be open.', '', $
								'This may cause problems.','Please close any duplicate windows.']
						endif
						goto, finish
					endif
						
					print, '*** Wizard return: from="'+(*pw).window+'", command="'+(*pw).command+'", error='+str_tidy((*pw).error)
					if (*pw).error then begin
						(*p)[(*pstate).index].on = 4				; Error
						warning,'batch wizard',['Error in processing this data-set:', $
								(*(*pw).pdata).output,'','"OK" to continue, "Cancel" to abort all.'], cancel=cancel
						if cancel then goto, abort_run
					endif
						
;					If there is a callback routine, execute it now, passing the returned wizard notify pointer ...

					if (*pw).callback ne '' then begin
						print, '   >>> callback = "'+(*pw).callback
						call_procedure, (*pw).callback, pstate, pw, error=error 
						if error then begin
							print, '		callback error='+str_tidy(error)
							(*pstate).busy = 0
							goto, finish
						endif
					endif

;					If there is a valid notify struct pointed to from 'pnext' in the returned event,
;					then Notify that in turn ...
;					Note that we don't free linked list memory here.

					if ptr_good( (*pw).pnext) then begin
						print, '   >>> Notify next, to="'+(*(*pw).pnext).window+'", command="'+(*(*pw).pnext).command
						notify, 'wizard-action', (*pw).pnext
					endif else begin
						print, '   >>> End sequence, no Notify next.'

;						Next row?
	
						if (*pstate).abort then goto, abort_run
						if (*pw).loop then begin
							print, '   >>> Loop to next row?'
							wizard_batch_process_blog, pstate, error=error
							if error then goto, finish
						endif
					endelse
				endif
				goto, finish
				end

			else:
		endcase
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request wizard_batch ...'
		goto, kill
		end
	else:
endcase

; From here we process other events from widgets and the top-level base
; By convention all widgets have a 'uname' to identify them.

uname = widget_info( event.id, /uname)
case uname of

	'wizard-batch-tlb': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				print,'Resize top level base ...'
				wizard_resize, event.top, oldx=(*pstate).tlb_xsize, oldy=(*pstate).tlb_ysize, $
								minx=700, miny=600
				geom = widget_info( event.top, /geometry)
				(*pstate).tlb_xsize = geom.scr_xsize
				(*pstate).tlb_ysize = geom.scr_ysize

;				wizard_batch_update_plots, pstate
				end
			else:
		endcase
		end

	'wizard-batch-tab-panel': begin
		(*pstate).tab = clip( event.tab, 0, n_elements((*pstate).tab_names)-1)
		wizard_batch_update_info, pstate
		
;		case (*pstate).tab_names[event.tab] of
;			'Summary': begin		; summary
;				end
;			else:
;		endcase
		end
	
	'device-mode': begin
		(*pstate).device = event.index
		DevObj = (*(*pstate).pDevObjList)[(*pstate).device]			; current device object
		*(*pstate).pDevObj = DevObj
		end

	'blog-dir-button': begin
		F = file_requester( /read, title='Select the "batch" raw dir', path=(*pstate).blog_dir, $
							/dir, group=event.top )
		if F[0] ne '' then begin
			(*pstate).blog_dir = F[0]
			*(*pstate).dpath = F[0]
			set_widget_text, (*pstate).blog_dir_text, (*pstate).blog_dir
			s = build_output_path( (*pstate).blog_dir, (*pstate).output_dir, (*pstate).root, /set)
			if lenchr(s) gt 0 then begin
				(*pstate).output_dir = s
				set_widget_text, (*pstate).output_dir_text, s
			endif
			notify, 'dpath', (*pstate).dpath, from=event.top
		endif
		end
		
	'blog-dir-text': begin
		widget_control, event.id, get_value=s
		(*pstate).blog_dir = s
		*(*pstate).dpath = s
		s = build_output_path( (*pstate).blog_dir, (*pstate).output_dir, (*pstate).root, /set)
		if lenchr(s) gt 0 then begin
			(*pstate).output_dir = s
			set_widget_text, (*pstate).output_dir_text, s
		endif
		notify, 'dpath', (*pstate).dpath, from=event.top
		end
					
	'energy-cal-file-button': begin
		F = file_requester( /read, title='Select a detector energy cal file', path=*(*pstate).path, $
							file=(*pstate).energy_cal_file, filter='*.spec', group=event.top )
		if F[0] ne '' then begin
			(*pstate).energy_cal_file = F[0]
			set_widget_text, (*pstate).energy_cal_file_text, F[0]
		endif
		end
		
	'energy-cal-file-text': begin
		widget_control, event.id, get_value=s
		(*pstate).energy_cal_file = s
		end
					
	'output-dir-button': begin
		F = file_requester( /read, title='Select "batch" analysis output dir', path=(*pstate).output_dir, $
							/dir, group=event.top )
		if F[0] ne '' then begin
			(*pstate).output_dir = F[0]
			set_widget_text, (*pstate).output_dir_text, (*pstate).output_dir 
			s = build_output_path( (*pstate).blog_dir, (*pstate).output_dir, (*pstate).root, /set)
			*(*pstate).path = (*pstate).output_dir 
			notify, 'path', (*pstate).path, from=event.top
		endif
		end
		
	'output-dir-text': begin
		widget_control, event.id, get_value=s
		(*pstate).output_dir = s
		*(*pstate).path = (*pstate).output_dir 
		notify, 'path', (*pstate).path, from=event.top
		end
				
	'same-dir-button': begin
		(*pstate).output_dir = (*pstate).blog_dir
		set_widget_text, (*pstate).output_dir_text, (*pstate).output_dir 
		s = build_output_path( (*pstate).blog_dir, (*pstate).output_dir, (*pstate).root, /set)
		*(*pstate).path = (*pstate).output_dir 
		end

	'template-sort-button': begin
		F = file_requester( /read, title='Select a template DAI', path=(*pstate).output_dir, file=(*pstate).template_sort_dai, $
							filter='*.dai', group=event.top )
		if F[0] eq '' then goto, finish
		(*pstate).template_sort_dai = F[0]
		set_widget_text, (*pstate).template_sort_text, (*pstate).template_sort_dai
		wizard_batch_load_template, pstate, error=error
		if error then begin
;			warning,'wizard_batch_event','No raw files found.'		
			goto, finish
		endif
		widget_control, (*pstate).corrections_element, set_value = *(*pstate).pcel	
		widget_control, (*pstate).r_element, set_value = *(*pstate).pcel
		widget_control, (*pstate).g_element, set_value = *(*pstate).pcel
		widget_control, (*pstate).b_element, set_value = *(*pstate).pcel
		widget_control, (*pstate).conv_text, set_value = str_tidy((*(*pstate).pdai).IC.conversion)

		wizard_batch_process_setup, pstate, error=error

		if typevar( *(*pstate).pcorr) ne 'STRUCT' then begin
			(*pstate).template_corrections_dai = (*pstate).template_sort_dai
			set_widget_text, (*pstate).template_corrections_text, (*pstate).template_corrections_dai
	
			table = wizard_batch_load_corr( pstate, title=title, type=type, element=element, error=error)
			if error then begin
				goto, finish
			endif
			*(*pstate).pcorr = table
			*(*pstate).pctitle = title
			*(*pstate).pctype = type
			wizard_batch_update_ctable, pstate
	
			(*pstate).options_process[0] = 0		; do not enable corrections yet
			widget_control, (*pstate).options_process_id, set_value=(*pstate).options_process
			(*pstate).template_corrections_dai = ''
			set_widget_text, (*pstate).template_corrections_text, (*pstate).template_corrections_dai
		endif
		end
		
	'template-sort-text': begin
		widget_control, event.id, get_value=s
		if file_test(s) eq 0 then begin
			warning,'wizard_batch_event','File not found.'		
			goto, finish
		endif
		(*pstate).template_sort_dai = s
		wizard_batch_load_template, pstate, error=error
		if error then begin
;			warning,'wizard_batch_event','No raw files found.'		
			goto, finish
		endif
		widget_control, (*pstate).corrections_element, set_value = *(*pstate).pcel	
		widget_control, (*pstate).r_element, set_value = *(*pstate).pcel
		widget_control, (*pstate).g_element, set_value = *(*pstate).pcel
		widget_control, (*pstate).b_element, set_value = *(*pstate).pcel

		wizard_batch_process_setup, pstate, error=error

		if typevar( *(*pstate).pcorr) ne 'STRUCT' then begin
			(*pstate).template_corrections_dai = (*pstate).template_sort_dai
			set_widget_text, (*pstate).template_corrections_text, (*pstate).template_corrections_dai
	
			table = wizard_batch_load_corr( pstate, title=title, type=type, element=element, error=error)
			if error then begin
				goto, finish
			endif
			*(*pstate).pcorr = table
			*(*pstate).pctitle = title
			*(*pstate).pctype = type
			wizard_batch_update_ctable, pstate
	
			(*pstate).options_process[0] = 0		; do not enable corrections yet
			widget_control, (*pstate).options_process_id, set_value=(*pstate).options_process
			(*pstate).template_corrections_dai = ''
			set_widget_text, (*pstate).template_corrections_text, (*pstate).template_corrections_dai
		endif
		end
					
	'scan-blog-button': begin
		s = build_output_path( (*pstate).blog_dir, (*pstate).output_dir, (*pstate).root, /set)

		table = wizard_batch_scan_dir( pstate, title=title, type=type, error=error)
		if error then begin
			warning,'wizard_batch_event','No raw files found.'		
			goto, finish
		endif
		*(*pstate).presults = table
		*(*pstate).ptitle = title
		*(*pstate).ptype = type
		wizard_batch_update_table, pstate
		end
	
;	'reload-standards-button': begin
;		config = wizard_batch_read_config( fix_path((*pstate).resource_dir)+'standards'+path_sep()+'standards.csv', error=error)
;		if error then goto, finish
;		*(*pstate).pconfig = config
;		end
		
	'template-corrections-button': begin
		F = file_requester( /read, title='Select a template corrections DAI', path=(*pstate).output_dir, file=(*pstate).template_corrections_dai, $
							filter='*.dai', group=event.top )
		if F[0] eq '' then goto, finish
		(*pstate).template_corrections_dai = F[0]
		set_widget_text, (*pstate).template_corrections_text, F[0]

		table = wizard_batch_load_corr( pstate, title=title, type=type, element=element, error=error)
		if error then begin
;			warning,'wizard_batch_event','No raw files found.'		
			goto, finish
		endif
		*(*pstate).pcorr = table
		*(*pstate).pctitle = title
		*(*pstate).pctype = type
;		*(*pstate).pcel = element				; now set from template DAI
		wizard_batch_update_ctable, pstate

		(*pstate).options_process[0] = 1		; enable corrections
		widget_control, (*pstate).options_process_id, set_value=(*pstate).options_process
		end
	
	'template-rgb-button': begin
		F = file_requester( /read, title='Select an RGB export "Learn" file', path=(*pstate).output_dir, file=(*pstate).template_sort_dai, $
							filter='*.rgb.csv', group=event.top )
		(*pstate).template_rgb_export = F[0]
		set_widget_text, (*pstate).template_rgb_text, F[0]
		(*pstate).options_export[6] = 0
		widget_control, (*pstate).options_export_id, set_value=(*pstate).options_export
		if F[0] eq '' then begin
			*(*pstate).prgb = ''
			wizard_batch_update_rgb_table, pstate
			goto, finish
		endif

		table = wizard_batch_load_rgb( pstate, title=title, type=type, error=error)
		if error then goto, finish
		*(*pstate).prgb = table
		*(*pstate).prtitle = title
		*(*pstate).prtype = type
		wizard_batch_update_rgb_table, pstate

		(*pstate).options_export[6] = 1			; enable RGB exports
		widget_control, (*pstate).options_export_id, set_value=(*pstate).options_export
		end
		
	'template-rgb-text': begin
		widget_control, event.id, get_value=F
		(*pstate).template_rgb_export = F
		set_widget_text, (*pstate).template_rgb_text, F[0]
		(*pstate).options_export[6] = 0
		widget_control, (*pstate).options_export_id, set_value=(*pstate).options_export
		if F[0] eq '' then begin
			*(*pstate).prgb = ''
			wizard_batch_update_rgb_table, pstate
			goto, finish
		endif

		table = wizard_batch_load_rgb( pstate, title=title, type=type, error=error)
		if error then goto, finish
		*(*pstate).prgb = table
		*(*pstate).prtitle = title
		*(*pstate).prtype = type
		wizard_batch_update_rgb_table, pstate

		(*pstate).options_export[6] = 1			; enable RGB exports
		widget_control, (*pstate).options_export_id, set_value=(*pstate).options_export
		end
					
	'corrections-table': begin
		case tag_names( event, /structure_name) of
			'WIDGET_TABLE_CELL_SEL': begin
				(*pstate).csel.left = event.sel_left
				(*pstate).csel.top = event.sel_top
				(*pstate).csel.right = event.sel_right
				(*pstate).csel.bottom = event.sel_bottom
				help, (*pstate).csel
				end
				
			'WIDGET_TABLE_TEXT_SEL': begin
				help,event,/str
				end
			else:
		endcase
		end

	'corrections-mode': begin
		(*pstate).correction_mode = event.index
		end
	
	'corrections-element': begin
		(*pstate).correction_element_mode = event.index
		end
	
	'ctable-add-button': begin
		if typevar(*(*pstate).pcel) ne 'STRING' then begin
			warning,'Batch Wizard','Need to load a template DAI file on tab 1 first.'
			return
		endif
		el = (*(*pstate).pcel)[ (*pstate).correction_element_mode]
		op = (*pstate).uv.list[ (*pstate).correction_mode]
		if strmid(op,0,1) eq '*' then op = op + ' [' + el + ']'
		t = *(*pstate).pcorr
		if typevar(t) ne 'STRUCT' then begin
			t = [{el:el, history:op, bottom:0., top:100., log:0}]
		endif else begin
			t = [t, {el:el, history:op, bottom:0., top:100., log:0}]
		endelse
		*(*pstate).pcorr = t
		wizard_batch_update_ctable, pstate

		(*pstate).options_process[0] = 1		; enable corrections
		widget_control, (*pstate).options_process_id, set_value=(*pstate).options_process
		end
	
	'ctable-delete-button': begin
		t = *(*pstate).pcorr
		n = n_elements(t)
		if n eq 0 then return
		if typevar(t) ne 'STRUCT' then return
		i = clip( (*pstate).csel.top, 0,n-1)
		if (i eq 0) then begin
			t = t[1:n-1]
		endif else if (i gt 0) and (i lt n-1) then begin
			t = [t[0:i-1], t[i+1:n-1]]
		endif else if (i eq n-1) then begin
			t = t[0:i-1]
		endif
		*(*pstate).pcorr = t
		wizard_batch_update_ctable, pstate
		end
	
	'ctable-clear-button': begin
		*(*pstate).pcorr = ''
		wizard_batch_update_ctable, pstate

		(*pstate).options_process[0] = 0		; enable corrections
		widget_control, (*pstate).options_process_id, set_value=(*pstate).options_process
		(*pstate).template_corrections_dai = ''
		set_widget_text, (*pstate).template_corrections_text, (*pstate).template_corrections_dai
		end

	'display-apply-button': begin
		t = *(*pstate).pcorr
		n = n_elements(t)
		if n eq 0 then return
		if typevar(t) ne 'STRUCT' then return
		widget_control, (*pstate).display_bottom_text, get_value=bot
		widget_control, (*pstate).display_top_text, get_value=top
		widget_control, (*pstate).display_log_text, get_value=log
		i = (*pstate).csel.top
		el = ((*(*pstate).pcorr)[i]).el
		q = where( t.el eq el, nq)
		if nq eq 0 then goto, finish
		for j=0,nq-1 do begin
			t[q[j]].bottom = clip( fix2(bot), 0,99)
			t[q[j]].top = clip( fix2(top), 1,100)
			t[q[j]].log = clip( fix2(log), 0,2)
		endfor
		*(*pstate).pcorr = t
		wizard_batch_update_ctable, pstate

		(*pstate).options_process[0] = 1		; enable corrections
		widget_control, (*pstate).options_process_id, set_value=(*pstate).options_process
		end
	
	'rgb-table': begin
		case tag_names( event, /structure_name) of
			'WIDGET_TABLE_CELL_SEL': begin
				(*pstate).rsel.left = event.sel_left
				(*pstate).rsel.top = event.sel_top
				(*pstate).rsel.right = event.sel_right
				(*pstate).rsel.bottom = event.sel_bottom
				help, (*pstate).rsel
				end
				
			'WIDGET_TABLE_TEXT_SEL': begin
				help,event,/str
				end
			else:
		endcase
		end

	'rgb-r-element': begin
		(*pstate).r_element_mode = event.index
		end
	
	'rgb-g-element': begin
		(*pstate).g_element_mode = event.index
		end
	
	'rgb-b-element': begin
		(*pstate).b_element_mode = event.index
		end
	
	'rgb-add-button': begin
		if typevar(*(*pstate).pcel) ne 'STRING' then begin
			warning,'Batch Wizard','Need to load a template DAI file on tab 1 first.'
			return
		endif
		r = (*(*pstate).pcel)[(*pstate).r_element_mode]
		g = (*(*pstate).pcel)[(*pstate).g_element_mode]
		b = (*(*pstate).pcel)[(*pstate).b_element_mode]
		widget_control, (*pstate).rgb_zoom_text, get_value=s
		izoom = clip( fix2(s), -5,2)
		t = *(*pstate).prgb
		if typevar(t) ne 'STRUCT' then begin
			t = [{r:r, g:g, b:b, zoom:izoom}]
		endif else begin
			t = [t, {r:r, g:g, b:b, zoom:izoom}]
		endelse
		*(*pstate).prgb = t
		wizard_batch_update_rgb_table, pstate

		(*pstate).options_export[6] = 1			; enable RGB exports
		widget_control, (*pstate).options_export_id, set_value=(*pstate).options_export
		end

	'rgb-delete-button': begin
		t = *(*pstate).prgb
		n = n_elements(t)
		if n eq 0 then return
		if typevar(t) ne 'STRUCT' then return
		i = clip( (*pstate).rsel.top, 0,n-1)
		if (i eq 0) then begin
			t = t[1:n-1]
		endif else if (i gt 0) and (i lt n-1) then begin
			t = [t[0:i-1], t[i+1:n-1]]
		endif else if (i eq n-1) then begin
			t = t[0:i-1]
		endif
		*(*pstate).prgb = t
		wizard_batch_update_rgb_table, pstate
		end

	'rgb-clear-button': begin
		*(*pstate).prgb = ''
		wizard_batch_update_rgb_table, pstate

		(*pstate).options_export[6] = 0			; enable RGB exports
		widget_control, (*pstate).options_export_id, set_value=(*pstate).options_export
		end

	'rgb-save-button': begin
		t = *(*pstate).prgb
		n = n_elements(t)
		if typevar(t) ne 'STRUCT' then return
		if n eq 0 then begin
			warning,'wizard_batch_event','No RGB images lists to save.'
			return
		endif
		F = file_requester( /write, filter=['*.rgb.csv'], group=event.top, path=*(*pstate).path, $
					title='Save RGB image display list to a file', /fix_filter)
		if F eq '' then return
		F = strip_file_ext(F,/double) + '.rgb.csv'
	
		wizard_batch_save_rgb, pstate, F[0]
		end

	'options-file': begin
		(*pstate).options_file[event.value] = event.select
		widget_control, (*pstate).options_file_id, set_value=(*pstate).options_file
		end

	'options-process': begin
		(*pstate).options_process[event.value] = event.select
		widget_control, (*pstate).options_process_id, set_value=(*pstate).options_process
		end

	'options-export': begin
		(*pstate).options_export[event.value] = event.select
		if event.value eq 0 then begin
			(*pstate).options_export[1] = 0
		endif
		if event.value eq 1 then begin
			(*pstate).options_export[0] = 0
		endif
		if event.value eq 3 then begin
			(*pstate).options_export[4] = 0
			(*pstate).options_export[5] = 0
		endif
		if event.value eq 4 then begin
			(*pstate).options_export[3] = 0
			(*pstate).options_export[5] = 0
		endif
		if event.value eq 5 then begin
			(*pstate).options_export[3] = 0
			(*pstate).options_export[4] = 0
		endif
		widget_control, (*pstate).options_export_id, set_value=(*pstate).options_export
		end

	'results-table': begin
;		help,event,/str
		case tag_names( event, /structure_name) of
			'WIDGET_TABLE_CELL_SEL': begin
				(*pstate).sel.left = event.sel_left
				(*pstate).sel.top = event.sel_top
				(*pstate).sel.right = event.sel_right
				(*pstate).sel.bottom = event.sel_bottom
				i = (*pstate).sel.top < (n_elements( *(*pstate).presults) - 1)
				ibot = (*pstate).sel.bottom < (n_elements( *(*pstate).presults) - 1)
				(*pstate).sel.bottom = ibot
				if i lt 0 then goto, finish
			
;				print,'re-load table ...'
;				print, (*pstate).sel
				view = widget_info( (*pstate).results_table, /table_view)
				widget_control, (*pstate).results_table, get_value=t
				widget_control, (*pstate).results_table, set_value=t
				widget_control, (*pstate).results_table, set_table_select=[(*pstate).sel.left,(*pstate).sel.top,(*pstate).sel.right,ibot]
				widget_control, (*pstate).results_table, set_table_view=view
				
				i = (*pstate).sel.top
				j = (*pstate).sel.left
				if (i lt 0) or (j lt 0) then goto, finish
				q = where( (*(*pstate).pheadings)[j] eq *(*pstate).ptitle, nq)		; Find heading in list of tag names
				if nq gt 0 then begin												; for row struct of results table.
					k = q[0]														; q[0] is then tag index in struct.
					if ((*(*pstate).ptype)[k] eq 'file') and ((event.sel_right-j eq 0) and (event.sel_bottom-i eq 0)) then begin
						name = (*(*pstate).ptitle)[k]
						if name eq 'Raw' then begin
							DevObj = *(*pstate).pDevObj
							F = file_requester( /read, filter = '*'+DevObj->extension(), group=event.top, file=(*p)[i].(k), $
								title='Select the Raw data file', fix_filter=0, cancel=cancel, $
								numeric=(DevObj->multi_files() and DevObj->extension() eq '') )
						endif else begin
							f = file_requester( /read, filter=['*.'+strlowcase(name)+'.var','*.txt','*.'+strlowcase(name)], /fix_filter, $
								file=(*p)[i].(k), group=event.top, title='Select '+name+' File', cancel=cancel)
						endelse
						if cancel eq 0 then begin
							(*p)[i].(k) = f[0]
							wizard_batch_update_table, pstate
						endif
					endif
				endif
				end
				
			'WIDGET_TABLE_CH': begin
				if (event.ch eq 13B) or (event.ch eq 10B) then begin				; <cr> after edit cell
					if no_data eq 0 then begin										; ignore if now rows loaded
						widget_control, (*pstate).results_table, get_value=t
						i = (*pstate).sel.top
						if i lt 0 then goto, finish
						for j=3,(*pstate).columns-1 do begin
							q = where( (*(*pstate).pheadings)[j] eq *(*pstate).ptitle, nq)	; Find heading in list of tag names
							if nq gt 0 then begin									; for row struct of results table.
								k = q[0]											; q[0] is then tag index in struct.
								err = 0
								case (*(*pstate).ptype)[k] of
									'string': begin
										(*p)[i].(k) = t[j,i]
										end
									'int': begin
										(*p)[i].(k) = fix2(t[j,i], error=err)
										end
									'long': begin
										(*p)[i].(k) = long2(t[j,i], error=err)
										end
									'float': begin
										(*p)[i].(k) = float2(t[j,i], error=err)
										end
									'double': begin
										(*p)[i].(k) = double2(t[j,i], error=err)
										end
									else:
								endcase
								if err then begin
									warning,'wizard_batch_event','Illegal character for "'+(*(*pstate).ptype)[k]+'" in "'+(*(*pstate).ptitle)[k]+'" on row '+str_tidy(i)
									return
								endif
							endif
						endfor
					endif
					wizard_batch_update_table, pstate
					if ((*pstate).sel.top lt (*pstate).rows-1) then begin
						view = widget_info( (*pstate).results_table, /table_view)
						(*pstate).sel.left = (*pstate).sel.left
						(*pstate).sel.right = (*pstate).sel.left
						(*pstate).sel.top = (*pstate).sel.top+1
						(*pstate).sel.bottom = (*pstate).sel.top > (*pstate).sel.bottom
						widget_control, (*pstate).results_table, set_table_select=[(*pstate).sel.left,(*pstate).sel.top,(*pstate).sel.right,(*pstate).sel.bottom]
						widget_control, (*pstate).results_table, set_table_view=view
						widget_control, (*pstate).results_table, edit_cell=[(*pstate).sel.top,(*pstate).sel.left]
					endif
				endif
				end
			'WIDGET_TABLE_TEXT_SEL': begin
				end
			else:
		endcase
		end

	'table-fill-button': begin
		if no_data then goto, finish
		np = n_elements(*p)
		if (((*pstate).sel.top le np-1) and ((*pstate).sel.bottom ge (*pstate).sel.top)) and  $
						((*pstate).sel.left le (*pstate).sel.right) then begin

			widget_control, (*pstate).results_table, get_value=t
			i1 = (*pstate).sel.top
			i2 = np-1
			if (*pstate).sel.bottom gt (*pstate).sel.top then i2=(*pstate).sel.bottom
			for j=(*pstate).sel.left,(*pstate).sel.right do begin
				q = where( (*(*pstate).pheadings)[j] eq *(*pstate).ptitle, nq)	; Find heading is list of tag names
				if nq gt 0 then begin											; for row struct of results table.
					k = q[0]													; q[0] is then tag index in struct.
					for i=i1,i2 do begin
						(*p)[i].(k) = (*p)[i1].(k)
					endfor
				endif
			endfor
			wizard_batch_update_table, pstate
		endif
		end

	'table-delete-button': begin
		if no_data then goto, finish
		np = n_elements(*p)
		if ((*pstate).sel.top ge 0) and ((*pstate).sel.bottom lt np) then begin
			n = (*pstate).columns
			ns = -1
			if (*pstate).sel.top eq 0 then begin
				if (*pstate).sel.bottom eq np-1 then begin
					*p = ptr_new()
				endif else begin
					*p = (*p)[(*pstate).sel.bottom+1:np-1]
					ns = 0
				endelse
			endif else begin
				t = (*p)[0:(*pstate).sel.top-1]
				if (*pstate).sel.bottom lt np-1 then begin
					t = [t,(*p)[(*pstate).sel.bottom+1:np-1]]
					ns = (*pstate).sel.top
				endif else begin
					ns = (*pstate).sel.top-1
				endelse
				*p = t
			endelse
		endif
		wizard_batch_update_table, pstate
		end

	'table-enable-button': begin
		if no_data then goto, finish
		np = n_elements(*p)
		if ((*pstate).sel.top ge 0) and ((*pstate).sel.bottom lt np) then begin
			for i=(*pstate).sel.top,(*pstate).sel.bottom do begin
				if (*p)[i].on eq 1 then begin						; was "On"
					(*p)[i].on = 0
				endif else if (*p)[i].on eq 0 then begin			; was "Off"
					(*p)[i].on = 1
				endif else if (*p)[i].on eq 2 then begin			; was "Done"
					(*p)[i].on = 0
				endif else if (*p)[i].on eq 3 then begin			; was "Error"
					(*p)[i].on = 0
				endif
			endfor
		endif
		wizard_batch_update_table, pstate
		end

	'table-clear-button': begin
		if no_data then goto, finish
		*p = 0
		wizard_batch_update_table, pstate
		(*pstate).sel.top = -1
		(*pstate).sel.bottom = -1
		end

	'process-button': begin
		(*pstate).first = 1
		wizard_batch_process_blog, pstate, error=error
		end

	'abort-button': begin
		(*pstate).abort = 1
		end
	
;	'detector-mode': begin
;		n = n_elements(*(*pstate).detector_list)
;		if n lt 1 then goto, finish
;		(*pstate).detector_mode = event.index < (n-1)
;		wizard_batch_update_plots, pstate
;		end

;	'detector-new-mode': begin
;		n = n_elements(*(*pstate).detector_list)
;		if n lt 1 then goto, finish
;		(*pstate).detector_new_mode = event.index < (n-1)
;		wizard_batch_update_plots, pstate
;		end

	'back-button': begin
		(*pstate).tab = clip( (*pstate).tab-1, 0, n_elements((*pstate).tab_names)-1)
		widget_control, (*pstate).tab_panel, set_tab_current=(*pstate).tab
		wizard_batch_update_info, pstate
		end
		
	'next-button': begin
		(*pstate).tab = clip( (*pstate).tab+1, 0, n_elements((*pstate).tab_names)-1)
		widget_control, (*pstate).tab_panel, set_tab_current=(*pstate).tab
		wizard_batch_update_info, pstate
		end
		
	'figure-button': begin
		wizard_batch_update_info, pstate, /force
		end
		
	else:
endcase

finish:
	widget_control, hourglass=0
	return

abort_run:
	(*pstate).abort = 0										; abort
	(*pstate).busy = 0
	wizard_batch_update_table, pstate
	goto, finish

done:
	goto, kill

bad_state:
	warning,'wizard_batch_event',['STATE variable has become ill-defined.','Abort Wizard.'],/error
	goto, kill
bad_ptr:
	warning,'wizard_batch_event',['Parameter structure variable has become ill-defined.','Abort Wizard.'],/error
	goto, kill

; Free memory and exit cleanly ...

kill:
;	heap_free, pstate, /verbose			; slow and not recommended

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).root) then ptr_free, (*pstate).root
;	if ptr_valid( (*pstate).pconfig) then ptr_free, (*pstate).pconfig
	if ptr_valid( (*pstate).presults) then ptr_free, (*pstate).presults
	if ptr_valid( (*pstate).ptitle) then ptr_free, (*pstate).ptitle

	if ptr_valid( (*pstate).ptype) then ptr_free, (*pstate).ptype
	if ptr_valid( (*pstate).pheadings) then ptr_free, (*pstate).pheadings
	if ptr_valid( (*pstate).pcheadings) then ptr_free, (*pstate).pcheadings
	if ptr_valid( (*pstate).prheadings) then ptr_free, (*pstate).prheadings
;	if ptr_valid( (*pstate).detector_list) then ptr_free, (*pstate).detector_list

	if ptr_valid( (*pstate).windows_open[0]) then ptr_free, (*pstate).windows_open

	if ptr_valid( (*pstate).pwizard1) then free_wizard, (*pstate).pwizard1
	if ptr_valid( (*pstate).pwizard2) then free_wizard, (*pstate).pwizard2
	if ptr_valid( (*pstate).pwizard3) then free_wizard, (*pstate).pwizard3
	if ptr_valid( (*pstate).pwizard4) then free_wizard, (*pstate).pwizard4
	if ptr_valid( (*pstate).pwizard5) then free_wizard, (*pstate).pwizard5

	for i=0,n_elements((*pstate).parray)-1 do begin
		if ptr_valid( (*pstate).parray[i]) then free_wizard, (*pstate).parray[i]
	endfor

	if ptr_valid( (*pstate).parg1) then ptr_free, (*pstate).parg1
	if ptr_valid( (*pstate).parg2) then ptr_free, (*pstate).parg2
	if ptr_valid( (*pstate).parg3) then ptr_free, (*pstate).parg3
	if ptr_valid( (*pstate).parg4) then ptr_free, (*pstate).parg4
	if ptr_valid( (*pstate).parg5) then ptr_free, (*pstate).parg5
	if ptr_valid( (*pstate).parg6) then ptr_free, (*pstate).parg6
	
die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;--------------------------------------------------------------------------

pro wizard_batch_callback_image_done, pstate, pep, error=error

; Callback to: After sort of raw data into images done
; 'pep'	event.pointer returned in Notify, points to wizard_notify struct

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
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
       warning,'wizard_batch_callback_image_done',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
;	if (*pep).error ne 0 then return
	pd = (*pep).pdata

	n = 0
	p = (*pstate).presults
	no_data = 1
	if ptr_good(p) eq 1 then begin
		if size( (*p)[0],/tname) eq 'STRUCT' then begin
			no_data = 0
			n = n_elements( *p)
		endif
	endif
	if no_data then return
	
;	Use 'pnew' new files struct to update any changed file-names in the table (anywhere?)
;	Free the struct when done.

	if ptr_good( (*pd).pnew) then begin
		pfiles = (*(*pd).pnew).pfiles
		if ptr_good(pfiles) then begin
			if (*pfiles).pileup.new ne '' then begin
				q = where( *(*pstate).ptitle eq 'Pileup', nq)
				if nq gt 0 then begin
					q = where( (*p)[*].pileup eq (*pfiles).pileup.old, nq)
					if nq gt 0 then (*p)[q].pileup = (*pfiles).pileup.new					
				endif
			endif
			if (*pfiles).throttle.new ne '' then begin
				q = where( *(*pstate).ptitle eq 'Throttle', nq)
				if nq gt 0 then begin
					q = where( (*p)[*].throttle eq (*pfiles).throttle.old, nq)
					if nq gt 0 then (*p)[q].throttle = (*pfiles).throttle.new					
				endif
			endif
			if (*pfiles).linear.new ne '' then begin
				q = where( *(*pstate).ptitle eq 'Linear', nq)
				if nq gt 0 then begin
					q = where( (*p)[*].linear eq (*pfiles).linear.old, nq)
					if nq gt 0 then (*p)[q].linear = (*pfiles).linear.new					
				endif
			endif
			if (*pfiles).dam.new ne '' then begin
				q = where( *(*pstate).ptitle eq 'DAM', nq)
				if nq gt 0 then begin
					q = where( (*p)[*].dam eq (*pfiles).dam.old, nq)
					if nq gt 0 then (*p)[q].dam = (*pfiles).dam.new					
				endif
			endif
		endif

		j = (*pstate).index
		stats = (*(*pd).pnew).stats
		charge =  (*(*pep).pdata).charge
		output =  (*(*pep).pdata).output

		(*p)[j].processed = stats.processed
		(*p)[j].valid = stats.valid
		(*p)[j].clipped = stats.clipped
		(*p)[j].bad_xy = stats.bad_xy
		(*p)[j].charge = charge
		(*p)[j].output = output
		
;		Note that setting (*p)[].on here is important, as this is used to determine the next 
;		row to process in 'wizard_batch_process_blog'. It must be set to something other
;		than 1 ("On"), or the row will get repeated endlessly.

		if (*pep).error then begin
			(*p)[j].on = 3				; Error
		endif else begin
			(*p)[j].on = 2				; Done
		endelse

		wizard_batch_update_table, pstate
		ptr_free, pfiles
	endif

	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_batch_callback_display_done, pstate, pep, error=error

; Callback to: After image display parameters set
; 'pep'	event.pointer returned in Notify, points to wizard_notify struct

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
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
       warning,'wizard_batch_callback_display_done',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return
	
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_batch_callback_corrections_done, pstate, pep, error=error

; Callback to: After image corrections done
; 'pep'	event.pointer returned in Notify, points to wizard_notify struct

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
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
       warning,'wizard_batch_callback_corrections_done',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return
	
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_batch_callback_save_done, pstate, pep, error=error

; Callback to: After image saves done
; 'pep'	event.pointer returned in Notify, points to wizard_notify struct

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
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
       warning,'wizard_batch_callback_save_done',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return
	
	error = 0
	return
end

;---------------------------------------------------------------------------------------------------

pro wizard_batch_callback_rgb_done, pstate, pep, error=error

; Callback to: After RGB exports done
; 'pep'	event.pointer returned in Notify, points to wizard_notify struct

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
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
       warning,'wizard_batch_callback_save_done',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return
	
	error = 0
	return
end

;---------------------------------------------------------------------------------------------------

function wizard_batch_find_config, pstate, row, error=error

; Look for this table row in the stds config rows.

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
       warning,'wizard_batch_find_config',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return, 0
    endif
endif

	error = 1
	f = ''

	j = wizard_batch_find_match( pstate, row.name, row.serial, row.detector, row.energy, error=error)

	if error eq 0 then begin
		senergy = strtrim(round(1000.*(*(*pstate).pconfig)[j].energy),2) + 'eV'
		f = (*pstate).resource_dir + (*(*pstate).pconfig)[j].detector + path_sep() +  $
				'standards' + path_sep() + senergy + path_sep() + (*(*pstate).pconfig)[j].dam
	endif
	
	return, f
end

;--------------------------------------------------------------------------

function wizard_batch_find_match, pstate, sample, serial, detector, energy, error=error

; Look for these details in the stds config rows.

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
       warning,'wizard_batch_find_match',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return, 0
    endif
endif

;	Config entries look like:
;		entries = {Name:'', Serial:'', areal:0.0, el:'', conc:0.0, detector:'', energy:0.0, dam:''}
;
;	Table rows look like:
;		row = {on:1, blog:blog[i], name:sample, serial:serial, detector:detector, energy:energy, xsize:xsize, ysize:ysize, $
;			pv:pv, gain:gain, el:el, pileup:pileup, throttle:throttle, conv:0.0, mean:0.0, error:0.0, sd:0.0, relsd:0.0}

	error = 1
	pc = (*pstate).pconfig
	if ptr_good(pc) eq 0 then return, -1
	
	q = where( ((*pc)[*].name eq sample) and (strupcase((*pc)[*].serial) eq strupcase(serial)) and $
					((*pc)[*].detector eq detector) and (abs((*pc)[*].energy - energy) lt 0.002), nq)

	if nq eq 0 then begin
		q = where( ((*pc)[*].serial eq serial) and $
					((*pc)[*].detector eq detector) and (abs((*pc)[*].energy - energy) lt 0.002), nq)
		if nq ge 1 then begin
			sample = (*pc)[q[0]].name
		endif
	endif
	error = (nq eq 0)
	return, q[0]
end

;--------------------------------------------------------------------------

function wizard_batch_output_file, pstate, blog, error=err
	
; Determine the matching output file or path appropriate for 'blog'.
; If 'blog' is a raw path, return the new path. If a file, return new DAI file name.

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
	       warning,'wizard_batch_output_file',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
	      return, ''
	    endif
	endif
	
	err = 1
	if n_elements(blog) eq 0 then return, ''
	DevObj = *(*pstate).pDevObj

	path = build_output_path( blog, (*pstate).output_dir, (*pstate).root)
;	f = path + strip_path( blog)

	T = strip_file_ext(blog)
	if DevObj->multi_files() and (DevObj->multi_char() ne '.') then begin
		T = strip_file_m( T, ending=DevObj->multi_char() + ((adc_offset_device(DevObj) eq -1) ? '0' : '1'))
	endif
	if DevObj->embed_detector() then begin
		m = locate_last( DevObj->multi_char(), T)		; "_" before detector number, after strip off sequence 0 above
		if m gt 0 then begin							; assumes now that mutli_char is before det# too.
			T = strmid( T,0,m)
		endif
	endif
	f = path + strip_path(T,/keep) + '.dai'
	err = 0
	
finish:
	return, f
end

;------------------------------------------------------------------------------------------

function wizard_batch_read_config, file, error=err
	
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
	       warning,'wizard_batch_read_config',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
	      return, ''
	    endif
	endif
	
	err = 1
	if n_elements(file) eq 0 then return, ''
	entries = {Name:'', Serial:'', areal:0.0, el:'', conc:0.0, detector:'', energy:0.0, dam:''}
	
	on_ioerror, bad_file
	openr, lun, file, /get_lun
	first = 1
	line = ''
	while eof(lun) eq 0 do begin
		readf, lun, line
		if (strmid( line, 0,1) eq '#') or (lenchr(line) eq 0) then continue
		str = strtrim( strsplit( line, ',', /extract, /preserve_null), 2 )
		ns = n_elements(str)
		if ns lt 8 then goto, bad_format
		error = 0

		entries.name = str[0]
		entries.serial = str[1]
		entries.areal = float2(str[2], error=error)
		entries.el = str[3]
		entries.conc = float2(str[4], error=error)
		entries.detector = str[5]
		entries.energy = float2(str[6], error=error) / 1000.
		entries.dam = str[7]
		if n_elements(config) eq 0 then begin
			config = entries
		endif else begin
			config = [config, entries]
		endelse
		if error then begin
			warning,'wizard_batch_read_config','Illegal character in "float" value in config file.'
		endif
	endwhile
	err = 0
finish:
	close_file, lun
	return, (n_elements(config) eq 0) ? 0 : config
	
bad_file:
	warning,'wizard_batch_read_config','Failed to open standards config file '+file
	return, 0
bad_format:
	warning,'wizard_batch_read_config','Bad format in standards config file '+file
	goto, finish
end

;--------------------------------------------------------------------------

function wizard_batch_scan_dir, pstate, title=title, type=type, error=error

; Scan selected raw dir for runs to populate the Table.

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
       warning,'wizard_batch_scan_dir',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return, 0
    endif
endif
if n_elements(silent) eq 0 then silent=0

	error = 1

	drop = ['Use all event/ run files','Filter numeric event file (run) names']
	help_drop = 'For numeric event file names, using run numbers, you can choose a range within min/max run numbers.'
	text = ['Low run number:','high run number:']
	map_text = [0,1]
	initial_text = ['','']
	help_text = ['Low event run number to include.','High event run number to include.']
	Help_default = 'For numeric event file names, using run numbers, you can choose a range within min/max run numbers.'
	r = options_popup( title='Event run number range option', text=text, initial_text=initial_text, help_text=help_text, map_text=map_text, $
				help_default=help_default, drop=drop, help_drop=help_drop, error=err)		; , min_xsize=200
	if err then return, 0
	
	use_range = r.drop[0]
	rmin = r.text[0] eq '' ? 0 : long2(r.text[0])
	rmax = r.text[1] eq '' ? 0 : long2(r.text[1])
	if rmax eq 0 then rmax = 10000000L

cont:
	p = scan_dir_evt( /image, (*pstate).blog_dir, dai_dir=(*pstate).output_dir, *(*pstate).pDevObj, ppath=(*pstate).path, proot=(*pstate).root, rmin=rmin, rmax=rmax, error=err)
	if err then return, 0

	nb = n_elements(p)
	if nb eq 0 then begin
		warning,'wizard_batch_scan_dir','No raw files found in - '+(*pstate).blog_dir
		return, 0
	endif
	
	widget_control, (*pstate).conv_text, get_value=s
	set_conv = 0
	if s eq '' then set_conv = 1

	for i=0,nb-1 do begin
		if set_conv and ((*p[i]).conv ne 0.0) then begin
			widget_control, (*pstate).conv_text, set_value=str_tidy((*p[i]).conv)
			set_conv = 0
		endif

		mp = get_header_info( *(*pstate).pDevObj, (*p[i]).file, error=err)	; output=output, silent=silent
		if err eq 0 then begin
			detector = mp.metadata.detector_identity
			serial = mp.metadata.sample_serial
			sample_type = mp.metadata.sample_type
;			if sample_type eq 'standard' then continue			; skip 'standard_type' = "standard"
		endif else begin
			detector = ''
			serial = ''
			sample_type = ''
		endelse

;		If a DAI was not found in scan and a template DAI is available, use that ...

		if ptr_good( (*pstate).pdai) then begin
			if ((*p[i]).linear eq '') and ( (*(*pstate).pdai).linearize ne '') then begin
				(*p[i]).linear = (*(*pstate).pdai).linearize
			endif
		endif

;		This defines the names and types of the columns of the table struct.
;		It must match the order of entries in the row struct of the table.
;		This is NOT the selection of the columns as shown. See "wizard_batch_update_table" for that.

		title = ['On','Raw','DAM', 'Name','Serial','Detector','Energy','Xpixels','Ypixels','Xsize', $
				'Ysize','PV name','IC Gain','Pileup','Throttle','Linear', 'Conv', 'Charge','Output','Mean','Error','Std.Dev', $
				'SD/Error','Processed','Clipped','Valid','Bad_xy']
		type = ['toggle','file','string','string','string','string','float','int','int','int', $
				'int','string','float','file','file','file', 'float','float','string','float','float','float', $
				'float','long64','long64','long64','long64']
		
		row = {on:1, blog:(*p[i]).file, dam:(*p[i]).dam, name:(*p[i]).sample, serial:serial, detector:detector, $
			energy:(*p[i]).energy, xpixels:(*p[i]).xrange, ypixels:(*p[i]).yrange, xsize:(*p[i]).xsize, $
			ysize:(*p[i]).ysize, pv:(*p[i]).pv, gain:(*p[i]).gain, $
			pileup:(*p[i]).pileup, throttle:(*p[i]).throttle, linear:(*p[i]).linear,$
			conv:(*p[i]).conv, charge:(*p[i]).charge, output:(*p[i]).output, mean:0.0, error:0.0, sd:0.0, relsd:0.0, $
			processed:0LL, clipped:0LL, valid:0LL, bad_xy:0LL}
		
		if wizard_batch_test_row( row) eq 0 then row.on=0

		if n_elements(table) lt 1 then begin
			table = row
		endif else begin
			table = [table, row]
		endelse		
	endfor
	(*pstate).busy = 0
	
	if n_elements(table) ge 1 then error = 0	
	return, (error ? 0 : table)
end

;--------------------------------------------------------------------------

pro wizard_batch_load_template, pstate, error=error

; Load the template DAI file and scan it for defaults for sorting.

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
       warning,'wizard_batch_load_template',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif
if n_elements(silent) eq 0 then silent=0

	error = 1
	p = read_geopixe_image( (*pstate).template_sort_dai, error=err)
	if err then return

	(*pstate).pdai = p
	nel = (*p).n_el
	if nel eq 0 then return
	*(*pstate).pcel = *(*p).el
	
	error = 0	
	return
end

;--------------------------------------------------------------------------

function wizard_batch_load_corr, pstate, title=title, type=type, element=element, error=error

; Load the template DAI file and scan it for image correction history to populate the Table.

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
       warning,'wizard_batch_load_corr',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return, 0
    endif
endif
if n_elements(silent) eq 0 then silent=0

	error = 1
	p = read_geopixe_image( (*pstate).template_corrections_dai, error=err)
	if err then return, 0

	nel = (*p).n_el
	if nel eq 0 then return, 0

	start = 1
	for i=0,nel-1 do begin

;		This defines the names and types of the columns of the Corr struct.
;		It must match the order of entries in the row struct of the corr table.
;		This is NOT the selection of the columns as shown. See "wizard_batch_update_ctable" for that.

		title = ['El','History','Bottom','Top','Log']
		type = ['string','string','int','int','int']
		element = *(*p).el
		
		phist = (*(*p).history)[i]
		found = 0
		first = 1
		if ptr_good(phist) eq 0 then goto, more_corr

		nhist = n_elements(*phist)
		for j=0,nhist-1 do begin
			s = hide_embedded( (*phist)[j], ' ')
			sub = strsplit( s, ',:()', /extract, count=n_sub)
			hist = sub[0]
			OK = 0

			if strlowcase(hist) eq 'plugin' then begin
				OK = 1
			endif else if hist eq 'inter-element' then begin
				OK = 1
			endif else begin

;				Look for other processing commands, as listed in the 'image_process' routine list.
;				"*" indicates an operation that gets applied to all element planes.
;				If one particular element must be displayed to do this, it is put in brackets "[]".
;				Else, this is done for element i=0.

				skip_el = 0
				if strmid(hist,0,1) eq '*' then begin
					l1 = locate('[',s)
					l2 = locate(']',s)
					if (l1 ge 0) and (l2 ge 0) and (l2 gt l1+1) then begin
						tag = strmid( s,l1+1,l2-l1-1)
						if (*(*p).el)[i] ne tag then skip_el=1
						hist = strmid( s,0,l1-1)
					endif else begin
						if not start then skip_el=1
					endelse
				endif
				OK = (skip_el eq 0)
			endelse

			if OK then begin
				row = {el:(*(*p).el)[i], history:s, bottom:(*(*p).options)[i].bottom, $
					top:(*(*p).options)[i].top, log:(*(*p).options)[i].log}
				found = 1
				if start then begin
					table = row
					start = 0
				endif else begin
					table = [table, row]
				endelse
				first = 0
			endif
		endfor
more_corr:
		if first and not found then begin
			row = {el:(*(*p).el)[i], history:'', bottom:(*(*p).options)[i].bottom, $
				top:(*(*p).options)[i].top, log:(*(*p).options)[i].log}
			if start then begin
				table = row
				start = 0
			endif else begin
				table = [table, row]
			endelse
			first = 0
		endif
	endfor
	
	if n_elements(table) ge 1 then error = 0	
	return, (error ? 0 : table)
end

;--------------------------------------------------------------------------

function wizard_batch_load_rgb, pstate, title=title, type=type, error=error

; Load the RGB Export file and scan it for export combinations to populate the Table.

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
       warning,'wizard_batch_load_rgb',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return, 0
    endif
endif
if n_elements(silent) eq 0 then silent=0

	error = 1
	if (*pstate).template_rgb_export eq '' then return, 0
		
	on_ioerror, bad_file
	openr, unit, (*pstate).template_rgb_export, /get_lun

	list = replicate( {r:'', g:'', b:'', zoom:0}, 200)
	s = ''
	on_ioerror, cont
	for i=0,199 do begin
		readf, unit, s
		str = strsplit( s, ', 	', /extract, count=ns)
		if ns eq 0 then goto, cont
		if ns lt 4 then begin
			warning,'wizard_batch_load_rgb','Bad file format.'
			goto, bad_file
		endif
		list[i].r = str[0]
		list[i].g = str[1]
		list[i].b = str[2]
		list[i].zoom = fix2(str[3])
	endfor
cont:
	q = where( list.r ne '', nq)
	if nq eq 0 then goto, bad_file
	list = list[q]
	*(*pstate).prgb = list
	close_file, unit

	start = 1
	for i=0,nq-1 do begin
		first = 1

;		This defines the names and types of the columns of the Corr struct.
;		It must match the order of entries in the row struct of the corr table.
;		This is NOT the selection of the columns as shown. See "wizard_batch_update_ctable" for that.

		title = ['R','G','B','Zoom']
		type = ['string','string','string','int']
		
		row = {R:list[i].r, G:list[i].g, B:list[i].b, Zoom:list[i].zoom}
		
		if n_elements(table) lt 1 then begin
			table = row
		endif else begin
			table = [table, row]
		endelse		
	endfor
	
	if n_elements(table) ge 1 then error = 0	
	return, (error ? 0 : table)

bad_file:
	close_file, unit
	return, 0
end

;--------------------------------------------------------------------------

function wizard_batch_test_row, row, error=error

; Test a table row for completeness.

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
       warning,'wizard_batch_test_row',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return, 0
    endif
endif

;	Table rows look like:
;		row = {on:1, blog:blog[i], name:sample, serial:serial, detector:detector, energy:energy, xsize:xsize, ysize:ysize, $
;			pv:pv, gain:gain, el:el, pileup:pileup, throttle:throttle, conv:0.0, mean:0.0, error:0.0, sd:0.0, relsd:0.0}

	error = 1

;	Removed (row.serial ne '') from this test ...
;	if (row.blog ne '') and (row.detector ne '') and (row.energy ne 0.) and   $		; from standards wizard
;			(row.gain ne 0.) and (row.el ne '') then begin

	if (row.blog ne '') then begin
		error = 0
		return, 1
	endif 
	
	error = 0
	return, 0
end

;--------------------------------------------------------------------------

function wizard_batch_ops, pstate, display=display, error=error

; Build list of 'ops' for image processing

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
       warning,'wizard_batch_ops',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return, 0
    endif
endif

	error = 1
	ops = 0
	pops = (*pstate).pcorr
	pcel = (*pstate).pcel
	uv = (*pstate).uv
	np = n_elements(*pops)
	if np eq 0 then return, 0

;	Build display settings list ...

	for i=0, np-1 do begin
		d = {el:(*pops)[i].el, bottom:(*pops)[i].bottom, top:(*pops)[i].top, log:(*pops)[i].log }
		if n_elements(display) eq 0 then begin
			display = d
		endif else begin
			display = [display, d]
		endelse
	endfor

;	Build ops list ...

	for k=0L,np-1 do begin
		i = (where( (*pops)[k].el eq *pcel))[0]
		if i eq -1 then continue

		image_build_op, i, *pcel, uv, (*pops)[k].history, ops
	endfor

	error = 0
	return, ops
end

;--------------------------------------------------------------------------

pro wizard_batch_process_setup, pstate, error=error

;	Send template DAI parameters to evt window.
;	They can be modified by user from there before processing starts.

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
	       warning,'wizard_batch_process_setup',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
		   (*pstate).busy = 0
	      return
	    endif
	endif

	error = 1
	if (*(*pstate).pdai).DevObj->name() ne (*(*pstate).pDevObj)->name() then begin
		warning,'wizard_batch_process_setup',['Device of template DAI does not match your Device droplist choice.', '', $
						'Change Device droplist or select another DAI file.']
		return
	endif

;	Maintain options agreement between 3 Device Object instances: 1/ One in 'pdai', 2/ one in *(*pstate).pDevObj,
;	and 3/ one in the Sort EVT window.

	options = (*(*pstate).pdai).DevObj->get_options()
	(*(*pstate).pDevObj)->set_options, options

	wz = define(/wizard_notify)
	wz.wizard = 'batch'
	wz.window = 'Sort EVT'								; Sort image
	wz.command = 'sort-setup'						
	wz.pdata = ptr_new( {	$
		device:			(*(*pstate).pDevObj)->name(), $	; device name
		device_options:	options, $						; internal device options in Sort EVT
		image_mode:		0, $							; sort mode (images)
		type:			(*(*pstate).pdai).detector, $	; data type
		array:			(*(*pstate).pdai).array, $		; array detector?
		blog:			(*(*pstate).pdai).source, $		; blog file
		pileup:			(*(*pstate).pdai).pileup, $		; pileup file
		throttle:		(*(*pstate).pdai).throttle, $	; throttle file
		linear:			(*(*pstate).pdai).linearize, $	; linearize file
		output:			(*(*pstate).pdai).file, $		; output file
		conv:			(*(*pstate).pdai).IC.conversion, $		; 'conv'
		charge:			(*(*pstate).pdai).charge, $		; for the returned charge
		charge_mode:	(*(*pstate).pdai).IC.mode, $	; flux/charge mode (IC w/ PV)
		flux_scaler: 	(*(*pstate).pdai).IC.pv.name, $	; scaler ID
		gain_value:		(*(*pstate).pdai).IC.pv.val, $	; IC gain
		gain_units:		(*(*pstate).pdai).IC.pv.unit, $	; for gain in 'nA/V' 
;		cal:			(*pstate).energy_cal_file, $	; energy calibration file
		proj_mode:		'DA', $							; DA projection mode
		dam:			(*(*pstate).pdai).matrix.file }, /no_copy)		; DA matrix file
	wz.local = 1
;	wz.callback = 'wizard_batch_callback_image_done'
	pw = ptr_new(wz, /no_copy)
	pl = pw									; new current one
	p0 = pw									; first one

	clear_wizard, (*pstate).pwizard1
	*(*pstate).pwizard5 = *p0
	ptr_free, p0
	notify, 'wizard-action', (*pstate).pwizard5	
	error = 0
	return
end
;--------------------------------------------------------------------------

pro wizard_batch_process_blog, pstate, error=error

; For a single raw file, do the following:
;	1. Sort the raw data to form the DAI image file
;	2. Apply any selected corrections to images.
;	3. Apply display ranges.
;	4. Save and export in various formats.
;	5. Export RGB images.

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
	       warning,'wizard_batch_process_blog',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
		   (*pstate).busy = 0
	      return
	    endif
	endif

	wizard_check_windows, pstate, error=error
	if error then begin
		(*pstate).busy = 0
		return
	endif

	error = 1
	if ptr_good( (*pstate).pdai) eq 0 then begin
		warning,'wizard_batch_process_blog',['Need to setup a Template DAI file for processing.', '', $
						'Use the "Template" button on tab 1.']
		(*pstate).busy = 0
		return
	endif

;	This checks for first row set to 1 ("On"), which depends on 'wizard_batch_callback_image_done' to 
;	set (*p)[j].on to 2 ("Done") or 3 (Error") (i.e. not 1 ("On")) after a sort completes.

	no_data = 1
	p = (*pstate).presults
	n = 0
	if ptr_good(p) eq 1 then begin
		if size( (*p)[0],/tname) eq 'STRUCT' then begin
			no_data = 0
			q = where( (*p)[*].on eq 1, n) 			; ignores 'Off', 'Done', 'Error'
		endif
	endif
	
	if n eq 0 then begin
		print,'	process_blog: no more to process.'
		(*pstate).busy = 0
		wizard_batch_update_summary, pstate
		wizard_batch_update_table, pstate
		return
	endif

	j = q[0]										; next row (assumes that last one was set to "Done" or "Error") on completion
	(*pstate).index = j
	print,'	process_blog: process index, raw =',j,'  ',(*p)[j].blog

;	Don't create output file name here. It WAS done in 'scan_dir_evt' already (properly), 
;	and users may edit it in the table.

;	output = wizard_batch_output_file( pstate, (*p)[j].blog, error=err)
;	if err then begin
;		warning,'wizard_batch_process_blog','Unable to form output file for raw: '+ strip_file_ext(strip_path((*p)[j].blog))
;		(*pstate).busy = 0
;		return
;	endif
	
;	Setup struct for Sort EVT ...

;	gain_value = charge_gain_units( (*p)[j].gain, units=gain_units)
;	if gain_value eq 0.0 then begin
;		warning,'wizard_batch_process_blog','Missing valid "IC Gain" value.'
;		return
;	endif
	widget_control, (*pstate).conv_text, get_value=s
	conv = 1.0
	if s ne '' then conv = float2(s)

	first = (*pstate).first
	(*pstate).first = 0

;	load = need to load any existing images (if not sorted afresh) so that corrections can
;	be applied or exports done.
;	save = save the resulting images as a fresh image (or over-written, if 'over' set).

	save = 0
	load = 0
	for k=0,n_elements( (*pstate).options_process)-1 do save = save or (*pstate).options_process[k]
	for k=0,n_elements( (*pstate).options_export)-1 do load = load or (*pstate).options_export[k]
	load = save or load
;	save = load

	wz = define(/wizard_notify)
	wz.wizard = 'batch'
	wz.window = 'Sort EVT'								; Sort image
	wz.command = 'sort-image'							; (or load if exists and 'load' or 'skip' set)
	wz.pdata = ptr_new( {	$
		device:			(*(*pstate).pDevObj)->name(), $	; device name
		image_mode:		0, $							; sort mode (images)
;		type:			(*(*pstate).pdai).detector, $	; data type
;		array:			(*(*pstate).pdai).array, $		; array detector?
		blog:			(*p)[j].blog, $					; blog file
		pileup:			(*p)[j].pileup, $				; pileup file
		throttle:		(*p)[j].throttle, $				; throttle file
		linear:			(*p)[j].linear, $				; linearize file
		output:			(*p)[j].output, $				; output file
		load:			load, $							; load (1) image file if exists
		skip:			(*pstate).options_file[1], $	; skip (1) sort if exists already
		verify:			1, $							; enable file verification
		pnew:			ptr_new(/allocate_heap), $		; pointer to new (/verify) file-names struct
		conv:			conv, $							; initial 'conv'
		charge:			(*p)[j].charge, $				; set charge, and for the returned charge
;		charge_mode:	(*(*pstate).pdai).IC.mode, $	; flux/charge mode (IC w/ PV)
;		flux_scaler: 	(*(*pstate).pdai).IC.pv.name, $	; scaler ID
;		gain_value:		(*(*pstate).pdai).IC.pv.val, $	; IC gain
;		gain_units:		(*(*pstate).pdai).IC.pv.unit, $	; for gain in 'nA/V' 
		cal:			(*pstate).energy_cal_file}, /no_copy)	; energy calibration file
;		proj_mode:		'DA', $							; DA projection mode
;		dam:			(*(*pstate).pdai).matrix.file }, /no_copy)		; DA matrix file
	wz.local = 1
	wz.callback = 'wizard_batch_callback_image_done'
	pw = ptr_new(wz, /no_copy)
	pl = pw									; new current one
	p0 = pw									; first one
	
;	Setup structs for Image Display and Processing

	do_ops = (*pstate).options_process[0]
	if do_ops then begin
		ops = wizard_batch_ops( pstate, display=display, error=error)
	
		if error then begin
			do_ops = 0
;			warning,'wizard_batch_process_blog','Error building display & operations lists for Image Operations.'
;			return
		endif
	endif

	over = (*pstate).options_file[0]			; overwrite original DAI option

	if do_ops then begin
		wz = define(/wizard_notify)
		wz.wizard = 'batch'
		wz.window = 'Image'						; do image corrections
		wz.command = 'image-corrections'
		wz.pdata = ptr_new( {	$
			ops:		ops, $					; corrections list
			overwrite:	over }, /no_copy)		; overwrite original DAI
		wz.local = 1
		wz.callback = 'wizard_batch_callback_corrections_done'
		pw = ptr_new(wz, /no_copy)
		(*pl).pnext = pw						; link current one to this 'next' one
		pl = pw									; current one

		if n_elements(display) ne 0 then begin
			wz = define(/wizard_notify)
			wz.wizard = 'batch'
			wz.window = 'Image'						; set display parameters
			wz.command = 'set-display'
			wz.pdata = ptr_new( display, /no_copy)	; display list
					
			wz.local = 1
			wz.callback = 'wizard_batch_callback_display_done'
			pw = ptr_new(wz, /no_copy)
			(*pl).pnext = pw						; link current one to this 'next' one
			pl = pw									; new current one
		endif

;		As the images have now been modified, add the suffix ...

		if over eq 0 then begin
			ext = extract_extension(output)
			output = strip_file_m( strip_file_ext(output), ending='-x') + '-x.' + ext
		endif
	endif

;	Setup Save options, including some misc corrections (from batch-sort) ...

;	flipx = 1 - (((*pstate).current_sort - (*pstate).first_sort) mod 2)
	flipx = 1 - (((*pstate).index - 0) mod 2)

	*(*pstate).parg1 = {first:first, save:save, html:'', bw:'', $
					export:'', overwrite:over, correctX:(*pstate).options_process[1], $
					mirrorX:(*pstate).options_process[2] and flipx, tiff:'', tiff_type:0, $
					rgb:(*pstate).options_export[6], metadata:(*pstate).options_export[7]}

	if (*pstate).options_export[0] then begin						; output HTML
		path = extract_path(output) + 'html' + path_sep()
		file = path + strip_file_ext(strip_path(output)) + '.html'
		(*(*pstate).parg1).html = file
	endif else if (*pstate).options_export[1] then begin			; output B/W HTML
		path = extract_path(output) + 'html' + path_sep() + 'bw' + path_sep()
		file = path + strip_file_ext(strip_path(output)) + '-bw.html'
		(*(*pstate).parg1).bw = file
	endif
	if (*pstate).options_export[2] then begin						; export CSV/TXT image(s)
		path = extract_path(output) + 'export' + path_sep()
		file = path + strip_file_ext(strip_path(output)) + '.csv'
		(*(*pstate).parg1).export = file
	endif
	if (*pstate).options_export[3] or (*pstate).options_export[4] or (*pstate).options_export[5] then begin			; export TIFF image(s)
		path = extract_path(output) + 'tiff' + path_sep()
		file = path + strip_file_ext(strip_path(output)) + '.html'
		(*(*pstate).parg1).tiff = file
		(*(*pstate).parg1).tiff_type = 0
		if (*pstate).options_export[4] then (*(*pstate).parg1).tiff_type = 1
		if (*pstate).options_export[5] then (*(*pstate).parg1).tiff_type = 2
	endif

	wz = define(/wizard_notify)
	wz.wizard = 'batch'
	wz.window = 'Image'						; save modified images
	wz.command = 'save-batch'
	wz.pdata = ptr_new( *(*pstate).parg1)	; save options
			
	wz.local = 1
	wz.callback = 'wizard_batch_callback_save_done'
	pw = ptr_new(wz, /no_copy)
	(*pl).pnext = pw						; link current one to this 'next' one
	pl = pw									; new current one

;	Setup to Save selected RGB images ...

	do_rgb = (*pstate).options_export[6]		; export RGB option

	if do_rgb then begin						; export RGB
		file = (*(*pstate).path) + 'temp.rgb.csv'
		wizard_batch_save_rgb, pstate, file, error=err
	
		if err eq 0 then begin
			wz = define(/wizard_notify)
			wz.wizard = 'batch'
			wz.window = 'Image RGB'				; save RGB images
			wz.command = 'save-rgb'
			wz.pdata = ptr_new( file)			; RGB export temp file
					
			wz.local = 1
			wz.callback = 'wizard_batch_callback_rgb_done'
			pw = ptr_new(wz, /no_copy)
			(*pl).pnext = pw					; link current one to this 'next' one
			pl = pw								; new current one
		endif
	endif

;	Send off the linked list to be actioned ...
;	The first wizard data pointer may still be in use, so we use a new one for the next row ...	

	(*pl).loop = 1								; last command, so check for loop to next
	(*pstate).busy = 1

	if (*pstate).index mod 2 then begin
		clear_wizard, (*pstate).pwizard1
		*(*pstate).pwizard1 = *p0
		ptr_free, p0
		notify, 'wizard-action', (*pstate).pwizard1	
	endif else begin
		clear_wizard, (*pstate).pwizard2
		*(*pstate).pwizard2 = *p0
		ptr_free, p0
		notify, 'wizard-action', (*pstate).pwizard2	
	endelse

	wizard_batch_update_table, pstate
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_batch_save_rgb, pstate, file, error=error

;	Save RGB settings (if defined) to a file

	COMPILE_OPT STRICTARR
	ErrorNo = 0
	common c_working_dir, geopixe_root
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
	       warning,'wizard_batch_save_rgb',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
	       return
	    endif
	endif
	error = 1

	if n_elements(file) eq 0 then return
	if file eq '' then return
	if ptr_good( (*pstate).prgb) eq 0 then return
	file = strip_file_ext( file,/double) + '.rgb.csv'

	on_ioerror, bad_file
	openw, unit, file, /get_lun

	list = *(*pstate).prgb
	n = n_elements(list)

	for i=0,n-1 do begin
		printf, unit, list[i].r, list[i].g, list[i].b, list[i].zoom, format='(A,",",A,",",A,",",I3)'
	endfor
	error = 0

bad_file:
	close_file, unit
	return
end

;--------------------------------------------------------------------------

pro wizard_batch_update_info, pstate, force=force

; Update info text and figure table

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
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
       warning,'wizard_batch_update_info',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif
if n_elements(force) eq 0 then force=0

	i = (*pstate).tab < (n_elements((*pstate).tab_names)-1)
	file = geopixe_root+'wizard/wizard_batch-' + (*pstate).tab_names[i] + '.txt'
	list = wizard_instructions_file( file, error=err)
	if err then begin
		print,'Wizard_batch: text file not found: '+file
	endif else begin
		widget_control, (*pstate).instructions_text, set_value=list
	endelse

	if force or ((*pstate).tab_used[i] eq 0) then begin
		file = geopixe_root + 'wizard/wizard_batch-' + (*pstate).tab_names[i] + '.png'
		figure, file, group=(*pstate).tlb, title='Batch Wizard - Figure '+str_tidy(i+1)
	endif
	(*pstate).tab_used[i] = 1
	return
end

;--------------------------------------------------------------------------

pro wizard_batch_update_summary, pstate

; Export the results summary table

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
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
       warning,'wizard_batch_update_summary',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, finish
    endif
endif

	no_data = 1
	p = (*pstate).presults
	n = 0
	if ptr_good(p) eq 1 then begin
		if size( (*p)[0],/tname) eq 'STRUCT' then begin
			no_data = 0
			n = n_elements( *p)
		endif
	endif
	if n eq 0 then return
	output = (*pstate).output_dir + 'wizard-batch-results.csv'
	
	on_ioerror, bad
	openw, 1, output
	printf, 1, 'Raw, Name, Serial, Detector, Energy, Xsize, Ysize, IC Gain, Conv, Charge, Pileup, Throttle, Linear, Processed, Clipped, Valid, Bad_xy, Output'
	for i=0,n-1 do begin
		printf, 1, (*p)[i].blog, (*p)[i].name, (*p)[i].serial, (*p)[i].detector, str_tidy((*p)[i].energy), $
				str_tidy((*p)[i].Xsize), str_tidy((*p)[i].Ysize), str_tidy((*p)[i].gain),  $
				str_tidy((*p)[i].conv), str_tidy((*p)[i].charge), (*p)[i].pileup, (*p)[i].throttle, (*p)[i].linear, $
				str_tidy((*p)[i].processed), str_tidy((*p)[i].clipped), str_tidy((*p)[i].valid), str_tidy((*p)[i].bad_xy), $
				(*p)[i].output, format='(17(A,","),A)' 
	endfor

finish:
	close_file, 1
	return
bad:
	warning,'wizard_batch_update_summary','Failed to open output file: '+output
	goto, finish
end

;--------------------------------------------------------------------------

;pro wizard_batch_update_plots, pstate
;
;; Update results table
;
;COMPILE_OPT STRICTARR
;ErrorNo = 0
;common c_working_dir, geopixe_root
;common c_errors_1, catch_errors_on
;if catch_errors_on then begin
;    Catch, ErrorNo
;    if (ErrorNo ne 0) then begin
;       Catch, /cancel
;       on_error, 1
;       help, calls = s
;       n = n_elements(s)
;       c = 'Call stack: '
;       if n gt 2 then c = [c, s[1:n-2]]
;       warning,'wizard_batch_update_plots',['IDL run-time error caught.', '', $
;          'Error:  '+strtrim(!error_state.name,2), $
;          !error_state.msg,'',c], /error
;       MESSAGE, /RESET
;       return
;    endif
;endif
;
;	no_data = 1
;	p = (*pstate).presults
;	n = 0
;	if ptr_good(p) eq 1 then begin
;		if size( (*p)[0],/tname) eq 'STRUCT' then begin
;			no_data = 0
;			n = n_elements( *p)
;		endif
;	endif
;	if n eq 0 then return
;	
;	conv = (*p).conv
;	q1 = where( (conv ne 0.), nq1)
;	if nq1 eq 0 then return
;	run = long(strip_file_ext(strip_path( (*p).blog)))
;	e = (*p).energy
;	el_code, (*p).el, el, z, shell, bad, error
;	eline = e_line( z, major_line( z, shell))
;
;	det0 = (*(*pstate).detector_list)[(*pstate).detector_mode]
;	det = (*(*pstate).detector_list)[(*pstate).detector_new_mode]
;	if (det0 ne '') and (det ne '') then begin
;		print,'Update_plots: Dets: ',det0,'  ',det
;		detector_update, present=det0, new=i, file=f
;		pdet0 = read_detector( f, error=error0)
;		if error0 then begin
;			warning, 'wizard_batch_update_plots','Error reading old Detectors file: '+det0, /error
;		endif
;		detector_update, present=det, new=i, file=f
;		pdet = read_detector( f, error=error)
;		if error then begin
;			warning, 'wizard_batch_update_plots','Error reading new Detectors file: '+det, /error
;		endif
;		 
;		if (error0 eq 0) and (error eq 0) then begin
;			eff0 = detector_efficiency( pdet0, null, eline, effective=aeff0, solid_angle=omega0)
;			eff = detector_efficiency( pdet, null, eline, effective=aeff, solid_angle=omega)
;			conv = conv * eff0/eff
;		endif
;	endif
;	
;	!x.title = 'Run #'
;	!y.title = '"Conv" Calibration Factor'
;	!p.title = ''
;	!p.charsize = 1.0
;	!p.thick = 1.0
;	!p.charthick = 1.0
;	xleg = 0.83
;	yleg = 0.16
;	dxleg = 0.02
;	dyleg = 0.05
;	wset, (*pstate).wid1
;	
;	dr = (max(run)-min(run))*0.03 > 2
;	xrange = [min(run)-dr, max(run)+dr]
;	yrange = [0.8*(min(conv[q1])>0),1.2*max(conv[q1])]
;	done = intarr(n)
;	q = indgen(n)
;	more = 1
;	count = 0
;	cols = ['green','red','yellow','orange','l.blue','violet','blue','grey','white','brown']
;	repeat begin
;		e1 = e[q[0]]
;		q0 = where( (e eq e1), nq0)
;		q1 = where( (e eq e1) and (conv ne 0.), nq1)
;		if count eq 0 then begin
;			plot, [run[0],run], [conv[0],conv], color=spec_colour('white'), ticklen=1.0, /nodata, $
;					xrange=xrange, yrange=yrange, xstyle=1,ystyle=1
;		endif
;		if nq1 gt 0 then begin
;			oplot, [run[q1[0]],run[q1]], [conv[q1[0]],conv[q1]], color=spec_colour(cols[count]), psym=-((count mod 7)+1)
;		endif
;		plots, xleg+[0,0],yleg+count*dyleg+[0.005,0.005],/norm, color=spec_colour(cols[count]), psym=((count mod 7)+1)
;		xyouts, xleg+dxleg, yleg+count*dyleg,/norm, str_tidy(e[q1[0]])
;		count = count+1
;		done[q0] = 1
;
;		q = where( done eq 0, more)
;	endrep until more eq 0
;	
;	q1 = where( (conv ne 0.) and (eline gt 0.), nq1)
;	if nq1 eq 0 then return
;
;	!x.title = 'Line Energy (keV)'
;	!y.title = '"Conv" Calibration Factor'
;	!p.title = ''
;	!p.charsize = 1.0
;	!p.thick = 1.0
;	!p.charthick = 1.0
;	xleg = 0.83
;	yleg = 0.16
;	dxleg = 0.02
;	dyleg = 0.05
;	wset, (*pstate).wid2
;	
;	dr = (max(eline)-min(eline))*0.03 > 0.5
;	xrange = [min(eline)-dr, max(eline)+dr]
;	yrange = [0.8*(min(conv[q1])>0),1.2*max(conv[q1])]
;	done = intarr(n)
;	q = indgen(n)
;	more = 1
;	count = 0
;	cols = ['green','red','yellow','orange','l.blue','violet','blue','grey','white','brown']
;	repeat begin
;		e1 = e[q[0]]
;		q0 = where( (e eq e1), nq0)
;		q1 = where( (e eq e1) and (conv ne 0.) and (eline gt 0.), nq1)
;		if count eq 0 then begin
;			plot, [eline[0],eline], [conv[0],conv], color=spec_colour('white'), ticklen=1.0, /nodata, $
;					xrange=xrange, yrange=yrange, xstyle=1,ystyle=1
;		endif
;		if nq1 gt 0 then begin
;			oplot, [eline[q1[0]],eline[q1]], [conv[q1[0]],conv[q1]], color=spec_colour(cols[count]), psym=-((count mod 7)+1)
;		endif
;		plots, xleg+[0,0],yleg+count*dyleg+[0.005,0.005],/norm, color=spec_colour(cols[count]), psym=((count mod 7)+1)
;		xyouts, xleg+dxleg, yleg+count*dyleg,/norm, str_tidy(e[q1[0]])
;		count = count+1
;		done[q0] = 1
;
;		q = where( done eq 0, more)
;	endrep until more eq 0
;	
;	return
;end

;--------------------------------------------------------------------------

;pro wizard_batch_update_stats, pstate
;
;; Update results table
;
;COMPILE_OPT STRICTARR
;ErrorNo = 0
;common c_working_dir, geopixe_root
;common c_errors_1, catch_errors_on
;if catch_errors_on then begin
;    Catch, ErrorNo
;    if (ErrorNo ne 0) then begin
;       Catch, /cancel
;       on_error, 1
;       help, calls = s
;       n = n_elements(s)
;       c = 'Call stack: '
;       if n gt 2 then c = [c, s[1:n-2]]
;       warning,'wizard_batch_update_stats',['IDL run-time error caught.', '', $
;          'Error:  '+strtrim(!error_state.name,2), $
;          !error_state.msg,'',c], /error
;       MESSAGE, /RESET
;       return
;    endif
;endif
;
;	no_data = 1
;	p = (*pstate).presults
;	n = 0
;	if ptr_good(p) eq 1 then begin
;		if size( (*p)[0],/tname) eq 'STRUCT' then begin
;			no_data = 0
;			n = n_elements( *p)
;		endif
;	endif
;
;case !version.os_family of
;	'MacOS': begin
;		ch_scale = 1.2
;		end
;	'unix': begin
;		ch_scale = 1.2
;		end
;	else: begin
;		ch_scale = 1.0
;		end
;endcase
;
;;	The heading labels chosen here need to be a subset of the 'title' strings for the tags in each
;;	row struct of the results table (see 'wizard_batch_scan_dir'), except "#", which is the row index.
;
;	rows = string(indgen(n>1))
;	headings = ['#', 'Raw','Name','Serial', 'Energy','El', 'Mean','Error','Std.Dev','SD/Error']
;	nc = n_elements(headings)
;	widths = [3, 7,5,9, 7,4, replicate(10,nc-6)] * !d.x_ch_size * ch_scale
;	t = strarr(nc,256)
;	
;	if no_data eq 0 then begin
;		for i=0,n-1 do begin
;			t[0,i] = str_tidy(i)									; first column is just index #
;			
;			for j=1,nc-1 do begin
;				q = where( headings[j] eq *(*pstate).ptitle, nq)	; Find heading in list of tag name 'titles'
;				if nq gt 0 then begin								; for row struct of results table.
;					k = q[0]										; q[0] is then tag index in struct.
;					case (*(*pstate).ptype)[k] of
;						'string': begin
;							t[j,i] = (*p)[i].(k)
;							end
;						'file': begin
;							t[j,i] = strip_file_ext( strip_path((*p)[i].(k)), /double)
;							end
;						'toggle': begin
;							t[j,i] = (*p)[i].(k) ? 'On' : 'Off'
;							end
;						'int': begin
;							t[j,i] = str_tidy((*p)[i].(k))
;							end
;						'long': begin
;							t[j,i] = str_tidy((*p)[i].(k))
;							end
;						'float': begin
;							t[j,i] = str_tidy((*p)[i].(k))
;							end
;						'double': begin
;							t[j,i] = str_tidy((*p)[i].(k))
;							end
;					endcase
;				endif
;			endfor
;		endfor	
;	endif
;	widget_control, (*pstate).stats_table, set_value=t, column_widths=widths, align=2, $
;						column_labels=headings, table_xsize=nc, table_ysize=n>1
;	widget_control, (*pstate).stats_table, use_table_select=[0,0,nc-1,n-1]
;	(*pstate).scolumns = nc
;	*(*pstate).psheadings = headings
;	(*pstate).srows = n
;	return
;end

;--------------------------------------------------------------------------

pro wizard_batch_update_ctable, pstate

; Update results table

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
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
       warning,'wizard_batch_update_ctable',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

	no_data = 1
	p = (*pstate).pcorr
	n = 0
	if ptr_good(p) eq 1 then begin
		if size( (*p)[0],/tname) eq 'STRUCT' then begin
			no_data = 0
			n = n_elements( *p)
		endif
	endif

case !version.os_family of
	'MacOS': begin
		ch_scale = 1.2
		end
	'unix': begin
		ch_scale = 1.25
		end
	else: begin
		ch_scale = 1.0
		end
endcase

;	The heading labels chosen here need to be a subset of the 'title' strings for the tags in each
;	row struct of the results table (see 'wizard_batch_scan_dir'), except "#", which is the row index.

	rows = string(indgen(n>1))
	headings = ['#', 'El', 'History', 'Bottom', 'Top', 'Log']
	nc = n_elements(headings)
	widths = [4,9, 40, 9,9,5] * !d.x_ch_size * ch_scale
	t = strarr(nc,256)
	toggle_modes = ['Off', 'On', 'Done', 'Error']

	if typevar(*(*pstate).pctitle) eq 'UNDEFINED' then begin	
		*(*pstate).pctitle = ['El','History','Bottom','Top','Log']
		*(*pstate).pctype = ['string','string','int','int','int']
	endif

	if no_data eq 0 then begin
		for i=0,n-1 do begin
			t[0,i] = str_tidy(i)									; first column is just index #
			
			for j=1,nc-1 do begin
				q = where( headings[j] eq *(*pstate).pctitle, nq)	; Find heading in list of tag name 'titles'
				if nq gt 0 then begin								; for row struct of results table.
					k = q[0]										; q[0] is then tag index in struct.
					case (*(*pstate).pctype)[k] of
						'string': begin
							t[j,i] = (*p)[i].(k)
							end
						'file': begin
							t[j,i] = strip_file_ext( strip_path((*p)[i].(k)), /double)
							end
						'toggle': begin
							t[j,i] = toggle_modes[(*p)[i].(k)]
							end
						'int': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
						'long': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
						'float': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
						'double': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
						'long64': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
					endcase
				endif
			endfor
		endfor	
	endif
	widget_control, (*pstate).corrections_table, set_value=t, column_widths=widths, align=2, $
						column_labels=headings, table_xsize=nc, table_ysize=n>1
	widget_control, (*pstate).results_table, use_table_select=[0,0,nc-1,n-1]
	(*pstate).ccolumns = nc
	*(*pstate).pcheadings = headings
	(*pstate).crows = n
	return
end

;----------------------------------------------------------------------

pro wizard_batch_update_rgb_table, pstate

; Update results table

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
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
       warning,'wizard_batch_update_rgb_table',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

	no_data = 1
	p = (*pstate).prgb
	n = 0
	if ptr_good(p) eq 1 then begin
		if size( (*p)[0],/tname) eq 'STRUCT' then begin
			no_data = 0
			n = n_elements( *p)
		endif
	endif

case !version.os_family of
	'MacOS': begin
		ch_scale = 1.2
		end
	'unix': begin
		ch_scale = 1.25
		end
	else: begin
		ch_scale = 1.0
		end
endcase

;	The heading labels chosen here need to be a subset of the 'title' strings for the tags in each
;	row struct of the results table (see 'wizard_batch_scan_dir'), except "#", which is the row index.

	rows = string(indgen(n>1))
	headings = ['#', 'R', 'G', 'B', 'Zoom']
	nc = n_elements(headings)
	widths = [4, replicate(10,4)] * !d.x_ch_size * ch_scale
	t = strarr(nc,256)
	toggle_modes = ['Off', 'On', 'Done', 'Error']

	if typevar(*(*pstate).prtitle) eq 'UNDEFINED' then begin	
		*(*pstate).prtitle = ['R','G','B','Zoom']
		*(*pstate).prtype = ['string','string','string','int']
	endif

	if no_data eq 0 then begin
		for i=0,n-1 do begin
			t[0,i] = str_tidy(i)									; first column is just index #
			
			for j=1,nc-1 do begin
				q = where( headings[j] eq *(*pstate).prtitle, nq)	; Find heading in list of tag name 'titles'
				if nq gt 0 then begin								; for row struct of results table.
					k = q[0]										; q[0] is then tag index in struct.
					case (*(*pstate).prtype)[k] of
						'string': begin
							t[j,i] = (*p)[i].(k)
							end
						'file': begin
							t[j,i] = strip_file_ext( strip_path((*p)[i].(k)), /double)
							end
						'toggle': begin
							t[j,i] = toggle_modes[(*p)[i].(k)]
							end
						'int': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
						'long': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
						'float': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
						'double': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
						'long64': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
					endcase
				endif
			endfor
		endfor	
	endif
	widget_control, (*pstate).rgb_table, set_value=t, column_widths=widths, align=2, $
						column_labels=headings, table_xsize=nc, table_ysize=n>1
	widget_control, (*pstate).rgb_table, use_table_select=[0,0,nc-1,n-1]
	(*pstate).rgb_columns = nc
	*(*pstate).prheadings = headings
	(*pstate).rrows = n
	return
end

;----------------------------------------------------------------------

pro wizard_batch_update_table, pstate

; Update results table

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
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
       warning,'wizard_batch_update_table',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

	no_data = 1
	p = (*pstate).presults
	n = 0
	if ptr_good(p) eq 1 then begin
		if size( (*p)[0],/tname) eq 'STRUCT' then begin
			no_data = 0
			n = n_elements( *p)
		endif
	endif

case !version.os_family of
	'MacOS': begin
		ch_scale = 1.2
		end
	'unix': begin
		ch_scale = 1.25
		end
	else: begin
		ch_scale = 1.0
		end
endcase

;	The heading labels chosen here need to be a subset of the 'title' strings for the tags in each
;	row struct of the results table (see 'wizard_batch_scan_dir'), except "#", which is the row index.

	rows = string(indgen(n>1))
	headings = ['#','On', 'Raw', 'Xpixels','Ypixels','Xsize','Ysize', 'Charge', 'Output', 'Pileup','Throttle','Linear']
	widths = [3,5, 12, replicate(7,2),replicate(7,2), 8, 41, replicate(20,3)] * !d.x_ch_size * ch_scale
	nc = n_elements(headings)
	t = strarr(nc,256)
	toggle_modes = ['Off', 'On', 'Done', 'Error']

	c = intarr(3,nc,256)
	c[0,*,*] = (spec_colour('white',/rgb))[0]						; table colours
	c[1,*,*] = (spec_colour('white',/rgb))[1]
	c[2,*,*] = (spec_colour('white',/rgb))[2]

	if no_data eq 0 then begin
		for i=0,n-1 do begin
			t[0,i] = str_tidy(i)									; first column is just index #
			if (*p)[i].on eq 3 then begin
				for l=0,nc-1 do c[*,l,i] = spec_colour('yellow',/rgb)
			endif

			if (*pstate).busy and (i eq (*pstate).index) then begin
				for l=0,nc-1 do c[*,l,i] = spec_colour('green',/rgb)
			endif
			for j=1,nc-1 do begin
				q = where( headings[j] eq *(*pstate).ptitle, nq)	; Find heading in list of tag name 'titles'
				if nq gt 0 then begin								; for row struct of results table.
					k = q[0]										; q[0] is then tag index in struct.
					case (*(*pstate).ptype)[k] of
						'string': begin
							t[j,i] = (*p)[i].(k)
							end
						'file': begin
							t[j,i] = strip_file_ext( strip_path((*p)[i].(k)), /double)
							end
						'toggle': begin
							t[j,i] = toggle_modes[(*p)[i].(k)]
							end
						'int': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
						'long': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
						'float': begin
							t[j,i] = str_tidy((*p)[i].(k), places=-2)
							end
						'double': begin
							t[j,i] = str_tidy((*p)[i].(k), places=-3)
							end
						'long64': begin
							t[j,i] = str_tidy((*p)[i].(k))
							end
					endcase
				endif
			endfor
		endfor	
	endif
	widget_control, (*pstate).results_table, set_value=t, column_widths=widths, align=2, $
						column_labels=headings, table_xsize=nc, table_ysize=n>1
	widget_control, (*pstate).results_table, use_table_select=[0,0,nc-1,n-1]
	widget_control, (*pstate).results_table, background_color=c
	(*pstate).columns = nc
	*(*pstate).pheadings = headings
	(*pstate).rows = n
	return
end

;----------------------------------------------------------------------

pro OnRealize_wizard_batch_device_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( pstate) then begin
	widget_control, wWidget, set_combobox_select=(*pstate).device

	DevObj = (*(*pstate).pDevObjList)[(*pstate).device]			; current device object
	*(*pstate).pDevObj = DevObj
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_wizard_batch_draw1, wWidget

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
		warning,'OnRealize_wizard_batch_draw1',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate
	
	widget_control, wWidget, get_value=wid
	(*pstate).wid1 = wid
	return
end

;----------------------------------------------------------------------

pro OnRealize_wizard_batch_draw2, wWidget

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
		warning,'OnRealize_wizard_batch_draw2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate
	
	widget_control, wWidget, get_value=wid
	(*pstate).wid2 = wid
	return
end

;----------------------------------------------------------------------

pro OnRealize_wizard_batch_instructions, wWidget

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
		warning,'OnRealize_wizard_batch_instructions',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate
	
	(*pstate).tab = 0
	wizard_batch_update_info, pstate
	return
end

;----------------------------------------------------------------------

pro OnRealize_wizard_batch_ctable, wWidget

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
		warning,'OnRealize_wizard_batch_ctable',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

;	wizard_batch_update_ctable, pstate
	return
end

;----------------------------------------------------------------------

pro OnRealize_wizard_batch_rgbtable, wWidget

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
		warning,'OnRealize_wizard_batch_rgbtable',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

;	wizard_batch_update_rgbtable, pstate
	return
end

;----------------------------------------------------------------------

pro OnRealize_wizard_batch_results_table, wWidget

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
		warning,'OnRealize_wizard_batch_results_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	wizard_batch_update_table, pstate
	return
end

;----------------------------------------------------------------------

;pro OnRealize_wizard_batch_stats_table, wWidget
;
;COMPILE_OPT STRICTARR
;ErrorNo = 0
;common c_errors_1, catch_errors_on
;if catch_errors_on then begin
;	Catch, ErrorNo
;	if (ErrorNo ne 0) then begin
;		Catch, /cancel
;		on_error, 1
;		help, calls = s
;		n = n_elements(s)
;		c = 'Call stack: '
;		if n gt 2 then c = [c, s[1:n-2]]
;		warning,'OnRealize_wizard_batch_stats_table',['IDL run-time error caught.', '', $
;				'Error:  '+strtrim(!error_state.name,2), $
;				!error_state.msg,'',c], /error
;		MESSAGE, /RESET
;		return
;	endif
;endif
;
;	top = tlb_id( wWidget)
;	child = widget_info( top, /child)
;	widget_control, child, get_uvalue=pstate
;
;	wizard_batch_update_stats, pstate
;	return
;end

;--------------------------------------------------------------------------

pro wizard_batch, debug=debug

; Wizard to batch process raw data into images using the Sort EVT window.
; 
; It calls some GeoPIXE routines directly and also uses the "Notify" mechanism to send
; global messages to designated GeoPIXE images. Using the 'global' approach means that
; we don't need to know the top-level-base ID of the window. 
; 
; However, it does mean that we should NOT have more than one instance of any window 
; open at a time, as ALL windows of that type will act on the requests. This may be 
; desirable at times, but incur wasteful execution and duplication at other times. 
; 
; We send a "wizard-action" Notify event, with an argument pointer pointing to a linked list
; of structs of this general form, created using "ev = define(/wizard_notify)":
; 
; 	{	wizard:		"wizard name", $	; originating wizard name
; 		window:		"window name", $	; specific name label for destination window
; 		command:	"command", $		; command to be executed by window
; 		qual1:		"qual1", $			; some extra qualifier or string data 1
; 		qual2:		"qual2", $			; some extra qualifier or string data 2
; 		error:		err, $				; error code for return only
;		top:		0L, $				; return 'event.top' in case of 'open-test'
;		pdata:		ptr_new(), $		; general data to be transferred
;		local:		1, $				; indicates 'pdata' managed (e.g. freed) by Wizard
;		callback:	'', $				; name of callback routine
;		pnext:		ptr_new() }			; next action ev in linked list
; 	
; Take care: It is best to use separate pointers and heap storage for each command sent,
; so that any returned pdata remains separated for each command.
;	
; Many GeoPIXE windows will receive these "wizard-action" Notify events, but only respond
; to ones directed to its "window name". 
; 
; By convention, the window sends a reply as a "wizard-return" Notify event returning the
; same event pointer, but with the error code filled in (0=OK). Optionally, the GeoPIXE
; routine may set the 'pdata' pointer to some return data.
;
; An exception is the 'wizard-return' to the 'open-test' Notify. To test the possibility that
; there are multiple windows of a type open, all windows MUST make a copy of the pointer
; contents, set (*pw).top to event.top and return this copy. Otherwise, they trample each
; other's 'top' setting if in the same source pointer struct.
; 
; Only the Wizard with the right "wizard name" will respond to the return event. It will
; execute the callback routine and then if 'pnext' is set, send a notify with 'pnext' as the
; pointer argument.		

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
common c_errors_1, catch_errors_on
if n_elements(debug) lt 1 then debug=0
catch_errors_on = 1							; enable error CATCHing
if debug then catch_errors_on = 0			; disable error CATCHing

wversion = '8.9d'							; wizard version

; Each wizard sav loads routines from GeoPIXE.sav, if GeoPIXE is not running.
; The GeoPIXE routines are NOT to be compiled into each wizard sav file.
;
; First set a catch to test whether "GeoPIXE.sav" can be found ...

Catch, ErrorNo								; GeoPIXE.sav only loaded if needed
if (ErrorNo ne 0) then begin
	Catch, /cancel
	
	found = 0
	file = 'GeoPIXE.sav'						; current dir is the runtime dir
	if file_test(file) eq 0 then begin
		file = '../GeoPIXE.sav'					; current dir in a subdir of runtime dir
		if file_test(file) eq 0 then begin
			a = dialog_message('wizard_batch: Failed to restore GeoPIXE.sav.',/error)
		endif else found=1
	endif else found = 1
	if found then begin
		restore, file
		print,'"GeoPIXE.sav" restored.'
		geopixe
	endif else begin
		a = dialog_message(['GeoPIXE is not loaded in memory.','No "GeoPIXE.sav" file found locally.','Abort Wizard.','', $
				'Check that your working directory is the main "geopixe" dir.'], /error)
		return
	endelse
endif
test_geopixe_loaded						; tests whether GeoPIXE.sav routines loaded
Catch, /cancel							; this is a new feature of GeoPIXE v7.0e onwards

;.................................................................................

; Now establish the normal error catching for the wizard

if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'wizard_batch',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif else on_error,0

;.................................................................................

startupp, /colours, /database			; setup GeoPIXE defaults, database
register_notify							; notification routines
version = geopixe_version()				; GeoPIXE Version text

default = geopixe_defaults(source='wizard_batch')
config = default.path.config
detector_update, list=detector_list, title=detector_title

; List the names of the windows needed, in the format of their 'wizard-action' Notify
; window name. The "open-test" Notify message will be sent to these windows periodically
; to check on their 'open' status. They MUST make a copy of the Notify pointer contents,
; set (*pw).top of the copy to 'event.top' and return the new pw. 

windows_needed = ['Image','Sort EVT','Image Operations','Image RGB']

case !version.os_family of
	'MacOS': begin
		space1 = 1
		space2 = 2
		space5 = 5
		space10 = 10
		space15 = 15
		left_xsize = 700
		left_ysize = 600
		right_xsize = 400
		right_ysize = left_ysize + 36
		right_ylines = 28
		text_xsize = 580
		button_xsize = 100
		button_xsize1 = 50
		button_xsize2 = 170
		help_xsize = left_xsize + right_xsize + 55
		ch_scale = 1.2
		retain = 2
		end
	'unix': begin
		space1 = 1
		space2 = 2
		space5 = 5
		space10 = 10
		space15 = 15
		left_xsize = 800
		left_ysize = 700
		right_xsize = 400
		right_ysize = left_ysize + 36
		right_ylines = 28
		text_xsize = 580
		button_xsize = 100
		button_xsize1 = 50
		button_xsize2 = 170
		help_xsize = left_xsize + right_xsize + 55
		ch_scale = 1.25
		retain = 2
		end
	else: begin
		space1 = 1
		space2 = 2
		space5 = 5
		space10 = 10
		space15 = 15
		left_xsize = 700
		left_ysize = 600
		right_xsize = 310
		right_ysize = left_ysize + 36
		right_ylines = 28
		text_xsize = 600
		button_xsize = 70
		button_xsize1 = 50
		button_xsize2 = 170
		help_xsize = left_xsize + right_xsize + 55
		ch_scale = 1.0
		retain = 1
		end
endcase

tracking = 1				; enable tracking and context help for widgets
xoffset = 0					; and Help field to shows these help comments
yoffset = 0
		
left_resize = 0.7			; fraction of resize changes to use for left and right column widgets
right_resize = 0.3

;	Device object list ...

define_devices, titles=device_titles, names=device_names
DevObjList = instance_device_objects( device_names, error=err)
if err then begin
	warning,'wizard_batch',['Failed to open Device Objects.','Missing "xxx_device__define.sav" files in "/interface" ?']
	return
endif
if obj_valid(DevObjList[0]) eq 0 then begin
	warning,'wizard_batch',['Failed to open Device Objects.','Obj array invalid.']
	return
endif
device_initial = "MAIA_DEVICE"
device = 0
q = where( device_initial eq device_names, nq)
if nq ne 0 then device = q[0]
image_process, return_list=uv

; 	top-level base

tlb = widget_base( /column, title='Batch Processing Wizard ' + wversion + ' (GeoPIXE '+version+')', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='wizard-batch-tlb', /TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2 ,xoffset=xoffset, $
					yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

lbase = widget_base( tbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

tab_panel = widget_tab( lbase, location=2, /align_center, uname='wizard-batch-tab-panel')
tab_names = ['input','corrections','rgb','options','table']

; Files and paths -----------------------------------------

file_base = widget_base( tab_panel, title=' 1. User Input  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize, uvalue={xresize:left_resize,yresize:1})
label = widget_label( file_base, value='Select raw data directory')
text = widget_text( file_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='curve-explanation', tracking=tracking, $
				value=['Select the data directory to scan for all raw data. Select an output path, and select a template DAI image file to set initial sort parameters. ' + $
					'You can edit some sort parameters in the Table on tab 4.'], $
				uvalue={xresize:left_resize, help:'Explanation of the role of the User Input panel.'}, frame=1)


file_base0 = widget_base( file_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, $
				uvalue={xresize:left_resize}, scr_xsize=left_xsize)
label = widget_label( file_base0, value='Select Raw Data File Device')
file_base0b = widget_base( file_base0, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

file_base0c = widget_base( file_base0b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
lab = widget_label( file_base0c, value='Device:')
device_mode = widget_combobox( file_base0c, value=device_titles, uname='device-mode', /tracking, xsize=text_xsize, $
					notify_realize='OnRealize_wizard_batch_device_mode', $
					uvalue='Select input device driver for the raw data file(s).')


file_base1 = widget_base( file_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, $
				uvalue={xresize:left_resize}, scr_xsize=left_xsize)
label = widget_label( file_base1, value='Select Files and Paths')
file_base1b = widget_base( file_base1, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

file_base1c = widget_base( file_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( file_base1c, value='Raw dir:', uname='blog-dir-button', tracking=tracking, $
						uvalue='Click to browse for the raw data directory. ', scr_xsize=button_xsize )
blog_dir_text = widget_text( file_base1c, uname='blog-dir-text', value=default.path.data, tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter the raw data directory, or click on button to browse for the dir. '}, scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_batch_blog_dir_text')

file_base1e = widget_base( file_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( file_base1e, value='Output Path:', uname='output-dir-button', tracking=tracking, $
						uvalue='Click to browse for the output directory tree for image data. ', scr_xsize=button_xsize )
output_dir_text = widget_text( file_base1e, uname='output-dir-text', value=default.path.analysis, tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter the directory tree for image output, or click on button to browse for the dir. '}, scr_xsize=text_xsize-button_xsize2-5, /edit)
;						Notify_Realize='OnRealize_batch_output_dir_text')
button = widget_button( file_base1e, value='Same as Raw', uname='same-dir-button', tracking=tracking, $
						uvalue='Click to set the Anaysis path the same as the Raw path. ', scr_xsize=button_xsize2 )

file_base1d = widget_base( file_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( file_base1d, value='Energy Cal:', uname='energy-cal-file-button', tracking=tracking, $
						uvalue='Click to browse for the energy calibration SPEC file for all good detectors. Delete any detectors in the SPEC file to exclude these detector channels.', scr_xsize=button_xsize )
energy_cal_file_text = widget_text( file_base1d, uname='energy-cal-file-text', value='', tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter file-name for the energy cal SPEC file for the detector, or click on button to browse for the file. Delete any detectors in the SPEC file to exclude these detector channels.'}, scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_batch_energy_cal_file_text')


file_base2 = widget_base( file_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize, uvalue={xresize:left_resize})
label = widget_label( file_base2, value='Template settings for processing')
file_base2b = widget_base( file_base2, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

file_base2c = widget_base( file_base2b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( file_base2c, value='Template:', uname='template-sort-button', tracking=tracking, $
						uvalue='Click to browse for a template image DAI file to use to set default sort settings such as the DAM file.', scr_xsize=button_xsize )
template_sort_text = widget_text( file_base2c, uname='template-sort-text', value='', tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter file-name for a template image DAI file to use to set default sort settings such as the DAM file. ' + $
						'Parameters in the Sort EVT window will be set from this file.'}, scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_batch_template_sort_text')

;file_base3 = widget_base( file_base, /row, xpad=1, ypad=0, space=20, /align_center, /base_align_center)
;
;button = widget_button( file_base3, value='Reload "standards.csv"', uname='reload-standards-button', tracking=tracking, $
;						uvalue='Click to reload the "standards.csv" file from Resources. Use this during testing. Later we will hide this button as it is automatic after selecting Resources.', scr_xsize=2*button_xsize )


; Template image corrections table  -----------------------------------------

ctable_base = widget_base( tab_panel, title=' 2. Corrections Table  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize, uvalue={xresize:left_resize,yresize:1})
label = widget_label( ctable_base, value='Table of image corrections and display parameters')
text = widget_text( ctable_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='ctable-explanation', tracking=tracking, $
				value=['Table showing all image operations from a template DAI file, which can be selected here. ' + $
				'Operations that effect ALL planes (shown with a "*") are only shown againt ' + $
				'the element selected to guide that operation. Corrections can be deleted or more added. The "Log" column shows display mode: Linear (0), LOG (1), SQRT (2).'], $
				uvalue={xresize:left_resize, help:'Explanation of the role of the Corretions panel.'}, frame=1)

ctable_base0 = widget_base( ctable_base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)

button = widget_button( ctable_base0, value='Template:', uname='template-corrections-button', tracking=tracking, $
						uvalue='Click to browse for a template image DAI file to use for image corrections, digital filters and display settings.', scr_xsize=button_xsize )
template_corrections_text = widget_text( ctable_base0, uname='template-sort-text', value='', tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter file-name for a template image DAI file to use for image corrections, digital filters and display settings.'}, scr_xsize=text_xsize, /edit)

ctable_base1 = widget_base( ctable_base, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)

ctable1_base = widget_base( ctable_base1, title='   Corrections Table    ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center) ; , scr_xsize=left_xsize, scr_ysize=left_ysize-180-20, uvalue={xresize:left_resize,yresize:1})

cheadings = strarr(6)					; dummy values (see 'wizard_batch_update_ctable' for actual headings)
ncc = n_elements(cheadings)
widths = replicate(6,ncc) * !d.x_ch_size * ch_scale
t = strarr(ncc,256)

ctable = Widget_Table(ctable1_base, UNAME='corrections-table', /all_events, /editable, Y_SCROLL_SIZE=13, $	;, X_SCROLL_SIZE=8, $
				value=t, /RESIZEABLE_COLUMNS, alignment=2, scr_xsize=left_xsize, scr_ysize=left_ysize-182-20, /no_row_headers, $
				tracking=tracking, uvalue={xresize:left_resize,yresize:1, help:'The table shows all element image corrections from the template DAI file. ' + $
				'Operations that effect ALL planes (shown with a "*") are only shown againt the element selected to guide that operation. ' + $
				'Corrections can be deleted or more added. The "Log" column shows display mode: Linear (0), LOG (1), SQRT (2).'}, $
				column_labels=headings, column_widths=widths, $
				NOTIFY_REALIZE='OnRealize_wizard_batch_ctable')
			
ctable_base2a = widget_base( ctable_base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)

label = widget_label( ctable_base2a, value='Edit Correction:')
corrections_element = widget_combobox( ctable_base2a, value=['Back'], uname='corrections-element', /tracking, xsize=button_xsize, $
;					notify_realize='OnRealize_wizard_batch_corrections_element', $
					uvalue='Select an image processing function to add.')
corrections_mode = widget_combobox( ctable_base2a, value=uv.list, uname='corrections-mode', /tracking, xsize=button_xsize2, $
;					notify_realize='OnRealize_wizard_batch_corrections_mode', $
					uvalue='Select an image processing function to add.')
button = widget_button( ctable_base2a, value='Add', uname='ctable-add-button', tracking=tracking, $
						uvalue='Add the selected image processing command for the selected element plane. Select the element and command first.', scr_xsize=button_xsize )
button = widget_button( ctable_base2a, value='Delete', uname='ctable-delete-button', tracking=tracking, $
						uvalue='Click to deleted the elected rows of the table. Click in a cell to select that row; click and drag to select multuiple rows.', scr_xsize=button_xsize )
button = widget_button( ctable_base2a, value='Clear', uname='ctable-clear-button', tracking=tracking, $
						uvalue='Click to clear the corrections table.', scr_xsize=button_xsize )

ctable_base2b = widget_base( ctable_base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)

label = widget_label( ctable_base2b, value='Edit Display:  Bottom:')
display_bottom_text = widget_text( ctable_base2b, uname='display-bottom-text', value='0', tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter the "Bottom" display value (0-99).'}, scr_xsize=button_xsize, /edit)
label = widget_label( ctable_base2b, value='  Top:')
display_top_text = widget_text( ctable_base2b, uname='display-top-text', value='100', tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter the "Top" display value (1-100).'}, scr_xsize=button_xsize, /edit)
label = widget_label( ctable_base2b, value='  Log:')
display_log_text = widget_text( ctable_base2b, uname='display-log-text', value='0', tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter the display intensity curve function value (0=Linear, 1=Log, 2=SQRT).'}, scr_xsize=button_xsize, /edit)
button = widget_button( ctable_base2b, value='Apply', uname='display-apply-button', tracking=tracking, $
						uvalue='Click to apply these display values to the selected row. ', scr_xsize=button_xsize )
		

; Template RGB exports table  -----------------------------------------

rgbtable_base = widget_base( tab_panel, title=' 3. RGB Exports  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize, uvalue={xresize:left_resize,yresize:1})
label = widget_label( rgbtable_base, value='Table of RGB export options')
text = widget_text( rgbtable_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='rgbtable-explanation', tracking=tracking, $
				value=['Table showing selected RGB images/plots to export. The table can be set from a "Learn" RGB.csv file created in the RGB Image window ' + $
				'(see the "Learn" menu) or edited here.'], $
				uvalue={xresize:left_resize, help:'Explanation of the role of the RGB Export panel.'}, frame=1)

rgbtable_base0 = widget_base( rgbtable_base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)

button = widget_button( rgbtable_base0, value='RGB Export:', uname='template-rgb-button', tracking=tracking, $
						uvalue='Click to browse for a RGB CSV file list of RGB export combinations.', scr_xsize=button_xsize )
template_rgb_text = widget_text( rgbtable_base0, uname='template-rgb-text', value='', tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter file-name for a RGB CSV file list of RGB export combinations.'}, scr_xsize=text_xsize, /edit)

rgbtable_base1 = widget_base( rgbtable_base, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)

rgbtable1_base = widget_base( rgbtable_base1, title='   RGB Export list Table    ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center) ; , scr_xsize=left_xsize, scr_ysize=left_ysize-180, uvalue={xresize:left_resize,yresize:1})

rgb_headings = strarr(12)			; dummy values (see 'wizard_batch_update_rgbtable' for actual headings)
ncr = n_elements(rgb_headings)
widths = replicate(6,ncr) * !d.x_ch_size * ch_scale
t = strarr(ncr,256)

rgbtable = Widget_Table(rgbtable1_base, UNAME='rgb-table', /all_events, /editable, Y_SCROLL_SIZE=13, $	;, X_SCROLL_SIZE=8, $
				value=t, /RESIZEABLE_COLUMNS, alignment=2, scr_xsize=left_xsize, scr_ysize=left_ysize-182, /no_row_headers, $
				tracking=tracking, uvalue={xresize:left_resize,yresize:1, help:'The table shows selected RGB export combinations to export for each processed image ' + $
				'using the selected Zoom factor.'}, $
				column_labels=headings, column_widths=widths, $
				NOTIFY_REALIZE='OnRealize_wizard_batch_rgbtable')
			
rgbtable_base2 = widget_base( rgbtable_base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)

label = widget_label( rgbtable_base2, value='Edit Export:  R')
r_element = widget_combobox( rgbtable_base2, value=['Back'], uname='rgb-r-element', /tracking, xsize=button_xsize1, $
;					notify_realize='OnRealize_wizard_batch_corrections_r_element', $
					uvalue='Select the element for Red to add.')
label = widget_label( rgbtable_base2, value='  G:')
g_element = widget_combobox( rgbtable_base2, value=['Back'], uname='rgb-g-element', /tracking, xsize=button_xsize1, $
;					notify_realize='OnRealize_wizard_batch_corrections_g_element', $
					uvalue='Select the element for Green to add.')
label = widget_label( rgbtable_base2, value='  B:')
b_element = widget_combobox( rgbtable_base2, value=['Back'], uname='rgb-b-element', /tracking, xsize=button_xsize1, $
;					notify_realize='OnRealize_wizard_batch_corrections_b_element', $
					uvalue='Select the element for Blue to add.')
label = widget_label( rgbtable_base2, value='  Zoom:')
rgb_zoom_text = widget_text( rgbtable_base2, uname='rgb-zoom-text', value='0', tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter the new "Zoom" factor (-2 to 2 typ.).'}, scr_xsize=button_xsize1, /edit)
button = widget_button( rgbtable_base2, value='Add', uname='rgb-add-button', tracking=tracking, $
						uvalue='Click to add these RGB selections to the RGB Export table. ', scr_xsize=button_xsize )
button = widget_button( rgbtable_base2, value='Delete', uname='rgb-delete-button', tracking=tracking, $
						uvalue='Click to deleted selected rows of the RGB table. Click in a cell to select that row; click and drag to select multuiple rows.', scr_xsize=button_xsize )
button = widget_button( rgbtable_base2, value='Clear', uname='rgb-clear-button', tracking=tracking, $
						uvalue='Click to clear the RGB table.', scr_xsize=button_xsize )
button = widget_button( rgbtable_base2, value='Save', uname='rgb-save-button', tracking=tracking, $
						uvalue='Click to save the RGB table to a ".rgb.csv" file for the "Learn" function here and in the RGB Image window', scr_xsize=button_xsize )
		

; Processing and save/export options  -----------------------------------------

options_base = widget_base( tab_panel, title=' 4. Options  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize, uvalue={xresize:left_resize,yresize:1})
label = widget_label( options_base, value='Processing, Save & Export Options')
text = widget_text( options_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='options-explanation', tracking=tracking, $
				value=['Tab showing a selection of processing and output/export options. The image corrections options are setup on tab 2 (Corrections). ' + $
				'The RGB Export options are setup on tab 3 (RGB Exports).'], $
				uvalue={xresize:left_resize, help:'Explanation of the role of the Options panel.'}, frame=1)

options_base2 = widget_base( options_base, /row, xpad=10, ypad=0, space=2, /align_top, /base_align_top)

options_base3a = widget_base( options_base2, /column, /frame, xpad=2, ypad=2, space=2, /align_top, /base_align_center)
label = widget_label( options_base3a, value='File Options')

options_file = [0,1]
options_file_id = cw_bgroup2( options_base3a, ['Overwrite original DAI file','Skip sort if original DAI already exists'], $
			/column, xpad=0, ypad=0, space=0, /return_index, tracking=tracking, $
			uname='options-file', set_value=options_file, /nonexclusive, $
			uvalue=['Overwrite the initial DAI file with the digital filtered images, rather than write a separate "-m.DAI" or "-x.DAI" file. This saves disk space, but it may be better to avoid this to ensure that the modified image data is stored in a different file.', $
					'Skip the sorting of the raw data to DAI image file if a DAI file already exists for a particular run. Use this feature to apply filtering and output options to previously sorted runs.'])

options_base3b = widget_base( options_base2, /column, /frame, xpad=2, ypad=2, space=2, /align_top, /base_align_center)
label = widget_label( options_base3b, value='Processing Options')

options_process = [0,0,0]
options_process_id = cw_bgroup2( options_base3b, ['Apply corrections to images','Apply a CorrectX scaling file','Mirror X (odd images only)'], $
			/column, xpad=0, ypad=0, space=0, /return_index, tracking=tracking, $
			uname='options-process', set_value=options_process, /nonexclusive, $
			uvalue=['Enable the application of digital filters and corrections to the resulting images, as defined on tab 2 (Corrections). ', $
					'Select an existing file of CorrectX corrections to scale image for variations in X current, flux, etc.', $
					'A nasty hack to flip an image in X [to fix the Epics negative width error at XFM that produces negative X and flips every second image]. Set "First" on the first image to flip in X.'])

options_base3c = widget_base( options_base2, /column, /frame, xpad=2, ypad=2, space=2, /align_top, /base_align_center)
label = widget_label( options_base3c, value='Export Options')

options_export = [0,0,0,0,0,0,0,0]
options_export_id = cw_bgroup2( options_base3c, ['Save images as colour PNG to HTML', 'Save images as B/W PNG to HTML','Export images as Tab delimited text', $
							'Save images as TIFF concentration', 'Save images as TIFF counts','Save images as TIFF ng/cm**2','Save selected RGB images','Save METADATA for each image'], $
			/column, xpad=0, ypad=0, space=0, /return_index, tracking=tracking, $
			uname='options-export', set_value=options_export, /nonexclusive, $
			uvalue=['Save colour images in PNG format, and build a summary HTML file using thumb-nail versions of the images. You will be prompted for an element list after the first sort in the table.', $
					'Save black and white images in PNG format, and build a summary HTML file using thumb-nail versions of the images. You will be prompted for an element list after the first sort in the table.', $
					'Export selected element images as tab delimited text files. You will be prompted for an element list after the first sort in the table.', $
					'Save floating point images proportional to concentration in TIFF format, and build a summary HTML file using thumb-nail versions of the images. You will be prompted for an element list after the first sort in the table.', $
					'Save floating point images proportional to counts in TIFF format, and build a summary HTML file using thumb-nail versions of the images. You will be prompted for an element list after the first sort in the table.', $
					'Save floating point images proportional to ng/cm^2 in TIFF format, and build a summary HTML file using thumb-nail versions of the images. You will be prompted for an element list after the first sort in the table.', $)
					'Save selected RGB images as defined on tab 3 (RGB Exports). Use "Learn" in the RGB Image window to create a template file, or build the table on tab 3, to provide the RGB list.', $
					'Save a Metadata JSON file of image parameters for each image.'])


; Results table  -----------------------------------------

table_base = widget_base( tab_panel, title=' 5. Processing Table  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize, uvalue={xresize:left_resize,yresize:1})
label = widget_label( table_base, value='Work Table and Processing Progress')
results_text = widget_text( table_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='table-explanation', tracking=tracking, $
				value=['Scan for the details of the raw files (set raw path on tab 1). ' + $
				'Missing values for raw data lacking metadata need to be entered. See more detailed instructions in the info panel (right). When ready click "Start Process" to start processing.'], $
				uvalue={xresize:left_resize, help:'Explanation of the role of the Results panel.'}, frame=1)

table_base1 = widget_base( table_base, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)

table_base1a = widget_base( table_base1, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)

button = widget_button( table_base1a, value='Scan Raw and Analysis folders for data', uname='scan-blog-button', tracking=tracking, $
						uvalue='Click to scan the selected raw and analysis folders (Tab 1) for all raw data and associated analysis results to populate the table. ' + $
						'Click on "Start processing" to begin processing.', scr_xsize=4*button_xsize )
label = widget_label( table_base1a, value='    Conv:')
conv_text = widget_text( table_base1a, uname='conv-text', value='', tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter the conversion from flux to charge, for those devices that measure charge indirectly and need to convert some flux measure to charge using "conv".'}, scr_xsize=button_xsize2, /edit)

table1_base = widget_base( table_base1, title='  Results Table    ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center) ;, scr_xsize=left_xsize, scr_ysize=left_ysize-180, uvalue={xresize:left_resize,yresize:1})

headings = strarr(12)			; dummy values (see 'wizard_batch_update_table' for actual headings)
nc = n_elements(headings)
widths = replicate(6,nc) * !d.x_ch_size * ch_scale
t = strarr(nc,256)

results_table = Widget_Table(table1_base, UNAME='results-table', /all_events, /editable, Y_SCROLL_SIZE=13, $	;, X_SCROLL_SIZE=8, $
				value=t, /RESIZEABLE_COLUMNS, alignment=2, scr_xsize=left_xsize, scr_ysize=left_ysize-182, /no_row_headers, $
				tracking=tracking, uvalue={xresize:left_resize,yresize:1, help:'The table shows the raw data to process and tracks processing progress.'}, $
				column_labels=headings, column_widths=widths, $
				NOTIFY_REALIZE='OnRealize_wizard_batch_results_table')
			
table_base2 = widget_base( table_base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)

;button = widget_button( table_base2, value='Scan Blog Dir', uname='scan-blog-button', tracking=tracking, $
;						uvalue='Click to scan the selected raw dir (previous Tab page), if this was not done already, and populate the table. Missing values may need to be added manually after the scan.', scr_xsize=1.5*button_xsize )
button = widget_button( table_base2, value='Enable', uname='table-enable-button', tracking=tracking, $
						uvalue='Click to toggle the "On/off" state of selected rows. Click in a cell and drag down to select multiple rows.', scr_xsize=button_xsize )
button = widget_button( table_base2, value='Fill', uname='table-fill-button', tracking=tracking, $
						uvalue='Click to duplicate a value down a column of selected cells. Click in a cell to select it; click and drag to select multuiple rows (and/or columns). One row selected will fill down to the bottom of the table. Use multiple row selection to confine fill to this range.', scr_xsize=button_xsize )
button = widget_button( table_base2, value='Delete', uname='table-delete-button', tracking=tracking, $
						uvalue='Click to deleted selected rows of the table. Click in a cell to select that row; click and drag to select multuiple rows.', scr_xsize=button_xsize )
button = widget_button( table_base2, value='Clear', uname='table-clear-button', tracking=tracking, $
						uvalue='Click to clear the entire table. ', scr_xsize=button_xsize )
label = widget_label( table_base2, value='            ')
button = widget_button( table_base2, value='Start Processing', uname='process-button', tracking=tracking, $
						uvalue='Click to start the calculation of the calibration factors "conv". Make sure the table entries are correct first. ', scr_xsize=1.5*button_xsize )
button = widget_button( table_base2, value='Abort', uname='abort-button', tracking=tracking, $
						uvalue='Click to abort the processing loop. Current row will complete sorting, image operations and exports first.', scr_xsize=button_xsize )
		
;------------------------------------------------------------------------------------------------

sbase = widget_base( lbase, /row, xpad=0, ypad=0, space=20, /base_align_center, /align_center)
button = widget_button( sbase, value='  <<  Back  ', uname='back-button', tracking=tracking, uvalue='Go back a page in the procedure to the previous page. ' + $
			' You can also click on the Tab label for a previous page to go directly to it.')
button = widget_button( sbase, value=' Figure ', uname='figure-button', tracking=tracking, uvalue='Re-display the Figure for this tab (if available).')
button = widget_button( sbase, value='  Next  >>  ', uname='next-button', tracking=tracking, uvalue='Go to the next page in the procedure, if all entries have been made and prerequisite steps have been completed.')
		
;------------------------------------------------------------------------------------------------

rbase = widget_base( tbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

instructions_text = widget_list( rbase, scr_xsize=right_xsize, scr_ysize=right_ysize, uname='instruction-explanation', tracking=tracking, $
				value='', Notify_Realize='OnRealize_wizard_batch_instructions', $
				uvalue={xresize:right_resize, yresize:1, help:'Explanation of the function and controls on this tab panel.'});, frame=1)

;------------------------------------------------------------------------------------------------

if tracking then begin
	help = widget_text( tlb, scr_xsize=help_xsize, ysize=3, /wrap, uname='HELP', tracking=tracking, $
			value='', frame=0, uvalue={xresize:1})
endif else help=0L

; Set-up the 'state' variable, which will be stored in heap with a pointer in the 'uvalue' of the first
; child (base) of the top-level base. This uvalue cannot include a struct with xresize, etc.

state = { $
		path:					ptr_new(path), $				; pointer to current path
		dpath:					ptr_new(dpath), $				; pointer to current dpath
		tracking:				tracking, $						; tracking mode
		tab:					0, $							; current Tab selected
		tab_names:				tab_names, $					; tab names
		tab_used:				intarr(n_elements(tab_names)), $	; flags that figure is displayed
		windows_needed:			windows_needed, $				; list of needed window names
		windows_open:			ptrarr(n_elements(windows_needed), /allocate_heap), $	; lists unique window IDs found to be open.

		message: {	pwizard:	ptr_new(/allocate_heap), $		; message about another Wizard being open
					pwindow:	ptr_new(/allocate_heap)}, $		; message about duplicate windows being open

		pwizard1:				ptr_new(/allocate_heap), $		; data area for wizard notify commands to GeoPIXE
		pwizard2:				ptr_new(/allocate_heap), $		; data area for wizard notify commands to GeoPIXE
		pwizard3:				ptr_new(/allocate_heap), $		; data area for wizard notify commands to GeoPIXE
		pwizard4:				ptr_new(/allocate_heap), $		; data area for wizard notify commands to GeoPIXE
		pwizard5:				ptr_new(/allocate_heap), $		; data area for wizard notify commands to GeoPIXE
		parray:					ptrarr(100, /allocate_heap), $	; ptr arrray for windows test (and ...)
		parg1:					ptr_new(/allocate_heap), $		; temp data ptr for storing intermediate results
		parg2:					ptr_new(/allocate_heap), $		; temp data ptr for storing intermediate results
		parg3:					ptr_new(/allocate_heap), $		; temp data ptr for storing intermediate results
		parg4:					ptr_new(/allocate_heap), $		; temp data ptr for storing intermediate results
		parg5:					ptr_new(/allocate_heap), $		; temp data ptr for storing intermediate results
		parg6:					ptr_new(/allocate_heap), $		; temp data ptr for storing intermediate results
		
		tlb:					tlb, $							; TLB ID
		tab_panel:				tab_panel, $					; tab ID
		tlb_xsize:				0L, $							; TLB X size
		tlb_ysize:				0L, $							; TLB Y size
		
;		DevObj:					obj_new('MAIA_DEVICE'), $		; Device object
		pDevObj:				ptr_new( DevObjList[device]), $	; current Device object
		device:					device, $						; device index
		pDevObjList:			ptr_new( DevObjList), $			; device object list
		sel: {left:-1, top:-1, right:-1, bottom:-1, edit:0 }, $	; use "(*pstate).sel.top" as current row in processing table
		csel: {left:-1, top:-1, right:-1, bottom:-1, edit:0 }, $	; use "(*pstate).sel.top" as current row in corrections table
		rsel: {left:-1, top:-1, right:-1, bottom:-1, edit:0 }, $	; use "(*pstate).sel.top" as current row in RGB table
		uv:						uv, $							; image process list

		blog_dir:	 			default.path.data, $			; blog data dir tree
		energy_cal_file:		'', $							; energy cal file name
		output_dir:				default.path.analysis, $		; output dir tree
		template_sort_dai:		'', $							; template DAI for sorting
		template_corrections_dai:	'', $						; template DAI for corrections
		template_rgb_export:		'', $						; template RGB.csv for RGB export
		root:					ptr_new(/allocate_heap), $		; storage foor 'build_output_path' root path

;		pconfig:				ptr_new(/allocate_heap), $		; room for standards.csv config table
		first:					0, $							; first run to process
		index:					0, $							; table row index
		busy:					0, $							; processing busy flag
		abort:					0, $							; abort loop flag	
		pdai:					ptr_new(/allocate_heap), $		; pointer to template DAI image struct
		pcel:					ptr_new(/allocate_heap), $		; element names from template DAI file

		presults:				ptr_new(/allocate_heap), $		; room for results table
		ptitle:					ptr_new(/allocate_heap), $		; names for table struct columns 
		ptype:					ptr_new(/allocate_heap), $		; data-types for table struct columns 

		pcorr:					ptr_new(/allocate_heap), $		; room for corrections table
		pctitle:				ptr_new(/allocate_heap), $		; names for corr struct columns 
		pctype:					ptr_new(/allocate_heap), $		; data-types for corr struct columns 

		prgb:					ptr_new(/allocate_heap), $		; room for RGB table
		prtitle:				ptr_new(/allocate_heap), $		; names for RGB struct columns 
		prtype:					ptr_new(/allocate_heap), $		; data-types for RGB struct columns 

		columns:				nc, $							; table columns
		pheadings:				ptr_new(headings), $			; headings
		rows:					0, $							; rows

		ccolumns:				ncc, $							; corrections columns
		pcheadings:				ptr_new(cheadings), $			; headings
		crows:					0, $							; rows
		correction_mode:		0, $							; mode for corr processing ID
		correction_element_mode:	0, $						; mode for corr el ID

		rgb_columns:			ncr, $							; RGB columns
		prheadings:				ptr_new(rgb_headings), $		; headings
		rrows:					0, $							; rows
		r_element_mode:			0, $							; R element mode
		g_element_mode:			0, $							; G element mode
		b_element_mode:			0, $							; B element mode
		
		options_file:			options_file, $					; file options
		options_process:		options_process, $				; processing options
		options_export:			options_export, $				; save/export options

;		wid1:					0L, $							; Draw 1 window ID
;		wid2:					0L, $							; Draw 2 window ID
;		detector_mode:			0, $							; detector (old) droplist setting
;		detector_new_mode:		0, $							; detector (new) droplist setting
;		detector_list:			ptr_new(/allocate_heap), $		; pointer to list of detector file names
		
		blog_dir_text: 			blog_dir_text, $				; blog dir text ID
		energy_cal_file_text:	energy_cal_file_text, $			; energy cal file text ID
		output_dir_text: 		output_dir_text, $				; output dir text ID
		template_sort_text: 	template_sort_text, $			; template sort DAI text ID
		template_corrections_text: 	template_corrections_text, $	; template corrections DAI text ID
		template_rgb_text:	 	template_rgb_text, $			; template RGB export list text ID

		corrections_table:		ctable, $						; corrections table ID
		rgb_table:				rgbtable, $						; RGB export table ID
		results_table:			results_table, $				; results table ID
		corrections_mode:		corrections_mode, $				; process corrections mode ID
		corrections_element:	corrections_element, $			; corrections element for Add ID	
		display_bottom_text:	display_bottom_text, $			; display bottom text ID
		display_top_text:		display_top_text, $				; display top text ID
		display_log_text:		display_log_text, $				; display log text ID
		r_element:				r_element, $					; R element mode ID	
		g_element:				g_element, $					; G element mode ID	
		b_element:				b_element, $					; B element mode ID	
		rgb_zoom_text:			rgb_zoom_text, $				; Zoom text ID
		conv_text:				conv_text, $					; conv factor text ID

		options_file_id:		options_file_id, $				; file options bgroup ID
		options_process_id:		options_process_id, $			; processing options bgroup ID
		options_export_id:		options_export_id, $			; save/export options bgroup ID

;		ptab_panel:				ptab_panel, $					; plot tab ID
;		results_draw1:			results_draw1, $				; results plot1 (by run)
;		results_draw2:			results_draw2, $				; results plot2 (by energy)
;		detector_mode_id:		detector_mode, $				; detector (old) droplist ID
;		detector_new_mode_id:	detector_new_mode, $			; detector (new) droplist ID
		
		instructions_text:		instructions_text, $			; instructions text ID
		help:					help $							; help text ID
		}

;*(state.detector_list) = dlist

child = widget_info( tlb, /child)
pstate = ptr_new(state, /no_copy)
widget_control, child, set_uvalue=pstate
widget_control, tlb, /realize

geom = widget_info( tlb, /geometry)
(*pstate).tlb_xsize = geom.scr_xsize
(*pstate).tlb_ysize = geom.scr_ysize

register_notify, tlb, ['wizard-return', $				; returns from GeoPIXE windows
				'path', $								; new paths
				'new-detectors']						; detectors changed
xmanager, 'wizard_batch', tlb, /no_block

wizard_test_windows, 'batch', pstate					; check for open GeoPIXE windows and
widget_control, tlb, timer=8.0							; start timer to check periodically
return
end
				