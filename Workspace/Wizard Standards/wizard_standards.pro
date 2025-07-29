
; Wizard to determine the 'conv' calibration factors for standards foils.
; 
;	For a selected raw dir, find all raw files (of 'standard_type' = "standard"),
;	and calculate the IC conversion calibration factor 'conv' by using a
;	region over most of the foil image.

pro wizard_standards_event, event

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
		warning,'wizard_standards_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
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
						'Go back to previous tab using "Back" or  select specific tabs on the left.'
		endelse
		goto, finish
		end
	'WIDGET_TIMER': begin
		(*pstate).windows_veto = ((*pstate).windows_veto-1) > 0	; decrement count-down, to give time to close some windows
		if ((*pstate).windows_veto eq 0) then begin
			wizard_test_windows, 'standards', pstate			; periodically check which GeoPIXE windows are
		endif													; currently open (not if warning open [windows_veto])
		widget_control, event.id, timer=8.0	
		goto, finish
		end

	'NOTIFY': begin												; events from other GeoPIXE windows
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
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

					if (*pw).wizard ne 'standards' then begin
						(*pstate).windows_veto = 5				; veto tests while this pop-up is open
																; count down gives time to close somewindows
						warning, 'wizard_standards_return', ['Another Wizard appears to be open ("'+(*pw).wizard+'").', '', $
								'This will cause many problems.','Only open one Wizard at a time.','Please close other Wizards.']
						goto, finish
					endif

;					A reply from a window to 'open-test' shows that it is open ('top' returns 'event.top' of sending window) ...
;					Accumulate a list of all open and valid IDs to test if unwanted duplicates are open.

					if (*pw).command eq 'open-test' then begin
						wizard_check_window_id, needed=(*pstate).windows_needed, open=(*pstate).windows_open, name=(*pw).window, id=(*pw).top, count=count, error=error
						if error then goto, finish

						if count gt 1 then begin
							(*pstate).windows_veto = 4			; veto tests while this pop-up is open
																; count down gives time to close somewindows
							warning, 'wizard_standards_return', ['Multiple windows of types "'+strjoin((*pstate).windows_needed,', ')+'" may be open.', '', $
								'This may cause problems.','Please close any duplicate windows.']
						endif
						goto, finish
					endif
						
					print, '*** Wizard return: from="'+(*pw).window+'", command="'+(*pw).command+'", error='+str_tidy((*pw).error)
					if (*pw).error then begin
						warning,'standards wizard',['Error in processing this data-set:', $
								(*(*pw).pdata).output,'','"OK" to continue, "Cancel" to abort all.'], cancel=cancel
						if cancel then goto, finish
					endif
						
;					If there is a callback routine, execute it now, passing the returned wizard notify pointer ...

					if (*pw).callback ne '' then begin
						print, '   >>> callback = "'+(*pw).callback
						call_procedure, (*pw).callback, pstate, pw, error=error 
						if error then begin
							print, '		callback error='+str_tidy(error)
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
					endelse
				endif
				goto, finish
				end

			else:
		endcase
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request wizard_standards ...'
		goto, kill
		end
	else:
endcase

; From here we process other events from widgets and the top-level base
; By convention all widgets have a 'uname' to identify them.

uname = widget_info( event.id, /uname)
case uname of

	'wizard-standards-tlb': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				wizard_resize, event.top, oldx=(*pstate).tlb_xsize, oldy=(*pstate).tlb_ysize, $
								minx=700, miny=600
				geom = widget_info( event.top, /geometry)
				(*pstate).tlb_xsize = geom.scr_xsize
				(*pstate).tlb_ysize = geom.scr_ysize

				wizard_standards_update_plots, pstate
				end
			else:
		endcase
		end

	'wizard-standards-tab-panel': begin
		(*pstate).tab = clip( event.tab, 0, n_elements((*pstate).tab_names)-1)
		wizard_standards_update_info, pstate
		
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
		F = file_requester( /read, title='Select the "standards" raw dir', path=(*pstate).blog_dir, $
							/dir, group=event.top )
		if F[0] ne '' then begin
			(*pstate).blog_dir = F[0]
			set_widget_text, (*pstate).blog_dir_text, F[0]
			s = build_output_path( (*pstate).blog_dir, (*pstate).output_dir, (*pstate).root, /set)
			if lenchr(s) gt 0 then begin
				(*pstate).output_dir = s
				set_widget_text, (*pstate).output_dir_text, s
			endif
		endif
		end
		
	'blog-dir-text': begin
		widget_control, event.id, get_value=s
		(*pstate).blog_dir = s
		s = build_output_path( (*pstate).blog_dir, (*pstate).output_dir, (*pstate).root, /set)
		if lenchr(s) gt 0 then begin
			(*pstate).output_dir = s
			set_widget_text, (*pstate).output_dir_text, s
		endif
		end
					
	'energy-cal-file-button': begin
		F = file_requester( /read, title='Select a detector energy cal file', path=(*pstate).output_dir, $
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
		F = file_requester( /read, title='Select "standards" analysis output dir', path=(*pstate).output_dir, $
							/dir, group=event.top )
		if F[0] ne '' then begin
			(*pstate).output_dir = F[0]
			set_widget_text, (*pstate).output_dir_text, F[0]
			s = build_output_path( (*pstate).blog_dir, (*pstate).output_dir, (*pstate).root, /set)
		endif
		end
		
	'output-dir-text': begin
		widget_control, event.id, get_value=s
		(*pstate).output_dir = s
		end
					
	'resource-dir-button': begin
		F = file_requester( /read, title='Select GeoPIXE Resources dir', path=(*pstate).resource_dir, $
							/dir, group=event.top )
		if F[0] ne '' then begin
			(*pstate).resource_dir = F[0]
			set_widget_text, (*pstate).resource_dir_text, F[0]
		endif
		end
		
	'resource-dir-text': begin
		widget_control, event.id, get_value=s
		(*pstate).resource_dir = s

		config = wizard_standards_read_config( fix_path((*pstate).resource_dir)+'standards'+path_sep()+'standards.csv', error=error)
		if error then goto, finish
		*(*pstate).pconfig = config
		end
					
	'scan-blog-button': begin
		s = build_output_path( (*pstate).blog_dir, (*pstate).output_dir, (*pstate).root, /set)

		table = wizard_standards_scan_dir( pstate, title=title, type=type, error=error)
		if error then begin
			warning,'wizard_standards_event','No raw files found.'		
			goto, finish
		endif
		*(*pstate).presults = table
		*(*pstate).ptitle = title
		*(*pstate).ptype = type
		wizard_standards_update_table, pstate
		wizard_standards_update_stats, pstate
		
		(*pstate).tab = clip( (*pstate).tab+1, 0, n_elements((*pstate).tab_names)-1)
		widget_control, (*pstate).tab_panel, set_tab_current=(*pstate).tab
		wizard_standards_update_info, pstate
		end
	
	'reload-standards-button': begin
		config = wizard_standards_read_config( fix_path((*pstate).resource_dir)+'standards'+path_sep()+'standards.csv', error=error)
		if error then goto, finish
		*(*pstate).pconfig = config
		end
		
	'results-table': begin
;		help,event,/str
		case tag_names( event, /structure_name) of
			'WIDGET_TABLE_CELL_SEL': begin
				(*pstate).sel.left = event.sel_left
				(*pstate).sel.top = event.sel_top
				(*pstate).sel.right = event.sel_right
				(*pstate).sel.bottom = event.sel_bottom
				i = (*pstate).sel.top
				if i lt 0 then goto, finish
			
;				print,'re-load table ...'
;				print, (*pstate).sel
				view = widget_info( (*pstate).results_table, /table_view)
				widget_control, (*pstate).results_table, get_value=t
				widget_control, (*pstate).results_table, set_value=t
				widget_control, (*pstate).results_table, set_table_select=[(*pstate).sel.left,(*pstate).sel.top,(*pstate).sel.right,(*pstate).sel.bottom]
				widget_control, (*pstate).results_table, set_table_view=view
				
				i = (*pstate).sel.top
				j = (*pstate).sel.left
				if (i lt 0) or (j lt 0) then goto, finish
				q = where( (*(*pstate).pheadings)[j] eq *(*pstate).ptitle, nq)	; Find heading is list of tag names
				if nq gt 0 then begin											; for row struct of results table.
					k = q[0]													; q[0] is then tag index in struct.
					if ((*(*pstate).ptype)[k] eq 'file') and ((event.sel_right-j eq 0) and (event.sel_bottom-i eq 0)) then begin
						name = (*(*pstate).ptitle)[k]
						f = file_requester( /read, filter=['*.'+strlowcase(name)+'.var','*.txt'], /fix_filter, $
							file=(*p)[i].(k), group=event.top, title='Select '+name+' File', cancel=cancel)
						if cancel eq 0 then begin
							(*p)[i].(k) = f[0]
							wizard_standards_update_table, pstate
						endif
					endif
				endif
				end
				
			'WIDGET_TABLE_CH': begin
				if (event.ch eq 13B) or (event.ch eq 10B) then begin			; <cr> after edit cell
					if no_data eq 0 then begin									; ignore if now rows loaded
						widget_control, (*pstate).results_table, get_value=t
						i = (*pstate).sel.top
						if i lt 0 then goto, finish
						for j=3,(*pstate).columns-1 do begin
							q = where( (*(*pstate).pheadings)[j] eq *(*pstate).ptitle, nq)	; Find heading is list of tag names
							if nq gt 0 then begin								; for row struct of results table.
								k = q[0]										; q[0] is then tag index in struct.
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
									warning,'wizard_standards_event','Illegal character for "'+(*(*pstate).ptype)[k]+'" in "'+(*(*pstate).ptitle)[k]+'" on row '+str_tidy(i)
									return
								endif
							endif
						endfor
					endif
					wizard_standards_update_table, pstate
					wizard_standards_update_stats, pstate
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
			wizard_standards_update_table, pstate
			wizard_standards_update_stats, pstate
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
		wizard_standards_update_table, pstate
		wizard_standards_update_stats, pstate
		end

	'table-enable-button': begin
		if no_data then goto, finish
		np = n_elements(*p)
		if ((*pstate).sel.top ge 0) and ((*pstate).sel.bottom lt np) then begin
			for i=(*pstate).sel.top,(*pstate).sel.bottom do begin
				(*p)[i].on = 1 - (*p)[i].on 
			endfor
		endif
		wizard_standards_update_table, pstate
		end

	'table-clear-button': begin
		if no_data then goto, finish
		*p = 0
		wizard_standards_update_table, pstate
		wizard_standards_update_stats, pstate
		(*pstate).sel.top = -1
		(*pstate).sel.bottom = -1
		end

	'process-button': begin
		(*pstate).loop = 0
		wizard_standards_process_blog, pstate, error=error
		end
	
	'stats-table': begin
		case tag_names( event, /structure_name) of
			'WIDGET_TABLE_CELL_SEL': begin
				(*pstate).sel.left = event.sel_left
				(*pstate).sel.top = event.sel_top
				(*pstate).sel.right = event.sel_right
				(*pstate).sel.bottom = event.sel_bottom
				end
				
			'WIDGET_TABLE_TEXT_SEL': begin
				help,event,/str
				end
			else:
		endcase
		end

	'detector-mode': begin
		n = n_elements(*(*pstate).detector_list)
		if n lt 1 then goto, finish
		(*pstate).detector_mode = event.index < (n-1)
		wizard_standards_update_plots, pstate
		end

	'detector-new-mode': begin
		n = n_elements(*(*pstate).detector_list)
		if n lt 1 then goto, finish
		(*pstate).detector_new_mode = event.index < (n-1)
		wizard_standards_update_plots, pstate
		end

	'back-button': begin
		(*pstate).tab = clip( (*pstate).tab-1, 0, n_elements((*pstate).tab_names)-1)
		widget_control, (*pstate).tab_panel, set_tab_current=(*pstate).tab
		wizard_standards_update_info, pstate
		end
		
	'next-button': begin
		(*pstate).tab = clip( (*pstate).tab+1, 0, n_elements((*pstate).tab_names)-1)
		widget_control, (*pstate).tab_panel, set_tab_current=(*pstate).tab
		wizard_standards_update_info, pstate
		end
		
	'figure-button': begin
		wizard_standards_update_info, pstate, /force
		end
		
	else:
endcase

finish:
	widget_control, hourglass=0
	return

done:
	goto, kill

bad_state:
	warning,'wizard_standards_event',['STATE variable has become ill-defined.','Abort Wizard.'],/error
	goto, kill
bad_ptr:
	warning,'wizard_standards_event',['Parameter structure variable has become ill-defined.','Abort Wizard.'],/error
	goto, kill

; Free memory and exit cleanly ...

kill:
;	heap_free, pstate, /verbose			; slow and not recommended

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).root) then ptr_free, (*pstate).root
	if ptr_valid( (*pstate).pconfig) then ptr_free, (*pstate).pconfig
	if ptr_valid( (*pstate).presults) then ptr_free, (*pstate).presults
	if ptr_valid( (*pstate).ptitle) then ptr_free, (*pstate).ptitle

	if ptr_valid( (*pstate).ptype) then ptr_free, (*pstate).ptype
	if ptr_valid( (*pstate).pheadings) then ptr_free, (*pstate).pheadings
	if ptr_valid( (*pstate).psheadings) then ptr_free, (*pstate).psheadings
	if ptr_valid( (*pstate).detector_list) then ptr_free, (*pstate).detector_list

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

pro wizard_standards_callback_image_done, pstate, pep, error=error

; Callback to: After image done, check loop and do the next one.
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
       warning,'wizard_standards_callback_image_done',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return

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

	if ptr_good( (*(*pep).pdata).pnew) then begin
		pret = (*(*pep).pdata).pnew
		p = (*pstate).presults
		if (*pret).pileup.new ne '' then begin
			q = where( *(*pstate).ptitle eq 'Pileup', nq)
			if nq gt 0 then begin
				q = where( (*p)[*].pileup eq (*pret).pileup.old, nq)
				if nq gt 0 then (*p)[q].pileup = (*pret).pileup.new					
			endif
		endif
		if (*pret).throttle.new ne '' then begin
			q = where( *(*pstate).ptitle eq 'Throttle', nq)
			if nq gt 0 then begin
				q = where( (*p)[*].throttle eq (*pret).throttle.old, nq)
				if nq gt 0 then (*p)[q].throttle = (*pret).throttle.new					
			endif
		endif
		wizard_standards_update_table, pstate
		wizard_standards_update_stats, pstate
		ptr_free, pret
	endif
	
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_standards_callback_region_done, pstate, pep, error=error

; Callback to: After region done, access results and calculate 'conv'

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
       warning,'wizard_standards_callback_region_done',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return
	pd = (*(*pep).pdata).presults						; results from region
	local = (*(*pep).pdata).local						; is it managed here?
	if ptr_good(pd) eq 0 then begin
		warning,'wizard_standards_callback_region_done','Pointer bad in region results return (local='+str_tidy(local)+').'
		return
	endif
	
;	Process region ...

	row = (*(*pstate).presults)[ (*pstate).index]		; current row of table
	pc = (*pstate).pconfig								; full config list

;	Find results for element selected for this row ...

	q1 = where( *(*pd).el eq row.el, nq1)
	if nq1 eq 0 then begin
		warning,'wizard_standards_callback_region_done','Element "'+row.el+'" not found in region results.'
		return
	endif

;	Find matching standard specification (remember that 'el' is based on standard spec) ...

	q2 = wizard_standards_find_match( pstate, row.name, row.serial, row.detector, row.energy, error=error)
	if error then begin
		warning,'wizard_standards_callback_region_done','Standards spec: "'+row.name+','+row.serial+','+row.detector+','+std_tidy(row.eenergy)+'" not found in "standards.csv" config file.'
		return
	endif

;	q2 = where( ((*pc).el eq row.el) and ((*pc).name eq row.name) and ((*pc).serial eq row.serial) and ((*pc).detector eq row.detector) and (abs((*pc).energy - row.energy) lt 0.01), nq2)
;	if nq2 eq 0 then begin
;		warning,'wizard_standards_callback_region_done','Element "'+row.el+'" not found in "standards.csv" config file.'
;		return
;	endif
	
;	copy_pointer_data, pd, pt, /init					; for testing, to examine *pd (pointer becomes invalid in debug)

;	Ratio of conc to expected ...

	r = (*(*pd).conc)[q1[0]] / (1.e+4 * (*pc)[q2[0]].conc)
	(*(*pstate).presults)[ (*pstate).index].conv = r * (*pd).IC.conversion
	wizard_standards_update_table, pstate

;	Stats table ...

	(*(*pstate).presults)[ (*pstate).index].mean = (*(*pd).conc)[q1[0]] / r
	(*(*pstate).presults)[ (*pstate).index].error = (*(*pd).error)[q1[0]] / r
	(*(*pstate).presults)[ (*pstate).index].sd = (*(*pd).sd)[q1[0]] / r
	(*(*pstate).presults)[ (*pstate).index].relsd = (*(*pd).relsd)[q1[0]]
	wizard_standards_update_stats, pstate
	
;	plots ...

	wizard_standards_update_plots, pstate
	wizard_standards_update_export, pstate
	
;	Next row?
	
	(*pstate).loop += 1
	wizard_standards_process_blog, pstate, error=error
	error = 0
	return
end

;---------------------------------------------------------------------------------------------------

function wizard_standards_find_config, pstate, row, error=error

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
       warning,'wizard_standards_find_config',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return, 0
    endif
endif

	error = 1
	f = ''

	j = wizard_standards_find_match( pstate, row.name, row.serial, row.detector, row.energy, error=error)

	if error eq 0 then begin
		senergy = strtrim(round(1000.*(*(*pstate).pconfig)[j].energy),2) + 'eV'
		f = (*pstate).resource_dir + (*(*pstate).pconfig)[j].detector + path_sep() +  $
				'standards' + path_sep() + senergy + path_sep() + (*(*pstate).pconfig)[j].dam
	endif
	
	return, f
end

;--------------------------------------------------------------------------

function wizard_standards_find_match, pstate, sample, serial, detector, energy, error=error

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
       warning,'wizard_standards_find_match',['IDL run-time error caught.', '', $
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

function wizard_standards_output_file, pstate, blog, error=err
	
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
	       warning,'wizard_standards_output_file',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
	      return, ''
	    endif
	endif
	
	err = 1
	if n_elements(blog) eq 0 then return, ''

	path = build_output_path( blog, (*pstate).output_dir, (*pstate).root)
	f = path + strip_path( blog)

;	f = blog
;	j = locate('blog',f)
;	if j ge 0 then begin
;		f = strmid(f, 0,j) + 'analysis' + strmid(f, j+4)
;	endif

	if extract_extension() ne '' then begin
		f = strip_file_ext( f) + '.dai'
	endif
	err = 0
	
finish:
	return, f
end

;------------------------------------------------------------------------------------------

function wizard_standards_read_config, file, error=err
	
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
	       warning,'wizard_standards_read_config',['IDL run-time error caught.', '', $
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
			warning,'wizard_standards_read_config','Illegal character in "float" value in config file.'
		endif
	endwhile
	err = 0
finish:
	close_file, lun
	return, (n_elements(config) eq 0) ? 0 : config
	
bad_file:
	warning,'wizard_standards_read_config','Failed to open standards config file '+file
	return, 0
bad_format:
	warning,'wizard_standards_read_config','Bad format in standards config file '+file
	goto, finish
end

;--------------------------------------------------------------------------

function wizard_standards_scan_dir, pstate, title=title, type=type, error=error

; Scan selected raw dir for standards runs to populate the Table.

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
       warning,'wizard_standards_scan_dir',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return, 0
    endif
endif
if n_elements(silent) eq 0 then silent=0

	error = 1
	config = wizard_standards_read_config( fix_path((*pstate).resource_dir)+'standards'+path_sep()+'standards.csv', error=err)
	if err then return, 0
	
	pc = (*pstate).pconfig
	*pc = config
	
;	Config entries look like:
;	entries = {Name:'', Serial:'', areal:0.0, el:'', conc:0.0, detector:'', energy:0.0, dam:''}

;	if silent then begin
;		rmin = (*pstate).run_min
;		rmax = (*pstate).run_max
;		use_range = 0
;		if (*pstate).run_max gt 0 then use_range=1
;		goto, cont
;	endif

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
	p = scan_dir_evt( (*pstate).blog_dir, *(*pstate).pDevObj, ppath=(*pstate).path, proot=(*pstate).root, rmin=rmin, rmax=rmax, error=err)
	if err then return, 0

	nb = n_elements(p)
	if nb eq 0 then begin
		warning,'wizard_standards_scan_dir','No standards raw files found in - '+(*pstate).blog_dir
		return, 0
	endif
	blog = strarr(nb)
	for i=0,nb-1 do blog[i] = (*p[i]).file
	
	for i=0,nb-1 do begin
		mp = get_header_info( *(*pstate).pDevObj, blog[i], error=err)	; output=output, silent=silent
		if err then begin
			warning,'wizard_standards_scan_dir','Error in header read for file: '+blog[i]
			return, 0
		endif
		
		detector = mp.metadata.detector_identity
		serial = mp.metadata.sample_serial
		sample_type = mp.metadata.sample_type
		if sample_type ne 'standard' then continue			; filter here on 'standard_type' = "standard"

		energy = mp.energy
		sample = mp.sample
		gain = mp.sensitivity
		pv = mp.IC_name
		pileup = mp.pileup.file
		if (mp.pileup.on eq 0) then pileup = ''		
		throttle = mp.throttle.file
		if (mp.throttle.on eq 0) then throttle = ''		
		xsize = mp.scan.x_pixels
		ysize = mp.scan.y_pixels

		if (mp.metadata.facility eq 'MM.Mel.') and (pv eq '') then begin
			gain = 1.0										; to test using old Udimet data
			pv = 'Maia:dwell.time'
		endif

		if gain lt 1.0e-6 then begin
			gain = 1.0										; is this a good idea?
			print, '** wizard_standards_scan_dir: gain was zero, so set it to 1.0 to continue.'
		endif
		
		el = ''
		j = wizard_standards_find_match( pstate, sample, serial, detector, energy, error=err)
		if err eq 0 then begin
			el = (*pc)[j].el
		endif

;		This defines the names and types of the columns of the table struct.
;		It must match the order of entries in the row struct of the table.
;		This is NOT the selection of the columns as shown. See "wizard_standards_update_table" for that.

		title = ['On','Raw','Name','Serial','Detector','Energy','Xsize','Ysize','PV name','IC Gain','El','Pileup','Throttle', $
				'Conv','Mean','Error','Std.Dev','SD/Error']
		type = ['toggle','file','string','string','string','float','int','int','string','float','string','file','file', $
				'float','float','float','float','float']
		
		row = {on:1, blog:blog[i], name:sample, serial:serial, detector:detector, energy:energy, xsize:xsize, ysize:ysize, $
			pv:pv, gain:gain, el:el, pileup:pileup, throttle:throttle, conv:0.0, mean:0.0, error:0.0, sd:0.0, relsd:0.0}
		
		if wizard_standards_test_row( row) eq 0 then row.on=0

		if n_elements(table) lt 1 then begin
			table = row
		endif else begin
			table = [table, row]
		endelse		
	endfor
	
	if n_elements(table) ge 1 then error = 0	
	return, (error ? 0 : table)
end

;--------------------------------------------------------------------------

function wizard_standards_test_row, row, error=error

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
       warning,'wizard_standards_test_row',['IDL run-time error caught.', '', $
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
	if (row.blog ne '') and (row.detector ne '') and (row.energy ne 0.) and   $
			(row.gain ne 0.) and (row.el ne '') then begin
		error = 0
		return, 1
	endif 
	
	error = 0
	return, 0
end

;--------------------------------------------------------------------------

pro wizard_standards_process_blog, pstate, error=error

; For a single raw file, do the following:
;	1. Form DAI image file for standard 
;			Update pileup, throttle file paths
;			(done in "wizard_standards_callback_image_done" on reply)
;	2. Set and integrate Region
;			Based on conc for element, calculate 'conv' 
;			(done in "wizard_standards_callback_region_done" on reply)
;	
; Later will also need to redo and save Region and Update DAI with the new 'conv' and new charge.

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
       warning,'wizard_standards_process_blog',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	wizard_check_windows, pstate, error=error
	if error then return

	error = 1
	no_data = 1
	p = (*pstate).presults
	n = 0
	if ptr_good(p) eq 1 then begin
		if size( (*p)[0],/tname) eq 'STRUCT' then begin
			no_data = 0
			q = where( (*p)[*].on eq 1, n) 
		endif
	endif
	
	i = (*pstate).loop						; our loop index
	if i ge n then return
	j = q[i]
	(*pstate).index = j
	print,'	process_blog: process index, raw =',j,'  ',(*p)[j].blog
	
	output = wizard_standards_output_file( pstate, (*p)[j].blog, error=err)
	if err then begin
		warning,'wizard_standards_process_blog','Unable to form output file for raw: '+ strip_file_ext(strip_path((*p)[j].blog))
		return
	endif
	
	da_file = wizard_standards_find_config( pstate, (*p)[j], error=err)
	if err then begin
		s = ['Check for the following in the "standards.csv" file: ','Name = '+(*p)[j].name, 'Serial number = '+(*p)[j].serial, $
				'Energy = '+str_tidy((*p)[j].energy), 'Detector = '+(*p)[j].detector,'Note that name matches are case-sensitive.']
		warning,'wizard_standards_process_blog',['No match found in config file for raw: '+ strip_file_ext(strip_path((*p)[j].blog)),'',s]
		return
	endif
	
	gain_value = charge_gain_units( (*p)[j].gain, units=gain_units)
	if gain_value eq 0.0 then begin
		warning,'wizard_standards_process_blog','Missing valid "IC Gain" value.'
		return
	endif
	
	wz = define(/wizard_notify)
	wz.wizard = 'standards'
	wz.window = 'Sort EVT'								; Sort image
	wz.command = 'sort-image'
	wz.pdata = ptr_new( {	$
		device:			(*(*pstate).pDevObj)->name(), $		; device name
		image_mode:		0, $							; sort mode (images)
		type:			7, $							; SXRF data type
		array:			1, $							; array detector
		blog:			(*p)[j].blog, $					; blog file
		pileup:			(*p)[j].pileup, $				; pileup file
		throttle:		(*p)[j].throttle, $				; throttle file
		output:			output, $						; output file
		verify:			1, $							; enable file verification
		pnew:			ptr_new(/allocate_heap), $		; pointer to new (/verify) file-names struct
		conv:			1.0, $							; initial 'conv'
		charge_mode:	1, $							; flux/charge mode (IC w/ PV)
		flux_scaler: 	(*p)[j].pv, $					; scaler ID
		gain_value:		gain_value, $					; IC gain
		gain_units:		gain_units, $					; for gain in 'nA/V' 
		cal:			(*pstate).energy_cal_file, $	; energy calibration file
		proj_mode:		'DA', $							; DA projection mode
		dam:			da_file }, /no_copy)			; DA matrix file
	wz.local = 1
	wz.callback = 'wizard_standards_callback_image_done'
	pw = ptr_new(wz, /no_copy)
	p0 = pw									; first one
	pl = pw									; current one
	
	margin = 0.025							; 2.5% margin
	Xs = (*p)[j].xsize
	Ys = (*p)[j].ysize
	dX = fix( margin * Xs) > 2 				; margin (must be at least 2 pixels each side)
	dY = fix( margin * Ys) > 1 				; margin (must miss at least 1 row, top and bottom)
	
	x = [dX, Xs-1-dX, Xs-1-dX, dX]			; corners
	x = [x, mean(x), (2*x[1]+mean(x))/3]	; plus centre handle and angle handle
	y = [dY, dY, Ys-1-dY, Ys-1-dY]
	y = [y, mean(y), (2*y[1]+mean(y))/3]
	theta = 0.								; rotation angle
	
	wz = define(/wizard_notify)
	wz.wizard = 'standards'
	wz.window = 'Image'						; sum Box region
	wz.command = 'sum-region'
	wz.pdata = ptr_new( {	$
		mode:			0, $				; "+" sum mode
		shape:			1, $				; Box shape index
		x:				x, $				; X handles
		y:				y, $				; Y handles
		theta:			theta, $		 	; Rotation angle
		get_stats:		1, $				; we need stats calculated and returned
		uniform_element: (*p)[j].el, $		; element to test for uniformity
		presults:		ptr_new(/allocate_heap), $		; will return region results here
		local:			1 }, /no_copy)		; if zero, 'presults' managed elsewhere
	wz.local = 1
	wz.callback = 'wizard_standards_callback_region_done'
	pw = ptr_new(wz, /no_copy)
	(*pl).pnext = pw						; link current one to this 'next' one
	pl = pw									; current one

;	The first wizard data pointer is still in use, so we use a new one for the next row ...	
	
	if (*pstate).loop mod 2 then begin
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
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_standards_update_info, pstate, force=force

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
       warning,'wizard_standards_update_info',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif
if n_elements(force) eq 0 then force=0

	i = (*pstate).tab < (n_elements((*pstate).tab_names)-1)
	file = geopixe_root+'wizard/wizard_standards-' + (*pstate).tab_names[i] + '.txt'
	list = wizard_instructions_file( file, error=err)
	if err then begin
		print,'Wizard_standards: text file not found: '+file
	endif else begin
		widget_control, (*pstate).instructions_text, set_value=list
	endelse

	if force or ((*pstate).tab_used[i] eq 0) then begin
		file = geopixe_root + 'wizard/wizard_standards-' + (*pstate).tab_names[i] + '.png'
		figure, file, group=(*pstate).tlb, title='Standards Wizard - Figure '+str_tidy(i+1)
	endif
	(*pstate).tab_used[i] = 1
	return
end

;--------------------------------------------------------------------------

pro wizard_standards_update_export, pstate

; Export the results table

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
       warning,'wizard_standards_update_export',['IDL run-time error caught.', '', $
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
	output = (*pstate).output_dir + 'wizard-standards-results.csv'
	
	on_ioerror, bad
	openw, 1, output
	printf, 1, 'Raw, Name, Serial, Detector, Energy, Xsize, Ysize, IC Gain, El, Conv, Pileup, Throttle, Mean, Error, Std.Dev, SD/Error'
	for i=0,n-1 do begin
		printf, 1, (*p)[i].blog, (*p)[i].name, (*p)[i].serial, (*p)[i].detector, str_tidy((*p)[i].energy), $
				str_tidy((*p)[i].Xsize), str_tidy((*p)[i].Ysize), str_tidy((*p)[i].gain), (*p)[i].el, $
				str_tidy((*p)[i].conv), (*p)[i].pileup, (*p)[i].throttle, str_tidy((*p)[i].mean), $
				str_tidy((*p)[i].error), str_tidy((*p)[i].sd), str_tidy((*p)[i].relsd), $
				format='(15(A,","),A)' 
	endfor

finish:
	close_file, 1
	return
bad:
	warning,'wizard_standards_update_export','Failed to open output file: '+output
	goto, finish
end

;--------------------------------------------------------------------------

pro wizard_standards_update_plots, pstate

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
       warning,'wizard_standards_update_plots',['IDL run-time error caught.', '', $
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
	if n eq 0 then return
	
	conv = (*p).conv
	q1 = where( (conv ne 0.), nq1)
	if nq1 eq 0 then return
	run = long(strip_file_ext(strip_path( (*p).blog)))
	e = (*p).energy
	el_code, (*p).el, el, z, shell, bad, error
	eline = e_line( z, major_line( z, shell))

	det0 = (*(*pstate).detector_list)[(*pstate).detector_mode]
	det = (*(*pstate).detector_list)[(*pstate).detector_new_mode]
	if (det0 ne '') and (det ne '') then begin
		print,'Update_plots: Dets: ',det0,'  ',det
		detector_update, present=det0, new=i, file=f
		pdet0 = read_detector( f, error=error0)
		if error0 then begin
			warning, 'wizard_standards_update_plots','Error reading old Detectors file: '+det0, /error
		endif
		detector_update, present=det, new=i, file=f
		pdet = read_detector( f, error=error)
		if error then begin
			warning, 'wizard_standards_update_plots','Error reading new Detectors file: '+det, /error
		endif
		 
		if (error0 eq 0) and (error eq 0) then begin
			eff0 = detector_efficiency( pdet0, null, eline, effective=aeff0, solid_angle=omega0)
			eff = detector_efficiency( pdet, null, eline, effective=aeff, solid_angle=omega)
			conv = conv * eff0/eff
		endif
	endif
	
	!x.title = 'Run #'
	!y.title = '"Conv" Calibration Factor'
	!p.title = ''
	!p.charsize = 1.0
	!p.thick = 1.0
	!p.charthick = 1.0
	xleg = 0.83
	yleg = 0.16
	dxleg = 0.02
	dyleg = 0.05
	wset, (*pstate).wid1
	
	dr = (max(run)-min(run))*0.03 > 2
	xrange = [min(run)-dr, max(run)+dr]
	yrange = [0.8*(min(conv[q1])>0),1.2*max(conv[q1])]
	done = intarr(n)
	q = indgen(n)
	more = 1
	count = 0
	cols = ['green','red','yellow','orange','l.blue','violet','blue','grey','white','brown']
	repeat begin
		e1 = e[q[0]]
		q0 = where( (e eq e1), nq0)
		q1 = where( (e eq e1) and (conv ne 0.), nq1)
		if count eq 0 then begin
			plot, [run[0],run], [conv[0],conv], color=spec_colour('white'), ticklen=1.0, /nodata, $
					xrange=xrange, yrange=yrange, xstyle=1,ystyle=1
		endif
		if nq1 gt 0 then begin
			oplot, [run[q1[0]],run[q1]], [conv[q1[0]],conv[q1]], color=spec_colour(cols[count]), psym=-((count mod 7)+1)
		endif
		plots, xleg+[0,0],yleg+count*dyleg+[0.005,0.005],/norm, color=spec_colour(cols[count]), psym=((count mod 7)+1)
		xyouts, xleg+dxleg, yleg+count*dyleg,/norm, str_tidy(e[q1[0]])
		count = count+1
		done[q0] = 1

		q = where( done eq 0, more)
	endrep until more eq 0
	
	q1 = where( (conv ne 0.) and (eline gt 0.), nq1)
	if nq1 eq 0 then return

	!x.title = 'Line Energy (keV)'
	!y.title = '"Conv" Calibration Factor'
	!p.title = ''
	!p.charsize = 1.0
	!p.thick = 1.0
	!p.charthick = 1.0
	xleg = 0.83
	yleg = 0.16
	dxleg = 0.02
	dyleg = 0.05
	wset, (*pstate).wid2
	
	dr = (max(eline)-min(eline))*0.03 > 0.5
	xrange = [min(eline)-dr, max(eline)+dr]
	yrange = [0.8*(min(conv[q1])>0),1.2*max(conv[q1])]
	done = intarr(n)
	q = indgen(n)
	more = 1
	count = 0
	cols = ['green','red','yellow','orange','l.blue','violet','blue','grey','white','brown']
	repeat begin
		e1 = e[q[0]]
		q0 = where( (e eq e1), nq0)
		q1 = where( (e eq e1) and (conv ne 0.) and (eline gt 0.), nq1)
		if count eq 0 then begin
			plot, [eline[0],eline], [conv[0],conv], color=spec_colour('white'), ticklen=1.0, /nodata, $
					xrange=xrange, yrange=yrange, xstyle=1,ystyle=1
		endif
		if nq1 gt 0 then begin
			oplot, [eline[q1[0]],eline[q1]], [conv[q1[0]],conv[q1]], color=spec_colour(cols[count]), psym=-((count mod 7)+1)
		endif
		plots, xleg+[0,0],yleg+count*dyleg+[0.005,0.005],/norm, color=spec_colour(cols[count]), psym=((count mod 7)+1)
		xyouts, xleg+dxleg, yleg+count*dyleg,/norm, str_tidy(e[q1[0]])
		count = count+1
		done[q0] = 1

		q = where( done eq 0, more)
	endrep until more eq 0
	
	return
end

;--------------------------------------------------------------------------

pro wizard_standards_update_stats, pstate

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
       warning,'wizard_standards_update_stats',['IDL run-time error caught.', '', $
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
		ch_scale = 1.2
		end
	else: begin
		ch_scale = 1.0
		end
endcase

;	The heading labels chosen here need to be a subset of the 'title' strings for the tags in each
;	row struct of the results table (see 'wizard_standards_scan_dir'), except "#", which is the row index.

	rows = string(indgen(n>1))
	headings = ['#', 'Raw','Name','Serial', 'Energy','El', 'Mean','Error','Std.Dev','SD/Error']
	nc = n_elements(headings)
	widths = [3, 7,5,9, 7,4, replicate(10,nc-6)] * !d.x_ch_size * ch_scale
	t = strarr(nc,256)
	
	if no_data eq 0 then begin
		for i=0,n-1 do begin
			t[0,i] = str_tidy(i)									; first column is just index #
			
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
							t[j,i] = (*p)[i].(k) ? 'On' : 'Off'
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
					endcase
				endif
			endfor
		endfor	
	endif
	widget_control, (*pstate).stats_table, set_value=t, column_widths=widths, align=2, $
						column_labels=headings, table_xsize=nc, table_ysize=n>1
	widget_control, (*pstate).stats_table, use_table_select=[0,0,nc-1,n-1]
	(*pstate).scolumns = nc
	*(*pstate).psheadings = headings
	(*pstate).srows = n
	return
end

;--------------------------------------------------------------------------

pro wizard_standards_update_table, pstate

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
       warning,'wizard_standards_update_table',['IDL run-time error caught.', '', $
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
;	row struct of the results table (see 'wizard_standards_scan_dir'), except "#", which is the row index.

	rows = string(indgen(n>1))
	headings = ['#','On', 'Raw','Name','Serial', 'Detector','Energy', 'Xsize','Ysize', 'IC Gain','El','Conv', 'Pileup','Throttle']
	nc = n_elements(headings)
	widths = [3,5, 7,8,7, replicate(7,2), replicate(5,2), 7,4,12, 10,10] * !d.x_ch_size * ch_scale
	t = strarr(nc,256)
	
	if no_data eq 0 then begin
		for i=0,n-1 do begin
			t[0,i] = str_tidy(i)									; first column is just index #
			
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
							t[j,i] = (*p)[i].(k) ? 'On' : 'Off'
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
					endcase
				endif
			endfor
		endfor	
	endif
	widget_control, (*pstate).results_table, set_value=t, column_widths=widths, align=2, $
						column_labels=headings, table_xsize=nc, table_ysize=n>1
	widget_control, (*pstate).results_table, use_table_select=[0,0,nc-1,n-1]
	(*pstate).columns = nc
	*(*pstate).pheadings = headings
	(*pstate).rows = n
	return
end

;----------------------------------------------------------------------

pro OnRealize_wizard_standards_device_mode, wWidget

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

pro OnRealize_wizard_standards_draw1, wWidget

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
		warning,'OnRealize_wizard_standards_draw1',['IDL run-time error caught.', '', $
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

pro OnRealize_wizard_standards_draw2, wWidget

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
		warning,'OnRealize_wizard_standards_draw2',['IDL run-time error caught.', '', $
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

pro OnRealize_wizard_standards_instructions, wWidget

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
		warning,'OnRealize_wizard_standards_instructions',['IDL run-time error caught.', '', $
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
	wizard_standards_update_info, pstate
	return
end

;----------------------------------------------------------------------

pro OnRealize_wizard_standards_results_table, wWidget

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
		warning,'OnRealize_wizard_standards_results_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	wizard_standards_update_table, pstate
	return
end

;----------------------------------------------------------------------

pro OnRealize_wizard_standards_stats_table, wWidget

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
		warning,'OnRealize_wizard_standards_stats_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	wizard_standards_update_stats, pstate
	return
end

;--------------------------------------------------------------------------

pro wizard_standards, debug=debug

; Wizard to determine the 'conv' calibration factors for standards foils.
; (code assumes GeoPIXE version 7.6s or later [8.6s or later w/ py3])
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

wversion = '8.8'							; wizard version

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
			a = dialog_message('wizard_standards: Failed to restore GeoPIXE.sav.',/error)
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
		warning,'wizard_standards',['IDL run-time error caught.', '', $
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

default = geopixe_defaults(source='wizard_standards')
config = default.path.config
detector_update, list=detector_list, title=detector_title

; List the names of the windows needed, in the format of their 'wizard-action' Notify
; window name. The "open-test" Notify message will be sent to these windows periodically
; to check on their 'open' status. They MUST make a copy of the Notify pointer contents,
; set (*pw).top of the copy to 'event.top' and return the new pw. 

windows_needed = ['Image','Sort EVT']

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
		left_xsize = 760
		left_ysize = 600
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
	warning,'wizard_standards',['Failed to open Device Objects.','Missing "xxx_device__define.sav" files in "/interface" ?']
	return
endif
if obj_valid(DevObjList[0]) eq 0 then begin
	warning,'wizard_standards',['Failed to open Device Objects.','Obj array invalid.']
	return
endif
device_initial = "MAIA_DEVICE"
device = 0
q = where( device_initial eq device_names, nq)
if nq ne 0 then device = q[0]

; 	top-level base

tlb = widget_base( /column, title='Standards Calibration Wizard ' + wversion + ' (GeoPIXE '+version+')', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='wizard-standards-tlb', /TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2 ,xoffset=xoffset, $
					yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

lbase = widget_base( tbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

tab_panel = widget_tab( lbase, location=2, /align_center, uname='wizard-standards-tab-panel')
tab_names = ['input','table','stats','plots']

; Files and paths -----------------------------------------

file_base = widget_base( tab_panel, title='  1. User Input    ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize, uvalue={xresize:left_resize,yresize:1})
label = widget_label( file_base, value='Select raw data directory for standards')
file_text = widget_text( file_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='curve-explanation', tracking=tracking, $
				value=['Select the raw data directory to scan for all "standard" analyses. These need to be collected as image scans and flagged as "standard" in the metadata. ' + $
					'You can edit some parameters in the Table on tab two.'], $
				uvalue={xresize:left_resize, help:'Explanation of the role of the User Input panel.'}, frame=1)


file_base0 = widget_base( file_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, $
				uvalue={xresize:left_resize}, scr_xsize=left_xsize)
label = widget_label( file_base0, value='Select Raw Data File Device')
file_base0b = widget_base( file_base0, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

file_base0c = widget_base( file_base0b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
lab = widget_label( file_base0c, value='Device:')
device_mode = widget_combobox( file_base0c, value=device_titles, uname='device-mode', /tracking, xsize=text_xsize, $
					notify_realize='OnRealize_wizard_standards_device_mode', $
					uvalue='Select input device driver for the raw data file(s).')


file_base1 = widget_base( file_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, $
				uvalue={xresize:left_resize}, scr_xsize=left_xsize)
label = widget_label( file_base1, value='Select Files and Paths')
file_base1b = widget_base( file_base1, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

file_base1c = widget_base( file_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( file_base1c, value='Raw dir:', uname='blog-dir-button', tracking=tracking, $
						uvalue='Click to browse for the raw data directory for any "standard". ', scr_xsize=button_xsize )
blog_dir_text = widget_text( file_base1c, uname='blog-dir-text', value=default.path.data, tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter file-name for the raw data directory for any "standard", or click on button to browse for the dir. '}, scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_standards_blog_dir_text')

file_base1d = widget_base( file_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( file_base1d, value='Energy Cal:', uname='energy-cal-file-button', tracking=tracking, $
						uvalue='Click to browse for the energy calibration SPEC file for all good detectors. Delete any detectors in the SPEC file to exclude these detector channels.', scr_xsize=button_xsize )
energy_cal_file_text = widget_text( file_base1d, uname='energy-cal-file-text', value='', tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter file-name for the energy cal SPEC file for the detector, or click on button to browse for the file. Delete any detectors in the SPEC file to exclude these detector channels.'}, scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_standards_energy_cal_file_text')

file_base1e = widget_base( file_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( file_base1e, value='Output Path:', uname='output-dir-button', tracking=tracking, $
						uvalue='Click to browse for the output directory tree for image and region data. ', scr_xsize=button_xsize )
output_dir_text = widget_text( file_base1e, uname='output-dir-text', value=default.path.analysis, tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter the directory tree for image and region output, or click on button to browse for the dir. '}, scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_standards_output_dir_text')


file_base2 = widget_base( file_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize, uvalue={xresize:left_resize})
label = widget_label( file_base2, value='Resource Files')
file_base2b = widget_base( file_base2, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

file_base2c = widget_base( file_base2b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( file_base2c, value='Resources:', uname='resource-dir-button', tracking=tracking, $
						uvalue='Click to browse for the GeoPIXE Resources directory. This tree contains all the files needed for processing these "standard" data. ' + $
						'The key files are the DA matrix files, stored according to detector and energy (e.g. "384C14/standards/18500eV/Pt_MM15931_384C14_18500eV.damx"). ' + $
						'Defaults to the "path config" on your home "~/.geopixe/geopixe.conf" file.', scr_xsize=button_xsize )
resource_dir_text = widget_text( file_base2c, uname='resource-dir-text', value=default.path.config, tracking=tracking, $
						uvalue={xresize:left_resize, help:'Enter file-name for the GeoPIXE Resources directory, or click on button to browse for the dir. ' + $
						'The key files are the DA matrix files, stored according to detector and energy (e.g. "384C14/standards/18500eV/Pt_MM15931_384C14_18500eV.damx"). ' + $
						'Defaults to the "path config" on your home "~/.geopixe/geopixe.conf" file.'}, scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_standards_resource_dir_text')

file_base3 = widget_base( file_base, /row, xpad=1, ypad=0, space=20, /align_center, /base_align_center)

button = widget_button( file_base3, value='Reload "standards.csv"', uname='reload-standards-button', tracking=tracking, $
						uvalue='Click to reload the "standards.csv" file from Resources. Use this during testing. Later we will hide this button as it is automatic after selecting Resources.', scr_xsize=2*button_xsize )
button = widget_button( file_base3, value='Scan Raw Dir', uname='scan-blog-button', tracking=tracking, $
						uvalue='Click to scan the selected raw dir for all "standard" data and populate the table (on the next tab page). Click on "Next" to move to the next page, or use the tabs on the left.', scr_xsize=2*button_xsize )

; Results table  -----------------------------------------

table_base = widget_base( tab_panel, title='  2. Results Table    ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize, uvalue={xresize:left_resize,yresize:1})
label = widget_label( table_base, value='Work Table and Results')
results_text = widget_text( table_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='table-explanation', tracking=tracking, $
				value=['Table showing the details of the raw files scanned, and parameters needed for the calculation of the calibration factor "conv". ' + $
				'Missing values lacking metadata need to be entered. See more detailed instructions in the info panel (right). When ready click "Process" to start processing.'], $
				uvalue={xresize:left_resize, help:'Explanation of the role of the Results panel.'}, frame=1)

table_base1 = widget_base( table_base, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)

tab2_panel = widget_tab( table_base1, location=0, /align_center, uname='wizard-standards-results-tab-panel')

table1_base = widget_base( tab2_panel, title='  Results Table    ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize, scr_ysize=left_ysize-180, uvalue={xresize:left_resize,yresize:1})

headings = strarr(12)			; dummy values (see 'wizard_standards_update_table' for actual headings)
nc = n_elements(headings)
widths = replicate(6,nc) * !d.x_ch_size * ch_scale
t = strarr(nc,256)

results_table = Widget_Table(table1_base, UNAME='results-table', /all_events, /editable, Y_SCROLL_SIZE=13, $	;, X_SCROLL_SIZE=8, $
				value=t, /RESIZEABLE_COLUMNS, alignment=2, scr_xsize=left_xsize, scr_ysize=left_ysize-182, /no_row_headers, $
				tracking=tracking, uvalue={xresize:left_resize,yresize:1, help:'The table shows the resulting "conv" factors after calibration using the standard foils.'}, $
				column_labels=headings, column_widths=widths, $
				NOTIFY_REALIZE='OnRealize_wizard_standards_results_table')
			
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
button = widget_button( table_base2, value='Process', uname='process-button', tracking=tracking, $
						uvalue='Click to start the calculation of the calibration factors "conv". Make sure the table entries are correct first. ', scr_xsize=1.5*button_xsize )
		
; Stats table  -----------------------------------------

stats_base = widget_base( tab_panel, title='  3. Statistics    ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize, uvalue={xresize:left_resize,yresize:1})
label = widget_label( stats_base, value='Image Statistics')
stats_text = widget_text( stats_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='stats-explanation', tracking=tracking, $
				value=['Table showing statistics for the region selected on the image. See more detailed instructions in the info panel (right). '], $
				uvalue={xresize:left_resize, help:'Explanation of the role of the Stats panel.'}, frame=1)

stats_base1 = widget_base( stats_base, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)

tab2_panel = widget_tab( stats_base1, location=0, /align_center, uname='wizard-standards-stats-tab-panel')

stats2_base = widget_base( tab2_panel, title='  Statistics Table    ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize, scr_ysize=left_ysize-180, uvalue={xresize:left_resize,yresize:1})

sheadings = strarr(12)			; dummy values (see 'wizard_standards_update_stats' for actual headings)
ncs = n_elements(sheadings)
swidths = replicate(6,nc) * !d.x_ch_size
t = strarr(ncs,256)

stats_table = Widget_Table(stats2_base, UNAME='stats-table', /all_events, Y_SCROLL_SIZE=13, $	;, X_SCROLL_SIZE=8, $
				value=t, /RESIZEABLE_COLUMNS, alignment=2, scr_xsize=left_xsize, scr_ysize=left_ysize-182, /no_row_headers, $
				tracking=tracking, uvalue={xresize:left_resize,yresize:1, help:'The table shows the image statistics for the images on the standard foils.'}, column_labels=sheadings, column_widths=swidths, $
				NOTIFY_REALIZE='OnRealize_wizard_standards_stats_table')
			
;stats_base2 = widget_base( stats_base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)

;button = widget_button( stats_base2, value='Clear', uname='table-clear-button', tracking=tracking, $
;						uvalue='Click to clear the entire table. ', scr_xsize=button_xsize )
		
; Plots  -----------------------------------------

plot_base = widget_base( tab_panel, title='  4. Plots    ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize, uvalue={xresize:left_resize,yresize:1})
label = widget_label( plot_base, value='Calibration Results Plots')
plot_text = widget_text( plot_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='plots-explanation', tracking=tracking, $
				value=['Results for "conv" calibration factors plotted against "raw" run or line energy.'], frame=1)

ptab_panel = widget_tab( plot_base, location=3, /align_center, uname='wizard-standards-ptab-panel')
ptab_names = ['run','energy']

prun_base = widget_base( ptab_panel, title='  1.   Run # order    ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize-10, scr_ysize=left_ysize-150, uvalue={xresize:left_resize,yresize:1})

results_draw1 = widget_draw( prun_base, uname='results-draw1', xsize=left_xsize-25, ysize=left_ysize-160, notify_realize='OnRealize_wizard_standards_draw1', $
			retain=retain, uvalue={xresize:left_resize,yresize:1})
 
penergy_base = widget_base( ptab_panel, title='  2.   Line Energy    ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize-10, scr_ysize=left_ysize-150, uvalue={xresize:left_resize,yresize:1})

results_draw2 = widget_draw( penergy_base, uname='results-draw2', xsize=left_xsize-25, ysize=left_ysize-160, notify_realize='OnRealize_wizard_standards_draw2', $
			retain=retain, uvalue={xresize:left_resize,yresize:1})
 
det_base = widget_base( plot_base, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( det_base, value='Detector:  Old:')
dtitle = [' ',detector_title]
dlist = ['',detector_list]
detector_mode = widget_combobox( det_base, value=dtitle, uname='detector-mode', $
					uvalue='Select the existing detector calibration. ', xsize=0.4*text_xsize-50)
lab = widget_label( det_base, value='      New:')
detector_new_mode = widget_combobox( det_base, value=dtitle, uname='detector-new-mode', $
					uvalue='Select a new detector calibration to compare. ', xsize=0.4*text_xsize-50)
					
;------------------------------------------------------------------------------------------------

sbase = widget_base( lbase, /row, xpad=0, ypad=0, space=20, /base_align_center, /align_center)
button = widget_button( sbase, value='  <<  Back  ', uname='back-button', tracking=tracking, uvalue='Go back a page in the procedure to the previous page. ' + $
			' You can also click on the Tab label for a previous page to go directly to it.')
button = widget_button( sbase, value=' Figure ', uname='figure-button', tracking=tracking, uvalue='Re-display the Figure for this tab.')
button = widget_button( sbase, value='  Next  >>  ', uname='next-button', tracking=tracking, uvalue='Go to the next page in the procedure, if all entries have been made and prerequisite steps have been completed.')
		
;------------------------------------------------------------------------------------------------

rbase = widget_base( tbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

instructions_text = widget_list( rbase, scr_xsize=right_xsize, scr_ysize=right_ysize, uname='instruction-explanation', tracking=tracking, $
				value='', Notify_Realize='OnRealize_wizard_standards_instructions', $
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
		tracking:				tracking, $						; tracking mode
		tab:					0, $							; current Tab selected
		tab_names:				tab_names, $					; tab names
		tab_used:				intarr(n_elements(tab_names)), $		; flags that figure is displayed
		windows_needed:			windows_needed, $						; list of needed window names
		windows_open:			ptrarr(n_elements(windows_needed), /allocate_heap), $	; lists unique window IDs found to be open.
		windows_veto:			0, $									; veto Timer when pop-up is open

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
		sel: {left:-1, top:-1, right:-1, bottom:-1, edit:0 }, $	; use "(*pstate).sel.top" as current region

		blog_dir:	 			default.path.data, $			; blog data dir tree
		energy_cal_file:		'', $							; energy cal file name
		output_dir:				default.path.analysis, $		; output dir tree
		resource_dir:			default.path.config, $			; path to GeoPIXE resources for standards
		root:					ptr_new(/allocate_heap), $		; storage foor 'build_output_path' root path

		pconfig:				ptr_new(/allocate_heap), $		; room for standards.csv config table
		loop:					0, $							; loop counter
		index:					0, $							; table row index

		presults:				ptr_new(/allocate_heap), $		; room for results table
		ptitle:					ptr_new(/allocate_heap), $		; names for table struct columns 
		ptype:					ptr_new(/allocate_heap), $		; data-types for table struct columns 

		columns:				nc, $							; table columns
		pheadings:				ptr_new(headings), $			; headings
		rows:					0, $							; rows

		scolumns:				ncs, $							; stats columns
		psheadings:				ptr_new(sheadings), $			; headings
		srows:					0, $							; rows
		
		wid1:					0L, $							; Draw 1 window ID
		wid2:					0L, $							; Draw 2 window ID
		detector_mode:			0, $							; detector (old) droplist setting
		detector_new_mode:		0, $							; detector (new) droplist setting
		detector_list:			ptr_new(/allocate_heap), $		; pointer to list of detector file names
		
		blog_dir_text: 			blog_dir_text, $				; standards curve calc filter filename text ID
		energy_cal_file_text:	energy_cal_file_text, $			; standards curve calc yield filename text ID
		output_dir_text: 		output_dir_text, $				; standards curve calc outer filename text ID
		resource_dir_text: 		resource_dir_text, $			; standards curve calc inner filename text ID

		results_table:			results_table, $				; results table ID
		stats_table:			stats_table, $					; stats table ID

		ptab_panel:				ptab_panel, $					; plot tab ID
		results_draw1:			results_draw1, $				; results plot1 (by run)
		results_draw2:			results_draw2, $				; results plot2 (by energy)
		detector_mode_id:		detector_mode, $				; detector (old) droplist ID
		detector_new_mode_id:	detector_new_mode, $			; detector (new) droplist ID
		
		instructions_text:		instructions_text, $			; instructions text ID
		help:					help $							; help text ID
		}

*(state.detector_list) = dlist

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
xmanager, 'wizard_standards', tlb, /no_block

wizard_test_windows, 'standards', pstate				; check for open GeoPIXE windows and
widget_control, tlb, timer=8.0							; start timer to check periodically
return
end
				