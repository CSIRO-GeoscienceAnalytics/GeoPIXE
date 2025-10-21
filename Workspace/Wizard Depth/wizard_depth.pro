
;	Wizard to determine the depth of particles that appear as hot-spots on images.
; 
;	Calculate the depth of particles seen in hot-spots in images.
;	First, calculate a "Depth Curve" relating the ratio of Outer/Inner detector signals to
;	particle depth. Then produce RGB maps of particles with colour as a depth cue.
;	Finally, repeatedly determine Outer/Inner signal for spectra extracted from 
;	hot-spot regions to infer depth from for each from the Depth Curve.

pro wizard_depth_event, event

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
		warning,'wizard_depth_event',['IDL run-time error caught.', '', $
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
						'Go back to previous tab using "Back" or the select specific tabs on the left.'
		endelse
		goto, finish
		end
	'WIDGET_TIMER': begin
		if *(*pstate).message.pwizard ne !null then begin
			warning, 'Depth Wizard', *(*pstate).message.pwizard
			*(*pstate).message.pwizard  = !null
		endif
		if *(*pstate).message.pwindow ne !null then begin
			warning, 'Depth Wizard', *(*pstate).message.pwindow
			*(*pstate).message.pwindow  = !null
		endif
		wizard_test_windows, 'depth', pstate				; periodically check which GeoPIXE windows are
		widget_control, event.id, timer=20.0	
		goto, finish
		end

	'NOTIFY': begin										; events from other GeoPIXE windows
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end

;			These are the events returned from GeoPIXE windows after 'wizard-action' operations.
;			We check for an error return, and for callback operations to perform, and then if there
;			is a next 'wizard-action' event to send.

			'wizard-return': begin	
				if ptr_valid( event.pointer) then begin
					pw = event.pointer

					if (*pw).wizard ne 'depth' then begin
						print,'Wizard Depth: found another Wizard: '+(*pw).wizard
						*(*pstate).message.pwizard = 	['Another Wizard appears to be open ("'+(*pw).wizard+'").', '', $
								'This will cause many problems.','Only open one Wizard at a time.','Please close other Wizards.']
						goto, finish
					endif

;					A reply from a window to 'open-test' shows that it is open ...
;					increment a count to test if unwanted duplicates are open.

					if (*pw).command eq 'open-test' then begin
						wizard_check_window_id, needed=(*pstate).windows_needed, open=(*pstate).windows_open, name=(*pw).window, id=(*pw).top, count=count, error=error
						if error then goto, finish

						if count gt 1 then begin
							print,'Wizard Depth: found duplicate windows.'
							*(*pstate).message.pwindow = ['Multiple windows of types "'+strjoin((*pstate).windows_needed,', ')+'" may be open.', '', $
								'This may cause problems.','Please close any duplicate windows.']
						endif
						goto, finish
					endif
						
					print, '*** Wizard return: from="'+(*pw).window+'", command="'+(*pw).command+'", error='+str_tidy((*pw).error)
					if (*pw).error then goto, finish
						
;					If there is a callback routine, execute it now, passing the returned wizard notify pointer ...

					if (*pw).callback ne '' then begin
						print, '   >>> callback = "'+(*pw).callback
						call_procedure, (*pw).callback, pstate, pw, error=error 
						if error then goto, finish
					endif

;					If there is a valid notify struct pointed to from 'pnext' in the returned event,
;					then notify that in turn ...
;					Note that we don't free linked list memory here.

					if ptr_good( (*pw).pnext) then begin
						print, '   >>> Notify next, to="'+(*(*pw).pnext).window+'", command="'+(*(*pw).pnext).command
						notify, 'wizard-action', (*pw).pnext
					endif
				endif
				goto, finish
				end

			'new-yields': begin										; from layer_setup "New" button
				if ptr_valid( event.pointer) then begin
					F = *event.pointer
					if F ne '' then begin
;						(*p).yield_file = F
						yields = read_yield(F, /many, error=error)
						if error or (n_elements(*yields) le 1) then begin
							warning, 'wizard_depth',['Error in transferred PIXE/SXRF Yield file.', $
								'Recalculate yields and save them to a YIELD file.', $
								'Make sure it contains a 1D depth series of yield calculations.']
						endif else begin
							(*pstate).depth_curve_yield_file = F[0]
							set_widget_text, (*pstate).depth_curve_yield_text, F[0]
						endelse
						if ptr_valid(yields) then ptr_free, yields
					endif
				endif
				goto, finish
				end
			else:
		endcase
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request wizard_depth ...'
		goto, kill
		end
	else:
endcase

; From here we process other events from widgets and the top-level base
; By convention all widgets have a 'uname' to identify them.

uname = widget_info( event.id, /uname)
case uname of

	'wizard-depth-tlb': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				wizard_resize, event.top, oldx=(*pstate).tlb_xsize, oldy=(*pstate).tlb_ysize, $
								minx=750, miny=500
				geom = widget_info( event.top, /geometry)
				(*pstate).tlb_xsize = geom.scr_xsize
				(*pstate).tlb_ysize = geom.scr_ysize
				end
			else:
		endcase
		end

	'wizard-depth-tab-panel': begin
		(*pstate).tab = clip( event.tab, 0, n_elements((*pstate).tab_names)-1)
		wizard_depth_update_info, pstate

;		case (*pstate).tab_names[event.tab] of
;			'Summary': begin		; summary
;				end
;			else:
;		endcase
		end
	
	'back-button': begin
		(*pstate).tab = clip( (*pstate).tab-1, 0, n_elements((*pstate).tab_names)-1)
		widget_control, (*pstate).tab_panel, set_tab_current=(*pstate).tab
		wizard_depth_update_info, pstate
		end
		
	'next-button': begin
		(*pstate).tab = clip( (*pstate).tab+1, 0, n_elements((*pstate).tab_names)-1)
		widget_control, (*pstate).tab_panel, set_tab_current=(*pstate).tab
		wizard_depth_update_info, pstate
		end
		
	'figure-button': begin
		wizard_depth_update_info, pstate, /force
		end
		
	'depth-curve-filter-file-button': begin
		F = file_requester( /read, title='Select an external filter file', path=geopixe_root, $
							file=(*pstate).depth_curve_filter_file, filter='*.filter', group=event.top )
		if F[0] ne '' then begin
			(*pstate).depth_curve_filter_file = F[0]
			set_widget_text, (*pstate).depth_curve_filter_text, F[0]
		endif
		end
		
	'depth-curve-filter-text': begin
		widget_control, event.id, get_value=s
		(*pstate).depth_curve_filter_file = s
		end
			
	'depth-curve-yield-file-button': begin
		F = file_requester( /read, title='Select an existing Yield file', path=*(*pstate).path, $
							file=(*pstate).depth_curve_yield_file, filter='*.yield', group=event.top )
		if F[0] ne '' then begin
			(*pstate).depth_curve_yield_file = F[0]
			*(*pstate).path = extract_path( F[0]) 
			set_widget_text, (*pstate).depth_curve_yield_text, F[0]
		endif
		end
		
	'depth-curve-yield-text': begin
		widget_control, event.id, get_value=s
		(*pstate).depth_curve_yield_file = s
		*(*pstate).path = extract_path( s) 
		end
		
	'depth-curve-yield-new-button': begin
		layer_setup, group_leader=event.top, TLB=tlb, pars=(*pstate).player, path=*(*pstate).path
		register_notify, event.top, [ 'path','new-yields'], from=tlb
		end
			
	'depth-curve-outer-file-button': begin
		F = file_requester( /read, title='Select the "Outer" detector selection file', path=*(*pstate).path, $
							file=(*pstate).depth_curve_outer_file, filter='*.select.csv', group=event.top )
		if F[0] ne '' then begin
			(*pstate).depth_curve_outer_file = F[0]
			*(*pstate).path = extract_path( F[0]) 
			set_widget_text, (*pstate).depth_curve_outer_text, F[0]
			set_widget_text, (*pstate).cal_spec_outer_text, F[0]
			set_widget_text, (*pstate).spectra_outer_text, F[0]
		endif
		end
		
	'depth-curve-outer-text': begin
		widget_control, event.id, get_value=s
		(*pstate).depth_curve_outer_file = s
		*(*pstate).path = extract_path( s) 
		set_widget_text, (*pstate).depth_curve_outer_text, F[0]
		set_widget_text, (*pstate).cal_spec_outer_text, F[0]
		set_widget_text, (*pstate).spectra_outer_text, F[0]
		end
		
	'depth-curve-outer-inner-set': begin
		detector_select, group=event.top, TLB=tlb, pars=(*pstate).pselect, path=*(*pstate).path
		end
		
	'depth-curve-inner-file-button': begin
		F = file_requester( /read, title='Select the "Inner" detector selection file', path=*(*pstate).path, $
							file=(*pstate).depth_curve_inner_file, filter='*.select.csv', group=event.top )
		if F[0] ne '' then begin
			(*pstate).depth_curve_inner_file = F[0]
			*(*pstate).path = extract_path( F[0]) 
			set_widget_text, (*pstate).depth_curve_inner_text, F[0]
			set_widget_text, (*pstate).cal_spec_inner_text, F[0]
			set_widget_text, (*pstate).spectra_inner_text, F[0]
		endif
		end
		
	'depth-curve-inner-text': begin
		widget_control, event.id, get_value=s
		(*pstate).depth_curve_inner_file = s
		*(*pstate).path = extract_path( s) 
		set_widget_text, (*pstate).depth_curve_inner_text, F[0]
		set_widget_text, (*pstate).cal_spec_inner_text, F[0]
		set_widget_text, (*pstate).spectra_inner_text, F[0]
		end
		
	'depth-curve-calc-button': begin
		wizard_depth_calc_depth_curve, pstate, error=err
		if err eq 0 then set_widget_text, (*pstate).depth_curve_depth_text, (*pstate).depth_curve_depth_file
		end

	'depth-curve-depth-file-button': begin
		F = file_requester( /read, title='Select the Depth Curve output file', path=*(*pstate).path, $
							file=(*pstate).depth_curve_depth_file, filter='*.csv', group=event.top )
		if F[0] ne '' then begin
			(*pstate).depth_curve_depth_file = F[0]
			*(*pstate).path = extract_path( F[0]) 
			set_widget_text, (*pstate).depth_curve_depth_text, F[0]
		endif
		end
		
	'depth-curve-depth-text': begin
		widget_control, event.id, get_value=s
		*(*pstate).path = extract_path( s) 
		(*pstate).depth_curve_depth_file = s
		end
		
	'cal-spec-cal-file-button': begin
		F = file_requester( /read, title='Select the Cal Spec file', path=*(*pstate).path, $
							file=(*pstate).cal_spec_cal_file, filter='*.spec', group=event.top )
		if F[0] ne '' then begin
			(*pstate).cal_spec_cal_file = F[0]
			*(*pstate).path = extract_path( F[0]) 
			set_widget_text, (*pstate).cal_spec_cal_file_text, F[0]
		endif
		end
		
	'cal-spec-cal-file-text': begin
		widget_control, event.id, get_value=s
		*(*pstate).path = extract_path( s) 
		(*pstate).cal_spec_cal_file = s
		end
		
	'cal-spec-calc-button': begin
		wizard_depth_form_cals, pstate, error=err
		if err eq 0 then begin
			set_widget_text, (*pstate).cal_spec_output_outer_file_text, (*pstate).cal_spec_output_outer_file
			set_widget_text, (*pstate).cal_spec_output_inner_file_text, (*pstate).cal_spec_output_inner_file
		endif
		end

	'cal-spec-output-outer-file-button': begin
		F = file_requester( /read, title='Select the "Outer" Cal Spec file', path=*(*pstate).path, $
							file=(*pstate).cal_spec_output_outer_file, filter='*.spec', group=event.top )
		if F[0] ne '' then begin
			(*pstate).cal_spec_output_outer_file = F[0]
			*(*pstate).path = extract_path( F[0]) 
			set_widget_text, (*pstate).cal_spec_output_outer_file_text, F[0]
		endif
		end
		
	'cal-spec-output-outer-text': begin
		widget_control, event.id, get_value=s
		*(*pstate).path = extract_path( s) 
		(*pstate).cal_spec_output_outer_file = s
		end
		
	'cal-spec-output-inner-file-button': begin
		F = file_requester( /read, title='Select the "Inner" Cal Spec file', path=*(*pstate).path, $
							file=(*pstate).cal_spec_output_inner_file, filter='*.spec', group=event.top )
		if F[0] ne '' then begin
			(*pstate).cal_spec_output_inner_file = F[0]
			*(*pstate).path = extract_path( F[0]) 
			set_widget_text, (*pstate).cal_spec_output_inner_file_text, F[0]
		endif
		end
		
	'cal-spec-output-inner-text': begin
		widget_control, event.id, get_value=s
		*(*pstate).path = extract_path( s) 
		(*pstate).cal_spec_output_inner_file = s 
		end
		
	'rgb-map-sort-button': begin
		wizard_depth_sort_outer_inner, pstate, error=err
		end

	'rgb-map-outer-file-button': begin
		F = file_requester( /read, title='Select the "Outer" Image DAI file', path=*(*pstate).path, $
							file=(*pstate).rgb_map_outer_file, filter='*.dai', group=event.top )
		if F[0] ne '' then begin
			(*pstate).rgb_map_outer_file = F[0]
			*(*pstate).path = extract_path( F[0]) 
			set_widget_text, (*pstate).rgb_map_outer_file_text, F[0]
		endif
		end
		
	'rgb-map-outer-text': begin
		widget_control, event.id, get_value=s
		*(*pstate).path = extract_path( s) 
		(*pstate).rgb_map_outer_file = s 
		end
		
	'rgb-map-inner-file-button': begin
		F = file_requester( /read, title='Select the "Inner" Image DAI file', path=*(*pstate).path, $
							file=(*pstate).rgb_map_inner_file, filter='*.dai', group=event.top )
		if F[0] ne '' then begin
			(*pstate).rgb_map_inner_file = F[0]
			*(*pstate).path = extract_path( F[0]) 
			set_widget_text, (*pstate).rgb_map_inner_file_text, F[0]
		endif
		end
		
	'rgb-map-inner-text': begin
		widget_control, event.id, get_value=s
		*(*pstate).path = extract_path( s) 
		(*pstate).rgb_map_inner_file = s 
		end
		
	'rgb-map-element-text': begin
		widget_control, event.id, get_value=s
		(*pstate).rgb_map_element = s
		set_widget_text, (*pstate).rgb_map_element_text, s
		set_widget_text, (*pstate).spectra_element_text, s
		end
		
	'rgb-map-rgb-display-button': begin
		widget_control, (*pstate).rgb_map_element_text, get_value=s
		(*pstate).rgb_map_element = s
		wizard_depth_rgb_display, pstate, error=error
		end
		
	'spectra-extract-button': begin
		wizard_depth_extract, pstate, error=error
		end
		
	'spectra-output-outer-file-button': begin
		F = file_requester( /read, title='Select the "Outer" Sum Spec file', path=*(*pstate).path, $
							file=(*pstate).cal_spec_output_outer_file, filter='*.spec', group=event.top )
		if F[0] ne '' then begin
			(*pstate).spectra_output_outer_file = F[0]
			*(*pstate).path = extract_path( F[0]) 
			set_widget_text, (*pstate).spectra_output_outer_file_text, F[0]
		endif
		end
		
	'spectra-output-outer-text': begin
		widget_control, event.id, get_value=s
		*(*pstate).path = extract_path( s) 
		(*pstate).spectra_output_outer_file = s
		end
		
	'spectra-output-inner-file-button': begin
		F = file_requester( /read, title='Select the "Inner" Sum Spec file', path=*(*pstate).path, $
							file=(*pstate).cal_spec_output_inner_file, filter='*.spec', group=event.top )
		if F[0] ne '' then begin
			(*pstate).spectra_output_inner_file = F[0]
			*(*pstate).path = extract_path( F[0]) 
			set_widget_text, (*pstate).spectra_output_inner_file_text, F[0]
		endif
		end
		
	'spectra-output-inner-text': begin
		widget_control, event.id, get_value=s
		*(*pstate).path = extract_path( s) 
		(*pstate).spectra_output_inner_file = s
		end
		
	'spectra-fit-button': begin
		widget_control, (*pstate).spectra_element_text, get_value=s
		(*pstate).rgb_map_element = s
		set_widget_text, (*pstate).rgb_map_element_text, s
		wizard_depth_fit, pstate, error=error
		end
		
;	'results-delete': begin
;		if n_elements( *(*pstate).presults) eq 0 then begin
;			*(*pstate).presults = results
;		endif else begin
;			*(*pstate).presults = [*(*pstate).presults,results]
;		endelse
;		wizard_depth_update_table, pstate
;		end

	'results-clear-button': begin
		if ptr_valid( (*pstate).presults) then ptr_free, (*pstate).presults
		(*pstate).presults = ptr_new(/alloc)
		wizard_depth_update_table, pstate
		end

	'results-export-button': begin
		F = file_requester( /write, title='Export results to a CSV file', path=*(*pstate).path, $
							file='', filter='*.csv', group=event.top )
		if F[0] ne '' then begin
			F = strip_file_ext( F[0]) + '.csv'
			wizard_depth_export_table, pstate, F[0], error=err
		endif
		end
		
	else:
endcase

finish:
	widget_control, hourglass=0
	return

done:
	goto, kill

bad_state:
	warning,'wizard_depth_event',['STATE variable has become ill-defined.','Abort Maia setup.'],/error
	goto, kill

; Free memory and exit cleanly ...

kill:
;	heap_free, pstate, /verbose			; slow and not recommended

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).player) then ptr_free, (*pstate).player
	if ptr_valid( (*pstate).pselect) then ptr_free, (*pstate).pselect
	if ptr_valid( (*pstate).presults) then ptr_free, (*pstate).presults

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

pro wizard_depth_callback_calc_depths, pstate, pep, error=error

; Callback to: Use returned fit data to form ratio and then depth via "Depth Curve"
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
       warning,'wizard_depth_callback_calc_depths',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

	error = 1
;	Take the returned outer, inner areas to form ratio ...
;	Compare to Depth Curve and add to results table.

	d = *(*pep).pdata
	if n_elements(d) lt 4 then return
	oarea = d[0]
	oerror = d[1]
	iarea = d[2]
	ierror = d[3]
	ratio = oarea/iarea
	eratio = sqrt( (oerror/oarea)^2 + (ierror/iarea)^2)
	print, 'ratio=',ratio, ', err=',eratio
	oname = strip_path(strip_file_ext( (*pstate).spectra_output_outer_file))
	iname = strip_path(strip_file_ext( (*pstate).spectra_output_inner_file))
	
	g = get_depth_curve( (*pstate).depth_curve_depth_file, error=error)
	if error then begin
		depth = 0.0
		edepth = 0.0
	endif else begin
		el_code, (*pstate).rgb_map_element,el,z,shell
		q = where( (z[0] eq g.z) and (shell[0] eq g.shell), nq)
		if nq eq 0 then begin
			warning,'wizard_depth',['Selected element "'+(*pstate).rgb_map_element+'" not found', $
					'in depth file: '+(*pstate).depth_curve_depth_file]
			depth = 0.0
			edepth = 0.0
		endif else begin
			d = g.data[0,*]
			r = g.data[q[0],*]
			depth = (interpol( d, r, ratio ))[0] > 0.0
			q = where(depth lt d, nq)
			if nq eq 0 then q=g.n_depth-1
			i = clip( q[0], 0, g.n_depth-2)
			edepth = abs(eratio * (d[i+1]-d[i])/(r[i+1]-r[i])) 							
		endelse
	endelse
	
	results = { $
		oname:		oname, $		; name of Outer spec file
		iname:		iname, $		; name of Inner spec file
		oarea:		oarea, $		; Outer area
		oerror:		oerror, $		; Outer error
		iarea:		iarea, $		; Inner area
		ierror:		ierror, $		; Inner error
		ratio:		ratio, $		; ratio Outer/Inner
		eratio:		eratio, $		; error ratio
		depth:		depth, $		; depth (um)
		edepth:		edepth $		; depth error (um)
		}
		
	if n_elements( *(*pstate).presults) eq 0 then begin
		*(*pstate).presults = results
	endif else begin
		*(*pstate).presults = [*(*pstate).presults,results]
	endelse
	wizard_depth_update_table, pstate
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_callback_form_cals1, pstate, pep, error=error

; Callback to: Set Outer detectors Cal spec file widget
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
       warning,'wizard_depth_callback_form_cals1',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return
	pd = (*pep).pdata
	
	(*pstate).cal_spec_output_outer_file = *(*pd).poutput
	set_widget_text, (*pstate).cal_spec_output_outer_file_text, (*pstate).cal_spec_output_outer_file 
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_callback_form_cals2, pstate, pep, error=error

; Callback to: Set Inner detectors Cal spec file widget
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
       warning,'wizard_depth_callback_form_cals2',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return
	pd = (*pep).pdata
	
	(*pstate).cal_spec_output_inner_file = *(*pd).poutput
	set_widget_text, (*pstate).cal_spec_output_inner_file_text, (*pstate).cal_spec_output_inner_file 
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_callback_set_outer_inner, pstate, pep, error=error

; Callback to: Set text widgets values for Spec files for Inner and Outer detectors.
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
       warning,'wizard_depth_set_outer_inner',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return
	pd = (*pep).pdata
	
	file = (*pd).output
	*(*pstate).path = extract_path( file)
	case (*pep).qual1 of
		'inner': begin
			(*pstate).rgb_map_inner_file = file
			set_widget_text, (*pstate).rgb_map_inner_file_text, (*pstate).rgb_map_inner_file
			end
		'outer': begin
			(*pstate).rgb_map_outer_file = file
			set_widget_text, (*pstate).rgb_map_outer_file_text, (*pstate).rgb_map_outer_file
			end
	endcase
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_callback_sum_spectra1, pstate, pep, error=error

; Callback to: Set-up for forming "Outer" detectors sum spec 
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
       warning,'wizard_depth_callback_sum_spectra1',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return
	file = *(*pep).pdata
	fouter = strip_file_m(strip_file_ext(file),ending='-detector')+'-outer.spec'

	*(*pstate).parg1 = fouter				; store 'fouter' for later
	*(*pstate).parg2 = file					; store 'file' later
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_callback_sum_spectra2, pstate, pep, error=error

; Callback to: Set Outer detectors sum spec file widget
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
       warning,'wizard_depth_callback_sum_spectra2',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return
	pd = (*pep).pdata
	
;	'poutput' ptr here is actually the 'parg1' ptr filled in with 'fouter' by the 
;	callback routine "wizard_depth_callback_sum_spectra1"

	(*pstate).spectra_output_outer_file = *(*pd).poutput			; could have used '*(pstate).parg1' here equally
	set_widget_text, (*pstate).spectra_output_outer_file_text, (*pstate).spectra_output_outer_file 
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_callback_sum_spectra3, pstate, pep, error=error

; Callback to: Set-up for forming "Inner" detectors sum spec 
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
       warning,'wizard_depth_callback_sum_spectra3',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return
	
	file = *(*pep).pdata					; could have used '*(*pstate).parg2' equally here
	finner = strip_file_m(strip_file_ext(file),ending='-detector')+'-inner.spec'

	*(*pstate).parg3 = finner				; store 'finner' for later
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_callback_sum_spectra4, pstate, pep, error=error

; Callback to: Set Inner detectors sum spec file widget, and set-up to load both Outer,Inner
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
       warning,'wizard_depth_callback_sum_spectra4',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	error = 1
	if (*pep).error ne 0 then return
	pd = (*pep).pdata
	
	(*pstate).spectra_output_inner_file = *(*pd).poutput
	set_widget_text, (*pstate).spectra_output_inner_file_text, (*pstate).spectra_output_inner_file 

	fouter = (*pstate).spectra_output_outer_file
	finner = (*pstate).spectra_output_inner_file

	*(*pstate).parg4 = [fouter,finner]
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_calc_depth_curve, pstate, error=err

; Calculate the Outer/Inner versus depth curve.

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
       warning,'wizard_depth_calc_depth_curve',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, finish
    endif
endif

	err = 1
	if (strlen((*pstate).depth_curve_filter_file) eq 0) then begin
		warning,'wizard_depth_calc_depth_curve',['No filter file is specified.', '', $
					'Select the filter file for external absorbers, or load the "Null" filter.', $
					'and also fill in the other fields in top panel.', $
					'Then re-try this step.']
		return
	endif
	if (strlen((*pstate).depth_curve_yield_file) eq 0) then begin
		warning,'wizard_depth_calc_depth_curve',['No yield file is specified.', '', $
					'Select the YIELD file for the 2-layer sample problem,','or click on "New" to create one.', $
					'Also fill in the other fields in top panel.', $
					'Then re-try this step.']
		return
	endif
	if (strlen((*pstate).depth_curve_outer_file) eq 0) or (strlen((*pstate).depth_curve_inner_file) eq 0) then begin
		warning,'wizard_depth_calc_depth_curve',['No "Outer" and/or "Inner" detector select files are specified.', '', $
					'Select the desired "Outer" / "Inner" .select.csv files,', 'and also other fields in top panel.', $
					'and re-try this step.']
		return
	endif
	
	Ffilter = (*pstate).depth_curve_filter_file
	Fyields = (*pstate).depth_curve_yield_file
	Factive1 = (*pstate).depth_curve_outer_file
	Factive2 = (*pstate).depth_curve_inner_file
	output = (*pstate).depth_curve_depth_file
;	if strlen(output) lt 1 then begin
		output = file_requester( /read, title='Select the Depth Curve output file', path=*(*pstate).path, $
							file=output, filter='*.csv', group=(*pstate).tlb )
		if output[0] eq '' then begin
			warning, 'wizard_depth_calc_depth_curve','No valid Depth Curve output file provided.', /error
			goto, finish
		endif
		output = strip_file_ext(output) + '.csv'
;	endif

	pfilter = read_filters( Ffilter, error=error)
	if error then begin
		warning, 'wizard_depth_calc_depth_curve','Error reading Filter file '+Ffilter, /error
		goto, finish
	endif
	
	pyields = read_yield(Fyields, error=error, /many)
	if error then begin
		warning, 'wizard_depth_calc_depth_curve','Error reading Yield file: '+Fyields, /error
		goto, finish
	endif else begin
		if (*pyields)[0].array then begin
			fdetector = strip_path((*pyields)[0].detector_file)
			detector_update, present=fdetector, new=i, file=f
			pdetector = read_detector( f, error=error)
			if error then begin
				warning, 'wizard_depth_calc_depth_curve','Error reading Detectors file: '+fdetector, /error
				goto, finish
			endif else begin
				if (*pdetector).array and (strlen((*pdetector).layout) gt 0) then begin
					d = read_detector_layout( (*pdetector).layout, error=error)
					if error then begin
						warning, 'wizard_depth_calc_depth_curve','Error reading Detectors layout file: '+(*pdetector).layout, /error
						goto, finish
					endif else begin
						playout = ptr_new( d, /no_copy)
					endelse
				endif else begin
					warning, 'wizard_depth_calc_depth_curve',['Error reading Detectors layout file.','Not an array or no layout found.'], /error
					goto, finish
				endelse				
			endelse
		endif else begin
			warning, 'wizard_depth_calc_depth_curve',['Error in yields file.','Not a detector array yield file.'], /error
			goto, finish
		endelse				
	endelse

	n = get_select( Factive1[0], error=err)
	if err or (n[0] eq -1) then begin
		warning, 'wizard_depth_calc_depth_curve',['Error reading "Outer" .select.csv file:',Factive1[0]], /error
		goto, finish
	endif
	active1 = n

	n = get_select( Factive2[0], error=err)
	if err or (n[0] eq -1) then begin
		warning, 'wizard_depth_calc_depth_curve',['Error reading "Inner" .select.csv file:',Factive2[0]], /error
		goto, finish
	endif
	active2 = n
	
	progress, tlb=progress_tlb, title='GeoPIXE: PIXE/SXRF Array depth profile Calculation'
	openw, 1, output
	printf,1, 'Table of ratio versus depth for each element'
	printf,1, 'Z, ' + strjoin( (*pyields)[0].z, ', ')
	printf,1, 'element, ' + strjoin( element_name((*pyields)[0].z), ', ')
	printf,1, 'shell, ' + strjoin( (*pyields)[0].shell, ', ')
	for i=0,n_elements(*pyields)-1 do begin
		progress, /update, progress_tlb, {unit:0, value:0, current:i, size:n_elements(*pyields)}, cancel=cancel
		if cancel then break

		ratio = ratio_detector_yields( pdetector, playout, pfilter, ptr_new((*pyields)[i]), $
						active1, active2)
		printf,1, string( (*pyields)[i].thick[0]) + ', ' + strjoin( ratio, ', ')
	endfor
	
	progress, /complete, progress_tlb, 'Yield calculation completed.'
	progress, /ending, progress_tlb
	
	if ptr_valid(p) then *p = r
	(*pstate).depth_curve_depth_file = output
	err = 0

finish:
	close_file, 1
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_export_table, pstate, file, error=error

; Export results table

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
       warning,'wizard_depth_export_table',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

	error = 1
	presults = (*pstate).presults
	if ptr_good(presults) eq 0 then return
	n = n_elements( *presults)
	if n_elements(file) lt 1 then file=''
	if strlen(file) eq 0 then goto, bad_open
	
	on_ioerror, bad_open
	openw, lun, file, /get_lun
	on_ioerror, bad_open
	
	printf,lun, '# Wizard_depth: Export results'
	printf,lun, '# NOTE: On read do NOT ignore blanks to preserve any spaces in file-names.'
	printf,lun, '#'
	printf,lun, 'n,', str_tidy(n)
	printf,lun, '# i, O file, I file, O area, O error, I area, I error, Ratio, eRatio, Depth, eDepth'
	for i=0,n-1 do begin
		s = ((*presults)[i]).oname + ',' + ((*presults)[i]).iname + ','
		s = s + str_tidy(((*presults)[i]).oarea) + ',' + str_tidy(((*presults)[i]).oerror) + ','
		s = s + str_tidy(((*presults)[i]).iarea) + ',' + str_tidy(((*presults)[i]).ierror) + ','
		s = s + str_tidy(((*presults)[i]).ratio) + ',' + str_tidy(((*presults)[i]).eratio) + ','
		s = s + str_tidy(((*presults)[i]).depth) + ',' + str_tidy(((*presults)[i]).edepth)
		printf,lun, str_tidy(i) + ',' + s
	endfor
	error = 0
	goto, finish

bad_open:
	warning,'wizard_depth_export','Failed to open file = "'+file+'"'
	goto, finish
bad_write:
	warning,'wizard_depth_export','Bad write to file = "'+file+'"'
	goto, finish
finish:
	close_file, lun
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_extract, pstate, error=error

; Form Spec files for Inner and Outer detectors.

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
       warning,'wizard_depth_extract',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	wizard_check_windows, pstate, error=error
	if error then return

	error = 1
	if (strlen((*pstate).depth_curve_outer_file) eq 0) or (strlen((*pstate).depth_curve_inner_file) eq 0) then begin
		warning,'wizard_depth_extract',['No "Outer" and/or "Inner" detector select files are specified.', $
					'Select the desired "Outer" / "Inner" .select.csv files,', $
					'and re-try this step.']
		return
	endif

;	Build a linked list of Notify structs with commands for GeoPIXE windows, and callbacks for
;	operations to be performed in this wizard after each operation.

	wz = define(/wizard_notify)
	wz.wizard = 'depth'
	wz.window = 'Image Regions'
	wz.command = 'extract-individual'
	wz.callback = 'wizard_depth_callback_sum_spectra1'
	wz.pdata = ptr_new(/allocate_heap)					; will return file-name
	wz.local = 1
	p = ptr_new(wz, /no_copy)
	pl = p
	p0 = p

;	Use the (*pstate).parg1 ptr to enable this next message to use the 'fouter' filename filled into
;	'parg1' by the callback routine "wizard_depth_callback_sum_spectra1".
;
;	NOTE: Never trust the arg pointers to start with, as they may have been freed with a previous
;	pwizard linked list. Always create them afresh for a new list, as here.
	
	wz = define(/wizard_notify)						; sum all 'outer' detectors
	wz.wizard = 'depth'
	wz.window = 'Spectrum Display'
	wz.command = 'sum-selected-spectra'
	wz.pdata = ptr_new( {	$
		select:			(*pstate).depth_curve_outer_file, $		; select (*.select.csv) or spec (*.spec) file
		label:			'Outer', $								; 'label' string for spectrum
		poutput:		(*pstate).parg1 }, /no_copy)			; ptr to output spec file-name
	wz.local = 1
	wz.callback = 'wizard_depth_callback_sum_spectra2'
	p = ptr_new(wz, /no_copy)
	(*pl).pnext = p
	pl = p
	
;	Use the (*pstate).parg2 ptr to enable this next message to use the 'file' filename filled into
;	'parg2' by the callback routine "wizard_depth_callback_sum_spectra1"
	
	wz = define(/wizard_notify)						; reload all detectors
	wz.wizard = 'depth'
	wz.window = 'Spectrum Display'
	wz.command = 'load-spectra'
	wz.pdata = (*pstate).parg2
	wz.local = 0									; do not FREE 'parg2'
	wz.callback = 'wizard_depth_callback_sum_spectra3'
	p = ptr_new(wz, /no_copy)
	(*pl).pnext = p
	pl = p
	
;	Use the (*pstate).parg3 ptr to enable this next message to use the 'finner' filename filled into
;	'parg3' by the callback routine "wizard_depth_callback_sum_spectra3"
		
	wz = define(/wizard_notify)						; sum all 'inner' detectors
	wz.wizard = 'depth'
	wz.window = 'Spectrum Display'
	wz.command = 'sum-selected-spectra'
	wz.pdata = ptr_new( {	$
		select:			(*pstate).depth_curve_inner_file, $		; select (*.select.csv) or spec (*.spec) file
		label:			'Inner', $								; 'label' string for spectrum
		poutput:		(*pstate).parg3 }, /no_copy)			; ptr to output spec file-name
	wz.local = 1
	wz.callback = 'wizard_depth_callback_sum_spectra4'
	p = ptr_new(wz, /no_copy)
	(*pl).pnext = p
	pl = p
	
;	Use the (*pstate).parg4 ptr to enable this next message to use the [fouter,finner] filenames filled into
;	'parg4' by the callback routine "wizard_depth_callback_sum_spectra4"
		
	wz = define(/wizard_notify)						; load both outer and inner sum spectra
	wz.wizard = 'depth'
	wz.window = 'Spectrum Display'
	wz.command = 'load-spectra'
	wz.pdata = (*pstate).parg4
	wz.local = 0									; do not FREE 'parg4'
	p = ptr_new(wz, /no_copy)
	(*pl).pnext = p
	pl = p	
	
	clear_wizard, (*pstate).pwizard1
	*(*pstate).pwizard1 = *p0
	ptr_free, p0
	notify, 'wizard-action', (*pstate).pwizard1	
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_fit, pstate, error=error

; Fit the Outer, Inner sum spectra to determine peak area for selected element
; in "outer" and "inner" spectra, form ratio, and use Depth Curve to determine depth.

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
       warning,'wizard_depth_fit',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	wizard_check_windows, pstate, error=error
	if error then return

	error = 1
	if strlen((*pstate).depth_curve_depth_file) eq 0 then begin
		warning,'wizard_depth_fit',['No file supplied for depth curve.', '', $
					'Select a valid "Depth Curve" CSV file on the first page,', $
					'or calculate a new one there.']
		return
	endif
	if (strlen((*pstate).spectra_output_outer_file) eq 0) or (strlen((*pstate).spectra_output_inner_file) eq 0) then begin
		warning,'wizard_depth_fit',['No "Outer" and/or "Inner" sum spectra files are specified.', '', $
					'Select the desired "Outer" / "Inner" sum spectra files,', 'or build them using the top "Extract" panel.', $
					'and re-try this step.']
		return
	endif
	if strlen((*pstate).rgb_map_element) eq 0 then begin
		warning,'wizard_depth_fit',['No "element" selection has been made.', '', $
					'Select the desired element to use for depth determination,', $
					'and re-try this step.']
		return
	endif

	wz = define(/wizard_notify)
	wz.wizard = 'depth'
	wz.window = 'Spectrum Display'
	wz.command = 'load-spectra'
	wz.pdata = ptr_new( [(*pstate).spectra_output_outer_file,(*pstate).spectra_output_inner_file])	; file-names
	wz.local = 1
	p = ptr_new( wz, /no_copy)
	pl = p
	p0 = p
	
	wz = define(/wizard_notify)
	wz.wizard = 'depth'
	wz.window = 'Spectrum Fit'
	wz.command = 'fit-all'
	p = ptr_new(wz, /no_copy)
	(*pl).pnext = p
	pl = p
	
	wz = define(/wizard_notify)
	wz.wizard = 'depth'
	wz.window = 'Spectrum Display'
	wz.command = 'outer-inner-ratio'						; form outer/inner ratio
	wz.qual1 = (*pstate).rgb_map_element					; result will come back in 'pdata'
	wz.callback = 'wizard_depth_callback_calc_depths'
	wz.pdata = ptr_new(/allocate_heap)						; will return result array
	wz.local = 1
	p = ptr_new(wz, /no_copy)
	(*pl).pnext = p
	pl = p
	
	clear_wizard, (*pstate).pwizard2
	*(*pstate).pwizard2 = *p0
	ptr_free, p0
	notify, 'wizard-action', (*pstate).pwizard2	
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_form_cals, pstate, error=error

; Form Cal Spec files for Inner and Outer detectors.

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
       warning,'wizard_depth_form_cals',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

	error = 1
	if (strlen((*pstate).cal_spec_cal_file) eq 0) then begin
		warning,'wizard_depth_form_cals',['No Cal SPEC file is specified.', '', $
					'Select the Cal SPEC file for all detector channels,', $
					'and select the desired "Outer" / "Inner" .select.csv files.', $
					'Then re-try this step.']
		return
	endif
	if (strlen((*pstate).depth_curve_outer_file) eq 0) or (strlen((*pstate).depth_curve_inner_file) eq 0) then begin
		warning,'wizard_depth_form_cals',['No "Outer" and/or "Inner" detector select files are specified.', '', $
					'Select the desired "Outer" / "Inner" .select.csv files,', 'and load the Cal SPEC file for all detector channels.', $
					'and re-try this step.']
		return
	endif

	if strlen( (*pstate).cal_spec_output_outer_file) eq 0 then begin
		F1 = file_requester( /read, filter = '*.spec', path=*(*pstate).path, $
			group=(*pstate).tlb, file=(*pstate).cal_spec_output_outer_file, $
			title='Select SPEC file for "Outer" cal selection', fix_filter=0)
		if F1[0] eq '' then return
	endif else F1 = (*pstate).cal_spec_output_outer_file

	if strlen( (*pstate).cal_spec_output_inner_file) eq 0 then begin
		F2 = file_requester( /read, filter = '*.spec', path=*(*pstate).path, $
			group=(*pstate).tlb, file=(*pstate).cal_spec_output_inner_file, $
			title='Select SPEC file for "Inner" cal selection', fix_filter=0)
		if F2[0] eq '' then return
	endif else F2 = (*pstate).cal_spec_output_inner_file
	
;	Store the file-names in the 'parg' storage, which don't need to be freed.

	*(*pstate).parg5 = F1[0]
	*(*pstate).parg6 = F2[0]
	
	wz = define(/wizard_notify)
	wz.wizard = 'depth'
	wz.window = 'Spectrum Display'
	wz.command = 'load-spectra'
	wz.pdata = ptr_new( (*pstate).cal_spec_cal_file)
	wz.local = 1
	p = ptr_new(wz, /no_copy)
	pl = p
	p0 = p
	
	wz = define(/wizard_notify)
	wz.wizard = 'depth'
	wz.window = 'Spectrum Display'
	wz.command = 'select-spectra'
	wz.pdata = ptr_new( {	$
		select:			(*pstate).depth_curve_outer_file, $		; select (*.select.csv) or spec (*.spec) file
		poutput:		(*pstate).parg5 }, /no_copy)			; ptr to output spec file-name
	wz.local = 1
	wz.callback = 'wizard_depth_callback_form_cals1'
	p = ptr_new(wz, /no_copy)
	(*pl).pnext = p
	pl = p
	
	wz = define(/wizard_notify)
	wz.wizard = 'depth'
	wz.window = 'Spectrum Display'
	wz.command = 'load-spectra'
	wz.pdata = ptr_new( (*pstate).cal_spec_cal_file)
	wz.local = 1
	p = ptr_new(wz, /no_copy)
	(*pl).pnext = p
	pl = p
	
	wz = define(/wizard_notify)
	wz.wizard = 'depth'
	wz.window = 'Spectrum Display'
	wz.command = 'select-spectra'
	wz.pdata = ptr_new( {	$
		select:			(*pstate).depth_curve_inner_file, $		; select (*.select.csv) or spec (*.spec) file
		poutput:		(*pstate).parg6 }, /no_copy)			; ptr to output spec file-name
	wz.local = 1
	wz.callback = 'wizard_depth_callback_form_cals2'
	p = ptr_new(wz, /no_copy)
	(*pl).pnext = p
	pl = p
	
	clear_wizard, (*pstate).pwizard3
	*(*pstate).pwizard3 = *p0
	ptr_free, p0
	notify, 'wizard-action', (*pstate).pwizard3	
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_rgb_display, pstate, error=error

; Display RGB depth images using Outer, Inner DAI files.

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
       warning,'wizard_depth_rgb_display',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	wizard_check_windows, pstate, error=error
	if error then return

	error = 1
	if (strlen((*pstate).rgb_map_outer_file) eq 0) or (strlen((*pstate).rgb_map_inner_file) eq 0) then begin
		warning,'wizard_depth_rgb_display',['No "Outer" and/or "Inner" image DAI files are specified.', '', $
					'Select the desired "Outer" / "Inner" DAI files,','or build them using the "Sort" approach in the top panel.', $
					'and re-try this step.']
		return
	endif
	if strlen((*pstate).rgb_map_element) eq 0 then begin
		warning,'wizard_depth_rgb_display',['No "element" selection has been made.', '', $
					'Select the desired element to display in RGB,', $
					'and re-try this step.']
		return
	endif

	wz = define(/wizard_notify)			; create a command struct for Notify, "wizard-action"
	wz.wizard = 'depth'
	wz.window = 'Image'
	wz.command = 'load-image'
	wz.pdata = ptr_new( (*pstate).rgb_map_inner_file)	; "inner" images file
	wz.local = 1
	p = ptr_new(wz, /no_copy)
	pl = p
	p0 = p												; head command in list

	wz = define(/wizard_notify)
	wz.wizard = 'depth'
	wz.window = 'Image RGB'
	wz.command = 'compare-image'
	wz.qual1 = (*pstate).rgb_map_element
	wz.pdata = ptr_new( (*pstate).rgb_map_outer_file)	; "outer" images file
	wz.local = 1										; pdata will be freed by wizard
	p = ptr_new(wz, /no_copy)
	(*pl).pnext = p										; link this command to first
	pl = p
	
	clear_wizard, (*pstate).pwizard4					; clear previous data
	*(*pstate).pwizard4 = *p0							; copy struct to send
	ptr_free, p0
	notify, 'wizard-action', (*pstate).pwizard4	
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_sort_outer_inner, pstate, error=error

; Form Spec files for Inner and Outer detectors.

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
       warning,'wizard_depth_sort_outer_inner',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	wizard_check_windows, pstate, error=error
	if error then return

	error = 1
	if (strlen((*pstate).cal_spec_output_outer_file) eq 0) or (strlen((*pstate).cal_spec_output_inner_file) eq 0) then begin
		warning,'wizard_depth_sort_outer_inner',['No "Outer" and/or "Inner" cal SPEC files are specified on page 2.', '', $
					'Select the desired "Output Cal SPEC files" on page 2,','or build them using the "Form Outer..." approach.', $
					'Open the "Sort EVT" window and load it ','with the set-up for a particle of interest.', $
					'i.e. Load the image DAI for for the particle that uses all detectors.', $
					'Then re-try this step.']
		return
	endif
	
	wz = define(/wizard_notify)
	wz.wizard = 'depth'
	wz.window = 'Sort EVT'
	wz.command = 'sort-image'
	wz.qual1 = 'outer'
	wz.qual2 = 'inner'
	wz.pdata = ptr_new( {	$
		output:			'', $												; output filename on return
		verify:			1, $												; check all file-names first
		cal:			(*pstate).cal_spec_output_outer_file}, /no_copy)	; CAL and channel select
	wz.local = 1
	wz.callback = 'wizard_depth_callback_set_outer_inner'
	p = ptr_new(wz, /no_copy)
	pl = p
	p0 = p
	
	wz = define(/wizard_notify)
	wz.wizard = 'depth'
	wz.window = 'Sort EVT'
	wz.command = 'sort-image'
	wz.qual1 = 'inner'
	wz.qual2 = 'outer'
	wz.pdata = ptr_new( {	$
		output:			'', $												; output filename on return
		cal:			(*pstate).cal_spec_output_inner_file}, /no_copy)	; CAL and channel select
	wz.local = 1
	wz.callback = 'wizard_depth_callback_set_outer_inner'
	p = ptr_new(wz, /no_copy)
	(*pl).pnext = p
	pl = p
	
	clear_wizard, (*pstate).pwizard5
	*(*pstate).pwizard5 = *p0
	ptr_free, p0
	notify, 'wizard-action', (*pstate).pwizard5	
	error = 0
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_update_info, pstate, force=force

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
       warning,'wizard_depth_update_info',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif
if n_elements(force) eq 0 then force=0

	i = (*pstate).tab < (n_elements((*pstate).tab_names)-1)
	file = geopixe_root + 'wizard/wizard_depth-' + (*pstate).tab_names[i] + '.txt'
	list = wizard_instructions_file( file, error=err)
	if err then begin
		print,'Wizard_depth: text file not found: '+file
	endif else begin
		widget_control, (*pstate).instructions_text, set_value=list
	endelse

	if force or ((*pstate).tab_used[i] eq 0) then begin
		file = geopixe_root + 'wizard/wizard_depth-' + (*pstate).tab_names[i] + '.png'
		figure, file, group=(*pstate).tlb, title='Depth Wizard - Figure '+str_tidy(i+1)
	endif
	(*pstate).tab_used[i] = 1
	return
end

;--------------------------------------------------------------------------

pro wizard_depth_update_table, pstate

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
       warning,'wizard_depth_update_table',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

	presults = (*pstate).presults
	if ptr_valid(presults) eq 0 then return
	n = n_elements( *presults)
	
	rows = string(indgen(n>1))
	columns = ['#','O file','I file','O Area','O Err','I Area','I Err','Ratio','eRatio','Depth','eDepth']
	nc = n_elements(columns)
	widths = [4, replicate(9,nc-1)] * !d.x_ch_size
	t = strarr(nc,15)
	
	for i=0,n-1 do begin
		t[0,i] = str_tidy(i)
		t[1,i] = ((*presults)[i]).oname
		t[2,i] = ((*presults)[i]).iname
		t[3,i] = str_tidy( ((*presults)[i]).oarea, places=1)
		t[4,i] = str_tidy( ((*presults)[i]).oerror, places=1)
		t[5,i] = str_tidy( ((*presults)[i]).iarea, places=1)
		t[6,i] = str_tidy( ((*presults)[i]).ierror, places=1)
		t[7,i] = str_tidy( ((*presults)[i]).ratio, places=-3)
		t[8,i] = str_tidy( ((*presults)[i]).eratio, places=-3)
		t[9,i] = str_tidy( ((*presults)[i]).depth, places=2)
		t[10,i] = str_tidy( ((*presults)[i]).edepth, places=-2)
	endfor	
	widget_control, (*pstate).results_table, set_value=t, column_widths=widths, align=2, $
				column_labels=columns, table_xsize=nc, table_ysize=n>1
	widget_control, (*pstate).results_table, use_table_select=[0,0,nc-1,(n-1)>0]
	(*pstate).columns = nc
	(*pstate).rows = n
	return
end

;----------------------------------------------------------------------

pro OnRealize_wizard_depth_instructions, wWidget

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
		warning,'OnRealize_wizard_depth_instructions',['IDL run-time error caught.', '', $
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
	wizard_depth_update_info, pstate
	return
end

;--------------------------------------------------------------------------

pro wizard_depth, debug=debug

; Wizard to determine the depth of particles that appear as hot-spots on images.
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
; other's 'top' setting it in the same source pointer struct.
; 
; Only the Wizard with the right "wizard name" will respond to the return event. It will
; execute the callback routine and then if 'pnext' is set, send a notify with 'pnext' as the
; pointer argument.		

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
common c_errors_1, catch_errors_on
if n_elements(debug) lt 1 then debug=0
catch_errors_on = 1                           ; enable error CATCHing
if debug then catch_errors_on = 0             ; disable error CATCHing

wversion = '8.9d'							; wizard version

; Each wizard sav loads routines from GeoPIXE.sav, if GeoPIXE is not already running.
; The GeoPIXE routines are NOT to be compiled into each wizard sav file.
;
; First set a catch to test whether "GeoPIXE.sav" can be found ...

Catch, ErrorNo							; GeoPIXE.sav only loaded if needed
if (ErrorNo ne 0) then begin
	Catch, /cancel
	
;................................................................................................
; This code fragmwent will appear at the start of most stand-alone programs
; that need to load "GeoPIXE.sav" as a library. It needs to work when the current
; working directory is: (i) the "geopixe" runtime dir, i.e. for SAV files stored in the
; runtime dir along side GeoPIXE.sav, (ii) a subdir of "geopixe" runtime, such as the
; "maia", "daq", "wizards", e.g. for compiled Maia or Wizard SAV files, and (iii) a 
; project dir during debugging of this program in IDLDE. 
;
; The latter assumes that the runtime dir is named "geopixe" (see notes in "startupp.pro").

	found = 0
	file = 'GeoPIXE.sav'						; current dir is the runtime dir
	if file_test(file) eq 0 then begin
		file = '../GeoPIXE.sav'					; current dir in a subdir of runtime dir
		if file_test(file) eq 0 then begin
			file = '../geopixe/GeoPIXE.sav'		; current dir is another project dir
			if file_test(file) then found=1
		endif else found=1
	endif else found = 1

	if found then begin
		restore, file
		print,'"GeoPIXE.sav" restored.'
		geopixe
	endif else begin
		a = dialog_message(['GeoPIXE is not loaded in memory.','No "GeoPIXE.sav" file found locally.','Abort Wizard.','', $
				'Check that your working directory is the "geopixe" or "main" dir.'], /error)
		return
	endelse
endif
test_geopixe_loaded						; tests whether GeoPIXE.sav routines loaded
Catch, /cancel							; this is a new feature of GeoPIXE v7.0e onwards

;................................................................................................

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
		warning,'wizard_depth',['IDL run-time error caught.', '', $
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

; List the names of the windows needed, in the format of their 'wizard-action' Notify
; window name. The "open-test" Notify message will be sent to these windows periodically
; to check on their 'open' status. They MUST make a copy of the Notify pointer contents,
; set (*pw).top of the copy to 'event.top' and return pw. 

windows_needed = ['Spectrum Display','Spectrum Fit','Image RGB','Image','Image Regions','Sort EVT']

case !version.os_family of
	'MacOS': begin
		space1 = 1
		space2 = 2
		space5 = 5
		space10 = 10
		space15 = 15
		left_xsize = 380
		left_ysize = 380
		right_xsize = 410
		right_ysize = left_ysize + 36
		right_ylines = 28
		text_xsize = 280
		button_xsize = 70
		button_xsize1 = 50
		button_xsize2 = 190
		help_xsize = left_xsize + right_xsize + 55
		end
	'unix': begin
		space1 = 1
		space2 = 2
		space5 = 5
		space10 = 10
		space15 = 15
		left_xsize = 380
		left_ysize = 400
		right_xsize = 410
		right_ysize = left_ysize + 36
		right_ylines = 28
		text_xsize = 280
		button_xsize = 70
		button_xsize1 = 50
		button_xsize2 = 190
		help_xsize = left_xsize + right_xsize + 55
		end
	else: begin
		space1 = 1
		space2 = 2
		space5 = 5
		space10 = 10
		space15 = 15
		left_xsize = 380
		left_ysize = 380
		right_xsize = 400
		right_ysize = left_ysize + 36
		right_ylines = 28
		text_xsize = 280
		button_xsize = 70
		button_xsize1 = 50
		button_xsize2 = 170
		help_xsize = left_xsize + right_xsize + 55
		end
endcase

tracking = 1				; enable tracking and context help for widgets
xoffset = 0					; and Help field to shows these help comments
yoffset = 0
		
; 	top-level base

tlb = widget_base( /column, title='Particle Depth Wizard ' + wversion + ' (GeoPIXE '+version+')', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='wizard-depth-tlb', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2 ,xoffset=xoffset, $
					yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

lbase = widget_base( tbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

tab_panel = widget_tab( lbase, location=2, /align_center, uname='wizard-depth-tab-panel')	;, $
tab_names = ['depth-curve','cal-spec','rgb-map','region-spectra','results']

; Depth curve -----------------------------------------

curve_base = widget_base( tab_panel, title='1. Depth Curve', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( curve_base, value='Calculate Curve for "Outer"/"Inner" versus Depth')
text = widget_text( curve_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='curve-explanation', tracking=tracking, $
				value=['Model X-ray yields for all elements to calculate the ratio of the selected "outer" and "inner" detectors versus depth. ' + $
				'Set-up this depth scale using the thickness series 1D mode for layer #1 in the Yield calculation.'], $
				uvalue='Explanation of the role of the Depth Curve panel.', frame=1)

curve_base1 = widget_base( curve_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( curve_base1, value='Yield Calculation')
curve_base1b = widget_base( curve_base1, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

curve_base1c = widget_base( curve_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( curve_base1c, value='Filters:', uname='depth-curve-filter-file-button', tracking=tracking, $
						uvalue='Click to browse to select the external filter file name. This is important as the absorption of the filter varies with angle to the detector in the array.', scr_xsize=button_xsize )
depth_curve_filter_text = widget_text( curve_base1c, uname='depth-curve-filter-text', value='', tracking=tracking, $
						uvalue='Enter file-name for the external filter, or click on button to browse for the file. This is important as the absorption of the filter varies with angle to the detector in the array.', scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_depth_curve_filter_text')

curve_base1d = widget_base( curve_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( curve_base1d, value='Yields:', uname='depth-curve-yield-file-button', tracking=tracking, $
						uvalue='Click to browse for the file-name of the yield calculation performed for a range of depth steps, ' + $
						'or the "New" button to perform a fresh yield calculation. See instructions in the right panel.', scr_xsize=button_xsize )
depth_curve_yield_text = widget_text( curve_base1d, uname='depth-curve-yield-text', value='', tracking=tracking, $
						uvalue='Enter file-name for the yield calculation performed for a range of depth steps, or click on the left button to browse for the file, ' + $
						'or the "New" button to perform a fresh yield calculation. See instructions in the right panel.', scr_xsize=text_xsize - button_xsize1 - 5, /edit)
;						Notify_Realize='OnRealize_depth_curve_yield_text')
button = widget_button( curve_base1d, value='New', uname='depth-curve-yield-new-button', tracking=tracking, $
						uvalue='Click to open the X-ray yield calculation to model yields as a function of depth. ' + $
						'This is done using a thickness 1D series for the first layer. The second layer is the particle.', scr_xsize=button_xsize1 )

curve_base1e = widget_base( curve_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( curve_base1e, value='"Outer":', uname='depth-curve-outer-file-button', tracking=tracking, $
						uvalue='Click to browse for the file name of the ".select.csv" file that selects the "Outer" detectors. Use the outer-most detectors and a few radial groups in. ' + $
						'Use the "radial" class selection on the "Array Detector Channel Selection" window (click "Set") to build these files.', scr_xsize=button_xsize )
depth_curve_outer_text = widget_text( curve_base1e, uname='depth-curve-outer-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of the ".select.csv" file that selects the "Outer" detectors. Use the outer-most detectors and a few radial groups in. ' + $
						'Use the "radial" class selection on the "Array Detector Channel Selection" window (click "Set") to build these files.', scr_xsize=text_xsize - button_xsize1 - 5, /edit)
;						Notify_Realize='OnRealize_depth_curve_outer_text')
button = widget_button( curve_base1e, value='Set', uname='depth-curve-outer-inner-set', tracking=tracking, $
						uvalue='Click to open the "Array Detector Channel Selection" window to construct "Outer" and "Inner" detector selections.', scr_xsize=button_xsize1 )

curve_base1f = widget_base( curve_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( curve_base1f, value='"Inner":', uname='depth-curve-inner-file-button', tracking=tracking, $
						uvalue='Click to browse for the file name of the ".select.csv" file that selects the "Inner" detectors. Use the inner-most detectors and a couple of radial groups out. ' + $
						'Use the "radial" class selection on the "Array Detector Channel Selection" window (click "Set") to build these files.', scr_xsize=button_xsize )
depth_curve_inner_text = widget_text( curve_base1f, uname='depth-curve-inner-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of the ".select.csv" file that selects the "Inner" detectors. Use the outer-most detectors and a few radial groups in. ' + $
						'Use the "radial" class selection on the "Array Detector Channel Selection" window (click "Set") to build these files.', scr_xsize=text_xsize - button_xsize1 - 5, /edit)
;						Notify_Realize='OnRealize_depth_curve_inner_text')
button = widget_button( curve_base1f, value='Set', uname='depth-curve-outer-inner-set', tracking=tracking, $
						uvalue='Click to open the "Array Detector Channel Selection" window to construct "Outer" and "Inner" detector selections.', scr_xsize=button_xsize1 )

curve_base1bb = widget_base( curve_base1b, /column, xpad=0, ypad=0, space=5, /align_right)
button = widget_button( curve_base1bb, value='Calculate Depth Curve', uname='depth-curve-calc-button', tracking=tracking, /align_center, $
						uvalue='Start the calculation of the depth curve. A file requester will prompt to select the output ".csv" file, ' + $
						'which defaults to the Depth Curve file below.', scr_xsize=button_xsize2 )

curve_base2 = widget_base( curve_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( curve_base2, value='Depth Curve')
curve_base2b = widget_base( curve_base2, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

curve_base2c = widget_base( curve_base2b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( curve_base2c, value='Curve:', uname='depth-curve-depth-file-button', tracking=tracking, $
						uvalue='Click to enter the file name of an existing depth curve ".csv" file, to use in subsequent wizard steps, or calculate a new one in the panel above.', scr_xsize=button_xsize )
depth_curve_depth_text = widget_text( curve_base2c, uname='depth-curve-depth-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of an existing depth curve output ".csv" file, to use in subsequent wizard steps, or calculate a new one in the panel above.', scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_depth_curve_depth_text')


; Cal Inner, outer spec files  -----------------------------------------

cal_base = widget_base( tab_panel, title='2. Cal Spec', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( cal_base, value='Form "Outer" and "Inner" Spec Cal Files')
text = widget_text( cal_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='cal-explanation', tracking=tracking, $
				value=['Take an energy Cal SPEC file and create separate SPEC files for the selected "Outer" and "Inner" detectors to be used in "Sort EVT" ' + $
				'in order to project DA images as seen by the "Outer" and "Inner" detectors as a basis to construct RGB images with Depth contrast.'], $
				uvalue='Explanation of the role of the Cal Spec panel.', frame=1)

cal_base1 = widget_base( cal_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( cal_base1, value='Cal SPEC and Select files')
cal_base1b = widget_base( cal_base1, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

cal_base1c = widget_base( cal_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( cal_base1c, value='Cal:', uname='cal-spec-cal-file-button', tracking=tracking, $
						uvalue='Click to browse for the file name of the SPEC file that contains the energy cal for all detectors. ', scr_xsize=button_xsize )
cal_spec_cal_file_text = widget_text( cal_base1c, uname='cal-spec-cal-file-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of the SPEC file that contains the energy cal for all detectors.', scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_cal_spec_cal_file_text')

cal_base1e = widget_base( cal_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( cal_base1e, value='"Outer":', uname='depth-curve-outer-file-button', tracking=tracking, $
						uvalue='Click to browse for the file name of the ".select.csv" file that selects the "Outer" detectors. Use the outer-most detectors and a few radial groups in. ' + $
						'Use the "radial" class selection on the "Array Detector Channel Selection" window (click "Set") to build these files.', scr_xsize=button_xsize )
cal_spec_outer_text = widget_text( cal_base1e, uname='depth-curve-outer-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of the ".select.csv" file that selects the "Outer" detectors. Use the outer-most detectors and a few radial groups in. ' + $
						'Use the "radial" class selection on the "Array Detector Channel Selection" window (click "Set") to build these files.', scr_xsize=text_xsize - button_xsize1 - 5, /edit)
;						Notify_Realize='OnRealize_cal_spec_outer_text')
button = widget_button( cal_base1e, value='Set', uname='depth-curve-outer-inner-set', tracking=tracking, $
						uvalue='Click to open the "Array Detector Channel Selection" window to construct "Outer" and "Inner" detector selections.', scr_xsize=button_xsize1 )

cal_base1f = widget_base( cal_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( cal_base1f, value='"Inner":', uname='depth-curve-inner-file-button', tracking=tracking, $
						uvalue='Click to browse for the file name of the ".select.csv" file that selects the "Inner" detectors. Use the inner-most detectors and a couple of radial groups out. ' + $
						'Use the "radial" class selection on the "Array Detector Channel Selection" window (click "Set") to build these files.', scr_xsize=button_xsize )
cal_spec_inner_text = widget_text( cal_base1f, uname='depth-curve-inner-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of the ".select.csv" file that selects the "Inner" detectors. Use the outer-most detectors and a few radial groups in. ' + $
						'Use the "radial" class selection on the "Array Detector Channel Selection" window (click "Set") to build these files.', scr_xsize=text_xsize - button_xsize1 - 5, /edit)
;						Notify_Realize='OnRealize_cal_spec_inner_text')
button = widget_button( cal_base1f, value='Set', uname='depth-curve-outer-inner-set', tracking=tracking, $
						uvalue='Click to open the "Array Detector Channel Selection" window to construct "Outer" and "Inner" detector selections.', scr_xsize=button_xsize1 )

cal_base1bb = widget_base( cal_base1b, /column, xpad=0, ypad=0, space=5, /align_right)
button = widget_button( cal_base1bb, value='Form Outer,Inner Cals', uname='cal-spec-calc-button', tracking=tracking, /align_center, $
						uvalue='Form separate Cal Spec files for the selected "Outer" and "Inner" detector channels. File names will be prompted for, using the filenames below as defaults.', scr_xsize=button_xsize2 )

cal_base2 = widget_base( cal_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( cal_base2, value='Output Cal Spec files')
cal_base2b = widget_base( cal_base2, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

cal_base2c = widget_base( cal_base2b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( cal_base2c, value='"Outer":', uname='cal-spec-output-outer-file-button', tracking=tracking, $
						uvalue='Click to enter the file name of an existing output Cal Spec file for "Outer" channels, to use in subsequent wizard steps, or calculate a new one in the panel above.', scr_xsize=button_xsize )
cal_spec_output_outer_file_text = widget_text( cal_base2c, uname='cal-spec-output-outer-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of an existing output Cal Spec file for "Outer" channels, to use in subsequent wizard steps, or calculate a new one in the panel above.', scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_cal_spec_output_outer_file_text')
		
cal_base2d = widget_base( cal_base2b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( cal_base2d, value='"Inner":', uname='cal-spec-output-inner-file-button', tracking=tracking, $
						uvalue='Click to enter the file name of an existing output Cal Spec file for "Inner" channels, to use in subsequent wizard steps, or calculate a new one in the panel above.', scr_xsize=button_xsize )
cal_spec_output_inner_file_text = widget_text( cal_base2d, uname='cal-spec-output-inner-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of an existing output Cal Spec file for "Inner" channels, to use in subsequent wizard steps, or calculate a new one in the panel above.', scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_cal_spec_output_inner_file_text')
		

; RGB maps  -----------------------------------------

rgb_base = widget_base( tab_panel, title='3. RGB', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( rgb_base, value='Construct RGB Images of Depth Contrast')
text = widget_text( rgb_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='rgb-explanation', tracking=tracking, $
				value=['Take separate Cal SPEC files for "Outer" and "Inner" detectors from previous page and use them in "Sort EVT" to makes images from the ' + $
				'outer and inner perspectives as a basis for forming RGB depth contrast images.'], $
				uvalue='Explanation of the role of the RGB map panel.', frame=1)

rgb_base1 = widget_base( rgb_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( rgb_base1, value='"Outer", "Inner" Perspective Images')
;rgb_base1b = widget_base( rgb_base1, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
rgb_base1b = widget_base( rgb_base1, /column, xpad=4, ypad=0, space=5, /base_align_right, /align_right)

rgb_base1bb = widget_base( rgb_base1b, /column, xpad=0, ypad=0, space=5, /align_right)
button = widget_button( rgb_base1bb, value='Sort "Outer", "Inner" Images', uname='rgb-map-sort-button', tracking=tracking, /align_center, $
						uvalue='Trigger "Sort EVT" window to sort out separate images for the selected "Outer" and "Inner" detector channels. File names will inherit the existing map filenames, with "-outer" or "-inner" appended.', scr_xsize=button_xsize2 )

rgb_base2 = widget_base( rgb_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( rgb_base2, value='Form RGB Images of Depth Contrast')
rgb_base2b = widget_base( rgb_base2, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

rgb_base2c = widget_base( rgb_base2b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( rgb_base2c, value='"Outer":', uname='rgb-map-outer-file-button', tracking=tracking, $
						uvalue='Click to enter the file name of an existing DAI image file for "Outer" channels, to use as "Green" in the RGB step below, or calculate a new one in the panel above.', scr_xsize=button_xsize )
rgb_map_outer_file_text = widget_text( rgb_base2c, uname='rgb-map-outer-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of an existing DAI image file for "Outer" channels, to use as "Green" in the RGB step below, or calculate a new one in the panel above.', scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_rgb_map_outer_file_text')
		
rgb_base2d = widget_base( rgb_base2b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( rgb_base2d, value='"Inner":', uname='rgb-map-inner-file-button', tracking=tracking, $
						uvalue='Click to enter the file name of an existing DAI image file for "Inner" channels, to use as "Red" in the RGB step below, or calculate a new one in the panel above.', scr_xsize=button_xsize )
rgb_map_inner_file_text = widget_text( rgb_base2d, uname='rgb-map-inner-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of an existing DAI image file for "Inner" channels, to use as "Red" in the RGB step below, or calculate a new one in the panel above.', scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_rgb_map_inner_file_text')

rgb_base2e = widget_base( rgb_base2b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_left)
label = widget_label( rgb_base2e, value='Element:', scr_xsize=button_xsize, /align_right)
rgb_map_element_text = widget_text( rgb_base2e, uname='rgb-map-element-text', value='', tracking=tracking, $
						uvalue='Enter the name of the element to display in the RGB depth maps. Remember the "L" or "M" suffix for elements using L or M X-rays.', scr_xsize=button_xsize, /edit)
;						Notify_Realize='OnRealize_rgb_map_element_text')

rgb_base2f = widget_base( rgb_base2b, /column, xpad=0, ypad=0, space=5, /align_right)
button = widget_button( rgb_base2f, value='Display RGB Depth Images', uname='rgb-map-rgb-display-button', tracking=tracking, /align_center, $
						uvalue='Trigger the "RGB Image" window to load the "Outer" and "Inner" perspective image files and display the selected element in RGB as a depth contrast.', scr_xsize=button_xsize2 )


; Extracted Inner, outer spec files  -----------------------------------------

spec_base = widget_base( tab_panel, title='4. Spectra', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( spec_base, value='Find Depths of Selected Particles')
text = widget_text( spec_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='spec-explanation', tracking=tracking, $
				value=['Extract spectra for selected Regions set on particle hot-spots and find the ratio of "Outer" to "Inner" signal, and relate this to particle depth using the "Depth Curve". ' + $
					'Use these steps repeatedly for each particle. Results appear on the next page.'], $
				uvalue='Explanation of the role of the Spectra panel.', frame=1)

spec_base1 = widget_base( spec_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( spec_base1, value='Extract Sum Spectra from Selected Region')
spec_base1b = widget_base( spec_base1, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

spec_base1e = widget_base( spec_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( spec_base1e, value='"Outer":', uname='depth-curve-outer-file-button', tracking=tracking, $
						uvalue='Click to browse for the file name of the ".select.csv" file that selects the "Outer" detectors. Use the outer-most detectors and a few radial groups in. ' + $
						'Use the "radial" class selection on the "Array Detector Channel Selection" window (click "Set") to build these files.', scr_xsize=button_xsize )
spectra_outer_text = widget_text( spec_base1e, uname='depth-curve-outer-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of the ".select.csv" file that selects the "Outer" detectors. Use the outer-most detectors and a few radial groups in. ' + $
						'Use the "radial" class selection on the "Array Detector Channel Selection" window (click "Set") to build these files.', scr_xsize=text_xsize - button_xsize1 - 5, /edit)
;						Notify_Realize='OnRealize_spectra_outer_text')
button = widget_button( spec_base1e, value='Set', uname='depth-curve-outer-inner-set', tracking=tracking, $
						uvalue='Click to open the "Array Detector Channel Selection" window to construct "Outer" and "Inner" detector selections.', scr_xsize=button_xsize1 )

spec_base1f = widget_base( spec_base1b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( spec_base1f, value='"Inner":', uname='depth-curve-inner-file-button', tracking=tracking, $
						uvalue='Click to browse for the file name of the ".select.csv" file that selects the "Inner" detectors. Use the inner-most detectors and a couple of radial groups out. ' + $
						'Use the "radial" class selection on the "Array Detector Channel Selection" window (click "Set") to build these files.', scr_xsize=button_xsize )
spectra_inner_text = widget_text( spec_base1f, uname='depth-curve-inner-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of the ".select.csv" file that selects the "Inner" detectors. Use the outer-most detectors and a few radial groups in. ' + $
						'Use the "radial" class selection on the "Array Detector Channel Selection" window (click "Set") to build these files.', scr_xsize=text_xsize - button_xsize1 - 5, /edit)
;						Notify_Realize='OnRealize_spectra_inner_text')
button = widget_button( spec_base1f, value='Set', uname='depth-curve-outer-inner-set', tracking=tracking, $
						uvalue='Click to open the "Array Detector Channel Selection" window to construct "Outer" and "Inner" detector selections.', scr_xsize=button_xsize1 )

spec_base1bb = widget_base( spec_base1b, /column, xpad=0, ypad=0, space=5, /align_right)
button = widget_button( spec_base1bb, value='Extract Spectra', uname='spectra-extract-button', tracking=tracking, /align_center, $
						uvalue='Extract detector spectra from selected Region row and form sum spectra for all selected "Outer" and "Inner" detector channels. File names will be prompted for, using the filenames below as defaults.', scr_xsize=button_xsize2 )

spec_base2 = widget_base( spec_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( spec_base2, value='Fit Extracted Region Sum Spectra')
spec_base2b = widget_base( spec_base2, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

spec_base2c = widget_base( spec_base2b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( spec_base2c, value='"Outer":', uname='spectra-output-outer-file-button', tracking=tracking, $
						uvalue='Click to enter the file name of an existing region Sum Spec file for "Outer" channels, to use in subsequent wizard steps, or calculate a new one in the panel above.', scr_xsize=button_xsize )
spectra_output_outer_file_text = widget_text( spec_base2c, uname='spectra-output-outer-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of an existing region Sum Spec file for "Outer" channels, to use in subsequent wizard steps, or calculate a new one in the panel above.', scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_spectra_output_outer_file_text')
		
spec_base2d = widget_base( spec_base2b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( spec_base2d, value='"Inner":', uname='spectra-output-inner-file-button', tracking=tracking, $
						uvalue='Click to enter the file name of an existing region Sum Spec file for "Inner" channels, to use in subsequent wizard steps, or calculate a new one in the panel above.', scr_xsize=button_xsize )
spectra_output_inner_file_text = widget_text( spec_base2d, uname='spectra-output-inner-text', value='', tracking=tracking, $
						uvalue='Enter the file-name of an existing region Sum Spec file for "Inner" channels, to use in subsequent wizard steps, or calculate a new one in the panel above.', scr_xsize=text_xsize, /edit)
;						Notify_Realize='OnRealize_spectra_output_inner_file_text')
		
spec_base2e = widget_base( spec_base2b, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_left)
label = widget_label( spec_base2e, value='Element:', scr_xsize=button_xsize, /align_right)
spectra_element_text = widget_text( spec_base2e, uname='rgb-map-element-text', value='', tracking=tracking, $
						uvalue='Enter the name of the element to extract signal for in fit to form "Outer"/"Inner" ratio and depth estimate.', scr_xsize=button_xsize, /edit)
;						Notify_Realize='OnRealize_spectra_element_text')

spec_base2bb = widget_base( spec_base2b, /column, xpad=0, ypad=0, space=5, /align_right)
button = widget_button( spec_base2bb, value='Fit Sum Spectra', uname='spectra-fit-button', tracking=tracking, /align_center, $
						uvalue='Fit the "Outer" and "Inner" sum spectra, evaluate the ratio and infer particle depth using the Depth Curve. Each result is added as a new row in the Results table on the next page.', scr_xsize=button_xsize2 )

		
; Results  -----------------------------------------

results_base = widget_base( tab_panel, title='5. Results', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( results_base, value='Results for Depths of Selected Particles')
text = widget_text( results_base, scr_xsize=left_xsize, ysize=4, /wrap, uname='results-explanation', tracking=tracking, $
				value=['Particle depth results: Element peak areas and errors determined from "Outer" and "Inner" sum spectra on the previous page.'], $
				uvalue='Explanation of the role of the Results panel.', frame=1)

results_base2 = widget_base( results_base, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

columns = ['#','O file','I file','O Area','O Err','I Area','I Err','Ratio','eRatio','Depth','eDepth']
nc = n_elements(columns)
widths = [3, replicate(7,nc-1)] * !d.x_ch_size
t = strarr(nc,15)

results_table = Widget_Table(results_base2, UNAME='results-table', /all_events, $	;, X_SCROLL_SIZE=8, Y_SCROLL_SIZE=6, $
				value=t, /RESIZEABLE_COLUMNS, alignment=2, scr_xsize=left_xsize, scr_ysize=left_ysize-130, /no_row_headers, $
				tracking=tracking, uvalue='The table shows the total peak area for all detectors in "Outer" ' + $
				'detectors as "O Area" and its simple statistical uncertainty as "O error". Similarly, "Inner" area is "I area". The ratio of "Outer/ ' + $
				'Inner" and its uncertainty is shown as "Ratio" and "eRatio". The ratio interpolated into the "Depth Curve" gives the estimate of "Depth" ' + $
				'and its uncertainty (statistical only) eDepth.', column_labels=columns, column_widths=widths )
;				NOTIFY_REALIZE='OnRealize_wizard_depth_results_table'

results_base3 = widget_base( results_base2, /row, xpad=0, ypad=0, space=5, /align_right)
button = widget_button( results_base3, value='Clear', uname='results-clear-button', tracking=tracking, $
						uvalue='Clear the results table.', scr_xsize=button_xsize )
button = widget_button( results_base3, value='Export Results', uname='results-export-button', tracking=tracking, $
						uvalue='Export the results table to a CSV file.', scr_xsize=button_xsize2 )
				
				
;------------------------------------------------------------------------------------------------

sbase = widget_base( lbase, /row, xpad=0, ypad=0, space=20, /base_align_center, /align_center)
button = widget_button( sbase, value='<<  Back', uname='back-button', tracking=tracking, uvalue='Go back a page in the procedure to the previous page. ' + $
			' You can also click on the Tab label for a previous page to go directly to it.')
button = widget_button( sbase, value=' Figure ', uname='figure-button', tracking=tracking, uvalue='Re-display the Figure for this tab.')
button = widget_button( sbase, value='Next  >>', uname='next-button', tracking=tracking, uvalue='Go to the next page in the procedure, if all entries have been made and prerequisite steps have been completed.')
		
;------------------------------------------------------------------------------------------------

rbase = widget_base( tbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

instructions_text = widget_list( rbase, scr_xsize=right_xsize, scr_ysize=right_ysize, uname='hermes-explanation', tracking=tracking, $
				value='', Notify_Realize='OnRealize_wizard_depth_instructions', $
				uvalue={xresize:1,yresize:1, help:'Explanation of the role of the Hermes panel.'});, frame=1)

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

		depth_curve_filter_file: '', $							; filter file name
		depth_curve_yield_file:	'', $							; yield file name
		depth_curve_outer_file:	'', $							; outer select file name
		depth_curve_inner_file:	'', $							; inner select file name
		depth_curve_depth_file:	'', $							; output depth curve file name			
		player:					ptr_new(/allocate_heap), $		; yield calc data area
		pselect:				ptr_new(/allocate_heap), $		; select array data area

		cal_spec_cal_file:	'', $								; cal spec for all channels file name
		cal_spec_output_outer_file:	'', $						; output cal spec for outer file name			
		cal_spec_output_inner_file:	'', $						; output cal spec for inner file name			

		rgb_map_outer_file:	'', $								; output DAI image for outer file name			
		rgb_map_inner_file:	'', $								; output DAI image for inner file name	
		rgb_map_element:	'', $								; RGB element	

		spectra_output_outer_file:	'', $						; output region sum spec for outer file name			
		spectra_output_inner_file:	'', $						; output region sum spec for inner file name			
		
		presults:				ptr_new(/allocate_heap), $		; room for results table
		columns:				0, $							; columns
		rows:					0, $							; rows
		
		left_xsize:				left_xsize, $					; left frame width
		left_ysize:				left_ysize, $					; left frame height
		
		depth_curve_filter_text: depth_curve_filter_text, $		; depth curve calc filter filename text ID
		depth_curve_yield_text:	depth_curve_yield_text, $		; depth curve calc yield filename text ID
		depth_curve_outer_text: depth_curve_outer_text, $		; depth curve calc outer filename text ID
		depth_curve_inner_text: depth_curve_inner_text, $		; depth curve calc inner filename text ID
		depth_curve_depth_text: depth_curve_depth_text, $		; depth curve calc depth filename text ID

		cal_spec_cal_file_text:	cal_spec_cal_file_text, $		; cal spec for all channels text ID
		cal_spec_outer_text:	cal_spec_outer_text, $			; outer filename text ID
		cal_spec_inner_text:	cal_spec_inner_text, $			; inner filename text ID
		cal_spec_output_outer_file_text: cal_spec_output_outer_file_text, $ ; output outer spec cal file text ID
		cal_spec_output_inner_file_text: cal_spec_output_inner_file_text, $ ; output inner spec cal file text ID
		
		rgb_map_outer_file_text: rgb_map_outer_file_text, $		; output outer image file text ID
		rgb_map_inner_file_text: rgb_map_inner_file_text, $ 	; output inner image file text ID
		rgb_map_element_text:	rgb_map_element_text, $			; element name text ID

		spectra_outer_text:		spectra_outer_text, $			; outer filename text ID
		spectra_inner_text:		spectra_inner_text, $			; inner filename text ID
		spectra_output_outer_file_text: spectra_output_outer_file_text, $ ; output outer spec cal file text ID
		spectra_output_inner_file_text: spectra_output_inner_file_text, $ ; output inner spec cal file text ID
		spectra_element_text:	spectra_element_text, $			; element name text ID

		results_table:			results_table, $				; results table ID
		
		instructions_text:		instructions_text, $			; instructions text ID
		help:					help $							; help text ID
		}

child = widget_info( tlb, /child)
pstate = ptr_new(state, /no_copy)
widget_control, child, set_uvalue=pstate
widget_control, tlb, /realize

geom = widget_info( tlb, /geometry)
(*pstate).tlb_xsize = geom.scr_xsize
(*pstate).tlb_ysize = geom.scr_ysize

register_notify, tlb, ['wizard-return']			; returns from GeoPIXE windows
xmanager, 'wizard_depth', tlb, /no_block

wizard_test_windows, 'depth', pstate			; check for open GeoPIXE windows and
widget_control, tlb, timer=8.0					; start timer to check periodically
return
end
				