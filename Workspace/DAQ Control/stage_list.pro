
;	Fit Results table.

pro stage_list_event, event

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
		warning,'stage_list_event',['IDL run-time error caught.', '', $
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

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

ps = (*pstate).ps
psm = (*pstate).psm
pm = (*pstate).pm

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'scan-list-update': begin
				update_stage_list_table, pstate
				if no_list eq 0 then begin
					np = n_elements(*p)
					(*pstate).sel.top = np-1
					view = widget_info( (*pstate).table, /table_view)
					widget_control, (*pstate).table, set_table_select=[-1,(*pstate).sel.top,-1,(*pstate).sel.top]
					widget_control, (*pstate).table, set_table_view=view

					*(*pstate).pselect = (*pstate).sel
					notify, 'scan-edit-select', (*pstate).pselect, from=event.top
				endif
				end
			'scan-list-select': begin
				(*pstate).sel.top = *event.pointer
				view = widget_info( (*pstate).table, /table_view)
				widget_control, (*pstate).table, set_table_select=[-1,(*pstate).sel.top,-1,(*pstate).sel.top]
				widget_control, (*pstate).table, set_table_view=view

				*(*pstate).pselect = (*pstate).sel
				notify, 'scan-edit-select', (*pstate).pselect, from=event.top
				end
			'master': begin
				if (*pstate).lock_master eq 0 then begin
					(*pstate).master = *event.pointer
					widget_control, (*pstate).master_check, set_value=(*pstate).master
				endif
				end
			'scan-sequence-next': begin
				stage_list_next, ps, psm, p, pm, pstate, event.top
				end
			'scan-command': begin
				case *event.pointer of
					'start': begin
						stage_list_start, ps, psm, p, pm, pstate, no_list, event.top
						end
					'stop': begin
						stage_list_stop, ps, psm, pstate, event.top
						end
					'pause': begin
						stage_list_pause, ps, psm, pm, no_list, pstate
						end
					'skip': begin
						(*pm).control.status.raster_status = daq_launch_raster_status( ps, pm)
						(*pstate).scan_sequence.raster.on = ((*pm).control.status.raster_status eq 1) or ((*pm).control.status.raster_status eq 2)
						if (*pstate).scan_sequence.active and (*pstate).scan_sequence.raster.on then begin
							stage_list_stop, ps, psm, pstate, event.top, /no_complete
							stage_list_next, ps, psm, p, pm, pstate, event.top
						endif
						end
					else:
				endcase
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
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request stage_list ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'scan-table': begin
		case tag_names( event, /structure_name) of

			'WIDGET_TABLE_CELL_SEL': begin
				if (event.sel_left eq -1) and (event.sel_right eq -1) then goto, finish
				(*pstate).sel.left = event.sel_left
				(*pstate).sel.top = event.sel_top
				(*pstate).sel.right = event.sel_right
				(*pstate).sel.bottom = event.sel_bottom
;				print, (*pstate).sel

				if no_list then goto, finish
				if (event.sel_left eq 0) and (event.sel_right eq 0) then begin
					if ptr_valid(p) eq 0 then goto, finish
					if size(*p,/tname) ne 'POINTER' then goto, finish
					n = n_elements(*p)
					for i=event.sel_top,event.sel_bottom do begin
						pi = (*p)[i]
						if ptr_valid( pi ) eq 0 then goto, finish
						if size(*pi,/tname) ne 'STRUCT' then goto, finish
						if (*pi).active le 3 then begin
							(*pi).active = ((*pi).active + 1) mod 4
						endif
					endfor
					update_stage_list_table, pstate
				endif
				
				if (event.sel_left eq 0) and (event.sel_right eq ((*pstate).columns-1)) then begin
					if (*pstate).sel.top ge 0 then begin
						*(*pstate).pselect = (*pstate).sel
						notify, 'scan-edit-select', (*pstate).pselect, from=event.top
					endif
				endif
				end

			'WIDGET_TABLE_CH': begin
				end
			else:
		endcase
		end

	'stage_list_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				x = (event.x - (*pstate).xoffset) > 620
				n = ((event.y - (*pstate).yoffset + 9)/ (*pstate).row_height) - 2
				y = (n + 2) * (*pstate).row_height
				widget_control, (*pstate).table, scr_xsize=x, scr_ysize=y
				widget_control, (*pstate).help, scr_xsize=x
				end
			else:
		endcase
		end

	'grab-button': begin
		if strlen( (*pm).scan.info) gt 0 then begin
			t1 = unstringify( (*pm).scan.info, error=err)
			if err eq 0 then begin
				pt1 = ptr_new( t1, /no_copy)
				t2 = define( /scan_spec)
				pt2 = ptr_new( t2, /no_copy)
				copy_pointer_data, pt1, pt2
				if no_list then begin
					*p = pt2
					no_list = 0
				endif else begin
					*p = [ *p, pt2]
				endelse
				update_stage_list_table, pstate
				ptr_free, pt1
			endif
		endif
		end

	'import-button': begin
		path = *(*pstate).path
;		file = strip_path(strip_file_ext((*(*p)[0]).spectrum.file)) + '.scan.csv'
		file = 'test.scan.csv'
		F = file_requester( /read, filter = '*.scan.csv', $
			path=path, file=file, group=event.top, $
			title='Read the scan list CSV file', /fix_filter)
		if F ne '' then begin
;			F = strip_file_ext(F,/double) + '.scan.csv'
			*(*pstate).path = extract_path(F)
			print,'save Scan List to ',F
			load_stage_list, pstate, F, data=p
			update_stage_list_table, pstate

			(*pstate).sel.top = 0
			(*pstate).sel.bottom = 0
			*(*pstate).pselect = (*pstate).sel
			widget_control, (*pstate).table, set_table_select=[-1,(*pstate).sel.top,-1,(*pstate).sel.top]
			notify, 'scan-edit-select', (*pstate).pselect, from=event.top
		endif
		end

	'save-button': begin
		if no_list then goto, finish
		path = *(*pstate).path
;		file = strip_path(strip_file_ext((*(*p)[0]).spectrum.file)) + '.scan.csv'
		file = 'test.scan.csv'
		F = file_requester( /write, filter = '*.scan.csv', $
			path=path, file=file, group=event.top, $
			title='Save the scan list to a CSV file', /fix_filter)
		if F ne '' then begin
;			F = strip_file_ext(F,/double) + '.scan.csv'
			*(*pstate).path = extract_path(F)
			print,'save Scan List to ',F
			save_stage_list, F, data=p, axes=(*pstate).axes, shapes=(*pstate).shapes, error=error
		endif
		end
		
	'edit-button': begin
		stage_edit, group=event.top, TLB=tlb, data=p, path=*(*pstate).path, pbeam=(*pstate).pbeam, $
					debug=(*pstate).debug, DAQ=(*pstate).pm, port=(*pstate).ps, row=(*pstate).sel.top
		register_notify, event.top, [ $			
					'scan-list-update', $		; update list display
					'scan-list-select' $		; select a scan list row
					], from=tlb
		end
		
	'fill-button': begin
		if event.select eq 0 then goto, finish
		if no_list then goto, finish
		n = n_elements(*p)
		nc = (*pstate).columns
		if n lt 2 then goto, finish
		i1 = clip( (*pstate).sel.top, 0, n-1)
		i2 = clip( (*pstate).sel.bottom, 0, n-1)
		if i2 eq i1 then goto, finish
		j1 = clip( (*pstate).sel.left, 0, nc-1)
		j2 = clip( (*pstate).sel.right, 0, nc-1)
		for i=i1+1,i2 do begin
			for j=j1,j2 do begin
				p0 = (*p)[i1]
				pi = (*p)[i]
				case j of
					0: (*pi).active = (*p0).active
					1: (*pi).sample = (*p0).sample
					2: (*pi).gain = (*p0).gain
					3: (*pi).origin.x = (*p0).origin.x
					4: (*pi).origin.y = (*p0).origin.y
					5: (*pi).origin.z = (*p0).origin.z
;					6: (*pi).raster.shape = (*p0).raster.shape

					6: (*pi).raster.axis.x = (*p0).raster.axis.x
					7: (*pi).raster.axis.y = (*p0).raster.axis.y
					8: (*pi).raster.step_scan = (*p0).raster.step_scan
					9: (*pi).raster.overscan = (*p0).raster.overscan
					10: (*pi).raster.interlace = (*p0).raster.interlace
					11: (*pi).raster.size.x = (*p0).raster.size.x
					12: (*pi).raster.size.y = (*p0).raster.size.y
					13: (*pi).raster.pixel.x = (*p0).raster.pixel.x
					14: (*pi).raster.pixel.y = (*p0).raster.pixel.y

					17: (*pi).raster.time.min = (*p0).raster.time.min
					18: (*pi).raster.time.max = (*p0).raster.time.max
					19: (*pi).raster.charge.min = (*p0).raster.charge.min
					20: (*pi).raster.charge.max = (*p0).raster.charge.max
					21: (*pi).raster.photon.min = (*p0).raster.photon.min
					22: (*pi).raster.photon.max = (*p0).raster.photon.max
					23: (*pi).comment = (*p0).comment
					else:
				endcase
			endfor
		endfor
		update_stage_list_table, pstate
		nc = (*pstate).columns
		j = ((*pstate).sel.top - 5) > 0
		widget_control, (*pstate).table, set_table_select=[j1,i1,j2,i2]
		widget_control, (*pstate).table, set_table_view=[0,j]
		end
		
	'arrow-up': begin
		if event.select eq 0 then goto, finish
		if no_list then goto, finish
		n = n_elements(*p)
		if n lt 2 then goto, finish
		i1 = clip( (*pstate).sel.top, 0, n-1)
		i2 = clip( (*pstate).sel.bottom, 0, n-1)
		if i1 eq 0 then goto, finish
		if i1 ge 2 then begin
			plist1 = (*p)[0:i1-2]
		endif else begin
			plist1 = 0L
		endelse
		plist2 = (*p)[i1:i2]
		if i2 lt n-1 then begin
			plist3 = [(*p)[i1-1],(*p)[i2+1:n-1]]
		endif else begin
			plist3 = (*p)[i1-1]
		endelse
		if ptr_valid(plist1[0]) then begin
			plist = plist1
		endif else plist = 0L
		if ptr_valid(plist[0]) then begin
			plist = [plist,plist2,plist3]
		endif else plist = [plist2,plist3]
		*p = plist
		n = n_elements(*p)
		(*pstate).sel.top = clip((*pstate).sel.top-1, 0, n-1)
		(*pstate).sel.bottom = clip((*pstate).sel.bottom-1, 0, n-1)
		update_stage_list_table, pstate
		nc = (*pstate).columns
		j = ((*pstate).sel.top - 5) > 0
		widget_control, (*pstate).table, set_table_select=[0,(*pstate).sel.top,nc-1,(*pstate).sel.bottom]
		widget_control, (*pstate).table, set_table_view=[0,j]
		end
		
	'arrow-down': begin
		if event.select eq 0 then goto, finish
		if no_list then goto, finish
		n = n_elements(*p)
		if n lt 2 then goto, finish
		i1 = clip( (*pstate).sel.top, 0, n-1)
		i2 = clip( (*pstate).sel.bottom, 0, n-1)
		if i2 ge n-1 then goto, finish
		if i1 ge 1 then begin
			plist1 = [(*p)[0:i1-1],(*p)[i2+1]]
		endif else begin
			plist1 = (*p)[i2+1]
		endelse
		plist2 = (*p)[i1:i2]
		if i2 lt n-2 then begin
			plist3 = (*p)[i2+2:n-1]
		endif else begin
			plist3 = 0L
		endelse
		if ptr_valid(plist3[0]) then begin
			plist = plist3
		endif else plist = 0L
		if ptr_valid(plist[0]) then begin
			plist = [plist1,plist2,plist]
		endif else plist = [plist1,plist2]
		*p = plist
		n = n_elements(*p)
		(*pstate).sel.top = clip((*pstate).sel.top+1, 0, n-1)
		(*pstate).sel.bottom = clip((*pstate).sel.bottom+1, 0, n-1)
		update_stage_list_table, pstate
		nc = (*pstate).columns
		j = ((*pstate).sel.top - 5) > 0
		widget_control, (*pstate).table, set_table_select=[0,(*pstate).sel.top,nc-1,(*pstate).sel.bottom]
		widget_control, (*pstate).table, set_table_view=[0,j]
		end
		
	'delete-button': begin
		if no_list then goto, finish
		i1 = (*pstate).sel.top
		i2 = (*pstate).sel.bottom
		n = n_elements(*p)
		if i1 ge 1 then begin
			plist1 = (*p)[0:i1-1]
		endif else begin
			plist1 = 0L
		endelse
		if i2 lt n-1 then begin
			plist2 = (*p)[i2+1:n-1]
		endif else begin
			plist2 = 0L
		endelse
		if ptr_valid(plist1[0]) then begin
			plist = plist1
			if ptr_valid(plist2[0]) then begin
				plist = [plist,plist2]
			endif		
		endif else begin
			if ptr_valid(plist2[0]) then begin
				plist = plist2
			endif else begin
				plist = [ptr_new()]
				no_list = 1
			endelse
		endelse
		*p = plist
		n = no_list ? 0 : n_elements(*p)
		(*pstate).sel.top = clip((*pstate).sel.top, 0, (n-1)>0)
		(*pstate).sel.bottom = clip((*pstate).sel.bottom, 0, (n-1)>0)
		update_stage_list_table, pstate
		
		*(*pstate).pselect = (*pstate).sel
		notify, 'scan-edit-select', (*pstate).pselect, from=event.top
		end
		
	'clear-button': begin
		n = n_elements(*p)
		for i=0,n-1 do ptr_free, (*p)[i]
		plist = ptr_new()
		*p = plist
		no_list = 1
		update_stage_list_table, pstate
		end
		
	'master-check': begin
		(*pstate).lock_master = 1						; lock it to stop notify changing it
		case event.value of
			0: begin
				(*pstate).master = event.select
				end
			else:
		endcase
		*(*pstate).pselect = (*pstate).master
		notify, 'master-set', (*pstate).pselect, from=event.top
		(*pstate).lock_master = 0
		end

	'start-button': begin
		stage_list_start, ps, psm, p, pm, pstate, no_list, tlb
		end
		
	'stop-button': begin
		stage_list_stop, ps, psm, pstate, event.top
		end
		
	'pause-resume-button': begin
		stage_list_pause, ps, psm, pm, no_list, pstate
		end
	
	'close-button': begin
		print,'Close scan list ...'
		goto, kill
		end
	else:
endcase

finish:
	widget_control, hourglass=0
	close, 1
	return

bad_state:
	warning,'stage_list_event',['STATE variable has become ill-defined.','Abort Fit Results.'],/error
	goto, kill
bad_ptr:
	warning,'stage_list_event',['Parameter structure variable has become ill-defined.','Abort Fit Results.'],/error
	goto, kill

kill:
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).pselect) then ptr_free, (*pstate).pselect
	if ptr_valid( (*pstate).headings) then ptr_free, (*pstate).headings

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;-----------------------------------------------------------------

pro stage_list_start, ps, psm, p, pm, pstate, no_list, tlb

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
		warning,'stage_list_start',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

	if (*pstate).master eq 0 then begin
		warning,'stage_list_start',['Scan list execution cannot be started when not in "master" mode.', $
				'Set "master" mode in "Scan List" window.']
		goto, finish
	endif
	if no_list then goto, finish
	n = n_elements(*p)
	active = intarr(n)
	for i=0,n-1 do active[i] = (*(*p)[i]).active
	q = where(active eq 2, nq)								; look for a 'start'	
	if nq ge 1 then ii = q[0]
	if nq eq 0 then begin
		q = where(active eq 0, nq)							; look for first 'on'	
		if nq ge 1 then ii = q[0]
	endif
	if nq eq 0 then begin
		warning,'stage_list_start',['No scans selected to run.','','Enable one or more scans,','and try again.','', $
			'Enable scans by clicking selected rows in the "Active" column.']
		goto, finish
	endif
	
	print,'scan_list: start sequence at item #',ii

	stage_list_do_scan, ps, psm, p, pm, ii, error=err	
	if err then goto, finish

	(*pstate).scan_sequence.active = 1
	(*pstate).scan_sequence.scan.index = ii
	(*pstate).scan_sequence.raster.on = 1
	
	notify,'scan-sequence-started', from=tlb

finish:
	update_stage_list_table, pstate
	return
end

;-----------------------------------------------------------------

pro stage_list_do_scan, ps, psm, p, pm, ii, error=err

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
		warning,'stage_list_do_scan',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

;	Move to origin first ...

	print,'stage_list_do_scan: move to origin via raster spec ...'
	build_raster_list4, pm, p, ii, ps, /set_origin, error=err		
	if err then return

;	Wait for 'raster' (i.e. moves to origin) to complete ...

	print,'stage_list_do_scan: wait on move to origin ...'
	stage_list_wait_on_raster, ps, error=err
	if err then begin
		warning,'stage_list_do_scan','Error waiting for raster origin position.'
		return
	endif

;	Set-up and start Maia acquisition ...

	wait, 2.0
	print,'stage_list_do_scan: set-up Maia ...'
	stage_list_maia_scan, pm, p, ii, psm, error=err
	if err then return

;	Build raster pattern and set-up and start DAQ acquisition, then launch raster ...

	wait, 2.0
	print,'stage_list_do_scan: set-up DAQ and start raster ...'
	build_raster_list4, pm, p, ii, ps, error=err		
	if err then begin
		warning,'stage_list_do_scan',['Error starting Raster scan.','Check that it is running.']
		return
	endif
	print,'stage_list_do_scan: raster underway.'
	return
end

;---------------------------------------------------------------------------------------------

pro stage_list_maia_scan, pm, plist, ii, psm, error=err

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
		warning,'stage_list_maia_scan',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

error = 1
if ptr_good(pm) eq 0 then begin
	warning,'stage_list_maia_scan','DAQ struct pointer is invalid.'
	return
endif
if (ii lt 0) or (ii ge n_elements( *plist)) then begin
	warning,'stage_list_maia_scan','Selected scan spec index is out of range.'
	return
endif
pj = (*plist)[ii]
if ptr_good(pj) eq 0 then begin
	warning,'stage_list_maia_scan','Selected scan spec pointer is invalid.'
	return
endif

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

	error = 1
	key = '?'		; for crossref - but DAQ blog run number is not known yet!

;	if (*pm).version.kandinski lt 7021 then begin
;		warning,'stage_list_maia_scan','Kandinski version too old.'
;		return
;	endif

	error = 0
	pitch = [(*pj).raster.pixel.x, (*pj).raster.pixel.y] 
;	hyst = ([(*pj).raster.pixel.x, (*pj).raster.pixel.y] * 0.1) > [0.001, 0.003]  
	hyst = [0.000, 0.000]  
	org = [(*pj).origin.x, (*pj).origin.y] 
	nx = round( float((*pj).raster.size.x) / float((*pj).raster.pixel.x)) > 1
	ny = round( float((*pj).raster.size.y) / float((*pj).raster.pixel.y)) > 1

;	Note the order here: scale, pitch, origin, position, origin
;	A 'wait' is needed after the move to origin (prior to this routine).
;	The extra 'origin' at the end then gives the correct pixel.coord immediately.

	socket_command_set, psm, 'scale', pitch, class='encoder.axis', chip=[0,1], error=err & error = error or err
	socket_command_set, psm, 'pitch', pitch, class='pixel.dim', chip=[0,1], error=err & error = error or err
	socket_command_set, psm, 'hysteresis', hyst, class='pixel.dim', chip=[0,1], error=err & error = error or err

	socket_command_set, psm, 'origin', org, class='pixel.dim', chip=[0,1], error=err & error = error or err
	socket_command_set, psm, 'position', org, class='encoder.axis', chip=[0,1], error=err & error = error or err
	socket_command_set, psm, 'origin', org, class='pixel.dim', chip=[0,1], error=err & error = error or err

	socket_command_set, psm, 'coord.extent', [nx,ny], class='pixel.dim', chip=-1, n_chip=2, error=err & error = error or err

	socket_command_set, psm, 'dwell', (*pj).raster.time.min, class='scan', error=err & error = error or err

	socket_command_set, psm, 'sample.name', '"'+(*pj).sample+'"', class='metadata', error=err & error = error or err
	socket_command_set, psm, 'scan.region', '"'+(*pj).grain+'"', class='metadata', error=err & error = error or err
;	socket_command_set, psm, 'scan.crossref', '"'+key+'"', class='metadata', error=err & error = error or err
;	socket_command_set, psm, 'crossref', '"'+key+'"', class='scan', error=err & error = error or err
	socket_command_set, psm, 'scan.info', '"'+(*pj).comment+'"', class='metadata', error=err & error = error or err

;	socket_command_set, psm, 'key', 'prefix', class='metadata.datum', chip=0, error=err & error = error or err
;	socket_command_set, psm, 'value', (*pstate).kvs_prefix, class='metadata.datum', chip=0, error=err & error = error or err
;	socket_command_set, psm, 'key', 'endstation', class='metadata.datum', chip=1, error=err & error = error or err
;	socket_command_set, psm, 'value', str_tidy((*pm).endstation), class='metadata.datum', chip=1, error=err & error = error or err

	socket_command_set, psm, 'enable', 1, class='event', error=err & error = error or err
	socket_command_set, psm, 'enable', 1, class='pixel', error=err & error = error or err
	socket_command_set, psm, 'enable', 1, class='photon', error=err & error = error or err
	if error then begin
		warning,'stage_list_maia_scan',['Error setting up Maia scan parameters.','Abort scan.']
		return
	endif

;	socket_command_set, psm, 'new', 1, class='scan', error=error
	socket_command_set, psm, 'newrun', 1, class='blog', error=error
	if error then begin
		warning,'stage_list_maia_scan','Error during Kandinski "newrun". Abort scan.'
		return
	endif

	error = 0
	return
end

;-----------------------------------------------------------------

pro stage_list_check_bounds, pm, p, pk, silent=silent, error=error

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
		warning,'stage_list_check_bounds',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif
if n_elements(silent) eq 0 then silent=0

error = 1
if ptr_good(pm) eq 0 then begin
	warning,'stage_list_check_bounds','DAQ struct pointer is invalid.'
	return
endif
if ptr_good(p) eq 0 then begin
	warning,'stage_list_check_bounds','Selected scan spec pointer is invalid.'
	return
endif

axes = ['DAC X','DAC Y','Stage X','Stage Y','Stage Z','Stage A']
shapes = ['Rectangular','Ellipse']

DAC_axis = [0,1]			; x,y
Stage_axis = [2,3,4,5]		; x,y,z,a

; Use an X-dominant (fastest) scan raster, unless Y is DAC and X is not ...

qdacx = where( (*p).raster.axis.x eq dac_axis, nqdacx)
qdacy = where( (*p).raster.axis.y eq dac_axis, nqdacy)
use_dacx = nqdacx gt 0
use_dacy = nqdacy gt 0
x_dominant = 1
if use_dacy and (NOT use_dacx) then x_dominant = 0
overscan = (*p).raster.overscan

xorg = (*p).origin.x									; origin for abs stage move
yorg = (*p).origin.y
zorg = (*p).origin.z
org = [xorg,yorg,zorg]

if x_dominant then begin
	af = (*p).raster.axis.x								; fast axis selection = axis.x
	as = (*p).raster.axis.y
	df = (nqdacx gt 0)									; indicates a DAC axis
	ds = (nqdacy gt 0)
	sf = (*p).raster.size.x								; scan size (mm)
	ss = (*p).raster.size.y
	pf = (*p).raster.pixel.x							; pixel size (mm)
	ps = (*p).raster.pixel.y
endif else begin
	af = (*p).raster.axis.y								; fast axis selection = axis.y
	as = (*p).raster.axis.x
	df = (nqdacy gt 0)									; indicates a DAC axis
	ds = (nqdacx gt 0)
	sf = (*p).raster.size.y								; scan size (mm)
	ss = (*p).raster.size.x
	pf = (*p).raster.pixel.y							; pixel size (mm)
	ps = (*p).raster.pixel.x
endelse

if (*pk).open then begin
	v = socket_command_get( pk, 'scale', class='deflect.axis', chip=-1, n_chip=2, error=error)
	if error then begin
		warning,'stage_list_check_bounds','Error reading "deflect axis scale" from Klee.'
		return
	endif
endif else begin
	v = [0.3,0.3]
	(*pm).stage.max.velocity[*] = 2.
endelse
(*pm).deflect.scale.x = v[0]
(*pm).deflect.scale.y = v[1]
deflect_max = [(*pm).deflect.max.x * v[0], (*pm).deflect.max.y * v[1] ]
pixel_min = [0.0003663 * [v[0],v[1]], replicate(0.001,4)]

if (*pk).open then begin
	v = socket_command_get( pk, 'position.range', class='stage.axis', chip=-1, n_chip=3, channel=-1, n_channels=2, error=error)
	if error then begin
		warning,'stage_list_check_bounds','Error reading "stage axis position range" from Klee.'
		return
	endif
endif else v=[0.,100.,-25.,25.,-12.,12.]
v = reform(v,2,n_elements(v)/2)
stage_min = reform(v[0,*])
stage_max = reform(v[1,*])

;	Do some bounds checks ...

if df then begin
	if sf gt 2.*deflect_max[af] then begin
		error = 1
		if silent eq 0 then warning,'stage_list_check_bounds',['Maximum DAC value exceeded for axis: '+axes[af], $
									'Maximum value = '+str_tidy(deflect_max[af])]
		return
	endif
endif
if ds then begin
	if ss gt 2.*deflect_max[as] then begin
		error = 1
		if silent eq 0 then warning,'stage_list_check_bounds',['Maximum DAC value exceeded for axis: '+axes[as], $
									'Maximum value = '+str_tidy(deflect_max[as])]
		return
	endif
endif
if pf lt pixel_min[af] then begin
	error = 1
	if silent eq 0 then warning,'stage_list_check_bounds',['Minimum pixel pitch violated for axis: '+axes[af], $
									'Minimum value = '+str_tidy(pixel_min[af])]
	return
endif
if ps lt pixel_min[as] then begin
	error = 1
	if silent eq 0 then warning,'stage_list_check_bounds',['Minimum pixel pitch violated for axis: '+axes[as], $
									'Minimum value = '+str_tidy(pixel_min[as])]
	return
endif
if (xorg lt stage_min[0]) or (xorg gt stage_max[0]) then begin
	error = 1
	if silent eq 0 then warning,'stage_list_check_bounds','Xorg stage position out of range'
	return
endif
if (yorg lt stage_min[1]) or (yorg gt stage_max[1]) then begin
	error = 1
	if silent eq 0 then warning,'stage_list_check_bounds','Yorg stage position out of range'
	return
endif
if (zorg lt stage_min[2]) or (zorg gt stage_max[2]) then begin
	error = 1
	if silent eq 0 then warning,'stage_list_check_bounds','Zorg stage position out of range'
	return
endif
if df eq 0 then begin
	if org[af-2]+sf gt stage_max[af-2] then begin
		error = 1
		if silent eq 0 then warning,'stage_list_check_bounds',['Maximum Stage value exceeded for axis: '+axes[af], $
									'Maximum value = '+str_tidy(stage_max[af-2])]
		return
	endif
endif
if ds eq 0 then begin
	if org[as-2]+ss gt stage_max[as-2] then begin
		error = 1
		if silent eq 0 then warning,'stage_list_check_bounds',['Maximum Stage value exceeded for axis: '+axes[as], $
									'Maximum value = '+str_tidy(stage_max[as-2])]
		return
	endif
endif

error = 0
return
end

;-----------------------------------------------------------------

pro stage_list_delete, p, top, bottom, nshow=ns

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
		warning,'stage_list_delete',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	np = n_elements(*p)
	if (top ge 0) and (bottom lt np) then begin
		for i=top,bottom do begin
			if ptr_valid( (*p)[i]) then ptr_free, (*p)[i]
		endfor
		ns = -1
		if top eq 0 then begin
			if bottom eq np-1 then begin
				*p = ptr_new()
			endif else begin
				*p = (*p)[bottom+1:np-1]
				ns = 0
			endelse
		endif else begin
			t = (*p)[0:top-1]
			if bottom lt np-1 then begin
				t = [t,(*p)[bottom+1:np-1]]
				ns = top
			endif else begin
				ns = top-1
			endelse
			*p = t
		endelse
	endif

	return
end

;-----------------------------------------------------------------

pro stage_list_stop, ps, psm, pstate, tlb, no_complete=no_complete

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
		warning,'stage_list_stop',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(no_complete) eq 0 then no_complete=0
;common c_stage_list_velocity, vel_stage_save
pm = (*pstate).pm

	socket_command_set, ps, 'move.type', 'stop', class='raster.step', chip=31
	socket_command_set, ps, 'move.beam.enable', 1, class='raster.step', chip=31
	socket_command_set, ps, 'dwell.beam.enable', 1, class='raster.step', chip=31
	socket_command_set, ps, 'start.step', 31, class='raster'
	if n_elements((*pm).stage.velocity) eq 4 then begin
		socket_command_set, ps, 'velocity', (*pm).stage.velocity, class='stage.axis', chip=-1, n_chip=4
	endif
	socket_command_set, ps, 'endrun', 1, class='blog'
	socket_command_set, psm, 'endrun', 1, class='blog'
	wait, 1.

	if no_complete eq 0 then begin
		(*pstate).scan_sequence.raster.on = 0
		(*pstate).scan_sequence.active = 0
		notify,'scan-sequence-complete', from=tlb
	endif

finish:
	update_stage_list_table, pstate
	return
end

;-----------------------------------------------------------------

pro stage_list_pause, ps, psm, pm, no_list, pstate

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
		warning,'stage_list_pause',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if (*pstate).master eq 0 then begin
		warning,'stage_list_pause',['Scan list can only be paused in "master" mode.', $
				'Set "master" mode in "Scan List" window.']
		goto, finish
	endif
	if no_list then goto, finish
	if (*pm).version.software gt 6260 then begin
		com = 'status'
		if (*pm).version.software gt 6880 then com = 'status.state'
		v = socket_command_get( ps, com, class='raster', /string, error=err)
		if err then begin
			warning,'stage_list','Failed to get "raster.status".'
			goto, finish
		endif
		if v[0] eq 'error' then begin
			warning,'stage_list','Klee "raster.status" returns "error".'
			goto, finish
		endif
		case v[0] of
			'stop': k = -1
			'run_move': k = 0		; running, so pause
			'run_dwell': k = 0		; running, so pause
			'run_pause': k = 1		; paused, so resume
		endcase
		if k ge 0 then begin
			socket_command_set, ps, 'enable', k, class='raster'
			socket_command_set, ps, 'enable', k, class='photon'
			socket_command_set, psm, 'enable', k, class='photon'
		endif else begin
			socket_command_set, ps, 'enable', 1, class='photon'
			socket_command_set, psm, 'enable', 1, class='photon'
		endelse
	endif

finish:
	return
end

;-----------------------------------------------------------------

pro stage_list_next, ps, psm, p, pm, pstate, tlb

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
		warning,'stage_list_next',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
;common c_stage_list_velocity, vel_stage_save

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

	if (*pstate).master eq 0 then begin
		warning,'stage_list_next',['Scan list sequence can only be advanced in "master" mode.', $
				'Set "master" mode in "Scan List" window.']
		goto, finish
	endif
	n = n_elements(*p)
	if (*pstate).scan_sequence.active then begin
		(*(*p)[ (*pstate).scan_sequence.scan.index ]).active = 1							; set OFF
	endif

;	Complete previous scan with resulting DAQ and Maia 'blog' run numbers ...
;	(Get these fresh, in case copies from activity processes are not up to date)

	v  = socket_command_get( ps, 'runno', class='blog', error=err)							; DAQ run number
	if (err eq 0) then begin
		(*pm).run.number = v[0]

		if ptr_good( plist) then begin
			(*(*p)[ (*pstate).scan_sequence.scan.index ]).blog = (*pm).run.number			
		endif
	endif
	v  = socket_command_get( psm, 'runno', class='status.blog', error=err)							; Maia run number
	if (err eq 0) then begin
;		(*pm).run.number = v[0]

		if ptr_good( plist) then begin
			(*(*p)[ (*pstate).scan_sequence.scan.index ]).crossref = str_tidy(v[0])			
		endif
	endif

	veto_more = 0
	if (*(*p)[(*pstate).scan_sequence.scan.index]).active eq 3 then veto_more=1				; was last scan a "STOP" scan?

	ii = (*pstate).scan_sequence.scan.index + 1
	if (ii le (n-1)) then begin
		if (veto_more eq 0) then begin
			while (((*(*p)[ii]).active eq 1) or ((*(*p)[ii]).active ge 4)) and (ii lt (n-1)) do ii++				; skip 'off', 'ref1', ref2'

			if ((*(*p)[ii]).active eq 0) or ((*(*p)[ii]).active eq 2) or ((*(*p)[ii]).active eq 3) then begin  		; 'on', 'start' or 'stop'
				print,'scan_list: continue sequence at item #',ii

				stage_list_do_scan, ps, psm, p, pm, ii, error=err	

				if err eq 0 then begin
					(*pstate).scan_sequence.active = 1
					(*pstate).scan_sequence.scan.index = ii
					(*pstate).scan_sequence.raster.on = 1
					
					notify,'scan-sequence-started', from=tlb
					goto, finish
				endif
			endif
		endif
	endif
	print,'scan_list: sequence finished.'
	(*pstate).scan_sequence.active = 0
	(*pstate).scan_sequence.raster.on = 0

	notify,'scan-sequence-complete', from=tlb
	if n_elements((*pm).stage.velocity) eq 4 then begin
		socket_command_set, ps, 'velocity', (*pm).stage.velocity, class='stage.axis', chip=-1, n_chip=4
	endif
	socket_command_set, ps, 'endrun', 1, class='blog', error=err
	socket_command_set, psm, 'endrun', 1, class='blog'

finish:
	update_stage_list_table, pstate
	return
end

;-----------------------------------------------------------------

pro stage_list_wait_on_move, pk, axes=axes, error=err

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
		warning,'stage_list_wait_on_move',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(axes) lt 1 then axes = [0,1]
	wait, 2.0

	repeat begin
		wait, 0.5
		v  = socket_command_get( pk, 'onpos', class='stage.axis', chip=axes, error=err)	
		if (err eq 0) then begin
;			print,'	onpos = ',v
			done = 0
			if total(v) eq n_elements(v) then done=1
		endif else begin
			wait, 5.0
			done = 1
		endelse
	endrep until done
	return
end

;-----------------------------------------------------------------

pro stage_list_wait_on_raster, pk, error=err

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
		warning,'stage_list_wait_on_raster',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	print,'stage_list_wait_on_raster: test raster.status for "stop" after it starts to move to origin ...'
	for i=0,5 do begin
		v  = socket_command_get( pk, 'state', /string, class='raster.status', error=err)	
		if (err eq 0) then begin
			stat = string(v[0])
		endif else stat='?'
		v  = socket_command_get( pk, 'position', /float, class='position.dim', chip=-1, n_chips=6, error=err)	
		if (err eq 0) then begin
			print,'	initial position = '+strjoin(str_tidy(v),', ')+' ('+stat+')'
		endif
		wait, 1.0
	endfor

	loop = 0
	repeat begin
		wait, 1.0
		v  = socket_command_get( pk, 'state', /string, class='raster.status', error=err)	
		if (err eq 0) then begin
			stat = string(v[0])
			done = 0
			if string(v) eq 'stop' then done=1
		endif else begin
			print,'	raster.status returns an error.'
			stat = '?'
			wait, 5.0
			done = 1			; But 'err' flag is set.
		endelse
		v  = socket_command_get( pk, 'position', /float, class='position.dim', chip=-1, n_chips=6, error=err2)	
		if (err2 eq 0) then begin
			print,'	position = '+strjoin(str_tidy(v),', ')+' ('+stat+')'
		endif
		if loop++ gt 300 then begin
			err = 1
			print,'stage_list_wait_on_raster: failed to "stop" after 300s.'
			done = 1
		endif
	endrep until done
	print,'stage_list_wait_on_raster: done.'
	return
end

;-----------------------------------------------------------------

function stage_list_time_string, time_toti

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
		warning,'stage_list_time_string',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0.
	endif
endif
	time_tot = time_toti

	times = ['s','m','h','d','m','y']
	rtimes = [60.,60.,24.,30.,12.]
	it = 0
	while (time_tot gt 100.) and (it lt 5) do begin
		time_tot = time_tot/rtimes[it]
		it += 1
	endwhile
	time_label = times[it]
	stime = str_tidy( time_tot, places=-1) + ' ' + time_label
	return, stime
end

;-----------------------------------------------------------------

function stage_list_time, p, beam, suffix=suffix, time_only=tonly, charge_only=qonly

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
		warning,'stage_list_time',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0.
	endif
endif
if n_elements(qonly) eq 0 then qonly = 0
if n_elements(tonly) eq 0 then tonly = 0
if n_elements(beam) eq 0 then beam = 0.0

	suffix = ''
	nx = round( float((*p).raster.size.x) / float((*p).raster.pixel.x)) > 1
	ny = round( float((*p).raster.size.y) / float((*p).raster.pixel.y)) > 1
	
;	Simple options:
;		1. just set charge.max - then stop at this charge. Dwell estimate needs charge estimate.
;		2. Just set time.max - then stop at this dwell.
;		
;	Viable options:
;		1. charge.min and time.min must be met, then either of charge.max or time.max can terminate a pixel
;			could just set charge.max, time.max, where time.max was based on a lower current?

	tqmin = (beam lt 1.0e-6) ? 0.0 : ((*p).raster.charge.min / (beam * 1000.))
	tqmax = (beam lt 1.0e-6) ? 1.0e+10 : ((*p).raster.charge.max / (beam * 1000.))

	ttmin = (*p).raster.time.min
	ttmax = ((*p).raster.time.max lt 1.0e-6) ? 1.0e+10 : (*p).raster.time.max

	if tonly then begin
		by = ['(T)']
		tmin = [ttmin]
		tmax = [ttmax]
	endif else if qonly then begin
		by = ['(Q)']
		tmin = [tqmin]		
		tmax = [tqmax]
	endif else begin
		by = ['(T)', '(Q)']
		tmin = [ttmin, tqmin]
		tmax = [ttmax, tqmax]
	endelse

	qmin = reverse(sort(tmin))
	tmin = tmin[qmin[0]]					; max of the mins
	by_min = by[qmin[0]]

	qmax = sort(tmax)
	tmax = tmax[qmax[0]]					; min of the maxs
	by_max = by[qmax[0]]

	if tmax gt 1.0e+6 then begin			; ignore huge (1.0e+10) maxs
		t = tmin
		suffix = by_min
	endif else begin
		t = [tmin,tmax]
		qt = reverse(sort(t))
		byt = [by_min, by_max]
		t = t[qt[0]]
		suffix = byt[qt[0]]
	endelse

	time_tot = float(nx)*float(ny)* t
	if ((*p).raster.shape eq 1) then time_tot = time_tot * !pi/4.
	time_tot = time_tot * ((*p).raster.overscan > 1)

	return, time_tot
end

;-----------------------------------------------------------------

pro load_stage_list, pstate, file, data=plist

; Read the scan list from 'File'

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
		warning,'load_stage_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		close, 1
		return
	endif
endif

	if lenchr(file) lt 1 then return
	if ptr_valid(plist) eq 0 then goto, bad_ptr
	no_list = 0
	if size(*plist,/tname) ne 'POINTER' then begin
		no_list = 1
	endif else begin
		if ptr_valid( (*plist)[0] ) eq 0 then no_list=1
	endelse

	valid_versions = [-1]
	
	on_ioerror, bad_file
	openr,unit, file, /get_lun
	on_ioerror, bad_io

	s = ''
	readf, unit, s
	str = strsplit( s, ', ', /extract)
	ns = n_elements(str)
	if ns lt 2 then goto, bad_version
	version = fix(str[1])
	q = where( version eq valid_versions, nq)
	if nq lt 1 then goto, bad_version
	i = 0
	
	while( not( EOF(unit)) ) do begin
		readf, unit, s
		if strmid(s,0,1) eq '#' then continue
		str = strsplit( s, ', ', /extract)
		ns = n_elements(str)
		if ns ge 2 then begin
			srest = strjoin( str[1:*], ' ')
			
			case strlowcase(str[0]) of
				'scan-number:': begin
					p = ptr_new(define(/scan_spec))
					if n_elements(pt) ge 1 then begin
						pt = [pt,p]
					endif else begin
						pt = p
					endelse
					end
				'sample:': begin
					(*p).sample = srest
					end
				'grain:': begin
					(*p).grain = srest
					end
				'comment:': begin
					(*p).comment = srest
					end
				'origin:': begin
					if ns ge 4 then begin
						(*p).origin.x = float2(str[1])
						(*p).origin.y = float2(str[2])
						(*p).origin.z = float2(str[3])
					endif
					end
				'axis:': begin
					if ns ge 3 then begin
						(*p).raster.axis.x = fix2(str[1]) < (n_elements((*pstate).axes)-1)
						(*p).raster.axis.y = fix2(str[2]) < (n_elements((*pstate).axes)-1)
					endif
					end
				'shape:': begin
					(*p).raster.shape = fix2(str[1]) < (n_elements((*pstate).SHAPES)-1)
					end
				'step-scan:': begin
					(*p).raster.step_scan = fix2(str[1]) < 1
					end
				'overscan:': begin
					(*p).raster.overscan = fix2(str[1]) > 1
					end
				'interlace:': begin
					(*p).raster.interlace = fix2(str[1]) < 1
					end
				'size:': begin
					if ns ge 3 then begin
						(*p).raster.size.x = float2(str[1])
						(*p).raster.size.y = float2(str[2])
					endif
					end
				'pixel:': begin
					if ns ge 3 then begin
						(*p).raster.pixel.x = float2(str[1])
						(*p).raster.pixel.y = float2(str[2])
					endif
					end
				'charge-min/max:': begin
					if ns ge 3 then begin
						(*p).raster.charge.min = float2(str[1])
						(*p).raster.charge.max = float2(str[2])
					endif
					end
				'photons-min/max:': begin
					if ns ge 3 then begin
						(*p).raster.photons.min = float2(str[1])
						(*p).raster.photons.max = float2(str[2])
					endif
					end
				'time-min/max:': begin
					if ns ge 3 then begin
						(*p).raster.time.min = float2(str[1])
						(*p).raster.time.max = float2(str[2])
					endif
					end
				else:
			endcase
		endif
	endwhile
	
	*plist = no_list ? pt : [*plist,pt]
	close_file, unit
	return

bad_file:
	warning,'load_stage_list',['Error opening scan list file: ',file],/error
	return
bad_io:
	warning,'load_stage_list','Error reading scan list file.',/error
	return
bad_ptr:
	warning,'load_stage_list','Bad initial results pointer',/error
	return
bad_version:
	warning, 'load_stage_list', 'Bad file version',/error
	return
end

;-----------------------------------------------------------------

pro save_stage_list, file, data=plist, axes=axes, shapes=shapes, error=error

; Write the scan list *p to file 'F'

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
		warning,'save_stage_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		close, 1
		return
	endif
endif

	if lenchr(file) lt 1 then return
	if n_elements(axes) lt 1 then return
	if n_elements(shapes) lt 1 then return
	if ptr_valid(plist) eq 0 then goto, bad_ptr
	no_list = 0
	if size(*plist,/tname) ne 'POINTER' then begin
		no_list = 1
	endif else begin
		if ptr_valid( (*plist)[0] ) eq 0 then no_list=1
;		if no_list eq 0 then if size(*(*plist)[0],/tname) ne 'STRUCT' then no_list=1
	endelse
	if no_list then return
	
	n = 0
	for i=0L,n_elements(*plist)-1 do begin
		p = (*plist)[i]
		if ptr_good(p) then n = n+1
	endfor
	if n eq 0 then return

	version = -1

	saxes = '# Axis modes:, 0:' + axes[0]
	for i=1,n_elements(axes)-1 do saxes = saxes + ', '+str_tidy(i)+':' + axes[i]
	sshapes = '# Shape modes:, 0:' + shapes[0]
	for i=1,n_elements(shapes)-1 do sshapes = sshapes + ', '+str_tidy(i)+':' + shapes[i]
	
	on_ioerror, bad_file
	openw,unit, file, /get_lun
	on_ioerror, bad_io

	printf, unit, 'Version:, ' + str_tidy(version)
	printf, unit, saxes
	printf, unit, sshapes	
;	printf, unit, 'Number:, ' + str_tidy(n)

	for i=0L,n_elements(*plist)-1 do begin
		p = (*plist)[i]
		if ptr_good(p) eq 0 then continue
		
		printf, unit, '# ------------------------------------------------------------------------------'
		printf, unit, 'Scan-Number:, ' + str_tidy(i)
		printf, unit, 'Sample:, ' + (*p).sample
		printf, unit, 'Grain:, ' + (*p).grain
		printf, unit, 'Comment:, ' + (*p).comment
		
		printf, unit, 'Origin:, ' + str_tidy((*p).origin.x) + ', ' + str_tidy((*p).origin.y)  + ', ' + str_tidy((*p).origin.z) 
		printf, unit, 'Axis:, ' + str_tidy((*p).raster.axis.x) + ', ' + str_tidy((*p).raster.axis.y) 
		printf, unit, 'Shape:, ' + str_tidy((*p).raster.shape) 
		printf, unit, 'Step-scan:, ' + str_tidy((*p).raster.step_scan) 
		printf, unit, 'Overscan:, ' + str_tidy((*p).raster.overscan) 
		printf, unit, 'Interlace:, ' + str_tidy((*p).raster.interlace) 
		printf, unit, 'Size:, ' + str_tidy((*p).raster.size.x) + ', ' + str_tidy((*p).raster.size.y) 
		printf, unit, 'Pixel:, ' + str_tidy((*p).raster.pixel.x) + ', ' + str_tidy((*p).raster.pixel.y) 

		printf, unit, 'Charge-Min/Max:, ' + str_tidy((*p).raster.charge.min) + ', ' + str_tidy((*p).raster.charge.max) 
		printf, unit, 'Time-Min/Max:, ' + str_tidy((*p).raster.time.min) + ', ' + str_tidy((*p).raster.time.max) 
		printf, unit, 'Photons-Min/Max:, ' + str_tidy((*p).raster.photons.min) + ', ' + str_tidy((*p).raster.photons.max) 
	endfor

finish:
	close_file, unit
	return

bad_io:
	warning,'save_stage_list','Error writing results file',/error
	goto, finish
bad_ptr:
	warning,'save_stage_list','bad results pointer',/error
	goto, finish
bad_file:
	warning,'save_stage_list',['error opening file: ',file],/error
	goto, finish
end

;-----------------------------------------------------------------

pro OnRealize_stage_list_Table, wWidget

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

w = widget_info( wWidget, /row_heights)
geom = widget_info( wWidget, /geometry)
tlb_geom = widget_info( top, /geometry)

(*pstate).table = wWidget
(*pstate).row_height = w[0]
(*pstate).rows = 6
(*pstate).yoffset = tlb_geom.ysize - geom.scr_ysize + 2
(*pstate).xoffset = tlb_geom.xsize - geom.scr_xsize

update_stage_list_table, pstate

done:
end

;------------------------------------------------------------------------------------------

pro update_stage_list_table, pstate, nsi

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
		warning,'update_stage_list_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(nsi) lt 1 then nsi=(*pstate).sel.top
ns = nsi
if n_elements(ns) lt 1 then ns=(*pstate).sel.top

; NOTE: Also see 'fill-button' code for column order ...

columns = ['Active','Sample','Grain','X origin','Y origin','Z origin', $
			'X axis', 'Y axis','Fly','Overscan','Interlace','X size','Y size','X pixel','Y pixel', $
			'T (T only)','T (Q only)', 'T min','T max','Q min','Q max','P min','P max','Comment', 'blog', 'Crossref']
nc = n_elements(columns)
widths = [6,8,5,replicate(7,3),replicate(7,2),4,8,7,replicate(7,4),replicate(11,2),replicate(8,6),30,8,8] * !d.x_ch_size

plist = (*pstate).plist
if ptr_valid(plist) eq 0 then goto, bad
if size(*plist,/tname) ne 'POINTER' then goto, bad
if ptr_valid( (*plist)[0] ) eq 0 then goto, bad
if size(*(*plist)[0],/tname) ne 'STRUCT' then goto, bad
ps = (*pstate).ps
pm = (*pstate).pm

n = n_elements(*plist)
if n eq 0 then goto, bad
ns = ns < (n-1)

istart = intarr(n)
for i=0,n-1 do begin
	pi = (*plist)[i]
	if (*pi).active eq 2 then istart[i] = 1				; START - there can be only one
endfor
qstart = where( istart eq 1, nqstart)
if nqstart gt 1 then begin
	for j=1,nqstart-1 do begin
		i = qstart[j]
		(*(*plist)[i]).active = 3						; START - turn extra "START" to "STOP"
	endfor
endif
istop = intarr(n)
for i=0,n-1 do begin
	pi = (*plist)[i]
	if (*pi).active eq 3 then istop[i] = 1				; STOP - there can be only one
endfor
qstop = where( istop eq 1, nqstop)
q = where( (qstop lt qstart[0]) and (nqstop gt 0), nq)
if nq gt 0 then begin
	for j=0,nq-1 do begin
		i = qstop[q[j]]
		(*(*plist)[i]).active = 0						; any "STOP" before first "START" cycle to "ON"
	endfor
endif
if nqstop gt 1 then begin
	for j=1,nqstop-1 do begin
		i = qstop[j]
		(*(*plist)[i]).active = 0						; STOP - turn extra "STOP" to "ON"
	endfor
endif

t = strarr(nc,n)
active = (*pstate).active
axes = (*pstate).axes
shapes = (*pstate).shapes
yes_no = ['no','yes']
c = intarr(3,nc,n)
c[0,*,*] = (spec_colour('white',/rgb))[0]
c[1,*,*] = (spec_colour('white',/rgb))[1]
c[2,*,*] = (spec_colour('white',/rgb))[2]
tt = 0.0
no_stop = 1

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

for i=0L,n-1 do begin
	k = 0
	p = (*plist)[i]
	if ptr_good(p) eq 0 then continue

	if (*pstate).scan_sequence.active and (*pstate).scan_sequence.raster.on and ((*pstate).scan_sequence.scan.index eq i) then begin
		for j=0,nc-1 do c[*,j,i] = spec_colour('green',/rgb)
	endif
	stage_list_check_bounds, pm, p, ps, /silent, error=error
	if error then begin
		for j=0,nc-1 do c[*,j,i] = spec_colour('yellow',/rgb)
	endif
	ttonly = stage_list_time( p, *(*pstate).pbeam, suffix=ttsuffix, /time_only)
	tqonly = stage_list_time( p, *(*pstate).pbeam, suffix=tqsuffix, /charge_only)
	sttime = stage_list_time_string( ttonly)
	sqtime = stage_list_time_string( tqonly)
	if no_stop and ((active[ (*p).active] eq 'START') or (active[ (*p).active] eq 'STOP') or (active[ (*p).active] eq 'ON')) then begin
		tt = tt + stage_list_time( p, *(*pstate).pbeam, suffix=tsuffix)
	endif
	if active[ (*p).active] eq 'STOP' then no_stop = 0

	t[k,i] = active[ (*p).active]  &  k++
	t[k,i] = (*p).sample  &  k++
	t[k,i] = (*p).grain  &  k++
	t[k,i] = str_tidy( (*p).origin.x, places=3,length=6)  &  k++
	t[k,i] = str_tidy( (*p).origin.y, places=3,length=6)  &  k++
	t[k,i] = str_tidy( (*p).origin.z, places=3,length=6)  &  k++
;	t[k,i] = shapes[(*p).raster.shape]  &  k++
	t[k,i] = axes[ (*p).raster.axis.x]  &  k++
	t[k,i] = axes[ (*p).raster.axis.y]  &  k++
	t[k,i] = yes_no[1-(*p).raster.step_scan]  &  k++
	t[k,i] = str_tidy((*p).raster.overscan > 1)  &  k++
	t[k,i] = yes_no[(*p).raster.interlace]  &  k++
	t[k,i] = str_tidy( (*p).raster.size.x, places=3,length=6)  &  k++
	t[k,i] = str_tidy( (*p).raster.size.y, places=3,length=6)  &  k++
	t[k,i] = str_tidy( (*p).raster.pixel.x, places=4,length=6)  &  k++
	t[k,i] = str_tidy( (*p).raster.pixel.y, places=4,length=6)  &  k++
	t[k,i] = sttime  &  k++
	t[k,i] = sqtime  &  k++
	t[k,i] = str_tidy( (*p).raster.time.min, places=-4)  &  k++
	t[k,i] = str_tidy( (*p).raster.time.max, places=-4)  &  k++
	t[k,i] = str_tidy( (*p).raster.charge.min, places=-3)  &  k++
	t[k,i] = str_tidy( (*p).raster.charge.max, places=-3)  &  k++
	t[k,i] = str_tidy( (*p).raster.photons.min, places=0)  &  k++
	t[k,i] = str_tidy( (*p).raster.photons.max, places=0)  &  k++
	t[k,i] = (*p).comment  &  k++
	t[k,i] = str_tidy((*p).blog)  &  k++
	t[k,i] = (*p).crossref  &  k++
endfor

stt = stage_list_time_string( tt)
widget_control, (*pstate).time_text, set_value = stt

if ptr_valid( (*pstate).headings) then ptr_free, (*pstate).headings
(*pstate).headings = ptr_new(columns)
(*pstate).columns = nc
(*pstate).rows = n
rows = string( indgen(n))

widget_control, (*pstate).table, set_value = t, column_widths=widths, $
			row_labels = rows, column_labels=columns, $
			table_xsize=(*pstate).columns, table_ysize=n, align=2
;			use_table_select=[0,0,nc-1,n-1]
widget_control, (*pstate).table, background_color=c
goto, done

bad:
	t = strarr(nc,256)
	widget_control, (*pstate).table, set_value = t, $
			row_labels = str_tidy(indgen(256)), column_labels=columns, $
			table_xsize=nc, table_ysize=16, align=2
	ns = 0
done:
	return
	end

;------------------------------------------------------------------------------------------

pro stage_list, group_leader=group, TLB=tlb, data=plist, path=path, master=master, $
				debug=debug, daq=daq_pars, port=daq_port, mport=maia_port, pbeam=pbeam

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
		warning,'stage_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''
if n_elements(debug) lt 1 then debug=0
if n_elements(master) lt 1 then master=0
no_list = 0
if ptr_good(pbeam) eq 0 then pbeam=ptr_new(/allocate_heap)
if ptr_valid(plist) eq 0 then plist = ptr_new(/alloc)
if size(*plist,/tname) ne 'POINTER' then begin
	no_list = 1
endif else begin
	if ptr_valid( (*plist)[0] ) eq 0 then no_list=1
	if no_list eq 0 then if size(*(*plist)[0],/tname) ne 'STRUCT' then no_list=1
endelse
if no_list then begin
	*plist = [ptr_new()]
;	(*(*plist)[0]).active = 1							; set this one to 'OFF'
endif

version = '3.2'

; DAQ control socket parameters struct

ps = bad_pars_struct( daq_port, make_pars=make_ps)
if make_ps then begin
	ps = ptr_new( define(/maia_port))
;	warning,'stage_list','No open DAQ control socket.'
;	return
endif

; Slave Maia control socket parameters struct

psm = bad_pars_struct( maia_port, make_pars=make_ps)
if make_ps then begin
	psm = ptr_new( define(/maia_port))
;	warning,'stage_list','No open DAQ control socket.'
;	return
endif

; Default DAQ control parameters struct

pm = bad_pars_struct( daq_pars, make_pars=make_pm)
if make_pm then begin
	pm = ptr_new( define(daq_struct=36))
;	warning,'stage_list','No DAQ control struct.'
;	return
endif

; Default detector layout struct parameters from file "DAQ_36.csv"

prefix = 'DAQ_36'
letters = ['','A','B','C','D','E','F','G','H','I','J','K','L','M']
prefix = prefix + letters[ (*pm).version.daq ]

case !version.os_family of
	'MacOS': begin
		yw = 220
		mode_xsize = 90
		help_xsize = 900
		end
	'unix': begin
		yw = 252
		mode_xsize = 130
		help_xsize = 900
		end
	else: begin
		yw = 219
		mode_xsize = 90
		help_xsize = 850
		end
endcase

active = ['ON','OFF','START','STOP','REF1','REF2']
axes = ['DAC X','DAC Y','Stage X','Stage Y','Stage Z','Stage A']
shapes = ['Rectangular scan','Elliptical scan']

; 	top-level base

tracking = 1					; later have context-sensitive help window

tlb = widget_base( /column, title='Stage List '+version, /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='stage_list_TLB', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2, /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)

; Table

t = strarr(19,256)

table = Widget_Table( tbase, UNAME='scan-table', /all_events, value=t, Notify_Realize='OnRealize_stage_list_Table',  $
			X_SCROLL_SIZE=12, Y_SCROLL_SIZE=10, /RESIZEABLE_COLUMNS, alignment=2, tracking=tracking, $
			scr_xsize = help_xsize, $
			uvalue='Scan list table: Select row to edit by clicking on row label. Click on "Active" cell to cycle run state ' + $
			'between "ON", "OFF", "START" and "STOP". On/Off enable a scan, Start/Stop indicate the range to run. ' + $
			'Need to be in "master" mode to Start a scan sequence.')

; Buttons

bbase = widget_base( tbase, /row, /base_align_center, xpad = 0, ypad=0, space=20)

data_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
button = widget_button( data_base, value='Grab', uname='grab-button', tracking=tracking, $
					uvalue='Append a scan based on the "metadata.scan.info" metadata read back from Klee for the present scan.')
button = widget_button( data_base, value='Import', uname='import-button', tracking=tracking, $
					uvalue='Load the scan list from a coordinates .CSV or .TXT file.')
button = widget_button( data_base, value='Save', uname='save-button', tracking=tracking, $
					uvalue='Save the current scan list in the table to a .CSV file.')

edit_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
button = widget_button( edit_base, value='Edit', uname='edit-button', tracking=tracking, $
					uvalue='Open scan edit window to edit the selected row in the list, or to append or insert new rows.')
button = widget_button( edit_base, value='Fill', uname='fill-button', tracking=tracking, $
					uvalue='Duplicate a value or values of cells down a range of selected rows in selected columns.')

arrows_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
arrow = picture_button( arrows_base, geopixe_root + 'images/up-16x14.jpeg', uname='arrow-up', $
			/tracking, uvalue='Move selected scan list row(s) up one position. Select rows by clicking in a cell and dragging up or down.', /pushbutton_events)
arrow = picture_button( arrows_base, geopixe_root + 'images/down-16x14.jpeg', uname='arrow-down', $
			/tracking, uvalue='Move selected scan list rows down one position. Select rows by clicking in a cell and dragging up or down.', /pushbutton_events)

del_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
button = widget_button( del_base, value='Delete', uname='delete-button', tracking=tracking, $
					uvalue='Delete the selected scan row(s) in the list. Click and drag down a column [except left column] to select rows.')
button = widget_button( del_base, value='Clear', uname='clear-button', tracking=tracking, $
					uvalue='Clear the entire scan list.')

stage_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
master_check = cw_bgroup2( stage_base, ['Master'], /row, xpad=0, ypad=0, space=0, /return_index, /tracking, $
					uname='master-check', set_value=[0], /nonexclusive, $
					uvalue=['Make this DAQ instance the "master", with control of the stage and Scan execution. ' + $
					'There is very limited control (e.g. "Stop") if not in "Master" mode. Become "Master" before executing a scan list. ' + $
					'Only the "Master" can advance to the next scan.'])
label = widget_label( stage_base, value='Sequence:')
start_button = widget_button( stage_base, value='Start', uname='start-button', tracking=tracking, $
					uvalue='"Start" the execution of the scan list, for rows tagged "ON" between rows labelled "START" and "STOP". ' + $
					'Click left column to toggle state between "ON", "OFF", "START" and "STOP".' + $
					'Need to be in "master" mode to Start a scan sequence.')
stop_button = widget_button( stage_base, value='Stop', uname='stop-button', tracking=tracking, $
					uvalue='"Stop" the execution of the scan list.')
pause_button = widget_button( stage_base, value='Pause', uname='pause-resume-button', tracking=tracking, $
					uvalue='Toggle scan list pause state between "Pause" and "Resume".' + $
					'Need to be in "master" mode to Pause a scan sequence.')

;close_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
;button = widget_button( close_base, value='Close', uname='close-button', tracking=tracking, $
;					uvalue='Close the scan list window. Scan list data remains stored in the parent DAQ Control window.')

time_base = widget_base( bbase, /row, xpad=0, ypad=0, space=2, /align_right, /base_align_center)
label = widget_label( time_base, value='Total Time:')
time_text = widget_text( time_base, value='', uname='time-text', tracking=tracking, scr_xsize=mode_xsize, $
			uvalue='Shows the total time for all scans marked "START" or "ON" before a "STOP".')

help = widget_text( tbase, scr_xsize=help_xsize, ysize=2, /wrap, uname='help', tracking=tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

state = { $
		path:			ptr_new(path), $		; pointer to current path
		plist:			plist, $				; pointer to array of pointers to scan detail structs
		ps:				ps, $					; pointer to Klee port struct
		psm:			psm, $					; pointer to Maia port struct
		pm:				pm, $					; pointer to DAQ parameters struct
		pselect:		ptr_new(/allocate_heap), $	; pointer to select for notify to scan_edit
		pbeam:			pbeam, $				; pointer to beam current estimate (nA)

		scan_sequence: { $
			active:				0, $			; scan list is running	
			scan: {		index:	0}, $			; present scan list index
			raster: {	on:		0}}, $			; raster started and 'running'		
		master:			master, $				; flags this instance as the DAQ master
		master_check:	master_check, $			; ID of master checkbox
		lock_master:	0, $					; locks master during check set
			
		tracking:		tracking, $				; is tracking enabled
		debug:			debug, $				; debug flag
		row_height:		0L, $					; table row height
		xoffset:		0L, $					; X offset for resize
		yoffset:		0L, $					; Y offset for resize
		axes:			axes, $					; axis list
		shapes:			shapes, $				; shapes list
		active:			active, $				; activation modes for items in list

		table:			table, $				; table ID
		rows:			256, $					; number of rows
		columns:		19, $					; number of colums
		headings:		ptr_new(), $			; pointer to column headings
		sel: {left:-1, top:-1, right:-1, bottom:-1 }, $	; use "(*pstate).sel.top" as current region
		cr_found:		0, $					; to fight MAC IDL bug
		
		pause_button:	pause_button, $			; pause/resume button ID
		time_text:		time_text, $			; total time text ID
		help:			help $					; ID of help text
	}

*state.pselect = state.sel

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['path', $				; new path
					'master', $					; change master state
					'scan-list-update', $		; update list display
					'scan-sequence-next', $		; step to next in scan list sequence
					'scan-command', $			; execute a remote command (e.g. 'stop', 'pause', 'start')
					'scan-done' $				; scan finished message
					], from=group

xmanager, 'stage_list', tlb, /no_block

return
end
