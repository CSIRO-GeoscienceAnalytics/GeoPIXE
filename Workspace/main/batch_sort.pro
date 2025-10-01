
;	Batch Sort table.

pro batch_sort_event, event

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
		warning,'batch_sort_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

no_batch = 0
if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
p = (*pstate).pbatch
if ptr_valid(p) eq 0 then goto, bad_ptr
if size(*p,/tname) ne 'POINTER' then begin
	no_batch = 1
endif else begin
	if ptr_valid( (*p)[0] ) eq 0 then no_batch=1
	if no_batch eq 0 then if size(*(*p)[0],/tname) ne 'STRUCT' then no_batch=1
endelse
obj = (*pstate).DevObj

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
				;	print,'fit batch: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
					s = build_output_path( *(*pstate).dpath, *(*pstate).path, (*pstate).root, /set)
					if lenchr(s) gt 0 then *(*pstate).path = s
				endif
				goto, finish
				end
			'dpath': begin
				if ptr_valid( event.pointer) then begin
				;	print,'fit batch: new data path = ',(*event.pointer)
					*(*pstate).dpath = (*event.pointer)
					s = build_output_path( *(*pstate).dpath, *(*pstate).path, (*pstate).root, /set)
					if lenchr(s) gt 0 then *(*pstate).path = s
				endif
				goto, finish
				end
			'root': begin
				if ptr_valid( event.pointer) then begin
				;	print,'fit batch: new root = ',(*event.pointer).path
					*(*pstate).root = (*event.pointer)
				endif
;				help, *(*pstate).dpath, *(*pstate).path
;				help, *(*pstate).root, /str

				r = dialog_message( ['Use new path assignment to correct all output paths in Batch Sort?', $
							'', 'Only enabled runs will be corrected.', '', $
							'Make sure in/out paths in Sort EVT window make sense.', $
							'All runs will inherit the same path remapping rules.'], /question, $
							title='Correct Batch output paths')
				if r eq 'Yes' then begin
					batch_correct_output, pstate, error=error
				endif
				update_batch_sort_table, pstate, event
				goto, finish
				end
			'evt-mode': begin
				if ptr_valid( event.pointer) then begin
					(*pstate).mode = (*event.pointer)
				endif
				goto, finish
				end
			'evt-type': begin
				if ptr_valid( event.pointer) then begin
					(*pstate).type = (*event.pointer)
				endif
				goto, finish
				end
			'device': begin
				if ptr_valid( event.pointer) then begin
					if obj_valid( *event.pointer) eq 0 then begin
						print,'batch_sort_Event: new device is invalid Object'
						help, *event.pointer, /str
					endif else begin
						(*pstate).DevObj = clone_device_object(*event.pointer)
						obj = (*pstate).DevObj
						print,'batch_sort_Event: new device =', obj->name()
					endelse
				endif
				goto, finish
				end
			'abort-evt': begin
				print,'Batch Sort: abort-evt ...........................................................'
				progress, /complete, (*pstate).progress_tlb, 'Batch sort aborted ...'
				progress, /ending, (*pstate).progress_tlb
				goto, finish
				end
			'done-evt': begin
				print,'Batch Sort: done-evt ...........................................................'
				if ptr_valid(event.pointer) then begin
					(*(*p)[(*pstate).current_sort]).charge = *event.pointer
					print,' Charge returned to batch-sort = ',*event.pointer
				endif
				; digital filters, etc.
				*(*pstate).pargs0 = {first:(*pstate).first, do_it:(*pstate).options[0]}
				notify, 'batch-filter', (*pstate).pargs0, from=event.top
				update_batch_sort_table, pstate, event
				goto, finish
				end
			'done-filter': begin
				print,'Batch Sort: done-filter ...........................................................'
				flipx = 1 - (((*pstate).current_sort - (*pstate).first_sort) mod 2)
				over = (*pstate).options[1]
				sav = 1
				if ptr_valid( event.pointer) then begin
					if *event.pointer eq 0 then begin
						over = 1
						sav = 0
					endif					
				endif
				*(*pstate).pargs1 = {first:((*pstate).current_sort eq (*pstate).first_sort), save:sav, html:'', bw:'', $
								export:'', overwrite:over, correctX:(*pstate).options[6], $
								mirrorX:(*pstate).options[7] and flipx, tiff:'', tiff_type:0, $
								rgb:(*pstate).options[11], metadata:(*pstate).options[12]}
				if (*pstate).options[2] then begin						; output HTML
					file = (*(*p)[(*pstate).current_sort]).file
					path = *(*pstate).path + 'html' + path_sep()
					file = path + strip_file_ext(strip_path(file)) + '.html'
					(*(*pstate).pargs1).html = file
				endif else if (*pstate).options[3] then begin			; output B/W HTML
					file = (*(*p)[(*pstate).current_sort]).file
					path = *(*pstate).path + 'html' + path_sep() + 'bw' + path_sep()
					file = path + strip_file_ext(strip_path(file)) + '-bw.html'
					(*(*pstate).pargs1).bw = file
				endif
				if (*pstate).options[4] then begin						; export CSV/TXT image(s)
					file = (*(*p)[(*pstate).current_sort]).file
					path = *(*pstate).path + 'export' + path_sep()
					file = path + strip_file_ext(strip_path(file)) + '.csv'
					(*(*pstate).pargs1).export = file
				endif
				if (*pstate).options[8] or (*pstate).options[9] or (*pstate).options[10] then begin						; export TIFF image(s)
					file = (*(*p)[(*pstate).current_sort]).file
					path = *(*pstate).path + 'tiff' + path_sep()
					file = path + strip_file_ext(strip_path(file)) + '.html'
					(*(*pstate).pargs1).tiff = file
					(*(*pstate).pargs1).tiff_type = 0
					if (*pstate).options[9] then (*(*pstate).pargs1).tiff_type = 1
					if (*pstate).options[10] then (*(*pstate).pargs1).tiff_type = 2
				endif
				notify, 'batch-save', (*pstate).pargs1, from=event.top
				goto, finish
				end
			'done-save': begin
				print,'Batch Sort: done-save ...........................................................'
				if no_batch then goto, finish
				if (*pstate).first then begin				; was first, now start at the beginning
					(*pstate).first = 0
					np = n_elements(*p)
					for i=0L,np-1 do begin
						if (*(*p)[i]).enable and ((*(*p)[i]).suppress eq 1) then begin
							(*pstate).current_sort = i
							set_table_select, (*pstate).table, i,i, rows=(*pstate).rows, columns=(*pstate).columns
;							widget_control, (*pstate).table, set_table_select=[-1,i,-1,i]
							load = ((*pstate).options[0] eq 1) or ((*pstate).options[2] eq 1) or ((*pstate).options[3] eq 1) or $
									((*pstate).options[4] eq 1) or ((*pstate).options[6] eq 1) or ((*pstate).options[7] eq 1) or $
									((*pstate).options[8] eq 1) or ((*pstate).options[9] eq 1) or ((*pstate).options[10] eq 1) or $
									((*pstate).options[11] eq 1) or ((*pstate).options[12] eq 1)
							*(*pstate).psort = {p:(*p)[(*pstate).current_sort], skip:(*pstate).options[5], load:load}
							progress, /update, (*pstate).progress_tlb, {unit:0, value:0, current:(i>1), size:np}, cancel=cancel
							if cancel then goto, finish
							print,'start-evt: #',i
							notify, 'start-evt', (*pstate).psort, from=event.top
							goto, finish
						endif
					endfor
				endif else begin							; find next enabled (was not first [suppress=0])
					np = n_elements(*p)
					if (*pstate).current_sort lt np-1 then begin
						ndone = 0L
						ntotal = 0L
						for i=0,(*pstate).current_sort do begin
							if (*(*p)[i]).enable and ((*(*p)[i]).suppress eq 1) then ndone = ndone + 1
						endfor
						for i=0,np-1 do begin
							if (*(*p)[i]).enable and ((*(*p)[i]).suppress eq 1) then ntotal = ntotal + 1
						endfor
						for i=(*pstate).current_sort+1,np-1 do begin
							if (*(*p)[i]).enable and ((*(*p)[i]).suppress eq 1) then begin
								(*pstate).current_sort = i
								set_table_select, (*pstate).table, i,i, rows=(*pstate).rows, columns=(*pstate).columns
;								widget_control, (*pstate).table, set_table_select=[-1,i,-1,i]
								load = ((*pstate).options[0] eq 1) or ((*pstate).options[2] eq 1) or ((*pstate).options[3] eq 1) or $
										((*pstate).options[4] eq 1) or ((*pstate).options[6] eq 1) or ((*pstate).options[7] eq 1) or $
										((*pstate).options[8] eq 1) or ((*pstate).options[9] eq 1) or ((*pstate).options[10] eq 1) or $
										((*pstate).options[11] eq 1) or ((*pstate).options[12] eq 1)
								*(*pstate).psort = {p:(*p)[(*pstate).current_sort], skip:(*pstate).options[5], load:load}
								progress, /update, (*pstate).progress_tlb, {unit:0, value:0, current:(ndone>1), size:ntotal}, cancel=cancel
								if cancel then goto, finish
								print,'start-evt: #',i
								notify, 'start-evt', (*pstate).psort, from=event.top
								goto, finish
							endif
						endfor
					endif
				endelse
				progress, /complete, (*pstate).progress_tlb, 'Batch sort finished ...'
				progress, /ending, (*pstate).progress_tlb
				print,'Batch Sort: finished.   ........................................................'
				goto, finish
				end
			else:
		endcase
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
		print,'Kill request batch_sort ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'batch-table': begin
		case tag_names( event, /structure_name) of

			'WIDGET_TABLE_CELL_SEL': begin
				(*pstate).sel.left = event.sel_left
				(*pstate).sel.top = event.sel_top
				(*pstate).sel.right = event.sel_right
				(*pstate).sel.bottom = event.sel_bottom
				end
			'WIDGET_TABLE_CH': begin

				if (event.ch eq 13B) or (event.ch eq 10B) then begin
					if no_batch eq 0 then begin
						n = (*pstate).sel.top
						widget_control, (*pstate).table, get_value=t
						(*(*p)[n]).file = t[0,n]
						(*(*p)[n]).xrange = fix(t[1,n])
						(*(*p)[n]).yrange = fix(t[2,n])
						(*(*p)[n]).xsize = float(t[3,n])
						(*(*p)[n]).ysize = float(t[4,n])
						(*(*p)[n]).charge = float(t[5,n])
						(*(*p)[n]).comment = t[6,n]
					endif
					if ((*pstate).sel.top lt (*pstate).rows-1) then begin
						widget_control, (*pstate).table, edit_cell=[(*pstate).sel.top+1,(*pstate).sel.left]
						(*pstate).sel.left = (*pstate).sel.left
						(*pstate).sel.top = (*pstate).sel.top+1
						(*pstate).sel.bottom = (*pstate).sel.top > (*pstate).sel.bottom
					endif
				endif
				end
			'WIDGET_TABLE_TEXT_SEL': begin
				end
			else:
		endcase
		end

	'batch_sort_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
			print,event.x,event.y
				yoff = (*pstate).tracking ? 50 : 0
				x = (event.x > 435) - 10
				y = (event.y > (200+yoff)) - (160+yoff)
				n = (y/ (*pstate).row_height) - 2
				y = (n + 2) * (*pstate).row_height
				widget_control, (*pstate).table, scr_xsize=x, scr_ysize=y
				end
			else:
		endcase
		end

	'batch-setup-file': begin
		widget_control, event.id, get_value=F
		path = *(*pstate).path
		if F ne '' then begin
			F = strip_file_ext(F) + '.sbatch'
			*(*pstate).path = extract_path(F)
			print,'Load batch set-up from ',F
			load_batch_sort, pstate, F
			(*pstate).file = F

			update_batch_sort_table, pstate, event
			(*pstate).sel.top = -1
			(*pstate).sel.bottom = -1
		endif
		end

	'load-setup-button': begin
		path = *(*pstate).path
		F = file_requester( /read, filter = '*.sbatch', $
				/must_exist, path=path, group=event.top, $
				title='Select the BATCH set-up file for Sort EVT', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.sbatch'
			*(*pstate).path = extract_path(F)
			print,'Load batch set-up from ',F
			load_batch_sort, pstate, F
			(*pstate).file = F
			widget_control, (*pstate).setup_text, set_value=F

			update_batch_sort_table, pstate, event
			(*pstate).sel.top = -1
			(*pstate).sel.bottom = -1
		endif
		end

	'save-setup-button': begin
		if no_batch then goto, finish
		path = (*pstate).dir
		file = extract(path,0,strlen(path)-2)
		file = strip_path(file) + '.sbatch'
		F = file_requester( /write, filter = '*.sbatch', $
				path=path, file=file, group=event.top, $
				title='Save the batch set-up table to a BATCH file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.sbatch'
			*(*pstate).path = extract_path(F)
			print,'save batch set-up to ',F
			widget_control, (*pstate).setup_text, set_value=F
			save_batch_sort, pstate, F
		endif
		end

	'dir-button': begin
		path = (*pstate).dir
		if strlen(path) lt 1 then path = *(*pstate).path
		F = file_requester( /read, /must_exist, /dir, $
				path=path, group=event.top, $
				title='Select the root directory tree to scan')
		if F ne '' then begin
			*(*pstate).path = F
			print,'Scan dir tree ',F
			scan_batch_dir, pstate, F, group=event.top
			(*pstate).dir = F
			widget_control, (*pstate).dir_text, set_value=F

			update_batch_sort_table, pstate, event
			(*pstate).sel.top = -1
			(*pstate).sel.bottom = -1
		endif
		end

	'dir-text': begin
		widget_control, event.id, get_value=F
		path = *(*pstate).path
		if F ne '' then begin
			*(*pstate).path = F
			print,'Load batch set-up from ',F
			scan_batch_dir, pstate, F, group=event.top
			(*pstate).dir = F

			update_batch_sort_table, pstate, event
			(*pstate).sel.top = -1
			(*pstate).sel.bottom = -1
		endif
		end

	'delete-dai': begin
		ok = dialog_message(['Delete all DAI files in list within run number range.','Are you sure?'], /question)
		if ok eq 'Yes' then begin
			batch_delete_dai, pstate
			scan_batch_dir, pstate, (*pstate).dir, group=event.top, /silent
			update_batch_sort_table, pstate, event
		endif
		end

	'build-tree-button': begin
		batch_build_dir_tree, pstate

		scan_batch_dir, pstate, (*pstate).dir, group=event.top
		update_batch_sort_table, pstate, event
		(*pstate).sel.top = -1
		(*pstate).sel.bottom = -1
		end

	'options': begin
		(*pstate).options[event.value] = event.select
		if event.value ne 3 then begin
			if (*pstate).options[2] then (*pstate).options[3]=0
		endif
		if event.value ne 9 then begin
			if (*pstate).options[8] then (*pstate).options[9]=0
			if (*pstate).options[10] then (*pstate).options[9]=0
		endif
		if event.value ne 10 then begin
			if (*pstate).options[8] then (*pstate).options[10]=0
			if (*pstate).options[9] then (*pstate).options[10]=0
		endif
		if (*pstate).options[3] then (*pstate).options[2]=0
		if (*pstate).options[9] then (*pstate).options[8]=0
		if (*pstate).options[10] then (*pstate).options[8]=0
		if (*pstate).options[10] then (*pstate).options[9]=0
		widget_control, (*pstate).options_id, set_value=(*pstate).options
		end

	'start-button': begin
		if no_batch then goto, finish

		; Make sure the operations and RGB windows are open ...

		notify, 'batch-operations-open', from=event.top									; pass no pointer, just opens window
		if (*pstate).options[11] then notify, 'batch-RGB-open', from=event.top			; pass no pointer, just opens window

		widget_control, (*pstate).table, get_value=t
		fix_first = 0
		first_found = 0
		for i=0L,min([n_elements(*p),n_elements(t[0,*])])-1 do begin
			(*(*p)[i]).xrange = fix2(t[1,i])
			(*(*p)[i]).yrange = fix2(t[2,i])
			(*(*p)[i]).xsize = float2(t[3,i])
			(*(*p)[i]).ysize = float2(t[4,i])
			(*(*p)[i]).charge = float2(t[5,i])
			(*(*p)[i]).comment = t[6,i]						; suppress means no pop-ups
			if ((*(*p)[i]).suppress eq 0) then begin		; suppress=0 for 'first'
				if ((*(*p)[i]).enable eq 0) then fix_first=1
				first_found = 1
			endif
		endfor
		np = n_elements(*p)
		if fix_first or (first_found eq 0) then begin
			for i=0L,np-1 do begin
				(*(*p)[i]).suppress = 1
			endfor
			for i=0L,np-1 do begin
				if (*(*p)[i]).enable eq 1 then begin
					(*(*p)[i]).suppress = 0
					break
				endif
			endfor
		endif
		for i=0L,np-1 do begin
			if (*(*p)[i]).suppress eq 0 then begin				; suppress=0 for first to run
				(*pstate).current_sort = i						; starting run  
				(*pstate).first_sort = i 
				load = ((*pstate).options[0] eq 1) or ((*pstate).options[2] eq 1) or ((*pstate).options[3] eq 1) or $
						((*pstate).options[4] eq 1) or ((*pstate).options[6] eq 1) or ((*pstate).options[7] eq 1) or $
						((*pstate).options[8] eq 1) or ((*pstate).options[9] eq 1) or ((*pstate).options[10] eq 1) or $
						((*pstate).options[11] eq 1) or ((*pstate).options[12] eq 1) 
				*(*pstate).psort = {p:(*p)[i], skip:(*pstate).options[5], load:load}
				break
			endif
		endfor
		ntodo = 0L
		for i=0L,np-1 do begin
			if (*(*p)[i]).enable eq 1 then begin
				ntodo = ntodo + 1
			endif
		endfor
		
		progress, tlb=progress_tlb, title='Batch Processing of Images', /top
		(*pstate).progress_tlb = progress_tlb
		set_table_select, (*pstate).table, (*pstate).current_sort,(*pstate).current_sort, rows=(*pstate).rows, columns=(*pstate).columns
;		widget_control, (*pstate).table, set_table_select=[-1,(*pstate).current_sort,-1,(*pstate).current_sort]
		(*pstate).first = 1
		progress, /update, (*pstate).progress_tlb, {unit:0, value:0, current:0, size:ntodo}, cancel=cancel
		if cancel eq 0 then begin
			print,'start-evt: #',(*pstate).current_sort
			notify, 'start-evt', (*pstate).psort, from=event.top
		endif
		end

	'fill-button': begin
		if no_batch then goto, finish
		np = n_elements(*p)
		if (((*pstate).sel.top le np-1) and ((*pstate).sel.bottom ge (*pstate).sel.top)) and  $
						((*pstate).sel.left le (*pstate).sel.right) then begin

			widget_control, (*pstate).table, get_value=t
			i1 = (*pstate).sel.top
			i2 = np-1
			if (*pstate).sel.bottom gt (*pstate).sel.top then i2=(*pstate).sel.bottom
			for k=(*pstate).sel.left,(*pstate).sel.right do begin
				for i=i1,i2 do begin
					t[k,i] = t[k,i1]
				endfor
			endfor

			for i=0L,min([n_elements(*p),n_elements(t[0,*])])-1 do begin
				(*(*p)[i]).xrange = fix2(t[1,i])
				(*(*p)[i]).yrange = fix2(t[2,i])
				(*(*p)[i]).xsize = float2(t[3,i])
				(*(*p)[i]).ysize = float2(t[4,i])
				(*(*p)[i]).charge = float2(t[5,i])
				(*(*p)[i]).comment = t[6,i]
			endfor
			widget_control, (*pstate).table, set_value=t
		endif
		end

	'delete-button': begin
		if no_batch then goto, finish
		np = n_elements(*p)
		if ((*pstate).sel.top ge 0) and ((*pstate).sel.bottom lt np) then begin
			for i=(*pstate).sel.top,(*pstate).sel.bottom do begin
				if ptr_valid( (*p)[i]) then ptr_free, (*p)[i]
			endfor
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
		update_batch_sort_table, pstate, event
		end

	'enable-button': begin
		if no_batch then goto, finish
		fix_first = 0
		np = n_elements(*p)
		if ((*pstate).sel.top ge 0) and ((*pstate).sel.bottom lt np) then begin
			for i=(*pstate).sel.top,(*pstate).sel.bottom do begin
				(*(*p)[i]).enable = 1 - (*(*p)[i]).enable 
			if ((*(*p)[i]).enable eq 0) and ((*(*p)[i]).suppress eq 0) then fix_first=1
			endfor
		endif
		if fix_first then begin
			for i=0L,np-1 do begin
				(*(*p)[i]).suppress = 1
			endfor
			for i=0L,np-1 do begin
				if (*(*p)[i]).enable eq 1 then begin
					(*(*p)[i]).suppress = 0
					break
				endif
			endfor
		endif
		update_batch_sort_table, pstate, event
		end

	'first-button': begin
		if no_batch then goto, finish
		fix_first = 0
		np = n_elements(*p)
		for i=0L,np-1 do begin
			(*(*p)[i]).suppress = (i ne (*pstate).sel.top)
			if ((*(*p)[i]).enable eq 0) and ((*(*p)[i]).suppress eq 0) then fix_first=1
		endfor
		if fix_first then begin
			for i=0L,np-1 do begin
				(*(*p)[i]).suppress = 1
			endfor
			for i=0L,np-1 do begin
				if (*(*p)[i]).enable eq 1 then begin
					(*(*p)[i]).suppress = 0
					break
				endif
			endfor
		endif
		update_batch_sort_table, pstate, event
		end

	'clear-button': begin
		if no_batch then goto, finish
		for i=0L,n_elements(*p)-1 do begin
			if ptr_valid( (*p)[i]) then ptr_free, (*p)[i]
		endfor
		*p = ptr_new()
		update_batch_sort_table, pstate, event
		(*pstate).sel.top = -1
		(*pstate).sel.bottom = -1
		end

	'close-button': begin
		print,'Close fit batch ...'
		goto, kill
		end
	else:
endcase

finish:
	widget_control, hourglass=0
	close, 1
	return

bad_state:
	warning,'batch_sort_event',['STATE variable has become ill-defined.','Abort Batch Sort.'],/error
	goto, kill
bad_ptr:
	warning,'batch_sort_event',['Parameter structure variable has become ill-defined.','Abort Batch Sort.'],/error
	goto, kill

kill:
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).dpath) then ptr_free, (*pstate).dpath
	if ptr_valid( (*pstate).headings) then ptr_free, (*pstate).headings
	if ptr_valid( (*pstate).psort) then ptr_free, (*pstate).psort
	if ptr_valid( (*pstate).pargs0) then ptr_free, (*pstate).pargs0
	if ptr_valid( (*pstate).pargs1) then ptr_free, (*pstate).pargs1
	if ptr_valid( (*pstate).pargs2) then ptr_free, (*pstate).pargs2

;	What about (*pstate).pbatch?  Should be done in parent ...

	if (*pstate).local then begin
		if size(*(*pstate).pbatch,/tname) eq 'POINTER' then begin
			for i=0L,n_elements(*(*pstate).pbatch)-1 do begin
				if ptr_valid( (*(*pstate).pbatch)[i] ) then ptr_free, (*(*pstate).pbatch)[i]
			endfor
		endif
	endif
	free_device_objects, (*pstate).DevObj
	ptr_free, pstate

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;-----------------------------------------------------------------

pro batch_build_dir_tree, pstate

; Scan the dir tree F for list-mode files

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
if n_elements(geopixe_root) lt 1 then geopixe_root=''

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	pbatch = (*pstate).pbatch
	if ptr_valid(pbatch) eq 0 then return
	if size(*pbatch,/tname) ne 'POINTER' then return
	if ptr_valid( (*pbatch)[0] ) eq 0 then return

	dir = (*pstate).dir
	ns = n_elements(*pbatch)
	if ns eq 0 then return
	widget_control, /hourglass

;	Each table entry like this: { file:s[i], output:'', suppress:0, enable:1, xrange:0,yrange:0, xsize:0.0,ysize:0.0, charge:0.0, comment:''}

	evt = strarr(ns)
	done = intarr(ns)
	q = indgen(ns)
	names = strarr(16,ns)
	b2 = bytarr(256)

	for i=0L,ns-1 do begin
		evt[i] = (*(*pbatch)[i]).file
		b = byte(strip_path(strip_file_ext(evt[i])))
		nb = n_elements(b)
		b2[0:nb-1] = b

		if nb ge 2 then begin							; split on switch from alpha to numeric
			for j=nb-2,0,-1 do begin
;				if (((b[j] ge 48) and (b[j] le 57)) and (((b[j+1] ge 65) and (b[j+1] le 90)) or ((b[j+1] ge 97) and (b[j+1] le 122)))) or $
;							(((b[j+1] ge 48) and (b[j+1] le 57)) and (((b[j] ge 65) and (b[j] le 90)) or ((b[j] ge 97) and (b[j] le 122)))) then begin
				if (((b[j+1] ge 48) and (b[j+1] le 57)) and (((b[j] ge 65) and (b[j] le 90)) or ((b[j] ge 97) and (b[j] le 122)))) then begin
					b2[j+2:nb] = b2[j+1:nb-1]
					b2[j+1] = 45
					nb = nb+1
				endif
			endfor
		endif
		str = string(b2[0:nb-1])
		sub = strsplit( str, '-_$%@!#~', /extract)		; split on separators
		names[0:n_elements(sub)-1,i] = sub
	endfor

	dir = strtrim(dir,2)
	dir = extract(dir,0,strlen(dir)-2)

	batch_disperse_tree, names, q, done, level=0, files=evt, root=dir, error=error
	if error then begin
		warning,'batch_build_dir_tree',['','Error building directory tree,','or dispersing files into it.']
		return
	endif
end

;--------------------------------------------------------------------------------------------------------

pro batch_correct_output, pstate, error=error

	COMPILE_OPT STRICTARR
	if ptr_good(pstate) eq 0 then return
	error = 1

	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		message, /RESET
		return
	endif

	p = (*pstate).pbatch

	for j=0,n_elements(*p)-1 do begin
		if (*(*p)[j]).enable then begin
			s = build_output_path( (*(*p)[j]).file, (*(*p)[j]).output, (*pstate).root)
			T = strip_file_ext( strip_path((*(*p)[j]).file))
			if (*pstate).DevObj->multi_files() and ((*pstate).DevObj->multi_char() ne '.') then begin
				T = strip_file_m( T, ending=(*pstate).DevObj->multi_char() + ((adc_offset_device((*pstate).DevObj) eq -1) ? '0' : '1'))
			endif
			if (*pstate).DevObj->embed_detector() then begin
				k = locate_last( "_", T)								; det # is before the multifile char
				q = where( k ge 0, nq)
				if nq ge 1 then T[q] = strmid2( T[q], 0,k[q])
			endif
			if ((*pstate).mode eq 1) then T = strip_file_m( T, ending='-cuts') + '-cuts'
			if ((*pstate).mode eq 3) then T = strip_file_m( T, ending='-MPDA') + '-MPDA'
			f = s + T + '.dai'
			print, ' Corrected output path = '+ f
			if lenchr(s) gt 0 then (*(*p)[j]).output = f
		endif
	endfor

	error = 0
	return
end

;-----------------------------------------------------------------

pro batch_disperse_tree, names, q, done, level=level, files=files, root=rooti, error=error

COMPILE_OPT STRICTARR
	if n_elements(names) lt 1 then return
	if n_elements(q) lt 1 then return
	if n_elements(done) lt 1 then return
	if n_elements(files) lt 1 then return
	if n_elements(level) lt 1 then level=0
	if n_elements(rooti) lt 1 then root=extract_path(files[0])
	error = 0
	on_ioerror, bad_io

	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		message, /RESET
		error = 1
		return
	endif
	widget_control, /hourglass

;	print,'disperse: level=',level,' rooti=',rooti
	nq = n_elements(q)
	for i=0L,nq-1 do begin
		root = rooti
		if done[q[i]] eq 0 then begin
			token = names[level,q[i]]
			if strlen(token) lt 1 then begin
;				print,'blank token - make dir ',root,'    if not exist.'
				if file_test(root,/dir) eq 0 then begin
					safe_file_mkdir, root, error=error, /noexpand_path
					if error then begin
						warning,'batch_sort',['Illegal directory name:',root]
					endif
				endif
;				print,'   copy ',strip_file_ext(files[q[i]])+'.*',' to ',root
				file_move, strip_file_ext(files[q[i]])+'.*', root, /require_dir
				done[q[i]] = 1
				continue
			endif

			q2 = where( (names[level,q] eq token) and (done[q] eq 0), nq2)
			if nq2 eq 0 then continue
			if nq2 eq 1 then begin
;				print,'nq2=1 - make dir ',root,'    if not exist.'
				if file_test(root,/dir) eq 0 then begin
					safe_file_mkdir, root, error=error, /noexpand_path
					if error then begin
						warning,'batch_sort',['Illegal directory name:',root]
					endif
				endif
				l = level+1
				while strlen(names[l,q[i]]) gt 0 do begin
					token = token+'-'+names[l,q[i]]
					l = l+1
				endwhile
				root = root + path_sep() + token
;				print,'nq2=1 - make local dir ',root,'    if not exist.'
				if file_test(root,/dir) eq 0 then begin
					if file_test(root) eq 1 then goto, bad_dir
					safe_file_mkdir, root, error=error, /noexpand_path
					if error then begin
						warning,'batch_sort',['Illegal directory name:',root]
					endif
				endif
;				print,'   copy ',strip_file_ext(files[q[i]])+'.*',' to ',root
				file_move, strip_file_ext(files[q[i]])+'.*', root, /require_dir
				done[q[i]] = 1
			endif else begin
				if nq2 eq nq then begin
					root = root + '-' + token
				endif else begin
;					print,'nq2>1 - make dir ',root,'    if not exist.'
					if file_test(root,/dir) eq 0 then begin
						safe_file_mkdir, root, error=error, /noexpand_path
						if error then begin
							warning,'batch_sort',['Illegal directory name:',root]
						endif
					endif
					root = root + path_sep() + token
				endelse

				batch_disperse_tree, names, q[q2], done, level=level+1, files=files, root=root, error=error
				if error then return
			endelse
		endif
	endfor
	return

bad_io:
	warning,'batch_disperse_tree',['I/O Error.','Read-only directory, or exisiting files.']
	error = 1
	return
bad_dir:
	warning,'batch_disperse_tree',['Ordinary file exists:"'+root+'"', $
							'with same name as desired sub-directory.', $
							'Remove or rename this file, and try again.']
	error = 1
	return
end

;-----------------------------------------------------------------

pro scan_batch_dir, pstate, F, group=group, silent=silent

; Scan the dir tree F for list-mode files

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
if n_elements(geopixe_root) lt 1 then geopixe_root=''
if n_elements(silent) eq 0 then silent=0

	if n_params() lt 2 then begin
		print,'scan_batch_dir: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	pbatch = (*pstate).pbatch
	if ptr_valid(pbatch) eq 0 then return
	obj = (*pstate).DevObj
	
	if silent then begin
		rmin = (*pstate).run_min
		rmax = (*pstate).run_max
		use_range = 0
		if (*pstate).run_max gt 0 then use_range=1
		goto, cont
	endif

	drop = ['Use all event files/ runs','Filter numeric event file (run) names']
	help_drop = 'For numeric event file names, using run numbers, you can choose a range within min/max run numbers.'
	text = ['Low run number:','high run number:']
	map_text = [0,1]
	initial_text = str_tidy(string([0,0]))
	help_text = ['Low event run number to include.','High event run number to include.']
	Help_default = 'For numeric event file names, using run numbers, you can choose a range within min/max run numbers.'
	r = options_popup( title='Event run number range option', text=text, initial_text=initial_text, help_text=help_text, map_text=map_text, $
				help_default=help_default, drop=drop, help_drop=help_drop, min_xsize=500, error=error)
	if error then return
	
	use_range = r.drop[0]
	rmin = long2(r.text[0])
	rmax = long2(r.text[1])

cont:
	widget_control, /hourglass
	if use_range then begin
		(*pstate).run_min = rmin
		(*pstate).run_max = rmax
		p = scan_dir_evt( F, obj, /image, ppath=(*pstate).path, proot=(*pstate).root, mode=(*pstate).mode, rmin=rmin, rmax=rmax, error=error)
	endif else begin
		(*pstate).run_min = 0L
		(*pstate).run_max = -1L
		p = scan_dir_evt( F, obj, /image, ppath=(*pstate).path, proot=(*pstate).root, mode=(*pstate).mode, error=error)
	endelse

	if error eq 0 then begin
		if size(*pbatch,/tname) eq 'POINTER' then begin
			for i=0L,n_elements(*pbatch)-1 do begin
				if ptr_valid( (*pbatch)[i] ) then ptr_free, (*pbatch)[i]
			endfor
		endif
		*pbatch = p
	endif
	return
end

;-----------------------------------------------------------------

pro batch_delete_dai, pstate

; Scan the dir tree for all DAI files with same name as list-modes

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
if n_elements(geopixe_root) lt 1 then geopixe_root=''

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	pbatch = (*pstate).pbatch
	if ptr_valid(pbatch) eq 0 then return
	obj = (*pstate).DevObj

;	ext = obj->extension()
;	evt = '*' + ext
;	s = file_search( (*pstate).dir, evt)		; need to do more for Lund, look before "%" char
;	ns = n_elements(s)							; accept only first file, reject any extensions

	ext = obj->extension()
	if obj->multi_files() then begin								; for multi-file sets, need to
		s = (adc_offset_device(obj) eq -1) ? '0' : '1'				; search for only first file
		evt = '*' + obj->multi_char() + s + ext
	endif else begin
		evt = '*' + ext
	endelse
	s = file_search( (*pstate).dir, evt)										; all evt files in tree

	dai = strip_file_ext(evt) + '.dai'								; all original dai files
	s2 = file_search( (*pstate).dir, dai)
	ok = file_test( s2, /read) and (file_test( s2, /zero_length) ne 1)
	q = where( ok eq 1, nd)
	if nd gt 0 then begin
		s2 = s2[q]
		if lenchr(s2[0]) eq 0 then nd=0
	endif
	if nd gt 0 then s = [s,s2]
	ns = n_elements(s)

	if (n_elements((*pstate).run_min) ge 0) or (n_elements((*pstate).run_max) gt 0) then begin
		name = strip_file_ext( strip_path( s))
		if obj->multi_files() then begin
			j = locate_last( obj->multi_char(), name)				; run # is before the multifile char
			q = where( j ge 0, nq)
			if nq ge 1 then name[q] = strmid( name[q], 0,j[q])
		endif
		q = where( inumeric(name) eq 1, nq)
		if (nq ge 1) and ((*pstate).run_max gt 0) then begin
			q2 = where( (long2(name[q]) ge (*pstate).run_min) and (long2(name[q]) le (*pstate).run_max), nq2)
			if nq2 ge 1 then begin
				q = q[q2]
			endif
			nq = nq2
		endif
		if nq ge 1 then begin
			s = s[q]
		endif else s = ''
		ns = n_elements(s)
	endif

	if ns eq 0 then return
	widget_control, /hourglass

	for i=0L,ns-1 do begin
		fim = strip_file_ext(s[i])+'.dai'
		if file_test( fim, /read) then begin
			file_delete, fim, /quiet
			print, ' Deleted ', fim
		endif
	endfor
	return
end

;-----------------------------------------------------------------

pro load_batch_sort, pstate, F

; Read the batch from 'F'

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
if n_elements(geopixe_root) lt 1 then geopixe_root=''

	if n_params() lt 2 then begin
		print,'load_batch_sort: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return

	on_ioerror, bad_io
	close, 1
	openr, 1, F, /XDR

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	pbatch = (*pstate).pbatch
	if ptr_valid(pbatch) eq 0 then return

	if size(*pbatch,/tname) eq 'POINTER' then begin
		for i=0L,n_elements(*pbatch)-1 do begin
			if ptr_valid( (*pbatch)[i] ) then ptr_free, (*pbatch)[i]
		endfor
	endif

cont:
	valid_versions = [-1,-2]

	version = 0L
	readu,1, version
	q = where( version eq valid_versions)
	if q[0] eq -1 then goto, bad_version

	n = 0
	readu,1, n
	if (n le 0) or (n gt 10000) then goto, bad_io

	dir = ''
	readu,1, dir
	(*pstate).dir = dir
	widget_control, (*pstate).dir_text, set_value=(*pstate).dir

	p = ptrarr(n)
	t1 = { file:'', xrange:0,yrange:0, xsize:0.0,ysize:0.0, charge:0.0, comment:''}
	t2 = { file:'', output:'', xrange:0,yrange:0, xsize:0.0,ysize:0.0, charge:0.0, comment:''}

	t = { file:'', output:'', suppress:0, enable:1, xrange:0,yrange:0, xsize:0.0,ysize:0.0, charge:0.0, comment:''}

	for i=0L,n-1 do begin
		if version eq -1 then begin
			readu,1, t1
			t.file = t1.file
			t.output = strip_path(strip_file_ext(t.file))
			t.xrange = t1.xrange
			t.yrange = t1.yrange
			t.xsize = t1.xsize
			t.ysize = t1.ysize
			t.charge = t1.charge
			t.comment = t1.comment	
		endif else if version eq -2 then begin
			readu,1, t2
			t.file = t2.file
			t.output = t2.output
			t.xrange = t2.xrange
			t.yrange = t2.yrange
			t.xsize = t2.xsize
			t.ysize = t2.ysize
			t.charge = t2.charge
			t.comment = t2.comment	
		endif
		t.suppress = (i ne 0)
		p[i] = ptr_new(t)
	endfor
	*pbatch = p

	*(*pstate).path = build_output_path( (*p[0]).file, (*p[0]).output, (*pstate).root, /set)

	readu,1, n
	opts = intarr(n)
	readu,1, opts
	n1 = n_elements((*pstate).options)
	n2 = min([n,n1])
	(*pstate).options[*] = 0
	(*pstate).options[0:n2-1] = opts[0:n2-1]
	widget_control, (*pstate).options_id, set_value=(*pstate).options

	close,1
	return

bad_io:
	warning,'load_batch_sort','Error reading batch file',/error
	return
bad_ptr:
	warning,'load_batch_sort','Bad initial batch pointer',/error
	return
bad_version:
	warning, 'load_batch_sort', 'Bad file version',/error
	return
end

;-----------------------------------------------------------------

pro save_batch_sort, pstate, F

; Write the batch to 'F'

	COMPILE_OPT STRICTARR
	if n_params() lt 2 then begin
		print,'save_batch_sort: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return
	no_batch = 0
	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	pbatch = (*pstate).pbatch
	if ptr_valid(pbatch) eq 0 then goto, bad_ptr
	if size(*pbatch,/tname) ne 'POINTER' then begin
		no_batch = 1
	endif else begin
		if ptr_valid( (*pbatch)[0] ) eq 0 then no_batch=1
		if no_batch eq 0 then if size(*(*pbatch)[0],/tname) ne 'STRUCT' then no_batch=1
	endelse
	if no_batch then goto, bad_data

	n = n_elements(*pbatch)
	if n eq 0 then goto, bad_data

	version = -2L									; version number
	on_ioerror, bad_io
	close, 1
	openw, 1, F, /XDR

	writeu,1, version
	writeu,1, n
	writeu,1, (*pstate).dir

	for i=0L,n-1 do begin
		p = (*pbatch)[i]

		t = { file:'', output:'', xrange:0,yrange:0, xsize:0.0,ysize:0.0, charge:0.0, comment:''}
		t.file = (*p).file
		t.output = (*p).output
		t.xrange = (*p).xrange
		t.yrange = (*p).yrange
		t.xsize = (*p).xsize
		t.ysize = (*p).ysize
		t.charge = (*p).charge
		t.comment = (*p).comment	
		
		writeu,1, t
	endfor

	writeu,1, n_elements((*pstate).options)
	writeu,1, (*pstate).options

	close,1
	return

bad_io:
	warning,'save_batch_sort','Error writing batch file',/error
	return
bad_ptr:
	warning,'save_batch_sort','bad batch pointer',/error
	return
bad_data:
	warning,'save_batch_sort','bad batch data structure',/error
	return
end

;-----------------------------------------------------------------

pro OnRealize_batch_Table, wWidget

COMPILE_OPT STRICTARR
top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

w = widget_info( wWidget, /row_heights)
geom = widget_info( wWidget, /geometry)
tlb_geom = widget_info( top, /geometry)

(*pstate).table = wWidget
(*pstate).row_height = w[0]
(*pstate).rows = 10

update_batch_sort_table, pstate

done:
end

;------------------------------------------------------------------------------------------

pro update_batch_sort_table, pstate, event

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
		warning,'update_batch_sort_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

pbatch = (*pstate).pbatch
if ptr_valid(pbatch) eq 0 then goto, bad
if size(*pbatch,/tname) ne 'POINTER' then goto, bad
if ptr_valid( (*pbatch)[0] ) eq 0 then goto, bad
if size(*(*pbatch)[0],/tname) ne 'STRUCT' then goto, bad

columns = ['File','X range','Y range','X size','Y size','Charge','Comment','Output']
widths = [20,8,8,8,8,9,16,20]
nc = 8
n = n_elements(*pbatch)
max_widths = intarr(nc)

t = strarr(nc,n)
rows = strarr(n)

for i=0L,n-1 do begin
	p = (*pbatch)[i]
	t[0,i] = strtrim( (*p).file,2)
	t[1,i] = str_tidy((*p).xrange)
	t[2,i] = str_tidy((*p).yrange)
	t[3,i] = str_tidy( (*p).xsize)
	t[4,i] = str_tidy( (*p).ysize)
	t[5,i] = str_tidy( (*p).charge)
	t[6,i] = strtrim( (*p).comment,2)
	t[7,i] = strtrim( (*p).output,2)
	
	rows[i] = str_tidy(i)
	if (*p).enable then begin
		rows[i] = '* ' + rows[i] + ' on'
	endif
	if (*p).suppress eq 0 then begin
		rows[i] = rows[i] + ' First'
	endif
endfor
for j=0L,7 do begin
	max_widths[j] = max( strlen( t[j,0:n-1]) ) < 40
endfor
widths = (max_widths > widths) * !d.x_ch_size

if ptr_valid( (*pstate).headings) then ptr_free, (*pstate).headings
(*pstate).headings = ptr_new(columns)
(*pstate).columns = nc
(*pstate).rows = n

widget_control, (*pstate).table, set_value = t, column_widths=widths, $
			row_labels = rows, column_labels=columns, $
			table_xsize=(*pstate).columns, table_ysize=n, align=2
;			use_table_select=[0,0,nc-1,n-1]
goto, done

bad:
	t = strarr(8,16)
	widget_control, (*pstate).table, set_value = t, $
			row_labels = '', column_labels='', $
			table_xsize=8, table_ysize=16, align=2
	ns = 0
done:
	return
	end

;------------------------------------------------------------------------------------------

pro batch_sort, group_leader=group, TLB=tlb, pbatch=pbatch, path=path, dpath=dpath, $
				_extra=extra, xoffset=xoffset, yoffset=yoffset, device=obji, $
				mode=mode, type=type, root=proot

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
if n_elements(geopixe_root) lt 1 then startupp

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
		warning,'batch_sort',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''
if n_elements(proot) lt 1 then begin
	proot = ptr_new({strip:0, path:''})
	path = build_output_path( dpath, path, proot, /set)
endif
if n_elements(dpath) lt 1 then dpath=path
if n_elements(mode) lt 1 then mode=0
if n_elements(type) lt 1 then type=0
define_devices
if n_elements(obji) lt 1 then begin
	obj = obj_new('MAIA_DEVICE')
endif else begin
	obj = clone_device_object(obji)
endelse
tracking = 1
print,'Batch sort mode=',mode

w = 0
h = 0
xoff = 0
yoff = 0
if widget_info( Group, /valid) then begin
	geom = widget_info( Group, /geometry)
	w = geom.scr_xsize
	h = geom.scr_ysize
	xoff = geom.xoffset
	yoff = geom.yoffset
endif
screen = get_screen_size()
if n_elements(xoffset) lt 1 then begin
	xoffset = ((xoff + w) < (screen[0]-34 - 562)) > 0
endif
if n_elements(yoffset) lt 1 then begin
	yoffset = ((yoff) < (screen[1]-28 - 200)) > 0
endif

pbatch = bad_pars_struct( pbatch, make_pars=no_batch)

; 	top-level base

tlb = widget_base( /column, title='Batch Sort EVT', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, _extra=extra, uname='batch_sort_TLB', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2 ,xoffset=xoffset, yoffset=yoffset, $
					/base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

sbase = widget_base( tbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( sbase, value='Set-up:')
setup_text = widget_text( sbase, value='', uname='batch-setup-file', tracking=tracking, /editable, $
					uvalue='Enter a SBATCH file-name for batch source set-up details, or click on the "Load" button on the right.',scr_xsize=450)
load_setup_button = widget_button( sbase, value='Load', uname='load-setup-button', tracking=tracking, $
					uvalue='Load batch set-up parameters from a previous SBATCH file.', scr_xsize=38)
save_setup_button = widget_button( sbase, value='Save', uname='save-setup-button', tracking=tracking, $
					uvalue='Save batch set-up parameters to a SBATCH file.', scr_xsize=38)

b1base = widget_base( tbase, /row, /base_align_center, xpad = 0, ypad=0, space=5)
dir_button = widget_button( b1base, value='Dir:', uname='dir-button', tracking=tracking, scr_xsize=35, $
					uvalue='Select the directory tree to scan for list-mode files to register in the batch table. '+ $
					'This assumes the Device set on the Sort EVT window. Hence use "From DAI" to load a successful image there first.')
dir_text = widget_text( b1base, value='', uname='dir-text', tracking=tracking, /editable, $
					uvalue='Enter a directory tree to scan for list-mode files to register in the batch table, or use the "Dir" button on the left. Hit <return> to scan the directory tree. '+ $
					'This assumes the Device set on the Sort EVT window. Hence use "From DAI" to load a successful image there first.', scr_xsize=468)
build_tree_button = widget_button( b1base, value='Build Tree', uname='build-tree-button', tracking=tracking, $
					uvalue='Build a directory tree, rooted in the directory at left, and disperse all list-mode and parameter files into sub-directories in this tree. '+ $
					'Then rescan this tree for list-mode files and reload the table.', scr_xsize=76, sensitive=0)

; Table

t = strarr(16,256)

table = Widget_Table( tbase, UNAME='batch-table', /all_events, /editable, value=t, Notify_Realize='OnRealize_batch_Table',  $
			scr_xsize=830, Y_SCROLL_SIZE=10, /RESIZEABLE_COLUMNS, alignment=2, tracking=tracking, uvalue='Edit values for XY range and size (microns) ' + $
			'and Charge. If flux is available in the evt data, and the same conversion from flux to charge applies to all data files, then only enter "Charge" ' + $
			'for the first run. In this case, a pop-up requester will show total flux and prompt for charge, calculated using the existing conversion. Enter a new charge ' + $
			'to establish a new conversion. The other data-sets will have charge calculated using total flux and the same conversion factor.')


options = [0,0,0,0,0,1,0,0,0,0,0,0,0]
options_id = cw_bgroup2( tbase, ['Apply digital filters to images','Overwrite original DAI file','Save images as colour PNG to HTML', $
							'Save images as B/W PNG to HTML','Export images as Tab delimited text','Skip sort if original DAI already exists', $
							'Apply a CorrectX scaling file','Mirror X (odd images only)','Save images as TIFF concentration', $
							'Save images as TIFF counts','Save images as TIFF ng/cm**2','Save selected RGB images','Save METADATA for each image'], $
			column=3, xpad=0, ypad=0, space=0, /return_index, tracking=tracking, $
			uname='options', set_value=options, /nonexclusive, $
			uvalue=['Enable the application of digital filters to the images, based on a prior DAI file used as a template. A requester will prompt for this file after the first sort in the table. Then all filter operations used on each element in the template will be applied.', $
					'Overwrite the initial DAI file with the digital filtered images, rather than write a separate "-m.DAI" or "-x.DAI" file. This saves disk space, but it may be better to avoid this to ensure that the modified image data is stored in a different file.', $
					'Save colour images in PNG format, and build a summary HTML file using thumb-nail versions of the images. You will be prompted for an element list after the first sort in the table.', $
					'Save black and white images in PNG format, and build a summary HTML file using thumb-nail versions of the images. You will be prompted for an element list after the first sort in the table.', $
					'Export selected element images as tab delimited text files. You will be prompted for an element list after the first sort in the table.', $
					'Skip the sorting of the raw data to DAI image file if a DAI file already exists for a particular run. Use this feature to apply filtering and output options to previously sorted runs.', $
					'Select an exsiting file of CorrectX corrections to scale image for variations in X current, flux, etc.', $
					'A nasty hack to flip an image in X [to fix the Epics negative width error at XFM that produces negative X and flips every second image]. Set "First" on the first image to flip in X.', $
					'Save floating point images proportional to concentration in TIFF format, and build a summary HTML file using thumb-nail versions of the images. You will be prompted for an element list after the first sort in the table.', $
					'Save floating point images proportional to counts in TIFF format, and build a summary HTML file using thumb-nail versions of the images. You will be prompted for an element list after the first sort in the table.', $
					'Save floating point images proportional to ng/cm^2 in TIFF format, and build a summary HTML file using thumb-nail versions of the images. You will be prompted for an element list after the first sort in the table.', $)
					'Save selected RGB images at full resolution (zoom=0). You will be prompted for a RGB display list filename after the first sort in the table. Use "Learn" in the RGB Image window to create this file first.', $
					'Save a METADATA JSON file of image parameters for each image.'])

; Buttons

bbase = widget_base( tbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
button = widget_button( bbase, value='Start', uname='start-button', tracking=tracking, $
					uvalue='Start sorting the table of list-mode files using the settings in the table and remaining fixed parameters in Sort EVT.')
lab = widget_label( bbase, value=' ',scr_xsize=30)
button = widget_button( bbase, value='First', uname='first-button', tracking=tracking, $
					uvalue='Select which run to do first. This one will pop-up flux requesters, image processing template, output element selections, etc. Select this one for the run for which "charge" is known.')
button = widget_button( bbase, value='Toggle Enable', uname='enable-button', tracking=tracking, $
					uvalue='Toggle between enable and disable for selected run(s). Disabled runs will be skipped.')
button = widget_button( bbase, value='Fill', uname='fill-button', tracking=tracking, $
					uvalue='Duplicate entries down a column(s) from currently selected entry, or down a selected range. Select a range by clicking and dragging a range with the mouse.')
lab = widget_label( bbase, value=' ',scr_xsize=30)
button = widget_button( bbase, value='Delete DAI', uname='delete-dai', tracking=tracking, $
					uvalue='Delete all DAI files in tree, within the optional run number range, with same name as list-mode files.')
lab = widget_label( bbase, value=' ',scr_xsize=30)
button = widget_button( bbase, value='Delete', uname='delete-button', tracking=tracking, $
					uvalue='Delete the currently selected row(s) of the table. Save the table to a SBATCH file using the "Save" button.')
button = widget_button( bbase, value='Clear', uname='clear-button', tracking=tracking, $
					uvalue='Clear the table and delete ALL batch entries in memory.')

;.................................................................................

help = widget_text( tbase, scr_xsize=780, ysize=3, /wrap, uname='help', tracking=tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

state = { $
		path:			ptr_new(path), $		; pointer to current path
		dpath:			ptr_new(dpath), $		; pointer to current data path
		root:			proot, $					; pointer to current path root
		mode:			mode, $					; projection mode
		type:			type, $					; data type
		pbatch:			pbatch, $				; pointer to array of pointers to batch
		local:			no_batch, $				; local responsibility for pbatch data
		file:			'', $					; batch file name
		dir:			'', $					; batch dir tree
		DevObj:			obj, $					; current device
		run_min:		0L, $					; minimum run #
		run_max:		-1L, $					; max run #

		current_sort:	0, $					; current sort index
		first_sort:		0, $					; first sort index
		first:			0, $					; first one to do (make sure it is enabled)
		psort:			ptr_new(/allocate_heap), $	; to store current sort pars for EVT
		pargs0:			ptr_new(/allocate_heap), $	; to pass filter arguments to Image
		pargs1:			ptr_new(/allocate_heap), $	; to pass html arguments to Image
		pargs2:			ptr_new(/allocate_heap), $	; to pass b/w html arguments to Image
		progress_tlb:	0L, $

		tracking:		tracking, $				; is tracking enabled
		options_id:		options_id, $			; options checkmark widgets
		options:		options, $				; options checkmark widgets
		setup_text:		setup_text, $			; set-up BATCH file text widget ID
		dir_text:		dir_text, $				; dir text widget ID

		table:			table, $				; table ID
		rows:			256, $					; number of rows
		columns:		50, $					; number of colums
		headings:		ptr_new(), $			; pointer to headings array
		row_height:		0, $					; table row height
		sel: {left:-1, top:-1, right:-1, bottom:-1, edit:0 }, $	; use "(*pstate).sel.top" as current region

;		cr_found:		0 }						; to fight MAC IDL bug

		cr_found:		0, $					; to fight MAC IDL bug
		help:			help $					; ID of help text
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['path', $				; new path
						'root', $				; input/output path root details struct
						'dpath', $				; new data path
						'evt-mode', $			; change of projection mode
						'evt-type', $			; change of detector type
						'device', $				; new device
						'done-evt', $			; finished current sort
						'abort-evt', $			; error in sort, abort sequence
						'done-filter', $		; finished digital filters
						'done-save' ], $		; finished image/HTML save
						from=group

xmanager, 'batch_sort', tlb, /no_block


return
end
