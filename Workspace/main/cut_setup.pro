;
;	cut Results table.

pro cut_results_event, event

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
		warning,'cut_results_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

no_cuts = 0
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
p = (*pstate).pcut
if ptr_valid(p) eq 0 then goto, bad_ptr
if size(*p,/tname) ne 'POINTER' then begin
	no_cuts = 1
endif else begin
	if ptr_valid( (*p)[0] ) eq 0 then no_cuts=1
	if no_cuts eq 0 then if size(*(*p)[0],/tname) ne 'STRUCT' then no_cuts=1
endelse

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
				;print,'cut Setup: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'cut-results': begin
				print,'cut_Results_Event: new cut results notified.'
				if no_cuts then goto, finish

				i = (*pstate).sel.top
				if (i ge 0) and (i lt n_elements(*p)) then begin
					widget_control, (*pstate).name_text, get_value=s
					(*(*(*pstate).pcut)[i]).el = s
				endif

				ns = n_elements( *p) - 1
				update_cut_table, pstate, event, ns
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
		print,'Kill request cut_results ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'results-table': begin
		case tag_names( event, /structure_name) of

			'WIDGET_TABLE_CELL_SEL': begin
				i = (*pstate).sel.top
				if (i ge 0) and ((*pstate).sel.left eq 0) and ((*pstate).sel.right eq (*pstate).columns-1) then begin
					widget_control, (*pstate).name_text, get_value=s
					if ptr_valid( (*(*pstate).pcut)[i]) then (*(*(*pstate).pcut)[i]).el = s
				endif

				(*pstate).sel.left = event.sel_left
				(*pstate).sel.top = event.sel_top
				(*pstate).sel.right = event.sel_right
				(*pstate).sel.bottom = event.sel_bottom
				*(*pstate).pselect = (*pstate).sel
		;		print,'left,right,top,bottom,columns=',(*pstate).sel,(*pstate).columns

				if ((*pstate).sel.left eq 0) and ((*pstate).sel.right eq (*pstate).columns-1) then begin
					update_cut_table, pstate, event
				endif
				if (*pstate).sel.top ge 0 then begin
					notify, 'cut-select', (*pstate).pselect, from=event.top
				endif
				end

			'WIDGET_TABLE_CH': begin
				end
			else:
		endcase
		end

	'cut_results_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				x = event.x - (*pstate).xoffset
				n = ((event.y - (*pstate).yoffset + 9)/ (*pstate).row_height) - 2
				y = (n + 2) * (*pstate).row_height
				widget_control, (*pstate).table, scr_xsize=x, scr_ysize=y
				end
			else:
		endcase
		end

	'name-text': begin
		i = (*pstate).sel.top
		if i ge 0 then begin
			widget_control, (*pstate).name_text, get_value=s
			(*(*(*pstate).pcut)[i]).el = s
			update_cut_table, pstate, event
		endif
		end

	'load-button': begin
		path = *(*pstate).path
		F = file_requester( /read, filter = '*.cuts', $
				/must_exist, path=path, group=event.top, $
				title='Select spectrum Cuts from a CUTS file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.cuts'
			*(*pstate).path = extract_path(F)
			print,'Load cut results from ',F
			load_cut_results, pstate, F

			update_cut_table, pstate, event
			(*pstate).sel.top = -1
			(*pstate).sel.bottom = -1
			*(*pstate).pselect = (*pstate).sel
			notify, 'cut-select', (*pstate).pselect, from=event.top
		endif
		end

	'save-button': begin
		if no_cuts then goto, finish
		i = (*pstate).sel.top
		if i ge 0 then begin
			widget_control, (*pstate).name_text, get_value=s
			(*(*(*pstate).pcut)[i]).el = s
		endif

		path = *(*pstate).path
		F = file_requester( /write, filter = '*.cuts', $
				path=path, group=event.top, $
				title='Save the Cuts definitions to a CUTS file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.cuts'
			*(*pstate).path = extract_path(F)
			print,'save cut results to ',F
			save_cut_results, pstate, F
		endif
		end

	'delete-button': begin
		if no_cuts then goto, finish
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
		update_cut_table, pstate, event, ns
		end

	'clear-button': begin
		if no_cuts then goto, finish
		for i=0L,n_elements(*p)-1 do begin
			if ptr_valid( (*p)[i]) then ptr_free, (*p)[i]
		endfor
		*p = ptr_new()
		update_cut_table, pstate, event
		(*pstate).sel.top = -1
		(*pstate).sel.bottom = -1
		*(*pstate).pselect = (*pstate).sel
		notify, 'cut-select', (*pstate).pselect, from=event.top
		end

	'close-button': begin
		print,'Close cut results ...'
		goto, kill
		end
	else:
endcase

finish:
	widget_control, hourglass=0
	close, 1
	return

bad_state:
	warning,'cut_results_event',['pstate variable has become ill-defined.','Abort cut Results.'],/error
	goto, kill
bad_ptr:
	warning,'cut_results_event',['Parameter structure variable has become ill-defined.','Abort cut Results.'],/error
	goto, kill

kill:
	cancel_notify, event.top

	if n_elements(pstate) lt 1 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).headings) then ptr_free, (*pstate).headings

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;-----------------------------------------------------------------

pro load_cut_results, pstate, F

; Read the cuts from 'F'

	if n_params() lt 2 then begin
		print,'load_cut_results: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return
	if n_elements(pstate) lt 1 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return

	p = read_cuts(F, error=error)
	if error then goto, bad_io

	no_cuts = 0
	pcut = (*pstate).pcut
	if ptr_valid(pcut) eq 0 then goto, bad_ptr
	if size(*pcut,/tname) ne 'POINTER' then begin
		no_cuts = 1
	endif else begin
		if ptr_valid( (*pcut)[0] ) eq 0 then no_cuts=1
		if no_cuts eq 0 then if size(*(*pcut)[0],/tname) ne 'STRUCT' then no_cuts=1
	endelse

	n = n_elements(*p)
	for i=0L,n-1 do begin
		if no_cuts then begin
			*pcut = ptr_new( (*p)[i])
			no_cuts = 0
		endif else begin
			*pcut = [*pcut, ptr_new( (*p)[i])]
		endelse
	endfor
	ptr_free, p
	return

bad_io:
	warning,'load_cut_results','Error reading cuts file',/error
	return
bad_ptr:
	warning,'load_cut_results','Bad initial cuts pointer',/error
	return
end

;-----------------------------------------------------------------

pro save_cut_results, pstate, F

; Write the results to 'F'

	if n_params() lt 2 then begin
		print,'save_cut_results: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return
	no_cuts = 0
	if n_elements(pstate) lt 1 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return

	pcut = (*pstate).pcut
	if ptr_valid(pcut) eq 0 then goto, bad_ptr
	if size(*pcut,/tname) ne 'POINTER' then begin
		no_cuts = 1
	endif else begin
		if ptr_valid( (*pcut)[0] ) eq 0 then no_cuts=1
		if no_cuts eq 0 then if size(*(*pcut)[0],/tname) ne 'STRUCT' then no_cuts=1
	endelse
	if no_cuts then goto, bad_data

	n = n_elements( *pcut)
	cuts = replicate( *(*pcut)[0], n)
	for i=0L,n-1 do begin
		cuts[i] = *(*pcut)[i]
	endfor
	p = ptr_new( cuts, /no_copy)

	write_cuts, p, F
	return

bad_ptr:
	warning,'save_cut_results','bad results pointer',/error
	return
bad_data:
	warning,'save_cut_results','bad results data structure',/error
	return
end

;-----------------------------------------------------------------

pro OnRealize_cut_table, wWidget

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

update_cut_table, pstate

done:
end

;------------------------------------------------------------------------------------------

pro update_cut_table, pstate, event, ns

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
		warning,'update_cut_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

nc = 17
if n_elements(ns) lt 1 then ns=(*pstate).sel.top

pcut = (*pstate).pcut
if ptr_valid(pcut) eq 0 then goto, bad
if size(*pcut,/tname) ne 'POINTER' then goto, bad
if ptr_valid( (*pcut)[0] ) eq 0 then goto, bad
if size(*(*pcut)[0],/tname) ne 'STRUCT' then goto, bad

n = n_elements(*pcut)
no_cuts = 0

all = 0
for i=0L,n-1 do begin
	p = (*pcut)[i]
	if (*p).type ge 1 then all=1
endfor
if all then begin
	nc = 17
	columns = ['Name','Type','X0','X1','C0, X2','C1, X3','X4','X5','Units','Area', $
		'Error','Total','Back','Left','Right','Cal A','Cal B']
	widths = [6,6,8,8,8,8,8,8,7,8,8,8,8,8,8,8,8]* !d.x_ch_size
endif else begin
	nc = 13
	columns = ['Name','Type','C0','C1','Units','Area', $
		'Error','Total','Back','Left','Right','Cal A','Cal B']
	widths = [9,6,8,8,7,8,8,8,8,8,8,8,8]* !d.x_ch_size
endelse
types = ['Cut 0,1','X0-X5','Old']

t = strarr(nc,n)

off = 0
for i=0L,n-1 do begin
	p = (*pcut)[i]

	t[0,i] = (*p).el
	t[1,i] = types[(*p).type]
	if all then begin
		t[2,i] = ((*p).x[0] le 0) ? '' : string((*p).e[0])
		t[3,i] = ((*p).x[1] le 0) ? '' : string((*p).e[1])
	endif else begin
		off = -2
	endelse
	t[4+off,i] = string((*p).e[2])
	t[5+off,i] = string((*p).e[3])
	if all then begin
		t[6,i] = ((*p).x[4] le 0) ? '' : string((*p).e[4])
		t[7,i] = ((*p).x[5] le 0) ? '' : string((*p).e[5])
	endif else begin
		off = -4
	endelse
	t[8+off,i] = (*p).units
	t[9+off,i] = (abs((*p).area) lt 0.01) ? '' : string((*p).area)
	t[10+off,i] = (abs((*p).error) lt 0.01) ? '' : string((*p).error)
	t[11+off,i] = (abs((*p).sum) lt 0.01) ? '' : string((*p).sum)
	t[12+off,i] = (abs((*p).back) lt 0.01) ? '' : string((*p).back)
	t[13+off,i] = (abs((*p).left) lt 0.01) ? '' : string((*p).left)
	t[14+off,i] = (abs((*p).right) lt 0.01) ? '' : string((*p).right)
	t[15+off,i] = (abs((*p).cal_a-1.0) lt 0.001) ? '' : string((*p).cal_a)
	t[16+off,i] = (abs((*p).cal_b) lt 0.0001) ? '' : string((*p).cal_b)
endfor

if ptr_valid( (*pstate).headings) then ptr_free, (*pstate).headings
(*pstate).headings = ptr_new(columns)
(*pstate).columns = nc
(*pstate).rows = n
rows = string( indgen(n))

widget_control, (*pstate).table, set_value = t, column_widths=widths, $
			row_labels = rows, column_labels=columns, $
			table_xsize=(*pstate).columns, table_ysize=n, align=2
;			use_table_select=[0,0,nc-1,n-1]
goto, done

bad:
	t = strarr(17,16)
	widget_control, (*pstate).table, set_value = t, $
			row_labels = '', column_labels='', $
			table_xsize=17, table_ysize=16, align=2
	ns = 0
	no_cuts = 1
done:
	if ns ge 0 then begin
		old_ns = (*pstate).sel.top
		widget_control, (*pstate).table, set_table_select=[0,ns,nc-1,ns]
		(*pstate).sel.top = ns
		(*pstate).sel.bottom = ns
		*(*pstate).pselect = (*pstate).sel
		if (n_elements(event) gt 0) and (ns ne old_ns) then begin
			notify, 'cut-select', (*pstate).pselect, from=event.top
		endif
	endif

	i = (*pstate).sel.top
	if (i ge 0) and (no_cuts eq 0) then begin
		widget_control, (*pstate).name_text, set_value=(*(*(*pstate).pcut)[i]).el
	endif
	return
	end

;------------------------------------------------------------------------------------------

pro cut_setup, group_leader=group, TLB=tlb, pcut=pcut, path=path, $
				_extra=extra, xoffset=xoffset, yoffset=yoffset

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
		warning,'cut_Results',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''

case !version.os_family of
	'MacOS': begin
		yw = 220
		end
	'unix': begin
		yw = 249
		end
	else: begin
		yw = 219
		end
endcase

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
	xoffset = ((xoff + w - 250) < (screen[0]-34 - 562)) > 0
endif
if n_elements(yoffset) lt 1 then begin
	yoffset = ((yoff - yw) < (screen[1]-28 - 200)) > 0
endif

if n_elements(player) lt 1 then player = ptr_new(/allocate_heap)

pcut = bad_pars_struct( pcut, make_pars=no_cuts)

; 	top-level base

tracking = 0

tlb = widget_base( /column, title='Cuts Setup', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, _extra=extra, uname='cut_results_TLB', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2 ,xoffset=xoffset, yoffset=yoffset, $
					/base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

; Table

t = strarr(17,256)

table = Widget_Table( tbase, UNAME='results-table', /all_events, value=t, Notify_Realize='OnRealize_cut_table',  $
			X_SCROLL_SIZE=7, Y_SCROLL_SIZE=6, /RESIZEABLE_COLUMNS, alignment=2 )

; Buttons

bbase = widget_base( tbase, /row, /base_align_center, xpad = 0, ypad=0, space=5)
lab = Widget_Label( bbase, VALUE='Name:')
name = ''
if no_cuts eq 0 then name = (*(*pcut)[0]).el
name_text = Widget_Text(bbase, UNAME='name-text', /EDITABLE, VALUE=name ,XSIZE=20 ,YSIZE=1, $
		tracking=tracking, uvalue='Name of currently select Cut. To select a Cut, click in the row. Enter a new name and hit <return>.')

lab = widget_label( bbase, value='   ')
button = widget_button( bbase, value='Load', uname='load-button', tracking=tracking, $
					uvalue='Load the table with previous results from a PIXE cut Results .PFR file.')
button = widget_button( bbase, value='Save', uname='save-button', tracking=tracking, $
					uvalue='Save all the current results displayed in the table to a PIXE cut Results .PFR file.')
lab = widget_label( bbase, value='   ')
button = widget_button( bbase, value='Delete', uname='delete-button', tracking=tracking, $
					uvalue='Delete the currently selected range of results. Select a range by clicking and dragging a range with the mouse.')
button = widget_button( bbase, value='Clear', uname='clear-button', tracking=tracking, $
					uvalue='Clear the table and delete ALL results in memory.')

;.................................................................................

;help = widget_text( tbase, scr_xsize=380, ysize=3, /wrap, uname='help', tracking=tracking, $
;				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
;				frame=0)

state = { $
		path:			ptr_new(path), $		; pointer to current path
		pcut:			pcut, $					; pointer to array of pointers to results

		tracking:		tracking, $				; is tracking enabled

		table:			table, $				; table ID
		rows:			256, $					; number of rows
		columns:		17, $					; number of colums
		headings:		ptr_new(), $			; pointer to headings array
		row_height:		0, $					; table row height
		xoffset:		0, $					; offset in xsize for resize
		yoffset:		0, $					; offset in ysize for resize
		name_text:		name_text, $			; ID of name text widget
		sel: {left:-1, top:-1, right:-1, bottom:-1 }, $	; use "(*pstate).sel.top" as current region
		pselect:		ptr_new(), $				; pointer to select for notify to spectrum_display
		cr_found:		0 }						; to fight MAC IDL bug

;		cr_found:		0, $					; to fight MAC IDL bug
;		help:			help $					; ID of help text
;	}

state.pselect = ptr_new( state.sel)

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['path', $				; new path
						'cut-results' ], $		; new cut results
						from=group

xmanager, 'cut_results', tlb, /no_block

return
end
