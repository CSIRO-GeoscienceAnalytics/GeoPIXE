;
; IDL Event Callback Procedures
; spectrum_select_eventcb
;
; Generated on:	06/30/99 14:45.20
;
;-----------------------------------------------------------------

function str_show, pstate, i

COMPILE_OPT STRICTARR

s = str_tidy(i)
if ptr_valid( (*(*pstate).p)[i]) eq 0 then return,s

if (*(*pstate).pshow)[i] eq 1 then begin
	s = s + '  *'
	if (*(*(*pstate).p)[i]).showfit eq 1 then begin
		s = s + 'F'
	endif
endif

return, s
end

;-----------------------------------------------------------------

pro load_select_table, pstate, init=init

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
		warning,'Load_select_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if ptr_valid( (*pstate).p) eq 0 then return
if n_elements(init) lt 1 then init=0
if ptr_valid( (*(*pstate).p)[0]) then begin

case !version.os_family of
	'MacOS': begin
		ytrim = 25
		headn = 1
		end
	'unix': begin
		ytrim = 35
		headn = 1.7
		end
	else: begin
		ytrim = 22
		headn = 1
		end
endcase

	n = n_elements( *(*pstate).p)

;	This is the table organization:
;
;	Remember that the order is used in routine "select_update_cel";
;	any changes here must be reflected there too.

	s = { label:'', sample:'', grain:'', comment:'', adc:'', mult:0, charge:0.0, IC_total:0.0, conversion:0.0, $
		deadtime:1.0, size:0, cal_a:0.0, cal_b:0.0, units:'', ecompress:0, x:0.0, y:0.0, z:0.0, $
		theta:0.0, phi:0.0, scanx:0.0, scany:0.0}
	columns = ['Label','Sample','Grain','Comment','ADC','Mult','Charge','IC Count','Conversion','Rel DT Corr','Size', $
		'Cal A','Cal B','Units','Compress','X','Y','Z','Theta','Phi','Scan X','Scan Y']
	widths = [13,7,5,12,5,5,9,12,12,12,5,12,12,7,8,9,9,9,9,9,9,9,5,9,10,10,9,9,6] * !d.x_ch_size
	(*pstate).columns = 22

	rows = strarr(n)
	t = replicate( s, n)
	c = intarr(3,(*pstate).columns,n)
	c[0,*,*] = (spec_colour('white',/rgb))[0]
	c[1,*,*] = (spec_colour('white',/rgb))[1]
	c[2,*,*] = (spec_colour('white',/rgb))[2]
	j = 0
	
	for i=0L,n-1 do begin
		p = (*(*pstate).p)[i]
		t[i].label = (*p).label
		t[i].sample = (*p).sample
		t[i].grain = (*p).grain
		t[i].comment = (*p).comment
		t[i].charge = (*p).charge
		t[i].IC_total = (*p).IC_total
		t[i].conversion = (*p).IC.conversion
		t[i].deadtime = (*p).deadtime_correction
		if (*(*pstate).pshow)[i] then begin
			if (*pstate).highlight then begin
				c[*,1,i] = spec_colour('l.grey',/rgb)
			endif else begin
				c[*,1,i] = spec_colour(j,/rgb)
				j = j+1
			endelse
		endif
		if (*pstate).highlight then begin
			if ((*pstate).sel.left eq -1) and ((*pstate).sel.top eq -1) and $
						((*pstate).sel.right eq -1) then begin
				if i eq (*(*pstate).phighlight).highlight then c[*,1,i] = spec_colour('green',/rgb)
			endif
			if ((*pstate).sel.left eq 0) and ((*pstate).sel.top eq (*pstate).sel.bottom) and $
						((*pstate).sel.right eq (*pstate).columns-1) then begin
				if i eq (*pstate).sel.top then c[*,1,i] = spec_colour('green',/rgb)
			endif
		endif
		
;		if i eq 0 then warning,'load_select_table','charge='+string((*p).charge)

		if (*p).array then begin
			first = min( *(*p).pactive)
			last = max( *(*p).pactive)
			sadc = str_tidy(first+1+adc_offset_device((*p).DevObj))
			if last ne first then sadc=sadc + '...' + str_tidy(last+1+adc_offset_device((*p).DevObj))
			t[i].mult = n_elements(*(*p).pactive)
		endif else begin
			a = (*p).station + adc_offset_device((*p).DevObj)
			sadc = str_tidy(a)
			t[i].mult = (*p).multiplicity > 1				; cater for old data using multiplicity
		endelse
		t[i].size = (*p).size
		t[i].cal_a = (*p).cal.poly[1]
		t[i].cal_b = (*p).cal.poly[0]
		t[i].units = (*p).cal.units
		t[i].adc = sadc
		t[i].ecompress = (*p).ecompress

		t[i].x = (*p).x
		t[i].y = (*p).y
		t[i].z = (*p).z
		t[i].theta = (*p).theta
		t[i].phi = (*p).phi
		t[i].scanx = (*p).scan.x
		t[i].scany = (*p).scan.y

		rows[i] = str_show( pstate, i)
	endfor

	widget_control, (*pstate).table, set_value = t, $
			row_labels = rows, column_labels=columns, $
			table_xsize=(*pstate).columns, table_ysize=n, alignment=2
;			use_table_select=[0,0,(*pstate).columns-1,n-1]
	widget_control, (*pstate).table, background_color=c
	if init then begin
		widget_control, (*pstate).table, column_widths=widths
	endif
	(*pstate).display_rows = n < (*pstate).display_rows
;	if !version.os_family ne 'MacOS' then begin
;		widget_control, (*pstate).table, scr_ysize=((*pstate).display_rows+headn)*(*pstate).row_height+ytrim
;	endif
		(*pstate).rows = n
endif
end

;--------------------------------------------------------------------

; Select just spectrum 'n'.
; If /keep_less, then don't alter selection of spectra < 'n'

pro select_one_spectrum, pstate, n, keep_less=keep

COMPILE_OPT STRICTARR
if (n lt 0) or ( n ge n_elements(*(*pstate).p)) then return
if n_elements(keep) lt 1 then keep=0

active = first_active(pstate)
if ptr_valid( (*(*pstate).p)[active]) eq 0 then return

elow = (*(*(*pstate).p)[active]).elow
ehigh = (*(*(*pstate).p)[active]).ehigh
cal_a = (*(*(*pstate).p)[active]).cal.poly[1]
cal_b = (*(*(*pstate).p)[active]).cal.poly[0]

active = n
(*pstate).sel.top = n
(*pstate).sel.bottom = n
np = n_elements(*(*pstate).p)
widget_control, (*pstate).table, set_table_select=[0,0,(*pstate).columns-1,np-1]
widget_control, (*pstate).table, set_table_select=[0,n,(*pstate).columns-1,n]

if (abs(cal_a - (*(*(*pstate).p)[active]).cal.poly[1]) lt 0.001) and $
   (abs(cal_b - (*(*(*pstate).p)[active]).cal.poly[0]) lt 0.1) then begin
		(*(*(*pstate).p)[active]).elow = elow
		(*(*(*pstate).p)[active]).ehigh = ehigh
endif

for i=n_elements(*(*pstate).p)-1,0,-1 do begin
	(*(*pstate).pshow)[i] = ((i eq active) ? 1 : 0)
	if (i eq active) and keep then return
endfor

return
end

;-----------------------------------------------------------------

pro update_headings, pstate, n

COMPILE_OPT STRICTARR
np = n_elements( *(*pstate).p)
rows = strarr(np)
for i=0L,np-1 do begin
	rows[i] = str_show( pstate, i)
endfor
widget_control, (*pstate).table, row_labels= rows
widget_control, (*pstate).table, set_table_select=[0,0,(*pstate).columns-1,np-1]
widget_control, (*pstate).table, set_table_select=[0,n,(*pstate).columns-1,n]
(*pstate).sel.top = n
(*pstate).sel.bottom = n
return
end

;-----------------------------------------------------------------
pro spectrum_select_update_colours, pstate

COMPILE_OPT STRICTARR

	if ptr_good( pstate,/struct) eq 0 then return
	if ptr_good( (*pstate).p,/struct) eq 0 then return
	
	n = n_elements( *(*pstate).p)
	c = intarr(3,(*pstate).columns,n)
	c[0,*,*] = (spec_colour('white',/rgb))[0]
	c[1,*,*] = (spec_colour('white',/rgb))[1]
	c[2,*,*] = (spec_colour('white',/rgb))[2]

	j = 0
	for i=0L,n-1 do begin
		if (*(*pstate).pshow)[i] then begin
			if (*pstate).highlight then begin
				c[*,1,i] = spec_colour('l.grey',/rgb)
			endif else begin
				c[*,1,i] = spec_colour(j,/rgb)
				j = j+1
			endelse
		endif
		if (*pstate).highlight then begin
			if ((*pstate).sel.left eq 0) and ((*pstate).sel.top eq (*pstate).sel.bottom) and $
						((*pstate).sel.right eq (*pstate).columns-1) then begin
				if i eq (*pstate).sel.top then c[*,1,i] = spec_colour('green',/rgb)
			endif else begin
				if i eq (*(*pstate).phighlight).highlight then c[*,1,i] = spec_colour('green',/rgb)
			endelse
		endif
	endfor
	
	widget_control, (*pstate).table,  background_color=c
	return
end

;-----------------------------------------------------------------

function toggle, n

return, (n) ? 0 : 1
end

;-----------------------------------------------------------------

pro OnDestroy_Select, wWidget

	return
end

;-----------------------------------------------------------------

pro OnKill_Select, Event

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
		warning,'OnKill_Select',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	cancel_notify, event.top

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return

	if ptr_good( (*pstate).parray) then begin
		p = (*pstate).parray
		if ptr_good(p) then begin
			if ptr_valid( (*p).detector_list) then ptr_free, (*p).detector_list
			if ptr_valid( (*p).pdetector) then ptr_free, (*p).pdetector
			if ptr_valid( (*p).playout) then ptr_free, (*p).playout
			if ptr_valid( (*p).pselect) then ptr_free, (*p).pselect
			if ptr_valid( (*p).pactive) then ptr_free, (*p).pactive
			if ptr_valid( (*p).pshow) then ptr_free, (*p).pshow
			ptr_free, p
		endif
	endif

	if ptr_valid( (*pstate).matrix) then ptr_free, (*pstate).matrix
	if ptr_valid( (*pstate).phighlight) then ptr_free, (*pstate).phighlight
	if ptr_valid( (*pstate).pseed) then ptr_free, (*pstate).pseed
	if ptr_valid((*pstate).pwiz) then ptr_free, (*pstate).pwiz
	if ptr_valid( (*pstate).pselect) then ptr_free, (*pstate).pselect

	widget_control, event.top, /destroy
	return
end

;-----------------------------------------------------------------
; NOTIFY Callback Procedure.
;
;   {NOTIFY, ID:0L, TOP:0L, HANDLER:0L, TAG:t, POINTER:p, FROM:f }
;
;	TAG	string showing the notification name, as registered.
;	POINTER	pointer passed as a general argument (can be null).
;	FROM	id of widget sending the notify (or zero).
;-----------------------------------------------------------------

pro OnNotify_Select, Event

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
		warning,'OnNotify_select',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, done
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

;print,'spectrum_select:  ', event.tag, '  '

case event.tag of
	'spectra-changed': begin
		(*pstate).p = event.pointer
		load_select_table, pstate
		if ptr_valid( (*pstate).matrix) then ptr_free, (*pstate).matrix
		(*pstate).matrix_file = ''
		end

	'image-region-select': begin
		if ptr_valid( event.pointer) eq 0 then goto, done
		if ptr_valid((*pstate).p) eq 0 then goto, done
		if ptr_valid((*(*pstate).p)[0]) eq 0 then goto, done
		if (*(*(*pstate).p)[0]).type eq 1 then goto, done			; not for traverses
		n = (*event.pointer).sel.top
		if (n lt 0) or (n ge n_elements(*(*pstate).p)) then goto, done
		select_one_spectrum, pstate, n
		update_headings, pstate, n
		end

;	'image-region-delete': begin
;		if ptr_good( event.pointer) eq 0 then goto, done
;		if ptr_valid((*pstate).p) eq 0 then goto, done
;		if ptr_valid((*(*pstate).p)[0]) eq 0 then goto, done
;		if (*(*(*pstate).p)[0]).type eq 1 then goto, done			; not for traverses
;		OnButton_Delete_Spec_Select, Event, select=*event.pointer, /keep_zero
;		end

	'select-update': begin
		if ptr_valid((*pstate).p) eq 0 then goto, done
		if ptr_valid((*(*pstate).p)[0]) eq 0 then goto, done
		n = (*event.pointer)
		if (n lt 0) or (n ge n_elements(*(*pstate).p)) then goto, done
		widget_control, (*pstate).table, set_table_select=[0,n,(*pstate).columns-1,n]
;		(*pstate).highlight = n
		update_headings, pstate, n
		spectrum_select_update_colours, pstate
		end

	'select-highlight': begin
		if ptr_valid((*pstate).p) eq 0 then goto, done
		if ptr_valid((*(*pstate).p)[0]) eq 0 then goto, done
		n = (*event.pointer)
		if (n lt 0) or (n ge n_elements(*(*pstate).p)) then goto, done
		widget_control, (*pstate).table, set_table_select=[0,n,(*pstate).columns-1,n]
		if (*pstate).highlight then begin
			(*(*pstate).phighlight).on = 1
			(*(*pstate).phighlight).highlight = n
			update_headings, pstate, n
			spectrum_select_update_colours, pstate
		endif
		end

	'array-select': begin
;		print, *event.pointer
		p = *(*pstate).p
		np = n_elements(p)
		(*(*pstate).pshow)[*] = 0
		if np lt 1 then return
		if ptr_good( event.pointer) then begin
			for i=0,np-1 do begin
				if (*p[i]).array and ptr_good((*p[i]).pactive) then begin
					mask = bytarr( 1000)
					mask[ *(*p[i]).pactive] = 1
				endif else begin
					mask = bytarr( 1000)
					mask[ (*p[i]).station + adc_offset_device( (*p[i]).DevObj) ] = 1
				endelse
				if (*event.pointer)[0] ne -1 then begin
					q = where( mask[*event.pointer] eq 1, nq)
				endif else nq=0
				if nq ge 1 then (*(*pstate).pshow)[i] = 1 
			endfor
		endif else nq=0
		j = ((*pstate).sel.top > 0) < (np-1)
		q = where( *(*pstate).pshow eq 1, nq)
		if nq eq 0 then begin
			select_one_spectrum, pstate, j, /keep_less
		endif
		update_headings, pstate, j

		*(*pstate).pchannel = active_channels( pstate)
		notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top 
		end
		
	'wizard-action': begin
		if ptr_valid( event.pointer) then begin
			if (*event.pointer).window eq 'Spectrum Select' then begin
				case (*event.pointer).command of
					'open-test': begin
;						print,'*** Wizard Spectrum Select: test if window is open ...'
						pw = (*pstate).pwiz
						*pw = *event.pointer
						(*pw).top = event.top
						(*pw).error = 0
						notify, 'wizard-return', pw
						end

					'delete': begin
						print,'*** Wizard Spectrum Select: delete ...'
						pw = event.pointer
						pd = (*pw).pdata
						mode = (*pd).mode			; see modes in 'OnButton_Delete_by_mode_Select'
						err = 0

						p = (*pstate).p
						if ptr_good(p) eq 0 then err=1
						if err eq 0 then begin
							p = *p
							multiple = 0
							if (*p[0]).array and ptr_good((*p[0]).pactive) then multiple=1

							err = 1							
							case mode of
								'xyt': begin									; delete all XY and T
									OnButton_NoXY_Spec_Select, Event
									OnButton_NoT_Spec_Select, Event
									end
								'xy': begin										; delete all XY spectra
									OnButton_NoXY_Spec_Select, Event
									end
								't': begin										; delete all T spectra
									OnButton_NoT_Spec_Select, Event
									end
								'e': begin										; delete all E spectra
									OnButton_NoE_Spec_Select, Event
									end
								'odd': begin									; delete All Odd detectors
									if multiple then goto, multiple
									OnButton_NoOdd_Spec_Select, Event
									end
								'even': begin									; delete All Even detectors
									if multiple then goto, multiple
									OnButton_NoEven_Spec_Select, Event
									end
								'selected': begin								; delete selected range of spectra
									OnButton_Delete_Spec_Select, Event
									end
								'displayed': begin								; delete Displayed spectra
									OnButton_Delete_Active_Select, Event
									end
								'overlays': begin								; delete Fit Overlays
									OnButton_Delete_Fit_Select, Event
									end
								'charge': begin									; delete Charge values
									OnButton_NoQ_Spec_Select, Event
									end
								'not-selected': begin							; delete Not selected range of spectra
									OnButton_Delete_Spec_Select, Event, /Nots
									end
								'not-displayed': begin							; delete Not Displayed spectra
									OnButton_Delete_Active_Select, Event, /Nots
									end
								'invalid-cal': begin							; delete spectra with invalid energy calibration
									OnButton_Delete_Active_Select, Event, /evalid
									end
								else:
							endcase
							err = 0
						endif
multiple:
						(*pw).error = err
						notify, 'wizard-return', pw
						end

					else:
				endcase
			endif
		endif
		end

	else: begin
		print,'OnNotify_Select: unknown tag = ',event.tag
		end
endcase

done:
end

;-----------------------------------------------------------------
; Tracking Callback Procedure.
;
;   {WIDGET_TRACKING, ID:0L, TOP:0L, HANDLER:0L, ENTER:0 }
;
;   ENTER is 1 if the tracking event is an entry event, and 0 if it
;       is an exit event.
;-----------------------------------------------------------------

pro OnTracking_Select, Event

;widget_control, event.id, get_uvalue=message
;if n_elements(message) lt 1 then return
;if size(message,/tname) ne 'STRING' then return

;child = widget_info( event.top, /child)
;widget_control, child, get_uvalue=pstate

;if event.enter eq 1 then begin
;	widget_control, (*pstate).help, set_value=message
;endif else begin
;	widget_control, (*pstate).help, set_value=' '
;endelse
end

;-----------------------------------------------------------------
; TLB_SIZE_EVENTS Callback Procedure.
;
;   {WIDGET_BASE, ID:0L, TOP:0L, HANDLER:0L, X:0, Y:0 }
;
;   The X and Y fields return the new width of the base, not
;       including any frame provided by the window manager.
;-----------------------------------------------------------------

pro OnSize_Select, event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

xmax = (*pstate).realtime ? 450 : 590

x = (event.x > xmax) - (*pstate).xoffset
y = (event.y - (*pstate).yoffset) > 54

; Use integer arithmetic for y to ensure a whole number of rows
;n = ((event.y - (*pstate).yoffset + 9)/ (*pstate).row_height) - 2
;n = (*pstate).rows < n
;(*pstate).display_rows = n
;y = (n + 1) * (*pstate).row_height + 22

widget_control, (*pstate).table, scr_xsize=x, scr_ysize=y
end

;-----------------------------------------------------------------

pro OnRealize_Select_Table, wWidget

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

w = widget_info( wWidget, /row_heights)
geom = widget_info( wWidget, /geometry)
tlb_geom = widget_info( top, /geometry)

(*pstate).table = wWidget
(*pstate).row_height = w[0]
(*pstate).rows = 6
(*pstate).display_rows = 6
(*pstate).yoffset = tlb_geom.ysize - geom.scr_ysize + 2
(*pstate).xoffset = tlb_geom.xsize - geom.scr_xsize

load_select_table, pstate, /init
end

;-----------------------------------------------------------------

pro OnRealize_Select_Highlight, wWidget

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if n_elements((*pstate).phighlight) gt 0 then begin
	if ptr_valid( (*pstate).phighlight) then begin
		(*pstate).highlight = (*(*pstate).phighlight).on
		widget_control, wWidget, set_combobox_select=(*pstate).highlight
	endif
endif
end

;-----------------------------------------------------------------

pro PostCreate_Select_Table, wWidget, spectrum=p, show=pshow, _EXTRA=_VWBExtra_, $
			update_notify=update_notify, highlight=highlight, layout=layout, $
			realtime=realtime

if n_elements(p) lt 1 then p=ptr_new()
if n_elements(highlight) lt 1 then highlight = {on:0, highlight:0}
if n_elements(layout) lt 1 then layout = ''
if n_elements(realtime) lt 1 then realtime = 0

state = {	p:			p, $				; pointer to spectrum pointer array (kept in parent)
			pshow:		pshow, $			; pointer to spectrum show flags (kept in parent)
		parray:			ptr_new(/allocate_heap), $ ; pointer to parameters struct used in 'detector_select'
		pchannel:		ptr_new(/allocate_heap), $ ; pointer to detector channel selection
		pselect:		ptr_new(/allocate_heap), $ ; pointer to selection range
		pwiz:			ptr_new(/alloc), $		; pointer for Wizard return
		matrix_file:	'', $				; DA matrix file name
		matrix:			ptr_new(), $		; pointer to DA matrix struct
		layout:			layout, $			; detector layout file, else blank
		realtime:		realtime, $			; realtime spectra mode
		
		highlight:		highlight.on, $		; highlight mode ON
		delete_mode:	0, $				; delete mode
		phighlight:		ptr_new(highlight, /no_copy), $ ; pointer to highlight info
		table:			0L, $				; table ID
		rows:			16, $				; number of rows
		columns:		5, $				; number of colums
		display_rows:	6, $				; number of rows displayed
		row_height:		0, $				; table row height
		xoffset:		0, $				; offset in xsize for resize
		yoffset:		0, $				; offset in ysize for resize
		sel: 			{left:-1, top:-1, right:-1, bottom:-1 }, $
		pseed:			ptr_new(/allocate_heap), $
		update_notify:	update_notify, $	; 'spectrum-display' NOTIFY STRING
		cr_found:		0 }					; to fight MAC IDL bug

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
end

;-----------------------------------------------------------------
; Table Insert String Callback Procedure.
;
;   {WIDGET_TABLE_STR, ID:0L, TOP:0L, HANDLER:0L, TYPE:1, OFFSET:0L,
;       STR:'', X:0L, Y:0L }
;
;   OFFSET is the (zero-based) insertion position that will result
;       after the character is inserted. STR is the string to be
;       inserted. X and Y give the zero-based address of the cell
;       within the table.
;-----------------------------------------------------------------

pro OnChangeValue_Select, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if (event.ch eq 13B) or (event.ch eq 10B) then begin
	if (*pstate).cr_found eq 1 then goto, finish
	(*pstate).cr_found = 1
endif else begin
	(*pstate).cr_found = 0
	goto, finish
endelse

if (event.x gt 25) or (event.y gt n_elements(*(*pstate).p)-1) then begin
	print, 'event.x,y inverted again!'
	goto, finish
endif

select_update_cel, pstate, event.x,event.y, event.top

finish:
end

;-----------------------------------------------------------------

pro select_update_cel, pstate, x,y, top

; Remember the order assumed here is set in "load_select_table"
; any changes there must be reflected here.

if ptr_valid( (*(*pstate).p)[0]) eq 0 then return

widget_control, (*pstate).table, get_value=v, $
		use_table_select=[x,y,x,y]

;print,'update cel x,y=',x,y,'  with ',v

case x of
	0: begin
		(*(*(*pstate).p)[y]).label = v
		end
	1: begin
		(*(*(*pstate).p)[y]).sample = v
		end
	2: begin
		(*(*(*pstate).p)[y]).grain = v
		end
	3: begin
		(*(*(*pstate).p)[y]).comment = v
		end
	4: begin
		(*(*(*pstate).p)[y]).channel = v
		(*(*(*pstate).p)[y]).station = v+1
		end
	5: begin
		(*(*(*pstate).p)[y]).multiplicity = v
		end
	6: begin
		(*(*(*pstate).p)[y]).charge = v
		notify, (*pstate).update_notify, from=top
		end
	7: begin
		(*(*(*pstate).p)[y]).IC_total = v
		print,'  Warning: IC count changed!'
		end
	8: begin
		(*(*(*pstate).p)[y]).charge = (*(*(*pstate).p)[y]).IC_total * v
		notify, (*pstate).update_notify, from=top
		load_select_table, pstate
		end
	9: begin
		(*(*(*pstate).p)[y]).deadtime_correction = v
		end
	10: begin
		print,'  Size not changed!'
		end
	11: begin
		(*(*(*pstate).p)[y]).cal.poly[1] = v
		notify, (*pstate).update_notify, from=top
		end
	12: begin
		(*(*(*pstate).p)[y]).cal.poly[0] = v
		notify, (*pstate).update_notify, from=top
		end
	13: begin
		(*(*(*pstate).p)[y]).cal.units = v
		end
	14: begin
		(*(*(*pstate).p)[y]).ecompress = v
		end
	15: begin
		(*(*(*pstate).p)[y]).x = v
		end
	16: begin
		(*(*(*pstate).p)[y]).y = v
		end
	17: begin
		(*(*(*pstate).p)[y]).z = v
		end
	18: begin
		(*(*(*pstate).p)[y]).theta = v
		end
	19: begin
		(*(*(*pstate).p)[y]).phi = v
		end
	20: begin
		(*(*(*pstate).p)[y]).scan.x = v
		end
	21: begin
		(*(*(*pstate).p)[y]).scan.y = v
		end
	else:
endcase

return
end

;-----------------------------------------------------------------
; Table Cell Select Callback Procedure
;
;   {WIDGET_TABLE_CELL_SEL, ID:0L, TOP:0L, HANDLER:0L, TYPE:4,
;       SEL_LEFT:0L, SEL_TOP:0L, SEL_RIGHT:0L, SEL_BOTTOM:0L}
;
;   The range of cells selected is given by the zero-based indices
;       into the table specified by the SEL_LEFT, SEL_TOP, SEL_RIGHT,
;       and SEL_BOTTOM fields. When cells are deselected (either by
;       changing the selection or by clicking in the upper left
;       corner of the table) an event is generated in which the
;       SEL_LEFT, SEL_TOP, SEL_RIGHT, and SEL_BOTTOM fields contain
;       the value -1.
;-----------------------------------------------------------------

pro OnCellSelect_Select, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if (event.sel_left eq -1) and (event.sel_top eq -1) and (event.sel_right eq -1) and (event.sel_bottom eq -1) then begin
	if ((*pstate).sel.left ge 0) and ((*pstate).sel.left lt (*pstate).columns) and ((*pstate).sel.top ge 0) and ((*pstate).sel.top lt (*pstate).rows) then begin
		select_update_cel, pstate, (*pstate).sel.left,(*pstate).sel.top, event.top
	endif
	goto, done
endif
if event.sel_left eq -1 then goto, done

if (event.sel_left eq 0) and (event.sel_top eq event.sel_bottom) and $
		(event.sel_right eq (*pstate).columns-1) then begin
	n = event.sel_top
	(*pstate).sel.left = event.sel_left
	(*pstate).sel.top = event.sel_top
	(*pstate).sel.right = event.sel_right
	(*pstate).sel.bottom = event.sel_bottom

	if (*pstate).highlight then begin
		(*(*pstate).phighlight).highlight = clip( (*pstate).sel.top, 0, n_elements(*(*pstate).p)-1)
		notify, 'spectrum-highlight', (*pstate).phighlight, from=event.top
		*(*pstate).pselect = clip( (*pstate).sel.top, 0, n_elements(*(*pstate).p)-1)
		notify, 'select-highlight', (*pstate).pselect, from=event.top
	endif
endif else if (event.sel_top eq 0) and (event.sel_left eq event.sel_right) and $
		(event.sel_bottom eq (*pstate).rows-1) then begin
	n = event.sel_LEFT
	(*pstate).sel.left = -1
	(*pstate).sel.top = -1
	(*pstate).sel.right = -1
	(*pstate).sel.bottom = -1
endif else begin
	(*pstate).sel.left = event.sel_left
	(*pstate).sel.top = event.sel_top
	(*pstate).sel.right = event.sel_right
	(*pstate).sel.bottom = event.sel_bottom
;	print,' new sel: ', (*pstate).sel
endelse
spectrum_select_update_colours, pstate

done:
end

;-----------------------------------------------------------------
; Activate Button Callback Procedure.
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.
;-----------------------------------------------------------------

pro OnButton_Select_Prev, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
view = widget_info( (*pstate).table, /table_view)

if (*pstate).highlight then begin
	active = (*pstate).sel.top < ((*pstate).rows-1)
	active = (active - 1) > 0
	(*pstate).sel.top = active
	(*pstate).sel.bottom = active
	(*(*pstate).phighlight).highlight = active
	notify, 'spectrum-highlight', (*pstate).phighlight, from=event.top
	*(*pstate).pselect = active
	notify, 'select-highlight', (*pstate).pselect, from=event.top
endif else begin
	if all_active(pstate) then begin
		active = 0
	endif else begin
		active = last_active(pstate)
		active = (active - 1) > 0
	endelse
	
	select_one_spectrum, pstate, active, /keep_less
	update_headings, pstate, active
	*(*pstate).pchannel = active_channels( pstate)
	notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
endelse
	widget_control, (*pstate).table, set_table_select=[0,active,(*pstate).columns-1,active]
	widget_control, (*pstate).table, set_table_view=[view[0],(view[1]-1)>0]
return
end

;-----------------------------------------------------------------

pro OnButton_Select_Next, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
view = widget_info( (*pstate).table, /table_view)

if (*pstate).highlight then begin
	active = (*pstate).sel.top
	active = (active + 1) < (n_elements(*(*pstate).p)-1)
	(*pstate).sel.top = active
	(*pstate).sel.bottom = active
	(*(*pstate).phighlight).highlight = active
	notify, 'spectrum-highlight', (*pstate).phighlight, from=event.top
	*(*pstate).pselect = active
	notify, 'select-highlight', (*pstate).pselect, from=event.top
endif else begin
	active = last_active(pstate)
	(*(*pstate).pshow)[active] = 0
	active = (active + 1) < (n_elements(*(*pstate).p)-1)
	elow = (*(*(*pstate).p)[active]).elow
	ehigh =(*(*(*pstate).p)[active]).ehigh
	
	; Add this (14-9-07) to make next same as previous
	(*(*(*pstate).p)[active]).elow = elow
	(*(*(*pstate).p)[active]).ehigh = ehigh
	
	select_one_spectrum, pstate, active, /keep_less
	update_headings, pstate, active
	*(*pstate).pchannel = active_channels( pstate)
	notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
endelse
	widget_control, (*pstate).table, set_table_select=[0,active,(*pstate).columns-1,active]
	widget_control, (*pstate).table, set_table_view=[view[0],(view[1]+1)<((n_elements(*(*pstate).p)-1))]
return
end

;-----------------------------------------------------------------

; 'Array ...' button

pro OnButton_Select_Sel, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_good(p) eq 0 then return
p = *p

mask1 = bytarr(1000)					; detector present
mask2 = bytarr(1000)					; detector spectrum shown
for i=0,n_elements(p)-1 do begin
	if (*p[i]).array and ptr_good((*p[i]).pactive) then begin
		mask1[ *(*p[i]).pactive] = 1
	endif else begin
		mask1[ (*p[i]).station + adc_offset_device( (*p[i]).DevObj) > 0 ] = 1
	endelse

	if (*(*pstate).pshow)[i] then begin
		if (*p[i]).array and ptr_good((*p[i]).pactive) then begin
			mask2[ *(*p[i]).pactive] = 1
		endif else begin
			mask2[ (*p[i]).station + adc_offset_device( (*p[i]).DevObj) > 0 ] = 1
		endelse
	endif
endfor
qactive = where( mask1 eq 1, nq)
if nq eq 0 then return 
qselect = where( mask2 eq 1, nq)

path = ''
if ptr_good( p[0]) then begin
	path = extract_path( (*p[0]).file)
endif
detector_select, group=event.top, TLB=tlb, pars=(*pstate).parray, active=qactive, $
			select=qselect, path=path, watch=(*pstate).update_notify
register_notify, event.top, [ 'array-select'], from=tlb
return
end

;-----------------------------------------------------------------

pro OnButton_Delete_by_mode_Select, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_good(p) eq 0 then return
p = *p
multiple = 0
if (*p[0]).array and ptr_good((*p[0]).pactive) then multiple=1

	case (*pstate).delete_mode of
		0: begin										; delete all XY and T
			OnButton_NoXY_Spec_Select, Event
			OnButton_NoT_Spec_Select, Event
			end
		1: begin										; delete all XY spectra
			OnButton_NoXY_Spec_Select, Event
			end
		2: begin										; delete all T spectra
			OnButton_NoT_Spec_Select, Event
			end
		3: begin										; delete all E spectra
			OnButton_NoE_Spec_Select, Event
			end
		4: begin										; delete All Odd detectors
			if multiple then goto, multiple
			OnButton_NoOdd_Spec_Select, Event
			end
		5: begin										; delete All Even detectors
			if multiple then goto, multiple
			OnButton_NoEven_Spec_Select, Event
			end
		6: begin										; delete selected range of spectra
			OnButton_Delete_Spec_Select, Event
			end
		7: begin										; delete Displayed spectra
			OnButton_Delete_Active_Select, Event
			end
		8: begin										; delete Fit Overlays
			OnButton_Delete_Fit_Select, Event
			end
		9: begin										; delete Charge values
			OnButton_NoQ_Spec_Select, Event
			end
		10: begin										; delete Not selected range of spectra
			OnButton_Delete_Spec_Select, Event, /Nots
			end
		11: begin										; delete Not Displayed spectra
			OnButton_Delete_Active_Select, Event, /Nots
			end
		12: begin										; delete spectra with invalid energy calibration
			OnButton_Delete_Active_Select, Event, /evalid
			end
		13: begin										; delete spectra with no counts
			OnButton_Delete_Zero_Select, Event
			end
		else:
	endcase
	return
	
multiple:
	warning,'OnButton_Delete_by_mode_Select',['Spectra contain multiple detectors.','No spectra deleted.']
	return
end

;-----------------------------------------------------------------

pro OnButton_Delete_Active_Select, Event, nots=nots, evalid=evalid

; Delete displayed spectra
; /nots		delete not displayed spectra
; /evalid	delete spectra with valid energy calibrations

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(nots) eq 0 then nots=0
if n_elements(evalid) eq 0 then evalid=0

if ptr_good( (*pstate).p) eq 0 then return
np = n_elements(*(*pstate).p)
if np eq 0 then return
p = (*pstate).p
if ptr_good( (*p)[0]) eq 0 then return

kill = intarr(np)
if evalid then begin
	for i=0,np-1 do begin
		if ((*(*p)[i]).cal.units eq 'channel') or (abs((*(*p)[i]).cal.poly[1]-1.) le 0.0001) then kill[i]=1
	endfor
endif else begin
	show = *(*pstate).pshow
	q = where( show eq (nots ? 0 : 1), nq)
	if nq ge 1 then kill[q] = 1
endelse
for i=0,np-1 do begin
	if kill[i] and ptr_valid( (*p)[i]) then free_spectrum, (*p)[i]
endfor
q = where(kill eq 0)
if q[0] ne -1 then begin
	*p = (*p)[q]
	*(*pstate).pshow = (*(*pstate).pshow)[q]
endif else begin
	*p = ptr_new()
	*(*pstate).pshow = 0
endelse

np = n_elements( *p)
OK = 0
for i=0L,np-1 do begin
	if (*(*pstate).pshow)[i] eq 1 then OK=1
endfor

if OK eq 0 then select_one_spectrum, pstate, 0, /keep_less
update_headings, pstate, 0

*(*pstate).pchannel = active_channels( pstate)
notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
load_select_table, pstate
return
end


;-----------------------------------------------------------------

pro OnButton_Delete_Spec_Select, Event, nots=nots, keep_zero=keep_zero, select=sel

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(nots) eq 0 then nots=0
if n_elements(keep_zero) eq 0 then keep_zero=0

	if ptr_good( (*pstate).p) eq 0 then return
	p = (*pstate).p
	np = n_elements(*p)
	if np eq 0 then return
	if ptr_good( (*p)[0], /struct) eq 0 then return
	mask = intarr(np)
	
	if n_elements(sel) eq 0 then begin
		if ((*pstate).sel.top lt 0) or ((*pstate).sel.bottom ge np) then return
		i1 = (*pstate).sel.top
		i2 = (*pstate).sel.bottom
		if nots then begin
			mask[i1:i2] = 1
		endif else begin
			mask[*] = 1
			mask[i1:i2] = 0
		endelse
	endif else begin
		q = sel
		nq = n_elements(sel)
		if q[0] eq -1 then return
		mask[*] = 1
		mask[q] = 0
	endelse
	if keep_zero then mask[0] = 1			; do not delete region #0

;	NOTE: Do not pause execution in debug between the free and the *(*pstate).p
;	as notifies go elsewhere and use this pointer array

	qc = where( mask eq 0, nqc)				; delete these
	if nqc eq 0 then return
	for j=0,nqc-1 do begin
		i = qc[j]
		if ptr_valid( (*p)[i]) then free_spectrum, (*p)[i]
	endfor

	q = where( mask eq 1, nq)				; keep these
	if nq ge 1 then begin
		if keep_zero then begin				; make #0 the sum of the others
			*(*(*p)[0]).data = 0
			for j=0,nq-1 do begin
				i = q[j]
				if i eq 0 then continue
				*(*(*p)[0]).data = *(*(*p)[0]).data + *(*(*p)[i]).data
			endfor
		endif

		pq = (*p)[q]
		s = (*(*pstate).pshow)[q]
	endif else begin
		pq = ptr_new()
		s = 0
	endelse

	*(*pstate).p = pq
	*(*pstate).pshow = s

	active = (*pstate).sel.top
	view = widget_info( (*pstate).table, /table_view)
	
	active = active < (n_elements(p)-1)
	q = where(*(*pstate).pshow ne 0, nq)
	if nq eq 0 then select_one_spectrum, pstate, active, /keep_less
	update_headings, pstate, active

	*(*pstate).pchannel = active_channels( pstate)
	notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
	load_select_table, pstate

	widget_control, (*pstate).table, set_table_select=[0,active,(*pstate).columns-1,active]
	widget_control, (*pstate).table, set_table_view=[view[0],view[1]<((n_elements(*(*pstate).p)-1))]
return
end

;-----------------------------------------------------------------

pro OnButton_Delete_Zero_Select, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good( (*pstate).p) eq 0 then return
np = n_elements(*(*pstate).p)
if np eq 0 then return
if ptr_good( (*(*pstate).p)[0]) eq 0 then return

kill = intarr(np)
for i=0L,np-1 do begin
	if ptr_valid( (*(*pstate).p)[i]) eq 0 then begin
		kill[i] = 1
		continue
	endif

	sum = total( *(*(*(*pstate).p)[i]).data)
	if sum lt 1. then begin
		free_spectrum, (*(*pstate).p)[i]
		kill[i] = 1
	endif
endfor
q = where(kill eq 0)
if q[0] ne -1 then begin
	*(*pstate).p = (*(*pstate).p)[q]
	*(*pstate).pshow = (*(*pstate).pshow)[q]
endif

n = n_elements( *(*pstate).p)
changed = 0
for i=0L,n-1 do begin
	if (*(*pstate).pshow)[i] eq 0 then changed=1
	(*(*pstate).pshow)[i] = 1
endfor
rows = strarr(n)
for i=0L,n-1 do begin
	rows[i] = str_show( pstate, i)
endfor
widget_control, (*pstate).table, row_labels= rows

*(*pstate).pchannel = active_channels( pstate)
notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
load_select_table, pstate
return
end

;-----------------------------------------------------------------

pro OnButton_NoOdd_Spec_Select, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good( (*pstate).p) eq 0 then return
np = n_elements(*(*pstate).p)
if np eq 0 then return
if ptr_good( (*(*pstate).p)[0]) eq 0 then return
p = (*pstate).p

kill = intarr(np)
for i=0L,np-1 do begin
	j = (*(*p)[i]).station + adc_offset_device( (*(*p)[i]).DevObj)
	if j ne 2*(j/2) then begin
		if ptr_valid( (*p)[i]) then free_spectrum, (*p)[i]
		kill[i] = 1
	endif
endfor
q = where(kill eq 0)
if q[0] ne -1 then begin
	*p = (*p)[q]
	*(*pstate).pshow = (*(*pstate).pshow)[q]
endif

n = n_elements( *p)
changed = 0
for i=0L,n-1 do begin
	if (*(*pstate).pshow)[i] eq 0 then changed=1
	(*(*pstate).pshow)[i] = 1
endfor
rows = strarr(n)
for i=0L,n-1 do begin
	rows[i] = str_show( pstate, i)
endfor
widget_control, (*pstate).table, row_labels= rows

*(*pstate).pchannel = active_channels( pstate)
notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
load_select_table, pstate
return
end

;-----------------------------------------------------------------

pro OnButton_NoEven_Spec_Select, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good( (*pstate).p) eq 0 then return
np = n_elements(*(*pstate).p)
if np eq 0 then return
if ptr_good( (*(*pstate).p)[0]) eq 0 then return
p = (*pstate).p

kill = intarr(np)
for i=0L,np-1 do begin
	j = (*(*p)[i]).station + adc_offset_device( (*(*p)[i]).DevObj)
	if j eq 2*(j/2) then begin
		if ptr_valid( (*p)[i]) then free_spectrum, (*p)[i]
		kill[i] = 1
	endif
endfor
q = where(kill eq 0)
if q[0] ne -1 then begin
	*p = (*p)[q]
	*(*pstate).pshow = (*(*pstate).pshow)[q]
endif

n = n_elements( *p)
changed = 0
for i=0L,n-1 do begin
	if (*(*pstate).pshow)[i] eq 0 then changed=1
	(*(*pstate).pshow)[i] = 1
endfor
rows = strarr(n)
for i=0L,n-1 do begin
	rows[i] = str_show( pstate, i)
endfor
widget_control, (*pstate).table, row_labels= rows

*(*pstate).pchannel = active_channels( pstate)
notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
load_select_table, pstate
return
end

;-----------------------------------------------------------------

pro OnButton_NoXY_Spec_Select, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good( (*pstate).p) eq 0 then return
np = n_elements(*(*pstate).p)
if np eq 0 then return
if ptr_good( (*(*pstate).p)[0]) eq 0 then return

kill = intarr(np)
for i=0L,np-1 do begin
	j = locate_last('/', (*(*(*pstate).p)[i]).label)
	if j ge 0 then begin
		lab = strlowcase(strtrim(extract((*(*(*pstate).p)[i]).label,j+1,lenchr((*(*(*pstate).p)[i]).label)-1),2))
		if (lab eq 'x') or (lab eq 'y') then begin
			if ptr_valid( (*(*pstate).p)[i]) then free_spectrum, (*(*pstate).p)[i]
			kill[i] = 1
		endif
	endif
endfor
q = where(kill eq 0)
if q[0] ne -1 then begin
	*(*pstate).p = (*(*pstate).p)[q]
	*(*pstate).pshow = (*(*pstate).pshow)[q]
endif

n = n_elements( *(*pstate).p)
changed = 0
for i=0L,n-1 do begin
	if (*(*pstate).pshow)[i] eq 0 then changed=1
	(*(*pstate).pshow)[i] = 1
endfor
rows = strarr(n)
for i=0L,n-1 do begin
	rows[i] = str_show( pstate, i)
endfor
widget_control, (*pstate).table, row_labels= rows

*(*pstate).pchannel = active_channels( pstate)
notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
load_select_table, pstate
return
end

;-----------------------------------------------------------------

pro OnButton_NoE_Spec_Select, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good( (*pstate).p) eq 0 then return
np = n_elements(*(*pstate).p)
if np eq 0 then return
if ptr_good( (*(*pstate).p)[0]) eq 0 then return

kill = intarr(np)
for i=0L,np-1 do begin
	j = locate_last('/', (*(*(*pstate).p)[i]).label)
	if j ge 0 then begin
		lab = strlowcase(strtrim(extract((*(*(*pstate).p)[i]).label,j+1,lenchr((*(*(*pstate).p)[i]).label)-1),2))
		if (lab eq 'e') then begin
			if ptr_valid( (*(*pstate).p)[i]) then free_spectrum, (*(*pstate).p)[i]
			kill[i] = 1
		endif
	endif
endfor
q = where(kill eq 0)
if q[0] ne -1 then begin
	*(*pstate).p = (*(*pstate).p)[q]
	*(*pstate).pshow = (*(*pstate).pshow)[q]
endif

n = n_elements( *(*pstate).p)
changed = 0
for i=0L,n-1 do begin
	if (*(*pstate).pshow)[i] eq 0 then changed=1
	(*(*pstate).pshow)[i] = 1
endfor
rows = strarr(n)
for i=0L,n-1 do begin
	rows[i] = str_show( pstate, i)
endfor
widget_control, (*pstate).table, row_labels= rows

*(*pstate).pchannel = active_channels( pstate)
notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
load_select_table, pstate
return
end

;-----------------------------------------------------------------

pro OnButton_NoQ_Spec_Select, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good( (*pstate).p) eq 0 then return
n = n_elements(*(*pstate).p)
if n eq 0 then return
if ptr_good( (*(*pstate).p)[0]) eq 0 then return

for i=0L,n-1 do begin
	p = (*(*pstate).p)[i]
	(*p).charge = 0.0
endfor

*(*pstate).pchannel = active_channels( pstate)
notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
load_select_table, pstate
return
end

;-----------------------------------------------------------------

pro OnButton_NoT_Spec_Select, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good( (*pstate).p) eq 0 then return
np = n_elements(*(*pstate).p)
if np eq 0 then return
if ptr_good( (*(*pstate).p)[0]) eq 0 then return

kill = intarr(np)
for i=0L,np-1 do begin
	j = locate_last('/', (*(*(*pstate).p)[i]).label)
	if j ge 0 then begin
		lab = strlowcase(strtrim(extract((*(*(*pstate).p)[i]).label,j+1,lenchr((*(*(*pstate).p)[i]).label)-1),2))
		if (lab eq 't') then begin
			if ptr_valid( (*(*pstate).p)[i]) then free_spectrum, (*(*pstate).p)[i]
			kill[i] = 1
		endif
	endif
endfor
q = where(kill eq 0)
if q[0] ne -1 then begin
	*(*pstate).p = (*(*pstate).p)[q]
	*(*pstate).pshow = (*(*pstate).pshow)[q]
endif

n = n_elements( *(*pstate).p)
changed = 0
for i=0L,n-1 do begin
	if (*(*pstate).pshow)[i] eq 0 then changed=1
	(*(*pstate).pshow)[i] = 1
endfor
rows = strarr(n)
for i=0L,n-1 do begin
	rows[i] = str_show( pstate, i)
endfor
widget_control, (*pstate).table, row_labels= rows

*(*pstate).pchannel = active_channels( pstate)
notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
load_select_table, pstate
return
end

;-----------------------------------------------------------------

pro OnButton_Delete_Fit_Select, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

np = n_elements(*(*pstate).p)
if np eq 0 then return
if ptr_good( (*(*pstate).p)[0]) eq 0 then return

if ((*pstate).sel.top ge 0) and ((*pstate).sel.bottom lt np) then begin
	for i=(*pstate).sel.top, (*pstate).sel.bottom do begin
		p = (*(*pstate).p)[i]
		if (*p).n_fit gt 0 then begin
			if ptr_valid( ((*p).fit)[(*p).n_fit-1]) then begin
				free_spectrum, ((*p).fit)[(*p).n_fit-1]
			endif
			(*p).n_fit = (*p).n_fit-1
		endif
	endfor
	*(*pstate).pchannel = active_channels( pstate)
	notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
endif
return
end

;-----------------------------------------------------------------

pro OnButton_Display, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

changed = 0
if (*pstate).sel.top ge 0 then begin
	for i=(*pstate).sel.top, (*pstate).sel.bottom do begin
		(*(*pstate).pshow)[i] = toggle((*(*pstate).pshow)[i])
		changed = 1
	endfor
	n = n_elements( *(*pstate).p)
	rows = strarr(n)
	for i=0L,n-1 do begin
		rows[i] = str_show( pstate, i)
	endfor
	spectrum_select_update_colours, pstate
	
	widget_control, (*pstate).table, row_labels= rows
	print,'select', changed, '  ', (*pstate).update_notify
	*(*pstate).pchannel = active_channels( pstate)
	if changed then notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
endif
return
end

;-----------------------------------------------------------------

pro OnButton_Display_One, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

	changed = 0
	n = n_elements( *(*pstate).p)
	i = (*pstate).sel.top > 0
	for j=0L,n-1 do begin
		(*(*pstate).pshow)[j] = 0
	endfor
	(*(*pstate).pshow)[i] = 1
	changed = 1
	rows = strarr(n)
	
	for i=0L,n-1 do begin
		rows[i] = str_show( pstate, i)
	endfor
	spectrum_select_update_colours, pstate
	
	widget_control, (*pstate).table, row_labels= rows
	print,'select', changed, '  ', (*pstate).update_notify
	*(*pstate).pchannel = active_channels( pstate)
	if changed then notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
	return
end

;-----------------------------------------------------------------

pro OnButton_Display_All, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

n = n_elements( *(*pstate).p)
changed = 0
for i=0L,n-1 do begin
	if (*(*pstate).pshow)[i] eq 0 then changed=1
	(*(*pstate).pshow)[i] = 1
endfor
rows = strarr(n)
for i=0L,n-1 do begin
	rows[i] = str_show( pstate, i)
endfor
spectrum_select_update_colours, pstate

widget_control, (*pstate).table, row_labels= rows
*(*pstate).pchannel = active_channels( pstate)
if changed then notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
return
end

;-----------------------------------------------------------------

pro OnButton_Display_Random, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

n = n_elements( *(*pstate).p)
q = round( n* randomu( *(*pstate).pseed, 16)) < (n-1)
nq = 16

(*(*pstate).pshow)[*] = 0
(*(*pstate).pshow)[ ((*(*pstate).phighlight).highlight > 0) < (n-1)] = 1
for i=0L,nq-1 do begin
	(*(*pstate).pshow)[q[i]] = 1
endfor

rows = strarr(n)
for i=0L,n-1 do begin
	rows[i] = str_show( pstate, i)
endfor
spectrum_select_update_colours, pstate

widget_control, (*pstate).table, row_labels= rows
*(*pstate).pchannel = active_channels( pstate)
notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
return
end

;-----------------------------------------------------------------

pro OnButton_ShowFit, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

changed = 0
if (*pstate).sel.top ge 0 then begin
	for i=(*pstate).sel.top, (*pstate).sel.bottom do begin
		if (i ge 0) and (i lt n_elements(*(*pstate).p)) then begin
			(*(*(*pstate).p)[i]).showfit = toggle((*(*(*pstate).p)[i]).showfit)
			changed = 1
		endif
	endfor
	n = n_elements( *(*pstate).p)
	rows = strarr(n)
	for i=0L,n-1 do begin
		rows[i] = str_show( pstate, i)
	endfor
	widget_control, (*pstate).table, row_labels= rows
	*(*pstate).pchannel = active_channels( pstate)
	if changed then notify, (*pstate).update_notify, (*pstate).pchannel, from=event.top
end
return
end

;-----------------------------------------------------------------

pro OnButton_Close_Select, Event

;OnDestroy_Select, event.top
OnKill_Select, event

end

;-----------------------------------------------------------------
; Droplist Select Item Callback Procedure.
;
;   {WIDGET_combobox, ID:0L, TOP:0L, HANDLER:0L, INDEX:0L }
;
;   INDEX returns the index of the selected item. This can be used to
;       index the array of names originally used to set the widget's
;       value.
;-----------------------------------------------------------------

pro OnSelect_Highlight_Select, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).highlight = event.index

(*(*pstate).phighlight).on = (*pstate).highlight
(*(*pstate).phighlight).highlight = clip( (*pstate).sel.top, 0, n_elements(*(*pstate).p)-1)
spectrum_select_update_colours, pstate

notify, 'spectrum-highlight', (*pstate).phighlight, from=event.top
end

;-----------------------------------------------------------------
;
pro OnSelect_Delete_Mode_Select, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).delete_mode = event.index
return
end

;-----------------------------------------------------------------
;
; Empty stub procedure used for autoloading.
;
pro spectrum_select_eventcb
end
