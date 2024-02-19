;
;	select = export_select( group, names, mode_list, old_select=old_select)
;
;	group		parent widget
;	names		element names
;	old_select	current selection states
;	select		new selection states
;
;	select = {el:el_names, el_enable:enable, mode:mode_list, mode_enable:enablem, error:0 }
;
;----------------------------------------------------------------------

pro export_select_event, Event

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	uname = widget_info( event.id, /uname)

	case uname of

		'export_select_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				widget_control, event.top, /destroy
				return
			endif
			end

		'select-elements': begin
			(*(*pstate).p).el_enable = event.select.enable
			end

		'select-modes': begin
			(*(*pstate).p).mode_enable = event.select.enable
			end

		'ok': begin
			widget_control, event.top, /destroy
			return
			end

		'cancel': begin
			(*(*pstate).p).error = 1
			widget_control, event.top, /destroy
			return
			end
		else:
	endcase

finish:
	return
end

;-----------------------------------------------------------------

function export_select, group, el_names, mode_list, path=path, old_select=old_select, $
		title=title, initial_mode_on=initial_mode_on

;	Select elements, and export catagories, from the lists.
;	'old_select' is the selection, of the form:
;		{el:els, el_enable:enables, mode:els, mode_enable:enables}

	if n_elements(path) lt 1 then path=''
	if n_elements(el_names) lt 1 then return, 0
	if n_elements(mode_list) lt 1 then return, 0
	if n_elements(group) lt 1 then return, 0
	if n_elements(title) lt 1 then title=['Export Select','Select Elements','Select Output Options']
	n_els = n_elements(el_names)

	enable = replicate(1,n_els)
	q = where( (strlowcase(strmid(el_names,0,4)) eq 'back') or (el_names eq 'Sum'))
	if q[0] ne -1 then enable[q] = 0
	nm = n_elements(mode_list)
	enablem = intarr(nm)
	if n_elements(initial_mode_on) ge 1 then begin
		enablem[initial_mode_on	< (nm-1)] = 1
	endif else begin
		enablem[0:min([2,n_elements(enablem)-1])] = 1
	endelse

	if (n_elements(old_select) gt 0) and (size(old_select,/tname) eq 'STRUCT') then begin
		for i=0L,n_elements(old_select.el)-1 do begin
			q = where( old_select.el[i] eq el_names)
			if q[0] ne -1 then enable[q[0]] = old_select.el_enable[i]
		endfor
		for i=0L,n_elements(old_select.mode)-1 do begin
			q = where( old_select.mode[i] eq mode_list)
			if q[0] ne -1 then enablem[q[0]] = old_select.mode_enable[i]
		endfor
	endif

	select = {el:el_names, el_enable:enable, mode:mode_list, mode_enable:enablem, error:0 }

case !version.os_family of
	'MacOS': begin
		cols7 = 7
		end
	'unix': begin
		cols7 = 10
		end
	else: begin
		cols7 = 7
		end
endcase

	xsize = 300
	ysize = 500
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title=title[0], /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='export_select_TLB', /base_align_center, $
					/modal, /floating, xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center)

	els = select_element( tbase, el_names, old_select=select.el_enable, path=path, $
				uname='select-elements', title=title[1], $
				columns=cols7 )

	modes = select_element( tbase, mode_list, old_select=select.mode_enable, path=path, $
				uname='select-modes', title=title[2], $
				columns=2 )

	p = ptr_new( select )			; current selection

	bbase = widget_base( tbase, /row, /base_align_center, /align_center, ypad=1, space=2)
	button = widget_button( bbase, value='Cancel', uname='cancel')
	lab = widget_label( bbase, value='', scr_xsize=30)
	button = widget_button( bbase, value='OK', uname='ok')

	state = {	$
			path:	path, $					; pointer to current path
			p:		p, $				; pointer to selection
			els:	els $					; ID of select_element widget
		}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	xmanager, 'export_select', tlb					;, /no_block

	r = *p
	return, r
end
