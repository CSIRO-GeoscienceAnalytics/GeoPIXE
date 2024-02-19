;
;	Compound widget for selecting items from a list 'names'. Returns selection
;	in events. Originally used for element names, but can be used for any
;	list of strings.
;
;	Note: There is a simple modal standalone version called "element_select".
;
; Compound Widget:
;	widget_id = select_element( parent, names, old_select=old_select, title=title)
;
;	parent		parent widget
;	name		element names
;	old_select	current selection states
;	title		title text
;
;	returns event:
;
;	{SELECT_ELEMENT, ID:id, TOP:top, HANDLER:handler, SELECT:{name:names, enable:select-states}}
;
;	Can get and set value of selection enable vector.
;
;----------------------------------------------------------------------

function select_element_event, Event

	child = widget_info( event.handler, /child)
	widget_control, child, get_uvalue=pstate
	return_event = 0

	uname = widget_info( event.id, /uname)

	case uname of

		'select': begin
			widget_control, event.id, get_uvalue=i
			widget_control, event.id, get_value=v
			(*(*pstate).p)[i] = v
			goto, update
			end
		'select-all': begin
			(*(*pstate).p)[*] = 1
			goto, update
			end
		'select-none': begin
			(*(*pstate).p)[*] = 0
			goto, update
			end
		'load': begin
			F = file_requester( /read, /must_exist, filter = '*.select', $
					title='Load Element Selection from File', path=(*pstate).path, $
					group=event.top, /fix_filter)
			if F ne '' then begin
				load_select_element, pstate, F
				goto, update
			endif
			end
		'save': begin
			F = file_requester( /write, filter = '*.select', $
					title='Save Element Selection to File', path=(*pstate).path, $
					group=event.top, /fix_filter)
			if F ne '' then begin
				save_select_element, pstate, F
			endif
			end
		else:
	endcase

finish:
	return, return_event

update:
	select_element_update, pstate

	return_event = { ID:event.handler, TOP:event.top, $
			HANDLER:0L, SELECT:{ name:(*pstate).el, enable:*(*pstate).p} }
	goto, finish
end

;-----------------------------------------------------------------

pro select_element_update, pstate

	for i=0L,(*pstate).columns-1 do begin
		i1 = i*(*pstate).n
		i2 = min([i*(*pstate).n+(*pstate).n-1,(*pstate).n_els-1])
		ni = i2-i1+1
		if ni ge 1 then widget_control, (*pstate).id[i], set_value=(*(*pstate).p)[i1:i2]
	endfor
	return
end

;-----------------------------------------------------------------

pro select_element_set, id, value

	child = widget_info( id, /child)
	widget_control, child, get_uvalue=pstate

	n = min([n_elements( *(*pstate).p), n_elements(value)])
	if n gt 0 then begin
		(*(*pstate).p)[0:n-1] = value[0:n-1]
		select_element_update, pstate
	endif
	return
end

;-----------------------------------------------------------------

function select_element_get, id

	child = widget_info( id, /child)
	widget_control, child, get_uvalue=pstate

	result = (*(*pstate).p)
	return, result
end

;-----------------------------------------------------------------

pro save_select_element, pstate, file

	file = strip_file_ext(file) + '.select'
	on_ioerror, bad_open
	openw, lun, file, /xdr, /get_lun

	on_ioerror, bad_io
	version = -1
	writeu,lun, version
	writeu,lun, n_elements( *(*pstate).p)
	writeu,lun, (*(*pstate).p)
	writeu,lun, (*pstate).el

done:
	close_file, lun
	return

bad_open:
	warning,'save_select_element','error opening file: '+file
	goto, done
bad_io:
	warning,'save_select_element','error writing file: '+file
	goto, done
end

;-----------------------------------------------------------------

pro load_select_element, pstate, file

	on_ioerror, bad_open
	openr, lun, file, /xdr, /get_lun
	on_ioerror, bad_io

	valid = [-1]

	version = 0
	readu,lun, version
	q = where( version eq valid)
	if q[0] eq -1 then goto, bad_io

	n = 0
	readu,lun, n
	if n le 0 then goto, bad_io

	select = intarr(n)
	el = strarr(n)
	readu,lun, select
	readu,lun, el

	for i=0L,n-1 do begin
		q = where( el[i] eq (*pstate).el )
		if q[0] ne -1 then begin
			(*(*pstate).p)[q[0]] = select[i]
		endif
	endfor

done:
	close_file, lun
	return

bad_open:
	warning,'load_select_element','error opening file: '+file
	goto, done
bad_io:
	warning,'load_select_element','error reading file: '+file
	goto, done
end

;-----------------------------------------------------------------

function select_element, parent, el_names, old_select=old_select, path=path, $
		uname=uname, uvalue=uvalue, title=title, columns=columns, _extra=extra

;	Select elements from the list.
;	'old_select' is an earlier selection

	if n_elements(path) lt 1 then path=''
	if n_elements(title) lt 1 then title='Choose from these Selections'
	if n_elements(uname) lt 1 then uname=''
	if n_elements(columns) lt 1 then columns=4
	if n_elements(uvalue) lt 1 then uvalue=0
	if n_elements(el_names) lt 1 then return, 0
	if n_elements(parent) lt 1 then return, 0
	n_els = n_elements(el_names)
	if (n_elements(old_select) lt 1) or (n_elements(old_select) ne n_els) then begin
		old_select = replicate(1,n_els)
		q = where( (strlowcase(strmid(el_names,0,4)) eq 'back') or (el_names eq 'Sum'))
		if q[0] ne -1 then old_select[q] = 0
	endif

	tlb = widget_base( parent, /column, uname=uname, uvalue=uvalue, $
					/base_align_center, /frame, _extra=extra, $
					event_func='select_element_event', $
					pro_set_value='select_element_set', $
					func_get_value='select_element_get')

	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( tbase, value=title, /align_center)

	sbase = widget_base( tbase, /row, /base_align_top, ypad=0, xpad=0, space=0)
	n = ceil( float(n_els)/float(columns))
	id = lonarr(columns)

	for i=0L,columns-1 do begin
		i1 = i*n
		i2 = min([i*n+n-1,n_els-1])
		ni = i2-i1+1
		if ni ge 1 then begin
			id[i] = cw_bgroup2( sbase, el_names[i1:i2], /column, xpad=0, ypad=0, space=0, $
					/return_index, uname='select', /nonexclusive, $
					set_value=old_select[i1:i2], uvalue=i1+indgen(ni) )
		endif
	endfor

	p = ptr_new( old_select )			; current selection

	bbase = widget_base( tbase, /row, /base_align_center, /align_center, ypad=1, space=2)

	button = widget_button( bbase, value='Load', uname='load')
	button = widget_button( bbase, value='Save', uname='save')
	lab = widget_label( bbase, value=' ')

	lab = widget_label( bbase, value='Select:')
	button = widget_button( bbase, value='All', uname='select-all')
	button = widget_button( bbase, value='None', uname='select-none')

	state = {	$
			path:	path, $					; pointer to current path
			p:		p, $					; pointer to selection
			n_els:	n_els, $				; number of els
			el:		el_names, $				; element names
			n:		n, $					; number in a column
			columns:	columns, $			; number of columns
			id:		id $					; IDs of bgroup widgets
		}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)

	return, tlb
end
