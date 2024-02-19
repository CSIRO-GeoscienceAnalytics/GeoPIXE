;
; Modal pop-up window to select elements.
; Note: There is also a compound widget form of this called "select_element".
;
;	select = element_select( group, names, old_select=old_select)
;
;	group		parent widget
;	name		element names
;	old_select	current selection states
;	select		new selection states
;
;----------------------------------------------------------------------

pro element_select_event, Event

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	uname = widget_info( event.id, /uname)

	case uname of

		'element_select_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				widget_control, event.top, /destroy
				if (*pstate).local then widget_control, (*pstate).group, /destroy
				return
			endif
			end
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
				load_element_select, pstate, F
				goto, update
			endif
			end
		'save': begin
			F = file_requester( /write, filter = '*.select', $
					title='Save Element Selection to File', path=(*pstate).path, $
					group=event.top, /fix_filter)
			if F ne '' then begin
				save_element_select, pstate, F
			endif
			end
		'ok': begin
			*(*pstate).perror = 0
			widget_control, event.top, /destroy
			if (*pstate).local then widget_control, (*pstate).group, /destroy
			return
			end
		else:
	endcase

finish:
	return

update:
	for i=0L,(*pstate).nc-1 do begin
		i1 = i*(*pstate).n
		i2 = min([i*(*pstate).n+(*pstate).n-1,(*pstate).n_els-1])
		ni = i2-i1+1
		if ni ge 1 then widget_control, (*pstate).id[i], set_value=(*(*pstate).p)[i1:i2]
	endfor
	goto, finish
end

;-----------------------------------------------------------------

pro save_element_select, pstate, file

	file = strip_file_ext(file) + '.select'
	on_ioerror, bad_open
	openw, lun, file, /xdr, /get_lun

	on_ioerror, bad_io
	version = -1
	writeu,lun, version
	writeu,lun, n_elements( *(*pstate).p)
	writeu,lun, *(*pstate).p
	writeu,lun, (*pstate).el

done:
	close_file, lun
	return

bad_open:
	warning,'save_element_select','error opening file: '+file
	goto, done
bad_io:
	warning,'save_element_select','error writing file: '+file
	goto, done
end

;-----------------------------------------------------------------

pro load_element_select, pstate, file

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
	warning,'load_element_select','error opening file: '+file
	goto, done
bad_io:
	warning,'load_element_select','error reading file: '+file
	goto, done
end

;-----------------------------------------------------------------

function element_select, group, el_names, old_select=old_select, path=path, title=title, $
					debug=debug, off=off

;	Select elements from the list.
;	'old_select' is an earlier selection

	if n_elements(path) lt 1 then path=''
	if n_elements(el_names) lt 1 then return, 0
	if n_elements(debug) lt 1 then debug=0
	if n_elements(off) lt 1 then off=0
	if n_elements(group) lt 1 then group=0L
	if widget_info(group,/valid) eq 0 then begin
		group = widget_base( scr_xsize=1, scr_ysize=1)
		local = 1
	endif else local=0
	if debug then begin
		modal = 0
		floating = 0
	endif else begin
		modal = 1
		floating = 1
	endelse
	if n_elements(title) lt 1 then title='Select Elements'
	n_els = n_elements(el_names)
	if (n_elements(old_select) lt 1) or (n_elements(old_select) ne n_els) then begin
		old_select = replicate(1-off,n_els)
		q = where( (strlowcase(strmid(el_names,0,4)) eq 'back') or (el_names eq 'Sum'))
		if q[0] ne -1 then old_select[q] = 0
	endif

	xsize = 300
	ysize = 500
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='element_select_TLB', /base_align_center, $
					modal=modal, floating=floating, xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center)

;	4 columns ...

	sbase = widget_base( tbase, /row, /base_align_top, ypad=0, xpad=0, space=0)
	m = ceil(sqrt(n_els)/2.) > 4
	n = floor(n_els/float(m))+1
	id = lonarr(m)

	for i=0L,m-1 do begin
		i1 = i*n
		i2 = min([i*n+n-1,n_els-1])
		ni = i2-i1+1
		if ni ge 1 then begin
			id[i] = cw_bgroup2( sbase, el_names[i1:i2], /column, xpad=0, ypad=0, space=0, $
					/return_index, uname='select', /nonexclusive, $
					set_value=old_select[i1:i2], uvalue=i1+indgen(ni) )
		endif
	endfor

	p = ptr_new( old_select )				; current selection
	perror = ptr_new(1)						; error flag

	bbase = widget_base( tbase, /row, /base_align_center, /align_center, ypad=1, space=2)

	button = widget_button( bbase, value='Load', uname='load')
	button = widget_button( bbase, value='Save', uname='save')
	lab = widget_label( bbase, value=' ')

	lab = widget_label( bbase, value='Select:')
	button = widget_button( bbase, value='All', uname='select-all')
	button = widget_button( bbase, value='None', uname='select-none')

	lab = widget_label( bbase, value=' ')
	button = widget_button( bbase, value='OK', uname='ok')

	state = {	$
			path:	path, $					; pointer to current path
			group:		group, $			; group leader
			local:		local, $			; local group leader
			p:		p, $					; pointer to selection
			n_els:	n_els, $				; number of els
			el:		el_names, $				; element names
			n:		n, $					; number in a column
			nc:		m, $					; number of columns
			id:		id, $					; IDs of bgroup widgets
			perror:	perror $				; error flag
		}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	if debug then begin
		xmanager, 'element_select', tlb, /no_block
	endif else begin
		xmanager, 'element_select', tlb
	endelse

	r = *p			; could not pass *p back. It became "undefined" under IDL 8.0
	if *perror then r[*]=0
	return, r
end
