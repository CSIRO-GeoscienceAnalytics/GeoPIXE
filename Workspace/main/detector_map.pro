pro detector_map_event, event

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
			warning,'detector_map_event',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			goto, kill
		endif
	endif
	
	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	pars = (*pstate).pars
	
	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

	case tag_names( event,/structure) of
		'NOTIFY': begin
			case event.tag of
				'path': begin
					if ptr_valid( event.pointer) then begin
						print,'detector_map: new path = ',(*event.pointer)
						*(*pstate).path = (*event.pointer)
					endif
					goto, finish
					end
				'spectra-changed': begin
					(*pstate).p = event.pointer
					goto, update
					end
				else:
			endcase
			end
		'WIDGET_TRACKING': begin
			widget_control, event.id, get_uvalue=s
			if size(s,/tname) eq 'STRING' then begin
				if event.enter eq 1 then begin
					widget_control, (*pstate).help, set_value=s
				endif else begin
					widget_control, (*pstate).help, set_value=(*pstate).help_text
				endelse
			endif
			goto, finish
			end
		'WIDGET_KILL_REQUEST': begin
			print,'Kill request detector_map ...'
			goto, kill
			end
		else:
	endcase
	
	uname = widget_info( event.id, /uname)
	case uname of

		'detector_map_tlb': begin
			if tag_names(event,/structure_name) eq 'WIDGET_BASE' then begin
				w = (event.x - 80) > 750
				h = (event.y - 150) > 700
				widget_control, (*pstate).draw, draw_xsize=w, draw_ysize=h
				(*pstate).width = w
				(*pstate).height = h
				wdelete, (*pstate).pix
				window, /free, xsize=(*pstate).width, ysize=(*pstate).height, /pixmap
				(*pstate).pix = !d.window
				goto, update
			endif
			end
	
		'draw': begin
			end
	
		'array-mode': begin
			n = n_elements(*(*pars).detector_list)
			if n lt 1 then goto, finish
			detector_update, /array, present=(*(*pars).detector_list)[event.index], file=f
			(*pars).detector_mode = event.index
			detector = read_detector( f, error=error)
			if error then begin
				warning, 'detector_map_event','Error reading Detectors file '+f, /error
				goto, finish
			endif else begin
				*(*pars).pdetector = *detector
				d = read_detector_layout( (*(*pars).pdetector).layout, error=error)
				if error eq 0 then begin
					play = (*pars).playout
					*play = d
					if ptr_good(play,/struct) eq 0 then begin
						warning,'detector_map_event','No detector layout found for selected array.'
						return
					endif
				endif
			endelse
			goto, update
			end

		'rgamma-norm': begin
			case event.value of
				0: begin
					(*pars).rGamma_norm = event.select
					end
				1: begin
					(*pars).Geom_true = event.select
					end
				else:
			endcase
			goto, update
			end

		'add-button': begin
			F = file_requester( /read, filter = '*.dam*', path=*(*pstate).path, group=event.top, $
					title='Select a DA matrix file to add to list', /fix_filter)
			if F ne '' then begin
				da = read_da( F, error=err)
				if err then goto, finish

				*(*pstate).path = extract_path(F)
				*(*pars).pfile = [ *(*pars).pfile, F]
				*(*pars).pmatrix = [ *(*pars).pmatrix, strip_path( strip_file_ext(F))]
				q = where( strmid(*(*pars).pfile,0,3) ne 'Add', nq)
				if nq gt 0 then begin
					*(*pars).pfile = (*(*pars).pfile)[q]
					*(*pars).pmatrix = (*(*pars).pmatrix)[q]
					(*pars).matrix_index = nq-1
					*(*pars).pda = da
				endif
				q = where( strmid(*(*pars).pelement,0,1) ne '?', nq)
				if nq gt 0 then begin
					*(*pars).pelement = da.el
				endif
				(*pars).element_index = 0
				widget_control, (*pstate).matrix_id, set_value=*(*pars).pmatrix
				widget_control, (*pstate).matrix_id, set_combobox_select = (*pars).matrix_index
				widget_control, (*pstate).element_id, set_value=*(*pars).pelement
				widget_control, (*pstate).element_id, set_combobox_select = (*pars).element_index

				if (n_elements(*(*pars).pmatrix) eq 1) and ((*pars).matrix_index eq 0) then begin
					F = (*(*pars).pfile)[ (*pars).matrix_index]
					da = read_da( F, error=err)
					*(*pars).pda = da
					if err then goto, finish
					*(*pars).pelement = da.el
					widget_control, (*pstate).element_id, set_value=*(*pars).pelement
					widget_control, (*pstate).element_id, set_combobox_select = (*pars).element_index
				endif

				if ptr_good((*pars).pdetector) eq 0 then begin
					detector_update, /array, present=(*(*pars).detector_list)[(*pars).detector_mode], file=f
					detector = read_detector( f, error=error)
					if error then begin
						warning, 'detector_map_event','Error reading Detector file '+f, /error
						goto, finish
					endif else begin
						*(*pars).pdetector = *detector
						d = read_detector_layout( (*(*pars).pdetector).layout, error=error)
						if error eq 0 then begin
							play = (*pars).playout
							*play = d
							if ptr_good(play,/struct) eq 0 then begin
								warning,'detector_map_event','No detector layout found for selected array.'
								return
							endif
						endif
					endelse
				endif
				goto, update
			endif
			end

		'select-matrix': begin
			(*pars).matrix_index = event.index
			F = (*(*pars).pfile)[ (*pars).matrix_index]
			da = read_da( F, error=err)
			*(*pars).pda = da
			if err then goto, finish
			el = (*(*pars).pelement)[(*pars).element_index]
			q = where( el eq da.el, nq)
			(*pars).element_index = (nq ge 1) ? q[0] : 0
			*(*pars).pelement = da.el
			widget_control, (*pstate).element_id, set_value=*(*pars).pelement
			widget_control, (*pstate).element_id, set_combobox_select = (*pars).element_index
			goto, update
			end

		'select-element': begin
			(*pars).element_index = event.index
			goto, update
			end

		else:
	endcase

finish:
	return

;	Use DA matrix *(*pars).da and selected element (*(*pars).pelement)[(*pars).element_index]
;	to process all spectra (*(*pstate).p))[*] to obtain an array of signal to plot on detector array.
;	Need to map spectra to DA element row based on E cals.

update:
	detector_map_draw, pstate
	goto, finish

bad_state:
	warning,'detector_map_event',['STATE variable has become ill-defined.','Abort filter Setup.'],/error
	goto, kill
bad_ptr:
	warning,'detector_map_event',['Parameter structure variable has become ill-defined.','Abort filter Setup.'],/error
	goto, kill

kill:
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	wdelete, (*pstate).pix

die:
	widget_control, event.top, /destroy
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_map_draw, wWidget

	COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate
	
	widget_control, wWidget, get_value=wid
	(*pstate).wid = wid
	(*pstate).draw = wWidget
	
	window, /free, xsize=(*pstate).width, ysize=(*pstate).height, /pixmap
	(*pstate).pix = !d.window
	
	detector_map_draw, pstate
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_map_element, wWidget

	COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate
	
	pars = (*pstate).pars
	if ptr_good(pars) eq 0 then return
	
	widget_control, wWidget, set_combobox_select = (*pars).element_index
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_map_matrix, wWidget

	COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate
	
	pars = (*pstate).pars
	if ptr_good(pars) eq 0 then return
	
	widget_control, wWidget, set_combobox_select = (*pars).matrix_index
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_map_detector, wWidget

	COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate
	
	pars = (*pstate).pars
	if ptr_good(pars) eq 0 then return
	
	widget_control, wWidget, set_combobox_select = (*pars).detector_mode
	return
end

;------------------------------------------------------------------------------------------

pro detector_map_draw, pstate

	COMPILE_OPT STRICTARR
	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
	pars = (*pstate).pars

	if ptr_good((*pars).pda) eq 0 then return
	da = *(*pars).pda
	if ptr_good((*pstate).p) eq 0 then return
	p = *(*pstate).p
	if ptr_good((*pars).pdetector) eq 0 then return

	n = clip( n_elements(p), 1,384)
	nd = (*(*pars).playout).n
	id = indgen(nd) + (*(*pars).playout).start
	signal = fltarr(nd + (*(*pars).playout).start)
	k = (*pars).element_index
	cda = da.cal.a
	cdb = da.cal.b
	x = indgen( da.size)
	e = cda*x + cdb
	for i=0,n-1 do begin
		ichan = -1
		s = strsplit( (*p[i]).label, ' ', /extract, count=ns)	; find "label 0/E" form
		if ns ge 2 then begin
			m = locate( '/E', s[ns-1])
			if m ge 1 then begin
				ichan = fix( strmid(s[ns-1], 0, m))
			endif
		endif
		m = locate( 'Detector', (*p[i]).label)				; find "Detector #0" form
		if m ge 0 then begin
			m2 = strlen('Detector')
			s = strmid( (*p[i]).label, m+m2)
			ichan = fix(s)
		endif
		if (ichan-(*(*pars).playout).start lt 0) or (ichan-(*(*pars).playout).start ge nd) then continue

		a = (*p[i]).cal.poly[1]
		b = (*p[i]).cal.poly[0]
		j = round( (e-b)/a)
		q = where( (j ge 0) and (j lt (*p[i]).size), nq)
		if nq eq 0 then continue
		if ichan-(*(*pars).playout).start ge n_elements( da.array.rgamma[*,0]) then continue
		y = da.matrix[x[q],k] * (*(*p[i]).data)[j[q]]
		if (*pars).rGamma_norm then y = y / da.array.rgamma[ichan-(*(*pars).playout).start,k]
		signal[ichan] = total( y )

;		if ichan eq 58 then begin
;			top = max( (*(*p[i]).data)[j])
;			scale = top / max( da.matrix[x[q],k])
;			wset, 0
;			!x.title = 'Energy (keV)'
;			!y.title = 'Counts per channel'
;			!p.title = 'Spectrum #58 (red) versus DA matrix row for ' + (*(*pars).pelement)[ (*pars).element_index]
;			plot, e[q], scale*da.matrix[x[q],k]
;			oplot, e[q], (*(*p[i]).data)[j] , color=spec_colour('red')
;		endif
	endfor
	if (*(*pars).playout).start gt 0 then signal=signal[(*(*pars).playout).start:*]

;	wset, (*pstate).wid
	wset, (*pstate).pix
	ymax = image_weighted_max( signal)
	plot_maia_parameter, id, signal, /white, title='Element "'+(*(*pars).pelement)[ (*pars).element_index]+'" using DA "'+(*(*pars).pmatrix)[ (*pars).matrix_index]+'"', $
			min=0.0, max=ymax, /screen, layout=(*(*pars).pdetector).layout, /wset_done, true=(*pars).Geom_true, only_maia=0

	wset, (*pstate).wid
	device, copy=[0,0, (*pstate).width,(*pstate).height, 0,0,(*pstate).pix]
	return

bad_state:
	warning,'detector_map_draw',['STATE variable has become ill-defined.'],/error
	return
bad_ptr:
	warning,'detector_map_draw',['Parameter structure variable has become ill-defined.'],/error
	return
end

;------------------------------------------------------------------------------------------

; Detector array plot showing intensity extracted from spectra using DA/Cuts matrices

pro detector_map, p, group=group, TLB=tlb, path=path, pars=pars

	COMPILE_OPT STRICTARR
	common c_working_dir, geopixe_root
	common c_errors_1, catch_errors_on
	if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
	ErrorNo = 0
	if catch_errors_on then begin
		Catch, ErrorNo
		if (ErrorNo ne 0) then begin
			Catch, /cancel
			on_error, 1
			help, calls = s
			n = n_elements(s)
			c = 'Call stack: '
			if n gt 2 then c = [c, s[1:n-2]]
			warning,'detector_map',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif else on_error,0
	if n_elements(group) lt 1 then group=0L
	if n_elements(path) lt 1 then path=''
	if ptr_good(p) eq 0 then return

	pars = bad_pars_struct( pars, make_pars=make_pars)
	
	if make_pars then begin
		detector_update,  list=detector_list, title=detector_title, /array
		good = intarr(n_elements(detector_list))
		for i=0,n_elements(detector_list)-1 do begin
			detector_update, /array, present=detector_list[i], new=j, file=f
			detector = read_detector( f, error=error)
			if error then continue
			d = read_detector_layout( (*detector).layout, maia=maia_like, error=error)
			ptr_free, detector
			if error or (maia_like eq 0) then continue
			good[i] = 1
		endfor
		q = where( good eq 1, nq)
		if nq eq 0 then begin
			warning,'detector_map','No valid array detectors found in your installation.'
			return
		endif
		detector_list = detector_list[q]
		detector_title = detector_title[q]
		matrix = ['Add a DA matrix -->']
		element = ['?']

		pt = {	$
			pmatrix:		ptr_new( matrix), $			; matrix label list
			pfile:			ptr_new( matrix), $			; matrix file list
			pelement:		ptr_new( element), $		; elements
			matrix_index: 	0, $						; index into DA list
			element_index: 	0, $						; index into element list
			pda:			ptr_new( /alloc), $			; current DA
			detector_list: 	ptr_new(detector_list), $	; list of detector files
			detector_title: ptr_new(detector_title), $	; list of detectors
			detector_mode: 	0, $						; current detector
			pdetector:		ptr_new(/alloc), $			; detector struct
			rGamma_norm:	0, $						; normalize out rGamma variation across array
			Geom_true:		0, $						; show true sizes of detectors
			playout:		ptr_new(/alloc) $			; current layout struct
		}
		*pars = pt
	endif

	case !version.os_family of
		'MacOS': begin
			xw = 810
			yh = 900
			draw_trim = 15
			scr_trim = 21
			DA_size = 300
			end
		'unix': begin
			xw = 810
			yh = 900
			draw_trim = 0
			scr_trim = 15
			DA_size = 300
			end
		else: begin
			xw = 810
			yh = 900
			draw_trim = 0
			scr_trim = 15
			DA_size = 300
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
		screen = get_screen_size()
		xoffset = ((xoff - xw) < (screen[0] - xw)) > 0
	endif
	if n_elements(yoffset) lt 1 then begin
		screen = get_screen_size()
		yoffset = ((yoff - yh+h) < (screen[1] - yh)) > 0
	endif
	help_text = 'Display the projection (matrix multiply) of each detector spectrum using the selected ' + $
				'DA matrix and selected element row of the matrix. Select the correct "Array Detector" layout first. Use "Add" to add DA matrices to the list ' + $
				'and select the desired "Element" row of the DA matrix.'
	
	tlb = widget_base( /column, title='Detector Map', /TLB_KILL_REQUEST_EVENTS, $
			group_leader=group, _extra=extra, uname='detector_map_tlb', space=1, $
			/tlb_size_events, xoffset=xoffset, yoffset=yoffset, xpad=1, /base_align_center)

	base1 = Widget_Base(tlb, UNAME='detector_map_Base', SPACE=1, XPAD=1, YPAD=1, /column)

	dw = 800
	dh = 760
	draw = Widget_Draw( base1, UNAME='detector_map_draw',  $
			NOTIFY_REALIZE='OnRealize_detector_map_draw', $		;KILL_NOTIFY='OnDestroy_detector_map_draw', $
			XSIZE=dw+draw_trim, YSIZE=dh+draw_trim, RETAIN=retain, /BUTTON_EVENTS)
			;,/VIEWPORT_EVENTS, /SCROLL)

	array_base = widget_base( base1, /row, /base_align_center, ypad=0, xpad=1, space=4)
	label = widget_label( array_base, value='Select Array Detector layout:')
	detector_id = widget_combobox( array_base, value=*(*pars).detector_title, uname='array-mode', xsize=240, /tracking, $
					notify_realize='OnRealize_detector_map_detector', $
					uvalue='Select an available Array Detector layout which is appropriate to the collection of individual detector spectra loaded. ')

	lab = widget_label( array_base, value=' ', scr_xsize=20)
	label = widget_label( array_base, value='Detector array options:')
	rgamma = cw_bgroup2( array_base, ['rGamma norm','True masked size'], /row, set_value=[(*pars).rGamma_norm, (*pars).Geom_true], /return_index, uname='rgamma-norm',/ nonexclusive, /tracking, $
					uvalue=['Enable correction of the detector map for the "rGamma" Relative Efficiency Factors.', $
							'Show true effective sizes of detector cells after masking.'], xpad=0, ypad=0, space=1)

	row1 = Widget_Base(base1, UNAME='button_row_base', SPACE=4, XPAD=1, YPAD=0, /row, /base_align_center)

	lab = widget_label( row1, value='Matrix:')
	matrix_id = widget_combobox( row1, value=*(*pars).pmatrix, uname='select-matrix', xsize=DA_size, /tracking, $
			notify_realize='OnRealize_detector_map_matrix', $
			uvalue='Select a DA matrix to apply to spectral data. Use "Add" to load new ones.')

	button = widget_button( row1, value='Add', uname='add-button', /tracking, $
			uvalue='Add a new DA matrix to the list.', scr_xsize=50)

	lab = widget_label( row1, value=' ', scr_xsize=20)
	lab = widget_label( row1, value='Element:')
	element_id = widget_combobox( row1, value=*(*pars).pelement, uname='select-element', xsize=70, /tracking, $
			notify_realize='OnRealize_detector_map_element', $
			uvalue='Select an element from the DA matrix. Use "Add" to load a new DA matrix.')

	help = widget_text( tlb, scr_xsize=xw, ysize=2, /wrap, uname='help', /tracking, value=help_text, $
			uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
			frame=0)

	state = { $
		p:			p, $					; spectrum display 'p'
		path:		ptr_new( path), $		; default file path
		pars:		pars, $					; parameters struct

		draw:		draw, $					; ID of draw widget
		wid:		0, $					; draw ID
		pix:		0, $					; background pixmap ID
		width:		dw, $					; draw width
		height:		dh, $					; draw height
		help_text:	help_text, $			; default help text
		matrix_id:	matrix_id, $			; matrix droplist ID
		element_id: element_id, $			; element droplist ID
		detector_id:detector_id, $			; array detector ID
		help:		help $					; ID of help text
	}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize
	
	register_notify, tlb, ['path', $				; new path
							'spectra-changed'], $	; spectra changed or loaded
;							'spectra'], $			; new spectra (others?)
			from=group
	
	xmanager, 'detector_map', tlb, /no_block
	return
end

	