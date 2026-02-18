pro identify2_event, event

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
		warning,'identify2_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
  widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'new-detectors': begin
				present = (*(*pstate).detector_list)[(*pstate).detector_mode]
				detector_update, list=list, title=title, present=present, new=new, file=f
				list = ['null',list]
				title = ['---   none   ---', title]
				*(*pstate).detector_list = list
				(*pstate).detector_mode = new+1
				widget_control, (*pstate).detector_id, set_value=title, set_combobox_select=(new+1)
				goto, finish
				end
			'new-filters': begin
				present = (*(*pstate).filter_list)[(*pstate).filter_mode]
				filter_update, list=list, title=title, present=present, new=new
				list = ['null',list]
				title = ['---   none   ---', title]
				widget_control, (*pstate).filter_id, set_value=title, set_combobox_select=(new+1)
				*(*pstate).filter_list = list
				goto, finish
				end
			'identify-e': begin
				if ptr_valid( event.pointer) then begin
					t = *(event.pointer)
					r = [0.001,1.0,1000.,1000000.,1.0]
					q = where( strupcase(t.units) eq ['EV','KEV','MEV','GEV','ENERGY'])
					scale = 1.0
					if q[0] ne -1 then scale=r[q[0]]
					t.e = scale * t.e
					t.units = 'keV'
					*((*pstate).pe) = t
;					print, 'Energy = ',t.e,'  ',t.units

					n = bin_search( t.e, -1,-1)
					g = widget_info( (*pstate).list,/geometry)
					l = g.scr_ysize / (*pstate).row_height
					nt = round(((n-(l/2)+1) < (n-3)) > 0)
;					print,'select row=',n,' set view to=',nt
					widget_control, (*pstate).list, set_table_select=[0,n,4,n]
					widget_control, (*pstate).list, set_table_view=[0,nt]
				endif
				goto, finish
				end
			'identify-element': begin
				(*pstate).mode = 1
				widget_control, (*pstate).toggle, set_value=1
				widget_control, (*pstate).ptable, set_value={SHOW: 1}
				widget_control, (*pstate).help, scr_xsize=(*pstate).whelp[(*pstate).mode]
;				geom = widget_info( b2, /geometry)
				widget_control, (*pstate).base1, map = 0
				widget_control, (*pstate).base2, map = 1

				widget_control, (*pstate).base1, scr_xsize = 1
				widget_control, (*pstate).base1, scr_ysize = 1
				widget_control, (*pstate).base2, scr_xsize = (*pstate).base2_xsize
				widget_control, (*pstate).base2, scr_ysize = (*pstate).base2_ysize
				goto, finish
				end
			'identify-line': begin
				(*pstate).mode = 0
				widget_control, (*pstate).toggle, set_value=0
				widget_control, (*pstate).ptable, set_value={SHOW: 0}
				widget_control, (*pstate).help, scr_xsize=(*pstate).whelp[(*pstate).mode]
;				geom = widget_info( b2, /geometry)
				widget_control, (*pstate).base2, map = 0
				widget_control, (*pstate).base1, map = 1

				widget_control, (*pstate).base2, scr_xsize = 1
				widget_control, (*pstate).base2, scr_ysize = 1
				widget_control, (*pstate).base1, scr_xsize = (*pstate).base1_xsize
				widget_control, (*pstate).base1, scr_ysize = (*pstate).base1_ysize
				goto, finish
				end
			else: begin
				goto, finish
				end
		endcase
		end
	'WIDGET_TRACKING': begin
		widget_control, event.id, get_uvalue=s
		if size(s,/tname) eq 'STRING' then begin
			if event.enter eq 1 then begin
				widget_control, (*pstate).help, set_value=s
			endif else begin
				if (*pstate).mode eq 0 then begin
					widget_control, (*pstate).help, set_value='Click on a line to mark it on the spectrum. ' + $
							'Adjust lines visible with Threshold slider.'
				endif else begin
					widget_control, (*pstate).help, set_value='LEFT click an element to mark its lines on the spectrum. ' + $
							'Use "Filter", "Detector" to modify relative intensities. ' + $
							'RIGHT click will enable the element in "X-ray Spectrum Fit". '
				endelse
			endelse
		endif
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		goto, kill
		end
	else:
endcase

case !version.os_family of
	'MacOS': begin
		small_table_x = 215
		small_table_y = 141				; 190
		large_table_x = 453
		large_table_y = 215				; 240
		xoff = 12
		yoff = 91						; 125
		table_minx = 220
		table_miny = 145
		width_off = 10
		height_off = 32					; 52
		large_slider_width = 220
		small_slider_width = 70
		large_drop_width = 160
		small_drop_width = 60
		end
	'unix': begin
		small_table_x = 215
		small_table_y = 95				; 144
		large_table_x = 453
		large_table_y = 195				; 220
		xoff = 12
		yoff = 106						; 140
		table_minx = 190
		table_miny = 145
		width_off = 10
		height_off = 43					; 63
		large_slider_width = 220
		small_slider_width = 70
		large_drop_width = 160
		small_drop_width = 60
		end
	else: begin
		small_table_x = 215
		small_table_y = 141				; 190
		large_table_x = 453
		large_table_y = 215				; 240
		xoff = 10
		yoff = 91						; 125
		table_minx = 220
		table_miny = 145
		width_off = 10
		height_off = 32					; 52
		large_slider_width = 220
		small_slider_width = 70
		large_drop_width = 160
		small_drop_width = 60
		end
endcase

uname = widget_info( event.id, /uname)
case uname of
	'identify2_TLB': begin					; resize
;		print,event.x,event.y
		if (*pstate).mode eq 0 then begin
			w = (event.x - xoff) > table_minx
			h = (event.y - yoff) > table_miny
;			print,w,h
			widget_control, (*pstate).list, scr_xsize=w, scr_ysize=h
			widget_control, (*pstate).thresh, scr_xsize=w+12
			widget_control, (*pstate).help, scr_xsize=w+6-19
			(*pstate).whelp[0] = w+6

			widget_control, (*pstate).base1, scr_xsize=w+width_off
			widget_control, (*pstate).base1, scr_ysize=h+height_off
			(*pstate).base1_xsize = w+width_off
			(*pstate).base1_ysize = h+height_off
		endif else begin
			w = (event.x - xoff) > table_minx
			h = (event.y - yoff) > table_miny
			w2 = (*pstate).base2_xsize
			h2 = (*pstate).base2_ysize
			if (w gt (3*large_table_x)/4) then begin
				if ((*pstate).tiny eq 1) then begin
					w2 = large_table_x
					h2 = large_table_y
					widget_control, (*pstate).ptable, set_value={TINY: 0}
					(*pstate).tiny = 0
					widget_control, (*pstate).help, scr_xsize=w2
					(*pstate).whelp[1] = w2
					(*pstate).wslider = [large_slider_width,large_drop_width]
					widget_control, (*pstate).filter_base, map=1, scr_ysize=(*pstate).filter_base_ysize
				endif
			endif
			if (w le (large_table_x)/2) then begin
				if ((*pstate).tiny eq 0) then begin
					w2 = small_table_x
					h2 = small_table_y
					widget_control, (*pstate).ptable, set_value={TINY: 1}
					(*pstate).tiny = 1
					widget_control, (*pstate).help, scr_xsize=w2
					(*pstate).whelp[1] = w2
					(*pstate).wslider = [small_slider_width,small_drop_width]
					widget_control, (*pstate).filter_base, map=0, scr_ysize=1
				endif
			endif
;			geom = widget_info((*pstate).base2,/geometry)
			widget_control, (*pstate).base2, scr_xsize=w2
			widget_control, (*pstate).base2, scr_ysize=h2
			widget_control, (*pstate).zslide, scr_xsize=(*pstate).wslider[1]
			widget_control, (*pstate).filter_id, xsize=(*pstate).wslider[0]
			widget_control, (*pstate).detector_id, xsize=(*pstate).wslider[0]
			(*pstate).base2_xsize = w2
			(*pstate).base2_ysize = h2
		endelse
		end
	'TOGGLE': begin
		(*pstate).mode = event.value
		if event.value eq 1 then begin
			widget_control, (*pstate).ptable, set_value={SHOW: 1}
			b1 = (*pstate).base1
			b2 = (*pstate).base2
			x2 = (*pstate).base2_xsize
			y2 = (*pstate).base2_ysize
		endif else begin
			widget_control, (*pstate).ptable, set_value={SHOW: 0}
			b1 = (*pstate).base2
			b2 = (*pstate).base1
			x2 = (*pstate).base1_xsize
			y2 = (*pstate).base1_ysize
		endelse
		widget_control, (*pstate).help, scr_xsize=(*pstate).whelp[(*pstate).mode]
		widget_control, (*pstate).thresh, scr_xsize=(*pstate).whelp[(*pstate).mode] + 6
		widget_control, b1, map = 0
		widget_control, b2, map = 1

		widget_control, b1, scr_xsize = 100
		widget_control, b1, scr_ysize = 100
		widget_control, b2, scr_xsize = x2
		widget_control, b2, scr_ysize = y2
		end
	'PERIODIC': begin
		case tag_names( event, /structure) of
			'PERIODIC': begin
				identify2_select, pstate, event.z
				widget_control, (*pstate).element, set_value=event.element
				notify, 'mark-element', (*pstate).pz, from=event.top
				widget_control, (*pstate).zslide, set_value=event.z

				q = where (  (*((*pstate).xlist)).el eq element_name(event.z) )
				if q[0] ne -1 then begin
					nt = n_elements( (*((*pstate).xlist)).e)
					back = widget_info( (*pstate).list, /table_background_color)
					colour = bytarr(3,5,nt)
					colour[0,*,*] = back[0] & colour[1,*,*] = back[1] & colour[2,*,*] = back[2]
					colour[0,*,q] = 241 & colour[1,*,q] = 255 & colour[2,*,q] = 59
					widget_control, (*pstate).list, background_color=colour
				endif

				if event.alt then begin
					notify, 'mark-fit', (*pstate).pz, from=event.top
				endif
				end
		endcase
		end
	'OPTIONS': begin
		(*pstate).shells[event.value] = event.select
		if (*((*pstate).pz)).z lt 1 then goto, finish

		identify2_select, pstate, (*((*pstate).pz)).z
		notify, 'mark-element', (*pstate).pz, from=event.top
		end
	'THRESHOLD': begin
		s = xsort_list( event.value, list=xlist, /table)
		set_bin_search, xlist
		*((*pstate).xlist) = xlist
		widget_control, (*pstate).list, set_value=s
		if ptr_valid((*pstate).pe) then begin
			nt = n_elements( (*((*pstate).xlist)).e)
			back = widget_info( (*pstate).list, /table_background_color)
			colour = bytarr(3,5,nt)
			colour[0,*,*] = back[0] & colour[1,*,*] = back[1] & colour[2,*,*] = back[2]
			widget_control, (*pstate).list, background_color=colour

			n = bin_search( (*((*pstate).pe)).e, -1,-1)		; assumes "keV"
			g = widget_info( (*pstate).list,/geometry)
			l = g.scr_ysize / (*pstate).row_height
			nt = round(((n-(l/2)+1) < (n-3)) > 0)

			widget_control, (*pstate).list, set_table_select=[0,n,4,n]
			widget_control, (*pstate).list, set_table_view=[0,nt]
		endif
		end
	'LIST': begin
		case tag_names( event, /structure_name) of

			'WIDGET_TABLE_CELL_SEL': begin
				if event.sel_top ge 0 then begin
					n = event.sel_top
					(*((*pstate).pe)).e = (*((*pstate).xlist)).e[n]
					(*((*pstate).pe)).units = 'keV'
					notify, 'mark-e', (*pstate).pe, from=event.top
					view = widget_info( (*pstate).list, /table_view)
					widget_control, (*pstate).list, set_table_select=[0,n,4,n]
					q = where (  (*((*pstate).xlist)).el eq (*((*pstate).xlist)).el[n] )
					if q[0] ne -1 then begin
						nt = n_elements( (*((*pstate).xlist)).e)
						back = widget_info( (*pstate).list, /table_background_color)
						colour = bytarr(3,5,nt)
						colour[0,*,*] = back[0] & colour[1,*,*] = back[1] & colour[2,*,*] = back[2]
						colour[0,*,q] = 241 & colour[1,*,q] = 255 & colour[2,*,q] = 59
						widget_control, (*pstate).list, background_color=colour
					endif
					widget_control, (*pstate).list, set_table_view=view
				endif
				end

			'WIDGET_TABLE_CH': begin
				end
			else:
		endcase
		end
	'ZSLIDE': begin
		identify2_select, pstate, event.value
		notify, 'mark-element', (*pstate).pz, from=event.top
		widget_control, (*pstate).element, set_value=element_name(event.value)
		widget_control, (*pstate).ptable, set_value={Z:event.value, STATE:1}
		end
	'ELEMENT': begin
		widget_control, (*pstate).element, get_value=s
		z = atomic_number(s)
		if z[0] ge 1 then begin
			identify2_select, pstate, z
			widget_control, (*pstate).element, set_value=element_name((*((*pstate).pz)).z)
			notify, 'mark-element', (*pstate).pz, from=event.top
			widget_control, (*pstate).ptable, set_value={Z:z[0], STATE:1}
			widget_control, (*pstate).zslide, set_value=z[0]
		endif else begin
			widget_control, (*pstate).element, set_value=' '
			widget_control, (*pstate).ptable, set_value={Z:1, STATE:1}
			widget_control, (*pstate).zslide, set_value=1
		endelse
		end
	'filter-mode': begin
		(*pstate).filter_mode = event.index
		if event.index eq 0 then begin
			filters = ptr_new( make_filter('C'))
			error = 0
		endif else begin
			filter_update, present=(*(*pstate).filter_list)[(*pstate).filter_mode], new=i, file=f
			filters = read_filters( f, error=error)
		endelse
		if error then begin
			warning, 'identify2_event','Error reading Filter file '+f, /error
			goto, finish
		endif else begin
			if ptr_valid((*pstate).filters) then ptr_free, (*pstate).filters
			(*pstate).filters = filters

			identify2_select, pstate, (*((*pstate).pz)).z
			widget_control, (*pstate).element, set_value=element_name((*((*pstate).pz)).z)
			notify, 'mark-element', (*pstate).pz, from=event.top
		endelse
		end
	'detector-mode': begin
		(*pstate).detector_mode = event.index
		if event.index eq 0 then begin
			detector = ptr_new( make_detector('U'))
			(*detector).crystal.name = 'Inf'
			(*detector).crystal.Z[0] = 100
			error = 0
		endif else begin
			detector_update, present=(*(*pstate).detector_list)[(*pstate).detector_mode], new=i, file=f
			detector = read_detector( f, error=error)
		endelse
		if error then begin
			warning, 'identify2_event','Error reading Detector file '+f, /error
			goto, finish
		endif else begin
			if ptr_valid((*pstate).detector) then ptr_free, (*pstate).detector
			(*pstate).detector = detector

			identify2_select, pstate, (*((*pstate).pz)).z
			widget_control, (*pstate).element, set_value=element_name((*((*pstate).pz)).z)
			notify, 'mark-element', (*pstate).pz, from=event.top
		endelse
		end
	'query-button':begin
		geopixe_browser, 'Help/GeoPIXE-Users-Guide.htm', title='GeoPIXE Users Guide', group=event.top, key='X-ray Identification Window'
		end
	else:
endcase

finish:
	widget_control, hourglass=0
	return

bad_state:
	warning,'identify2',['STATE variable has become ill-defined.','Abort identify2.'],/error
	goto, kill

kill:
	print,'Kill identify2 ...'
	cancel_notify, event.top

	if n_elements(pstate) lt 1 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid((*pstate).pe) then ptr_free, (*pstate).pe
	if ptr_valid((*pstate).pz) then begin
		if ptr_valid((*((*pstate).pz)).e) then ptr_free, (*((*pstate).pz)).e
		if ptr_valid((*((*pstate).pz)).shell) then ptr_free, (*((*pstate).pz)).shell
		if ptr_valid((*((*pstate).pz)).rel) then ptr_free, (*((*pstate).pz)).rel
		ptr_free, (*pstate).pz
	endif
	if ptr_valid((*pstate).xlist) then ptr_free, (*pstate).xlist

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;-----------------------------------------------------------

pro identify2_select, pstate, z

	if z[0] lt 1 then return
	(*((*pstate).pz)).z = z[0]
	e = 0.0
	rel = 0.0
	shell = 0
	if (*pstate).shells[0] then begin
		e = all_lines( z[0], /K, rel=rel)
		shell = replicate(0, n_elements(e))
		e = [e, edge(z[0],1)]					; add K edge
		shell = [shell,-10]
		rel = [rel, 0.01]
	endif
	if (*pstate).shells[1] then begin
		e2 = all_lines( z[0], /L, rel=rel2)
		if n_elements(rel2) gt 0 then begin
			e = [e, e2]
			rel = [rel, rel2]
			shell = [shell, replicate(1, n_elements(e2))]
			for k=2,4 do begin
				e = [e, edge(z[0],k)]			; add L edges
				shell = [shell,-1]
				rel = [rel, 0.01]
			endfor
		endif
	endif
	if (*pstate).shells[2] then begin
		e2 = all_lines( z[0], /M, rel=rel2)
		if n_elements(rel2) gt 0 then begin
			e = [e, e2]
			rel = [rel, rel2]
			shell = [shell, replicate(2, n_elements(e2))]
			for k=5,9 do begin
				e = [e, edge(z[0],k)]			; add M1 edge
				shell = [shell,-2]
				rel = [rel, 0.01]
			endfor
		endif
	endif
	if n_elements(rel) gt 1 then begin
		*((*((*pstate).pz)).e) = e
		*((*((*pstate).pz)).rel) = rel
		*((*((*pstate).pz)).shell) = shell

		identify2_filter, (*pstate).filters, (*pstate).detector, (*(*pstate).pz).e, (*(*pstate).pz).rel, (*((*pstate).pz)).shell
	endif

	widget_control, (*pstate).zslide, set_value=z[0]
	widget_control, (*pstate).ptable, set_value={Z:z[0], STATE:1}

	return
end

;-----------------------------------------------------------

pro identify2_filter, pf, pd, pe, pr, ps

	if ptr_valid(pf) eq 0 then return
	if ptr_valid(pd) eq 0 then return
	if ptr_valid(pe) eq 0 then return
	if ptr_valid(pr) eq 0 then return
	if ptr_valid(ps) eq 0 then return
	if n_elements(*pe) lt 1 then return
	if n_elements(*pe) ne n_elements(*pr) then return

;	t = transmit( pf, *pe, /photo) * det_eff( pd, *pe)

	t = det_eff( pd, *pe, external_filters=pf, /photo)

	q = where(t gt 1.0e-20)
;	print,*pe,*pr,*ps,t

	if q[0] eq -1 then return
	q = where(t gt 1.0e-30)
	t = t[q]
	*pe = (*pe)[q]
	*pr = (*pr)[q]
	*ps = (*ps)[q]

	for i=0L,2 do begin
		qs = where( *ps eq i)
		if qs[0] ne -1 then begin
			q = reverse(sort( (*pr)[qs] ))
			(*pr)[qs] = ( (*pr)[qs] * t[qs]) / ( (*pr)[qs[q[0]]] * t[qs[q[0]]])
		endif
	endfor
	return
end

;-----------------------------------------------------------

pro OnRealize_identify2_filter_base, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).filter_base, /geometry)
(*pstate).filter_base_ysize = geo.ysize
end

;-----------------------------------------------------------

pro OnRealize_identify2_base1, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).base1, /geometry)
(*pstate).base1_xsize = geo.xsize
(*pstate).base1_ysize = geo.ysize
end

;------------------------------------------------------------------------------------------

pro OnRealize_identify2_Table, wWidget

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

w = widget_info( wWidget, /row_heights)
geom = widget_info( wWidget, /geometry)
tlb_geom = widget_info( top, /geometry)

(*pstate).row_height = w[0]
;(*pstate).yoffset = tlb_geom.ysize - geom.scr_ysize + 2
;(*pstate).xoffset = tlb_geom.xsize - geom.scr_xsize
end

;------------------------------------------------------------------------------------------

pro identify2, group_leader=group, TLB=tlb, _extra=extra, xoffset=xoffset, yoffset=yoffset

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'identify2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(geopixe_root) lt 1 then startupp

filter_update, list=filter_list, title=filter_title
detector_update,  list=detector_list, title=detector_title

filter_list = ['null',filter_list]
filters = ptr_new( make_filter('C'))
filter_title = ['---   none   ---', filter_title]

detector_list = ['null',detector_list]
detector = ptr_new( make_detector('U'))
(*detector).crystal.name = 'Inf'
(*detector).crystal.Z[0] = 100
detector_title = ['---   none   ---',detector_title]

  case !version.os_family of
	'MacOS': begin
		fnt = 'COURIER*BOLD*10'
		large_table_x = 453
		large_table_y = 240
		large_slider_width = 220
		large_drop_width = 160
		large_drop_width2 = 160
		space5 = 2
		space10 = 5
		texty = 21
		list_xsize = 280
		xw = 0
		ysize_help = 3
		end
	'unix': begin
		fnt = '6x10'
		large_table_x = 453
		large_table_y = 220
		large_slider_width = 220
		large_drop_width = 160
		large_drop_width2 = 170
		space5 = 1
		space10 = 0
		texty = 29
		list_xsize = 270
		xw = 5
		ysize_help = 3
		end
	else: begin
		fnt = 'COURIER*10'
		large_table_x = 453
		large_table_y = 214
		large_slider_width = 220
		large_drop_width = 160
		large_drop_width2 = 160
		space5 = 5
		space10 = 8
		texty = 21
		list_xsize = 281
		xw = 0
		ysize_help = 2
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
	xoffset = ((xoff + w + xw) < (screen[0]-34 - 237)) > 0
endif
if n_elements(yoffset) lt 1 then begin
	screen = get_screen_size()
	yoffset = ((yoff - 159) < (screen[1]-28 - 342)) > 0
endif

tlb = widget_base( /column, title='Line Identification', /TLB_KILL_REQUEST_EVENTS, $
					/TLB_SIZE_EVENTS, /base_align_center, xpad=0, ypad=0, space=0, $
					group_leader=group, _extra=extra, uname='identify2_TLB', xoffset=xoffset, yoffset=yoffset)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=0, /base_align_center)

toggle = cw_bgroup2( tbase, ['Identify','Mark Element'], /row, $
					/exclusive, /no_release, /return_index, /tracking, $
					uname='TOGGLE', set_value=0, xpad=0, ypad=0, space=3, $
					uvalue='Select line identification or mark element lines')
bbbase = widget_base( tlb, xpad=0, ypad=0, space=0)


base1 = widget_base( bbbase, /column, map=1, /base_align_center, xpad=0, ypad=0, space=1, $
				notify_realize='OnRealize_identify2_base1')

s = xsort_list( 0.0001, list=xlist, /table)
set_bin_search,  xlist

;list = widget_list( base1, value=s, uname='LIST', /tracking, $
;					uvalue='Sorted X-ray list (energy, Siegbahn/IUPAC id, relative intensity). Click a line to mark it on the spectrum.', $
;					font=fnt, scr_xsize=list_xsize, scr_ysize=200)

list = Widget_Table( base1, UNAME='LIST', /all_events, value=s, Notify_Realize='OnRealize_identify2_Table', $
					/no_row_headers, column_widths=[9,4,7,8,9] * !d.x_ch_size, $
					column_labels=['Energy','El','Line','Rel. Int','IUPAC'], $
					/RESIZEABLE_COLUMNS, alignment=2, /tracking, scr_xsize=list_xsize, scr_ysize=300, $
;					X_SCROLL_SIZE=7, Y_SCROLL_SIZE=6,	$		;	, font=fnt, $
					uvalue='Sorted X-ray table (energy, element, relative intensity, Siegbahn/IUPAC labels).' )


thresh = cw_fslider2( base1, format='(f6.4)', minimum=0.0001, maximum=0.5, layout=1, $
				value=0.0001, uname='THRESHOLD', xsize=list_xsize-58, /tracking, /edit, $
				uvalue='Display only X-ray relative intensities above this level.')


base2 = widget_base( bbbase, /column, map=0, xpad=1, ypad=0, space=1, /base_align_center)

ptable = periodic_table( base2, uname='PERIODIC', /tracking, /exclusive, n_states=2, /start_Li, $
				uvalue='Periodic Table. Click on an element to view its markers on the spectrum.')

check_base = widget_base( base2, xpad=2, ypad=0, space=5, /row, /base_align_bottom)
zslide = widget_slider( check_base, minimum=1, maximum=103, /drag, /tracking, xsize=large_slider_width, $
				uvalue='Select element X-ray lines by Z using this slider.', uname='ZSLIDE')
element = widget_text( check_base, xsize=2, scr_ysize=texty, /editable, uname='ELEMENT', /tracking, $
			uvalue='Currently marked element. Type a new one to mark lines.')
lab = widget_label(check_base, value=' ')
options = cw_bgroup2( check_base, ['K','L','M'], /row, set_value=[1,1,0], $
				/return_index, uname='OPTIONS',/ nonexclusive, /tracking, $
				uvalue='Enable X-ray lines by shell.', xpad=0, ypad=0, space=1)

filter_base = widget_base( base2, /row, map=1, /base_align_center, notify_realize='OnRealize_identify2_filter_base', $
				ypad=0, xpad=0, space=space10)
lab = widget_label( filter_base, value='Filter:')
filter_mode = widget_combobox( filter_base, value=filter_title, uname='filter-mode', /tracking, $
					uvalue='Select the filter to use to modify X-ray relative intensities.', $
					xsize=large_drop_width)

lab = widget_label( filter_base, value=' Detector:')
detector_mode = widget_combobox( filter_base, value=detector_title, uname='detector-mode', /tracking, $
					uvalue='Select the detector calibration to use to modify X-ray relative intensities.', xsize=large_drop_width2)


Help_Base = Widget_Base(tlb, UNAME='Help_Base', SPACE=1, XPAD=0, YPAD=0, /ROW, /base_align_center)

help = widget_text( Help_Base, scr_xsize=list_xsize-19, ysize=ysize_help, /wrap, uname='HELP', /tracking, $
				uvalue='Help window. Displays info about widgets.',frame=0)

query_button = Widget_Button(Help_Base, UNAME='query-button', xsize=15, ysize=20,  $
      /ALIGN_CENTER ,VALUE='?', /tracking_events, uvalue='Jump to the help on this window in the GeoPIXE Users Guide.')

state = { element:	element, $
	base1:			base1, $				; ident list map base ID
	base2:			base2, $				; periodic table map base ID
	base1_xsize:	0, $					; X sizes of map bases
	base2_xsize:	large_table_x, $		; X sizes of map bases
	base1_ysize:	0, $					; Y sizes of map bases
	base2_ysize:	large_table_y, $		; Y sizes of map bases
	toggle:			toggle, $				; mode toggle ID - line versus periodic table
	mode:			0, $					; mode (0:list, 1:periodic table)
	ptable:			ptable, $				; periodic table ID
	tiny:			0, $					; tiny table flag
	list:			list, $					; identify2 line list Table ID
	row_height:		0, $					; table row heights
	thresh:			thresh, $				; thresh slider ID
	options:		options, $				; K,L,M selection ID
	zslide:			zslide, $				; Z slider ID
	filter_base:	filter_base, $			; filter base ID
	filter_base_ysize:	0, $				; filter base initial Y size
	filter_id:		filter_mode, $			; filter ID
	detector_id:	detector_mode, $		; detector ID
	xlist:			ptr_new(xlist), $		; pointer to line list
	pe:				ptr_new( {e:0.0, units:''}), $		; pointer to E for notify
	help:			help, $					; help text ID
	whelp:			[list_xsize-5,447], $			; help width
	wslider:		[large_drop_width,large_slider_width], $				; width of the sliders
	filter_mode:	0, $					; current filter
	filters:		filters, $				; filters
	filter_list:	ptr_new(filter_list), $			; pointer to list of filter file names
	detector_mode:	0, $					; current detector
	detector:		detector, $				; detector
	detector_list:	ptr_new(detector_list), $		; pointer to list of detector file names
	pz:				ptr_new({z:0, e:ptr_new([0.0]), rel:ptr_new([0.0]) , shell:ptr_new([0]) }), $ ; pointer to lines for notify
	shells:			[1,1,0] }				; shells on/off

widget_control, base2, scr_xsize = 100
widget_control, base2, scr_ysize = 100

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['identify-e','identify-element','identify-line'], from=group
register_notify, tlb, ['new-detectors','new-filters']

xmanager, 'identify2', tlb, /no_block

return
end
