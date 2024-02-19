
pro layer_plot_event, event

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
		warning,'layer_plot_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
;p1 = (*pstate).p1
;if ptr_valid(p1) eq 0 then goto, bad_ptr
;if size(*p1,/tname) ne 'STRUCT' then goto, bad_ptr
;p2 = (*pstate).p2
;if ptr_valid(p2) eq 0 then goto, bad_ptr
;if size(*p2,/tname) ne 'STRUCT' then goto, bad_ptr

  case !version.os_family of
	'MacOS': begin
		xoff = 118
		yoff = 70
		end
	'unix': begin
		xoff = 132
		yoff = 64
		end
	else: begin
		xoff = 138
		yoff = 67
 		end
  endcase

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'select-periodic': begin
				if ptr_valid( event.pointer) then begin
					(*pstate).z1 = (*event.pointer).zon[0]
					(*pstate).z2 = (*event.pointer).zalt[0]
					widget_control, (*pstate).green, set_value={VALUE: element_name((*pstate).z1)}
					widget_control, (*pstate).orange, set_value={VALUE: element_name((*pstate).z2)}
				endif
				goto, more
				end
			'layer-plot': begin
				if ptr_valid( event.pointer) then begin
					if ptr_valid( (*event.pointer).p1) then (*pstate).p1 = (*event.pointer).p1
					if ptr_valid( (*event.pointer).p2) then (*pstate).p2 = (*event.pointer).p2
				endif
				goto, more
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
				widget_control, (*pstate).help, set_value='Element values plotted in green and orange. ' + $
						'Layer identification is shown stepped in violet.'
			endelse
		endif
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request layer_plot ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'layer_plot_TLB': begin
		if tag_names(event,/structure_name) eq 'WIDGET_BASE' then begin
			w = (event.x - xoff) > 300
			h = (event.y - yoff) > 300
			widget_control, (*pstate).draw, draw_xsize=w, draw_ysize=h
			widget_control, (*pstate).help, scr_xsize=w-20
		endif
		end

	'y-axis': begin
		if event.select eq 0 then goto, finish
		(*pstate).y_axis = event.value
		photo = ((*(*pstate).p1).a1 eq 0) and ((*(*pstate).p1).z1 eq 0)
		continuum = (*(*pstate).p1).beam.continuum
		case (*pstate).y_axis of
			0: begin
				if (*pstate).x_axis eq 2 then (*pstate).x_axis = 0
				(*pstate).x_axis = 0
				end
			1: begin
				if (*pstate).x_axis eq 2 then (*pstate).x_axis = 0
				if photo then (*pstate).x_axis = 0
				end
			2: begin
				if (*pstate).x_axis eq 2 then (*pstate).x_axis = 0
				if photo then (*pstate).x_axis = 0
				if continuum then (*pstate).x_axis = 1
				end
			3: begin
				if (*pstate).x_axis eq 2 then (*pstate).x_axis = 0
				if photo then (*pstate).x_axis = 0
				end
			4: begin
				if (*pstate).x_axis eq 2 then (*pstate).x_axis = 0
				if photo then (*pstate).x_axis = 0
				end
			5: begin
				(*pstate).x_axis = 2
				end
			else:
		endcase
		widget_control, (*pstate).x_axis_id, set_value=(*pstate).x_axis
		end

	'y-log': begin
		if event.select eq 0 then goto, finish
		(*pstate).y_log = event.value
		end

	'x-axis': begin
		if event.select eq 0 then goto, finish
		(*pstate).x_axis = event.value
		case (*pstate).x_axis of
			2: begin
				(*pstate).y_axis = 5
				end
			else: begin
				if (*pstate).y_axis eq 5 then (*pstate).y_axis = 1
				end
		endcase
		widget_control, (*pstate).y_axis_id, set_value=(*pstate).y_axis
		end

	'element-1': begin
		select_periodic, group=event.top, tlb=tlb, zon=[(*pstate).z1,(*pstate).z2], zstate=[1,3]
		register_notify, event.top, ['select-periodic'], from=tlb
		end

	'shell-1': begin
		(*pstate).shell1 = event.index+1
		end

	'element-2': begin
		select_periodic, group=event.top, tlb=tlb, zon=[(*pstate).z1,(*pstate).z2], zstate=[1,3]
		register_notify, event.top, ['select-periodic'], from=tlb
		end

	'shell-2': begin
		(*pstate).shell2 = event.index+1
		end

	'plot': begin
		layer_export, event.top, pstate, /cgm, /white
		goto, finish
		end

	'close-button': begin
		print,'Close filter setup ...'
		goto, kill
		end

	else:
endcase

more:
	plot_yields, pstate

finish:
	widget_control, hourglass=0
	return

bad_state:
	warning,'layer_plot_event',['STATE variable has become ill-defined.','Abort filter Setup.'],/error
	goto, kill
bad_ptr:
	warning,'layer_plot_event',['Parameter structure variable has become ill-defined.','Abort filter Setup.'],/error
	goto, kill

kill:
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;------------------------------------------------------------------------------------------

pro layer_export, top, pstate, cgm=cgm, wmf=wmf, ps=ps, white=white

COMPILE_OPT STRICTARR

if n_elements(cgm) eq 0 then cgm=0
if n_elements(wmf) eq 0 then wmf=0
if n_elements(ps) eq 0 then ps=0
if n_elements(white) eq 0 then white=0

;el_names = ''
;if (*pstate).z1 gt 0 then el_names = [el_names,element_name((*pstate).z1)]
;if (*pstate).z2 gt 0 then el_names = [el_names,element_name((*pstate).z2)]
;if n_elements(el_names) gt 1 then el_names=el_names[1:*]

;select = plot_image_select( event.top, el_names, just_one=(*pstate).image, $
;			cgm=cgm, wmf=wmf, old_select=*(*pstate).pexport, path=*(*pstate).path, image_state=*pstate )
;select = plot_image_select( top, el_names, $
;			cgm=cgm, wmf=wmf, path=*(*pstate).path)
;if select.error then goto, done

;goto, done		; use this bypass if testing plot_image_select without /modal, etc.

;qselect = where(select.el_enable eq 1)
;if qselect[0] eq -1 then goto, done

name = 'Save Yield Data as '
;if n_elements(qselect) gt 1 then name='Select File Root to Save Yield Data as '

if cgm then begin
;if select.plot.type eq 'CGM' then begin
	file = 'yield.cgm'
	file = file_requester( /write, filter='*.cgm', path=*(*pstate).path, $
			file=file, title=name+'CGM', group=top, /fix_filter)
	if strlen(file) lt 1 then goto, done

	plot_yields, pstate, /cgm, white=white, file=file		;, options=select.plot

endif else if wmf then begin
;endif else if select.plot.type eq 'METAFILE' then begin
	file ='yield.wmf'
	file = file_requester( /write, filter='*.wmf', path=*(*pstate).path, $
			file=file, title=name+'WMF', group=top, /fix_filter)
	if strlen(file) lt 1 then goto, done

	plot_yields, pstate, /wmf, white=white, file=file		;, options=select.plot

endif else if ps then begin
;endif else if select.plot.type eq 'PS' then begin
	file = 'yield.eps'
	file = file_requester( /write, filter='*.eps', path=*(*pstate).path, $
			file=file, title=name+'EPS', group=top, /fix_filter)
	if strlen(file) lt 1 then goto, done

	plot_yields, pstate, /eps, white=white, file=file		;, options=select.plot

endif else begin

	plot_yields, pstate, /printer		;, options=select.plot
endelse

;if all then begin
;	*(*pstate).pexport = select
;endif else begin
;	*(*pstate).pexport = {el:old_els, el_enable:old_enable, plot:select.plot}
;endelse

done:
end

;------------------------------------------------------------------------------------------

pro plot_yields, pstate, cgm=cgm, wmf=wmf, ps=ps, white=white, bw=bw, file=file, printer=printer

;	sz = 1.0				; normal defaults
;	!p.symsize = 0.5
;	ts = 1.0

	sz = 2.0				; CGM defaults
	!p.symsize = 0.6
	ts = 1.5

	!p.charsize = sz
	used_printer = 0

	if n_elements(cgm) lt 1 then cgm = 0
	if n_elements(wmf) lt 1 then wmf = 0
	if n_elements(ps) lt 1 then ps = 0
	if n_elements(white) lt 1 then white = 0
	if n_elements(bw) lt 1 then bw = 0
	if n_elements(printer) lt 1 then printer = 0
	if n_elements(file) lt 1 then begin
		if cgm then file = 'Yield.cgm'
		if wmf then file = 'Yield.wmf'
		if ps then file = 'Yield.ps'
	endif

	portrait = 0
	landscape = 1
	xwin = !d.name

	if cgm then begin
		set_device, 'CGM', white=white, file=file
		used_printer = 1
	endif else if wmf then begin
		set_device, 'METAFILE', white=white, file=file
		used_printer = 1
	endif else if ps then begin
		set_device, 'PS', white=white, portrait=portrait, landscape=landscape, file=file
		used_printer = 1
	endif else if printer then begin
		if new_dialog_printersetup(portrait=portrait, landscape=landscape) then begin
			used_printer = 1
		endif else begin
			used_printer = 0
			wset, (*pstate).wid
		endelse
	endif else begin
		set_device, xwin, white=white
		used_printer = 0
		wset, (*pstate).wid
	endelse

on_ioerror, cont
default_plot, thick, athick, csize, cthick, thick_scale=ts
!p.symsize = !p.symsize * csize/sz

if used_printer eq 0 then begin
	csize = 1.0
	athick = 1.0
	cthick = 1.0
endif

;----------------------------------------------------------------------------

	if ptr_valid( (*pstate).p1) and ptr_valid( (*pstate).p2) then begin
		if n_elements(*(*pstate).p1) eq 0 then goto, finish
		if n_elements(*(*pstate).p2) eq 0 then goto, finish
	endif else goto, finish

	!p.title = ''
	continuum = (*(*pstate).p1).beam.continuum
	veto_purple = 0
	choose_microns = 0
	if ((*(*pstate).p1).microns)[0] then choose_microns=1
	if n_elements(((*(*pstate).p1).microns)) gt 1 then choose_microns=0
	case (*pstate).x_axis of
		0: begin
			if choose_microns then begin
				scale = 10. / (*(*pstate).p1).density 
				!x.title = 'Distance (microns)'
			endif else begin
				scale = 1.0
				!x.title = 'Distance (mg/cm^2)'
			endelse
			x1 = scale * (*(*pstate).p2).x_slow
			x2 = x1
			rev_x = 0
			end
		1: begin
			if ((*(*pstate).p1).a1 eq 0) and ((*(*pstate).p1).z1 eq 0) then begin
				if continuum then begin
					!x.title = 'Continuum Energy (keV)'
					x1 = (*(*pstate).p1).beam.spectrum.E
					rev_x = 0
				endif else begin
					!x.title = 'Beam Energy (keV)'
					x1 = (*(*pstate).p2).e_slow
					rev_x = 1
				endelse
			endif else begin
				!x.title = 'Beam Energy (MeV)'
				x1 = (*(*pstate).p2).e_slow
				rev_x = 1
			endelse
			x2 = x1
			veto_purple = 1
			end
		2: begin
			qz1 = where( (*(*pstate).p1).shell eq 1, nqz1)
			x1 = (*(*pstate).p1).z[qz1]
			qz2 = where( (*(*pstate).p1).shell eq 2, nqz2)
			x2 = (*(*pstate).p1).z[qz2]
			!x.title = 'Atomic number Z'
			rev_x = 0
			veto_purple = 1
			end
		else: goto, finish
	endcase

	no_el1 = 0
	case (*pstate).y_axis of
		0: begin
			y1 = (*(*pstate).p2).e_slow
			if ((*(*pstate).p1).a1 eq 0) and ((*(*pstate).p1).z1 eq 0) then begin
				!y.title = 'Beam Energy (keV)'
			endif else begin
				!y.title = 'Beam Energy (MeV)'
			endelse
			no_el2 = 1
			ylog = (*pstate).y_log
			goto, cont
			end
		4: begin
			if ((*(*pstate).p1).a1 eq 0) and ((*(*pstate).p1).z1 eq 0) then begin
				y1 = (*(*pstate).p2).flux
				!y.title = 'Relative Flux'
			endif else begin
				y1 = (*(*pstate).p2).dedx
				!y.title = 'Stopping Power (MeV/mg/cm^2)'
			endelse
			no_el2 = 1
			ylog = (*pstate).y_log
			goto, cont
			end
		5: begin
			y1 = fltarr(nqz1)
			if choose_microns then begin
				scale = 10. / (*(*pstate).p1).density 
				!y.title = 'Escape Depth (microns)'
			endif else begin
				scale = 1.0
				!y.title = 'Escape Depth (mg/cm^2)'
			endelse
			for i=0,nqz1-1 do begin
				yz = (*(*pstate).p2).yieldx[*,qz1[i]]
				dist = (*(*pstate).p2).x_slow
				y1[i] = scale * interpol( dist, yz, yz[0]/exp(1.))
			endfor
			y2 = fltarr(nqz2)
			for i=0,nqz2-1 do begin
				yz = (*(*pstate).p2).yieldx[*,qz2[i]]
				dist = (*(*pstate).p2).x_slow
				y2[i] = scale * interpol( dist, yz, yz[0]/exp(1.))
			endfor
			no_el2 = 0
			ylog = (*pstate).y_log
			goto, cont
			end
		else:
	endcase

	no_el1 = 1
	q1 = where( ((*pstate).z1 eq (*(*pstate).p1).z) and ((*pstate).shell1 eq (*(*pstate).p1).shell))
	if q1[0] ne -1 then no_el1=0
	no_el2 = 1
	q2 = where( ((*pstate).z2 eq (*(*pstate).p1).z) and ((*pstate).shell2 eq (*(*pstate).p1).shell))
	if q2[0] ne -1 then no_el2=0
	if no_el1 and no_el2 then begin
		warning,'plot_yields',['Select two elements in green and orange first.', '', $
				'Click on green or orange to open the periodic table.', $
				'Then click "left" mouse button on element for green, "right" for orange.']
		goto, finish
	endif

	case (*pstate).y_axis of
		1: begin
			if no_el1 eq 0 then y1 = (*(*pstate).p2).yieldx[*,q1[0]]
			if no_el2 eq 0 then y2 = (*(*pstate).p2).yieldx[*,q2[0]]
			!y.title = 'Yield (Counts/ppm.uC.msr.mg/cm^2)'
			ylog = (*pstate).y_log
			end
		2: begin
			if no_el1 eq 0 then y1 = (*(*pstate).p2).xsect[q1[0],*]
			if no_el2 eq 0 then y2 = (*(*pstate).p2).xsect[q2[0],*]
			!y.title = 'Cross Section (cm^2)'
			ylog = (*pstate).y_log
			end
		3: begin
			if no_el1 eq 0 then y1 = (*(*pstate).p2).cmux[0,q1[0],*]
			if no_el2 eq 0 then y2 = (*(*pstate).p2).cmux[0,q2[0],*]
			!y.title = 'Cummulative Mass Absorption (cm^2/mg)'
			ylog = (*pstate).y_log
			end
		else:
	endcase

cont:
	fix_range = 0
	lstyle = 0

	fix_yrange = 0							; to manually set the Y range
	yrange_fix = [0.,100.]
;	lstyle = 5

	if no_el1 eq 0 then begin
		if rev_x then begin
			xrange = [max([x1,x2]),min([x1,x2])]
		endif else begin
			xrange = [min([x1,x2]),max([x1,x2])]
		endelse
		yscale = (ylog) ? 2. : 1.2
		if no_el2 eq 0 then begin
			y = [y1,y2]
		endif else begin
			y = y1
		endelse
		q = where(y gt 1.0e-40)
		if q[0] eq -1 then goto, finish
		yrange = fix_yrange ? yrange_fix : [min(y[q])/yscale,max(y[q])*yscale]
		plot, x1,y1, /nodata, xrange=xrange,yrange=yrange,xstyle=1,ystyle=1, $
			ylog=ylog, thick=thick, xthick=athick,ythick=athick, $
			charthick=cthick, charsize=csize
		oplot, x1,y1, color=spec_colour('green'), thick=thick, linestyle=lstyle
		if no_el2 eq 0 then oplot, x2,y2, color=spec_colour('orange'), thick=thick, linestyle=lstyle
	endif else begin
		xrange = [min(x2),max(x2)]
		yrange = fix_yrange ? yrange_fix : [min(y2),max(y2)]
		plot, x2,y2, /nodata, xrange=xrange,yrange=yrange,xstyle=1,ystyle=1, $
			ylog=ylog, thick=thick, xthick=athick,ythick=athick, $
			charthick=cthick, charsize=csize
		oplot, x2,y2, color=spec_colour('orange'), thick=thick, linestyle=lstyle
	endelse

	if veto_purple eq 0 then begin
		b = yrange[0]
		s = 0.1*(yrange[1]-yrange[0])
		lid = (*(*pstate).p2).half_slice_id
		oplot, x1,b+s*lid, color=spec_colour('violet'), thick=thick
	endif

finish:
	if used_printer then begin
		device,/close
		set_plot, xwin
	endif
	!p.charsize = 1.0
	return
	end

;----------------------------------------------------------------------------------------------

pro OnRealize_layer_plot, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=wid
(*pstate).wid = wid
(*pstate).draw = wWidget
plot_yields, pstate
end

;------------------------------------------------------------------------------------------

pro layer_plot, group_leader=group, TLB=tlb, p1=p1, p2=p2, _extra=extra, $
				gamma=gamma, xoffset=xoffset, yoffset=yoffset, path=path

COMPILE_OPT STRICTARR

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
		warning,'layer_plot',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''

if n_elements(p1) lt 1 then p1=0L
if n_elements(p2) lt 1 then p2=0L
if n_elements(gamma) lt 1 then gamma=0
bad = 0
if (ptr_valid(p1) eq 0L) or (ptr_valid(p2) eq 0L) then begin
	bad = 1
endif else begin
	if n_elements(*p1) eq 0 then bad=1
	if n_elements(*p2) eq 0 then bad=1
endelse

;if n_elements(p1) lt 1 then return
;if n_elements(p2) lt 1 then return
;if ptr_valid(p1) eq 0 then return
;if ptr_valid(p2) eq 0 then return

  case !version.os_family of
	'MacOS': begin
		symbol = 'SYMBOL*12'
		large_font = 'Arial*12'
;@2		widget_control, default_font='Geneva*10'		; set font for all windows
		xs1 = 22
		ys1 = 20
		help_xsize = 250
		mode_xsize = 35
		end
	'unix': begin
		symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
		large_font = '10x20'
;@2		widget_control, default_font='6x13'				; set font for all windows
		xs1 = 22
		ys1 = 20
		help_xsize = 290
		mode_xsize = 50
		end
	else: begin
		symbol = 'SYMBOL*BOLD*14'
		large_font = 'COURIER*BOLD*10'
;		widget_control, default_font='Arial*14'				; set font for all windows
		xs1 = 22
		ys1 = 20
		help_xsize = 250
		mode_xsize = 35
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
;	xoffset = ((xoff + w) < (screen[0]-34 - 350)) > 0
	xoffset = 150
endif
if n_elements(yoffset) lt 1 then begin
	screen = get_screen_size()
;	yoffset = ((yoff) < (screen[1]-28 - 350)) > 0
	yoffset = 150
endif

; 	top-level base

tlb = widget_base( /column, title='Layer Plot', /TLB_KILL_REQUEST_EVENTS, $
				group_leader=group, _extra=extra, uname='layer_plot_TLB', $
				/tlb_size_events, xoffset=xoffset, yoffset=yoffset, /base_align_right)

rbase = widget_base( tlb, /row, xpad=5, ypad=5, space=5, /base_align_top, /align_top)
ybase = widget_base( rbase, /column, xpad=0, ypad=0, space=5, /base_align_left, /align_top)
lab = widget_label( ybase, value='Y AXIS', /align_center)

phot = 0
if bad eq 0 then begin
	if ((*p1).z1 eq 0) and ((*p1).a1 eq 0) then phot = 1
endif
if phot then begin
	yaxis_names = ['Energy','Yield','X Section','Absorption','Flux','Esc. Depth']
	yaxis_help = ['Beam energy (MeV), during slowing down process.', 'X-ray yields per mg/cm^2 of depth (select elements below).', $
				'Cross sections (cm^2; select elements below).','Cummulative Absorption from surface (select elements below).', $
				'Flux decay while traversing the target.','Depth (mg/cm2) at which Yield drops to 1/e. K shell in Green, L shell in Orange.']
endif else begin
	yaxis_names = ['Energy','Yield','X Section','Absorption','Stopping Power','Esc. Depth']
	yaxis_help = ['Beam energy (MeV), during slowing down process.', 'X-ray yields per mg/cm^2 of depth (select elements below).', $
				'Cross sections (cm^2; select elements below).','Cummulative Absorption from surface (select elements below).', $
				'Stopping power during slowing down process (MeV/mg/cm^2)','Depth (mg/cm2) at which Yield drops to 1/e.']
endelse
y_axis = cw_bgroup2( ybase, yaxis_names, /column, xpad=0, $
				ypad=0, space=0, /return_index, /tracking, uname='y-axis', /exclusive, set_value=0, $
				uvalue=yaxis_help)

ebase = widget_base( ybase, /column, xpad=10, ypad=2, space=3, /base_align_left, /align_left, $
				/frame)
lab = widget_label( ebase, value='ELEMENT', /align_center)

e1base = widget_base( ebase, /row, xpad=0, ypad=0, space=3, /base_align_center)
e1 = state_button( e1base, xsize=xs1,ysize=ys1,value='',uname='element-1',n_states=1, $
				n_alt_states=0, colours=[spec_colour('green')], $
				/tracking, uvalue='Pop-up a periodic table to select elements for plots.' )
shells = ['K','L','M']
if gamma then shells = ['g']
shell1 = widget_combobox( e1base, value=shells, uname='shell-1', /tracking, $
					uvalue='Select the shell to plot for this element (plots in green).',xsize=mode_xsize)

e2base = widget_base( ebase, /row, xpad=0, ypad=0, space=3, /base_align_center)
e2 = state_button( e2base, xsize=xs1,ysize=ys1,value='',uname='element-2',n_states=1, $
				n_alt_states=0, colours=[spec_colour('orange')], $
				/tracking, uvalue='Pop-up a periodic table to select elements for plots.' )
shell2 = widget_combobox( e2base, value=shells, uname='shell-2', /tracking, $
					uvalue='Select the shell to plot for this element (plots in orange).',xsize=mode_xsize)

ylog_names = ['Linear','Log']
ylog_help = ['linear Y axis scale','Log  axis scale']
y_log = cw_bgroup2( ybase, ylog_names, /column, xpad=0, $
				ypad=0, space=0, /return_index, /tracking, uname='y-log', /exclusive, set_value=0, $
				uvalue=ylog_help)
plot_button = widget_button( ybase, value='Plot', uname='plot')

  if !version.os_family eq 'unix' then begin
	retain = 2
  endif else begin
	retain = 1
  endelse

draw = widget_draw( rbase, uname='draw', xsize=600, ysize=500, notify_realize='OnRealize_layer_plot', retain=retain)
;			/button_events)

xbase = widget_base( tlb, /row, xpad=0, ypad=0, space=3, /base_align_center, /align_right)
help = widget_text( xbase, scr_xsize=help_xsize, ysize=2, /wrap, uname='help', /tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

x1base = widget_base( xbase, /column, xpad=0, ypad=0, space=3, /base_align_right, /align_right)
lab = widget_label( x1base, value='X AXIS', /align_center)
x_axis = cw_bgroup2( x1base, ['Distance','Energy','Z'], /row, xpad=0, set_value=0, $
				ypad=0, space=0, /return_index, /tracking, uname='x-axis', /exclusive, $
				uvalue=['Distance travelled into target, normal to target surface (mg/cm^2).','Beam energy (MeV) during slowing down process.','Atomic number.'] )

state = {	$
		p1:				p1, $					; pointer to parameters (peaks)
		p2:				p2, $					; pointer to parameters (peaks2)
		gamma:			gamma, $				; Gamma mode?
		path:			ptr_new(path), $		; pointer to current path

		x_axis:			0, $					; x axis selection
		y_axis:			0, $					; y axis selection
		y_log:			0, $					; y axis log scale
		z1:				0, $					; element 1 Z
		z2:				0, $					; element 2 Z
		shell1:			1, $					; element 1 shell
		shell2:			1, $					; element 1 shell

		wid:			0, $					; draw widget drawID
		draw:			draw, $					; draw widget ID
		green:			e1, $					; green element widget ID
		orange:			e2, $					; orange element widget ID
		x_axis_id:		x_axis, $				; X axis select ID
		y_axis_id:		y_axis, $				; Y axis select ID		

		help:			help $					; ID of help text
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['layer-plot'], $			; new data
					from=group

xmanager, 'layer_plot', tlb, /no_block

return
end
