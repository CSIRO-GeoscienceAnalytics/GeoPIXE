
;	detector setup and edit.

pro detector_setup_event, event
common c_working_dir, geopixe_root

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
		warning,'detector_setup_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
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
p = (*pstate).p
if ptr_valid(p) eq 0 then goto, bad_ptr
if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr

  case !version.os_family of
	'MacOS': begin
		xoff = 414
		yoff = 22
		end
	'unix': begin
		xoff = 444
		yoff = 20
		end
	else: begin
		xoff = 414
		yoff = 22
 		end
  endcase
drag = 0

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
					print,'detector Setup: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'mark-e': begin
				(*pstate).energy = (*event.pointer).e
				goto, update
				end
			'detector-load': begin						; layout details loaded elsewhere
				if ptr_valid(event.pointer) then begin
					widget_control,(*pstate).layout_file,set_value=(*(*p).playout).file
					(*p).shape_mode = (*(*p).playout).shape
					widget_control, (*pstate).shape_mode, set_combobox_select=(*p).shape_mode, sensitive=(*p).array_mode eq 0
				endif
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
				widget_control, (*pstate).help, set_value='Click and drag in the plot window to display intrinsic efficiency and total efficiency at a selected energy.'
			endelse
		endif
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request detector_Setup ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'detector_TLB': begin
		if tag_names(event,/structure_name) eq 'WIDGET_BASE' then begin
			w = (event.x - xoff) > 400
			h = (event.y - yoff) > 580
			widget_control, (*pstate).draw, draw_xsize=w, draw_ysize=h
			(*pstate).width = w
			(*pstate).height = h
			wdelete, (*pstate).pix
			window, /free, xsize=(*pstate).width, ysize=(*pstate).height, /pixmap
			(*pstate).pix = !d.window
		endif
		end

	'draw': begin
		if( Event.type eq 3 )then begin
;			OnViewport_corr, Event
		endif else begin
			OnButton_detector, pstate, Event
			drag = 1
		endelse
		end

	'load-detector-button': begin
		file = find_file2( (*p).detector_file)
		path = extract_path( file[0])
;		if lenchr(path) eq 0 then path = *(*pstate).path
		if lenchr(path) eq 0 then path = geopixe_root
		F = file_requester( /read, filter = '*.detector', $
				/must_exist, path=path, group=event.top, $
				title='Select the source detector file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.detector'
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).detector_file, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).detector_file, set_value=F, set_text_select=[n-1,0]
			(*p).detector_file = F

			load_detector_parameters, pstate, F
			notify, 'detector-setup-load', from=event.top
		endif
		end

	'save-detector-button': begin
		file = find_file2( (*p).detector_file)
		path = extract_path( file[0])
;		if lenchr(path) eq 0 then path = *(*pstate).path
		if lenchr(path) eq 0 then path = geopixe_root
		F = file_requester( /write, filter = '*.detector', $
				path=path, group=event.top, $
				title='Save the detector definitions to a detector file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.detector'
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).detector_file, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).detector_file, set_value=F, set_text_select=[n-1,0]
			(*p).detector_file = F

			detector_update_pars, pstate
			save_detector_parameters, pstate, F
			notify, 'new-detectors'
		endif
		end

	'detector-file': begin
		widget_control, event.id, get_value=F
		F = strip_file_ext(F[0]) + '.detector'
		(*p).detector_file = F
		*(*pstate).path = extract_path(F)
		n = lenchr(F)
		k = lenchr(strip_path(F))
		widget_control, (*pstate).detector_file, set_value=F, set_text_select=[n-k,k]
		widget_control, (*pstate).detector_file, set_value=F, set_text_select=[n-1,0]

		load_detector_parameters, pstate, F
		notify, 'detector-setup-load', from=event.top
		end

	'load-detector-overlay': begin
		widget_control, (*pstate).overlay_text, get_value=file
		file = strip_file_ext(file[0]) + '.detector'
		(*pstate).overlay_file = file
		file = find_file2( (*pstate).overlay_file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = geopixe_root
		F = file_requester( /read, filter = '*.detector', file=file[0], /must_exist, $
				path=path, group=event.top, title='Select the detector overlay file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.detector'
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).overlay_text, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).overlay_text, set_value=F, set_text_select=[n-1,0]
			(*pstate).overlay_file = F

			if ptr_valid((*pstate).poverlay) then ptr_free, (*pstate).poverlay
			pd = read_detector( F, error=error)
			if (error eq 0) and ptr_valid(pd) then begin
				(*pstate).poverlay = pd
				if ptr_valid((*pstate).poverlay_layout) then *(*pstate).poverlay_layout = 0L
				if (strlen((*pd).layout) gt 0) then begin
					d = read_detector_layout((*pd).layout, error=error)
					if error eq 0 then begin
						*(*pstate).poverlay_layout = d
					endif else begin
						warning,'detector_setup','error reading layout: '+(*pd).layout
					endelse
				endif
			endif
		endif else begin
			(*pstate).overlay_file = ''
			widget_control, (*pstate).overlay_text, set_value=''
			ptr_free, (*pstate).poverlay
		endelse
		end

	'title-text': begin
		widget_control, event.id, get_value=s
		(*p).title = s[0]
		end

	'crystal-mode': begin
		(*p).crystal_mode = event.index

		case (*p).crystal_mode of
			0: begin
				(*p).formula = 'Si'
				(*p).density = density(atomic_number('Si'))
				end
			1: begin
				(*p).formula = 'Ge'
				(*p).density = density(atomic_number('Ge'))
				end
			else:
		endcase
		end

	'detector-mode': begin
		(*p).gamma = event.index

		case (*p).gamma of
			0:	begin
				widget_control, (*pstate).map_base0, map=1
				widget_control, (*pstate).map_base1, map=0
				end
			1:	begin
				widget_control, (*pstate).map_base0, map=0
				widget_control, (*pstate).map_base1, map=1
				end
			else:
		endcase
		end

	'crystal-shape': begin
		(*p).shape_mode = event.index
		(*p).area = (*p).diameter * (*p).diameter
		if (*p).shape_mode eq 0 then (*p).area = (*p).area * !pi/4.
		(*p).solid_angle = 0.0
		widget_control, (*pstate).area_text, set_value=str_tidy((*p).area)
		end

	'array-mode': begin
		(*p).array_mode = event.index
		widget_control, (*pstate).layout_base, map=(*p).array_mode
		widget_control, (*pstate).shape_mode, sensitive=(*p).array_mode eq 0
		end

	'load-layout-button': begin
		file = find_file2( (*p).layout_file)
		path = extract_path( file[0])
;		if lenchr(path) eq 0 then path = *(*pstate).path
		if lenchr(path) eq 0 then path = geopixe_root
		F = file_requester( /read, filter = '*.csv', /must_exist, path=path, group=event.top, $
					title='Select the array detector layout file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.csv'
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).layout_file, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).layout_file, set_value=F, set_text_select=[n-1,0]
			(*p).layout_file = F
			d = read_detector_layout(F, error=error)
			if error then begin
				warning,'detector_setup','Bad read of layout file.'
			endif else begin
				*(*p).playout = d
				(*p).shape_mode = (*(*p).playout).shape
				widget_control, (*pstate).shape_mode, set_combobox_select=(*p).shape_mode, sensitive=(*p).array_mode eq 0

				notify, 'detector-setup-load', from=event.top
			endelse
		endif
		end

	'layout-file': begin
		widget_control, event.id, get_value=F
		F = strip_file_ext(F[0]) + '.csv'
		(*p).layout_file = F
		*(*pstate).path = extract_path(F)
		n = lenchr(F)
		k = lenchr(strip_path(F))
		widget_control, (*pstate).layout_file, set_value=F, set_text_select=[n-k,k]
		widget_control, (*pstate).layout_file, set_value=F, set_text_select=[n-1,0]
		d = read_detector_layout(F, error=error)
		if error then begin
			warning,'detector_setup','Bad read of layout file.'
		endif else begin
			*(*p).playout = d
			(*p).shape_mode = (*(*p).playout).shape
			widget_control, (*pstate).shape_mode, set_combobox_select=(*p).shape_mode, sensitive=(*p).array_mode eq 0

			notify, 'detector-setup-load', from=event.top
		endelse
		end

	'detector-layout-popup': begin
		detector_layout, group_leader=event.top, TLB=tlb, path=(*pstate).path, data=(*p).playout
		register_notify, event.top, ['detector-load'], from=tlb
		end

	'diameter-text': begin
		widget_control, event.id, get_value=s
		(*p).diameter = float(s[0])
		area = (*p).diameter * (*p).diameter
		if (*p).shape_mode eq 0 then area = area * !pi/4.
		(*p).area = area
		widget_control, (*pstate).area_text, set_value=str_tidy((*p).area)
		end

	'area-text': begin
		widget_control, event.id, get_value=s
		(*p).area = float(s[0])
		diam = sqrt((*p).area)
		if (*p).shape_mode eq 0 then diam = diam * 2.0/sqrt(!pi)
		(*p).diameter = diam
		widget_control, (*pstate).diameter_text, set_value=str_tidy((*p).diameter)
		end

	'detector-thick-text': begin
		widget_control, event.id, get_value=s
		(*p).thick = float(s[0])
		end

	'detector-tilt-text': begin
		widget_control, event.id, get_value=s
		(*p).tilt = float(s[0])
		end

	'detector-solid-text': begin
;		widget_control, event.id, get_value=s
;		(*p).solid_angle = float(s[0])
;		(*p).distance = sqrt( 1000.* (*p).area / (*p).solid_angle)
;		widget_control, (*pstate).distance_text, set_value=str_tidy((*p).distance)
		end

	'distance-text': begin
		widget_control, event.id, get_value=s
		(*p).distance = float(s[0])
		end

	'resolution-text': begin
		widget_control, event.id, get_value=s
		(*p).resolution = float(s[0])
		end

	'options': begin
		case event.value of
			0: begin
				(*p).cohen = event.select
				end
			1: begin
				(*p).correct_solid_angle = event.select
				end
			else:
		endcase
		end

	'filter-number': begin
		(*p).n_filters = event.index + 1
		if (*pstate).active gt (*p).n_filters-1 then begin
			widget_control, (*pstate).filter_base[(*pstate).active], map=0
			(*pstate).active = (*p).n_filters-1
			widget_control, (*pstate).filter_base[(*pstate).active], map=1
			widget_control, (*pstate).define_filter, set_combobox_select=(*pstate).active
		endif
		end

	'define-filter': begin
		widget_control, (*pstate).filter_base[(*pstate).active], map=0
		(*pstate).active = event.index
		widget_control, (*pstate).filter_base[(*pstate).active], map=1
		if (*pstate).active gt (*p).n_filters-1 then begin
			(*p).n_filters = (*pstate).active+1
			widget_control, (*pstate).filter_number, set_combobox_select=(*p).n_filters-1
		endif
		end

	'thick-text': begin
		widget_control, event.id, get_value=s
		t = float(s[0])
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*p).n_filters_max-1)
		(*p).filter[n].thick = t
		if (*p).filter[n].microns eq 2 then detector_update_density, pstate
		end

	'thick-mode': begin
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*p).n_filters_max-1)
		(*p).filter[n].microns = event.index
		if (*p).filter[n].microns eq 2 then (*p).filter[n].pinhole = 0

		case (*p).filter[n].pinhole of
			0: begin
				map_pinhole = 0
				map_bragg = 0
				end
			1: begin
				map_pinhole = 1
				map_bragg = 0
				end
			2: begin
				map_pinhole = 0
				map_bragg = 1
				end
		endcase
		widget_control, (*pstate).ratio_base[n], map=map_pinhole
		widget_control, (*pstate).bragg_base[n], map=map_bragg
		widget_control, (*pstate).pinhole_mode[n], set_combobox_select=(*p).filter[n].pinhole

		widget_control, (*pstate).density_base[n], map=(*p).filter[n].microns
		if (*p).filter[n].microns  and ((*p).filter[n].density lt 0.001) then begin
			z = atomic_number((*p).filter[n].formula)
			if z gt 0 then begin
				(*p).filter[n].density = density(z)
				widget_control, (*pstate).density_text[n], set_value=str_tidy((*p).filter[n].density)
			endif
		endif
		if (*p).filter[n].microns eq 2 then filter_update_density, pstate
		end

	'filter-density': begin
		widget_control, event.id, get_value=s
		t = float(s[0])
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*p).n_filters_max-1)
		(*p).filter[n].density = t
		end

	'weight-mode': begin
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*p).n_filters_max-1)
		(*p).filter[n].weight = event.index
		end

	'filter-formula': begin
		widget_control, event.id, get_value=s
		t = ''
		for i=0L,n_elements(s)-1 do t = t+s[i]
		t = strcompress(t,/remove_all)
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*p).n_filters_max-1)

		(*p).filter[n].formula = t
		z = atomic_number(t)
		if (*p).filter[n].microns eq 2 then begin
			detector_update_density, pstate
		endif else if (*p).filter[n].microns eq 1 then begin
			if z gt 0 then begin
				(*p).filter[n].density = density(z)
				widget_control, (*pstate).density_text[n], set_value=str_tidy((*p).filter[n].density)
			endif
		endif
		end

	'pinhole-mode': begin
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*p).n_filters_max-1)
		(*p).filter[n].pinhole = event.index
		if (*p).filter[n].microns eq 2 then (*p).filter[n].pinhole = 0

		case (*p).filter[n].pinhole of
			0: begin
				map_pinhole = 0
				map_bragg = 0
				end
			1: begin
				map_pinhole = 1
				map_bragg = 0
				end
			2: begin
				map_pinhole = 0
				map_bragg = 1
				end
		endcase

		widget_control, (*pstate).ratio_base[n], map=map_pinhole
		widget_control, (*pstate).bragg_base[n], map=map_bragg
		widget_control, (*pstate).pinhole_mode[n], set_combobox_select=(*p).filter[n].pinhole
		end

	'filter-pinratio': begin
		widget_control, event.id, get_value=s
		t = float(s[0])
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*p).n_filters_max-1)
		(*p).filter[n].pinratio = t
		end

	'bragg-button': begin
		widget_control, event.id, get_value=s
		t = float(s[0])
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*p).n_filters_max-1)

		info = hopg_filter( event.top, hopg=(*p).filter[n].bragg)
		if info.cancel eq 0 then begin
			(*p).filter[n].bragg = info.hopg
		endif
		end

	'a0-text': begin
		widget_control, event.id, get_value=s
		(*p).a[0] = float(s[0])
		end

	'a1-text': begin
		widget_control, event.id, get_value=s
		(*p).a[1] = float(s[0])
		end

	'a2-text': begin
		widget_control, event.id, get_value=s
		(*p).a[2] = float(s[0])
		end

	'a3-text': begin
		widget_control, event.id, get_value=s
		(*p).a[3] = float(s[0])
		end

	'a4-text': begin
		widget_control, event.id, get_value=s
		(*p).a[4] = float(s[0])
		end

	'a5-text': begin
		widget_control, event.id, get_value=s
		(*p).a[5] = float(s[0])
		end

	'e_low-text': begin
		widget_control, event.id, get_value=s
		(*p).e_low = float(s[0])
		end

	'e_high-text': begin
		widget_control, event.id, get_value=s
		(*p).e_high = float(s[0])
		end

	'cursor-text': begin
		widget_control, event.id, get_value=s
		(*pstate).energy = clip( float(s[0]), 0.1, 1000000.)
		end

	'calculate-button': begin

;		later calculate transmission and graph in plot area

		end

	'close-button': begin
		print,'Close detector setup ...'
		goto, kill
		end

	else:
endcase

update:
	detector_update_pars, pstate
	update_detector_struct, pstate, error=error

	total_eff = detector_efficiency( (*pstate).detector, (*p).playout, (*pstate).energy, $
					solid_angle=omega, effective=efficiency)
	(*p).solid_angle = omega
	total_overlay = detector_efficiency( (*pstate).poverlay, (*pstate).poverlay_layout, (*pstate).energy, $
					solid_angle=overlay_omega, effective=overlay_efficiency)

;	print, ' Efficiency (E, intrinsic, total) = ', (*pstate).energy, efficiency, total_eff
;	print, 'Overlay Eff (E, intrinsic, total) = ', (*pstate).energy, overlay_efficiency, total_overlay
	widget_control, (*pstate).detector_solid_text, set_value=str_tidy(omega)
	widget_control, (*pstate).cursor_text, set_value=str_tidy((*pstate).energy)
	widget_control, (*pstate).efficiency_text, set_value=str_tidy(efficiency)
	widget_control, (*pstate).total_text, set_value=str_tidy(total_eff)
	widget_control, (*pstate).overlay_efficiency_text, set_value=str_tidy(efficiency/overlay_efficiency)
	widget_control, (*pstate).overlay_total_text, set_value=str_tidy(total_eff/total_overlay)

	if (drag eq 0) and (error eq 0) then begin
		detector_draw, pstate
	endif

finish:
	widget_control, hourglass=0
	return

bad_state:
	warning,'detector_setup_event',['STATE variable has become ill-defined.','Abort detector Setup.'],/error
	goto, kill
bad_ptr:
	warning,'detector_setup_event',['Parameter structure variable has become ill-defined.','Abort detector Setup.'],/error
	goto, kill

kill:
;	detector_update_pars, pstate
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).poverlay) then ptr_free, (*pstate).poverlay
	if ptr_valid( (*pstate).poverlay_layout) then ptr_free, (*pstate).poverlay_layout
	if ptr_valid( (*pstate).detector) then ptr_free, (*pstate).detector
	wdelete, (*pstate).pix

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;------------------------------------------------------------------------------------------

pro detector_draw, pstate

	e = 10.^(findgen(50)/5.)
	t = det_eff( (*pstate).detector, e)
	q = where((t gt 1.0e-4) and (abs(t[n_elements(t)-1]-t) gt t[n_elements(t)-1]*1.0e-2))
	if q[0] eq -1 then begin
		q = where((1-t) lt 0.01)
		if q[0] ne -1 then begin
			elow = 1.0
			ehigh = 1000.0
		endif else begin
			return
		endelse
	endif else begin
		elow = 0.6*min(e[q]) > 1.02
		ehigh = 1.2*max(e[q])
	endelse
	elow = (elow < ehigh/3.) > 1.
	ehigh = ehigh > elow*12.
	e = 10.^(alog10(elow) + alog10(ehigh/elow)*findgen(1000)/1000.)

;	eff = det_eff( (*pstate).detector, e)
;	overlay = det_eff( (*pstate).poverlay, e)

	total_eff = detector_efficiency( (*pstate).detector, (*(*pstate).p).playout, e, effective=eff)
	total_overlay = detector_efficiency( (*pstate).poverlay, null, e, effective=overlay)

	top = 1.2*max(eff)
	bot = 0.5*min(eff) > 0.5e-4*top

	!p.color = spec_colour('white')
	!p.background = spec_colour('black')
	!x.title = 'Photon Energy (keV)'
	!y.title = 'Efficiency'
	!p.title = ''
	!p.charsize = 1.0
	!p.thick = 1.0
	!p.charthick = 1.0
	wset, (*pstate).wid
	if n_elements(eff) lt 2 then return
	plot, e,eff, xrange=[elow,ehigh],yrange=[bot,top], $
				color=spec_colour('white'), /xlog, /ylog, $
				ticklen=1.0, /nodata					, xstyle=1,ystyle=1
	oplot, e,0.1*total_eff/total_overlay, color=spec_colour('violet')
	oplot, e,overlay, color=spec_colour('red')
	oplot, e,eff, color=spec_colour('green')

	wset, (*pstate).pix
	device,copy=[0,0,(*pstate).width,(*pstate).height, 0,0,(*pstate).wid]

	wset, (*pstate).wid
	if ((*pstate).energy gt elow) and ((*pstate).energy lt ehigh) then begin
		plots, [(*pstate).energy,(*pstate).energy], 10.^(!y.crange), color=spec_colour('orange')
	endif
	return
end

;------------------------------------------------------------------------------------------

pro detector_update_density, pstate

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
		warning,'detector_update_density',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	if size(*p,/tname) ne 'STRUCT' then return

	i = (*pstate).active	
	moles = 0.001 / (0.08206 * 293.15)		; moles per cm3 at NPT
	
	radical = radical_split( (*p).filter[i].formula)
	nr = n_elements(radical)
	ni = fltarr(nr)
	mi = fltarr(nr)
	for j=0,nr-1 do begin
		chop_radical, radical[j], form,x
		ni[j] = x
		decode_radical, form, n, z, f, /number
		mi[j] = total( mass(z[0:n-1])*f[0:n-1])
		if (*p).filter[i].weight then ni[j] = ni[j] / mi[j]
	endfor
	m = total( mi*ni) / total(ni)
	dens = m * moles

	(*p).filter[i].density = dens
	widget_control, (*pstate).density_text[i], set_value=str_tidy(dens)
	return
end

;------------------------------------------------------------------------------------------

pro detector_update_pars, pstate, error=error

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
		warning,'detector_update_pars',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	error = 1
	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	if size(*p,/tname) ne 'STRUCT' then return

	error = 0
	widget_control, (*pstate).title_text, get_value=F
	(*p).title = F[0]
	widget_control, (*pstate).detector_file, get_value=F
	(*p).detector_file = F[0]

	widget_control, (*pstate).diameter_text, get_value=F
	(*p).diameter = float(F[0])
	widget_control, (*pstate).area_text, get_value=F
	(*p).area = float(F[0])
	widget_control, (*pstate).distance_text, get_value=F
	(*p).distance = float(F[0])
	widget_control, (*pstate).detector_thick_text, get_value=F
	(*p).thick = float(F[0])
	widget_control, (*pstate).resolution_text, get_value=F
	(*p).resolution = float(F[0])
	widget_control, (*pstate).detector_tilt_text, get_value=F
	(*p).tilt = float(F[0])

	widget_control, (*pstate).a0_text, get_value=F
	(*p).a[0] = float(F[0])
	widget_control, (*pstate).a1_text, get_value=F
	(*p).a[1] = float(F[0])
	widget_control, (*pstate).a2_text, get_value=F
	(*p).a[2] = float(F[0])
	widget_control, (*pstate).a3_text, get_value=F
	(*p).a[3] = float(F[0])
	widget_control, (*pstate).a4_text, get_value=F
	(*p).a[4] = float(F[0])
	widget_control, (*pstate).a5_text, get_value=F
	(*p).a[5] = float(F[0])
	widget_control, (*pstate).e_low_text, get_value=F
	(*p).e_low = float(F[0])
	widget_control, (*pstate).e_high_text, get_value=F
	(*p).e_high = float(F[0])

	if lenchr((*p).formula) lt 1 then error=1

	d = make_layer( (*p).formula, (*p).thick*1000., /microns, $
				density=(*p).density, name=(*p).title, error=error2)
	if error2 then begin
		error = 1
	endif else begin
		(*p).crystal = d
	endelse

	(*p).crystal.name = (*p).title

	if (*p).gamma eq 0 then begin
		if (*p).diameter lt 1.0e-6 then error=1
		if (*p).area lt 1.0e-6 then error=1
		if (*p).distance lt 1.0e-6 then error=1
		if (*p).thick lt 1.0e-6 then error=1
	endif

	for i=0L,(*p).n_filters-1 do begin
		widget_control, (*pstate).thick_text[i], get_value=s
		(*p).filter[i].thick = float(s[0])
		widget_control, (*pstate).density_text[i], get_value=s
		(*p).filter[i].density = float(s[0])

		widget_control, (*pstate).formula_text[i], get_value=s
		t = ''
		for k=0L,n_elements(s)-1 do t = t+s[k]
		(*p).filter[i].formula = strcompress(t,/remove_all)

;		if (*p).filter[i].density lt 0.0001 then begin
			z = atomic_number((*p).filter[i].formula)
			if (z gt 0) and ((*p).filter[i].microns ne 2) then begin
				(*p).filter[i].density = density(z)
				widget_control, (*pstate).density_text[i], set_value=str_tidy((*p).filter[i].density)
			endif
;		endif

		widget_control, (*pstate).ratio_text[i], get_value=s
		(*p).filter[i].pinratio = float(s[0])
	endfor

	return
end

;------------------------------------------------------------------------------------------

pro update_detector_struct, pstate, error=error

	error = 1
	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
	p = (*pstate).p
	if ptr_valid(p) eq 0 then goto, bad_ptr
	if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr

; N.B.	Filter struct has microns for convenience, if /microns is set.
;	But thick is ALWAYS in mg/cm^2 in filter files. So we convert here
;	in "make_filter". The f[i] then ALWAYS have thick in mg/cm^2.
;	But we leave (*p).filter set in microns still.

	error = 0
	f = (*p).filter[0:(*p).n_filters-1]
	for i=0L,(*p).n_filters-1 do begin
		r = (*p).filter[i].pinratio
		if (*p).filter[i].pinhole eq 0 then r=0.0

		f[i] = make_filter( (*p).filter[i].formula, (*p).filter[i].thick, pinratio=r, $
				microns=(*p).filter[i].microns < 1, density=(*p).filter[i].density, $
				weight=(*p).filter[i].weight, name=(*p).filter[i].name, error=error2)

		f[i].pinhole = (*p).filter[i].pinhole
		f[i].bragg = (*p).filter[i].bragg
		if (*p).filter[i].microns eq 2 then f[i].thick = f[i].thick * 1000. 
		if error2 then error=1
	endfor

	det = make_detector( (*p).formula, thick=(*p).thick, diameter=(*p).diameter, filters=f, $
				density=(*p).density, distance=(*p).distance, resolution=(*p).resolution, $
				name=(*p).title, cohen=(*p).cohen, gamma=(*p).gamma, poly=(*p).a, tilt=(*p).tilt, $
				e_low=(*p).e_low, e_high=(*p).e_high, layout=(*p).layout_file, array=(*p).array_mode, $
				shape=(*p).shape_mode, correct_solid_angle=(*p).correct_solid_angle, error=error2, $

;	remaining arguments come from the existing detector struct, as loaded ...

				source=(*(*pstate).detector).source, Aeff=(*(*pstate).detector).Aeff, Beff=(*(*pstate).detector).Beff, $
				tail=(*(*pstate).detector).tail, w1=(*(*pstate).detector).w1)

; NOTE	Special options have been disabled for now ...
;
;	if (*(*pstate).detector).use_special1 then begin
;		det.use_special1 = 1
;		det.special1 = (*(*pstate).detector).special1
;	endif
;	if (*(*pstate).detector).use_special2 then begin
;		det.use_special2 = 1
;		det.special2 = (*(*pstate).detector).special2
;	endif

	if (*p).gamma eq 0 then begin
		if det.diameter lt 0.001 then error=1
		if det.distance lt 0.001 then error=1
		if det.crystal.thick lt 0.001 then error=1
	endif

;	if ptr_valid((*pstate).detector) then ptr_free, (*pstate).detector
	if error2 then begin
;		(*pstate).detector = ptr_new()
		error = 1
	endif else begin
		(*pstate).detector = ptr_new( det, /no_copy)
	endelse
	return

bad:
	warning,'update_detector_struct','bad input parameters'
	goto, done
bad_state:
	warning,'update_detector_struct','bad state structure'
	goto, done
bad_ptr:
	warning,'update_detector_struct','bad parameter pointer or structure'
	goto, done
done:
	return
	end

;------------------------------------------------------------------------------------------------------

pro load_detector_parameters, pstate, file

;	Load the parameters into '(*pstate).p' from 'file' (.detector)

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
		warning,'Load_detector_parameters',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, done
	endif
endif

	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
	if n_elements(file) lt 1 then goto, bad
	p = (*pstate).p
	if ptr_valid(p) eq 0 then goto, bad_ptr
	if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr

	pd = read_detector( file, error=error)
	if error or (ptr_valid(pd) eq 0) then goto, bad_io
	n = n_elements( (*pd).absorbers) < (*p).n_filters_max

	(*p).title = (*pd).crystal.name
	widget_control, (*pstate).title_text, set_value=(*p).title
	(*p).crystal = (*pd).crystal

	(*p).diameter = (*pd).diameter
	widget_control, (*pstate).diameter_text, set_value=str_tidy((*p).diameter)
	(*p).distance = (*pd).distance
	widget_control, (*pstate).distance_text, set_value=str_tidy((*p).distance)
	(*p).resolution = (*pd).resolution
	widget_control, (*pstate).resolution_text, set_value=str_tidy((*p).resolution)

	(*p).tilt = (*pd).tilt
	widget_control, (*pstate).detector_tilt_text, set_value=str_tidy((*p).tilt)
	(*p).cohen = (*pd).cohen
	(*p).correct_solid_angle = (*pd).correct_solid_angle
	options = [(*p).cohen, (*p).correct_solid_angle]
	widget_control, (*pstate).options, set_value=options

	if ((*pd).crystal.n eq 1) and ((*pd).crystal.z[0] eq 14) then begin				; Si
		(*p).crystal_mode = 0
		(*p).formula = 'Si'
		(*p).density = density(14)
		widget_control, (*pstate).crystal_mode, set_combobox_select=0

	endif else if ((*pd).crystal.n eq 1) and ((*pd).crystal.z[0] eq 32) then begin		; Ge
		(*p).crystal_mode = 1
		(*p).formula = 'Ge'
		(*p).density = density(32)
		widget_control, (*pstate).crystal_mode, set_combobox_select=1

	endif else begin
		warning,'load_detector_parameters','unknown detector type: Z=',string((*pd).crystal.z[0:(*pd).crystal.n-1])
	endelse

	(*p).gamma = (*pd).pige
	widget_control, (*pstate).detector_model, set_combobox_select=(*p).gamma
	case (*p).gamma of
		0:	begin
			widget_control, (*pstate).map_base0, map=1
			widget_control, (*pstate).map_base1, map=0
			end
		1:	begin
			widget_control, (*pstate).map_base0, map=0
			widget_control, (*pstate).map_base1, map=1
			end
		else:
	endcase
	(*p).a = (*pd).a
	widget_control, (*pstate).a0_text, set_value=str_tidy((*p).a[0])
	widget_control, (*pstate).a1_text, set_value=str_tidy((*p).a[1])
	widget_control, (*pstate).a2_text, set_value=str_tidy((*p).a[2])
	widget_control, (*pstate).a3_text, set_value=str_tidy((*p).a[3])
	widget_control, (*pstate).a4_text, set_value=str_tidy((*p).a[4])
	widget_control, (*pstate).a5_text, set_value=str_tidy((*p).a[5])

;	Note that (*p).thick is in mm, while in (*pd).crystal.thick it is in mg/cm^2.

	(*p).thick = (*pd).crystal.thick / (100. * (*p).density)
	widget_control, (*pstate).detector_thick_text, set_value=str_tidy((*p).thick)

;	Array, shape and layout parameters

	(*p).shape_mode = (*pd).shape
	(*p).array_mode = (*pd).array
	(*p).layout_file = (*pd).layout
	widget_control, (*pstate).shape_mode, set_combobox_select=(*p).shape_mode
	widget_control, (*pstate).array_mode, set_combobox_select=(*p).array_mode
	widget_control, (*pstate).layout_file, set_value=(*p).layout_file
	widget_control, (*pstate).layout_base, map=(*p).array_mode
	widget_control, (*pstate).shape_mode, sensitive=(*p).array_mode eq 0
	ns = lenchr((*p).layout_file)
	ks = lenchr(strip_path((*p).layout_file))
	widget_control, (*pstate).layout_file, set_value=(*p).layout_file, set_text_select=[ns-ks,ks]
	widget_control, (*pstate).layout_file, set_value=(*p).layout_file, set_text_select=[ns-1,0]

	area = (*p).diameter * (*p).diameter
	if (*p).shape_mode eq 0 then area = area * !pi/4.
	(*p).area = area
	widget_control, (*pstate).area_text, set_value=str_tidy((*p).area)
	(*p).solid_angle = 0.0
	if (*p).distance gt 0.0001 then begin
		(*p).solid_angle = 1000.* (*p).area / ((*p).distance*(*p).distance)
	endif
	widget_control, (*pstate).detector_solid_text, set_value=str_tidy((*p).solid_angle)

	if (*p).array_mode and (strlen((*p).layout_file) gt 0) then begin
		d = read_detector_layout((*p).layout_file, error=error)
		if error eq 0 then *(*p).playout = d
	endif

;	filters

	(*p).n_filters = n
	widget_control, (*pstate).filter_number, set_combobox_select=n-1

	if (n-1) lt (*pstate).active then begin
		widget_control, (*pstate).filter_base[(*pstate).active], map=0
		(*pstate).active = (*pstate).active < (n-1)
		widget_control, (*pstate).filter_base[(*pstate).active], map=1
		widget_control, (*pstate).define_filter, set_combobox_select=(*pstate).active
	endif

	if n gt 0 then begin

		(*p).filter[0:n-1] = (*pd).absorbers[0:n-1]

		for i=0L,(*p).n_filters-1 do begin
			z = atomic_number((*p).filter[i].formula)
			dz = density(z)

			if (*p).filter[i].microns and ((*p).filter[i].density gt 1.0e-8) then begin
				(*p).filter[i].thick = 10.0 * (*p).filter[i].thick / (*p).filter[i].density
				(*p).filter[i].microns = ((*p).filter[i].density lt 0.03) ? 2: 1
				if (*p).filter[i].microns eq 2 then (*p).filter[i].thick = (*p).filter[i].thick/1000. 
			endif else if (z gt 0) and (dz gt 0) and (*p).filter[i].microns then begin
				(*p).filter[i].density = dz
				(*p).filter[i].thick = 10.0 * (*p).filter[i].thick / (*p).filter[i].density
				(*p).filter[i].microns = ((*p).filter[i].density lt 0.03) ? 2: 1
				if (*p).filter[i].microns eq 2 then (*p).filter[i].thick = (*p).filter[i].thick/1000. 
			endif else begin
				(*p).filter[i].microns = 0
			endelse
			if (*p).filter[i].microns eq 2 then (*p).filter[i].pinhole = 0

			widget_control, (*pstate).thick_text[i], set_value=str_tidy((*p).filter[i].thick)
			widget_control, (*pstate).density_text[i], set_value=str_tidy((*p).filter[i].density)

			widget_control, (*pstate).thick_mode[i], set_combobox_select=(*p).filter[i].microns
			widget_control, (*pstate).weight_mode[i], set_combobox_select=(*p).filter[i].weight
			widget_control, (*pstate).formula_text[i], set_value=(*p).filter[i].formula

			widget_control, (*pstate).pinhole_mode[i], set_combobox_select=(*p).filter[i].pinhole
			widget_control, (*pstate).ratio_text[i], set_value=str_tidy((*p).filter[i].pinratio)

			case (*p).filter[i].pinhole of
				0: begin
					map_pinhole = 0
					map_bragg = 0
					end
				1: begin
					map_pinhole = 1
					map_bragg = 0
					end
				2: begin
					map_pinhole = 0
					map_bragg = 1
					end
			endcase
			widget_control, (*pstate).ratio_base[i], map=map_pinhole
			widget_control, (*pstate).bragg_base[i], map=map_bragg
			widget_control, (*pstate).density_base[i], map=(*p).filter[i].microns
		endfor
	endif

	if ptr_valid(pd) then begin
		if ptr_valid((*pstate).detector) then ptr_free, (*pstate).detector
		(*pstate).detector = pd
	endif

;	update_detector_struct, pstate
	return

bad:
	warning,'load_detector_parameters','bad input parameters'
	goto, done
bad_io:
	warning,'load_detector_parameters','error reading detector file'
	goto, done
bad_state:
	warning,'load_detector_parameters','bad state structure'
	goto, done
bad_ptr:
	warning,'load_detector_parameters','bad parameter pointer or structure'
	goto, done
done:
	return
	end

;------------------------------------------------------------------------------------------

pro save_detector_parameters, pstate, file

;	Save the parameters in '(*pstate).p' to 'file' (.detector)

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
		warning,'Save_detector_parameters',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, done
	endif
endif

	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
	if n_elements(file) lt 1 then goto, bad
	p = (*pstate).p
	if ptr_valid(p) eq 0 then goto, bad_ptr
	if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr

	update_detector_struct, pstate

	write_detector, (*pstate).detector, file, error=error
	if error then goto, bad_io
	return

bad_io:
	warning,'save_detector_parameters','error writing detectors file'
	goto, done
bad:
	warning,'save_detector_parameters','bad input parameters'
	goto, done
bad_state:
	warning,'save_detector_parameters','bad state structure'
	goto, done
bad_ptr:
	warning,'save_detector_parameters','bad parameter pointer or structure'
	goto, done
done:
	return
	end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_filter_number, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).n_filters-1
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_crystal_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).crystal_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_crystal_shape, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).shape_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_array_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).array_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_model_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).gamma
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_thick_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = (fix(s[0]) > 0) < ((*pstate).n_filters_max-1)

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).filter[n].microns
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_weight_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = (fix(s[0]) > 0) < ((*pstate).n_filters_max-1)

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).filter[n].weight
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_pinhole_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = (fix(s[0]) > 0) < ((*pstate).n_filters_max-1)

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).filter[n].pinhole
endif
end

;-----------------------------------------------------------------

pro OnRealize_Detector, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=wid
(*pstate).wid = wid
(*pstate).draw = wWidget

window, /free, xsize=(*pstate).width, ysize=(*pstate).height, /pixmap
(*pstate).pix = !d.window

update_detector_struct, pstate, error=error
if (error eq 0) then detector_draw, pstate
end

;------------------------------------------------------------------------------------------

pro detector_setup, group_leader=group, TLB=tlb, pars=p, path=path, _extra=extra, xoffset=xoffset, yoffset=yoffset

COMPILE_OPT STRICTARR
common c_geopixe_vm, geopixe_enable_vm
if n_elements(geopixe_enable_vm) lt 1 then geopixe_enable_vm=1
startupp

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
		warning,'detector_setup',['IDL run-time error caught.', '', $
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
		symbol = 'SYMBOL*12'
		large_font = 'Arial*12'
;@2		widget_control, default_font='Geneva*10'		; set font for all windows
		help_xsize = 88
		mode_xsize = 68
		spc_filters = 2
		xsize_nfilters = 90
		xsize_deffilter= 90
		xsize_atomic = 130
		col_widths = 110
		special_xsize = 157
		formula_xsize = 193
		retain = 1
		draw_xsize = 480
		draw_ysize = 533
		yw = 400
		end
	'unix': begin
		symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
		large_font = '10x20'
;@2		widget_control, default_font='6x13'				; set font for all windows
		help_xsize = 42
		mode_xsize = 50
		spc_filters = 5
		xsize_nfilters = 90
		xsize_deffilter= 90
		xsize_atomic = 144
		col_widths = 88
		special_xsize = 140
		formula_xsize = 180
		retain = 2
		draw_xsize = 480
		draw_ysize = 640
		yw = 620
		end
	else: begin
		symbol = 'SYMBOL*BOLD*14'
		large_font = 'COURIER*BOLD*10'
;		widget_control, default_font='Arial*14'				; set font for all windows
		help_xsize = 42
		mode_xsize = 50
		spc_filters = 5
		xsize_nfilters = 90
		xsize_deffilter= 90
		xsize_atomic = 120
		col_widths = 88
		special_xsize = 157
		formula_xsize = 193
		retain = 1
		draw_xsize = 480
		draw_ysize = 580
		yw = 400
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
	xoffset = ((xoff - 402) < (screen[0]-34 - 402)) > 0
endif
if n_elements(yoffset) lt 1 then begin
	screen = get_screen_size()
	yoffset = ((yoff - yw+h) < (screen[1]-28 - yw)) > 0
endif

p = bad_pars_struct( p, make_pars=make_p)

; Note that the detector thickness is in mm while all filter thicknesses are in mg/cm^2.

if make_p then begin
	n_filters = 20												; maxmimum # filters
	filter = define(/filter)
	lay = make_layer('Si',1.0)

	pars = {	$
			title:				'', $							; composite filter title
			detector_file:		'', $							; detector file name
			crystal:			lay, $							; crystal layer struct
			crystal_mode:		0, $							; crystal type mode
			formula:			'Si', $							; crystal
			thick:				0.0, $							; crystal thick (mm)
			density:			0.0, $							; density
			diameter:			0.0, $							; diameter (mm)
			area:				0.0, $							; area (mm^2)
			distance:			0.0, $							; distance (mm)
			solid_angle:		0.0, $							; solid angle (msr)
			tilt:				0.0, $							; tilt (degrees)
			resolution:			0.16, $							; resolution (keV)
			layout_file:		'', $							; layout file name
			shape_mode:			0, $							; shape (0=round, 1=square)
			array_mode:			0, $							; 0=single, 1=array detector
			playout:			ptr_new(/allocate_heap), $		; pointer to layout data
			cohen:				0, $							; Cohen factor?
			correct_solid_angle: 1, $							; enable volume solid-amgle correction
			
			gamma:				0, $							; flags a gamma-ray detector
			a:					fltarr(6), $					; polynomial terms
			e_low:				0.1, $							; applicable E range
			e_high:				10.0, $							;
			n_filters:			1, $							; # filters
			n_filters_max:		n_filters, $					; maximum number of filters
			filter:				replicate(filter,n_filters) $	; filter details
	}
	*p = pars
endif
n_filters_max = (*p).n_filters_max

; 	top-level base

tlb = widget_base( /column, title='Detector Setup & Edit', /TLB_KILL_REQUEST_EVENTS, $
				group_leader=group, _extra=extra, uname='detector_TLB', $
				/tlb_size_events, xoffset=xoffset, yoffset=yoffset, /base_align_center)

rbase = widget_base( tlb, /row, xpad=5, ypad=5, space=5, /base_align_top, /align_top)
tbase = widget_base( rbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

; set-up file buttons

sbase = widget_base( tbase, /row, /base_align_center, ypad=1, xpad=0, space=5, /align_right)
lab = widget_label( sbase, value='File:')
detector_file = widget_text( sbase, value=(*p).detector_file, uname='detector-file', /tracking, /editable, $
					uvalue='Enter the name of a detector file to retrieve retrieve and edit.',scr_xsize=243)
load_setup_button = widget_button( sbase, value='Load', uname='load-detector-button', /tracking, $
					uvalue='Load detector parameters from a previous detector file.', scr_xsize=38)
save_setup_button = widget_button( sbase, value='Save', uname='save-detector-button', /tracking, $
					uvalue='Save detector parameters to a detector file.', scr_xsize=38)

titlebase = widget_base( tbase, /row, /base_align_center, ypad=0, xpad=0, space=5, /align_right)
lab = widget_label( titlebase, value='Title:')
title_text = widget_text( titlebase, value=(*p).title, uname='title-text', /tracking, /editable, $
					uvalue='Enter a title descriptor for this composite detector.',scr_xsize=333)

; detector mode

dbase1 = widget_base( tbase, /row, /base_align_center, ypad=0, xpad=0, space=5, /align_right)
lab = widget_label( dbase1, value='Material:')
crystal_mode = widget_combobox( dbase1, value=[' Si  (Si(Li) or Si drift)',' Ge  (intrinsic Ge)'], uname='crystal-mode', /tracking, $
					notify_realize='OnRealize_detector_crystal_mode', $
					uvalue='Select the type of detector crystal.', xsize=141)

lab = widget_label( dbase1, value='  Model:')
detector_model = widget_combobox( dbase1, value=[' X-ray detector model',' Exponential poly fit'], uname='detector-mode', /tracking, $
					notify_realize='OnRealize_detector_model_mode', $
					uvalue='Select the type of detector calibration model.', xsize=141)

map_base = widget_base( tbase, /base_align_center, ypad=0, xpad=0, space=0)
map_base0 = widget_base( map_base, /column, /base_align_center, ypad=0, xpad=0, space=5, map=1, /align_right)
map_base1 = widget_base( map_base, /column, /base_align_center, ypad=0, xpad=0, space=5, map=0, /align_center)

; detector details

pars_xsize = 60
pars_xpad = 1
dbase2a = widget_base( map_base0, column=3, /base_align_right, ypad=0, xpad=0, space=3, /align_right)
dbase2 = widget_base( dbase2a, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase2, value='Diameter:')
diameter_text = widget_text( dbase2, value=str_tidy((*p).diameter), uname='diameter-text', /tracking, /editable, $
					uvalue='Enter the detector diameter (mm); this will update area and solid-angle (hit <return>). ' + $
					'Note that aperture "pin-hole filters" inside the detector may reduce this. ' + $
					'For array detectors, individual dimensions come from the layout file.', scr_xsize=pars_xsize)

dbase3 = widget_base( dbase2a, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase3, value='Area:')
area_text = widget_text( dbase3, value=str_tidy((*p).area), uname='area-text', /tracking, /editable, $
					uvalue='Enter the detector area (mm^2); this will update diameter and solid-angle (hit <return> to update others). Alternatively, enter the diameter (mm) above. ' + $
					'Note that aperture "pin-hole filters" inside the detector may reduce this.', scr_xsize=pars_xsize)

dbase5c = widget_base( dbase2a, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase5c, value='Tilt Angle:')
detector_tilt_text = widget_text( dbase5c, value=str_tidy((*p).tilt), uname='detector-tilt', /tracking, /editable, $
					uvalue='Enter the detector global tilt angle, for the detector or array, away from pointing directly at target (degrees). At zero tilt angle, the detector points at the focussed beam-spot.',scr_xsize=pars_xsize)

dbase4 = widget_base( dbase2a, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase4, value='Distance:')
distance_text = widget_text( dbase4, value=str_tidy((*p).distance), uname='distance-text', /tracking, /editable, $
					uvalue='Enter the distance from target to the front-face of the active volume of the detector crystal; this will update solid-angle (hit <return> to update it). ' + $
					'(There is commonly ~5 mm from detector crystal to window face.)',scr_xsize=pars_xsize)

dbase5b = widget_base( dbase2a, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase5b, value='Solid Angle:')
detector_solid_text = widget_text( dbase5b, value=str_tidy((*p).thick), uname='detector-solid-text', /tracking, $
					uvalue='Solid-angle of the detector or total for the active (not bad or vetoed) elements of the array (msr), as read from the layout file. ' + $
					'Note that aperture "pin-hole filters" inside the detector may reduce this.',scr_xsize=pars_xsize)

dbase5 = widget_base( dbase2a, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase5, value='Thickness:')
detector_thick_text = widget_text( dbase5, value=str_tidy((*p).thick), uname='detector-thick-text', /tracking, /editable, $
					uvalue='Enter the thickness of the active volume of the detector (mm).',scr_xsize=pars_xsize)

dbase6 = widget_base( dbase2a, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase6, value='Resolution:')
resolution_text = widget_text( dbase6, value=str_tidy((*p).resolution), uname='resolution-text', /tracking, /editable, $
					uvalue='Enter the detector working resolution (keV). This is defined at 5.895 keV for X-rays and at 1.332 MeV for gamma-rays.',scr_xsize=pars_xsize)

; detector array?

dbase6 = widget_base( map_base0, /row, /base_align_center, ypad=0, xpad=0, space=5, /align_right)
lab = widget_label( dbase6, value='Shape:')
shape_mode = widget_combobox( dbase6, value=[' Round, Circular ',' Square, Rectangular '], uname='crystal-shape', /tracking, $
					notify_realize='OnRealize_detector_crystal_shape', $
					uvalue='Select the shape of each detector crystal.', xsize=141, sensitive=(*p).array_mode eq 0)

lab = widget_label( dbase6, value='   Array:')
array_mode = widget_combobox( dbase6, value=[' Single detector',' Array detector'], uname='array-mode', /tracking, $
					notify_realize='OnRealize_detector_array_mode', $
					uvalue='Select single detector or detector array. The latter will bring up detector layout specification options.', xsize=141)

; layout file buttons

layout_base = widget_base( map_base0, /row, /base_align_center, ypad=0, xpad=0, space=5, /align_right, map=(*p).array_mode)
lab = widget_label( layout_base, value='Layout:')
layout_file = widget_text( layout_base, value=(*p).layout_file, uname='layout-file', /tracking, /editable, $
					uvalue='Enter the name of a detector layout CSV file that specifies the position, size, etc. of detector elements in an array.',scr_xsize=262)
load_layout_button = widget_button( layout_base, value='Load', uname='load-layout-button', /tracking, $
					uvalue='Load detector layout parameters from a layout CSV file that specifies the position, size, etc. of detector elements in an array.', scr_xsize=38)
layout_popup_button = widget_button( layout_base, value='?', uname='detector-layout-popup', /tracking, $
					uvalue='Open the detector layout window to view the layout representation.', scr_xsize=20)

; options

obase = widget_base( map_base0, /row, /align_center, /base_align_left, ypad=0, xpad=0, space=3)
options = cw_bgroup2( obase, ['Scale factor of Cohen et al.','Volume solid-angle correction'], /row, xpad=0, ypad=0, space=15, $
					/return_index, /tracking, uname='options', set_value=[0,1], /nonexclusive, $
					uvalue=['Check this option to enable the Cohen et al. detector efficiency multiplicative factor of 0.717.', $
					'Enable correction for the effective solid-angle reduction through the volume of a thick detector for increasing energy X-rays.'])
; absorber details

laybase = widget_base( map_base0, /column, /base_align_center, /align_right, ypad=1, xpad=2, space=spc_filters, /frame)
lab = widget_label( laybase, value='Absorber Layer Definitions', /align_center)

lay2base = widget_base( laybase, /row, /base_align_center, ypad=0, xpad=0, space=2)
lab = widget_label( lay2base, value='# absorbers:')
filter_number = widget_combobox( lay2base, value=str_tidy(indgen(n_filters_max)+1), uname='filter-number', /tracking, $
					notify_realize='OnRealize_detector_filter_number', $
					uvalue='Select the number of layers in this composite filter.', xsize=xsize_nfilters)

lab = widget_label( lay2base, value='   Define absorber:')
define_filter = widget_combobox( lay2base, value=str_tidy(indgen(n_filters_max)+1), uname='define-filter', /tracking, $
					uvalue='Select a filter by number, for editing. The parameters for the selected filter will ' + $
					'be displayed in the panel below for editing.', xsize=xsize_deffilter)

; The map base areas for setting up 'n_filters_max' filters ...

mapbull = widget_base( laybase, /align_center)
filter_base = lonarr(n_filters_max)
thick_text = lonarr(n_filters_max)
thick_mode = lonarr(n_filters_max)
density_base = lonarr(n_filters_max)
density_text = lonarr(n_filters_max)
weight_mode = lonarr(n_filters_max)
formula_text = lonarr(n_filters_max)
ratio_text = lonarr(n_filters_max)
ratio_base = lonarr(n_filters_max)
pinhole_mode = lonarr(n_filters_max)
bragg_base = lonarr(n_filters_max)
bragg_button = lonarr(n_filters_max)

map = 1
for i=0L,n_filters_max-1 do begin
	filter_base[i] = widget_base( mapbull, /column, /frame, map=map, /base_align_right, /align_center, xpad=2, ypad=1, space=2)
	lab = widget_label( filter_base[i], value='Absorber Details', /align_center)

	ll1base = widget_base( filter_base[i], /row, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( ll1base, value='Thick:')
	thick_text[i] = widget_text( ll1base, value=str_tidy((*p).filter[i].thick), uname='thick-text', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the thickness of the selected filter layer (in either mg/cm^2, microns or mm (for a Gas at NPT)).', scr_xsize=70)
	thick_mode[i] = widget_combobox( ll1base, value=[' mg/cm^2',' microns',' Gas (mm NPT)'], uname='thick-mode', /tracking, $
					notify_realize='OnRealize_detector_thick_mode', $
					uvalue=str_tidy(i)+'  Select filter layer thickness in "mg/cm^2", "microns" or "mm" (for a Gas at NPT). ' + $
					'If microns is selected, a new box appears for entry of density of a compound. ' + $
					'For a gas NPT conditions (P=1013.25 mbar, T=20C) are assumed; hit <return> on Formula or Thickness to calculate an ideal gas density.', xsize=120)

	density_base[i] = widget_base( ll1base, /row, map=(*p).filter[i].microns, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( density_base[i], value=' Density:')
	density_text[i] = widget_text( density_base[i], value=str_tidy((*p).filter[i].density), uname='filter-density', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the density of a compound composition filter layer (g/cm^3). '+ $
					'For pure element filters the density will be obtained from the database automatically .' + $
					'For a Gas, hit <return> on Formula or Thickness to calculate an ideal gas density; NPT conditions (P=1013.25 mbar, T=20C) are assumed.', scr_xsize=72)

	ll2base = widget_base( filter_base[i], /row, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( ll2base, value='Formula:')
	formula_text[i] = widget_text( ll2base, value=(*p).filter[i].formula, uname='filter-formula', /tracking, /editable, ysize=1,  $
					uvalue=str_tidy(i)+'  Enter the chemical formula for the layer. ' + $
					'Enclose radicals in brackets "( )", with optional multipliers in atomic fraction or weight %. ' + $
					'e.g. Components in wt%: "(SiO2)18.3(MgO)34.3"; atomic proportions: "FeAsS". ' + $
					'For a Gas, enclose gas molecules in the brackets "( )".', scr_xsize=formula_xsize)

	weight_mode[i] = widget_combobox( ll2base, value=[' Atomic Fraction','  Weight %'], uname='weight-mode', /tracking, $
					notify_realize='OnRealize_detector_weight_mode', $
					uvalue=str_tidy(i)+'  Select the type of multiplier factors in formulae. These appear after ' + $
					'a radical term contained in brackets, e.g. "(SiO2)12.3". Options include "atomic fraction" or "weight %".', xsize=xsize_atomic)

	ll3base = widget_base( filter_base[i], /row, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( ll3base, value='Special:')
	pinhole_mode[i] = widget_combobox( ll3base, value=['Plain filter','Pinhole filter','Bragg filter'], uname='pinhole-mode', /tracking, $
					notify_realize='OnRealize_detector_pinhole_mode', $
					uvalue=str_tidy(i)+'  Select special filter types. ' + $
					'If "pinhole filter" is selected, a new box appears for entry of the solid-angle ratio for the pinhole. ' + $
					'This is the ratio of the detector solid-angle divided by the hole solid-angle.', xsize=special_xsize)

	case (*p).filter[i].pinhole of
		0: begin
			map_pinhole = 0
			map_bragg = 0
			end
		1: begin
			map_pinhole = 1
			map_bragg = 0
			end
		2: begin
			map_pinhole = 0
			map_bragg = 1
			end
	endcase

;	map_base = widget_base( ll3base, ypad=0, xpad=0)
	ratio_base[i] = widget_base( ll3base, /row, map=map_pinhole, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( ratio_base[i], value=' Solid-angle ratio:')
	ratio_text[i] = widget_text( ratio_base[i], value=str_tidy((*p).filter[i].pinratio), uname='filter-pinratio', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the pin-hole filter solid-angle ratio. '+ $
					'This is the ratio of the detector solid-angle divided by the hole solid-angle.', scr_xsize=70)

	bragg_base[i] = widget_base( map_base, /row, map=map_bragg, /base_align_center, /align_right, ypad=3, xpad=0, space=3)
	bragg_button[i] = widget_button( bragg_base[i], value='      Edit Bragg     ', uname='bragg-button', /tracking, uvalue=str_tidy(i)+'  Open Bragg filter editor.')
	map = 0
endfor

; Exponential of polynomial model

lab = widget_label( map_base1, value='')
lab = widget_label( map_base1, value='Enter coefficients of an exponential of a polynomial in log(E)   [keV]')

pbase = widget_base( map_base1, column=2, /base_align_right, ypad=0, xpad=0, space=3)
pbase1 = widget_base( pbase, /row, /base_align_center, ypad=0, xpad=10, space=5)
lab = widget_label( pbase1, value='A[0]:')
a0_text = widget_text( pbase1, value=str_tidy((*p).diameter), uname='a0-text', /tracking, /editable, $
					uvalue='Enter the exponential polynomial constant A[0].',scr_xsize=70)

pbase2 = widget_base( pbase, /row, /base_align_center, ypad=0, xpad=10, space=5)
lab = widget_label( pbase2, value='A[1]:')
a1_text = widget_text( pbase2, value=str_tidy((*p).diameter), uname='a1-text', /tracking, /editable, $
					uvalue='Enter the exponential polynomial log(E) constant A[1].',scr_xsize=70)

pbase3 = widget_base( pbase, /row, /base_align_center, ypad=0, xpad=10, space=5)
lab = widget_label( pbase3, value='A[2]:')
a2_text = widget_text( pbase3, value=str_tidy((*p).diameter), uname='a2-text', /tracking, /editable, $
					uvalue='Enter the exponential polynomial log(E)^2 constant A[2].',scr_xsize=70)

pbase4 = widget_base( pbase, /row, /base_align_center, ypad=0, xpad=10, space=5)
lab = widget_label( pbase4, value='A[3]:')
a3_text = widget_text( pbase4, value=str_tidy((*p).diameter), uname='a3-text', /tracking, /editable, $
					uvalue='Enter the optional exponential polynomial log(E)^3 constant A[3].',scr_xsize=70)

pbase5 = widget_base( pbase, /row, /base_align_center, ypad=0, xpad=10, space=5)
lab = widget_label( pbase5, value='A[4]:')
a4_text = widget_text( pbase5, value=str_tidy((*p).diameter), uname='a4-text', /tracking, /editable, $
					uvalue='Enter the optional exponential polynomial log(E)^4 constant A[4].',scr_xsize=70)

pbase6 = widget_base( pbase, /row, /base_align_center, ypad=0, xpad=10, space=5)
lab = widget_label( pbase6, value='A[5]:')
a5_text = widget_text( pbase6, value=str_tidy((*p).diameter), uname='a5-text', /tracking, /editable, $
					uvalue='Enter the optional exponential polynomial log(E)^5 constant A[5].',scr_xsize=70)

; Applicable energy range

lab = widget_label( map_base1, value='')
lab = widget_label( map_base1, value='Enter the energy range of applicability of this calibration.  [keV]')

ebase = widget_base( map_base1, column=2, /base_align_right, ypad=0, xpad=0, space=3)
ebase1 = widget_base( ebase, /row, /base_align_center, ypad=0, xpad=10, space=5)
lab = widget_label( ebase1, value='E min:')
e_low_text = widget_text( ebase1, value=str_tidy((*p).e_low), uname='e_low-text', /tracking, /editable, $
					uvalue='Enter the lowest energy (keV) of applicability of this calibration polynomial.',scr_xsize=70)

ebase2 = widget_base( ebase, /row, /base_align_center, ypad=0, xpad=10, space=5)
lab = widget_label( ebase2, value='E max:')
e_high_text = widget_text( ebase2, value=str_tidy((*p).e_high), uname='e_high-text', /tracking, /editable, $
					uvalue='Enter the highest energy (keV) of applicability of this calibration polynomial.',scr_xsize=70)

draw = widget_draw( rbase, uname='draw', xsize=draw_xsize, ysize=draw_ysize, notify_realize='OnRealize_Detector', $
			retain=retain, /button_events)

; Buttons

;bbase = widget_base( tbase, /row, /base_align_center, ypad=0, space=15)
;button = widget_button( bbase, value='Calculate Transmission', uname='calculate-button', /tracking, $
;					uvalue='Calculate X-ray transmissioin curve, using the current parameters. ' + $
;					'The calculation includes all X-ray detector defined.')
;lab = widget_label( bbase, value='                         ')
;button = widget_button( bbase, value=' Close ', uname='close-button', /tracking, $
;					uvalue='Exit the Filter Setup popup window. The parameters will be recovered if you reopen this window later.')

;.................................................................................

t2base = widget_base( tbase, /column, xpad=0, ypad=2, space=1, /base_align_center, /align_center)
cursor_base = widget_base( t2base, /row, /base_align_center, /align_right, ypad=0, xpad=0, space=3)
lab = widget_label( cursor_base, value='Cursor:')
cursor_text = widget_text( cursor_base, value=' ', uname='cursor-text', /tracking, /editable, $
				uvalue='Enter the Energy (keV) for the cursor to display corresponding efficiency, or drag the energy cursor on the plot, or click line energy in X-ray Identification window.', scr_xsize=70)
lab = widget_label( cursor_base, value=' Efficiency:')
efficiency_text = widget_text( cursor_base, value=' ', uname='efficiency-text', /tracking, $
				uvalue='Shows detector efficiency (intrinsic for X-ray, total for gamma-ray model) at the cursor energy. ' + $
				'Enter the Energy (keV) for the cursor in the left widget, or drag the cursor energy, or click line energy in X-ray Identification window.', scr_xsize=80)
lab = widget_label( cursor_base, value='    Total:')
total_text = widget_text( cursor_base, value=' ', uname='total-text', /tracking, $
				uvalue='Shows detector total efficiency (including solid-angle effect) at the cursor energy. ' + $
				'Enter the Energy (keV) for the cursor in the left widget, or drag the cursor energy, or click line energy in X-ray Identification window.', scr_xsize=80)

overlay_base = widget_base( t2base, /row, /base_align_center, /align_right, ypad=0, xpad=0, space=3)
lab = widget_label( overlay_base, value='Relative to overlay:')
lab = widget_label( overlay_base, value='    Rel-Eff:')
overlay_efficiency_text = widget_text( overlay_base, value=' ', uname='overlay-efficiency-text', /tracking, $
				uvalue='Shows detector efficiency (intrinsic for X-ray, total for gamma-ray model) at the cursor energy, relative to the overlay. ', scr_xsize=80)
lab = widget_label( overlay_base, value=' Rel-Tot:')
overlay_total_text = widget_text( overlay_base, value=' ', uname='overlay-total-text', /tracking, $
				uvalue='Shows detector total efficiency (including solid-angle effect) at the cursor energy, relative to the overlay. ', scr_xsize=80)

obase = widget_base( t2base, /row, /base_align_center, ypad=0, xpad=0, space=5, /align_right)
lab = widget_label( obase, value='Overlay:')
overlay_text = widget_text( obase, value='', uname='load-detector-overlay', /tracking, /editable, $
					uvalue='Enter the name of a detector file to retrieve and overlay on graph; main detector in "green", overlay in "red", ratio of total efficiency to overlay in "violet" (x0.1).',scr_xsize=288)
load_overlay_button = widget_button( obase, value='Load', uname='load-detector-overlay', /tracking, $
					uvalue='Load detector parameters from a previous detector file to overlay on graph; main detector in "green", overlay in "red", ratio of total efficiency to overlay in "violet" (x0.1).', scr_xsize=38)

help = widget_text( tbase, scr_xsize=387, ysize=4, /wrap, uname='help', /tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

state = {	$
		path:				ptr_new(path), $		; pointer to current path
		p:					p, $					; pointer to parameters
		active:				0, $					; active filter for edit
		n_filters_max:		n_filters_max, $		; maximum number of filters
		title_text:			title_text, $			; ID of title text widget
		detector_file:		detector_file, $		; ID of filter file text widget
		filter_number:		filter_number, $		; ID of # filters droplist
		define_filter:		define_filter, $		; ID of filter define selection droplist

		draw:				draw, $					; ID of draw widget
		wid:				0, $					; window ID of draw widget
		pix:				0L, $					; pix-map
		width:				draw_xsize, $			; width of window
		height:				draw_ysize, $			; height of window
		energy:				0.0, $					; energy cursor

		detector:			ptr_new(make_detector()), $		; pointer to detector parameters

		map_base0:			map_base0, $			; base ID for normal model
		map_base1:			map_base1, $			; base ID for polynomial model
		crystal_mode:		crystal_mode, $			; ID array of crystal mode droplists
		detector_model:		detector_model, $		; ID of detector model
		shape_mode:			shape_mode, $			; ID shape droplist
		array_mode:			array_mode, $			; ID of array yes/no droplist
		layout_file:		layout_file, $			; ID of layout file name text widget
		layout_base:		layout_base, $			; ID of the layout file map base
		diameter_text:		diameter_text, $		; ID array of diameter text widgets
		detector_thick_text: detector_thick_text, $	; ID array of detector active thickness text widgets
		detector_solid_text: detector_solid_text, $	; ID array of detector active solid-angle text widgets
		area_text:			area_text, $			; ID array of detector area text widgets
		distance_text:		distance_text, $		; ID array of detector distance text widgets
		resolution_text:	resolution_text, $		; ID array of resolution text widgets
		detector_tilt_text: detector_tilt_text, $	; ID array of detector tilt angle text widgets
		a0_text:			a0_text, $				; ID of A[0] polynomial term
		a1_text:			a1_text, $				; ID of A[1] polynomial term
		a2_text:			a2_text, $				; ID of A[2] polynomial term
		a3_text:			a3_text, $				; ID of A[3] polynomial term
		a4_text:			a4_text, $				; ID of A[4] polynomial term
		a5_text:			a5_text, $				; ID of A[5] polynomial term
		e_low_text:			e_low_text, $			; ID of e_low
		e_high_text:		e_high_text, $			; ID of e_high text widget

		filter_base:		filter_base, $			; ID array of map bases for filters
		thick_text:			thick_text, $			; ID array of filter thickness text widgets
		thick_mode:			thick_mode, $			; ID array of thick mode droplists
		density_base:		density_base, $			; ID array of density base widgets for mapping
		density_text:		density_text, $			; ID array of density text widgets
		weight_mode:		weight_mode, $			; ID array of weight mode droplists
		formula_text:		formula_text, $			; ID array of formula text widgets
		ratio_text:			ratio_text, $			; ID array of formula text widgets
		ratio_base:			ratio_base, $			; ID array of density base widgets for mapping
		pinhole_mode:		pinhole_mode, $			; ID array of weight mode droplists
		bragg_base:			bragg_base, $
		bragg_button:		bragg_button, $
		options:			options, $				; ID of option button group
		cursor_text:		cursor_text, $			; ID of cursor energy text widget
		efficiency_text:	efficiency_text, $		; ID of efficiency widget
		total_text:			total_text, $			; ID of total efficiency widget

		overlay_text:		overlay_text, $			; ID of overlay file text widget
		overlay_file:		'', $					; overlay file name
		poverlay:			ptr_new(), $			; pointer to overlay detector parameters
		poverlay_layout:	ptr_new(/allocate_heap), $				; pointer to overlay layout parameters
		overlay_efficiency_text:	overlay_efficiency_text, $		; ID of overlay efficiency widget
		overlay_total_text:			overlay_total_text, $			; ID of overlay total efficiency widget

		help:				help $					; ID of help text
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['path', $				; new path
						'mark-e'], $			; mark line energy from Identify
					from=group

xmanager, 'detector_setup', tlb, /no_block

return
end
