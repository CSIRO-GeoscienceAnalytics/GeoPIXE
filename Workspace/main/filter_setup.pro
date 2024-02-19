;
;	filter setup and edit.

pro filter_setup_event, event

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
		warning,'filter_setup_event',['IDL run-time error caught.', '', $
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
		xoff = 425
		yoff = 15
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
					print,'filter Setup: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'mark-e': begin
				(*pstate).energy = (*event.pointer).e
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
				widget_control, (*pstate).help, set_value=' '
			endelse
		endif
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request filter_Setup ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'filter_TLB': begin
		if tag_names(event,/structure_name) eq 'WIDGET_BASE' then begin
			w = (event.x - xoff) > 300
			h = (event.y - yoff) > 338
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
			OnButton_filter, pstate, Event
			drag = 1
		endelse
		end

	'load-filter-button': begin
		file = find_file2( (*p).filter_file)
		path = extract_path( file[0])
;		if lenchr(path) eq 0 then path = *(*pstate).path
		if lenchr(path) eq 0 then path = geopixe_root
		F = file_requester( /read, filter = '*.filter', $
				/must_exist, path=path, group=event.top, $
				title='Select the source filter file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.filter'
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).filter_file, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).filter_file, set_value=F, set_text_select=[n-1,0]
			(*p).filter_file = F

			load_filter_parameters, pstate, F
		endif
		end

	'save-filter-button': begin
		file = find_file2( (*p).filter_file)
		path = extract_path( file[0])
;		if lenchr(path) eq 0 then path = *(*pstate).path
		if lenchr(path) eq 0 then path = geopixe_root
		F = file_requester( /write, filter = '*.filter', $
				path=path, group=event.top, $
				title='Save the filter definitions to a filter file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.filter'
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).filter_file, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).filter_file, set_value=F, set_text_select=[n-1,0]
			(*p).filter_file = F

			filter_update_pars, pstate
			save_filter_parameters, pstate, F
			notify, 'new-filters'
		endif
		end

	'filter-file': begin
		widget_control, event.id, get_value=F
		F = strip_file_ext(F[0]) + '.filter'
		(*p).filter_file = F
		*(*pstate).path = extract_path(F)
		n = lenchr(F)
		k = lenchr(strip_path(F))
		widget_control, (*pstate).filter_file, set_value=F, set_text_select=[n-k,k]
		widget_control, (*pstate).filter_file, set_value=F, set_text_select=[n-1,0]

		load_filter_parameters, pstate, F
		end

	'title-text': begin
		widget_control, event.id, get_value=s
		(*p).title = s[0]
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
		if (*p).filter[n].microns eq 2 then filter_update_density, pstate
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
		if (*p).filter[n].microns and ((*p).filter[n].density lt 0.001) then begin
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
			filter_update_density, pstate
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

	'cursor-text': begin
		widget_control, event.id, get_value=s
		(*pstate).energy = clip( float(s[0]), 0.1, 1000000.)
		end

	'calculate-button': begin

;		later calculate transmission and graph in plot area

		end

	'close-button': begin
		print,'Close filter setup ...'
		goto, kill
		end

	else:
endcase

update:
	filter_update_pars, pstate
	update_filter_struct, pstate, error=error

	transmission1 = float( transmit( (*pstate).filters, (*pstate).energy, /gamma) )
	transmission2 = float( transmit( (*pstate).filters, (*pstate).energy) )
;	print, (*pstate).energy, transmission1, transmission2
	widget_control, (*pstate).cursor_text, set_value=str_tidy((*pstate).energy)
	widget_control, (*pstate).absorb1_text, set_value=str_tidy(transmission1)
	widget_control, (*pstate).absorb2_text, set_value=str_tidy(transmission2)

	if (drag eq 0) and (error eq 0) then begin
		filter_draw, pstate
	endif

finish:
	widget_control, hourglass=0
	return

bad_state:
	warning,'filter_setup_event',['STATE variable has become ill-defined.','Abort filter Setup.'],/error
	goto, kill
bad_ptr:
	warning,'filter_setup_event',['Parameter structure variable has become ill-defined.','Abort filter Setup.'],/error
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
	widget_control, hourglass=0
	return
end

;------------------------------------------------------------------------------------------

pro filter_draw, pstate

	e = 10.^(findgen(50)/5.)
	t = transmit( (*pstate).filters, e, /photo)
	q = where((t gt 1.0e-5) and (abs(t[n_elements(t)-1]-t) gt t[n_elements(t)-1]*1.0e-2))		;t[n_elements(t)-1]
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
	transmission = transmit( (*pstate).filters, e, /gamma)
	top = 1.2*max(transmission)
	bot = 0.5*min(transmission) > 0.5e-4*top

	!x.title = 'Photon Energy (keV)'
	!y.title = 'Transmission'
	!p.title = ''
	!p.charsize = 1.0
	!p.thick = 1.0
	!p.charthick = 1.0
	wset, (*pstate).wid
	plot, e,transmission, xrange=[elow,ehigh],yrange=[bot,top], $
				color=spec_colour('white'), /xlog, /ylog, $
				ticklen=1.0, /nodata					, xstyle=1,ystyle=1
	oplot, e,transmission, color=spec_colour('green')

	wset, (*pstate).pix
	device,copy=[0,0,(*pstate).width,(*pstate).height, 0,0,(*pstate).wid]

	wset, (*pstate).wid
	if ((*pstate).energy gt elow) and ((*pstate).energy lt ehigh) then begin
		plots, [(*pstate).energy,(*pstate).energy], 10.^(!y.crange), color=spec_colour('orange')
	endif
	return
end

;------------------------------------------------------------------------------------------

pro filter_update_density, pstate

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
		warning,'filter_update_density',['IDL run-time error caught.', '', $
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

pro filter_update_pars, pstate, error=error

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
		warning,'filter_update_pars',['IDL run-time error caught.', '', $
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
	widget_control, (*pstate).filter_file, get_value=F
	(*p).filter_file = F[0]

	(*p).filter[0].name = (*p).title

	for i=0L,(*p).n_filters-1 do begin
		widget_control, (*pstate).thick_text[i], get_value=s
		(*p).filter[i].thick = float(s[0])
		widget_control, (*pstate).density_text[i], get_value=s
		(*p).filter[i].density = float(s[0])

		widget_control, (*pstate).formula_text[i], get_value=s
		t = ''
		for k=0L,n_elements(s)-1 do t = t+s[k]
		(*p).filter[i].formula = strcompress(t,/remove_all)

;		if (*p).filter[i].density lt 0.001 then begin
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

pro update_filter_struct, pstate, error=error

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
	found_pinhole = 0
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
		if f[i].pinhole gt 1.0e-6 then found_pinhole=found_pinhole+1
		if error2 then error=1
	endfor

;	if found_pinhole gt 1 then begin
;		warning,'filter-setup',['Multiple pin-hole filters found in filter stack.', $
;					'Simple filter model does not permit multiple pin-hole filters.', $
;					'Use only one pin-hole in any filter stack.']
;		error = 1
;	endif

	if ptr_valid((*pstate).filters) then ptr_free, (*pstate).filters
	(*pstate).filters = ptr_new( f, /no_copy)
	goto, done

bad:
	warning,'update_filter_struct','bad input parameters'
	goto, done
bad_state:
	warning,'update_filter_struct','bad state structure'
	goto, done
bad_ptr:
	warning,'update_filter_struct','bad parameter pointer or structure'
	goto, done
done:
	return
	end

;------------------------------------------------------------------------------------------------------

pro load_filter_parameters, pstate, file

;	Load the parameters into '(*pstate).p' from 'file' (.filter)

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
		warning,'Load_filter_parameters',['IDL run-time error caught.', '', $
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

	pf = read_filters( file, error=error)
	if error then goto, bad_io
	n = n_elements(*pf) < (*p).n_filters_max

	(*p).title = (*pf)[0].name
	widget_control, (*pstate).title_text, set_value=(*p).title

	(*p).n_filters = n
	widget_control, (*pstate).filter_number, set_combobox_select=n-1

	if (n-1) lt (*pstate).active then begin
		widget_control, (*pstate).filter_base[(*pstate).active], map=0
		(*pstate).active = (*pstate).active < (n-1)
		widget_control, (*pstate).filter_base[(*pstate).active], map=1
		widget_control, (*pstate).define_filter, set_combobox_select=(*pstate).active
	endif

	if n gt 0 then begin
		(*p).filter[0:n-1] = (*pf)[0:n-1]

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

	ptr_free, pf
	close, 2

	update_filter_struct, pstate
	return

bad:
	warning,'load_filter_parameters','bad input parameters'
	goto, done
bad_io:
	warning,'load_filter_parameters','error reading filter file'
	goto, done
bad_state:
	warning,'load_filter_parameters','bad state structure'
	goto, done
bad_ptr:
	warning,'load_filter_parameters','bad parameter pointer or structure'
	goto, done
done:
	close,1
	return
	end

;------------------------------------------------------------------------------------------

pro save_filter_parameters, pstate, file

;	Save the parameters in '(*pstate).p' to 'file' (.filter)

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
		warning,'Save_filter_parameters',['IDL run-time error caught.', '', $
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

	update_filter_struct, pstate, error=error
	if error then goto, bad_entry

	write_filters, (*pstate).filters, file, error=error
	if error then goto, bad_io
	return

bad_entry:
	warning,'save_filter_parameters',['Incomplete or invalid input parameters.','Check all entry boxes.']
	goto, done
bad_io:
	warning,'save_filter_parameters','error writing filters file'
	goto, done
bad:
	warning,'save_filter_parameters','bad input parameters'
	goto, done
bad_state:
	warning,'save_filter_parameters','bad state structure'
	goto, done
bad_ptr:
	warning,'save_filter_parameters','bad parameter pointer or structure'
	goto, done
done:
	return
	end

;------------------------------------------------------------------------------------------

pro OnRealize_Filter_filter_number, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).n_filters-1
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_Filter_thick_mode, wWidget

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

pro OnRealize_Filter_weight_mode, wWidget

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

pro OnRealize_Filter_pinhole_mode, wWidget

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

pro OnRealize_filter, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=wid
(*pstate).wid = wid
(*pstate).draw = wWidget

window, /free, xsize=(*pstate).width, ysize=(*pstate).height, /pixmap
(*pstate).pix = !d.window

update_filter_struct, pstate, error=error
if (error eq 0) then filter_draw, pstate
end

;------------------------------------------------------------------------------------------

pro filter_setup, group_leader=group, TLB=tlb, pars=p, path=path, _extra=extra, xoffset=xoffset, yoffset=yoffset

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
		warning,'filter_setup',['IDL run-time error caught.', '', $
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
		mode_xsize = 68
		spc_filters = 2
		xsize_nfilters = 90
		xsize_deffilter= 90
		xsize_atomic = 130
		col_widths = 110
		special_xsize = 157
		formula_xsize = 173
		draw_ysize = 300
		help_xsize = 400
		retain = 1
		end
	'unix': begin
		symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
		large_font = '10x20'
;@2		widget_control, default_font='6x13'				; set font for all windows
		mode_xsize = 50
		spc_filters = 5
		xsize_nfilters = 90
		xsize_deffilter= 90
		xsize_atomic = 140
		col_widths = 88
		special_xsize = 140
		formula_xsize = 167
		draw_ysize = 336
		help_xsize = 400
		retain = 2
		end
	else: begin
		symbol = 'SYMBOL*BOLD*14'
		large_font = 'COURIER*BOLD*10'
;		widget_control, default_font='Arial*14'				; set font for all windows
		mode_xsize = 50
		spc_filters = 5
		xsize_nfilters = 90
		xsize_deffilter= 90
		xsize_atomic = 120
		col_widths = 88
		special_xsize = 152
		formula_xsize = 191
		draw_ysize = 352
		help_xsize = 390
		retain = 1
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
	yoffset = ((yoff - 400+h) < (screen[1]-28 - 400)) > 0
endif

p = bad_pars_struct( p, make_pars=make_p)

if make_p then begin
	n_filters = 20												; maxmimum # filters
	filter = define(/filter)

	pars = {	$
			title:				'', $							; composite filter title
			filter_file:		'', $							; filter file name
			n_filters:			1, $							; # filters
			n_filters_max:		n_filters, $					; maximum number of filters
			filter:				replicate(filter,n_filters) $	; filter details
	}
	*p = pars
endif
n_filters_max = (*p).n_filters_max

; 	top-level base

tlb = widget_base( /column, title='Filter Setup & Edit', /TLB_KILL_REQUEST_EVENTS, $
				group_leader=group, _extra=extra, uname='filter_TLB', $
				/tlb_size_events, xoffset=xoffset, yoffset=yoffset, /base_align_center)

rbase = widget_base( tlb, /row, xpad=5, ypad=5, space=5, /base_align_top, /align_top)
tbase = widget_base( rbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

; set-up file buttons

sbase = widget_base( tbase, /row, /base_align_center, ypad=1, xpad=0, space=5)
lab = widget_label( sbase, value='File:')
filter_file = widget_text( sbase, value=(*p).filter_file, uname='filter-file', /tracking, /editable, $
					uvalue='Enter the name of a filter file to retrieve retrieve and edit.',scr_xsize=235)
load_setup_button = widget_button( sbase, value='Load', uname='load-filter-button', /tracking, $
					uvalue='Load filter parameters from a previous filter file.', scr_xsize=38)
save_setup_button = widget_button( sbase, value='Save', uname='save-filter-button', /tracking, $
					uvalue='Save filter parameters to a filter file.', scr_xsize=38)

titlebase = widget_base( tbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( titlebase, value='Title:')
title_text = widget_text( titlebase, value=(*p).title, uname='title-text', /tracking, /editable, $
					uvalue='Enter a title descriptor for this composite filter. This will appear in filter droplists [keep it brief].',scr_xsize=325)

; filter details

laybase = widget_base( tbase, /column, /base_align_center, /align_center, ypad=1, xpad=2, space=spc_filters, /frame)
lab = widget_label( laybase, value='Multilayer Filter Definitions', /align_center)

lay2base = widget_base( laybase, /row, /base_align_center, ypad=0, xpad=0, space=2)
lab = widget_label( lay2base, value='# filters:')
filter_number = widget_combobox( lay2base, value=str_tidy(indgen(n_filters_max)+1), uname='filter-number', /tracking, $
					notify_realize='OnRealize_Filter_filter_number', $
					uvalue='Select the number of layers in this composite filter.', xsize=xsize_nfilters)

lab = widget_label( lay2base, value='       Define filter:')
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
	filter_base[i] = widget_base( mapbull, /column, /frame, map=map, /base_align_center, /align_center, xpad=2, ypad=1, space=1)
	lab = widget_label( filter_base[i], value='Filter Details', /align_center)

	ll1base = widget_base( filter_base[i], /row, /base_align_center, /align_right, ypad=0, xpad=0, space=3)
	lab = widget_label( ll1base, value='Thick:')
	thick_text[i] = widget_text( ll1base, value=str_tidy((*p).filter[i].thick), uname='thick-text', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the thickness of the selected filter layer (in either mg/cm^2, microns or mm (for a Gas at NPT).', scr_xsize=70)
	thick_mode[i] = widget_combobox( ll1base, value=['   mg/cm^2','   microns','   Gas (mm NPT)'], uname='thick-mode', /tracking, $
					notify_realize='OnRealize_Filter_thick_mode', $
					uvalue=str_tidy(i)+'  Select filter layer thickness in "mg/cm^2", "microns" or "mm" (for a Gas at NPT). ' + $
					'If microns is selected, a new box appears for entry of density of compound composition filters. ' + $
					'For a gas NPT conditions (P=1013.25 mbar, T=20C) are assumed; hit <return> on Formula or Thickness to calculate an ideal gas density.', xsize=120)

	density_base[i] = widget_base( ll1base, /row, map=(*p).filter[i].microns, /base_align_center, ypad=3, xpad=0, space=3)
	lab = widget_label( density_base[i], value='    Density:')
	density_text[i] = widget_text( density_base[i], value=str_tidy((*p).filter[i].density), uname='filter-density', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the density of a compound composition filter layer (g/cm^3). '+ $
					'For pure element filters the density will be obtained from the database automatically .' + $
					'For a Gas, hit <return> on Formula or Thickness to calculate an ideal gas density; NPT conditions (P=1013.25 mbar, T=20C) are assumed.', scr_xsize=70)

	ll2base = widget_base( filter_base[i], /row, /base_align_center, /align_right, ypad=0, xpad=0, space=2)
	lab = widget_label( ll2base, value='Formula:')
	formula_text[i] = widget_text( ll2base, value=(*p).filter[i].formula, uname='filter-formula', /tracking, /editable, ysize=1,  $
					uvalue=str_tidy(i)+'  Enter the chemical formula for the layer. ' + $
					'Enclose radicals in brackets "( )", with optional multipliers in atomic fraction or weight %. ' + $
					'e.g. Components in wt%: "(SiO2)18.3(MgO)34.3"; atomic proportions: "FeAsS". ' + $
					'For a Gas, enclose gas molecules in the brackets "( )".', scr_xsize=formula_xsize)

	weight_mode[i] = widget_combobox( ll2base, value=[' Atomic Fraction','  Weight %'], uname='weight-mode', /tracking, $
					notify_realize='OnRealize_Filter_weight_mode', $
					uvalue=str_tidy(i)+'  Select the type of multiplier factors in formulae. These appear after ' + $
					'a radical term contained in brackets, e.g. "(SiO2)12.3". Options include "atomic fraction" or "weight %".', xsize=xsize_atomic)

	ll3base = widget_base( filter_base[i], /row, /base_align_center, /align_right, ypad=0, xpad=0, space=2)
	lab = widget_label( ll3base, value='Special:')
	pinhole_mode[i] = widget_combobox( ll3base, value=['Plain filter','Pinhole filter','Bragg filter'], uname='pinhole-mode', /tracking, $
					notify_realize='OnRealize_Filter_pinhole_mode', $
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

	map_base = widget_base( ll3base, ypad=0, xpad=0)
	ratio_base[i] = widget_base( map_base, /row, map=map_pinhole, /base_align_center, /align_right, ypad=3, xpad=0, space=3)
	lab = widget_label( ratio_base[i], value=' Solid-angle ratio:')
	ratio_text[i] = widget_text( ratio_base[i], value=str_tidy((*p).filter[i].pinratio), uname='filter-pinratio', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the pin-hole filter solid-angle ratio. '+ $
					'This is the ratio of the detector solid-angle divided by the hole solid-angle.', scr_xsize=70)

	bragg_base[i] = widget_base( map_base, /row, map=map_bragg, /base_align_center, /align_right, ypad=3, xpad=0, space=3)
	bragg_button[i] = widget_button( bragg_base[i], value='      Edit Bragg     ', uname='bragg-button', /tracking, uvalue=str_tidy(i)+'  Open Bragg filter editor.')
	map = 0
endfor

draw = widget_draw( rbase, uname='draw', xsize=350, ysize=draw_ysize, notify_realize='OnRealize_Filter', $
			retain=retain, /button_events)

; Buttons

;bbase = widget_base( tbase, /row, /base_align_center, ypad=0, space=15)
;button = widget_button( bbase, value='Calculate Transmission', uname='calculate-button', /tracking, $
;					uvalue='Calculate X-ray transmissioin curve, using the current parameters. ' + $
;					'The calculation includes all X-ray filters defined.')
;lab = widget_label( bbase, value='                         ')
;button = widget_button( bbase, value=' Close ', uname='close-button', /tracking, $
;					uvalue='Exit the Filter Setup popup window. The parameters will be recovered if you reopen this window later.')

;.................................................................................

cursor_base = widget_base( tbase, /row, /base_align_center, /align_right, ypad=3, xpad=0, space=3)
lab = widget_label( cursor_base, value='Cursor:')
cursor_text = widget_text( cursor_base, value=' ', uname='cursor-text', /tracking, /editable, $
				uvalue='Enter the Energy (keV) for the cursor to display corresponding absorption, or drag the energy cursor on the plot, or click line energy in X-ray Identification window.', scr_xsize=65)
lab = widget_label( cursor_base, value=' Transmission (1):')
absorb1_text = widget_text( cursor_base, value=' ', uname='absorb1-text', /tracking, $
				uvalue='Shows absorption at the cursor energy (Berger & Hubbell). Enter the Energy (keV) for the cursor in the left widget, or drag the cursor energy, or click line energy in X-ray Identification window.', scr_xsize=75)
lab = widget_label( cursor_base, value='(2):')
absorb2_text = widget_text( cursor_base, value=' ', uname='absorb2-text', /tracking, $
				uvalue='Shows absorption at the cursor energy (Mayer & Rimini). Enter the Energy (keV) for the cursor in the left widget, or drag the cursor energy, or click line energy in X-ray Identification window.', scr_xsize=75)

help = widget_text( tbase, scr_xsize=help_xsize, ysize=4, /wrap, uname='help', /tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

state = {	$
		path:			ptr_new(path), $		; pointer to current path
		p:				p, $					; pointer to parameters
		active:			0, $					; active filter for edit
		title_text:		title_text, $			; ID of title text widget
		filter_file:	filter_file, $			; ID of filter file text widget
		filter_number:	filter_number, $		; ID of # filters droplist
		define_filter:	define_filter, $		; ID of filter define selection droplist
		n_filters_max:	n_filters_max, $		; maximum number of filters

		draw:			draw, $					; ID of draw widget
		wid:			0, $					; draw ID
		pix:			0L, $					; pix-map
		width:			350, $					; width of window
		height:			draw_ysize, $			; height of window
		energy:			0.0, $					; energy cursor

		filters:		ptr_new(), $			; completed filters struct array

		filter_base:	filter_base, $			; ID array of map bases for filters
		thick_text:		thick_text, $			; ID array of filter thickness text widgets
		thick_mode:		thick_mode, $			; ID array of thick mode droplists
		density_base:	density_base, $			; ID array of density base widgets for mapping
		density_text:	density_text, $			; ID array of density text widgets
		weight_mode:	weight_mode, $			; ID array of weight mode droplists
		formula_text:	formula_text, $			; ID array of formula text widgets
		ratio_text:		ratio_text, $			; ID array of formula text widgets
		ratio_base:		ratio_base, $			; ID array of density base widgets for mapping
		pinhole_mode:	pinhole_mode, $			; ID array of weight mode droplists
		bragg_base:		bragg_base, $
		bragg_button:	bragg_button, $
		cursor_text:	cursor_text, $			; ID of cursor energy text widget
		absorb1_text:	absorb1_text, $			; ID of cursor absorption widget 1
		absorb2_text:	absorb2_text, $			; ID of cursor absorption widget 2

		help:			help $					; ID of help text
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['path', $				; new path
						'mark-e'], $			; mark line energy from Identify
					from=group

xmanager, 'filter_setup', tlb, /no_block
return
end
