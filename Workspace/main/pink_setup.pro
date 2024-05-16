
;	Pink beam setup and edit.

pro pink_setup_event, event
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
		warning,'pink_setup_event',['IDL run-time error caught.', '', $
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
					print,'pink setup: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'select-periodic': begin
				if ptr_valid( event.pointer) then begin
					(*p).mono.z = (*event.pointer).zon[0]
					widget_control, (*pstate).mono_element_button, set_value={VALUE: element_name((*p).mono.z)}
					e = e_line( (*p).mono.z, line_index('Ka_'))
					(*p).modata.mono[0] = e
					widget_control, (*pstate).mono_energy_text, set_value=str_tidy((*p).modata.mono[0])
					goto, update
				endif
				goto, finish
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
				widget_control, (*pstate).help, set_value='Green curve is current model results. Orange curve is previous loaded pink beam model. '
			endelse
		endif
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request pink_setup ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'pink_TLB': begin
		if tag_names(event,/structure_name) eq 'WIDGET_BASE' then begin
			w = (event.x - xoff) > 400
			h = (event.y - yoff) > 580
			widget_control, (*pstate).draw2, draw_xsize=w, draw_ysize=h
			(*pstate).width = w
			(*pstate).height = h
			wdelete, (*pstate).pix2
			window, /free, xsize=(*pstate).width, ysize=(*pstate).height, /pixmap
			(*pstate).pix2 = !d.window
		endif
		end

	'draw': begin
		if( Event.type eq 3 )then begin
;			OnViewport_corr, Event
		endif else begin
;			OnButton_pink, pstate, Event
			drag = 1
		endelse
		end

	'load-pink-button': begin
		file = find_file2( (*p).file)
		path = *(*pstate).path
		F = file_requester( /read, filter = '*.pink', $
				/must_exist, path=path, group=event.top, $
				title='Select the pink beam file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.pink'
			set_widget_text, (*pstate).pink_file, F
			src = read_pink( F, error=err)
			if err eq 0 then begin
				*(*pstate).path = extract_path(F)
				*(*pstate).plast = *p
				*p = src
				(*p).file = F
				n = (*p).n_filters
				if n gt 0 then f = pink_convert_filter_to_local( (*p).filters[0:n-1], error=error)
				(*p).filters[0:n-1] = f
				pink_setup_pars, pstate
			endif
		endif
		end

	'save-pink-button': begin
		file = find_file2( (*p).file)
		path = *(*pstate).path
		F = file_requester( /write, file=file, filter = '*.pink', $
			/noconfirm, path=path, group=event.top, $
			title='Save pink beam definitions and model spectrum to file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.pink'
			*(*pstate).path = extract_path(F)
			set_widget_text, (*pstate).pink_file, F
			pink_calculate, p, path=*(*pstate).path, /convert
			pink_setup_pars, pstate
			
			src = *p
			n = (*p).n_filters
			if n gt 0 then begin
				filt = pink_convert_filter_to_standard( (*p).filters[0:n-1], error=error)
				src.filters[0:n-1] = filt
			endif
			write_pink, src, F, error=err
			if err eq 0 then begin
				(*p).file = F
			endif
			notify, 'new-pink', p, from=event.top
		endif
		end

	'pink-file': begin
		widget_control, event.id, get_value=F
		F = strip_file_ext(F[0]) + '.pink'
		(*p).file = F
		set_widget_text, (*pstate).pink_file, F
		src = read_pink( F, error=err)
		if err eq 0 then begin
			*(*pstate).path = extract_path(F)
			*(*pstate).plast = *p
			*p = src
			(*p).file = F
			n = (*p).n_filters
			if n gt 0 then f = pink_convert_filter_to_local( (*p).filters[0:n-1], error=error)
			(*p).filters[0:n-1] = f
			pink_setup_pars, pstate
		endif
		end

	'title-text': begin
		widget_control, event.id, get_value=s
		(*p).title = s[0]
		end

	'load-fe-spectrum-button': begin
		file = find_file2( (*p).fe_spectrum_file)
		path = *(*pstate).path
		F = file_requester( /read, filter = ['*.txt','*.csv'], $
				/must_exist, path=path, group=event.top, $
				title='Select the pink beam spectrum file')
		if F ne '' then begin
			*(*pstate).path = extract_path(F)
			(*p).fe_spectrum_file = F
			set_widget_text, (*pstate).fe_spectrum_text, F
		endif
		end

	'fe-spectrum-file': begin
		widget_control, event.id, get_value=F
		(*p).fe_spectrum_file = F
		*(*pstate).path = extract_path(F)
		set_widget_text, (*pstate).fe_spectrum_text, F
		end

	'spot-text': begin
		widget_control, event.id, get_value=s
		(*p).modata.spot = float2(s[0])
		end

	'optics-mode': begin
		i = event.index

		i = 0
		widget_control, event.id,set_combobox_select=0			; disable for now

		case i of
			0: begin
				(*p).mono.mode = 0
				(*p).modata.mono[0] = 0
				(*p).poly.mode = 0
				end
			1: begin
				(*p).mono.mode = 1
				(*p).poly.mode = 0
				if (*p).mono.z gt 0 then (*p).modata.mono[0] = e_line( (*p).mono.z, line_index('Ka_'))
				widget_control, (*pstate).mono_energy_text, set_value=str_tidy((*p).modata.mono[0])
				end
			2: begin
				(*p).mono.mode = 0
				(*p).poly.mode = 1
				end
		endcase
		pink_setup_pars, pstate
		end
		
	'mono-element': begin
		select_periodic, group=event.top, tlb=tlb, zon=[(*p).mono.z], zstate=[1]
		register_notify, event.top, ['select-periodic'], from=tlb
		end

	'mono-energy-text': begin
		widget_control, event.id, get_value=s
		(*p).modata.mono[0] = float2(s[0])
		(*p).mono.z = 0
		widget_control, (*pstate).mono_element_button, set_value={VALUE: ''}
		end
	
	'mono-bw-text': begin
		widget_control, event.id, get_value=s
		(*p).modata.mono[1] = float2(s[0])
		end

	'mono-eff-text': begin
		widget_control, event.id, get_value=s
		(*p).modata.mono[2] = float2(s[0])
		end

	'poly-type': begin
		(*pstate).poly_active = event.index
		poly = get_poly_config( (*pstate).poly_list[ (*pstate).poly_active], error=err)
		if err eq 0 then begin
			(*p).poly = poly
			(*p).poly.mode = 1
			pink_setup_pars, pstate	
		endif
		end

	'poly-gain-text': begin
		widget_control, event.id, get_value=s
		(*p).poly.gain = float2(s[0])
		end
	
	'poly-energy-text': begin
		widget_control, event.id, get_value=s
		(*p).poly.energy = float2(s[0])
		end
	
	'poly-pinhole-text': begin
		widget_control, event.id, get_value=s
		(*p).poly.pinhole = float2(s[0])
		end
	
	'mirror-number': begin
		(*p).n_mirrors = event.index
		if (*p).n_mirrors eq 0 then begin
			widget_control, (*pstate).mirror_base[(*pstate).mactive], map=0
			(*pstate).mactive = 0
			widget_control, (*pstate).define_mirror, set_combobox_select=(*pstate).mactive
			goto, update
		endif
		if (*pstate).mactive ge (*p).n_mirrors then begin
			widget_control, (*pstate).mirror_base[(*pstate).mactive], map=0
			(*pstate).mactive = (*p).n_mirrors-1
		endif
		widget_control, (*pstate).mirror_base[(*pstate).mactive], map=1
		widget_control, (*pstate).define_mirror, set_combobox_select=(*pstate).mactive
		end

	'define-mirror': begin
		widget_control, (*pstate).mirror_base[(*pstate).mactive], map=0
		(*pstate).mactive = event.index
		widget_control, (*pstate).mirror_base[(*pstate).mactive], map=1
		if (*pstate).mactive gt (*p).n_mirrors then begin
			(*p).n_mirrors = (*pstate).mactive+1
			widget_control, (*pstate).mirror_number, set_combobox_select=(*p).n_mirrors
		endif
		end

	'mirror-title-text': begin
		widget_control, event.id, get_value=s
		widget_control, event.id, get_uvalue=u
		t = strsplit(string(u),/extract)
		n = (fix(t[0]) > 0) < (n_elements((*p).mirrors)-1)
		if n gt (*p).n_mirrors-1 then begin
			(*p).n_mirrors = n+1
			widget_control, (*pstate).mirror_number, set_combobox_select=(*p).n_mirrors
		endif
		(*p).mirrors[n].title = s
		end

	'load-mirror-file-button': begin
		file = find_file2( (*p).mirrors[(*pstate).mactive].file)
		path = *(*pstate).path
		F = file_requester( /read, filter = ['*.txt','*.csv'], $
				/must_exist, path=path, group=event.top, $
				title='Select the CXRO mirror reflectivity file')
		if F ne '' then begin
			*(*pstate).path = extract_path(F)
			(*p).mirrors[(*pstate).mactive].file = F
			set_widget_text, (*pstate).mfile_text[(*pstate).mactive], F
		endif
		end

	'mirror-file-text': begin
		widget_control, event.id, get_value=s
		widget_control, event.id, get_uvalue=u
		t = strsplit(string(u),/extract)
		n = (fix(t[0]) > 0) < (n_elements((*p).mirrors)-1)
		if n gt (*p).n_mirrors then begin
			(*p).n_mirrors = n+1
			widget_control, (*pstate).mirror_number, set_combobox_select=(*p).n_mirrors
		endif
		(*p).mirrors[n].file = s
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
		n = (fix(s[0]) > 0) < (n_elements((*p).filters)-1)
		if n gt (*p).n_filters-1 then begin
			(*p).n_filters = n+1
			widget_control, (*pstate).filter_number, set_combobox_select=(*p).n_filters-1
		endif
		(*p).filters[n].thick = t
		if (*p).filters[n].microns eq 2 then pink_update_density, pstate
		end

	'thick-mode': begin
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < (n_elements((*p).filters)-1)
		(*p).filters[n].microns = event.index
		if (*p).filters[n].microns eq 2 then (*p).filters[n].pinhole = 0

		case (*p).filters[n].pinhole of
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
		widget_control, (*pstate).pinhole_mode[n], set_combobox_select=(*p).filters[n].pinhole

		widget_control, (*pstate).density_base[n], map=(*p).filters[n].microns
		if (*p).filters[n].microns ge 1 then begin
			z = atomic_number((*p).filters[n].formula)
			if z gt 0 then begin
				(*p).filters[n].density = density(z)
				widget_control, (*pstate).density_text[n], set_value=str_tidy((*p).filters[n].density)
			endif
		endif
		if (*p).filters[n].microns eq 2 then pink_update_density, pstate
		end

	'filter-density': begin
		widget_control, event.id, get_value=s
		t = float(s[0])
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < (n_elements((*p).filters)-1)
		(*p).filters[n].density = t
		end

	'weight-mode': begin
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < (n_elements((*p).filters)-1)
		(*p).filters[n].weight = event.index
		end

	'filter-formula': begin
		widget_control, event.id, get_value=s
		t = ''
		for i=0L,n_elements(s)-1 do t = t+s[i]
		t = strcompress(t,/remove_all)
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < (n_elements((*p).filters)-1)
		if n gt (*p).n_filters-1 then begin
			(*p).n_filters = n+1
			widget_control, (*pstate).filter_number, set_combobox_select=(*p).n_filters-1
		endif

		(*p).filters[n].formula = t
		z = atomic_number(t)
		if (*p).filters[n].microns eq 2 then begin
			pink_update_density, pstate
		endif else if (*p).filters[n].microns eq 1 then begin
			if z gt 0 then begin
				(*p).filters[n].density = density(z)
				widget_control, (*pstate).density_text[n], set_value=str_tidy((*p).filters[n].density)
			endif
		endif
		end

	'pinhole-mode': begin
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < (n_elements((*p).filters)-1)
		(*p).filters[n].pinhole = event.index
		if (*p).filters[n].microns eq 2 then (*p).filters[n].pinhole = 0

		case (*p).filters[n].pinhole of
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
		widget_control, (*pstate).pinhole_mode[n], set_combobox_select=(*p).filters[n].pinhole
		end

	'filter-pinratio': begin
		widget_control, event.id, get_value=s
		t = float(s[0])
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < (n_elements((*p).filters)-1)
		(*p).filters[n].pinratio = t
		end

	'bragg-button': begin
		widget_control, event.id, get_value=s
		t = float(s[0])
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < (n_elements((*p).filters)-1)

		info = hopg_filter( event.top, hopg=(*p).filters[n].bragg)
		if info.cancel eq 0 then begin
			(*p).filters[n].bragg = info.hopg
		endif
		end

	'calculate-button': begin

;		later calculate transmission and graph in plot area

		end

	'close-button': begin
		print,'Close pink beam setup ...'
		goto, kill
		end

	else:
endcase

update:
	pink_update_pars, pstate
	pink_calculate, p, /convert, path=*(*pstate).path, Energy=E, spec=spec, error=error
	if error then return
	pink_setup_pars, pstate

	norm = 7.e+15									; norm spec*cross to ~counts (see plot_tube_yields3)
	crossa = photo_subshell( 79, 4, E)				; Au L3 subshell cross-section across spectrum
	print,'Total spec = ', total( spec)
	print,'Total Au L3 yield = ', total( norm * spec * crossa)


;	To overlay another pink ...
	
	p2 = (*pstate).plast
	if error eq 0 then begin
		pink_calculate, /convert, p2, path=*(*pstate).path, Energy=E2, spec=spec2, error=error
		pink_setup_pars, pstate
	endif

	if (error eq 0) then begin
		pink_draw, pstate, p=p, E=E, spec=spec, /overlay, altp=p2, altE=E2, altspec=spec2, test=(*pstate).test
	endif else begin
		pink_draw, pstate, p=p, E=E, spec=spec, test=(*pstate).test
	endelse

finish:
	widget_control, hourglass=0
	return

bad_tube:
	warning,'pink_setup_event',['Error returned from tube spectrum calculation.','Check pink parameters.'],/error
	goto, finish
bad_state:
	warning,'pink_setup_event',['STATE variable has become ill-defined.','Abort pink setup.'],/error
	goto, kill
bad_ptr:
	warning,'pink_setup_event',['Parameter structure variable has become ill-defined.','Abort pink setup.'],/error
	goto, kill

kill:
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).plast) then ptr_free, (*pstate).plast

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;------------------------------------------------------------------------------------------

function pink_convert_filter_to_local, filter, error=error

;	Local format for filt struct has 'thick' in microns or mg/cm^2 depending on microns flag.

	COMPILE_OPT STRICTARR
	error = 1
	nf = n_elements(filter)
	if nf lt 1 then return, 0.0
	if size(filter[0],/tname) ne 'STRUCT' then return, 0.0
	
	f = filter
	for i=0L,nf-1 do begin
		z = atomic_number(filter[i].formula)
		dz = density(z)

		if filter[i].microns and (filter[i].density gt 1.0e-8) then begin
			f[i].thick = 10.0 * filter[i].thick / filter[i].density
			f[i].microns = (f[i].density lt 0.03) ? 2: 1
			if f[i].microns eq 2 then f[i].thick = f[i].thick/1000. 
		endif else if (z gt 0) and (dz gt 0) and filter[i].microns then begin
			f[i].density = dz
			f[i].thick = 10.0 * filter[i].thick / f[i].density
			f[i].microns = (f[i].density lt 0.03) ? 2: 1
			if f[i].microns eq 2 then f[i].thick = f[i].thick/1000. 
		endif else begin
			f[i].microns = 0
		endelse
	endfor

	error = 0
	return, f
end

;------------------------------------------------------------------------------------------

function pink_convert_filter_to_standard, filter, error=error

;	Standard format for filt struct has 'thick' in mg/cm^2 always, even if microns flag is set.

	COMPILE_OPT STRICTARR
	error = 1
	nf = n_elements(filter)
	if nf lt 1 then return, 0.0
	if size(filter[0],/tname) ne 'STRUCT' then return, 0.0
	
	f = filter
	for i=0L,nf-1 do begin
		r = filter[i].pinratio
		if filter[i].pinhole eq 0 then r=0.0

		f[i] = make_filter( filter[i].formula, filter[i].thick, pinratio=r, $
				microns=filter[i].microns < 1, density=filter[i].density, $
				weight=filter[i].weight, name=filter[i].formula, error=error)

		f[i].pinhole = filter[i].pinhole
		f[i].bragg = filter[i].bragg
		if filter[i].microns eq 2 then f[i].thick = f[i].thick * 1000. 
	endfor
	if error then return, 0.0

	return, f
end

;------------------------------------------------------------------------------------------

pro pink_draw, pstate, p=p, e=e, spec=speci, overlay=overlay, altp=p2, altE=E2, altspec=spec2, test=test

;	Draw the full spectrum E, Spec, as passed from the calculate routine.

	COMPILE_OPT STRICTARR
	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(p) lt 1 then p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	if size(*p,/tname) ne 'STRUCT' then return
	if n_elements(speci) lt 1 then begin
		spec = (*p).spectrum.data
		e = (*p).spectrum.E
	endif else spec=speci
	if n_elements(overlay) eq 0 then overlay=0
	if overlay then begin
		if ptr_valid(p2) eq 0 then return
		if size(*p2,/tname) ne 'STRUCT' then return
	endif
	if n_elements(e) lt 2 then return
	if n_elements(test) lt 1 then test=0
	
	!x.title = 'Photon Energy (keV)'
	!p.title = ''
	!p.charsize = 1.0
	!p.thick = 1.0
	!p.charthick = 1.0
	
;	Note: much of the code here was inherited from 'source_setup' and may not be correct
;	for this pink beam model. Those sections (e.g. tissue dose) will need to be updated,
;	especially for absolute flux, which is not handled well here now.

	spot = (*p).modata.spot
	solid = (*p).acceptance * 1000.							; usr
	if (*p).poly.mode eq 1 then begin
		mode = 2
		spot = (*p).poly.spot
		solid = 1.0e+6*((*p).poly.diameter/2)^2 / ((*p).poly.focus)^2
	endif else if (*p).mono.mode eq 1 then begin
		mode = 1
	endif else mode=0

	ys = spec												; for brightness plot ...
	area = !pi * (spot / 2)^2 
	yb = ys / area											; per source area
	yb = yb / solid											; per usr of solid-angle

	if overlay then begin
		spot2 = (*p2).modata.spot
		solid2 = (*p2).acceptance * 1000.
		if (*p2).poly.mode eq 1 then begin
			mode2 = 2
			spot2 = (*p2).poly.spot
			solid2 = 1.0e+6*((*p2).poly.diameter/2)^2 / ((*p2).poly.focus)^2
		endif else if (*p2).mono.mode eq 1 then begin
			mode2 = 1
		endif else mode2=0

		ys2 = spec2
		area2 = !pi * (spot2 / 2)^2 
		yb2 = ys2 / area2
		yb2 = yb2 / solid2									; per usr of solid-angle
	endif
		
;........................................... Dose 1

;	Estimate dose rate at 1 m distance using 1 cm phantom and source output solid-angle
;	Do not add air path here. If wanted add that to the pink model.

	distance = 100.											; distance (cm)
	area = (*p).acceptance * (distance^2) / 1000.			; exposed area at 1m (cm^2)
	thick = 1.												; tissue 1 cm thick
	density = 1.04											; density (g/cm^3)
	J_per_eV = 1.602e-19									; Joule per eV
	um_per_cm = 1.0e+4										; micron per cm

	cell = make_layer( '(H2O)77(C6H10O5)19(NaCl)2', thick * um_per_cm, /microns, density=density)
	deposit = spec
	transmitted = spec										; 'spec' is ph/s versus E[]
	
	for i=0,n_elements(spec)-1 do begin
		abs = 1. - transmit( cell, E[i], /Hubbell)			; absorbed in phantom
		J_per_ph = J_per_eV * (E[i] * 1000.)
		transmitted[i] = spec[i] * (1.-abs) * J_per_ph		; transmitted energy (J/s)
		deposit[i] = spec[i] * abs * J_per_ph				; deposited energy (J/s) spectrum
	endfor
	mass = density * area * thick / 1000.					; kg
	dose = total(deposit) / mass							; J/kg/s (Gy/s)
	
	if test then begin
	print, 'Tissue (thick = ',str_tidy(thick*10.),' mm):'
	print, '	Total spectrum = ', total(spec),' (ignoring detector efficiency)'
	print, '	Dose (at ',str_tidy(distance/100.),' m) = ',float(dose),' Gy/s (J/kg/s), ',float(dose*1.e+6*3600.),' uSv/h'
	print, '	Occupational  = ',1.0e-6/3600.,' Gy/s (J/kg/s), ',1.0,' uSv/h'
	endif
;........................................... Dose 2

;	Estimate dose rate at 0.1 m distance using 300 um Si PIN diode and source output solid-angle
;	Do not add air path here. If wanted add that to the pink model.

	distance = 10.											; distance (cm)
	area = (*p).acceptance * (distance^2) / 1000.			; exposed area at 1m (cm^2)
	thick = 0.05											; (cm) diode 500 um thick
	density = 2.322											; density (g/cm^3)
	J_per_eV = 1.602e-19									; Joule per eV
	um_per_cm = 1.0e+4										; micron per cm

	pin = make_layer( 'Si', thick * um_per_cm, /microns, density=density)
	deposit = spec
	transmitted = spec										; 'spec' is ph/s versus E[]
	
	for i=0,n_elements(spec)-1 do begin
		abs = 1. - transmit( pin, E[i], /Hubbell)			; absorbed in PIN diode
		J_per_ph = J_per_eV * (E[i] * 1000.)
		transmitted[i] = spec[i] * (1.-abs) * J_per_ph		; transmitted energy (J/s)
		deposit[i] = spec[i] * abs * J_per_ph				; deposited energy (J/s) spectrum
	endfor
	mass = density * area * thick / 1000.					; kg
	dose = total(deposit) / mass							; J/kg/s (Gy/s)
	
	if test then begin
	print, 'Silicon PIN diode (thick = ',str_tidy(thick*10.),' mm):'
	print, '	Total spectrum = ', total(spec),' (ignoring detector efficiency)'
	print, '	Dose (at ',str_tidy(distance/100.),' m) = ',float(dose),' Gy/s (J/kg/s), ',float(dose*3600.),' Sv/h'
	endif
;........................................... Dose 3

;	Estimate dose rate at 0.1 m distance into top 1 um of silicon surface 
;	Do not add air path here. If wanted add that to the pink model.

	distance = 10.											; distance (cm)
	area = (*p).acceptance * (distance^2) / 1000.			; exposed area at 1m (cm^2)
	thick = 0.0001											; (cm) silicon surface 1 um thick
	density = 2.322											; density (g/cm^3)
	J_per_eV = 1.602e-19									; Joule per eV
	um_per_cm = 1.0e+4										; micron per cm

	pin = make_layer( 'Si', thick * um_per_cm, /microns, density=density)
	deposit = spec
	transmitted = spec										; 'spec' is ph/s versus E[]
	
	for i=0,n_elements(spec)-1 do begin
		abs = 1. - transmit( pin, E[i], /Hubbell)			; absorbed in PIN diode
		J_per_ph = J_per_eV * (E[i] * 1000.)
		transmitted[i] = spec[i] * (1.-abs) * J_per_ph		; transmitted energy (J/s)
		deposit[i] = spec[i] * abs * J_per_ph				; deposited energy (J/s) spectrum
	endfor
	mass = density * area * thick / 1000.					; kg
	dose = total(deposit) / mass							; J/kg/s (Gy/s)
	
	if test then begin
	print, 'Silicon surface (thick = ',str_tidy(thick*10.),' mm):'
	print, '	Total spectrum = ', total(spec),' (ignoring detector efficiency)'
	print, '	Dose (at ',str_tidy(distance/100.),' m) = ',float(dose),' Gy/s (J/kg/s), ',float(dose*3600.),' Sv/h'
	endif
;........................................... Spectrum

	if (*pstate).wid2 eq 0 then return
	wset, (*pstate).wid2
	ymax = max(ys)
	ymin = 1.0e-4 * ymax
	xmax = max(e)
	if overlay then begin
		ymax = max( [ymax, max(ys2)])
		ymin = min( [ymax*1.0e-4, max(ys)*0.1, max(ys2)*0.1])
		xmax = max( [xmax, max(e2)])
	endif
	sbin = str_tidy( 1000. * (*p).modata.bin)
	!y.title = 'Spectrum (ph/s/'+sbin+' eV bandwidth)'
	
	plot, e,ys, xrange=[0.,xmax*1.05], yrange=[ymin*0.8,ymax*1.2], $
					color=spec_colour('white'), /ylog, $
					ticklen=1.0, /nodata, ystyle=1	;, xstyle=1
	
	if overlay then begin
		oplot, e2,ys2, color=spec_colour('orange')
		xyouts, 0.93, 0.88, 'Total = '+str_tidy(total(ys2),places=2), /norm,align=1, color=spec_colour('orange')
	endif
	oplot, e,ys, color=spec_colour('green')
	xyouts, 0.93, 0.92, 'Total = '+str_tidy(total(ys),places=2), /norm,align=1, color=spec_colour('green')

	wset, (*pstate).pix2
	device,copy=[0,0,(*pstate).width,(*pstate).height, 0,0,(*pstate).wid2]
	return
end

;------------------------------------------------------------------------------------------

pro pink_update_density, pstate

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
		warning,'pink_update_density',['IDL run-time error caught.', '', $
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
	
	radical = radical_split( (*p).filters[i].formula)
	nr = n_elements(radical)
	ni = fltarr(nr)
	mi = fltarr(nr)
	for j=0,nr-1 do begin
		chop_radical, radical[j], form,x
		ni[j] = x
		decode_radical, form, n, z, f, /number
		mi[j] = total( mass(z[0:n-1])*f[0:n-1])
		if (*p).filters[i].weight then ni[j] = ni[j] / mi[j]
	endfor
	m = total( mi*ni) / total(ni)
	dens = m * moles

	(*p).filters[i].density = dens
	widget_control, (*pstate).density_text[i], set_value=str_tidy(dens)
	return
end

;------------------------------------------------------------------------------------------

pro pink_update_pars, pstate, error=error

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
		warning,'pink_update_pars',['IDL run-time error caught.', '', $
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
	widget_control, (*pstate).pink_file, get_value=F
	(*p).file = F[0]

	widget_control, (*pstate).fe_spectrum_text, get_value=F
	(*p).fe_spectrum_file = F[0]

;	widget_control, (*pstate).spot_text, get_value=F
;	(*p).modata.spot = float(F[0])

	widget_control, (*pstate).mono_energy_text, get_value=F
	(*p).modata.mono[0] = float(F[0])
	widget_control, (*pstate).mono_bw_text, get_value=F
	(*p).modata.mono[1] = float(F[0])
	widget_control, (*pstate).mono_eff_text, get_value=F
	(*p).modata.mono[2] = float(F[0])

	widget_control, (*pstate).poly_gain_text, get_value=F
	(*p).poly.gain = float(F[0])
	widget_control, (*pstate).poly_energy_text, get_value=F
	(*p).poly.energy = float(F[0])
	widget_control, (*pstate).poly_pinhole_text, get_value=F
	(*p).poly.pinhole = float(F[0])

	for i=0L,(*p).n_mirrors-1 do begin
		widget_control, (*pstate).mtitle_text[i], get_value=s
		(*p).mirrors[i].title = s
		widget_control, (*pstate).mfile_text[i], get_value=s
		(*p).mirrors[i].file = s
	endfor

	for i=0L,(*p).n_filters-1 do begin
		widget_control, (*pstate).thick_text[i], get_value=s
		(*p).filters[i].thick = float(s[0])
		widget_control, (*pstate).density_text[i], get_value=s
		(*p).filters[i].density = float(s[0])

		widget_control, (*pstate).formula_text[i], get_value=s
		t = ''
		for k=0L,n_elements(s)-1 do t = t+s[k]
		(*p).filters[i].formula = strcompress(t,/remove_all)

		z = atomic_number((*p).filters[i].formula)
		if (z gt 0) and ((*p).filters[i].microns ne 2) then begin
			(*p).filters[i].density = density(z)
			widget_control, (*pstate).density_text[i], set_value=str_tidy((*p).filters[i].density)
		endif

		widget_control, (*pstate).ratio_text[i], get_value=s
		(*p).filters[i].pinratio = float(s[0])
	endfor

	return
end

;------------------------------------------------------------------------------------------

pro pink_setup_pars, pstate, error=error

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
		warning,'pink_update_pars',['IDL run-time error caught.', '', $
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

	set_widget_text, (*pstate).pink_file, (*p).file
	widget_control, (*pstate).title_text, set_value=(*p).title
	set_widget_text, (*pstate).fe_spectrum_text, (*p).fe_spectrum_file
	
;	widget_control, (*pstate).spot_text, set_value=str_tidy((*p).modata.spot)
	
	if (*p).poly.mode eq 1 then begin
		mode = 2
	endif else if (*p).mono.mode eq 1 then begin
		mode = 1
	endif else mode=0
	widget_control, (*pstate).optics_mode, set_combobox_select=mode

	widget_control, (*pstate).mono_element_button, set_value={VALUE: ((*p).mono.z gt 0) ? element_name((*p).mono.z) : ''}
	widget_control, (*pstate).mono_energy_text, set_value=str_tidy((*p).modata.mono[0])
	widget_control, (*pstate).mono_bw_text, set_value=str_tidy((*p).modata.mono[1])
	widget_control, (*pstate).mono_eff_text, set_value=str_tidy((*p).modata.mono[2])
	widget_control, (*pstate).mono_mapbase1, map=(*p).mono.mode
	widget_control, (*pstate).mono_mapbase2, map=(*p).mono.mode

	q = where( (*p).poly.model eq (*pstate).poly_model)
	if q[0] ne -1 then begin
		(*pstate).poly_active = q[0]	
		poly = get_poly_config( (*pstate).poly_list[ (*pstate).poly_active], error=err)
		if err eq 0 then begin
			(*p).poly = poly
			(*p).poly.mode = 1
		endif
	endif 
	if mode eq 0 then begin
		(*p).poly.mode = 0
	endif
	widget_control, (*pstate).poly_type, set_combobox_select= (*pstate).poly_active

	widget_control, (*pstate).poly_gain_text, set_value=str_tidy((*p).poly.gain)
	widget_control, (*pstate).poly_energy_text, set_value=str_tidy((*p).poly.energy)
	widget_control, (*pstate).poly_pinhole_text, set_value=str_tidy((*p).poly.pinhole)
	widget_control, (*pstate).poly_mapbase1, map=(*p).poly.mode
	widget_control, (*pstate).poly_mapbase2, map=(*p).poly.mode

;	mirror

	n = (*p).n_mirrors
	widget_control, (*pstate).mirror_number, set_combobox_select=n
	widget_control, (*pstate).mirror_base[(*pstate).mactive], map=0

	if (*pstate).mactive ge n then begin
		if n eq 0 then (*pstate).mactive = 0
		if n gt 0 then (*pstate).mactive = (*pstate).mactive < (n-1)
	endif
	if n gt 0 then widget_control, (*pstate).mirror_base[(*pstate).mactive], map=1
	widget_control, (*pstate).define_mirror, set_combobox_select=(*pstate).mactive

	if n gt 0 then begin
		for i=0,n-1 do begin
			widget_control, (*pstate).mtitle_text[i], set_value=(*p).mirrors[i].title
			set_widget_text, (*pstate).mfile_text[i], (*p).mirrors[i].file
		endfor
	endif

;	filters

	n = (*p).n_filters
	widget_control, (*pstate).filter_number, set_combobox_select=n-1

	if (n-1) lt (*pstate).active then begin
		widget_control, (*pstate).filter_base[(*pstate).active], map=0
		(*pstate).active = (*pstate).active < (n-1)
		widget_control, (*pstate).filter_base[(*pstate).active], map=1
		widget_control, (*pstate).define_filter, set_combobox_select=(*pstate).active
	endif

	if n gt 0 then begin
		for i=0,n-1 do begin
			if (*p).filters[i].microns eq 2 then (*p).filters[i].pinhole = 0
			widget_control, (*pstate).thick_text[i], set_value=str_tidy((*p).filters[i].thick)
			widget_control, (*pstate).density_text[i], set_value=str_tidy((*p).filters[i].density)

			widget_control, (*pstate).thick_mode[i], set_combobox_select=(*p).filters[i].microns
			widget_control, (*pstate).weight_mode[i], set_combobox_select=(*p).filters[i].weight
			widget_control, (*pstate).formula_text[i], set_value=(*p).filters[i].formula

			widget_control, (*pstate).pinhole_mode[i], set_combobox_select=(*p).filters[i].pinhole
			widget_control, (*pstate).ratio_text[i], set_value=str_tidy((*p).filters[i].pinratio)

			case (*p).filters[i].pinhole of
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
			widget_control, (*pstate).density_base[i], map=(*p).filters[i].microns
		endfor
	endif
	return
end
	
;------------------------------------------------------------------------------------------

pro OnRealize_pink_mirror_number, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).n_mirrors
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_pink_filter_number, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).n_filters-1
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_pink_anode_weight_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).modata.anode.weight
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_pink_beam_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).beam.mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_pink_poly_type, wWidget

COMPILE_OPT STRICTARR						; @3-23
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	q = where( (*(*pstate).p).poly.model eq (*pstate).poly_model)
	if q[0] ne -1 then widget_control, wWidget, set_combobox_select=q[0]
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_pink_optics_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	if (*(*pstate).p).poly.mode eq 1 then begin
		mode = 2
	endif else if (*(*pstate).p).mono.mode eq 1 then begin
		mode = 1
	endif else mode=0
	widget_control, wWidget, set_combobox_select=mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_pink_thick_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = (fix(s[0]) > 0) < ((*pstate).n_filters_max-1)

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).filters[n].microns
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_pink_weight_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = (fix(s[0]) > 0) < ((*pstate).n_filters_max-1)

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).filters[n].weight
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_pink_pinhole_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = (fix(s[0]) > 0) < ((*pstate).n_filters_max-1)

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).filters[n].pinhole
endif
end

;-----------------------------------------------------------------

pro OnRealize_pink_drawS, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=wid
(*pstate).wid2 = wid
(*pstate).draw2 = wWidget

window, /free, xsize=(*pstate).width, ysize=(*pstate).height, /pixmap
(*pstate).pix2 = !d.window

pink_draw, pstate
end

;------------------------------------------------------------------------------------------

pro pink_setup, group_leader=group, TLB=tlb, pars=p, path=path, _extra=extra, xoffset=xoffset, yoffset=yoffset, test=test_mode

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
		warning,'pink_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

startupp, /database
if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''
if n_elements(test_mode) lt 1 then test_mode=0

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
		draw_xsize = 600
		draw_ysize = 536
		yw = 400
		frame_width = 405
		pars_xsize = 70
		pars_xpad = 1
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
		draw_xsize = 600
		draw_ysize = 580
		yw = 420
		frame_width = 405
		pars_xsize = 70
		pars_xpad = 1
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
		draw_xsize = 600
		draw_ysize = 580
		yw = 400
		frame_width = 390
		pars_xsize = 70
		pars_xpad = 1
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

; Note that the source thickness is in mm while all filter thicknesses are in mg/cm^2.

if make_p then begin
	pars = define(/pink)
	pars.continuum = 1
	*p = pars
	(*p).n_filters = 1
	(*p).n_mirrors = 0
endif
n_filters_max = n_elements((*p).filters)
n_mirrors_max = n_elements((*p).mirrors)

poly_update, list=poly_list, title=poly_model, count=poly_count, error=error
poly_model = ['XOS default', poly_model]
poly_list = ['', poly_list]						;@3-23
poly_count++

; 	top-level base

tlb = widget_base( /column, title='Pink Beam Setup & Edit', /TLB_KILL_REQUEST_EVENTS, $
				group_leader=group, _extra=extra, uname='pink_TLB', $
				/tlb_size_events, xoffset=xoffset, yoffset=yoffset, /base_align_center)

rbase = widget_base( tlb, /row, xpad=5, ypad=5, space=5, /base_align_top, /align_top)
tbase = widget_base( rbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

; set-up file buttons

lbase = widget_base( tbase, /column, xpad=0, ypad=0, space=3, /base_align_right, /align_center)
sbase = widget_base( lbase, /row, /base_align_center, ypad=0, xpad=0, space=5, /align_right)
lab = widget_label( sbase, value='File:')
pink_file = widget_text( sbase, value=(*p).file, uname='pink-file', /tracking, /editable, $
					uvalue='Enter the name of a pink beam file to retrieve and edit.',scr_xsize=253)
load_setup_button = widget_button( sbase, value='Load', uname='load-pink-button', /tracking, $
					uvalue='Load pink parameters from a previous pink beam file.', scr_xsize=38)
save_setup_button = widget_button( sbase, value='Save', uname='save-pink-button', /tracking, $
					uvalue='Save pink beam parameters to a file. A "Save" is also needed to transfer modelled spectra results ' + $
					'to the yield calculation window.', scr_xsize=38)

titlebase = widget_base( lbase, /row, /base_align_center, ypad=0, xpad=0, space=5, /align_right)
lab = widget_label( titlebase, value='Title:')
title_text = widget_text( titlebase, value=(*p).title, uname='title-text', /tracking, /editable, $
					uvalue='Enter a title descriptor for this X-ray pink beam.',scr_xsize=343)

; FE Beam

febase = widget_base( tbase, /column, /base_align_right, /align_right, ypad=1, xpad=2, space=3, /frame, scr_xsize=frame_width)
lab = widget_label( febase, value='FE Beam Spectrum', /align_center)

fbase = widget_base( febase, /row, /base_align_center, ypad=0, xpad=0, space=5, /align_right)
lab = widget_label( fbase, value='File:')
fe_spectrum_text = widget_text( fbase, value=(*p).file, uname='fe-spectrum-file', /tracking, /editable, $
					uvalue='Enter the name of a Front-end Spectrum file to retrieve.',scr_xsize=303)
load_spec_button = widget_button( fbase, value='Load', uname='load-fe-spectrum-button', /tracking, $
					uvalue='Load Front-end Spectrum file.', scr_xsize=38)

; mirror details

mirbase = widget_base( tbase, /column, /base_align_center, /align_right, ypad=1, xpad=2, space=spc_filters, /frame, scr_xsize=frame_width)
lab = widget_label( mirbase, value='Mirror Definitions', /align_center)

mir2base = widget_base( mirbase, /row, /base_align_center, ypad=0, xpad=0, space=2)
lab = widget_label( mir2base, value='# mirrors:')
mirror_number = widget_combobox( mir2base, value=str_tidy(indgen(n_mirrors_max+1)), uname='mirror-number', /tracking, $
					notify_realize='OnRealize_pink_mirror_number', $
					uvalue='Select the number of mirrors.', xsize=xsize_nfilters)

lab = widget_label( mir2base, value='   Define mirror:')
define_mirror = widget_combobox( mir2base, value=str_tidy(indgen(n_mirrors_max)+1), uname='define-mirror', /tracking, $
					uvalue='Select a mirror by number, for editing. The parameters for the selected mirror will ' + $
					'be displayed in the panel below for editing.', xsize=xsize_deffilter)

; The map base areas for setting up 'n_mirrors_max' mirrors ...

mapbull2 = widget_base( mirbase, /align_center)
mirror_base = lonarr(n_mirrors_max)
mtitle_text = lonarr(n_mirrors_max)
mfile_text = lonarr(n_mirrors_max)

map = 0
for i=0L,n_mirrors_max-1 do begin
	mirror_base[i] = widget_base( mapbull2, /column, /frame, map=map, /base_align_right, /align_center, xpad=2, ypad=1, space=2)
	lab = widget_label( mirror_base[i], value='Mirror Details', /align_center)

	mll1base = widget_base( mirror_base[i], /row, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( mll1base, value='Title:')
	mtitle_text[i] = widget_text( mll1base, value=(*p).mirrors[i].title, uname='mirror-title-text', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the title for this mirror.', scr_xsize=338)
	mll2base = widget_base( mirror_base[i], /row, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( mll2base, value='File:')
	mfile_text[i] = widget_text( mll2base, value=(*p).mirrors[i].file, uname='mirror-file-text', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the file path giving CXRO reflectivity data for this mirror, or use "Load".', scr_xsize=296)
	button = widget_button( mll2base, value='Load', uname='load-mirror-file-button', /tracking, $
					uvalue=str_tidy(i)+'  Load the file giving CXRO reflectivity data for this mirror.', scr_xsize=38)
	map = 0
endfor

; Optics details

opticsbase = widget_base( tbase, /column, /base_align_right, /align_right, ypad=1, xpad=2, space=spc_filters, /frame, scr_xsize=frame_width)
lab = widget_label( opticsbase, value='Inline Optics', /align_center)

mbase0 = widget_base( opticsbase, column=1, /base_align_right, ypad=0, xpad=0, space=3, /align_center)

mbase0a = widget_base( mbase0, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=20)
optics_mode = widget_combobox( mbase0a, value=['    None',' Monochromator',' Polycapillary lens'], uname='optics-mode', /tracking, $
					notify_realize='OnRealize_pink_optics_mode', xsize=140, $
					uvalue='Select optional in-line optics, such as Monochromator or Polycapillary lens.')
mbase0a_pad = widget_base( mbase0a, ypad=0, xpad=0)

mono_mapbase1 = widget_base( mbase0a_pad, /row, /base_align_center, ypad=0, xpad=0, space=5, map=((*p).mono.mode eq 1))
szn = ((*p).mono.z gt 0) ? element_name((*p).mono.z) : ''
mono_element_button = state_button( mono_mapbase1, xsize=40,ysize=22,value=szn,uname='mono-element',n_states=1, $
				n_alt_states=0, colours=[spec_colour('green')], $
				/tracking, uvalue='Pop-up a periodic table to select GREEN element. Use its Ka energy as the mono centre energy.' )

; @3-23
poly_mapbase1 = widget_base( mbase0a_pad, /row, /base_align_center, ypad=0, xpad=0, space=5, map=((*p).poly.mode eq 1))
poly_type = widget_combobox( poly_mapbase1, value=poly_model, uname='poly-type', /tracking, $
					notify_realize='OnRealize_pink_poly_type', xsize=160, $
					uvalue='Select Polycapillary lens model, from "polycapillary-*.csv" files loaded.')

opticsbase2 = widget_base( opticsbase )
mono_mapbase2 = widget_base( opticsbase2, column=3, /base_align_right, ypad=0, xpad=0, space=3, /align_right, map=(*p).mono.mode)

mbase1a = widget_base( mono_mapbase2, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( mbase1a, value='Energy:')
mono_energy_text = widget_text( mbase1a, value=str_tidy((*p).modata.mono[0]), uname='mono-energy-text', /tracking, /editable, $
					uvalue='Enter the energy of the centre of the monochromator window (keV). Alternatively, use the GREEN element button to pop-up ' + $
					"a periodic table to select centre energy based on element's Ka energy.", scr_xsize=pars_xsize)

mbase1b = widget_base( mono_mapbase2, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( mbase1b, value='Bandwidth:')
mono_bw_text = widget_text( mbase1b, value=str_tidy((*p).modata.mono[1]), uname='mono-bw-text', /tracking, /editable, $
					uvalue='Enter the bandwidth of the monochromator (%).', scr_xsize=pars_xsize)

mbase1c = widget_base( mono_mapbase2, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( mbase1c, value='Eff:')
mono_eff_text = widget_text( mbase1c, value=str_tidy((*p).modata.mono[2]), uname='mono-eff-text', /tracking, /editable, $
					uvalue='Enter the transmission efficiency of the monochromator window (<1).', scr_xsize=pars_xsize)

poly_mapbase2 = widget_base( opticsbase2, column=3, /base_align_right, ypad=0, xpad=0, space=3, /align_right, map=(*p).poly.mode)

pbase1a = widget_base( poly_mapbase2, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( pbase1a, value='Gain:')
poly_gain_text = widget_text( pbase1a, value=str_tidy((*p).poly.gain), uname='poly-gain-text', /tracking, /editable, $
					uvalue='Enter the flux Gain of the polycapillary lens.' , scr_xsize=pars_xsize)

pbase1b = widget_base( poly_mapbase2, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( pbase1b, value='Energy:')
poly_energy_text = widget_text( pbase1b, value=str_tidy((*p).poly.energy), uname='poly-energy-text', /tracking, /editable, $
					uvalue='Enter the Energy (keV) appropriate for this flux Gain.', scr_xsize=pars_xsize)

pbase1c = widget_base( poly_mapbase2, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( pbase1c, value='Pinhole:')
poly_pinhole_text = widget_text( pbase1c, value=str_tidy((*p).poly.pinhole), uname='poly-pinhole-text', /tracking, /editable, $
					uvalue='Enter the diameter (mm) of the reference Pinhole that this Gain is relative to (at a distance of 100 mm).', scr_xsize=pars_xsize)

; absorber details

laybase = widget_base( tbase, /column, /base_align_center, /align_right, ypad=1, xpad=2, space=spc_filters, /frame, scr_xsize=frame_width)
lab = widget_label( laybase, value='Filter Layer Definitions', /align_center)

lay2base = widget_base( laybase, /row, /base_align_center, ypad=0, xpad=0, space=2)
lab = widget_label( lay2base, value='# filters:')
filter_number = widget_combobox( lay2base, value=str_tidy(indgen(n_filters_max)+1), uname='filter-number', /tracking, $
					notify_realize='OnRealize_pink_filter_number', $
					uvalue='Select the number of pink filters.', xsize=xsize_nfilters)

lab = widget_label( lay2base, value='   Define filter:')
define_filter = widget_combobox( lay2base, value=str_tidy(indgen(n_filters_max)+1), uname='define-filter', /tracking, $
					uvalue='Select a filter by number, for editing. The parameters for the selected filter will ' + $
					'be displayed in the panel below for editing.', xsize=xsize_deffilter)

;map_base = widget_base( tbase, /base_align_center, ypad=0, xpad=0, space=0)

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
	lab = widget_label( filter_base[i], value='Filter Details', /align_center)

	ll1base = widget_base( filter_base[i], /row, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( ll1base, value='Thick:')
	thick_text[i] = widget_text( ll1base, value=str_tidy((*p).filters[i].thick), uname='thick-text', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the thickness of the selected filter layer in either mg/cm^2, microns or mm (for a Gas at NPT).', scr_xsize=72)
	thick_mode[i] = widget_combobox( ll1base, value=[' mg/cm^2',' microns',' Gas (mm NPT)'], uname='thick-mode', /tracking, $
					notify_realize='OnRealize_pink_thick_mode', $
					uvalue=str_tidy(i)+'  Select filter layer thickness in "mg/cm^2", "microns" or "Gas (mm NPT)". ' + $
					'If microns is selected, a new box appears for entry of density of a compound. ' + $
					'For a gas NPT conditions (P=1013.25 mbar, T=20C) are assumed; hit <return> on Formula or Thickness to calculate an ideal gas density.', xsize=120)

	density_base[i] = widget_base( ll1base, /row, map=(*p).filters[i].microns, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( density_base[i], value=' Density:')
	density_text[i] = widget_text( density_base[i], value=str_tidy((*p).filters[i].density), uname='filter-density', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the density of a compound composition filter layer (g/cm^3). '+ $
					'For pure element filters the density will be obtained from the database automatically. ' + $
					'For a Gas, hit <return> on Formula or Thickness to calculate an ideal gas density; NPT conditions (P=1013.25 mbar, T=20C) are assumed.', scr_xsize=72)

	ll2base = widget_base( filter_base[i], /row, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( ll2base, value='Formula:')
	formula_text[i] = widget_text( ll2base, value=(*p).filters[i].formula, uname='filter-formula', /tracking, /editable, ysize=1,  $
					uvalue=str_tidy(i)+'  Enter the chemical formula for the layer. ' + $
					'Enclose radicals in brackets "( )", with optional multipliers in atomic fraction or weight %. ' + $
					'e.g. Components in wt%: "(SiO2)18.3(MgO)34.3"; atomic proportions: "FeAsS". ' + $
					'For a Gas, enclose gas molecules in the brackets "( )".', scr_xsize=formula_xsize)

	weight_mode[i] = widget_combobox( ll2base, value=[' Atomic Fraction','  Weight %'], uname='weight-mode', /tracking, $
					notify_realize='OnRealize_pink_weight_mode', $
					uvalue=str_tidy(i)+'  Select the type of multiplier factors in formulae. These appear after ' + $
					'a radical term contained in brackets, e.g. "(SiO2)12.3". Options include "atomic fraction" or "weight %".', xsize=xsize_atomic)

	ll3base = widget_base( filter_base[i], /row, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( ll3base, value='Special:')
	pinhole_mode[i] = widget_combobox( ll3base, value=['Plain filter','Pinhole filter','Bragg filter'], uname='pinhole-mode', /tracking, $
					notify_realize='OnRealize_pink_pinhole_mode', $
					uvalue=str_tidy(i)+'  Select special filter types. ' + $
					'If "pinhole filter" is selected, a new box appears for entry of the solid-angle ratio for the pinhole. ' + $
					'This is the ratio of the source solid-angle divided by the hole solid-angle.', xsize=special_xsize)

	case (*p).filters[i].pinhole of
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
	ratio_base[i] = widget_base( map_base, /row, map=map_pinhole, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( ratio_base[i], value=' Solid-angle ratio:')
	ratio_text[i] = widget_text( ratio_base[i], value=str_tidy((*p).filters[i].pinratio), uname='filter-pinratio', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the pin-hole filter solid-angle ratio. '+ $
					'This is the ratio of the pink solid-angle divided by the hole solid-angle.', scr_xsize=70)

	bragg_base[i] = widget_base( map_base, /row, map=map_bragg, /base_align_center, /align_right, ypad=0, xpad=20, space=3)
	bragg_button[i] = widget_button( bragg_base[i], value='      Edit Bragg     ', uname='bragg-button', /tracking, uvalue=str_tidy(i)+'  Open Bragg filter editor.')
	map = 0
endfor

;.................................................................................

tab_panel = widget_tab( rbase, location=3, /align_center, uname='plot-tab-panel')

spectrum_base = widget_base( tab_panel, title='Spectrum', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=draw_xsize+20, scr_ysize=left_ysize)

draw_spectrum = widget_draw( spectrum_base, uname='draw-spectrum', xsize=draw_xsize, ysize=draw_ysize, notify_realize='OnRealize_pink_drawS', $
			retain=retain, /button_events)


help = widget_text( tbase, scr_xsize=391, ysize=4, /wrap, uname='help', /tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

state = {	$
		path:				ptr_new(path), $		; current path
		p:					p, $					; pointer to parameters
		plast:				ptr_new(/allocate_heap), $	; pointer to previous set of parameters
		active:				0, $					; active filter for edit
		mactive:			0, $					; active mirror for edit
		n_filters_max:		n_filters_max, $		; maximum number of filters
		n_mirrors_max:		n_mirrors_max, $		; maximum number of mirrors
		title_text:			title_text, $			; ID of title text widget
		pink_file:			pink_file, $			; ID of filter file text widget
		filter_number:		filter_number, $		; ID of # filters droplist
		define_filter:		define_filter, $		; ID of filter define selection droplist
		mirror_number:		mirror_number, $		; ID of # mirrors droplist
		define_mirror:		define_mirror, $		; ID of mirror define selection droplist
		fe_spectrum_text:	fe_spectrum_text, $		; ID of FE spectrum filename text
		poly_list:			poly_list, $			; polycapillary config files ;@3-23
		poly_model:			poly_model, $			; polycapillary model names
		poly_active:		0, $					; active polycapillary model

		draw2:				draw_spectrum, $		; ID of spectrum draw widget
		wid2:				0, $					; window ID of spectrum draw widget
		pix2:				0L, $					; pix-map2
		width:				draw_xsize, $			; width of window
		height:				draw_ysize, $			; height of window
		energy:				0.0, $					; energy cursor
		test:				test_mode, $			; test mode

		optics_mode:		optics_mode, $			; ID of optics mode droplist
		poly_type:			poly_type, $			; ID of polycapillary model droplist
		mono_element_button: mono_element_button, $	; ID of mono element select button
		mono_energy_text:	mono_energy_text, $		; ID of mono energy text
		mono_bw_text:		mono_bw_text, $			; ID of mono b/w text
		mono_eff_text:		mono_eff_text, $		; ID of mono efficiency text
		mono_mapbase1:		mono_mapbase1, $		; ID of map base for mono element
		mono_mapbase2:		mono_mapbase2, $		; ID of map base for mono details

		poly_gain_text:		poly_gain_text, $		; ID of poly gain text
		poly_energy_text:	poly_energy_text, $		; ID of poly energy text
		poly_pinhole_text:	poly_pinhole_text, $	; ID of poly pinhole text
		poly_mapbase1:		poly_mapbase1, $		; ID of map base for poly type		; @3-23
		poly_mapbase2:		poly_mapbase2, $		; ID of map base for poly details
		
		mirror_base:		mirror_base, $			; ID array of map bases for mirrors
		mtitle_text:		mtitle_text, $			; ID array of mirror title text widgets
		mfile_text:			mfile_text, $			; ID array of mirror file text widgets

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

		help:				help $					; ID of help text
	}

child = widget_info( tlb, /child)
pstate = ptr_new(state, /no_copy)
widget_control, child, set_uvalue=pstate
widget_control, tlb, /realize

register_notify, tlb, ['path' $						; new path
						], $						; 
					from=group

xmanager, 'pink_setup', tlb, /no_block

pink_setup_pars, pstate, error=error
return
end
