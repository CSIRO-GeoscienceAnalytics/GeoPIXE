
;	source setup and edit.

pro source_setup_event, event
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
		warning,'source_setup_event',['IDL run-time error caught.', '', $
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
					print,'source setup: new path = ',(*event.pointer)
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
				widget_control, (*pstate).help, set_value='Green curve is current model results. Orange curve is previous loaded source model. ' + $
				'Use Tabs on right to select "Brightness" or "Spectrum" (uses output solid-angle "Omega" or polycapillary parameters) display.'
			endelse
		endif
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request source_setup ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'source_TLB': begin
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
;			OnButton_source, pstate, Event
			drag = 1
		endelse
		end

	'load-source-button': begin
		file = find_file2( (*p).file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = geopixe_root
		F = file_requester( /read, filter = '*.source', $
				/must_exist, path=path, group=event.top, $
				title='Select the source file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.source'
			*(*pstate).path = extract_path(F)
			set_widget_text, (*pstate).source_file, F
			src = read_source( F, error=err)
			if err eq 0 then begin
				*(*pstate).plast = *p
				*p = src
				(*p).file = F
				n = (*p).n_filters
				if n gt 0 then f = source_convert_filter_to_local( (*p).filters[0:n-1], error=error)
				(*p).filters[0:n-1] = f
				source_setup_pars, pstate
			endif
		endif
		end

	'save-source-button': begin
		file = find_file2( (*p).file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = geopixe_root
		F = file_requester( /write, file=file, filter = '*.source', $
			/noconfirm, path=path, group=event.top, $
			title='Save source definitions and model spectrum to file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.source'
			*(*pstate).path = extract_path(F)
			set_widget_text, (*pstate).source_file, F
			source_update_pars, pstate
			source_calculate, p, /convert
			
			src = *p
			n = (*p).n_filters
			if n gt 0 then begin
				filt = source_convert_filter_to_standard( (*p).filters[0:n-1], error=error)
				src.filters[0:n-1] = filt
			endif
			write_source, src, F, error=err
			if err eq 0 then begin
				(*p).file = F
			endif
			notify, 'new-source', p, from=event.top
		endif
		end

	'source-file': begin
		widget_control, event.id, get_value=F
		F = strip_file_ext(F[0]) + '.source'
		(*p).file = F
		*(*pstate).path = extract_path(F)
		set_widget_text, (*pstate).source_file, F
		src = read_source( F, error=err)
		if err eq 0 then begin
			*(*pstate).plast = *p
			*p = src
			(*p).file = F
			n = (*p).n_filters
			if n gt 0 then f = source_convert_filter_to_local( (*p).filters[0:n-1], error=error)
			(*p).filters[0:n-1] = f
			source_setup_pars, pstate
		endif
		end

	'title-text': begin
		widget_control, event.id, get_value=s
		(*p).title = s[0]
		end

	'name-text': begin
		widget_control, event.id, get_value=s
		(*p).modata.anode.name = s[0]
		end

	'anode-formula': begin
		widget_control, event.id, get_value=s
		(*p).modata.anode.formula = s
		end

	'anode-weight-mode': begin
		(*p).modata.anode.weight = event.index
		end

	'volts-text': begin
		widget_control, event.id, get_value=s
		(*p).modata.volts = float2(s[0])
		end

	'power-text': begin
		widget_control, event.id, get_value=s
		(*p).modata.power = float2(s[0])
		end

	'spot-text': begin
		widget_control, event.id, get_value=s
		(*p).modata.spot = float2(s[0])
		end

	'omega-text': begin
		widget_control, event.id, get_value=s
		(*p).acceptance = float2(s[0])
		end

	'phi-text': begin
		widget_control, event.id, get_value=s
		(*p).modata.phi = float2(s[0])
		end

	'eps-text': begin
		widget_control, event.id, get_value=s
		(*p).modata.eps = float2(s[0])
		end

	'beam-mode': begin
		(*p).beam.mode = event.index
		widget_control, (*pstate).trans_thick_mapbase, map=(*p).beam.mode

;		case (*p).beam.mode of
;			0: begin
;				end
;			1: begin
;				end
;			else:
;		endcase
		end

	'anode-thick-text': begin
		widget_control, event.id, get_value=s
		(*p).beam.thick = float2(s[0])
		end

	'optics-mode': begin
		case event.index of
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
		source_setup_pars, pstate
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
			source_setup_pars, pstate			; @3-23
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
		if (*p).filters[n].microns eq 2 then source_update_density, pstate
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
		if (*p).filters[n].microns eq 2 then source_update_density, pstate
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
			source_update_density, pstate
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
		print,'Close source setup ...'
		goto, kill
		end

	else:
endcase

update:
	source_update_pars, pstate
	source_calculate, p, /convert, Energy=E, spec=spec, error=error

	norm = 7.e+15									; norm spec*cross to ~counts (see plot_tube_yields3)
	crossa = photo_subshell( 79, 4, E)				; Au L3 subshell cross-section across spectrum
	print,'Total spec = ', total( spec)
	print,'Total Au L3 yield = ', total( norm * spec * crossa)


;	To overlay another source ...
	
	p2 = (*pstate).plast
	if error eq 0 then begin
		source_calculate, /convert, p2, Energy=E2, spec=spec2, error=error
	endif
	
	if (error eq 0) then begin
		source_draw, pstate, p=p, E=E, spec=spec, /overlay, altp=p2, altE=E2, altspec=spec2, test=(*pstate).test
	endif else begin
		source_draw, pstate, p=p, E=E, spec=spec, test=(*pstate).test
	endelse

finish:
	widget_control, hourglass=0
	return

bad_tube:
	warning,'source_setup_event',['Error returned from tube spectrum calculation.','Check source parameters.'],/error
	goto, finish
bad_state:
	warning,'source_setup_event',['STATE variable has become ill-defined.','Abort source setup.'],/error
	goto, kill
bad_ptr:
	warning,'source_setup_event',['Parameter structure variable has become ill-defined.','Abort source setup.'],/error
	goto, kill

kill:
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).plast) then ptr_free, (*pstate).plast
	wdelete, (*pstate).pix

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;------------------------------------------------------------------------------------------

function source_convert_filter_to_local, filter, error=error

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

function source_convert_filter_to_standard, filter, error=error

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

pro source_draw, pstate, p=p, e=e, spec=speci, overlay=overlay, altp=p2, altE=E2, altspec=spec2, test=test

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
;	Do not add air path here. If wanted add that to the source model.

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
;	Do not add air path here. If wanted add that to the source model.

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
;	Do not add air path here. If wanted add that to the source model.

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
;........................................... Brightness

	if (*pstate).wid eq 0 then return
	wset, (*pstate).wid
	ymax = max(yb)
	ymin = 1.0e-4 * ymax
	xmax = max(e)
	if overlay then begin
		ymax = max( [ymax, max(yb2)])
		ymin = min( [ymax*1.0e-4, max(yb)*0.1, max(yb2)*0.1])
		xmax = max( [xmax, max(e2)])
	endif
	!y.title = 'Brightness (ph/s/mm2/usr/0.1% bandwidth)'
	
	plot, e,yb, xrange=[0.,xmax*1.05], yrange=[ymin*0.8,ymax*1.2], $
					color=spec_colour('white'), /ylog, $
					ticklen=1.0, /nodata, ystyle=1	;, xstyle=1
	
	if overlay then begin
		oplot, e2,yb2, color=spec_colour('orange')
		xyouts, 0.93, 0.88, 'Total = '+str_tidy(total(yb2),places=2), /norm,align=1, color=spec_colour('orange')
	endif
	oplot, e,yb, color=spec_colour('green')
	xyouts, 0.93, 0.92, 'Total = '+str_tidy(total(yb),places=2), /norm,align=1, color=spec_colour('green')

	wset, (*pstate).pix
	device,copy=[0,0,(*pstate).width,(*pstate).height, 0,0,(*pstate).wid]

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

;...........................................

	wset, (*pstate).wid
;	if ((*pstate).energy gt elow) and ((*pstate).energy lt ehigh) then begin
;		plots, [(*pstate).energy,(*pstate).energy], 10.^(!y.crange), color=spec_colour('orange')
;	endif
	return
end

;------------------------------------------------------------------------------------------

pro source_update_density, pstate

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
		warning,'source_update_density',['IDL run-time error caught.', '', $
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

pro source_update_pars, pstate, error=error

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
		warning,'source_update_pars',['IDL run-time error caught.', '', $
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
	widget_control, (*pstate).source_file, get_value=F
	(*p).file = F[0]

	widget_control, (*pstate).name_text, get_value=F
	(*p).modata.anode.name = F[0]
	widget_control, (*pstate).anode_formula_text, get_value=F
	(*p).modata.anode.formula = F[0]
	
	widget_control, (*pstate).volts_text, get_value=F
	(*p).modata.volts = float(F[0])
	widget_control, (*pstate).power_text, get_value=F
	(*p).modata.power = float(F[0])
	widget_control, (*pstate).phi_text, get_value=F
	(*p).modata.phi = float(F[0])
	widget_control, (*pstate).eps_text, get_value=F
	(*p).modata.eps = float(F[0])
	widget_control, (*pstate).spot_text, get_value=F
	(*p).modata.spot = float(F[0])
	widget_control, (*pstate).anode_thick_text, get_value=F
	(*p).beam.thick = float(F[0])
	widget_control, (*pstate).omega_text, get_value=F
	(*p).acceptance = float(F[0])

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

pro source_setup_pars, pstate, error=error

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
		warning,'source_update_pars',['IDL run-time error caught.', '', $
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

	set_widget_text, (*pstate).source_file, (*p).file
	widget_control, (*pstate).title_text, set_value=(*p).title
	
	widget_control, (*pstate).name_text, set_value=(*p).modata.anode.name
	widget_control, (*pstate).anode_formula_text, set_value=(*p).modata.anode.formula
	widget_control, (*pstate).anode_weight_mode, set_combobox_select=(*p).modata.anode.weight
	widget_control, (*pstate).volts_text, set_value=str_tidy((*p).modata.volts)
	widget_control, (*pstate).power_text, set_value=str_tidy((*p).modata.power)
	widget_control, (*pstate).phi_text, set_value=str_tidy((*p).modata.phi)
	widget_control, (*pstate).eps_text, set_value=str_tidy((*p).modata.eps)
	widget_control, (*pstate).spot_text, set_value=str_tidy((*p).modata.spot)
	widget_control, (*pstate).omega_text, set_value=str_tidy((*p).acceptance)
	
	widget_control, (*pstate).beam_mode, set_combobox_select=(*p).beam.mode
	widget_control, (*pstate).anode_thick_text, set_value=str_tidy((*p).beam.thick)
	widget_control, (*pstate).trans_thick_mapbase, map=(*p).beam.mode

	if (*p).poly.mode eq 1 then begin
		mode = 2
		map_poly = 1
		map_mono = 0
	endif else if (*p).mono.mode eq 1 then begin
		mode = 1
		map_poly = 0
		map_mono = 1
	endif else begin
		mode=0
		map_poly = 0
		map_mono = 0
	endelse
	widget_control, (*pstate).optics_mode, set_combobox_select=mode
	widget_control, (*pstate).omega_mapbase1, map=((*p).poly.mode ne 1)

	widget_control, (*pstate).mono_element_button, set_value={VALUE: ((*p).mono.z gt 0) ? element_name((*p).mono.z) : ''}
	widget_control, (*pstate).mono_energy_text, set_value=str_tidy((*p).modata.mono[0])
	widget_control, (*pstate).mono_bw_text, set_value=str_tidy((*p).modata.mono[1])
	widget_control, (*pstate).mono_eff_text, set_value=str_tidy((*p).modata.mono[2])
	widget_control, (*pstate).mono_mapbase1, map=map_mono
	widget_control, (*pstate).mono_mapbase2, map=map_mono

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
	widget_control, (*pstate).poly_mapbase1, map=map_poly
	widget_control, (*pstate).poly_mapbase2, map=map_poly

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

pro OnRealize_source_filter_number, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).n_filters-1
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_source_anode_weight_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).modata.anode.weight
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_source_beam_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).beam.mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_source_poly_type, wWidget

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

pro OnRealize_source_optics_mode, wWidget

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

pro OnRealize_source_thick_mode, wWidget

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

pro OnRealize_source_weight_mode, wWidget

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

pro OnRealize_source_pinhole_mode, wWidget

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

pro OnRealize_source_drawB, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=wid
(*pstate).wid = wid
(*pstate).draw = wWidget

window, /free, xsize=(*pstate).width, ysize=(*pstate).height, /pixmap
(*pstate).pix = !d.window

source_draw, pstate
end

;-----------------------------------------------------------------

pro OnRealize_source_drawS, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=wid
(*pstate).wid2 = wid
(*pstate).draw2 = wWidget

window, /free, xsize=(*pstate).width, ysize=(*pstate).height, /pixmap
(*pstate).pix2 = !d.window

source_draw, pstate
end

;------------------------------------------------------------------------------------------

pro source_setup, group_leader=group, TLB=tlb, pars=p, path=path, _extra=extra, xoffset=xoffset, yoffset=yoffset, test=test_mode

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
		warning,'source_setup',['IDL run-time error caught.', '', $
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
	pars = define(/source)
	pars.continuum = 1
	*p = pars
	(*p).n_filters = 1
endif
n_filters_max = n_elements((*p).filters)

poly_update, list=poly_list, title=poly_model, count=poly_count, error=error
poly_model = ['XOS default', poly_model]
poly_list = ['', poly_list]						;@3-23
poly_count++

; 	top-level base

tlb = widget_base( /column, title='Source Setup & Edit', /TLB_KILL_REQUEST_EVENTS, $
				group_leader=group, _extra=extra, uname='source_TLB', $
				/tlb_size_events, xoffset=xoffset, yoffset=yoffset, /base_align_center)

rbase = widget_base( tlb, /row, xpad=5, ypad=5, space=5, /base_align_top, /align_top)
tbase = widget_base( rbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

; set-up file buttons

lbase = widget_base( tbase, /column, xpad=0, ypad=0, space=3, /base_align_right, /align_center)
sbase = widget_base( lbase, /row, /base_align_center, ypad=0, xpad=0, space=5, /align_right)
lab = widget_label( sbase, value='File:')
source_file = widget_text( sbase, value=(*p).file, uname='source-file', /tracking, /editable, $
					uvalue='Enter the name of a source file to retrieve and edit.',scr_xsize=253)
load_setup_button = widget_button( sbase, value='Load', uname='load-source-button', /tracking, $
					uvalue='Load source parameters from a previous source file.', scr_xsize=38)
save_setup_button = widget_button( sbase, value='Save', uname='save-source-button', /tracking, $
					uvalue='Save source parameters to a source file. A "Save" is also needed to transfer modelled spectra results ' + $
					'to the yield calculation window.', scr_xsize=38)

titlebase = widget_base( lbase, /row, /base_align_center, ypad=0, xpad=0, space=5, /align_right)
lab = widget_label( titlebase, value='Title:')
title_text = widget_text( titlebase, value=(*p).title, uname='title-text', /tracking, /editable, $
					uvalue='Enter a title descriptor for this X-ray source.',scr_xsize=343)

; anode

anodebase = widget_base( tbase, /column, /base_align_right, /align_right, ypad=1, xpad=2, space=3, /frame, scr_xsize=frame_width)
lab = widget_label( anodebase, value='Anode', /align_center)

abase1 = widget_base( anodebase, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( abase1, value='Name:')
name_text = widget_text( abase1, value=(*p).modata.anode.name, uname='name-text', /tracking, /editable, $
				uvalue='Enter the name of the anode material.', scr_xsize=333-13)

abase2 = widget_base( anodebase, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( abase2, value='Formula:')
anode_formula_text = widget_text( abase2, value=(*p).modata.anode.formula, uname='anode-formula', /tracking, /editable, ysize=1,  $
				uvalue=str_tidy(i)+'  Enter the chemical formula for the anode. ' + $
				'Enclose radicals in brackets "( )", with optional multipliers in atomic fraction or weight %. ' + $
				'e.g. Components in wt%: "(Ga)95(In)5"; atomic proportions: "LaB6".', scr_xsize=formula_xsize)
anode_weight_mode = widget_combobox( abase2, value=[' Atomic Fraction','  Weight %'], uname='anode-weight-mode', /tracking, $
					notify_realize='OnRealize_source_anode_weight_mode', $
					uvalue=str_tidy(i)+'  Select the type of multiplier factors in formulae. These appear after ' + $
					'a radical term contained in brackets, e.g. "(SiO2)12.3". Options include "atomic fraction" or "weight %".', xsize=xsize_atomic)

; source details

parbase = widget_base( tbase, /column, /base_align_right, /align_right, ypad=1, xpad=2, space=spc_filters, /frame, scr_xsize=frame_width)
lab = widget_label( parbase, value='Source Parameters', /align_center)

dbase1 = widget_base( parbase, column=3, /base_align_right, ypad=0, xpad=0, space=1, /align_right)

dbase1a = widget_base( dbase1, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase1a, value='Volts:')
volts_text = widget_text( dbase1a, value=str_tidy((*p).modata.volts), uname='volts-text', /tracking, /editable, $
					uvalue='Enter the source electon beam anode bias (kV).', scr_xsize=pars_xsize)

dbase1b = widget_base( dbase1, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase1b, value='Power:')
power_text = widget_text( dbase1b, value=str_tidy((*p).modata.power), uname='power-text', /tracking, /editable, $
					uvalue='Enter the source electron beam power (W). Remember to keep power/spot size within anode material de-rating factors ' + $
					'(typically 1.5 W/um for Cu, Mo and 2.5 W/um for W on fixed solids up to ~10 W/um for liquid metal jets and rotating anodes).', scr_xsize=pars_xsize)

dbase1d = widget_base( dbase1, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase1d, value='Angle in:')
phi_text = widget_text( dbase1d, value=str_tidy((*p).modata.phi), uname='phi-text', /tracking, /editable, $
					uvalue='Enter the incident electon beam angle to the surface (degrees). ' + $
					'Typically, use 90 degrees for an in-line transmission anode.', scr_xsize=pars_xsize)

dbase1e = widget_base( dbase1, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase1e, value='Angle out:')
eps_text = widget_text( dbase1e, value=str_tidy((*p).modata.eps), uname='eps-text', /tracking, /editable, $
					uvalue='Enter the exit X-ray beam take-off angle to the surface (degrees). ' + $
					'Typically, use 90 degrees for an in-line transmission anode.', scr_xsize=pars_xsize)

dbase1c = widget_base( dbase1, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase1c, value='Spot:')
spot_text = widget_text( dbase1c, value=str_tidy((*p).modata.spot), uname='spot-text', /tracking, /editable, $
					uvalue='Enter the source electron beam spot size (mm). Remember to keep power/spot size within anode material de-rating factors ' + $
					'(typically 1.5 W/um for Cu, Mo and 2.5 W/um for W on fixed solids up to ~10 W/um for liquid metal jets and rotating anodes).',scr_xsize=pars_xsize)

omega_mapbase1 = widget_base( dbase1, /row, /base_align_center, ypad=0, xpad=0, space=5, map=((*p).poly.mode ne 1))
dbase1f = widget_base( omega_mapbase1, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( dbase1f, value='Omega:')
omega_text = widget_text( dbase1f, value=str_tidy((*p).acceptance), uname='omega-text', /tracking, /editable, $
					uvalue='Enter the source output solid-angle (msr) as accepted by the source output and rest of system.',scr_xsize=pars_xsize)

dbase2 = widget_base( parbase, column=1, /base_align_right, ypad=0, xpad=0, space=3, /align_center)

dbase2a = widget_base( dbase2, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=20)
beam_mode = widget_combobox( dbase2a, value=['    Reflection','    Transmission'], uname='beam-mode', /tracking, $
					notify_realize='OnRealize_source_beam_mode', $
					uvalue='Select the exit X-ray mode between "Reflection", for a conventional side-window source, ' + $
					'or "Transmission", for a thin in-line transmission anode source.', xsize=141)

trans_thick_mapbase = widget_base( dbase2a, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5, map=(*p).beam.mode)
lab = widget_label( trans_thick_mapbase, value='Thick:')
anode_thick_text = widget_text( trans_thick_mapbase, value=str_tidy((*p).beam.thick), uname='anode-thick-text', /tracking, /editable, $
					uvalue='Enter the thickness of a transmission anode (mg/cm2 = microns * density /10). Put extra end-window layers in the ' + $
					'filter specifications below.', scr_xsize=pars_xsize)

; Optics details

opticsbase = widget_base( tbase, /column, /base_align_right, /align_right, ypad=1, xpad=2, space=spc_filters, /frame, scr_xsize=frame_width)
lab = widget_label( opticsbase, value='Inline Optics', /align_center)

mbase0 = widget_base( opticsbase, column=1, /base_align_right, ypad=0, xpad=0, space=3, /align_center)

mbase0a = widget_base( mbase0, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=20)
optics_mode = widget_combobox( mbase0a, value=['    None',' Monochromator',' Polycapillary lens'], uname='optics-mode', /tracking, $
					notify_realize='OnRealize_source_optics_mode', xsize=140, $
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
					notify_realize='OnRealize_source_poly_type', xsize=160, $
					uvalue='Select Polycapillary lens model, from "polycapillary-*.csv" files loaded.')

opticsbase2 = widget_base( opticsbase )
mono_mapbase2 = widget_base( opticsbase2, column=3, /base_align_right, ypad=0, xpad=0, space=3, /align_right, map=(*p).mono.mode)

mbase1a = widget_base( mono_mapbase2, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( mbase1a, value='Energy:')
mono_energy_text = widget_text( mbase1a, value=str_tidy((*p).modata.mono[0]), uname='mono-energy-text', /tracking, /editable, $
					uvalue='Enter the energy of the centre of the monochromator (keV). Alternatively, use the GREEN element button to pop-up ' + $
					"a periodic table to select centre energy based on element's Ka energy.", scr_xsize=pars_xsize)

mbase1b = widget_base( mono_mapbase2, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( mbase1b, value='Bandwidth:')
mono_bw_text = widget_text( mbase1b, value=str_tidy((*p).modata.mono[1]), uname='mono-bw-text', /tracking, /editable, $
					uvalue='Enter the bandwidth of the monochromator (%).', scr_xsize=pars_xsize)

mbase1c = widget_base( mono_mapbase2, /row, /base_align_center, ypad=0, xpad=pars_xpad, space=5)
lab = widget_label( mbase1c, value='Eff:')
mono_eff_text = widget_text( mbase1c, value=str_tidy((*p).modata.mono[2]), uname='mono-eff-text', /tracking, /editable, $
					uvalue='Enter the transmission efficiency of the monochromator (<1).', scr_xsize=pars_xsize)

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
					notify_realize='OnRealize_source_filter_number', $
					uvalue='Select the number of source filters.', xsize=xsize_nfilters)

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
					notify_realize='OnRealize_source_thick_mode', $
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
					notify_realize='OnRealize_source_weight_mode', $
					uvalue=str_tidy(i)+'  Select the type of multiplier factors in formulae. These appear after ' + $
					'a radical term contained in brackets, e.g. "(SiO2)12.3". Options include "atomic fraction" or "weight %".', xsize=xsize_atomic)

	ll3base = widget_base( filter_base[i], /row, /base_align_center, ypad=0, xpad=0, space=2)
	lab = widget_label( ll3base, value='Special:')
	pinhole_mode[i] = widget_combobox( ll3base, value=['Plain filter','Pinhole filter','Bragg filter'], uname='pinhole-mode', /tracking, $
					notify_realize='OnRealize_source_pinhole_mode', $
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
					'This is the ratio of the source solid-angle divided by the hole solid-angle.', scr_xsize=70)

	bragg_base[i] = widget_base( map_base, /row, map=map_bragg, /base_align_center, /align_right, ypad=0, xpad=20, space=3)
	bragg_button[i] = widget_button( bragg_base[i], value='      Edit Bragg     ', uname='bragg-button', /tracking, uvalue=str_tidy(i)+'  Open Bragg filter editor.')
	map = 0
endfor

;.................................................................................

tab_panel = widget_tab( rbase, location=3, /align_center, uname='plot-tab-panel')

brightness_base = widget_base( tab_panel, title='Brightness', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=draw_xsize+20, scr_ysize=left_ysize)

draw_brightness = widget_draw( brightness_base, uname='draw-brightness', xsize=draw_xsize, ysize=draw_ysize, notify_realize='OnRealize_source_drawB', $
			retain=retain, /button_events)

spectrum_base = widget_base( tab_panel, title='Spectrum', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=draw_xsize+20, scr_ysize=left_ysize)

draw_spectrum = widget_draw( spectrum_base, uname='draw-spectrum', xsize=draw_xsize, ysize=draw_ysize, notify_realize='OnRealize_source_drawS', $
			retain=retain, /button_events)


help = widget_text( tbase, scr_xsize=387, ysize=4, /wrap, uname='help', /tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

state = {	$
		path:				ptr_new(path), $		; pointer to current path
		p:					p, $					; pointer to parameters
		plast:				ptr_new(/allocate_heap), $	; pointer to previous set of parameters
		active:				0, $					; active filter for edit
		n_filters_max:		n_filters_max, $		; maximum number of filters
		title_text:			title_text, $			; ID of title text widget
		source_file:		source_file, $			; ID of filter file text widget
		filter_number:		filter_number, $		; ID of # filters droplist
		define_filter:		define_filter, $		; ID of filter define selection droplist
		poly_list:			poly_list, $			; polycapillary config files ;@3-23
		poly_model:			poly_model, $			; polycapillary model names
		poly_active:		0, $					; activbe polycapillary model

		draw:				draw_brightness, $		; ID of brightness draw widget
		wid:				0, $					; window ID of brightness widget
		pix:				0L, $					; pix-map
		draw2:				draw_spectrum, $		; ID of spectrum draw widget
		wid2:				0, $					; window ID of spectrum draw widget
		pix2:				0L, $					; pix-map2
		width:				draw_xsize, $			; width of window
		height:				draw_ysize, $			; height of window
		energy:				0.0, $					; energy cursor
		test:				test_mode, $			; test mode

		name_text:			name_text, $			; ID of anode name text
		anode_formula_text:	anode_formula_text, $	; ID of anode formula text
		anode_weight_mode:	anode_weight_mode, $	; ID of anode weight/at% droplist
		volts_text:			volts_text, $			; ID of volts text
		power_text:			power_text, $			; ID of power text
		spot_text:			spot_text, $			; ID of spot size text
		phi_text:			phi_text, $				; ID of phi text
		eps_text:			eps_text, $				; ID of eps text		
		omega_text:			omega_text, $			; ID of acceptance omega text
		beam_mode:			beam_mode, $			; ID of beam mode
		trans_thick_mapbase: trans_thick_mapbase, $	; ID of transmission mode anode thickness text
		anode_thick_text:	anode_thick_text, $		; ID of transmission anode thickness (mg/cm2)
		omega_mapbase1:		omega_mapbase1, $		; ID of Omage map base

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
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['path' $						; new path
						], $						; 
					from=group

xmanager, 'source_setup', tlb, /no_block

return
end
