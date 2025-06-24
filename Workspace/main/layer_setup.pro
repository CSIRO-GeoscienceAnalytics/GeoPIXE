;
;	LAYER setup and execution.

pro layer_setup_event, event

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
		warning,'Layer_setup_event',['IDL run-time error caught.', '', $
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

if (*pstate).gamma eq 1 then begin
	pname = 'PIGE'
	yname = 'yieldg'
	xname = 'gamma-ray'
endif else begin
	pname = 'PIXE/SXRF'
	yname = 'yield'
	xname = 'X-ray'
endelse

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
				;print,'layer Setup: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'new-detectors': begin
				present = (*(*p).detector_list)[(*p).detector_mode]
				detector_update, /array, list=list, title=title, present=present, new=new, file=f
				*(*p).detector_list = list
				widget_control, (*pstate).detector_mode, set_value=title, set_combobox_select=new
				end
			'new-source': begin						; from source_setup
				if ptr_valid( event.pointer) then begin
					*(*p).source = *event.pointer
					set_widget_text, (*pstate).source_text, (*(*p).source).file
					(*p).beam.mode = 6
					(*p).beam.z1 = 0
					(*p).beam.a1 = 0
					(*p).beam.state = 1.0
					(*p).beam.state_factor = 1.0
					(*p).beam.e_factor = 1.0
					mono = ((*p).beam.mode le 5)
					sense = ((*p).beam.mode lt 5) and ((*pstate).gamma eq 0)
					widget_control, (*pstate).z1_text, set_value=str_tidy((*p).beam.z1), sensitive=sense
					widget_control, (*pstate).a1_text, set_value=str_tidy((*p).beam.a1), sensitive=sense
					widget_control, (*pstate).state_text, set_value=str_tidy((*p).beam.state), sensitive=sense
					widget_control, (*pstate).beam_mapbase_ZA, map=mono
					widget_control, (*pstate).beam_mapbase_mono, map=mono
					widget_control, (*pstate).beam_mapbase_continuum, map=1-mono							
				endif
				end
			'new-pink': begin						; from pink_setup
				if ptr_valid( event.pointer) then begin
					(*(*p).source) = *event.pointer
					set_widget_text, (*pstate).source_text, (*(*p).source).file
					(*p).beam.mode = 7
					(*p).beam.z1 = 0
					(*p).beam.a1 = 0
					(*p).beam.state = 1.0
					(*p).beam.state_factor = 1.0
					(*p).beam.e_factor = 1.0
					mono = ((*p).beam.mode le 5)
					sense = ((*p).beam.mode lt 5) and ((*pstate).gamma eq 0)
					widget_control, (*pstate).z1_text, set_value=str_tidy((*p).beam.z1), sensitive=sense
					widget_control, (*pstate).a1_text, set_value=str_tidy((*p).beam.a1), sensitive=sense
					widget_control, (*pstate).state_text, set_value=str_tidy((*p).beam.state), sensitive=sense
					widget_control, (*pstate).beam_mapbase_ZA, map=mono
					widget_control, (*pstate).beam_mapbase_mono, map=mono
					widget_control, (*pstate).beam_mapbase_continuum, map=1-mono							
				endif
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
				widget_control, (*pstate).help, set_value='Enter target, beam and detector details and "calculate yields" to a "YIELD" output file. ' + $
						'Remember to save settings in an LCM file using "Save" at the top.'
			endelse
		endif
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request layer_Setup ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'load-setup-button': begin
		file = find_file2( (*p).lcm_file)
		path = extract_path( file[0])
		file = strip_path(file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		F = file_requester( /read, filter = '*.lcm', file=file, path=path, group=event.top, $
					title='Select the source LCM parameter file', /fix_filter, $
					preview_routine='file_lcm_preview')
		if F ne '' then begin
			F = strip_file_ext(F) + '.lcm'
			*(*pstate).path = extract_path(F)
			set_widget_text, (*pstate).lcm_file, F			
			(*p).lcm_file = F
			load_lcm_parameters, pstate, F
		endif
		end

	'save-setup-button': begin
		file = find_file2( (*p).lcm_file)
		path = extract_path( file[0])
		file = strip_path(file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		F = file_requester( /write, file=file, filter = '*.lcm', path=path, group=event.top, $
					title='Save the layer setup parameters to a LCM file', /fix_filter, $
					preview_routine='file_lcm_preview')
		if F ne '' then begin
			F = strip_file_ext(F) + '.lcm'
			*(*pstate).path = extract_path(F)
			set_widget_text, (*pstate).lcm_file, F
			(*p).lcm_file = F

			layer_update_pars, pstate
			save_lcm_parameters, pstate, F
		endif
		end

	'lcm-file': begin
		widget_control, event.id, get_value=F
		F = strip_file_ext(F[0]) + '.lcm'
		(*p).lcm_file = F
		*(*pstate).path = extract_path(F)
		set_widget_text, (*pstate).lcm_file, F			
		load_lcm_parameters, pstate, F
		end

	'output-file-button': begin
		file = find_file2( (*p).output_file)
		path = extract_path( file[0])
		file = strip_path(file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		F = file_requester( /write, filter = '*.'+yname, path=path, group=event.top, $
					title='Select the yields output file name', file=file, /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.'+yname
			*(*pstate).path = extract_path(F)
			set_widget_text, (*pstate).output_file, F			
			(*p).output_file = F
		endif
		end

	'output-file': begin
		widget_control, event.id, get_value=F
		F = strip_file_ext(F[0]) + '.'+yname
		(*p).output_file = F
		*(*pstate).path = extract_path(F)
		set_widget_text, (*pstate).output_file, F			
		end

	'beam-mode': begin
		(*p).beam.mode = event.index
		case (*p).beam.mode of
			0: begin											; proton
				(*p).beam.z1 = 1
				(*p).beam.a1 = 1
				(*p).beam.state = 1.0
				(*p).beam.state_factor = 1.0
				(*p).beam.e_factor = 1.0
				end
			1: begin											; molecular H2
				(*p).beam.z1 = 1
				(*p).beam.a1 = 1
				(*p).beam.state = 1.0
				(*p).beam.state_factor = 0.5
				(*p).beam.e_factor = 0.5
				end
			2: begin											; helium +1
				(*p).beam.z1 = 2
				(*p).beam.a1 = 4
				(*p).beam.state = 1.0
				(*p).beam.state_factor = 1.0
				(*p).beam.e_factor = 1.0
				end
			3: begin											; alphas
				(*p).beam.z1 = 2
				(*p).beam.a1 = 4
				(*p).beam.state = 2.0
				(*p).beam.state_factor = 1.0
				(*p).beam.e_factor = 1.0
				end
			4: begin											; general
				(*p).beam.state_factor = 1.0
				(*p).beam.e_factor = 1.0
				end
			5: begin											; photons (mono)
				(*p).beam.z1 = 0
				(*p).beam.a1 = 0
				(*p).beam.state = 1.0
				(*p).beam.state_factor = 1.0
				(*p).beam.e_factor = 1.0
				end

;			Additional beam plugins. For now only accept one ...

			else: begin											; photons (continuum, pink, ...)
				(*p).beam.z1 = 0
				(*p).beam.a1 = 0
				(*p).beam.state = 1.0
				(*p).beam.state_factor = 1.0
				(*p).beam.e_factor = 1.0
				end
		endcase
		mono = ((*p).beam.mode le 5)
		sense = ((*p).beam.mode lt 5) and ((*pstate).gamma eq 0)
		widget_control, (*pstate).z1_text, set_value=str_tidy((*p).beam.z1), sensitive=sense
		widget_control, (*pstate).a1_text, set_value=str_tidy((*p).beam.a1), sensitive=sense
		widget_control, (*pstate).state_text, set_value=str_tidy((*p).beam.state), sensitive=sense
		widget_control, (*pstate).beam_mapbase_ZA, map=mono
		widget_control, (*pstate).beam_mapbase_mono, map=mono
		widget_control, (*pstate).beam_mapbase_continuum, map=1-mono
		end

	'load-source-button': begin
		case (*p).beam.mode of
			6: begin									; lab source
				file = ptr_good((*p).source) ? find_file2( (*(*p).source).file) : ''
				path = extract_path( file[0])
				file = strip_path(file[0])
				if lenchr(path) eq 0 then path = *(*pstate).path
				F = file_requester( /read, filter = '*.source', file=file, path=path, group=event.top, $
							title='Select the X-ray Lab SOURCE parameter file', /fix_filter, $
							preview_routine='file_source_preview')
				if F ne '' then begin
					F = strip_file_ext(F) + '.source'
					src = read_source( F, error=err)
					if err eq 0 then begin
						*(*pstate).path = extract_path(F)
						set_widget_text, (*pstate).source_text, F			
						(*(*p).source) = src
						(*(*p).source).file = F
					endif
				endif
				end
			7: begin									; pink beam
				file = ptr_good((*p).source) ? find_file2( (*(*p).source).file) : ''
				path = extract_path( file[0])
				file = strip_path(file[0])
				if lenchr(path) eq 0 then path = *(*pstate).path
				F = file_requester( /read, filter = '*.pink', file=file, $
							path=path, group=event.top, $
							title='Select the X-ray PINK beam parameter file', /fix_filter, $
							preview_routine='file_source_preview')
				if F ne '' then begin
					F = strip_file_ext(F) + '.pink'
					src = read_pink( F, error=err)
					if err eq 0 then begin
						*(*pstate).path = extract_path(F)
						set_widget_text, (*pstate).source_text, F			
						(*(*p).source) = src
						(*(*p).source).file = F
					endif
				endif
				end
			else:
		endcase
		end

	'new-source-button': begin
		case (*p).beam.mode of
			6: begin									; lab source
				source_setup, group_leader=event.top, TLB=tlb, pars=(*pstate).psource, path=*(*pstate).path
				register_notify, event.top, [ 'path','new-source'], from=tlb
				end
			7: begin									; lab source
				pink_setup, group_leader=event.top, TLB=tlb, pars=(*pstate).ppink, path=*(*pstate).path
				register_notify, event.top, [ 'path','new-pink'], from=tlb
				end
			else:
		endcase
		end
		
	'title-text': begin
		widget_control, event.id, get_value=s
		(*p).title = s[0]
		end

	'energy-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).beam.energy = float(s)
		end

	'z1-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).beam.z1 = fix(s)
		(*p).beam.state_factor = 1.0
		(*p).beam.e_factor = 1.0
		(*p).beam.mode = 4
		widget_control, (*pstate).beam_mode, set_combobox_select=4
		end

	'a1-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).beam.a1 = fix(s)
		(*p).beam.state_factor = 1.0
		(*p).beam.e_factor = 1.0
		(*p).beam.mode = 4
		widget_control, (*pstate).beam_mode, set_combobox_select=4
		end

	'state-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).beam.state = float(s)
		(*p).beam.state_factor = 1.0
		(*p).beam.e_factor = 1.0
		(*p).beam.mode = 4
		widget_control, (*pstate).beam_mode, set_combobox_select=4
		end

	'emin-text': begin
		widget_control, event.id, get_value=s
		e = float(s[0]) > 0.2
		(*p).emin = e
		widget_control, event.id, set_value=str_tidy(e)
		end

	'emax-text': begin
		widget_control, event.id, get_value=s
		e = float(s[0]) > (*p).emin
		widget_control, event.id, set_value=str_tidy(e)
		(*p).emax = float(s)
		end

	'array-option': begin
		case event.value of
			0: begin
				(*p).array = event.select
				widget_control, (*pstate).detector_base, map=(*p).array
				*(*pstate).pnew = {array:(*p).array, file:(*(*p).detector_list)[(*p).detector_mode]}

				if (*p).array then begin
					n = n_elements(*(*p).detector_list)
					if n lt 1 then goto, finish
					detector_update, /array, list=list, title=title, present=(*(*p).detector_list)[(*p).detector_mode], new=i, file=f
					*(*p).detector_list = list
					(*p).detector_mode = i
					widget_control, (*pstate).detector_mode, set_value=title, set_combobox_select=i, sensitive=(*p).array
					detector = read_detector( f, error=error)
					if error then begin
						warning, 'layer_setup','Error reading Detectors file '+f, /error
						goto, finish
					endif else begin
						*(*p).pdetector = *detector

						if (*(*p).pdetector).array and (strlen((*(*p).pdetector).layout) gt 0) then begin
							d = read_detector_layout((*(*p).pdetector).layout, error=error)
							if error eq 0 then *(*p).playout = d
						endif
					endelse
				endif
				end
			else:
		endcase
		end

	'detector-mode': begin
		n = n_elements(*(*p).detector_list)
		if n lt 1 then goto, finish
		(*p).detector_mode = event.index
		detector_update, /array, list=list, title=title, present=(*(*p).detector_list)[(*p).detector_mode], new=i, file=f
		*(*p).detector_list = list
					widget_control, (*pstate).detector_mode, set_value=title, set_combobox_select=i, sensitive=(*p).array
		detector = read_detector( f, error=error)
		if error then begin
			warning, 'layer_setup','Error reading Detectors file '+(*(*p).detector_list)[(*p).detector_mode], /error
			goto, finish
		endif else begin
			*(*p).pdetector = *detector

			if (*(*p).pdetector).array and (strlen((*(*p).pdetector).layout) gt 0) then begin
				d = read_detector_layout((*(*p).pdetector).layout, error=error)
				if error eq 0 then *(*p).playout = d
			endif
		endelse
		end

	'detector-show': begin
		layer_update_pars, pstate
		detector_show, p, group=event.top, tlb=xtlb
		(*pstate).xobj_tlb = xtlb
		end

	'theta-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).detector.theta = float(s)
		end

	'phi-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).detector.phi = float(s)
		end

	'alpha-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).target.alpha = float(s)
		end

	'beta-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).target.beta = float(s)
		end

	'layer-number': begin
		(*p).n_layers = event.index + 1
		if (*p).unknown gt (*p).n_layers then begin
			(*p).unknown = (*p).n_layers
			widget_control, (*pstate).unknown_number, set_combobox_select=(*p).unknown-1
		endif
		if (*pstate).active gt (*p).n_layers-1 then begin
			widget_control, (*pstate).layer_base[(*pstate).active], map=0
			(*pstate).active = (*p).n_layers-1
			widget_control, (*pstate).layer_base[(*pstate).active], map=1
			widget_control, (*pstate).define_layer, set_combobox_select=(*pstate).active
		endif

		widget_control, (*pstate).unknown_base, map=((*p).n_layers gt 1)
		end

	'unknown-number': begin
		(*p).unknown = (event.index + 1) < (*p).n_layers
		widget_control, (*pstate).unknown_number, set_combobox_select=(*p).unknown-1
		end

	'define-layer': begin
		widget_control, (*pstate).layer_base[(*pstate).active], map=0
		(*pstate).active = event.index
		widget_control, (*pstate).layer_base[(*pstate).active], map=1
		if (*pstate).active gt (*p).n_layers-1 then begin
			(*p).n_layers = (*pstate).active+1
			widget_control, (*pstate).layer_number, set_combobox_select=(*p).n_layers-1
			widget_control, (*pstate).unknown_base, map=((*p).n_layers gt 1)
		endif
		end

	'thick-text': begin
		widget_control, event.id, get_value=s
		t = float(s[0])
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)
		(*p).layer[n].thick = t
		end

	'thick-mode': begin
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)
		(*p).layer[n].microns = event.index

;		widget_control, (*pstate).density_base[n], map=(*p).layer[n].microns
		z = atomic_number( (*p).layer[n].formula)
		if z gt 0 then begin
			(*p).layer[n].density = density(z)
			widget_control, (*pstate).density_text[n], set_value=str_tidy((*p).layer[n].density)
		endif
		end

	'thick-many': begin
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)
		(*p).many[n] = event.index

		if (*p).many[n] eq 0 then begin
			widget_control, (*pstate).many_base[n], map=0, scr_ysize=1
		endif else begin
			widget_control, (*pstate).many_base[n], map=1, scr_ysize=(*pstate).many_base_y[n]
		endelse
		end

	'thick-min': begin
		widget_control, event.id, get_value=s
		t = float(s[0])
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)
		(*p).thick_min[n] = t
		end

	'thick-max': begin
		widget_control, event.id, get_value=s
		t = float(s[0])
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)
		(*p).thick_max[n] = t
		end

	'thick-step': begin
		widget_control, event.id, get_value=s
		t = float(s[0])
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)
		(*p).thick_step[n] = t
		end

	'layer-density': begin
		widget_control, event.id, get_value=s
		t = float(s[0])
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)
		(*p).layer[n].density = t
		end

	'formula-mode': begin
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)
		(*p).layer[n].formula_mode = event.index

		widget_control, (*pstate).formula_map[n], map=1-(*p).layer[n].formula_mode
		widget_control, (*pstate).oxide_map[n], map=(*p).layer[n].formula_mode
		end

	'weight-mode': begin
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)
		(*p).layer[n].weight = event.index

		if (*p).layer[n].weight eq 0 then begin
			widget_control, (*pstate).oxide_table1[n], column_labels=['at. frac.']
			widget_control, (*pstate).oxide_table2[n], column_labels=['at. frac.']
		endif else begin
			widget_control, (*pstate).oxide_table1[n], column_labels=['wt %']
			widget_control, (*pstate).oxide_table2[n], column_labels=['wt %']
		endelse
		end

	'layer-formula': begin
		widget_control, event.id, get_value=s
		t = ''
		for i=0L,n_elements(s)-1 do t = t+s[i]
		t = strcompress(t,/remove_all)
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)
		(*p).layer[n].formula = t

		z = atomic_number( (*p).layer[n].formula)
		if z gt 0 then begin
			(*p).layer[n].density = density(z)
			widget_control, (*pstate).density_text[n], set_value=str_tidy((*p).layer[n].density)
		endif
		end

	'oxide-table1': begin
		widget_control, event.id, get_value=t
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)
		(*p).layer[n].oxides[0:5] = t
		end

	'oxide-table2': begin
		widget_control, event.id, get_value=t
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)
		(*p).layer[n].oxides[6:11] = t
		end

	'calculate-button': begin
		layer_update_pars, pstate
		widget_control, /hour

		n = (*p).n_layers
		formula = (*p).layer[0:n-1].formula
		thick = (*p).layer[0:n-1].thick
		microns = (*p).layer[0:n-1].microns
		density = (*p).layer[0:n-1].density
		weight = (*p).layer[0:n-1].weight
		energy = (*p).beam.energy * (*p).beam.e_factor
		charge_state = (*p).beam.state * (*p).beam.state_factor

		print,'Calculate yields ...'

;	The hierarchy of routines for yield calculation follows:
;		geo_array_yield			Accumulate yields across detector array.
;			geo_yield2			Initially for the central detector, including secondary fluorescence.
;			geo_yield2			In a loop over all detectors in an array, without sec. fluorescence.
;				experiment_angles	Determine direction cosines for beam and detectors.
;				calc_slices		Construct incremental slices of sample from input layer specs.
;				slow_beam		Model slowing (or attenuation) of input beam.
;				calc_abs		Build array of mass-absorption coefficients for all lines.
;				calc_cross		Calculate cross-sections as beam slow for all elements.
;				calc_yield		Integrate X-ray yields over all slices.

		theta = (*p).detector.theta
		phi = (*p).detector.phi
		array = (*p).array
		beam = (*(*p).source)

		if n_elements(beam) ne 0 then begin
			if (*p).beam.mode le 5 then begin
				beam.continuum = 0
				beam.energy = energy
			endif
		endif

;;;		select = [26,28,30]
;;;		select = [24,26,82]

;		Keep additions here in tune with those in 'fit_recalculate_yields'.

		yield = geo_array_yield( formula, thick, microns=microns, density=density, weight=weight, $
					energy=energy, theta=theta, phi=phi, beam=beam, $
					alpha=(*p).target.alpha, beta=(*p).target.beta, unknown=(*p).unknown, $
					z1=(*p).beam.z1, a1=(*p).beam.a1, state=charge_state, $
					e_min=(*p).emin, e_max=(*p).emax, /progress, error=error, $
					intensity=intensity, lines=lines, n_lines=n_lines, z2=z2, shell=shell, $
					layers=layers, sec_yield=sec_yield, select=select, mu_zero=mu_zero, $
					gamma=(*pstate).gamma, e_lines=e_lines, $
					detector=(*p).pdetector, layout=(*p).playout, ratio_yield=ratio_yield, array=array, $
					ratio_intensity=ratio_intensity, $
					x_slow=x_slow, e_slow=e_slow, xsect=xsect, yieldx=yieldx, $
					cmux=cmux, cos_beam=cos_beam, dydx=dydx, half_slice_id=hlid, flux=flux)

		print,'Done geo_array_yield.'
		if error then goto, finish

		detfile = ''
		if n_elements( *(*p).pdetector) gt 0 then detfile = (*(*p).pdetector).file

		peaks = make_peaks( z2=z2, shell=shell, n_lines=n_lines, lines=lines, intensity=intensity, yield=yield, $
				layers=layers, unknown=(*p).unknown, e_beam=energy, theta=(*p).detector.theta, $
				phi=(*p).detector.phi, alpha=(*p).target.alpha, beta=(*p).target.beta, $
				title=(*p).title, formula=formula, thick=thick, microns=microns, $
				density=density, weight=weight, z1=(*p).beam.z1, a1=(*p).beam.a1, $
				state=charge_state, e_min=(*p).emin, e_max=(*p).emax, mu_zero=mu_zero, $
				e_lines=e_lines, ratio_yield=ratio_yield, array=array, detector_file=detfile, $
				ratio_intensity=ratio_intensity, beam=beam )

;		Use these for plotting various results as a function of x,e ...

		*(*p).peaks = peaks
		*(*p).peaks2 = {x_slow:x_slow, e_slow:e_slow, xsect:xsect, yieldx:yieldx, $
						cmux:cmux, cos_beam:cos_beam, dedx:dydx, half_slice_id:hlid, flux:flux}

		q = where( (*p).many[0:(*p).n_layers-1] ne 0, nq)
		if q[0] ne -1 then begin
		  progress, tlb=progress_tlb, title='GeoPIXE: PIXE/SXRF Grid Yield Calculation'

		  if ((*p).many[q[0]] eq 1) or (nq eq 1) then begin								; 1D many mode

			nt = min( round(((*p).thick_max[q] - (*p).thick_min[q])/(*p).thick_step[q]) )
			thick[q] = (*p).thick_min[q]

			for i=0L,nt do begin
				progress, /update, progress_tlb, {unit:0, value:0, current:i, size:nt+1}, cancel=cancel

				yield = geo_array_yield( formula, thick, microns=microns, density=density, weight=weight, $
					energy=energy, theta=theta, phi=phi, beam=beam, $
					alpha=(*p).target.alpha, beta=(*p).target.beta, unknown=(*p).unknown, $
					z1=(*p).beam.z1, a1=(*p).beam.a1, state=charge_state, $
					e_min=(*p).emin, e_max=(*p).emax, progress=0, error=error, $
					intensity=intensity, lines=lines, n_lines=n_lines, z2=z2, shell=shell, $
					layers=layers, sec_yield=sec_yield, select=select, mu_zero=mu_zero, $
					gamma=(*pstate).gamma, e_lines=e_lines, ratio_intensity=ratio_intensity, $
					detector=(*p).pdetector, layout=(*p).playout, ratio_yield=ratio_yield, array=array, $
					x_slow=x_slow, e_slow=e_slow, xsect=xsect, yieldx=yieldx, $
					cmux=cmux, cos_beam=cos_beam, dydx=dydx, half_slice_id=hlid, flux=flux )

				if error then goto, finish

				peaks2 = make_peaks( z2=z2, shell=shell, n_lines=n_lines, lines=lines, intensity=intensity, yield=yield, $
					layers=layers, unknown=(*p).unknown, e_beam=energy, theta=(*p).detector.theta, $
					phi=(*p).detector.phi, alpha=(*p).target.alpha, beta=(*p).target.beta, $
					title=(*p).title, formula=formula, thick=thick, microns=microns, $
					density=density, weight=weight, z1=(*p).beam.z1, a1=(*p).beam.a1, $
					state=charge_state, e_min=(*p).emin, e_max=(*p).emax, mu_zero=mu_zero, $
					e_lines=e_lines, ratio_yield=ratio_yield, array=array, detector_file=(*(*p).pdetector).file, $
					ratio_intensity=ratio_intensity, beam=beam)
				peaks = [peaks, peaks2]

				thick[q] = thick[q] + (*p).thick_step[q]
			endfor

		  endif else if (*p).many[q[0]] eq 2 then begin									; 2D many mode

			nt1 = min( round(((*p).thick_max[q[0]] - (*p).thick_min[q[0]])/(*p).thick_step[q[0]]) ) > 1
			nt2 = min( round(((*p).thick_max[q[1]] - (*p).thick_min[q[1]])/(*p).thick_step[q[1]]) ) > 1
			thick[q[0]] = (*p).thick_min[q[0]]

			for i=0L,nt1 do begin
			  thick[q[1:*]] = (*p).thick_min[q[1:*]]

			  for j=0L,nt2 do begin
				progress, /update, progress_tlb, {unit:0, value:0, current:i*(nt2+1)+j, size:(nt1+1)*(nt2+1)}, cancel=cancel

				yield = geo_array_yield( formula, thick, microns=microns, density=density, weight=weight, $
					energy=energy, theta=theta, phi=phi, beam=beam, $
					alpha=(*p).target.alpha, beta=(*p).target.beta, unknown=(*p).unknown, $
					z1=(*p).beam.z1, a1=(*p).beam.a1, state=charge_state, $
					e_min=(*p).emin, e_max=(*p).emax, progress=0, error=error, $
					intensity=intensity, lines=lines, n_lines=n_lines, z2=z2, shell=shell, $
					ratio_intensity=ratio_intensity, $
					layers=layers, sec_yield=sec_yield, select=select, mu_zero=mu_zero, $
					gamma=(*pstate).gamma, e_lines=e_lines, $
					detector=(*p).pdetector, layout=(*p).playout, ratio_yield=ratio_yield, array=array, $
					x_slow=x_slow, e_slow=e_slow, xsect=xsect, yieldx=yieldx, $
					cmux=cmux, cos_beam=cos_beam, dydx=dydx, half_slice_id=hlid, flux=flux )

				if error then goto, finish

				peaks2 = make_peaks( z2=z2, shell=shell, n_lines=n_lines, lines=lines, intensity=intensity, yield=yield, $
					layers=layers, unknown=(*p).unknown, e_beam=energy, theta=(*p).detector.theta, $
					phi=(*p).detector.phi, alpha=(*p).target.alpha, beta=(*p).target.beta, $
					title=(*p).title, formula=formula, thick=thick, microns=microns, $
					density=density, weight=weight, z1=(*p).beam.z1, a1=(*p).beam.a1, $
					state=charge_state, e_min=(*p).emin, e_max=(*p).emax, mu_zero=mu_zero, $
					e_lines=e_lines, ratio_yield=ratio_yield, array=array, detector_file=(*(*p).pdetector).file, $
					ratio_intensity=ratio_intensity, beam=beam )

				peaks = [peaks, peaks2]

				thick[q[1:*]] = thick[q[1:*]] + (*p).thick_step[q[1:*]]
			  endfor

			  thick[q[0]] = thick[q[0]] + (*p).thick_step[q[0]]
			endfor

		  endif
		  progress, /complete, progress_tlb, 'Yield calculation completed.'
		  progress, /ending, progress_tlb
		endif

		if lenchr((*p).output_file) lt 1 then begin
			path = *(*pstate).path
		endif else begin
			path = extract_path( (*p).output_file)
		endelse
		F = file_requester( /write, filter = '*.'+yname, $
					path=path, group=event.top, file=strip_path((*p).output_file), $
					title='Select the '+pname+' yield output file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.'+yname
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).output_file, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).output_file, set_value=F, set_text_select=[n-1,0]
			(*p).output_file = F

			print,'Write file ...'
			write_yield, peaks, (*p).output_file

			*(*pstate).pnew = (*p).output_file
			notify, 'new-yields', (*pstate).pnew, from=event.top
		endif

		(*pstate).pplot = ptr_new( {p1:(*p).peaks, p2:(*p).peaks2} )
		notify, 'layer-plot', (*pstate).pplot, from=event.top
		end

	'plot-button': begin
		layer_plot, group=event.top, tlb=tlb, path=*(*pstate).path, p1=(*p).peaks, p2=(*p).peaks2, gamma=(*pstate).gamma
		(*pstate).plot = tlb
		end

	'export-button': begin
		if lenchr((*p).output_file) lt 1 then begin
			path = *(*pstate).path
		endif else begin
			path = extract_path( (*p).output_file)
		endelse
		if ptr_valid((*p).peaks) ne 1 then goto, finish
		F = file_requester( /write, filter = '*.csv', path=path, group=event.top, $
					title='Export the yield results to an ASCII CSV file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.csv'
			*(*pstate).path = extract_path(F)
			peaks = (*p).peaks
			peaks2 = (*p).peaks2
			klm = ['K','K','L','M']

			el_names = element_name( (*peaks).z) + ' ' + klm[(*peaks).shell]
			titles = ['Relative Intensities','Yields per layer','Yields with depth']
			initial = [1,0]

			select = export_select( event.top, el_names, titles, initial=initial, $
					old_select=*(*pstate).pexport, path=*(*pstate).path)
			if select.error then goto, finish

			qmode = where( select.mode_enable eq 1)
			if qmode[0] eq -1 then goto, finish
			q = where(select.el_enable eq 1)
			if q[0] eq -1 then goto, finish
			*(*pstate).pexport = select
			sxrf = ( ((*peaks).Z1 eq 0) and ((*peaks).A1 eq 0))
			
			on_ioerror, bad_export_open
			openw,1, F
			on_ioerror, bad_export_io
			printf,1,(*peaks).title
			printf,1,'E beam, Theta, Phi, Alpha, Beta'
			printf,1,(*peaks).e_beam,',',(*peaks).theta,',',(*peaks).phi,',',(*peaks).alpha,',',(*peaks).beta
			printf,1,'Z1, A1, state, unknown'
			printf,1,(*peaks).z1,',',(*peaks).a1,',',(*peaks).state,',',(*peaks).unknown
			printf,1, ''
			if select.mode_enable[0] then begin			; rel int
				printf,1,'Relative Intensities ---------------------------------------------'
				printf,1,'i, Z, element, k, line, index, energy, intensity'
				for i=0L,n_elements(q)-1 do begin
					printf,1, q[i],',',(*peaks).z[q[i]],',',el_names[q[i]]
					for k=0L,(*peaks).n_lines[q[i]]-1 do begin
						printf,1, ',,,',k,',',strip_non_alphanumeric(line_id((*peaks).lines[k,q[i]])),',', $
							(*peaks).lines[k,q[i]],',',(*peaks).e[k,q[i]],',', $
							(*peaks).intensity[k,q[i]]
					endfor
				endfor
			endif
			if select.mode_enable[1] then begin			; yields
				printf,1,'Yields per layer -----------------------------------------------------------'
				printf,1,'layer, formula, thickness, units, density, i, element, shell, yield'
				for l=0L,(*peaks).n_layers-1 do begin
					printf,1, l+1, ',', (*peaks).formula[l],',',(*peaks).thick[l],',',((*peaks).microns[l] ? 'microns' : 'mg/cm2'),',',((*peaks).microns[l] ? str_tidy((*peaks).density[l]) : '')
					for i=0L,n_elements(q)-1 do begin
						printf,1, ',,,,,',q[i],',',el_names[q[i]],',',(*peaks).shell[q[i]],',', (*peaks).yield[q[i],l]
					endfor
				endfor
			endif
			if select.mode_enable[2] then begin			; with depth
				printf,1,'Yield variation with depth (by variable width slice) ----------------------------------------'
				printf,1,',Slice'
				printf,1,'Layer,'+strjoin((*peaks2).half_slice_id+1,',')
				printf,1,'Depth (mg/cm2),'+strjoin((*peaks2).x_slow,',')
				if sxrf then begin
					printf,1,'Flux,'+strjoin((*peaks2).flux,',')
				endif else begin
					printf,1,'Energy (MeV),'+strjoin((*peaks2).e_slow,',')
					printf,1,'dE/dx (MeV/mg/cm2),'+strjoin((*peaks2).dedx,',')
				endelse
				for i=0L,n_elements(q)-1 do begin
					printf,1,el_names[q[i]]+','+strjoin((*peaks2).yieldx[*,q[i]],',')
				endfor
			endif
			close, 1
			goto, finish
		endif
bad_export_open:
			warning,'layer_setup_export','error opening export CSV file: '+F
			goto, finish
bad_export_io:
			warning,'layer_setup_export','I/O error writing export CSV file: '+F
			goto, finish
		end

	'close-button': begin
		print,'Close layer setup ...'
		goto, kill
		end

	else:
endcase

finish:
	widget_control, hourglass=0
	return

bad_state:
	warning,'layer_setup_event',['STATE variable has become ill-defined.','Abort Layer Setup.'],/error
	goto, kill
bad_ptr:
	warning,'layer_setup_event',['Parameter structure variable has become ill-defined.','Abort Layer Setup.'],/error
	goto, kill

kill:
	layer_update_pars, pstate
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;------------------------------------------------------------------------------------------

pro layer_update_pars, pstate

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
		warning,'Layer_update_pars',['IDL run-time error caught.', '', $
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

	widget_control, (*pstate).title_text, get_value=F
	(*p).title = F[0]
	widget_control, (*pstate).lcm_file, get_value=F
	(*p).lcm_file = F[0]
	widget_control, (*pstate).output_file, get_value=F
	(*p).output_file = F[0]
	widget_control, (*pstate).energy_text, get_value=s
	(*p).beam.energy = float(s[0])
	if (*p).beam.mode eq 4 then begin
		widget_control, (*pstate).z1_text, get_value=s
		(*p).beam.z1 = fix(s[0])
		widget_control, (*pstate).a1_text, get_value=s
		(*p).beam.a1 = fix(s[0])
		widget_control, (*pstate).state_text, get_value=s
		(*p).beam.state = fix(s[0])
	endif else begin
		widget_control, (*pstate).z1_text, set_value=str_tidy((*p).beam.z1)
		widget_control, (*pstate).a1_text, set_value=str_tidy((*p).beam.a1)
		widget_control, (*pstate).state_text, set_value=str_tidy((*p).beam.state)
	endelse
	widget_control, (*pstate).emin_text, get_value=s
;	(*p).emin = float(s[0]) > (((*pstate).gamma eq 1) ? 50.0 : 0.5)			; also in pixe_fit, strip_clip
	(*p).emin = float(s[0]) > (((*pstate).gamma eq 1) ? 50.0 : 0.2)
	widget_control, (*pstate).emin_text, set_value=str_tidy((*p).emin)
	widget_control, (*pstate).emax_text, get_value=s
	(*p).emax = float(s[0]) > (*p).emin
	widget_control, (*pstate).emax_text, set_value=str_tidy((*p).emax)
	widget_control, (*pstate).theta_text, get_value=s
	(*p).detector.theta = float(s[0])
	widget_control, (*pstate).phi_text, get_value=s
	(*p).detector.phi = float(s[0])
	widget_control, (*pstate).alpha_text, get_value=s
	(*p).target.alpha = float(s[0])
	widget_control, (*pstate).beta_text, get_value=s
	(*p).target.beta = float(s[0])

	for i=0L,(*p).n_layers-1 do begin
		widget_control, (*pstate).thick_text[i], get_value=s
		(*p).layer[i].thick = float(s[0])
		widget_control, (*pstate).density_text[i], get_value=s
		(*p).layer[i].density = float(s[0])
		if (*p).layer[i].formula_mode eq 1 then begin
			s = ''
			widget_control, (*pstate).oxide_table1[i], get_value=v
			(*p).layer[i].oxides[0:5] = v
			for k=0L,5 do begin
				if v[k] gt 0.0001 then s = s+'('+(*pstate).oxides1[k]+')'+str_tidy(v[k])
			endfor
			widget_control, (*pstate).oxide_table2[i], get_value=v
			(*p).layer[i].oxides[6:11] = v
			for k=0L,5 do begin
				if v[k] gt 0.0001 then s = s+'('+(*pstate).oxides2[k]+')'+str_tidy(v[k])
			endfor
			(*p).layer[i].formula = strcompress(s,/remove_all)
			widget_control, (*pstate).formula_text[i], set_value=(*p).layer[i].formula
		endif else begin
			widget_control, (*pstate).formula_text[i], get_value=s
			t = ''
			for k=0L,n_elements(s)-1 do t = t+s[k]
			(*p).layer[i].formula = strcompress(t,/remove_all)
		endelse
		widget_control, (*pstate).thick_min[i], get_value=s
		(*p).thick_min[i] = float(s[0])
		widget_control, (*pstate).thick_max[i], get_value=s
		(*p).thick_max[i] = float(s[0])
		widget_control, (*pstate).thick_step[i], get_value=s
		(*p).thick_step[i] = float(s[0])

		z = atomic_number((*p).layer[i].formula)
		if (z gt 0) and ((*p).layer[i].density lt 0.001) then begin
			(*p).layer[i].density = density(z)
			widget_control, (*pstate).density_text[i], set_value=str_tidy((*p).layer[i].density)
		endif
	endfor

	return
end

;------------------------------------------------------------------------------------------------------

pro load_lcm_parameters, pstate, file

;	Load the parameters into '(*pstate).p' from 'file' (.lcm)
;	Changes here may also effect 'file_lcm_preview'

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
		warning,'Load_LCM_parameters',['IDL run-time error caught.', '', $
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

	on_ioerror, bad_file
	close, 2
	openr, 2, file, /xdr

	on_ioerror, bad_io
	s = ''
	readu,2, s
	version = 0
	gamma = 0
	if inumeric(s) then begin
		version = fix(s)
		if version le -2 then begin
			readu,2, gamma
			mess = 'Attempt to change from '+ (((*pstate).gamma eq 1)?'PIGE to PIXE/SXRF':'PIXE/SXRF to PIGE') + ' mode.'
			if (*pstate).gamma ne gamma then begin
				warning,'load_lcm_parameters',[mess,'Abort loading parameters.']
				goto, done
			endif
		endif
		if version lt 0 then begin
			readu,2, s
		endif else version = 0
	endif

	valid = [0,-1,-2,-3,-4,-5]
	q = where( version eq valid)
	if q[0] eq -1 then begin
		warning,'load_lcm_parameters',['Error in LCM file.','Bad version number.']
		goto, done
	endif

	(*p).output_file = s
	widget_control, (*pstate).output_file, set_value=s
	readu,2, s
	(*p).title = s
	widget_control, (*pstate).title_text, set_value=s

	beam = {beam, mode:0, z1:0, a1:0, state:0.0, state_factor:0.0, e_factor:0.0, energy:0.0}
	readu,2, beam
	(*p).beam = beam
	
	on = ((*p).beam.mode lt 5) and ((*pstate).gamma eq 0)
	widget_control, (*pstate).beam_mode, set_combobox_select=beam.mode
	widget_control, (*pstate).z1_text, set_value=str_tidy(beam.z1),sensitive=on
	widget_control, (*pstate).a1_text, set_value=str_tidy(beam.a1),sensitive=on
	widget_control, (*pstate).state_text, set_value=str_tidy(beam.state),sensitive=on
	widget_control, (*pstate).energy_text, set_value=str_tidy(beam.energy)

	source = define(/source)
	(*(*p).source) = source
	if (*p).beam.mode eq 6 then begin
		source_file = ''
		readu,2, source_file
		F = file_requester(/read, filter = '*.source', file=source_file, updir=3, /skip_if_exists, $
					path=*(*pstate).path, title='Select the X-ray source parameter SOURCE file', /fix_filter, $
					preview_routine='file_source_preview')		; , group=(*pstate).lcm_file
		if F[0] ne '' then begin
			src = read_source( F[0], error=err) 
			if err eq 0 then (*(*p).source) = src
		endif
	endif else if (*p).beam.mode eq 7 then begin
		source_file = ''
		readu,2, source_file
		F = file_requester(/read, filter = '*.pink', file=source_file, updir=3, /skip_if_exists, $
			path=*(*pstate).path, title='Select the X-ray source parameter PINK beam file', /fix_filter, $
					preview_routine='file_source_preview')		; , group=(*pstate).lcm_file
		if F[0] ne '' then begin
			src = read_pink( F[0], error=err)
			if err eq 0 then (*(*p).source) = src
		endif
	endif
	
	mono = ((*p).beam.mode le 5)
	set_widget_text, (*pstate).source_text, (*(*p).source).file
	widget_control, (*pstate).beam_mapbase_ZA, map=mono
	widget_control, (*pstate).beam_mapbase_mono, map=mono
	widget_control, (*pstate).beam_mapbase_continuum, map=1-mono

	detector = {detector2, theta:0.0, phi:0.0}
	readu,2, detector
	(*p).detector = detector
	widget_control, (*pstate).theta_text, set_value=str_tidy(detector.theta)
	widget_control, (*pstate).phi_text, set_value=str_tidy(detector.phi)

	target = {target, alpha:0.0, beta:0.0}
	readu,2, target
	(*p).target = target
	widget_control, (*pstate).alpha_text, set_value=str_tidy(target.alpha)
	widget_control, (*pstate).beta_text, set_value=str_tidy(target.beta)

	emin = 0.0
	emax = 0.0
	readu,2, emin, emax
	(*p).emin = emin
	(*p).emax = emax
	widget_control, (*pstate).emin_text, set_value=str_tidy(emin)
	widget_control, (*pstate).emax_text, set_value=str_tidy(emax)

	array = 0
	dfile = ''
	if version le -3 then begin
		readu,2, array
		if array then readu,2, dfile
	endif
	(*p).array = array
	widget_control, (*pstate).array_option, set_value=(*p).array
	widget_control, (*pstate).detector_base, map=(*p).array
	if (*p).array then begin
		detector_update, /array, list=list, title=title, present=dfile, new=i, file=f
		*(*p).detector_list = list
		widget_control, (*pstate).detector_mode, set_value=title
		if i eq -1 then begin
			warning,'layer_setup','restored detector file not found:'+dfile
		endif else begin
			(*p).detector_mode = i
			widget_control, (*pstate).detector_mode, set_combobox_select=i
			detector = read_detector( f, error=error)
			if error then begin
				warning, 'layer_setup','Error reading Detectors file '+(*(*p).detector_list)[(*p).detector_mode], /error
			endif else begin
				*(*p).pdetector = *detector

				if (*(*p).pdetector).array and (strlen((*(*p).pdetector).layout) gt 0) then begin
					d = read_detector_layout((*(*p).pdetector).layout, error=error)
					if error eq 0 then *(*p).playout = d
				endif
			endelse
		endelse
	endif

	unknown = 0L
	n = 0L
	readu,2, unknown, n
	(*p).unknown = unknown
	(*p).n_layers = n
	widget_control, (*pstate).layer_number, set_combobox_select=n-1
	widget_control, (*pstate).unknown_number, set_combobox_select=unknown-1
	widget_control, (*pstate).unknown_base, map=((*p).n_layers gt 1)

	if n-1 lt (*pstate).active then begin
		widget_control, (*pstate).layer_base[(*pstate).active], map=0
		(*pstate).active = n-1
		widget_control, (*pstate).layer_base[(*pstate).active], map=1
		widget_control, (*pstate).define_layer, set_combobox_select=(*pstate).active
	endif

	if n gt 0 then begin
		layer = replicate( {layer2, thick:0.0, microns:0, density:0.0, formula_mode:0, weight:0, $
							formula:'', oxides:fltarr(16) }, n)
		many = intarr(n)
		tmin = fltarr(n)
		tmax = fltarr(n)
		tstep = fltarr(n)

		readu,2, layer
		if version le -4 then begin
			readu,2, many
			readu,2, tmin
			readu,2, tmax
			readu,2, tstep
		endif

		n = n < (*pstate).n_layers_max
		(*p).layer[0:n-1] = layer[0:n-1]
		(*p).many[0:n-1] = many
		(*p).thick_min[0:n-1] = tmin
		(*p).thick_max[0:n-1] = tmax
		(*p).thick_step[0:n-1] = tstep

		for i=0L,(*p).n_layers-1 do begin
			widget_control, (*pstate).thick_text[i], set_value=str_tidy((*p).layer[i].thick)
			widget_control, (*pstate).density_text[i], set_value=str_tidy((*p).layer[i].density)

			widget_control, (*pstate).thick_many[i], set_combobox_select=(*p).many[i]
			widget_control, (*pstate).thick_min[i], set_value=str_tidy((*p).thick_min[i])
			widget_control, (*pstate).thick_max[i], set_value=str_tidy((*p).thick_max[i])
			widget_control, (*pstate).thick_step[i], set_value=str_tidy((*p).thick_step[i])
			if (*p).many[i] eq 0 then begin
				widget_control, (*pstate).many_base[i], map=0, scr_ysize=1
			endif else begin
				widget_control, (*pstate).many_base[i], map=1, scr_ysize=(*pstate).many_base_y[i]
			endelse

			widget_control, (*pstate).thick_mode[i], set_combobox_select=(*p).layer[i].microns
			widget_control, (*pstate).formula_mode[i], set_combobox_select=(*p).layer[i].formula_mode
			widget_control, (*pstate).weight_mode[i], set_combobox_select=(*p).layer[i].weight

;			widget_control, (*pstate).density_base[i], map=(*p).layer[i].microns
			widget_control, (*pstate).formula_map[i], map=1-(*p).layer[i].formula_mode
			widget_control, (*pstate).oxide_map[i], map=(*p).layer[i].formula_mode

			if (*p).layer[i].formula_mode eq 1 then begin
				widget_control, (*pstate).oxide_table1[i], set_value=reform((*p).layer[i].oxides[0:5],1,6)
				s = ''
				for k=0L,5 do begin
					if (*p).layer[i].oxides[k] gt 0.0001 then begin
						s = s+'('+(*pstate).oxides1[k]+')'+str_tidy((*p).layer[i].oxides[k])
					endif
				endfor
				widget_control, (*pstate).oxide_table2[i], set_value=reform((*p).layer[i].oxides[6:11],1,6)
				for k=6,11 do begin
					if (*p).layer[i].oxides[k] gt 0.0001 then begin
						s = s+'('+(*pstate).oxides2[k-5]+')'+str_tidy((*p).layer[i].oxides[k])
					endif
				endfor
				(*p).layer[i].formula = strcompress(s,/remove_all)
				widget_control, (*pstate).formula_text[i], set_value=(*p).layer[i].formula
			endif else begin
				widget_control, (*pstate).formula_text[i], set_value=(*p).layer[i].formula
			endelse
		endfor
	endif

	close, 2
	return

bad:
	warning,'load_lcm_parameters','bad input parameters'
	goto, done
bad_io:
	warning,'load_lcm_parameters','error reading LCM file'
	goto, done
bad_state:
	warning,'load_lcm_parameters','bad state structure'
	goto, done
bad_file:
	warning,'load_lcm_parameters',['error opening file:', file]
	goto, done
bad_ptr:
	warning,'load_lcm_parameters','bad parameter pointer or structure'
	goto, done
done:
	close,2
	return
	end

;------------------------------------------------------------------------------------------

pro save_lcm_parameters, pstate, file

;	Save the parameters in '(*pstate).p' to 'file' (.lcm)

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
		warning,'Save_LCM_parameters',['IDL run-time error caught.', '', $
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

	version = -5

	on_ioerror, bad_file
	close, 1
	openw, 1, file, /xdr

	on_ioerror, bad_io
	writeu,1, string(version)
	writeu,1, (*pstate).gamma
	writeu,1, (*p).output_file
	writeu,1, (*p).title
	writeu,1, (*p).beam
	if ((*p).beam.mode eq 6) or ((*p).beam.mode eq 7) then writeu,1, (*(*p).source).file
	writeu,1, (*p).detector
	writeu,1, (*p).target
	writeu,1, (*p).emin, (*p).emax
	writeu,1, (*p).array
	if (*p).array then writeu,1, (*(*p).detector_list)[(*p).detector_Mode]

	writeu,1, long((*p).unknown), long((*p).n_layers)
	if (*p).n_layers gt 0 then begin
		writeu,1, (*p).layer[0:(*p).n_layers-1]

		writeu,1, (*p).many[0:(*p).n_layers-1]
		writeu,1, (*p).thick_min[0:(*p).n_layers-1]
		writeu,1, (*p).thick_max[0:(*p).n_layers-1]
		writeu,1, (*p).thick_step[0:(*p).n_layers-1]
	endif

	close, 1
	return

bad:
	warning,'save_lcm_parameters','bad input parameters'
	goto, done
bad_io:
	warning,'save_lcm_parameters','error writing LCM file'
	goto, done
bad_state:
	warning,'save_lcm_parameters','bad state structure'
	goto, done
bad_file:
	warning,'save_lcm_parameters',['error opening file:', file]
	goto, done
bad_ptr:
	warning,'save_lcm_parameters','bad parameter pointer or structure'
	goto, done
done:
	close,1
	return
	end

;------------------------------------------------------------------------------------------

pro OnRealize_Layer_beam_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).beam.mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_layer_detector_mode, wWidget

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	n = n_elements(*(*(*pstate).p).detector_list)
	if (n eq 1) and ( (*(*(*pstate).p).detector_list)[0] eq '') then goto, done
	if n lt 1 then goto, done
	(*(*pstate).p).detector_mode = (*(*pstate).p).detector_mode < (n-1)
	detector_update, /array, present=(*(*(*pstate).p).detector_list)[(*(*pstate).p).detector_mode], new=i, file=f, count=count
	widget_control, wWidget, set_combobox_select=i
	detector = read_detector( f, error=error)
	if error then begin
		warning, 'layer_setup','Realize error in Detectors file: '+(*(*(*pstate).p).detector_list)[(*(*pstate).p).detector_mode], /error
	endif else begin
		*(*(*pstate).p).pdetector = *detector

		if (*(*(*pstate).p).pdetector).array and (strlen((*(*(*pstate).p).pdetector).layout) gt 0) then begin
			d = read_detector_layout((*(*(*pstate).p).pdetector).layout, error=error)
			if error eq 0 then *(*(*pstate).p).playout = d
		endif
	endelse
endif

done:
end

;------------------------------------------------------------------------------------------

pro OnRealize_Layer_layer_number, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).n_layers-1
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_Layer_unknown_number, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).unknown-1
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_Layer_thick_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).layer[n].microns
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_Layer_thick_many, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).many[n]
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_Layer_thick_min, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)

geo = widget_info( (*pstate).many_base[n], /geometry)
(*pstate).many_base_y[n] = geo.ysize

if ptr_valid( (*pstate).p) then begin
	if (*(*pstate).p).many[n] eq 0 then begin
		widget_control, (*pstate).many_base[n], map=0, scr_ysize=1
	endif
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_Layer_formula_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).layer[n].formula_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_Layer_weight_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = (fix(s[0]) > 0) < ((*pstate).n_layers_max-1)

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).layer[n].weight
endif
end

;------------------------------------------------------------------------------------------

pro layer_setup, group_leader=group, TLB=tlb, pars=p, path=path, gamma=gamma, $
				_extra=extra, xoffset=xoffset, yoffset=yoffset, nosav=nosav

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_geopixe_vm, geopixe_enable_vm
if n_elements(geopixe_enable_vm) lt 1 then geopixe_enable_vm=1
startupp

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
		warning,'Layer_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(gamma) lt 1 then gamma=0L
if n_elements(path) lt 1 then path=''
if n_elements(nosav) eq 0 then nosav=0

  case !version.os_family of
	'MacOS': begin
		symbol = 'SYMBOL*12'
		large_font = 'Arial*12'
;@2		widget_control, default_font='Geneva*10'		; set font for all windows
		help_xsize = 88
		mode_xsize = 68
		spc_layers = 1
		space2 = 2
		xsize_nlayers = 65
		xsize_deflayer= 65
		xsize_unknown = 65
		xsize_oxide = 170
		xsize_atomic = 130
		col_widths = 110
		xw = 398
		yh = 625
		end
	'unix': begin
		symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
		large_font = '10x20'
;@2		widget_control, default_font='6x13'				; set font for all windows
		help_xsize = 42
		mode_xsize = 50
		spc_layers = 1
		space2 = 0
		xsize_nlayers = 60
		xsize_deflayer= 60
		xsize_unknown = 60
		xsize_oxide = 170
		xsize_atomic = 135
		col_widths = 88
		xw = 430
		yh = 714
		end
	else: begin
		symbol = 'SYMBOL*BOLD*14'
		large_font = 'COURIER*BOLD*10'
;@2		widget_control, default_font='Arial*14'				; set font for all windows
		help_xsize = 42
		mode_xsize = 50
		spc_layers = 1
		space2 = 2
		xsize_nlayers = 55
		xsize_deflayer= 55
		xsize_unknown = 55
		xsize_oxide = 162
		xsize_atomic = 120
		col_widths = 88
		xw = 405
		yh = 625
 		end
  endcase

; For the 'beam-Mode' list, an additional source mode may be added as a plugin later below.
; Only one plugin is added to droplst at the moment, as mode=6.

if gamma eq 1 then begin
	pname = 'PIGE'
	yname = 'yieldg'
	xname = 'gamma-ray'
	emin_help='Enter the minimum gamma-ray energy (keV) to calculate. All gamma-ray lines will be calculated that fall between Emin and Emax.'
	emax_help='Enter the maximum gamma-ray energy (keV) to calculate. All gamma-ray lines will be calculated that fall between Emin and Emax.'
	beam_modes = ['  Proton            +1','  Molecular H2  +1']
endif else begin
	pname = 'PIXE/SXRF'
	yname = 'yield'
	xname = 'X-ray'
	emin_help='Enter the minimum X-ray energy (keV) to calculate. All X-ray lines, for all K, L and M shells will be calculated that fall between Emin and Emax.'
	emax_help='Enter the maximum X-ray energy (keV) to calculate. All X-ray lines, for all K, L and M shells will be calculated that fall between Emin and Emax.'
	beam_modes = ['  Proton            +1','  Molecular H2  +1','  Helium            +1','  Alpha             +2','  General ion','  Photons   (mono)']
endelse

beam_modes = [beam_modes, '  '+'Photons   (lab source)', '  '+'Photons   (pink beam)']

detector_update,  list=detector_list, title=detector_title, /array

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

p = bad_pars_struct( p, make_pars=make_p)

n_layers = 20										; maxmimum # layers

layer = {layer2, thick:		0.0, $					; layer thickness (mg/cm^2)
			microns:		0, $					; flags microns
			density:		0.0, $					; density
			formula_mode:	0, $					; formula, oxides, ... mode
			weight:			0, $					; weight% versus at.fraction
			formula:		'', $					; chemical formula
			oxides:			fltarr(16) } 			; oxide components

; also in 'detector_show'

if make_p then begin
	pars = {	$
			title:				'', $					; calculation title
			lcm_file:			'', $					; LCM file name
			output_file:		'', $					; output yield file name
			beam: {	beam, mode:	0, $					; beam particle mode
					z1:			1, $					; beam Z
					a1:			1, $					; beam A
					state:		1.0, $					; beam charge state
					state_factor:	1.0, $				; charge state fraction scaling (e.g. for molecular H2)
					e_factor:	1.0, $					; energy scaling (e.g. for molecular H2)
					energy:		3.0}, $					; beam energy (is getting superceded by 'pbeam' struct.
			source:				ptr_new(/alloc), $		; source beam struct (supercedes simple mono energy for continuum beams)
			detector: {detector2, theta: (gamma eq 1) ? 0.0 : 135.0, $		; detector angle
					phi:		0.0 }, $				; out of plane angle
			array: 				0, $					; flags an array detector
			detector_mode:		0, $					; detector droplist index
			detector_list:		ptr_new(/allocate_heap), $	; pointer to list of detector file names
			pdetector:			ptr_new(/allocate_heap), $	; pointer to detector struct (kept in fit-setup)
			playout:			ptr_new(/allocate_heap), $	; pointer to detector layout struct (kept in fit-setup)
			target: {target, alpha:	0.0, $				; target rotation angle (vertical axis)
							beta:	0.0 }, $			; target title (about centre horizontal)
			Emin:				(gamma eq 1) ? 50.0 : 2.0, $		; minimum X-ray energy
			Emax:				(gamma eq 1) ? 4000.0 : 48.0, $		; maximum X-ray energy

			n_layers:			1, $					; # layers
			unknown:			1, $					; unknown layer
			layer:				replicate(layer,n_layers), $ ; layer details

			many:				intarr(n_layers), $		; multi-thick mode (0: off, 1:on-linked, 2:on-independent)
			thick_min:			fltarr(n_layers), $		; array of multi-thicknesses minima for "many" thickness mode
			thick_max:			fltarr(n_layers), $		; array of multi-thicknesses maxima for "many" thickness mode
			thick_step:			fltarr(n_layers), $		; array of multi-thicknesses steps for "many" thickness mode

			peaks:		ptr_new(/allocate_heap), $		; pointer to peaks results struct
			peaks2:		ptr_new(/allocate_heap) $		; pointer to extra stuff for plotting
	}
	*p = pars
endif
*(*p).detector_list = detector_list

; 	top-level base

tlb = widget_base( /column, title=pname+' Yield Calculation', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, _extra=extra, uname='layer_TLB', xoffset=xoffset, yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=3, /base_align_center, /align_center)

; set-up file droplist and buttons

sbase = widget_base( tbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( sbase, value='Set-up:')
lcm_file = widget_text( sbase, value=(*p).lcm_file, uname='lcm-file', /tracking, /editable, $
					uvalue='Enter the name of an LCM file to retrieve yield calculation set-up details, or use the "Load" button.',scr_xsize=235)
load_setup_button = widget_button( sbase, value='Load', uname='load-setup-button', /tracking, $
					uvalue='Load set-up parameters from a previous LCM file.', scr_xsize=38)
save_setup_button = widget_button( sbase, value='Save', uname='save-setup-button', /tracking, $
					uvalue='Save set-up parameters to an LCM file.', scr_xsize=38)

titlebase = widget_base( tbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( titlebase, value='Title:')
title_text = widget_text( titlebase, value=(*p).title, uname='title-text', /tracking, /editable, $
					uvalue='Enter a title descriptor for this yield calculation.',scr_xsize=313)

rowbase = widget_base( tbase, /row, /base_align_top, ypad=0, xpad=0, space=5)
lbase = widget_base( rowbase, /column, /base_align_right, ypad=0, xpad=0, space=2)
rbase = widget_base( rowbase, /column, /base_align_right, ypad=0, xpad=0, space=2)

; Beam details

sense = ((*p).beam.mode lt 5) and (gamma eq 0)
mono = ((*p).beam.mode le 5)

l2base = widget_base( lbase, /column, /base_align_right, ypad=1, xpad=2, space=1, /frame, scr_xsize=250)
lab = widget_label( l2base, value='Beam Particle', /align_center)
beambase = widget_base( l2base, /row, /base_align_top, ypad=0, xpad=0, space=10)
beam_mode = widget_combobox( beambase, value=beam_modes, uname='beam-mode', xsize=138, /tracking, $
					notify_realize='OnRealize_Layer_beam_mode', $
					uvalue='Select the beam particle to use, "general" to select the beam by Z1, A1 and charge state, or "Photons" either using a monochromatic beam ' + $
					'or a model continuum including characteristic anode lines.')
beam_mapbase_ZA = widget_base( beambase, /row, /base_align_center, ypad=0, xpad=0, space=3, map=mono)
lab = widget_label( beam_mapbase_ZA, value='Z:')
z1_text = widget_text( beam_mapbase_ZA, value=str_tidy((*p).beam.z1), uname='z1-text', /tracking, /editable, $
					uvalue='Enter the beam particle atomic number Z1.', scr_xsize=26, sensitive=sense)
lab = widget_label( beam_mapbase_ZA, value='A:')
a1_text = widget_text( beam_mapbase_ZA, value=str_tidy((*p).beam.a1), uname='a1-text', /tracking, /editable, $
					uvalue='Enter the beam particle mass number.', scr_xsize=26, sensitive=sense)

beamBBbase = widget_base( l2base, ypad=0, xpad=0, /base_align_left, /align_left)
beam_mapbase_mono = widget_base( beamBBbase, /row, /base_align_center, ypad=0, xpad=10, space=10, map=mono, xoffset=0, yoffset=0)
lab = widget_label( beam_mapbase_mono, value='Energy:')
energy_text = widget_text( beam_mapbase_mono, value=str_tidy((*p).beam.energy), uname='energy-text', /tracking, /editable, $
					uvalue='Enter the beam energy (MeV for ions, keV for photons).', scr_xsize=70)
beam3base = widget_base( beam_mapbase_mono, /row, /base_align_center, ypad=0, xpad=0, space=3)
lab = widget_label( beam3base, value='Charge:')
state_text = widget_text( beam3base, value=str_tidy((*p).beam.state), uname='state-text', /tracking, /editable, $
					uvalue='Enter the beam particle charge state (use 1 for photons).', scr_xsize=45, sensitive=sense)

beam_mapbase_continuum = widget_base( beamBBbase, /row, /base_align_center, ypad=0, xpad=0, space=10, map=1-mono, xoffset=0, yoffset=0)
beam5base = widget_base( beam_mapbase_continuum, /row, /base_align_center, ypad=0, xpad=0, space=3)
lab = widget_label( beam5base, value='Source:')
file = ptr_good((*p).source) ? (*(*p).source).file : ''
source_text = widget_text( beam5base, value=file, uname='source-file-text', /tracking, /editable, scr_xsize=120, $
					uvalue='Enter the filename for the continuum source file or LOAD an existing one, or use NEW to open the source set-up window.')
new_source_button = widget_button( beam5base, value='New', uname='new-source-button', /tracking, $
					uvalue='Open source set-up window to model a continuum source. Remember to select "Save" on the Source Setup window to ' + $
					'copy new source modelling results back to this yield calculation window.', scr_xsize=30)
load_source_button = widget_button( beam5base, value='Load', uname='load-source-button', /tracking, $
					uvalue='Load source parameters and spectra from a previous SOURCE file.', scr_xsize=38)

; Detector parameters

l3base = widget_base( lbase, /column, /base_align_right, ypad=1, xpad=2, space=1, /frame, scr_xsize=250)
lab = widget_label( l3base, value='Detector', /align_center)
det1base = widget_base( l3base, /row, /base_align_center, ypad=0, xpad=0, space=3)
lab = widget_label( det1base, value='Theta:')
theta_text = widget_text( det1base, value=str_tidy((*p).detector.theta), uname='theta-text', /tracking, /editable, $
					uvalue='Enter the detector angle (degrees). Scattering angle relative to the beam direction at zero degrees (' + $
					'e.g. common 45 degree back-angle X-ray take-off angle is at theta=135).', scr_xsize=70)
lab = widget_label( det1base, value=' Phi:')
phi_text = widget_text( det1base, value=str_tidy((*p).detector.phi), uname='phi-text', /tracking, /editable, scr_xsize=70, $
					uvalue='Enter the azimuthal detector angle out of the plane about the Z axis (degrees, relative to zero in the horizontal plane).')
button = widget_button( det1base, value='?', uname='detector-show', /tracking, $
					uvalue='Show the 3D detector, beam and sample geometry in a pop-up XobjView window. The view can be rotated, zoomed, translated and saved from this window.')
det2base = widget_base( l3base, /row, /base_align_center, ypad=0, xpad=0, space=0)
array_option = cw_bgroup2( det2base, ['Array'], /column, xpad=0, ypad=0, space=0, $
					/return_index, /tracking, uname='array-option', set_value=(*p).array, /nonexclusive, $
					uvalue=['Enable the calculation of yields for a large detector array where theta phi will vary ' + $
						'across the face of the detector.'])
detector_base = widget_base( det2base, /row, /base_align_center, ypad=0, xpad=0, space=0, map=(*p).array)
detector_mode = widget_combobox( detector_base, value=detector_title, uname='detector-mode', xsize=179, /tracking, $
					notify_realize='OnRealize_layer_detector_mode', $
					uvalue='Select an available array detector parameter set. Unlike using single "generic" detectors, ' + $
					'for array detector the yield needs to be computed for each detector element, with varying theta, phi, across the array.')

; E min,max

r2base = widget_base( rbase, /column, /base_align_right, ypad=1, xpad=2, space=2, /frame, scr_xsize=125)
lab = widget_label( r2base, value='Energy Range', /align_center)
e1base = widget_base( r2base, /row, /base_align_center, ypad=0, xpad=0, space=3)
lab = widget_label( e1base, value='E  min:')
emin_text = widget_text( e1base, value=str_tidy((*p).emin), uname='emin-text', /tracking, /editable, $
					uvalue=emin_help, scr_xsize=70)
e2base = widget_base( r2base, /row, /base_align_center, ypad=0, xpad=0, space=3)
lab = widget_label( e2base, value='E max:')
emax_text = widget_text( e2base, value=str_tidy((*p).emax), uname='emax-text', /tracking, /editable, $
					uvalue=emax_help, scr_xsize=70)

; Target parameters

r3base = widget_base( rbase, /column, /base_align_right, ypad=1, xpad=2, space=3, /frame, scr_xsize=125)
lab = widget_label( r3base, value='Target', /align_center)
target1base = widget_base( r3base, /row, /base_align_center, ypad=0, xpad=0, space=3)
lab = widget_label( target1base, value='Alpha:')
alpha_text = widget_text( target1base, value=str_tidy((*p).target.alpha), uname='alpha-text', /tracking, /editable, $
					uvalue='Enter the target rotation (around a vertical axis through the target). Zero degrees is normal to the beam ' + $
					'(e.g. for detector at 90 degrees, alpha = -10 is towards detector).', scr_xsize=70)
target2base = widget_base( r3base, /row, /base_align_center, ypad=0, xpad=0, space=3)
lab = widget_label( target2base, value='Beta:')
beta_text = widget_text( target2base, value=str_tidy((*p).target.beta), uname='beta-text', /tracking, /editable, scr_xsize=70, $
					uvalue='Enter the target tilt angle (about a horizontal axis across the face of the target). Zero degrees is normal to the beam.')

; Layer details

laybase = widget_base( tbase, /column, /base_align_center, /align_center, ypad=1, xpad=2, space=spc_layers, /frame)
lab = widget_label( laybase, value='Target Layer Selection', /align_center)

lay2base = widget_base( laybase, /row, /base_align_center, ypad=0, xpad=0, space=space2, /align_center)
lab = widget_label( lay2base, value='# Layers:')
layer_number = widget_combobox( lay2base, value=str_tidy(indgen(n_layers)+1), uname='layer-number', /tracking, $
					notify_realize='OnRealize_Layer_layer_number', $
					uvalue='Select the number of layers in the target.', xsize=xsize_nlayers)

lab = widget_label( lay2base, value=' Define Layer:')
define_layer = widget_combobox( lay2base, value=str_tidy(indgen(n_layers)+1), uname='define-layer', /tracking, $
					uvalue='Select a layer by number, for editing. The parameters for the selected layer will ' + $
					'be displayed in the panel below for editing.', xsize=xsize_deflayer)

unknown_base = widget_base( lay2base, /row, map=((*p).n_layers gt 1), /base_align_center, ypad=0, xpad=0, space=space2)
lab = widget_label( unknown_base, value=' Unknown:')
unknown_number = widget_combobox( unknown_base, value=str_tidy(indgen(n_layers)+1), uname='unknown-number', /tracking, $
					notify_realize='OnRealize_Layer_unknown_number', $
					uvalue='Select the number of the unknown layer. i.e. The layer to be determined by analysis. ' + $
					'Any trace element not specified in the layer formulae will be assumed to reside in this layer.', xsize=xsize_unknown)

; The map base areas for setting up 'n_layers' layers ...

mapbull = widget_base( laybase, /align_center)
layer_base = lonarr(n_layers)
thick_text = lonarr(n_layers)
thick_mode = lonarr(n_layers)
many_base = lonarr(n_layers)
many_base_y = intarr(n_layers)
thick_many = lonarr(n_layers)
thick_min = lonarr(n_layers)
thick_max = lonarr(n_layers)
thick_step = lonarr(n_layers)
density_base = lonarr(n_layers)
density_text = lonarr(n_layers)
formula_mode = lonarr(n_layers)
weight_mode = lonarr(n_layers)
formula_map = lonarr(n_layers)
formula_text = lonarr(n_layers)
oxide_map = lonarr(n_layers)
oxide_table1 = lonarr(n_layers)
oxide_table2 = lonarr(n_layers)

oxides1 = ['Na2O','MgO','Al2O3','SiO2','K2O','CaO']
oxides2 = ['TiO2','Cr2O3','MnO','FeO','NiO','ZnO']

map = 1
for i=0L,n_layers-1 do begin
	layer_base[i] = widget_base( mapbull, /column, /frame, map=map, /base_align_center, /align_center, xpad=2, ypad=1, space=1)
	lab = widget_label( layer_base[i], value='Target Layer Details', /align_center)
	ll1base = widget_base( layer_base[i], /row, /base_align_center, /align_right, ypad=0, xpad=0, space=1)
	lab = widget_label( ll1base, value='Thick:')
	thick_many[i] = widget_combobox( ll1base, value=['One','+ 1D','+ 2D'], uname='thick-many', /tracking, $
					notify_realize='OnRealize_Layer_thick_many', $
					uvalue=str_tidy(i)+'  Select single or many-thickness mode. ' + $
					'For "+ 1D" or "+ 2D", new controls appear to select a series of thickness for generation of a series of yields as a function of thickness. ' + $
					'Layer thicknesses are linked for "+ 1D" and independent for "+ 2D" (i.e. 2D grid of thicknesses).', xsize=52)
	thick_text[i] = widget_text( ll1base, value=str_tidy((*p).layer[i].thick), uname='thick-text', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the thickness of the selected target layer (in either mg/cm^2 or microns).', scr_xsize=70)
	thick_mode[i] = widget_combobox( ll1base, value=[' mg/cm^2',' microns'], uname='thick-mode', /tracking, $
					notify_realize='OnRealize_Layer_thick_mode', $
					uvalue=str_tidy(i)+'  Select layer thickness in "mg/cm^2" or "microns". ' + $
					'For "microns", a new box allows entry of density of compound layers. ' + $
					'(Pure element densities come from database.)', xsize=75)

;	map_density = (*p).layer[i].microns
	map_density = 1
	density_base[i] = widget_base( ll1base, /row, map=map_density, /base_align_center, /align_right, ypad=0, xpad=0, space=3)
	lab = widget_label( density_base[i], value='   Density:')
	density_text[i] = widget_text( density_base[i], value=str_tidy((*p).layer[i].density), uname='layer-density', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the density of a compound composition layer (g/cm^3). '+ $
					'For pure element layers the density will be obtained from the database automatically ' + $
					'(Remember to hit <enter> after formula).', scr_xsize=70)

	map_many = (*p).many[i]
	many_base[i] = widget_base( layer_base[i], /row, /base_align_center, /align_right, ypad=0, xpad=0, space=1, map=(map_many ne 0))
	lab = widget_label( many_base[i], value='Thick Min:')
	thick_min[i] = widget_text( many_base[i], value=str_tidy((*p).thick_min[i]), uname='thick-min', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the minimum thickness of the selected target layer for thickness series mode.', $
					notify_realize='OnRealize_Layer_thick_min', scr_xsize=70)
	lab = widget_label( many_base[i], value=' Thick Max:')
	thick_max[i] = widget_text( many_base[i], value=str_tidy((*p).thick_max[i]), uname='thick-max', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the maximum thickness of the selected target layer for thickness series mode.', scr_xsize=70)
	lab = widget_label( many_base[i], value=' Step:')
	thick_step[i] = widget_text( many_base[i], value=str_tidy((*p).thick_step[i]), uname='thick-step', /tracking, /editable, $
					uvalue=str_tidy(i)+'  Enter the thickness step of the selected target layer for thickness series mode.', scr_xsize=70)

	ll2base = widget_base( layer_base[i], /row, /base_align_center, ypad=0, xpad=0, space=space2)
	lab = widget_label( ll2base, value='Formula Mode:')
	formula_mode[i] = widget_combobox( ll2base, value=['Type in formula','Enter oxide components'], uname='formula-mode', /tracking, $
					notify_realize='OnRealize_Layer_formula_mode', $
					uvalue=str_tidy(i)+'  Select the entry mode for chemical composition of the current layer. ' + $
					'This switches the panel below between an area for entry of a chemical formula and ' + $
					'tables for entry of oxide proportions (in either wt% or atomic fraction).', xsize=xsize_oxide)
	weight_mode[i] = widget_combobox( ll2base, value=['Atomic Fraction','Weight %'], uname='weight-mode', /tracking, $
					notify_realize='OnRealize_Layer_weight_mode', $
					uvalue=str_tidy(i)+'  Select the type of multiplier factors in formulae. These appear after ' + $
					'a radical term contained in brackets, e.g. "(SiO2)12.3". Options include "atomic fraction" or "weight %".', xsize=xsize_atomic)

	laybull = widget_base( layer_base[i], /align_center)
	formula_map[i] = widget_base( laybull, map=1-(*p).layer[i].formula_mode, /row, /base_align_center, ypad=3, xpad=0, space=3)
	lab = widget_label( formula_map[i], value='Formula:')
	formula_text[i] = widget_text( formula_map[i], value=(*p).layer[i].formula, uname='layer-formula', /tracking, /editable, ysize=5, /wrap, $
					uvalue=str_tidy(i)+'  Enter the chemical formula for the layer. ' + $
					'Enclose radicals in brackets "( )", with optional multipliers in atomic fraction or weight %. ' + $
					'e.g. Components in wt%: "(SiO2)18.3(MgO)34.3"; atomic proportions: "FeAsS".', scr_xsize=315)

	oxide_map[i] = widget_base( laybull, map=(*p).layer[i].formula_mode, /row, /base_align_center, /align_center, ypad=0, xpad=0, space=5)
	label = (*p).layer[i].weight ? ['wt %'] : ['at. frac.']
	oxide_table1[i] = widget_table( oxide_map[i], value=reform((*p).layer[i].oxides[0:5],1,6), uname='oxide-table1', /tracking, /align_center, $
					/editable, /resizeable_columns, /scroll, xsize=1, ysize=6, y_scroll_size=3, x_scroll_size=1, column_labels=label, $
					row_labels=oxides1, column_widths=col_widths, $
					uvalue=str_tidy(i)+'  Enter chemical composition as oxide components, in weight % or atomic fraction. ' + $
					'To enter a value, click in a cell and hit "return". Then enter the value and hit "return" again.')
	oxide_table2[i] = widget_table( oxide_map[i], value=reform((*p).layer[i].oxides[6:11],1,6), uname='oxide-table2', /tracking, /align_center, $
					/editable, /resizeable_columns, /scroll, xsize=1, ysize=6, y_scroll_size=3, x_scroll_size=1, column_labels=label, $
					row_labels=oxides2, column_widths=col_widths, $
					uvalue=str_tidy(i)+'  Enter chemical composition as oxide components, in weight % or atomic fraction. ' + $
					'To enter a value, click in a cell and hit "return". Then enter the value and hit "return" again.')

	map = 0
endfor

; set-up the output file

obase = widget_base( tbase, /row, /base_align_center, ypad=1, xpad=0, space=5)
output_file_button = widget_button( obase, value='Output:', uname='output-file-button', /tracking, $
					uvalue='Select the name of the output '+yname+' file. You will be prompted to confirm this file name ' + $
					'(or enter a new one) after calculation of the '+pname+' yields.', scr_xsize=50)
output_file = widget_text( obase, value=(*p).output_file, uname='output-file', /tracking, /editable, $
					uvalue='Select the name of the output '+yname+' file. You will be prompted to confirm this file name ' + $
					'(or enter a new one) after calculation of the '+pname+' yields.',scr_xsize=310)

; Buttons

s1 = ', for K, L and M shells, including secondary fluorescence'
if gamma then s1 = ''

bbase = widget_base( tbase, /row, /base_align_center, ypad=0, space=15)
button = widget_button( bbase, value='Calculate Yields', uname='calculate-button', /tracking, $
					uvalue='Calculate '+pname+' yields, using the current parameters. ' + $
					'The calculation includes all '+xname+' lines, between Emin and Emax'+s1+'.')
lab = widget_label( bbase, value='  ')
button = widget_button( bbase, value='Plot Yields', uname='plot-button', /tracking, $
					uvalue='Plot the results of the last calculation.')
lab = widget_label( bbase, value='  ')
button = widget_button( bbase, value='Export', uname='export-button', /tracking, $
					uvalue='Export the yield results of the last calculation to a CSV file.')
lab = widget_label( bbase, value='  ')
button = widget_button( bbase, value=' Close ', uname='close-button', /tracking, $
					uvalue='Exit the '+pname+' layer yields popup window. The parameters will be recovered if you reopen this window later.')

;.................................................................................

help = widget_text( tbase, scr_xsize=380, ysize=4, /wrap, uname='help', /tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

state = {	$
		path:			ptr_new(path), $		; pointer to current path
		p:				p, $					; pointer to parameters
		pnew:			ptr_new(''), $			; pointer to pass new yield/detector file name along
		gamma:			gamma, $				; PIGE mode
		active:			0, $					; active layer for edit
		n_layers_max:	n_layers, $				; maximum number of layers
		title_text:		title_text, $			; ID of title text widget
		lcm_file:		lcm_file, $				; ID of LCM file text widget
		output_file:	output_file, $			; ID of output file text widget
		beam_mapbase_ZA: beam_mapbase_ZA, $		; ID of Z,A source map base
		beam_mapbase_mono: beam_mapbase_mono, $	; ID of mono source map base
		beam_mapbase_continuum: beam_mapbase_continuum, $	; ID of continuum source map base
		beam_mode:		beam_mode, $			; ID of beam-mode droplist
		source_text:	source_text, $			; ID of source file test wudget
		energy_text:	energy_text, $			; ID of beam energy text widget
		z1_text:		z1_text, $				; ID of beam Z1 text widget
		a1_text:		a1_text, $				; ID of beam A1 text widget
		state_text:		state_text, $			; ID of beam charge state text widget
		array_option:	array_option, $			; ID of array enable cw_bgroup2
		detector_base:	detector_base, $		; UD of detector map base
		detector_mode:	detector_mode, $		; ID of array detector droplist
		emin_text:		emin_text, $			; ID of energy E min text widget
		emax_text:		emax_text, $			; ID of energy E max text widget
		theta_text:		theta_text, $			; ID of detector theta text widget
		phi_text:		phi_text, $				; ID of detector phi text widget
		alpha_text:		alpha_text, $			; ID of target alpha text widget
		beta_text:		beta_text, $			; ID of target beta text widget
		layer_number:	layer_number, $			; ID of # layers droplist
		unknown_base:	unknown_base, $			; ID of unknown base for mapping
		unknown_number:	unknown_number, $		; ID of unknown layer droplist
		define_layer:	define_layer, $			; ID of layer define selection droplist
		oxides1:		oxides1, $				; list of oxides 1
		oxides2:		oxides2, $				; list of oxides 2

		layer_base:		layer_base, $			; ID array of map bases for layers
		thick_text:		thick_text, $			; ID array of layer thickness text widgets
		thick_mode:		thick_mode, $			; ID array of thick mode droplists
		many_base:		many_base, $			; ID array of thick many base
		many_base_y:	many_base_y, $			; Y size of thick many bases
		thick_many:		thick_many, $			; ID array of thick many droplists
		thick_min:		thick_min, $			; ID array of thick min widgets
		thick_max:		thick_max, $			; ID array of thick max widgets
		thick_step:		thick_step, $			; ID array of thick step widgets
		density_base:	density_base, $			; ID array of density base widgets for mapping
		density_text:	density_text, $			; ID array of density text widgets
		formula_mode:	formula_mode, $			; ID array of formula mode droplists
		weight_mode:	weight_mode, $			; ID array of weight mode droplists
		formula_map:	formula_map, $			; ID array of formula map bases
		formula_text:	formula_text, $			; ID array of formula text widgets
		oxide_map:		oxide_map, $			; ID array of oxide map bases
		oxide_table1:	oxide_table1, $			; ID array of oxide table widgets
		oxide_table2:	oxide_table2, $			; ID array of oxide table widgets

		pexport:		ptr_new(/allocate_heap), $		; pointer to export select
		psource:		ptr_new(/allocate_heap), $		; pointer to source struct for soure_setup
		ppink:			ptr_new(/allocate_heap), $		; pointer to pink struct for pink_setup

		plot:			0L, $					; ID of layer_plot TLB (if open)
		pplot:			ptr_new(), $			; pointer to pass new data to layer_plot

		xobj_tlb:		0L, $					; TLB of the XobjView 
		help:			help $					; ID of help text
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

if ptr_good((*p).source) then set_widget_text, source_text, (*(*p).source).file	; redisplay any file text in widgets right-justified
set_widget_text, lcm_file, (*p).lcm_file
set_widget_text, output_file, (*p).output_file

register_notify, tlb, ['path'], $				; new path
					from=group
register_notify, tlb, 'new-detectors'			; new detectors (global notify)

xmanager, 'layer_setup', tlb, /no_block
return
end
