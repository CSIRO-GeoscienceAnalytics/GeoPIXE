;
;	sim setup and execution.
;
pro sim_setup

sim_setup_geopixe
return
end

;-----------------------------------------------------------------------

pro sim_pixe_xrf

sim_setup_geopixe
return
end

;-----------------------------------------------------------------------

pro sim_setup_geopixe_event, event

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
		warning,'sim_setup',['IDL run-time error caught.', '', $
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

if (*pstate).gamma then begin
	pname = 'PIGE'
	yname = 'yieldg'
	sname = 'Step'
	xname = 'gamma-ray'
	dname = 'damg'
endif else begin
	pname = 'PIXE/SXRF'
	yname = 'yield'
	sname = 'Tail'
	xname = 'X-ray'
	dname = 'dam'
endelse

do_sim = 0

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
					print,'sim Setup: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'new-yields': begin
				if ptr_valid( event.pointer) then begin
					F = *event.pointer
					if F ne '' then begin
						(*p).yield_file = F
						yields = read_yield(F, error=error)
						if error then begin
							warning, 'sim_setup',['Error in transferred PIXE/SXRF/PIGE Yield file.', $
							'Recalculate yields and save them to a '+yname+' file.']
						endif else begin
							n = lenchr(F)
							k = lenchr(strip_path(F))
							widget_control, (*pstate).yield_file, set_value=F, set_text_select=[n-k,k]
							widget_control, (*pstate).yield_file, set_value=F, set_text_select=[n-1,0]
							if ptr_valid((*p).yields) then ptr_free, (*p).yields
							(*p).yields = yields
							do_sim = 1
							goto, cont
						endelse
					endif
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
				widget_control, (*pstate).help, set_value=' '
			endelse
		endif
		goto, finish
		end
	'WIDGET_TIMER': begin
	;	print,' got a timer event; update text reads ...'
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request sim_Setup ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'sim_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				x = event.x - 6 > 455
				n = (((event.y - 338)/ (*pstate).row_height) - 2) > 1
				y = (n + 2) * (*pstate).row_height
				widget_control, (*pstate).table, scr_xsize=x, scr_ysize=y
				end
			else:
		endcase
		end

	'sim-table': begin
;		case tag_names( event, /structure_name) of
;
;			'WIDGET_TABLE_CELL_SEL': begin
;				(*pstate).sel.left = event.sel_left
;				(*pstate).sel.top = event.sel_top
;				(*pstate).sel.right = event.sel_right
;				(*pstate).sel.bottom = event.sel_bottom
;				*(*pstate).pselect = (*pstate).sel
;
;				if (*pstate).sel.top ge 0 then begin
;					notify, 'results-select', (*pstate).pselect, from=event.top
;				endif
;				end
;
;			'WIDGET_TABLE_CH': begin
;				end
;			else:
;		endcase
		end

	'load-setup-button': begin
		file = find_file2( (*p).SIM_file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		F = file_requester( /read, filter = '*.SIM', $
				/must_exist, path=path, group=event.top, $
				title='Select the source SIM parameter file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.SIM'
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).SIM_file, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).SIM_file, set_value=F, set_text_select=[n-1,0]
			(*p).SIM_file = F
			load_SIM_parameters, pstate, F
			do_sim = 1
		endif
		end

	'save-setup-button': begin
		file = find_file2( (*p).SIM_file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		F = file_requester( /write, filter = '*.SIM', $
				path=path, group=event.top, $
				title='Save the sim setup parameters to a SIM file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.SIM'
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).SIM_file, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).SIM_file, set_value=F, set_text_select=[n-1,0]
			(*p).SIM_file = F
			save_SIM_parameters, pstate, F
		endif
		end

	'SIM-file': begin
		widget_control, event.id, get_value=F
		F = strip_file_ext(F[0]) + '.SIM'
		(*p).SIM_file = F
		*(*pstate).path = extract_path(F)
		n = lenchr(F)
		k = lenchr(strip_path(F))
		widget_control, (*pstate).SIM_file, set_value=F, set_text_select=[n-k,k]
		widget_control, (*pstate).SIM_file, set_value=F, set_text_select=[n-1,0]
		load_SIM_parameters, pstate, F
		do_sim = 1
		end

	'periodic-table': begin
		if (event.alt eq 0) and (event.select eq 1) then begin
			if ptr_valid( (*p).pz_sim) eq 0 then begin
				(*p).pz_sim = ptr_new( {element, Z:event.Z, Shell:event.Shell}, /no_copy)
			endif else begin
				q = where( (*(*p).pz_sim).Z eq event.Z)
				if q[0] ne -1 then begin
					(*(*p).pz_sim)[q].Shell = event.Shell						; new one
				endif else begin
					t = [ *(*p).pz_sim, {element, Z:event.Z, Shell:event.Shell} ]
					ptr_free, (*p).pz_sim
					(*p).pz_sim = ptr_new(t, /no_copy)							; append
				endelse
			endelse
		endif
		if event.select eq 0 then begin
			if ptr_valid( (*p).pz_sim) then begin
				q = where( (*(*p).pz_sim).Z eq event.z)
				if q[0] ne -1 then begin
					(*(*p).pz_sim)[q].Z = 0
					(*(*p).pz_sim)[q].shell = 0
				endif
			endif
		endif
		if ptr_valid( (*p).pz_sim) then begin
			t = *(*p).pz_sim
			ptr_free, (*p).pz_sim
			q = where( t.Shell ne 0)											; weed out Shell=0
			if q[0] ne -1 then begin
				t = t[q]
				(*p).pz_sim = ptr_new(t, /no_copy)
			endif
		endif

		if ptr_valid((*p).pz_sim) then begin
			do_sim = 1
			print,'sim Z:',	(*(*p).pz_sim).Z
			print,'sim S:',	(*(*p).pz_sim).Shell
		endif
		end

	'layer-number': begin
		(*p).layer = event.index
		do_sim = 1
		end

	'conc-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).conc = float(s)
		do_sim = 1
		end

	'filter-mode': begin
		(*p).filter_mode = event.index
		filter_update, present=(*(*p).filter_list)[event.index], new=i, file=f
		filter = read_filters( f, error=error)
		if error then begin
			warning, 'sim_setup','Error reading Filter file '+f, /error
			goto, finish
		endif else begin
			if ptr_valid((*p).filter) then ptr_free, (*p).filter
			(*p).filter = filter
			do_sim = 1
		endelse
		end

	'charge-mode': begin
		(*p).charge_mode = event.index
		do_sim = 1
		end

	'q-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).charge = float(s)
		do_sim = 1
		end

	'detector-mode': begin
		(*p).detector_mode = event.index
		detector_update, present=(*(*p).detector_list)[event.index], new=i, file=f
		detector = read_detector( f, error=error)
		if error then begin
			warning, 'sim_setup','Error reading Detectors file '+f, /error
			goto, finish
		endif else begin
			if ptr_valid((*p).detector) then ptr_free, (*p).detector
			(*p).detector = detector
			do_sim = 1
		endelse
		end

	'yield-mode': begin
		(*p).yield_mode = event.index
		do_sim = 1
		end

	'yield-new-button': begin
		layer_setup, group_leader=event.top, TLB=tlb, pars=(*pstate).player, path=*(*pstate).path, gamma=(*pstate).gamma

		register_notify, event.top, [ $
					'path', $						; new path
					'new-yields' $					; new yields
					], from=tlb
		end

	'yield-popup-button': begin
		file = find_file2( (*p).yield_file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		F = file_requester( /read, filter = '*.'+yname, $
				/must_exist, path=path, group=event.top, $
				title='Select '+pname+' Yield file', /fix_filter)
		if F ne '' then begin
			yields = read_yield(F, error=error)
			if error then begin
				warning, 'sim_setup','Error reading '+pname+' Yield file: '+F, /error
				goto, finish
			endif else begin
				n = lenchr(F)
				k = lenchr(strip_path(F))
				widget_control, (*pstate).yield_file, set_value=F, set_text_select=[n-k,k]
				widget_control, (*pstate).yield_file, set_value=F, set_text_select=[n-1,0]
				if ptr_valid((*p).yields) then ptr_free, (*p).yields
				(*p).yield_file = F
				(*p).yields = yields
				do_sim = 1
			endelse
		endif else goto, finish
		end

	'yield-file': begin
		widget_control, event.id, get_value=F
		F = F[0]
		(*p).yield_file = F
		if F ne '' then begin
			yields = read_yield(F, error=error)
			if error then begin
				warning, 'sim_setup','Error reading '+pname+' Yield file: '+F, /error
			endif else begin
				n = lenchr(F)
				k = lenchr(strip_path(F))
				widget_control, (*pstate).yield_file, set_value=F, set_text_select=[n-k,k]
				widget_control, (*pstate).yield_file, set_value=F, set_text_select=[n-1,0]
				if ptr_valid((*p).yields) then ptr_free, (*p).yields
				(*p).yields = yields
				do_sim = 1
			endelse
		endif else goto, finish
		end

	else:
endcase

cont:
	widget_control, (*pstate).conc_text, get_value=s
	s = s[0]
	(*p).conc = float(s)
	if (*p).conc le 0.0 then do_sim=0
	widget_control, (*pstate).q_text, get_value=s
	s = s[0]
	(*p).charge = float(s)
	if (*p).charge le 0.0 then do_sim=0

	widget_control, (*pstate).yield_file, get_value=F
	F = F[0]
	if F ne (*p).yield_file then begin
		(*p).yield_file = F
		yields = read_yield(F, error=error)
		if error then begin
			warning, 'sim_setup',['Error reading GeoPIXE Yield file: '+F,'Make sure path to yield file is correct.']
			if ptr_valid((*p).yields) then ptr_free, (*p).yields
			(*p).yields = ptr_new()
			do_sim = 0
			goto, finish
		endif else begin
			if ptr_valid((*p).yields) then ptr_free, (*p).yields
			(*p).yields = yields
		endelse
	endif

	if (do_sim) then begin			; do the sim

		OK = 0
		if ptr_valid((*p).pz_sim) then begin
			z = (*(*p).pz_sim).Z
			shell = (*(*p).pz_sim).Shell
			OK = 1
		endif

;		This routine now accepts compound shell settings:
;		If use_m=0: "shell=3" means to sim BOTH K and L lines
;		If use_m=1: "shell=4,5" means sim K+L or L+M.

		if ptr_valid( (*p).yields) eq 0 then goto, more
		if OK eq 0 then goto, more

		(*p).peaks = select_element_lines( (*p).yields, z, shell, el_free, $
							use_m=(*pstate).use_m, /no_extras)

		if ptr_valid( (*p).peaks) eq 0 then goto, more

		(*p).layer = min([(*p).layer,(*(*p).peaks).n_layers-1])
		widget_control, (*pstate).layer_number, set_combobox_select=(*p).layer

		if (*(*p).detector).pige eq 1 then begin
			omega = 4.0 * !pi * 1000.
		endif else begin
			omega = 1000.0 * !pi * (*(*p).detector).diameter * (*(*p).detector).diameter / $
						(4.0 * (*(*p).detector).distance * (*(*p).detector).distance)
		endelse

		Y = (*(*p).peaks).yield[*,(*p).layer]
		e = (*(*p).peaks).e[0,*]
		if n_elements( e) gt 1 then e = reform(e)

;		eff = det_eff( (*p).detector, e, external_filters=(*p).filter, gamma=(*(*p).detector).pige)
;		counts_per_ppm_uc = float(omega * eff * Y)

		total_eff = detector_efficiency( (*p).detector, (*(*p).detector).layout, e, $
					solid_angle=omega, effective=eff)

;	Here for now just use filter without regard for range if take-off angles. This
;	needs to change to be more like the code in pixe_fit.pro, and use the relative
;	eff, omega, filt, y sums to accumulate the product of omega, eff, T and Y for
;	each detector. (Also below for majors.)

		T = transmit( (*p).filter, e, gamma=(*(*p).detector).pige)
		counts_per_ppm_uc = float(omega * eff * T * Y)

		n = (*(*p).peaks).n_els
		tot_rel = fltarr(n)
		for i=0L,n-1 do begin
			e2 = (*(*p).peaks).e[0:(*(*p).peaks).n_lines[i],i]
			if n_elements( e2) gt 1 then e2 = reform(e2)
;			eff2 = det_eff( (*p).detector, e2)
;			Tr2 = transmit( (*p).filter, e2, gamma=(*(*p).detector).pige)
			eff2 = det_eff( (*p).detector, e2, external_filters=(*p).filter, gamma=(*(*p).detector).pige)
			Tr2 = 1.
			rel = (*(*p).peaks).intensity[0:(*(*p).peaks).n_lines[i],i] * eff2*Tr2
			tot_rel[i] = total(rel) / rel[0]
		endfor

		case (*p).charge_mode of
			0: begin
				charge = (*p).charge * 0.001			; charge per second
				end
			1: begin
				charge = (*p).charge					; charge total
				end
			2: begin
				charge = (*p).charge * 1.602e-13		; photons per second
				end
		endcase
		yield = counts_per_ppm_uc * charge
		count = (*p).conc * yield

		t1 = strarr(8,n)
		t1[0,*] = string( (*p).layer + 1)
		t1[1,*] = element_name((*(*p).peaks).z)
		t1[2,*] = string( (*(*p).peaks).shell)
		t1[3,*] = string( counts_per_ppm_uc)
		t1[4,*] = string( (*p).conc)
		t1[5,*] = string( count)
		t1[6,*] = string( tot_rel)
		t1[7,*] = string( count * tot_rel)
		(*pstate).rows = n

		OK = 0
;		for i=0L,(*(*p).yields).n_layers-1 do begin
		i = (*p).layer

			decode_formula, (*(*p).yields).formula[i], n2,z2,f2, weight=(*(*p).yields).weight[i]
			conc2 = 1.0e+6 * f2[0:n2-1] * mass(z2[0:n2-1]) / total( f2[0:n2-1] * mass(z2[0:n2-1]))

			for j=0L,n2-1 do begin
				if OK then begin
					q = where( (Z eq z2[j]) and (Z ne 0))
					if q[0] eq -1 then begin
						q = where( (*(*p).yields).z eq z2[j])
						if q[0] ne -1 then begin
							for k=0L,n_elements(q)-1 do begin
								z = [z,z2[j]]
								shell = [shell,(*(*p).yields).shell[q[k]]]
								conc = [conc,conc2[j]]
							endfor
						endif
					endif
				endif else begin
					q = where( ((*(*p).yields).z eq z2[j]) and ((*(*p).yields).z ne 0))
					if q[0] ne -1 then begin
						for k=0L,n_elements(q)-1 do begin
							if OK then begin
								z = [z,z2[j]]
								shell = [shell,(*(*p).yields).shell[q[k]]]
								conc = [conc,conc2[j]]
							endif else begin
								z = z2[j]
								shell = (*(*p).yields).shell[q[k]]
								conc = conc2[j]
								OK = 1
							endelse
						endfor
					endif
				endelse
			endfor
;		endfor
		if OK eq 0 then goto, show

		(*p).peaks = select_element_lines( (*p).yields, z, shell, el_free, $
							use_m=(*pstate).use_m, /no_extras)

		if ptr_valid( (*p).peaks) eq 0 then goto, more

		if (*(*p).detector).pige eq 1 then begin
			omega = 4.0 * !pi * 1000.
		endif else begin
			omega = 1000.0 * !pi * (*(*p).detector).diameter * (*(*p).detector).diameter / $
						(4.0 * (*(*p).detector).distance * (*(*p).detector).distance)
		endelse

		Y = (*(*p).peaks).yield[*,(*p).layer]

		e = (*(*p).peaks).e[0,*]
		if n_elements( e) gt 1 then e = reform(e)

;		eff = det_eff( (*p).detector, e, external_filters=(*p).filter, gamma=(*(*p).detector).pige)
;		counts_per_ppm_uc = float(omega * eff * Y)

		total_eff = detector_efficiency( (*p).detector, (*(*p).detector).layout, e, $
					solid_angle=omega, effective=eff)

;	Here for now just use filter without regard for range if take-off angles. This
;	needs to change to be more like the code in pixe_fit.pro, and use the relative
;	eff, omega, filt, y sums to accumulate the product of omega, eff, T and Y for
;	each detector. (Also above for traces.)

		T = transmit( (*p).filter, e, gamma=(*(*p).detector).pige)
		counts_per_ppm_uc = float(omega * eff * T * Y)

		n = (*(*p).peaks).n_els
		tot_rel = fltarr(n)
		for i=0L,n-1 do begin
			e2 = (*(*p).peaks).e[0:(*(*p).peaks).n_lines[i],i]
			if n_elements( e2) gt 1 then e2 = reform(e2)
;			eff2 = det_eff( (*p).detector, e2)
;			Tr2 = transmit( (*p).filter, e2, gamma=(*(*p).detector).pige)
			eff2 = det_eff( (*p).detector, e2, external_filters=(*p).filter, gamma=(*(*p).detector).pige)
			Tr2 = 1.
			rel = (*(*p).peaks).intensity[0:(*(*p).peaks).n_lines[i],i] * eff2*Tr2
			tot_rel[i] = total(rel) / rel[0]
		endfor

		case (*p).charge_mode of
			0: begin
				charge = (*p).charge * 0.001			; charge per second
				end
			1: begin
				charge = (*p).charge					; charge total
				end
			2: begin
				charge = (*p).charge * 1.602e-13		; photons per second
				end
		endcase
		yield = counts_per_ppm_uc * charge
		count = conc * yield							; using major element conc

		t2 = strarr(8,n)
		t2[0,*] = string( (*p).layer + 1)
		t2[1,*] = element_name((*(*p).peaks).z)
		t2[2,*] = string( (*(*p).peaks).shell)
		t2[3,*] = string( counts_per_ppm_uc)
		t2[4,*] = string( conc)
		t2[5,*] = string( count)
		t2[6,*] = string( tot_rel)
		t2[7,*] = string( count * tot_rel)

		t1 = [[t1],[t2]]
		(*pstate).rows = (*pstate).rows + n

show:
		widget_control, (*pstate).table, set_value = t1, $
			table_xsize=8, table_ysize=(*pstate).rows, align=2

more:
	endif

finish:
	widget_control, hourglass=0
	return

bad_state:
	warning,'sim_setup_event',['STATE variable has become ill-defined.','Abort sim Setup.'],/error
	goto, kill
bad_ptr:
	warning,'sim_setup_event',['Parameter structure variable has become ill-defined.','Abort sim Setup.'],/error
	goto, kill

kill:
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

pro load_SIM_parameters, pstate, file

;	Load the parameters into '(*pstate).p' from 'file' (.SIM)

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
		warning,'Load_SIM_parameters',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		close, 1
		close, 2
		return
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

	valid = [-1]

	on_ioerror, bad_io
	version = 0
	gamma = 0
	readu,2, version
	q = where( version eq valid)
	if q[0] eq -1 then goto, bad_version

	readu,2, gamma
	if gamma ne (*pstate).gamma then begin
		mess = ((*pstate).gamma eq 0) ? 'Attempt to change from PIXE/SXRF to PIGE mode.' : 'Attempt to change from PIGE to PIXE/SXRF mode.'
		warning,'load_SIM_parameters',['Error in SIM file.',mess,'Abort Load operation.']
		goto, done
	endif

	s1 = ''
	readu,2, s1
	s = strip_path(s1)
	if lenchr( (*(*p).filter_list)[0]) gt 0 then begin
		q = where( s eq *(*p).filter_list)
		if q[0] ne -1 then begin
			n = q[0]
		endif else begin
			*(*p).filter_list = [*(*p).filter_list,s]
			n = n_elements(*(*p).filter_list)-1
		endelse
	endif else begin
		*(*p).filter_list = [s]
	endelse
	widget_control, (*pstate).filter_mode, set_combobox_select=n
	(*p).filter_mode = n

	filter_update, present=s, new=i, file=f
	filter = read_filters( f, error=error)
	if error then begin
		warning, 'load_SIM_parameters',['Error in FILTERS file:',s], /error
		if ptr_valid((*p).filter) then ptr_free, (*p).filter
		(*p).filter = ptr_new()
	endif else begin
		if ptr_valid((*p).filter) then ptr_free, (*p).filter
		(*p).filter = filter
	endelse

	readu,2, s1
	s = strip_path(s1)
	if lenchr( (*(*p).detector_list)[0]) gt 0 then begin
		q = where( s eq *(*p).detector_list)
		if q[0] ne -1 then begin
			n = q[0]
		endif else begin
			*(*p).detector_list = [*(*p).detector_list,s]
			n = n_elements(*(*p).detector_list)-1
		endelse
	endif else begin
		*(*p).detector_list = [s]
	endelse
	widget_control, (*pstate).detector_mode, set_combobox_select=n
	(*p).detector_mode = n

	detector_update, present=s, new=i, file=f
	detector = read_detector( f, error=error)
	if error then begin
		warning, 'load_SIM_parameters',['Error in DETECTOR file:',s], /error
		if ptr_valid((*p).detector) then ptr_free, (*p).detector
		(*p).detector = ptr_new()
	endif else begin
		if ptr_valid((*p).detector) then ptr_free, (*p).detector
		(*p).detector = detector
	endelse

	readu,2, s
	(*p).yield_file = s
	yields = read_yield((*p).yield_file, error=error)
	if error then begin
		file = *(*pstate).path + strip_path( (*p).yield_file)
		yields = read_yield( file, error=error)
		if error eq 0 then (*p).yield_file = file
	endif
	if error then begin
		warning, 'load_SIM_parameters',['Error reading Yields file:',s, $
					'Check that the path to the file is correct.']
		if ptr_valid((*p).yields) then ptr_free, (*p).yields
		(*p).yields = ptr_new()
	endif else begin
		if ptr_valid((*p).yields) then ptr_free, (*p).yields
		(*p).yields = yields
	endelse
	n = lenchr((*p).yield_file)
	k = lenchr(strip_path((*p).yield_file))
	widget_control, (*pstate).yield_file, set_value=(*p).yield_file, set_text_select=[n-k,k]
	widget_control, (*pstate).yield_file, set_value=(*p).yield_file, set_text_select=[n-1,0]

	if ptr_valid( (*p).pz_sim) then begin
		q = where( (*(*p).pz_sim).z ne 0)
		if q[0] ne -1 then begin
			nq = n_elements(q)
			widget_control, (*pstate).ptable, set_value={Z:(*(*p).pz_sim)[q].z, STATE:replicate(0,nq), ALT:replicate(0,nq)}
		endif
		ptr_free, (*p).pz_sim
	endif

	n = 0L
	readu,2, n
	if n gt 0 then begin
		zs = replicate( {element, Z:0, Shell:0}, n)
		readu,2, zs
		widget_control, (*pstate).ptable, set_value={Z:zs.z, STATE:zs.shell, ALT:replicate(0,n)}
		(*p).pz_sim = ptr_new(zs, /no_copy)
	endif else begin
		(*p).pz_sim = ptr_new()
	endelse

	layer = 0
	conc = 0.0
	readu,2, layer, conc
	(*p).layer = layer
	widget_control, (*pstate).layer_number, set_combobox_select=layer
	(*p).conc = conc
	widget_control, (*pstate).conc_text, set_value=string(conc)

	qmode = 0
	q = 0.0
	readu,2, qmode, q
	(*p).charge_mode = qmode
	widget_control, (*pstate).charge_mode, set_combobox_select=qmode
	(*p).charge = q
	widget_control, (*pstate).q_text, set_value=string(q)

	close, 2
	return

bad:
	warning,'load_SIM_parameters','bad input parameters'
	goto, done
bad_version:
	warning,'load_SIM_parameters','bad version in SIM file'
	goto, done
bad_io:
	warning,'load_SIM_parameters','error reading SIM file'
	goto, done
bad_state:
	warning,'load_SIM_parameters','bad state structure'
	goto, done
bad_file:
	warning,'load_SIM_parameters',['error opening file:', file]
	goto, done
bad_ptr:
	warning,'load_SIM_parameters','bad parameter pointer or structure'
	goto, done
done:
	close,1
	return
	end

;------------------------------------------------------------------------------------------

pro save_SIM_parameters, pstate, file

;	Save the parameters in '(*pstate).p' to 'file' (.SIM)

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
		warning,'Save_SIM_parameters',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		close, 1
		return
	endif
endif

	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
	if n_elements(file) lt 1 then goto, bad
	p = (*pstate).p
	if ptr_valid(p) eq 0 then goto, bad_ptr
	if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr

	version = -1

	on_ioerror, bad_file
	close, 1
	openw, 1, file, /xdr

	on_ioerror, bad_io
	writeu,1, version
	writeu,1, (*pstate).gamma
	writeu,1, (*(*p).filter_list)[ (*p).filter_mode]
	writeu,1, (*(*p).detector_list)[ (*p).detector_mode]
	writeu,1, (*p).yield_file

	if ptr_valid( (*p).pz_sim) eq 0 then begin
		n = 0
	endif else begin
		n = n_elements( *(*p).pz_sim)
	endelse
	writeu,1, n
	if (n gt 0) then writeu,1, *(*p).pz_sim

	writeu,1, (*p).layer, (*p).conc
	writeu,1, (*p).charge_mode, (*p).charge

	close, 1
	return

bad:
	warning,'save_SIM_parameters','bad input parameters'
	goto, done
bad_io:
	warning,'save_SIM_parameters','error writing SIM file'
	goto, done
bad_state:
	warning,'save_SIM_parameters','bad state structure'
	goto, done
bad_file:
	warning,'save_SIM_parameters',['error opening file:', file]
	goto, done
bad_ptr:
	warning,'save_SIM_parameters','bad parameter pointer or structure'
	goto, done
done:
	close,1
	return
	end

;------------------------------------------------------------------------------------------

pro OnRealize_sim_charge_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).charge_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_sim_filter_mode, wWidget

COMPILE_OPT STRICTARR

common c_working_dir, geopixe_root

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).filter_mode
	filter_update, present=(*(*(*pstate).p).filter_list)[(*(*pstate).p).filter_mode], new=i, file=f
	filter = read_filters( f, error=error)
	if error then begin
		warning, 'sim_setup','Realize error in Filter file: '+f, /error
	endif else begin
		if ptr_valid((*(*pstate).p).filter) then ptr_free, (*(*pstate).p).filter
		(*(*pstate).p).filter = filter
	endelse
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_sim_detector_mode, wWidget

COMPILE_OPT STRICTARR

common c_working_dir, geopixe_root

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).detector_mode
	detector_update, present=(*(*(*pstate).p).detector_list)[(*(*pstate).p).detector_mode], new=i, file=f
	detector = read_detector( f, error=error)
	if error then begin
		warning, 'sim_setup','Realize error in Detectors file: '+f, /error
	endif else begin
		if ptr_valid((*(*pstate).p).detector) then ptr_free, (*(*pstate).p).detector
		(*(*pstate).p).detector = detector
	endelse
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_sim_layer_number, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).layer
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_sim_yield_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).yield_mode
endif
end

;-----------------------------------------------------------------

pro OnRealize_sim_Table, wWidget

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

w = widget_info( wWidget, /row_heights)
geom = widget_info( wWidget, /geometry)
tlb_geom = widget_info( top, /geometry)

(*pstate).table = wWidget
(*pstate).row_height = w[0]
(*pstate).rows = 6

;update_sim_table, pstate

done:
end

;------------------------------------------------------------------------------------------

pro sim_setup_geopixe, group_leader=group, TLB=tlb, pspec=pspec, xoffset=xoffset, yoffset=yoffset, $
		pars=p, layer_pars=player, presults=presults, path=path, _extra=extra, $
		nosav=nosav, gamma=gamma

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'sim_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(gamma) lt 1 then gamma=0
if n_elements(nosav) eq 0 then nosav=0
if n_elements(path) lt 1 then path=''
if (n_elements(pspec) lt 1) then pspec=ptr_new()
if ptr_valid(pspec) then begin
	charge = (*pspec).charge
endif else begin
	pspec = ptr_new()
	charge = 0.0
endelse

; Each wizard sav loads routines from GeoPIXE.sav, if GeoPIXE is not already running.
; The GeoPIXE routines are NOT to be compiled into each wizard sav file.
;
; First set a catch to test whether "GeoPIXE.sav" can be found ...

Catch, ErrorNo							; GeoPIXE.sav only loaded if needed
if (ErrorNo ne 0) then begin
	Catch, /cancel

;................................................................................................
; This code fragmwent will appear at the start of most stand-alone programs
; that need to load "GeoPIXE.sav" as a library. It needs to work when the current
; working directory is: (i) the "geopixe" runtime dir, i.e. for SAV files stored in the
; runtime dir along side GeoPIXE.sav, (ii) a subdir of "geopixe" runtime, such as the
; "maia", "daq", "wizards", e.g. for compiled Maia or Wizard SAV files, and (iii) a 
; project dir during debugging of this program in IDLDE. 
;
; The latter assumes that the runtime dir is named "geopixe" (see notes in "startupp.pro").

	found = 0
	file = 'GeoPIXE.sav'						; current dir is the runtime dir
	if file_test(file) eq 0 then begin
		file = '../GeoPIXE.sav'					; current dir in a subdir of runtime dir
		if file_test(file) eq 0 then begin
			file = '../geopixe/GeoPIXE.sav'		; current dir is another project dir
			if file_test(file) then found=1
		endif else found=1
	endif else found = 1

	if found then begin
		restore, file
		print,'"GeoPIXE.sav" restored.'
	endif else begin
		a = dialog_message(['GeoPIXE is not loaded in memory.','No "GeoPIXE.sav" file found locally.','Abort Wizard.','', $
				'Check that your working directory is the "geopixe" or "main" dir.'], /error)
		return
	endelse
endif
test_geopixe_loaded						; tests whether GeoPIXE.sav routines loaded
Catch, /cancel							; this is a new feature of GeoPIXE v7.0e onwards

;................................................................................................

if !version.os_family eq 'MacOS' then begin
	xw = 500
	yh = 325
	xsize_help = 500
endif else begin
	xw = 449
	yh = (gamma eq 1) ? 533 : 558
	xsize_help = 455
endelse

if gamma then begin
	pname = 'PIGE'
	yname = 'yieldg'
	sname = 'Step'
	xname = 'gamma-ray'
	dname = 'damg'
endif else begin
	pname = 'PIXE/SXRF'
	yname = 'yield'
	sname = 'Tail'
	xname = 'X-ray'
	dname = 'dam'
endelse

if n_elements(xoffset) lt 1 then begin
	screen = get_screen_size()
	xoffset = (screen[0]-34 - xw) > 0
endif
if n_elements(yoffset) lt 1 then begin
	screen = get_screen_size()
	yoffset = (screen[1]-28 - yh) > 0
endif

startupp, /colours									; setup IDL graphics

filter_update, list=filter_list, title=filter_title
detector_update, list=detector_list, title=detector_title

if n_elements(player) lt 1 then player = ptr_new(/allocate_heap)

p = bad_pars_struct( p, make_pars=make_p)

if make_p then begin
	pars = {	$
		sim_file:		'', $						; sim file name
		yields:			ptr_new(), $				; pointer to ALL yield struct
		peaks:			ptr_new(), $				; pointer to yields for only selected elements
		presults:		ptr_new(), $				; pointer to array of pointers to results
		yield_file:		'', $						; Yield file name
		yield_mode:		0, $						; current yield set
		pz_sim:			ptr_new(), $				; pointer to sim Z array
		layer:			0, $						; layer number
		filter:			ptr_new(), $				; pointer to selected filter(s)
		filter_list:	ptr_new(filter_list), $		; pointer to list of filter file names
		filter_mode:	0, $						; selected filter
		charge:			charge, $					; i (nA) / Q (uC)
		charge_mode:	0, $						; current / charge mode
		conc:			1000.0, $					; trace conc (ppm)
		detector:		ptr_new(), $				; pointer to detector calibration parameters
		detector_list:	ptr_new(detector_list), $	; pointer to list of detector file names
		detector_mode:	0 $							; selected detector calibration set
	}
	*p = pars
endif else begin
	if charge gt 0.00001 then (*p).charge = charge
endelse
*((*p).filter_list) = filter_list
*((*p).detector_list) = detector_list

presults = bad_pars_struct( presults, make_pars=no_results)

; 	top-level base

tlb = widget_base( /column, title=pname+' Sim', /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, _extra=extra, uname='sim_TLB', /base_align_center, /TLB_SIZE_EVENTS)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center)

; set-up file droplist and buttons

sbase = widget_base( tbase, /row, /base_align_center, ypad=2, xpad=0, space=5)
lab = widget_label( sbase, value='Set-up:')
sim_file = widget_text( sbase, value=(*p).sim_file, uname='sim-file', /tracking, /editable, $
					uvalue='Enter a SIM file-name for source set-up details.',scr_xsize=290)
load_setup_button = widget_button( sbase, value='Load', uname='load-setup-button', /tracking, $
					uvalue='Load set-up parameters from a previous SIM file.', scr_xsize=38)
save_setup_button = widget_button( sbase, value='Save', uname='save-setup-button', /tracking, $
					uvalue='Save set-up parameters to a SIM file.', scr_xsize=38)


; set-up periodic table

if ptr_valid( (*p).pz_sim) then begin
	zon = (*(*p).pz_sim).z
endif else begin
	zon = 0
	zstate = 0
endelse

t2base = widget_base( tbase, /column, xpad=3, ypad=3, space=8, /base_align_center, /frame)
if gamma then begin
	use_m = 0
	ptable = periodic_table( t2base, uname='periodic-table', n_states=2, n_alt_states=0, $
			z_on=zon, z_state=zstate, /no_tiny, /no_ree, $
			/start_Li, /right, legend=['','on'])
endif else begin
	use_m = 1
	ptable = periodic_table( t2base, uname='periodic-table', n_states=4, n_alt_states=0, $
			z_on=zon, z_state=zstate, /no_tiny, $
			/start_Na, /right, legend=['','K','L','M'])
endelse

; Layer number, conc select

lay_names = str_tidy(indgen(16)+1)
lbase = widget_base( t2base, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( lbase, value='Layer #:')
layer_number = widget_combobox( lbase, value=lay_names, uname='layer-number', /tracking, $
					notify_realize='OnRealize_sim_layer_number', $
					uvalue='Select the layer to calculate trace element counts/rates for.', xsize=65)
lab = widget_label( lbase, value='',scr_xsize=50)
lab = widget_label( lbase, value='Conc:')
conc_text = widget_text( lbase, value='1000.0', uname='conc-text', /tracking, /editable, $
					uvalue='Enter the concentration (ppm) for all traces.', scr_xsize=85)


cbase = widget_base( tbase, /column, /base_align_right, ypad=3, xpad=0, space=3)

; filters, charge mode

dbase = widget_base( cbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( dbase, value='Filter:')
filter_mode = widget_combobox( dbase, value=filter_title, uname='filter-mode', /tracking, $
					notify_realize='OnRealize_sim_filter_mode', $
					uvalue='Select the '+xname+' filter used.', xsize=190)
lab = widget_label( dbase, value='',scr_xsize=97)
charge_mode = widget_combobox( dbase, value=[' Current (nA)',' Charge (uC)',' Flux (p/s)'], uname='charge-mode', /tracking, $
					notify_realize='OnRealize_sim_charge_mode', $
					uvalue='Select either "Current" for count rates, "Charge" for integrated counts, or "Flux" for photons/particles per second.', xsize=100)

; detectors, charge

abase = widget_base( cbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( abase, value='Detector:')
detector_mode = widget_combobox( abase, value=detector_title, uname='detector-mode', /tracking, $
					notify_realize='OnRealize_sim_detector_mode', $
					uvalue='Select the relevant detector calibration.', xsize=190)
lab = widget_label( abase, value='',scr_xsize=20)
lab = widget_label( abase, value='i/ Q/ flux:',scr_xsize=70)
q_text = widget_text( abase, value=string((*p).charge), uname='q-text', /tracking, /editable, $
					uvalue='Enter the beam current (nA), integrated charge (microCoulomb) or flux (photons/particles per second).',scr_xsize=100)

; yields

ybase = widget_base( cbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( ybase, value='Yields:')
yield_mode = 0L
yield_file = widget_text( ybase, value=(*p).yield_file, uname='yield-file', /tracking, /editable, $
					uvalue='Select the '+pname+' Yield file name, or use the "Load" button to load.',scr_xsize=319)
yield_new_button = widget_button( ybase, value='New', uname='yield-new-button', /tracking, scr_xsize=30, $
					uvalue='Go to the Layer popup window to calculated '+pname+' yields.')
yield_load_button = widget_button( ybase, value='Load', uname='yield-popup-button', /tracking, scr_xsize=38, $
					uvalue='Load a set of precalculated '+pname+' yields from a '+yname+' file.')

; Table

t = strarr(8,256)

table = Widget_Table( tbase, UNAME='sim-table', /all_events, value=t, Notify_Realize='OnRealize_sim_Table',  $
			x_scroll_size=8, Y_SCROLL_SIZE=8, /RESIZEABLE_COLUMNS, alignment=2, $
			column_widths=[5,4,5,12,12,12,12,12]* !d.x_ch_size, /tracking, $
			column_labels=['Layer','El','Shell','Yield','Conc','a Counts / Rate','Tot Rel','tot Counts/Rate'], $
			uvalue='Yield: Counts/ppm.uC; Conc: ppm; a Counts / Rate: Counts or Count rate (per second) for alpha peak, ' + $
					'depending on Current versus Charge setting; Tot Rel: Total relative intensities; ' + $
					'tot Counts / Rate: total Counts or Count rate (per second) for all peaks.')

;.................................................................................

help = widget_text( tbase, scr_xsize=xsize_help, ysize=3, /wrap, uname='help', /tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

state = {	$
		path:			ptr_new(path), $		; pointer to current path
		p:				p, $					; pointer to parameters
		presults:		presults, $				; pointer to array of pointers to results
		player:			player, $				; pointer to layer setup parameters
		psim:			ptr_new(), $			; pointer to simted spec (these will be freed in spectrym_display)

		gamma:			gamma, $				; PIGE mode
		SIM_file:		SIM_file, $				; ID of SIM file text widget
		ptable:			ptable, $				; ID of periodic table
		use_m:			use_m, $				; flags use of M shell
		filter_mode:	filter_mode, $			; ID of filters droplist
		q_text:			q_text, $				; ID of charge text widget
		charge_mode:	charge_mode, $			; ID of charge mode widget
		conc_text:		conc_text, $			; ID of conc text widget
		layer_number:	layer_number, $			; ID of layer number droplist widget
		detector_mode:	detector_mode, $		; ID of detector_modes droplist
		yield_mode:		yield_mode, $			; ID of yields droplist
		yield_file:		yield_file, $			; ID of yields file text widget
		table:			table, $				; ID of table widget
		rows:			0, $					; number of valid rows in table
		row_height:		16, $					; height of each row (pixels)
		help:			help $					; ID of help text
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['path'], $			; new path
				from=group

xmanager, 'sim_setup_geopixe', tlb, /no_block

return
end
