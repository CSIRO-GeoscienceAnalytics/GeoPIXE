;
;	FIT setup and execution.
;
pro fit_setup_event, event

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common aps_4, aps_count_to_charge
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
		warning,'Fit_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
  widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good(pstate,/struct) eq 0 then goto, bad_state
p = (*pstate).p
if ptr_good(p,/struct) eq 0 then goto, bad_ptr

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

update_background = 0
update_initial = 0
do_fit = 0
refit = 0
save_da = 0
loop_on = 0
first_pass = 0
i_loop = 0

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
;					print,'Fit Setup: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'new-detectors': begin
				present = (*(*p).detector_list)[(*p).detector_mode]
				detector_update, list=list, title=title, present=present, new=new
				widget_control, (*pstate).detector_mode, set_value=title, set_combobox_select=new
				*(*p).detector_list = list
				goto, finish
				end
			'new-filters': begin
				present = (*(*p).filter_list)[(*p).filter_mode]
				filter_update, list=list, title=title, present=present, new=new
				widget_control, (*pstate).filter_mode, set_value=title, set_combobox_select=new
				*(*p).filter_list = list
				goto, finish
				end
			'spectrum-fit': begin
				if ptr_valid( event.pointer) then begin
					if ptr_valid( (*event.pointer).pspec) then begin
						(*pstate).pspec = (*event.pointer).pspec
						if ptr_valid( (*pstate).pspec) then begin
							(*p).charge = (*(*pstate).pspec).charge
							widget_control, (*pstate).q_text, set_value=str_tidy((*p).charge)
						endif
					endif
					if ptr_valid( ((*event.pointer).pall)[0]) then begin
						(*pstate).pall = (*event.pointer).pall
					endif
					if ptr_good( (*p).save_detector) then begin					;@29-3-16
						(*(*p).detector).w0  =  (*(*p).save_detector).w0
						(*(*p).detector).w1  =  (*(*p).save_detector).w1
						FWHM_Mn = 1000.*sqrt(abs( (*(*p).detector).w1 * 5.898 + (*(*p).detector).w0 ))
						widget_control, (*pstate).width_slider, set_value=FWHM_Mn
					endif

					(*pstate).pfit = ptr_new()
					(*pstate).pback = ptr_new()
					(*pstate).da_fresh = 0
;					fit_setup_do_fit, pstate, p, /update_initial, /update_background
					goto, finish
				endif
				goto, finish
				end
			'spectrum-view': begin
				if ptr_valid( event.pointer) then begin
					cal_ab, (*(*pstate).pspec).cal, a,b,u, error=error
					if error eq 0 then begin
						(*p).view[0] = ((*event.pointer).low*a + b) > 0.32
						(*p).view[1] = (*event.pointer).high*a + b
					endif
				endif
				goto, finish
				end
			'results-select': begin
				if ptr_valid( event.pointer) then begin
					(*pstate).select = (*event.pointer).top
				endif
				goto, finish
				end
			'time-amp-pileup': begin
				if ptr_valid( event.pointer) then begin
					(*pstate).pileup = event.pointer
				endif
				goto, finish
				end
			'mark-fit': begin
				if ptr_valid( event.pointer) then begin
					z = (*event.pointer).z
					widget_control, (*pstate).ptable, set_value={Z:z, STATE:(z le 56 ? 1 : 2), ALT:0}
				endif

				if ptr_valid( (*p).pz_fit) eq 0 then begin
					(*p).pz_fit = ptr_new( {element, Z:Z, Shell:(z le 56 ? 1 : 2)}, /no_copy)
				endif else begin
					q = where( (*(*p).pz_fit).Z eq Z)
					if q[0] ne -1 then begin
						(*(*p).pz_fit)[q].Shell = (z le 56 ? 1 : 2)					; new one
					endif else begin
						t = [ *(*p).pz_fit, {element, Z:Z, Shell:(z le 56 ? 1 : 2)} ]
						ptr_free, (*p).pz_fit
						(*p).pz_fit = ptr_new(t, /no_copy)							; append
					endelse
				endelse
				if ptr_valid( (*p).pz_mdl) then begin
					q = where( (*(*p).pz_mdl).Z eq (*event.pointer).Z)
					if q[0] ne -1 then begin
						(*(*p).pz_mdl)[q].Shell = 0
					endif
				endif

				if ptr_valid( (*p).pz_fit) and ptr_valid( (*p).pz_mdl) then begin
					tm = (*(*p).pz_mdl)
					for i=0L,n_elements((*(*p).pz_fit).z)-1 do begin
						q = where( ((*(*p).pz_fit)[i].z eq tm.z) and ((*(*p).pz_fit)[i].shell eq tm.shell))
						if q[0] ne -1 then begin
							tm[q].shell = 0
						endif
					endfor
					ptr_free, (*p).pz_mdl
					q = where( tm.Shell ne 0)												; weed out Z,Shell
					if q[0] ne -1 then begin												; that are in both lists
						tm = tm[q]
						(*p).pz_mdl = ptr_new(tm, /no_copy)
					endif
				endif
;				if ptr_valid((*p).pz_fit) then print,'fit Z:',	(*(*p).pz_fit).Z
;				if ptr_valid((*p).pz_fit) then print,'fit S:',	(*(*p).pz_fit).Shell
;				if ptr_valid((*p).pz_mdl) then print,'mdl Z:',	(*(*p).pz_mdl).Z
;				if ptr_valid((*p).pz_mdl) then print,'mdl S:',	(*(*p).pz_mdl).Shell

				(*pstate).da_fresh = 0
				fit_setup_do_fit, pstate, p, /update_initial
				goto, cont
				end

			'new-yields': begin						; from layer_setup
				if ptr_valid( event.pointer) then begin
					F = *event.pointer
					if F ne '' then begin
						(*p).yield_file = F
						yields = read_yield(F, error=error)
						if error then begin
							warning, 'fit_setup',['Error in transferred PIXE/SXRF Yield file.', $
							'Recalculate yields and save them to a '+yname+' file.']
						endif else begin
							set_widget_text, (*pstate).yield_file, F
							set_widget_text, (*pstate).yield_file2, F
							if ptr_valid((*p).yields) then ptr_free, (*p).yields

;							only use the first of a "many" series of thickness yields ...
							(*p).yields = ptr_new( (*yields)[0] )
							(*p).ambient.on = 0

							if (*(*p).yields).array then begin
								n = n_elements(*(*p).detector_list)							; array on in layer-setup
								if n lt 1 then goto, finish									; set a new detector
								fdets = strip_path((*(*p).yields).detector_file)
								detector_update, list=list, title=title, present=fdets, new=i, file=f
								*(*p).detector_list = list
								widget_control, (*pstate).detector_mode, set_value=title
								if i eq -1 then goto, finish
								(*p).detector_mode = i
								widget_control, (*pstate).detector_mode, set_combobox_select=i, sensitive=1-(*(*p).yields).array

								detector = read_detector( f, error=error)
								if error then begin
									warning, 'fit_setup','Error reading Detectors file '+f, /error
									goto, finish
								endif else begin
									*(*p).detector = *detector

									if (*(*p).detector).array and (strlen((*(*p).detector).layout) gt 0) then begin
										d = read_detector_layout((*(*p).detector).layout, error=error)
										if error eq 0 then *(*p).playout = d
									endif

									*(*p).save_detector = *(*p).detector
									widget_control, (*pstate).tail_amp_slider, set_value=(*(*p).detector).tail.amp
									widget_control, (*pstate).tail_F_slider, set_value=(*(*p).detector).tail.F
									widget_control, (*pstate).tail_B_slider, set_value=(*(*p).detector).tail.B
									widget_control, (*pstate).tail_L_slider, set_value=(*(*p).detector).tail.L
									widget_control, (*pstate).tail_S_slider, set_value=(*(*p).detector).tail.S
								endelse
							endif else begin
								widget_control, (*pstate).detector_mode, sensitive=1-(*(*p).yields).array
							endelse
							(*pstate).da_fresh = 0
							fit_setup_do_fit, pstate, p, /update_initial
							goto, cont
						endelse
					endif
				endif
				goto, finish
				end
				
			'new-correct': begin						; from correct_yield
				if ptr_valid( event.pointer) then begin
					F = *event.pointer
					if F ne '' then begin
						(*p).correct_file = F
						set_widget_text, (*pstate).correct_file, F
					endif
				endif
				end

			'wizard-action': begin
				if ptr_valid( event.pointer) then begin
					if (*event.pointer).window eq 'Spectrum Fit' then begin
						case (*event.pointer).command of
							'open-test': begin
;								print,'*** Wizard Spectrum Fit: test if window is open ...'
								pw = (*pstate).pwiz
								*pw = *event.pointer
								(*pw).top = event.top
								(*pw).error = 0
								notify, 'wizard-return', pw
								end

							'fit-one': begin
								print,'*** Wizard Spectrum Fit: fit one spectra ...'
								pw = event.pointer
								pd = (*pw).pdata

								(*pstate).pall = ptr_new()
								fit_setup_do_fit, pstate, p, /do_fit, /update_background, error=err

								(*pd).presults = (*pstate).presults
								(*pw).error = err
								notify, 'wizard-return', pw
								end

							'fit-all': begin
								print,'*** Wizard Spectrum Fit: fit all spectra ...'
								pw = event.pointer
								if ptr_good( (*pstate).pall) eq 0 then begin
									warning,'Fit_setup: Notify: Pointer "pall" undefined.'
									err = 1
								endif else begin
									old_pspec = (*pstate).pspec
									(*pstate).pspec = (*(*pstate).pall)[0]
									
									fit_setup_do_fit, pstate, p, /do_fit, /loop_on, /update_background, $
											first_pass=1, i_loop=0, n_loop=n_elements( *(*pstate).pall)
											
									if ptr_valid( old_pspec) then (*pstate).pspec = old_pspec
									err = 0
								endelse

								(*pw).error = err
								notify, 'wizard-return', pw
								end
							else: begin
								warning,'fit_setup: Notify',['Unknown wizard command: '+(*event.pointer).command, $
										'Make sure GeoPIXE version is compatible with Wizard.']
							endelse
						endcase
					endif
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
				widget_control, (*pstate).help, set_value='LEFT click an element to enable its K shell. Click again for L, M, both K&L, ...    ' + $
							'RIGHT click to temporarily disable an element (still calculate MDL); ' + $
							'LEFT click to re-enable it.
			endelse
		endif
		goto, finish
		end
	'WIDGET_TIMER': begin
;		if widget_info(event.top,/update) eq 0 then widget_control, event.top, update=1
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request Fit_Setup ...'
		goto, kill
		end
;	'WIDGET_BASE': begin
;		uname = widget_info( event.id, /uname)
;		case uname of
;			'fit_TLB': begin
;				goto, finish
;				end
;			else:
;		endcase
;		end
	else:
endcase

;widget_control, event.top, update=0
uname = widget_info( event.id, /uname)
case uname of

	'load-setup-button': begin
		file = (*p).pcm_file
		path = *(*pstate).path
		F = file_requester( /read, filter = '*.pcm', path=path, file=file,  dialog_parent=event.top, $
					title='Select the source PCM parameter file', /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.pcm'
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).pcm_file, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).pcm_file, set_value=F, set_text_select=[n-1,0]
			(*p).pcm_file = F
			load_pcm_parameters, pstate, F
			(*pstate).da_fresh = 0
			fit_setup_do_fit, pstate, p, /update_initial, /update_background
		endif
		end

	'save-setup-button': begin
		file = (*p).pcm_file
		path = *(*pstate).path
		F = file_requester( /write, filter = '*.pcm', path=path, dialog_parent=event.top, $
					title='Save the fit setup parameters to a PCM file', file=file, /fix_filter)
		if F ne '' then begin
			F = strip_file_ext(F) + '.pcm'
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).pcm_file, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).pcm_file, set_value=F, set_text_select=[n-1,0]
			(*p).pcm_file = F
			save_pcm_parameters, pstate, F
		endif
		end

	'pcm-file': begin
		widget_control, event.id, get_value=F
		F = strip_file_ext(F[0]) + '.pcm'
		(*p).pcm_file = F
		*(*pstate).path = extract_path(F)
		n = lenchr(F)
		k = lenchr(strip_path(F))
		widget_control, (*pstate).pcm_file, set_value=F, set_text_select=[n-k,k]
		widget_control, (*pstate).pcm_file, set_value=F, set_text_select=[n-1,0]
		load_pcm_parameters, pstate, F
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'tab-panel': begin
		widget_control_update, (*pstate).tlb, update=0
		g = widget_info( event.id, /geometry)
		if widget_info( (*pstate).adjust_base, /valid_id) then begin
			ga = widget_info((*pstate).advanced_base,/geometry)
			gx = widget_info((*pstate).adjust_base,/geometry)
			if (event.tab eq 3) and ((*pstate).adjust_on eq 0) then begin
				widget_control,event.id,scr_ysize = g.scr_ysize + (gx.scr_ysize-ga.scr_ysize)
				(*pstate).adjust_on = 1
			endif else if (event.tab ne 3) and ((*pstate).adjust_on eq 1) then begin
				widget_control,event.id,scr_ysize = g.scr_ysize - (gx.scr_ysize-ga.scr_ysize)
				(*pstate).adjust_on = 0
			endif
		endif
		widget_control_update, (*pstate).tlb, update=1
		goto, finish
		end

	'advanced-panel': begin
		goto, finish
		end

	'periodic-table': begin
		if event.alt eq 0 then begin
			if ptr_valid( (*p).pz_fit) eq 0 then begin
				(*p).pz_fit = ptr_new( {element, Z:event.Z, Shell:event.Shell}, /no_copy)
			endif else begin
				q = where( (*(*p).pz_fit).Z eq event.Z)
				if q[0] ne -1 then begin
					(*(*p).pz_fit)[q].Shell = event.Shell						; existing, set shell
				endif else begin
					t = [ *(*p).pz_fit, {element, Z:event.Z, Shell:event.Shell} ]
					ptr_free, (*p).pz_fit
					(*p).pz_fit = ptr_new(t, /no_copy)							; new one, append
				endelse
			endelse
			if ptr_valid( (*p).pz_mdl) then begin
				q = where( (*(*p).pz_mdl).Z eq event.Z)
				if q[0] ne -1 then begin
					(*(*p).pz_mdl)[q].Shell = 0									; flag MDL off
				endif
			endif
		endif else begin
			if ptr_valid( (*p).pz_mdl) eq 0 then begin
				(*p).pz_mdl = ptr_new( {element, Z:event.Z, Shell:event.Shell}, /no_copy)
			endif else begin
				q = where( (*(*p).pz_mdl).Z eq event.Z)
				if q[0] ne -1 then begin
					(*(*p).pz_mdl)[q].Shell = event.Shell						; existing
				endif else begin
					t = [ *(*p).pz_mdl, {element, Z:event.Z, Shell:event.Shell} ]
					ptr_free, (*p).pz_mdl
					(*p).pz_mdl = ptr_new(t, /no_copy)							; new one, append
				endelse
			endelse
			if ptr_valid( (*p).pz_fit) then begin
				q = where( (*(*p).pz_fit).Z eq event.Z)
				if q[0] ne -1 then begin
					(*(*p).pz_fit)[q].Shell = 0									; MDL, so flag normal off
				endif
			endif
		endelse
		if ptr_valid( (*p).pz_fit) then begin
			t = *(*p).pz_fit
			ptr_free, (*p).pz_fit
			q = where( t.Shell ne 0)											; weed out Shell=0
			if q[0] ne -1 then begin
				t = t[q]
				(*p).pz_fit = ptr_new(t, /no_copy)
			endif
		endif
		if ptr_valid( (*p).pz_mdl) then begin
			t = *(*p).pz_mdl
			ptr_free, (*p).pz_mdl
			q = where( t.Shell ne 0)											; weed out Shell=0
			if q[0] ne -1 then begin
				t = t[q]
				(*p).pz_mdl = ptr_new(t, /no_copy)
			endif
		endif

		if ptr_valid( (*p).pz_fit) and ptr_valid( (*p).pz_mdl) then begin
			tm = (*(*p).pz_mdl)
			for i=0L,n_elements((*(*p).pz_fit).z)-1 do begin
				q = where( ((*(*p).pz_fit)[i].z eq tm.z) and ((*(*p).pz_fit)[i].shell eq tm.shell))
				if q[0] ne -1 then begin
					tm[q].shell = 0
				endif
			endfor
			ptr_free, (*p).pz_mdl
			q = where( tm.Shell ne 0)											; weed out Z,Shell
			if q[0] ne -1 then begin											; that are in both lists
				tm = tm[q]
				(*p).pz_mdl = ptr_new(tm, /no_copy)
			endif
		endif
		if ptr_valid((*p).pz_fit) then print,'fit Z:',	(*(*p).pz_fit).Z
		if ptr_valid((*p).pz_fit) then print,'fit S:',	(*(*p).pz_fit).Shell
		if ptr_valid((*p).pz_mdl) then print,'mdl Z:',	(*(*p).pz_mdl).Z
		if ptr_valid((*p).pz_mdl) then print,'mdl S:',	(*(*p).pz_mdl).Shell

		if ptr_valid((*p).pz_fit) then update_initial = 1
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, update_initial=update_initial
		end

	'elow-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).e_low = float(s)
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'ehigh-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).e_high = float(s)
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'use-view-button': begin
		(*p).e_low = (*p).view[0] > 0.05
		(*p).e_high = (*p).view[1] > ((*p).e_low + 0.1)
		widget_control, (*pstate).elow_text, set_value=str_tidy( (*p).e_low)
		widget_control, (*pstate).ehigh_text, set_value=str_tidy( (*p).e_high)
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'filter-mode': begin
		n = n_elements(*(*(*pstate).p).filter_list)
		if n lt 1 then goto, cont
		filter_update, present=(*(*p).filter_list)[event.index < (n-1)], new=i, file=f
		(*p).filter_mode = i < (n-1)
		filter = read_filters( f, error=error)
		if error then begin
			warning, 'fit_setup','Error reading Filter file '+f, /error
			goto, finish
		endif else begin
			if ptr_valid((*p).filter) then ptr_free, (*p).filter
			(*p).filter = filter
		endelse
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'q-text': begin
		widget_control, event.id, get_value=s
		s = s[0]
		(*p).charge = float(s)
		if ptr_valid( (*pstate).pspec) then begin
			(*(*pstate).pspec).charge = (*p).charge
			if (*(*pstate).pspec).IC_total gt 0.0 then begin
				aps_count_to_charge = (*(*pstate).pspec).charge / (*(*pstate).pspec).IC_total
				(*(*pstate).pspec).IC.conversion = aps_count_to_charge 
				notify, 'fit-display', from=event.top
			endif
		endif
		end

	'flux': begin
		widget_control, (*pstate).q_text, get_value=s
		(*p).charge = float(s[0])
		
		conv = aps_count_to_charge
		charge = (*p).charge
		charge_select, event.top, title='Flux to Charge conversion', conv=conv, charge=charge, flux=(*(*pstate).pspec).IC_total, error=error
		
;		head = {charge:(*p).charge, fluence:(*(*pstate).pspec).IC_total, conversion:aps_count_to_charge, use_flux:0, error:0}
;		head = APS_charge_select( event.top, head)
;		if head.error eq 0 then begin
		
		if error eq 0 then begin
			(*p).charge = charge
			aps_count_to_charge = conv
			widget_control, (*pstate).q_text, set_value=str_tidy((*p).charge)
			(*(*pstate).pspec).charge = (*p).charge
			(*(*pstate).pspec).IC.conversion = aps_count_to_charge 
			notify, 'fit-display', from=event.top
		endif
		end

	'detector-mode': begin
		detector_update, list=list, title=title, present=(*(*p).detector_list)[event.index], new=i, file=f, count=n
		widget_control, (*pstate).detector_mode, set_value=title
		*(*p).detector_list = list
		if n lt 1 then goto, cont
		if i eq -1 then goto, finish
		(*p).detector_mode = i
		widget_control, (*pstate).detector_mode, set_combobox_select=i
		if ptr_good((*p).yields) then begin
			widget_control, (*pstate).detector_mode, sensitive=1-(*(*p).yields).array
		endif
		detector = read_detector( f, error=error)
		if error then begin
			warning, 'fit_setup','Error reading Detectors file '+(*(*p).detector_list)[(*p).detector_mode], /error
			goto, finish
		endif else begin
			*(*p).detector = *detector
			*(*p).save_detector = *detector
			widget_control, (*pstate).tail_amp_slider, set_value=(*(*p).detector).tail.amp
			widget_control, (*pstate).tail_F_slider, set_value=(*(*p).detector).tail.F
			widget_control, (*pstate).tail_B_slider, set_value=(*(*p).detector).tail.B
			widget_control, (*pstate).tail_L_slider, set_value=(*(*p).detector).tail.L
			widget_control, (*pstate).tail_S_slider, set_value=(*(*p).detector).tail.S

			if (*(*p).detector).array and (strlen((*(*p).detector).layout) gt 0) then begin
				d = read_detector_layout((*(*p).detector).layout, error=error)
				if error eq 0 then *(*p).playout = d
			endif
		endelse
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'update-detector-button': begin
		if ptr_valid( (*p).detector) eq 0 then goto, finish
		file = (*(*p).detector_list)[(*p).detector_mode]
		path = extract_path( file[0])
		file = strip_path(file[0])
		if lenchr(path) eq 0 then path = ''
		F = file_requester( /write, filter = '*.detector', $
					path=path, file=file, group=event.top, $
					title='Save altered detector parameters to a DETECTOR file', /fix_filter)
		if F ne '' then begin
			write_detector, (*p).detector, F, error=error
			if error then begin
				warning, 'fit_setup','Error writing DETECTOR file: '+F, /error
			endif else begin
				*(*p).save_detector = *(*p).detector
			endelse
		endif
		goto, finish
		end

	'cuts-file': begin
		widget_control, event.id, get_value=F
		F = F[0]
		(*p).cuts_file = F
		(*pstate).da_fresh = 0
		if lenchr(F) eq 0 then begin
			if ptr_valid((*p).cuts) then ptr_free, (*p).cuts
			(*p).cuts = ptr_new()
			widget_control, (*pstate).cuts_file, set_value=''
		endif else begin
			*(*pstate).path = extract_path(F)
			n = lenchr(F)
			k = lenchr(strip_path(F))
			widget_control, (*pstate).cuts_file, set_value=F, set_text_select=[n-k,k]
			widget_control, (*pstate).cuts_file, set_value=F, set_text_select=[n-1,0]
			cuts = read_cuts(F, error=error)
			if error then begin
				warning, 'fit_setup','Error reading CUTS file: '+F, /error
				if ptr_valid((*p).cuts) then ptr_free, (*p).cuts
				(*p).cuts = ptr_new()
				goto, finish
			endif else begin
				if ptr_valid((*p).cuts) then ptr_free, (*p).cuts
				(*p).cuts = cuts
			endelse
		endelse
		fit_setup_do_fit, pstate, p, /update_initial
		end

	'load-cuts-button': begin
		file =  (*p).cuts_file
		path = *(*pstate).path
		F = file_requester( /read, filter = '*.cuts', path=path, dialog_parent=event.top, $
					title='Select CUTS file', file=file, /fix_filter)
		if F ne '' then begin
			*(*pstate).path = extract_path(F)
			set_widget_text, (*pstate).cuts_file, F
			cuts = read_cuts(F, error=error)
			(*p).cuts_file = F
			if error then begin
				warning, 'fit_setup','Error reading CUTS file: '+F, /error
				if ptr_valid((*p).cuts) then ptr_free, (*p).cuts
				(*p).cuts = ptr_new()
				goto, finish
			endif else begin
				if ptr_valid((*p).cuts) then ptr_free, (*p).cuts
				(*p).cuts = cuts
				(*pstate).da_fresh = 0
				fit_setup_do_fit, pstate, p, /update_initial
			endelse
		endif else goto, finish
		end

	'yield-mode': begin
		(*p).yield_mode = event.index
		end

	'yield-new-button': begin
		layer_setup, group_leader=event.top, TLB=tlb, pars=(*pstate).player, path=*(*pstate).path, gamma=(*pstate).gamma
		register_notify, event.top, [ 'path','new-yields'], from=tlb
		end

	'yield-popup-button': begin
		file = find_file2( (*p).yield_file)
		path = *(*pstate).path
		F = file_requester( /read, filter = '*.'+yname, path=path, group=event.top, $
					title='Select '+pname+' Yield file', file=file, /fix_filter)
		if F ne '' then begin
			yields = read_yield(F, error=error)
			if error then begin
				warning, 'fit_setup','Error reading '+pname+' Yield file: '+F, /error
				goto, finish
			endif else begin
				set_widget_text, (*pstate).yield_file, F
				set_widget_text, (*pstate).yield_file2, F
				if ptr_valid((*p).yields) then ptr_free, (*p).yields
				(*p).yield_file = F
				(*p).yields = yields
				(*p).ambient.on = 0												; reset to NPT conditions
				if (*(*p).yields).array then begin
					n = n_elements(*(*p).detector_list)							; array on in layer-setup
					if n lt 1 then goto, finish									; set a new detector
					fdets = strip_path((*(*p).yields).detector_file)
					detector_update, list=list, title=title, present=fdets, new=i, file=f
					*(*p).detector_list = list
					widget_control, (*pstate).detector_mode, set_value=title
					if i eq -1 then goto, finish
					(*p).detector_mode = i
					widget_control, (*pstate).detector_mode, set_combobox_select=i, sensitive=1-(*(*p).yields).array

					detector = read_detector( f, error=error)
					if error then begin
						warning, 'fit_setup','Error reading Detectors file '+(*(*p).detector_list)[(*p).detector_mode], /error
						goto, finish
					endif else begin
						*(*p).detector = *detector

						if (*(*p).detector).array and (strlen((*(*p).detector).layout) gt 0) then begin
							d = read_detector_layout((*(*p).detector).layout, error=error)
							if error eq 0 then *(*p).playout = d
						endif

						*(*p).save_detector = *(*p).detector
						widget_control, (*pstate).tail_amp_slider, set_value=(*(*p).detector).tail.amp
						widget_control, (*pstate).tail_F_slider, set_value=(*(*p).detector).tail.F
						widget_control, (*pstate).tail_B_slider, set_value=(*(*p).detector).tail.B
						widget_control, (*pstate).tail_L_slider, set_value=(*(*p).detector).tail.L
						widget_control, (*pstate).tail_S_slider, set_value=(*(*p).detector).tail.S
					endelse
				endif else begin
					widget_control, (*pstate).detector_mode, sensitive=1-(*(*p).yields).array
				endelse
				(*pstate).da_fresh = 0
				fit_setup_do_fit, pstate, p, /update_initial
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
				warning, 'fit_setup','Error reading '+pname+' Yield file: '+F, /error
			endif else begin
				set_widget_text, (*pstate).yield_file, F
				set_widget_text, (*pstate).yield_file2, F
				if ptr_valid((*p).yields) then ptr_free, (*p).yields
				(*p).yields = yields
				(*p).ambient.on = 0												; reset to NPT conditions
				if (*(*p).yields).array then begin
					n = n_elements(*(*p).detector_list)							; array on in layer-setup
					if n lt 1 then goto, finish									; set a new detector
					fdets = strip_path((*(*p).yields).detector_file)
					detector_update, list=list, title=title, present=fdets, new=i, file=f
					*(*p).detector_list = list
					widget_control, (*pstate).detector_mode, set_value=title
					if i eq -1 then goto, finish
					(*p).detector_mode = i
					widget_control, (*pstate).detector_mode, set_combobox_select=i, sensitive=1-(*(*p).yields).array

					detector = read_detector( f, error=error)
					if error then begin
						warning, 'fit_setup','Error reading Detectors file '+(*(*p).detector_list)[(*p).detector_mode], /error
						goto, finish
					endif else begin
						*(*p).detector = *detector

						if (*(*p).detector).array and (strlen((*(*p).detector).layout) gt 0) then begin
							d = read_detector_layout((*(*p).detector).layout, error=error)
							if error eq 0 then *(*p).playout = d
						endif

						*(*p).save_detector = *(*p).detector
						widget_control, (*pstate).tail_amp_slider, set_value=(*(*p).detector).tail.amp
						widget_control, (*pstate).tail_F_slider, set_value=(*(*p).detector).tail.F
						widget_control, (*pstate).tail_B_slider, set_value=(*(*p).detector).tail.B
						widget_control, (*pstate).tail_L_slider, set_value=(*(*p).detector).tail.L
						widget_control, (*pstate).tail_S_slider, set_value=(*(*p).detector).tail.S
					endelse
				endif else begin
					widget_control, (*pstate).detector_mode, sensitive=1-(*(*p).yields).array
				endelse
				(*pstate).da_fresh = 0
				fit_setup_do_fit, pstate, p, /update_initial
			endelse
		endif else goto, finish
		end

	'correct-mode': begin
		(*p).mpda_mode = event.index
		widget_control, (*pstate).mpda_base, map=((*p).mpda_mode gt 0)
		end

	'correct-new-button': begin
		el = fit_setup_elements( p, free=el_free, use_m=(*pstate).use_m, OK=OK)
		if OK eq 0 then begin
			warning,'fit_setup_event','Need to select elements in fit first.'
		endif
		correct_yield, group_leader=event.top, TLB=tlb, /big, pars=(*p).pcorrect, damode=0, el=el, path=*(*pstate).path, gamma=(*pstate).gamma
		register_notify, event.top, [ 'path', 'new-correct'], from=tlb
		end

	'correct-popup-button': begin
		file = (*p).correct_file
		path = *(*pstate).path
		F = file_requester( /read, filter = '*.comat', path=path, group=event.top, $
					title='Select Composition correction file', file=file, /fix_filter)
		if F ne '' then begin
			correct = read_correct(F, mode=0, error=error)
			if error then begin
				warning, 'fit_setup','Error reading Corrections file: '+F, /error
				goto, finish
			endif else begin
				set_widget_text, (*pstate).correct_file, F
				(*p).correct_file = F
				if ptr_valid((*p).pcorrect) then ptr_free, (*p).pcorrect
				(*p).pcorrect = correct
			endelse
		endif else goto, finish
		end

	'correct-file': begin
		widget_control, event.id, get_value=F
		F = F[0]
		if F ne '' then begin
			correct = read_correct(F, mode=0, error=error)
			if error then begin
				warning, 'fit_setup','Error reading Corrections file: '+F, /error
				goto, finish
			endif else begin
				set_widget_text, (*pstate).correct_file, F
				(*p).correct_file = F
				if ptr_valid((*p).pcorrect) then ptr_free, (*p).pcorrect
				(*p).pcorrect = correct
			endelse
		endif else goto, finish
		end

	'fix-free1': begin
		(*p).free[event.value] = event.select
		(*pstate).da_fresh = 0
		case event.value of
			0: begin
				if (event.select eq 1) and ((*p).cal_mode ne 0) then begin
					(*p).cal_mode = 0
					widget_control, (*pstate).cal_mode, set_combobox_select=(*p).cal_mode
				endif
				if (event.select eq 0) and ((*p).cal_mode eq 0) then begin
					(*p).cal_mode = 1
					widget_control, (*pstate).cal_mode, set_combobox_select=(*p).cal_mode
				endif
				end
			1: begin
				if (event.select eq 1) and ((*p).width_mode ne 0) then begin
					(*p).width_mode = 0
					widget_control, (*pstate).width_mode, set_combobox_select=(*p).width_mode
				endif
				if (event.select eq 0) and ((*p).width_mode eq 0) then begin
					(*p).width_mode = 1
					widget_control, (*pstate).width_mode, set_combobox_select=(*p).width_mode
				endif
				end
			else:
		endcase
		end

	'fix-free2': begin
		case event.value of
			0: begin
				(*p).free[2] = event.select
				if (event.select eq 1) and ((*p).tail_mode ne 0) then begin
					(*p).tail_mode = 0
					widget_control, (*pstate).tail_mode, set_combobox_select=(*p).tail_mode
				endif
				if (event.select eq 0) and ((*p).tail_mode eq 0) then begin
					(*p).tail_mode = 1
					widget_control, (*pstate).tail_mode, set_combobox_select=(*p).tail_mode
				endif
				if (*p).free[2] eq 1 then (*p).free[3]=0
				end
			1: begin
				(*p).boost = event.select
				widget_control, (*pstate).boost_mode, set_combobox_select=(*p).boost
				end
			else:
		endcase
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'pixe-options': begin
		case event.value of
			0: begin
				(*p).trim_seb = event.select
				end
			else:
		endcase
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'free-gain': begin
		case event.value of
			0: begin
				(*p).free_gain = event.select
				end
			else:
		endcase
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'cal-mode': begin
		(*p).cal_mode = event.index
		(*p).free[0] = ((*p).cal_mode eq 0)
		widget_control, (*pstate).fix_free1, set_value=[(*p).free[0],(*p).free[1]]
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial
		end

	'free-fano': begin
		case event.value of
			0: begin
				(*p).free_fano = event.select
				end
			else:
		endcase
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'width-mode': begin
		(*p).width_mode = event.index
		(*p).free[1] = ((*p).width_mode eq 0)
		widget_control, (*pstate).fix_free1, set_value=[(*p).free[0],(*p).free[1]]
		case (*p).width_mode of
			2: begin				; default
				if ptr_valid( (*p).detector) then begin
					w0 = (*(*p).detector).w0
					w1 = (*(*p).detector).w1
					(*p).old_a[0] = sqrt( w0 + w1* (*(*pstate).p).e_low ) * (*p).old_a[3]
					FWHM_Mn = 1000.*sqrt(abs( w1 * 5.898 + w0 ))
					widget_control, (*pstate).width_slider, set_value=FWHM_Mn
					(*p).width_mode = 1
					widget_control, (*pstate).width_mode, set_combobox_select=(*p).width_mode
				endif
				end
			else:
		endcase
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial
		end

	'width-slider': begin
		if ptr_valid( (*p).detector) then begin
			w1 = (*(*p).detector).w1
			w0 = (event.value/1000.)^2 - w1 * 5.898
			(*p).old_a[0] = sqrt( w0 + w1* (*(*pstate).p).e_low ) * (*p).old_a[3]
			(*(*p).detector).w0 = w0

			(*p).width_mode = 1
			widget_control, (*pstate).width_mode, set_combobox_select=(*p).width_mode
			(*p).free[1] = ((*p).width_mode eq 0)
			widget_control, (*pstate).fix_free1, set_value=[(*p).free[0],(*p).free[1]]
		endif
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial
		end

	'tail-mode': begin
		(*p).tail_mode = event.index
		(*p).free[2] = ((*p).tail_mode eq 0)
		widget_control, (*pstate).fix_free2, set_value=[(*p).free[2],(*p).boost]
		case (*p).tail_mode of
			0: begin				; free
				(*p).free[3] = 0					; enable tails
				end
			1: begin				; fixed
				(*p).free[3] = 0
				end
			2: begin				; default
				(*p).free[3] = 0
				if ptr_valid( (*p).detector) and ptr_good( (*p).save_detector) then begin
					(*(*p).detector).tail.amp = (*(*p).save_detector).tail.amp
					(*(*p).detector).tail.F = (*(*p).save_detector).tail.F
					(*(*p).detector).tail.B = (*(*p).save_detector).tail.B
					(*(*p).detector).tail.L = (*(*p).save_detector).tail.L
					(*(*p).detector).tail.S = (*(*p).save_detector).tail.S
					widget_control, (*pstate).tail_amp_slider, set_value=(*(*p).detector).tail.amp
					widget_control, (*pstate).tail_F_slider, set_value=(*(*p).detector).tail.F
					widget_control, (*pstate).tail_B_slider, set_value=(*(*p).detector).tail.B
					widget_control, (*pstate).tail_L_slider, set_value=(*(*p).detector).tail.L
					widget_control, (*pstate).tail_S_slider, set_value=(*(*p).detector).tail.S
					(*p).tail_mode = 1
					widget_control, (*pstate).tail_mode, set_combobox_select=(*p).tail_mode
				endif
				end
			3: begin				; zero
				(*p).free[3] = 1					; no tails
				end
			else:
		endcase
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial
		end

	'tail-amp-slider': begin
		if ptr_valid( (*p).detector) then begin
			(*(*p).detector).tail.amp = event.value
		endif
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /tweak_par	;/update_initial
		end

	'tail-F-slider': begin
		if ptr_valid( (*p).detector) then begin
			(*(*p).detector).tail.F = event.value
		endif
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /tweak_par	;/update_initial
		end

	'tail-B-slider': begin
		if ptr_valid( (*p).detector) then begin
			(*(*p).detector).tail.B = event.value
		endif
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /tweak_par	;/update_initial
		end

	'tail-L-slider': begin
		if ptr_valid( (*p).detector) then begin
			(*(*p).detector).tail.L = event.value
		endif
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /tweak_par	;/update_initial
		end

	'tail-S-slider': begin
		if ptr_valid( (*p).detector) then begin
			(*(*p).detector).tail.S = event.value
		endif
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /tweak_par	;/update_initial
		end

	'Ctail-amp-slider': begin
		(*(*p).Compton).tail.amp = event.value
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /tweak_par	;/update_initial
		end

	'Ctail-len-slider': begin
		(*(*p).Compton).tail.len = event.value
		print,'Compton tail length=',event.value
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /tweak_par	;/update_initial
		end

	'Compton-shift-slider': begin
		help, event.value
		(*(*p).Compton).shift = event.value
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /tweak_par	;, /update_initial
		end

	'Compton-spread-slider': begin
		(*(*p).Compton).spread = event.value
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /tweak_par	;/update_initial
		end

	'back-mode': begin
		(*p).background = event.index
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'passes-mode': begin
		(*p).passes = event.index + 1
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'default-curvature-button': begin
		(*p).curve = 1.0003
		widget_control, (*pstate).curvature_slider, set_value=(*p).curve
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'curvature-slider': begin
		(*p).curve = event.value
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'boost-mode': begin
		(*p).boost = event.index
		(*pstate).da_fresh = 0
		widget_control, (*pstate).fix_free2, set_value=[(*p).free[2],(*p).boost]
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'back2-mode': begin
		(*p).background2 = event.index
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'back2-split-energy': begin
		widget_control, (*pstate).back2_split_energy, get_value=v
		(*p).back2_split_energy = float2(v)
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'pileup-mode': begin
		(*p).pileup_mode = event.index
		(*pstate).da_fresh = 0
		end

	'sum-deficit-slider': begin
		(*p).sum_deficit = event.value
		(*pstate).da_fresh = 0
		fit_setup_do_fit, pstate, p, /update_initial, /update_background
		end

	'tweek-el': begin
		(*p).tweek.el = event.index-1
		fit_setup_set_adjust, pstate
		end

	'tweek-par': begin
		widget_control, event.id, get_uvalue=u
		s = strsplit(string(u),/extract)
		n = clip(fix(s[0]) , 0,19)
		if (n eq 0) and (event.index ne 0) then begin
			warning,'fit_setup_event',['Varying the main peak may make the fit unstable.', $
										'Keep this line fixed ("off") and vary others.']
			(*p).tweek.lines[n] = -1
			widget_control, event.id, set_combobox_select=0
		endif else begin
			(*p).tweek.lines[n] = event.index-1
		endelse
		fit_setup_set_adjust, pstate
		end

	'update-button': begin
		if ((*p).tweek.el ge 0) and ptr_valid( (*p).yields) and ptr_valid( (*p).peaks) then begin
			iz = where( ((*(*p).yields).z eq (*(*p).peaks).z[(*p).tweek.el]) and $
								 ((*(*p).yields).shell eq (*(*p).peaks).shell[(*p).tweek.el]) )
			if iz[0] ne -1 then begin
				iz = iz[0]
				n = min([(*(*p).yields).n_lines[iz],20])
				adj = replicate(1.0,n)
				q = where( (*p).tweek.lines ge 0)
				if q[0] ne -1 then begin
					adj[q] = adj[q] * (*p).tweek.a[ (*p).tweek.lines[q]]
					(*(*p).yields).intensity[q,iz] = adj[q] * (*(*p).yields).intensity[q,iz]
				endif
			endif
		endif else begin
			warning,'fit_setup_event',['Adjust element not set, or fit peaks not defined.', $
						'Need to select adjustment element, lines, and perform the fit first.']
		endelse
		end

	'save-yields-button': begin
		if ptr_valid( (*p).yields) then begin
			file = (*p).yield_file
			path = *(*pstate).path
			F = file_requester( /write, filter = '*.'+yname, $
						file=file, path=path, group=event.top, title='Select the Yields output file name', /fix_filter)
			if F ne '' then begin
				F = strip_file_ext(F) + '.'+yname
				*(*pstate).path = extract_path(F)
				(*p).yield_file = F
				write_yield, (*p).yields, (*p).yield_file
				set_widget_text, (*pstate).yield_file, F
				set_widget_text, (*pstate).yield_file2, F
			endif
		endif
		end

	'fit_TLB': begin
;		print,' the timer event.'
	;	widget_control, event.top, timer=2.0
		end

	'show-df-mode': begin
		(*pstate).show_df = event.index - 1
		widget_control, (*pstate).show_df_mode2, set_combobox_select=0
		widget_control, (*pstate).show_df_mode3, set_combobox_select=0
		fit_setup_do_fit, pstate, p, /update_initial
		end

	'show-df-mode2': begin
		(*pstate).show_df = event.index -1 + 10
		widget_control, (*pstate).show_df_mode, set_combobox_select=0
		widget_control, (*pstate).show_df_mode3, set_combobox_select=0
		if (*pstate).show_df eq 9 then (*pstate).show_df = -1
		fit_setup_do_fit, pstate, p, /update_initial
		end

	'show-df-mode3': begin
		(*pstate).show_df = event.index -1 + 20
		widget_control, (*pstate).show_df_mode2, set_combobox_select=0
		widget_control, (*pstate).show_df_mode, set_combobox_select=0
		if (*pstate).show_df eq 19 then (*pstate).show_df = -1
		fit_setup_do_fit, pstate, p, /update_initial
		end

	'scale-df': begin
		(*pstate).scale_df = 10.0 ^ event.index
		fit_setup_do_fit, pstate, p, /update_initial
		end

	'offset-df': begin
		(*pstate).offset_df = event.index
		fit_setup_do_fit, pstate, p, /update_initial
		end

	'fit-button': begin
		print,'Do fit ...'
		fit_setup_do_fit, pstate, p, /do_fit, /update_background
		end

	'fit-python-button': begin
		print,'Do fit ...'
		fit_setup_do_fit, pstate, p, /do_fit, /update_background, /python
		end

	'fit-all-button': begin
		print,'Do fit to ALL ...'
		if ptr_good( (*pstate).pall) eq 0 then goto, finish
		old_pspec = (*pstate).pspec
		(*pstate).pspec = (*(*pstate).pall)[0]
		
		fit_setup_do_fit, pstate, p, /do_fit, /loop_on, /update_background, $
				first_pass=1, i_loop=0, n_loop=n_elements( *(*pstate).pall)
				
		if ptr_valid( old_pspec) then (*pstate).pspec = old_pspec
		end

	'refit-button': begin
		print,'Do refit ...  select =', (*pstate).select
		if (((*pstate).select ge 0) and ((*pstate).select lt n_elements( *(*pstate).presults))) or $
							(n_elements( *(*pstate).presults) eq 1) then begin
			if (*pstate).select lt 0 then (*pstate).select=0
			if ptr_valid( (*(*pstate).presults)[(*pstate).select]) then begin
				refit = 1
				do_fit = 1
			endif
			fit_setup_do_fit, pstate, p, do_fit=do_fit, refit=refit
			
		endif else begin
			warning,'Fit_setup','Select the desired analysis row first, then use "Refit".'
		endelse
		end

	'dynamic-button': begin
		if (*(*pstate).pspec).charge eq 0 then begin
			warning,'fit_setup',['Spectrum charge "Q" is zero.','Need a valid charge to build DA matrix.']
			goto, finish
		endif
		explanation = ['Generate a "single" DA matrix for DA imaging or a series of DA matrices, one for each energy for XANES imaging.','', $
				'For MPDA an optional "Master Weights Spectrum" can be selected that samples all phases well. Use the "MPDA Master Weights" spectrum plugin ' + $
				'to construct a weights spectrum from a set of region spectra that sample all phases and hotspots well.']
		drop = ['Simple single DA matrix','Single DA matrix (w/ Master Weights)','XANES energy series DA matrix']
		help_drop = 'Select the DA matrix generation mode: (i) a single DA matrix for fluorescence imaging, ' + $
				'(ii) a single DA matrix using a "Master Weights" spectrum for MPDA imaging, or ' + $
				'(iii) a series of DA matrices at selected beam energies for XANES imaging.'
		map_file = [0,1,1]
		file = 'Master/Energies'
		initial_file = (*p).file
		help_file = 'Select the file of "Master Weights" or "XANES beam energies".'
		map_text = [0,0,1]
		text = '        Merge E (keV)'
		initial_text = strtrim(string((*p).E_scatter_merge),2)
		help_text = 'Energy below which Compton and elastic are treated as one called "scatter". Maximum XANES series energy is used for this test.'
		
		r = options_popup( title='Generate DA Matrix', drop=drop, help_drop=help_drop, path=*(*pstate).path, $
								map_file=map_file, file=file, initial_file=initial_file, help_file=help_file, filter=['*.csv'], $
								map_text=map_text, text=text, initial_text=initial_text, help_text=help_text, $
								min_xsize=500, explanation=explanation, error=error)
		if error eq 0 then begin
			if ptr_valid( (*pstate).pback) eq 0 then begin
				(*pstate).da_fresh=0
			endif else begin
				if ptr_valid( (*(*pstate).pback)[0] ) eq 0 then (*pstate).da_fresh=0
			endelse
			if (*pstate).da_fresh eq 0 then begin
				do_fit = 1
				update_background = 1
			endif
			(*p).file = r.file[0]									; save file-name for next time

			master_weights = r.drop[0] eq 1
			if master_weights then begin
				(*(*pstate).p).weight_da_mode = 3					; see 'calc_da_matrix2', uses (*p).file
			endif else begin
				(*(*pstate).p).weight_da_mode = 0
			endelse

			xanes_DA = r.drop[0] eq 2
			if xanes_DA then begin
				*(*(*pstate).p).xanes_energies = get_xanes_energies( (*p).file, do_xanes=do_xanes)
				if do_xanes then begin
					print,'Make XANES series DA ...'
					save_da = 1
				endif else begin
					warning,'fit_setup','Failed to read XANES energies file.'
				endelse
				E = float2(r.text[0])
				if (E gt 4.) and (E lt 20.) then begin
					print,'		use Compton-elastic merge energy =',E
					(*p).E_scatter_merge = E
				endif else begin
					warning,'fit_setup',['Illegal Compton/elastic merge energy.','Ignore.', $
										'Values must be in range: [4,20]']
				endelse
			endif else begin
				print,'Make DA ...'
				save_da = 1
			endelse
			fit_setup_do_fit, pstate, p, do_fit=do_fit, update_background=update_background, $
					save_da=save_da, xanes_DA=xanes_DA
		endif
		end

	'pure-button': begin
		print,'Overlay Pure ...'
		fit_overlay_pure, pstate
		notify, 'fit-display', from=event.top
		goto, finish
		end

	'export-button': begin
		print,'Export DA ...'
		if (*pstate).da_fresh then begin
;			export_da, (*pstate).pall, (*pstate).pda, group=event.top
			export_DA2, (*pstate).pda, group=event.top
		endif else begin
			warning,'fit_setup','Generate DA matrix first.'
		endelse
		goto, finish
		end

	'close-button': begin
		print,'Close fit setup ...'
		goto, kill
		end

	else:
endcase

cont:
	fit_setup_update_pars, pstate, p

finish:
	widget_control, (*pstate).export_button, sensitive=(*pstate).da_fresh
	widget_control, (*pstate).pure_button, sensitive=(*pstate).da_fresh
;	if widget_info(event.top,/update) eq 0 then widget_control, event.top, update=1
	return

bad_pileup:
	warning, 'fit_setup',['No "Image Pileup" spectrum found for spectrum:',(*(*pstate).pspec).label, '', $
			'Generate one for this spectrum using the Spectrum Display','menu "Display->Pileup->from Images".', '', $
			'To avoid these messages, you may','like to close the "PIXE Fit" window.']
	goto, finish
bad_state:
	warning,'fit_setup_event',['STATE variable has become ill-defined.','Abort Fit Setup.'],/error
	goto, kill
bad_ptr:
	warning,'fit_setup_event',['Parameter structure variable has become ill-defined.','Abort Fit Setup.'],/error
	goto, kill

kill:
	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	cancel_notify, event.top

;	These are dangerous, if they are now "owned" by spectrum_display ...
;;	if ptr_valid( (*pstate).pfit) then free_spectrum, (*pstate).pfit
;;	if ptr_valid( (*pstate).pback) then free_spectrum, (*pstate).pback

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).pda) then ptr_free, (*pstate).pda
	if ptr_valid((*pstate).pwiz) then ptr_free, (*pstate).pwiz

	if ptr_valid( (*pstate).pda_pars) then begin
		if ptr_valid( (*(*pstate).pda_pars).peaks) then ptr_free, (*(*pstate).pda_pars).peaks
		ptr_free, (*pstate).pda_pars
	endif

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
return
end

;------------------------------------------------------------------------------------------

pro fit_setup_update_pars, pstate, p

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	if size(*p,/tname) ne 'STRUCT' then return

;#	widget_control, (*pstate).tlb, update=0
	widget_control, (*pstate).elow_text, get_value=s
	(*p).e_low = float(s[0])
	widget_control, (*pstate).ehigh_text, get_value=s
	(*p).e_high = float(s[0])
	widget_control, (*pstate).q_text, get_value=s
	(*p).charge = float(s[0])

;	widget_control, (*pstate).ctail_amp_slider, get_value=v
;	(*p).compton.tail.amp = v

;	if (loop_on eq 0) and ptr_valid( (*pstate).pspec) then begin
;		(*(*pstate).pspec).charge = (*p).charge
;	endif

	widget_control, (*pstate).cuts_file, get_value=F
	F = F[0]
	if F ne (*p).cuts_file then begin
		(*p).cuts_file = F
		cuts = read_cuts(F, error=error)
		if error then begin
			if lenchr(F) gt 0 then begin
;				warning, 'fit_setup',['Error reading CUTs file: '+F,'Make sure path to cuts file is correct.']
			endif
			if ptr_valid((*p).cuts) then ptr_free, (*p).cuts
			(*p).cuts = ptr_new()
		endif else begin
			if ptr_valid((*p).cuts) then ptr_free, (*p).cuts
			(*p).cuts = cuts
		endelse
	endif

	widget_control, (*pstate).yield_file, get_value=F
	F = F[0]
	if F ne (*p).yield_file then begin
		(*p).yield_file = F
		yields = read_yield(F, error=error)
		if error then begin
;			warning, 'fit_setup',['Error reading GeoPIXE Yield file: '+F,'Make sure path to yield file is correct.']
			if ptr_valid((*p).yields) then ptr_free, (*p).yields
			(*p).yields = ptr_new()
		endif else begin
			if ptr_valid((*p).yields) then ptr_free, (*p).yields
			(*p).yields = yields
		endelse
	endif

	det_ok = 0
	if ptr_valid((*p).detector) then begin
		if size(*(*p).detector, /tname) eq 'STRUCT' then det_ok=1
	endif
	widget_control, (*pstate).tail_amp_slider, get_value=v
	if det_ok then (*(*p).detector).tail.amp = v
	widget_control, (*pstate).tail_F_slider, get_value=v
	if det_ok then (*(*p).detector).tail.F = v
	widget_control, (*pstate).tail_B_slider, get_value=v
	if det_ok then (*(*p).detector).tail.B = v
	widget_control, (*pstate).tail_L_slider, get_value=v
	if det_ok then (*(*p).detector).tail.L = v
	widget_control, (*pstate).tail_S_slider, get_value=v
	if det_ok then (*(*p).detector).tail.S = v

	widget_control, (*pstate).Ctail_amp_slider, get_value=v
	(*(*p).Compton).tail.amp = v
	widget_control, (*pstate).Ctail_len_slider, get_value=v
	(*(*p).Compton).tail.len = v
	widget_control, (*pstate).Compton_shift_slider, get_value=v
	(*(*p).Compton).shift = v
	widget_control, (*pstate).Compton_spread_slider, get_value=v
	(*(*p).Compton).spread = v

	widget_control, (*pstate).Curvature_slider, get_value=v
	(*p).Curve = v

	widget_control, (*pstate).back2_split_energy, get_value=v
	(*p).back2_split_energy = float2(v)

;	if widget_info((*pstate).tlb,/update) eq 0 then widget_control, (*pstate).tlb, update=1
	return
end

;------------------------------------------------------------------------------------------

function fit_setup_elements, p, free=el_free, use_m=use_m, OK=OK

	;	Form element list. Remember that shell=0,1,2 means K,L,M, but shell=(3),4,5 means double selection
	;	depending on "use_m". Then need to sort and order by shell.
	;	Q. Has this been done somewhere else?

	COMPILE_OPT STRICTARR

	if ptr_good( p) eq 0 then goto, done
	if n_elements(use_m) eq 0 then use_m=0

	el = ''
	OK = 0
	if ptr_valid((*p).pz_fit) then begin
		z = (*(*p).pz_fit).Z
		shell = (*(*p).pz_fit).Shell
		el_free = replicate(1, n_elements(*(*p).pz_fit))
		OK = 1
	endif
	if ptr_valid((*p).pz_mdl) then begin
		if OK then begin
			z = [ z, (*(*p).pz_mdl).Z ]
			shell = [ shell, (*(*p).pz_mdl).Shell ]
			el_free = [ el_free, replicate(0, n_elements(*(*p).pz_mdl)) ]
		endif else begin
			z = (*(*p).pz_mdl).Z
			shell = (*(*p).pz_mdl).Shell
			el_free = replicate(0, n_elements(*(*p).pz_mdl))
		endelse
		OK = 1
	endif
	if OK then begin
		if use_m then begin
			q = where( shell eq 4, nq)
			if nq gt 0 then begin
				shell[q] = 1
				z = [z, z[q]]
				shell = [shell, replicate(2,nq)]
			endif
			q = where( shell eq 5, nq)
			if nq gt 0 then begin
				shell[q] = 2
				z = [z, z[q]]
				shell = [shell, replicate(3,nq)]
			endif
		endif else begin
			q = where( shell eq 3, nq)
			if nq gt 0 then begin
				shell[q] = 1
				z = [z, z[q]]
				shell = [shell, replicate(2,nq)]
			endif
		endelse
		suffix = ['','','L','M']
		q = sort(10*Z+shell)
		el = element_name(Z[q]) + suffix[shell[q]]
	endif

	done:
	return, el
end

;------------------------------------------------------------------------------------------

pro fit_setup_merge_yields, p, minerals, phase, files, use_m=use_m, error=error

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
			warning,'fit_setup_merge_yields',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif
	error = 1

;	First check all files for correct paths, and prompt for new ones if not found ...

	n_comp = n_elements(minerals)
	path = extract_path( (*p).pcm_file)
	for i=0,n_comp-1 do begin
		F = file_requester( /read, filter='*.yield', path=path, file=files[i], /translate, updir=3, $	
					title='Select new path to yield file for phase: '+minerals[i], /fix_filter, /skip_if_exists)
		if F[0] ne '' then files[i]=F[0]
	endfor

;	To do:
;		1. Form weighted average (1/Y style) of 'yield' and 'intensity' from phases.
;			- Cut down list to selected elements (and add Compton, elastic for SXRF).
;			- Compare elements/shells to  '(*p).yields', to work on common Z,shell indices
;			- Check that these are normalized correctly (major intensity = 1).
;		2. Return resulting yields to (*p).peaks.

	p0 = fit_setup_trim_list( p, (*p).yields, use_m=use_m, OK=OK)
	error = 1-OK
	if error then goto, bad_pyield
	zs0 = intarr(120,4)
	zs0[ (*p0).z, (*p0).shell] = 1
	zs0[0,*] = 0
	qs0 = zs0
	qs0[ (*p0).z, (*p0).shell] = indgen((*p0).n_els)

	py = ptr_new( *p0)
;	(*py).yield[*] = 0.0									; Don't clear all, as some we keep
;	(*py).intensity[*] = 0.0								; such as Back, Compton, elastic, sum, ...
;	(*py).mu_zero[*] = 0.0									; Clear only those referenced below
	unknown = (*py).unknown									; in both source and dest yield arrays.
	cl0 = zs0  &  cl0[*] = 0								; Use 'cl0' 2D array to flag 'already cleared'
	
	pi = ptrarr(n_comp)
	zs = intarr(120,4)
	qs = zs

	for i=0,n_comp-1 do begin
		pt = read_yield( files[i], error=error)
		if error then goto, bad_yield_read
		t = fit_setup_trim_list( p, pt, use_m=use_m, OK=OK)
		error = 1-OK
		if error then goto, bad_phase_trim
		pi[i] = t
		if ptr_valid(pt) then ptr_free, pt
		
		zs[ (*pi[i]).z, (*pi[i]).shell] = 1
		zs[0,*] = 0
		qs[ (*pi[i]).z, (*pi[i]).shell] = indgen((*pi[i]).n_els)
		
		q = where( (zs eq 1) and (zs0 eq 1), nq)			; look for common Z, shell in both yields
		q0 = qs0[q]											; index of common ones in 'p0'
		qi = qs[q]											; index of common ones in 'p[i]'
		qc = where( cl0[q] eq 0, nqc)						; any not cleared yet?
		if nqc gt 0 then begin
			(*py).yield[ q0[qc]] = 0.0						; index into element list: qs0[q[qc]]
			(*py).intensity[*,q0[qc]] = 0.0					; since qs0[q] --> q0, use q0[qc]
			(*py).mu_zero[*,q0[qc]] = 0.0
			cl0[q[qc]] = 1									; clear done once in 2D [z,shell] array
		endif
		zs[*] = 0
		
		for j=0,nq-1 do begin
			(*py).yield[q0[j],unknown-1] += phase[i] / (*pi[i]).yield[qi[j],unknown-1]
			(*py).intensity[*,q0[j]] += phase[i] / ((*pi[i]).yield[qi[j],unknown-1] * (*pi[i]).intensity[*,qi[j]])
			(*py).mu_zero[*,q0[j]] += phase[i] * (*pi[i]).mu_zero[*,qi[j]]
		endfor		
	endfor
	for i=0,n_comp-1 do begin
		if ptr_valid(pi[i]) then ptr_free, pi[i]
	endfor

	(*py).yield[q0,unknown-1] = 1.0 / (*py).yield[q0,unknown-1]
	(*py).intensity[*,q0] = 1.0 / (*py).intensity[*,q0]
	for k=1,max( (*py).n_lines[q0])-1 do begin
		(*py).intensity[k,q0] = (*py).intensity[k,q0] / (*py).intensity[0,q0] 
	endfor
	(*py).intensity[0,q0] = 1.0
	qinf = where( finite((*py).intensity) eq 0, ninf)		; check for Nan, Inf after inverse ...
	if ninf gt 0 then (*py).intensity[qinf] = 0.0 

	if ptr_good((*p).peaks) then ptr_free, (*p).peaks
	(*p).peaks = py
	error = 0

finish:
	return

bad_pyield:
	warning,'fit_setup_merge_yields','Error trimming element list for original (*p).yield?'
	goto, finish
bad_phase_trim:
	warning,'fit_setup_merge_yields','Error trimming element list for phase: '+minerals[i]
	goto, finish
bad_yield_read:
	warning,'fit_setup_merge_yields','Error reading phase yields file: '+files[i]
	goto, finish
end

;------------------------------------------------------------------------------------------

pro fit_setup_phase_yields, p, conc, phase=phase, minerals=minerals, use_m=use_m, error=error

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
			warning,'fit_setup_phase_yields',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif
	if n_elements(use_m) lt 1 then use_m=1
	error = 1
	if ptr_good( p) eq 0 then goto, finish
	
;	To do:
;		1. Calculate phase proportions from 'conc' and R matrix.
;		2. Form weighted average (1/Y style) of 'yield' and 'intensity' from phases.
;		3. Store results in (*p).yields.
;		4. Cut down list to selected elements (and add Compton, elastic for SXRF).
;		5. Set (*p).peaks to this cut-down list and loop.

	el = fit_setup_elements( p, free=el_free, use_m=use_m, OK=OK)
	if OK eq 0 then goto, finish

	pc = (*p).pcorrect
	if ptr_good( pc) eq 0 then goto, finish

	n_comp = (*pc).n_comp
	if n_comp gt 1 then begin
		R = 10000. * transpose( (*pc).R )
	endif else begin
		R = 10000. * (*pc).R
	endelse
	
	; Make transform matrix F, to transform elements to minerals

	if n_comp gt 1 then begin
		F = invert( R, status)
		if status ne 0 then begin
			print,'Bad matrix invert status = ',status
			warning,'fit_setup_phase_yields','Bad matrix invert, status = '+string(status)
			goto, finish
		endif
	endif else begin
		F = 1.0 / R
	endelse

	; Assemble just those relevant elemental images

	C = fltarr( n_comp)

	in = 'Source ='
	out = 'Dest ='
	for i=0L,n_comp-1 do begin
		q = where( strtrim((*pc).comp[i],2) eq el)
		if q[0] eq -1 then begin
			print,'Missing element: ',(*pc).comp[i]
			warning,'fit_setup_phase_yields',['Missing element: ' + (*pc).comp[i], $
				'This element was not found in this fit-setup.','','Abort correction.']
			goto, finish
		endif
		C[i] = conc[q[0]]
		in = in + ' ' + (*pc).comp[i]
		out = out + ' ' + (*pc).minerals[i]
	endfor
	files = [(*pc).files, (*pc).rest]
	minerals = [(*pc).minerals, 'rest']
	
	; Transform the selected elemental conc to minerals.
	; Ignore any negative components for now.

	phase = (C # F) > 0.0
	phase = phase / (total(phase) > 1.)
	
	rest = (1.0 - total(phase)) > 0.0			; remainder
	phase = [reform(phase), rest]

;	print,format='(/A,(T7,20A11))','   el=',el
;	print,format='(A,(T10,20(G10.3,1x)))',' conc=',conc
	print,format='(/A,(T7,20A11))','   min=', minerals
	print,format='(A,(T10,20(G10.3,1x)))',' phase=', phase
	print,' '

	fit_setup_merge_yields, p, minerals, phase, files, use_m=use_m, error=error
	if error then goto, finish
	print,' '

finish:
	return
end

;------------------------------------------------------------------------------------------

pro fit_setup_set_adjust, pstate

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
			warning,'fit_setup_set_adjust',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
	p = (*pstate).p
	if ptr_good(p,/struct) eq 0 then goto, bad_ptr

	if (*pstate).tweek and ptr_valid( (*p).peaks) then begin
		els = ['off', element_name((*(*p).peaks).z)]
		n = n_elements(els)
		while (els[n-1] eq '') do begin
			els = els[0:n-2]
			n = n-1
		endwhile
		i = clip( (*p).tweek.el+1, 0,n-1)
		(*p).tweek.el = i-1
		widget_control, (*pstate).tweek_el, set_value=els, set_combobox_select=i

		if (*p).tweek.el ge 0 then begin
			n_lines = min([(*(*p).peaks).n_lines[(*p).tweek.el],20])
			for i=0L,n_lines-1 do begin
				lin = line_id((*(*p).peaks).lines[i,(*p).tweek.el]) + ' ('+build_result((*(*p).peaks).e[i,(*p).tweek.el])+')'
				widget_control, (*pstate).tweek_labels[i], set_value=lin
			endfor
		endif else begin
			n_lines = 0
		endelse
		if n_lines lt 20 then begin
			for i=n_lines,19 do begin
				widget_control, (*pstate).tweek_labels[i], set_value=' '
				(*p).tweek.lines[i] = -1
				widget_control, (*pstate).tweek_pars[i],set_combobox_select=0
			endfor
		endif
	endif
	return

bad_state:
	warning,'fit_setup_set_adjust',['STATE variable has become ill-defined.','Abort Adjust Setup.'],/error
	return
bad_ptr:
	warning,'fit_setup_set_adjust',['Parameter structure variable has become ill-defined.','Abort Adjust Setup.'],/error
	return
end

;------------------------------------------------------------------------------------------

function fit_setup_trim_list, p, pyields, use_m=use_m, OK=OK

	;	Trim the yields struct for the selected elements.
	;	Add extra lines for XRF for Elastic and Compton.

	COMPILE_OPT STRICTARR
	if n_elements(use_m) lt 1 then use_m=1

	peaks = 0L
	if ptr_good( p) eq 0 then goto, done
	if ptr_good( pyields) eq 0 then goto, done

	OK = 0
	if ptr_valid((*p).pz_fit) then begin
		z = (*(*p).pz_fit).Z
		shell = (*(*p).pz_fit).Shell
		el_free = replicate(1, n_elements(*(*p).pz_fit))
		OK = 1
	endif
	if ptr_valid((*p).pz_mdl) then begin
		if OK then begin
			z = [ z, (*(*p).pz_mdl).Z ]
			shell = [ shell, (*(*p).pz_mdl).Shell ]
			el_free = [ el_free, replicate(0, n_elements(*(*p).pz_mdl)) ]
		endif else begin
			z = (*(*p).pz_mdl).Z
			shell = (*(*p).pz_mdl).Shell
			el_free = replicate(0, n_elements(*(*p).pz_mdl))
		endelse
		OK = 1
	endif
	if OK then begin

		;			This routine now accepts compound shell settings:
		;			"shell=0,1,2" means "K", "L", "M" shells.
		;			If use_m=0: "shell=3" means to fit BOTH K and L lines
		;			If use_m=1: "shell=4,5" means fit BOTH K+L or L+M.

		peaks = select_element_lines( pyields, z, shell, el_free, use_m=use_m)
	endif

done:
	return, peaks
end

;------------------------------------------------------------------------------------------

pro fit_setup_do_fit, pstate, p, do_fit=do_fit, loop_on=loop_on, update_background=update_background, $
		first_pass=first_pass, i_loop=i_loop, n_loop=n_loop, update_initial=update_initial, $
		refit=refit, save_da=save_da, xanes_DA=xanes_DA, python=python, tweak_par=tweak_par, error=error

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
		warning,'fit_setup_do_fit',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
	if n_elements(do_fit) lt 1 then do_fit=0
	if n_elements(refit) lt 1 then refit=0
	if n_elements(loop_on) lt 1 then loop_on=0
	if n_elements(update_background) lt 1 then update_background=0
	if n_elements(update_initial) lt 1 then update_initial=0
	if n_elements(tweak_par) lt 1 then tweak_par=0
	if n_elements(first_pass) lt 1 then first_pass=0
	if n_elements(i_loop) lt 1 then i_loop=0
	if n_elements(n_loop) lt 1 then n_loop=1
	if n_elements(save_da) lt 1 then save_da=0
	if n_elements(xanes_DA) lt 1 then xanes_DA=0
	if n_elements(python) lt 1 then python=0

	if (loop_on eq 0) and ptr_valid( (*pstate).pspec) then begin
		(*(*pstate).pspec).charge = (*p).charge
	endif
	cancel = 0
	error = 1
	
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

	if ptr_good( (*p).yields) eq 0 then return

	pressure = 0.
	temp = 0.
	if do_fit then begin											; changed ambient? Redo yields ...
		if (*(*pstate).pspec).ambient.on then begin
			pressure = (*(*pstate).pspec).ambient.P
			temp = (*(*pstate).pspec).ambient.T
			if ((*p).ambient.on eq 0) or (pressure ne (*p).ambient.P) or (temp ne (*p).ambient.T) then begin
				yield = fit_recalculate_yields( p, peaks=peaks, gamma=(*pstate).gamma, pressure=pressure, temp=temp, error=error)
				if error eq 0 then begin
					if ptr_valid( (*p).yields) then ptr_free, (*p).yields
					(*p).yields = peaks
					use_last = 0
					(*p).ambient = (*(*pstate).pspec).ambient
				endif
			endif
		endif
	endif

	t = fit_setup_trim_list( p, (*p).yields, use_m=(*pstate).use_m, OK=OK)
	if size(t,/tname) eq 'POINTER' then (*p).peaks = t

	fit_setup_set_adjust, pstate

	if loop_on and do_fit then begin
		progress, tlb=progress_tlb, title='GeoPIXE: Spectrum Fit All'
	endif
	silent = 0
	
loop:
	if ptr_valid((*pstate).pspec) then begin
		bad = 0
		if (ptr_valid((*pstate).pback) eq 0) then begin
			bad = 1
		endif else begin
			if (ptr_valid( (*(*pstate).pback)[0] ) eq 0) then begin
				bad = 1
			endif else begin
				if (ptr_valid( (*(*(*pstate).pback)[0]).data ) eq 0) then bad = 1
			endelse
		endelse
		bad_cal = 0
		if (abs((*(*pstate).pspec).cal.poly[1]-1.0) lt 0.001) or  $
				((*(*pstate).pspec).cal.poly[1] lt 1.0e-5) then bad_cal=1

;		if (update_background or bad) and (bad_cal eq 0) and ((*pstate).gamma eq 0) and (save_da eq 0) then begin
		if (update_background or bad) and (bad_cal eq 0) and ((*pstate).gamma eq 0) and ((save_da eq 0) or do_fit) then begin

;	warning,'fit_setup','pback ...'				; debug
			if ptr_valid( (*pstate).plugins) eq 0 then begin
				(*p).background = 0
			endif else begin
				if (*p).background gt n_elements((*(*pstate).plugins).list) then (*p).background=0
			endelse
			sxrf = 0
			if ptr_valid((*p).yields) then begin
				sxrf = ((*(*p).yields).z1 eq 0) and ((*(*p).yields).a1 eq 0)
			endif
			case (*p).background of
				0: begin
					pback = strip_clip( (*pstate).pspec, (*p).e_low, (*p).e_high, $
						passes=(*p).passes, boost=(*p).boost, curve=(*p).curve, $
						filters=(*p).filter, detector=(*p).detector, trim_seb=(*p).trim_seb, $
						yields=(*p).yields, sxrf=sxrf )

					(*pstate).pback = ptr_new(pback)				; don't free it (done below, indirectly).
					(*pstate).pfit = ptr_new()
					end
				else: begin
					pback = call_function( (*(*pstate).plugins).list[(*p).background-1], $
						(*pstate).pspec, (*p).e_low, (*p).e_high, $
						passes=(*p).passes, filters=(*p).filter, yields=(*p).yields, $
						detector=(*p).detector, boost=(*p).boost, curve=(*p).curve )

					(*pstate).pback = ptr_new(pback)				; don't free it (done below, indirectly).
					(*pstate).pfit = ptr_new()
					end
			endcase

			(*pstate).pback = split_back( (*pstate).pspec, (*pstate).pback, do_split=((*p).background2 eq 1), $
									emid = (*p).back2_split_energy)

		endif

		if ((*(*pstate).pspec).n_fit eq 0) and ((*p).pileup_mode eq 1) then goto, bad_pileup
		pfound = 0
		ppileup = ptr_new()
		if (*(*pstate).pspec).n_fit gt 0 then begin
			for i=0L,(*(*pstate).pspec).n_fit-1 do begin
				pf = (*(*pstate).pspec).fit[i]
				if ptr_valid(pf) then begin
					if (*pf).label eq 'Image Pileup' then begin
						pfound = 1
						ppileup = pf
					endif
				endif
			endfor
		endif
		if (pfound eq 0) and ((*p).pileup_mode eq 1) then goto, bad_pileup

		if do_fit and bad_cal then begin
			warning,'fit_setup',['Bad energy calibration.','Spectrum must be calibrated in keV first.'], cancel=cancel
			if cancel then goto, finish
		endif

		if (update_initial or do_fit or tweak_par) and (bad_cal eq 0) then begin			; do the fit
			initial = 1
			if do_fit then initial=0

			if OK eq 0 then goto, more

			if refit then begin
				old = (*(*pstate).presults)[(*pstate).select]
			endif

			tweek_el = -1
			tweek_lines = -1
			if (*pstate).tweek then begin									; lines to tweek intensity
				tweek_el = (*p).tweek.el
				tweek_lines = (*p).tweek.lines
			endif

			if (loop_on) then begin											;@29-3-16
				if ((*p).free[1] eq 1) and ptr_good( (*p).save_detector) then begin
					(*(*p).detector).w0  =  (*(*p).save_detector).w0
					(*(*p).detector).w1  =  (*(*p).save_detector).w1
				endif
			endif
			use_last = (loop_on and (i_loop ne 0))

;			Do the fit. 'use_last' means use geometry factors from last fit, not fitting parameters.

			pfit = pixe_fit( p, (*pstate).pspec, (*pstate).pback, ppileup=ppileup, $
				fix_cal=1-(*p).free[0], fix_fano=1-(*p).free_fano, fix_width=1-(*p).free[1], $
				fix_gain=1-(*p).free_gain, fix_tail=1-(*p).free[2], no_tail=(*p).free[3], dynamic=2, $
				back_mode=(*p).background, pileup_mode=(*p).pileup_mode, sum_deficit=(*p).sum_deficit, gamma=(*pstate).gamma, $
				progress=(loop_on eq 0), initial=initial, refit=refit, use_last=use_last, $
				silent=silent, pcm=(*p).pcm_file, old=old, last_a=(*p).old_a, use_m=(*pstate).use_m, $
				show_df=(*pstate).show_df, scale_df=[(*pstate).scale_df,(*pstate).offset_df], $
				tweek_el=tweek_el, tweek_lines=tweek_lines, mp_loop=(*p).mpda_mode, correct=(*p).pcorrect, $
				results=results, da_pars=da_pars, pressure=pressure, temp=temp, tweak_par=tweak_par, $
				cancel=cancel, python=python, do_split=((*p).background2 eq 1), $
				emid = (*p).back2_split_energy, error=error )

			if python then goto, finish
			if cancel then goto, show
			if error eq 0 then (*pstate).pfit = pfit		; dont't free it (done below, indirectly).
			(*pstate).da_fresh = 0
			if (initial eq 0) then begin
				if ptr_valid( (*pstate).pda_pars) then begin
					if ptr_valid( (*(*pstate).pda_pars).peaks) then ptr_free, (*(*pstate).pda_pars).peaks
					ptr_free, (*pstate).pda_pars
				endif
				if (n_elements(da_pars) gt 0) and ptr_valid(da_pars) then begin
					(*pstate).pda_pars = da_pars
					(*pstate).da_fresh = 1
				endif

				if error eq 0 then begin
					no_results = 0
					if n_elements( *(*pstate).presults)	eq 0 then no_results=1
					if size(results,/tname) ne 'STRUCT' then no_results=1
					if no_results eq 0 then begin
						if ptr_valid( (*(*pstate).presults)[0]) eq 0 then no_results=1
					endif
;					if no_results eq 0 then begin				; commented OFF (30/6/08) - what was it for?
						results.filter.file = (*(*p).filter_list)[(*p).filter_mode]
						(*p).tweek.a = results.tweek.a
;						print,'Tweek A=',(*p).tweek.a
						(*p).old_a = results.nlinear.a			; copy a[] to old_a[]
;					endif
					if no_results then begin
						*(*pstate).presults = ptr_new( results, /no_copy)
					endif else begin
						if refit then begin
							if ((*pstate).select ge 0) and ((*pstate).select lt n_elements( *(*pstate).presults)) then begin
								ptr_free, (*(*pstate).presults)[(*pstate).select]
								(*(*pstate).presults)[(*pstate).select] =  ptr_new( results, /no_copy)
							endif
						endif else begin
							*(*pstate).presults = [ *(*pstate).presults, ptr_new( results, /no_copy)]
						endelse
					endelse
					(*pstate).select = n_elements(*(*pstate).presults) - 1
					if (loop_on eq 0) then notify, 'new-results', from=(*pstate).tlb
				endif
				w1 = (*(*p).detector).w1
				w0 = (*(*p).detector).w0
				FWHM_Mn = 1000.*sqrt(abs( w1 * 5.898 + w0 ))

				widget_control, (*pstate).width_slider, set_value=FWHM_Mn
				widget_control, (*pstate).tail_amp_slider, set_value=(*(*p).detector).tail.amp
				widget_control, (*pstate).tail_L_slider, set_value=(*(*p).detector).tail.L
				widget_control, (*pstate).tail_S_slider, set_value=(*(*p).detector).tail.S
				widget_control, (*pstate).Ctail_amp_slider, set_value=(*(*p).Compton).tail.amp
				widget_control, (*pstate).Ctail_len_slider, set_value=(*(*p).Compton).tail.len
			endif
		endif

		if save_da then begin									; save DA matrix
			if  (*pstate).da_fresh then begin

				if xanes_DA then begin
					pda = calc_da_loop( (*pstate).p, (*pstate).pspec, (*pstate).pback, (*pstate).pda_pars, gamma=(*pstate).gamma, $
								pileup_mode=(*p).pileup_mode, ppileup=ppileup, e_beam=*(*(*pstate).p).xanes_energies, $
								E_scatter_merge=(*p).E_scatter_merge)
				endif else begin
					pda = calc_da_loop( (*pstate).p, (*pstate).pspec, (*pstate).pback, (*pstate).pda_pars, gamma=(*pstate).gamma, $
								pileup_mode=(*p).pileup_mode, ppileup=ppileup)
				endelse
				if ptr_valid( (*pstate).pda) then free_da, (*pstate).pda
				(*pstate).pda = pda
				if (n_elements(pda) gt 0) and ptr_valid(pda) then begin
					file = strip_file_ext((*p).pcm_file)
					if xanes_DA then file = file + '-xanes'
					path = *(*pstate).path
					case dname of 
						'dam': begin
							ext = dname
							if ((*(*(*pstate).p).peaks).z1 eq 0) and ((*(*(*pstate).p).peaks).a1 eq 0) then begin
								ext = 'damx'
							endif
							end
						'damg': begin
							ext = dname
							end
					endcase
					file = file[0] + '.' + ext
					F = file_requester( /write, filter = '*.'+ext, path=path, group=(*pstate).tlb, file=file, $
								title='Save the DA matrix to a "'+ext+'" file', /fix_filter)
					if F ne '' then begin
						F = strip_file_ext(F) + '.'+ext
;;;						*(*pstate).path = extract_path(F)
						(*(*pstate).pda).file = F
						write_DA, (*pstate).pda, F
						(*pstate).DAM_file = F
					endif
				endif
			endif else begin
				warning, 'fit_setup', ['Some parameters have been changed.','Fit the spectrum again first.', $
							'Then generate a DA matrix.']
			endelse
		endif

more:
;		CAREFUL HERE! Remember that some of the (*(*pstate).pspec).fit may still
;		point to current (*pstate).pback or (*pstate).pfit (e.g. if back has not changed).
;		Therefore, we only want to free fit spectra that are NOT the current
;		pfit and pback. Otherwise we junk the current pback, pfit structs in heap.
;		Should really have copied/cloned data into fresh pointers (copy_pointer_data)
;		to avoid this kind of issue.

;		Display order is spec, back[0], fit, back[1], pileup, and these use
;		  the colours:	green, violet,  red, cadmium, l.blue
;		  priority:				3		2		1		0	  (for the fit overlays)

		back_already = 0
		fit_already = 0
		if (*(*pstate).pspec).n_fit gt 0 then begin
			nf = (*(*pstate).pspec).n_fit
			kill = replicate(1,nf)
			priority = kill
			if ptr_valid((*pstate).pback) then begin
				for i=0,n_elements( *(*pstate).pback)-1 do begin
					q = where( ptr_valid( (*(*pstate).pback)[i] ) and $
							((*(*pstate).pback)[i] eq (*(*pstate).pspec).fit[0:nf-1]))	; don't kill current local pback
					if q[0] ne -1 then begin
						kill[q] = 0
						back_already = 1
						priority[q] = (i eq 0) ? 3 : 1
					endif
				endfor
			endif
			q = where( ptr_valid((*pstate).pfit) and $
					((*pstate).pfit eq (*(*pstate).pspec).fit[0:nf-1]))		; don't kill current local fit
			if q[0] ne -1 then begin
				kill[q] = 0
				fit_already = 1
				priority[q] = 2
			endif
			q = where( (*(*pstate).pspec).fit[0:nf-1] eq ppileup  )			; don't kill image pileup
			if q[0] ne -1 then begin
				kill[q] = 0
				priority[q] = 0
			endif

			q = where( kill eq 1)
			if q[0] ne -1 then begin
				ps = ptr_new( (*(*pstate).pspec).fit[q] )
				free_spectra, ps											; only kill foreign stuff
				(*(*pstate).pspec).n_fit = nf-n_elements(q)					; or local already superceded
				qn = where( kill eq 0)
				if qn[0] ne -1 then begin
					(*(*pstate).pspec).fit[0:n_elements(qn)-1] = (*(*pstate).pspec).fit[qn]
					priority = priority[qn]
				endif
			endif
		endif

		n = (*(*pstate).pspec).n_fit
		if (back_already eq 0) and ptr_valid( (*pstate).pback) then begin
			if ptr_valid( (*(*pstate).pback)[0]) then begin
				(*(*pstate).pspec).fit[n] = (*(*pstate).pback)[0]
				if n eq 0 then begin
					priority = 3
				endif else begin
					priority = [priority,3]
				endelse
				(*(*pstate).pspec).n_fit = n+1
				n = n+1
			endif
			if n_elements( *(*pstate).pback) gt 1 then begin				; 2nd back component separately
				if ptr_valid( (*(*pstate).pback)[1]) then begin
					if total( *(*(*(*pstate).pback)[1]).data) gt 0 then begin
						(*(*pstate).pspec).fit[n] = (*(*pstate).pback)[1]
						if n eq 0 then begin
							priority = 1
						endif else begin
							priority = [priority,1]
						endelse
						(*(*pstate).pspec).n_fit = n+1
						n = n+1
					endif
				endif
			endif
		endif
		if (fit_already eq 0) and ptr_valid( (*pstate).pfit) then begin
			(*(*pstate).pspec).fit[n] = (*pstate).pfit
			(*(*pstate).pspec).n_fit = n+1
			if n eq 0 then begin
				priority = 2
			endif else begin
				priority = [priority,2]
			endelse
			n = n+1
		endif
		q = reverse(sort(priority))
		(*(*pstate).pspec).fit[0:n_elements(q)-1] = (*(*pstate).pspec).fit[q]

show:
		if loop_on then begin
			if first_pass then begin
				old_charge = (*(*pstate).pspec).charge
				old_IC_count = (*(*pstate).pspec).IC_total
			endif
			progress, /update, progress_tlb, {unit:0, value:0, current:i_loop, size:n_loop}, cancel=cancel2, skip=skip
			if cancel2 or cancel then goto, show2

			first_pass = 0
			i_loop = i_loop+1
			if i_loop lt n_loop then begin
				if ptr_valid( (*(*pstate).pall)[i_loop]) eq 0 then begin
					warning,'Fit_Setup',['Invalid pointer to spectrum encountered.','Abort fit ALL.']
					goto, finish
				endif
				(*pstate).pspec = (*(*pstate).pall)[i_loop]
				if (skip eq 0) and ((*(*pstate).pspec).charge lt 1.0e-6) then begin
					if (*(*pstate).pspec).IC_total gt 1.e-3 then begin
						APS_COUNT_TO_CHARGE = old_charge / old_IC_count
						(*(*pstate).pspec).charge = APS_COUNT_TO_CHARGE * (*(*pstate).pspec).IC_total
					endif else begin						
						(*(*pstate).pspec).charge = (*p).charge
					endelse
				endif
				if (skip eq 0) then goto, loop
			endif
show2:
			progress, /complete, progress_tlb, 'PIXE Fit completed.'
			notify, 'new-results', from=(*pstate).tlb
		endif
;		print,'fit_setup: send ',n,' fit spectra ...'
		notify, 'fit-display', from=(*pstate).tlb
	endif

finish:
	if loop_on then progress, /ending, progress_tlb
	widget_control, hourglass=0
	return
	
bad_state:
	warning,'fit_setup_do_fit','bad state structure'
	goto, finish
bad_pileup:
	warning, 'fit_setup_do_fit',['No "Image Pileup" spectrum found for spectrum:',(*(*pstate).pspec).label, '', $
			'Generate one for this spectrum using the Spectrum Display','menu "Display->Pileup->from Images".', '', $
			'To avoid these messages, you may','like to close the "PIXE Fit" window.']
	goto, finish
end

;------------------------------------------------------------------------------------------

pro load_pcm_parameters, pstate, file

;	Load the parameters into '(*pstate).p' from 'file' (.pcm)

;	NOTE:	Any changes to PCM file format will also effect the
;			'Crunch' PCM reading routine in \contracts.

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
		warning,'Load_PCM_parameters',['IDL run-time error caught.', '', $
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

	on_ioerror, bad_io
	s1 = ''
	readu,2, s1
	version = 0
	gamma = 0
	detector_file = ''
	if inumeric(s1) then begin
		version = fix(s1)
		if version le -2 then begin
			readu,2, gamma
			if gamma ne (*pstate).gamma then begin
				mess = ((*pstate).gamma eq 0) ? 'Attempt to change from PIXE to PIGE mode.' : 'Attempt to change from PIGE to PIXE mode.'
				warning,'load_pcm_parameters',['Error in PCM file.',mess,'Abort Load operation.']
				goto, done
			endif
		endif
		if version lt 0 then begin
			readu,2, s1
		endif else version = 0
	endif

;	Don't forget to update "Crunch_rio" / "Crunch_rio_equip" too if new versions added ...

	valid = [0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12]
	q = where( version eq valid)
	if q[0] eq -1 then begin
		warning,'load_pcm_parameters',['Error in PCM file.','Bad version number.']
		goto, done
	endif
	widget_control_update, (*pstate).tlb, update=0

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
	filter_update, present=s, new=i, file=F
	if i eq -1 then begin
		F = file_requester(/read, filter='*.filter', path=*(*pstate).path, group=(*pstate).tlb, file=geopixe_root+s, $
					title='select filter file', /fix_filter, /skip_if_exists)
	endif
	dud = 1
	if F[0] ne '' then begin
		filter = read_filters(F[0], error=error)
		if error then begin
			warning, 'load_pcm_parameters',['Error in FILTERS file:',s], /error
		endif else dud=0
	endif
	if ptr_valid((*p).filter) then ptr_free, (*p).filter
	if dud then begin
		(*p).filter = ptr_new()
	endif else begin
		(*p).filter = filter
	endelse

	readu,2, s1
	s = strip_path(s1)
	detector_update, list=list, title=title, present=s, new=i, file=F
	*(*p).detector_list = list
	widget_control, (*pstate).detector_mode, set_value=title

;	if lenchr( (*(*p).detector_list)[0]) gt 0 then begin
;		q = where( s eq *(*p).detector_list)
;		if q[0] ne -1 then begin
;			n = q[0]
;		endif else begin
;			*(*p).detector_list = [*(*p).detector_list,s]
;			n = n_elements(*(*p).detector_list)-1
;		endelse
;	endif else begin
;		*(*p).detector_list = [s]
;	endelse
	if i eq -1 then begin
		warning,'load_pcm_parameters','Detector not found: '+s
;		F = file_requester(/read, filter='*.detector', path=*(*pstate).path, group=(*pstate).tlb, file=geopixe_root+s, $
;					title='select detector file', /fix_filter, /skip_if_exists)
	endif else begin
		widget_control, (*pstate).detector_mode, set_combobox_select=i
		(*p).detector_mode = i
	endelse
	if F[0] ne '' then begin
		detector_file = F[0]
		detector = read_detector(F[0], error=error)
		if error then begin
			warning, 'load_pcm_parameters',['Error in DETECTOR file:',s], /error
			*(*p).detector = 0
		endif else begin
			*(*p).detector = *detector
			*(*p).save_detector = *detector
	
			FWHM_Mn = 1000.*sqrt(abs( (*(*p).detector).w1 * 5.898 + (*(*p).detector).w0 ))
	
			widget_control, (*pstate).width_slider, set_value=FWHM_Mn
			widget_control, (*pstate).tail_amp_slider, set_value=(*(*p).detector).tail.amp
			widget_control, (*pstate).tail_F_slider, set_value=(*(*p).detector).tail.F
			widget_control, (*pstate).tail_B_slider, set_value=(*(*p).detector).tail.B
			widget_control, (*pstate).tail_L_slider, set_value=(*(*p).detector).tail.L
			widget_control, (*pstate).tail_S_slider, set_value=(*(*p).detector).tail.S
		endelse
	endif

	readu,2, s
	(*p).cuts_file = s
	
	F = file_requester( /read, filter='*.cuts', path=*(*pstate).path, group=(*pstate).tlb, file=s, /translate, updir=3, $
				title='select CUTs file', fix_filter=1, /skip_if_exists, /skip_if_null)
	dud = 1
	if F[0] ne '' then begin
		(*p).cuts_file = F[0]
	
		cuts = read_cuts((*p).cuts_file, error=error)
		if error then begin
			if (lenchr(s) gt 0) then begin
				warning, 'load_pcm_parameters',['Error reading CUTS file:',s, $
						'Check that the path to the file is correct.'], /error
			endif
		endif else dud=0
		set_widget_text, (*pstate).cuts_file, (*p).cuts_file
	endif
	if ptr_valid((*p).cuts) then ptr_free, (*p).cuts
	if dud then begin
		(*p).cuts = ptr_new()
	endif else begin
		(*p).cuts = cuts
	endelse
	
	readu,2, s
	(*p).yield_file = s
	
	F = file_requester( /read, filter='*.yield', path=*(*pstate).path, group=(*pstate).tlb, file=s, /translate, updir=3, $
				title='select YIELD file', fix_filter=1, /skip_if_exists)
	dud = 1
	if F[0] ne '' then begin
		(*p).yield_file = F[0]
	
		yields = read_yield((*p).yield_file, error=error)
		if error then begin
			warning, 'load_pcm_parameters',['Error reading Yields file:',s, $
						'Check that the path to the file is correct.'], /error
		endif else begin
			dud = 0
			n = n_elements(*(*p).detector_list)
			if (*yields).array and (n ge 1) then begin
				fdets = strip_path((*yields).detector_file)
				detector_update, list=list, title=title, present=fdets, new=i, file=f
				*(*p).detector_list = list
				widget_control, (*pstate).detector_mode, set_value=title
				if i ne -1 then begin
					(*p).detector_mode = i
					detector_file = f	
					detector = read_detector( f, error=error)
					if error then begin
						warning, 'fit_setup','Error reading Detectors file '+f, /error
					endif else begin
						*(*p).detector = *detector
						*(*p).save_detector = *(*p).detector
	
						widget_control, (*pstate).detector_mode, set_combobox_select=(*p).detector_mode, sensitive=1-(*(*p).detector).array
						widget_control, (*pstate).tail_amp_slider, set_value=(*(*p).detector).tail.amp
						widget_control, (*pstate).tail_F_slider, set_value=(*(*p).detector).tail.F
						widget_control, (*pstate).tail_B_slider, set_value=(*(*p).detector).tail.B
						widget_control, (*pstate).tail_L_slider, set_value=(*(*p).detector).tail.L
						widget_control, (*pstate).tail_S_slider, set_value=(*(*p).detector).tail.S
					endelse
				endif
			endif else begin
				widget_control, (*pstate).detector_mode, sensitive=1-(*yields).array
			endelse
		endelse
	endif
	if ptr_valid((*p).yields) then ptr_free, (*p).yields
	if dud then begin
		(*p).yields = ptr_new()
	endif else begin
		(*p).yields = yields
	endelse
	(*p).ambient.on = 0

	det_ok = 0
	if ptr_valid((*p).detector) then begin
		if size(*(*p).detector, /tname) eq 'STRUCT' then det_ok=1
	endif

	if det_ok then begin
		if (*(*p).detector).array and (strlen((*(*p).detector).layout) gt 0) then begin
			F = file_requester( /read, filter='*.csv', path=extract_path(detector_file), group=(*pstate).tlb, file=(*(*p).detector).layout, /translate, updir=3, $
						title='Select Detector Layout file', fix_filter=1, /skip_if_exists)
			d = read_detector_layout( F, error=error)
			if error eq 0 then begin
				*(*p).playout = d
				(*(*p).detector).layout = F
			endif
		endif
	endif

	set_widget_text, (*pstate).yield_file, (*p).yield_file
	set_widget_text, (*pstate).yield_file2, (*p).yield_file

	if ptr_valid( (*p).pz_fit) then begin
		q = where( (*(*p).pz_fit).z ne 0)
		if q[0] ne -1 then begin
			nq = n_elements(q)
			widget_control, (*pstate).ptable, set_value={Z:(*(*p).pz_fit)[q].z, STATE:replicate(0,nq), ALT:replicate(0,nq)}
		endif
		ptr_free, (*p).pz_fit
	endif
	if ptr_valid( (*p).pz_mdl) then begin
		q = where( (*(*p).pz_mdl).z ne 0)
		if q[0] ne -1 then begin
			nq = n_elements(q)
			widget_control, (*pstate).ptable, set_value={Z:(*(*p).pz_mdl)[q].z, STATE:replicate(0,nq), ALT:replicate(0,nq)}
		endif
		ptr_free, (*p).pz_mdl
	endif

	n = 0L
	readu,2, n
	if n gt 0 then begin
		zs = replicate( {element, Z:0, Shell:0}, n)
		readu,2, zs
		widget_control, (*pstate).ptable, set_value={Z:zs.z, STATE:zs.shell, ALT:replicate(0,n)}
		(*p).pz_fit = ptr_new(zs, /no_copy)
	endif else begin
		(*p).pz_fit = ptr_new()
	endelse

	n = 0L
	readu,2, n
	if n gt 0 then begin
		zs = replicate( {element, Z:0, Shell:0}, n)
		readu,2, zs
		widget_control, (*pstate).ptable, set_value={Z:zs.z, STATE:zs.shell, ALT:replicate(1,n)}
		(*p).pz_mdl = ptr_new(zs, /no_copy)
	endif else begin
		(*p).pz_mdl = ptr_new()
	endelse

	if version eq 0 then begin
		free = [0,0,0]
		readu,2, free
		(*p).free = [free,0]
	endif else begin
		free = [0,0,0,0]				; cal, fwhm, tail, no-tail	flags
		readu,2, free
		(*p).free = free
	endelse
	(*p).tail_mode = ((*p).free[2] eq 0)
	widget_control, (*pstate).tail_mode, set_combobox_select=(*p).tail_mode
	if (*p).free[3] eq 1 then begin
		(*p).tail_mode = 3
		widget_control, (*pstate).tail_mode, set_combobox_select=3
		(*p).free[2] = 0
	endif
	
	(*p).width_mode = ((*p).free[1] eq 0)
	widget_control, (*pstate).width_mode, set_combobox_select=(*p).width_mode

	if ((*p).free[1] eq 0) and (version le -8) then begin
		width = 170.
		readu,2, width

		w1 = (*(*p).detector).w1
		w0 = (width/1000.)^2 - w1 * 5.898
		(*(*p).detector).w0 = w0
		if ptr_good((*pstate).pspec) then begin
			(*p).old_a[3] = 1./(*(*pstate).pspec).cal.poly[1]
			(*p).old_a[0] = sqrt( w0 + w1* (*(*pstate).p).e_low ) * (*p).old_a[3]
		endif

		FWHM_Mn = 1000.*sqrt(abs( (*(*p).detector).w1 * 5.898 + (*(*p).detector).w0 ))
		widget_control, (*pstate).width_slider, set_value=FWHM_Mn
	endif

	if (version le -9) then begin
		free_fano = 0
		readu,2, free_fano
		(*p).free_fano = free_fano
		widget_control, (*pstate).width_options, set_value=(*p).free_fano
	endif

	(*p).cal_mode = ((*p).free[0] eq 0)
	widget_control, (*pstate).cal_mode, set_combobox_select=(*p).cal_mode

	free_gain = 1
	if (version le -10) then begin
		readu,2, free_gain
	endif
	(*p).free_gain = free_gain
	widget_control, (*pstate).cal_options, set_value=(*p).free_gain

	back = 0
	boost = 0
	pileup_mode = 0
	sum_deficit = 0.1
	background2 = 0
	back2_split_energy = 10.0
	if version lt 0 then begin
		readu,2, back
		readu,2, boost
	endif else begin
		readu,2, back
		if back gt 0 then begin
			back = 0
			boost = 1
		endif
	endelse
	if version le -12 then begin
		readu,2, background2, back2_split_energy
	endif
	if version le -3 then begin
		readu,2, pileup_mode
	endif
	if version le -9 then begin
		readu,2, sum_deficit
	endif
	(*p).background = back
	(*p).boost = boost
	(*p).pileup_mode = pileup_mode
	(*p).sum_deficit = sum_deficit
	(*p).background2 = background2
	(*p).back2_split_energy = back2_split_energy
	if (*pstate).gamma eq 0 then begin
		widget_control, (*pstate).boost_mode, set_combobox_select=boost
		widget_control, (*pstate).pileup_mode, set_combobox_select=pileup_mode
		widget_control, (*pstate).sum_deficit, set_value=sum_deficit
		widget_control, (*pstate).back2_mode, set_combobox_select=background2
		widget_control, (*pstate).back2_split_energy, set_value=str_tidy(back2_split_energy)
	endif
	widget_control, (*pstate).fix_free1, set_value=(*p).free[0:1]
	widget_control, (*pstate).fix_free2, set_value=[(*p).free[2],(*p).boost]

	el = 0.0
	eh = 0.0
	readu,2, el, eh
	(*p).e_low = el
	(*p).e_high = eh
	widget_control, (*pstate).elow_text, set_value=str_tidy(el)
	widget_control, (*pstate).ehigh_text, set_value=str_tidy(eh)

	tweek =  {el:-1, lines: replicate(-1,20), a:replicate(1.0,10)}
	if version le -4 then readu,2, tweek
	if (*pstate).tweek then begin
		(*p).tweek = tweek
		for i=0L,19 do begin
			widget_control, (*pstate).tweek_pars[i], set_combobox_select=tweek.lines[i]+1
		endfor
	endif

	c = 1.0003
	seb = 0
	passes = 8
	if version le -5 then begin
		readu,2, c, seb, passes

		if (*p).background ge 1 then begin
			s1 = ''
			readu,2, s1
			q = where( s1 eq (*(*pstate).plugins).title)
			if q[0] ne -1 then begin
				n = q[0]
			endif else begin
				n = 0
			endelse
			(*p).background = n+1
		endif
	endif
	(*p).curve = c
	(*p).trim_seb = seb
	(*p).passes = passes
	if (*pstate).gamma eq 0 then begin
		widget_control, (*pstate).back_mode, set_combobox_select=(*p).background
		widget_control, (*pstate).passes_mode, set_combobox_select=((((*p).passes-1) > 0) < 19)
		widget_control, (*pstate).curvature_slider, set_value=(*p).curve
	endif

	compton_shift = -0.006
	compton_spread = 1.0
	if version le -6 then begin
		readu,2, compton_shift, compton_spread
	endif
	(*(*p).compton).shift = compton_shift
	(*(*p).compton).spread = compton_spread
	widget_control, (*pstate).compton_shift_slider, set_value=compton_shift
	widget_control, (*pstate).compton_spread_slider, set_value=compton_spread

	compton_tail_amp = 1.0
	compton_tail_len = 1.0
	if version le -7 then begin
		readu,2, compton_tail_amp, compton_tail_len
	endif
	(*(*p).compton).tail.amp = compton_tail_amp
	(*(*p).compton).tail.len = compton_tail_len
	widget_control, (*pstate).Ctail_amp_slider, set_value=compton_tail_amp
	widget_control, (*pstate).Ctail_len_slider, set_value=compton_tail_len

	mpda_mode = 0
	correct_file = ''
	if version le -11 then begin
		readu,2, mpda_mode
		if mpda_mode ge 1 then begin
			readu,2, correct_file
		endif
	endif
	(*p).mpda_mode = mpda_mode
	(*p).correct_file = correct_file
	widget_control, (*pstate).mpda_mode, set_combobox_select=(*p).mpda_mode
	widget_control, (*pstate).mpda_base, map=((*p).mpda_mode gt 0)
	widget_control, (*pstate).correct_file, set_value=(*p).correct_file
	
	goto, done

bad:
	warning,'load_pcm_parameters','bad input parameters'
	goto, done
bad_io:
	warning,'load_pcm_parameters','error reading PCM file'
	goto, done
bad_state:
	warning,'load_pcm_parameters','bad state structure'
	goto, done
bad_file:
	warning,'load_pcm_parameters',['error opening file:', file]
	goto, done
bad_ptr:
	warning,'load_pcm_parameters','bad parameter pointer or structure'
	goto, done
done:
	close,2
	widget_control_update, (*pstate).tlb, update=1
	return
	end

;------------------------------------------------------------------------------------------

pro save_pcm_parameters, pstate, file

;	Save the parameters in '(*pstate).p' to 'file' (.pcm)

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
		warning,'Save_PCM_parameters',['IDL run-time error caught.', '', $
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

	version = -12					; remember 'crunch_rio' if version changes

	on_ioerror, bad_file
	close, 1
	openw, 1, file, /xdr

	on_ioerror, bad_io
	writeu,1, string( version)
	writeu,1, (*pstate).gamma
	writeu,1, (*(*p).filter_list)[ (*p).filter_mode]
	writeu,1, (*(*p).detector_list)[ (*p).detector_mode]
	writeu,1, (*p).cuts_file
	writeu,1, (*p).yield_file

	if ptr_valid( (*p).pz_fit) eq 0 then begin
		n = 0
	endif else begin
		n = n_elements( *(*p).pz_fit)
	endelse
	writeu,1, n
	if (n gt 0) then writeu,1, *(*p).pz_fit

	if ptr_valid( (*p).pz_mdl) eq 0 then begin
		n = 0
	endif else begin
		n = n_elements( *(*p).pz_mdl)
	endelse
	writeu,1, n
	if (n gt 0) then writeu,1, *(*p).pz_mdl

	writeu,1, (*p).free
	if ((*p).free[1] eq 0) and (version le -8) then begin
		FWHM_Mn = 1000.*sqrt(abs( (*(*p).detector).w1 * 5.898 + (*(*p).detector).w0 ))
		writeu,1, FWHM_mn
	endif
	writeu,1, (*p).free_fano
	writeu,1, (*p).free_gain
	writeu,1, (*p).background
	writeu,1, (*p).boost
	writeu,1, (*p).background2, (*p).back2_split_energy
	writeu,1, (*p).pileup_mode
	writeu,1, (*p).sum_deficit
	writeu,1, (*p).e_low, (*p).e_high
	writeu,1, (*p).tweek
	writeu,1, (*p).curve, (*p).trim_seb, (*p).passes
	if (*p).background ge 1 then begin
		writeu,1, (*(*pstate).plugins).title[(*p).background-1]
	endif
	writeu,1, (*(*p).compton).shift, (*(*p).compton).spread
	writeu,1, (*(*p).compton).tail.amp, (*(*p).compton).tail.len
	writeu,1, (*p).mpda_mode
	if (*p).mpda_mode ge 1 then begin
		writeu,1, (*p).correct_file
	endif

	close, 1
	return

bad:
	warning,'save_pcm_parameters','bad input parameters'
	goto, done
bad_io:
	warning,'save_pcm_parameters','error writing PCM file'
	goto, done
bad_state:
	warning,'save_pcm_parameters','bad state structure'
	goto, done
bad_file:
	warning,'save_pcm_parameters',['error opening file:', file]
	goto, done
bad_ptr:
	warning,'save_pcm_parameters','bad parameter pointer or structure'
	goto, done
done:
	close,1
	return
	end

;------------------------------------------------------------------------------------------

pro OnRealize_back_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).background
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_back2_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).background2
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_boost_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).boost
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_filter_mode, wWidget

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	n = n_elements(*(*(*pstate).p).filter_list)
	if n lt 1 then goto, done
	(*(*pstate).p).filter_mode = (*(*pstate).p).filter_mode < (n-1)
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).filter_mode
	filter_update, present=(*(*(*pstate).p).filter_list)[(*(*pstate).p).filter_mode], new=i, file=f
	filter = read_filters( f, error=error)
	if error then begin
		warning, 'fit_setup','Realize error in Filter file: '+(*(*(*pstate).p).filter_list)[(*(*pstate).p).filter_mode], /error
	endif else begin
		if ptr_valid((*(*pstate).p).filter) then ptr_free, (*(*pstate).p).filter
		(*(*pstate).p).filter = filter
	endelse
endif

done:
end

;------------------------------------------------------------------------------------------

pro OnRealize_detector_mode, wWidget

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
;#widget_control, top, update=0

if ptr_valid( (*pstate).p) then begin
	n = n_elements(*(*(*pstate).p).detector_list)
	if n lt 1 then goto, done
	(*(*pstate).p).detector_mode = (*(*pstate).p).detector_mode < (n-1)
	detector_update, present=(*(*(*pstate).p).detector_list)[(*(*pstate).p).detector_mode], new=i, file=f
	detector = read_detector( f, error=error)
	if error then begin
		warning, 'fit_setup','Realize error in Detectors file: '+(*(*(*pstate).p).detector_list)[(*(*pstate).p).detector_mode], /error
	endif else begin
		*(*(*pstate).p).detector = *detector
		(*(*(*pstate).p).detector).file = f

		widget_control, (*pstate).tail_amp_slider, set_value=(*detector).tail.amp
		widget_control, (*pstate).tail_F_slider, set_value=(*detector).tail.F
		widget_control, (*pstate).tail_B_slider, set_value=(*detector).tail.B
		widget_control, (*pstate).tail_L_slider, set_value=(*detector).tail.L
		widget_control, (*pstate).tail_S_slider, set_value=(*detector).tail.S

		if (*(*(*pstate).p).detector).layout ne '' then begin
			F2 = file_requester( /read, filter='*.csv', path=extract_path(f), group=(*pstate).tlb, file=(*(*(*pstate).p).detector).layout, /translate, updir=3, $
						title='Select Detector Layout file', fix_filter=1, /skip_if_exists)

			if (*(*(*pstate).p).detector).array and (strlen((*(*(*pstate).p).detector).layout) gt 0) then begin
				(*(*(*pstate).p).detector).layout = F2
				d = read_detector_layout( F2, error=error)
				if error eq 0 then *(*(*pstate).p).playout = d
			endif
		endif
	endelse
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).detector_mode
	if ptr_good((*p).yields) then begin
		widget_control, wWidget, set_combobox_select=i, sensitive=1-(*(*p).yields).array
	endif else begin
		widget_control, wWidget, set_combobox_select=i, sensitive=1
	endelse
endif

done:
;	if widget_info(top,/update) eq 0 then widget_control, top, update=1
end

;------------------------------------------------------------------------------------------

pro OnRealize_fit_tweek_el, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	if ptr_valid( (*(*pstate).p).peaks) then begin
		els = ['off', element_name((*(*(*pstate).p).peaks).z)]
		n = n_elements(els)
		while (els[n-1] eq '') do begin
			els = els[0:n-2]
			n = n-1
		endwhile
		i = clip( (*(*pstate).p).tweek.el+1, 0, n-1)
		(*(*pstate).p).tweek.el = i-1
		widget_control, wWidget, set_value=els, set_combobox_select=i
	endif
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_fit_tweek_label, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = clip(fix(s[0]), 0,19)

if ptr_valid( (*pstate).p) then begin
	if ptr_valid( (*(*pstate).p).peaks) then begin
		if (*(*pstate).p).tweek.el ge 0 then begin
			lin = line_id((*(*(*pstate).p).peaks).lines[n,(*(*pstate).p).tweek.el])
			if lenchr(lin) gt 0 then lin = lin + ' ('+build_result((*(*(*pstate).p).peaks).e[n,(*(*pstate).p).tweek.el])+')'
			widget_control, wWidget, set_value=lin
		endif
	endif
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_Fit_Tweek_par, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_uvalue=u
s = strsplit(string(u),/extract)
n = clip(fix(s[0]), 0,19)

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).tweek.lines[n]+1
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_pileup_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).pileup_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_passes_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=((((*(*pstate).p).passes - 1) > 0) < 19)
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_tail_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).tail_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_cal_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).cal_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_width_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).width_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_fit_mpda_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).mpda_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_fit_correct_file, wWidget

	COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	if ptr_valid( (*pstate).p) then begin
		set_widget_text, wWidget, (*(*pstate).p).correct_file
	endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_yield_mode, wWidget

	COMPILE_OPT STRICTARR
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	if ptr_valid( (*pstate).p) then begin
		widget_control, wWidget, set_combobox_select=(*(*pstate).p).yield_mode
	endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_fit_yield_file, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	set_widget_text, wWidget, (*(*pstate).p).yield_file
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_fit_pcm_file, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	set_widget_text, wWidget, (*(*pstate).p).pcm_file
endif
end

;------------------------------------------------------------------------------------------

pro fit_setup, group_leader=group, TLB=tlb, pspec=pspec, xoffset=xoffset, yoffset=yoffset, $
		pars=p, layer_pars=player, presults=presults, path=path, view=view, _extra=extra, $
		pall=pall, nosav=nosav, gamma=gamma, tweek=tweek, test=test

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
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
		warning,'Fit_setup',['IDL run-time error caught.', '', $
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
if (n_elements(pall) lt 1) then pall=ptr_new()
if ptr_valid(pall) eq 0 then pall = ptr_new()
if n_elements(tweek) lt 1 then tweek=1
if n_elements(test) lt 1 then test=0
t = strip_clip()						; compile SNIP routines

case !version.os_family of
	'MacOS': begin
		trim = 20
		xw = 500
		yh = (gamma eq 1) ? 359 : 384
		xsize_help = 490
		detector_xsize = 136
		filter_xsize = 98
		yield_file_xsize = 229-3*trim
		det_label = 'Detector:'
		space4 = 3
		space5 = 4
		space8 = 8
		boost_xsize = 65
		snip_xsize = 130
		cut_xsize = 320
		pileup_xsize = 140
		slider_xsize = 80
		xsize_flux = 22
		mode_xsize = 320
		xsize_tweek = 38
		ysize_help = 3
		tgeneral = 'General'
		tbackground = 'Back 1'
		tbackground2 = 'Back 2'
		tcals = 'Cal'
		twidths = 'Widths'
		ttails1 = 'Tail 1'
		ttails2 = 'Tail 2'
		tpixe = 'PIXE'
		tsxrf = 'XRF'
		end
	'unix': begin
		trim = 20
		xw = 472
		yh = (gamma eq 1) ? 359 : 454
		xsize_help = 443
		detector_xsize = 136
		filter_xsize = 118
		yield_file_xsize = 209-3*trim
		det_label = 'Det:'
		space4 = 0
		space5 = 0
		space8 = 2
		boost_xsize = 60
		snip_xsize = 130
		cut_xsize = 320
		pileup_xsize = 140
		slider_xsize = 75
		xsize_flux = 22
		mode_xsize = 320
		xsize_tweek = 38
		ysize_help = 3
		tgeneral = 'General'
		tbackground = 'Back 1'
		tbackground2 = 'Back 2'
		tcals = 'Cal'
		twidths = 'Widths'
		ttails1 = 'Tail 1'
		ttails2 = 'Tail 2'
		tpixe = 'PIXE'
		tsxrf = 'XRF'
		end
	else: begin
		trim = 20
		xw = 449
		yh = (gamma eq 1) ? 359 : 422
		xsize_help = 443
		detector_xsize = 136
		filter_xsize = 118
		yield_file_xsize = 209-3*trim
		det_label = 'Detector:'
		space4 = 4
		space5 = 5
		space8 = 8
		boost_xsize = 65
		snip_xsize = 130
		cut_xsize = 320
		pileup_xsize = 140
		slider_xsize = 80
		xsize_flux = 22
		mode_xsize = 320
		xsize_tweek = 38
		ysize_help = 2
		tgeneral = ' General '
		tbackground = ' Back 1 '
		tbackground2 = ' Back 2 '
		tcals = ' Cal '
		twidths = ' Widths '
		ttails1 = ' Tails 1 '
		ttails2 = ' Tails 2 '
		tpixe = ' PIXE '
		tsxrf = ' XRF '
		end
endcase

if gamma then begin
	pname = 'PIGE'
	yname = 'yieldg'
	sname = 'Step'
	xname = 'gamma-ray'
	dname = 'damg'
endif else begin
	pname = 'X-ray'
	yname = 'yield'
	sname = 'Tail'
	xname = 'X-ray'
	dname = 'dam'
endelse

if n_elements(view) lt 1 then view= ((gamma eq 1) ? [50.0,4000.0] : [3.0,38.0])
if n_elements(xoffset) lt 1 then begin
	screen = get_screen_size()
	xoffset = (screen[0] - xw) > 0
endif
if n_elements(yoffset) lt 1 then begin
	screen = get_screen_size()
	yoffset = (screen[1]-28 - yh) > 0
endif

filter_update, list=filter_list, title=filter_title
detector_update,  list=detector_list, title=detector_title

add_plugins = 0
if (nosav eq 0) and (gamma eq 0) then begin
	back_update, list=plugin_list, title=plugin_title
	if plugin_list[0] ne '' then begin
		plugins = ptr_new( {list:plugin_list, title:plugin_title})
		add_plugins = 1
	endif
endif

cont_fit:
if n_elements(player) lt 1 then player = ptr_new(/allocate_heap)

p = bad_pars_struct( p, make_pars=make_p)

; The definitions here also effect Crunch_Rio, Crunch_Rio_Equip struct definition ...

if make_p then begin
	pars = {	$
		pcm_file:		'', $						; PCM file name
		cuts_file:		'', $						; CUTS file name (if used)
		cuts: 			ptr_new(), $				; pointer to cuts struct
		yields:			ptr_new(), $				; pointer to ALL yield struct
		peaks:			ptr_new(), $				; pointer to yields for only selected elements
		pcorrect:		ptr_new(/allocate), $		; pointer to phase correction struct
		presults:		ptr_new(), $				; pointer to array of pointers to results
		yield_file:		'', $						; Yield file name
		yield_mode:		0, $						; current yield set
		mpda_mode:		0, $						; current multiphase fit mode
		correct_file:	'', $						; phase correction file .comat
		pz_fit:			ptr_new(), $				; pointer to fit Z array
		pz_mdl:			ptr_new(), $				; pointer to MDL only Z array
		free:			[1,1,1,0], $				; free certain parameters
		free_fano:		0, $						; free Fano factor
		free_gain:		1, $						; free Cal gain term

		background:		0, $						; background mode
		boost:			0, $						; enable Boost mode
		background2:	0, $						; background2 split mode
		back2_split_energy: 10.0, $					; back 2 E split
		pileup_mode:	0, $						; pileup mode
		sum_deficit:	0.1, $						; pu sum peak deficit (%)
		passes:			8, $						; SNIP passes
		curve:			1.0003, $					; low energy background curvature adjust
		trim_seb:		0, $						; trim secondary electron bremssrahlung hump
		Compton:	ptr_new( {Tail: {Amp:1.0, $		; Compton tail amplitude
									Len:1.0}, $		; Compton tail length
								Shift: -0.006, $	; Compton peak shift adjustment for e-momentum
								Spread: 1.0 }), $	; Compton peak spread adjustment
		E_scatter_merge:	10.0, $					; energy below which Compton and elastic are merged for DA
		xanes_energies:	ptr_new(/allocate_heap), $	; energy list for XANES
		e_low:			view[0], $					; E low (KeV)
		e_high:			view[1], $					; E high
		view:			view, $						; view energy range
		charge:			charge, $					; Q (uC)

		filter_mode:	0, $						; selected filter
		filter:			ptr_new(), $				; pointer to selected filter(s)
		filter_list:	ptr_new(/allocate_heap), $	; pointer to list of filter file names
		detector_mode:	0, $						; selected detector calibration set
		detector:		ptr_new(/allocate_heap), $	; pointer to detector calibration parameters
		detector_list:	ptr_new(/allocate_heap), $	; pointer to list of detector file names
		save_detector:	ptr_new(/allocate_heap), $	; pointer to saved detector parameters
		playout:		ptr_new(/allocate_heap), $	; pointer to detector layout parameters

		ambient:	{	on:		0, $				; ambient conditions detected in last fitted spectrum data
						P:		0.0, $				; pressure (mbar)
						T:		0.0 }, $			; temperature (C)
		cal_mode:		0, $						; selected cal fit mode
		width_mode:		0, $						; selected width fit mode
		tail_mode:		0, $						; selected tail fit mode
		tweek: {el:		-1, $						; element to tweek lines intensities for
			lines:		replicate(-1,20), $			; pars for each line (MUST be 20 elements!)
			a:			replicate(1.0,10)}, $		; latest adjustments
		weight_da_mode:	0, $						; DA matrix weight mode (0=1/f, 1=clipped 'a', 2=unity, 3=master weights spec)
		file:			'', $						; XANES energies file-name or Master Weights spectrum file-name	
		old_a:			fltarr(20) $				; Nonlinear A parameters from last fit.
	}
	*p = pars
endif else begin
	if charge gt 0.00001 then (*p).charge = charge
endelse
*((*p).filter_list) = filter_list
*((*p).detector_list) = detector_list

presults = bad_pars_struct( presults, make_pars=no_results)

; 	top-level base

tlb = widget_base( /column, title=pname+' Spectrum Fit', /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, _extra=extra, uname='fit_TLB', /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center)

; set-up file droplist and buttons

sbase = widget_base( tbase, /row, /base_align_center, ypad=1, xpad=0, space=5)
lab = widget_label( sbase, value='Set-up:')
pcm_file = widget_text( sbase, value=(*p).pcm_file, uname='pcm-file', /tracking, /editable, $
					notify_realize='OnRealize_fit_pcm_file', $
					uvalue='Enter a PCM file-name for source set-up details, or use the "Load" button.',scr_xsize=290)
load_setup_button = widget_button( sbase, value='Load', uname='load-setup-button', /tracking, $
					uvalue='Load set-up parameters from a previous PCM file.', scr_xsize=38)
save_setup_button = widget_button( sbase, value='Save', uname='save-setup-button', /tracking, $
					uvalue='Save set-up parameters to a PCM file.', scr_xsize=38)

; set-up periodic table

if ptr_valid( (*p).pz_fit) then begin
	zon = (*(*p).pz_fit).z
	zstate = (*(*p).pz_fit).shell
endif else begin
	zon = 0
	zstate = 0
endelse
if ptr_valid( (*p).pz_mdl) then begin
	zalt = (*(*p).pz_mdl).z
	zastate = (*(*p).pz_mdl).shell
endif else begin
	zalt = 0
	zastate = 0
endelse

if gamma then begin
	use_m = 0
	ptable = periodic_table( tbase, uname='periodic-table', n_states=2, n_alt_states=2, $
			z_on=zon, z_state=zstate, z_alt=zalt, z_astate=zastate, /no_tiny, /no_ree, $
			/start_Li, /right, legend=['','on','','?'])
endif else begin
	use_m = 1
	ptable = periodic_table( tbase, uname='periodic-table', n_states=6, n_alt_states=3, $
			z_on=zon, z_state=zstate, z_alt=zalt, z_astate=zastate, /no_tiny, $
			/start_Li, /right, legend=['','K','L','M','K,L','L,M','','?K','?L'])
endelse

tab_panel = widget_tab( tbase, location=0, /align_center, uname='tab-panel')


; Setup panel  --------------------------------------------------------------------------------------

setup_base = widget_base( tab_panel, title='    Setup    ', /column, xpad=1, ypad=0, space=0, $
								/align_center, /base_align_center, scr_xsize=xsize_help)

rowbase = widget_base( setup_base, /row, /base_align_center, ypad=0, xpad=0, space=space8)
lbase = widget_base( rowbase, /column, /base_align_right, ypad=2, xpad=0, space=4)
rbase = widget_base( rowbase, /column, /base_align_center, ypad=0, xpad=0, space=1)
row2base = widget_base( rbase, /row, /base_align_center, ypad=0, xpad=0, space=space4)
row3base = widget_base( setup_base, /row, /base_align_center, ypad=0, xpad=0, space=space8)

; Energy range

ebase = widget_base( lbase, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( ebase, value='Energy Range:')
elow_text = widget_text( ebase, value=str_tidy((*p).e_low), uname='elow-text', /tracking, /editable, $
					uvalue='Enter the low energy limit (keV) for fit range, or click "Use View" to transfer from spectrum.', scr_xsize=85-trim)
ehigh_text = widget_text( ebase, value=str_tidy((*p).e_high), uname='ehigh-text', /tracking, /editable, $
					uvalue='Enter the high energy limit (keV) for fit range, or click "Use View" to transfer from spectrum.',scr_xsize=85-trim)
erange_view = widget_button( ebase, value='Use View', uname='use-view-button', /tracking, $
					uvalue='Use the current spectrum display VIEW range as the fitting energy range.', scr_xsize=92-trim)
; detectors, charge

abase = widget_base( lbase, /row, /base_align_center, ypad=0, xpad=0, space=space4)
lab = widget_label( abase, value=det_label)
detector_mode = widget_combobox( abase, value=detector_title, uname='detector-mode', sensitive=1, /tracking, $
					notify_realize='OnRealize_detector_mode', $
					uvalue='Select the relevant detector calibration. For array detectors, this is done in the yield calculation, which then becomes ' + $
					'dependent on take-off angles across the array.', xsize=detector_xsize)
lab = widget_label( abase, value='Q:', scr_xsize=15)
q_text = widget_text( abase, value=str_tidy((*p).charge), uname='q-text', /tracking, /editable, $
					uvalue='Enter the integrated charge (uC). If a flux count (IC counter) is found, ' + $
					'the conversion from IC to Q is calculated for further use.',scr_xsize=80-trim)
flux_Button = Widget_Button(abase, UNAME='flux', VALUE='?', /tracking, xsize=xsize_flux, $
      				uvalue='Open the Ion chamber panel to view the conversion factor (Q/IC), or to set Charge based on another conversion factor.')

; fix/free options, background

fbase = widget_base( row2base, /column, /base_align_left, ypad=0, xpad=0, space=0)
fix_free1 = cw_bgroup2( fbase, ['Cal On','FWHM'], /column, xpad=0, ypad=0, space=0, $
					/return_index, /tracking, $
					uname='fix-free1', set_value=(*p).free[0:1], /nonexclusive, $
					uvalue=['Enable the variation of energy calibration parameters in the fit', $
							'Enable the variation of peak width parameters in the fit'])

f2base = widget_base( row2base, /column, /base_align_left, ypad=0, xpad=0, space=0)
fix_free2 = cw_bgroup2( f2base, [sname+' On','Boost'], /column, xpad=0, ypad=0, space=0, $
					/return_index, /tracking, $
					uname='fix-free2', set_value=[(*p).free[2],(*p).boost], /nonexclusive, $
					uvalue=['Enable the variation of '+sname+' parameters in the fit. Deselect this option for preset tail values.', $
							'Remove absorption and detector efficiency before applying the background algorithm.'])

; filters

dbase = widget_base( row3base, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( dbase, value='Filter:')
filter_mode = widget_combobox( dbase, value=filter_title, uname='filter-mode', /tracking, $
					notify_realize='OnRealize_filter_mode', $
					uvalue='Select the '+xname+' filter used.', xsize=filter_xsize)

; yields

ybase = widget_base( row3base, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( ybase, value='Yields:')
yield_mode = 0L
yield_file = widget_text( ybase, value=(*p).yield_file, uname='yield-file', /tracking, /editable, $
					notify_realize='OnRealize_fit_yield_file', $
					uvalue='Select the '+xname+' production Yield file name, or use the "Load" button to load yields and relative-intensities.',scr_xsize=218-3*trim)
yield_new_button = widget_button( ybase, value='New', uname='yield-new-button', /tracking, scr_xsize=30, $
					uvalue='Go to the Layer popup window to calculated '+xname+' yields and relative-intensities.')
yield_load_button = widget_button( ybase, value='Load', uname='yield-popup-button', /tracking, scr_xsize=38, $
					uvalue='Load a set of precalculated '+xname+' yields and relative-intensities from a '+yname+' file.')


; Composition panel  --------------------------------------------------------------------------------------

loop_base = widget_base( tab_panel, title='  Multiphase Loop  ', /column, xpad=1, ypad=0, space=0, $
	/align_center, /base_align_center, scr_xsize=xsize_help)

colbase = widget_base( loop_base, /column, /base_align_right, ypad=0, xpad=1, space=4)

rowbase1 = widget_base( colbase, /row, /base_align_center, ypad=0, xpad=1, space=space5)
lab = widget_label( rowbase1, value='Mode:')
mpda_mode = widget_combobox( rowbase1, value='    '+['Single phase, single fit','Multiphase fit, loop N=2','Multiphase fit, loop N=3','Multiphase fit, loop N=4','Multiphase fit, loop N=5'], uname='correct-mode', /tracking, $
		notify_realize='OnRealize_fit_mpda_mode', $
		uvalue='Select mode between: Single fit using the "Yields" supplied, or a Multiphase fit loop (uses ' + $
				'fit results to determine phase proportions, refine yields, and repeat fit).', xsize=mode_xsize)

rowbase2 = widget_base( colbase, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( rowbase2, value='Yields:')
yield_file2 = widget_text( rowbase2, value=(*p).yield_file, uname='yield-file', /tracking, /editable, $
		notify_realize='OnRealize_fit_yield_file',scr_xsize=280, $
		uvalue='Select the '+xname+' production Yield file name, or use the "Load" button to load yields and relative-intensities, or "New" to create a new one.')
yield_new_button2 = widget_button( rowbase2, value='New', uname='yield-new-button', /tracking, scr_xsize=30, $
		uvalue='Go to the Layer popup window to calculated '+xname+' yields and relative-intensities.')
yield_load_button2 = widget_button( rowbase2, value='Load', uname='yield-popup-button', /tracking, scr_xsize=38, $
		uvalue='Load a set of precalculated '+xname+' yields and relative-intensities from a '+yname+' file.')

mpda_base = widget_base( colbase, /row, /base_align_center, ypad=0, xpad=0, space=space5, map=((*p).mpda_mode gt 0))
lab = widget_label( mpda_base, value='Multiphase:')
correct_file = widget_text( mpda_base, value=(*p).correct_file, uname='correct-file', /tracking, /editable, $
	notify_realize='OnRealize_fit_correct_file',scr_xsize=280, $
	uvalue='Select the Multiphase yield correction matrix file name, or use the "Load" button to load the composition matrix and yields, or "New" to create a new one.')
yield_new_button2 = widget_button( mpda_base, value='New', uname='correct-new-button', /tracking, scr_xsize=30, $
	uvalue='Go to the Composition Correction popup window to specify phases, a compostion matrix and phase yields and relative-intensities.')
yield_load_button2 = widget_button( mpda_base, value='Load', uname='correct-popup-button', /tracking, scr_xsize=38, $
	uvalue='Load a set of precalculated phases, compostion matrix and phase yields and relative-intensities from a .comat file.')


; Advanced panel  --------------------------------------------------------------------------------------

advanced_base = widget_base( tab_panel, title='    Advanced    ', /column, xpad=0, ypad=0, space=2, $
								/align_center, /base_align_center, scr_xsize=xsize_help)

tab2_panel = widget_tab( advanced_base, location=0, /align_center, scr_xsize=xsize_help-4, uname='advanced-panel')


; ---------------- General panel  -------------------------------------------------------------------

general_base = widget_base( tab2_panel, title=tgeneral, /column, xpad=0, ypad=1, space=3, $
								/align_center, /base_align_center, scr_xsize=xsize_help-6)

g0base = widget_base( general_base, /column, /base_align_center, ypad=0, xpad=0, space=3)

; cuts

cbase = widget_base( g0base, /row, /base_align_right, ypad=0, xpad=0, space=space5)
lab = widget_label( cbase, value='Cuts:')
cuts_file = widget_text( cbase, value=(*p).cuts_file, uname='cuts-file', /tracking, /editable, $
					uvalue='If necessary, select a cuts file to veto selected channels.',scr_xsize=cut_xsize)
cuts_load_button = widget_button( cbase, value='Load', uname='load-cuts-button', /tracking, $
					uvalue='Load cuts details, to veto selected channels, from a CUTS file.', scr_xsize=38)

; Pileup

if gamma eq 0 then begin
	pbase = widget_base( g0base, /row, /base_align_center, ypad=0, xpad=0, space=space5)
	lab = widget_label( pbase, value='Pileup:')
	pileup_mode = widget_combobox( pbase, value=['Line intensities pileup','Image product pileup'], uname='pileup-mode', /tracking, $
					notify_realize='OnRealize_pileup_mode', $
					uvalue='Calculate the pileup spectrum based on: just the lines in the "Spectrum", or using pixel intensity products of the "Images". Model this for spectrum first.', $
					scr_xsize=pileup_xsize)
	lab = widget_label( pbase, value='   Deficit(%):')
	sum_deficit = cw_fslider2( pbase, format='(F8.3)', minimum=0.001, maximum=5.0, layout=1, scroll=0.005, $
				value=((*p).sum_deficit>0.001)<5.0, uname='sum-deficit-slider', xsize=slider_xsize, /tracking, /edit, /drag, $
				uvalue='Adjust the deficit of sum peak amplitudes (%) due to finite pile-up rejection timing resolution and shaping time.')
endif else begin
	pileup_mode = 0L
	sum_deficit = 0L
endelse


; ---------------- Background 1 panel  -------------------------------------------------------------------

background_base = widget_base( tab2_panel, title=tbackground, /column, xpad=0, ypad=1, space=3, $
								/align_center, /base_align_center, scr_xsize=xsize_help-6)

b0base = widget_base( background_base, /column, /base_align_right, ypad=0, xpad=0, space=3)

; background

if gamma eq 0 then begin
	f11base = widget_base( b0base, /row, /base_align_center, ypad=0, xpad=0, space=15)
	lab = widget_label( f11base, value='Background algorithm:')
	back_names = ['SNIP']
	if add_plugins then begin
		back_names = [back_names, plugin_title]
	endif
	back_mode = widget_combobox( f11base, value=back_names, uname='back-mode', /tracking, $
					notify_realize='OnRealize_back_mode', $
					uvalue='Select the background algorithm used. SNIP is built-in. Other backgrounds are loaded from plugins.', xsize=snip_xsize)
	lab = widget_label( f11base, value='Passes:')
	passes_mode = widget_combobox( f11base, value=str_tidy(indgen(20)+1), uname='passes-mode', /tracking, $
					notify_realize='OnRealize_passes_mode', $
					uvalue='Number of passes through the background algorithm. Default SNIP passes is 8.', xsize=boost_xsize)

	f12base = widget_base( b0base, /row, /base_align_center, ypad=0, xpad=0, space=space5)
	lab = widget_label( f12base, value='Boost mode enable:')
	boost_mode = widget_combobox( f12base, value=['Off','On'], uname='boost-mode', /tracking, $
					notify_realize='OnRealize_boost_mode', $
					uvalue='"Boost" indicates remove of absorption and detector efficiency before applying the background algorithm.', xsize=boost_xsize)
endif else begin
	back_mode = 0L
	passes_mode = 0L
	boost_mode = 0L
endelse


; ---------------- Background 2 panel  -------------------------------------------------------------------

background2_base = widget_base( tab2_panel, title=tbackground2, /column, xpad=0, ypad=1, space=3, $
								/align_center, /base_align_center, scr_xsize=xsize_help-6)

b20base = widget_base( background2_base, /column, /base_align_right, ypad=0, xpad=0, space=3)

; background 2

if gamma eq 0 then begin
	f21base = widget_base( b20base, /row, /base_align_center, ypad=0, xpad=0, space=15)
	lab = widget_label( f21base, value='Multiple Background mode:')
	back2_mode = widget_combobox( f21base, value=['Off','Split back'], uname='back2-mode', /tracking, $
					notify_realize='OnRealize_back2_mode', $
					uvalue='Select the optional split background mode. The split background components will be tracked independently in the DA analysis.', scr_xsize=pileup_xsize)

	f22base = widget_base( b20base, /row, /base_align_center, ypad=0, xpad=0, space=space5)
	lab = widget_label( f22base, value='Split energy:')
	back2_split_energy = widget_text( f22base, value=str_tidy((*p).back2_split_energy), uname='back2-split-energy', /tracking, /editable, $
					uvalue='Centre energy around which the background is split into two independent components',scr_xsize=pileup_xsize)
endif else begin
	back2_mode = 0L
	back2_split_energy = 0L
endelse


; ---------------- Cal panel  -------------------------------------------------------------------

cal_base = widget_base( tab2_panel, title=tcals, /column, xpad=0, ypad=0, space=3, $
								/align_center, /base_align_center, scr_xsize=xsize_help-6)

c0base = widget_base( cal_base, /column, /base_align_center, ypad=0, xpad=0, space=2)

c1base = widget_base( c0base, /row, /base_align_center, ypad=0, xpad=0, space=15)
lab = widget_label( c1base, value='Cal Fit Parameters:')
cal_names = ['Free','Fixed']
cal_mode = widget_combobox( c1base, value=cal_names, uname='cal-mode', /tracking, $
				notify_realize='OnRealize_cal_mode', $
				uvalue='Select freedom of energy cal parameters in fit between Free or Fixed to the current values.', xsize=tail_xsize)

lab = widget_label( c1base, value='   ')
cal_options = cw_bgroup2( c1base, ['Free Gain term'], /row, xpad=0, ypad=0, space=0, $
					/return_index, /tracking, $
					uname='free-gain', set_value=[(*p).free_gain], /nonexclusive, $
					uvalue=['Free Gain term (cal energy dependence) in fit.'])


; ---------------- Widths panel  -------------------------------------------------------------------

width_base = widget_base( tab2_panel, title=twidths, /column, xpad=0, ypad=0, space=3, $
								/align_center, /base_align_center, scr_xsize=xsize_help-6)

w0base = widget_base( width_base, /column, /base_align_center, ypad=0, xpad=0, space=2)

w1base = widget_base( w0base, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( w1base, value='Width Fit Parameters:')
width_names = ['Free','Fixed','Default']
width_mode = widget_combobox( w1base, value=width_names, uname='width-mode', /tracking, $
				notify_realize='OnRealize_width_mode', $
				uvalue='Select freedom of peak width parameter (Noise) in fit between Free (potentially), Fixed to the value shown, or internal Defaults.', xsize=tail_xsize)

lab = widget_label( w1base, value='   ')
width_options = cw_bgroup2( w1base, ['Free Fano factor'], /row, xpad=0, ypad=0, space=0, $
					/return_index, /tracking, $
					uname='free-fano', set_value=[(*p).free_fano], /nonexclusive, $
					uvalue=['Free Fano factor (peak width energy dependence) in fit, else use default.'])

w2base = widget_base( w0base, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( w2base, value='Mn K FWHM:')
width_slider = cw_fslider2( w2base, format='(F8.1)', minimum=0.0, maximum=500.0, layout=1, $
				value=160.0, uname='width-slider', xsize=210, /tracking, /edit, /drag, scroll=5.0, $
				uvalue='Adjust the peak width noise parameter in the fit, as expressed as Mn K FWHM (eV).')


; ---------------- Tails 1 panel  -------------------------------------------------------------------

tails1_base = widget_base( tab2_panel, title=ttails1, /column, xpad=0, ypad=0, space=3, $
								/align_center, /base_align_center, scr_xsize=xsize_help-6)

t0base = widget_base( tails1_base, /column, /base_align_center, ypad=0, xpad=0, space=0)

t1base = widget_base( t0base, /row, /base_align_center, ypad=0, xpad=0, space=10)
lab = widget_label( t1base, value='Tail Fit Parameters:')
tail_names = ['Free','Fixed','Default','Zero']
tail_mode = widget_combobox( t1base, value=tail_names, uname='tail-mode', /tracking, $
				notify_realize='OnRealize_tail_mode', $
				uvalue='Select freedom of tail parameters in fit between Free (potentially), Fixed to the value shown, internal Defaults, or Zero.', xsize=tail_xsize)
button = widget_button( t1base, value='Write detector parameters', uname='update-detector-button', /tracking, $
					uvalue='Write the current detector tail parameters (amp, F,B, L,S) to a new detector file.')

t2base = widget_base( t0base, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( t2base, value='Tail Amplitude:')
tail_amp_slider = cw_fslider2( t2base, format='(F9.4)', minimum=0.0, maximum=1.0, layout=1, $
				value=0.0001, uname='tail-amp-slider', xsize=210, /tracking, /edit, /drag, scroll=0.001, $
				uvalue='Adjust the global Tail amplitude parameter to scale peak tail amplitudes in the fit.')


; ---------------- Tails 2 panel  -------------------------------------------------------------------

tails2_base = widget_base( tab2_panel, title=ttails2, /column, xpad=0, ypad=0, space=3, $
								/align_center, /base_align_center, scr_xsize=xsize_help-6)

t20base = widget_base( tails2_base, /column, /base_align_right, ypad=0, xpad=0, space=0)

t21base = widget_base( t20base, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( t21base, value='Amplitude:')
lab = widget_label( t21base, value=' F:')
tail_F_slider = cw_fslider2( t21base, format='(F9.2)', minimum=0.0, maximum=200.0, layout=1, scroll=0.05, $
				value=0.0001, uname='tail-F-slider', xsize=slider_xsize, /tracking, /edit, /drag, $
				uvalue='Adjust the global Tail F parameter (effective charge loss region at front of detector, microns) to affect peak tail amplitudes in the fit.')
lab = widget_label( t21base, value=' B:')
tail_B_slider = cw_fslider2( t21base, format='(F9.2)', minimum=0.0, maximum=10000.0, layout=1, scroll=0.5, $
				value=0.0001, uname='tail-B-slider', xsize=slider_xsize, /tracking, /edit, /drag, $
				uvalue='Adjust the global Tail B parameter (effective charge loss region at back of detector, microns) to affect peak tail amplitudes in the fit.')

t22base = widget_base( t20base, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( t22base, value='Length:')
lab = widget_label( t22base, value=' L:')
tail_L_slider = cw_fslider2( t22base, format='(F9.4)', minimum=0.1, maximum=5.0, layout=1, scroll=0.0049, $
				value=0.1, uname='tail-L-slider', xsize=slider_xsize, /tracking, /edit, /drag, $
				uvalue='Adjust the global Tail Length L (length) parameter. Tail Length = L + S*(E-6.4). If tail length is "Free" both L,S are scaled together in the fit.')
lab = widget_label( t22base, value=' S:')
tail_S_slider = cw_fslider2( t22base, format='(F9.4)', minimum=-1.0, maximum=1.0, layout=1, scroll=0.002, $
				value=0.0001, uname='tail-S-slider', xsize=slider_xsize, /tracking, /edit, /drag, $
				uvalue='Adjust the global Tail Length S (slope) parameter. Tail Length = L + S*(E-6.4). If tail length is "Free" both L,S are scaled together in the fit.')


; ---------------- PIXE panel  -------------------------------------------------------------------

PIXE_base = widget_base( tab2_panel, title=tpixe, /column, xpad=0, ypad=1, space=3, $
								/align_center, /base_align_center, scr_xsize=xsize_help-6)

p0base = widget_base( PIXE_base, /column, /base_align_center, ypad=0, xpad=0, space=1)

p1base = widget_base( p0base, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( p1base, value='Curvature:')
curvature_slider = cw_fslider2( p1base, format='(F9.5)', minimum=1.0, maximum=2.0, layout=1, scroll=0.00005, $
				value=((*p).curve>1.0)<2.0, uname='curvature-slider', xsize=slider_xsize, /tracking, /edit, /drag, $
				uvalue='Adjust the Curvature of the low-energy PIXE background.')
button = widget_button( p1base, value='Default', uname='default-curvature-button', /tracking, $
					uvalue='Set the PIXE low-energy background curvature parameter to its default.')

p2base = widget_base( p0base, /column, /base_align_left, ypad=0, xpad=0, space=0)
pixe_options = cw_bgroup2( p2base, ['Secondary electron bremsstrahlung removal'], /row, xpad=0, ypad=0, space=0, $
					/return_index, /tracking, $
					uname='pixe-options', set_value=[(*p).trim_seb], /nonexclusive, $
					uvalue=['Enable removal of the secondary electron bremsstrahlung hump.'])


; ---------------- XRF panel  -------------------------------------------------------------------

SXRF_base = widget_base( tab2_panel, title=tsxrf, /column, xpad=0, ypad=0, space=3, $
								/align_center, /base_align_center, scr_xsize=xsize_help-6)

s0base = widget_base( SXRF_base, /column, /base_align_right, ypad=0, xpad=0, space=0)

s1base = widget_base( s0base, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( s1base, value='Comp Tail:')
Ctail_amp_slider = cw_fslider2( s1base, format='(F9.3)', minimum=0.001, maximum=5.0, layout=1, scroll=0.05, $
				value=((*(*p).Compton).tail.amp>0.001)<5.0, uname='Ctail-amp-slider', xsize=slider_xsize, /tracking, /edit, /drag, $
				uvalue='Adjust the Compton tail Amplitude parameter to scale Compton peak tail amplitudes in the fit. Parameter is "Free" or "Fixed" according to "Tail 1" tab.')
lab = widget_label( s1base, value=' Shift:')
Compton_shift_slider = cw_fslider2( s1base, format='(F9.4)', minimum=-0.3, maximum=0.15, layout=1, scroll=0.005, $
				value=((*(*p).Compton).shift>(-0.3))<0.15, uname='Compton-shift-slider', xsize=slider_xsize, /tracking, /edit, /drag, $
				uvalue='Adjust the Compton offset effected by sample mean atomic electron momentum.')

s2base = widget_base( s0base, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( s2base, value='Tail Len:')
Ctail_len_slider = cw_fslider2( s2base, format='(F9.3)', minimum=0.1, maximum=5.0, layout=1, scroll=0.05, $
				value=((*(*p).Compton).tail.len>0.1)<5.0, uname='Ctail-len-slider', xsize=slider_xsize, /tracking, /edit, /drag, $
				uvalue='Adjust the Compton tail Length parameter to scale peak tail lengths in the fit. Parameter is "Free" or "Fixed" according to "Tail 1" tab.')
lab = widget_label( s2base, value='Spread:')
Compton_spread_slider = cw_fslider2( s2base, format='(F9.4)', minimum=0.3, maximum=15.0, layout=1, scroll=0.05, $
				value=((*(*p).Compton).spread>0.3)<15.0, uname='Compton-spread-slider', xsize=slider_xsize, /tracking, /edit, /drag, $
				uvalue='Adjust the Compton peak spread effected by mean electron momentum distribution and detection solid-angle.')


; Adjust panel  --------------------------------------------------------------------------------------

tweek_labels = lonarr(20)
tweek_pars = lonarr(20)
tweek_el = 0L
if tweek then begin

adjust_base = widget_base( tab_panel, title='    Adjust    ', /column, xpad=0, ypad=0, space=2, $
								/align_center, /base_align_center, scr_xsize=xsize_help)

; Tweek line intensity parameters ...

	par_list = strtrim(['off',str_tidy(indgen(10)+1)],2)
	t2base = widget_base( adjust_base, /column, xpad=1, ypad=1, space=2, /align_center, /base_align_center)

	telbase = widget_base( t2base, /row, /base_align_center, ypad=0, xpad=0, space=1)
	lab = widget_label( telbase, value='Adjust '+xname+' Lines for Element:', /align_right)
	tweek_el = widget_combobox( telbase, value=['off',' '], uname='tweek-el', /tracking, $
				notify_realize='OnRealize_fit_tweek_el', $
				uvalue='Select the element to adjust line intensities for. ',xsize=60)

	wbase = widget_base( t2base, column=4, /base_align_right, /align_center, ypad=0, xpad=0, space=2)
	for i=0L,19 do begin
		wrbase = widget_base( wbase, /row, /base_align_center, ypad=0, xpad=0, space=2)
		tweek_labels[i] = widget_label( wrbase, value=' ', scr_xsize=65, /align_right, $
					notify_realize='OnRealize_fit_tweek_label',uvalue=str_tidy(i))
		tweek_pars[i] = widget_combobox( wrbase, value=par_list, uname='tweek-par', /tracking, $
					notify_realize='OnRealize_fit_tweek_par', $
					uvalue=str_tidy(i)+' Select the adjustment parameter to tie this line to. ' + $
					'Alpha line should remain fixed. ', xsize=xsize_tweek)
	endfor
	tbbase = widget_base( t2base, /row, /base_align_center, ypad=0, space=5)
	button = widget_button( tbbase, value='Update Relative Intensities', uname='update-button', /tracking, $
					uvalue='Update the Yield relative intensities for this element, in memory only (does not alter the database).')
	lab = widget_label( tbbase, value=' ', scr_xsize=30)
	button = widget_button( tbbase, value='Save Yields', uname='save-yields-button', /tracking, $
					uvalue='Save the updated yields in memory to a YIELD file.')
endif else adjust_base=0L


; Debug panel  --------------------------------------------------------------------------------------

if test then begin
	debug_base = widget_base( tab_panel, title='    Debug    ', /column, xpad=0, ypad=1, space=3, $
								/align_center, /base_align_center, scr_xsize=xsize_help)

; Test show_DF

	xbase = widget_base( debug_base, /row, /align_center, /base_align_center, ypad=0, xpad=0, space=0)
	lab = widget_label( xbase, value='Show DF: ')
	vals = strarr(11)
	vals[0] = 'off'
	vals[1:2] = ['0 Noise','1 Fano factor']
	vals[3:4] = ['2 Cal offset','3 Cal gain']
	vals[5] = (gamma eq 1) ? '4 -free-' : '4 Pileup'
	vals[6:7] = ['5 '+sname+' amplitude','6 '+sname+((gamma eq 1) ? ' energy term' : ' length')]
	vals[8] = (gamma eq 1) ? '7 -free-' : '7 Background scale'
	vals[9:10] = (gamma eq 1) ? ['8 -free-','9 -free-'] : ['8 Comp tail amp','9 Comp tail slope']
	show_df_mode = widget_combobox( xbase, value=vals, uname='show-df-mode', /tracking, $
			uvalue='Select a non-linear parameter to generate a finite-difference differential to compare with the DF function as test.', xsize=105)

	vals = ['1.000','10.00','100.0','1000.']
	vals = [vals, '1.0e+' + str_tidy(indgen(7)+4)]
	lab = widget_label( xbase, value='   Scale by: ')
	scale_df = widget_combobox( xbase, value=vals, uname='scale-df', /tracking, $
			uvalue='Scale the selected DF for display.', xsize=90)
	offset_df = widget_combobox( xbase, value=['No offset','Scale by 2','Scale by 10','Scale by 100','Offset by 1','Offset by 10','Offset by 100','Offset by 1000'], uname='offset-df', /tracking, $
			uvalue='Offset the finite-difference test of derivatives: scale for log-scale, offset for linear.', xsize=90)

	x2base = widget_base( debug_base, /row, /align_center, /base_align_center, ypad=0, xpad=0, space=0)
	lab = widget_label( x2base, value='Adjust line intensity DF: ')
	vals2 = 'Adjust Line ' + str_tidy( indgen(11))
	vals2[0] = 'off'
	show_df_mode2 = widget_combobox( x2base, value=vals2, uname='show-df-mode2', /tracking, $
			uvalue='Select a line intensity adjustment parameter to generate a finite-difference differential to compare with the DF function as test.', xsize=105)

	x3base = widget_base( debug_base, /row, /align_center, /base_align_center, ypad=0, xpad=0, space=0)
	lab = widget_label( x3base, value='Element DF: ')
	vals3 = 'Element ' + str_tidy( indgen(31)-1) + '   k=' + str_tidy( indgen(31)+19)
	vals3[0] = 'off'
	show_df_mode3 = widget_combobox( x3base, value=vals3, uname='show-df-mode3', /tracking, $
			uvalue='Select an element parameter to generate a finite-difference differential to compare with the DF function as test.', xsize=105)
endif else begin
	show_df_mode = 0L
	show_df_mode2 = 0L
	show_df_mode3 = 0L
endelse


;-------------------------------------------------------------------------------------------------------


bbase = widget_base( tlb, /row, /base_align_center, ypad=1, space=2)
lab = widget_label( bbase, value='Fit:')
button = widget_button( bbase, value='One', uname='fit-button', /tracking, $
					uvalue='Perform the non-linear least squares fit to the currently displayed '+xname+' spectrum.')
button = widget_button( bbase, value='All', uname='fit-all-button', /tracking, $
					uvalue='Perform the non-linear least squares fit to ALL loaded '+xname+' spectra.')
lab = widget_label( bbase, value='Spectra.')
if test then begin
	button = widget_button( bbase, value='Pyfit', uname='fit-python-button', /tracking, $
					uvalue='Initiate a fit, but instead of fitting export all lines data, spectrum, background, pileup, etc. to files for Python program.')
endif else begin
	button = widget_button( bbase, value='Refit', uname='refit-button', /tracking, $
					uvalue='Repeat the fit for the spectrum selected in the "Fit Results" window (if selected), and update the current results row.')
endelse
lab = widget_label( bbase, value='', scr_xsize=11)
da_button = widget_button( bbase, value='Generate DA matrix', uname='dynamic-button', /tracking, $
					uvalue='Generate the Dynamic Analysis transform matrix and write a DAM file.')
export_button = widget_button( bbase, value='Export', uname='export-button', /tracking, sensitive=0, $
					uvalue='Export the Dynamic Analysis matrix to an export format file (e.g. for download into a data acquisition system for realtime imaging).')
pure_button = widget_button( bbase, value='P', uname='pure-button', /tracking, sensitive=0, $
					uvalue='Overlay the elemental component spectra on the fit to the spectrum. Need to "Generate DA matrix" first.')
lab = widget_label( bbase, value='', scr_xsize=11)
button = widget_button( bbase, value='Close', uname='close-button', /tracking, $
					uvalue='Exit the Spectrum Fit popup window.')

;.................................................................................

help = widget_text( tlb, scr_xsize=xsize_help, ysize=ysize_help, /wrap, uname='help', /tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)

state = {	$
		path:			ptr_new(path), $		; pointer to current path
		pspec:			pspec, $				; pointer to remote spectrum
		pall:			pall, $					; pointer to all remote spectra
		p:				p, $					; pointer to parameters
		presults:		presults, $				; pointer to array of pointers to results
		player:			player, $				; pointer to layer setup parameters
		pileup:			ptr_new(), $			; pileup table results (from time_amp)

		pfit:			ptr_new(), $			; pointer to fitted spec (these will be freed in spectrum_display)
		pback:			ptr_new(), $			; pointer to pointer(s) to SNIP background spec (
		
		pda:			ptr_new(), $			; pointer to DA matrix
		pda_pars:		ptr_new(), $			; pointer to DA matrix parameters
		DAM_file:		'', $					; last DA matrix file name

		pwiz:			ptr_new(/alloc), $		; pointer for Wizard return

		plugins:		plugins, $				; pointer to background plugins
		gamma:			gamma, $				; PIGE mode

		tweek:			tweek, $				; tweek mode is ON
		tweek_el:		tweek_el, $				; tweek element doplist ID
		tweek_labels:	tweek_labels, $			; tweek pars label IDs
		tweek_pars:		tweek_pars, $			; tweek pars droplist IDs

		da_fresh:		0, $					; flags DA matrix as ready to save
		select:			-1, $					; current row of results for Refit
		tlb:			tlb, $					; top level base ID
		pcm_file:		pcm_file, $				; ID of PCM file text widget
		cuts_file:		cuts_file, $			; ID of CUTS file text widget
		ptable:			ptable, $				; ID of periodic table
		use_m:			use_m, $				; flags use of M shell
		fix_free1:		fix_free1, $			; ID of fix/free cw_bgroup2 widget
		fix_free2:		fix_free2, $			; ID of fix/free cw_bgroup2 widget
		back_mode:		back_mode, $			; ID of background-mode droplist
		boost_mode:		boost_mode, $			; ID of boost-mode droplist
		passes_mode:	passes_mode, $			; ID of passes droplist
		back2_mode:		back2_mode, $			; ID of background2-mode droplist
		back2_split_energy: back2_split_energy, $  ; ID of background2 E split text
		pileup_mode:	pileup_mode, $			; ID of pileup-mode droplist
		sum_deficit:	sum_deficit, $			; ID of sum-deficit text widget
		elow_text:		elow_text, $			; ID of e_low text widget
		ehigh_text:		ehigh_text, $			; ID of e_high text widget
		filter_mode:	filter_mode, $			; ID of filters droplist
		q_text:			q_text, $				; ID of charge text widget
		detector_mode:	detector_mode, $		; ID of detector_modes droplist
		yield_mode:		yield_mode, $			; ID of yields droplist
		yield_file:		yield_file, $			; ID of yields file text widget
		mpda_mode:		mpda_mode, $			; ID of multiphase mode droplist
		mpda_base:		mpda_base, $			; ID of the Mutliphase row base for map
		correct_file:	correct_file, $			; ID of phase correction .comat file
		yield_file2:	yield_file2, $			; ID of yields file text widget (on MPDA tab)
		da_button:		da_button, $			; ID of DAM button
		export_button:	export_button, $		; ID of export button
		pure_button:	pure_button, $			; ID of pure overlay button
		advanced_base:	advanced_base, $		; ID of Advanced tab base widget
		adjust_base:	adjust_base, $			; ID of Adjust tab base widget
		adjust_on:		0, $					; is adjust panel open?
		width_mode:		width_mode, $			; ID of width mode droplist
		width_slider:	width_slider, $			; UD of width slider
		width_options:	width_options,$			; ID of width fix/free widget
		cal_mode:		cal_mode, $				; ID of cal mode droplist
		cal_options:	cal_options,$			; ID of cal gain free widget
		tail_mode:		tail_mode, $			; ID of tail mode droplist
		tail_amp_slider: tail_amp_slider, $		; ID of tail 'amp' slider
		tail_F_slider:	tail_F_slider, $		; ID of tail F slider
		tail_B_slider:	tail_B_slider, $		; ID of tail B slider
		tail_L_slider:	tail_L_slider, $		; ID of tail L slider
		tail_S_slider:	tail_S_slider, $		; ID of tail S slider
		Ctail_amp_slider: Ctail_amp_slider, $	; ID of Compton tail amp slider
		Ctail_len_slider: Ctail_len_slider, $	; ID of Compton tail len slider
		Compton_shift_slider: Compton_shift_slider, $	; ID of Compton shift slider
		Compton_spread_slider: Compton_spread_slider, $	; ID of Compton spread slider
		pixe_options:	pixe_options, $			; ID of the PIXE-options check-boxes
		curvature_slider: curvature_slider, $	; ID of curvature slider
		show_df_mode:	show_df_mode, $			; ID of DF test droplist
		show_df_mode2:	show_df_mode2, $		; ID of DF 2 test droplist
		show_df_mode3:	show_df_mode3, $		; ID of DF 3 test droplist
		show_df:		-1, $					; which parameter to show
		scale_df:		1.0, $					; scale all DFs by this
		offset_df:		0.0, $					; offset DF
		help:			help $					; ID of help text
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

if widget_info( adjust_base, /valid_id) then begin
	g = widget_info( tab_panel, /geometry)
	ga = widget_info( advanced_base, /geometry)
	gx = widget_info( adjust_base, /geometry)
	widget_control, tab_panel, scr_ysize = g.scr_ysize - (gx.scr_ysize-ga.scr_ysize)
	widget_control, tlb, yoffset=yoffset
endif

register_notify, tlb, ['path', $			; new path
				'spectrum-view', $			; view energy range change
				'results-select', $			; row selected in fit results (pass to fit setup for refit)
				'time-amp-pileup', $		; pileup table results from time_amp
				'mark-fit', $				; element select from Identify
				'spectrum-fit'], $			; pointer to new spectrum to fit
				from=group
register_notify, tlb, 'new-detectors'		; new detectors (global notify)
register_notify, tlb, 'new-filters'			; new filters (global notify)
register_notify, tlb, ['wizard-action']		; global notify from a wizard

xmanager, 'fit_setup', tlb, /no_block
return
end
