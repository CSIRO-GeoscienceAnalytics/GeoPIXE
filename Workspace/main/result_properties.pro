;
;   Results properties.

pro result_properties_event, event

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
       warning,'result_properties_event',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, kill
    endif
endif
  widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

no_results = 0
if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
p = (*pstate).presults
if ptr_valid(p) eq 0 then goto, bad_ptr
if size(*p,/tname) ne 'POINTER' then begin
    no_results = 1
endif else begin
    if ptr_valid( (*p)[0] ) eq 0 then no_results=1
    if no_results eq 0 then if size(*(*p)[0],/tname) ne 'STRUCT' then no_results=1
endelse

case tag_names( event,/structure) of
    'NOTIFY': begin
       case event.tag of
         'path': begin
	          if ptr_valid( event.pointer) then begin
       ;        print,'Result_Properties_Event: new path = ',(*event.pointer)
	              *(*pstate).path = (*event.pointer)
	          endif
			  goto, finish
	          end
         'new-results': begin
    ;	       print,'Result_Properties_Event: new results notified.'

	          initial_property_parameters, pstate
			  goto, finish
	          end
         'results-select': begin
    ;	       print,'Result_Properties_Event: new select notified.'
	          if ptr_valid( event.pointer) then begin
	              (*pstate).select = [(*event.pointer).top,(*event.pointer).bottom]
	              initial_property_parameters, pstate
	          endif
			  goto, finish
	          end
         'image-region-select': begin
    ;	       print,'Result_Properties_Event: new region select notified.'
	          if ptr_valid( event.pointer) then begin
	              pregion = (*event.pointer).pregion
	              if (*pregion).detector eq 4 then begin		; STIM data type
					  IF PTR_VALID( (*pregion).conc) then begin
						  (*pstate).emean = (*(*pregion).conc)[0] / 1000.
						  widget_control, (*pstate).emean_text, set_value=str_tidy((*pstate).emean)
					  endif
	              endif
	          endif
			  goto, finish
	          end
		'new-yields': begin						; from layer_setup
			  if ptr_valid( event.pointer) then begin
				F = *event.pointer
				if F ne '' then begin
		          n = lenchr(F)
		          k = lenchr(strip_path(F))
		          widget_control, (*pstate).yield_text, set_value=F, set_text_select=[n-k,k]
		          widget_control, (*pstate).yield_text, set_value=F, set_text_select=[n-1,0]
		          (*pstate).yield_file = F
		          (*pstate).yield_file_changed = 1
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
    'WIDGET_KILL_REQUEST': begin
       print,'Kill request result_properties ...'
       goto, kill
       end
    else:
endcase

uname = widget_info( event.id, /uname)
case uname of

    'analysis-mode': begin
       if (*pstate).mode ne event.index then (*pstate).mode_changed = 1
       (*pstate).mode = event.index
       widget_control, (*pstate).inc_base, map=(*pstate).mode
       widget_control, (*pstate).norm_base, map=1-(*pstate).mode
       end

    'norm-tab-panel': begin
       (*pstate).norm_mode = event.tab
       end

    'beam-mode': begin
       (*pstate).beam_mode_changed = 1
       (*pstate).beam_mode = event.index
       end

    'density-option': begin
       (*pstate).density_option_changed = 1
       (*pstate).density_option = event.index
       widget_control, (*pstate).bubble_base, map=((*pstate).density_option eq 2)
       widget_control, (*pstate).explanation, set_value=(*pstate).notes[(*pstate).density_option]
       if (*pstate).density_option eq 1 then begin
         (*pstate).fluid_type = 0
         widget_control, (*pstate).fluid_type_id, set_combobox_select=0
         (*pstate).fluid_type_changed = 1
       endif
       end

    'fluid-type': begin
       (*pstate).fluid_type_changed = 1
       (*pstate).fluid_type = event.index
       end

    'host-weight-mode': begin
       (*pstate).host_weight_mode_changed = 1
       (*pstate).host_weight_mode = event.index
       end

    'correction-type': begin
    	if (*pstate).correct ne 0 then begin
			if event.index ne (*pstate).correct then begin
				warning,'result_properties',['Cannot change the correction type once applied.', $
							'Go back to the original fits and blanks', 'and re-apply the correction.']
			endif
    	endif else begin
			(*pstate).new_correct = event.index
			(*pstate).correct_changed = 1
    	endelse
       end

    'filter-mode': begin
       (*pstate).filter_mode_changed = 1
       (*pstate).filter_mode = event.index
       end

    'detector-mode': begin
       (*pstate).detector_mode_changed = 1
       (*pstate).detector_mode = event.index
       end

    'yield-new-button': begin
       layer_setup, group_leader=event.top, TLB=tlb, pars=(*pstate).player, path=*(*pstate).path

       register_notify, event.top, [ $
              'path', $						; new path
              'new-yields' $				; new yields file name
              ], from=tlb
       end

    'yield-load-button': begin
       file = find_file2( (*pstate).yield_file)
       path = extract_path( file[0])
       if lenchr(path) eq 0 then path = *(*pstate).path
       F = file_requester( /read, filter = '*.yield', $
         	/must_exist, path=path, group=event.top, $
			title='Select PIXE Yield file', /fix_filter)
       if F ne '' then begin
;      yields = read_yield(F, error=error)
;      if error then begin
;          warning, 'results_properties','Error in PIXE Yield file.', /error
;          goto, finish
;      endif else begin
          n = lenchr(F)
          k = lenchr(strip_path(F))
          widget_control, (*pstate).yield_text, set_value=F, set_text_select=[n-k,k]
          widget_control, (*pstate).yield_text, set_value=F, set_text_select=[n-1,0]
          (*pstate).yield_file = F
          (*pstate).yield_file_changed = 1
;          if ptr_valid((*pstate).yields) then ptr_free, (*pstate).yields
;          (*pstate).yields = yields
;      endelse
       endif else goto, finish
       end

    'yield-file': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then (*pstate).yield_file_changed = 1
       widget_control, event.id, get_value=F
       F = F[0]
       (*pstate).yield_file = F
       if F ne '' then begin
;      yields = read_yield(F, error=error)
;      if error then begin
;          warning, 'fit_setup','Error in PIXE Yield file.', /error
;      endif else begin
          n = lenchr(F)
          k = lenchr(strip_path(F))
          widget_control, (*pstate).yield_text, set_value=F, set_text_select=[n-k,k]
          widget_control, (*pstate).yield_text, set_value=F, set_text_select=[n-1,0]
;          if ptr_valid((*pstate).yields) then ptr_free, (*pstate).yields
;          (*pstate).yields = yields
;      endelse
       endif else goto, finish
       end

    'inc-x-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then (*pstate).inc_x_changed = 1
       end

    'inc-y-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then (*pstate).inc_y_changed = 1
       end

    'inc-t-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then begin
         (*pstate).inc_t_changed = 1
         widget_control, (*pstate).inc_t_text, get_value=s
         t = float(s[0])
         widget_control, (*pstate).inc_m_text, get_value=s
         m = float(s[0])
         d = m - 0.5*t > 0.0
         widget_control, (*pstate).inc_d_text, set_value=str_tidy(d)
         (*pstate).inc_t = t
         (*pstate).inc_d = d
         (*pstate).inc_d_changed = 1
       endif
       end

    'inc-m-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then begin
         (*pstate).inc_m_changed = 1
         widget_control, (*pstate).inc_t_text, get_value=s
         t = float(s[0])
         widget_control, (*pstate).inc_m_text, get_value=s
         m = float(s[0])
         d = m - 0.5*t > 0.0
         widget_control, (*pstate).inc_d_text, set_value=str_tidy(d)
         (*pstate).inc_m = m
         (*pstate).inc_d = d
         (*pstate).inc_d_changed = 1
       endif
       end

    'inc-d-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then begin
         (*pstate).inc_d_changed = 1
         widget_control, (*pstate).inc_m_text, get_value=s
         m = float(s[0])
         widget_control, (*pstate).inc_d_text, get_value=s
         d = float(s[0])
         t = 2.0*(m-d) > 0.0
         widget_control, (*pstate).inc_t_text, set_value=str_tidy(t)
         (*pstate).inc_t = t
         (*pstate).inc_d = d
         (*pstate).inc_t_changed = 1
       endif
       end

    'inc-density-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then (*pstate).inc_density_changed = 1
       end

    'beam-x-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then (*pstate).beam_x_changed = 1
       end

    'beam-y-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then (*pstate).beam_y_changed = 1
       end

    'bubble-diameter-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then (*pstate).bubble_diameter_changed = 1
       end

    'host-formula-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then (*pstate).host_formula_changed = 1
       end

    'host-density-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then (*pstate).host_density_changed = 1
       end

    'scale-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then (*pstate).scale_text_changed = 1
       end

    'tie-formula-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then (*pstate).tie_formula_changed = 1
       end

    'tie-value-text': begin
       if (tag_names( event,/structure) eq 'WIDGET_TEXT_CH') or (tag_names( event,/structure) eq 'WIDGET_TEXT_DEL') then (*pstate).tie_value_changed = 1
       end

    'apply-button': begin
       print,'Apply ...'

       widget_control, /hour
       apply_properties, pstate
       clear_property_changed, pstate
       notify, 'new-results', (*pstate).presults, from=event.top
       end

    'copy-button': begin
       print,'Copy ...'

       end

    'close-button': begin
       print,'Close results properties ...'
       goto, kill
       end

    else:
endcase

finish:
    widget_control, hourglass=0
    return

bad_state:
    warning,'result_properties_event',['STATE variable has become ill-defined.','Abort Results Properties.'],/error
    goto, kill
bad_ptr:
    warning,'result_properties_event',['Parameter structure variable has become ill-defined.','Abort Results Properties.'],/error
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

pro initial_property_parameters, pstate

common c_working_dir, geopixe_root

    no_results = 0
	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
    presults = (*pstate).presults
    if ptr_valid(presults) eq 0 then return
    if size(*presults,/tname) ne 'POINTER' then begin
       no_results = 1
    endif else begin
       if ptr_valid( (*presults)[0] ) eq 0 then no_results=1
       if no_results eq 0 then if size(*(*presults)[0],/tname) ne 'STRUCT' then no_results=1
    endelse
    if no_results then return

    n = n_elements(*presults)
    low = ((*pstate).select[0] > 0) < (n-1)
    high = (((*pstate).select[1] > 0) > low) < (n-1)
    p = (*presults)[low]

    (*pstate).select[0] = low             ; set at least default values
    (*pstate).select[1] = high

    mode = (*p).mode
    matrix = (*p).yield.file
    filter = strip_path( (*p).filter.file)
    detector = strip_path( (*p).detector.file)
    scale = (*p).scale
    filter_mode = 0
    q = where( (*pstate).filter_list eq filter)
    if q[0] ne -1 then filter_mode = q[0]
    detector_mode = 0
    q = where( (*pstate).detector_list eq detector)
    if q[0] ne -1 then detector_mode = q[0]

    inc_d = (*p).yield.thick[0]
    norm = (*p).inclusion.norm
    inc_m = (*p).inclusion.m
    inc_x = (*p).inclusion.x
    inc_y = (*p).inclusion.y
    density_option = (*p).inclusion.option
    inc_density = (*p).inclusion.density > 0.001
    fluid_type = (*p).inclusion.type
    bubble_diameter = (*p).inclusion.bubble
    beam_x = (*p).inclusion.beam.x
    beam_y = (*p).inclusion.beam.y
    beam_mode = (*p).inclusion.beam.shape
    host_weight_mode = (*p).yield.weight[0]
    host_density = (*p).yield.density[0] > 0.001
    host_formula = (*p).yield.formula[0]

    n_layers = n_elements((*p).yield.thick)
    if n_layers ne 3 then mode=0
    if n_layers gt 1 then begin                   ; fluid inclusions
       inc_t = (*p).yield.thick[1]
    endif else begin
       inc_t = 0.0
    endelse
    correct = (*p).correct

	(*pstate).stim_ok = (*p).stim.ok
    widget_control, (*pstate).stim_ok_text, set_value=(*pstate).stim_ok ? ' Yes ' : ' No'
	if (*pstate).stim_ok then begin
		(*pstate).ebeam = (*p).stim.E0
		(*pstate).emean = (*p).stim.Emean
		(*pstate).xmean = (*p).stim.x
	    widget_control, (*pstate).ebeam_text, set_value=str_tidy((*pstate).ebeam)
	    widget_control, (*pstate).emean_text, set_value=str_tidy((*pstate).emean)
	    widget_control, (*pstate).xmean_text, set_value=str_tidy((*pstate).xmean)
	endif else begin
		(*pstate).xmean = 0.0
	    widget_control, (*pstate).xmean_text, set_value=str_tidy((*pstate).xmean)
	endelse

    (*pstate).mode = mode
    (*pstate).density_option = density_option
    (*pstate).fluid_type = fluid_type
    (*pstate).beam_mode = beam_mode
    (*pstate).host_weight_mode = host_weight_mode
    (*pstate).filter_mode = filter_mode
    (*pstate).detector_mode = detector_mode
    (*pstate).correct = correct

    widget_control, (*pstate).analysis_mode, set_combobox_select=mode
    widget_control, (*pstate).norm_base, map=1-mode
    widget_control, (*pstate).inc_base, map=mode
    widget_control, (*pstate).filter_mode_id, set_combobox_select=filter_mode
    widget_control, (*pstate).detector_mode_id, set_combobox_select=detector_mode
    widget_control, (*pstate).yield_text, set_value=matrix
    widget_control, (*pstate).scale_text, set_value=str_tidy(scale)
    widget_control, (*pstate).inc_d_text, set_value=str_tidy(inc_d)
    widget_control, (*pstate).inc_m_text, set_value=str_tidy(inc_m)
    widget_control, (*pstate).inc_x_text, set_value=str_tidy(inc_x)
    widget_control, (*pstate).inc_y_text, set_value=str_tidy(inc_y)
    widget_control, (*pstate).inc_t_text, set_value=str_tidy(inc_t)
    widget_control, (*pstate).inc_density_text, set_value=str_tidy(inc_density)
    widget_control, (*pstate).density_option_id, set_combobox_select=density_option
    widget_control, (*pstate).fluid_type_id, set_combobox_select=fluid_type
    widget_control, (*pstate).bubble_diameter_text, set_value=str_tidy(bubble_diameter)
    widget_control, (*pstate).bubble_base, map=(density_option eq 2)
    widget_control, (*pstate).beam_x_text, set_value=str_tidy(beam_x)
    widget_control, (*pstate).beam_y_text, set_value=str_tidy(beam_y)
    widget_control, (*pstate).beam_mode_id, set_combobox_select=beam_mode
    widget_control, (*pstate).host_weight_mode_id, set_combobox_select=host_weight_mode
    widget_control, (*pstate).host_density_text, set_value=str_tidy(host_density)
    widget_control, (*pstate).host_formula_text, set_value=host_formula
    widget_control, (*pstate).correct_id, set_combobox_select=correct

    clear_property_changed, pstate
    return
end

;------------------------------------------------------------------------------------------

pro apply_properties, pstate

common c_working_dir, geopixe_root

    no_results = 0
	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
    presults = (*pstate).presults
    if ptr_valid(presults) eq 0 then return
    if size(*presults,/tname) ne 'POINTER' then begin
       no_results = 1
    endif else begin
       if ptr_valid( (*presults)[0] ) eq 0 then no_results=1
       if no_results eq 0 then if size(*(*presults)[0],/tname) ne 'STRUCT' then no_results=1
    endelse
    if no_results then return

    n = n_elements(*presults)
    low = ((*pstate).select[0] > 0) < (n-1)
    high = (((*pstate).select[1] > 0) > low) < (n-1)
    (*pstate).select[0] = low             ; set at least default values
    (*pstate).select[1] = high
;   if ((*pstate).select[0] eq -1) or ((*pstate).select[1] eq -1) then return

    low = ((*pstate).select[0] > 0) < (n-1)
    high = (((*pstate).select[1] > 0) > low) < (n-1)

    get_properties, pstate

	case (*pstate).mode of
      0: begin                           ; thick targets

		if (*pstate).filter_mode_changed then begin
			filter_update, present=(*pstate).filter_list[(*pstate).filter_mode], new=i, file=f
			gfilter = read_filters( f, error=error)
			if error then begin
				warning, 'apply_properties','Error in Filter file '+f, /error
				goto, finish
			endif
		endif
		if (*pstate).detector_mode_changed then begin
			detector_update, present=(*pstate).detector_list[(*pstate).detector_mode], new=i, file=f
			gdetector = read_detector( f, error=error)
			if error then begin
				warning, 'apply_properties','Error in detector file '+f, /error
				goto, finish
			endif
		endif
		if (*pstate).yield_file_changed then begin
			gyield = read_yield((*pstate).yield_file, error=error)
			if error then begin
				warning, 'apply_properties','Error in PIXE/SXRF Yield file.', /error
				goto, finish
			endif
		endif
		end

      1: begin                           ; fluid inclusions

		if (*pstate).filter_mode_changed then begin
			filter_update, present=(*pstate).filter_list[(*pstate).filter_mode], new=i, file=f
			gfilter = read_filters( f, error=error)
			if error then begin
				warning, 'apply_properties','Error in Filter file '+(*pstate).filter_list[(*pstate).filter_mode], /error
				goto, finish
			endif
		endif
		if (*pstate).detector_mode_changed then begin
			detector_update, present=(*pstate).detector_list[(*pstate).detector_mode], new=i, file=f
			gdetector = read_detector( f, error=error)
			if error then begin
				warning, 'apply_properties','Error in detector file '+(*pstate).detector_list[(*pstate).detector_mode], /error
				goto, finish
			endif
		endif

		fluids = ['(H2O)97.5(NaCl)2.5','(H2O)92.5(NaCl)7.5','(H2O)85(NaCl)15','(H2O)75(NaCl)25', $
						'(H2O)65(NaCl)35','(H2O)55(NaCl)45','(H2O)40(NaCl)60']
		end
	  else:
	endcase

	if (*pstate).correct_changed and (high ne low) then begin
		high = low
		warning,'apply_properties','Can only apply a correction to the first analysis selcted.'
	endif

    for i=low,high do begin
       p = (*presults)[i]
       (*p).filter.file = strip_path( (*p).filter.file)
       (*p).detector.file = strip_path( (*p).detector.file)
       force = (*p).force

       if ((*pstate).mode ne (*p).mode) then begin
         warning, 'apply_properties',['The number of layers cannot be changed here.', $
                   'Re-fit the spectrum using the correct number of layers.']
              goto, finish
       endif

       case (*pstate).mode of
         0: begin                            ; thick targets

			if (*pstate).filter_mode_changed then begin
				filter = gfilter
				(*p).filter.file = (*pstate).filter_list[(*pstate).filter_mode]
				(*p).filter.name = (*filter)[0].name
			endif else begin
				filter_update, present=(*p).filter.file, new=i, file=f
				filter = read_filters( f, error=error)
				if error then begin
					warning, 'apply_properties','Error in Filter file '+f, /error
					goto, finish
				endif
			endelse
			if (*pstate).detector_mode_changed then begin
				detector = gdetector
				(*p).detector.file = (*pstate).detector_list[(*pstate).detector_mode]
				(*p).detector.name = (*detector).crystal.name
			endif else begin
				detector_update, present=(*p).detector.file, new=i, file=f
				detector = read_detector( f, error=error)
				if error then begin
					warning, 'apply_properties','Error in detector file '+f, /error
					goto, finish
				endif
			endelse
			if (*detector).array and (strlen((*detector).layout) gt 0) then begin
				layout = read_detector_layout((*detector).layout, error=error)
				if error then begin
					warning, 'apply_properties','Bad detector layout.',/error
					goto, finish
				endif
				playout = ptr_new( layout)
			endif else playout = 0L
          
			if (*pstate).yield_file_changed then begin

              pt = select_element_lines( gyield, (*p).el.z, (*p).el.shell, (*p).el.mask, /use_m)

;             This has potential to build a different element list ...
;             Need to align this with original element list.
;             This has been done by:
;			  (i)   setting emin, emax to the values used originally, stored in (*p).yield.emin...
;			  (ii)  including geo_yield [emin,emax] range in make_peaks, and saved yields.
;			  This may not apply to some old yield files.
;			  (iii) the copy below does NOT copy the emin, emax from this new yield file.
;			        instead is uses the old one, so their use plus selec_z will select the
;				    same element list when a new yield calc is performed below.

              (*p).yield.title = (*pt).title
              (*p).yield.file = (*pt).file
              (*p).yield.yield = (*pt).yield
              (*p).yield.z1 = (*pt).z1
              (*p).yield.a1 = (*pt).a1
              (*p).yield.e_beam = (*pt).e_beam
              (*p).yield.state = (*pt).state
              (*p).yield.theta = (*pt).theta
              (*p).yield.phi = (*pt).phi
              (*p).yield.alpha = (*pt).alpha
              (*p).yield.beta = (*pt).beta
              (*p).yield.unknown = (*pt).unknown
              (*p).yield.formula = (*pt).formula
              (*p).yield.weight = (*pt).weight
              (*p).yield.thick = (*pt).thick
              (*p).yield.microns = (*pt).microns
              (*p).yield.density = (*pt).density
              (*p).beam = (*pt).beam
          endif
          if (*pstate).scale_text_changed then (*p).scale = (*pstate).scale

;		Correct sample thickness based on STIM mean energy loss.
;		This uses an extended yield file with a series of yields calculated for a range of thicknesses.

			if (*pstate).norm_mode eq 1 then begin

			  (*pstate).stim_ok = 0
			  widget_control, (*pstate).stim_ok_text, set_value=' No '
			  (*p).stim.ok = 0

			  E0 = (*pstate).ebeam
			  Eav = (*pstate).emean
			  if E0 lt 0.01 or Eav lt 0.01 then begin
			  	warning,'apply_properties',['Enter beam energy in the entry field.', $
			  					'And select a Region evaluated on a STIM cut mean image,', $
			  					'to update the mean STIM transmission energy,', $
			  					'or enter the transmitted energy directly.']
				goto, finish
			  endif
              emin = (*p).yield.emin
              emax = (*p).yield.emax
              if emin lt 0.1 then emin=1.0
              if emax lt 0.1 then emax=50.0
              (*p).yield.emin = emin
              (*p).yield.emax = emax
              (*p).beam.energy = E0

;			  Changed the way Ebeam is set. Did use the value in original Yield calc.
;			  Now we change this to what the user enters for E0.

              yield = geo_array_yield( (*p).yield.formula, 200., microns=(*p).yield.microns, $
                   density=(*p).yield.density, weight=(*p).yield.weight, beam=(*p).beam, $
                   energy=E0, theta=(*p).yield.theta, phi=(*p).yield.phi, $
                   alpha=(*p).yield.alpha, beta=(*p).yield.beta, unknown=(*p).yield.unknown, $
                   z1=(*p).yield.z1, a1=(*p).yield.a1, state=(*p).yield.state, $
                   intensity=intensity, lines=lines, n_lines=n_lines, z2=z2, shell=shell, $
                   e_min=emin, e_max=emax, layers=layers, select=(*p).el.z, $
                   gamma=gamma, e_lines=e_lines, x_slow=x, e_slow=e, $
                   array=(*p).array.on, detector=detector, layout=playout, $
                   mu_zero=mu_zero, ratio_yield=ratio_yield, ratio_intensity=ratio_intensity, error=error )
              if error then begin
                 warning, 'apply_properties','Error calculating thick dE/dx yields.',/error
				 goto, finish
              endif
              if (Eav lt min(e)) or (Eav gt max(e)) then begin
                 warning, 'apply_properties','Nominated beam energy is out of range of sample energy curve.',/error
				 goto, finish
              endif
              
			  xmean = interpol(x,e, Eav, /spline)

			  (*pstate).xmean = xmean
			  widget_control, (*pstate).xmean_text, set_value=string(xmean)

			  (*p).stim.E0 = E0
			  (*p).stim.Emean = Eav
			  (*p).stim.x = xmean

			  (*p).yield.thick = xmean

              yield = geo_array_yield( (*p).yield.formula, (*p).yield.thick, microns=(*p).yield.microns, $
                   density=(*p).yield.density, weight=(*p).yield.weight, beam=(*p).beam, $
                   energy=E0, theta=(*p).yield.theta, phi=(*p).yield.phi, $
                   alpha=(*p).yield.alpha, beta=(*p).yield.beta, unknown=(*p).yield.unknown, $
                   z1=(*p).yield.z1, a1=(*p).yield.a1, state=(*p).yield.state, $
                   intensity=intensity, lines=lines, n_lines=n_lines, z2=z2, shell=shell, $
                   e_min=emin, e_max=emax, layers=layers, select=(*p).el.z, $
                   gamma=gamma, e_lines=e_lines, $
                   array=(*p).array.on, detector=detector, layout=playout, $
                   mu_zero=mu_zero, ratio_yield=ratio_yield, ratio_intensity=ratio_intensity, error=error )

              if error then goto, finish
              
              pyield = make_peaks( z2=z2, shell=shell, n_lines=n_lines, lines=lines, intensity=intensity, yield=yield, $
                   layers=layers, unknown=(*p).yield.unknown, e_beam=E0, theta=(*p).yield.theta, beam=(*p).beam, $
                   phi=(*p).yield.phi, alpha=(*p).yield.alpha, beta=(*p).yield.beta, $
                   title=(*p).yield.title, formula=(*p).yield.formula, thick=(*p).yield.thick, $
                   microns=(*p).yield.microns, density=(*p).yield.density, $
                   weight=(*p).yield.weight, z1=(*p).yield.z1, a1=(*p).yield.a1, $
                   state=(*p).yield.state, e_min=emin, e_max=emax, e_lines=e_lines, $
                   array=(*p).array.on, detector_file=(*detector).file, $
                   mu_zero=mu_zero, ratio_yield=ratio_yield, ratio_intensity=ratio_intensity, /pointer)

              pt = select_element_lines( pyield, (*p).el.z, (*p).el.shell, (*p).el.mask, /use_m)
              if ptr_valid(pt) eq 0 then begin
                 warning, 'apply_properties',['Error calculating yields.','Error in select_element_lines.'],/error
				 goto, finish
              endif

;             This has potential to build a different element list ...
;             Need to align this with original element list.
;             This has been done by:
;			  (i)  setting emin, emax to the values used originally, stored in (*p).yield.emin...
;			  (ii) including geo_yield [emin,emax] range in make_peaks, and saved yields.
;			  This may not apply to some old yield files.

			  (*pstate).stim_ok = 1
			  widget_control, (*pstate).stim_ok_text, set_value=' Yes '
			  (*p).stim.ok = 1

              ny = n_elements((*pt).yield[*,0])
              (*p).yield.yield[0:ny-1,0] = (*pt).yield[*,0]
			  (*p).yield.e_beam = E0
			  (*p).yield.thick = (*pt).thick
			endif

			array = (*p).array.on
			n_det = (*p).array.n_det
			Y = (*p).yield.yield[*,0]
			active = (*p).array.active
			e = (*p).el.e
			
			yield = array_yield( pdetector=detector, playout=playout, pfilter=filter, $
						Energy=e, array=array, charge=(*p).spectrum.charge, active=active, /refit, $
						n_det=n_det, multiplicity=(*p).spectrum.multiplicity, rGamma=(*p).array.rGamma, $
						Y=Y, n_els=(*p).n_els, theta=(*p).yield.theta, phi=(*p).yield.phi, $
						counts_per_ppm_uc=counts_per_ppm_uc, error=error)
			if error then begin
				warning,'result_properties','error returned by "array_yield".'
				goto, finish
			endif
          
          (*p).conc = (*p).scale * (*p).deadtime_correction * float( (*p).area / yield)
          (*p).error = (*p).scale * float( (*p).aerror / yield)
          (*p).mdl = (*p).scale * float( (*p).amdl / yield)

          if (*pstate).tie_formula_changed and (lenchr((*pstate).tie_formula) gt 0) then begin

;	Need to work out scaling/norm for a Tie applied to a formula here ...

          endif

          if (*pstate).filter_mode_changed eq 0 then ptr_free, filter
          if (*pstate).detector_mode_changed eq 0 then ptr_free, detector
          end

         1: begin                            ; fluid inclusions

          n_layers = n_elements((*p).yield.thick)
          if n_layers ne 3 then begin
              warning, 'apply_properties',['Incorrect number of layers in the fit for a fluid inclusion.', $
                   'Return to X-ray Fit/Yield Calculation Setup and set-up a 3 layer target.']
			goto, finish
          endif
		  if ((*pstate).new_correct ne (*p).correct) and ((*p).correct ne 0) then begin
         		warning, 'apply_properties',['The type of correction cannot be changed here.', $
                   'Re-fit the original spectrum and then apply the new correction.']
              goto, finish
       	  endif
		  if (*pstate).correct_changed then begin
			  nr = n_elements(*presults)
			  if ((*pstate).new_correct eq 1) then begin
			  	if (i+1 ge nr) then begin
         		  warning, 'apply_properties',['This type of correction requires a fit to a blank as the next row of results.', $
                  	 'Append a fit to the blank as the next results row, then try the correction.']
					 goto, finish
			    endif else begin
				  p1 = (*presults)[i+1]
				  q1 = cross_reference( (*p).el.name, (*p1).el.name)
			    endelse
			  endif
			  if ((*pstate).new_correct eq 2) then begin
			  	if (i+2 ge nr) then begin
         		  warning, 'apply_properties',['This type of correction requires a fit to a matrix blank as the next row of results.', $
         		  	 'both fitted using a fluid inclusion set-up, and a fit to the matrix blank using a thick matrix set-up.', $
                  	 'Append the fits to the blank as the next results rows, then try the correction.']
					 goto, finish
			    endif else begin
				  p1 = (*presults)[i+1]
				  p2 = (*presults)[i+2]
				  q1 = cross_reference( (*p).el.name, (*p1).el.name)
				  q2 = cross_reference( (*p).el.name, (*p2).el.name)
			    endelse
			  endif
       	  endif

          if (*pstate).filter_mode_changed then begin
              filter = gfilter
              (*p).filter.file = (*pstate).filter_list[(*pstate).filter_mode]
              (*p).filter.name = (*filter)[0].name
          endif else begin
		  	  filter_update, present=(*p).filter.file, new=i, file=f
              filter = read_filters( f, error=error)
              if error then begin
                 warning, 'apply_properties','Error in Filter file '+f, /error
				 goto, finish
              endif
          endelse
          if (*pstate).detector_mode_changed then begin
              detector = gdetector
              (*p).detector.file = (*pstate).detector_list[(*pstate).detector_mode]
              (*p).detector.name = (*detector).crystal.name
          endif else begin
		  	  detector_update, present=(*p).detector.file, new=i, file=f
              detector = read_detector( f, error=error)
              if error then begin
                 warning, 'apply_properties','Error in detector file '+f, /error
				 goto, finish
              endif
          endelse

;          Recalculate yields if any of these change:
;             inc_t, inc_d, inc_m, inc_density, fluid_type,
;             host_formula, host_weight_mode, host_density

          if (*pstate).inc_t_changed or (*pstate).inc_d_changed or (*pstate).inc_m_changed or $
              (*pstate).inc_density_changed or (*pstate).fluid_type_changed or $
              (*pstate).host_formula_changed or (*pstate).host_weight_mode_changed or $
              (*pstate).host_density_changed or force then begin

              if (*pstate).inc_m_changed then (*p).inclusion.m = (*pstate).inc_m

              host_formula = (*p).yield.formula[0]
              inc_formula = (*p).yield.formula[1]
              if (*pstate).host_formula_changed then host_formula = (*pstate).host_formula
              if (*pstate).fluid_type_changed then begin
                 (*p).inclusion.type = (*pstate).fluid_type
                 inc_formula = fluids[(*pstate).fluid_type]
              endif
              (*p).yield.formula = [host_formula,inc_formula,host_formula]

              host_weight_mode = (*p).yield.weight[0]
              inc_weight_mode = (*p).yield.weight[1]
              if (*pstate).host_weight_mode_changed then host_weight_mode = (*pstate).host_weight_mode
              (*p).yield.weight = [host_weight_mode,inc_weight_mode,host_weight_mode]

              thick = (*p).yield.thick
              if (*pstate).inc_d_changed then thick[0] = (*pstate).inc_d
              if (*pstate).inc_t_changed then thick[1] = (*pstate).inc_t
              (*p).yield.thick = thick

              dens = (*p).yield.density
              if (*pstate).host_density_changed then begin
                 dens[0] = (*pstate).host_density          ; dens[1] stays as used in original
                 dens[2] = (*pstate).host_density          ; layer model
              endif
              if (*pstate).inc_density_changed then begin
                 (*p).inclusion.density = (*pstate).inc_density
                 dens[1] = (*pstate).inc_density
              endif
              (*p).yield.density = dens

              (*p).yield.title = 'Fluid inclusion, d=' + str_tidy((*p).yield.thick[0]) + $
                           ', T=' + str_tidy((*p).yield.thick[1]) + ', fluid= ' + $
                           inc_formula + ', host= ' + host_formula
              (*p).yield.file = ''
              (*p).yield.unknown = 2
              (*p).yield.microns = [1,1,(*p).yield.microns[2]]

              emin = (*p).yield.emin
              emax = (*p).yield.emax
              if emin lt 0.1 then emin=1.0
              if emax lt 0.1 then emax=50.0
              (*p).yield.emin = emin
              (*p).yield.emax = emax
              gamma = 0
              if ((*p).type eq 1) or ((*p).el.line[0] eq 'gamma') then begin
                 gamma = 1
              endif

              yield = geo_array_yield( (*p).yield.formula, (*p).yield.thick, microns=(*p).yield.microns, $
                   density=(*p).yield.density, weight=(*p).yield.weight, beam=(*p).beam, $
                   energy=(*p).yield.e_beam, theta=(*p).yield.theta, phi=(*p).yield.phi, $
                   alpha=(*p).yield.alpha, beta=(*p).yield.beta, unknown=(*p).yield.unknown, $
                   z1=(*p).yield.z1, a1=(*p).yield.a1, state=(*p).yield.state, $
                   intensity=intensity, lines=lines, n_lines=n_lines, z2=z2, shell=shell, $
                   e_min=emin, e_max=emax, layers=layers, select=(*p).el.z, $
                   gamma=gamma, e_lines=e_lines, error=error )

              if error then begin
                 warning, 'apply_properties','Error calculating yields.',/error
				 goto, finish
              endif

              pyield = make_peaks( z2=z2, shell=shell, n_lines=n_lines, lines=lines, intensity=intensity, yield=yield, $
                   layers=layers, unknown=(*p).yield.unknown, e_beam=(*p).yield.e_beam, theta=(*p).yield.theta, $
                   phi=(*p).yield.phi, alpha=(*p).yield.alpha, beta=(*p).yield.beta, beam=(*p).beam, $
                   title=(*p).yield.title, formula=(*p).yield.formula, thick=(*p).yield.thick, $
                   microns=(*p).yield.microns, density=(*p).yield.density, $
                   weight=(*p).yield.weight, z1=(*p).yield.z1, a1=(*p).yield.a1, $
                   state=(*p).yield.state, e_min=emin, e_max=emax, e_lines=e_lines, /pointer)

              pt = select_element_lines( pyield, (*p).el.z, (*p).el.shell, (*p).el.mask, /use_m)
              if ptr_valid(pt) eq 0 then begin
                 warning, 'apply_properties',['Error calculating yields.','Error in select_element_lines.'],/error
				 goto, finish
              endif

;             This has potential to build a different element list ...
;             Need to align this with original element list.
;             This has been done by:
;			  (i)  setting emin, emax to the values used originally, stored in (*p).yield.emin...
;			  (ii) including geo_yield [emin,emax] range in make_peaks, and saved yields.
;			  This may not apply to some old yield files.
;			* Better to scan element list and match them up to avoid mismatches (later).

              ny = n_elements((*pt).yield[*,1])
              (*p).yield.yield[0:ny-1,1] = (*pt).yield[*,1]
          endif

;          Recalculate norm geometry factors if any of these change:
;             inc_x, inc_y, inc_t, inc_m, density_option, inc_density,
;             fluid_type, bubble_diameter, host_formula, host_weight_mode,
;             host_density, beam_x, beam_y, beam_mode

          e = (*p).el.e

          if force or (*pstate).inc_x_changed or (*pstate).inc_y_changed or (*pstate).inc_t_changed or $
              (*pstate).inc_m_changed or (*pstate).density_option_changed or $
              (*pstate).inc_density_changed or (*pstate).fluid_type_changed or $
              (*pstate).bubble_diameter_changed or (*pstate).host_formula_changed or $
              (*pstate).host_weight_mode_changed or (*pstate).host_density_changed or $
              (*pstate).beam_x_changed or (*pstate).beam_y_changed or (*pstate).beam_mode_changed then begin

              if (*pstate).inc_x_changed then (*p).inclusion.x = (*pstate).inc_x
              if (*pstate).inc_y_changed then (*p).inclusion.y = (*pstate).inc_y
              if (*pstate).density_option_changed then (*p).inclusion.option = (*pstate).density_option
              if (*pstate).bubble_diameter_changed then (*p).inclusion.bubble = (*pstate).bubble_diameter
              if (*pstate).beam_x_changed then (*p).inclusion.beam.x = (*pstate).beam_x
              if (*pstate).beam_y_changed then (*p).inclusion.beam.y = (*pstate).beam_y
              if (*pstate).beam_mode_changed then (*p).inclusion.beam.shape = (*pstate).beam_mode

              for j=0L,(*p).n_els-1 do begin
                 (*p).inclusion.norm[j] = fluid( e[j], (*p).inclusion.beam.x, (*p).inclusion.beam.y, $
                    (*p).inclusion.x, (*p).inclusion.y, (*p).yield.thick[1], $
                    (*p).inclusion.density, (*p).yield.density[1], (*p).yield.formula[0], (*p).yield.density[0], $
                    (*p).inclusion.bubble, bubble_mode=(*p).inclusion.option, $
                    beam_mode=(*p).inclusion.beam.shape, host_weight=(*p).yield.weight[0], $
                    error=error )
                 if error then goto, finish
              endfor
          endif

          gamma = 0
          if ((*p).type eq 1) or ((*p).el.line[0] eq 'gamma') then gamma = 1
          if gamma then begin
              omega = 4.0 * !pi * 1000.
          endif else begin
              omega = 1000.0 * !pi * (*detector).diameter * (*detector).diameter / $
                   (4.0 * (*detector).distance * (*detector).distance)
          endelse
;         eff = det_eff( detector, e)
;         T = transmit( filter, e, gamma=(*detector).pige)
          eff = det_eff( detector, e, external_filters=filter, gamma=(*detector).pige)
          T = 1.
          Y = (*p).yield.yield[*,1]                    ; unknown = 2
          counts_per_ppm_uc = float(omega * eff * T * Y)

;		Correct for background effects: 1) scatter, 2) matrix
;		assume that these spectra/anyses come next in results,
;		and they were collected with the SAME detector and filters

		  if (*pstate).correct_changed and ((*pstate).new_correct ne 0) then begin
			  r1 = (*p).spectrum.charge/(*p1).spectrum.charge
			  i2 = 1

			  case (*pstate).new_correct of
			  	1: begin
			  		q = where( q1 ne -1)
					(*p).area[q] = (*p).area[q] - (*p1).area[q1[q]] * r1
					(*p).aerror[q] = sqrt((*p).aerror[q]*(*p).aerror[q] + (*p1).aerror[q1[q]]*(*p1).aerror[q1[q]] * r1*r1)
			  		end
			  	2: begin
				    r2 = (*p).spectrum.charge/(*p2).spectrum.charge
					y2 = (*p2).yield.yield[*,0]
			  		q = where( (q1 ne -1) and (q2 ne -1))
					(*p).area[q] = (*p).area[q] - (*p1).area[q1[q]] * r1 + (*p2).area[q2[q]] * r2 * (y[q]/(*p).inclusion.norm[q])/y2[q2[q]]
					d2 = (*p2).aerror[q2[q]] * r2 * (y[q]/(*p).inclusion.norm[q])/y2[q2[q]]
					(*p).aerror[q] = sqrt((*p).aerror[q]*(*p).aerror[q] + (*p1).aerror[q1[q]]*(*p1).aerror[q1[q]] * r1*r1 + d2*d2)
					i2 = 2
			  		end
			   endcase
			   (*pstate).correct = (*pstate).new_correct
			   (*p).correct = (*pstate).correct

				*(*pstate).pdelete = [i+1,i+i2]
				notify, 'delete-results', (*pstate).pdelete, from=(*pstate).tlb
		  endif

          yield = counts_per_ppm_uc * (*p).spectrum.charge
          (*p).conc = (*p).inclusion.norm * float( (*p).area / yield)
          (*p).error = (*p).inclusion.norm * float( (*p).aerror / yield)
          (*p).mdl = (*p).inclusion.norm * float( (*p).amdl / yield)
          end
         else:
       endcase

    endfor

finish:
	if ptr_valid(gdetector) then ptr_free, gdetector
	if ptr_valid(gfilter) then ptr_free, gfilter
	if ptr_valid(gyield) then ptr_free, gyield
	if ptr_valid(detector) then ptr_free, detector
	if ptr_valid(filter) then ptr_free, filter
	if ptr_valid(pt) then ptr_free, pt
	if ptr_valid(playout) then ptr_free, playout
    return
    end

;------------------------------------------------------------------------------------------

pro get_properties, pstate

    widget_control, (*pstate).yield_text, get_value=s
    (*pstate).yield_file = s
    widget_control, (*pstate).scale_text, get_value=s
    (*pstate).scale = float(s)
    widget_control, (*pstate).inc_d_text, get_value=s
    (*pstate).inc_d = float(s)
    widget_control, (*pstate).inc_m_text, get_value=s
    (*pstate).inc_m = float(s)
    widget_control, (*pstate).inc_x_text, get_value=s
    (*pstate).inc_x = float(s)
    widget_control, (*pstate).inc_y_text, get_value=s
    (*pstate).inc_y = float(s)
    widget_control, (*pstate).inc_t_text, get_value=s
    (*pstate).inc_t = float(s)
    widget_control, (*pstate).inc_density_text, get_value=s
    (*pstate).inc_density = float(s) > 0.001
    widget_control, (*pstate).inc_density_text, set_value=str_tidy((*pstate).inc_density)
    widget_control, (*pstate).bubble_diameter_text, get_value=s
    (*pstate).bubble_diameter = float(s)
    widget_control, (*pstate).beam_x_text, get_value=s
    (*pstate).beam_x = float(s)
    widget_control, (*pstate).beam_y_text, get_value=s
    (*pstate).beam_y = float(s)
    widget_control, (*pstate).host_density_text, get_value=s
    (*pstate).host_density = float(s) > 0.001
    widget_control, (*pstate).host_density_text, set_value=str_tidy((*pstate).host_density)
    widget_control, (*pstate).host_formula_text, get_value=s
    (*pstate).host_formula = s
    widget_control, (*pstate).ebeam_text, get_value=s
    (*pstate).ebeam = float(s)
    widget_control, (*pstate).emean_text, get_value=s
    (*pstate).emean = float(s)
    return
end

;------------------------------------------------------------------------------------------

pro clear_property_changed, pstate

    (*pstate).mode_changed = 0
    (*pstate).inc_x_changed = 0
    (*pstate).inc_y_changed = 0
    (*pstate).inc_t_changed = 0
    (*pstate).inc_m_changed = 0
    (*pstate).inc_d_changed = 0
    (*pstate).inc_density_changed = 0
    (*pstate).beam_x_changed = 0
    (*pstate).beam_y_changed = 0
    (*pstate).beam_mode_changed = 0
    (*pstate).density_option_changed = 0
    (*pstate).fluid_type_changed = 0
    (*pstate).bubble_diameter_changed = 0
    (*pstate).host_formula_changed = 0
    (*pstate).host_weight_mode_changed = 0
    (*pstate).host_density_changed = 0
    (*pstate).scale_text_changed = 0
    (*pstate).tie_formula_changed = 0
    (*pstate).tie_value_changed = 0
    (*pstate).detector_mode_changed = 0
    (*pstate).filter_mode_changed = 0
    (*pstate).yield_file_changed = 0
    (*pstate).correct_changed = 0
    return
end

;------------------------------------------------------------------------------------------

pro OnRealize_density_option, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=(*pstate).density_option
end

;------------------------------------------------------------------------------------------

pro OnRealize_fluid_type, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=(*pstate).fluid_type
end

;------------------------------------------------------------------------------------------

pro OnRealize_beam_shape, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=(*pstate).beam_mode
end

;------------------------------------------------------------------------------------------

pro OnRealize_results_correction, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=(*pstate).correct
end

;------------------------------------------------------------------------------------------

pro OnRealize_host_weight_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=(*pstate).host_weight_mode
end

;------------------------------------------------------------------------------------------

pro OnRealize_analysis_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=(*pstate).mode
end

;------------------------------------------------------------------------------------------

pro OnRealize_results_filter_mode, wWidget

common c_working_dir, geopixe_root

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=(*pstate).filter_mode
end

;------------------------------------------------------------------------------------------

pro OnRealize_results_detector_mode, wWidget

common c_working_dir, geopixe_root

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=(*pstate).detector_mode
end

;------------------------------------------------------------------------------------------

pro result_properties, group_leader=group, TLB=tlb, presults=presults, path=path, layer_pars=player, $
                 select=select, _extra=extra, xoffset=xoffset, yoffset=yoffset

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
       warning,'result_properties',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''
if n_elements(select) lt 1 then select=[-1,-1]

case !version.os_family of
    'MacOS': begin
       yw = 512
       xw = 0
       help_xsize = 312
       space5 = 5
       space2 = 2
       shape_xsize = 78
       bubble_xsize = 90
       notes_xsize = 166
       notes7 = 7
       fluid_label = 'Fluid Type:'
       fluid_xsize = 100
       atomic_xsize = 100
       formula_xsize = 148
       correct_xsize = help_xsize-10
       end
    'unix': begin
       yw = 512
       xw = 5
       help_xsize = 312
       space5 = 0
       space2 = 0
       shape_xsize = 92
       bubble_xsize = 121
       notes_xsize = 176
       notes7 = 8
       fluid_label = 'Type:'
       fluid_xsize = 135
       atomic_xsize = 135
       formula_xsize = 118
       correct_xsize = help_xsize-10
       end
    else: begin
       yw = 492
       xw = 0
       help_xsize = 312
       space5 = 5
       space2 = 2
       shape_xsize = 78
       bubble_xsize = 90
       notes_xsize = 166
       notes7 = 7
       fluid_label = 'Fluid Type:'
       fluid_xsize = 100
       atomic_xsize = 100
       formula_xsize = 148
       correct_xsize = help_xsize-9
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
    xoffset = ((xoff + w + xw) < (screen[0]-34 - 326)) > 0
endif
if n_elements(yoffset) lt 1 then begin
    screen = get_screen_size()
    yoffset = ((yoff - yw+h) < (screen[1]-28 - 463)) > 0
endif

filter_update, list=filter_list, title=filter_title
detector_update, list=detector_list, title=detector_title

if n_elements(player) lt 1 then player = ptr_new(/allocate_heap)

presults = bad_pars_struct( presults, make_pars=no_results)

low = select[0] > 0
high = (select[1] > 0) > low
mode = 0
matrix = ''
filter = ''
detector = ''
scale = 1.0

inc_d = 0.0
inc_t = 0.0
norm = 1.0
inc_m = 0.0
inc_x = 0.0
inc_y = 0.0
density_option = 0
fluid_type = 0
bubble_diameter = 0.0
beam_x = 0.0
beam_y = 0.0
beam_mode = 0
host_weight_mode = 0
inc_density = 1.0
host_density = 0.0
host_formula = ''
correct = 0

if (no_results eq 0) and ((low ge 0) and (high ge low)) then begin
    n = n_elements(*presults)
    low = low < (n-1)
    high = high < (n-1)
    p = (*presults)[low]
    mode = (*p).mode
    matrix = (*p).yield.file
    filter = (*p).filter.file
    detector = (*p).detector.file
    scale = (*p).scale

    inc_d = (*p).yield.thick[0]
    norm = (*p).inclusion.norm
    inc_m = (*p).inclusion.m
    inc_x = (*p).inclusion.x
    inc_y = (*p).inclusion.y
    inc_density = (*p).inclusion.density
    density_option = (*p).inclusion.option
    fluid_type = (*p).inclusion.type
    bubble_diameter = (*p).inclusion.bubble
    beam_x = (*p).inclusion.beam.x
    beam_y = (*p).inclusion.beam.y
    beam_mode = (*p).inclusion.beam.shape
    host_weight_mode = (*p).yield.weight[0]
    host_density = (*p).yield.density[0]
    host_formula = (*p).yield.formula[0]

    n_layers = n_elements((*p).yield.thick)
    if n_layers ne 3 then mode=0
    if n_layers gt 1 then begin                   ; fluid inclusions
       inc_t = (*p).yield.thick[1]
    endif
    correct = (*p).correct
endif

filter_mode = 0
q = where( filter_list eq strip_path( filter))
if q[0] ne -1 then filter_mode = q[0]
detector_mode = 0
q = where( detector_list eq strip_path( detector))
if q[0] ne -1 then detector_mode = q[0]

;   top-level base

tlb = widget_base( /column, title='Results Properties', /TLB_KILL_REQUEST_EVENTS, $
              group_leader=group, _extra=extra, uname='Properties_TLB', xoffset=xoffset, yoffset=yoffset, /base_align_center)

tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

; Analysis mode

base = widget_base( tbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( base, value='Analysis Type:')
analysis_mode = widget_combobox( base, value=[' Thick target (1 layer) ',' Fluid Inclusion (3 layers) '], $
              uname='analysis-mode', /tracking, $
              notify_realize='OnRealize_analysis_mode', xsize=200, $
              uvalue='Type of analyis: "Thick Target" for uniform composition with depth, single layer samples; ' + $
              '"Fluid Inclusion" to model the 3D generation of PIXE/SXRF X-rays from a fluid inclusion, 3 layer target. ' + $
              'This should not be changed.')


mapbase = widget_base( tbase, xpad=0, ypad=0, /base_align_center)

;........................................................................................

; Normal thick targets

norm1_base = widget_base( mapbase, /column, /base_align_center, /align_center, map=1-mode)

norm_tab_panel = widget_tab( norm1_base, location=0, /align_center, uname='norm-tab-panel')

;	yield parameters tab ...

norm_base = widget_base( norm_tab_panel, title=' Yield Parameters ',/column, xoffset=3,yoffset=15, $
				xpad=0,ypad=0,space=15, /base_align_center, /align_center)

; filters

n2base = widget_base( norm_base, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( n2base, value='Filter:')
filter_mode_id = widget_combobox( n2base, value=filter_title, uname='filter-mode', /tracking, $
              notify_realize='OnRealize_results_filter_mode', $
              uvalue='Select the X-ray filter used.', xsize=190)

; detectors

n3base = widget_base( norm_base, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( n3base, value='Detector:')
detector_mode_id = widget_combobox( n3base, value=detector_title, uname='detector-mode', /tracking, $
              notify_realize='OnRealize_results_detector_mode', $
              uvalue='Select the relevant detector calibration.', xsize=210)

; yields

ybase = widget_base( norm_base, /row, /base_align_center, ypad=0, xpad=0, space=2)
lab = widget_label( ybase, value='Yields:')
yield_file = widget_text( ybase, value=matrix, uname='yield-file', /tracking, /editable, /all_events, $
              uvalue='Select the PIXE Yield file name, or use the "Load" button to load.',scr_xsize=190)
yield_new_button = widget_button( ybase, value='New', uname='yield-new-button', /tracking, scr_xsize=30, $
              uvalue='Go to the Layer popup window to calculated PIXE yields.')
yield_load_button = widget_button( ybase, value='Load', uname='yield-load-button', /tracking, scr_xsize=38, $
              uvalue='Load a set of precalculated PIXE yields from a YIELD file.')

; scale

lab = widget_label( norm_base, value=' ',/align_center)
stbase = widget_base( norm_base, /column, /base_align_center, ypad=1, xpad=1, space=10, /frame)
lab = widget_label( stbase, value='Apply scaling factors',/align_center)
base = widget_base( stbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( base, value='Scale all by:')
scale_text = widget_text( base, value=str_tidy(scale), uname='scale-text', /tracking, /editable, /all_events, $
              uvalue='Enter a scaling factor to apply to all concentrations.',scr_xsize=100)

; Tie

do_tie = 0
if do_tie then begin
	tibase = widget_base( stbase, /column, /base_align_center, ypad=1, xpad=1, space=5, /frame)
	lab = widget_label( tibase, value='Tie Expression in Each Analysis',/align_center)
	base = widget_base( tibase, /row, /base_align_center, ypad=0, xpad=0, space=5)
	lab = widget_label( base, value='Formula:')
	tie_formula_text = widget_text( base, value=' -- Available soon -- ', uname='tie-formula-text', /tracking, /editable, /all_events, $
              uvalue='Enter a formula that you wish to tie to a specified value (wt %).',scr_xsize=240)
	base = widget_base( tibase, /row, /base_align_center, ypad=0, xpad=0, space=5)
	lab = widget_label( base, value='Tie formula expression to value (%):')
	tie_value_text = widget_text( base, value=str_tidy(100.0), uname='tie-value-text', /tracking, /editable, /all_events, $
              uvalue='Enter the value (wt %) to tie formula to for selected analyses.',scr_xsize=100)
endif else begin
	tie_formula_text = 0L
	tie_value_text = 0L
endelse


;	STIM correct tab ...

stim_base = widget_base( norm_tab_panel, title=' STIM Correction ',/column, /base_align_right, $
			xoffset=3,yoffset=15, xpad=5,ypad=5,space=10, /align_center)

stimokbase = widget_base( stim_base, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( stimokbase, value='STIM Corrected:')
stim_ok_text = widget_text( stimokbase, value='No', uname='stim-ok', /tracking, $
					uvalue='Shows status of STIM correction.', scr_xsize=100)

e0base = widget_base( stim_base, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( e0base, value='Beam Energy:')
ebeam_text = widget_text( e0base, value='0.0', uname='stim-ebeam', /tracking, /editable, $
					uvalue='Enter the STIM beam energy (MeV).', scr_xsize=100)

eabase = widget_base( stim_base, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( eabase, value='Mean STIM Energy:')
emean_text = widget_text( eabase, value='0.0', uname='stim-emean', /tracking, /editable, $
					uvalue='Enter the mean STIM energy (MeV), or (i) select/apply a Region giving mean STIM energy, (ii) select/apply a CUT on a STIM energy spectrum.', scr_xsize=100)

xbase = widget_base( stim_base, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( xbase, value='Mean thickness (mg/cm2):')
xmean_text = widget_text( xbase, value='0.0', uname='stim-xmean', /tracking, $
					uvalue='The calculated mean thickness X (mg/cm2).', scr_xsize=100)

;........................................................................................

; Fluid inclusion modelling control panel ...

inc_base = widget_base( mapbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center, map=mode)

rowbase = widget_base( inc_base, /row, /base_align_center, ypad=0, xpad=0, space=5)
lbase = widget_base( rowbase, /column, /base_align_center, ypad=0, xpad=0, space=5)

; Inclusion parameters

ibase = widget_base( lbase, /column, /base_align_right, ypad=1, xpad=1, space=space2, /frame)
lab = widget_label( ibase, value='Inclusion Shape',/align_center)
base = widget_base( ibase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( base, value='X length:')
inc_x_text = widget_text( base, value=str_tidy(inc_x), uname='inc-x-text', /tracking, /editable, /all_events, $
              uvalue='Enter the X size (length, microns) of an equivalent ellipse approximating the inclusion area as seen under the microscope.',scr_xsize=60)
base = widget_base( ibase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( base, value='Y width:')
inc_y_text = widget_text( base, value=str_tidy(inc_y), uname='inc-y-text', /tracking, /editable, /all_events, $
              uvalue='Enter the Y size (width, microns) of an equivalent ellipse approximating the inclusion area as seen under the microscope.',scr_xsize=60)
base = widget_base( ibase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( base, value='Thickness:')
inc_t_text = widget_text( base, value=str_tidy(inc_t), uname='inc-t-text', /tracking, /editable, /all_events, $
              uvalue='Enter the thickness of the inclusion ellipsoid model (microns). Inclusion depth will be determined from thickness and mid-plane depth.',scr_xsize=60)
base = widget_base( ibase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( base, value='Mid-plane:')
inc_m_text = widget_text( base, value=str_tidy(inc_m), uname='inc-m-text', /tracking, /editable, /all_events, $
              uvalue='Enter the depth to the inclusion mid-plane (microns) as observed under the microscope. Adjust this if you need to change the inclusion depth.',scr_xsize=60)

; Beam parameters

bbase = widget_base( lbase, /column, /base_align_right, ypad=1, xpad=1, space=2, /frame)
lab = widget_label( bbase, value='Beam Selection Area',/align_center)
base = widget_base( bbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( base, value='X size:')
beam_x_text = widget_text( base, value=str_tidy(beam_x), uname='beam-x-text', /tracking, /editable, /all_events, $
              uvalue='Enter the X size of the selected beam scan area (microns), for either an elliptical selection area (length), scan rectangle (X size) or Gaussian defocused beam (X FWHM)',scr_xsize=78)
base = widget_base( bbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( base, value='Y size:')
beam_y_text = widget_text( base, value=str_tidy(beam_y), uname='beam-y-text', /tracking, /editable, /all_events, $
              uvalue='Enter the Y size of the selected beam scan area (microns), for either an elliptical selection area (width), scan rectangle (Y size) or Gaussian defocused beam (Y FWHM).',scr_xsize=78)
base = widget_base( bbase, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( base, value='Shape:')
beam_mode_id = widget_combobox( base, value=['XY scan','Ellipse','Gaussian'], $
              uname='beam-mode', /tracking, $
              notify_realize='OnRealize_beam_shape', xsize=shape_xsize, $
              uvalue='Select the beam selection type to use: "XY scan" for a rastered beam, ' + $
              '"Ellipse" for spectrum extracted from an elliptical region, ' + $
              'or "Gaussian" for a defocussed beam (size is then FWHM).')

; Inclusion contents

rbase = widget_base( rowbase, /column, /base_align_right, ypad=1, xpad=1, space=space2, /frame)
lab = widget_label( rbase, value='Inclusion Contents',/align_center)
base = widget_base( rbase, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( base, value='Bubble:')
density_option_id = widget_combobox( base, value=['None','Large dilute','Specify size'], $
              uname='density-option', /tracking, $
              notify_realize='OnRealize_density_option', xsize=bubble_xsize, $
              uvalue='Select the bubble option: "None" for no bubble; use this with the homogeneous density, ' + $
              '"Large dilute" for a significant bubble in a density=1 fluid, or "Specify size" to enter the bubble diameter and fluid density.')
notes = ['For saline fluid inclusions, or those with small vapour bubbles, use the bubble option "None". ' + $
         'Enter the homogeneous density under "density" below, and select the composition type.', $
         'For vapour inclusions with fluid density close to 1.0 (at room temperature), ' + $
         'use the "Large Dilute" bubble option and enter the homogeneous density under "Fluid Density".', $
         'To explicitly set the bubble size and select the room temperature fluid density, use the "Specify Size" bubble option. ' + $
         'Another box will appear for bubble diameter (microns).']
explanation = widget_text( rbase, scr_xsize=notes_xsize, ysize=notes7, value=notes[density_option], /wrap, uname='explanation', /tracking, $
          uvalue='Displays detailed explanation and instructions about the "Bubble Option" selected.', $
          frame=0)
base = widget_base( rbase, /row, /base_align_center, ypad=0, xpad=0, space=5)
lab = widget_label( base, value='Fluid Density:')
inc_density_text = widget_text( base, value=str_tidy(inc_density), uname='inc-density-text', /tracking, /editable, /all_events, $
              uvalue='Enter the fluid density (g/cm^3). See the explanation above relating to the currently selected "Bubble Option".',scr_xsize=70)
base = widget_base( rbase, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( base, value=fluid_label)

fluid_types = ['Dilute aqueous','5-10%  NaCl','10-20% NaCl','20-30% NaCl','30-40% NaCl','40-50% NaCl','>50%   NaCl']

fluid_type_id = widget_combobox( base, value=fluid_types, uname='fluid-type', /tracking, $
              notify_realize='OnRealize_fluid_type', xsize=fluid_xsize, $
              uvalue='Select the fluid composition class based on equivalent NaCl. This has a weak effect on yield calculation.')
bubble_base = widget_base( rbase, /row, /base_align_center, ypad=0, xpad=0, space=5, map=0)
lab = widget_label( bubble_base, value='Bubble diameter:')
bubble_diameter_text = widget_text( bubble_base, value=str_tidy(bubble_diameter), uname='bubble-diameter-text', /tracking, /editable, /all_events, $
              uvalue='Enter the bubble diameter (microns). See the explanation above relating to the currently selected "Bubble Option".',scr_xsize=70)

; Host details

cbase = widget_base( inc_base, /column, /base_align_center, ypad=1, xpad=1, space=space2, /frame)
lab = widget_label( cbase, value='Host Mineral',/align_center)
base = widget_base( cbase, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( base, value='Formula:')
host_formula_text = widget_text( base, value=host_formula, uname='host-formula-text', /tracking, /editable, /all_events, $
              uvalue='Enter the chemical formula for the host mineral.',scr_xsize=formula_xsize)
host_weight_mode_id = widget_combobox( base, value=['Atomic Fraction','Weight %'], uname='host-weight-mode', /tracking, $
              notify_realize='OnRealize_host_weight_mode', xsize=atomic_xsize, $
              uvalue='Select the type of multiplier factors in formulae. These appear after ' + $
              'a radical term contained in brackets, e.g. "(SiO2)12.3". Options include "atomic fraction" or "weight %".')
base = widget_base( cbase, /row, /base_align_center, ypad=0, xpad=0, space=space5)
lab = widget_label( base, value='Inclusion Depth:')
inc_d_text = widget_text( base, value=str_tidy(inc_d), uname='inc-d-text', /tracking, /editable, /all_events, $
              uvalue='Enter the depth to the top of the inclusion through the host mineral (microns). This is usually determined from Thickness and Mid-plane. ' $
              +'If Depth is changed, then a new thickness is calculated consistent with Mid-plane.',scr_xsize=70)
lab = widget_label( base, value='  Density:')
host_density_text = widget_text( base, value=str_tidy(host_density), uname='host-density-text', /tracking, /editable, /all_events, $
              uvalue='Enter the density of the host mineral (g/cm^3). '+ $
              'For pure element layers the density will be obtained from the database automatically.', scr_xsize=70)

; Corrections

fbase = widget_base( inc_base, /column, /base_align_center, ypad=1, xpad=1, space=space2, /frame)
lab = widget_label( fbase, value='Corrections',/align_center)
base = widget_base( fbase, /row, /base_align_center, ypad=0, xpad=0, space=space5)
correct_id = widget_combobox( base, value=[' - none - ','Subtract scatter blank spectrum','Subtract matrix blank spectrum'], uname='correction-type', /tracking, $
              notify_realize='OnRealize_results_correction', xsize=correct_xsize, $
              uvalue='Correct for scatter background (need 1. inclusion fit, 2. fit to quartz blank), or subtract matrix contributions ' + $
              '(need 1. inclusion fit, 2. inc fit to quartz blank, 3. quartz fit to blank).')


; Buttons

b2base = widget_base( tbase, /row, /base_align_center, ypad=0, space=15)
button = widget_button( b2base, value='Apply', uname='apply-button', /tracking, $
              uvalue='Apply the changes made, to the currently selected analyses. Select analyses by clicking and dragging in the Results Table.')
lab = widget_label( b2base, value='       ')
;button = widget_button( b2base, value='Copy Prev', uname='copy-button', /tracking, $
;             uvalue='Copy the fluid inclusion geometry parameters from the previous results row in "Fit Results".')
lab = widget_label( b2base, value='       ')
button = widget_button( b2base, value='Close', uname='close-button', /tracking, $
              uvalue='Close the Results Properties window.')

help = widget_text( tbase, scr_xsize=help_xsize, ysize=4, /wrap, uname='help', /tracking, $
          uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
          frame=0)


state = {   $
       path:               ptr_new(path), $     ; pointer to current path
       tlb:					tlb, $				; local TLB
       presults:           presults, $          ; pointer to results pointer array
       player:               player, $          ; pointer to layer parameters
       select:               select, $
       pdelete:				ptr_new(/allocate_heap), $

       mode:               mode, $              ; current analysis mode
       beam_mode:              beam_mode, $      ; beam mode
       density_option:          density_option, $   ; density option
       fluid_type:             fluid_type, $        ; fluid type
       host_weight_mode:        host_weight_mode, $     ; host weight mode
       inc_x:                 inc_x, $          ; inclusion X size
       inc_y:                 inc_y, $          ; inclusion Y size
       inc_t:                 inc_t, $          ; inclusion T thickness
       inc_d:                 inc_d, $          ; inclusion d depth
       inc_m:                 inc_m, $          ; inclusion M mid-plane
       inc_density:          inc_density, $         ; inclusion density
       beam_x:               beam_x, $          ; beam X size
       beam_y:               beam_y, $          ; beam Y size
       bubble_diameter:       bubble_diameter, $     ; bubble diameter
       host_formula:          host_formula, $       ; host formula
       host_density:          host_density, $       ; host density

       scale:                 scale, $          ; scale factor
       tie_formula:          '', $           ; tie formula
       tie_value:              100.0, $          ; tie value
       filter_mode:          filter_mode, $         ; current filter
       detector_mode:          detector_mode, $       ; detector
       yield_file:             matrix, $          ; yield file

		norm_mode:				0, $				; norm tab mode
		stim_ok:				0, $				; STIM correction done
		ebeam:					0.0, $				; E beam
		emean:					0.0, $				; mean STIM E
		xmean:					0.0, $				; mean STIM thickness
		stim_base:				stim_base, $		; STIM correction base
		stim_ok_text:			stim_ok_text, $		; stim OK status text widget ID
		ebeam_text:				ebeam_text, $		; E beam text widget ID
		emean_text:				emean_text, $		; E mean text widget ID
		xmean_text:				xmean_text, $		; X mean text widget ID

       norm_base:              norm1_base, $      ; ID map base for normal thick targets
       inc_base:           inc_base, $          ; ID map base for fluid inclusions
       bubble_base:          bubble_base, $         ; ID map base to specify bubble size

       analysis_mode:          analysis_mode, $       ; ID of analysis-mode droplist
       mode_changed:          0, $           ; flags a change of widget

       inc_x_text:             inc_x_text, $        ; ID of inclusion X size text widget
       inc_y_text:             inc_y_text, $        ; ID of inclusion Y size text widget
       inc_t_text:             inc_t_text, $        ; ID of inclusion T thickness text widget
       inc_m_text:             inc_m_text, $        ; ID of inclusion M mid-plane text widget
       inc_d_text:             inc_d_text, $        ; ID of inclusion d depth text widget
       inc_density_text:        inc_density_text, $     ; ID of inclusion density text widget
       inc_x_changed:          0, $              ; flags edit change to text widget
       inc_y_changed:          0, $              ; flags edit change to text widget
       inc_t_changed:          0, $              ; flags edit change to text widget
       inc_m_changed:          0, $              ; flags edit change to text widget
       inc_d_changed:          0, $              ; flags edit change to text widget
       inc_density_changed:   0, $              ; flags edit change to text widget

       beam_x_text:          beam_x_text, $         ; ID of beam X size text widget
       beam_y_text:          beam_y_text, $         ; ID of beam Y size text widget
       beam_mode_id:          beam_mode_id, $       ; ID of beam-mode droplist
       beam_x_changed:          0, $             ; flags edit change to text widget
       beam_y_changed:          0, $             ; flags edit change to text widget
       beam_mode_changed:         0, $           ; flags a change of widget

       density_option_id:         density_option_id, $ ; ID of density option droplist
       explanation:          explanation, $         ; ID of explanation text widget
       fluid_type_id:          fluid_type_id, $       ; ID of fluid-type droplist
       bubble_diameter_text:     bubble_diameter_text, $    ; ID of bubble diameter text widget
       density_option_changed:     0, $           ; flags a change of widget
       fluid_type_changed:      0, $              ; flags a change of widget
       bubble_diameter_changed:  0, $           ; flags edit change to text widget
       notes:                 notes, $          ; notes on bubble options

       host_formula_text:         host_formula_text, $ ; ID of host formula text widget
       host_weight_mode_id:   host_weight_mode_id, $  ; ID of host weight mode droplist
       host_density_text:         host_density_text, $ ; ID of host density text widget
       host_formula_changed:     0, $             ; flags edit change to text widget
       host_weight_mode_changed: 0, $              ; flags a change of widget
       host_density_changed:     0, $             ; flags edit change to text widget

		correct_id:				correct_id, $		; ID of correction droplist
       correct:				correct, $				; current type of correction applied
       new_correct:				0, $				; new type of correction to apply
		correct_changed:		0, $				; flags a change of correct droplist

       scale_text:             scale_text, $        ; ID of scale text widget
       tie_formula_text:        tie_formula_text, $     ; ID of tie formula text widget
       tie_value_text:          tie_value_text, $   ; ID of tie value text widget
       scale_text_changed:      0, $              ; flags change of widget
       tie_formula_changed:   0, $              ; flags change of widget
       tie_value_changed:         0, $           ; flags change of widget

       yield_text:             yield_file, $        ; ID of yield file text widget
       filter_mode_id:          filter_mode_id, $   ; ID of filter droplist
       detector_mode_id:        detector_mode_id, $     ; ID of detector droplist
       detector_mode_changed:       0, $            ; flags change of mode
       filter_mode_changed:   0, $              ; flags change of mode
       yield_file_changed:      0, $              ; flags change of text
       filter_list:          filter_list, $         ; list of filters
       detector_list:          detector_list, $       ; list of detectors

       help:               help $           ; ID of help text
    }

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['path', $              ; new path
                 'new-results', $       ; new results
	          'image-region-select', $	; new Region row selected --> result-properties
                 'results-select' ], $     ; new results select range
                 from=group

xmanager, 'result_properties', tlb, /no_block

return
end
