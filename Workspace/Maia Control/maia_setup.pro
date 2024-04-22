pro maia_setup_event, event

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
		warning,'maia_setup_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return						; goto, kill
	endif
endif
widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good(pstate, /struct) eq 0 then goto, bad_state

; The *pm struct holds the current values of parameters. Many are updated regularly from
; Maia via the Kandinski socket. The local copy *pl is made from *pm when
; (i)   a new Tab is selected, or
; (ii)  when Setup is first started. 
; 
; Certain parameters are changed locally (using the *pl copy), such as 
; (i)   enable checkboxes on all panels (except the "Enable" panel which is live). 
;       The enables modify the local *pl values and send these to Maia, only
;       when "Apply" is used. 
; (ii)  the Bias, Guard and Peltier sliders on the "Control" panel. 
;       These are sent immediately when the slider are used. They are read back 
;       periodically as *pm values.
; (iii) the Group table data uses the *pl copy to load the group table.
;       Data are read into *pl with the 'read' button. They can be modified there 
;       and then Applied to Maia.
; (iv)	the Hymod and Imaging panel file names.
;       
; NOTE: On "Apply" make sure that the specific parameters (local to this Tab) are copied
;       from *pl back into *pm, unless they will be read back from Kandinski. 
;       
; The copy is made in 'maia-tab-panel' event code. Care is taken in copying from *pm to *pl
; to preserve the plist pointers in both. Hymod panel enable checkboxes and files are set
; from *pm in the 'maia_setup_update_dynamic' routine. The Enable panel enables are only
; set after the update from Maia (in Notify code).
;
; Detector array:
;	CSV table index is used for elements of the detector array, such as in the
;	'select' vector. To convert this to detector number, use the lookup table:
;
;	(*(*pstate).playout).data.index			CSV table index to detector number
;	(*(*pstate).playout).ref				detector number to CSV table index
;	(*pstate).sort.index					re-ordered (sorted) CSV table index in Summary

pm = (*pstate).pmaia
pl = (*pstate).plocal
ps = (*pstate).psocket
pr = (*pstate).preadout
play = (*pstate).playout

n_detectors = (*pm).n_detectors
status = {on:0, mask:bytarr(n_detectors), text:''}
version = (*pm).version.software
obj = (*pm).DevObj

@maia_scratch.def

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
		if (*pstate).tracking eq 0 then goto, finish
		widget_control, event.id, get_uvalue=s0
		if size(s0,/tname) eq 'STRING' then begin
			s = s0
		endif else if size(s0,/tname) eq 'STRUCT' then begin
			s = s0.help
		endif else s=''
		if event.enter eq 1 then begin
			widget_control, (*pstate).help, set_value=s
		endif else begin
			widget_control, (*pstate).help, set_value='Select functional groups of Maia parameters using the Tabs on the left, and use the mimic display on the right to either "Set parameters" or display parameters across the array using "Show Parameters". ' + $
					'Many operations in the left panels use the currently selected detector channels as targets. The controls on the left will be set to the parameters for the detector you click on.'
		endelse
		goto, finish
		end
	'WIDGET_TIMER': begin
		u = widget_info( event.id, /uname)
		case u of
			'pulser-start': begin
				if (*pstate).pulser.on eq 0 then break				
				widget_control, (*pstate).pulser_list, set_list_select=(*ps).current, set_list_top=((*ps).current-8)>0
				socket_command_set, ps, /next, error=error
				widget_control, (*pstate).pulser_list, set_list_select=((*ps).current-1)>0, set_list_top=((*ps).current-9)>0
				if (*ps).current le (*ps).last then begin
					if error eq 0 then widget_control, (*pstate).pulser_start_button, timer=(*ps).time
				endif else begin
					(*pstate).pulser.on = 0
					widget_control, (*pstate).pulser_program_mode, sensitive=1
					widget_control, (*pstate).pulser_start_button, set_value=(*pstate).start_text[(*pstate).pulser.mode]
					if (*pstate).pulser.mode eq 5 then begin
						maia_setup_correct_leakage, (*pstate).pelk
						notify, 'update-elk', (*pstate).pelk, from=event.top

						maia_setup_colour, pstate, (*(*pstate).pelk)[(*play).data.index], title='Hermes ELK leakage'					
						(*pstate).colour_mode = 1
						widget_control, (*pstate).display_toggle, set_value=1
						for i=0L,n_elements((*pstate).check_ids)-1 do begin
							if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
						endfor
						widget_control, (*pstate).hermes_apply_button, sensitive=0
						widget_control, (*pstate).scepter_apply_button, sensitive=0
						widget_control, (*pstate).detector_mode, sensitive=0
						widget_control, (*pstate).colour_display_text, sensitive=0

						maia_setup_write_leakage, pstate, (*pstate).pelk, title='Hermes ELK leakage'	
					endif else if (*pstate).pulser.mode eq 6 then begin
;						notify, 'update-ean', (*pstate).pelk, from=event.top

						maia_setup_colour, pstate, (*(*pstate).pelk)[(*play).data.index], title='Hermes EAN baseline'					
						(*pstate).colour_mode = 1
						widget_control, (*pstate).display_toggle, set_value=1
						for i=0L,n_elements((*pstate).check_ids)-1 do begin
							if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
						endfor
						widget_control, (*pstate).hermes_apply_button, sensitive=0
						widget_control, (*pstate).scepter_apply_button, sensitive=0
						widget_control, (*pstate).detector_mode, sensitive=0
						widget_control, (*pstate).colour_display_text, sensitive=0
					endif
				endelse
				end
			else:
		endcase
		goto, finish		
		end
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
				;	print,'fit results: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'dpath': begin
				if ptr_valid( event.pointer) then begin
				;	print,'fit results: new path = ',(*event.pointer)
					*(*pstate).dpath = (*event.pointer)
				endif
				goto, finish
				end
			'detector-toggle': begin
				if (*pstate).colour_mode eq 1 then goto, finish			; but not in "Colour" mode
				index = (*event.pointer).index
				enable_mode = ((*pstate).tab_names[(*pstate).tab] eq 'Enable')

				sel = 1 - (*pstate).select[index]
				(*pstate).select[index]=sel 
				widget_control, (*pstate).detector, set_value={select:index, value:sel, alt:enable_mode}
				end

			'maia-display': begin
			
;				Put things that need immediate updating here, such as widgets that
;				should update before your eyes. But not widgets that may get edited.
;				These should get updated in 'maia_setup_update_dynamic'.

				maia_setup_load_table, pstate
				
				if (*pstate).enable_quad_freeze eq 0 then sensitive=1 else sensitive=0
				widget_control, (*pstate).enable_quad_check, set_value=(*(*pstate).preadout).quad_enable, sensitive=sensitive
				(*pstate).enable_quad_freeze = ((*pstate).enable_quad_freeze-1) > 0
				
				if (*pstate).enable_rr_freeze eq 0 then sensitive=1 else sensitive=0
				widget_control, (*pstate).enable_rr_check, set_value=(*(*pstate).preadout).rr_enable, sensitive=sensitive
				(*pstate).enable_rr_freeze = ((*pstate).enable_rr_freeze-1) > 0

				if (*pstate).enable_modules_freeze eq 0 then sensitive=1 else sensitive=0
				widget_control, (*pstate).enable_modules_check, set_value=[(*pm).linear.on,(*pm).trim.on, $
							(*pm).pileup.on,(*pm).throttle.on,(*pm).DA.on,(*pm).ROI.on, (*pm).groups.on], sensitive=sensitive
				(*pstate).enable_modules_freeze = ((*pstate).enable_modules_freeze-1) > 0

				if (*pstate).enable_stream_freeze eq 0 then sensitive=1 else sensitive=0
				widget_control, (*pstate).enable_stream_check, set_value=[(*pr).scepter, (*pr).event, (*pr).photon, $
										(*pr).accum, (*pr).activity, (*pm).deadtime.on, (*pm).scan.on, (*pm).chart.on], sensitive=sensitive
				(*pstate).enable_stream_freeze = ((*pstate).enable_stream_freeze-1) > 0

		;		No. This will overwrite changes to PV selection on Image panel ...
		;		if (*pm).IC.remote then maia_setup_update_ic, pstate, pm

				widget_control, (*pstate).controls_bias_text, set_value=str_tidy((*pm).control.bias_monitor, length=5)+' V'
				widget_control, (*pstate).controls_leakage_text, set_value=str_tidy((*pm).control.leakage, length=5)+' uA'
				widget_control, (*pstate).controls_peltier_monitor_text, set_value=str_tidy((*pm).control.peltier_monitor, length=5)+' A'
				widget_control, (*pstate).controls_peltier_supply_text, set_value=str_tidy((*pm).control.peltier_supply, length=5)+' V'

				widget_control, (*pstate).controls_detector_temp_text, set_value=str_tidy((*pm).control.temp.detector, length=5)+' C'
				widget_control, (*pstate).controls_temp_hermes_text, set_value=str_tidy((*pm).control.temp.hermes, length=5)+' C'
				widget_control, (*pstate).controls_temp_water_text, set_value=str_tidy((*pm).control.temp.water, length=5)+' C'
				widget_control, (*pstate).controls_temp_coldtrap_text, set_value=str_tidy((*pm).control.temp.coldtrap, length=5)+' C'
				widget_control, (*pstate).controls_temp_mosfet_text, set_value=str_tidy((*pm).control.temp.mosfet, length=5)+' C'
				widget_control, (*pstate).controls_temp_fpga_text, set_value=str_tidy((*pm).control.temp.fpga, length=5)+' C'
				widget_control, (*pstate).controls_temp_hymod_fpga_text, set_value=str_tidy((*pm).control.temp.hymod_fpga, length=5)+' C'
				widget_control, (*pstate).controls_temp_hymod_cpu_text, set_value=str_tidy((*pm).control.temp.hymod_cpu, length=5)+' C'
				widget_control, (*pstate).controls_ddm_vcc_text, set_value=str_tidy((*pm).control.vcc, length=5)+' V'

;				This is now on Debug tab ...
				widget_control, (*pstate).controls_oan_text, set_value=str_tidy((*pm).control.oan, length=5)+' V'
				widget_control, (*pstate).controls_aux_text, set_value=str_tidy((*pm).control.aux, length=5)+' V'
				widget_control, (*pstate).controls_monitor_text, set_value=str_tidy((*pm).control.monitor, length=5)+' V'

;				widget_control, (*pstate).controls_linkloss_status, set_value={select: 2*(*pm).control.linkloss}
;				widget_control, (*pstate).controls_interlock_status, set_value={select: 2*(1-(*pm).control.interlock)}

				debug = *event.pointer
;				help, debug, /str
				*(*pstate).pHYMOD_debug = debug
				widget_control, (*pstate).warning_pulser_base, map=debug.pulser or debug.synth
				widget_control, (*pstate).warning_debug_base, map=debug.hermes or debug.scepter
				widget_control, (*pstate).warning_eblk_base, map=debug.eblk
				widget_control, (*pstate).bake_button_base, map=(*pm).control.status.bake
				widget_control, (*pstate).bake_button, set_value={select:debug.bake ? 2 : 1}
				
;				This is already done in the 'change tabs' code ...	
;					Ignore the pulser terms here.
;					Or, ignore here only if in Pulser tab mode?
;					
;				debug = *(*pstate).pHYMOD_debug			
;				(*pstate).pulser.on = 0
;				if debug.pulser then begin
;					if ((*pstate).pulser.mode ne 2) and ((*pstate).pulser.mode ne 3) then (*pstate).pulser.mode = 1
;					(*pstate).pulser.on = 1
;				endif
;				if debug.synth then begin
;					(*pstate).pulser.mode = 4
;					(*pstate).pulser.on = 1
;				endif
				if ((*pstate).tab_names[(*pstate).tab] ne 'Debug') then begin
					(*pstate).debug_ean_mode = debug.hermes
					(*pstate).debug_aux_mode = debug.scepter
				endif
				maia_setup_check_warning, pstate
				end
			else:
		endcase
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request maia_setup ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'maia-setup-tlb': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				case !version.os_family of
					'MacOS': begin
						xoff = 0
						yoff = 0
						end
					'unix': begin
						xoff = 0
						yoff = 0
						end
					else: begin
						xoff = 0
						yoff = 0
						end
				endcase
				widget_control, event.id, scr_xsize=(*pstate).scr_xsize+xoff, scr_ysize=(*pstate).scr_ysize+yoff
				end
			else:
		endcase
		end

	'maia-tab-panel': begin
		if ((*pstate).tab_names[(*pstate).tab] eq 'Hermes') or ((*pstate).tab_names[(*pstate).tab] eq 'Scepter') then begin
			(*pstate).asic_select = (*pstate).select
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Groups') then begin
			(*pstate).groups_select = (*pstate).select
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Pulser') then begin
			(*pstate).pulser_select = (*pstate).select
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Debug') then begin
			if (*pstate).debug_ean_check ne 0 then (*pstate).debug_select_ean = (*pstate).select
			if (*pstate).debug_aux_check ne 0 then (*pstate).debug_select_aux = (*pstate).select
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Enable') then begin
			*(*pstate).pdisable = (*pstate).select
			(*pstate).select = 0
			widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:0}
		endif 

		(*pstate).tab = event.tab

		maia_setup_update_dynamic, pstate				; set enables and detector sliders from *pm
		copy_pointer_data, pm, pl						; make local copy for enables, sliders, etc.
														; See notes at top of Event routine.

		case (*pstate).tab_names[event.tab] of
			'Summary': begin		; summary
				widget_control, (*pstate).detector_mode, set_combobox_select=1 
				(*pstate).group_mode = 1
				(*pstate).group_mask = [0,1,1,1,1,1,1,1]
				end
			'Enable': begin			; enable
				widget_control, (*pstate).detector_mode, set_combobox_select=1 
				(*pstate).group_mode = 1
				(*pstate).group_mask = [0,1,1,1,1,1,1,1]
				(*pstate).colour_mode = 0
				end
			'Hermes': begin			; hermes
				(*pstate).group_mask = (*pstate).old_mask
				end
			'Scepter': begin		; scepter
				(*pstate).group_mask = (*pstate).old_mask
				end
			'Hymod': begin			; hymod
				end
			'Spectra': begin		; spectra
				widget_control, (*pstate).detector_mode, set_combobox_select=2
				(*pstate).group_mode = 2
				(*pstate).group_mask = [0,1,1,1,1,1,1,1]
				(*pstate).colour_mode = 0
				end
			'Imaging': begin		; imaging
				widget_control, (*pstate).detector_mode, set_combobox_select=7
				(*pstate).group_mode = 7
				(*pstate).group_mask = [0,1,1,1,1,1,1,1]
				(*pstate).colour_mode = 0
				widget_control, (*pstate).auto_DA_check, set_value=(*pm).DA.save
				widget_control, (*pstate).save_path_text, set_value=(*pm).DA.save_path
				end
			'Controls': begin		; controls
				widget_control, (*pstate).detector_mode, set_combobox_select=0 
				(*pstate).group_mode = 0
				(*pstate).group_mask = [1,0,0,0,0,0,0,0]
				(*pstate).colour_mode = 0
				end
			'Layout': begin			; layout
				widget_control, (*pstate).detector_mode, set_combobox_select=0 
				(*pstate).group_mode = 0
				(*pstate).group_mask = [1,1,1,1,1,1,1,1]
				(*pstate).colour_mode = 0
				end
			'Pulser': begin			; pulser
				widget_control, (*pstate).detector_mode, set_combobox_select=1
				(*pstate).group_mode = 1
				(*pstate).group_mask = [0,1,1,1,1,1,1,1]
				(*pstate).colour_mode = 0
				v = socket_command_get( ps, 'enable', class='synth', error=err)
				if err eq 0 then begin
					if v[0] eq 1 then begin
						(*pstate).pulser.mode=4
						(*pstate).pulser.on = 1
						widget_control, (*pstate).pulser_program_mode, set_combobox_select=(*pstate).pulser.mode, sensitive=0
						widget_control, (*pstate).pulser_start_button, set_value='Abort'
						widget_control, (*pstate).pulser_rate_base, map=1
						widget_control, (*pstate).pulser_high_base, map=1
						widget_control, (*pstate).pulser_start_base, map=1
						widget_control, (*pstate).pulser_progress_base, map=0
						widget_control, (*pstate).pulser_count_base, map=0
					endif
				endif
				v2 = socket_command_get( ps, 'enable', class='pulser', error=err)
				if (err eq 0) and (v2[0] eq 1) and (v[0] eq 0) then begin
					if ((*pstate).pulser.on eq 0) or ((*pstate).pulser.mode eq 0) or ((*pstate).pulser.mode eq 4) then begin
						(*pstate).pulser.on = 1
						if ((*pstate).pulser.mode ne 2) and ((*pstate).pulser.mode ne 3) then (*pstate).pulser.mode = 1
						widget_control, (*pstate).pulser_program_mode, set_combobox_select=(*pstate).pulser.mode, sensitive=0
						widget_control, (*pstate).pulser_start_button, set_value='Abort'
						widget_control, (*pstate).pulser_rate_base, map=1
						widget_control, (*pstate).pulser_start_base, map=1
						widget_control, (*pstate).pulser_high_base, map=0
						widget_control, (*pstate).pulser_progress_base, map=0
						widget_control, (*pstate).pulser_count_base, map=0
					endif
				endif
				
				if ((*pstate).pulser.mode eq 4) then begin
					v = socket_command_get( ps, 'component', class='synth.event', chip=0, channel=2, error=err)
					if err eq 0 then begin
						sel = intarr( (*pm).n_detectors)
						sel[v[0]] = 1
						(*pstate).pulser_select = sel[(*play).data.index]
					endif
					v = socket_command_get( ps, 'rate', class='synth', error=err)
					if err eq 0 then begin
						(*pstate).pulser.rate = v[0]
						widget_control, (*pstate).pulser_rate_text, set_value=str_tidy(v[0])
					endif
					v = socket_command_get( ps, 'voltage', class='pulser', error=err)
					if (err eq 0) then begin
						(*pstate).pulser.low = v[0]
						widget_control, (*pstate).pulser_low_text, set_value=str_tidy(v[0])
					endif
				endif else if ((*pstate).pulser.mode eq 1) then begin
					v = socket_command_get( ps, 'ECAL', class='hermes', chip=-1, channel=-1, error=err)
					if err eq 0 then (*pstate).pulser_select = v[(*play).data.index]
					v = socket_command_get( ps, 'rate', class='pulser', error=err)
					if (err eq 0) then begin
						(*pstate).pulser.rate = v[0]
						widget_control, (*pstate).pulser_rate_text, set_value=str_tidy(v[0])
					endif
					v = socket_command_get( ps, 'voltage', class='pulser', error=err)
					if (err eq 0) and (((*pstate).pulser.mode eq 1) or ((*pstate).pulser.mode eq 4)) then begin
						(*pstate).pulser.low = v[0]
						widget_control, (*pstate).pulser_low_text, set_value=str_tidy(v[0])
					endif
				endif
				end
			'Debug': begin			; debug
				widget_control, (*pstate).detector_mode, set_combobox_select=0
				(*pstate).group_mode = 0
				(*pstate).group_mask = [1,0,0,0,0,0,0,0]
				(*pstate).colour_mode = 0

				v = socket_command_get( ps, 'EAN', class='hermes', chip=-1, channel=-1, error=err)
				if err eq 0 then (*pstate).debug_select_ean = v[(*play).data.index]
				qean = where(v ne 0, nqean)
				v = socket_command_get( ps, 'ELK', class='hermes', chip=-1, channel=-1, error=err)
				if err eq 0 then (*pstate).debug_select_elk = v[(*play).data.index]
				qelk = where(v ne 0, nqelk)
				(*pstate).debug_ean_mode = 0
				if nqean gt 0 then (*pstate).debug_ean_mode = 1
				if nqelk gt 0 then (*pstate).debug_ean_mode = 2

				lock = socket_command_get( ps, 'LOCK', class='scepter', chip=-1, error=err1)
				q = where( lock eq 1, nq)
				(*pstate).debug_aux_mode = (nq ne 0)
				if (*pm).version.scepter ge 6 then begin
					v = socket_command_get( ps, 'MASK', class='scepter', chip=-1, channel=-1, error=err)
					if err eq 0 then (*pstate).debug_select_aux = 1-v[(*play).data.index]
				endif else begin
					bla = socket_command_get( ps, 'BLA', class='scepter', chip=-1, error=err2)
					if (err1 eq 0) and (err2 eq 0) then begin
						v = intarr((*pm).n_detectors)
						q = where( lock eq 1, nq)
						if nq gt 0 then v[q[0]*32 + bla[q[0]]] = 1
						(*pstate).debug_select_aux = v[(*play).data.index]
					endif
				endelse
				if ((*pstate).debug_ean_check eq 0) and ((*pstate).debug_aux_check eq 0) then begin
					if (*pstate).debug_ean_mode ne 0 then begin
						(*pstate).debug_ean_check = 1
					endif else if (*pstate).debug_aux_mode ne 0 then begin
						(*pstate).debug_aux_check = 1
					endif
				endif
				v = socket_command_get( ps, 'monitor.select', class='dam', /string, error=err)
				i = where( (*pstate).monitor_modes eq v, nq)
				if err eq 0 then begin
					if nq ge 1 then (*pstate).debug_monitor_mode = i[0]
				endif

				widget_control, (*pstate).debug_ean_check_id, set_value=(*pstate).debug_ean_check
				widget_control, (*pstate).debug_aux_check_id, set_value=(*pstate).debug_aux_check
				widget_control, (*pstate).enable_debug_ean_mode, set_combobox_select=(*pstate).debug_ean_mode, sensitive=(*pstate).debug_ean_check
				widget_control, (*pstate).enable_debug_aux_mode, set_combobox_select=(*pstate).debug_aux_mode, sensitive=(*pstate).debug_aux_check
				widget_control, (*pstate).enable_debug_monitor_mode, set_combobox_select=(*pstate).debug_monitor_mode
				end
			else:
		endcase
		
		if ((*pstate).tab_names[(*pstate).tab] eq 'Hermes') or ((*pstate).tab_names[(*pstate).tab] eq 'Scepter') then begin
			(*pstate).select = (*pstate).asic_select
			if (*pstate).colour_mode eq 0 then widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Groups') then begin
			(*pstate).select = (*pstate).groups_select
			if (*pstate).colour_mode eq 0 then widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Pulser') then begin
			(*pstate).select = (*pstate).pulser_select
			if (*pstate).colour_mode eq 0 then widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Debug') then begin
			case (*pstate).debug_ean_mode of
				1: begin					; HERMES OAN
					(*pstate).select = (*pstate).debug_select_ean
					end
				2: begin					; HERMES ELK
					(*pstate).select = (*pstate).debug_select_elk
					end
				else:
			endcase
			if (*pstate).colour_mode eq 0 then widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Enable') then begin
			widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*(*pstate).pdisable), alt:1}
			(*pstate).select = *(*pstate).pdisable
		endif 
		
		maia_setup_check_group_mode, pstate
		(*pstate).last_check = (*pstate).check_mode
		widget_control, (*pstate).display_toggle, set_value=(*pstate).colour_mode
		if (*pstate).colour_mode eq 1 then begin
			maia_setup_colour, pstate, /redisplay
		endif else begin
			widget_control, (*pstate).detector, set_value={legend:['','','','']}
		endelse
		widget_control, (*pstate).hermes_apply_button, sensitive=1-(*pstate).colour_mode
		widget_control, (*pstate).scepter_apply_button, sensitive=1-(*pstate).colour_mode
		widget_control, (*pstate).detector_mode, sensitive=1-(*pstate).colour_mode
		widget_control, (*pstate).colour_display_text, sensitive=(*pstate).colour_mode
		end

	'apply-hermes-enable': begin
		(*(*pstate).pdisable)[*] = (*pstate).select
		socket_command_set, ps, 'ECH', 0, class='hermes', chip=-1, channel=-1 
		q = where( (*(*pstate).pdisable) eq 1, nq)
		if nq gt 0 then begin
			c = (*play).data[q].index mod 32
			h = (*play).data[q].hermes
			socket_command_set, ps, 'ECH', 1, class='hermes', chip=h, channel=c 
		endif
		end
		
	'read-enable': begin
		widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*(*pstate).pdisable), alt:1}
		(*pstate).select = *(*pstate).pdisable
		end
		
	'save-enable': begin
		(*(*pstate).pdisable)[*] = (*pstate).select
		file = strip_file_ext((*pm).file,/double) + '.enable.var'
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /write, filter = ['*.enable.var','*.enable.csv'], file=file, $
			path=path, group=event.top, $
			title='Select the Enable var file', fix_filter=1)
		if F ne '' then begin
			write_maia_enable, F, data=*(*pstate).pdisable, index=(*play).ref, error=error
			if error eq 0 then *(*pstate).path = path
		endif
		end
		
	'load-enable': begin
		file = strip_file_ext((*pm).file,/double) + '.enable.var'
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = ['*.enable.var','*.enable.csv'], file=file, $
			path=path, group=event.top, $
			title='Select the Enable var file', fix_filter=1)
		if F ne '' then begin
			disable = read_maia_enable( F, index=(*play).data.index, error=err)
			if err eq 0 then begin
				*(*pstate).path = path
				(*(*pstate).pdisable) = disable
				(*pstate).select = disable
				if ((*pstate).tab_names[(*pstate).tab] eq 'Enable') then begin
					widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*(*pstate).pdisable), alt:1}
				endif
			endif
		endif
		end
		
	'enable-rr-check': begin
		widget_control, (*pstate).enable_rr_check, sensitive=0
		socket_command_set, ps, 'enable', event.select, class='readout.clock', chip=event.value
		(*pstate).enable_rr_freeze = 1
		end
		
	'enable-quad-check': begin
		widget_control, (*pstate).enable_quad_check, sensitive=0
		socket_command_set, ps, 'enable', event.select, class='readout.quad', chip=event.value
		(*pstate).enable_quad_freeze = 1
		end
		
	'enable-modules-check': begin
		widget_control, (*pstate).enable_modules_check, sensitive=0
		(*pstate).enable_modules_freeze = 1
		case event.value of
			0: begin
				if ((*pm).version.software ge 9542) then begin
					socket_command_set, ps, 'ENABLE', event.select, class='linearise2'
					socket_command_set, ps, 'ENABLE', 0, class='linearise'
				endif else begin
					socket_command_set, ps, 'ENABLE', event.select, class='linearise'
				endelse
				info = { on:event.select, file:(*pm).linear.file }
				s = stringify( info)
;				socket_command_set, ps, 'info', s, class='linearise'
				socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_linearise
				end
			1: begin
				socket_command_set, ps, 'ENABLE', event.select, class='gaintrim'
				info = { on:event.select, file:(*pm).trim.file, file2:(*pm).trim.file2 }
				s = stringify( info)
;				socket_command_set, ps, 'info', s, class='gaintrim'
				socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_gaintrim
				end
			2: begin
				socket_command_set, ps, 'ENABLE', event.select, class='pileup'
				info = { on:event.select, file:(*pm).pileup.file }
				s = stringify( info)
;				socket_command_set, ps, 'info', s, class='pileup'
				socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_pileup
				s2 = (event.select) ? (*pm).pileup.file : '' 
				socket_command_set, ps, 'info', s2, class='pileup'
				end
			3: begin
				socket_command_set, ps, 'ENABLE', event.select, class='throttle'
				info = { on:event.select, file:(*pm).throttle.file }
				s = stringify( info)
;				socket_command_set, ps, 'info', s, class='throttle'
				socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_throttle
				s2 = (event.select) ? (*pm).throttle.file : '' 
				socket_command_set, ps, 'info', s2, class='throttle'
				end
			4: begin
				socket_command_set, ps, 'ENABLE', event.select, class='da'
				end
			5: begin
;				socket_command_set, ps, 'ENABLE', event.select, class='roi'
				(*pm).ROI.on = 0
				widget_control, (*pstate).enable_modules_check, set_value=[(*pm).linear.on,(*pm).trim.on, $
							(*pm).pileup.on,(*pm).throttle.on,(*pm).DA.on,(*pm).ROI.on, (*pm).groups.on]
				end
			6: begin
				if (*pm).number.spectra gt 0 then begin
					socket_command_set, ps, 'ENABLE', event.select, class='spectrum', chip=-1, n_chips=(*pm).number.spectra
				endif
				end
			7: begin
				(*pm).DA.save = event.select
				end
			else:
		endcase
		end

	'enable-stream-check': begin
		widget_control, (*pstate).enable_stream_check, sensitive=0
		(*pstate).enable_stream_freeze = 1
		case event.value of
			0: begin
				socket_command_set, ps, 'ENABLE', event.select, class='scepter'
				end
			1: begin
				socket_command_set, ps, 'ENABLE', event.select, class='event'
				end
			2: begin
				socket_command_set, ps, 'ENABLE', event.select, class='photon'
				end
			3: begin
				socket_command_set, ps, 'ENABLE', event.select, class='accum'
				end
			4: begin
				socket_command_set, ps, 'ENABLE', event.select, class='activity'
				end
			5: begin
				socket_command_set, ps, 'ENABLE', event.select, class='deadtime'
				end
			6: begin
				socket_command_set, ps, 'ENABLE', event.select, class='pixel'
				end
			7: begin
				(*pm).chart.on = event.select
				end
			else:
		endcase
		end

	'hermes-time-mode': begin
		(*pstate).hermes.time = event.index
		widget_control, (*pstate).check_ids_hermes_chip[0], set_value=1
		end
		
	'hermes-gain-mode': begin
		(*pstate).hermes.gain = event.index
		widget_control, (*pstate).check_ids_hermes_chip[1], set_value=1
		end
		
	'hermes-eblk-mode': begin
		(*pstate).hermes.eblk = event.index
		widget_control, (*pstate).check_ids_hermes_chip[2], set_value=1
		end
		
	'hermes-elk-mode': begin
		(*pstate).hermes.elk = event.index
		widget_control, (*pstate).check_ids_hermes_channel[0], set_value=1
		end
		
	'scepter-rr-slider': begin
		(*pstate).scepter.clock = event.value
		widget_control, (*pstate).check_ids_hermes_whole[0], set_value=1
		end
		
;	'class_check_ids' are the IDs for the groups of parameters
;	'check_ids' are the IDs for the check boxes on the right in each group

	'hermes-channel-check': begin
		for i=0L,n_elements((*pstate).class_check_ids)-1 do begin
			if widget_info((*pstate).class_check_ids[i],/valid) then widget_control, (*pstate).class_check_ids[i], set_value=0			; clear all check boxes
		endfor
		widget_control, event.id, set_value=1
		for i=0L,n_elements((*pstate).check_ids)-1 do begin
			if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all check boxes
		endfor
		(*pstate).group_mask = [1,0,0,0,0,0,0,0]
		(*pstate).old_mask = (*pstate).group_mask
		maia_setup_check_group_mode, pstate
		maia_setup_sensitive, pstate, hermes=1
		end

	'hermes-chip-check': begin
		for i=0L,n_elements((*pstate).class_check_ids)-1 do begin
			if widget_info((*pstate).class_check_ids[i],/valid) then widget_control, (*pstate).class_check_ids[i], set_value=0			; clear all check boxes
		endfor
		widget_control, event.id, set_value=1
		for i=0L,n_elements((*pstate).check_ids)-1 do begin
			if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all check boxes
		endfor
		(*pstate).group_mask = [0,0,0,0,0,1,1,1]
		(*pstate).old_mask = (*pstate).group_mask
		maia_setup_check_group_mode, pstate
		maia_setup_sensitive, pstate, hermes=2
		end

	'hermes-whole-check': begin
		for i=0L,n_elements((*pstate).class_check_ids)-1 do begin
			if widget_info((*pstate).class_check_ids[i],/valid) then widget_control, (*pstate).class_check_ids[i], set_value=0			; clear all check boxes
		endfor
		widget_control, event.id, set_value=1
		for i=0L,n_elements((*pstate).check_ids)-1 do begin
			if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all check boxes
		endfor
		(*pstate).group_mask = [0,0,0,0,0,0,0,1]
		(*pstate).old_mask = (*pstate).group_mask
		maia_setup_check_group_mode, pstate
		maia_setup_sensitive, pstate, hermes=3
		end

	'hermes-time-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.hermes.time, title=u.label
		endif
		end
		
	'hermes-gain-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.hermes.gain, title=u.label
		endif
		end
		
	'hermes-eblk-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.hermes.eblk, title=u.label
		endif
		end
		
	'hermes-elk-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.hermes.elk, title=u.label
		endif
		end
		
	'tokreset': begin
		end
		
	'hermes-apply': begin
		q = where((*pstate).select eq 1,nq)
		if nq eq 0 then begin
			warning,'maia_setup_event',['No detectors are selected.','Select detectors and try again.']
			goto, finish
		endif
		c = (*play).data[q].index mod 32
		h = (*play).data[q].hermes
		none = 1
		
		widget_control, (*pstate).hermes_time_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].hermes.time = (*pstate).hermes.time
				socket_command_set, ps, 'TIME', (*pm).channel[q[0]].hermes.time, class='hermes', chip=h 
				none = 0
			endif
		endif
		widget_control, (*pstate).hermes_gain_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].hermes.gain = (*pstate).hermes.gain
				socket_command_set, ps, 'GAIN', (*pm).channel[q[0]].hermes.gain, class='hermes', chip=h 
				none = 0
			endif
		endif
		widget_control, (*pstate).hermes_eblk_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].hermes.eblk = (*pstate).hermes.eblk
				socket_command_set, ps, 'EBLK', (*pm).channel[q[0]].hermes.eblk, class='hermes', chip=h 
				none = 0
			endif
		endif
;		widget_control, (*pstate).hermes_elk_mode, get_uvalue=u
;		if widget_info((*pstate).check_ids[u.id],/valid) then begin
;			widget_control, (*pstate).check_ids[u.id], get_value=on
;			if on then begin
;				c = (*play).data[q[0]].index mod 32
;				h = (*play).data[q[0]].hermes
;				(*pm).channel[*].hermes.elk = 0
;				socket_command_set, ps, 'ELK', 0, class='hermes', chip=-1, channel=-1
;				(*pm).channel[q[0]].hermes.elk = (*pstate).hermes.elk
;				socket_command_set, ps, 'ELK', (*pm).channel[q[0]].hermes.elk, class='hermes', chip=h, channel=c
;				none = 0
;				socket_command_set, ps, 'EAN', 0, class='hermes', chip=-1, channel=-1
;			endif
;		endif
		widget_control, (*pstate).scepter_rr_slider, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].scepter.clock = (*pstate).scepter.clock
				socket_command_set, ps, 'clock.rate', (*pm).channel[q[0]].scepter.clock * 1.0e+6, class='readout'
				none = 0
			endif
		endif
		if none then warning,'maia_setup_event',['No HERMES parameters are selected.','Select/modify a parameter and try again.']

		for i=0L,n_elements((*pstate).check_ids)-1 do begin
			if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all check boxes
		endfor
		maia_setup_check_warning, pstate
		maia_setup_load_table, pstate		
		end
		
	'stop-eblk-button': begin
		socket_command_set, ps, 'EBLK', 0, class='hermes', chip=-1 
		(*pm).channel.hermes.eblk = 0
		maia_setup_check_warning, pstate
		maia_setup_load_table, pstate
		end
		
	'scepter-trim-slider': begin
		(*pstate).scepter.trim = event.value
		widget_control, (*pstate).check_ids_scepter_channel[0], set_value=1
		(*pstate).scepter.tweak.trim.on = 0
		end
		
	'scepter-trim-down': begin
		if event.select eq 1 then begin
			widget_control, (*pstate).check_ids_scepter_channel[0], set_value=1
			if (*pstate).scepter.tweak.trim.on eq 0 then (*pstate).scepter.tweak.trim.val = 0.0
			(*pstate).scepter.tweak.trim.on = 1
			(*pstate).scepter.tweak.trim.val = (*pstate).scepter.tweak.trim.val + 0.01
			widget_control, (*pstate).scepter_trim_slider, set_value=(*pstate).scepter.trim + (*pstate).scepter.tweak.trim.val
		endif
		end

	'scepter-trim-up': begin
		if event.select eq 1 then begin
			widget_control, (*pstate).check_ids_scepter_channel[0], set_value=1
			if (*pstate).scepter.tweak.trim.on eq 0 then (*pstate).scepter.tweak.trim.val = 0.0
			(*pstate).scepter.tweak.trim.on = 1
			(*pstate).scepter.tweak.trim.val = (*pstate).scepter.tweak.trim.val - 0.01
			widget_control, (*pstate).scepter_trim_slider, set_value=(*pstate).scepter.trim + (*pstate).scepter.tweak.trim.val
		endif
		end

	'scepter-tdm-mode': begin
		(*pstate).scepter.tdm = event.index
		widget_control, (*pstate).check_ids_scepter_chip[0], set_value=1
		end
		
	'scepter-tds-mode': begin
		(*pstate).scepter.tds = event.index
		widget_control, (*pstate).check_ids_scepter_chip[1], set_value=1
		end
		
	'scepter-tos-mode': begin
		(*pstate).scepter.tos = event.index
		widget_control, (*pstate).check_ids_scepter_chip[2], set_value=1
		end
		
	'scepter-trk-mode': begin
		(*pstate).scepter.trk = event.index
		widget_control, (*pstate).check_ids_scepter_chip[3], set_value=1
		end
		
	'scepter-trke-mode': begin
		(*pstate).scepter.trke = event.index
		widget_control, (*pstate).check_ids_scepter_chip[4], set_value=1
		end
		
	'scepter-filt-mode': begin
		(*pstate).scepter.filt = event.index
		widget_control, (*pstate).check_ids_scepter_chip[5], set_value=1
		end
		
	'scepter-tcm-mode': begin
		(*pstate).scepter.tcm = event.index
		widget_control, (*pstate).check_ids_scepter_chip[6], set_value=1
		end
		
	'scepter-thresh-slider': begin
		(*pstate).scepter.thresh = event.value
		widget_control, (*pstate).check_ids_scepter_chip[7], set_value=1
		(*pstate).scepter.tweak.thresh.on = 0
		end
		
	'scepter-thresh-up': begin
		if event.select eq 1 then begin
			widget_control, (*pstate).check_ids_scepter_chip[7], set_value=1
			if (*pstate).scepter.tweak.thresh.on eq 0 then (*pstate).scepter.tweak.thresh.val = 0.0
			(*pstate).scepter.tweak.thresh.on = 1
			(*pstate).scepter.tweak.thresh.val = (*pstate).scepter.tweak.thresh.val + 0.01
			widget_control, (*pstate).scepter_thresh_slider, set_value=(*pstate).scepter.thresh + (*pstate).scepter.tweak.thresh.val
		endif
		end

	'scepter-thresh-down': begin
		if event.select eq 1 then begin
			widget_control, (*pstate).check_ids_scepter_chip[7], set_value=1
			if (*pstate).scepter.tweak.thresh.on eq 0 then (*pstate).scepter.tweak.thresh.val = 0.0
			(*pstate).scepter.tweak.thresh.on = 1
			(*pstate).scepter.tweak.thresh.val = (*pstate).scepter.tweak.thresh.val - 0.01
			widget_control, (*pstate).scepter_thresh_slider, set_value=(*pstate).scepter.thresh + (*pstate).scepter.tweak.thresh.val
		endif
		end

	'scepter-thpd-slider': begin
		(*pstate).scepter.thpd = event.value
		widget_control, (*pstate).check_ids_scepter_chip[8], set_value=1
		end
		
;	'class_check_ids' are the IDs for the groups of parameters
;	'check_ids' are the IDs for the check boxes on the right in each group

	'scepter-channel-check': begin
		for i=0L,n_elements((*pstate).class_check_ids)-1 do begin
			if widget_info((*pstate).class_check_ids[i],/valid) then widget_control, (*pstate).class_check_ids[i], set_value=0			; clear all check boxes
		endfor
		widget_control, event.id, set_value=1
		for i=0L,n_elements((*pstate).check_ids)-1 do begin
			if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all check boxes
		endfor
		(*pstate).group_mask = [0,1,1,1,1,1,1,1]
		(*pstate).old_mask = (*pstate).group_mask
		maia_setup_check_group_mode, pstate
		maia_setup_sensitive, pstate, scepter=1
		end

	'scepter-chip-check': begin
		for i=0L,n_elements((*pstate).class_check_ids)-1 do begin
			if widget_info((*pstate).class_check_ids[i],/valid) then widget_control, (*pstate).class_check_ids[i], set_value=0			; clear all check boxes
		endfor
		widget_control, event.id, set_value=1
		for i=0L,n_elements((*pstate).check_ids)-1 do begin
			if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all check boxes
		endfor
		(*pstate).group_mask = [0,0,0,0,0,1,1,1]
		(*pstate).old_mask = (*pstate).group_mask
		maia_setup_check_group_mode, pstate
		maia_setup_sensitive, pstate, scepter=2
		end

	'scepter-tdm-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.scepter.tdm, title=u.label
		endif
		end
		
	'scepter-tds-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.scepter.tds, title=u.label
		endif
		end
		
	'scepter-tos-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.scepter.tos, title=u.label
		endif
		end
		
	'scepter-trk-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.scepter.trk, title=u.label
		endif
		end
		
	'scepter-trke-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.scepter.trke, title=u.label
		endif
		end
		
	'scepter-filt-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.scepter.filt, title=u.label
		endif
		end
		
	'scepter-tcm-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.scepter.tcm, title=u.label
		endif
		end
		
	'scepter-trim-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.scepter.trim, title=u.label
		endif
		end
		
	'scepter-thresh-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.scepter.thresh, title=u.label
		endif
		end
		
	'scepter-thresh-trim-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.scepter.thresh - (*pm).channel.scepter.trim, title=u.label
		endif
		end
		
	'scepter-thpd-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.scepter.thpd, title=u.label
		endif
		end
		
	'scepter-rr-check': begin
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
			q = where( event.id eq (*pstate).check_ids, nq)
			if nq gt 0 then (*pstate).check_mode=q[0]
			widget_control, event.id, set_value=1
			widget_control, event.id, get_uvalue=u
			widget_control, (*pstate).colour_display_text, set_value=u.label
			maia_setup_colour, pstate, (*pm).channel.scepter.clock, title=u.label
		endif
		end
		
	'scepter-apply': begin
		q = where((*pstate).select eq 1,nq)
		if nq eq 0 then begin
			warning,'maia_setup_event',['No detectors are selected.','Select detectors and try again.']
			goto, finish
		endif
		c = (*play).data[q].index mod 32			; channel on chip for index list 'q'
		h = (*play).data[q].hermes					; Hermes chip for these 'q'
		none = 1
		
		widget_control, (*pstate).scepter_tdm_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].scepter.tdm = (*pstate).scepter.tdm
				socket_command_set, ps, 'TDM', (*pm).channel[q[0]].scepter.tdm, class='scepter', chip=h
				none = 0
			endif
		endif
		widget_control, (*pstate).scepter_tds_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].scepter.tds = (*pstate).scepter.tds
				socket_command_set, ps, 'TDS', (*pm).channel[q[0]].scepter.tds, class='scepter', chip=h
				none = 0
			endif
		endif
		widget_control, (*pstate).scepter_tos_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].scepter.tos = (*pstate).scepter.tos
				socket_command_set, ps, 'TOS', (*pm).channel[q[0]].scepter.tos, class='scepter', chip=h
				none = 0
			endif
		endif
		
		; Note that thpd and trim are stored as positive, but set as negative values ...
		
		if (*pm).version.scepter ge 6 then begin
			widget_control, (*pstate).scepter_trim_slider, get_uvalue=u
			if widget_info((*pstate).check_ids[u.id],/valid) then begin
				widget_control, (*pstate).check_ids[u.id], get_value=on
				if on then begin
					if (*pstate).scepter.tweak.trim.on then begin
						(*pm).channel[q].scepter.trim = clip((*pm).channel[q].scepter.trim + (*pstate).scepter.tweak.trim.val ,0,0.15)
						socket_command_set, ps, 'trim', -(*pm).channel[q].scepter.trim, class='scepter', chip=h, channel=c
					endif else begin
						(*pm).channel[q].scepter.trim = (*pstate).scepter.trim
						socket_command_set, ps, 'trim', -(*pm).channel[q[0]].scepter.trim, class='scepter', chip=h, channel=c
					endelse
					none = 0
				endif
			endif
		endif
		if (*pm).version.scepter ge 7 then begin
			widget_control, (*pstate).scepter_thpd_slider, get_uvalue=u
			if widget_info((*pstate).check_ids[u.id],/valid) then begin
				widget_control, (*pstate).check_ids[u.id], get_value=on
				if on then begin
					(*pm).channel[q].scepter.thpd = (*pstate).scepter.thpd
					socket_command_set, ps, 'thpd', -(*pm).channel[q[0]].scepter.thpd, class='scepter', chip=h
					none = 0
				endif
			endif
		endif
		widget_control, (*pstate).scepter_thresh_slider, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				if (*pstate).scepter.tweak.thresh.on then begin
					(*pm).channel[q].scepter.thresh = clip((*pm).channel[q].scepter.thresh + (*pstate).scepter.tweak.thresh.val ,0,2.0)
					q2 = sort_unique(h, nq2)
					for i=0,nq2-1 do begin
						socket_command_set, ps, 'thresh', (*pm).channel[q[q2[i]]].scepter.thresh, class='scepter', chip=h[q2[i]]
					endfor
				endif else begin
					(*pm).channel[q].scepter.thresh = (*pstate).scepter.thresh
					socket_command_set, ps, 'thresh', (*pm).channel[q[0]].scepter.thresh, class='scepter', chip=h
				endelse
				none = 0
			endif
		endif
		widget_control, (*pstate).scepter_trk_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].scepter.trk = (*pstate).scepter.trk
				socket_command_set, ps, 'TRK', (*pm).channel[q[0]].scepter.trk, class='scepter', chip=h
				none = 0
			endif
		endif
		widget_control, (*pstate).scepter_trke_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].scepter.trke = (*pstate).scepter.trke
				socket_command_set, ps, 'TRKE', (*pm).channel[q[0]].scepter.trke, class='scepter', chip=h
				none = 0
			endif
		endif
		if (*pm).version.scepter ge 6 then begin
			widget_control, (*pstate).scepter_tcm_mode, get_uvalue=u
			if widget_info((*pstate).check_ids[u.id],/valid) then begin
				widget_control, (*pstate).check_ids[u.id], get_value=on
				if on then begin
					(*pm).channel[q].scepter.tcm = (*pstate).scepter.tcm
					socket_command_set, ps, 'TCM', (*pm).channel[q[0]].scepter.tcm, class='scepter', chip=h
					none = 0
				endif
			endif
			widget_control, (*pstate).scepter_filt_mode, get_uvalue=u
			if widget_info((*pstate).check_ids[u.id],/valid) then begin
				widget_control, (*pstate).check_ids[u.id], get_value=on
				if on then begin
					(*pm).channel[q].scepter.filt = (*pstate).scepter.filt
					socket_command_set, ps, 'FILT', (*pm).channel[q[0]].scepter.filt, class='scepter', chip=h
					none = 0
				endif
			endif
		endif
		if none then warning,'maia_setup_event',['No SCEPTER parameters are selected.','','Select detector channels on the right,', $
			'a bank of parameters on the left, ', 'and adjust one or more to try again.']

		for i=0L,n_elements((*pstate).check_ids)-1 do begin
			if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all check boxes
		endfor
		(*pstate).scepter.tweak.trim.on = 0
		(*pstate).scepter.tweak.thresh.on = 0
		maia_setup_load_table, pstate
		end
		
	'hymod-linearization-check': begin
		(*pl).linear.on = event.select
		end
		
	'hymod-gain-trim-check': begin
		(*pl).trim.on = event.select
		end
		
	'hymod-pileup-check': begin
		(*pl).pileup.on = event.select
		end
		
	'hymod-throttle-check': begin
		(*pl).throttle.on = event.select
		end
		
	'load-linearization': begin
		file = find_file2( (*pl).linear.file)
		if file[0] eq '' then file=strip_path((*pl).linear.file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter=['*.linear.var','*.linear'], file=file[0], $
			path=path, group=event.top, $
			title='Select the linearization function file', fix_filter=1)
		if F ne '' then begin
			set_widget_text, (*pstate).hymod_linearization_text, F
			(*pl).linear.file = F
			*(*pstate).path = extract_path(F) 
		endif
		end
		
	'load-gain-trim': begin
		file = find_file2( (*pl).trim.file)
		if file[0] eq '' then file=strip_path((*pl).trim.file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = ['*.gaintrim.*.var','*.spec'], file=file[0], $
			path=path, group=event.top, $
			title='Select the gain trim file', fix_filter=0)
		if F ne '' then begin
			set_widget_text, (*pstate).hymod_gain_trim_text, F
			(*pl).trim.file = F
			*(*pstate).path = extract_path(F) 
		endif
		end
		
	'load-gain-trim2': begin
		file = find_file2( (*pl).trim.file2)
		if file[0] eq '' then file=strip_path((*pl).trim.file2)
		path = extract_path( (*pl).trim.file)
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = ['*.gaintrim.*.var','*.spec'], file=file[0], $
			path=path, group=event.top, $
			title='Select the gain trim file 2', fix_filter=0)
		if F ne '' then begin
			set_widget_text, (*pstate).hymod_gain_trim_text2, F
			(*pl).trim.file2 = F
			*(*pstate).path = extract_path(F) 
		endif
		end
		
	'hymod-gaintrim-T-off-up': begin
		if (*pl).trim.on eq 0 then begin
			warning,'maia_setup','Need to enable gain-trim first.'
			goto, finish
		endif
		if event.select then begin
			q = where( (*pstate).select eq 1, nq)
			if nq gt 0 then begin
				(*pl).channel[q].trim.T.b = (*pl).channel[q].trim.T.b + 10
			endif
		endif
		end
		
	'hymod-gaintrim-T-off-down': begin
		if (*pl).trim.on eq 0 then begin
			warning,'maia_setup','Need to enable gain-trim first.'
			goto, finish
		endif
		if event.select then begin
			q = where( (*pstate).select eq 1, nq)
			if nq gt 0 then begin
				(*pl).channel[q].trim.T.b = (*pl).channel[q].trim.T.b - 10
			endif
		endif
		end
		
	'hymod-gaintrim-T-gain-up': begin
		if (*pl).trim.on eq 0 then begin
			warning,'maia_setup','Need to enable gain-trim first.'
			goto, finish
		endif
		if event.select then begin
			q = where( (*pstate).select eq 1, nq)
			if nq gt 0 then begin
				(*pl).channel[q].trim.T.a = (*pl).channel[q].trim.T.a + 0.02
			endif
		endif
		end
		
	'hymod-gaintrim-T-gain-down': begin
		if (*pl).trim.on eq 0 then begin
			warning,'maia_setup','Need to enable gain-trim first.'
			goto, finish
		endif
		if event.select then begin
			q = where( (*pstate).select eq 1, nq)
			if nq gt 0 then begin
				(*pl).channel[q].trim.T.a = (*pl).channel[q].trim.T.a - 0.02
			endif
		endif
		end
		
	'hymod-apply-gaintrim-adj': begin
		if (*pl).trim.on eq 0 then begin
			warning,'maia_setup','Need to enable gain-trim first.'
			goto, finish
		endif
		q = where( (*pstate).select eq 1, nq)
		for i=0,nq-1 do begin
			j = (*play).data[q[i]].index
			ba = [(*pl).channel[q[i]].trim.T.b, (*pl).channel[q[i]].trim.T.a]
			socket_command_set, ps, 'Tcoeff', ba, class='gaintrim.det', chip=j, channel=-1, n_channels=2, /quiet
		endfor
		end
	
	'hymod-save-gaintrim': begin
		if (*pl).trim.on eq 0 then begin
			warning,'maia_setup','Need to enable gain-trim first.'
			goto, finish
		endif
		file = find_file2( (*pl).trim.file2)
		if file[0] eq '' then file=strip_path((*pl).trim.file2)
		path = extract_path( (*pl).trim.file2)
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /write, filter = '*.gaintrim.time.var', file=file[0], $
			path=path, group=event.top, title='Write Time Gain Trim file', fix_filter=0)
		if F ne '' then begin
			on_ioerror, bad_open
			openw, lun, F[0], /get_lun
			printf, lun, '# gaintrim time'
			printf, lun, '# written by "maia_setup: Save gain-trim adjust", ' + systime()
			printf, lun, '# file: ' + F[0]
			printf, lun, '# Maia type: ' + (*pm).identity.dam
			printf, lun, 'gaintrim.det[].tcoeff[0] 0'
			printf, lun, 'gaintrim.det[].tcoeff[1] 1'
			
			for i=0,(*pm).n_detectors-1 do begin
				j = (*play).data[i].index
				sba = strjoin(strtrim(string([(*pl).channel[i].trim.T.b, (*pl).channel[i].trim.T.a]),2),' ')
				printf, lun, 'gaintrim.det['+strtrim(string(j),2)+'].tcoeff[] ' + sba
			endfor
			
			(*pl).trim.file2 = F[0]
			if (*pm).version.software ge 4737 then begin
				info = { on:(*pl).trim.on, file:(*pl).trim.file, file2:(*pl).trim.file2 }
				s = stringify( info)
;				socket_command_set, ps, 'info', s, class='gaintrim'
				socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_gaintrim
			endif
			set_widget_text, (*pstate).hymod_gain_trim_text2, (*pl).trim.file2
			(*pm).trim = (*pl).trim

bad_open:
			close_file, lun
		endif
		end
		
	'load-pileup': begin
		file = find_file2( (*pl).pileup.file)
		if file[0] eq '' then file=strip_path((*pl).pileup.file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = ['*.pileup.var','*.txt'], file=file[0], $
			path=path, group=event.top, $
			title='Select the pileup limits file', fix_filter=0)
		if F ne '' then begin
			set_widget_text, (*pstate).hymod_pileup_text, F
			(*pl).pileup.file = F
			*(*pstate).path = extract_path(F) 
		endif
		end
		
	'load-throttle': begin
		file = find_file2( (*pl).throttle.file)
		if file[0] eq '' then file=strip_path((*pl).throttle.file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = ['*.throttle.var','*.txt'], file=file[0], $
			path=path, group=event.top, $
			title='Select the throttle factors file', fix_filter=0)
		if F ne '' then begin
			set_widget_text, (*pstate).hymod_throttle_text, F
			(*pl).throttle.file = F
			*(*pstate).path = extract_path(F) 
		endif
		end
		
	'hymod-cal-mode': begin
		(*pstate).cal_mode = event.index
		end
		
	'deadtime-offset': begin
		widget_control, event.id, get_value=s
		if gnumeric(s) then begin
			(*pl).deadtime.cal.b = float2(s)
		endif else begin
			warning,'Maia_setup_event',['Illegal character in "Deadtime offset" numeric value.','Try again ...'],/error
		endelse
		end
		
	'deadtime-slope': begin
		widget_control, event.id, get_value=s
		if gnumeric(s) then begin
			(*pl).deadtime.cal.a = float2(s)
		endif else begin
			warning,'Maia_setup_event',['Illegal character in Deadtime-slope" numeric value.','Try again ...'],/error
		endelse
		end
		
	'hymod-deadtime-auto': begin
		(*pl).deadtime.auto = event.select
		end
		
	'load-cal': begin
		file = find_file2( (*pl).cal.file)
		if file[0] eq '' then file=strip_path((*pl).cal.file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = '*.spec', file=file[0], $
			path=path, group=event.top, $
			title='Select the calibrated SPEC file', fix_filter=0)
		if F ne '' then begin
			set_widget_text, (*pstate).hymod_cal_text, F
			(*pl).cal.file = F
			*(*pstate).path = extract_path(F) 
		endif
		end
		
	'hymod-apply-hymod': begin
		; all parameters on this tab and their enable status
		; force the "enables" off if there are no linearization, trimming, pileup, throttle tables found/entered.

		maia_setup_apply_hymod, pstate, /throttle
		end
		
	'imaging-apply-axes': begin
		; Send the axis redirection settings to the DA Imaging process.

		maia_setup_apply_axes, pstate
		end
		
	'hymod-set-disables': begin
		(*(*pstate).pdisable)[*] = 1 - (*pstate).select
		socket_command_set, ps, 'ECH', 0, class='hermes', chip=-1, channel=-1 
		q = where( (*(*pstate).pdisable) eq 1, nq)
		if nq gt 0 then begin
			c = (*play).data[q].index mod 32
			h = (*play).data[q].hermes
			socket_command_set, ps, 'ECH', 1, class='hermes', chip=h, channel=c 
		endif
		maia_setup_load_table, pstate
		end
	
	'summary-table': begin
		case tag_names( event, /structure_name) of
			'WIDGET_CONTEXT': begin
				end
				
			'WIDGET_TABLE_CELL_SEL': begin
;				help, event,/struct
				if (event.sel_left eq 0) and (event.sel_top eq event.sel_bottom) then begin
					widget_control, (*pstate).detector, set_value={select:(*pstate).sort.index[event.sel_top], value:1, alt:0}
					view = widget_info( (*pstate).summary_table, /table_view)
					widget_control, (*pstate).summary_table, set_table_select=[0,event.sel_top,29,event.sel_top]
					widget_control, (*pstate).summary_table, set_table_view=view
					goto, finish
				endif
				if ((*pstate).colour_mode eq 1) and (event.sel_left ge 0) then begin
					i = event.sel_left
					for j=0L,n_elements((*pstate).check_ids)-1 do begin
						if widget_info((*pstate).check_ids[j],/valid) then widget_control, (*pstate).check_ids[j], set_value=0			; clear all other check boxes
					endfor
					check = (*pstate).table[i].check
					if check ge 0 then begin
						widget_control, (*pstate).check_ids[check], set_value=1
						widget_control, (*pstate).check_ids[check], get_uvalue=u
						widget_control, (*pstate).colour_display_text, set_value=u.label
						title = u.label
					endif else begin
						widget_control, (*pstate).colour_display_text, set_value=(*pstate).table[i].column
						title = (*pstate).table[i].column
					endelse
					view = widget_info( (*pstate).summary_table, /table_view)
					widget_control, (*pstate).summary_table, set_table_select=[i,0,i,n_detectors-1]
					widget_control, (*pstate).summary_table, set_table_view=view
	
					; set the colours to bytscl() the various values ...
					case (*pstate).table[i].chip of
						1: maia_setup_colour, pstate, (*pm).channel.hermes.((*pstate).table[i].par), title=title
						2: maia_setup_colour, pstate, (*pm).channel.scepter.((*pstate).table[i].par), title=title
						3: maia_setup_colour, pstate, (*pm).channel.cal.((*pstate).table[i].par), ignore=where( (abs((*pm).channel.cal.((*pstate).table[i].par)-1.) lt 0.0001) or (abs((*pm).channel.cal.((*pstate).table[i].par)) lt 0.0001)), title=title
						4: maia_setup_colour, pstate, (*pm).channel.trim.E.((*pstate).table[i].par), ignore=where( (abs((*pm).channel.Trim.E.((*pstate).table[i].par)-1.) lt 0.0001) or (abs((*pm).channel.Trim.E.((*pstate).table[i].par)) lt 0.0001)), title=title
						5: maia_setup_colour, pstate, (*pm).channel.trim.T.((*pstate).table[i].par), ignore=where( (abs((*pm).channel.Trim.T.((*pstate).table[i].par)-1.) lt 0.0001) or (abs((*pm).channel.Trim.T.((*pstate).table[i].par)) lt 0.0001)), title=title
						6: maia_setup_colour, pstate, (*play).data.((*pstate).table[i].par), title=title
						else:
					endcase
				endif
				if (event.sel_top eq 0) and (event.sel_bottom eq n_detectors-1) and (event.sel_left ge 0) then begin
					i = event.sel_left
					(*pstate).sort.column=i
					maia_setup_load_table, pstate
				endif
				end

			'WIDGET_TABLE_CH': begin
				end
			else:
		endcase
		end
		
	'group-table': begin
		case tag_names( event, /structure_name) of

			'WIDGET_TABLE_CELL_SEL': begin
				i = event.sel_left
				j = event.sel_top
				if event.sel_left ne event.sel_right then goto, finish
				if (event.sel_top eq event.sel_bottom) and (event.sel_top ge 0) then begin
					(*pstate).group_row = j
					widget_control, (*pstate).group_table, get_value=s, use_table_select=[3,event.sel_top,3,event.sel_top]
					widget_control, (*pstate).group_title_text, set_value=s
					if ((*pstate).colour_mode eq 1) then goto, finish
					case i of
						2: begin								; E/T
							j = event.sel_top
							if j ge (*pl).groups.spectra then goto, finish
							(*pl).groups.group[j].et_mode = 1 - (*pl).groups.group[j].et_mode 
							end
						4: begin								; SET
							j = event.sel_top
							if j ne 12 then (*pl).groups.group[j].table = (*pstate).select
							end
						5: begin								; Pileup
							j = event.sel_top
							(*pl).groups.group[j].pileup = 1 - (*pl).groups.group[j].pileup 
							end
						6: begin								; Throttle
							j = event.sel_top
							(*pl).groups.group[j].throttle = 1 - (*pl).groups.group[j].throttle 
							end
						else: begin
							j = event.sel_top
							(*pstate).select = (*pl).groups.group[j].table
							widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
							end
					endcase
					(*pl).groups.file = ''
					set_widget_text, (*pstate).spectra_file_text, (*pl).groups.file
					maia_setup_load_group_table, pstate
				endif
				end

			'WIDGET_TABLE_CH': begin
				end
			else:
		endcase
		end
		
	'group-title': begin
		widget_control, (*pstate).group_title_text, get_value=s
		(*pl).groups.group[(*pstate).group_row].title = s
		maia_setup_load_group_table, pstate
		end

	'spectra-groups-check': begin
		(*pl).groups.on = event.select
		end
		
	'spectra-groups-read': begin
		maia_setup_groups_read, pstate
		maia_setup_load_group_table, pstate
		set_widget_text, (*pstate).spectra_file_text, (*pl).groups.file
		end
		
	'hymod-apply-spectra': begin
		; only spectra parameters and their enable status
		; force the "enables" off if there are no group tables found/entered.
		
		maia_setup_apply_groups, pstate
		end
		
	'save-groups': begin
		file = find_file2( (*pl).groups.file)
		if file[0] eq '' then file=strip_file_ext((*pm).file,/double) + '.groups.csv'
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /write, filter = '*.groups.csv', file=file[0], $
			path=path, group=event.top, $
			title='Select the Groups CSV file', fix_filter=1)
		if F ne '' then begin
			F = strip_file_ext(F,/double) + '.groups.csv'
			write_maia_groups, F, data=(*pl).groups, error=error
			(*pl).groups.file = F
			set_widget_text, (*pstate).spectra_file_text, F
		endif
		end
		
	'load-groups-file': begin
		file = find_file2( (*pl).groups.file)
		if file[0] eq '' then file=strip_file_ext((*pm).file,/double) + '.groups.csv'
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = '*.groups.csv', file=file[0], $
			path=path, group=event.top, $
			title='Select the Groups CSV file', fix_filter=1)
		if F ne '' then begin
			F = strip_file_ext(F,/double) + '.groups.csv'
			set_widget_text, (*pstate).spectra_file_text, F
			groups = read_maia_groups( F, error=err)
			if err eq 0 then begin
				(*pl).groups.file = F
				(*pl).groups.group = groups
				(*pl).groups.on = 1
				set_widget_text, (*pstate).spectra_file_text, F
				*(*pstate).path = extract_path(F) 
				maia_setup_load_group_table, pstate
			endif
		endif
		end
		
	'imaging-da-check': begin
		(*pl).DA.on = event.select
		end
		
	'imaging-roi-check': begin
		(*pl).ROI.on = event.select
		end
		
	'load-da': begin
		file = find_file2( (*pl).DA.file)
		if file[0] eq '' then file=strip_path((*pl).DA.file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = '*.damx', file=file[0], $
			path=path, group=event.top, $
			title='Select the DA Matrix DAMX file', fix_filter=0)
		if F ne '' then begin
			set_widget_text, (*pstate).imaging_da_text, F
			(*pl).DA.file = F
			*(*pstate).path = extract_path(F) 
		endif
		end
		
	'load-roi': begin
		file = find_file2( (*pl).ROI.file)
		if file[0] eq '' then file=strip_path((*pl).ROI.file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = '*.cuts', file=file[0], $
			path=path, group=event.top, $
			title='Select the ROI CUTS file', fix_filter=0)
		if F ne '' then begin
			set_widget_text, (*pstate).imaging_roi_text, F
			(*pl).ROI.file = F
			*(*pstate).path = extract_path(F) 
		endif
		end
		
	'imaging-save-check': begin
		(*pl).DA.save = event.select
		end
		
	'da-save-text': begin
		widget_control, (*pstate).save_path_text, get_value=s
		if file_test(s, /dir) eq 0 then begin
			warning,'maia_setup_event',['Directory not found.','Save path not changed.']
		endif else begin
			(*pl).DA.save_path = s
			set_widget_text, (*pstate).save_path_text, s
		endelse
		end
		
	'da-save-path': begin
		path = (*pl).DA.save_path
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = '*', /dir, $
			path=path, group=event.top, $
			title='Select the path to save RT images', fix_filter=0)
		if F ne '' then begin
			set_widget_text, (*pstate).save_path_text, F
			(*pl).DA.save_path = F
			*(*pstate).path = F
		endif
		end
		
	'imaging-apply-rt': begin
		(*pm).DA.save = (*pl).DA.save
		(*pm).DA.save_path = (*pl).DA.save_path
		module_check = [(*pm).linear.on,(*pm).trim.on, (*pm).pileup.on,(*pm).throttle.on,(*pm).DA.on,(*pm).ROI.on, (*pm).groups.on, (*pm).DA.save]
		widget_control, (*pstate).enable_modules_check, set_value=module_check		
		end
		
	'charge-mode': begin
		(*pl).IC.mode = event.index
		case (*pl).IC.mode of
			0: begin
				widget_control, (*pstate).ic_base, map=0
				widget_control, (*pstate).scan_button, sensitive=0
				widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
				widget_control, (*pstate).ic_base2, scr_ysize=1
				end
			1: begin
				widget_control, (*pstate).ic_base, map=1
				widget_control, (*pstate).scan_button, sensitive=1
				widget_control, (*pstate).ic_base1, map=1, scr_ysize=(*pstate).ic_base1_ysize
				widget_control, (*pstate).ic_base2, scr_ysize=(*pstate).ic_base2_ysize
				end
			2: begin
				widget_control, (*pstate).ic_base, map=1
				widget_control, (*pstate).scan_button, sensitive=0
				widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
				widget_control, (*pstate).ic_base2, scr_ysize=(*pstate).ic_base2_ysize
				end
		endcase
		end
		
	'charge-scan': begin
		dpath = *(*pstate).dpath
		file = find_file2( (*pstate).evt_file)
		if lenchr(file[0]) ne 0 then dpath = extract_path(file[0])
		file = strip_path( file)
		F = file_requester( filter='*', /numeric, path=dpath, group=event.top, file=file, $
			title='Select representative Maia blog data', fix_filter=0 )
		if F[0] eq '' then goto, finish
		(*pstate).evt_file = F[0]
		*(*pstate).dpath = extract_path(F[0])
		notify, 'dpath', (*pstate).dpath, from=event.top
		
		flux_scan, obj,(*pstate).evt_file, PV_list, IC_name, IC_val, IC_vunit, dwell=dwell, $
					/image_mode, error=error, group=event.top, no_pv=no_pv, use_dwell=use_dwell
		if no_pv then begin
			(*pl).IC.mode = 2
			widget_control, (*pstate).charge_mode, set_combobox_select=2
		endif
		widget_control, (*pstate).ic_base, map=((*pl).IC.mode ne 0 )
		widget_control, (*pstate).scan_button, sensitive=((*pl).IC.mode eq 1 )
		if ((*pl).IC.mode eq 1) then begin
			widget_control, (*pstate).ic_base1, map=1, scr_ysize=(*pstate).ic_base1_ysize
		endif else begin
			widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
		endelse
		if (*pl).IC.mode ne 0 then begin
			widget_control, (*pstate).ic_base2, scr_ysize=(*pstate).ic_base2_ysize
		endif else begin
			widget_control, (*pstate).ic_base2, scr_ysize=1
		endelse
		if error eq 0 then begin
			if n_elements(PV_list) ne 0 then begin
				*(*pl).IC.plist = PV_list
				check_plist_maia, (*pl).IC.plist
				widget_control, (*pstate).ic_pv_mode, set_value='   '+*(*pl).IC.plist
			endif
			if (*pl).IC.remote eq 0 then begin
				(*pl).IC.pv.name = IC_name
				(*pl).IC.pv.val = IC_val
				(*pl).IC.pv.unit = IC_vunit
				l = locate('time', strlowcase((*pl).IC.PV.name))
				val = (*pl).IC.pv.val
				unit = (*pl).IC.pv.unit
				ival = find_charge_val_unit_index( val, unit, iunit=iunit, time=(l ge 0), /write_back)
				(*pl).IC.pv.val = val
				(*pl).IC.pv.unit = unit
			endif
			if (*pl).IC.pv.name eq 'Maia:scaler.FC0' then begin
				if (*pl).IC.remote eq 0 then (*pl).IC0.pv = (*pl).IC.pv
			endif else if (*pl).IC.pv.name eq 'Maia:scaler.FC1' then begin
				if (*pl).IC.remote eq 0 then (*pl).IC1.pv = (*pl).IC.pv
			endif else begin
				(*pl).ICE.pv = (*pl).IC.pv
			endelse
			q = where( (*pl).IC.pv.name eq *(*pl).IC.plist, nq)
			widget_control, (*pstate).ic_pv_mode, set_combobox_select=(nq ne 0) ? q[0] : 0
			if (*pl).IC.remote eq 0 then begin
				widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
				widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit
			endif
		endif else begin
			warning,'maia_setup','No Flux PV data found for this blog file.'
		endelse
		end

	'charge-conversion': begin
		widget_control, event.id, get_value=s
		(*pl).IC.conversion = float(s)
		end
		
	'ic-pv-mode': begin
		if ptr_good((*pl).IC.plist) then begin
			name = (*(*pl).IC.plist)[event.index]
			if name eq 'Maia:scaler.FC0' then begin
				(*pl).IC.remote = (*pl).IC0.remote
				if (*pl).IC.remote then begin
					(*pl).IC.pv = (*pl).IC0.pv
					(*pl).IC.pv.name = 'Maia:scaler.FC0'
				endif
			endif else if name eq 'Maia:scaler.FC1' then begin
				(*pl).IC.remote = (*pl).IC1.remote
				if (*pl).IC.remote then begin
					(*pl).IC.pv = (*pl).IC1.pv
					(*pl).IC.pv.name = 'Maia:scaler.FC1'
				endif
			endif else if name eq 'Maia:dwell.time' then begin
				(*pl).IC.remote = 0
				(*pl).IC.pv.name = 'Maia:dwell.time'
				(*pl).IC.pv.val = 1.0
				(*pl).IC.pv.unit = 1.0
			endif else begin
				(*pl).IC.pv = (*pl).ICE.pv
				(*pl).IC.remote = 0
			endelse
			if strmid(name,0,11) eq 'Maia:scaler' then begin
				l = locate('time', strlowcase((*pl).IC.PV.name))
				sens = (*pl).IC.pv.val * (*pl).IC.pv.unit
				ival = find_charge_val_unit_index( sens, iunit=iunit, time=(l ge 0)) 
				widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
				widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit
				widget_control, (*pstate).ic_val_mode, sensitive=1
				widget_control, (*pstate).ic_unit_mode, sensitive=1
			endif else begin
				widget_control, (*pstate).ic_val_mode, sensitive=0
				widget_control, (*pstate).ic_unit_mode, sensitive=0
			endelse
		endif
		end
	'ic-preamp-mode': begin
		if (*pl).IC.remote eq 0 then begin
			(*pl).IC.pv.val = (*pstate).ic_vals[event.index]
			if (*pl).IC.pv.unit eq 0. then (*pl).IC.pv.unit=1.
		endif
		l = locate('time', strlowcase((*pl).IC.PV.name))
		sens = (*pl).IC.pv.val * (*pl).IC.pv.unit
		ival = find_charge_val_unit_index( sens, iunit=iunit, time=(l ge 0)) 
		widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
		widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit
		end
	'ic-preamp-unit-mode': begin
		if (*pl).IC.remote eq 0 then begin
			(*pl).IC.pv.unit = (*pstate).ic_vunits[event.index]
			if (*pl).IC.pv.val eq 0. then (*pl).IC.pv.val=1.
		endif
		l = locate('time', strlowcase((*pl).IC.PV.name))
		sens = (*pl).IC.pv.val * (*pl).IC.pv.unit
		ival = find_charge_val_unit_index( sens, iunit=iunit, time=(l ge 0)) 
		widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
		widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit
		end		
		
	'hymod-apply-imaging': begin
		; only imaging parameters and their enable status
		; force the "enables" off if there are no DA and ROI tables found/entered.

		maia_setup_apply_imaging, pstate
		end
		
	'controls-apply': begin
		q = where((*pstate).select eq 1,nq)
		if nq eq 0 then begin
			warning,'maia_setup_event',['No detectors are selected.','Select detectors and try again.']
			goto, finish
		endif
;		widget_control, (*pstate).hermes_elk_mode, get_uvalue=u
;		if (*pstate).check_mode eq u.id then begin
;			c = (*play).data[q[0]].index mod 32
;			h = (*play).data[q[0]].hermes
;			(*pm).channel[q[0]].hermes.elk = (*pstate).hermes.elk
;			socket_command_set, ps, 'ELK', (*pm).channel[q[0]].hermes.elk, class='hermes', chip=h, channel=c
;		endif
		maia_setup_load_table, pstate
		end
		
	'controls-peltier-slider': begin
		peltier = event.value
		if (*pstate).peltier_mode eq 1 then begin
			sign = (*pm).version.dbpm ge 2 ? -1.0 : +1.0
			(*pl).control.peltier = sign * (peltier < (*pm).control.peltier_bake_max)
		endif else begin
			(*pl).control.peltier = peltier < (*pm).control.peltier_cool_max
		endelse
		widget_control, (*pstate).controls_peltier_slider, set_value = abs((*pl).control.peltier)
		socket_command_set, ps, 'peltier.current', (*pl).control.peltier, class='detector'
		end
		
	'stop-bake-button': begin
		(*pl).control.peltier = 0.0
		widget_control, (*pstate).controls_peltier_slider, set_value = abs((*pl).control.peltier)
		socket_command_set, ps, 'peltier.current', (*pl).control.peltier, class='detector'
		warning,'maia_setup',['The Peltier drive current will now be zeroed.', '', $
				'Both Cooling and Bake-out remain possible while ','the Bake-out Plug is present.', '', $
				'To prevent Bake-out, set to COOL mode and remove the Bake-out plug.'], /info
		maia_setup_check_warning, pstate
		end
		
	'peltier-mode': begin
		if (*pm).control.status.bake then begin
			if (event.index eq 1) and ((*pm).control.peltier gt 0.02) then begin							; change to Bake from Cool
				(*pl).control.peltier = 0.0
				widget_control, (*pstate).controls_peltier_slider, set_value = abs((*pl).control.peltier)
				socket_command_set, ps, 'peltier.current', (*pl).control.peltier, class='detector'
			endif else if (event.index eq 0) and ((*pm).control.peltier lt -0.02) then begin				; change to Cool from Bake
				(*pl).control.peltier = 0.0
				widget_control, (*pstate).controls_peltier_slider, set_value = abs((*pl).control.peltier)
				socket_command_set, ps, 'peltier.current', (*pl).control.peltier, class='detector'
			endif
			(*pstate).peltier_mode = event.index
		endif else begin
			if (event.index eq 1) then begin																; veto Bake
				widget_control, (*pstate).controls_peltier_mode, set_combobox_select = 0
				warning,'maia_setup_event',['Bake mode not permitted without Bake-out plug in place.','', $
					'To enable Bake-out mode, insert the "Bake-out Enable Plug" in the Limo socket in the Maia detector head.'],/info
			endif
			if ((*pm).control.peltier lt -0.02) then begin													; veto Bake current
				(*pl).control.peltier = 0.0
				widget_control, (*pstate).controls_peltier_slider, set_value = abs((*pl).control.peltier)
				socket_command_set, ps, 'peltier.current', (*pl).control.peltier, class='detector'
			endif
			(*pstate).peltier_mode = 0
		endelse
		end		
		
	'controls-bias-slider': begin
		(*pl).control.bias = (event.value > (*pm).control.bias_min) < (*pm).control.bias_max
		widget_control, (*pstate).controls_bias_slider, set_value = (*pl).control.bias
		socket_command_set, ps, 'bias.voltage', (*pl).control.bias, class='detector'
		end
		
	'controls-guard-slider': begin
		(*pl).control.guard = event.value < (*pm).control.guard_max
		widget_control, (*pstate).controls_guard_slider, set_value = (*pl).control.guard
		socket_command_set, ps, 'guard.voltage', (*pl).control.guard, class='detector'
		end
		
	'controls-link-loss-clear': begin
		socket_command_set, ps, 'linkloss_clr', 1, class='internal.ddm'
		end
		
	'controls-interlock-clear': begin
		socket_command_set, ps, 'ilock_clr', 0, class='internal.ddm'
		end
		
	'edit-id': begin
		if total((*pstate).select) gt 1 then begin
			warning,'maia_setup',['Only select a single detector pad to change the Number ID.','Try again.']
			goto, finish
		endif
		widget_control, (*pstate).edit_id_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then begin
				(*play).data[i].index = fix(s)
				widget_control, (*pstate).detector, set_value={select:i, label:s}
			endif
		endfor
		widget_control, (*pstate).edit_n_in_chip_text, set_value=str_tidy( fix(s) mod 32 )
		maia_setup_load_table, pstate
		end
		
	'edit-hermes': begin
		widget_control, (*pstate).edit_hermes_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].hermes = fix(s)
		endfor
		maia_setup_load_table, pstate
		end
		
	'edit-quadrant': begin
		widget_control, (*pstate).edit_quadrant_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].quadrant = fix(s)
		endfor
		maia_setup_load_table, pstate
		end
		
	'edit-radial': begin
		widget_control, (*pstate).edit_radial_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].radial = fix(s)
		endfor
		maia_setup_load_table, pstate
		end
		
	'edit-set-radial': begin
		widget_control, (*pstate).edit_pitch_text, get_value=s
		pitch = float(s)
		r = sqrt( ((*play).data.x)^2 + ((*play).data.y)^2)
		class = round(r/pitch[0] - 1)
		(*play).data.radial = (class > 0) < 15
		print, (*play).data.radial
		maia_setup_load_table, pstate
		end
		
	'edit-pitch': begin
		maia_setup_load_table, pstate
		end
		
	'edit-column': begin
		widget_control, (*pstate).edit_column_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].column = fix(s)
		endfor
		maia_setup_load_table, pstate
		end
		
	'edit-row': begin
		widget_control, (*pstate).edit_row_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].row = fix(s)
		endfor
		maia_setup_load_table, pstate
		end
		
	'edit-width': begin
		widget_control, (*pstate).edit_width_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].width = float(s)
		endfor
		maia_setup_load_table, pstate
		end
		
	'edit-height': begin
		widget_control, (*pstate).edit_height_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].height = float(s)
		endfor
		maia_setup_load_table, pstate
		end
		
	'edit-read': begin
		maia_setup_read_layout, pstate
		end
		
	'edit-save': begin
		write_detector_layout, (*play).file, data=play
		maia_setup_load_table, pstate
		end
		
	'edit-revert': begin
		d = read_detector_layout((*play).file, maia=maia, error=error)
		if error then begin
			warning,'Maia_Setup','Failed to read "'+(*play).file+'" to initialize layout.'
			goto, finish
		endif
		if maia eq 0 then begin
			warning,'Maia_Setup','"'+(*play).file+'" file does not contain extended columns.'
			goto, finish
		endif
		*play = d
		widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:0, label:str_tidy((*play).data.index)} 
		maia_setup_load_table, pstate
		end		
		
	'edit-number-sequence': begin
		if (*pstate).sequence.on then begin
			(*pstate).sequence.on = 0
			widget_control, (*pstate).edit_sequence_button, set_value='Start Number Sequence'
			maia_setup_load_table, pstate
		endif else begin
			(*pstate).sequence.on = 1
			(*pstate).sequence.count = 0
			widget_control, (*pstate).edit_sequence_button, set_value='STOP Number Sequence'
			widget_control, (*pstate).detector_mode, set_combobox_select=1 
			(*pstate).group_mode = 1
		endelse
		end
		
	'pulser-program-mode': begin
		(*pstate).pulser.mode = event.index
		widget_control, (*pstate).pulser_start_button, set_value=(*pstate).start_text[(*pstate).pulser.mode]
		maia_setup_check_warning, pstate
		maia_setup_pulser_map, pstate
		end
		
	'pulser-start': begin
		On = 1 - (*pstate).pulser.on
		abort = On

		case (*pstate).pulser.mode of
			0: begin								; manual pulser OFF
				socket_command_set, ps, 'ECAL', 0, class='hermes', chip=-1, channel=-1
				socket_command_set, ps, 'EBLK',0, class='hermes', chip=-1
				socket_command_set, ps, 'rate', 0, class='pulser'
				socket_command_set, ps, 'enable', 0, class='pulser'
				socket_command_set, ps, 'enable', 0, class='synth'
				end
			1: begin								; manual pulser ON
				if On then begin
					socket_command_set, ps, 'ECAL', 0, class='hermes', chip=-1, channel=-1
					socket_command_set, ps, 'enable', 0, class='pulser'
					q = where((*pstate).select eq 1,nq)
					if (nq eq 0) then begin
						if abort then warning,'maia_setup_event',['No detectors are selected.','Select detectors and try again.']
						goto, finish
					endif
					c = (*play).data[q].index mod 32
					h = (*play).data[q].hermes
					widget_control, (*pstate).pulser_low_text, get_value=s
					low = float(s)
					if (strlen(s) eq 0) then begin
						if abort then warning,'maia_setup_event',['No Amplitude set.','Select a valid amplitude and try again.']
						goto, finish
					endif
					widget_control, (*pstate).pulser_rate_text, get_value=s
					rate = float(s)
					if (strlen(s) eq 0) or (rate lt 1) then begin
						if abort then warning,'maia_setup_event',['No Rate set.','Select a valid rate and try again.']
						goto, finish
					endif
					socket_command_set, ps, 'ECAL', 1, class='hermes', chip=h, channel=c
					wait, 3.0
					socket_command_set, ps, 'voltage', low, class='pulser'
					socket_command_set, ps, 'rate', rate, class='pulser'
					socket_command_set, ps, 'enable', 1, class='pulser'
					socket_command_set, ps, 'enable', 1, class='photon'
				endif else begin
					socket_command_set, ps, 'ECAL', 0, class='hermes', chip=-1, channel=-1
					socket_command_set, ps, 'rate', 0, class='pulser'
					socket_command_set, ps, 'enable', 0, class='pulser'
				endelse
				end
			2: begin								; step-cal
				socket_command_set, ps, 'ECAL', 0, class='hermes', chip=-1, channel=-1
				socket_command_set, ps, 'rate', 0, class='pulser'
				socket_command_set, ps, 'enable', 0, class='pulser'
				socket_command_set, ps, 'endrun', 1, class='blog'
				if On then begin
					q = where((*pstate).select eq 1,nq)
					if (nq eq 0) then begin
						if abort then warning,'maia_setup_event',['No detectors are selected.','Select detectors and try again.']
						goto, finish
					endif
					widget_control, (*pstate).pulser_low_text, get_value=s
					low = float(s)
					widget_control, (*pstate).pulser_high_text, get_value=s2
					high = float(s2)
					if (strlen(s) eq 0) or (strlen(s2) eq 0) then begin
							if abort then warning,'maia_setup_event',['No Amplitudes set.','Select valid Low, High amplitudes and try again.']
							goto, finish
					endif
					widget_control, (*pstate).pulser_rate_text, get_value=s
					rate = float(s)
					widget_control, (*pstate).pulser_count_text, get_value=s2
					count = fix(s2)
					if (strlen(s) eq 0) or (strlen(s2) eq 0) or (count lt 2)  or (rate lt 1) then begin
							if abort then warning,'maia_setup_event',['No Rate or Count set.','Select valid Rate (c/s) and subdivision Count and try again.']
							goto, finish
					endif
					widget_control, (*pstate).pulser_time_text, get_value=s2
					time = float(s2)
					if (strlen(s2) eq 0) or (time lt 0.1) then begin
							if abort then warning,'maia_setup_event',['No Time set.','Select valid Time on each peak (s) and try again.']
							goto, finish
					endif
						
					; step-cal procedure ...
					; need to queue this for execution with Timer delays (set Timer on Execute button).
					; pick up next instruction with each Timer return.
					
					build_kandinsky_script_step_cal, ps, (*play).data[q].index, $
										time=time, rate=rate, low=low, high=high, count=count, ndet=(*pm).n_detectors

					(*ps).current = 0
					widget_control, (*pstate).pulser_start_button, timer=0.5
					widget_control, (*pstate).pulser_list, set_value=(*(*ps).ps)[(*ps).current:(*ps).last]
				endif else begin
					; abort running step-cal ...
					(*ps).current = 10000000L
					On = 0
					Abort = 0
				endelse
				end
			3: begin								; gain-trim
				socket_command_set, ps, 'ECAL', 0, class='hermes', chip=-1, channel=-1
				socket_command_set, ps, 'rate', 0, class='pulser'
				socket_command_set, ps, 'enable', 0, class='pulser'
				socket_command_set, ps, 'endrun', 1, class='blog'
				if On then begin
					widget_control, (*pstate).pulser_low_text, get_value=s
					low = float(s)
					widget_control, (*pstate).pulser_high_text, get_value=s2
					high = float(s2)
					if (strlen(s) eq 0) or (strlen(s2) eq 0) then begin
							if abort then warning,'maia_setup_event',['No Amplitudes set.','Select valid Low, High amplitudes and try again.']
							goto, finish
					endif
					widget_control, (*pstate).pulser_rate_text, get_value=s
					rate = float(s)
					if (strlen(s) eq 0) or (rate lt 1) then begin
							if abort then warning,'maia_setup_event',['No Rate set.','Select valid Rate (c/s) and try again.']
							goto, finish
					endif
					widget_control, (*pstate).pulser_time_text, get_value=s2
					time = float(s2)
					if (strlen(s2) eq 0) or (time lt 0.1) then begin
							if abort then warning,'maia_setup_event',['No Time set.','Select valid Time on each peak (s) and try again.']
							goto, finish
					endif
						
					; gain-trim procedure ...
					; need to queue this for execution with Timer delays (set Timer on Execute button).
					; pick up next instruction with each Timer return.
					
					build_kandinsky_script_gain_trim, ps, time=time, rate=rate, low=low, high=high, $
								n_chips=(*pm).n_detectors/32
								
					(*ps).current = 0
					widget_control, (*pstate).pulser_start_button, timer=0.5
					widget_control, (*pstate).pulser_list, set_value=*(*ps).ps
				endif else begin
					; abort running gain-trim ...
					(*ps).current = 10000000L
					On = 0
					Abort = 0
				endelse
				end
			4: begin								; synth pulser ON
				if On then begin
;					socket_command_set, ps, 'enable', 1, class='scepter'
					socket_command_set, ps, 'enable', 0, class='synth'
					q = where((*pstate).select eq 1,nq)
					if (nq eq 0) then begin
						if abort then warning,'maia_setup_event',['No detectors are selected.','Select detectors and try again.']
						goto, finish
					endif
					widget_control, (*pstate).pulser_low_text, get_value=s
					eAmp = long(s)
					if (strlen(s) eq 0) then begin
						if abort then warning,'maia_setup_event',['No E Amplitude set.','Select a valid amplitude and try again.']
						goto, finish
					endif
					widget_control, (*pstate).pulser_high_text, get_value=s
					tAmp = long(s)
					if (strlen(s) eq 0) then begin
						if abort then warning,'maia_setup_event',['No T Amplitude set.','Select a valid amplitude and try again.']
						goto, finish
					endif
					widget_control, (*pstate).pulser_rate_text, get_value=s
					rate = long(s)
					if (strlen(s) eq 0) or (rate lt 1) then begin
						if abort then warning,'maia_setup_event',['No Rate set.','Select a valid rate and try again.']
						goto, finish
					endif
					for i=0L,nq-1 do begin
						socket_command_set, ps, 'component', [eAMP,tAmp,(*play).data[q[i]].index], channel=-1, n_channels=3, class='synth.event', chip=i, n_chips=4096
					endfor
					socket_command_set, ps, 'event.count', nq, class='synth'
					socket_command_set, ps, 'rate', rate, class='synth'
					socket_command_set, ps, 'enable', 1, class='synth'
					socket_command_set, ps, 'enable', 1, class='event'
				endif else begin
					socket_command_set, ps, 'enable', 0, class='synth'
				endelse
				end
			5: begin								; sample ELK
				socket_command_set, ps, 'elk',0, class='hermes', channel=-1, chip=-1, error=error

				if On then begin
					q = where((*pstate).select eq 1,nq)
					if (nq eq 0) then begin
						if abort then warning,'maia_setup_event',['No detectors are selected.','Select detectors and try again.']
						goto, finish
					endif
					widget_control, (*pstate).pulser_time_text, get_value=s2
					time = float(s2)
					if (strlen(s2) eq 0) or (time lt 0.1) then begin
							if abort then warning,'maia_setup_event',['No Time set.','Select valid Time to sample each pad and try again.','Note that the ELK take at least 5-10 seconds to settle.']
							goto, finish
					endif
						
					; sample ELK procedure ...
					; need to queue this for execution with Timer delays (set Timer on Execute button).
					; pick up next instruction with each Timer return.
					; Return sampled OAK/OAN ADC values in 'pelk' pointer, indexed by detector.
					
					build_kandinsky_script_leakage_map, ps, (*play).data[q].index, time=time, n_chips=(*pm).n_detectors/32, map=(*pstate).pelk
								
					(*ps).current = 0
					widget_control, (*pstate).pulser_start_button, timer=0.5
					widget_control, (*pstate).pulser_list, set_value=*(*ps).ps
				endif else begin
					; abort running gain-trim ...
					(*ps).current = 10000000L
					On = 0
					Abort = 0
				endelse
				end
			6: begin								; sample EAN
				socket_command_set, ps, 'ean',0, class='hermes', channel=-1, chip=-1, error=error

				if On then begin
					widget_control, (*pstate).pulser_time_text, get_value=s2
					time = float(s2)
					if (strlen(s2) eq 0) or (time lt 0.1) then begin
							if abort then warning,'maia_setup_event',['No Time set.','Select valid Time to sample each pad and try again.']
							goto, finish
					endif
						
					; sample EAN procedure ...
					; need to queue this for execution with Timer delays (set Timer on Execute button).
					; pick up next instruction with each Timer return.
					; Return sampled OAN ADC values in 'pelk' pointer, indexed by detector.
					
					build_kandinsky_script_leakage_map, ps, time=time, n_chips=(*pm).n_detectors/32, map=(*pstate).pelk, /ean
								
					(*ps).current = 0
					widget_control, (*pstate).pulser_start_button, timer=0.5
					widget_control, (*pstate).pulser_list, set_value=*(*ps).ps
				endif else begin
					; abort running gain-trim ...
					(*ps).current = 10000000L
					On = 0
					Abort = 0
				endelse
				end
			7: begin								; step-cal ALL
				socket_command_set, ps, 'ECAL', 0, class='hermes', chip=-1, channel=-1
				socket_command_set, ps, 'rate', 0, class='pulser'
				socket_command_set, ps, 'enable', 0, class='pulser'
				socket_command_set, ps, 'endrun', 1, class='blog'
				if On then begin
					widget_control, (*pstate).pulser_low_text, get_value=s
					low = float(s)
					widget_control, (*pstate).pulser_high_text, get_value=s2
					high = float(s2)
					if (strlen(s) eq 0) or (strlen(s2) eq 0) then begin
							if abort then warning,'maia_setup_event',['No Amplitudes set.','Select valid Low, High amplitudes and try again.']
							goto, finish
					endif
					widget_control, (*pstate).pulser_rate_text, get_value=s
					rate = float(s)
					widget_control, (*pstate).pulser_count_text, get_value=s2
					count = fix(s2)
					if (strlen(s) eq 0) or (strlen(s2) eq 0) or (count lt 2)  or (rate lt 1) then begin
							if abort then warning,'maia_setup_event',['No Rate or Count set.','Select valid Rate (c/s) and subdivision Count and try again.']
							goto, finish
					endif
					widget_control, (*pstate).pulser_time_text, get_value=s2
					time = float(s2)
					if (strlen(s2) eq 0) or (time lt 0.1) then begin
							if abort then warning,'maia_setup_event',['No Time set.','Select valid Time on each peak (s) and try again.']
							goto, finish
					endif
						
					; step-cal procedure ...
					; need to queue this for execution with Timer delays (set Timer on Execute button).
					; pick up next instruction with each Timer return.
					
					build_kandinsky_script_step_cal, ps, time=time, rate=rate, $
										low=low, high=high, count=count, ndet=(*pm).n_detectors, /ALL

					(*ps).current = 0
					widget_control, (*pstate).pulser_start_button, timer=0.5
					widget_control, (*pstate).pulser_list, set_value=(*(*ps).ps)[(*ps).current:(*ps).last]
				endif else begin
					; abort running step-cal ...
					(*ps).current = 10000000L
					On = 0
					Abort = 0
				endelse
				end
		endcase
		(*pstate).pulser.on = On
		widget_control, (*pstate).pulser_program_mode, sensitive=1-On
		text = (*pstate).start_text[(*pstate).pulser.mode]
		if abort then text='Abort'
		widget_control, (*pstate).pulser_start_button, set_value=text
		end
		
	'stop-pulser-button': begin
		(*pstate).pulser.mode = 0
		(*pstate).pulser.on = 0
		widget_control, (*pstate).pulser_start_button, set_value=(*pstate).start_text[(*pstate).pulser.mode]
		widget_control, (*pstate).pulser_start_base, map=0
		widget_control, (*pstate).pulser_progress_base, map=0
		widget_control, (*pstate).pulser_count_base, map=0
		widget_control, (*pstate).pulser_program_mode, sensitive=1, set_combobox_select=0
		
;		socket_command_set, ps, 'rate', 0, class='pulser'
		socket_command_set, ps, 'enable', 0, class='pulser'
		socket_command_set, ps, 'enable', 0, class='synth'
;		if ((*pstate).pulser.mode eq 2) or ((*pstate).pulser.mode eq 3) or ((*pstate).pulser.mode eq 7) then begin
			socket_command_set, ps, 'endrun', 1, class='blog'
;		endif
		socket_command_set, ps, 'ECAL', 0, class='hermes', chip=-1, channel=-1	
		socket_command_set, ps, 'EBLK', 0, class='hermes', chip=-1	
		wait, 3.0
		maia_setup_check_warning, pstate
		end
		
	'save-pulser': begin
		file='Maia.pulser.csv'
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /write, filter = '*.pulser.csv', file=file[0], $
			path=path, group=event.top, $
			title='Select a Pulser CSV file', fix_filter=1)
		if F ne '' then begin
			F = strip_file_ext(F,/double) + '.pulser.csv'
			widget_control, (*pstate).pulser_low_text, get_value=s
			(*pstate).pulser.low = float(s)
			widget_control, (*pstate).pulser_high_text, get_value=s2
			(*pstate).pulser.high = float(s2)
			widget_control, (*pstate).pulser_rate_text, get_value=s
			(*pstate).pulser.rate = float(s)
			widget_control, (*pstate).pulser_time_text, get_value=s2
			(*pstate).pulser.time = float(s2)
			widget_control, (*pstate).pulser_count_text, get_value=s2
			(*pstate).pulser.count = fix(s2)
			
			write_maia_pulser, F[0], select=(*pstate).select, pulser=(*pstate).pulser, error=error
		endif
		end
		
	'load-pulser': begin
		file='Maia.pulser.csv'
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = '*.pulser.csv', file=file[0], $
			path=path, group=event.top, $
			title='Select a Pulser CSV file', fix_filter=1)
		if F ne '' then begin
			F = strip_file_ext(F,/double) + '.pulser.csv'
			
			sel = read_maia_pulser( F, pulser=pulser, error=err)
			if err eq 0 then begin
				(*pstate).select = sel
				(*pstate).pulser = pulser
				widget_control, (*pstate).pulser_low_text, set_value=str_tidy((*pstate).pulser.low)
				widget_control, (*pstate).pulser_high_text, set_value=str_tidy((*pstate).pulser.high)
				widget_control, (*pstate).pulser_rate_text, set_value=str_tidy((*pstate).pulser.rate)
				widget_control, (*pstate).pulser_time_text, set_value=str_tidy((*pstate).pulser.time)
				widget_control, (*pstate).pulser_count_text, set_value=str_tidy((*pstate).pulser.count)
				widget_control, (*pstate).pulser_program_mode, set_combobox_select=(*pstate).pulser.mode
				widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
				
				widget_control, (*pstate).pulser_start_button, set_value=(*pstate).start_text[(*pstate).pulser.mode]
				maia_setup_check_warning, pstate
				maia_setup_pulser_map, pstate
			endif
		endif
		end
		
	'enable-debug-ean': begin
		(*pstate).debug_ean_mode = event.index
		case (*pstate).debug_ean_mode of
			0: Begin
				socket_command_set, ps, 'EAN', 0, class='hermes', chip=-1, channel=-1
				socket_command_set, ps, 'ELK', 0, class='hermes', chip=-1, channel=-1
				end
			1: begin
				v = socket_command_get( ps, 'EAN', class='hermes', chip=-1, channel=-1, error=err)
				if err eq 0 then (*pstate).debug_select_ean = v
				(*pstate).select = (*pstate).debug_select_ean
				widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
				end
			2: begin
				v = socket_command_get( ps, 'ELK', class='hermes', chip=-1, channel=-1, error=err)
				if err eq 0 then (*pstate).debug_select_elk = v
				(*pstate).select = (*pstate).debug_select_elk
				widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
				end
			else:
		endcase
		maia_setup_check_warning, pstate
		end
		
	'enable-debug-aux': begin
		(*pstate).debug_aux_mode = event.index
		case (*pstate).debug_aux_mode of
			0: Begin
				socket_command_set, ps, 'LOCK', 0, class='scepter', chip=-1 
				socket_command_set, ps, 'AUX', 0, class='scepter', chip=-1 
				if (*pm).version.scepter ge 6 then begin
					socket_command_set, ps, 'MASK', 0, class='scepter', chip=-1, channel=-1
				endif
				end
			1: begin
				if (*pm).version.scepter ge 6 then begin
					v = socket_command_get( ps, 'MASK', class='scepter', chip=-1, channel=-1, error=err)
					if err eq 0 then (*pstate).debug_select_aux = 1-v
				endif else begin
					lock = socket_command_get( ps, 'LOCK', class='scepter', chip=-1, error=err1)
					bla = socket_command_get( ps, 'BLA', class='scepter', chip=-1, error=err2)
					if (err1 eq 0) and (err2 eq 0) then begin
						v = intarr((*pm).n_detectors)
						q = where( lock eq 1, nq)
						if nq gt 0 then v[q[0]*32 + bla[q[0]]] = 1
						(*pstate).debug_select_aux = v[(*play).data.index]
					endif
				endelse
				(*pstate).select = (*pstate).debug_select_aux
				widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
				end
			else:
		endcase
		maia_setup_check_warning, pstate
		end
		
	'enable-debug-monitor': begin
		(*pstate).debug_monitor_mode = event.index
		socket_command_set, ps, 'select', (*pstate).monitor_modes[event.index], class='dam.monitor'
		maia_setup_check_warning, pstate
		end
		
	'debug-ean-check': begin
		(*pstate).debug_ean_check = event.select
		if (*pstate).debug_ean_check then (*pstate).debug_aux_check = 0
		widget_control, (*pstate).enable_debug_ean_mode, sensitive=(*pstate).debug_ean_check
		widget_control, (*pstate).enable_debug_aux_mode, sensitive=(*pstate).debug_aux_check
		widget_control, (*pstate).debug_aux_check_id, set_value=(*pstate).debug_aux_check
		
		if (*pstate).debug_ean_check then begin
			case (*pstate).debug_ean_mode of
				1: begin
					(*pstate).select = (*pstate).debug_select_ean
					end
				2: begin
					(*pstate).select = (*pstate).debug_select_elk
					end
				else:
			endcase
		endif
		if (*pstate).debug_aux_check then (*pstate).select = (*pstate).debug_select_aux
		widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
		end
	
	'debug-aux-check': begin
		(*pstate).debug_aux_check = event.select
		if (*pstate).debug_aux_check then (*pstate).debug_ean_check = 0
		widget_control, (*pstate).enable_debug_ean_mode, sensitive=(*pstate).debug_ean_check
		widget_control, (*pstate).enable_debug_aux_mode, sensitive=(*pstate).debug_aux_check
		widget_control, (*pstate).debug_ean_check_id, set_value=(*pstate).debug_ean_check
		
		if (*pstate).debug_ean_check then begin
			case (*pstate).debug_ean_mode of
				1: begin
					(*pstate).select = (*pstate).debug_select_ean
					end
				2: begin
					(*pstate).select = (*pstate).debug_select_elk
					end
				else:
			endcase
		endif
		if (*pstate).debug_aux_check then (*pstate).select = (*pstate).debug_select_aux
		widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
		end
	
	'debug-apply': begin
		if (*pstate).debug_ean_check then begin
			case (*pstate).debug_ean_mode of
				1: begin
					(*pstate).debug_select_ean = (*pstate).select
					end
				2: begin
					(*pstate).debug_select_elk = (*pstate).select
					end
				else:
			endcase
		endif
		if (*pstate).debug_aux_check then (*pstate).debug_select_aux = (*pstate).select
		maia_setup_update_debug, pstate		
		end
		
	'stop-debug-button': begin
		(*pstate).debug_ean_mode = 0
		(*pstate).debug_aux_mode = 0
		widget_control, (*pstate).enable_debug_ean_mode, set_combobox_select=0
		widget_control, (*pstate).enable_debug_aux_mode, set_combobox_select=0
		socket_command_set, ps, 'EAN', 0, class='hermes', chip=-1, channel=-1
		socket_command_set, ps, 'LOCK', 0, class='scepter', chip=-1 
		socket_command_set, ps, 'AUX', 0, class='scepter', chip=-1 
		if (*pm).version.scepter ge 6 then begin
			socket_command_set, ps, 'MASK', 0, class='scepter', chip=-1, channel=-1
		endif
		maia_setup_check_warning, pstate
		end
		
	'detector': begin
		if (*pstate).select[event.index] eq 0 then begin
	
			; Set the edit ID parameter controls to those for this detector ...
			widget_control, (*pstate).edit_index_text, set_value=str_tidy( event.index)
			widget_control, (*pstate).edit_id_text, set_value=str_tidy( (*play).data[event.index].index )
			widget_control, (*pstate).edit_n_in_chip_text, set_value=str_tidy( (*play).data[event.index].index mod 32 )
			widget_control, (*pstate).edit_hermes_text, set_value=str_tidy( (*play).data[event.index].hermes )
			widget_control, (*pstate).edit_quadrant_text, set_value=str_tidy( (*play).data[event.index].quadrant )
			widget_control, (*pstate).edit_radial_text, set_value=str_tidy( (*play).data[event.index].radial )
			widget_control, (*pstate).edit_column_text, set_value=str_tidy( (*play).data[event.index].column )
			widget_control, (*pstate).edit_row_text, set_value=str_tidy( (*play).data[event.index].row )
			widget_control, (*pstate).edit_width_text, set_value=str_tidy( (*play).data[event.index].width )
			widget_control, (*pstate).edit_height_text, set_value=str_tidy( (*play).data[event.index].height )
	
			; Set all HERMES and SCEPTER parameter controls to those for this detector ...
			widget_control, (*pstate).hermes_time_mode, set_combobox_select=(*pm).channel[event.index].hermes.time
			widget_control, (*pstate).hermes_gain_mode, set_combobox_select=(*pm).channel[event.index].hermes.gain
			widget_control, (*pstate).hermes_eblk_mode, set_combobox_select=(*pm).channel[event.index].hermes.eblk
;			widget_control, (*pstate).hermes_elk_mode, set_combobox_select=(*pm).channel[event.index].hermes.elk
			widget_control, (*pstate).hermes_detector_text, set_value=str_tidy( (*play).data[event.index].index )
	
			widget_control, (*pstate).scepter_tdm_mode, set_combobox_select=(*pm).channel[event.index].scepter.tdm
			widget_control, (*pstate).scepter_tds_mode, set_combobox_select=(*pm).channel[event.index].scepter.tds
			widget_control, (*pstate).scepter_tos_mode, set_combobox_select=(*pm).channel[event.index].scepter.tos
			widget_control, (*pstate).scepter_trk_mode, set_combobox_select=(*pm).channel[event.index].scepter.trk
			widget_control, (*pstate).scepter_trke_mode, set_combobox_select=(*pm).channel[event.index].scepter.trke
			if (*pm).version.scepter ge 6 then begin
				widget_control, (*pstate).scepter_trim_slider, set_value=(*pm).channel[event.index].scepter.trim	
				widget_control, (*pstate).scepter_tcm_mode, set_combobox_select=(*pm).channel[event.index].scepter.tcm
				widget_control, (*pstate).scepter_filt_mode, set_combobox_select=(*pm).channel[event.index].scepter.filt
			endif
			if (*pm).version.scepter ge 7 then begin
				widget_control, (*pstate).scepter_thpd_slider, set_value=(*pm).channel[event.index].scepter.thpd
			endif
			widget_control, (*pstate).scepter_thresh_slider, set_value=(*pm).channel[event.index].scepter.thresh
			widget_control, (*pstate).scepter_rr_slider, set_value=(*pm).channel[event.index].scepter.clock
			widget_control, (*pstate).scepter_detector_text, set_value=str_tidy( (*play).data[event.index].index )
			
	;		widget_control, (*pstate).controls_detector_text, set_value=str_tidy( (*play).data[event.index].index )
	
			(*pstate).hermes.time = (*pm).channel[event.index].hermes.time
			(*pstate).hermes.gain = (*pm).channel[event.index].hermes.gain
			(*pstate).hermes.eblk = (*pm).channel[event.index].hermes.eblk
			(*pstate).hermes.elk = (*pm).channel[event.index].hermes.elk
	
			(*pstate).scepter.tdm = (*pm).channel[event.index].scepter.tdm
			(*pstate).scepter.tds = (*pm).channel[event.index].scepter.tds
			(*pstate).scepter.tos = (*pm).channel[event.index].scepter.tos
			(*pstate).scepter.trk = (*pm).channel[event.index].scepter.trk
			(*pstate).scepter.trke = (*pm).channel[event.index].scepter.trke
			(*pstate).scepter.tcm = (*pm).channel[event.index].scepter.tcm
			(*pstate).scepter.filt = (*pm).channel[event.index].scepter.filt
	
			(*pstate).scepter.trim = (*pm).channel[event.index].scepter.trim	
			(*pstate).scepter.thresh = (*pm).channel[event.index].scepter.thresh
			(*pstate).scepter.thpd = (*pm).channel[event.index].scepter.thpd
			(*pstate).scepter.clock = (*pm).channel[event.index].scepter.clock
			(*pstate).scepter.tweak.trim.on = 0
			(*pstate).scepter.tweak.thresh.on = 0
		endif
		
;		view = widget_info( (*pstate).summary_table, /table_view)
		q = where( (*pstate).sort.index eq event.index, nq)
		widget_control, (*pstate).summary_table, set_table_select=[0,q[0],30,q[0]]
;		widget_control, (*pstate).summary_table, set_table_view=view
		
		; Select pads by all members of group that event.index belongs to ...
		
		if (*pstate).colour_mode eq 1 then goto, finish			; but not in "Colour" mode
		enable_mode = ((*pstate).tab_names[(*pstate).tab] eq 'Enable')
		
		if (*pstate).sequence.on then begin
			if (*pstate).sequence.count eq 0 then begin
				(*pstate).sequence.count = 32 * (*play).data[event.index].hermes
			endif
			(*play).data[event.index].index = (*pstate).sequence.count
			widget_control, (*pstate).detector, set_value={select:event.index, label:str_tidy((*pstate).sequence.count), alt:enable_mode}
			(*pstate).sequence.count = (*pstate).sequence.count + 1
		endif

		case (*pstate).group_mode of
			0: begin			; one only
				sel = (*pstate).select
				q = where( sel eq 1, nq)
				on = 1 - sel[event.index]
				if nq gt 0 then begin
					widget_control, (*pstate).detector, set_value={select:q, value:0, alt:0}
				endif
				(*pstate).select=0 
				(*pstate).select[event.index]=on 
				widget_control, (*pstate).detector, set_value={select:event.index, value:on, alt:enable_mode}
				end
			1: begin			; Individual
				sel = 1 - (*pstate).select[event.index]
				(*pstate).select[event.index]=sel 
				widget_control, (*pstate).detector, set_value={select:event.index, value:sel, alt:enable_mode}
				end
			2: begin			; radial
				sel = 1 - (*pstate).select[event.index]
				q = where( (*play).data.radial eq (*play).data[event.index].radial, nq)
				if nq ge 1 then begin
					for j=0L,nq-1 do begin
						(*pstate).select[q[j]]=sel 
						widget_control, (*pstate).detector, set_value={select:q[j], value:sel, alt:enable_mode}
					endfor
				endif
				end
			3: begin			; column
				sel = 1 - (*pstate).select[event.index]
				q = where( (*play).data.column eq (*play).data[event.index].column, nq)
				if nq ge 1 then begin
					for j=0L,nq-1 do begin
						(*pstate).select[q[j]]=sel 
						widget_control, (*pstate).detector, set_value={select:q[j], value:sel, alt:enable_mode}
					endfor
				endif
				end
			4: begin			; row
				sel = 1 - (*pstate).select[event.index]
				q = where( (*play).data.row eq (*play).data[event.index].row, nq)
				if nq ge 1 then begin
					for j=0L,nq-1 do begin
						(*pstate).select[q[j]]=sel 
						widget_control, (*pstate).detector, set_value={select:q[j], value:sel, alt:enable_mode}
					endfor
				endif
				end
			5: begin			; chip
				sel = 1 - (*pstate).select[event.index]
				q = where( (*play).data.hermes eq (*play).data[event.index].hermes, nq)
				if nq ge 1 then begin
					for j=0L,nq-1 do begin
						(*pstate).select[q[j]]=sel 
						widget_control, (*pstate).detector, set_value={select:q[j], value:sel, alt:enable_mode}
					endfor
				endif
				end
			6: begin			; quadrant
				sel = 1 - (*pstate).select[event.index]
				q = where( (*play).data.quadrant eq (*play).data[event.index].quadrant, nq)
				if nq ge 1 then begin
					for j=0L,nq-1 do begin
						(*pstate).select[q[j]]=sel 
						widget_control, (*pstate).detector, set_value={select:q[j], value:sel, alt:enable_mode}
					endfor
				endif
				end
			7: begin			; All
				sel = 1 - (*pstate).select[event.index]
				for i=0L,n_detectors-1 do begin
					(*pstate).select[i]=sel 
					widget_control, (*pstate).detector, set_value={select:i, value:sel, alt:enable_mode}
				endfor
				end
		endcase
		end
		
	'save-maia': begin
		file = (*pm).file
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /write, filter = '*.parameters.csv', file=file, $
			path=path, group=event.top, $
			title='Select the Maia Parameters CSV file', fix_filter=1)
		if F ne '' then begin
			F = strip_file_ext(F,/double) + '.parameters.csv'
			*(*pstate).path = extract_path(F)
			(*pm).cal.mode = (*pstate).cal_mode
			write_maia_parameters, F, data=pm, index=(*play).data.index, error=error
		endif	
		end
		
	'load-maia': begin
		file = (*pm).file
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = '*.parameters.csv', file=file, $
			path=path, group=event.top, title='Select the Maia Parameters CSV file', fix_filter=1)
		if F ne '' then begin
			F = strip_file_ext(F,/double) + '.parameters.csv'
			path = extract_path(F[0])		; default path to same as the parameters file
			*(*pstate).path = path

;			
			copy_pointer_data, pm, pl							; make local copy of *pm

			err = read_maia_parameters( F, data=pl, path=path, group=event.top, index=dindex)
			if err eq 0 then begin
				(*play).data.index = dindex						; detector number
				(*pstate).cal_mode = (*pl).cal.mode
				widget_control, (*pstate).hymod_cal_mode, set_combobox_select=(*pstate).cal_mode
				maia_setup_load_table, pstate
				
				d = 32 * indgen( (*ps).n_detectors/32 )			; first detector on each chip
				q = (*play).ref[d]								; table index
				c = d mod 32									; channel number on chip
				h = (*play).data[q].hermes						; hermes chip number
				
				socket_command_set, ps, 'TIME', (*pl).channel[q].hermes.time, class='hermes', chip=h 
				socket_command_set, ps, 'GAIN', (*pl).channel[q].hermes.gain, class='hermes', chip=h
				socket_command_set, ps, 'EBLK', (*pl).channel[q].hermes.eblk, class='hermes', chip=h
;				socket_command_set, ps, 'ELK', (*pl).channel[q].hermes.elk, class='hermes', chip=h
				socket_command_set, ps, 'TDM', (*pl).channel[q].scepter.tdm, class='scepter', chip=h
				socket_command_set, ps, 'TDS', (*pl).channel[q].scepter.tds, class='scepter', chip=h
				socket_command_set, ps, 'TOS', (*pl).channel[q].scepter.tos, class='scepter', chip=h
				socket_command_set, ps, 'thresh', (*pl).channel[q].scepter.thresh, class='scepter', chip=h
				socket_command_set, ps, 'TRK', (*pl).channel[q].scepter.trk, class='scepter', chip=h
				socket_command_set, ps, 'TRKE', (*pl).channel[q].scepter.trke, class='scepter', chip=h
				socket_command_set, ps, 'clock.rate', (*pl).channel[0].scepter.clock * 1.0e+6, class='readout'

		; 		Note that thpd and trim are stored as positive, but set as negative values ...
		
				if (*pl).version.scepter ge 7 then begin
					socket_command_set, ps, 'thpd', -(*pl).channel[q].scepter.thpd, class='scepter', chip=h
				endif
				if (*pl).version.scepter ge 6 then begin
					socket_command_set, ps, 'TCM', (*pl).channel[q].scepter.tcm, class='scepter', chip=h
					socket_command_set, ps, 'FILT', (*pl).channel[q].scepter.filt, class='scepter', chip=h
					
					d = indgen( (*ps).n_detectors)					; all detectors
					q = (*play).ref[d]								; table index
					c = d mod 32									; channel number on chip
					h = (*play).data[q].hermes						; hermes chip number

					socket_command_set, ps, 'trim', -(*pl).channel[q].scepter.trim, class='scepter', chip=h, channel=c
				endif
				maia_launch_hardware_reset, ps, pm					; ??
				
				dt = (*pl).deadtime.cal
				obj->set_options, deadtime_cal=dt
				socket_command_set, ps, 'coeff', [dt.b,dt.a], class='deadtime.time', channel=-1, n_channels=2

				if (*pl).version.software ge 4737 then begin
					socket_command_set, ps, 'value', '""', class='scratch.datum', chip=scratch_linearise
					socket_command_set, ps, 'value', '""', class='scratch.datum', chip=scratch_gaintrim
					socket_command_set, ps, 'value', '""', class='scratch.datum', chip=scratch_pileup
					socket_command_set, ps, 'value', '""', class='scratch.datum', chip=scratch_cal
					socket_command_set, ps, 'value', '""', class='scratch.datum', chip=scratch_throttle
				endif
				
				; Copy back, mostly to set enable flags in *pl for Hymod panel,
				; and for execution in "maia_setup_apply_hymod" below.
				; Note that the scratch.datum values for files, etc. will be set in "maia_setup_apply_hymod". 

				(*pm).file = (*pl).file
				(*pm).channel = (*pl).channel						; others done in 'maia_setup_apply_hymod'

				maia_setup_apply_hymod, pstate, throttle=0
				maia_setup_update_dynamic, pstate
				
				for i=1L,n_elements((*pstate).check_ids)-1 do begin
					if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
				endfor
				widget_control, (*pstate).check_ids[0], set_value=1
				(*pstate).check_mode = 0
				(*pstate).group_mask = [0,0,0,0,0,1,1,1]
				(*pstate).old_mask = (*pstate).group_mask
				maia_setup_check_group_mode, pstate
	
				; set the colours to bytscl() the hermes time values ...
				if (*pstate).colour_mode eq 1 then maia_setup_colour, pstate, (*pl).channel.hermes.time
			endif
		endif
		
		file = strip_file_ext((*pl).file,/double) + '.enable.var'
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = ['*.enable.var','*.enable.csv'], file=file, $
			path=path, group=event.top, $
			title='Select the Enable CSV file', fix_filter=1)
		if F ne '' then begin
			disable = read_maia_enable( F, index=(*play).data.index, error=err)
			if err eq 0 then begin
				(*(*pstate).pdisable) = disable
				if ((*pstate).tab_names[(*pstate).tab] eq 'Enable') then begin
					(*pstate).select = disable
					widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*(*pstate).pdisable), alt:1}
				endif
				socket_command_set, ps, 'ECH', 0, class='hermes', chip=-1, channel=-1 
				q = where( (*(*pstate).pdisable) eq 1, nq)
				if nq gt 0 then begin
					c = (*play).data[q].index mod 32
					h = (*play).data[q].hermes
					socket_command_set, ps, 'ECH', 1, class='hermes', chip=h, channel=c 
				endif
			endif
		endif
		
		file = strip_file_ext((*pl).file,/double) + '.groups.csv'
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'maia'
		F = file_requester( /read, filter = '*.groups.csv', file=file, $
			path=path, group=event.top, $
			title='Select the Groups CSV file', fix_filter=1)
		if F ne '' then begin
			F = strip_file_ext(F,/double) + '.groups.csv'
			set_widget_text, (*pstate).spectra_file_text, F
			groups = read_maia_groups( F, error=err)
			if err eq 0 then begin
				(*pl).groups.file = F
				(*pl).groups.group = groups
				(*pl).groups.on = 1
				set_widget_text, (*pstate).spectra_file_text, F
				*(*pstate).path = extract_path(F) 
				maia_setup_load_group_table, pstate
				maia_setup_apply_groups, pstate
			endif
		endif
		end
		
	'select-mode': begin
		(*pstate).group_mode = event.index
		maia_setup_check_group_mode, pstate
		end
		
	'mimic-toggle': begin
		(*pstate).colour_mode = event.value
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
;			maia_setup_colour, pstate, /redisplay
;			widget_control, (*pstate).check_ids[(*pstate).last_check], set_value=1
			widget_control, (*pstate).detector, set_value={mode:0, select:indgen(n_detectors), value:intarr(n_detectors)}
		endif else begin
;			(*pstate).last_check = (*pstate).check_mode
			widget_control, (*pstate).detector, set_value={mode:0, select:indgen(n_detectors), value:(*pstate).select}
		endelse
		widget_control, (*pstate).hermes_apply_button, sensitive=1-(*pstate).colour_mode
		widget_control, (*pstate).scepter_apply_button, sensitive=1-(*pstate).colour_mode
		widget_control, (*pstate).detector_mode, sensitive=1-(*pstate).colour_mode
		widget_control, (*pstate).colour_display_text, sensitive=(*pstate).colour_mode
		end
		
	'clear-select': begin
		(*pstate).select = 0 
		widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:0}
		end

	'save-select': begin
		F = file_requester( /write, filter = ['*.select.csv'], path=*(*pstate).path, group=event.top, $
					title='Save selected detectors in a .select.csv file', /fix_filter)
		if F[0] ne '' then begin
			F = strip_file_ext(F[0], /double)+'.select.csv'
			on_ioerror, finish
			index = (*play).data.index
			openw, lun, F[0], /get_lun
			on_ioerror, bad_save
			q = where( (*pstate).select ne 0, nq)
			if nq eq 0 then begin
				warning,'maia_setup','No detectors currently selected.'
				goto, bad_save
			endif
			n = index[q]
			printf, lun, '# Selected detector channels.'
			printf, lun, '# Saved by "maia_setup".'	
			for i=0,nq-1 do begin
				printf, lun, n[i]
			endfor
bad_save:
			close_file, lun
		endif
		end
		
	'get-select': begin
		(*pstate).select = 0 
		widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:0}
		
		F = file_requester( /read, filter = ['*.select.csv','*.spec'], path=*(*pstate).path, group=event.top, $
					title='Set selected detectors from a .SPEC or .select.csv file', fix_filter=0)
		if F[0] ne '' then begin
			*(*pstate).path = extract_path(F[0])
			ext = extract_extension(F[0])
			case strupcase(ext) of
			'SPEC': begin
				pp = read_spec(F)
				if ptr_valid(pp) eq 0 then goto, finish
				npp = n_elements(pp)
				if (*pp[0]).array then begin
					if ptr_good( (*pp[0]).pactive) then begin
						n = *(*pp[0]).pactive
					endif else begin
						for j=0L,npp-1 do begin
							free_spectrum, pp[j]
						endfor
						goto, finish
					endelse
				endif else begin
					n = intarr(npp)
					for j=0L,npp-1 do begin
						if ptr_good(pp[j]) then n[j] = (*pp[j]).station + adc_offset_device(obj)
					endfor
				endelse
	
				for j=0L,npp-1 do begin
					free_spectrum, pp[j]
				endfor
				end
			'CSV': begin
				n = get_select( F[0], error=err)
				if err or (n[0] eq -1) then goto, finish
				end
			endcase
	
			csv_index = (*play).ref
			for i=0L,n_elements(n)-1 do begin
				j = csv_index[n[i]]
				(*pstate).select[j] = 1
				widget_control, (*pstate).detector, set_value={select:j, value:1, alt:0}
			endfor
		endif
		end
				
	else:
endcase

finish:
	widget_control, hourglass=0
	return

done:
	goto, kill

bad_state:
	warning,'maia_setup_event',['STATE variable has become ill-defined.','Abort Maia setup.'],/error
	goto, kill

kill:
	if ((*pstate).debug_ean_mode ne 0) or ((*pstate).debug_aux_mode ne 0) then begin
		warning,'maia_setup',['Debug mode still active.','Click "OK" to stop Debug mode before exit.'], cancel=cancel
		if cancel eq 0 then begin
			socket_command_set, ps, 'EAN', 0, class='hermes', chip=-1, channel=-1
			socket_command_set, ps, 'LOCK', 0, class='scepter', chip=-1 
			socket_command_set, ps, 'AUX', 0, class='scepter', chip=-1
			if (*pm).version.scepter ge 6 then begin
				socket_command_set, ps, 'MASK', 0, class='scepter', chip=-1, channel=-1
			endif
			(*pstate).debug_ean_mode = 0
			(*pstate).debug_aux_mode = 0
		endif 
	endif
;	q = where((*pm).channel.hermes.eblk ne 0, nq)
;	if nq ge 1 then begin
;		warning,'maia_setup',['EBLK mode still active for some channels.','Click "OK" to stop EBLK mode before exit.'], cancel=cancel
;		if cancel eq 0 then begin
;			socket_command_set, ps, 'EBLK', 0, class='hermes', chip=-1
;			(*pm).channel.hermes.eblk = 0
;		endif
;	endif
	if (*pstate).pulser.mode ne 0 then begin
		warning,'maia_setup',['Pulser is still active.','Click "OK" to stop Pulser mode before exit.'], cancel=cancel
		if cancel eq 0 then begin
			socket_command_set, ps, 'ECAL', 0, class='hermes', chip=-1, channel=-1	
			socket_command_set, ps, 'rate', 0, class='pulser'
			socket_command_set, ps, 'enable', 0, class='synth'
;			socket_command_set, ps, 'enable', 1, class='scepter'
			(*pstate).pulser.mode = 0
		endif
	endif
	
	if (*pm).control.peltier lt -0.02 then begin
		warning,'maia_setup',['Bake is still active.','Click "OK" to zero Bake current before exit.','', $
								'The Peltier drive current will be zeroed.'], cancel=cancel
		if cancel eq 0 then begin
			(*pl).control.peltier = 0.0
			widget_control, (*pstate).controls_peltier_slider, set_value = abs((*pl).control.peltier)
			socket_command_set, ps, 'peltier.current', (*pl).control.peltier, class='detector'
		endif
	endif
	maia_setup_check_warning, pstate
		
die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end
		
;--------------------------------------------------------------------------

pro maia_setup_apply_hymod, pstate, throttle=do_throttle

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
		warning,'maia_setup_apply_hymod',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(do_throttle) lt 1 then do_throttle=0
	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
	play = (*pstate).playout
	ps = (*pstate).psocket
	pm = (*pstate).pmaia
	pl = (*pstate).plocal			; local copy of pm made on tab change, and by check and file
									; widgets. These sent to Maia, and read back as pm elsewhere.
@maia_scratch.def
	
	n_detectors = (*pm).n_detectors
	status = {on:0, mask:bytarr(n_detectors), text:''}
	version = (*pm).version.software
	obj = (*pm).DevObj
	status.text='Detector selection (mimic panel on the right) shows the detector channels successfully setting all selected processing tables. '+ $
					'To use these settings to set HERMES channel disables (ECH), click on the "Set Disables" button.'
;goto, test_cal
		if (*pl).linear.on then begin
			found = 0
			linearize2 = 0
			if lenchr((*pl).linear.file) gt 0 then begin
				if extract_extension( (*pl).linear.file) ne 'var' then begin
					f = get_linearize( (*pl).linear.file, do_linear=do_linear, details=details, /new)
					if do_linear then begin
						found = 1
						n = (details.n_int < 4096)
						f2 = details.f
						if details.n_frac ne 32 then f2 = congrid(f2,32,n)
						for j=0L,31 do begin
							socket_command_set, ps, 'EFRAC', (f2[j,0:n-1]>0)<4095, class='linearise.energy', chip=-1, n_chips=n, channel=j, /quiet
						endfor
						if (*pm).version.software ge 4737 then begin
							info = { on:(*pl).linear.on, file:(*pl).linear.file }
							s = stringify( info)
;							socket_command_set, ps, 'info', s, class='linearise'
							socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_linearise
						endif
						socket_command_set, ps, 'ENABLE', (*pl).linear.on, class='linearise'
					endif else begin
						(*pl).linear.on = 0
						warning,'maia_setup_apply_hymod',['Failed to open Linearization file:',(*pl).linear.file]
					endelse
					socket_command_set, ps, /report
				endif else begin
					line = ''
					on_ioerror, bad_linear
					openr, lun, (*pl).linear.file, /get_lun
					while NOT EOF(lun) do begin
						readf, lun, line
						i = locate('#', line)
						if (i eq -1) and (lenchr(line) gt 0) then begin
							i = locate('linearise2', line)
							if (i eq 0) and (lenchr(line) gt 0) then begin
								linearize2 = 1
								if ((*pm).version.software lt 9542) then begin
									warning,'maia_setup_apply_hymod',['"linearise2" commands found in Linearization file:',(*pl).linear.file, $
												'','This version of Kandinski does not support the new "linearise2".']
								endif
							endif
							i = locate('linearise', line)
							if (i eq 0) and (lenchr(line) gt 0) then found = 1
							socket_command_set_item, ps, line, error=error, /quiet, /prefix
						endif
					endwhile
					on_ioerror, null
					close_file, lun
				endelse
			endif
			(*pl).linear.on = found
			if (*pm).version.software ge 4737 then begin
				info = { on:(*pl).linear.on, file:(*pl).linear.file }
				s = stringify( info)
				socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_linearise
			endif
			if linearize2 then begin
				socket_command_set, ps, 'ENABLE', 0, class='linearise'
				if ((*pm).version.software ge 9542) then begin
					socket_command_set, ps, 'ENABLE', (*pl).linear.on, class='linearise2'
				endif
			endif else begin
				if ((*pm).version.software ge 9542) then begin
					socket_command_set, ps, 'ENABLE', 0, class='linearise2'
				endif
				socket_command_set, ps, 'ENABLE', (*pl).linear.on, class='linearise'
			endelse
			goto, cont_linear
bad_linear:
			(*pl).linear.on = 0
			warning,'maia_setup_apply_hymod',['Failed to open Linearization file:',(*pl).linear.file]
cont_linear:
			socket_command_set, ps, /report
		endif
		set_widget_text, (*pstate).hymod_linearization_text, (*pl).linear.file
		widget_control, (*pstate).hymod_linearization_check, set_value=(*pl).linear.on
		(*pm).linear = (*pl).linear
		
		if (*pl).trim.on then begin
			gfile = (*pl).trim.file
			first = 1
start_gaintrim:
			if extract_extension( gfile) eq 'spec' then goto, old_gaintrim
			if lenchr(gfile) gt 0 then begin
				line = ''
				on_ioerror, bad_gaintrim
				openr, lun, gfile, /get_lun
				while NOT EOF(lun) do begin
					readf, lun, line
					i = locate('#', line)
					if (i eq -1) and (lenchr(line) gt 0) then begin
						found = 1
						socket_command_set_item, ps, line, error=error, /quiet, /prefix
					endif
				endwhile
				on_ioerror, null
			endif
			close_file, lun
			goto, cont_gaintrim

bad_gaintrim:
			warning,'maia_setup_apply_hymod',['error reading gaintrim file:',gfile]
			close_file, lun
			(*pl).trim.on = 0
			goto, cont_gaintrim
			
old_gaintrim:
			p = read_spec( gfile)
			if ptr_valid(p[0]) then begin
				if first then begin
					socket_command_set, ps, 'Ecoeff', 0., class='gaintrim.det', chip=-1, n_chips=n_detectors, channel=0
					socket_command_set, ps, 'Ecoeff', 1., class='gaintrim.det', chip=-1, n_chips=n_detectors, channel=1
					socket_command_set, ps, 'Tcoeff', 0., class='gaintrim.det', chip=-1, n_chips=n_detectors, channel=0
					socket_command_set, ps, 'Tcoeff', 1., class='gaintrim.det', chip=-1, n_chips=n_detectors, channel=1
				endif
				found = 0
				for i=0L,n_elements(p)-1 do begin
					n = strlen((*p[i]).label)
					lab = strmid( (*p[i]).label, n-2,2)
					if strupcase(lab) eq '/E' then begin
						j = fix((*p[i]).station + adc_offset_device(obj))
						found = 1
						k = (*play).ref[j]
						(*pl).channel[k].trim.E.b = (*p[i]).cal.poly[0] 
						(*pl).channel[k].trim.E.a = (*p[i]).cal.poly[1] 
						status.mask[k]=1 & status.on=1
						ba = [(*p[i]).cal.poly[0],(*p[i]).cal.poly[1]]
						socket_command_set, ps, 'Ecoeff', ba, class='gaintrim.det', chip=j, channel=-1, n_channels=2, /quiet
					endif else if strupcase(lab) eq '/T' then begin
						j = fix((*p[i]).station + adc_offset_device(obj))
						found = 1
						k = (*play).ref[j]
						(*pl).channel[k].trim.T.b = (*p[i]).cal.poly[0] 
						(*pl).channel[k].trim.T.a = (*p[i]).cal.poly[1] 
						status.mask[k]=1 & status.on=1
						ba = [(*p[i]).cal.poly[0],(*p[i]).cal.poly[1]]
						socket_command_set, ps, 'Tcoeff', ba, class='gaintrim.det', chip=j, channel=-1, n_channels=2, /quiet
					endif
				endfor
			endif else begin
				warning,'maia_setup_apply_hymod',['error reading gaintrim file:',gfile]
				(*pl).trim.on = 0
			endelse

			; "trim.on" and "found" together indicates that trim was enabled on the panel
			; and was successfully transferred from a file to HYMOD.
			; "trim.on = 0" indicates that either no gain trim was selected on the HYMOD
			; tab for transfer, or that the transfer failed (bad file or write to HYMOD), in
			; which case the HYMOD continues with the existing settings (gain table and enable).
			
cont_gaintrim:				
			if (*pl).trim.on then begin
				socket_command_set, ps, /report
				if found eq 0 then begin
					warning,'maia-setup',['No valid detector gain trim cals found.','Make sure this is a file of fits to','separate detector spectra, obtained','from Blog data playback.']
				endif else begin
					if (*pm).version.software ge 4737 then begin
						info = { on:(*pl).trim.on, file:(*pl).trim.file, file2:(*pl).trim.file2 }
						s = stringify( info)
;						socket_command_set, ps, 'info', s, class='gaintrim'
						socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_gaintrim
					endif
					socket_command_set, ps, 'ENABLE', (*pl).trim.on, class='gaintrim'
				endelse
			endif
			if first then begin
				gfile = (*pl).trim.file2
				first = 0
				goto, start_gaintrim
			endif
		endif

after_gaintrim:
		set_widget_text, (*pstate).hymod_gain_trim_text, (*pl).trim.file
		set_widget_text, (*pstate).hymod_gain_trim_text2, (*pl).trim.file2
		widget_control, (*pstate).hymod_gain_trim_check, set_value=(*pl).trim.on
		(*pm).trim = (*pl).trim
		(*pm).channel.trim = (*pl).channel.trim
		
		if (*pl).pileup.on then begin
			f = get_pileup( (*pl).pileup.file, do_pileup=do_pileup)
			if do_pileup then begin
				n = (n_elements(f[0,*])<4096)
				socket_command_set, ps, 'TRANGE', f[0,*], class='pileup.energy', chip=-1, n_chips=n, channel=0
				socket_command_set, ps, 'TRANGE', f[1,*], class='pileup.energy', chip=-1, n_chips=n, channel=1
				if (*pm).version.software ge 4737 then begin
					info = { on:(*pl).pileup.on, file:(*pl).pileup.file }
					s = stringify( info)
;					socket_command_set, ps, 'info', s, class='pileup'
					socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_pileup
					s2 = ((*pl).pileup.on) ? (*pl).pileup.file : ''
					socket_command_set, ps, 'info', s2, class='pileup'
				endif
				socket_command_set, ps, 'ENABLE', (*pl).pileup.on, class='pileup'
			endif else begin
				(*pl).pileup.on = 0
				if (*pl).pileup.file ne '' then warning, 'maia_setup_apply_hymod', 'Failed to read Pileup file.'
			endelse
		endif
		widget_control, (*pstate).hymod_pileup_check, set_value=(*pl).pileup.on
		set_widget_text, (*pstate).hymod_pileup_text, (*pl).pileup.file
		(*pm).pileup.on = (*pl).pileup.on
		(*pm).pileup.file = (*pl).pileup.file
		
;test_cal:
		p = read_spec( (*pl).cal.file)
		if ptr_valid(p[0]) then begin
			socket_command_set, ps, 'ecoeff', -0.8, class='cal.det', chip=-1, n_chips=n_detectors, channel=0
			socket_command_set, ps, 'ecoeff', 0.009, class='cal.det', chip=-1, n_chips=n_detectors, channel=1
			found = 0
			combine_status = status.on eq 1
			if combine_status then begin								; to only enable channels that
				status1 = status.mask									; were not disabled, and that
				status.mask[*] = 0										; are present in Cal file
				status.on = 0
			endif
			case (*pstate).cal_mode of
				0: begin												; individual cals
					for i=0L,n_elements(p)-1 do begin
						n = strlen((*p[i]).label)
						lab = strmid( (*p[i]).label, n-2,2)
						lab2 = strmid( strtrim((*p[i]).label,2), 0,8)
						if (strupcase(lab) eq '/E') or (strupcase(lab2) eq 'DETECTOR') then begin
							j = fix((*p[i]).station + adc_offset_device(obj))
							if (j ge 0) and (j le n_detectors-1) then begin
								found = 1
								k = (*play).ref[j]
								(*pl).channel[k].cal.b = (*p[i]).cal.poly[0]
								(*pl).channel[k].cal.a = (*p[i]).cal.poly[1]
								status.mask[k] = 1
								status.on = 1
								ba = [(*p[i]).cal.poly[0],(*p[i]).cal.poly[1]<0.1]
								socket_command_set, ps, 'ecoeff', ba, class='cal.det', chip=j, channel=-1, n_channels=2, /quiet
							endif
						endif
					endfor
					socket_command_set, ps, /report
					if found eq 0 then warning,'maia-setup',['No valid detector calibrations found.','Make sure this is a file of fits to','separate detector spectra, obtained','from Blog data playback.']
					end
				1: begin												; one cal for all detectors
					a = (*p[0]).cal.poly[1]
					b = (*p[0]).cal.poly[0]
					if (abs(b) lt 0.01) and (abs(a-1.) lt 0.01) then warning,'maia_setup_apply_hymod','First spectrum in file is NOT calibrated.'
					socket_command_set, ps, 'ecoeff', b, class='cal.det', chip=-1, n_chips=n_detectors, channel=0
					socket_command_set, ps, 'ecoeff', a, class='cal.det', chip=-1, n_chips=n_detectors, channel=1
					(*pl).channel.cal.b = b
					(*pl).channel.cal.a = a
					end
			endcase
			(*pm).channel.cal = (*pl).channel.cal
			(*pm).cal = (*pl).cal
			if (*pm).version.software ge 4737 then begin
;				socket_command_set, ps, 'info', '"'+(*pl).cal.file+'"', class='cal'
				socket_command_set, ps, 'value', '"'+(*pl).cal.file+'"', class='scratch.datum', chip=scratch_cal
			endif
			maia_launch_update_cal, (*pstate).ppspec, (*pstate).ppgroup, (*pstate).pimage, pm, play

			notify, 'spectrum-cal', from=(*pstate).tlb
			if combine_status then begin
				q = where(status1 eq 0, nq)
				if nq ne 0 then status.mask[q]=0
			endif
		endif else begin
			if (*pl).cal.file ne '' then warning, 'maia_setup_apply_hymod', 'Failed to read Cal file.'
		endelse
		set_widget_text, (*pstate).hymod_cal_text, (*pl).cal.file

		dt = (*pl).deadtime.cal
		(*pm).deadtime.cal = dt
		obj->set_options, deadtime_cal=dt
		socket_command_set, ps, 'coeff', [dt.b,dt.a], class='deadtime.time', channel=-1, n_channels=2

		info = { auto:(*pl).deadtime.auto }
		s = stringify( info)
		socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_deadtime

		if do_throttle then begin
			if (*pl).throttle.on then begin
				f = get_throttle( (*pl).throttle.file, do_throttle=throttle_ok)
				if throttle_ok then begin
					n = (n_elements(f)<4096)
					f = f < 15			; hard trim to 4 bits for now ...
					socket_command_set, ps, 'FACTOR', f[0:n-1], class='throttle.energy', chip=-1, n_chips=n
					if (*pm).version.software ge 4737 then begin
						info = { on:(*pl).throttle.on, file:(*pl).throttle.file }
						s = stringify( info)
;						socket_command_set, ps, 'info', s, class='throttle'
						socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_throttle
						s2 = ((*pl).throttle.on) ? (*pl).throttle.file : ''
						socket_command_set, ps, 'info', s2, class='throttle'
					endif
					socket_command_set, ps, 'ENABLE', (*pl).throttle.on, class='throttle'
				endif else begin
					(*pl).throttle.on = 0
					warning,'maia_setup_apply_hymod', 'Failed to read Throttle file.'
				endelse
			endif
		endif
		widget_control, (*pstate).hymod_throttle_check, set_value=(*pl).throttle.on
		set_widget_text, (*pstate).hymod_throttle_text, (*pl).throttle.file
		(*pm).throttle = (*pl).throttle
		
		maia_setup_load_table, pstate
		if status.on then begin
			(*pstate).select = status.mask
			widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:status.mask, alt:0}
			widget_control, (*pstate).help, set_value=status.text
			(*pstate).colour_mode = 0
			(*pstate).last_check = (*pstate).check_mode
			widget_control, (*pstate).display_toggle, set_value=0
		endif
	return
		
bad_state:
	warning,'maia_setup_apply_hymod',['STATE variable has become ill-defined.','Abort Maia apply.'],/error
	return		
end

;--------------------------------------------------------------------------

pro maia_setup_apply_axes, pstate

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
		warning,'maia_setup_apply_axes',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
	pm = (*pstate).pmaia

	opt = (*pm).DevObj->get_options()
	(*pm).DA.axes = [opt.source.x,opt.source.y,opt.source.z]
	return
		
bad_state:
	warning,'maia_setup_apply_axes',['STATE variable has become ill-defined.','Abort Maia apply.'],/error
	return		
end

;--------------------------------------------------------------------------------------------------------------------

pro maia_setup_apply_imaging, pstate

; only imaging parameters and their enable status
; force the "enables" off if there are no DA and ROI tables found/entered.

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
		warning,'maia_setup_apply_imaging',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

@maia_scratch.def
	
	if ptr_good(pstate, /struct) eq 0 then goto, bad_state
	play = (*pstate).playout
	pm = (*pstate).pmaia
	pimage = (*pstate).pimage
	ps = (*pstate).psocket
	pl = (*pstate).plocal			; local copy of pm made on tab change, and by check and file
									; widgets. These sent to Maia, and read back as pm elsewhere.
	n_detectors = (*pm).n_detectors
	
	if (*pl).DA.on then begin
		maia_launch_read_da, ps, pm, pimage, scan=0, /init
		
		da = read_da( (*pl).DA.file, error=error)
		if error eq 0 then begin
			(*(*pstate).pimage).matrix.label = da.label
			(*(*pstate).pimage).matrix.file = da.file
			(*(*pstate).pimage).matrix.charge = da.charge
			if ptr_valid( (*(*pstate).pimage).matrix.mdl) then ptr_free, (*(*pstate).pimage).matrix.mdl
			(*(*pstate).pimage).matrix.mdl = ptr_new(da.mdl) 
			(*(*pstate).pimage).mode = 0
				
			n_el = da.n_el < (*pl).number.DA			; limit to 32 elements and
			cal = da.cal								; map onto 2048 channels (see below)
			matrix = da.matrix
			els = da.el[0:n_el-1]
			mine = 2048
			maxe = 0
			scale = fltarr(n_el)
			for i=0L,n_el-1 do begin
				q = where(matrix[*,i] ne 0.0)
				if q[0] ne -1 then begin
					mine = mine < q[0]
					maxe = maxe > max(q)
				endif

;			Scale factors are fixed point in Kandinski (with 6 places), so can't be too small.
;			Clip them here at 0.001 to help display of Back1, Back2 maps in RT images.

				scale[i] = (2.1 * max( abs(matrix[*,i]))) > 0.001	;1.0e-10 
			endfor

			array1 = da.array							; rGamma in this may have more rows that (*pm).DA.N

			array = {on:1, n_det:(*pm).n_detectors, rGamma:replicate(1.,(*pm).n_detectors,(*pm).DA.N)}
			for i=0L, ((*pm).DA.N < n_elements(array1.rGamma[0,*]))-1 do begin
				array.rGamma[*,i] = array1.rGamma[*,i]
			endfor

			if mine gt 2000 then warning,'Maia_setup','Large parts of DA matrix are zero.'
			if maxe le mine then warning,'Maia_setup','Zero or null DA matrix.'
			matrix = matrix[mine:maxe,0:n_el-1]
			siz = maxe - mine + 1
			if siz gt 2048 then begin
				el = mine * cal.a + cal.b
				eh = maxe * cal.a + cal.b
				cal.a = (eh-el)/2047
				cal.b = el
				matrix = congrid(matrix,2048,n_el)
				siz = 2048
			endif else begin
				cal.b = mine * cal.a + cal.b
			endelse
			c = 1. / cal.a			; 0.5 / cal.a
			d = - cal.b/cal.a		; 0.5 - cal.b/cal.a
			index = indgen(siz)
			print,'DA: c,d = ',c,d
			(*pl).ROI.on = 0
		endif else (*pl).DA.on = 0
	endif
	if (*pl).ROI.on and ((*pl).DA.on eq 0) then begin
		pcuts = read_cuts( (*pl).ROI.file, error=error)
		(*(*pstate).pimage).mode = 1
		if error eq 0 then begin
			n_el = n_elements(*pcuts) < (*pl).DA.n		; limit to 32 elements and
			cal_a = (*pcuts)[0].cal_a					; map onto 2048 channels (see below)
			cal_b = (*pcuts)[0].cal_b
			mine = min( ((*pcuts)[0:n_el-1].e[2] - cal_b)/cal_a)
			maxe = max( ((*pcuts)[0:n_el-1].e[3] - cal_b)/cal_a)
			matrix = fltarr(maxe+1,n_el)
			if mine gt 2000 then warning,'Maia_setup','Large parts of DA matrix are zero.'
			if maxe lt 1 then warning,'Maia_setup','Zero or null DA matrix.'
			for i=0L,n_el-1 do begin
				matrix[((*pcuts)[i].e[2] - cal_b)/cal_a:((*pcuts)[i].e[3] - cal_b)/cal_a, i] = 1.
			endfor
			els = replace(' ','-',strcompress((*pcuts)[0:n_el-1].el))
			scale = replicate(2.1,n_el)
			array = {on:1, n_det:(*pm).n_detectors, rGamma:replicate(1.,(*pm).n_detectors,(*pm).DA.N)}
			matrix = matrix[mine:maxe,0:n_el-1]
			siz = maxe - mine + 1
			if siz gt 2048 then begin
				el = mine * cal_a + cal_b
				eh = maxe * cal_a + cal_b
				cal_a = (eh-el)/2047
				cal_b = el
				matrix = congrid(matrix,2048,n_el)
				siz = 2048
			endif else begin
				cal_b = mine * cal_a + cal_b
			endelse
			c = 0.5 / cal_a
			d = - cal_b/cal_a		; 0.5 - cal_b/cal_a
			index = indgen(siz)
			print,'ROI: c,d = ',c,d
		endif else (*pl).ROI.on = 0
	endif
		
	if (*pl).DA.on or (*pl).ROI.on then begin	
		(*pl).DA.N = n_el
		(*pl).DA.scale[0:n_el-1] = scale
		(*pl).DA.name[0:n_el-1] = els
		*(*pl).DA.parray = array
		pshrmem = (*pstate).pshrmem_da
		scale_offset = 10
		if ptr_valid(pshrmem) then begin							; copy Scale to backgnd process/ shared memory
			pl0 = (*pshrmem).plong[0]
			pf0 = (*pshrmem).pfloat[0]
			(*pl0)[2] = n_el
			(*pf0)[scale_offset:scale_offset+n_el-1] = scale
		endif
			
		print,'Set DA matrix (scaled), scale factors, names and E cal ...'
		socket_command_set, ps, 'number', n_el, class='da.element'
		socket_command_set, ps, 'name', '""', class='da.element', chip=-1, n_chips=(*pm).number.DA
		socket_command_set, ps, 'name', els, class='da.element', chip=indgen(n_el)
		socket_command_set, ps, 'scale', scale, class='da.element', chip=indgen(n_el)
		socket_command_set, ps, 'colweight', 0, class='da.element', chip=-1, n_chips=(*pm).number.da, channel=-1, n_channels=2048
		socket_command_set, ps, 'colmap.coeff', [d,c], class='da', channel=[0,1]
		if (*pm).version.software ge 5411 then begin
			socket_command_set, ps, 'geometry', 0, class='da.element', chip=-1, n_chips=(*pm).number.da, channel=-1, n_channels=(*pm).n_detectors
		endif
		dindex = indgen((*pm).n_detectors)
		for j=0L,n_el-1 do begin									; copy scaled matrix to Maia, but make sure first
			f = matrix[0:siz-1,j]/scale[j]							; and last are zero to cater for clipping of 'column'
			f[0] = 0
			socket_command_set, ps, 'colweight', f, class='da.element', chip=j, channel=index, /quiet
			socket_command_set, ps, 'colweight', 0, class='da.element', chip=j, channel=2047, /quiet
			if (*pm).version.software ge 5411 then begin
				socket_command_set, ps, 'geometry', array.rGamma[*,j], class='da.element', chip=j, channel=dindex, /quiet
			endif
		endfor
		socket_command_set, ps, /report
		socket_command_set, ps, 'ENABLE', (*pl).DA.on or (*pl).ROI.on, class='da'
			
		; Need to pack all this into info (label, file, charge, MDLs, IC:{mode, conversion, PVs, name, val, unit} )
			
		if (*pm).version.software ge 4737 then begin
			if (*pl).DA.on then begin
				if (*pl).IC.pv.name eq '' then (*pl).IC.pv.name = (*(*pl).IC.plist)[0]
				da_state = { file:(*pl).DA.file , IC:(*pl).IC, matrix:(*pimage).matrix }
				s = stringify( da_state, /embed_ptr)
				socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_DA
			endif
			if (*pl).ROI.on and ((*pm).number.roi gt 0) then begin
				socket_command_set, ps, 'value', '"'+(*pl).roi.file+'"', class='scratch.datum', chip=scratch_ROI
			endif
		endif			
		maia_launch_update_DA_images, pimage, pl
		notify, 'image-elements', from=(*pstate).tlb
		
	endif else begin
		socket_command_set, ps, 'ENABLE', 0, class='da'
	endelse
	
	print,'Apply Images: Compare parray in pl, pm ...'
	help, *(*pl).DA.parray, *(*pm).DA.parray 
	copy_pointer_data, pl, pm, tag=['IC','DA','ROI']		; only copy these struct tags
	
	widget_control, (*pstate).imaging_da_check, set_value=(*pl).DA.on
	widget_control, (*pstate).imaging_roi_check, set_value=(*pl).ROI.on
	module_check = [(*pm).linear.on,(*pm).trim.on, (*pm).pileup.on,(*pm).throttle.on,(*pm).DA.on,(*pm).ROI.on, (*pm).groups.on, (*pm).DA.save]
	widget_control, (*pstate).enable_modules_check, set_value=module_check		
	return
	
bad_state:
	warning,'maia_setup_apply_imaging',['STATE variable has become ill-defined.','Abort Maia apply.'],/error
	return		
end

;---------------------------------------------------------------------------------------------------

pro maia_setup_apply_groups, pstate

		; only spectra parameters and their enable status
		; force the "enables" off if there are no group tables found/entered.
		
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
		warning,'maia_setup_apply_groups',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
	play = (*pstate).playout
	pm = (*pstate).pmaia
	ps = (*pstate).psocket
	pl = (*pstate).plocal			; local copy of pm only used for local "enables"
									; many find their way to pm via update from Maia
	ref = (*play).ref[ (*play).start + indgen((*play).N) ]
									; index back from CSV table to detector order
@maia_scratch.def

		if (*pm).version.software ge 4813 then begin
;			socket_command_set, ps, 'info', '"'+ (*pl).groups.file +'"', class='group'
			socket_command_set, ps, 'value', '"'+ (*pl).groups.file +'"', class='scratch.datum', chip=scratch_group
		endif

		g = intarr(16)
		if (*pm).groups.spectra gt 0 then begin
			for i=0L,(*pm).groups.spectra-1 do begin
				g[*] = 0					; spectra
				g[i] = 1
				socket_command_set, ps, 'select.group', g, channel=-1, n_channels=16, class='spectrum', chip=i
			endfor
		endif

		g[*] = 0						; Activity (ET 2D handled via ET events now)
		g[12] = 1
		socket_command_set, ps, 'select.group', g, channel=-1, n_channels=16, class='activity'
	
		g[*] = 0						; ET events
		g[13] = 1
		socket_command_set, ps, 'select.group', g, channel=-1, n_channels=16, class='event'
	
		g[*] = 0						; DA
		g[14] = 1
		socket_command_set, ps, 'select.group', g, channel=-1, n_channels=16, class='da'
	
		g[*] = 0						; dead time
		g[15] = 1
		socket_command_set, ps, 'select.group', g, channel=-1, n_channels=16, class='deadtime'
		;socket_command_set, ps, 'select.group', g, channel=-1, n_channels=16, class='deadtime_pp'

		for j=0L,n_elements((*pl).groups.group)-1 do begin
			q = where( (*pl).groups.group[j].table ne 0, nq)
			if nq gt 0 then found=1
			case j of 
				12: begin
					if (*pm).number.et2d ge 1 then begin
						socket_command_set, ps, 'pileup.enable', (*pl).groups.group[j].pileup, class='et2d'
						socket_command_set, ps, 'throttle.enable', (*pl).groups.group[j].throttle, class='et2d'
					endif
					(*pl).groups.group[j].table[*] = 1
					socket_command_set, ps, 'select.pileup', (*pl).groups.group[j].pileup, class='activity'
					socket_command_set, ps, 'select.throttle', (*pl).groups.group[j].throttle, class='activity'
					socket_command_set, ps, 'group', (*pl).groups.group[j].table[ref], class='group.det', channel=j, chip=-1, n_chips=(*pl).n_detectors
					end
				13: begin
					socket_command_set, ps, 'select.pileup', (*pl).groups.group[j].pileup, class='event'
					socket_command_set, ps, 'select.throttle', (*pl).groups.group[j].throttle, class='event'
					socket_command_set, ps, 'group', (*pl).groups.group[j].table[ref], class='group.det', channel=j, chip=-1, n_chips=(*pl).n_detectors
					end
				14: begin
					socket_command_set, ps, 'select.pileup', (*pl).groups.group[j].pileup, class='da'
					socket_command_set, ps, 'select.throttle', (*pl).groups.group[j].throttle, class='da'
					socket_command_set, ps, 'group', (*pl).groups.group[j].table[ref], class='group.det', channel=j, chip=-1, n_chips=(*pl).n_detectors
					end
				15: begin
					socket_command_set, ps, 'select.pileup', (*pl).groups.group[j].pileup, class='deadtime'
					socket_command_set, ps, 'select.throttle', (*pl).groups.group[j].throttle, class='deadtime'
;					socket_command_set, ps, 'select.pileup', (*pl).groups.group[j].pileup, class='deadtime_pp'
;					socket_command_set, ps, 'select.throttle', (*pl).groups.group[j].throttle, class='deadtime_pp'
					socket_command_set, ps, 'group', (*pl).groups.group[j].table[ref], class='group.det', channel=j, chip=-1, n_chips=(*pl).n_detectors
					end
				else: begin
					if j lt (*pl).groups.spectra then begin
;						t = (*pl).groups.group[j].table
;						q = (*play).data.index
;						d = indgen((*pm).n_detectors)
;						d[q] = t
						et = ['energy','time']
						socket_command_set, ps, 'source', et[(*pl).groups.group[j].et_mode], class='spectrum', chip=j
						socket_command_set, ps, 'select.pileup', (*pl).groups.group[j].pileup, class='spectrum', chip=j
						socket_command_set, ps, 'select.throttle', (*pl).groups.group[j].throttle, class='spectrum', chip=j
						socket_command_set, ps, 'group', (*pl).groups.group[j].table[ref], class='group.det', channel=j, chip=-1, n_chips=(*pl).n_detectors
					endif else begin
						d = intarr((*pm).n_detectors)
						socket_command_set, ps, 'group', d, class='group.det', channel=j, chip=-1, n_chips=(*pl).n_detectors
					endelse
					end
			endcase
		endfor
		(*pm).groups = (*pl).groups
		if (*pl).groups.spectra gt 0 then begin
			socket_command_set, ps, 'enable', (*pl).groups.on, class='spectrum', chip=-1, n_chips=(*pl).groups.spectra
		endif
		return
		
bad_state:
	warning,'maia_setup_apply_groups',['STATE variable has become ill-defined.','Abort Maia apply.'],/error
	return				
end

;--------------------------------------------------------------------------------------------------------------------

pro maia_setup_check_warning, pstate

;	Check what warning states are needed to display.
;	Notify Launch panel too.

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
		warning,'maia_setup_check_warning',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

pm = (*pstate).pmaia
q2 = where( (*pm).channel.hermes.eblk ne 0, nq2)		; EBLK
warn_eblk = (nq2 ne 0)
widget_control, (*pstate).warning_eblk_base, map=warn_eblk

warn_pulser = ((*pstate).pulser.mode ne 0)
widget_control, (*pstate).warning_pulser_base, map=warn_pulser

warn_debug = ((*pstate).debug_ean_mode ne 0) or ((*pstate).debug_aux_mode ne 0)
widget_control, (*pstate).warning_debug_base, map=warn_debug

bake_on = (*pm).control.peltier lt -0.02
widget_control, (*pstate).warning_bake_base, map=bake_on

; Set return colour for "Setup" button in Maia-launch ...
; 
;*(*pstate).plaunch = warn_eblk or warn_pulser or warn_debug
*(*pstate).plaunch = warn_pulser or warn_debug ? 1 : 0
if bake_on then *(*pstate).plaunch = 2 

notify, 'warn-setup', (*pstate).plaunch, from=(*pstate).tlb
return
end

;--------------------------------------------------------------------------

pro maia_setup_update_ic, pstate, pm

;	Update widgets for IC value and units
;	
;	First copy into working (*pm).IC from any selected PV ...
;	The 'remote' flag comes from the 'maia_client_parameters_slow'.
;	Do not copy the 'name' into IC for the h/w scalers; use the
;	generic names for these.

;	Is this test superfluous, as it is done already in 'maia_launch_read_da_info'?
;	(also when PV changed on Image tab)

	if (*pm).IC.pv.name eq 'Maia:scaler.FC0' then begin
		(*pm).IC.remote = (*pm).IC0.remote
		if (*pm).IC.remote then begin
			(*pm).IC.pv = (*pm).IC0.pv
			(*pm).IC.pv.name = 'Maia:scaler.FC0'
		endif
	endif else if (*pm).IC.pv.name eq 'Maia:scaler.FC1' then begin
		(*pm).IC.remote = (*pm).IC1.remote
		if (*pm).IC.remote then begin
			(*pm).IC.pv = (*pm).IC1.pv
			(*pm).IC.pv.name = 'Maia:scaler.FC1'
		endif
	endif else if (*pm).IC.pv.name eq 'Maia:dwell.time' then begin
		(*pm).IC.remote = 0
	endif else begin
		(*pm).IC.pv = (*pm).ICE.pv
		(*pm).IC.remote = 0
	endelse
	
	check_plist_maia, (*pm).IC.plist
	q = where( (*pm).IC.pv.name eq *(*pm).IC.plist, nq)
	widget_control, (*pstate).ic_pv_mode, set_value='   '+*(*pm).IC.plist
	widget_control, (*pstate).ic_pv_mode, set_combobox_select=(nq ne 0) ? q[0] : 0
	
;	Pass 'val' and 'unit' by reference, with /write_back, to enable their ranges
;	to be checked and corrected. Then pass back the indices 'ival', 'iunit' for droplists.

	l = locate('time', strlowcase((*pm).IC.PV.name))
	val = (*pm).IC.pv.val
	unit = (*pm).IC.pv.unit
	ival = find_charge_val_unit_index( val, unit, iunit=iunit, time=(l ge 0), /write_back)
	(*pm).IC.pv.val = val
	(*pm).IC.pv.unit = unit
	widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
	widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit
	return
end

;--------------------------------------------------------------------------

pro maia_setup_copy_state, pl, pm, ic=ic, init=init, da=da, roi=roi

; Copy contents of *pl to *pm. OBSOLETE: use 'copy_pointer_data, tag=[...]'
;
; /init		initialize the pm struct first
; /IC		just copy the (*pl).IC part
; /DA		just copy the (*pl).DA part
; /ROI		just copy the (*pl).ROI part

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
		warning,'maia_setup_copy_state',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(ic) lt 1 then ic=0
	if n_elements(init) lt 1 then init=0
	if n_elements(da) lt 1 then da=0
	if n_elements(roi) lt 1 then roi=0
		
;	Copy the (*pl) struct to (*pm), init (*pm) and but take care of plist pointer, etc.

if init then begin
	copy_pointer_data, pl, pm, /init
	return
endif

;	Copy the (*pl).IC struct to (*pm).IC, but take care of plist pointer

if IC then begin
	t1 = ptr_new( (*pl).IC)
	t = ptr_new( (*pm).IC)
	copy_pointer_data, t1, t
	(*pm).IC = *t
	ptr_free, t
	ptr_free, t1
	return
endif

if DA then begin
	t1 = ptr_new( (*pl).DA)
	t = ptr_new( (*pm).DA)
	copy_pointer_data, t1, t
	(*pm).DA = *t
	ptr_free, t
	ptr_free, t1
	return
endif

if ROI then begin
	t1 = ptr_new( (*pl).ROI)
	t = ptr_new( (*pm).ROI)
	copy_pointer_data, t1, t
	(*pm).ROI = *t
	ptr_free, t
	ptr_free, t1
	return
endif

;	Copy the *pl struct to *pm, but take care of embedded pointers

	copy_pointer_data, pl, pm
	return
end

;--------------------------------------------------------------------------

pro maia_setup_colour, pstate, vals, redisplay=redisplay, ignore=q, title=title

;	Set the mimic display pad colours to bytscl() the values 'vals',
;	which is a vector of n_detectors values.
;	Index order is that of state-button order in mimic, not detector pad order.

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
		warning,'maia_setup_colour',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if (*pstate).colour_mode ne 1 then return
if n_elements(redisplay) eq 0 then redisplay=0
if n_elements(title) eq 0 then title=''
pm = (*pstate).pmaia
widget_control, (*pstate).detector, set_value={select:indgen((*pm).n_detectors), value:0}

if redisplay then begin
	vals = (*pstate).colour_vector
	title = (*pstate).colour_title
endif else begin
	if n_elements(vals) ne (*pm).n_detectors then begin
		warning,'maia_setup_colour','not a vector of '+string((*pm).n_detectors)
		return
	endif
	(*pstate).colour_vector = vals
	(*pstate).colour_title = title
endelse
n = n_elements(vals)
mask = bytarr(n)
if n_elements(q) ge 1 then begin
	if q[0] ne -1 then mask[q] = 1
endif
q2 = where(mask eq 0, nq)
if nq lt 1 then q2 = indgen(n)
c = 16B + bytscl(vals[q2],top=99)
widget_control, (*pstate).detector, set_value={mode:1, select:q2, colour:c}
low = str_tidy(min(vals[q2])) & high = str_tidy(max(vals[q2]))
s = [title,'Max = '+low, 'Min = '+high]
widget_control, (*pstate).detector, set_value={legend:s}
return
end

;--------------------------------------------------------------------------

pro maia_setup_check_group_mode, pstate

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
		warning,'maia_setup_group_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if (*pstate).group_mask[(*pstate).group_mode] eq 0 then begin
		q = where( ((*pstate).group_mask eq 1), nq)
		if nq ge 1 then begin
			q2 = where( q ge (*pstate).group_mode, nq2)
			if nq2 ge 1 then begin
				m = q[q2[0]]
			endif  else begin
				m = q[0]
			endelse
			(*pstate).group_mode = m
			widget_control, (*pstate).detector_mode, set_combobox_select=m
		endif
	endif
	return
end

;--------------------------------------------------------------------------

pro maia_setup_correct_leakage, p

;	Input ELK volts (V) and return current (pA)
;	Cal range and available voltage spans just 0-70 pA each

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
		warning,'maia_setup_correct_leakage',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if ptr_good(p) eq 0 then return
	
	return							; ignore conversion to pA for now
	
	intercept = 2.55				; V
	slope = 7.0						; mV/pA
	
	x = ((*p > intercept) - intercept) / (slope * 0.001)

	*p = x							; pA
	return
end

;--------------------------------------------------------------------------

pro maia_setup_write_leakage, pstate, p, title=title

;	Write the ELK map to local home/.geopixe dir

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
		warning,'maia_setup_write_leakage',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(title) eq 0 then title='ELK map'

	if ptr_good(p) eq 0 then return
	
	path = geopixe_environment()
	file = path + 'ELK-map.csv'
	on_ioerror, fin
	openw, lun, file, /get_lun
	printf, lun, '# ' + title
	printf, lun, '# Channel, ELK (pA or V)'
	for i=0,n_elements(*p)-1 do begin
		printf, lun, i, ',', (*p)[i]
	endfor
fin:
	close_file, lun
	return
end

;--------------------------------------------------------------------------

pro maia_setup_groups_read, pstate

; Read parameters from Maia for setting entries in the Group table.
; Note we only set parameters that are eventually read-back from Maia, so we'll
; use the *pl copy for all.

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
		warning,'maia_setup_groups_read',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ps = (*pstate).psocket
	pl = (*pstate).plocal
	play = (*pstate).playout

	(*pl).groups.file = ''
	maia_launch_read_groups, ps, pl, play
end

;--------------------------------------------------------------------------

pro maia_setup_initial, ps, pm, pr, pimage, play

; Read Maia parameters for initial display of Widgets and Summary table.

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
		warning,'maia_setup_initial',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	maia_launch_read_enable, ps, pm, pr, pimage, play
;	maia_launch_read_info, ps, pm, pimage						; done in read enable now
	maia_launch_read_groups, ps, pm, play
	return
end

;--------------------------------------------------------------------------

pro maia_setup_load_group_table, pstate

; Here we use and don't set any Maia parameters, so we'll use the *pl copy.

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
		warning,'maia_setup_load_group_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
pl = (*pstate).plocal

case !version.os_family of
	'MacOS': begin
       symbol = 'SYMBOL*12'
       large_font = 'Arial*12'
       def_font = 'Geneva*10'
		fnt1 = 'HELVETICA*8'       ;'HELVETICA*9'
		wscale = 1.2
		end
	'unix': begin
       symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
       large_font = '10x20'
       def_font = '6x13'
		fnt1 = 'timr08'             ;'6x12'
		wscale = 1.2
		end
	else: begin
       symbol = 'SYMBOL*BOLD*14'
       large_font = 'COURIER*BOLD*10'
       def_font = 'Arial*14'
		fnt1 = 'Helvetica*8'       ;'ARIAL*12'
		wscale = 1.0
		end
endcase

n = 16
nc = 7
rows = string(indgen(n))
columns = ['Indx','Type','E/T','Group Title','Set','PU Rej','Throt']
widths = wscale * [4,10,4,16,4,6,6] * !d.x_ch_size
t = strarr(nc,n)

t[0,*] = str_tidy(indgen(n))
t[1,*] = '- n/a -'
if (*pl).groups.spectra gt 0 then t[1,0:(*pl).groups.spectra-1] = 'Spectrum ' + str_tidy(indgen((*pl).groups.spectra))
t[1,12] = 'Activity'
t[1,13] = 'ET records'
t[1,14] = 'DA Imaging'
t[1,15] = 'Dead Time'
t[2,0:(*pl).groups.spectra-1] = 'E'
q = where( (*pl).groups.group.et_mode eq 1, nq)
if nq ge 1 then begin
	q2 = where( (q lt (*pl).groups.spectra), nq2)
	if nq2 ge 1 then begin
		q = q[q2]
		t[2,q] = 'T'
	endif
endif
t[2,12:15] = ''
t[3,*] = '---'
t[3,0:(*pl).groups.spectra-1] = (*pl).groups.group[0:(*pl).groups.spectra-1].title
(*pl).groups.group[12].title = 'All'
t[3,12:*] = (*pl).groups.group[12:*].title
t[4,0:(*pl).groups.spectra-1] = 'SET'
t[4,13:*] = 'SET'
t[5,*] = ''
q = where( (*pl).groups.group.pileup eq 1, nq)
if nq ge 1 then begin
	q2 = where( (q lt (*pl).groups.spectra) or (q ge 12), nq2)
	if nq2 ge 1 then begin
		q = q[q2]
		t[5,q] = 'On'
	endif
endif
t[6,*] = ''
q = where( (*pl).groups.group.throttle eq 1, nq)
if nq ge 1 then begin
	q2 = where( (q lt (*pl).groups.spectra) or (q ge 12), nq2)
	if nq2 ge 1 then begin
		q = q[q2]
		t[6,q] = 'On'
	endif
endif

widget_control, (*pstate).group_table, set_value=t, column_widths=widths, $
			row_labels=rows, column_labels=columns, $
			table_xsize=nc, table_ysize=n, align=1
;			use_table_select=[0,0,n_el,n-1]
return
end

;--------------------------------------------------------------------------

pro maia_setup_load_table, pstate

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
		warning,'maia_setup_load_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
pm = (*pstate).pmaia

case !version.os_family of
	'MacOS': begin
       symbol = 'SYMBOL*12'
       large_font = 'Arial*12'
       def_font = 'Geneva*10'
		fnt1 = 'HELVETICA*8'       ;'HELVETICA*9'
		wscale = 1.2
		end
	'unix': begin
       symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
       large_font = '10x20'
       def_font = '6x13'
		fnt1 = 'timr08'             ;'6x12'
		wscale = 1.2
		end
	else: begin
       symbol = 'SYMBOL*BOLD*14'
       large_font = 'COURIER*BOLD*10'
       def_font = 'Arial*14'
		fnt1 = 'Helvetica*8'       ;'ARIAL*12'
		wscale = 1.0
		end
endcase

n = (*pm).n_detectors
play = (*pstate).playout
nc = 35
rows = string(indgen(n))
widths = replicate(6.0,nc) 
t = strarr(nc,n)

; The table[] indices are for chip = 1 (hermes), 2 (scepter), 3 (ddm)
; to select the structs within 'channel' struct array, and used in
; summary table event. The par indices select an item within the
; respective structs (see channel).
; N.B. This means order of parameters in structs is important!
;
; Table 'chip' numbers, to identify structs in pmaia, etc., are:
;	pmaia channel:	0	none		3	Cal
;					1	Hermes		4	trim.E	
;					2	Scepter		5	trim.T
;	playout:		6	data
;	(see application in summary-table event)
;
; Check box ID index:
; time, gain, eblk, elk, tdm, tds, tos, trk, trke, trim, thresh, clock, thpd, filt, tcm
;  0     1     2    3    4    5    6    7    8     9      10     11     12    13    14
;
;            0      1     2     3      4      5     6     7      8       9      10      11     12    13     14    15     16
columns = ['Indx','Det','ech','time','gain','tdm','tds','tos','thresh','trim','thpd','clock','eblk','tcm','filt','trk','trke']
checks = [  -1,    -1,   -1,    0,    1,     4,     5,    6,     10,     9,     12,    11,     2,     14,   13,    7,     8]

widths[0:7] = 3.5 					; Indx, Det, ech, time, gain, tdm, tds, tos
widths[8:11] = [5.2,5.2,5.2,4.7] 	; thresh, trim, thpd, clock
widths[12:16] = 3.5 

; Assumes table in CSV table order, as is (*pm).channel
; (*play).data.index is detector number given CSV index.
; (*play).ref is CSV index given detector number.

t[0,*] = str_tidy(indgen(n))
t[1,*] = str_tidy((*play).data.index)
t[2,*] = str_tidy(*(*pstate).pdisable)
t[3,*] = str_tidy((*pm).channel.hermes.time)
	(*pstate).table[3].chip = 1  &  (*pstate).table[3].par = 0
t[4,*] = str_tidy((*pm).channel.hermes.gain)
	(*pstate).table[4].chip = 1  &  (*pstate).table[4].par = 1
t[5,*] = str_tidy((*pm).channel.scepter.tdm)
	(*pstate).table[5].chip = 2  &  (*pstate).table[5].par = 0
t[6,*] = str_tidy((*pm).channel.scepter.tds)
	(*pstate).table[6].chip = 2  &  (*pstate).table[6].par = 1
t[7,*] = str_tidy((*pm).channel.scepter.tos)
	(*pstate).table[7].chip = 2  &  (*pstate).table[7].par = 2
t[8,*] = str_tidy((*pm).channel.scepter.thresh, length=5)
	(*pstate).table[8].chip = 2  &  (*pstate).table[8].par = 6
t[9,*] = str_tidy((*pm).channel.scepter.trim, length=4)
	(*pstate).table[9].chip = 2  &  (*pstate).table[9].par = 5
t[10,*] = str_tidy((*pm).channel.scepter.thpd, length=4)
	(*pstate).table[10].chip = 2  &  (*pstate).table[10].par = 8
t[11,*] = str_tidy((*pm).channel.scepter.clock, length=4)
	(*pstate).table[11].chip = 2  &  (*pstate).table[11].par = 7
t[12,*] = str_tidy((*pm).channel.hermes.eblk)
	(*pstate).table[12].chip = 1  &  (*pstate).table[12].par = 2
t[13,*] = str_tidy((*pm).channel.scepter.tcm)
	(*pstate).table[13].chip = 2  &  (*pstate).table[13].par = 10
t[14,*] = str_tidy((*pm).channel.scepter.filt)
	(*pstate).table[14].chip = 2  &  (*pstate).table[14].par = 9
t[15,*] = str_tidy((*pm).channel.scepter.trk)
	(*pstate).table[15].chip = 2  &  (*pstate).table[15].par = 3
t[16,*] = str_tidy((*pm).channel.scepter.trke)
	(*pstate).table[16].chip = 2  &  (*pstate).table[16].par = 4
npars = 17

;t[4,*] = str_tidy((*pm).channel.hermes.elk)						; 'elk'

columns = [columns, 'CalA','CalB','TrimEa','TrimEb','TrimTa','TrimTb']
checks = [checks, replicate(-1,6)]
widths[npars:npars+5] = 6.0 

t[npars,*] = str_tidy((*pm).channel.cal.a, length=6)
	(*pstate).table[npars].chip = 3  &  (*pstate).table[npars].par = 0
t[npars+1,*] = str_tidy((*pm).channel.cal.b, length=6)
	(*pstate).table[npars+1].chip = 3  &  (*pstate).table[npars+1].par = 1
t[npars+2,*] = str_tidy((*pm).channel.trim.E.a, length=6)
	(*pstate).table[npars+2].chip = 4  &  (*pstate).table[npars+2].par = 0
t[npars+3,*] = str_tidy((*pm).channel.trim.E.b, length=6)
	(*pstate).table[npars+3].chip = 4  &  (*pstate).table[npars+3].par = 1
t[npars+4,*] = str_tidy((*pm).channel.trim.T.a, length=6)
	(*pstate).table[npars+4].chip = 5  &  (*pstate).table[npars+4].par = 0
t[npars+5,*] = str_tidy((*pm).channel.trim.T.b, length=6)
	(*pstate).table[npars+5].chip = 5  &  (*pstate).table[npars+5].par = 1
npars = npars+6

columns = [columns,'X','Y','Z','Width','Height','Tilt','FWHM','Chip','Quad','Radial','Col','Row']
checks = [checks, replicate(-1,12)]
widths[npars:npars+4] = 5.5 
widths[npars+5] = 4.8 
widths[npars+6] = 6.0 
widths[npars+7] = 4.3
widths[npars+8:npars+9] = 5.0
widths[npars+10:npars+11] = 4.3 

t[npars,*] = str_tidy((*play).data.x, length=5)
	(*pstate).table[npars].chip = 6  &  (*pstate).table[npars].par = 1
t[npars+1,*] = str_tidy((*play).data.y, length=5)
	(*pstate).table[npars+1].chip = 6  &  (*pstate).table[npars+1].par = 2
t[npars+2,*] = str_tidy((*play).data.z, length=5)
	(*pstate).table[npars+2].chip = 6  &  (*pstate).table[npars+2].par = 3
t[npars+3,*] = str_tidy((*play).data.width, length=5)
	(*pstate).table[npars+3].chip = 6  &  (*pstate).table[npars+3].par = 4
t[npars+4,*] = str_tidy((*play).data.height, length=5)
	(*pstate).table[npars+4].chip = 6  &  (*pstate).table[npars+4].par = 5
t[npars+5,*] = str_tidy((*play).data.tilt, length=4)
	(*pstate).table[npars+5].chip = 6  &  (*pstate).table[npars+5].par = 6
t[npars+6,*] = str_tidy((*play).data.FWHM, length=5)
	(*pstate).table[npars+6].chip = 6  &  (*pstate).table[npars+6].par = 8
t[npars+7,*] = str_tidy((*play).data.Hermes)
	(*pstate).table[npars+7].chip = 6  &  (*pstate).table[npars+7].par = 9
t[npars+8,*] = str_tidy((*play).data.Quadrant)
	(*pstate).table[npars+8].chip = 6  &  (*pstate).table[npars+8].par = 10
t[npars+9,*] = str_tidy((*play).data.radial)
	(*pstate).table[npars+9].chip = 6  &  (*pstate).table[npars+9].par = 11
t[npars+10,*] = str_tidy((*play).data.column)
	(*pstate).table[npars+10].chip = 6  &  (*pstate).table[npars+10].par = 12
t[npars+11,*] = str_tidy((*play).data.row)
	(*pstate).table[npars+11].chip = 6  &  (*pstate).table[npars+11].par = 13
npars = npars+12

(*pstate).table[0:nc-1].check = checks
(*pstate).table[0:nc-1].column = columns
widths = wscale * widths * !d.x_ch_size

v = float(t[(*pstate).sort.column,*])
vmax = max(v)
v = float(long(v * 10000./max(v)))
if (*pstate).sort.column gt 1 then v = v + float(t[1,*])/1000.
q2 = sort( v)
(*pstate).sort.index = q2

widget_control, (*pstate).summary_table, set_value=t[*,q2], column_widths=widths, $
			row_labels=rows[q2], column_labels=columns, $
			table_xsize=nc, table_ysize=n
;			use_table_select=[0,0,n_el,n-1], align=2

return
end

;--------------------------------------------------------------------------------------------------------------------

pro maia_setup_pulser_map, pstate

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
		warning,'maia_setup_pulset_map',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if ptr_good(pstate, /struct) eq 0 then return
	pm = (*pstate).pmaia

	if ((*pm).n_detectors lt 384) and ((*pm).version.dam lt 2) then begin
		if (*pstate).pulser.mode eq 5 then begin
			(*pstate).pulser.mode = 4
			warning,'maia_setup_pulser_map','ELK leakage mapping not supported by this hardware version.'
			widget_control, (*pstate).pulser_program_mode, set_combobox_select=4
		endif
	endif

	case (*pstate).pulser.mode of
			0: begin					; pulser off
				widget_control, (*pstate).pulser_start_base, map=0
				widget_control, (*pstate).pulser_progress_base, map=0
				widget_control, (*pstate).pulser_count_base, map=0
				widget_control, (*pstate).pulser_time_base, map=0
				widget_control, (*pstate).pulser_rate_base, map=0
				widget_control, (*pstate).pulser_low_label, map=0
				widget_control, (*pstate).pulser_high_base, map=0
				(*pstate).pulser.on = 0
				end
			1: begin					; manual pulser
				widget_control, (*pstate).pulser_start_base, map=1
				widget_control, (*pstate).pulser_progress_base, map=0
				widget_control, (*pstate).pulser_count_base, map=0
				widget_control, (*pstate).pulser_high_base, map=0
				widget_control, (*pstate).pulser_time_base, map=0
				widget_control, (*pstate).pulser_low_label, map=1, set_value='   Amp:'
				widget_control, (*pstate).pulser_rate_base, map=1
				end
			2: begin					; step-cal
				widget_control, (*pstate).pulser_start_base, map=1
				widget_control, (*pstate).pulser_progress_base, map=1
				widget_control, (*pstate).pulser_count_base, map=1
				widget_control, (*pstate).pulser_time_base, map=1
				widget_control, (*pstate).pulser_low_label, map=1, set_value='   Low:'
				widget_control, (*pstate).pulser_high_label, map=1, set_value='High:'
				widget_control, (*pstate).pulser_rate_base, map=1
				end
			3: begin					; gain-trim
				widget_control, (*pstate).pulser_start_base, map=1
				widget_control, (*pstate).pulser_progress_base, map=1
				widget_control, (*pstate).pulser_count_base, map=0
				widget_control, (*pstate).pulser_time_base, map=1
				widget_control, (*pstate).pulser_low_label, map=1, set_value='   Low:'
				widget_control, (*pstate).pulser_high_label, map=1, set_value='High:'
				widget_control, (*pstate).pulser_rate_base, map=1
				end
			4: begin					; synth pulser
				widget_control, (*pstate).pulser_start_base, map=1
				widget_control, (*pstate).pulser_progress_base, map=0
				widget_control, (*pstate).pulser_count_base, map=0
				widget_control, (*pstate).pulser_time_base, map=0
				widget_control, (*pstate).pulser_low_label, map=1, set_value=' E Amp:'
				widget_control, (*pstate).pulser_high_label, map=1, set_value='T Amp:'
				widget_control, (*pstate).pulser_rate_base, map=1
				end
			5: begin					; ELK mapper
				widget_control, (*pstate).pulser_start_base, map=1
				widget_control, (*pstate).pulser_progress_base, map=1
				widget_control, (*pstate).pulser_count_base, map=0
				widget_control, (*pstate).pulser_time_base, map=1
				widget_control, (*pstate).pulser_low_label, map=0
				widget_control, (*pstate).pulser_high_label, map=0
				widget_control, (*pstate).pulser_rate_base, map=0
				end
			6: begin					; EAN mapper
				widget_control, (*pstate).pulser_start_base, map=1
				widget_control, (*pstate).pulser_progress_base, map=1
				widget_control, (*pstate).pulser_count_base, map=0
				widget_control, (*pstate).pulser_time_base, map=1
				widget_control, (*pstate).pulser_low_label, map=0
				widget_control, (*pstate).pulser_high_label, map=0
				widget_control, (*pstate).pulser_rate_base, map=0
				end
			7: begin					; step-cal ALL
				widget_control, (*pstate).pulser_start_base, map=1
				widget_control, (*pstate).pulser_progress_base, map=1
				widget_control, (*pstate).pulser_count_base, map=1
				widget_control, (*pstate).pulser_time_base, map=1
				widget_control, (*pstate).pulser_low_label, map=1, set_value='   Low:'
				widget_control, (*pstate).pulser_high_label, map=1, set_value='High:'
				widget_control, (*pstate).pulser_rate_base, map=1
				end
			else:
	endcase
	return
end

;----------------------------------------------------------------------------

pro maia_setup_read_layout, pstate

; Read layout coords from Kandinski and determine det# based on these.

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
		warning,'maia_setup_read_layout',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	pm = (*pstate).pmaia
	ps = (*pstate).psocket
	play = (*pstate).playout	
	n_detectors = (*pm).n_detectors

	x = socket_command_get( ps, 'coord', class='config.layout.det', chip=-1, n_chips=n_detectors, channel=0, error=err)
	if err then goto, bad_read
	y = socket_command_get( ps, 'coord', class='config.layout.det', chip=-1, n_chips=n_detectors, channel=1, error=err)
	if err then goto, bad_read

	for i=0,n_detectors-1 do begin
		q = where( ((*play).data.column eq x[i]) and ((*play).data.row eq y[i]), nq)
		if (nq gt 0)then begin
			(*play).data[q[0]].index = i
			(*play).ref[i] = q[0]
		endif else begin
			warning,'maia_setup_read_layout','Failed to find pad X,Y=',x[i],y[i]
		endelse
	endfor
	widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), label:str_tidy((*play).data.index), value:0}
	return
	
bad_read:
	warning,'','bad read of layout from Kandinski.'
	return
end

;--------------------------------------------------------------------------------------------------------------------
	 	
pro maia_setup_sensitive, pstate, hermes=hermes, scepter=scepter

; Set sensitive=0 for all Hermes and Scepter parameter widgets, except the indicated pad class group

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
		warning,'maia_setup_sensitive',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(hermes) lt 1 then hermes=0
if n_elements(scepter) lt 1 then scepter=0

for i=0L,n_elements( (*pstate).ids_hermes)-1 do begin
	if widget_info((*pstate).ids_hermes[i],/valid) then widget_control, (*pstate).ids_hermes[i], sensitive=0
endfor
for i=0L,n_elements( (*pstate).ids_scepter)-1 do begin
	if widget_info((*pstate).ids_scepter[i],/valid) then widget_control, (*pstate).ids_scepter[i], sensitive=0
endfor

case hermes of
	1: begin
	 	for i=0L,n_elements( (*pstate).ids_hermes_channel)-1 do begin
			if widget_info((*pstate).ids_hermes_channel[i],/valid) then widget_control, (*pstate).ids_hermes_channel[i], sensitive=1
		endfor
	 	end
	2: begin
	 	for i=0L,n_elements( (*pstate).ids_hermes_chip)-1 do begin
			if widget_info((*pstate).ids_hermes_chip[i],/valid) then widget_control, (*pstate).ids_hermes_chip[i], sensitive=1
		endfor
	 	end
	3: begin
	 	for i=0L,n_elements( (*pstate).ids_hermes_whole)-1 do begin
			if widget_info((*pstate).ids_hermes_whole[i],/valid) then widget_control, (*pstate).ids_hermes_whole[i], sensitive=1
		endfor
	 	end
	 else:
endcase
case scepter of
	1: begin
	 	for i=0L,n_elements( (*pstate).ids_scepter_channel)-1 do begin
			if widget_info((*pstate).ids_scepter_channel[i],/valid) then widget_control, (*pstate).ids_scepter_channel[i], sensitive=1
		endfor
	 	end
	2: begin
	 	for i=0L,n_elements( (*pstate).ids_scepter_chip)-1 do begin
			if widget_info((*pstate).ids_scepter_chip[i],/valid) then widget_control, (*pstate).ids_scepter_chip[i], sensitive=1
		endfor
	 	end
	 else:
endcase
return
end

;--------------------------------------------------------------------------------------------------------------------
	 	
pro maia_setup_update_dynamic, pstate

;	Put things that need immediate updating in 'maia-display' Notify code in event
;	routine, such as widgets that should update before your eyes. 
;	But widgets that may get edited should go here.

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
		warning,'maia_setup_update_dynamic',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	pm = (*pstate).pmaia
	if (*pm).DA.on then (*pm).ROI.on=0

	widget_control, (*pstate).hymod_linearization_check, set_value=(*pm).linear.on
	widget_control, (*pstate).hymod_gain_trim_check, set_value=(*pm).trim.on
	widget_control, (*pstate).hymod_pileup_check, set_value=(*pm).pileup.on
	widget_control, (*pstate).hymod_throttle_check, set_value=(*pm).throttle.on
	widget_control, (*pstate).imaging_da_check, set_value=(*pm).DA.on
	widget_control, (*pstate).imaging_roi_check, set_value=(*pm).ROI.on
	widget_control, (*pstate).spectra_enable_check, set_value=(*pm).groups.on
	widget_control, (*pstate).hymod_deadtime_auto, set_value=(*pm).deadtime.auto
	
	widget_control, (*pstate).controls_peltier_slider, set_value = abs((*pm).control.peltier)
	widget_control, (*pstate).controls_bias_slider, set_value = (*pm).control.bias
	widget_control, (*pstate).controls_guard_slider, set_value = (*pm).control.guard
	(*pstate).peltier_mode = ((*pm).control.peltier lt -0.02)
	widget_control, (*pstate).controls_peltier_mode, set_combobox_select = (*pstate).peltier_mode

	set_widget_text, (*pstate).hymod_linearization_text, (*pm).linear.file
	set_widget_text, (*pstate).hymod_gain_trim_text, (*pm).trim.file
	set_widget_text, (*pstate).hymod_gain_trim_text2, (*pm).trim.file2
	set_widget_text, (*pstate).hymod_pileup_text, (*pm).pileup.file
	set_widget_text, (*pstate).hymod_cal_text, (*pm).cal.file
	set_widget_text, (*pstate).hymod_throttle_text, (*pm).throttle.file
	set_widget_text, (*pstate).imaging_da_text, (*pm).da.file
	set_widget_text, (*pstate).imaging_roi_text, (*pm).roi.file
	
	set_widget_text, (*pstate).hymod_deadtime_offset, str_tidy((*pm).deadtime.cal.b)
	set_widget_text, (*pstate).hymod_deadtime_slope, str_tidy((*pm).deadtime.cal.a)
		
	maia_setup_update_ic, pstate, pm
	widget_control, (*pstate).charge_conversion, set_value=str_tidy((*pm).IC.conversion)

	widget_control, (*pstate).charge_mode, set_combobox_select=(*pm).IC.mode
	case (*pm).IC.mode of
		0: begin
			widget_control, (*pstate).ic_base, map=0
			widget_control, (*pstate).scan_button, sensitive=0
			widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
			widget_control, (*pstate).ic_base2, scr_ysize=1
			end
		1: begin
			widget_control, (*pstate).ic_base, map=1
			widget_control, (*pstate).scan_button, sensitive=1
			widget_control, (*pstate).ic_base1, map=1, scr_ysize=(*pstate).ic_base1_ysize
			widget_control, (*pstate).ic_base2, scr_ysize=(*pstate).ic_base2_ysize
			end
		2: begin
			widget_control, (*pstate).ic_base, map=1
			widget_control, (*pstate).scan_button, sensitive=0
			widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
			widget_control, (*pstate).ic_base2, scr_ysize=(*pstate).ic_base2_ysize
			end
	endcase	
	widget_control, (*pstate).auto_DA_check, set_value=[(*pm).DA.save]
	
	return
end

;--------------------------------------------------------------------------

pro maia_setup_update_debug, pstate

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
		warning,'maia_setup_update_debug',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	ps = (*pstate).psocket
	pm = (*pstate).pmaia
	play = (*pstate).playout
	if (*pstate).debug_ean_check then begin
		case (*pstate).debug_ean_mode of
			0: begin
				socket_command_set, ps, 'EAN', 0, class='hermes', chip=-1, channel=-1
				end
			1: begin					; HERMES OAN
				socket_command_set, ps, 'EAN', 0, class='hermes', chip=-1, channel=-1 
				q = where( (*pstate).select eq 1, nq)
				if nq ge 1 then begin
					c = (*play).data[q[0]].index mod 32
					h = (*play).data[q[0]].hermes
					socket_command_set, ps, 'EAN', 1, class='hermes', chip=h, channel=c
					socket_command_set, ps, 'ELK', 0, class='hermes', chip=-1, channel=-1
				endif
				end
			2: begin					; HERMES ELK
				socket_command_set, ps, 'ELK', 0, class='hermes', chip=-1, channel=-1 
				q = where( (*pstate).select eq 1, nq)
				if nq ge 1 then begin
					c = (*play).data[q[0]].index mod 32
					h = (*play).data[q[0]].hermes
					socket_command_set, ps, 'ELK', 1, class='hermes', chip=h, channel=c
					socket_command_set, ps, 'EAN', 0, class='hermes', chip=-1, channel=-1
				endif
				end
		endcase
	endif

	if (*pstate).debug_aux_check then begin
		case (*pstate).debug_aux_mode of
			0: begin
				socket_command_set, ps, 'LOCK', 0, class='scepter', chip=-1 
				socket_command_set, ps, 'AUX', 0, class='scepter', chip=-1 
				if (*pm).version.scepter ge 6 then begin
					socket_command_set, ps, 'MASK', 0, class='scepter', chip=-1, channel=-1
				endif
				end
			1: begin					; SCEPTER AUX
				; Later may have selector droplist between AA output on Lemo (AUX=0) and PDOUT (AUX=1)
				socket_command_set, ps, 'LOCK', 0, class='scepter', chip=-1 
				socket_command_set, ps, 'AUX', 0, class='scepter', chip=-1 
				q = where( (*pstate).select eq 1, nq)
				if (nq ge 1) and (q[0] ge 0) then begin
					 
					; Scepter 6/7:  stop using BLA, and
					; use MASK to (de)select desired AUX channel. Also when zeroing debug
					; elsewhere. Use (*pm).version.scepter.
					
					c = (*play).data[q[0]].index mod 32
					h = (*play).data[q[0]].hermes
					socket_command_set, ps, 'LOCK', 1, class='scepter', chip=h 
					if (*pm).version.scepter ge 6 then begin
						socket_command_set, ps, 'MASK', 1, class='scepter', chip=-1, channel=-1
						socket_command_set, ps, 'MASK', 0, channel=c, class='scepter', chip=h
					endif else begin
						socket_command_set, ps, 'BLA', c, class='scepter', chip=h
					endelse
				endif
				end
		endcase
	endif
return
end

;--------------------------------------------------------------------------

pro OnRealize_maia_group_table, wWidget

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
		warning,'maia_setup_group_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

maia_setup_load_group_table, pstate
return
end

;--------------------------------------------------------------------------

pro OnRealize_maia_setup_detector, wWidget

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
		warning,'maia_setup_setup_detector',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, (*pstate).detector, set_value={select:indgen((*(*pstate).pmaia).n_detectors), value:(*(*pstate).pdisable), alt:1}
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_summary_table, wWidget

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
		warning,'OnRealize_maia_summary_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

maia_setup_load_table, pstate
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_linearization_file, wWidget

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
		warning,'OnRealize_maia_linearization_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

set_widget_text, wWidget, (*(*pstate).pmaia).linear.file
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_charge_conversion, wWidget

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
		warning,'OnRealize_maia_charge_conversion',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).IC_base2, /geometry)
(*pstate).IC_base2_ysize = geo.ysize
return
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_setup_charge_mode, wWidget

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
		warning,'OnRealize_maia_charge_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).pmaia) then begin
	pm = (*pstate).pmaia
	widget_control, wWidget, set_combobox_select=(*pm).IC.mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_setup_charge_preamp_mode, wWidget

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
		warning,'OnRealize_maia_preamp_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).pmaia) then begin
	pm = (*pstate).pmaia
;	Done in units now ...
;	q = where( abs((*pm).IC.pv.val - (*pstate).ic_vals) lt 0.01, nq)
;	if nq ne 0 then begin
;		widget_control, wWidget, set_combobox_select=q[0]
;	endif
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_setup_charge_unit_mode, wWidget

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
		warning,'OnRealize_maia_charge_unit_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).pmaia) then begin
	pm = (*pstate).pmaia
	l = locate('time', strlowcase((*pm).IC.PV.name))
	val = (*pm).IC.pv.val
	unit = (*pm).IC.pv.unit
	ival = find_charge_val_unit_index( val, unit, iunit=iunit, time=(l ge 0), /write_back)
	(*pm).IC.pv.val = val
	(*pm).IC.pv.unit = unit
	widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
	widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit
	off = ((*pm).IC.pv.name eq 'Maia:dwell.time') ? 0 : 1 
	widget_control, (*pstate).ic_val_mode, sensitive=off
	widget_control, (*pstate).ic_unit_mode, sensitive=off
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_setup_charge_pv_mode, wWidget

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
		warning,'OnRealize_maia_charge_pv_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).IC_base1, /geometry)
(*pstate).IC_base1_ysize = geo.ysize

if ptr_valid( (*pstate).pmaia) then begin
	pm = (*pstate).pmaia
	if ptr_good((*pm).IC.plist) then begin
		q = where( (*pm).IC.pv.name eq *(*pm).IC.plist, nq)
		if nq ne 0 then begin
			widget_control, wWidget, set_combobox_select=q[0]
		endif
	endif
	widget_control, (*pstate).ic_base, map=((*pm).IC.mode ne 0 )
	if (*pm).IC.mode eq 1 then begin
		widget_control, (*pstate).ic_base1, map=1, scr_ysize=(*pstate).ic_base1_ysize
	endif else begin
		widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
	endelse
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_setup_deadtime_offset, wWidget

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
		warning,'OnRealize_maia_setup_deadtime_offset',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

set_widget_text, wWidget, str_tidy((*(*pstate).pmaia).deadtime.cal.b)
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_deadtime_slope, wWidget

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
		warning,'OnRealize_maia_setup_deadtime_slope',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

set_widget_text, wWidget, str_tidy((*(*pstate).pmaia).deadtime.cal.a)
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_gain_trim_file, wWidget

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
		warning,'OnRealize_maia_gain_trim_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

set_widget_text, wWidget, (*(*pstate).pmaia).trim.file
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_gain_trim_file2, wWidget

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
		warning,'OnRealize_maia_gain_trim_file2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

set_widget_text, wWidget, (*(*pstate).pmaia).trim.file2
return
end

;------------------------------------------------------------------------------------------

pro OnRealize_maia_setup_peltier_mode, wWidget

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
		warning,'OnRealize_maia_setup_peltier_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).pmaia) then begin
	pm = (*pstate).pmaia
	bake = ((*pm).control.peltier lt -0.02)
	(*pstate).peltier_mode = bake
	widget_control, wWidget, set_combobox_select=bake
endif
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_pileup_file, wWidget

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
		warning,'OnRealize_maia_pileup_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

set_widget_text, wWidget, (*(*pstate).pmaia).pileup.file
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_hymod_cal_mode, wWidget

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
		warning,'OnRealize_maia_hymod_cal_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=(*(*pstate).pmaia).cal.mode
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_Cal_file, wWidget

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
		warning,'OnRealize_maia_cal_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

set_widget_text, wWidget, (*(*pstate).pmaia).cal.file
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_throttle_file, wWidget

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
		warning,'OnRealize_maia_throttle_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

set_widget_text, wWidget, (*(*pstate).pmaia).throttle.file
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_groups_file, wWidget

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
		warning,'OnRealize_maia_groups_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_value= (*(*pstate).pmaia).groups.file
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_DA_file, wWidget

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
		warning,'OnRealize_maia_da_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_value= (*(*pstate).pmaia).DA.file
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_detector_mode, wWidget

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
		warning,'OnRealize_maia_detector_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=(*pstate).group_mode
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_ROI_file, wWidget

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
		warning,'OnRealize_ROI_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_value= (*(*pstate).pmaia).ROI.file
return
end

;-----------------------------------------------------------------

pro OnRealize_maia_setup_clear, wWidget

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
		warning,'OnRealize_maia_clear',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

maia_setup_update_dynamic, pstate
maia_setup_groups_read, pstate
return
end

;-----------------------------------------------------------------
pro maia_setup, group_leader=group, data=data, maia=maia_pars, port=maia_port, debug=debug, $
			disable=maia_disable, readout=maia_readout, path=path, tlb=tlb, pshrmem_da=pshrmem_da, $
			pimage=pimage, ppspec=ppspec, ppgroup=ppgroup, default=default, dpath=dpath

; Setup, control and display real-time data from the Maia-384 detector array
;
; Data		a pointer (/allocate_heap) to a struct of layout parameters, else loaded from Maia_384.csv
; Maia		a pointer (/allocate_heap) to a struct of Maia detector ASIC and control parameters, else defaulted. 
; Port		a pointer (/allocate_heap) to a struct of Maia socket port parameters.
; Disable	a pointer (/allocate_heap) to an array of Maia channel disable (ECH) parameters.
; readout	a pointer (/allocate_heap) to readout struct

COMPILE_OPT STRICTARR
ErrorNo = 0
;print,'maia_setup startup ...'
common c_working_dir, geopixe_root
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if n_elements(debug) lt 1 then debug=0
catch_errors_on = 1                           ; enable error CATCHing
if debug then catch_errors_on = 0             ; disable error CATCHing

if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'Maia_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif else on_error,0
if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''
if n_elements(dpath) lt 1 then dpath=''
if n_elements(pshrmem_da) lt 1 then begin
	print,'maia_setup: pshrmem_da not defined.
	pshrmem_da = 0L
endif
if n_elements(pimage) lt 1 then  begin
	print,'maia_setup: pimage not defined.
	pimage = ptr_new()
endif
if n_elements(ppspec) lt 1 then  begin
	print,'maia_setup: ppspec not defined.
	ppspec = ptr_new()
endif
if n_elements(ppgroup) lt 1 then  begin
	print,'maia_setup: ppgroup not defined.
	ppgroup = ptr_new()
endif
if n_elements(default) eq 0 then default = maia_defaults( error=error, source='maia_setup')
n_detectors = default.detectors

;-------------------------------------------------------------------------------------

; Maia control socket parameters struct

ps = bad_pars_struct( maia_port, make_pars=make_ps)
if make_ps then begin
	print,'Open Maia control socket ...'
	*ps = open_socket( ip=default.maia.ip, port=default.maia.port, token='0', $
								enable=default.maia.enable, retries=0, error=error)
	(*ps).n_detectors = default.detectors
	(*ps).version = default.version	
	if error ne 0 then begin
;		warning,'maia_launch','Failed to open Maia control socket.'
;		return
	endif
endif
n_detectors = (*ps).n_detectors

; Default Maia control parameters struct

pm = bad_pars_struct( maia_pars, make_pars=make_pm)
if make_pm then begin
	prefix = 'Maia_384'
	file = prefix  + '.parameters.csv'
	warning,'maia_launch',['No *pm parameters found.','Loading defaults from file "'+file+'" for testing.']
	error = read_maia_parameters( file, data=pm)
	if error then begin
		warning,'maia_setup','Failed to read "'+file+'" to initialize Maia parameters.'
	endif
	file = prefix  + '.groups.csv'
	groups = read_maia_groups( file, error=err)
	if err eq 0 then begin
		(*pm).groups.file = file
		(*pm).groups.group = groups
		(*pm).groups.on = 1
	endif
endif

; Default detector layout struct parameters from file "Maia_384.csv"

letters = ['','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']
prefix = 'Maia_' + str_tidy((*pm).n_detectors) + letters[ (*pm).version.dam ]

pd = bad_pars_struct( data, make_pars=make_pd)
if make_pd then begin
	file = prefix + '.csv'
	d = read_detector_layout(file, maia=maia, error=error)
	if error then begin
		warning,'maia_launch','Failed to read "'+file+'" to initialize layout.'
		return
	endif
	if maia eq 0 then begin
		warning,'maia_launch','"'+file+'" file does not contain required extended columns.'
		return
	endif
	*pd = d
endif

; Maia disable parameters struct

pe = bad_pars_struct( maia_disable, make_pars=make_pe)
if make_pe then begin
	file = prefix  + '.enable.var'
	disable = read_maia_enable( file, index=(*pd).data.index, error=error)
	if error then begin
		warning,'maia_launch','Failed to read "'+file+'" to initialize disables.'
		disable = intarr(n_detectors)
	endif
	*pe = disable
endif

; Maia readout parameters struct

pr = bad_pars_struct( maia_readout, make_pars=make_pr)
if make_pr then begin
	readout = {	event:			1, $				; event enable
				photon:			1, $				; photon enable
				accum:			1, $				; accumulators enable
				activity:		1, $				; activity enable
				scepter:		1, $				; scepter
				RR_enable:		[1,1,1], $			; RR enable
				Quad_enable:	[1,1,1,1] }			; Quadrant enable
	*pr = readout
endif

;	Read back Maia parameters to display widgets and summary table ...
	
;print,'maia_setup initial ...'
maia_setup_initial, ps, pm, pr, pimage, pd
	
;-------------------------------------------------------------------------------------

case !version.os_family of
	'MacOS': begin
       symbol = 'SYMBOL*12'
       large_font = 'Arial*12'
       def_font = 'Geneva*10'
		fnt1 = 'HELVETICA*8'       ;'HELVETICA*9'
		csize = (n_detectors eq 384) ? 1.1 : 1.3
		help_xsize = 380
		help_xsize2 = 700
		space1 = 1
		space1b = 0
		space2 = 1
		space5 = 3
		space10 = 10
		space15 = 15

		left_xsize = 370
		left_ysize = 685
		right_xsize = 715
		right_ysize = 715
		right_xsize2 = (n_detectors eq 384) ? 705 : 600
		right_ysize2 = (n_detectors eq 384) ? 690 : 550
		text_xsize = 120
		text_xsize2 = 140
		text_xsize3 = 60
		button_xsize = 50
		table_yscroll = 32
		ptable_yscroll = 17
		list_ysize = 250
		xslide_off = 45
		charge_xsize2 = 215
		help_xsize = left_xsize+right_xsize+80
		text_lines_controls = 11
		text_lines_pulser = 10
		info_lines_imaging = 9
		end
	'unix': begin
       symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
       large_font = '10x20'
       def_font = '6x13'
		fnt1 = 'timr08'             ;'6x12'
		csize = (n_detectors eq 384) ? 1.1 : 1.3
		help_xsize = 380
		help_xsize2 = 700
		space1 = 1
		space1b = 0
		space2 = 1
		space5 = 3
		space10 = 10
		space15 = 15

		left_xsize = 370
		left_ysize = 685
		right_xsize = 715
		right_ysize = 715
		right_xsize2 = (n_detectors eq 384) ? 705 : 600
		right_ysize2 = (n_detectors eq 384) ? 690 : 550
		text_xsize = 120
		text_xsize2 = 140
		text_xsize3 = 60
		button_xsize = 50
		table_yscroll = 32
		ptable_yscroll = 17
		list_ysize = 250
		xslide_off = 45
		charge_xsize2 = 215
		help_xsize = left_xsize+right_xsize+80
		text_lines_controls = 13
		text_lines_pulser = 11
		info_lines_imaging = 9
		end
	else: begin
       symbol = 'SYMBOL*BOLD*14'
       large_font = 'COURIER*BOLD*10'
       def_font = 'Arial*14'
		fnt1 = 'Helvetica*8'       ;'ARIAL*12'
		csize = (n_detectors eq 384) ? 1.1 : 1.3
		help_xsize = 380
		help_xsize2 = 700
		space1 = 1
		space1b = 1
		space2 = 2
		space5 = 5
		space10 = 10
		space15 = 15

		left_xsize = 370
		left_ysize = 685
		right_xsize = 715
		right_ysize = 715
		right_xsize2 = (n_detectors eq 384) ? 705 : 600
		right_ysize2 = (n_detectors eq 384) ? 690 : 450
		text_xsize = 120
		text_xsize2 = 140
		text_xsize3 = 60
		button_xsize = 50
		table_yscroll = 34
		ptable_yscroll = 21
		list_ysize = 250
		xslide_off = 57
		charge_xsize2 = 215
		help_xsize = left_xsize+right_xsize+80
		text_lines_controls = 11
		text_lines_pulser = 10
		info_lines_imaging = 8
		end
endcase
;widget_control, default_font=def_font        ; set font for all windows

grey = spec_colour('l.grey')
green = spec_colour('green')
yellow = spec_colour('yellow')
lblue = spec_colour('l.blue')
red = spec_colour('red')
orange = spec_colour('orange')
dgreen = spec_colour('d.green')
blue = spec_colour('blue')
violet = spec_colour('violet')
pink = spec_colour('pink')
black = spec_colour('black')
white = spec_colour('white')

;	  select: 0     1     2     3
colours = [grey,green,violet,violet]			; for detector mimic
colours2 = [green, yellow, red]					; for warning "buttons"

if (*pm).version.scepter ge 5 then begin
	tds_times = ['19.4 us/V','9.8  us/V','4.9  us/V','2.5  us/V','1.25 us/V','0.83 us/V','0.63 us/V','0.32 us/V']
	tos_times = ['9.3  us/V','4.6  us/V','2.3  us/V','1.2  us/V','0.6  us/V','0.45 us/V','0.3  us/V','0.15 us/V']
endif else begin
	tds_times = ['64  us/V','32  us/V','16  us/V','8  us/V','4  us/V','3  us/V','2  us/V','1  us/V']
	tos_times = ['64  us/V','32  us/V','16  us/V','8  us/V','4  us/V','3  us/V','2  us/V','1  us/V']
endelse

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
	local_group = 0
endif else begin
	group = widget_base(scr_xsize=1, scr_ysize=1)
	local_group = 1
endelse
screen = get_screen_size()
if n_elements(xoffset) lt 1 then begin
	xoffset = (xoff + w) < (screen[0]- (600) > 0)
	if xoffset lt (xoff + w) then begin
		t = xoff - (600)
		if t ge 0 then xoffset=t
	endif
endif
if n_elements(yoffset) lt 1 then begin
	yoffset = ((yoff) < (screen[1]-28 - 200)) > 0
endif

; 	top-level base

;print,'maia_setup graphics ...'
tlb = widget_base( /column, title='Maia Setup', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='maia-setup-tlb', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2 ,xoffset=xoffset, $
					yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
lbase = widget_base( tbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

tab_panel = widget_tab( lbase, location=2, /align_center, uname='maia-tab-panel')	;, $
;					NOTIFY_REALIZE='OnRealize_maia_setup_preview_tab')

check_ids = lonarr(16)
class_check_ids = lonarr(5)
tab_names = ['Enable','Hermes','Scepter','Hymod','Summary','Spectra','Imaging', $
			'Controls','Layout','Pulser','Debug']

; Enable -----------------------------------------

Enable_base = widget_base( tab_panel, title='Enable', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( enable_base, value='Channel and Module Enable')
text = widget_text( enable_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='enable-explanation', /tracking, $
				value=['Select bad detectors to be disabled or HYMOD processor modules to be enabled; as a shortcut to controls on the "HYMOD" panel. See the "Data-Flow Diagram", using the tab on the far right, to view the Hymod functional diagram.'], $
				uvalue='Explanation of the role of the channel enable panel.', frame=1)

;label = widget_label( enable_base, value='  ')
enable_base1 = widget_base( enable_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( enable_base1, value='Disable Detector Channels')

text = widget_text( enable_base1, scr_xsize=left_xsize-10, ysize=8, /wrap, uname='disable-explanation', /tracking, $
				value=['Select detectors on the mimic panel to DISABLE using the HERMES channel disables (ECH). These will be coloured violet on the mimic display.  Read back DISABLE parameters using the "Read" button.','', $
				'These definitions are sent to Maia using the "Apply to HERMES" botton and saved to disk using the "Save to File" button.'], $
				uvalue='Explanation of the role of the HERMES disable panel.', frame=1)

enable_base21 = widget_base( enable_base1, /row, xpad=1, ypad=0, space=1, /align_center, /base_align_center)
button = widget_button( enable_base21, value='Read', uname='read-enable', /tracking, uvalue='Read the HERMES channel disable parameters (ECH) from Maia.')
button = widget_button( enable_base21, value='Apply to HERMES', uname='apply-hermes-enable', /tracking, uvalue='Send the Maia channel disable parameters to Maia.')
label = widget_label( enable_base21, value='  ')
button = widget_button( enable_base21, value='Load', uname='load-enable', /tracking, uvalue='Load the Maia channel enable parameters from a ".enable.var" file.')
button = widget_button( enable_base21, value='Save to File', uname='save-enable', /tracking, uvalue='Save the Maia channel disable parameters to a ".enable.var" file, and send them to Maia.')
label = widget_label( enable_base, value='  ')

enable_base2 = widget_base( enable_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( enable_base2, value='Enable Maia Clock Phases and Quadrants')
enable_base2b = widget_base( enable_base2, /row, xpad=1, ypad=1, space=40, /align_center, /base_align_top)

enable_rr_check = cw_bgroup2( enable_base2b, ['Read Request 0','Read Request 1','Read Request 2'], /column, set_value=(*pr).rr_enable, sensitive=1, $
				/return_index, uname='enable-rr-check',/ nonexclusive, /tracking, $
				uvalue='Check these boxes to enable any or all of the 3 DDM Read-Request phases for sampling data from the 3 SCEPTERS in each quadrant.', xpad=0, ypad=0, space=0)
enable_quad_check = cw_bgroup2( enable_base2b, ['Quadrant 0','Quadrant 1','Quadrant 2','Quadrant 3'], /column, set_value=(*pr).quad_enable, sensitive=1, $
				/return_index, uname='enable-quad-check',/ nonexclusive, /tracking, $
				uvalue='Check these boxes to enable any or all of the 4 DDM Quadrants of 96 detectors each. For a 96 detector array, only one Quandrant is active; or quadrant is ignored. ', xpad=0, ypad=0, space=0)

enable_base3 = widget_base( enable_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( enable_base3, value='Enable HYMOD Processing Modules')

enable_base3b = widget_base( enable_base3, /row, xpad=1, ypad=1, space=20, /align_center, /base_align_top)
module_check = [(*pm).linear.on,(*pm).trim.on, (*pm).pileup.on,(*pm).throttle.on,(*pm).DA.on,(*pm).ROI.on, (*pm).groups.on, (*pm).DA.save]
enable_modules_check = cw_bgroup2( enable_base3b, ['Linearization','Gain Trimming','Pileup Rejection','Throttle Control','Dynamic Analysis (DA)','ROI Imaging','Spectra Groups','Auto Save DA Images'], /column, set_value=module_check, sensitive=1, $
				/return_index, uname='enable-modules-check',/ nonexclusive, /tracking, $
				uvalue='Check these boxes to enable HYMOD processing modules, or use the controls on HYMOD, Spectra and Imaging panels. Use this panel for instant enabling/disabling modules; parameters need to be set-up on the other panels first (HYMOD, Spectra and Imaging). ' + $
				'Normally, "Linearization", "Trimming" and "Pileup rejection" would be set-up before experiments and enabled. View the HYMOD processing layout using the "Data-Flow Diagram" tab on the far right.', xpad=0, ypad=0, space=0)
enables_check = [(*pr).scepter, (*pr).event, (*pr).photon, (*pr).accum, (*pr).activity, (*pm).deadtime.on, (*pm).scan.on, (*pm).chart.on]
enable_stream_check = cw_bgroup2( enable_base3b, ['Scepter','Events','Photons','Accumlators','Activity','Dead-time','Pixel','Chart logging'], /column, set_value=enables_check, sensitive=1, $
				/return_index, uname='enable-stream-check',/ nonexclusive, /tracking, $
				uvalue='Check these boxes to enable various data flow switches in HYMOD for: (i) event flow (photon, events), (ii) accumulators (for activity, DA, region spectra, dead-time, etc.) (activity, accumulators, dead-time), low level hardware enable (Scepter) and chart logging.', xpad=0, ypad=0, space=0)

; Hermes -----------------------------------------

Hermes_base = widget_base( tab_panel, title='HERMES', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( hermes_base, value='HERMES ASIC Controls')

text = widget_text( hermes_base, scr_xsize=left_xsize, ysize=19, /wrap, uname='hermes-explanation', /tracking, $
				value=['Set operating parameters for the HERMES front-end ASICs, which provide pre-amplification and quasi-Gaussian pulse shaping to each of the 384 Maia detector channels.', $
					'','Select detector channels to set using the mimic panel on the right, selecting channels in groups defined using the droplist on the bottom right. Note that most parameters can only be set at the Chip level, or at Quadrant and All of Maia level.', $
					'','Select a parameter to change using the check-boxes, and click on "Apply to HERMES" to update the chosen parameter in Maia, for the selected detector channels.', $
					'','Show the current values of settings using the "Show Parameters" mode for the mimic panel by checking the check mark against the parameter of interest. Uniform values for all shows as all black.'], $
				uvalue='Explanation of the role of the Hermes panel.', frame=1)

;hermes_base2 = widget_base( hermes_base, /column, xpad=0, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
;
;cid = 0
;hbase01 = widget_base( hermes_base2, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
;check = cw_bgroup2( hbase01, [''], /row, set_value=[0], sensitive=1, $
;				/return_index, uname='hermes-channel-check',/ nonexclusive, /tracking, $
;				uvalue='Check this box to select this group of parameters for modification and transmission to Maia. Select channels on the mimic display on the right. ' + $
;					'Make sure the radio button at the bottom of the window is in "Set Parameters" mode.', xpad=0, ypad=0, space=0)
;class_check_ids[cid] = check
;label = widget_label( hbase01, value='Parameters for Individual HERMES channels')
;
;id = 3
;hbase4 = widget_base( hermes_base2, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
;label = widget_label( hbase4, value='HERMES Leakage current monitor:')
;hermes_elk_mode = widget_combobox(hbase4, uname='hermes-elk-mode', scr_xsize=text_xsize, sensitive=0, $
;;			NOTIFY_REALIZE='OnRealize_evt_station', $
;			value=['Disabled','Enabled'], /tracking, uvalue={id:id, help:'Select one HERMES detector channel to monitor leakage current (elk).  Normally, one HERMES channels is monitored and readout in the OAN/ELK ADC field on the Controls panel. Make sure OAN is NOT set on the Debug panel.'})
;check = cw_bgroup2( hbase4, [''], /row, set_value=[0], sensitive=1, $
;				/return_index, uname='hermes-elk-check',/ nonexclusive, /tracking, $
;				uvalue={label:'Hermes Leakage monitor',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to HERMES" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
;					'In "Set Parameters" mode, select channels on the mimic display on the right. '}, xpad=0, ypad=0, space=0)
;check_ids[id] = check
hermes_elk_mode = 0L

hermes_base1 = widget_base( hermes_base, /column, xpad=0, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)

cid = 1
hbase02 = widget_base( hermes_base1, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
check = cw_bgroup2( hbase02, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='hermes-chip-check',/ nonexclusive, /tracking, $
				uvalue='Check this box to select this group of parameters for modification and transmission to Maia. Select channels on the mimic display on the right. ' + $
					'Make sure the radio button at the bottom of the window is in "Set Parameters" mode.', xpad=0, ypad=0, space=0)
class_check_ids[cid] = check
label = widget_label( hbase02, value='Parameters for selected HERMES chips')

id = 0
hbase1 = widget_base( hermes_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hbase1, value='Peaking time:')
hermes_time_mode = widget_combobox(hbase1, uname='hermes-time-mode', scr_xsize=text_xsize, sensitive=0, $
;			NOTIFY_REALIZE='OnRealize_evt_station', $
			value=['1.0  us','0.5  us','4.0  us','2.0  us'], /tracking, uvalue={id:id,help:'Select the HERMES peaking time (time) for the selected detector(s). Normally, all HERMES channels are set to the same Peaking Time.'})
check = cw_bgroup2( hbase1, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='hermes-time-check',/ nonexclusive, /tracking, $
				uvalue={label:'Hermes Peaking Time',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to HERMES" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

id = 1
hbase2 = widget_base( hermes_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hbase2, value='Gain:')
hermes_gain_mode = widget_combobox(hbase2, uname='hermes-gain-mode', scr_xsize=text_xsize, sensitive=0, $
;			NOTIFY_REALIZE='OnRealize_evt_station', $
			value=['1500  mV/fC','750  mV/fC'], /tracking, uvalue={id:id, help:'Select the HERMES gain (gain) for the selected detector(s).  Normally, all HERMES channels are set to the same Gain. Maia typically uses Gain=1500.'})
check = cw_bgroup2( hbase2, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='hermes-gain-check',/ nonexclusive, /tracking, $
				uvalue={label:'Hermes Gain',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to HERMES" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

id = 2
hbase3 = widget_base( hermes_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hbase3, value='Input bias leakage control:')
hermes_eblk_mode = widget_combobox(hbase3, uname='hermes-eblk-mode', scr_xsize=text_xsize, sensitive=0, $
;			NOTIFY_REALIZE='OnRealize_evt_station', $
			value=['Disabled','Enabled'], /tracking, uvalue={id:id, help:'Select the HERMES internal bias leakage control mode (eblk) for the selected detector(s).  Normally, internal bias leakage is disabled on all HERMES channels. This is only enabled to test Maia without a detector wafer installed.'})
check = cw_bgroup2( hbase3, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='hermes-eblk-check',/ nonexclusive, /tracking, $
				uvalue={label:'Hermes Bias Leakage',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to HERMES" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

hermes_base3 = widget_base( hermes_base, /column, xpad=0, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)

cid = 2
hbase03 = widget_base( hermes_base3, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
check = cw_bgroup2( hbase03, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='hermes-whole-check',/ nonexclusive, /tracking, $
				uvalue='Check this box to select this group of parameters for modification and transmission to Maia. Select channels on the mimic display on the right. ' + $
					'Make sure the radio button at the bottom of the window is in "Set Parameters" mode.', xpad=0, ypad=0, space=0)
class_check_ids[cid] = check
label = widget_label( hbase03, value='Global Parameters for whole of Maia')

id = 11
sbase7 = widget_base( hermes_base3, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase7, value='Read-Request Clock (MHz):')
scepter_rr_slider = cw_fslider2( sbase7, format='(F5.1)', minimum=0.1, maximum=30.0, layout=1, scroll=0.1, sensitive=0, $
				value=3.0, uname='scepter-rr-slider', xsize=text_xsize-45, /tracking, /edit, /drag, xpad=0, ypad=0, $
				uvalue={id:id, help:'Adjust the Read-Request Clock frequency (MHz) for the selected quadrant(s). Normally, all SCEPTER chips are set to the same RR clock. The typical RR clock is 3 MHz.'})
check = cw_bgroup2( sbase7, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-rr-check',/ nonexclusive, /tracking, $
				uvalue={label:'Quadrant RR Clock',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to HERMES" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

check_ids_hermes_channel = check_ids[3]
check_ids_hermes_chip = check_ids[ [0,1,2] ]
check_ids_hermes_whole = check_ids[11]

ids_hermes_channel = [hermes_elk_mode]
ids_hermes_chip = [hermes_time_mode, hermes_gain_mode, hermes_eblk_mode]
ids_hermes_whole = [scepter_rr_slider]
ids_hermes = [ids_hermes_channel, ids_hermes_chip, ids_hermes_whole]

hbase8 = widget_base( Hermes_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hbase8, value='HERMES version:')
hermes_version_text = widget_text( hbase8, value=str_tidy((*pm).version.hermes), uname='hermes-version-text', /tracking, $
					uvalue='Shows the HERMES ASIC hardware revision number.', scr_xsize=50)
label = widget_label( hbase8, value=' ', scr_xsize=90)
label = widget_label( hbase8, value='Set from:')
hermes_detector_text = widget_text( hbase8, value='', uname='hermes-set-from-text1', /tracking, $
					uvalue='Shows the detector # that the parameters above have been set from.', scr_xsize=50)
hermes_parameter_text = 0L

hermes_apply_button = widget_button( hermes_base, value='Apply to HERMES', uname='hermes-apply', /tracking, uvalue='Apply the selected settings (set using check-boxes) to the selected HERMES channels in Maia. Normally, all channels ' + $
				'of all HERMES chips are set together so that all channels have the same characteristics.')

label = widget_label( hermes_base, value=' ')
hbase5 = widget_base( hermes_base, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
;label = widget_label( hbase5, value='ASIC Reset and refresh:')
;button = widget_button( hbase5, value='tokreset', uname='tokreset', /tracking, $
;				uvalue='Reset and then refresh all chips.', scr_xsize=text_xsize)


; Scepter -----------------------------------------

Scepter_base = widget_base( tab_panel, title='SCEPTER', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( scepter_base, value='SCEPTER ASIC Controls')

text = widget_text( scepter_base, scr_xsize=left_xsize, ysize=17, /wrap, uname='scepter-explanation', /tracking, $
				value=['Set operating parameters for the SCEPTER peak-detect ASICs, which sample pulse amplitude (E) and time-over-threshold (T) for each of the Maia detector channels.', $
					'','Select detector channels using the mimic panel on the right, selecting channels in groups. Note that most parameters can only be set at the Chip level, or at Quadrant and All of Maia level.', $
					'','Select a parameter to change using the check-boxes, and click on "Apply to SCEPTER" to update the chosen parameter in Maia, for the selected detector channels.', $
					'','Show the current values of settings using the "Show Parameters" mode by checking the check mark against the parameter of interest.'], $
				uvalue='Explanation of the role of the Scepter panel.', frame=1)

if (*pm).version.scepter ge 6 then begin
	scepter_base0 = widget_base( Scepter_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)

	cid = 3
	sbase01 = widget_base( scepter_base0, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
	check = cw_bgroup2( sbase01, [''], /row, set_value=[0], sensitive=1, $
					/return_index, uname='scepter-channel-check',/ nonexclusive, /tracking, $
					uvalue='Check this box to select this group of parameters for modification and transmission to Maia. Select channels on the mimic display on the right. ' + $
						'Make sure the radio button at the bottom of the window is in "Set Parameters" mode.', xpad=0, ypad=0, space=0)
	class_check_ids[cid] = check
	label = widget_label( sbase01, value='Parameters for Individual SCEPTER channels')

	id = 9
	sbase6b = widget_base( Scepter_base0, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
	arrow = picture_button( sbase6b, geopixe_root + 'images/up-16x14.jpeg', uname='scepter-trim-up', $
			/tracking, uvalue='Adjust selected channel Trim values: Increment all by 0.01V, but maintain relativity between channels. Select channels to adjust on detector mimic to right. Click "Apply to SCEPTER" to send changes to Maia.', /pushbutton_events)
	arrow = picture_button( sbase6b, geopixe_root + 'images/down-16x14.jpeg', uname='scepter-trim-down', $
			/tracking, uvalue='Adjust selected channel Trim values: Decrement all by 0.01V, but maintain relativity between channels. Select channels to adjust on detector mimic to right. Click "Apply to SCEPTER" to send changes to Maia.', /pushbutton_events)
	label = widget_label( sbase6b, value='   Trim DAC (V):')
	scepter_trim_slider = cw_fslider2( sbase6b, format='(F6.3)', minimum=0.0, maximum=0.15, layout=1, scroll=0.005, sensitive=0, $
					value=0.0, uname='scepter-trim-slider', xsize=text_xsize-50, /tracking, /edit, /drag, xpad=0, ypad=0, $
					uvalue={id:id, help:'Adjust the SCEPTER Threshold Trim DAC (0 - 0.15 Volts) for the selected SCEPTER channel(s). TRIM is subtracted from Thresh. Normally, all SCEPTER channels are set to the same Threshold DAC, and individual channels are adjusted using the Threshold Trim DACs.'})
	check = cw_bgroup2( sbase6b, [''], /row, set_value=[0], sensitive=1, $
					/return_index, uname='scepter-trim-check',/ nonexclusive, /tracking, $
					uvalue={label:'Scepter Trim DAC (V)',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
	check_ids[id] = check

	id = 15
	sbase6b2 = widget_base( Scepter_base0, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
	label = widget_label( sbase6b2, value='Overall threshold: Thresh - Trim (V):')
	check = cw_bgroup2( sbase6b2, [''], /row, set_value=[0], sensitive=1, $
					/return_index, uname='scepter-thresh-trim-check',/ nonexclusive, /tracking, $
					uvalue={label:'Thresh - Trim (V)',help:'In "Set Parameters" mode, this box is not used. In "Show Parameters" mode, check this box to display parameter values on the mimic display.'}, xpad=0, ypad=0, space=0)
	check_ids[id] = check

endif else scepter_trim_slider = 0L

scepter_base1 = widget_base( Scepter_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)

cid = 4
sbase02 = widget_base( scepter_base1, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
check = cw_bgroup2( sbase02, [''], /row, set_value=[0], sensitive=1, $
					/return_index, uname='scepter-chip-check',/ nonexclusive, /tracking, $
					uvalue='Check this box to select this group of parameters for modification and transmission to Maia. Select channels on the mimic display on the right. ' + $
						'Make sure the radio button at the bottom of the window is in "Set Parameters" mode.', xpad=0, ypad=0, space=0)
class_check_ids[cid] = check
label = widget_label( sbase02, value='Parameters for selected SCEPTER chips')

id = 4
sbase1 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase1, value='Timing mode:')
scepter_tdm_mode = widget_combobox(sbase1, uname='scepter-tdm-mode', scr_xsize=text_xsize, sensitive=0, $
;			NOTIFY_REALIZE='OnRealize_evt_station', $
			value=['Time-of-Occurence','Rise-time','Fall-time','Time-over-Threshold'], /tracking, uvalue={id:id, help:'Select the SCEPTER TAC timing mode (tdm) for the selected detector(s). Normally, all SCEPTER channels are set to the same timing mode. Maia uses tdm "Time-over-Threshold".'})
check = cw_bgroup2( sbase1, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-tdm-check',/ nonexclusive, /tracking, $
				uvalue={label:'Scepter Time Mode',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

id = 5
sbase2 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase2, value='TAC slope:')
scepter_tds_mode = widget_combobox(sbase2, uname='scepter-tds-mode', scr_xsize=text_xsize, sensitive=0, $
;			NOTIFY_REALIZE='OnRealize_evt_station', $
			value=tds_times, /tracking, uvalue={id:id, help:'Select the SCEPTER time to analogue converter slope (tds) for the selected detector. Normally, all SCEPTER channels are set to the same TAC slope. Maia typically uses tds maximum (64). NOTE: This setting will affect the DT Calibration (see HYMOD DTcalA and DTcalB).'})
check = cw_bgroup2( sbase2, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-tds-check',/ nonexclusive, /tracking, $
				uvalue={label:'Scepter TAC Slope',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

id = 6
sbase3 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase3, value='TAC timeout:')
scepter_tos_mode = widget_combobox(sbase3, uname='scepter-tos-mode', scr_xsize=text_xsize, sensitive=0, $
;			NOTIFY_REALIZE='OnRealize_evt_station', $
			value=tos_times, /tracking, uvalue={id:id, help:'Select the SCEPTER TAC timeout (tos) for the selected detector. Normally, all SCEPTER channels are set to the same timeout. Maia typically uses tos maximum (64).'})
check = cw_bgroup2( sbase3, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-tos-check',/ nonexclusive, /tracking, $
				uvalue={label:'Scepter TAC Timeout',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

id = 7
sbase4 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase4, value='Simultaneous event catching:')
scepter_trk_mode = widget_combobox(sbase4, uname='scepter-trk-mode', scr_xsize=text_xsize, sensitive=0, $
;			NOTIFY_REALIZE='OnRealize_evt_station', $
			value=['Off','On'], /tracking, uvalue={id:id, help:'Select the SCEPTER simultaneous events catching mode (trk) for the selected detector. Normally, all SCEPTER channels are set to the same mode. Maia typically uses trk On.'})
check = cw_bgroup2( sbase4, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-trk-check',/ nonexclusive, /tracking, $
				uvalue={label:'Scepter Simult. Catching',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

id = 8
sbase5 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase5, value='Enhanced Simult. event catching:')
scepter_trke_mode = widget_combobox(sbase5, uname='scepter-trke-mode', scr_xsize=text_xsize, sensitive=0, $
;			NOTIFY_REALIZE='OnRealize_evt_station', $
			value=['Off','On'], /tracking, uvalue={id:id, help:'Select the SCEPTER enhanced simultaneous events catching mode (trke) for the selected detector. Normally, all SCEPTER channels are set to the same mode. Maia typically uses trke On.'})
check = cw_bgroup2( sbase5, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-trke-check',/ nonexclusive, /tracking, xpad=0, ypad=0, space=0, $
				uvalue={label:'Scepter New Simult. Catching',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'})
check_ids[id] = check

if (*pm).version.scepter ge 6 then begin
	id = 13
	sbase10 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
	label = widget_label( sbase10, value='Added input filtering:')
	scepter_filt_mode = widget_combobox(sbase10, uname='scepter-filt-mode', scr_xsize=text_xsize, sensitive=0, $
	;			NOTIFY_REALIZE='OnRealize_evt_station', $
				value=['Off','On'], /tracking, uvalue={id:id, help:'Enable added input filtering (filtena) in newer SCEPTER chips.'})
	check = cw_bgroup2( sbase10, [''], /row, set_value=[0], sensitive=1, $
					/return_index, uname='scepter-filt-check',/ nonexclusive, /tracking, xpad=0, ypad=0, space=0, $
					uvalue={label:'Scepter Input Filter',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'})
	check_ids[id] = check

	id = 14
	sbase11 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
	label = widget_label( sbase11, value='Comparator multi-fire suppression:')
	scepter_tcm_mode = widget_combobox(sbase11, uname='scepter-tcm-mode', scr_xsize=text_xsize, sensitive=0, $
	;			NOTIFY_REALIZE='OnRealize_evt_station', $
				value=['0 ns','100 ns','1 us','2 us'], /tracking, uvalue={id:id, help:'SCEPTER comparator multi-fire suppression (tcm) (0=0, 1=100ns, 2=1us, 3=2us).'})
	check = cw_bgroup2( sbase11, [''], /row, set_value=[0], sensitive=1, $
					/return_index, uname='scepter-tcm-check',/ nonexclusive, /tracking, xpad=0, ypad=0, space=0, $
					uvalue={label:'Scepter Glitch Suppress',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'})
	check_ids[id] = check
endif else begin
	scepter_filt_mode = 0L
	scepter_tcm_mode = 0L
endelse

id = 10
sbase6 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
arrow = picture_button( sbase6, geopixe_root + 'images/up-16x14.jpeg', uname='scepter-thresh-up', $
			/tracking, uvalue='Adjust selected channel Threshold values: Increment all by 0.01V, but maintain relativity between chips. Select chips to adjust on detector mimic to right. Click "Apply to SCEPTER" to send changes to Maia.', /pushbutton_events)
arrow = picture_button( sbase6, geopixe_root + 'images/down-16x14.jpeg', uname='scepter-thresh-down', $
			/tracking, uvalue='Adjust selected channel Threshold values: Decrement all by 0.01V, but maintain relativity between chips. Select chips to adjust on detector mimic to right. Click "Apply to SCEPTER" to send changes to Maia.', /pushbutton_events)
label = widget_label( sbase6, value='   Threshold DAC (V):')
scepter_thresh_slider = cw_fslider2( sbase6, format='(F6.3)', minimum=0.005, maximum=2.0, layout=1, scroll=0.005, sensitive=0, $
				value=0.5, uname='scepter-thresh-slider', xsize=text_xsize-50, /tracking, /edit, /drag, xpad=0, ypad=0, $
				uvalue={id:id, help:'Set the main SCEPTER Threshold DAC (Volts) (thresh) for the selected SCEPTER(s). Normally, all SCEPTER channels are set to the same Threshold DAC, and individual channels are adjusted using the Threshold Trim DACs (Scepter 7 only) and each Scepter is adjusted using the THPD adjustment DACs (Scepter 7 only). '})
check = cw_bgroup2( sbase6, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-thresh-check',/ nonexclusive, /tracking, $
				uvalue={label:'Scepter Threshold (V)',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

if (*pm).version.scepter ge 7 then begin
	id = 12
	sbase9 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
	label = widget_label( sbase9, value='Threshold Adjust DAC (V):')
	scepter_thpd_slider = cw_fslider2( sbase9, format='(F6.3)', minimum=0.0, maximum=0.15, layout=1, scroll=0.005, sensitive=0, $
					value=0.0, uname='scepter-thpd-slider', xsize=text_xsize-50, /tracking, /edit, /drag, xpad=0, ypad=0, $
					uvalue={id:id, help:'Small adjustment to the SCEPTER Threshold DAC (0 - 0.15 Volts) (thpd) to match SCEPTER(s). THPD is subtracted from Thresh. Normally, all SCEPTER channels are set to the same Threshold DAC, and this is used to match offsets between SCEPTERs. '})
	check = cw_bgroup2( sbase9, [''], /row, set_value=[0], sensitive=1, $
					/return_index, uname='scepter-thpd-check',/ nonexclusive, /tracking, $
					uvalue={label:'Scepter Thresh adjust (V)',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to Maia when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
	check_ids[id] = check
endif else scepter_thpd_slider=0L

check_ids_scepter_channel = check_ids[ [9,15] ]
check_ids_scepter_chip = check_ids[ [4,5,6,7,8,13,14,10,12] ]

ids_scepter_channel = [scepter_trim_slider]
ids_scepter_chip = [scepter_tdm_mode, scepter_tds_mode, scepter_tos_mode, scepter_trk_mode, scepter_trke_mode, scepter_filt_mode, scepter_tcm_mode, scepter_thresh_slider, scepter_thpd_slider]
ids_scepter = [ids_scepter_channel, ids_scepter_chip]

sbase8 = widget_base( Scepter_base, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase8, value='SCEPTER version:')
scepter_version_text = widget_text( sbase8, value=str_tidy((*pm).version.scepter), uname='scepter-version-text', /tracking, $
					uvalue='Shows the SCEPTER ASIC hardware revision number.', scr_xsize=50)
label = widget_label( sbase8, value=' ', scr_xsize=90)
label = widget_label( sbase8, value='Set from:')
scepter_detector_text = widget_text( sbase8, value='', uname='scepter-set-from-text1', /tracking, $
					uvalue='Shows the detector # that the parameters above have been set from.', scr_xsize=50)
scepter_parameter_text = 0L

scepter_apply_button = widget_button( scepter_base, value='Apply to SCEPTER', uname='scepter-apply', /tracking, uvalue='Apply the selected settings (set using check-boxes) to the selected SCEPTER channels in Maia. Normally, all channels ' + $
				'of all SCEPTER chips are set together so that all channels have the same characteristics. One exception may be the Threshold DAC.')

; Hymod -----------------------------------------

Hymod_base = widget_base( tab_panel, title='HYMOD', /column, xpad=1, ypad=1, space=space5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( hymod_base, value='HYMOD Processor Controls')

text = widget_text( hymod_base, scr_xsize=left_xsize, ysize=6, /wrap, uname='hymod-explanation', /tracking, $
				value=['Linearization, Gain Trimming and Pileup are set-up to characterize the detector. Throttle will vary for each experiment.', $
						'','Parameter files for these processing modules are selected here and loaded using "Apply to HYMOD".'], $
				uvalue='Explanation of the role of the Hymod panel.', frame=1)

if !version.os_family ne 'unix' then label = widget_label( hymod_base, value='')
label = widget_label( hymod_base, value='Detector Characterization and Correction')
hymod_base3 = widget_base( hymod_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( hymod_base3, value='Gain Linearization')

hybase31 = widget_base( hymod_base3, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hybase31, value='Linearization:')
hymod_linearization_text = widget_text( hybase31, value='', uname='linearization-file', /tracking, editable=0, $
					NOTIFY_REALIZE='OnRealize_maia_setup_linearization_file', $
					uvalue='Select the file of "Linearization" lookup table entries used to remove small non-linearities in channel gain, or use the "Load" button. Hit <return> to load the file.', scr_xsize=text_xsize2)
button = widget_button( hybase31, value='Load', uname='load-linearization', /tracking, uvalue='Select and load the file of "Linearization" lookup table entries used to remove small non-linearities in channel gain.')
label = widget_label( hybase31, value=' ')
hymod_linearization_check = cw_bgroup2( hybase31, ['Send'], /row, set_value=[0], sensitive=1, $
				/return_index, uname='hymod-linearization-check',/ nonexclusive, /tracking, $
				uvalue={label:'Linearization',help:'Check this box to send gain "Linearization" mode to remove small non-linearities in channel gain using a non-linear function lookup table. ' + $
					'Enable/disable "linearization" on the Enable tab. Disable it only during initial detector set-up in order to collect data (using the pulser "step-cal" procedure) with the aim of characterizing linearity'}, xpad=0, ypad=0, space=0)

hymod_base7 = widget_base( hymod_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( hymod_base7, value='Gain Trimming')

hybase71a = widget_base( hymod_base7, /column, xpad=0, ypad=0, space=1, /align_right, /base_align_right)
hybase71 = widget_base( hybase71a, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hybase71, value='Gain Trim E File:')
hymod_gain_trim_text = widget_text( hybase71, value='', uname='gain-trim-file', /tracking, editable=0, $
					NOTIFY_REALIZE='OnRealize_maia_setup_gain_trim_file', $
					uvalue='Select the file of "Gain Trim" correction coefficients needed to map all detector E and T onto common amplitudes, or use the "Load" button. Hit <return> to load the file.', scr_xsize=text_xsize2)
button = widget_button( hybase71, value='Load', uname='load-gain-trim', /tracking, uvalue='Select and load the file of "Gain Trim" correction coefficients needed to map all detector E and T onto common amplitudes.')
label = widget_label( hybase71, value=' ')
hymod_gain_trim_check = cw_bgroup2( hybase71, ['Send'], /row, set_value=[0], sensitive=1, $
				/return_index, uname='hymod-gain-trim-check',/ nonexclusive, /tracking, $
				uvalue={label:'Gain Trim',help:'Check this box to send "Gain Trim" correction used to map all detector E and T onto common amplitudes. Enable/disable "gaintrim" on the Enable tab. ' + $
					'Disable it only during initial detector set-up in order to collect data (using the pulser "gain-trim" procedure) to characterize channel differences to build gain trimming corrections.'}, xpad=0, ypad=0, space=0)

hybase71b = widget_base( hybase71a, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hybase71b, value='Gain Trim T File:')
hymod_gain_trim_text2 = widget_text( hybase71b, value='', uname='gain-trim-file2', /tracking, editable=0, $
					NOTIFY_REALIZE='OnRealize_maia_setup_gain_trim_file2', $
					uvalue='Select another file of "Gain Trim" correction coefficients needed to map all detector E and T onto common amplitudes, or use the "Load" button. Hit <return> to load the file.', scr_xsize=text_xsize2)
button = widget_button( hybase71b, value='Load', uname='load-gain-trim2', /tracking, uvalue='Select and load another file of "Gain Trim" correction coefficients needed to map all detector E and T onto common amplitudes.')
label = widget_label( hybase71b, value=' ')
dummy = cw_bgroup2( hybase71b, ['Send'], /row, set_value=[0], sensitive=0, map=0, $
				/return_index, uname='hymod-gain-trim-check-dummy',/ nonexclusive, /tracking, $
				uvalue={label:'Gain Trim',help:'Check ...'}, xpad=0, ypad=0, space=0)
hybase72 = widget_base( hybase71a, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
label = widget_label( hybase72, value='Adj T Offsets:')
arrow = picture_button( hybase72, geopixe_root + 'images/up-16x14.jpeg', uname='hymod-gaintrim-T-off-up', $
				/tracking, uvalue='Adjust selected T Gain-Trim offset values (e.g. after Scepter Threshold changes): Increment selected channel T gain-trim offsets by +10. ' + $
				'Select channels/chips to be adjusted on detector mimic to right. Monitor T changes in position of the E-T curve relative to the pileup field on the ET2D display, or using the T spectra. ' + $
				'Hit "Apply" to send changes to Maia, and "Save" to save a new T gaintrim file.', /pushbutton_events)
arrow = picture_button( hybase72, geopixe_root + 'images/down-16x14.jpeg', uname='hymod-gaintrim-T-off-down', $
				/tracking, uvalue='Adjust selected T Gain-Trim offset values (e.g. after Scepter Threshold changes): Decrement selected channel T gain-trim offsets by -10. ' + $
				'Select channels/chips to be adjusted on detector mimic to right. Monitor T changes in position of the E-T curve relative to the pileup field on the ET2D display, or using the T spectra. ' + $
				'Hit "Apply" to send changes to Maia, and "Save" to save a new T gaintrim file.', /pushbutton_events)
label = widget_label( hybase72, value=' ')
label = widget_label( hybase72, value='T Gains:')
arrow = picture_button( hybase72, geopixe_root + 'images/up-16x14.jpeg', uname='hymod-gaintrim-T-gain-up', $
				/tracking, uvalue='Adjust selected T Gain-Trim gain values: Increment selected channel T gain-trim gain by +2%. ' + $
				'Select channels/chips to be adjusted on detector mimic to right. Monitor T changes in position of the E-T curve relative to the pileup field on the ET2D display, or using the T spectra. ' + $
				'Hit "Apply" to send changes to Maia, and "Save" to save a new T gaintrim file.', /pushbutton_events)
arrow = picture_button( hybase72, geopixe_root + 'images/down-16x14.jpeg', uname='hymod-gaintrim-T-gain-down', $
				/tracking, uvalue='Adjust selected T Gain-Trim gain values: Decrement selected channel T gain-trim gains by -2%. ' + $
				'Select channels/chips to be adjusted on detector mimic to right. Monitor T changes in position of the E-T curve relative to the pileup field on the ET2D display, or using the T spectra. ' + $
				'Hit "Apply" to send changes to Maia, and "Save" to save a new T gaintrim file.', /pushbutton_events)
label = widget_label( hybase72, value='  ')
button = widget_button( hybase72, value='Apply', uname='hymod-apply-gaintrim-adj', /tracking, uvalue='Apply T gain-trim adjustments (adjusted using the arrow buttons) to the Maia HYMOD processor.  ' + $
			'Monitor T changes in position of the E-T curve relative to the pileup field on the ET2D display, or using the T spectra.')
button = widget_button( hybase72, value='Save', uname='hymod-save-gaintrim', /tracking, uvalue='Save final Time gain-trim parameters to a new ".gaintrim.time.var" file. The file-name for "Gain Trim T" will change to reflect this. ' + $
			'Remember to save all Maia Parameters too.')

hymod_base2 = widget_base( hymod_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( hymod_base2, value='Pileup Rejection')

hybase21 = widget_base( hymod_base2, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hybase21, value='Pileup File:')
hymod_pileup_text = widget_text( hybase21, value='', uname='pileup-file', /tracking, editable=0, $
					NOTIFY_REALIZE='OnRealize_maia_setup_pileup_file', $
					uvalue='Select the file of "Pileup" low,high "T" limits for each "E", or use the "Load" button. Hit <return> to load the file.', scr_xsize=text_xsize2)
button = widget_button( hybase21, value='Load', uname='load-pileup', /tracking, uvalue='Select and load the file of "Pileup" low,high "T" limits for each "E".')
label = widget_label( hybase21, value=' ')
hymod_pileup_check = cw_bgroup2( hybase21, ['Send'], /row, set_value=[0], sensitive=1, $
				/return_index, uname='hymod-pileup-check',/ nonexclusive, /tracking, $
				uvalue={label:'Pileup',help:'Check this box to send "Pileup" mode to reject events with "T" values outside the low,high limits defined for each "E". Enable/disable "pileup" on the Enable tab. '}, xpad=0, ypad=0, space=0)

hymod_base6 = widget_base( hymod_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( hymod_base6, value='Energy Calibration')

hybase61 = widget_base( hymod_base6, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hybase61, value='Energy Cal Spec:')
hymod_cal_text = widget_text( hybase61, value='', uname='cal-file', /tracking, editable=0, $
					NOTIFY_REALIZE='OnRealize_maia_setup_Cal_file', $
					uvalue='Select the GeoPIXE SPEC file with an energy calibration for every detector channel needed for both DA and ROI mapping, or use the "Load" button. Hit <return> to load the file. ' + $
					'Use the droplist below to select how energy calibrations are set for Maia detector channels between ' + $
					'(i) using a SPEC file that provides a separate energy calibration for each detector channel (e.g. fit all detector channel spectra to determine calibration), or ' + $
					'(ii) using a SPEC file to provide a single energy calibration for all detector channels (e.g. after accurate gain-trimming has been accomplished).', scr_xsize=text_xsize2+55)
button = widget_button( hybase61, value='Load', uname='load-cal', /tracking, uvalue='Select and load the GeoPIXE Spec file with an energy calibration for every detector channel needed for both DA and ROI mapping. ' + $
					'Use the droplist below to select how energy calibrations are set for Maia detector channels between ' + $
					'(i) using a SPEC file that provides a separate energy calibration for each detector channel (e.g. fit all detector channel spectra to determine calibration), or ' + $
					'(ii) using a SPEC file to provide a single energy calibration for all detector channels (e.g. after accurate gain-trimming has been accomplished).')

hymod_cal_mode = widget_combobox(hymod_base6, uname='hymod-cal-mode', scr_xsize=2*text_xsize, $
			NOTIFY_REALIZE='OnRealize_maia_setup_hymod_cal_mode', $
			value=['  Individual energy calibrations','  Apply a single calibration to all'], /tracking, uvalue='Select how energy calibrations are set for Maia detector channels between ' + $
					'(i) using a SPEC file that provides a separate energy calibration for each detector channel (e.g. fit all detector channel spectra to determine calibration), or ' + $
					'(ii) using a SPEC file to provide a single energy calibration for all detector channels (e.g. after accurate gain-trimming has been accomplished).')

hymod_base8 = widget_base( hymod_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( hymod_base8, value='Dead-Time Calibration')

hybase81 = widget_base( hymod_base8, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hybase81, value='DTcal A:')
hymod_deadtime_slope = widget_text( hybase81, value='', uname='deadtime-slope', /tracking, editable=1, $
					NOTIFY_REALIZE='OnRealize_maia_setup_deadtime_slope', $
					uvalue='Select the Dead-time calibration "slope" A parameter (ns per Time-over-Threshold count). Note that this DT calibration will be affected by the SCEPTER TAC Slope setting and SCEPTER version. ' + $
					'For SCEPTER v4 in 384A, TDS = 0,1,2 corresponds to 64, 32, 16 us/V. For SCEPTER v7 in 384C, TDS = 0,1,2 corresponds to 19.4, 9.8, 4.9 us/V. Set "auto" to have this handled automatically.', scr_xsize=text_xsize-50)
label = widget_label( hybase81, value='  ')
label = widget_label( hybase81, value='DTcal B:')
hymod_deadtime_offset = widget_text( hybase81, value='', uname='deadtime-offset', /tracking, editable=1, $
					NOTIFY_REALIZE='OnRealize_maia_setup_deadtime_offset', $
					uvalue='Select the Dead-time calibration "offset" B parameter (ns). Note that this DT calibration will be affected by the SCEPTER TAC Slope setting.', scr_xsize=text_xsize-50)
label = widget_label( hybase81, value='  ')
hymod_deadtime_auto = cw_bgroup2( hybase81, ['Auto'], /row, set_value=[0], sensitive=1, $
				/return_index, uname='hymod-deadtime-auto',/ nonexclusive, /tracking, $
				uvalue={label:'Deadtime',help:'Check this box to enable auto setting of DTcalA when Scepter TDS (TAC slope) is changed. ' + $
					'Note that all Scepter TDS values (for all chips) should be the same.'}, xpad=0, ypad=0, space=0)

;label = widget_label( hymod_base, value='  ')
label = widget_label( hymod_base, value='Setup Options Specific to your Experimental Data')
hymod_base1 = widget_base( hymod_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( hymod_base1, value='Throttle Control')

hybase1 = widget_base( hymod_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hybase1, value='Throttle File:')
hymod_throttle_text = widget_text( hybase1, value='', uname='throttle-file', /tracking, editable=0, $
					NOTIFY_REALIZE='OnRealize_maia_setup_throttle_file', $
					uvalue='Select the file of "Throttle" scaling factors used to reduce data rates for high count rate situations, or use the "Load" button. Hit <return> to load the file.', scr_xsize=text_xsize2)
button = widget_button( hybase1, value='Load', uname='load-throttle', /tracking, uvalue='Select and load the file of "Throttle" scaling factors used to reduce data rates for high count rate situations.')
label = widget_label( hybase1, value=' ')
hymod_throttle_check = cw_bgroup2( hybase1, ['Send'], /row, set_value=[0], sensitive=1, $
				/return_index, uname='hymod-throttle-check',/ nonexclusive, /tracking, $
				uvalue={label:'Throttle',help:'Check this box to send "Throttle" mode to reduce data rates by scaling back selected high intensity peaks. Enable/disable "throttle" on the Enable tab. '}, xpad=0, ypad=0, space=0)

;label = widget_label( hymod_base, value=' ')
hybase2 = widget_base( hymod_base, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
button = widget_button( hybase2, value='Apply to HYMOD', uname='hymod-apply-hymod', /tracking, uvalue='Apply enabled settings (including eneregy calibration) to the Maia HYMOD processor. The setup files shown above are read here and the detailed ' + $
				'data sent to Maia. To just change the "enable" status of each processing module, use the check-marks here or on the "Enable" tab. With "Gain-Trimming" set, all detector channels sent to Maia will be shown selected on the mimic panel. ' + $
				'Without Gain-Trimming, the energy calibration channels send to Maia with be shown selected when in "Individual energy calibrations" mode.')
button = widget_button( hybase2, value='Set Disables', uname='hymod-set-disables', /tracking, uvalue='After a successful "Apply to HYMOD" using the individual energy calibrations option, ' + $
				'the displayed successful detector channels can be used to set the HERMES channel disable parameters (ECH), as on the "Enable" tab page.')

; Summary -----------------------------------------

;print,'maia_setup graphics 2 ...'
Summary_base = widget_base( tab_panel, title='Summary', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( summary_base, value='Parameter Summary Table')

summary_table = Widget_Table(summary_base, UNAME='summary-table', alignment=2,  $
      NOTIFY_REALIZE='OnRealize_maia_summary_table', scr_xsize=left_xsize, $
      /all_events, X_SCROLL_SIZE=13 ,Y_SCROLL_SIZE=table_yscroll ,value=strarr(30,n_detectors), $
      /RESIZEABLE_COLUMNS, /no_row_headers, /tracking, uvalue='Shows the current values of HERMES, SCEPTER and some HYMOD parameters. ' + $
      'Click on a detector "Indx" to select the detector on the mimic display in "Set Parameters" mode. Click on a column heading to (i) Sort the table in ascending order based on this column value in "Set Parameters" mode, or (ii) show the parameters variation on the mimic display in "Show Parameters" mode.' )

; Groups, Spectra -----------------------------------------

spectra_base = widget_base( tab_panel, title='Groups', /column, xpad=1, ypad=1, space=space5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( spectra_base, value='Online Spectra and Groups Controls')

text = widget_text( spectra_base, scr_xsize=left_xsize, ysize=10, /wrap, uname='spectra-explanation', /tracking, $
				value=['Selected Spectra are acquired in real-time in HYMOD. Define "Groups" of detectors to be assigned to each spectrum using the mimic panel on the right, the "Detector Selection Class" droplist and the Table below.', $
					'','Select detectors for a group using the mimic panel and click on "SET" in the row of the desired spectrum. Edit the name for this group in the "Title" text field and hit <return> to apply the change to the Table. ' + $
					'Also enable Pileup Rejection and Throttle for each Group here.'], $
				uvalue='Explanation of the role of the Spectra panel.', frame=1)

spbase0 = widget_base( spectra_base, /column, xpad=1, ypad=1, space=2, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( spbase0, value='"Group" Definitions')

spbase01 = widget_base( spbase0, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( spbase01, value='File:')
spectra_file_text = widget_text( spbase01, value='', uname='groups-file', /tracking, editable=0, $
					NOTIFY_REALIZE='OnRealize_maia_setup_groups_file', $
					uvalue='Groups define which detector channels are used for spectra and imaging. Select the file of "Group" definitions needed to select detectors for the on-line spectra, 2D ET map, ROI imaging and DA imaging, or use the "Load" button. Hit <return> to load the file.', scr_xsize=text_xsize2)
button = widget_button( spbase01, value='Load', uname='load-groups-file', /tracking, uvalue='Groups define which detector channels are used for spectra and imaging. Select the file of "Group" definitions needed to select detectors for the on-line spectra, 2D ET map, ROI imaging and DA imaging.')
label = widget_label( spbase01, value=' ')
spectra_enable_check = cw_bgroup2( spbase01, ['Spectra Enable'], /row, set_value=[0], sensitive=1, $
				/return_index, uname='spectra-groups-check',/ nonexclusive, /tracking, $
				uvalue={label:'Groups',help:'Check this box to enable collection of on-line "Group Spectra". ROI imaging and DA imaging are enabled on the "Imaging" panel. All "enables" are also accessible on the "Enable" panel. These definitions are essential to view on-line spectra and 2D ET maps.'}, xpad=0, ypad=0, space=0)

;label = widget_label( spectra_base, value=' ')
label = widget_label( spectra_base, value='Detector "Group" Table')
group_table = Widget_Table(spectra_base, UNAME='group-table', alignment=1, editable=0, $
      NOTIFY_REALIZE='OnRealize_maia_group_table', scr_xsize=left_xsize, $
      /all_events, X_SCROLL_SIZE=6 ,Y_SCROLL_SIZE=16 ,value=strarr(6,16), /tracking, $
      /no_row_headers, uvalue='Select detectors for a group using the mimic panel and click on "SET" in the row of the desired spectrum. Click elswhere in a row to show the selection on the mimic panel and the Title in the title field. Then enter ' + $
      '(or edit) a name for this group in the "Title" text field and hit <return> to apply the change to the Table. Enable pileup rejection by clicking in the "PU Rej" column. Enable Throttling by clicking in the "Throttle" column. ' )

spbase1 = widget_base( spectra_base, /row, xpad=1, ypad=1, space=5, /align_right, /base_align_center)
label = widget_label( spbase1, value='Edit title for selected:')
group_title_text = widget_text( spbase1, value='', uname='group-title', /tracking, /editable, $
					uvalue='Enter a new Title for the selected Group. Hit <return> to apply this change.', scr_xsize=text_xsize2+70)
      
;label = widget_label( spectra_base, value=' ')
spbase2 = widget_base( spectra_base, /row, xpad=1, ypad=1, space=5, /align_center, /base_align_center)
button = widget_button( spbase2, value='Read', uname='spectra-groups-read', /tracking, uvalue='Read back Group detector masks from the Maia HYMOD processor.')
button = widget_button( spbase2, value='Apply to HYMOD', uname='hymod-apply-spectra', /tracking, uvalue='Apply all these settings to the Maia HYMOD processor.')
button = widget_button( spbase2, value='Save to File', uname='save-groups', /tracking, uvalue='Save to a disk file with extension ".groups.csv". The default file on startup of Maia is "Maia-384.groups.csv".')

; imaging -----------------------------------------

;print,'maia_setup graphics 2b ...'
imaging_base = widget_base( tab_panel, title='Imaging', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( imaging_base, value='Online Imaging Controls')

text = widget_text( imaging_base, scr_xsize=left_xsize, ysize=info_lines_imaging, /wrap, uname='imaging-explanation', /tracking, $
				value=['Setup the Dynamic Analysis (DA) matrix definitions or ROI definitions here. Both depend on the Linearization, '+ $
				'Gain Trimming and Energy Calibration of each detector channel, which are set-up on the Hymod panel. Setup either DA or ROI. ROI uses a special case of the DA method.', $
				'','Setup a DA matrix through a GeoPIXE fit to a representative X-ray spectrum. Alternatively, ' + $
				'ROI are energy regions of interest (or CUTs in GeoPIXE). Set these up using CUTS Setup from the Spectrum Display.'], $
				uvalue='Explanation of the role of the Imaging panel.', frame=1)

imaging_base4 = widget_base( imaging_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( imaging_base4, value='Dynamic Analysis (DA), Real Time Imaging')

hybase41 = widget_base( imaging_base4, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hybase41, value='DA Matrix File:')
imaging_da_text = widget_text( hybase41, value='', uname='da-file', /tracking, editable=0, $
					NOTIFY_REALIZE='OnRealize_maia_setup_DA_file', $
					uvalue='Select the file of "Dynamic Analysis" lookup table matrix generated in the GeoPIXE program for DA real time imaging, or use the "Load" button. Hit <return> to load the file.', scr_xsize=text_xsize2)
button = widget_button( hybase41, value='Load', uname='load-da', /tracking, uvalue='Select and load the file of "Dynamic Analysis" lookup table matrix generated in the GeoPIXE program for DA real time imaging.')
label = widget_label( hybase41, value=' ')
imaging_da_check = cw_bgroup2( hybase41, ['Enable'], /row, set_value=[0], sensitive=1, $
				/return_index, uname='imaging-da-check',/ nonexclusive, /tracking, $
				uvalue={label:'DA',help:'Check this box to enable "Dynamic Analysis" real time imaging using a DA lookup table matrix generated in the GeoPIXE program.'}, xpad=0, ypad=0, space=0)

imaging_base5 = widget_base( imaging_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( imaging_base5, value='Regions of Interest (ROI) Mapping')

hybase51 = widget_base( imaging_base5, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hybase51, value='ROI CUTs File:')
imaging_roi_text = widget_text( hybase51, value='', uname='roi-file', /tracking, editable=0, $
					NOTIFY_REALIZE='OnRealize_maia_setup_ROI_file', $
					uvalue='Select the CUTs file containing the "Regions of Interest" (low,high) energy limits table generated in the GeoPIXE program, or use the "Load" button. Hit <return> to load the file. ROI uses a special case of the DA method.', scr_xsize=text_xsize2)
button = widget_button( hybase51, value='Load', uname='load-roi', /tracking, uvalue='Select and load the CUTs file containing the "Regions of Interest" (low,high) energy limits table generated in the GeoPIXE program. ROI uses a special case of the DA method.')
label = widget_label( hybase51, value=' ')
imaging_roi_check = cw_bgroup2( hybase51, ['Enable'], /row, set_value=[0], sensitive=1, $
				/return_index, uname='imaging-roi-check',/ nonexclusive, /tracking, $
				uvalue={label:'ROI',help:'Check this box to enable "Regions of Interest" mapping using a Elow,Ehigh limits table, or CUTS file generated in the GeoPIXE program.'}, xpad=0, ypad=0, space=0)

ionbeam = (*pm).DevObj->ionbeam()
charge_gain_unit_lists, ic_vals, ic_units, ic_vunits, ionbeam=ionbeam
if ionbeam then begin
	qmodes = ['Direct beam charge integration (Ion Beam)','Indirect using flux counter (with PV)','Indirect using flux counter (no PV)']
	qhelp = "Choose between direct integration of beam charge as a flux measure (ion beam), or indirect integration using a charge counter (e.g. charge integrator or detector in beam). For the latter, " + $
		"select a counter channel by name (e.g. using Epics PV) or not."
endif else begin
	qmodes = ['Direct beam charge integration (Ion Beam)','Indirect using Ion Chamber (Synchrotron, with PV)','Indirect using Ion Chamber (Synchrotron, no EPICS PV)']
	qhelp = "Chose between direct integration of beam charge as a flux measure (Ion Beam), or indirect integration using an Ion Chamber (Synchrotron X-ray beam). For the latter, " + $
		"select using Epics PV in data or not."
endelse

;print,'maia_setup graphics 2b 2 ...'
imaging_base6 = widget_base( imaging_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( imaging_base6, value='Beam Flux / Charge Set-up')

c0base = widget_base( imaging_base6, /row, /base_align_center, xpad=2, ypad=1, space=5)
charge_mode = widget_combobox( c0base, value=qmodes, notify_realize='OnRealize_maia_setup_charge_mode', uname='charge-mode', /tracking, $
					uvalue=qhelp, scr_xsize=left_xsize-30)

IC_base = widget_base( imaging_base6, /column, xpad=4, ypad=1, space=space1, /base_align_right, map=((*pm).IC.mode ne 0))
lab = widget_label( IC_base, value='Ion Chamber Parameters', /align_center)

IC_base1 = widget_base( IC_base, /column, xpad=0, ypad=0, space=space1, /base_align_right, map=((*pm).IC.mode eq 1))

check_plist_maia, (*pm).IC.plist
if (*pm).IC.pv.name eq '' then begin
	(*pm).IC.pv.name = (*(*pm).IC.plist)[0]
endif

s1base = widget_base( IC_base1, /row, xpad=0, ypad=0, space=2, /base_align_center)
lab = widget_label( s1base, value='IC Scaler channel:')
ic_pv_mode = widget_combobox( s1base, value='   '+*(*pm).IC.plist, uname='ic-pv-mode', /tracking, $
				notify_realize='OnRealize_maia_setup_charge_pv_mode', $
				uvalue='Select the Maia hardware scaler inputs ("Maia:scaler.FC0" or "Maia:scaler.FC1") or an external source, such as an Epics scaler PV, to record upstream ion-counter.',scr_xsize=charge_xsize2)

s2base = widget_base( IC_base1, /row, xpad=0, ypad=0, space=2, /base_align_center)
lab = widget_label( s2base, value='Preamp Sensitivity:')
ic_val_mode = widget_combobox( s2base, value='   '+str_tidy(ic_vals), uname='ic-preamp-mode', /tracking, scr_xsize=charge_xsize2, $
				notify_realize='OnRealize_maia_setup_charge_preamp_mode', $
				uvalue="Select the ion chamber preamp sensitivity multiplier. This cannot be changed if this parameter is managed externally via Beamline writes to Maia.")

s3base = widget_base( IC_base1, /row, xpad=0, ypad=0, space=2, /base_align_center)
lab = widget_label( s3base, value='Preamp Scale Units:')
ic_unit_mode = widget_combobox( s3base, value='   '+ic_units, uname='ic-preamp-unit-mode', /tracking, scr_xsize=charge_xsize2, $
				notify_realize='OnRealize_maia_setup_charge_unit_mode', $
				uvalue="Select the ion chamber preamp scale units. This cannot be changed if this parameter is managed externally via Beamline writes to Maia.")

ic_base2 = widget_base( IC_base, /row, /base_align_center, xpad=0, ypad=0, space=2)
scan_button = widget_button( ic_base2, value='Scan data for PVs', uname='charge-scan', /tracking, sensitive=((*pm).IC.mode eq 1), $
					uvalue='Open raw data file(s) and scan for Epics PVs for IC rate and preamplifier settings.')
lab = widget_label( ic_base2, value='     Conversion (Q/flux):')
charge_conversion = widget_text( ic_base2, uname='charge-conversion', /editable, /tracking, value=str_tidy((*pm).IC.conversion), $
				notify_realize='OnRealize_maia_setup_charge_conversion', $
				uvalue='Conversion factor from integrated flux (IC count) to charge (uC) for scan.', scr_xsize=text_xsize-40)
						
button = widget_button( imaging_base, value='Apply to HYMOD', uname='hymod-apply-imaging', /tracking, uvalue='Apply all these settings to the Maia HYMOD processor.')


;--------- Device options ----

maiabase = widget_base( imaging_base, /base_align_center, map=1, space=1, xpad=0, ypad=0)

device_option_mode_base = widget_base( maiabase, /column, /frame,  space=1, xpad=0, ypad=1, /base_align_center, xsize=left_xsize)

; Render sort options in 'render_options' method in object, else set Y size to 1

; Note this uses the switch /axes_only, which is ONLY valid for the Maia device object at the moment.

(*pm).DevObj->render_options, device_option_mode_base, /axes_only
widget_control, device_option_mode_base, map=(*pm).DevObj->show_sort_options(), scr_ysize=(*pm).DevObj->get_sort_ysize()

;-------------------------------

button = widget_button( imaging_base, value='Apply Axes', uname='imaging-apply-axes', /tracking, uvalue='Apply the axis redirection settings to the DA Image process.')

;print,'maia_setup graphics 2b 4 ...'
imaging_base7 = widget_base( imaging_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)

auto_DA_check = cw_bgroup2( imaging_base7, ['Auto save DA / ROI images after each run'], /row, set_value=[(*pm).DA.save], sensitive=1, $
				/return_index, uname='imaging-save-check',/ nonexclusive, /tracking, $
				uvalue='Check this box to enable auto saving of "Dynamic Analysis" or "ROI" real time images after run to a local directory. Select the save path using the text field and "Set" button below.', xpad=0, ypad=0, space=0)
hybase7 = widget_base( imaging_base7, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hybase7, value='Save Path:')
save_path_text = widget_text( hybase7, value=(*pm).DA.save_path, scr_xsize=text_xsize2+100, /wrap, uname='da-save-text', /tracking, $
				uvalue='Set the save local path for DA and ROI real-time images.',/edit)
button = widget_button( hybase7, value='Set', uname='da-save-path', /tracking, uvalue='Set the save local path for DA and ROI real-time images.')

button = widget_button( imaging_base7, value='Apply RT Save', uname='imaging-apply-rt', /tracking, uvalue='Apply these RT image save settings for local saving of RT images.')


; Controls -----------------------------------------

;print,'maia_setup graphics 2c ...'
Controls_base = widget_base( tab_panel, title='Controls', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( controls_base, value='Maia (DDM, DAM, DBPM) Controls')

text = widget_text( controls_base, scr_xsize=left_xsize, ysize=text_lines_controls, /wrap, uname='controls-explanation', /tracking, $
				value=['Status of DDM, DAM and DBPM modules and controls for detector bias and cooling.','','Ensure that vacuum is adequate (<1.e-5 mbar), vacuum interlock is active and water cooling is operational before biasing and cooling the detector.', $
						'','Adjust Peltier cooling slowly. A software maximum is set depending on Maia model. '+ $
						'Adjust bias slowly after vacuum is good, not exceeding the recommended working bias for your detector. A software maximum is set from Maia "setup" var file (for Maia B,C,D) to around 80 V.'], $
				uvalue='Explanation of the role of the Controls panel.', frame=1)

;controls_base2 = widget_base( controls_base, /column, xpad=1, ypad=1, space=2, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
;label = widget_label( controls_base2, value='Parameters for one channel at a time')
;
;id = 3
;cbase1 = widget_base( controls_base2, /row, xpad=1, ypad=1, space=5, /align_right, /base_align_center)
;label = widget_label( cbase1, value='HERMES Leakage current monitor:')
;hermes_elk_mode = widget_combobox(cbase1, uname='hermes-elk-mode', scr_xsize=text_xsize, $
;;			NOTIFY_REALIZE='OnRealize_evt_station', $
;			value=['Disabled','Enabled'], /tracking, uvalue={id:id, help:'Select the HERMES leakage current monitor mode (elk) for the selected detector(s).  Normally, all HERMES channels are set to the same leakage monitor. Maia typically uses elk "Enabled".'})
;check = cw_bgroup2( cbase1, [''], /row, set_value=[0], sensitive=1, $
;				/return_index, uname='hermes-elk-check',/ nonexclusive, /tracking, $
;				uvalue={label:'Hermes Leakage monitor',help:'Check this box to apply this parameter to select detectors or to have this parameter displayed over the detector array in "Show Parameters" mode.'}, xpad=0, ypad=0, space=0)
;check_ids[id] = check
;label = widget_label( hermes_base, value=' ')

;cbase2 = widget_base( controls_base, /row, xpad=1, ypad=1, space=5, /align_right, /base_align_center)
;label = widget_label( cbase2, value='Set from:')
;controls_detector_text = widget_text( cbase2, value='', uname='controls-set-from-text1', /tracking, $
;					uvalue='Shows the detector # that the parameters above have been set from.', scr_xsize=40)
;label = widget_label( cbase2, value='   Apply to:')
;controls_parameter_text = widget_text( cbase2, value='', uname='controls-set-from-text2', /tracking, $
;					uvalue='Shows the HERMES parameter that will be set by "Apply".', scr_xsize=160)
controls_detector_text = 0L
controls_parameter_text = 0L

;controls_apply_button = widget_button( controls_base, value='Apply to DDM', uname='controls-apply', /tracking, uvalue='Apply the selected settings to the DDM. ')
;label = widget_label( controls_base, value=' ')


controls_base4a = widget_base( controls_base, /column, xpad=1, ypad=1, space=2, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( controls_base4a, value='Detector Array')
controls_base4 = widget_base( controls_base4a, /row, xpad=1, ypad=1, space=2, /align_center, /base_align_top, scr_xsize=left_xsize)

cbase4l = widget_base( controls_base4, /column, xpad=2, ypad=1, space=1, /align_top, /base_align_right, /frame)
label = widget_label( cbase4l, value='Monitor', /align_center)

cbase4 = widget_base( cbase4l, /row, xpad=0, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( cbase4, value='Bias:')
controls_bias_text = widget_text( cbase4, value=str_tidy((*pm).control.bias_monitor), uname='controls-bias-monitor', /tracking, $
					uvalue='Shows the readback of detector bias (V).', scr_xsize=text_xsize3)

cbase4 = widget_base( cbase4l, /row, xpad=0, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( cbase4, value='Leakage:')
controls_leakage_text = widget_text( cbase4, value=str_tidy((*pm).control.leakage), uname='controls-leakage-monitor', /tracking, $
					uvalue='Shows the readback of detector leakage current (uA).', scr_xsize=text_xsize3)

cbase4b = widget_base( cbase4l, /row, xpad=0, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( cbase4b, value='Peltier:')
controls_peltier_monitor_text = widget_text( cbase4b, value=str_tidy((*pm).control.peltier_monitor), uname='controls-detector-peltier-monitor', /tracking, $
					uvalue='Shows the readback of Peltier current (A). Positive values in Cooling mode, negative values indicate Bake-out mode', scr_xsize=text_xsize3)

cbase4r = widget_base( controls_base4, /column, xpad=2, ypad=1, space=1, /align_top, /base_align_right, /frame)
label = widget_label( cbase4r, value='Controls', /align_center)

cbase4d = widget_base( cbase4r, /row, xpad=0, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( cbase4d, value='Bias (V):')
controls_bias_slider = cw_fslider2( cbase4d, format='(F5.1)', minimum=(*pm).control.bias_min, maximum=(*pm).control.bias_max, layout=1, scroll=5., $
				value=(((*pm).control.bias > (*pm).control.bias_min) < (*pm).control.bias_max), uname='controls-bias-slider', xsize=text_xsize3+xslide_off, /tracking, /edit, /drag, xpad=0, ypad=0, $
				uvalue='Adjust the detector bias (V). Normal setting is 80 V for Maia 384 (B,C,D).')

cbase4e = widget_base( cbase4r, /row, xpad=0, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( cbase4e, value='Guard (V):')
controls_guard_slider = cw_fslider2( cbase4e, format='(F5.2)', minimum=0., maximum=(*pm).control.guard_max, layout=1, scroll=0.1, $
				value=((*pm).control.guard < (*pm).control.guard_max), uname='controls-guard-slider', xsize=text_xsize3+xslide_off, /tracking, /edit, /drag, xpad=0, ypad=0, $
				uvalue='Adjust the guard ring bias (V). Normal setting range is 1-6-2.5 V.')
label = widget_label( controls_base, value=' ')

if (*pm).control.status.bake then begin
	sign = (*pm).version.dbpm ge 2 ? -1.0 : +1.0
	(*pm).control.peltier = sign * (abs((*pm).control.peltier) < (*pm).control.peltier_bake_max)
endif else begin
	(*pm).control.peltier = (*pm).control.peltier < (*pm).control.peltier_cool_max
endelse
pel_max = (*pm).control.peltier_cool_max > (*pm).control.peltier_bake_max

cbase4c = widget_base( cbase4r, /row, xpad=0, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( cbase4c, value='Peltier (A):')
controls_peltier_slider = cw_fslider2( cbase4c, format='(F5.2)', minimum=0., maximum=pel_max, layout=1, scroll=0.1, $
				value=abs((*pm).control.peltier), uname='controls-peltier-slider', xsize=text_xsize3+xslide_off, /tracking, /edit, /drag, xpad=0, ypad=0, $
				uvalue='Adjust the Peltier drive (A) for detector wafer cooling and bake-out. Cooling: Normal setting is 0.45 A for Maia 96, 1.0 A for Maia 384C/D, 2.7 A for Maia 384B. Bake-out: Much lower drive current is used with caution. ' + $
						'Take care not to exceed a detector temperature of 50 C or a HERMES temperature of 40 C. Take care not exceed 60 C on the MOSFET drivers .')

controls_base4b = widget_base( controls_base4a, /row, xpad=0, ypad=0, space=10, /align_center, /base_align_center)
controls_peltier_mode = widget_combobox( controls_base4b, value=['    COOL  ','    BAKE  '], uname='peltier-mode', /tracking, scr_xsize=1.5*text_xsize3, $
					notify_realize='OnRealize_maia_setup_peltier_mode', $
					uvalue='select Peltier "Cooling" or "Bakeout" control. "Bakeout" is only available with Bakeout plug inserted.')

bake_button_base = widget_base( controls_base4b, /column, xpad=1, ypad=1, space=5, /align_center, /base_align_center, map=(*pm).control.status.bake)
bake_button = state_button( bake_button_base, value='CAUTION: Bakeout Enabled', uname='detector-bakeout', /tracking, xsize=2*text_xsize3+xslide_off+50, ysize=22, $
					/freeze, select=2, colours=colours2, n_states=3, alt=0, charsize=1., charthick=1., $
					uvalue='Indicates that the detector bake-out plug has been inserted so that the Peltier can HEAT the detector for bake-out or COOL it. ' + $
				'RED: Bakeout enabled and Peltier heating applied. YELLOW: Bakeout enabled, cooling or no Peltier current. ' + $
				'Proceed with caution, ensure water cooling is flowing and monitor temperatures before heating. Keep HERMES temperature below 40 C.')

controls_base5a = widget_base( controls_base, /column, xpad=1, ypad=1, space=2, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( controls_base5a, value='DBPM, DDM & DAM Status')
controls_base5 = widget_base( controls_base5a, /row, xpad=1, ypad=1, space=10, /align_center, /base_align_top, scr_xsize=left_xsize)

cbase5l = widget_base( controls_base5, /column, xpad=2, ypad=1, space=1, /align_top, /base_align_right, /frame)
label = widget_label( cbase5l, value='Temperature', /align_center)

cbase5z = widget_base( cbase5l, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5z, value='Detector:')
controls_detector_temp_text = widget_text( cbase5z, value=str_tidy((*pm).control.temp.detector), uname='controls-detector-temp-monitor', /tracking, $
					uvalue='Shows the readback of detector temperature (C).', scr_xsize=text_xsize3)

cbase5 = widget_base( cbase5l, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5, value='HERMES:')
controls_temp_hermes_text = widget_text( cbase5, value=str_tidy((*pm).control.temp.hermes), uname='controls-temp-hermes-monitor', /tracking, $
					uvalue='Shows the readback of HERMES ASIC temperature (C).', scr_xsize=text_xsize3)

cbase5a = widget_base( cbase5l, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5a, value=' Water:')
controls_temp_water_text = widget_text( cbase5a, value=str_tidy((*pm).control.temp.water), uname='controls-temp-water-monitor', /tracking, $
					uvalue='Shows the readback of Water temperature (C).', scr_xsize=text_xsize3)

cbase5b = widget_base( cbase5l, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5b, value='Cold Trap:')
controls_temp_coldtrap_text = widget_text( cbase5b, value=str_tidy((*pm).control.temp.coldtrap), uname='controls-temp-coldtrap-monitor', /tracking, $
					uvalue='Shows the readback of cold trap temperature (C).', scr_xsize=text_xsize3)

cbase5c = widget_base( cbase5l, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5c, value='MOSFET:')
controls_temp_mosfet_text = widget_text( cbase5c, value=str_tidy((*pm).control.temp.mosfet), uname='controls-temp-mosfet-monitor', /tracking, $
					uvalue='Shows the readback of MOSFET temperature (C).', scr_xsize=text_xsize3)


cbase5r = widget_base( controls_base5, /column, xpad=2, ypad=1, space=1, /align_top, /base_align_right)

cbase5f = widget_base( cbase5r, /row, xpad=3, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5f, value='DDM Vcc:')
controls_ddm_vcc_text = widget_text( cbase5f, value=str_tidy((*pm).control.vcc), uname='controls-ddm-vcc-monitor', /tracking, $
					uvalue='Shows the readback of DDM Vcc supply voltage (V).', scr_xsize=text_xsize3)

;				This is now on Debug tab ...
;cbase5f2 = widget_base( cbase5r, /row, xpad=3, ypad=0, space=5, /base_align_center)
;label = widget_label( cbase5f2, value='OAN, ELK:')
;controls_oan_text = widget_text( cbase5f2, value=str_tidy((*pm).control.oan), uname='controls-oan-monitor', /tracking, $
;					uvalue='Shows the readback of either the OAN or ELK ADC voltage (V). Select an OAN HERMES channel on the Debug panel to monitor its baseline here, or select an ELK HERMES channel on the HERMES panel to monitor leakage. ' + $
;					'Do not have BOTH active at one time.', scr_xsize=text_xsize3)
;
cbase5g = widget_base( cbase5r, /row, xpad=3, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5g, value='Peltier Vcc:')
controls_peltier_supply_text = widget_text( cbase5g, value=str_tidy((*pm).control.peltier_supply), uname='controls-peltier-supply-monitor', /tracking, $
					uvalue='Shows the readback of the Peltier power supply voltage (V)', scr_xsize=text_xsize3)

cbase5r2 = widget_base( cbase5r, /column, xpad=2, ypad=1, space=1, /align_top, /base_align_right, /frame)
label = widget_label( cbase5r2, value='Temperature', /align_center)

cbase5d = widget_base( cbase5r2, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5d, value='DDM FPGA:')
controls_temp_fpga_text = widget_text( cbase5d, value=str_tidy((*pm).control.temp.fpga), uname='controls-temp-fpga-monitor', /tracking, $
					uvalue='Shows the readback of DDM FPGA temperature (C).', scr_xsize=text_xsize3)

cbase5h = widget_base( cbase5r2, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5h, value='HYMOD FPGA:')
controls_temp_hymod_fpga_text = widget_text( cbase5h, value=str_tidy((*pm).control.temp.hymod_fpga), uname='controls-temp-hymod-fpga-monitor', /tracking, $
					uvalue='Shows the readback of the HYMOD FPGA temperature (C).', scr_xsize=text_xsize3)

cbase5i = widget_base( cbase5r2, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5i, value='HYMOD CPU:')
controls_temp_hymod_cpu_text = widget_text( cbase5i, value=str_tidy((*pm).control.temp.hymod_cpu), uname='controls-temp-hymod-cpu-monitor', /tracking, $
					uvalue='Shows the readback of the HYMOD CPU temperature (C).', scr_xsize=text_xsize3)

; Layout -----------------------------------------

;print,'maia_setup graphics 2d ...'
Edit_base = widget_base( tab_panel, title='Layout', /column, xpad=1, ypad=1, space=space2, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( edit_base, value='Edit Detector Layout')

text = widget_text( edit_base, scr_xsize=left_xsize, ysize=12, /wrap, uname='edit-explanation', /tracking, $
				value=['Click on detectors to select them; their ID and membership in various classes are displayed.', $
					'','To set attributes for detectors, select them and enter the attribute in the relevant field, and hit <enter> to apply the change to all selected detectors. ' + $
					'Radial classes can be assigned, based on calculated distance from origin, using "Set Radial Classes".', $
					'','To number pads in order, use "Start Number Sequence" and click on pads in order starting with the first one for a particular HERMES.'], $
				uvalue='Explanation of the role of the Edit panel.', frame=1)

ebase00 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase00, value='CSV Table Index:')
edit_index_text = widget_text( ebase00, value='', uname='edit-index', /tracking, $
					uvalue='The index (row number in CSV table file) for the selected detector.', scr_xsize=text_xsize)

ebase0 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase0, value='Detector Number:')
edit_id_text = widget_text( ebase0, value='', uname='edit-id', /tracking, /editable, $
;					NOTIFY_REALIZE='OnRealize_file_requester_root', $
					uvalue='Edit the "Number" ID for the selected detector (starts at 0). Hit <return> to apply this change. Use the "Start Number Sequence" button to set a whole series of Number IDs for a particular Hermes. ' + $
					'Always start on the first pad for a Hermes. Starting detector number set according to the Hermes number (x32).', scr_xsize=text_xsize)

ebase01 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase01, value='Number in Chip:')
edit_n_in_chip_text = widget_text( ebase01, value='', uname='edit-n-in-chip', /tracking, $
					uvalue='The index within a chip for the selected detector.', scr_xsize=text_xsize)

label = widget_label( edit_base, value=' ')
edit_sequence_button = widget_button( edit_base, value='Start Number Sequence', uname='edit-number-sequence', /tracking, uvalue='Start numbering pads, at the first pad for any Hermes, by clicking on it and continuing ' + $
					'onto the next pad through to the end of a Hermes, clicking on pads once each in order. Optionally continue onto the next Hermes in the series starting with its first pad. ' + $
					'All pads will be numbered in order as you proceed. Remember to "STOP Number Sequence" and "Save Layout" when you have finished.')
label = widget_label( edit_base, value=' ')

ebase1 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase1, value='Hermes-Scepter:')
edit_hermes_text = widget_text( ebase1, value='', uname='edit-hermes', /tracking, /editable, $
;					NOTIFY_REALIZE='OnRealize_file_requester_root', $
					uvalue='Edit the "Hermes" ID for the selected detectors (starts at 0). Hit <return> to apply this change.', scr_xsize=text_xsize)

ebase2 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase2, value='Quadrant:')
edit_quadrant_text = widget_text( ebase2, value='', uname='edit-quadrant', /tracking, /editable, $
;					NOTIFY_REALIZE='OnRealize_file_requester_root', $
					uvalue='Edit the "Quadrant" ID for the selected detectors (starts at 0). Hit <return> to apply this change.', scr_xsize=text_xsize)

ebase3 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase3, value='Radial:')
edit_radial_text = widget_text( ebase3, value='', uname='edit-radial', /tracking, /editable, $
;					NOTIFY_REALIZE='OnRealize_file_requester_root', $
					uvalue='Edit the "Radial" ID for the selected detectors (starts at 0). Hit <return> to apply this change. In addition, Radial classes can be assigned programmatically using "Set Radial Classes" and "Pitch".', scr_xsize=text_xsize)

ebase4 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase4, value='Column:')
edit_column_text = widget_text( ebase4, value='', uname='edit-column', /tracking, /editable, $
;					NOTIFY_REALIZE='OnRealize_file_requester_root', $
					uvalue='Edit the "Column" ID for the selected detectors (starts at 0). Hit <return> to apply this change. Note: "column" is defined as X coordinate in initial hardware orientation before any rotation.', scr_xsize=text_xsize)

ebase5 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase5, value='Row:')
edit_row_text = widget_text( ebase5, value='', uname='edit-row', /tracking, /editable, $
;					NOTIFY_REALIZE='OnRealize_file_requester_root', $
					uvalue='Edit the "Row" ID for the selected detectors (starts at 0). Hit <return> to apply this change. Note: "row" is defined as Y coordinate in initial hardware orientation before any rotation.', scr_xsize=text_xsize)

ebase5b = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase5b, value=' Width (mm):')
edit_width_text = widget_text( ebase5b, value='', uname='edit-width', /tracking, /editable, $
;					NOTIFY_REALIZE='OnRealize_file_requester_root', $
					uvalue='Edit the "Width" (mm) of the selected detectors. Hit <return> to apply this change.', scr_xsize=text_xsize)

ebase5c = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase5c, value='Height (mm):')
edit_height_text = widget_text( ebase5c, value='', uname='edit-height', /tracking, /editable, $
;					NOTIFY_REALIZE='OnRealize_file_requester_root', $
					uvalue='Edit the "Height" (mm) of the selected detectors. Hit <return> to apply this change.', scr_xsize=text_xsize)

label = widget_label( edit_base, value=' ')
ebase6 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
button = widget_button( ebase6, value='Set Radial Classes', uname='edit-set-radial', /tracking, uvalue='Assign pads to Radial Classes based on distance from the origin binned according to the size of "Pitch" (mm).')
label = widget_label( ebase6, value='   Pitch:')
edit_pitch_text = widget_text( ebase6, value='1.0', uname='edit-pitch', /tracking, /editable, $
;					NOTIFY_REALIZE='OnRealize_file_requester_root', $
					uvalue='Edit the "Pitch" (mm) used to bin pads into Radial Classes. Distance from origin is binned using Pitch.', scr_xsize=text_xsize)

label = widget_label( edit_base, value=' ')
ebase20 = widget_base( edit_base, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
button = widget_button( ebase20, value='Read Layout', uname='edit-read', /tracking, uvalue='Read the detector layout assignments from Kandinski. This is normally unnecessary, as these are stored in the Maia layout CSV file "'+(*pd).file+'", but may be needed to define a new layout revision with new generation Maia hardware.')
button = widget_button( ebase20, value='Save Layout', uname='edit-save', /tracking, uvalue='Save the modified detector layout assignments to file "'+(*pd).file+'".')
button = widget_button( ebase20, value='Revert to Saved', uname='edit-revert', /tracking, uvalue='Revert to the original saved detector layout assignments in file "'+(*pd).file+'".')

; Pulser -----------------------------------------

;print,'maia_setup graphics 2e ...'
Pulser_base = widget_base( tab_panel, title='Pulser / Tests', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( pulser_base, value='Tests and Pulser Controls')

text = widget_text( pulser_base, scr_xsize=left_xsize, ysize=text_lines_pulser, /wrap, uname='pulser-explanation', /tracking, $
				value=['Control the pulser built into Maia to (i) probe selected detector channels, (ii) run a procedure to collect data to build gain linearization tables, (iii) run a procedure to collect data to build gain trimming tables to match E and T between channels, (iv) map detector leakage (ELK), (v) baseline (EAN).', '', $
						'Note that all pulses are coincident, and hence multiple channels selected on the same SCEPTER load up the peak detectors. Do not have more than 7 channels active on each SCEPTER, and preferably test only one per SCEPTER.'], $
				uvalue='Explanation of the role of the Test Pulser panel.', frame=1)

Pulser_base1 = widget_base( Pulser_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( Pulser_base1, value='Pulser or Test Program Mode')

pbase1 = widget_base( Pulser_base1, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
label = widget_label( pbase1, value='Mode:')
pulser_program_mode = widget_combobox(pbase1, uname='pulser-program-mode', scr_xsize=2*text_xsize, $
			value=['  Off','  Pulse  selected  channels','  Run  "step-cal"  procedure','  Run  "gain-trim"  procedure','  Synth pulser','  Run  "leakage mapping"  procedure','  Run  "baseline mapping"  procedure','  Run  "step-cal"  ALL channels'], /tracking, $
				uvalue={id:id,help:'Select the Pulser/Test mode between: (i) manual pulser operation for selected channels, ' + $
				'(ii) Running the "step-cal" procedure to characterize non-linearity in detector gain, (iii) Running the "gain-trim" procedure in order to correct E and T spectra, (iv) Run the event Synthesiser, (v) Run "sample ELK" to map leakage, and (vi) Run "baseline EAN" to map baseline. ' + $
				'Select channels: (i) Pulser: desired channels (<7 per SCEPTER); (ii) Step-Cal: one detector per SCEPTER; (iii) Gain-Trim: controlled by procedure; (iv) Synth: Synthetic ET pulser; (v) Step-cal ALL: controlled by procedure.'})

start_text = ['Abort','Enable Pulser','Execute "step-cal"','Execute "gain-trim"','Start Synth','Execute "ELK mapping"','Execute "EAN mapping"','Execute "step-cal" ALL']
pulser_start_base = widget_base( Pulser_base1, /row, xpad=1, ypad=1, space=5, /align_center, /base_align_center, map=0)
label = widget_label( pulser_start_base, value='Control:')
pulser_start_button = widget_button( pulser_start_base, value=start_text[1], uname='pulser-start', scr_xsize=2*text_xsize, /tracking, uvalue='Start the pulser operating, or execute the selected pulser procedure.')

Pulser_base2 = widget_base( Pulser_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( Pulser_base2, value='Pulser and Test Parameters')

pbase21 = widget_base( Pulser_base2, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
pulser_low_label = widget_label( pbase21, value='   Amp:')
pulser_low_text = widget_text( pbase21, scr_xsize=text_xsize, uname='pulser-low-text', /tracking, /editable, value='', $
				uvalue='Set the pulser amplitude (V). In the execute procedures, the pulser amplitude steps between a Low and High value. In this case, this sets the Low, or minimum, pulser amplitude (V). If "Count" is available, the low,high range is subdivided into "Counts" discrete peaks.')
pulser_high_base = widget_base( pbase21, /row, xpad=0, ypad=0, space=5, /align_right, /base_align_center, map=0)
pulser_high_label = widget_label( pulser_high_base, value='  High:', scr_xsize=50, /align_right)
pulser_high_text = widget_text( pulser_high_base, scr_xsize=text_xsize, uname='pulser-high-text', /tracking, /editable, value='', $
				uvalue='The pulser amplitude steps between a Low and High value. This set the High, or maximum, pulser amplitude (V). If "Count" is available, the low,high range is subdivided into "Counts" discrete peaks.')

pbase22 = widget_base( Pulser_base2, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
pulser_rate_base = widget_base( pbase22, /row, xpad=0, ypad=0, space=5, /align_right, /base_align_center, map=0)
label = widget_label( pulser_rate_base, value='Rate:')
pulser_rate_text = widget_text( pulser_rate_base, scr_xsize=text_xsize, uname='pulser-rate-text', /tracking, /editable, value='', $
				uvalue='This set the pulser count rate (c/s).')
pulser_count_base = widget_base( pbase22, /row, xpad=0, ypad=0, space=5, /align_right, /base_align_center, map=0)
label = widget_label( pulser_count_base, value='Count:', scr_xsize=50, /align_right)
pulser_count_text = widget_text( pulser_count_base, scr_xsize=text_xsize, uname='pulser-count-text', /editable, /tracking, value='', $
				uvalue='The selected pulser procedure involves subdividing the (Low,High) range into a number of discrete peaks. The "Count" set the number of peaks.')

pulser_time_base = widget_base( Pulser_base2, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center, map=0)
label = widget_label( pulser_time_base, value='Time:')
pulser_time_text = widget_text( pulser_time_base, scr_xsize=text_xsize, uname='pulser-time-text', /tracking, /editable, value='', $
				uvalue='The dwell time for counting each peak in each detector (seconds).')

pulser_progress_base = widget_base( Pulser_base, /row, xpad=0, ypad=1, space=5, /align_center, /base_align_center, map=0)
pulser_list = Widget_List( pulser_progress_base, UNAME='pulser-list', value = strarr(ptable_yscroll), scr_xsize=left_xsize ,scr_ysize=list_ysize, $
;				NOTIFY_REALIZE='OnRealize_maia_setup_pulser_list', $
				/tracking, uvalue='List showing the commands in the currently executing procedure, which is selected above. The cursor will follow the progress of execution.')

;label = widget_label( Pulser_base, value='')
pbase31 = widget_base( Pulser_base, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
button = widget_button( pbase31, value='Save Pulser Parameters', uname='save-pulser', /tracking, uvalue='Save the Maia pulser parameters, and the channel selection, to a file, which defaults to "Maia.pulser.csv".')
button = widget_button( pbase31, value='Load Pulser Parameters', uname='load-pulser', /tracking, uvalue='Load the Maia pulser parameters, and the channel selection, from a file, which defaults to "Maia.pulser.csv".')

; Debug -----------------------------------------

;print,'maia_setup graphics 3 ...'
debug_base = widget_base( tab_panel, title='Debug', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( debug_base, value='Debug Controls')

text = widget_text( debug_base, scr_xsize=left_xsize, ysize=9, /wrap, uname='debug-explanation', /tracking, $
				value=['HERMES debug: Select the output of only one channel across all HERMES to connect to OAN. Make sure this channel is not disabled on the "Enable" panel.', $
					'The Lemo connectors are (from top): AUX, OAN, DIAG, CAL.','', $
					'SCEPTER debug: Lock peak detector arbitration and select the output of only one channel across all SCEPTERs to connect to AUX. Make sure this channel is not disabled on the "Enable" panel.'], $
				uvalue='Explanation of the role of the Debug panel.', frame=1)

debug_base2 = widget_base( debug_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)

dbase02 = widget_base( debug_base2, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
debug_ean_check_id = cw_bgroup2( dbase02, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='debug-ean-check',/ nonexclusive, /tracking, $
				uvalue='Check this box to select this group of parameters for modification and transmission to Maia. Select channels on the mimic display on the right. ' + $
					'Make sure the radio button at the bottom of the window is in "Set Parameters" mode.', xpad=0, ypad=0, space=0)
label = widget_label( dbase02, value='HERMES Analogue Debug Output')

enable_debug_ean_mode = widget_combobox(debug_base2, uname='enable-debug-ean', scr_xsize=2*text_xsize, sensitive=0, $
			value=['Off','HERMES OAN output to OAN Lemo','HERMES ELK output to OAN Lemo'], /tracking, uvalue={label:'Debug',help:'Select the HERMES OAN or ELK output debug mode to use. Remember to select "Off" to return to normal operation. ' + $
				'OAN: Set one channel on to monitor one HERMES channel OAN pulse output on the OAK output socket, and also on the OAN/ELK field below. ' + $
				'ELK: Set one channel on to monitor one HERMES channel ELK leakage output on the OAK output socket, and also on the OAN/ELK field below. '})

cbase5f2 = widget_base( debug_base2, /row, xpad=3, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5f2, value='OAN / ELK:')
controls_oan_text = widget_text( cbase5f2, value=str_tidy((*pm).control.oan), uname='controls-oan-monitor', /tracking, $
					uvalue='Shows the readback of OAN ADC voltage (V). Select an OAN HERMES channel on the Debug droplist above to monitor its baseline here. ' + $
					'The voltage is also available at the output socket on the Maia housing.', scr_xsize=text_xsize3)

debug_base3 = widget_base( debug_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)

dbase03 = widget_base( debug_base3, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
debug_aux_check_id = cw_bgroup2( dbase03, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='debug-aux-check',/ nonexclusive, /tracking, $
				uvalue='Check this box to select this group of parameters for modification and transmission to Maia. Select channels on the mimic display on the right. ' + $
					'Make sure the radio button at the bottom of the window is in "Set Parameters" mode.', xpad=0, ypad=0, space=0)
label = widget_label( dbase03, value='SCEPTER Analogue Debug Output')

enable_debug_aux_mode = widget_combobox(debug_base3, uname='enable-debug-aux', scr_xsize=2*text_xsize, sensitive=0, $
			value=['Off','SCEPTER output to AUX Lemo'], /tracking, uvalue={label:'Debug',help:'Select the SCEPTER AUX output debug mode to use. Remember to select "Off" to return to normal operation. ' + $
				'AUX: Select just one SCEPTER output channel to monitor on the AUX output socket.'})

dbase032 = widget_base( debug_base3, /row, xpad=3, ypad=0, space=5, /base_align_center)
label = widget_label( dbase032, value='AUX:')
controls_aux_text = widget_text( dbase032, value=str_tidy((*pm).control.aux), uname='controls-aux-monitor', /tracking, $
					uvalue='Shows the readback of AUX ADC voltage (V). Select an AUX SCEPTER channel on the Debug droplist above to monitor its baseline here. ' + $
					'The voltage is also available at the output socket on the Maia housing.', scr_xsize=text_xsize3)

debug_apply_button = widget_button( debug_base, value='Apply to Maia', uname='debug-apply', /tracking, uvalue='Apply the selected settings (set using the radio buttons and mimic display selection) to the selected Maia ASICs.')
label = widget_label( debug_base, value=' ')

debug_base4 = widget_base( debug_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)

dbase04 = widget_base( debug_base4, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
label = widget_label( dbase04, value='DAM Monitor ADC Output')

monitor_modes = ['none','+3Vref','+4V','+5V','-5V','+150Vac','spare','-6V','+6V','VDD-HA-AB','VDD-HA-CD','VDD-HP','VDD-SA-AB','VDD-SA-CD','VDD-SD','VSS','VSSD']
enable_debug_monitor_mode = widget_combobox(debug_base4, uname='enable-debug-monitor', scr_xsize=2*text_xsize, sensitive=1, $
			value=monitor_modes, /tracking, uvalue={label:'Debug',help:'Select the DAM Monitor ADC output mode to use. The value is displayed below. '})

dbase042 = widget_base( debug_base4, /row, xpad=3, ypad=0, space=5, /base_align_center)
label = widget_label( dbase042, value='DAM monitor:')
controls_monitor_text = widget_text( dbase042, value=str_tidy((*pm).control.monitor), uname='controls-dam-monitor', /tracking, $
					uvalue='Shows the readback of DAM Monitor ADC voltage (V). Select the DAM Monitor channel on the Debug droplist above to monitor its ADC value here. ' + $
					'DAM Monitor also appears on the DIAG output Lemo (perhaps divided down).', scr_xsize=text_xsize3)

label = widget_label( debug_base, value=' ')
label = widget_label( debug_base, value=' ')
debug_base9 = widget_base( debug_base, /column, xpad=1, ypad=1, space=space1b, /align_center, /base_align_right)
letters = ['','A','B','C','D','E','F','G','H','I','J','K','L','M']
sver = str_tidy((*pm).version.dam) + '  (' + letters[ (*pm).version.dam ] + ')'
dbase9 = widget_base( debug_base9, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( dbase9, value='Maia DAM analogue board revision:')
dam_version_text = widget_text( dbase9, value=sver, uname='dam-version-text', /tracking, $
					uvalue='Shows the Maia DAM analogue board hardware revision number.', scr_xsize=50)
dbase91 = widget_base( debug_base9, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( dbase91, value='Maia DDM digital board revision:')
ddm_version_text = widget_text( dbase91, value=str_tidy((*pm).version.ddm), uname='ddm-version-text', /tracking, $
					uvalue='Shows the Maia DDM digital board hardware revision number.', scr_xsize=50)
dbase92 = widget_base( debug_base9, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( dbase92, value='Maia DBPM Peltier/power board revision:')
dbpm_version_text = widget_text( dbase92, value=str_tidy((*pm).version.dbpm), uname='dbpm-version-text', /tracking, $
					uvalue='Shows the Maia DBPM Peltier-p/s board hardware revision number.', scr_xsize=50)

;------------------------------------------------------------------------------------------------

wbase = widget_base( lbase, /row, xpad=0, ypad=0, space=10, /base_align_center, /align_center)
warning_pulser_base = widget_base( wbase, /row, xpad=0, ypad=0, space=1, /base_align_center, /align_center, map=0)
button = state_button( warning_pulser_base, value='STOP Pulser', uname='stop-pulser-button', /tracking, xsize=90, ysize=22, $
					/freeze, select=1, colours=colours2, n_states=3, alt=0, $
					uvalue='Flags a running "Pulser". This needs to be turned "Off" for normal operation to resume. Click on this button to switch it Off.')
warning_debug_base = widget_base( wbase, /row, xpad=0, ypad=0, space=1, /base_align_center, /align_center, map=0)
stop_debug_button = state_button( warning_debug_base, value='STOP Debug', uname='stop-debug-button', /tracking, xsize=90, ysize=22, $
					/freeze, select=1, colours=colours2, n_states=3, alt=0, $
					uvalue='Flags "Debug" option active. These need to be turned "Off" for normal operation to resume. Click on this button to switch it Off.')
warning_eblk_base = widget_base( wbase, /row, xpad=0, ypad=0, space=1, /base_align_center, /align_center, map=0)
button = state_button( warning_eblk_base, value='STOP EBLK', uname='stop-eblk-button', /tracking, xsize=90, ysize=22, $
					/freeze, select=1, colours=colours2, n_states=3, alt=0, $
					uvalue='Flags HERMES "EBLK" option active on at least one ASIC. These need to be turned "Off" for normal operation to resume. Click on this button to switch it Off.')
warning_bake_base = widget_base( wbase, /row, xpad=0, ypad=0, space=1, /base_align_center, /align_center, map=0)
button = state_button( warning_bake_base, value='STOP Bake', uname='stop-bake-button', /tracking, xsize=90, ysize=22, $
					/freeze, select=2, colours=colours2, n_states=3, alt=0, $
					uvalue='Flags detector Peltier Bake mode active (bake plug is inserted). Click on this button to zero the Peltier current (you will need to remove the Bake plug).')

sbase = widget_base( lbase, /row, xpad=0, ypad=0, space=20, /base_align_center, /align_center)
button = widget_button( sbase, value='Save Maia Parameters', uname='save-maia', /tracking, uvalue='Save the main Maia parameters (Hermes, Scepter, Hymod) to file (extension ".parameters.csv"). Exceptions: ' + $
					'Channel disable parameters (HERMES ech) are saved separately from the "Enable" panel, (ii) Group definitions are saved from the "Spectra" panel, and (iii) Throttle parameters are specific to a measurement ' + $
					'and are loaded and set when needed from the "HYMOD" page.')
button = widget_button( sbase, value='Load and Apply Maia Parameters', uname='load-maia', /tracking, uvalue='Load the main Maia parameters (Hermes, Scepter, Hymod) from a ".parameters.csv" file and Apply ' + $
					'them to Maia. Exceptions: (i) Channel disable parameters (HERMES ech) are saved separately from the "Enable" panel, (ii) Group definitions are ' + $
					'saved from the "Spectra" panel, and (iii) Throttle parameters are saved from the "HYMOD" page.')

;------------------------------------------------------------------------------------------------


rbase = widget_base( tbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

tab_panel2 = widget_tab( rbase, location=3, /align_center, uname='detector-tab-panel')	;, $
;					NOTIFY_REALIZE='OnRealize_maia_setup_preview_tab')

detector_base = widget_base( tab_panel2, title='  Detector Mimic Panel  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=right_xsize+4, scr_ysize=right_ysize)

detector = detector_mimic( detector_base, data=pd, uname='detector', uvalue='Select detectors(s) by ' + $
					'clicking on individual detector pads, or members of groups of pads or Classes. Select a grouping class using the ' + $
					'"Detector Selection Class" droplist. If the "Detector Selection Class" mode is not in "Individual" all members of a class ' + $
					'will be selected together. Click again to deselect.', /tracking, legend=4, position=((n_detectors eq 384) ? 0 : 1), $
					xsize_min=right_xsize2, xsize_max=right_xsize2, ysize_max=right_ysize2, csize=csize, colours=colours)

figure_base = widget_base( tab_panel2, title='  Detector Layout Diagram  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=right_xsize, scr_ysize=right_ysize)

picpath = geopixe_root + 'maia' + slash()
letters = ['','A','B','C','D','E','F','G','H','I','J','K','L','M']
prefix = 'Maia_' + str_tidy((*pm).n_detectors) + letters[ (*pm).version.dam ]
fig = prefix + '-layout.jpg'
pic = picture_button( figure_base, picpath+fig, uname='figure', xsize=right_xsize2-2, ysize=right_ysize2, $
			/tracking, uvalue='Layout of the Maia detector and Hermes assignments, as viewed from the "Sample side".', pushbutton_events=0)

data_flow_base = widget_base( tab_panel2, title='  Data-Flow Diagram  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=right_xsize, scr_ysize=right_ysize)

pic = picture_button( data_flow_base, picpath+'HYMOD-data-flow.png', uname='data-flow', xsize=right_xsize-2, ysize=right_ysize, $
			/tracking, uvalue='Layout of HYMOD processor data-flow for the Maia detector.', pushbutton_events=0)

system_layout_base = widget_base( tab_panel2, title='  System Layout  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=right_xsize, scr_ysize=right_ysize)

pic = picture_button( system_layout_base, picpath+'Maia-layout.png', uname='system-layout', xsize=right_xsize-2, ysize=right_ysize, $
			/tracking, uvalue='Overview layout of Maia detector system.', pushbutton_events=0)

mbase = widget_base( rbase, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
mimic_toggle = cw_bgroup2( mbase, ['Set Parameters','Show Parameters'], /row, $
					/exclusive, /no_release, /return_index, /tracking, $
					uname='mimic-toggle', set_value=0, xpad=0, ypad=0, space=0, $
					uvalue='Select the mimic display mode between "Set" and "Display" parameters. ' + $
							'"Show Parameters" uses false colour to show parameters across the array. ' + $
							'"Set Parameters" allows setting parameters for detectors channels in various groupings, established using the droplist to the right.')

colour_display_text = widget_text( mbase, value='', uname='colour-display', /tracking, editable=0, $
					notify_realize='onrealize_maia_setup_detector', $
					uvalue='Shows the name of the parameter displayed across the detector array in "Colour parameter" mode.', scr_xsize=160)

label = widget_label( mbase, value=' ')
label = widget_label( mbase, value='Selection Class:')
detector_mode = widget_combobox(mbase, uname='select-mode', scr_xsize=120, $
			NOTIFY_REALIZE='OnRealize_maia_setup_detector_mode', $
			value=['One only','Individual','Radial','Column','Row','Chip','Quadrant','All'], /tracking, uvalue='Select the grouping of detector pads activiated on a mouse click.')

button = widget_button( mbase, value='Clear', uname='clear-select', /tracking, uvalue='Clear all detector selections.', $
			NOTIFY_REALIZE='OnRealize_maia_setup_clear')
button = widget_button( mbase, value='Get', uname='get-select', /tracking, uvalue='Set detector selections based on detectors present in a .SPEC file or a .select.csv table.')
button = widget_button( mbase, value='Save', uname='save-select', /tracking, uvalue='Save detector selections to a .select.csv table file.')

hbase = widget_base( tlb, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
help = widget_text( hbase, scr_xsize=help_xsize-115, ysize=3, /wrap, uname='help', /tracking, $
				uvalue='Help text ...', frame=0)
pic = picture_button( hbase, picpath+'Maia-Logo2.png', tracking=0, pushbutton_events=0)	;, xsize=144, ysize=48

;print,'maia_setup copy state ...'
														; make local copy for enables, sliders, etc.
copy_pointer_data, pm, pl, /init						; See notes at top of Event routine.
									
state = { $
		path:				ptr_new(path), $			; pointer to current path
		dpath:				ptr_new(dpath), $			; pointer to current raw data blog path
		default:			default, $					; Maia defaults
		pimage:				pimage, $					; pointer to DA image
		ppspec:				ppspec, $					; pointer to E spectra
		ppgroup:			ppgroup, $					; pointer to Group spectra
		playout:			pd, $						; pointer to layout struct array
		pmaia:				pm, $						; pointer to maia parameters struct array
		plocal:				pl, $						; pointer to local copy of maia parameters struct (for enables, etc.)
		psocket:			ps, $						; pointer to maia socket port and command parameters
		pdisable:			pe, $						; pointer to maia disable detector channels
		preadout:			pr, $						; pointer to readout enables
		plaunch:			ptr_new(/allocate_heap), $	; pointer data for use with Notify to Launch
		pshrmem_da:			pshrmem_da, $				; pointer to DA shared memory (from Maia_Launch)
		pelk:				ptr_new(fltarr(n_detectors)), $		; pointer to ELK map data
		
		select:				*pe, $						; current selection state
		asic_select:		intarr(n_detectors), $		; asic selection state
		groups_select:		intarr(n_detectors), $		; groups selection state
		pulser_select:		intarr(n_detectors), $		; pulser selection state
		debug_select_ean:	intarr(n_detectors), $		; debug selection state for EAN
		debug_select_elk:	intarr(n_detectors), $		; debug selection state for ELK
		debug_select_aux:	intarr(n_detectors), $		; debug selection state for AUX
		tab:				0, $						; tab selection
		sequence:			{on:0, count:0}, $			; sequence controls
		colour_vector:		fltarr(n_detectors), $		; store last parameter vector for colouring pads
		colour_title:		'', $						; store last title
		last_check:			0, $						; last check box used for colour mode
		pseed:				ptr_new(1L), $				; seed for Randomu
		scr_xsize:			0, $						; TLB X size
		scr_ysize:			0, $						; TLB Y size
		evt_file:			'', $						; default blog data file to scan for PVs
		
		enable_rr_freeze:	0, $						; make RR phase check boxes insensitive for a cycle
		enable_quad_freeze:	0, $						; make Quad phase check boxes insensitive for a cycle
		enable_modules_freeze:	0, $					; make Modules phase check boxes insensitive for a cycle
		enable_stream_freeze:	0, $					; make Streams phase check boxes insensitive for a cycle		
		colour_mode:		0, $						; set/colour radio mode
		cal_mode:			0, $						; energy cal mode
		group_mode:			1, $						; group select droplist mode
		group_mask:			replicate(1,8), $			; controls which group_modes are legal at any time
		old_mask:			[0,0,0,0,0,1,1,1], $		; save the last mask used in Hermes, Scepter panels
		group_row:			0, $						; group row selected in spectra group table
		check_mode:			0, $						; currently selected check-box
		hermes:	{ time:		0, $						; Hermes peaking time (time) droplist mode
			gain:			0, $						; hermes gain (gain) droplist mode
			eblk:			0, $						; Hermes bias leakage control (eblk) droplist mode
			elk:			0}, $						; Hermes leakage monitor (elk) droplist mode
		scepter: { tdm:		0, $						; scepter timing mode (tdm) droplist mode
			tds:			0, $						; scepter TAC slope (tds) droplist mode
			tos:			0, $						; scepter TAC timeout (tos) droplist mode
			trk:			0, $						; scepter simultaneous catching mode (trk) droplist mode
			trke:			0, $						; scepter ehhanced simultaneous catching mode (trke) droplist mode
			tcm:			0, $						; scepter glitch suppress setting
			filt:			0, $						; scepter additional filtering
			trim:			0.0, $						; scepter trim DAC slider value
			thpd:			0.0, $						; scepter adj trim slider value
			thresh:			0.5, $						; scepter threshold DAC slider value
			clock:			3.0, $						; scepter RR clock slider value
			tweak: { trim: {on:		0, $				; scepter trim tweak on
							val:	0.0}, $				; scpter trim tweak val
					thresh: {on:	0, $				; scepter threshold tweak on
							val:	0.0}}}, $			; scepter threshold tweak val
		pulser: { mode:		0, $						; pulser mode
			on:				0, $						; pulser ON
			low:			0.0, $						; pulser low amp
			high:			0.0, $						; pulser high amp
			rate:			1000, $						; pulser rate
			count:			20, $						; pulser peak count
			time:			1.0 }, $					; pulser dwell time (s)
		pHYMOD_debug:		ptr_new(/allocate_heap), $	; pulser, synth, debug EAN, AUX, EBLK, Bake (from Maia Launch)
		table: 	replicate({	chip:		0, $			; table chip index for a column of the table
							par:		0, $			; table chip parameter index
							column:		'', $			; column name
							check:		0 }, 40), $		; index from column into check_id list
		sort:	{ column:	0, $						; sort by column
					index:	indgen(n_detectors)}, $		; index from table row to CSV index
					
		start_text:			start_text, $				; start button text strings
		debug_ean_mode:		0, $						; current EAN debug mode
		debug_aux_mode:		0, $						; current AUX debug mode
		debug_monitor_mode:	0, $						; current DAM Monitor debug mode
		debug_ean_check:	0, $						; debug EAN fields selection
		debug_aux_check:	0, $						; debug AUX fields selection
		tracking:			1, $						; tracking mode
		peltier_mode:		0, $						; state of the heat/cool droplist
		
		tlb:				tlb, $						; TLB ID
		check_ids:			check_ids, $				; check box IDs
		class_check_ids:	class_check_ids, $			; class box area check IDs
		tab_panel:			tab_panel, $				; tab ID
		tab_names:			tab_names, $				; tab names
		display_toggle:		mimic_toggle, $				; display mode toggle 
		detector:			detector, $					; detector mimic ID
		detector_mode:		detector_mode, $			; detector selection mode ID
		colour_display_text: colour_display_text, $		; colour display text ID
		warning_pulser_base: warning_pulser_base, $		; warning pulser on button base ID
		warning_debug_base: warning_debug_base, $		; warning debug on button base ID
		warning_eblk_base:	warning_eblk_base, $		; warning EBLK on button base ID
		warning_bake_base:	warning_bake_base, $		; warning Bake on button base ID
		
		summary_table:		summary_table, $			; parameter summary table ID
		enable_modules_check: enable_modules_check, $	; check box ID
		enable_rr_check:	enable_rr_check, $			; check box ID for RR phases
		enable_quad_check:	enable_quad_check, $		; check box ID for Quanrant enables
		enable_stream_check: enable_stream_check, $		; check box ID for stream enables

		hermes_time_mode:	hermes_time_mode, $			; hermes time mode ID
		hermes_gain_mode:	hermes_gain_mode, $			; hermes time mode ID
		hermes_eblk_mode: 	hermes_eblk_mode, $			; hermes bias leakage control ID
		hermes_elk_mode:	hermes_elk_mode, $			; hermes bias leakage monitor ID
		hermes_apply_button: hermes_apply_button, $		; hermes Apply button ID
		hermes_detector_text: hermes_detector_text, $	; hermes set from detector text ID
		hermes_parameter_text: hermes_parameter_text, $ ; hermes apply to parameters text ID
		check_ids_hermes_channel: check_ids_hermes_channel, $ ; hermes check IDs by channel
		check_ids_hermes_chip: check_ids_hermes_chip, $	; hermes check IDs by chip
		check_ids_hermes_whole: check_ids_hermes_whole, $ ; hermes check IDs for whole array
		ids_hermes_channel:	ids_hermes_channel, $		; hermes parameter IDs by channel
		ids_hermes_chip: 	ids_hermes_chip, $			; hermes parameter IDs by chip
		ids_hermes_whole:	ids_hermes_whole, $			; hermes parameter IDs for whole array
		ids_hermes:			ids_hermes, $				; all hermes parameter IDs
		
		scepter_tdm_mode:	scepter_tdm_mode, $			; scepter time mode ID
		scepter_tds_mode:	scepter_tds_mode, $			; scepter TAC slope ID
		scepter_tos_mode:	scepter_tos_mode, $			; scepter TAC timeout ID
		scepter_trk_mode:	scepter_trk_mode, $			; scepter simult event catching mode ID
		scepter_trke_mode:	scepter_trke_mode, $		; scepter enhanced simult event catching mode ID
		scepter_trim_slider: scepter_trim_slider, $		; scepter threshold trim DAC slider ID
		scepter_thresh_slider: scepter_thresh_slider, $	; scepter threshold DAC slider ID
		scepter_rr_slider:	scepter_rr_slider, $		; scepter RR clock slider ID
		scepter_apply_button: scepter_apply_button, $	; scepter Apply button ID
		scepter_detector_text: scepter_detector_text, $	; scepter set from detector text ID
		scepter_parameter_text: scepter_parameter_text, $ ; scepter apply to parameters text ID
		scepter_thpd_slider:	scepter_thpd_slider, $	; scepter thpd threshold slider ID
		scepter_filt_mode:	scepter_filt_mode, $		; scepter filter mode ID
		scepter_tcm_mode:	scepter_tcm_mode, $			; scepter tcm glitch suppress mode ID
		check_ids_scepter_channel: check_ids_scepter_channel, $ ; scepter check IDs by channel
		check_ids_scepter_chip: check_ids_scepter_chip, $ ; scepter check IDs by chip
		ids_scepter_channel: ids_scepter_channel, $		; scepter parameters IDs by channel
		ids_scepter_chip:	ids_scepter_chip, $			; scepter parameter IDs by chip
		ids_scepter:		ids_scepter, $				; all scepter parameter IDs
		
		hymod_linearization_check: hymod_linearization_check, $ ; check box ID
		hymod_gain_trim_check: hymod_gain_trim_check, $ ; check box ID
		hymod_pileup_check:	hymod_pileup_check, $		; check box ID
		hymod_throttle_check: hymod_throttle_check, $	; check box ID
		hymod_linearization_text: hymod_linearization_text, $ ; hymod linearization file text ID
		hymod_gain_trim_text: hymod_gain_trim_text, $	; hymod gain trim file text ID
		hymod_gain_trim_text2: hymod_gain_trim_text2, $	; hymod gain trim file2 text ID
		hymod_pileup_text:	hymod_pileup_text, $		; hymod pileip file text ID
		hymod_cal_text:		hymod_cal_text, $			; hymod cal file text ID
		hymod_throttle_text: hymod_throttle_text, $		; hymod throttle file text ID
		hymod_cal_mode:		hymod_cal_mode, $			; energy cal mode
		hymod_deadtime_offset: hymod_deadtime_offset, $	; deadtime offset text ID
		hymod_deadtime_slope: hymod_deadtime_slope, $	; deadtime slope text ID
		hymod_deadtime_auto:	hymod_deadtime_auto, $	; checkbox for deadtime DTcalA handled automatically
		dam_version_text:	dam_version_text, $			; DAM board revision text ID
		ddm_version_text:	ddm_version_text, $			; DDM board revision text ID
		dbpm_version_text:	dbpm_version_text, $		; DBPM board revision text ID
		
		group_table:		group_table, $				; spectra "group" table ID
		group_title_text:	group_title_text, $			; spectra group title text ID
		spectra_file_text:	spectra_file_text, $		; spectra group file text ID
		spectra_enable_check: spectra_enable_check, $	; spectra enable check ID
		
		imaging_da_check:	imaging_da_check, $			; imaging DA check box ID
		imaging_roi_check:	imaging_roi_check, $		; imaging ROI check box ID
		auto_DA_check:		auto_DA_check, $			; auto save of RT images check box ID
		save_path_text:		save_path_text, $			; imaging realtime DA save path text ID
		imaging_da_text:	imaging_da_text, $			; imaging DA file text ID
		imaging_roi_text:	imaging_roi_text, $			; imaging ROI file text ID
		charge_mode:		charge_mode, $				; Charge mode droplist ID
		charge_conversion:	charge_conversion, $		; charge conversion text ID
		scan_button:		scan_button, $				; scan PVs button ID
		IC_base:			IC_base, $					; Ion chamber base ID
		IC_base1:			IC_base1, $					; Ion chamber base1 ID
		IC_base2:			IC_base2, $					; Conversion base ID
		ic_pv_mode:			ic_pv_mode, $				; IC PV selector ID
		ic_val_mode:		ic_val_mode, $				; IC value selector ID
		ic_unit_mode:		ic_unit_mode, $				; IC unit selector ID

		ic_vals:			ic_vals, $					; IC vals list
		ic_units:			ic_units, $					; IC units string list
		ic_vunits:			ic_vunits, $				; scale units for units list
		IC_base1_ysize:		0, $						; Y size of IC_base1
		IC_base2_ysize:		0, $						; Y size of IC_base2
		
		controls_detector_text: controls_detector_text, $	; controls set from detector text ID
		controls_parameter_text: controls_parameter_text, $ ; controls apply to parameters text ID
		controls_bias_text:	controls_bias_text, $		; controls bias readback text ID
		controls_leakage_text: controls_leakage_text, $	; controls detector leakage readback text ID
		controls_detector_temp_text: controls_detector_temp_text, $ ; controls detector Temp text ID
		controls_peltier_slider: controls_peltier_slider, $ ; controls peltier slider ID
		controls_peltier_monitor_text: controls_peltier_monitor_text, $	; controls Peltier current monitor text ID
		controls_peltier_supply_text: controls_peltier_supply_text, $	; controls Peltier supply V text ID
		controls_bias_slider: controls_bias_slider, $	; controls bias slider ID
		controls_guard_slider: controls_guard_slider, $	; guard voltage slider ID
		controls_temp_hermes_text: controls_temp_hermes_text, $	; controls Hermes T readback text ID
		controls_temp_water_text: controls_temp_water_text, $	; controls water T readback text ID
		controls_temp_coldtrap_text: controls_temp_coldtrap_text, $	; controls coldtrap T readback text ID
		controls_temp_mosfet_text: controls_temp_mosfet_text, $	; controls Mosfet T readback text ID
		controls_temp_fpga_text: controls_temp_fpga_text, $		; controls DDM FPGA T readback text ID
		controls_temp_hymod_fpga_text: controls_temp_hymod_fpga_text, $	; controls HYMOD FPGA T readback text ID
		controls_temp_hymod_cpu_text: controls_temp_hymod_cpu_text, $	; controls HYMOD CPU T readback text ID
		controls_ddm_vcc_text: controls_ddm_vcc_text, $	; DDM Vcc test ID
;		controls_linkloss_status: controls_linkloss_status, $ ; controls linkloss status state_button ID
;		controls_interlock_status: controls_interlock_status, $ ; controls interlock status state_button ID
		controls_oan_text:	controls_oan_text, $		; OAN monitor text box ID
		controls_aux_text:	controls_aux_text, $		; AUX monitor text box ID
		controls_monitor_text:	controls_monitor_text, $ 	; DAM monitor ADC text box ID
		bake_button_base:	bake_button_base, $			; map base ID for bakeout warning
		bake_button:		bake_button, $				; bake button ID for bakeout warning
		controls_peltier_mode:		controls_peltier_mode, $	; Peltier Cool/Heat mode ID
		monitor_modes:		monitor_modes, $			; DAM monitor ADC modes

		pulser_program_mode: pulser_program_mode, $		; pulser mode ID
		pulser_start_base:	pulser_start_base, $		; pulser start button base ID
		pulser_start_button: pulser_start_button, $		; pulser start button ID
		pulser_low_label:	pulser_low_label, $			; pulser low label ID
		pulser_low_text:	pulser_low_text, $			; pulser low text ID
		pulser_high_base:	pulser_high_base, $			; pukser high text base ID
		pulser_high_label:	pulser_high_label, $		; pulser high label ID
		pulser_high_text:	pulser_high_text, $			; pulser high text ID
		pulser_rate_text:	pulser_rate_text, $			; pulser rate text ID
		pulser_count_base:	pulser_count_base, $		; pulser count base ID
		pulser_count_text:	pulser_count_text, $		; pulser count text ID
		pulser_time_text:	pulser_time_text, $			; pulser time text ID
		pulser_time_base:	pulser_time_base, $			; pulser time base ID
		pulser_rate_base:	pulser_rate_base, $			; pulser rate base ID
		pulser_progress_base: pulser_progress_base, $	; pulser progress base ID
		pulser_list: 		pulser_list, $				; pulser script list ID
		
		edit_id_text:		edit_id_text, $				; edit Number text ID 
		edit_n_in_chip_text: edit_n_in_chip_text, $		; number of detector within chip text ID
		edit_hermes_text:	edit_hermes_text, $			; edit hermes text ID
		edit_quadrant_text:	edit_quadrant_text, $		; edit quadrant text ID
		edit_radial_text:	edit_radial_text, $			; edit radial text ID
		edit_column_text:	edit_column_text, $			; edit column text ID
		edit_row_text:		edit_row_text, $			; edit row text ID
		edit_sequence_button: edit_sequence_button, $	; edit sequence button ID
		edit_pitch_text:	edit_pitch_text, $			; edit pitch text ID
		edit_index_text:	edit_index_text, $			; edit index text ID
		edit_width_text:	edit_width_text, $			; edit width text ID
		edit_height_text:	edit_height_text, $			; edit height text ID
		
		enable_debug_ean_mode:	enable_debug_ean_mode, $	; debug EAN droplist ID
		enable_debug_aux_mode:	enable_debug_aux_mode, $	; debug AUX droplist ID
		enable_debug_monitor_mode:	enable_debug_monitor_mode, $	; debug DAM Monitor droplist ID
		debug_ean_check_id:	debug_ean_check_id, $			; debug EAN fields selection
		debug_aux_check_id:	debug_aux_check_id, $			; debug AUX fields selection
		
		help:				help $						; help text ID
		}

print,'maia_setup realize ...'
child = widget_info( tlb, /child)
pstate = ptr_new(state, /no_copy)
widget_control, child, set_uvalue=pstate
widget_control, tlb, /realize
geo = widget_info( tlb, /geometry)
;help, geo, /str
(*pstate).scr_xsize = geo.scr_xsize
(*pstate).scr_ysize = geo.scr_ysize

register_notify, tlb, ['path', 'dpath', $		; new paths
						'detector-toggle', $	; detector right-click toggle on rates mimic display
						'maia-display'], $		; update table with new Maia values
						from=group

xmanager, 'maia_setup', tlb, /no_block
print,'maia_setup running ...'
end
		