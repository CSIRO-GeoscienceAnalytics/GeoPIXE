pro daq_setup_event, event

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
		warning,'daq_setup_event',['IDL run-time error caught.', '', $
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
; DAQ via the Kandinski socket. The local copy *pl is made from *pm when
; (i)   a new Tab is selected, or
; (ii)  when Setup is first started. 
; 
; Certain parameters are changed locally (using the *pl copy), such as 
; (i)   enable checkboxes on all panels (except the "Enable" panel which is live). 
;       The enables modify the local *pl values and send these to DAQ, only
;       when "Apply" is used. 
; (ii)  the Bias, Guard and Peltier sliders on the "Control" panel. 
;       These are sent immediately when the slider are used. They are read back 
;       periodically as *pm values.
; (iii) the Group table data uses the *pl copy to load the group table.
;       Data are read into *pl with the 'read' button. They can be modified there 
;       and then Applied to DAQ.
; (iv)	the Hymod and Imaging panel file names.
;       
; NOTE: On "Apply" make sure that the specific parameters (local to this Tab) are copied
;       from *pl back into *pm, unless they will be read back from Kandinski.
;       
; The copy is made in 'daq-tab-panel' event code. Care is taken in copying from *pm to *pl
; to preserve the plist pointers in both. Hymod panel enable checkboxes and files are set
; from *pm in the 'daq_setup_update_dynamic' routine. The Enable panel enables are only
; set after the update from DAQ (in Notify code).
;
; Detector array:
;	CSV table index is used for elements of the detector array, such as in the
;	'select' vector. To convert this to detector number, use the lookup table:
;
;	(*(*pstate).playout).data.index			CSV table index to detector number
;	(*(*pstate).playout).ref				detector number to CSV table index
;	(*pstate).sort.index					re-ordered (sorted) CSV table index in Summary

pm = (*pstate).pdaq
pl = (*pstate).plocal
ps = (*pstate).psocket
pr = (*pstate).preadout
play = (*pstate).playout

n_detectors = (*pm).n_detectors
status = {on:0, mask:bytarr(n_detectors), text:''}
version = (*pm).version.software
obj  = (*pm).DevObj

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
			widget_control, (*pstate).help, set_value='Select functional groups of DAQ parameters using the Tabs on the left, and use the mimic display on the right to either "Set parameters" or display parameters across the array using "Show Parameters". ' + $
					'Many operations in the left panels use the currently selected detector channels as targets. The controls on the left will be set to the parameters for the detector you click on.'
		endelse
		goto, finish
		end
	'WIDGET_TIMER': begin
		u = widget_info( event.id, /uname)
		case u of
			'pulser-start': begin
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
			'daq-display': begin
				daq_setup_load_table, pstate
				
				if (*pstate).enable_quad_freeze eq 0 then sensitive=1 else sensitive=0
				widget_control, (*pstate).enable_quad_check, set_value=(*(*pstate).preadout).quad_enable, sensitive=sensitive
				(*pstate).enable_quad_freeze = ((*pstate).enable_quad_freeze-1) > 0
				
				if (*pstate).enable_stream_freeze eq 0 then sensitive=1 else sensitive=0
				widget_control, (*pstate).enable_stream_check, set_value=[(*pr).scepter, (*pr).event, (*pr).photon, $
										(*pr).activity, (*pm).scan.on], sensitive=sensitive
				(*pstate).enable_stream_freeze = ((*pstate).enable_stream_freeze-1) > 0

				widget_control, (*pstate).controls_temp_hymod_fpga_text, set_value=str_tidy((*pm).control.temp.fpga, length=5)+' C'
				widget_control, (*pstate).controls_temp_hymod_cpu_text, set_value=str_tidy((*pm).control.temp.cpu, length=5)+' C'
				widget_control, (*pstate).controls_temp_hymod_board_text, set_value=str_tidy((*pm).control.temp.board, length=5)+' C'
				widget_control, (*pstate).controls_temp_hymod_eth0_text, set_value=str_tidy((*pm).control.temp.eth0, length=5)+' C'
				widget_control, (*pstate).controls_temp_hymod_eth1_text, set_value=str_tidy((*pm).control.temp.eth1, length=5)+' C'

;				debug = *event.pointer
;;				help, debug, /str
;				*(*pstate).pHYMOD_debug = debug
;				widget_control, (*pstate).warning_pulser_base, map=debug.pulser or debug.synth
;				widget_control, (*pstate).warning_debug_base, map=debug.hermes or debug.scepter
;				
;;				This is already done in the 'change tabs' code ...	
;;					Ignore the pulser terms here.
;;					Or, ignore here only if in Pulser tab mode?
;;					
;;				debug = *(*pstate).pHYMOD_debug			
;;				(*pstate).pulser.on = 0
;;				if debug.pulser then begin
;;					if ((*pstate).pulser.mode ne 2) and ((*pstate).pulser.mode ne 3) then (*pstate).pulser.mode = 1
;;					(*pstate).pulser.on = 1
;;				endif
;;				if debug.synth then begin
;;					(*pstate).pulser.mode = 4
;;					(*pstate).pulser.on = 1
;;				endif
;				if ((*pstate).tab_names[(*pstate).tab] ne 'Debug') then begin
;					(*pstate).debug_aux_mode = debug.scepter
;				endif
;				daq_setup_check_warning, pstate
				end
			else:
		endcase
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request daq_setup ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'daq-setup-tlb': begin
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

	'daq-tab-panel': begin
		if ((*pstate).tab_names[(*pstate).tab] eq 'Scepter') then begin
			(*pstate).asic_select = (*pstate).select
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Pulser') then begin
			(*pstate).pulser_select = (*pstate).select
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Debug') then begin
			if (*pstate).debug_aux_check ne 0 then (*pstate).debug_select_aux = (*pstate).select
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Enable') then begin
			*(*pstate).pdisable = (*pstate).select
			(*pstate).select = 0
			widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:0}
		endif 

		(*pstate).tab = event.tab

		daq_setup_update_dynamic, pstate				; set enables and detector sliders from *pm
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
			'Scepter': begin		; scepter
				(*pstate).group_mask = (*pstate).old_mask
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
				if ((*pstate).debug_aux_check eq 0) then begin
					if (*pstate).debug_aux_mode ne 0 then begin
						(*pstate).debug_aux_check = 1
					endif
				endif

				widget_control, (*pstate).debug_aux_check_id, set_value=(*pstate).debug_aux_check
				widget_control, (*pstate).enable_debug_aux_mode, set_combobox_select=(*pstate).debug_aux_mode, sensitive=(*pstate).debug_aux_check
				end
			else:
		endcase
		
		if ((*pstate).tab_names[(*pstate).tab] eq 'Scepter') then begin
			(*pstate).select = (*pstate).asic_select
			if (*pstate).colour_mode eq 0 then widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Pulser') then begin
			(*pstate).select = (*pstate).pulser_select
			if (*pstate).colour_mode eq 0 then widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Debug') then begin
			if (*pstate).debug_aux_check ne 0 then (*pstate).select = (*pstate).debug_select_aux
		if (*pstate).colour_mode eq 0 then widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
		endif else if ((*pstate).tab_names[(*pstate).tab] eq 'Enable') then begin
			widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*(*pstate).pdisable), alt:1}
			(*pstate).select = *(*pstate).pdisable
		endif 
		
		daq_setup_check_group_mode, pstate
		(*pstate).last_check = (*pstate).check_mode
		widget_control, (*pstate).display_toggle, set_value=(*pstate).colour_mode
		if (*pstate).colour_mode eq 1 then begin
			daq_setup_colour, pstate, /redisplay
		endif else begin
			widget_control, (*pstate).detector, set_value={legend:['','','','']}
		endelse
		widget_control, (*pstate).scepter_apply_button, sensitive=1-(*pstate).colour_mode
		widget_control, (*pstate).detector_mode, sensitive=1-(*pstate).colour_mode
		widget_control, (*pstate).colour_display_text, sensitive=(*pstate).colour_mode
		end

	'apply-daq-enable': begin
		(*(*pstate).pdisable)[*] = (*pstate).select
		socket_command_set, ps, 'enable', 1-(*(*pstate).pdisable), class='photon.chan', chip=-1, n_chips=n_detectors
		end
		
	'read-enable': begin
		widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*(*pstate).pdisable), alt:1}
		(*pstate).select = *(*pstate).pdisable
		end
		
	'save-enable': begin
		(*(*pstate).pdisable)[*] = (*pstate).select
		file = strip_file_ext((*pm).file,/double) + '.enable.var'
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'daq'
		F = file_requester( /write, filter = ['*.enable.var','*.enable.csv'], file=file, $
			path=path, group=event.top, $
			title='Select the Enable var file', fix_filter=1)
		if F ne '' then begin
			write_daq_enable, F, data=*(*pstate).pdisable, index=(*play).ref, error=error
			if error eq 0 then *(*pstate).path = path
		endif
		end
		
	'load-enable': begin
		file = strip_file_ext((*pm).file,/double) + '.enable.var'
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'daq'
		F = file_requester( /read, filter = ['*.enable.var','*.enable.csv'], file=file, $
			path=path, group=event.top, $
			title='Select the Enable var file', fix_filter=1)
		if F ne '' then begin
			disable = read_daq_enable( F, index=(*play).data.index, error=err)
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
		
	'enable-quad-check': begin
		widget_control, (*pstate).enable_quad_check, sensitive=0
		socket_command_set, ps, 'enable', event.select, class='scepter.clock'
		(*pstate).enable_quad_freeze = 1
		end
		
	'enable-stream-check': begin
		widget_control, (*pstate).enable_stream_check, sensitive=0
		(*pstate).enable_stream_freeze = 1
		case event.value of
			0: begin
				socket_command_set, ps, 'ENABLE', event.select, class='scepter'
				end
			1: begin
				socket_command_set, ps, 'ENABLE', event.select, class='event.blog'
				end
			2: begin
				socket_command_set, ps, 'ENABLE', event.select, class='photon'
				end
			3: begin
				socket_command_set, ps, 'ENABLE', event.select, class='activity.blog'
				end
			4: begin
				socket_command_set, ps, 'ENABLE', event.select, class='pixel'
				end
			else:
		endcase
		end

	'scepter-rr-slider': begin
		(*pstate).scepter.clock = event.value
		widget_control, (*pstate).check_ids_scepter_whole[0], set_value=1
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
			(*pstate).scepter.tweak.trim.val = (*pstate).scepter.tweak.trim.val + 0.005
			widget_control, (*pstate).scepter_trim_slider, set_value=(*pstate).scepter.trim + (*pstate).scepter.tweak.trim.val
		endif
		end

	'scepter-trim-up': begin
		if event.select eq 1 then begin
			widget_control, (*pstate).check_ids_scepter_channel[0], set_value=1
			if (*pstate).scepter.tweak.trim.on eq 0 then (*pstate).scepter.tweak.trim.val = 0.0
			(*pstate).scepter.tweak.trim.on = 1
			(*pstate).scepter.tweak.trim.val = (*pstate).scepter.tweak.trim.val - 0.005
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
			(*pstate).scepter.tweak.thresh.val = (*pstate).scepter.tweak.thresh.val + 0.005
			widget_control, (*pstate).scepter_thresh_slider, set_value=(*pstate).scepter.thresh + (*pstate).scepter.tweak.thresh.val
		endif
		end

	'scepter-thresh-down': begin
		if event.select eq 1 then begin
			widget_control, (*pstate).check_ids_scepter_chip[7], set_value=1
			if (*pstate).scepter.tweak.thresh.on eq 0 then (*pstate).scepter.tweak.thresh.val = 0.0
			(*pstate).scepter.tweak.thresh.on = 1
			(*pstate).scepter.tweak.thresh.val = (*pstate).scepter.tweak.thresh.val - 0.005
			widget_control, (*pstate).scepter_thresh_slider, set_value=(*pstate).scepter.thresh + (*pstate).scepter.tweak.thresh.val
		endif
		end

	'scepter-thpd-slider': begin
		(*pstate).scepter.thpd = event.value
		widget_control, (*pstate).check_ids_scepter_chip[9], set_value=1
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
		daq_setup_check_group_mode, pstate
		daq_setup_sensitive, pstate, scepter=1
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
		daq_setup_check_group_mode, pstate
		daq_setup_sensitive, pstate, scepter=2
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
			daq_setup_colour, pstate, (*pm).channel.scepter.tdm, title=u.label
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
			daq_setup_colour, pstate, (*pm).channel.scepter.tds, title=u.label
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
			daq_setup_colour, pstate, (*pm).channel.scepter.tos, title=u.label
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
			daq_setup_colour, pstate, (*pm).channel.scepter.trk, title=u.label
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
			daq_setup_colour, pstate, (*pm).channel.scepter.trke, title=u.label
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
			daq_setup_colour, pstate, (*pm).channel.scepter.filt, title=u.label
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
			daq_setup_colour, pstate, (*pm).channel.scepter.tcm, title=u.label
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
			daq_setup_colour, pstate, (*pm).channel.scepter.trim, title=u.label
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
			daq_setup_colour, pstate, (*pm).channel.scepter.thresh, title=u.label
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
			daq_setup_colour, pstate, (*pm).channel.scepter.thpd, title=u.label
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
			daq_setup_colour, pstate, (*pm).channel.scepter.clock, title=u.label
		endif
		end
		
	'scepter-apply': begin
		q = where((*pstate).select[0:31] eq 1,nq)
		if nq eq 0 then begin
			warning,'daq_setup_event',['No detectors are selected.','Select detectors and try again.']
			goto, finish
		endif
		c = (*play).data[q].index mod 32			; channel on chip for index list 'q'
;		h = (*play).data[q].hermes					; Hermes chip for these 'q'
		none = 1
		
		widget_control, (*pstate).scepter_tdm_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].scepter.tdm = (*pstate).scepter.tdm
				socket_command_set, ps, 'TDM', (*pm).channel[q[0]].scepter.tdm, class='scepter'		;, chip=h
				none = 0
			endif
		endif
		widget_control, (*pstate).scepter_tds_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].scepter.tds = (*pstate).scepter.tds
				socket_command_set, ps, 'TDS', (*pm).channel[q[0]].scepter.tds, class='scepter'		;, chip=h
				none = 0
			endif
		endif
		widget_control, (*pstate).scepter_tos_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].scepter.tos = (*pstate).scepter.tos
				socket_command_set, ps, 'TOS', (*pm).channel[q[0]].scepter.tos, class='scepter'		;, chip=h
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
						(*pm).channel[q].scepter.trim = (*pm).channel[q].scepter.trim + (*pstate).scepter.tweak.trim.val
						socket_command_set, ps, 'trim', -(*pm).channel[q].scepter.trim, class='scepter', channel=c	;, chip=h
					endif else begin
						(*pm).channel[q].scepter.trim = (*pstate).scepter.trim
						socket_command_set, ps, 'trim', -(*pm).channel[q[0]].scepter.trim, class='scepter', channel=c	;, chip=h
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
					socket_command_set, ps, 'thpd', -(*pm).channel[q[0]].scepter.thpd, class='scepter'		;, chip=h
					none = 0
				endif
			endif
		endif
		widget_control, (*pstate).scepter_thresh_slider, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				if (*pstate).scepter.tweak.thresh.on then begin
					(*pm).channel[q].scepter.thresh = (*pm).channel[q].scepter.thresh + (*pstate).scepter.tweak.thresh.val
					for i=0,n_elements(q)-1 do begin
						socket_command_set, ps, 'thresh', (*pm).channel[q[i]].scepter.thresh, class='scepter'	;, chip=h[i]
					endfor
				endif else begin
					(*pm).channel[q].scepter.thresh = (*pstate).scepter.thresh
					socket_command_set, ps, 'thresh', (*pm).channel[q[0]].scepter.thresh, class='scepter'		;, chip=h
				endelse
				none = 0
			endif
		endif
		widget_control, (*pstate).scepter_trk_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].scepter.trk = (*pstate).scepter.trk
				socket_command_set, ps, 'TRK', (*pm).channel[q[0]].scepter.trk, class='scepter'		;, chip=h
				none = 0
			endif
		endif
		widget_control, (*pstate).scepter_trke_mode, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].scepter.trke = (*pstate).scepter.trke
				socket_command_set, ps, 'TRKE', (*pm).channel[q[0]].scepter.trke, class='scepter'	;, chip=h
				none = 0
			endif
		endif
		if (*pm).version.scepter ge 6 then begin
			widget_control, (*pstate).scepter_tcm_mode, get_uvalue=u
			if widget_info((*pstate).check_ids[u.id],/valid) then begin
				widget_control, (*pstate).check_ids[u.id], get_value=on
				if on then begin
					(*pm).channel[q].scepter.tcm = (*pstate).scepter.tcm
					socket_command_set, ps, 'TCM', (*pm).channel[q[0]].scepter.tcm, class='scepter'		;, chip=h
					none = 0
				endif
			endif
			widget_control, (*pstate).scepter_filt_mode, get_uvalue=u
			if widget_info((*pstate).check_ids[u.id],/valid) then begin
				widget_control, (*pstate).check_ids[u.id], get_value=on
				if on then begin
					(*pm).channel[q].scepter.filt = (*pstate).scepter.filt
					socket_command_set, ps, 'FILT', (*pm).channel[q[0]].scepter.filt, class='scepter'	;, chip=h
					none = 0
				endif
			endif
		endif
		widget_control, (*pstate).scepter_rr_slider, get_uvalue=u
		if widget_info((*pstate).check_ids[u.id],/valid) then begin
			widget_control, (*pstate).check_ids[u.id], get_value=on
			if on then begin
				(*pm).channel[q].scepter.clock = (*pstate).scepter.clock
				socket_command_set, ps, 'clock.rate', (*pm).channel[q[0]].scepter.clock * 1.0e+6, class='scepter'
				none = 0
			endif
		endif
		if none then warning,'daq_setup_event',['No SCEPTER parameters are selected.','','Select detector channels on the right,', $
			'a bank of parameters on the left, ', 'and adjust one to try again.']

		for i=0L,n_elements((*pstate).check_ids)-1 do begin
			if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all check boxes
		endfor
		(*pstate).scepter.tweak.trim.on = 0
		(*pstate).scepter.tweak.thresh.on = 0
		daq_setup_load_table, pstate
		end
		
	'device-apply': begin
		daq_setup_apply_device, pstate
		end

	'load-cal': begin
		file = find_file2( (*pl).cal.file)
		if file[0] eq '' then file=strip_path((*pl).cal.file)
		path = extract_path( file[0])
		if lenchr(path) eq 0 then path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'daq'
		F = file_requester( /read, filter = '*.spec', file=file[0], $
			path=path, group=event.top, $
			title='Select the calibrated SPEC file', fix_filter=0)
		if F ne '' then begin
			set_widget_text, (*pstate).hymod_cal_text, F
			(*pl).cal.file = F
			*(*pstate).path = extract_path(F) 
		endif
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
						2: daq_setup_colour, pstate, (*pm).channel.scepter.((*pstate).table[i].par), title=title
						3: daq_setup_colour, pstate, (*pm).channel.cal.((*pstate).table[i].par), ignore=where( (abs((*pm).channel.cal.((*pstate).table[i].par)-1.) lt 0.0001) or (abs((*pm).channel.cal.((*pstate).table[i].par)) lt 0.0001)), title=title
						4: daq_setup_colour, pstate, (*pm).channel.trim.E.((*pstate).table[i].par), ignore=where( (abs((*pm).channel.Trim.E.((*pstate).table[i].par)-1.) lt 0.0001) or (abs((*pm).channel.Trim.E.((*pstate).table[i].par)) lt 0.0001)), title=title
						5: daq_setup_colour, pstate, (*pm).channel.trim.T.((*pstate).table[i].par), ignore=where( (abs((*pm).channel.Trim.T.((*pstate).table[i].par)-1.) lt 0.0001) or (abs((*pm).channel.Trim.T.((*pstate).table[i].par)) lt 0.0001)), title=title
						6: daq_setup_colour, pstate, (*play).data.((*pstate).table[i].par), title=title
						else:
					endcase
				endif
				if (event.sel_top eq 0) and (event.sel_bottom eq n_detectors-1) and (event.sel_left ge 0) then begin
					i = event.sel_left
					(*pstate).sort.column=i
					daq_setup_load_table, pstate
				endif
				end

			'WIDGET_TABLE_CH': begin
				end
			else:
		endcase
		end
		
	'beam-particle-mode': begin
		(*pl).beam.particle = (*pstate).beams[event.index]
		end
		
	'beam-energy': begin
		widget_control, event.id, get_value=s
		(*pl).beam.energy = float(s)
		end

	'charge-scale-mode': begin								; is this superceded?
		scales = ['0.01','0.1','1.0']
		(*pl).beam.charge.scale = scales[event.index]
		(*pl).beam.charge.unit = 'pC'
		end

	'hymod-apply': begin
		daq_setup_apply_hymod, pstate
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
			title='Select representative DAQ blog data', fix_filter=0 )
		if F[0] eq '' then goto, finish
		(*pstate).evt_file = F[0]
		*(*pstate).dpath = extract_path(F[0])
		
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
				widget_control, (*pstate).ic_pv_mode, set_value=*(*pl).IC.plist
			endif
			(*pl).IC.pv.name = IC_name
			q = where( (*pl).IC.pv.name eq *(*pl).IC.plist, nq)
			widget_control, (*pstate).ic_pv_mode, set_combobox_select=(nq ne 0) ? q[0] : 0
			(*pl).IC.pv.val = IC_val
			q = where( abs((*pl).IC.pv.val - (*pstate).ic_vals) lt 0.01, nq)
			widget_control, (*pstate).ic_val_mode, set_combobox_select=(nq ne 0) ? q[0] : 0
			(*pl).IC.pv.unit = IC_vunit
			q = where( abs(((*pl).IC.pv.unit - (*pstate).ic_vunits)/((*pstate).ic_vunits>0.001)) lt 0.01, nq)
			widget_control, (*pstate).ic_unit_mode, set_combobox_select=(nq ne 0) ? q[0] : 0
		endif else begin
			warning,'daq_setup','No Flux PV data found for this blog file.'
		endelse
		end

	'charge-conversion': begin
		widget_control, event.id, get_value=s
		(*pl).IC.conversion = float(s)
		end
		
	'ic-pv-mode': begin
		if ptr_good((*pl).IC.plist) then begin
			(*pl).IC.pv.name = (*(*pl).IC.plist)[event.index]
		endif
		end
	'ic-preamp-mode': begin
		(*pl).IC.pv.val = (*pstate).ic_vals[event.index]
		end
	'ic-preamp-unit-mode': begin
		(*pl).IC.pv.unit = (*pstate).ic_vunits[event.index]
		end
		
	'edit-id': begin
		if total((*pstate).select) gt 1 then begin
			warning,'daq_setup',['Only select a single detector pad to change the Number ID.','Try again.']
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
		daq_setup_load_table, pstate
		end
		
	'edit-hermes': begin
		widget_control, (*pstate).edit_hermes_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].hermes = fix(s)
		endfor
		daq_setup_load_table, pstate
		end
		
	'edit-quadrant': begin
		widget_control, (*pstate).edit_quadrant_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].quadrant = fix(s)
		endfor
		daq_setup_load_table, pstate
		end
		
	'edit-radial': begin
		widget_control, (*pstate).edit_radial_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].radial = fix(s)
		endfor
		daq_setup_load_table, pstate
		end
		
	'edit-set-radial': begin
		widget_control, (*pstate).edit_pitch_text, get_value=s
		pitch = float(s)
		r = sqrt( ((*play).data.x)^2 + ((*play).data.y)^2)
		class = round(r/pitch[0] - 1)
		(*play).data.radial = (class > 0) < 15
		print, (*play).data.radial
		daq_setup_load_table, pstate
		end
		
	'edit-pitch': begin
		daq_setup_load_table, pstate
		end
		
	'edit-column': begin
		widget_control, (*pstate).edit_column_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].column = fix(s)
		endfor
		daq_setup_load_table, pstate
		end
		
	'edit-row': begin
		widget_control, (*pstate).edit_row_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].row = fix(s)
		endfor
		daq_setup_load_table, pstate
		end
		
	'edit-width': begin
		widget_control, (*pstate).edit_width_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].width = float(s)
		endfor
		daq_setup_load_table, pstate
		end
		
	'edit-height': begin
		widget_control, (*pstate).edit_height_text, get_value=s
		for i=0L,n_detectors-1 do begin
			if (*pstate).select[i] then (*play).data[i].height = float(s)
		endfor
		daq_setup_load_table, pstate
		end
		
	'edit-read': begin
		daq_setup_read_layout, pstate			; read layout vars from Klee?
		end
		
	'edit-save': begin
		write_detector_layout, (*play).file, data=play
		daq_setup_load_table, pstate
		end
		
	'edit-revert': begin
		d = read_detector_layout((*play).file, maia=daq, error=error)
		if error then begin
			warning,'DAQ_Setup','Failed to read "'+(*play).file+'" to initialize layout.'
			goto, finish
		endif
		if daq eq 0 then begin
			warning,'DAQ_Setup','"'+(*play).file+'" file does not contain extended columns.'
			goto, finish
		endif
		*play = d
		widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:0, label:str_tidy((*play).data.index)} 
		daq_setup_load_table, pstate
		end		
		
	'edit-number-sequence': begin
		if (*pstate).sequence.on then begin
			(*pstate).sequence.on = 0
			widget_control, (*pstate).edit_sequence_button, set_value='Start Number Sequence'
			daq_setup_load_table, pstate
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
		daq_setup_check_warning, pstate
		daq_setup_pulser_map, pstate
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
						if abort then warning,'daq_setup_event',['No detectors are selected.','Select detectors and try again.']
						goto, finish
					endif
					c = (*play).data[q].index mod 32
					h = (*play).data[q].hermes
					widget_control, (*pstate).pulser_low_text, get_value=s
					low = float(s)
					if (strlen(s) eq 0) then begin
						if abort then warning,'daq_setup_event',['No Amplitude set.','Select a valid amplitude and try again.']
						goto, finish
					endif
					widget_control, (*pstate).pulser_rate_text, get_value=s
					rate = float(s)
					if (strlen(s) eq 0) or (rate lt 1) then begin
						if abort then warning,'daq_setup_event',['No Rate set.','Select a valid rate and try again.']
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
			4: begin								; synth pulser ON
				if On then begin
;					socket_command_set, ps, 'enable', 1, class='scepter'
					socket_command_set, ps, 'enable', 0, class='synth'
					q = where((*pstate).select eq 1,nq)
					if (nq eq 0) then begin
						if abort then warning,'daq_setup_event',['No detectors are selected.','Select detectors and try again.']
						goto, finish
					endif
					widget_control, (*pstate).pulser_low_text, get_value=s
					eAmp = long(s)
					if (strlen(s) eq 0) then begin
						if abort then warning,'daq_setup_event',['No E Amplitude set.','Select a valid amplitude and try again.']
						goto, finish
					endif
					widget_control, (*pstate).pulser_high_text, get_value=s
					tAmp = long(s)
					if (strlen(s) eq 0) then begin
						if abort then warning,'daq_setup_event',['No T Amplitude set.','Select a valid amplitude and try again.']
						goto, finish
					endif
					widget_control, (*pstate).pulser_rate_text, get_value=s
					rate = long(s)
					if (strlen(s) eq 0) or (rate lt 1) then begin
						if abort then warning,'daq_setup_event',['No Rate set.','Select a valid rate and try again.']
						goto, finish
					endif
					for i=0L,nq-1 do begin
						socket_command_set, ps, 'component', [eAMP,tAmp,(*play).data[q[i]].index], channel=-1, n_channels=3, class='synth.event', chip=i, n_chips=4096
					endfor
					socket_command_set, ps, 'event.count', nq, class='synth'
					socket_command_set, ps, 'rate', rate, class='synth'
					socket_command_set, ps, 'enable', 1, class='synth'
					socket_command_set, ps, 'enable', 1, class='event.blog'
				endif else begin
					socket_command_set, ps, 'enable', 0, class='synth'
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
		daq_setup_check_warning, pstate
		end
		
	'save-pulser': begin
		file='DAQ.pulser.csv'
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'daq'
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
			
			write_daq_pulser, F[0], select=(*pstate).select, pulser=(*pstate).pulser, error=error
		endif
		end
		
	'load-pulser': begin
		file='DAQ.pulser.csv'
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'daq'
		F = file_requester( /read, filter = '*.pulser.csv', file=file[0], $
			path=path, group=event.top, $
			title='Select a Pulser CSV file', fix_filter=1)
		if F ne '' then begin
			F = strip_file_ext(F,/double) + '.pulser.csv'
			
			sel = read_daq_pulser( F, pulser=pulser, error=err)
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
				daq_setup_check_warning, pstate
				daq_setup_pulser_map, pstate
			endif
		endif
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
		daq_setup_check_warning, pstate
		end
		
	'debug-aux-check': begin
		(*pstate).debug_aux_check = event.select
		if (*pstate).debug_aux_check then (*pstate).debug_ean_check = 0
		widget_control, (*pstate).enable_debug_ean_mode, sensitive=(*pstate).debug_ean_check
		widget_control, (*pstate).enable_debug_aux_mode, sensitive=(*pstate).debug_aux_check
		widget_control, (*pstate).debug_ean_check_id, set_value=(*pstate).debug_ean_check
		
		if (*pstate).debug_ean_check then (*pstate).select = (*pstate).debug_select_ean
		if (*pstate).debug_aux_check then (*pstate).select = (*pstate).debug_select_aux
		widget_control, (*pstate).detector, set_value={select:indgen(n_detectors), value:(*pstate).select, alt:0}
		end
	
	'debug-apply': begin
		if (*pstate).debug_ean_check then (*pstate).debug_select_ean = (*pstate).select
		if (*pstate).debug_aux_check then (*pstate).debug_select_aux = (*pstate).select
		daq_setup_update_debug, pstate		
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
		daq_setup_check_warning, pstate
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
	
			; Set all SCEPTER parameter controls to those for this detector ...
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
		widget_control, (*pstate).summary_table, set_table_select=[0,q[0],14,q[0]]
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
		
	'save-daq': begin
		file = (*pm).file
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'daq'
		F = file_requester( /write, filter = '*.parameters.csv', file=file, $
			path=path, group=event.top, $
			title='Select the DAQ Parameters CSV file', fix_filter=1)
		if F ne '' then begin
			F = strip_file_ext(F,/double) + '.parameters.csv'
			*(*pstate).path = extract_path(F)
			(*pm).cal.mode = (*pstate).cal_mode
			write_daq_parameters, F, data=pm, index=(*play).data.index, error=error
		endif	
		end
		
	'load-daq': begin
		file = (*pm).file
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'daq'
		F = file_requester( /read, filter = '*.parameters.csv', file=file, $
			path=path, group=event.top, title='Select the DAQ Parameters CSV file', fix_filter=1)
		if F ne '' then begin
			F = strip_file_ext(F,/double) + '.parameters.csv'
			err = read_daq_parameters( F, data=pm, index=dindex)
			if err eq 0 then begin
				*(*pstate).path = extract_path(F)
				(*play).data.index = dindex						; detector number
				(*pstate).cal_mode = (*pm).cal.mode
				widget_control, (*pstate).hymod_cal_mode, set_combobox_select=(*pstate).cal_mode
				daq_setup_load_table, pstate
				
				d = 32 * indgen( (*ps).n_detectors/32 )			; first detector on each chip
				q = (*play).ref[d]								; table index
				c = d mod 32									; channel number on chip
				h = (*play).data[q].hermes						; hermes chip number
				
				socket_command_set, ps, 'TIME', (*pm).channel[q].hermes.time, class='hermes', chip=h 
				socket_command_set, ps, 'GAIN', (*pm).channel[q].hermes.gain, class='hermes', chip=h
				socket_command_set, ps, 'EBLK', (*pm).channel[q].hermes.eblk, class='hermes', chip=h
;				socket_command_set, ps, 'ELK', (*pm).channel[q].hermes.elk, class='hermes', chip=h
				socket_command_set, ps, 'TDM', (*pm).channel[q].scepter.tdm, class='scepter', chip=h
				socket_command_set, ps, 'TDS', (*pm).channel[q].scepter.tds, class='scepter', chip=h
				socket_command_set, ps, 'TOS', (*pm).channel[q].scepter.tos, class='scepter', chip=h
				socket_command_set, ps, 'thresh', (*pm).channel[q].scepter.thresh, class='scepter', chip=h
				socket_command_set, ps, 'TRK', (*pm).channel[q].scepter.trk, class='scepter', chip=h
				socket_command_set, ps, 'TRKE', (*pm).channel[q].scepter.trke, class='scepter', chip=h
				socket_command_set, ps, 'clock.rate', (*pm).channel[0].scepter.clock * 1.0e+6, class='readout'

		; 		Note that thpd and trim are stored as positive, but set as negative values ...
		
				if (*pm).version.scepter ge 7 then begin
					socket_command_set, ps, 'thpd', -(*pm).channel[q].scepter.thpd, class='scepter', chip=h
				endif
				if (*pm).version.scepter ge 6 then begin
					socket_command_set, ps, 'TCM', (*pm).channel[q].scepter.tcm, class='scepter', chip=h
					socket_command_set, ps, 'FILT', (*pm).channel[q].scepter.filt, class='scepter', chip=h
					
					d = indgen( (*ps).n_detectors)					; all detectors
					q = (*play).ref[d]								; table index
					c = d mod 32									; channel number on chip
					h = (*play).data[q].hermes						; hermes chip number

					socket_command_set, ps, 'trim', -(*pm).channel[q].scepter.trim, class='scepter', chip=h, channel=c
				endif
				daq_launch_hardware_reset, ps, pm
				
				dt = (*pm).deadtime.cal
				obj->set_options, deadtime_cal=dt
				socket_command_set, ps, 'coeff', [dt.b,dt.a], class='deadtime.time', channel=-1, n_channels=2

				if (*pm).version.software ge 4737 then begin
					socket_command_set, ps, 'info', '"'+(*pm).linear.file+'"', class='linearise'
					socket_command_set, ps, 'info', '"'+(*pm).trim.file + ' ' + (*pm).trim.file2+'"', class='gaintrim'
					socket_command_set, ps, 'info', '"'+(*pm).pileup.file+'"', class='pileup'
					socket_command_set, ps, 'info', '"'+(*pm).cal.file+'"', class='cal'
					socket_command_set, ps, 'info', '"'+(*pm).throttle.file+'"', class='throttle'
				endif
				
				; copy back, mostly to set enable flags in *pl for Hymod panel,
				; and for execution in "daq_setup_apply_hymod" below.
				copy_pointer_data, pm, pl
				
				daq_setup_apply_hymod, pstate, throttle=0
				daq_setup_update_dynamic, pstate
				
				for i=1L,n_elements((*pstate).check_ids)-1 do begin
					if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
				endfor
				widget_control, (*pstate).check_ids[0], set_value=1
				(*pstate).check_mode = 0
				(*pstate).group_mask = [0,0,0,0,0,1,1,1]
				(*pstate).old_mask = (*pstate).group_mask
				daq_setup_check_group_mode, pstate
	
				; set the colours to bytscl() the hermes time values ...
				if (*pstate).colour_mode eq 1 then daq_setup_colour, pstate, (*pm).channel.hermes.time
			endif
		endif
		
		file = strip_file_ext((*pm).file,/double) + '.enable.var'
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'daq'
		F = file_requester( /read, filter = ['*.enable.var','*.enable.csv'], file=file, $
			path=path, group=event.top, $
			title='Select the Enable CSV file', fix_filter=1)
		if F ne '' then begin
			disable = read_daq_enable( F, index=(*play).data.index, error=err)
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
		
		file = strip_file_ext((*pm).file,/double) + '.groups.csv'
		path = *(*pstate).path
		if path eq '' then path = geopixe_environment() + slash() + 'daq'
		F = file_requester( /read, filter = '*.groups.csv', file=file, $
			path=path, group=event.top, $
			title='Select the Groups CSV file', fix_filter=1)
		if F ne '' then begin
			F = strip_file_ext(F,/double) + '.groups.csv'
			set_widget_text, (*pstate).spectra_file_text, F
			groups = read_daq_groups( F, error=err)
			if err eq 0 then begin
				(*pl).groups.file = F
				(*pl).groups.group = groups
				(*pl).groups.on = 1
				set_widget_text, (*pstate).spectra_file_text, F
				*(*pstate).path = extract_path(F) 
				daq_setup_load_group_table, pstate
				daq_setup_apply_groups, pstate
			endif
		endif
		end
		
	'select-mode': begin
		(*pstate).group_mode = event.index
		daq_setup_check_group_mode, pstate
		end
		
	'mimic-toggle': begin
		(*pstate).colour_mode = event.value
		if (*pstate).colour_mode eq 1 then begin
			for i=0L,n_elements((*pstate).check_ids)-1 do begin
				if widget_info((*pstate).check_ids[i],/valid) then widget_control, (*pstate).check_ids[i], set_value=0			; clear all other check boxes
			endfor
;			daq_setup_colour, pstate, /redisplay
;			widget_control, (*pstate).check_ids[(*pstate).last_check], set_value=1
			widget_control, (*pstate).detector, set_value={mode:0, select:indgen(n_detectors), value:intarr(n_detectors)}
		endif else begin
;			(*pstate).last_check = (*pstate).check_mode
			widget_control, (*pstate).detector, set_value={mode:0, select:indgen(n_detectors), value:(*pstate).select}
		endelse
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
				warning,'daq_setup','No detectors currently selected.'
				goto, bad_save
			endif
			n = index[q]
			printf, lun, '# Selected detector channels.'
			printf, lun, '# Saved by "daq_setup".'	
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
	warning,'daq_setup_event',['STATE variable has become ill-defined.','Abort DAQ setup.'],/error
	goto, kill

kill:
	if ((*pstate).debug_aux_mode ne 0) then begin
		warning,'daq_setup',['Debug mode still active.','Click "OK" to stop Debug mode before exit.'], cancel=cancel
		if cancel eq 0 then begin
			socket_command_set, ps, 'LOCK', 0, class='scepter', chip=-1 
			socket_command_set, ps, 'AUX', 0, class='scepter', chip=-1
			if (*pm).version.scepter ge 6 then begin
				socket_command_set, ps, 'MASK', 0, class='scepter', chip=-1, channel=-1
			endif
			(*pstate).debug_aux_mode = 0
		endif 
	endif
	if (*pstate).pulser.mode ne 0 then begin
		warning,'daq_setup',['Pulser is still active.','Click "OK" to stop Pulser mode before exit.'], cancel=cancel
		if cancel eq 0 then begin
			socket_command_set, ps, 'rate', 0, class='pulser'
			socket_command_set, ps, 'enable', 0, class='synth'
;			socket_command_set, ps, 'enable', 1, class='scepter'
			(*pstate).pulser.mode = 0
		endif
	endif	
	daq_setup_check_warning, pstate
		
die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;--------------------------------------------------------------------------

pro daq_setup_apply_device, pstate

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
			warning,'daq_setup_apply_device',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

@daq_scratch.def

	pm = (*pstate).pdaq
	ps = (*pstate).psocket
	play = (*pstate).playout
	pl = (*pstate).plocal			; local copy of pm made on tab change, and by check and file
	error = 0						; widgets. These sent to Maia, and read back as pm elsewhere.
	obj = (*pm).DevObj

	opt = obj->get_options()
	dt = opt.deadtime_cal

	info = { auto:(*pl).deadtime.auto, cal:dt }
	s = stringify( info)
	socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_deadtime, error=err

	s = '"' + str_tidy(opt.axis.x) + ' ' + str_tidy(opt.axis.y) + '"'
	socket_command_set, ps, 'order', s, class='metadata.scan', error=err & error = error or err

	if error then begin
		warning,'daq_setup_apply_device','Failed to set Klee/DAQ parameters.'
		return
	endif

	(*pm).deadtime.cal = dt
	return
	
bad_state:
	warning,'daq_setup_apply_device',['STATE variable has become ill-defined.','Abort DAQ Hymod apply.'],/error
	return
end

;--------------------------------------------------------------------------

pro daq_setup_apply_hymod, pstate

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
			warning,'daq_setup_apply_hymod',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	if n_elements(pstate) eq 0 then goto, bad_state
	if ptr_valid(pstate) eq 0 then goto, bad_state
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

@daq_scratch.def

	pm = (*pstate).pdaq
	ps = (*pstate).psocket
	play = (*pstate).playout
	pl = (*pstate).plocal			; local copy of pm made on tab change, and by check and file
	error = 0						; widgets. These sent to Maia, and read back as pm elsewhere.
	obj = (*pm).DevObj

	widget_control, (*pstate).beam_energy_text, get_value=s
	(*pl).beam.energy = float(s)

	socket_command_set, ps, 'energy', (*pl).beam.energy*1.0e+6, class='metadata.beam', error=err & error = error or err
	socket_command_set, ps, 'particle', (*pl).beam.particle, class='metadata.beam', error=err & error = error or err

	socket_command_set, ps, 'coeff', (*pl).beam.charge.scale, class='charge', error=err & error = error or err
	socket_command_set, ps, 'unit', (*pl).beam.charge.unit, class='charge', error=err & error = error or err
	
	daq_launch_update_IC, pl

	if error then begin
		warning,'daq_setup_apply_hymod','Failed to set Klee/DAQ parameters.'
		return
	endif

	(*pm).beam = (*pl).beam
	(*pm).IC = (*pl).IC
	return
	
bad_state:
	warning,'daq_setup_apply_hymod',['STATE variable has become ill-defined.','Abort DAQ Hymod apply.'],/error
	return
end

;--------------------------------------------------------------------------------------------------------------------

pro daq_setup_check_group_mode, pstate

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
		warning,'daq_setup_group_mode',['IDL run-time error caught.', '', $
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

pro daq_setup_check_warning, pstate

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
		warning,'daq_setup_check_warning',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

pm = (*pstate).pdaq
warn_pulser = ((*pstate).pulser.mode ne 0)
widget_control, (*pstate).warning_pulser_base, map=warn_pulser

warn_debug = ((*pstate).debug_aux_mode ne 0)
widget_control, (*pstate).warning_debug_base, map=warn_debug

; Set return colour for "Setup" button in daq-launch ...
; 
*(*pstate).plaunch = warn_pulser or warn_debug ? 1 : 0

notify, 'warn-setup', (*pstate).plaunch, from=(*pstate).tlb
return
end

;--------------------------------------------------------------------------

pro daq_setup_colour, pstate, vals, redisplay=redisplay, ignore=q, title=title

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
		warning,'daq_setup_colour',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if (*pstate).colour_mode ne 1 then return
if n_elements(redisplay) eq 0 then redisplay=0
if n_elements(title) eq 0 then title=''
pm = (*pstate).pdaq
widget_control, (*pstate).detector, set_value={select:indgen((*pm).n_detectors), value:0}

if redisplay then begin
	vals = (*pstate).colour_vector
	title = (*pstate).colour_title
endif else begin
	if n_elements(vals) ne (*pm).n_detectors then begin
		warning,'daq_setup_colour','not a vector of '+string((*pm).n_detectors)
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

pro daq_setup_load_table, pstate

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
		warning,'daq_setup_load_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
pm = (*pstate).pdaq

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
nc = 28
rows = string(indgen(n))
widths = replicate(6.0,nc) 
t = strarr(nc,n)

; The table[] indices are for chip = 1 (hermes), 2 (scepter), 3 (ddm)
; to select the structs within 'channel' struct array, and used in
; summary table event. The par indices select an item within the
; respective structs (see channel).
; N.B. This means order of parameters in structs is important!
;
; Table 'chip' numbers, to identify structs in pdaq, etc., are:
;	pdaq channel:	0	none		3	Cal
;					1				4	trim.E	
;					2	Scepter		5	trim.T
;	playout:		6	data
;	(see application in summary-table event)
;
; Check box ID index:
; tdm, tds, tos, trk, trke, trim, thresh, clock, thpd, filt, tcm
;  0    1    2    3    4     5      6       7     8     9     10
;
;            0      1     2     3      4      5     6     7      8       9      10      11     12    13    
columns = ['Indx','Det','off','tdm','tds','tos','thresh','trim','thpd','clock','tcm','filt','trk','trke']
checks = [  -1,    -1,   -1,    4,     5,    6,     10,     9,     12,    11,    14,   13,    7,     8]

widths[0:5] = 3.5 					; Indx, Det, ech, tdm, tds, tos
widths[6:9] = [5.2,5.2,5.2,4.7] 	; thresh, trim, thpd, clock
widths[10:13] = 3.5 

; Assumes table in CSV table order, as is (*pm).channel
; (*play).data.index is detector number given CSV index.
; (*play).ref is CSV index given detector number.

t[0,*] = str_tidy(indgen(n))
t[1,*] = str_tidy((*play).data.index)
t[2,*] = str_tidy(fix(*(*pstate).pdisable))
t[3,*] = str_tidy((*pm).channel.scepter.tdm)
	(*pstate).table[3].chip = 2  &  (*pstate).table[3].par = 0
t[4,*] = str_tidy((*pm).channel.scepter.tds)
	(*pstate).table[4].chip = 2  &  (*pstate).table[4].par = 1
t[5,*] = str_tidy((*pm).channel.scepter.tos)
	(*pstate).table[5].chip = 2  &  (*pstate).table[5].par = 2
t[6,*] = str_tidy((*pm).channel.scepter.thresh, length=5)
	(*pstate).table[6].chip = 2  &  (*pstate).table[6].par = 6
t[7,*] = str_tidy((*pm).channel.scepter.trim, length=5)
	(*pstate).table[7].chip = 2  &  (*pstate).table[7].par = 5
t[8,*] = str_tidy((*pm).channel.scepter.thpd, length=4)
	(*pstate).table[8].chip = 2  &  (*pstate).table[8].par = 8
t[9,*] = str_tidy((*pm).channel.scepter.clock, length=4)
	(*pstate).table[9].chip = 2  &  (*pstate).table[9].par = 7
t[10,*] = str_tidy((*pm).channel.scepter.tcm)
	(*pstate).table[10].chip = 2  &  (*pstate).table[10].par = 10
t[11,*] = str_tidy((*pm).channel.scepter.filt)
	(*pstate).table[11].chip = 2  &  (*pstate).table[11].par = 9
t[12,*] = str_tidy((*pm).channel.scepter.trk)
	(*pstate).table[12].chip = 2  &  (*pstate).table[12].par = 3
t[13,*] = str_tidy((*pm).channel.scepter.trke)
	(*pstate).table[13].chip = 2  &  (*pstate).table[13].par = 4
npars = 14

;columns = [columns, 'CalA','CalB','TrimEa','TrimEb','TrimTa','TrimTb']
;checks = [checks, replicate(-1,6)]
;widths[npars:npars+5] = 6.0 
columns = [columns, 'CalA','CalB']
checks = [checks, replicate(-1,2)]
widths[npars:npars+2] = 6.0 

t[npars,*] = str_tidy((*pm).channel.cal.a, length=6)
	(*pstate).table[npars].chip = 3  &  (*pstate).table[npars].par = 0
t[npars+1,*] = str_tidy((*pm).channel.cal.b, length=6)
	(*pstate).table[npars+1].chip = 3  &  (*pstate).table[npars+1].par = 1
;t[npars+2,*] = str_tidy((*pm).channel.trim.E.a, length=6)
;	(*pstate).table[npars+2].chip = 4  &  (*pstate).table[npars+2].par = 0
;t[npars+3,*] = str_tidy((*pm).channel.trim.E.b, length=6)
;	(*pstate).table[npars+3].chip = 4  &  (*pstate).table[npars+3].par = 1
;t[npars+4,*] = str_tidy((*pm).channel.trim.T.a, length=6)
;	(*pstate).table[npars+4].chip = 5  &  (*pstate).table[npars+4].par = 0
;t[npars+5,*] = str_tidy((*pm).channel.trim.T.b, length=6)
;	(*pstate).table[npars+5].chip = 5  &  (*pstate).table[npars+5].par = 1
;npars = npars+6
npars = npars+2

columns = [columns,'X','Y','Z','Width','Height','Tilt','FWHM','Chip','Quad','Radial','Col','Row']
checks = [checks, replicate(-1,12)]
widths[npars:npars+5] = 5.5 
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
t[npars+5,*] = str_tidy((*play).data.tilt, length=5)
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

if nc ne npars then warning,'daq_setup: daq_setup_load_table','Fix table "nc" not equal to "npars"='+string(npars)
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

pro daq_setup_sensitive, pstate, scepter=scepter

; Set sensitive=0 for all Scepter parameter widgets, except the indicated pad class group

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
		warning,'daq_setup_sensitive',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(scepter) lt 1 then scepter=0

for i=0L,n_elements( (*pstate).ids_scepter)-1 do begin
	if widget_info((*pstate).ids_scepter[i],/valid) then widget_control, (*pstate).ids_scepter[i], sensitive=0
endfor

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
	 	
pro daq_setup_update_dynamic, pstate

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
		warning,'daq_setup_update_dynamic',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	pm = (*pstate).pdaq
;	if (*pm).DA.on then (*pm).ROI.on=0

;	widget_control, (*pstate).imaging_da_check, set_value=(*pm).DA.on
;	widget_control, (*pstate).imaging_roi_check, set_value=(*pm).ROI.on
	
;	set_widget_text, (*pstate).hymod_cal_text, (*pm).cal.file
;	set_widget_text, (*pstate).imaging_da_text, (*pm).da.file
;	set_widget_text, (*pstate).imaging_roi_text, (*pm).roi.file
;	set_widget_text, (*pstate).hymod_deadtime_offset, str_tidy((*pm).deadtime.cal.b)
;	set_widget_text, (*pstate).hymod_deadtime_slope, str_tidy((*pm).deadtime.cal.a)
	

ps = (*pstate).psocket
v = socket_command_get( ps, 'coeff', class='charge', /float, error=err)
if err eq 0 then begin
	(*pm).beam.charge.scale = v[0]
	(*pm).IC.pv.val = v[0]
endif
v = socket_command_get( ps, 'unit', class='charge', /string, error=err)
if err eq 0 then begin
	(*pm).beam.charge.unit = v[0]
	sens = charge_sensitivity( (*pm).IC.pv.val, v[0], /ionbeam)
	val = charge_gain_units( sens, unit=vunit)
	(*pm).IC.pv.val = val
	(*pm).IC.pv.unit = vunit
endif


	set_widget_text, (*pstate).beam_energy_text, str_tidy((*pm).beam.energy)
	charges = [0.01, 0.1, 1.]
	q = sort( abs((*pm).beam.charge.scale - charges))
	widget_control, (*pstate).charge_scale_mode, set_combobox_select=q[0]
	
	
	
	
;	if ptr_good((*pm).IC.plist) then begin
;		q = where( (*pm).IC.pv.name eq *(*pm).IC.plist, nq)
;		widget_control, (*pstate).ic_pv_mode, set_value=*(*pm).IC.plist
;		widget_control, (*pstate).ic_pv_mode, set_combobox_select=(nq ne 0) ? q[0] : 0
;	endif else begin
;		widget_control, (*pstate).ic_pv_mode, set_value=['none']
;		widget_control, (*pstate).ic_pv_mode, set_combobox_select=0
;	endelse
;	q = where( abs((*pm).IC.pv.val - (*pstate).ic_vals) lt 0.01, nq)
;	widget_control, (*pstate).ic_val_mode, set_combobox_select=(nq ne 0) ? q[0] : 0
;	q = where( abs(((*pm).IC.pv.unit - (*pstate).ic_vunits)/((*pstate).ic_vunits>0.001)) lt 0.01, nq)
;	widget_control, (*pstate).ic_unit_mode, set_combobox_select=(nq ne 0) ? q[0] : 0
;	widget_control, (*pstate).charge_conversion, set_value=str_tidy((*pm).IC.conversion)
;
;	widget_control, (*pstate).charge_mode, set_combobox_select=(*pm).IC.mode
;	case (*pm).IC.mode of
;		0: begin
;			widget_control, (*pstate).ic_base, map=0
;			widget_control, (*pstate).scan_button, sensitive=0
;			widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
;			widget_control, (*pstate).ic_base2, scr_ysize=1
;			end
;		1: begin
;			widget_control, (*pstate).ic_base, map=1
;			widget_control, (*pstate).scan_button, sensitive=1
;			widget_control, (*pstate).ic_base1, map=1, scr_ysize=(*pstate).ic_base1_ysize
;			widget_control, (*pstate).ic_base2, scr_ysize=(*pstate).ic_base2_ysize
;			end
;		2: begin
;			widget_control, (*pstate).ic_base, map=1
;			widget_control, (*pstate).scan_button, sensitive=0
;			widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
;			widget_control, (*pstate).ic_base2, scr_ysize=(*pstate).ic_base2_ysize
;			end
;	endcase	
;	widget_control, (*pstate).auto_DA_check, set_value=[(*pm).DA.save]
	
	return
end

;--------------------------------------------------------------------------

pro OnRealize_daq_summary_table, wWidget

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
		warning,'OnRealize_daq_summary_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id(wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

daq_setup_load_table, pstate
return
end

;-----------------------------------------------------------------

pro daq_setup, group_leader=group, data=data, daq=daq_pars, port=daq_port, debug=debug, $
			disable=daq_disable, readout=daq_readout, path=path, tlb=tlb, $
			ppspec=ppspec, default=default, dpath=dpath, pimage=pimage

; Setup, control and display real-time data from the DAQ-36 data acquisition system
;
; Data		a pointer (/allocate_heap) to a struct of layout parameters, else loaded from DAQ_36.csv
; DAQ		a pointer (/allocate_heap) to a struct of DAQ detector ASIC and control parameters, else defaulted. 
; Port		a pointer (/allocate_heap) to a struct of DAQ socket port parameters.
; Disable	a pointer (/allocate_heap) to an array of DAQ channel disable parameters.
; readout	a pointer (/allocate_heap) to readout struct

COMPILE_OPT STRICTARR
ErrorNo = 0
;print,'daq_setup startup ...'
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
		warning,'DAQ_setup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif else on_error,0
if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''
if n_elements(dpath) lt 1 then dpath=''
if n_elements(pimage) lt 1 then  begin
	print,'daq_setup: pimage not defined.
	pimage = ptr_new()
endif
;if n_elements(pshrmem_da) lt 1 then begin
;	print,'daq_setup: pshrmem_da not defined.
;	pshrmem_da = 0L
;endif
if n_elements(ppspec) lt 1 then  begin
	print,'daq_setup: ppspec not defined.
	ppspec = ptr_new()
endif
if n_elements(default) eq 0 then default = daq_defaults( error=error, source='daq_setup')
n_detectors = default.detectors

;-------------------------------------------------------------------------------------

; DAQ control socket parameters struct

ps = bad_pars_struct( daq_port, make_pars=make_ps)
if make_ps then begin
	print,'Open DAQ control socket ...'
	*ps = open_socket( ip=default.daq.ip, port=default.daq.port, token='0', $
								enable=default.daq.enable, retries=0, error=error)
	(*ps).n_detectors = default.detectors
	(*ps).version = default.version	
	if error ne 0 then begin
;		warning,'daq_launch','Failed to open DAQ control socket.'
;		return
	endif
endif
n_detectors = (*ps).n_detectors

; Default DAQ control parameters struct

pm = bad_pars_struct( daq_pars, make_pars=make_pm)
if make_pm then begin
	print,'DAQ_setup: make DAQ *pm struct based on n_detectors =',n_detectors
	*pm = define(daq_struct = n_detectors)
endif

; Default detector layout struct parameters from file "DAQ_36.csv"

prefix = 'DAQ_36'
letters = ['','A','B','C','D','E','F','G','H','I','J','K','L','M']
prefix = prefix + letters[ (*pm).version.daq ]

pd = bad_pars_struct( data, make_pars=make_pd)
if make_pd then begin
	file = prefix + '.csv'
	d = read_detector_layout(file, daq=daq, error=error)
	if error then begin
		warning,'daq_launch','Failed to read "'+file+'" to initialize layout.'
		return
	endif
	if daq eq 0 then begin
		warning,'daq_launch','"'+file+'" file does not contain required extended columns.'
		return
	endif
	*pd = d
endif

; DAQ disable parameters struct

pe = bad_pars_struct( daq_disable, make_pars=make_pe)
if make_pe then begin
	*pe = intarr(n_detectors)
endif

; DAQ readout parameters struct
;	perhaps use 'quadrant' to refer to Scepter bank 1 of 16, bank 2 or 16, NIMs

pr = bad_pars_struct( daq_readout, make_pars=make_pr)
if make_pr then begin
	readout = define(/daq_readout)
	*pr = readout
endif

;	Read back DAQ parameters to display widgets and summary table ...
	
;print,'daq_setup initial ...'
;daq_setup_initial, ps, pm, pr, pimage, pd
	
;-------------------------------------------------------------------------------------

case !version.os_family of
	'MacOS': begin
       symbol = 'SYMBOL*12'
       large_font = 'Arial*12'
       def_font = 'Geneva*10'
		fnt1 = 'HELVETICA*8'       ;'HELVETICA*9'
		csize = (n_detectors eq 384) ? 1.1 : 1.3
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
		help_xsize2 = 700
		end
	'unix': begin
       symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
       large_font = '10x20'
       def_font = '6x13'
		fnt1 = 'timr08'             ;'6x12'
		csize = (n_detectors eq 384) ? 1.1 : 1.3
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
		help_xsize2 = 700
		end
	else: begin
       symbol = 'SYMBOL*BOLD*14'
       large_font = 'COURIER*BOLD*10'
       def_font = 'Arial*14'
		fnt1 = 'Helvetica*8'       ;'ARIAL*12'
		csize = (n_detectors eq 384) ? 1.1 : 1.3
		space1 = 1
		space1b = 1
		space2 = 2
		space5 = 5
		space10 = 10
		space15 = 15

		left_xsize = 370
		left_ysize = 570		;685
		right_xsize = 600		;715
		right_ysize = 600		;715
		right_xsize2 = 500		;(n_detectors eq 384) ? 705 : 600
		right_ysize2 = 350		;(n_detectors eq 384) ? 690 : 450
		text_xsize = 120
		text_xsize2 = 140
		text_xsize3 = 60
		button_xsize = 50
		table_yscroll = 28		;34
		ptable_yscroll = 21
		list_ysize = 250
		xslide_off = 57
		charge_xsize2 = 215
		help_xsize = left_xsize+right_xsize+80
		help_xsize2 = 600		;700
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

;print,'daq_setup graphics ...'
tlb = widget_base( /column, title='DAQ Setup', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='daq-setup-tlb', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2 ,xoffset=xoffset, $
					yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
lbase = widget_base( tbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

tab_panel = widget_tab( lbase, location=2, /align_center, uname='daq-tab-panel')

check_ids = lonarr(15)
class_check_ids = lonarr(5)
tab_names = ['Enable','Scepter','Device','Summary', 'HYMOD', 'Controls','Layout','Pulser','Debug']

; Enable -----------------------------------------

Enable_base = widget_base( tab_panel, title='Enable', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( enable_base, value='Channel and Module Enable')
text = widget_text( enable_base, scr_xsize=left_xsize, ysize=4, /wrap, uname='enable-explanation', /tracking, $
				value=['Select bad detectors to be disabled or HYMOD processor modules to be enabled. See the "Data-Flow Diagram", using the tab on the far right, to view the Hymod functional diagram.'], $
				uvalue='Explanation of the role of the channel enable panel.', frame=1)

;label = widget_label( enable_base, value='  ')
enable_base1 = widget_base( enable_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( enable_base1, value='Disable Detector Channels')

text = widget_text( enable_base1, scr_xsize=left_xsize-10, ysize=6, /wrap, uname='disable-explanation', /tracking, $
				value=['Select detectors on the mimic panel to DISABLE channels (coloured violet on the mimic display).  Read back from Klee using the "Read" button.','', $
				'Send to DAQ using the "Apply to DAQ" botton and saved to disk using the "Save to File" button.'], $
				uvalue='Explanation of the role of the DAQ disable panel.', frame=1)

enable_base21 = widget_base( enable_base1, /row, xpad=1, ypad=0, space=1, /align_center, /base_align_center)
button = widget_button( enable_base21, value='Read', uname='read-enable', /tracking, uvalue='Read the photon channel enable parameters from Klee.')
button = widget_button( enable_base21, value='Apply to DAQ', uname='apply-daq-enable', /tracking, uvalue='Send the DAQ channel disable parameters to Klee.')
label = widget_label( enable_base21, value='  ')
button = widget_button( enable_base21, value='Load', uname='load-enable', /tracking, uvalue='Load the DAQ channel enable parameters from a ".enable.var" file.')
button = widget_button( enable_base21, value='Save to File', uname='save-enable', /tracking, uvalue='Save the DAQ channel disable parameters to a ".enable.var" file, and send them to Klee.')
label = widget_label( enable_base, value='  ')

enable_base2 = widget_base( enable_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( enable_base2, value='Enable DAQ Clocks')
enable_base2b = widget_base( enable_base2, /row, xpad=1, ypad=1, space=40, /align_center, /base_align_top)

enable_quad_check = cw_bgroup2( enable_base2b, ['SCEPTER clock'], /column, set_value=(*pr).quad_enable, sensitive=1, $
				/return_index, uname='enable-quad-check',/ nonexclusive, /tracking, $
				uvalue='Check these boxes to enable clocks. ', xpad=0, ypad=0, space=0)

enable_base3 = widget_base( enable_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( enable_base3, value='Enable HYMOD Processing Modules')

enable_base3b = widget_base( enable_base3, /row, xpad=1, ypad=1, space=20, /align_center, /base_align_top)
enables_check = [(*pr).scepter, (*pr).event, (*pr).photon, (*pr).activity, (*pm).scan.on]
enable_stream_check = cw_bgroup2( enable_base3b, ['SCEPTER','Events','Photons','Activity','Pixel'], /column, set_value=enables_check, sensitive=1, $
				/return_index, uname='enable-stream-check',/ nonexclusive, /tracking, $
				uvalue='Check these boxes to enable various data flow switches in HYMOD for: (i) event flow (photon, events), (ii) accumulators (for activity, DA, region spectra, dead-time, etc.) (activity, accumulators, dead-time), low level hardware enable (Scepter) and chart logging.', xpad=0, ypad=0, space=0)

; Scepter -----------------------------------------

Scepter_base = widget_base( tab_panel, title='SCEPTER', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( scepter_base, value='SCEPTER ASIC Controls')

text = widget_text( scepter_base, scr_xsize=left_xsize, ysize=7, /wrap, uname='scepter-explanation', /tracking, $
				value=['Set SCEPTER parameters for each DAQ detector channels 0-31.', $
					'','Select parameter groups using the check-boxes, click "Apply to SCEPTER" to update altered parameters for selected channels.', $
					'','Show current values using "Show Parameters" mode by checking check mark against parameter of interest.'], $
				uvalue='Explanation of the role of the Scepter panel.', frame=1)

if (*pm).version.scepter ge 6 then begin
	scepter_base0 = widget_base( Scepter_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)

	cid = 3
	sbase01 = widget_base( scepter_base0, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
	check = cw_bgroup2( sbase01, [''], /row, set_value=[0], sensitive=1, $
					/return_index, uname='scepter-channel-check',/ nonexclusive, /tracking, $
					uvalue='Check this box to select this group of parameters for modification and transmission to DAQ. Select channels on the mimic display on the right. ' + $
						'Make sure the radio button at the bottom of the window is in "Set Parameters" mode.', xpad=0, ypad=0, space=0)
	class_check_ids[cid] = check
	label = widget_label( sbase01, value='Parameters for Individual SCEPTER channels')

	id = 9
	sbase6b = widget_base( Scepter_base0, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
	arrow = picture_button( sbase6b, geopixe_root + 'images/up-16x14.jpeg', uname='scepter-trim-up', $
			/tracking, uvalue='Adjust selected channel Trim values: Increment all by 0.005V', /pushbutton_events)
	arrow = picture_button( sbase6b, geopixe_root + 'images/down-16x14.jpeg', uname='scepter-trim-down', $
			/tracking, uvalue='Adjust selected channel Trim values: Decrement all by 0.005V', /pushbutton_events)
	label = widget_label( sbase6b, value='   Trim DAC (V):')
	scepter_trim_slider = cw_fslider2( sbase6b, format='(F6.3)', minimum=0.0, maximum=0.15, layout=1, scroll=0.005, sensitive=0, $
					value=0.0, uname='scepter-trim-slider', xsize=text_xsize-50, /tracking, /edit, /drag, xpad=0, ypad=0, $
					uvalue={id:id, help:'Adjust the SCEPTER Threshold Trim DAC (0 - 0.15 Volts) for the selected SCEPTER channel(s). TRIM is subtracted from Thresh. Normally, all SCEPTER channels are set to the same Threshold DAC, and individual channels are adjusted using the Threshold Trim DACs.'})
	check = cw_bgroup2( sbase6b, [''], /row, set_value=[0], sensitive=1, $
					/return_index, uname='scepter-trim-check',/ nonexclusive, /tracking, $
					uvalue={label:'Scepter Trim DAC (V)',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to DAQ when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
	check_ids[id] = check
endif else scepter_trim_slider = 0L

scepter_base1 = widget_base( Scepter_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)

cid = 4
sbase02 = widget_base( scepter_base1, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
check = cw_bgroup2( sbase02, [''], /row, set_value=[0], sensitive=1, $
					/return_index, uname='scepter-chip-check',/ nonexclusive, /tracking, $
					uvalue='Check this box to select this group of parameters for modification and transmission to DAQ. Select channels on the mimic display on the right. ' + $
						'Make sure the radio button at the bottom of the window is in "Set Parameters" mode.', xpad=0, ypad=0, space=0)
class_check_ids[cid] = check
label = widget_label( sbase02, value='Parameters for selected SCEPTER chips')

id = 4
sbase1 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase1, value='Timing mode:')
scepter_tdm_mode = widget_combobox(sbase1, uname='scepter-tdm-mode', scr_xsize=text_xsize, sensitive=0, $
			value=['Time-of-Occurence','Rise-time','Fall-time','Time-over-Threshold'], /tracking, uvalue={id:id, help:'Select the SCEPTER TAC timing mode (tdm) for the selected detector(s). Normally, all SCEPTER channels are set to the same timing mode. DAQ uses tdm "Time-over-Threshold".'})
check = cw_bgroup2( sbase1, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-tdm-check',/ nonexclusive, /tracking, $
				uvalue={label:'Scepter Time Mode',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to DAQ when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

id = 5
sbase2 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase2, value='TAC slope:')
scepter_tds_mode = widget_combobox(sbase2, uname='scepter-tds-mode', scr_xsize=text_xsize, sensitive=0, $
			value=tds_times, /tracking, uvalue={id:id, help:'Select the SCEPTER time to analogue converter slope (tds) for the selected detector. Normally, all SCEPTER channels are set to the same TAC slope. DAQ typically uses tds maximum (64). NOTE: This setting will affect the DT Calibration (see HYMOD DTcalA and DTcalB).'})
check = cw_bgroup2( sbase2, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-tds-check',/ nonexclusive, /tracking, $
				uvalue={label:'Scepter TAC Slope',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to DAQ when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

id = 6
sbase3 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase3, value='TAC timeout:')
scepter_tos_mode = widget_combobox(sbase3, uname='scepter-tos-mode', scr_xsize=text_xsize, sensitive=0, $
			value=tos_times, /tracking, uvalue={id:id, help:'Select the SCEPTER TAC timeout (tos) for the selected detector. Normally, all SCEPTER channels are set to the same timeout. DAQ typically uses tos maximum (64).'})
check = cw_bgroup2( sbase3, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-tos-check',/ nonexclusive, /tracking, $
				uvalue={label:'Scepter TAC Timeout',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to DAQ when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

id = 7
sbase4 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase4, value='Simultaneous event catching:')
scepter_trk_mode = widget_combobox(sbase4, uname='scepter-trk-mode', scr_xsize=text_xsize, sensitive=0, $
			value=['Off','On'], /tracking, uvalue={id:id, help:'Select the SCEPTER simultaneous events catching mode (trk) for the selected detector. Normally, all SCEPTER channels are set to the same mode. DAQ typically uses trk On.'})
check = cw_bgroup2( sbase4, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-trk-check',/ nonexclusive, /tracking, $
				uvalue={label:'Scepter Simult. Catching',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to DAQ when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

id = 8
sbase5 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase5, value='Enhanced Simult. event catching:')
scepter_trke_mode = widget_combobox(sbase5, uname='scepter-trke-mode', scr_xsize=text_xsize, sensitive=0, $
			value=['Off','On'], /tracking, uvalue={id:id, help:'Select the SCEPTER enhanced simultaneous events catching mode (trke) for the selected detector. Normally, all SCEPTER channels are set to the same mode. DAQ typically uses trke On.'})
check = cw_bgroup2( sbase5, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-trke-check',/ nonexclusive, /tracking, xpad=0, ypad=0, space=0, $
				uvalue={label:'Scepter New Simult. Catching',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to DAQ when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'})
check_ids[id] = check

if (*pm).version.scepter ge 6 then begin
	id = 13
	sbase10 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
	label = widget_label( sbase10, value='Added input filtering:')
	scepter_filt_mode = widget_combobox(sbase10, uname='scepter-filt-mode', scr_xsize=text_xsize, sensitive=0, $
				value=['Off','On'], /tracking, uvalue={id:id, help:'Enable added input filtering (filtena) in newer SCEPTER chips.'})
	check = cw_bgroup2( sbase10, [''], /row, set_value=[0], sensitive=1, $
					/return_index, uname='scepter-filt-check',/ nonexclusive, /tracking, xpad=0, ypad=0, space=0, $
					uvalue={label:'Scepter Input Filter',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to DAQ when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'})
	check_ids[id] = check

	id = 14
	sbase11 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
	label = widget_label( sbase11, value='Comparator multi-fire suppression:')
	scepter_tcm_mode = widget_combobox(sbase11, uname='scepter-tcm-mode', scr_xsize=text_xsize, sensitive=0, $
				value=['0 ns','100 ns','1 us','2 us'], /tracking, uvalue={id:id, help:'SCEPTER comparator multi-fire suppression (tcm) (0=0, 1=100ns, 2=1us, 3=2us).'})
	check = cw_bgroup2( sbase11, [''], /row, set_value=[0], sensitive=1, $
					/return_index, uname='scepter-tcm-check',/ nonexclusive, /tracking, xpad=0, ypad=0, space=0, $
					uvalue={label:'Scepter Glitch Suppress',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to DAQ when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'})
	check_ids[id] = check
endif else begin
	scepter_filt_mode = 0L
	scepter_tcm_mode = 0L
endelse

id = 10
sbase6 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
arrow = picture_button( sbase6, geopixe_root + 'images/up-16x14.jpeg', uname='scepter-thresh-up', $
			/tracking, uvalue='Adjust selected channel Threshold values: Increment all by 0.005V', /pushbutton_events)
arrow = picture_button( sbase6, geopixe_root + 'images/down-16x14.jpeg', uname='scepter-thresh-down', $
			/tracking, uvalue='Adjust selected channel Threshold values: Decrement all by 0.005V', /pushbutton_events)
label = widget_label( sbase6, value='   Threshold DAC (V):')
scepter_thresh_slider = cw_fslider2( sbase6, format='(F6.3)', minimum=0.005, maximum=2.0, layout=1, scroll=0.005, sensitive=0, $
				value=0.5, uname='scepter-thresh-slider', xsize=text_xsize-50, /tracking, /edit, /drag, xpad=0, ypad=0, $
				uvalue={id:id, help:'Set the main SCEPTER Threshold DAC (Volts) (thresh) for the selected SCEPTER(s). Normally, all SCEPTER channels are set to the same Threshold DAC, and individual channels are adjusted using the Threshold Trim DACs (Scepter 7 only) and each Scepter is adjusted using the THPD adjustment DACs (Scepter 7 only). '})
check = cw_bgroup2( sbase6, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-thresh-check',/ nonexclusive, /tracking, $
				uvalue={label:'Scepter Threshold (V)',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to DAQ when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
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
					uvalue={label:'Scepter Thresh adjust (V)',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to DAQ when the "Apply to SCEPTER" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
	check_ids[id] = check
endif else scepter_thpd_slider=0L

id = 11
sbase7 = widget_base( Scepter_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( sbase7, value='Read-Request Clock (MHz):')
scepter_rr_slider = cw_fslider2( sbase7, format='(F5.1)', minimum=0.1, maximum=30.0, layout=1, scroll=0.1, sensitive=0, $
				value=3.0, uname='scepter-rr-slider', xsize=text_xsize-45, /tracking, /edit, /drag, xpad=0, ypad=0, $
				uvalue={id:id, help:'Adjust the Read-Request Clock frequency (MHz) for the selected quadrant(s). Normally, all SCEPTER chips are set to the same RR clock. The typical RR clock is 3 MHz.'})
check = cw_bgroup2( sbase7, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='scepter-rr-check',/ nonexclusive, /tracking, $
				uvalue={label:'Quadrant RR Clock',help:'In "Set Parameters" mode, this box is checked if the parameter setting has been changed. It will then be sent to DAQ when the "Apply to HERMES" button is pressed. In "Show Parameters" mode, check this box to display parameter values on the mimic display. ' + $
					'In "Set Parameters" mode, select channels on the mimic display on the right.'}, xpad=0, ypad=0, space=0)
check_ids[id] = check

check_ids_scepter_channel = check_ids[9]
check_ids_scepter_chip = check_ids[ [4,5,6,7,8,13,14,10,11,12] ]
check_ids_scepter_whole = check_ids[11]

ids_scepter_channel = [scepter_trim_slider]
ids_scepter_chip = [scepter_tdm_mode, scepter_tds_mode, scepter_tos_mode, scepter_trk_mode, scepter_trke_mode, scepter_filt_mode, scepter_tcm_mode, scepter_thresh_slider, scepter_thpd_slider, scepter_rr_slider]
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

scepter_apply_button = widget_button( scepter_base, value='Apply to SCEPTER', uname='scepter-apply', /tracking, uvalue='Apply the selected settings (set using check-boxes) to the selected SCEPTER channels in DAQ. Normally, all channels ' + $
				'of all SCEPTER chips are set together so that all channels have the same characteristics. One exception may be the Threshold DAC.')


; ----------- device ------------------------------------------------------------------------------

device_base = widget_base( tab_panel, title='Device', /column, xpad=1, ypad=1, scr_xsize=tab_xsize, space=5, /base_align_center, /align_center)
lab = widget_label( device_base, value='Device Specific Parameters')

text = widget_text( device_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='device-explanation', /tracking, $
				value=['DAQ device specific parameters, managed by DAQ 36 Device Object.'], $
				uvalue='Explanation of the role of the Device panel.', frame=1)

device_base2 = widget_base( device_base, /column, xpad=1, ypad=1, space=2, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)

;	Device options

devbase = widget_base( device_base2, /base_align_center, map=1, space=1, xpad=0, ypad=0)

device_option_mode_base = widget_base( devbase, /column,  space=1, xpad=0, ypad=1, /base_align_center, xsize=stepmode_base_width, /align_center)

; Render sort options in render_options method in object, else set Y size to 1

obj = (*pm).DevObj
obj->render_options, device_option_mode_base
widget_control, device_option_mode_base, map=obj->show_sort_options(), scr_ysize=obj->get_sort_ysize()

devbase2 = widget_base( device_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( devbase2, value='Auto Dead-Time Calibration')

devbase3 = widget_base( devbase2, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
device_deadtime_auto = cw_bgroup2( devbase3, ['Auto'], /row, set_value=[1], sensitive=1, $
				/return_index, uname='device-deadtime-auto',/ nonexclusive, /tracking, $
				uvalue='Check this box to enable auto setting of DTcalA when Scepter TDS (TAC slope) is changed. ' + $
				'Disable "auto" if you want to set the DTCalA parameter manually to a non standard value.', xpad=0, ypad=0, space=0)

device_apply_button = widget_button( device_base, value='Apply to HYMOD', uname='device-apply', /tracking,  $
				uvalue='Apply the selected settings to Klee and DAQ. ')


; Summary -----------------------------------------

Summary_base = widget_base( tab_panel, title='Summary', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( summary_base, value='Parameter Summary Table')

summary_table = Widget_Table(summary_base, UNAME='summary-table', alignment=2, scr_xsize=left_xsize,  $
;      NOTIFY_REALIZE='OnRealize_daq_summary_table', $
      /all_events, X_SCROLL_SIZE=13 ,Y_SCROLL_SIZE=table_yscroll ,value=strarr(30,n_detectors), $
      /RESIZEABLE_COLUMNS, /no_row_headers, /tracking, uvalue='Shows the current values of SCEPTER and some DAQ parameters. ' + $
      'Click on a detector "Indx" to select the detector on the mimic display in "Set Parameters" mode. Click on a column heading to (i) Sort the table in ascending order based on this column value in "Set Parameters" mode, or (ii) show the parameters variation on the mimic display in "Show Parameters" mode.' )


; HYMOD -----------------------------------------

hymod_base = widget_base( tab_panel, title='HYMOD', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( hymod_base, value='HYMOD & Klee Parameters')

text = widget_text( hymod_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='hymod-explanation', /tracking, $
				value=['Status of HYMOD modules and set-up for DAQ (Klee) parameters.'], $
				uvalue='Explanation of the role of the HYMOD panel.', frame=1)

hymod_base1 = widget_base( hymod_base, /column, xpad=1, ypad=1, space=2, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( hymod_base1, value='DAQ & Klee Parameters')

hbase1a = widget_base( hymod_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hbase1a, value='Beam particle:')
beams = ['proton','deuteron','He3','He4']
beam_particle_mode = widget_combobox(hbase1a, uname='beam-particle-mode', scr_xsize=text_xsize, sensitive=1, $
		value=beams, /tracking, uvalue='Select the beam particle.')

hbase1b = widget_base( hymod_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hbase1b, value='Beam energy:')
beam_energy_text = widget_text( hbase1b, value=str_tidy((*pm).beam.energy), uname='beam-energy', /edit, /tracking, $
	uvalue='Display or set the beam energy (MeV).', scr_xsize=text_xsize)

hbase1c = widget_base( hymod_base1, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
label = widget_label( hbase1c, value='Charge scale:')
charge_scale_mode = widget_combobox(hbase1c, uname='charge-scale-mode', scr_xsize=text_xsize, sensitive=1, $
		value=['0.01','0.1','1.0'], /tracking, uvalue='Select the charge integrator scale (pC).')

hymod_apply_button = widget_button( hymod_base, value='Apply to HYMOD', uname='hymod-apply', /tracking,  $
		uvalue='Apply the selected settings to Klee and DAQ. ')


; Controls -----------------------------------------

Controls_base = widget_base( tab_panel, title='Controls', /column, xpad=1, ypad=1, space=5, $
	/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( controls_base, value='DAQ Controls')

text = widget_text( controls_base, scr_xsize=left_xsize, ysize=5, /wrap, uname='controls-explanation', /tracking, $
	value=['Status of DAQ and HYMOD modules and set-up for DAQ (Klee) parameters.'], $
	uvalue='Explanation of the role of the Controls panel.', frame=1)

controls_base5a = widget_base( controls_base, /column, xpad=1, ypad=1, space=2, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)
label = widget_label( controls_base5a, value='DAQ & HYMOD Status')
controls_base5 = widget_base( controls_base5a, /row, xpad=1, ypad=1, space=2, /align_center, /base_align_top, scr_xsize=left_xsize)

cbase5l = widget_base( controls_base5, /column, xpad=2, ypad=1, space=1, /align_top, /base_align_right, /frame)
label = widget_label( cbase5l, value='Temperature', /align_center)

cbase5a = widget_base( cbase5l, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5a, value='HYMOD FPGA:')
controls_temp_hymod_fpga_text = widget_text( cbase5a, value=str_tidy((*pm).control.temp.fpga), uname='controls-temp-hymod-fpga-monitor', /tracking, $
	uvalue='Shows the readback of the HYMOD FPGA temperature (C).', scr_xsize=text_xsize3)

cbase5b = widget_base( cbase5l, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5b, value='HYMOD CPU:')
controls_temp_hymod_cpu_text = widget_text( cbase5b, value=str_tidy((*pm).control.temp.cpu), uname='controls-temp-hymod-cpu-monitor', /tracking, $
	uvalue='Shows the readback of the HYMOD CPU temperature (C).', scr_xsize=text_xsize3)

cbase5c = widget_base( cbase5l, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5c, value='HYMOD Board:')
controls_temp_hymod_board_text = widget_text( cbase5c, value=str_tidy((*pm).control.temp.board), uname='controls-temp-hymod-board-monitor', /tracking, $
	uvalue='Shows the readback of HYMOD board temperature (C).', scr_xsize=text_xsize3)

cbase5d = widget_base( cbase5l, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5d, value='HYMOD ETH0:')
controls_temp_hymod_eth0_text = widget_text( cbase5d, value=str_tidy((*pm).control.temp.eth0), uname='controls-temp-hymod-eth0-monitor', /tracking, $
	uvalue='Shows the readback of HYMOD ETH0 temperature (C).', scr_xsize=text_xsize3)

cbase5c = widget_base( cbase5l, /row, xpad=0, ypad=0, space=5, /base_align_center)
label = widget_label( cbase5c, value='HYMOD ETH1')
controls_temp_hymod_eth1_text = widget_text( cbase5c, value=str_tidy((*pm).control.temp.eth1), uname='controls-temp-hymod-eth1-monitor', /tracking, $
	uvalue='Shows the readback of HYMOD ETH1 temperature (C).', scr_xsize=text_xsize3)


; Layout -----------------------------------------

;print,'daq_setup graphics 2d ...'
Edit_base = widget_base( tab_panel, title='Layout', /column, xpad=1, ypad=1, space=space2, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( edit_base, value='Edit Detector Layout')

text = widget_text( edit_base, scr_xsize=left_xsize, ysize=8, /wrap, uname='edit-explanation', /tracking, $
				value=['Click detectors to display their ID and membership in classes.', $
					'','To set attributes, select channels, enter attributes in relevant fields, and hit <enter> to apply change to selected detectors. ' + $
					'Radial classes can be assigned using "Set Radial Classes".', $
					'','To number pads in order, use "Start Number Sequence" and click on pads in order.'], $
				uvalue='Explanation of the role of the Edit panel.', frame=1)

ebase00 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase00, value='CSV Table Index:')
edit_index_text = widget_text( ebase00, value='', uname='edit-index', /tracking, $
					uvalue='The index (row number in CSV table file) for the selected detector.', scr_xsize=text_xsize)

ebase0 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase0, value='Detector Number:')
edit_id_text = widget_text( ebase0, value='', uname='edit-id', /tracking, /editable, $
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
					uvalue='Edit the "Hermes" ID for the selected detectors (starts at 0). Hit <return> to apply this change.', scr_xsize=text_xsize)

ebase2 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase2, value='Quadrant:')
edit_quadrant_text = widget_text( ebase2, value='', uname='edit-quadrant', /tracking, /editable, $
					uvalue='Edit the "Quadrant" ID for the selected detectors (starts at 0). Hit <return> to apply this change.', scr_xsize=text_xsize)

ebase3 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase3, value='Radial:')
edit_radial_text = widget_text( ebase3, value='', uname='edit-radial', /tracking, /editable, $
					uvalue='Edit the "Radial" ID for the selected detectors (starts at 0). Hit <return> to apply this change. In addition, Radial classes can be assigned programmatically using "Set Radial Classes" and "Pitch".', scr_xsize=text_xsize)

ebase4 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase4, value='Column:')
edit_column_text = widget_text( ebase4, value='', uname='edit-column', /tracking, /editable, $
					uvalue='Edit the "Column" ID for the selected detectors (starts at 0). Hit <return> to apply this change. Note: "column" is defined as X coordinate in initial hardware orientation before any rotation.', scr_xsize=text_xsize)

ebase5 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase5, value='Row:')
edit_row_text = widget_text( ebase5, value='', uname='edit-row', /tracking, /editable, $
					uvalue='Edit the "Row" ID for the selected detectors (starts at 0). Hit <return> to apply this change. Note: "row" is defined as Y coordinate in initial hardware orientation before any rotation.', scr_xsize=text_xsize)

ebase5b = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase5b, value=' Width (mm):')
edit_width_text = widget_text( ebase5b, value='', uname='edit-width', /tracking, /editable, $
					uvalue='Edit the "Width" (mm) of the selected detectors. Hit <return> to apply this change.', scr_xsize=text_xsize)

ebase5c = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
label = widget_label( ebase5c, value='Height (mm):')
edit_height_text = widget_text( ebase5c, value='', uname='edit-height', /tracking, /editable, $
					uvalue='Edit the "Height" (mm) of the selected detectors. Hit <return> to apply this change.', scr_xsize=text_xsize)

label = widget_label( edit_base, value=' ')
ebase6 = widget_base( edit_base, /row, /base_align_center, /align_right, xpad = 3, ypad=0, space=5)
button = widget_button( ebase6, value='Set Radial Classes', uname='edit-set-radial', /tracking, uvalue='Assign pads to Radial Classes based on distance from the origin binned according to the size of "Pitch" (mm).')
label = widget_label( ebase6, value='   Pitch:')
edit_pitch_text = widget_text( ebase6, value='1.0', uname='edit-pitch', /tracking, /editable, $
					uvalue='Edit the "Pitch" (mm) used to bin pads into Radial Classes. Distance from origin is binned using Pitch.', scr_xsize=text_xsize)

label = widget_label( edit_base, value=' ')
ebase20 = widget_base( edit_base, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
;button = widget_button( ebase20, value='Read Layout', uname='edit-read', /tracking, uvalue='Read the detector layout assignments from Kandinski. This is normally unnecessary, as these are stored in the DAQ layout CSV file "'+(*pd).file+'", but may be needed to define a new layout revision with new generation DAQ hardware.')
button = widget_button( ebase20, value='Save Layout', uname='edit-save', /tracking, uvalue='Save the modified detector layout assignments to file "'+(*pd).file+'".')
button = widget_button( ebase20, value='Revert to Saved', uname='edit-revert', /tracking, uvalue='Revert to the original saved detector layout assignments in file "'+(*pd).file+'".')

; Pulser -----------------------------------------

;print,'daq_setup graphics 2e ...'
Pulser_base = widget_base( tab_panel, title='Pulser / Tests', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( pulser_base, value='Tests and Pulser Controls')

text = widget_text( pulser_base, scr_xsize=left_xsize, ysize=10, /wrap, uname='pulser-explanation', /tracking, $
				value=['Control the pulser built into DAQ to (i) probe selected detector channels, (ii) run a procedure to collect data to build gain linearization tables, (iii) run a procedure to collect data to build gain trimming tables to match E and T between channels, (iv) map detector leakage (ELK), (v) baseline (EAN).', '', $
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
;				NOTIFY_REALIZE='OnRealize_daq_setup_pulser_list', $
				/tracking, uvalue='List showing the commands in the currently executing procedure, which is selected above. The cursor will follow the progress of execution.')

;label = widget_label( Pulser_base, value='')
pbase31 = widget_base( Pulser_base, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
button = widget_button( pbase31, value='Save Pulser Parameters', uname='save-pulser', /tracking, uvalue='Save the DAQ pulser parameters, and the channel selection, to a file, which defaults to "DAQ.pulser.csv".')
button = widget_button( pbase31, value='Load Pulser Parameters', uname='load-pulser', /tracking, uvalue='Load the DAQ pulser parameters, and the channel selection, from a file, which defaults to "DAQ.pulser.csv".')

; Debug -----------------------------------------

;print,'daq_setup graphics 3 ...'
debug_base = widget_base( tab_panel, title='Debug', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=left_xsize+20, scr_ysize=left_ysize)
label = widget_label( debug_base, value='Debug Controls')

text = widget_text( debug_base, scr_xsize=left_xsize, ysize=9, /wrap, uname='debug-explanation', /tracking, $
				value=['SCEPTER debug: Lock peak detector arbitration and select the output of only one channel across all SCEPTERs to connect to AUX. Make sure this channel is not disabled on the "Enable" panel.'], $
				uvalue='Explanation of the role of the Debug panel.', frame=1)

debug_base3 = widget_base( debug_base, /column, xpad=1, ypad=1, space=1, /frame, /align_center, /base_align_center, scr_xsize=left_xsize)

dbase03 = widget_base( debug_base3, /row, xpad=1, ypad=0, space=5, /align_center, /base_align_center)
debug_aux_check_id = cw_bgroup2( dbase03, [''], /row, set_value=[0], sensitive=1, $
				/return_index, uname='debug-aux-check',/ nonexclusive, /tracking, $
				uvalue='Check this box to select this group of parameters for modification and transmission to DAQ. Select channels on the mimic display on the right. ' + $
					'Make sure the radio button at the bottom of the window is in "Set Parameters" mode.', xpad=0, ypad=0, space=0)
label = widget_label( dbase03, value='SCEPTER Analogue Debug Output')

enable_debug_aux_mode = widget_combobox(debug_base3, uname='enable-debug-aux', scr_xsize=2*text_xsize, sensitive=0, $
			value=['Off','SCEPTER output to AUX (Lemo #3)'], /tracking, uvalue={label:'Debug',help:'Select the SCEPTER AUX output debug mode to use (using Lemo #3 on DAQ housing). Remember to select "Off" to return to normal operation. ' + $
				'AUX: Select just one SCEPTER output channel to monitor on the AUX output socket.'})

label = widget_label( debug_base, value=' ')
debug_apply_button = widget_button( debug_base, value='Apply to DAQ', uname='debug-apply', /tracking, uvalue='Apply the selected settings (set using the radio buttons and mimic display selection) to the selected DAQ ASICs.')

label = widget_label( debug_base, value=' ')
label = widget_label( debug_base, value=' ')
debug_base9 = widget_base( debug_base, /column, xpad=1, ypad=1, space=space1b, /align_center, /base_align_right)
letters = ['','A','B','C','D','E','F','G','H','I','J','K','L','M']
;dbase91 = widget_base( debug_base9, /row, xpad=1, ypad=0, space=5, /align_right, /base_align_center)
;label = widget_label( dbase91, value='DAQ DDM digital board revision:')
;ddm_version_text = widget_text( dbase91, value=str_tidy((*pm).version.ddm), uname='ddm-version-text', /tracking, $
;					uvalue='Shows the DAQ DDM digital board hardware revision number.', scr_xsize=50)

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

sbase = widget_base( lbase, /row, xpad=0, ypad=0, space=20, /base_align_center, /align_center)
button = widget_button( sbase, value='Save DAQ Pars', uname='save-daq', /tracking, uvalue='Save the main DAQ parameters (Scepter, Hymod) to file (extension ".parameters.csv"). Exceptions: ' + $
					'Channel disable parameters are saved separately from the "Enable" panel.')
button = widget_button( sbase, value='Load and Apply DAQ Pars', uname='load-daq', /tracking, uvalue='Load the main DAQ parameters (Scepter, Hymod) from a ".parameters.csv" file and Apply ' + $
					'them to DAQ. Exceptions: (i) Channel disable parameters are saved separately from the "Enable" panel.')

;------------------------------------------------------------------------------------------------


rbase = widget_base( tbase, /column, xpad=0, ypad=0, space=5, /base_align_center, /align_center)

tab_panel2 = widget_tab( rbase, location=3, /align_center, uname='detector-tab-panel')	;, $
;					NOTIFY_REALIZE='OnRealize_daq_setup_preview_tab')

detector_base = widget_base( tab_panel2, title='  Detector Mimic Panel  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=right_xsize+4, scr_ysize=right_ysize)

detector = detector_mimic( detector_base, data=pd, uname='detector', uvalue='Select detectors(s) by ' + $
					'clicking on individual detector pads, or members of groups of pads or Classes. Select a grouping class using the ' + $
					'"Detector Selection Class" droplist. If the "Detector Selection Class" mode is not in "Individual" all members of a class ' + $
					'will be selected together. Click again to deselect.', /tracking, legend=4, position=((n_detectors eq 384) ? 0 : 1), $
					xsize_min=right_xsize2, xsize_max=right_xsize2, ysize_max=right_ysize2, csize=csize, colours=colours)

figure_base = widget_base( tab_panel2, title='  Detector Layout Diagram  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=right_xsize, scr_ysize=right_ysize)

picpath = geopixe_root + 'daq' + slash()
prefix = 'DAQ_' + str_tidy((*pm).n_detectors)

fig = prefix + '-layout.png'
pic = picture_button( figure_base, picpath+fig, uname='figure', xsize=right_xsize-2, ysize=right_ysize, $
			/tracking, uvalue='Layout of the DAQ detector and index assignments, as viewed from the "Sample side".', pushbutton_events=0)

data_flow_base = widget_base( tab_panel2, title='  Data-Flow Diagram  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=right_xsize, scr_ysize=right_ysize)

pic = picture_button( data_flow_base, picpath+'HYMOD-DAQ-data-flow.png', uname='data-flow', xsize=right_xsize-2, ysize=right_ysize, $
			/tracking, uvalue='Layout of HYMOD processor data-flow for the DAQ data acquisition system.', pushbutton_events=0)

system_layout_base = widget_base( tab_panel2, title='  System Layout  ', /column, xpad=1, ypad=1, space=5, $
					/align_center, /base_align_center, scr_xsize=right_xsize, scr_ysize=right_ysize)

pic = picture_button( system_layout_base, picpath+'DAQ-layout.png', uname='system-layout', xsize=right_xsize-2, ysize=right_ysize, $
			/tracking, uvalue='Overview layout of DAQ detector system.', pushbutton_events=0)

mbase = widget_base( rbase, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
mimic_toggle = cw_bgroup2( mbase, ['Set Params','Show Params'], /row, $
					/exclusive, /no_release, /return_index, /tracking, $
					uname='mimic-toggle', set_value=0, xpad=0, ypad=0, space=0, $
					uvalue='Select the mimic display mode between "Set" and "Display" parameters. ' + $
							'"Show Parameters" uses false colour to show parameters across the array. ' + $
							'"Set Parameters" allows setting parameters for detectors channels in various groupings, established using the droplist to the right.')

colour_display_text = widget_text( mbase, value='', uname='colour-display', /tracking, editable=0, $
;					notify_realize='onrealize_daq_setup_detector', $
					uvalue='Shows the name of the parameter displayed across the detector array in "Colour parameter" mode.', scr_xsize=100)

label = widget_label( mbase, value=' ')
label = widget_label( mbase, value='Select Class:')
detector_mode = widget_combobox(mbase, uname='select-mode', scr_xsize=80, $
;			NOTIFY_REALIZE='OnRealize_daq_setup_detector_mode', $
			value=['One only','Individual','Radial','Column','Row','Chip','Quadrant','All'], /tracking, uvalue='Select the grouping of detector pads activiated on a mouse click.')

button = widget_button( mbase, value='Clear', uname='clear-select', /tracking, uvalue='Clear all detector selections.')	;, $
;			NOTIFY_REALIZE='OnRealize_daq_setup_clear')
button = widget_button( mbase, value='Get', uname='get-select', /tracking, uvalue='Set detector selections based on detectors present in a .SPEC file or a .select.csv table.')
button = widget_button( mbase, value='Save', uname='save-select', /tracking, uvalue='Save detector selections to a .select.csv table file.')

picpath = 'maia' + slash()
hbase = widget_base( tlb, /row, xpad=0, ypad=0, space=5, /base_align_center, /align_center)
help = widget_text( hbase, scr_xsize=help_xsize-254, ysize=3, /wrap, uname='help', /tracking, $
				uvalue='Help text ...', frame=0)
pic = picture_button( hbase, picpath+'Maia-Logo.png', tracking=0, pushbutton_events=0)	;, xsize=144, ysize=48

;print,'daq_setup copy state ...'
														; make local copy for enables, sliders, etc.
copy_pointer_data, pm, pl, /init						; See notes at top of Event routine.
									
state = { $
		path:				ptr_new(path), $			; pointer to current path
		dpath:				ptr_new(dpath), $			; pointer to current raw data blog path
		default:			default, $					; DAQ defaults
;		pimage:				pimage, $					; pointer to DA image
		ppspec:				ppspec, $					; pointer to E spectra
;		ppgroup:			ppgroup, $					; pointer to Group spectra
		playout:			pd, $						; pointer to layout struct array
		pdaq:				pm, $						; pointer to daq parameters struct array
		plocal:				pl, $						; pointer to local copy of daq parameters struct (for enables, etc.)
		psocket:			ps, $						; pointer to daq socket port and command parameters
		pdisable:			pe, $						; pointer to daq disable detector channels
		preadout:			pr, $						; pointer to readout enables
		plaunch:			ptr_new(/allocate_heap), $	; pointer data for use with Notify to Launch
;		pshrmem_da:			pshrmem_da, $				; pointer to DA shared memory (from DAQ_Launch)
		
		select:				*pe, $						; current selection state
		asic_select:		intarr(n_detectors), $		; asic selection state
		pulser_select:		intarr(n_detectors), $		; pulser selection state
		debug_select_aux:	intarr(n_detectors), $		; debug selection state
		tab:				0, $						; tab selection
		sequence:			{on:0, count:0}, $			; sequence controls
		colour_vector:		fltarr(n_detectors), $		; store last parameter vector for colouring pads
		colour_title:		'', $						; store last title
		last_check:			0, $						; last check box used for colour mode
		pseed:				ptr_new(1L), $				; seed for Randomu
		scr_xsize:			0, $						; TLB X size
		scr_ysize:			0, $						; TLB Y size
		evt_file:			'', $						; default blog data file to scan for PVs
		beams:				beams, $					; names of beams
		
		enable_quad_freeze:	0, $						; make Quad phase check boxes insensitive for a cycle
		enable_stream_freeze:	0, $					; make Streams phase check boxes insensitive for a cycle		
		colour_mode:		0, $						; set/colour radio mode
		cal_mode:			0, $						; energy cal mode
		group_mode:			1, $						; group select droplist mode
		group_mask:			replicate(1,8), $			; controls which group_modes are legal at any time
		old_mask:			[0,0,0,0,0,1,1,1], $		; save the last mask used in Hermes, Scepter panels
		check_mode:			0, $						; currently selected check-box
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
		pHYMOD_debug:		ptr_new(/allocate_heap), $	; pulser, synth, debug EAN, AUX, EBLK, Bake (from DAQ Launch)
		table: 	replicate({	chip:		0, $			; table chip index for a column of the table
							par:		0, $			; table chip parameter index
							column:		'', $			; column name
							check:		0 }, 40), $		; index from column into check_id list
		sort:	{ column:	0, $						; sort by column
					index:	indgen(n_detectors)}, $				; index from table row to CSV index
					
		start_text:			start_text, $				; start button text strings
		debug_aux_mode:		0, $						; current AUX debug mode
		debug_ean_check:	0, $						; debug EAN fields selection
		debug_aux_check:	0, $						; debug AUX fields selection
		tracking:			1, $						; tracking mode
		
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
		
		summary_table:		summary_table, $			; parameter summary table ID
;		enable_rr_check:	enable_rr_check, $			; check box ID for RR phases
		enable_quad_check:	enable_quad_check, $		; check box ID for Quanrant enables
		enable_stream_check: enable_stream_check, $		; check box ID for stream enables

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
		check_ids_scepter_whole: check_ids_scepter_whole, $ ; scepter check IDs for whole array
		ids_scepter_channel: ids_scepter_channel, $		; scepter parameters IDs by channel
		ids_scepter_chip:	ids_scepter_chip, $			; scepter parameter IDs by chip
		ids_scepter:		ids_scepter, $				; all scepter parameter IDs
		
		beam_particle_mode:	beam_particle_mode, $		; beam particle droplist ID
		beam_energy_text:	beam_energy_text, $			; beam energy text ID
		charge_scale_mode:	charge_scale_mode, $		; charge scale droplist ID
		deadtime_auto:		device_deadtime_auto, $		; auto checkbox ID
		
		controls_temp_hymod_fpga_text: controls_temp_hymod_fpga_text, $	; controls HYMOD FPGA T readback text ID
		controls_temp_hymod_cpu_text: controls_temp_hymod_cpu_text, $	; controls HYMOD CPU T readback text ID
		controls_temp_hymod_board_text: controls_temp_hymod_board_text, $	; controls HYMOD board T readback text ID
		controls_temp_hymod_eth0_text: controls_temp_hymod_eth0_text, $	; controls HYMOD ETH0 T readback text ID
		controls_temp_hymod_eth1_text: controls_temp_hymod_eth1_text, $	; controls HYMOD ETH1 T readback text ID

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
		
		enable_debug_aux_mode:	enable_debug_aux_mode, $	; debug AUX droplist ID
		debug_aux_check_id:	debug_aux_check_id, $			; debug AUX fields selection
		
		help:				help $						; help text ID
		}

print,'daq_setup realize ...'
child = widget_info( tlb, /child)
pstate = ptr_new(state, /no_copy)
widget_control, child, set_uvalue=pstate
widget_control, tlb, /realize
geo = widget_info( tlb, /geometry)
;help, geo, /str
(*pstate).scr_xsize = geo.scr_xsize
(*pstate).scr_ysize = geo.scr_ysize

register_notify, tlb, ['path', 'dpath', $		; new paths
						'daq-display'], $		; update table with new DAQ values
						from=group

xmanager, 'daq_setup', tlb, /no_block
print,'daq_setup running ...'
end
