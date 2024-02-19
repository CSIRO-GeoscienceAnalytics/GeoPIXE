
;	Fit Results table.

pro scan_list_event, event

COMPILE_OPT STRICTARR
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
		warning, timeout=300,'scan_list_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif
widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

no_list = 0
if ptr_good(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

p = (*pstate).plist
if ptr_valid(p) eq 0 then goto, bad_ptr
if size(*p,/tname) ne 'POINTER' then begin
	no_list = 1
endif else begin
	if ptr_valid( (*p)[0] ) eq 0 then no_list=1
	if no_list eq 0 then if size(*(*p)[0],/tname) ne 'STRUCT' then no_list=1
endelse
pindex = (*pstate).pindex

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

coords = (*pstate).src_coords_list

ps = (*pstate).ps
pm = (*pstate).pm

case !version.os_family of
	'MacOS': begin
		update_xsize = 110
		led_xsize = 70
		led_xsize2 = 85
		led_xsize3 = 70
		end
	'unix': begin
		update_xsize = 110
		led_xsize = 70
		led_xsize2 = 85
		led_xsize3 = 70
		end
	else: begin
		update_xsize = 80
		led_xsize = 60
		led_xsize2 = 75
		led_xsize3 = 60
		end
endcase

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end
			'scan-list-update': begin
				update_scan_list_table, pstate
				if no_list eq 0 then begin
					np = n_elements(*pindex)
					(*pstate).sel.top = np-1
					view = widget_info( (*pstate).table, /table_view)
					j = (*pstate).sel.top
					widget_control, (*pstate).table, set_table_select=[-1,j,-1,j]
					widget_control, (*pstate).table, set_table_view=view

					*(*pstate).pselect = (*pstate).sel
					notify, 'scan-edit-select', (*pstate).pselect, from=event.top
				endif
				end
			'scan-list-select': begin
				(*pstate).sel.top = *event.pointer
				view = widget_info( (*pstate).table, /table_view)
				j = (*pstate).sel.top < (n_elements(*pindex)-1)
				widget_control, (*pstate).table, set_table_select=[-1,j,-1,j]
				widget_control, (*pstate).table, set_table_view=view

				*(*pstate).pselect = (*pstate).sel
				notify, 'scan-edit-select', (*pstate).pselect, from=event.top
				end
			'scan-sequence-next': begin
				scan_list_get_state, pstate, error=err
				if err then goto, finish
				if (*pstate).master eq 0 then goto, finish
				log_message, (*pstate).comms, type='INFO', 'Notify: scan-sequence-next'
				scan_list_next, ps, p, pindex, pm, pstate, event.top, phase=(*pstate).phase.pnext, /init
				scan_list_put_state, pstate, error=err
				end
			'scan-command': begin
				scan_list_get_state, pstate, error=err
				if err then goto, finish
				if (*pstate).master eq 0 then goto, finish
				log_message, (*pstate).comms, type='INFO', 'Notify: scan-list-command: '+*event.pointer
				case *event.pointer of
					'start': begin
						scan_list_start, ps, p, pindex, pm, pstate, no_list, event.top, phase=(*pstate).phase.pstart, /init
						scan_list_put_state, pstate, /frame, error=err
						end
					'stop': begin
						scan_list_stop, ps, pstate, event.top
						scan_list_put_state, pstate, error=err
						end
					'pause': begin
						scan_list_pause, ps, pm, no_list, pstate
						scan_list_put_state, pstate, error=err
						end
					'skip': begin
						(*pm).control.status.raster_status = daq_launch_raster_status( ps, pm)
						go_on = ((*pm).control.status.raster_status eq 1) or ((*pm).control.status.raster_status eq 2)
						if (*pstate).scan_sequence.active and go_on then begin
							(*pstate).changed = 1
							(*pstate).scan_sequence.raster_on = go_on
							scan_list_stop, ps, pstate, event.top, /no_complete
							wait, 15
							scan_list_next, ps, p, pindex, pm, pstate, event.top, phase=(*pstate).phase.pnext, /init
							scan_list_put_state, pstate, error=err
						endif
						end
					else:
				endcase
				end
			else:
		endcase
		goto, finish
		end
	'WIDGET_TRACKING': begin
		if (*pstate).tracking eq 0 then goto, finish
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
		u = widget_info( event.id, /uname)
		case u of

			'home-button': begin									; timer to check home status
				template =  	{		status:		'', $			; 'homing', 'homed', 'not-homed', 'fault'
										error:		''}				; error message
				ref = 'home'
				stat = get_vsub( (*pstate).vsub_stage, 'home', template=template, error=error)
				if (size(stat, /tname) eq 'STRUCT') then begin
					if (error eq 0) then begin
						(*pm).status.home = -1
						if stat.status eq 'homed' then (*pm).status.home = 1
						if stat.status eq 'homing' then (*pm).status.home = -1
						if stat.status eq 'not-homed' then (*pm).status.home = 0
					endif else begin
;						print,'Home SUB error: '+stat.error
						log_message, (*pstate).vsub_stage, type='WARNING', 'Home timer, get_vsub error: '+stat.error
					endelse
				endif

				if (*(*pstate).presets)[0].name eq 'none' then begin
					preset_names = ['none']
					presets = {name:'none', x:0.0, Y:0.0, Z:0.0}
					
					r = req_vsub( (*pstate).vsub_stage, 'List-Move-Presets', error=err, message=mess)
					if err then begin
;						print,'List-Move-Presets SUB error: '+((size(r, /tname) eq 'STRUCT') ? r.error : "?")
;						warning, timeout=300, 'scan_list_event',['List-Move-Presets stage error.','REQ to Stage VSUB returned an error:',mess]
						log_message, (*pstate).vsub_stage, type='WARNING', 'Presets timer, req_vsub error: '+((size(r, /tname) eq 'STRUCT') ? r.error : "?")
					endif else begin
						n = n_elements(r)
						if n ge 1 then begin
							presets = replicate( {name:'', x:0.0, Y:0.0, Z:0.0}, n)
							for i=0,n-1 do begin
								presets[i].name = r[i]
							endfor
							*(*pstate).presets = presets
							preset_names = presets.name
							q = where( strlowcase(strtrim(preset_names,2)) eq 'sample change', nq)
							(*pstate).preset_mode = (nq gt 0) ? q[0] : 0
							widget_control, (*pstate).preset_mode_id, set_value='  '+preset_names, set_combobox_select=(*pstate).preset_mode
						endif
					endelse
				endif
				widget_control, (*pstate).home_button, timer = (*pstate).timer_home
				end

			'start-button': begin									; timer to check sequence
				scan_list_get_state, pstate, /frame, error=err

				; 'scan_sequence.active' indicates sequence controlled locally. If it is off, then either sequence
				; controlled locally has finished or it is controlled remotely.
				; remote: always check for scan progress, but do not test for 'next'.
				; local: if scan started, ignore scan progress until 12s after start time to give it time to get going.

				valid = ((*pstate).scan_sequence.active eq 0) or  $
						((*pstate).scan_sequence.active and ((systime(1)-(*pstate).scan_sequence.time_start) gt 12.)) 
				if (err eq 0) and valid then begin

					template =  {		status:		'', $			; 'moving', 'stopped', 'paused', 'fault'
										key:		'', $			; scan KVS key
										progress:	0.0, $			; progress % 
;										duration:	0L, $			; seconds duration
;										remaining:	0L, $			; seconds remaining
										stroke:		0L, $			; last 'stroke' completed
										error:		''}				; error message
					ref = 'scan'
					stat = get_vsub( (*pstate).vsub_stage, ref, template=template, error=error)
					if (error eq 0) and (size(stat, /tname) eq 'STRUCT') then begin
						if (*pm).stage.scan.status ne stat.status then begin
							(*pm).stage.scan.status = stat.status
							widget_control, (*pstate).help, set_value='Stage SCAN status: ' + stat.status
						endif
						(*pm).stage.scan.progress = stat.progress
;						help, stat

						if stat.status eq 'scanning' then begin
							(*pstate).changed = 1
							(*pstate).scan_sequence.raster_on = 1
							(*pstate).scan_sequence.stroke = stat.stroke
							(*pstate).scan_sequence.pause = 0
							(*pstate).scan_sequence.error = 0 
							(*pstate).scan_sequence.progress = stat.progress
							widget_control, (*pstate).append_text, set_value=str_tidy((*pstate).scan_sequence.stroke)

							n = n_elements(*pindex)
							if (n ge 1) and (no_list eq 0) then begin
								key = strarr(n)
								for i=0L,n-1 do begin
									j = (*pindex)[i]
									key[i] = (*(*p)[j]).key
								endfor
								q = where( stat.key eq key, nq)
								if nq gt 0 then begin
									(*pstate).scan_sequence.scan.index = q[0]
;									print, 'Scanning: Found scan key = ',q[0],'  ',stat.key
								endif
							endif
						endif
						if stat.status eq 'paused' then begin
							(*pstate).changed = 1
							(*pstate).scan_sequence.raster_on = 1
							(*pstate).scan_sequence.pause = 1
							(*pstate).scan_sequence.error = 0
							(*pstate).scan_sequence.progress = stat.progress
						endif
						if stat.status eq 'stopped' then begin
							(*pstate).changed = 1
							(*pstate).scan_sequence.raster_on = 0
							(*pstate).scan_sequence.pause = 0
							(*pstate).scan_sequence.error = 0
						endif
						if stat.status eq 'fault' then begin
							(*pstate).changed = 1
							(*pstate).scan_sequence.raster_on = 0
							(*pstate).scan_sequence.pause = 0
							(*pstate).scan_sequence.error = 1
							widget_control, (*pstate).help, set_value='Stage SCAN fault: ' + stat.error
							log_warning,'Scan-List event','Stage SCAN fault: ' + stat.error
						endif

;						If scanning, test source power gone. Pause or Resume ...

;						print,' Scan: active = ', (*pstate).scan_sequence.active, ', On = ', (*pstate).scan_sequence.raster_on, ', Pause = ', (*pstate).scan_sequence.pause, ', Power = ',(*pm).status.power

						shutter_bad = 0
						maia_up = (*ps).open 
						if (((*pm).status.bias lt 70.) or ((*pm).status.temp gt 0.) or ((*pm).status.bpinterlock eq 0)) then maia_up = maia_up < 0
						maia_up0 = maia_up
						blog_good = 1
						if (((*pm).status.discard_rate gt 100000.) and ((*pm).status.discard eq 0)) or ((*pm).status.blog eq 0) then begin
							blog_good = 0
							maia_up = maia_up < 0
						endif
						power_status = (*pm).status.power
						if ((*pstate).led2[5] ne 1) then shutter_bad=1					; shutter not open

						if (*pstate).scan_sequence.active and ((*pstate).scan_sequence.raster_on eq 1) $
									and ((*pstate).scan_sequence.error eq 0) and ((*pstate).power_ignore eq 0) then begin
							if ((*pstate).scan_sequence.pause eq 0) and ((power_status le 0) or (maia_up le 0) or shutter_bad) then begin
;								print,'		Do Pause ...'
								log_message, (*pstate).vsub_stage, type='INFO', 'scan_list, Do Pause (status [maia,blog,power,shutter] = '+str_tidy(maia_up0)+','+str_tidy(blog_good)+','+str_tidy(power_status)+','+str_tidy(1-shutter_bad)+').'
								scan_list_pause, ps, pm, no_list, pstate, pause=1, /soft
							endif
							if ((*pstate).scan_sequence.pause eq 1) and (power_status eq 1) then begin
;								if (*pstate).scan_sequence.lock_paused eq 0 then print,'		Do Resume ...'
								log_message, (*pstate).vsub_stage, type='INFO', 'scan_list, Do Resume (status [maia,blog,power,shutter] = '+str_tidy(maia_up0)+','+str_tidy(blog_good)+','+str_tidy(power_status)+','+str_tidy(1-shutter_bad)+').'
								scan_list_pause, ps, pm, no_list, pstate, pause=0, /soft
							endif
						endif

;						If scanning, test to check that stage is still moving ...

						if ((*pstate).scan_sequence.raster_on eq 1) and ((*pstate).scan_sequence.error eq 0) then begin
							if ((*pstate).scan_sequence.pause eq 0) then begin
								dx = (*pm).target.position.x - (*pm).previous.position.x
								dy = (*pm).target.position.y - (*pm).previous.position.y
								if ((abs(dx) lt 0.01) and (abs(dx) lt 0.01)) and ((systime(1)-(*pstate).scan_sequence.time_start) gt 60.) and (*pstate).scan_sequence.active then begin
									if widget_info( (*pstate).alarm_stage_popup, /valid) eq 0 then begin
										alarm_popup, group=event.top, tlb=atlb, title='Scan List: Alarm Condition', message='Stage Frozen?'
										(*pstate).alarm_stage_popup = atlb
									endif
								endif
								if (abs(dx) gt 0.1) or (abs(dx) gt 0.1) then begin
									if widget_info( (*pstate).alarm_stage_popup, /valid) then begin
										notify, 'cancel', from=event.top
									endif
								endif
							endif
						endif
							
						if ((*pstate).scan_sequence.raster_on eq 1) then begin
							if ((*pm).status.discard eq 1) and ((systime(1)-(*pstate).scan_sequence.time_start) gt 60.) and (*pstate).scan_sequence.active then begin
								if widget_info( (*pstate).alarm_maia_popup, /valid) eq 0 then begin
									alarm_popup, group=event.top, tlb=atlb, title='Scan List: Alarm Condition', message='Maia discard while Raster on?'
									(*pstate).alarm_maia_popup = atlb
								endif
							endif else begin
								if widget_info( (*pstate).alarm_maia_popup, /valid) then begin
									notify, 'cancel', from=event.top
								endif
							endelse
						endif
						(*pm).previous.position = (*pm).target.position

;						Was scanning and now has stopped. Try stop Maia and next scan ...

						if (*pstate).scan_sequence.active and ((*pstate).scan_sequence.raster_on eq 0) then begin
							print,'scan_list_next: Issue Maia Endrun 1.'
							scan_list_maia_stop, ps, pstate, event.top
						endif

						if (*pstate).scan_sequence.active and ((*pstate).scan_sequence.raster_on eq 0) $
									and ((*pstate).scan_sequence.error eq 0) then begin
							(*pstate).changed = 1
							(*pstate).scan_sequence.active = 0
							(*pstate).scan_sequence.progress = 0.0
							*(*pstate).phase.pstart = 0
;							print,'scan_list (Notify): Scan has "stopped", so try next ...'
							log_message, (*pstate).vsub_stage, type='INFO', 'Start timer, Scan has "stopped", so try next ...'

;						`	Note: to debug into here, you should break at 'skip-button' timer.

							scan_list_next, ps, p, pindex, pm, pstate, event.top, phase=(*pstate).phase.pnext, /init
						endif

;						Error in Raster, abort sequence ...

						if (*pstate).scan_sequence.active and ((*pstate).scan_sequence.error eq 1) then begin
							(*pstate).changed = 1
							(*pstate).scan_sequence.active = 0
							(*pstate).scan_sequence.error = 0
							if size(stat,/tname) eq 'STRUCT' then begin
								widget_control, (*pstate).help, set_value='Error returned from Stage [scan: "'+stat.scan+'"]: '+state.error
								warning, timeout=300, 'scan_list_event',['Error returned from Stage.','Scan: "'+stat.scan+'".',stat.error]
							endif else begin
								widget_control, (*pstate).help, set_value='Error returned from Stage. But bad "status" reply returned.'
								warning, timeout=300, 'scan_list_event',['Error returned from Stage.','But bad "status" reply returned.']
							endelse
						endif							
						update_scan_list_time, pstate
					endif else begin
						log_message, (*pstate).vsub_stage, type='WARNING', 'Scan timer, get_vsub error: '+((size(stat, /tname) eq 'STRUCT') ? stat.error : "?")

;						Don't check for error here, as get regular ones due to "Controller state not RASTER from Galil" when in RPAUSE state
;
;						widget_control, (*pstate).help, set_value='VSUB Stage error. Retry KVS and VSUB connections ...'
;						scan_list_retry_kvs, pstate, error=err
					endelse
					scan_list_put_state, pstate
				endif
start_again:
				widget_control, (*pstate).start_button, timer = (*pstate).timer_sequence
				end

			'skip-button': begin
				scan_list_check_kvs, pstate, error=err
				if (*pstate).master eq 0 then begin
					*(*pstate).phase.pnext = 0
					*(*pstate).phase.pstart = 0
				endif
				if (err eq 0) and (*pstate).master then begin
					if *(*pstate).phase.pstart gt 0 then begin
						*(*pstate).phase.pnext = 0
						scan_list_start, ps, p, pindex, pm, pstate, no_list, event.top, phase=(*pstate).phase.pstart
					endif
					if *(*pstate).phase.pnext gt 0 then begin
						*(*pstate).phase.pstart = 0
						scan_list_next, ps, p, pindex, pm, pstate, event.top, phase=(*pstate).phase.pnext
					endif
					scan_list_put_state, pstate
				endif
				widget_control, (*pstate).skip_button, timer = (*pstate).timer_skip
				end

			'go-button': begin							; timer to check move status
				scan_list_check_kvs, pstate, error=err
				if err eq 0 then begin
					valid = ((*pstate).scan_sequence.active eq 0) or  $
							((*pstate).scan_sequence.active and ((systime(1)-(*pstate).scan_sequence.time_start) gt 12.)) 
					if valid then begin
	
						template =  {		status:		'', $			; 'moving', 'stopped', 'paused', 'fault', 'locked'
											progress:	0.0, $			; progress % 
											error:		''}				; error message
						ref = 'move'
						stat = get_vsub( (*pstate).vsub_stage, ref, template=template, error=error)
						if (error eq 0) and (size(stat, /tname) eq 'STRUCT') then begin
							if (*pm).stage.move.status ne stat.status then begin
								(*pm).stage.move.status = stat.status
								widget_control, (*pstate).help, set_value='Stage MOVE status: ' + stat.status
							endif
	
							(*pm).status.moving = -1
							if stat.status eq 'moving' then (*pm).status.moving = 1
							if stat.status eq 'stopped' then (*pm).status.moving = -1
							if stat.status eq 'locked' then (*pm).status.moving = 0
							if stat.status eq 'fault' then begin
								widget_control, (*pstate).help, set_value='Stage MOVE fault: ' + stat.error
							endif
						endif else begin
							log_message, (*pstate).vsub_stage, type='WARNING', 'Go timer, get_vsub error: '+((size(stat, /tname) eq 'STRUCT') ? stat.error : "?")
						endelse
					endif
					scan_list_put_state, pstate
				endif
				widget_control, (*pstate).go_button, timer = (*pstate).timer_move
				end

			'frame-text': begin								; timer to check KVS frame scan list
				scan_list_check_kvs, pstate, error=err
				if err eq 0 then begin
					if ((*pstate).lock eq 0) and ((*pstate).scan_sequence.active eq 0) then begin
						if err eq 0 then begin
							scan_list_tidy, pstate
							load_kvs_scan_list, pstate, /protect, error=err
							if err eq 0 then begin		
								update_scan_list_table, pstate
				
								get_widget_table, (*pstate).table, view=view, rows=rows, columns=columns
								(*pstate).sel.top = (*pstate).sel.top < (rows-1)
								widget_control, (*pstate).table, set_table_select=[-1,(*pstate).sel.top,-1,(*pstate).sel.top]
								widget_control, (*pstate).table, set_table_view=view
							endif
						endif
					endif
					scan_list_put_state, pstate
				endif
				widget_control, (*pstate).frame_id_text, timer = (*pstate).timer_frame
				end
			'coords-src-mode': begin							; timer to read newscan specs
				scan_list_check_kvs, pstate, error=err
				if err then goto, coords_again

				(*pm).status.kvs = err ? 0 : 1
				(*pm).status.stage = get_vsub_server_active( (*pstate).vsub_stage, error=error)
				(*pm).status.safety = get_vsub_server_active( (*pstate).vsub_safety, error=error)
				rp = (*pstate).scan_sequence.pause ? 1 : 2
				r = (*pstate).scan_sequence.raster_on ? rp : 0
				if (*ps).open then begin
					v = socket_command_get( ps, 'connected', class='status.blog', error=error)
					if error eq 0 then (*pm).status.blog = v[0]
					if error then log_message, (*pstate).comms, type='WARNING', 'Coords timer, error reading status.blog.connected'
					v = socket_command_get( ps, 'discard', class='status.blog', error=error)
					if error eq 0 then (*pm).status.discard = v[0]
					if error then log_message, (*pstate).comms, type='WARNING', 'Coords timer, error reading status.blog.discard'

					v = socket_command_get( ps, 'bias.voltage', class='status.detector', /float, error=error)
					if error eq 0 then (*pm).status.bias = v[0]
					if error then log_message, (*pstate).comms, type='WARNING', 'Coords timer, error reading status.detector.bias.voltage'
					v = socket_command_get( ps, 'temp', class='status.detector', /float, error=error)
					if error eq 0 then (*pm).status.temp = v[0]
					if error then log_message, (*pstate).comms, type='WARNING', 'Coords timer, error reading status.detector.temp'
					v = socket_command_get( ps, 'bpinterlock', class='status', error=error)
					if error eq 0 then (*pm).status.bpinterlock = v[0]
					if error then log_message, (*pstate).comms, type='WARNING', 'Coords timer, error reading status.bpinterlock'
					v  = socket_command_get( ps, 'event.discard.rate', class='status', error=error)
					if error eq 0 then (*pm).status.discard_rate = v[0]
					if error then log_message, (*pstate).comms, type='WARNING', 'Coords timer, error reading discard rate'

				endif else (*pm).status.blog=0

				(*pm).status.excillum = get_vsub_server_active( (*pstate).vsub_excillum, error=error)
				if (*pm).status.excillum and (error eq 0) then begin
					estate = get_vsub( (*pstate).vsub_excillum, 'state', error=error)
					if (strmid(estate,0,1) eq "'") or (strmid(estate,0,1) eq '"') then estate=unstringify(estate)
					if error eq 0 then begin
						epower = get_vsub( (*pstate).vsub_excillum, 'generator_emission_power', error=error)
						if error eq 0 then begin
							max_epower = get_vsub( (*pstate).vsub_excillum, 'generator_max_power_at_settings', error=error)
							if error eq 0 then begin
								(*pm).status.power = ((float2(epower) gt 0.95*float2(max_epower)) and (estate eq 'on')) ? 1 : 0

								if (estate ne 'on') and (*pstate).scan_sequence.power_on  $
											and (*pstate).scan_sequence.active and ((*pstate).scan_sequence.raster_on eq 1) $
											and ((*pstate).scan_sequence.error eq 0) and ((*pstate).power_ignore eq 0) then begin
									if (widget_info( (*pstate).alarm_excillum_popup, /valid) eq 0) then begin
										alarm_popup, group=event.top, tlb=atlb, title='Scan List: Alarm Condition', message='Excillum in STATE ("'+str_tidy(estate)+'") during scan. Retry state "on"?'
										(*pstate).alarm_excillum_popup = atlb
										(*pstate).first_push_state = systime(1)
									endif
									if (*pstate).master and (estate eq 'ready') and (systime(1)-(*pstate).first_push_state gt 100.) $			; wait 100s before trying the first push to "ON"
																		and (systime(1)-(*pstate).last_push_state gt 1000.) then begin			; don't try again until after 1000s
										log_message, (*pstate).vsub_excillum, type='INFO', 'REQ to Excillum: push state back to "on"  ...'
										r2 = req_vsub( (*pstate).vsub_excillum, 'SET_VALUE', dictionary('name','state','value','on'), seq=seq, error=err, message=mess, errno=errno)
										if err then begin
											log_message, (*pstate).vsub_excillum, type='ERROR', 'Error sending state ON to Excillum: '+mess
										endif
										(*pstate).last_push_state = systime(1)
									endif
								endif else begin
									if widget_info( (*pstate).alarm_excillum_popup, /valid) then begin
										notify, 'cancel', from=event.top
									endif
								endelse

							endif else begin
								log_message, (*pstate).vsub_excillum, type='WARNING', 'Coords timer, Excillum "generator_max_power_at_settings" get_vsub error: '+((size(max_epower, /tname) eq 'STRUCT') ? max_epower.error : "?")
								(*pm).status.power = -1
							endelse
						endif else begin
							log_message, (*pstate).vsub_excillum, type='WARNING', 'Coords timer, Excillum "generator_emission_power" get_vsub error: '+((size(epower, /tname) eq 'STRUCT') ? epower.error : "?")
							(*pm).status.power = -1
						endelse
					endif else begin
						log_message, (*pstate).vsub_excillum, type='WARNING', 'Coords timer, Excillum "state" get_vsub error: '+((size(estate, /tname) eq 'STRUCT') ? estate.error : "?")
						(*pm).status.power = -1
					endelse
				endif else begin
					log_message, (*pstate).vsub_excillum, type='WARNING', 'Coords timer, Excillum "is_usable" error (useable='+str_tidy((*pm).status.excillum)+', error='+str_tidy(error)+').'
					(*pm).status.power = -1
				endelse

				maia_up = (*ps).open 
				if (((*pm).status.bias lt 70.) or ((*pm).status.temp gt 0.) or ((*pm).status.bpinterlock eq 0)) then maia_up = maia_up < 0
				power_status = (*pm).status.power
				if ((*pstate).led2[5] ne 1) then power_status = power_status < 0		; shutter not open
				if (maia_up le 0) then power_status = power_status < 0

; 				LEDs    0 (off)     1 (Red)     2 (Green)

				(*pstate).led[0] = (*pm).status.kvs + 1
				(*pstate).led[1] = (*pm).status.stage + 1
				(*pstate).led[2] = (*pm).status.safety + 1
				(*pstate).led[3] = maia_up + 1

				(*pstate).led[4] = (*pstate).lock + 1
				(*pstate).led[5] = fix2(r)
				(*pstate).led[6] = (*pm).status.home + 1
				(*pstate).led[7] = (*pm).status.blog + 1

				(*pstate).led[8] = (*pm).status.excillum + 1
				(*pstate).led[9] = power_status + 1

				(*pstate).led2[0] = (*pstate).lock + 1
				(*pstate).led2[1] = fix2(r)
				(*pstate).led2[2] = (*pm).status.home + 1
				(*pstate).led2[3] = (*pm).status.moving + 1
				(*pstate).led2[4] = (*pm).status.moving + 1

				s = ((*pm).status.power and ((*pstate).led2[5] eq 1)) ? 2 : 1
				(*pstate).led2[6] = (*pm).status.excillum ? s : 0

				for i=0,9 do begin
					widget_control, (*pstate).led_id[i], set_value=(*pstate).led[i]
				endfor
				for i=0,6 do begin
					widget_control, (*pstate).led2_id[i], set_value=(*pstate).led2[i]
				endfor

				if err eq 0 then begin
					templatep =  {		x:	0.0, $				; X stage/ captured cursor position
										y:	0.0, $				; Y
										z:	0.0}				; Z
					kname = (*pstate).kvs_prefix + 'CF.'
					ref = kname + 'position'
					pos = get_kvs( (*pstate).kvs, ref, template=templatep, error=error)
					if (error eq 0) and (size(pos, /tname) eq 'STRUCT') then begin
						(*pm).finder.position = pos
					endif else begin
;						log_message, (*pstate).kvs, type='WARNING', 'Coords timer get_kvs '+ref+' error.'
					endelse
	
					kname = (*pstate).kvs_prefix + 'GP.1.'
					ref = kname + 'position'
					pos = get_kvs( (*pstate).kvs, ref, template=templatep, error=error)
					if (error eq 0) and (size(pos, /tname) eq 'STRUCT') then begin
						(*pm).geopixe1.position = pos
					endif else begin
						log_message, (*pstate).kvs, type='WARNING', 'Coords timer get_kvs '+ref+' error.'
					endelse

					kname = (*pstate).kvs_prefix + 'GP.2.'
					ref = kname + 'position'
					pos = get_kvs( (*pstate).kvs, ref, template=templatep, error=error)
					if (error eq 0) and (size(pos, /tname) eq 'STRUCT') then begin
						(*pm).geopixe2.position = pos
					endif else begin
;						log_message, (*pstate).kvs, type='WARNING', 'Coords timer get_kvs '+ref+' error.'
					endelse

;					For 'newscan', later when we automatically append a 'newscan' to
;					the scan list, we should use "exchange_kvs()" to swap the read
;					newscan spec with this: {new:0}, which flags the newscan as used.
;					Then test on newscan.new=0 to skip the append.

					templates =  {	new: 0, $					; newscan
									origin: {	x: 0.0, $		; origin X
												y: 0.0, $		; Y
												z: 0.0}, $		; Z
									size: {		x: 0.0, $		; scan X size
												y: 0.0}, $		; Y
									zsurface: fltarr(4)}		; Z surface bilinear coefficients
					kname = (*pstate).kvs_prefix + 'CF.'
					ref = kname + 'newscan'
					newscan = get_kvs( (*pstate).kvs, ref, template=templates, error=error)
					if (error eq 0) and (size(newscan, /tname) eq 'STRUCT') then begin
						(*pm).finder.newscan = newscan
					endif else begin
						log_message, (*pstate).kvs, type='WARNING', 'Coords timer get_kvs '+ref+' error.'
					endelse
	
					kname = (*pstate).kvs_prefix + 'GP.1.'
					ref = kname + 'newscan'
					newscan = get_kvs( (*pstate).kvs, ref, template=templates, error=error)
					if (error eq 0) and (size(newscan, /tname) eq 'STRUCT') then begin
						(*pm).geopixe1.newscan = newscan
					endif else begin
						log_message, (*pstate).kvs, type='WARNING', 'Coords timer get_kvs '+ref+' error.'
					endelse

					kname = (*pstate).kvs_prefix + 'GP.2.'
					ref = kname + 'newscan'
					newscan = get_kvs( (*pstate).kvs, ref, template=templates, error=error)
					if (error eq 0) and (size(newscan, /tname) eq 'STRUCT') then begin
						(*pm).geopixe2.newscan = newscan
					endif else begin
						log_message, (*pstate).kvs, type='WARNING', 'Coords timer get_kvs '+ref+' error.'
					endelse

cont_coords:
					case (*pstate).coords_src of
						0: begin												; finder
							(*pm).current = (*pm).finder
							end
						1: begin												; mapper 1
							(*pm).current.position = (*pm).mapper1.position
							end
						2: begin												; mapper 2
							(*pm).current.position = (*pm).mapper2.position
							end
						3: begin												; GeoPIXE 1
							(*pm).current = (*pm).geopixe1
							end
						4: begin												; GeoPIXE 2
							(*pm).current = (*pm).geopixe2
							end
						else:
					endcase

;					Here we use 'coords_tgt' to select between live Stage coords for the endstation.

					case (*pstate).coords_tgt of
						0: begin												; mapper 1
							(*pm).target.position = (*pm).mapper1.position
							end
						1: begin												; mapper 2
							(*pm).target.position = (*pm).mapper2.position
							end
						else:
					endcase
				endif
				s = ['X = '+str_tidy((*pm).target.position.x, places=3),'Y = '+str_tidy((*pm).target.position.y, places=3), $
						'Z = '+str_tidy((*pm).target.position.z, places=3),'% = '+str_tidy((*pstate).scan_sequence.progress, places=-1)]
				widget_control, (*pstate).update, set_value=s
				scan_list_put_state, pstate
coords_again:
				widget_control, (*pstate).sample_coords_src_mode, timer = (*pstate).timer_coords
				end
			'query-button': begin							; timer to read positions, newscan specs
				scan_list_check_kvs, pstate, error=err
				if err eq 0 then begin

					template =  {		x:	0.0, $				; X stage/ captured cursor position
										y:	0.0, $				; Y
										z:	0.0}				; Z
					ref = 'position'
					pos = get_vsub( (*pstate).vsub_stage, ref, template=template, error=error)
					if (error eq 0) and (size(pos, /tname) eq 'STRUCT') then begin
						(*pm).stage.move.x = pos.x
						(*pm).stage.move.y = pos.y
						(*pm).stage.move.z = pos.z

						if (*pm).endstation eq 1 then begin
							(*pm).mapper1.position = pos
						endif else begin
							(*pm).mapper2.position = pos
						endelse
					endif else begin
						log_message, (*pstate).vsub_stage, type='WARNING', 'Query timer, get_vsub error: '+((size(pos, /tname) eq 'STRUCT') ? pos.error : "?")
					endelse

					case (*pstate).coords_src of
						0: begin												; finder
							(*pm).current = (*pm).finder
							end
						1: begin												; mapper 1
							(*pm).current.position = (*pm).mapper1.position
							end
						2: begin												; mapper 2
							(*pm).current.position = (*pm).mapper2.position
							end
						3: begin												; GeoPIXE 1
							(*pm).current = (*pm).geopixe1
							end
						4: begin												; GeoPIXE 2
							(*pm).current = (*pm).geopixe2
							end
						else:
					endcase

;					Here we use 'coords_tgt' to select between live Stage coords for the endstation.

					case (*pstate).coords_tgt of
						0: begin												; mapper 1
							(*pm).target.position = (*pm).mapper1.position
							end
						1: begin												; mapper 2
							(*pm).target.position = (*pm).mapper2.position
							end
						else:
					endcase
					scan_list_put_state, pstate
				endif
cont_pos:
				s = ['X = '+str_tidy((*pm).target.position.x, places=3),'Y = '+str_tidy((*pm).target.position.y, places=3), $
						'Z = '+str_tidy((*pm).target.position.z, places=3),'% = '+str_tidy((*pstate).scan_sequence.progress, places=-1)]
				widget_control, (*pstate).update, set_value=s
				widget_control, (*pstate).q_button, timer = (*pstate).timer_position
				end
			'shutter-open-button': begin							; timer to check shutter status
				scan_list_check_kvs, pstate, error=err
				if err eq 0 then begin
					template =  	{		left:		'', $			; 'OPEN', 'CLOSED', 'INIT'
											right:		''}				; 
					ref = 'Shutter'
					stat = get_vsub( (*pstate).vsub_safety, ref, template=template, error=error)
					if (size(stat, /tname) eq 'STRUCT') then begin
						if (error eq 0) then begin
							if 'Left' eq (*pstate).shutter_name[(*pm).endstation] then s = stat.left
							if "Right" eq (*pstate).shutter_name[(*pm).endstation] then s = stat.right
							q = where( s eq ['INIT','OPEN','CLOSED']) > 0
							(*pstate).led2[5] = q[0]
						endif else begin
							log_message, (*pstate).vsub_safety, type='WARNING', 'Shutter timer, get_vsub error: '+((size(stat, /tname) eq 'STRUCT') ? stat.error : "?")
;							print,'Safety PUB error: '+stat.error
						endelse
					endif
				endif
				scan_list_put_state, pstate
				widget_control, (*pstate).shutter_open_button, timer = (*pstate).timer_shutter
				end
			else:
		endcase
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request scan_list ...'
		goto, kill
		end
	else:
endcase

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

uname = widget_info( event.id, /uname)
case uname of

	'scan-table': begin
		case tag_names( event, /structure_name) of

			'WIDGET_TABLE_CELL_SEL': begin
				if (event.sel_left eq -1) and (event.sel_right eq -1) then goto, finish
				osel = (*pstate).sel
				(*pstate).sel.left = event.sel_left
				(*pstate).sel.top = event.sel_top
				(*pstate).sel.right = event.sel_right
				(*pstate).sel.bottom = event.sel_bottom
;				print, (*pstate).sel

				if no_list then goto, finish
				if (event.sel_left eq 0) and (event.sel_right eq 0) then begin
					if ptr_valid(p) eq 0 then goto, finish
					if size(*p,/tname) ne 'POINTER' then goto, finish
					n = n_elements(*p)
					for i=event.sel_top,event.sel_bottom do begin
						j = (*pindex)[i]
						pi = (*(*p)[j]).pval
						if ptr_valid( pi ) eq 0 then goto, finish
						if size(*pi,/tname) ne 'STRUCT' then goto, finish
						if (*pi).active le 3 then begin
							(*pi).active = ((*pi).active + 1) mod 4
							if ((*pi).active eq 3) and (i ne event.sel_bottom) then (*pi).active=0
						endif
					endfor
					update_scan_list_table, pstate
				endif
				
				if (event.sel_left eq 0) and (event.sel_right eq ((*pstate).columns-1)) then begin
					if (*pstate).sel.top ge 0 then begin
						if (osel.left eq event.sel_left) and (osel.right eq event.sel_right) and (osel.top eq event.sel_top) and (osel.bottom eq event.sel_bottom) then begin
							widget_control, (*pstate).table, set_table_select=[-1,-1,-1,-1]
							(*pstate).sel.top = -1
							(*pstate).sel.bottom = -1
						endif else begin
							*(*pstate).pselect = (*pstate).sel
							notify, 'scan-edit-select', (*pstate).pselect, from=event.top
						endelse
					endif
				endif
				end

			'WIDGET_TABLE_CH': begin
				end
			else:
		endcase
		end

	'scan_list_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				x = (event.x - (*pstate).xoffset) > 740
				n = ((event.y - (*pstate).yoffset + 9)/ (*pstate).row_height) - 2
				y = ((n + 2) * (*pstate).row_height) > 105
				widget_control, (*pstate).table, scr_xsize=x, scr_ysize=y
				widget_control, (*pstate).help, scr_xsize=x-led_xsize-led_xsize2-led_xsize3-update_xsize-8
				widget_control, (*pstate).edit_base, scr_xsize=x-7
				widget_control, (*pstate).control_base, scr_xsize=x-7
				widget_control, (*pstate).stage_base, scr_xsize=x-7
				end
			else:
		endcase
		end

	'import-button': begin
		path = *(*pstate).path
;		file = strip_path(strip_file_ext((*(*p)[0]).spectrum.file)) + '.scan.csv'
		file = 'test.scan.csv'
		F = file_requester( /read, filter = '*.scan.csv', $
					path=path, file=file, group=event.top, $
					title='Read the scan list CSV file', /fix_filter)
		if F ne '' then begin
;			F = strip_file_ext(F,/double) + '.scan.csv'
			*(*pstate).path = extract_path(F)
			print,'Import Scan List from ',F
			load_scan_list, pstate, F
			update_scan_list_table, pstate

			(*pstate).sel.top = 0
			(*pstate).sel.bottom = 0
			*(*pstate).pselect = (*pstate).sel
			widget_control, (*pstate).table, set_table_select=[-1,(*pstate).sel.top,-1,(*pstate).sel.top]
			notify, 'scan-edit-select', (*pstate).pselect, from=event.top
		endif
		end

	'export-button': begin
		if no_list then goto, finish
		path = *(*pstate).path
;		file = strip_path(strip_file_ext((*(*p)[0]).spectrum.file)) + '.scan.csv'
		file = 'test.scan.csv'
		F = file_requester( /write, filter = '*.scan.csv', $
			path=path, file=file, group=event.top, $
			title='Export the scan list to a CSV file', /fix_filter)
		if F ne '' then begin
;			F = strip_file_ext(F,/double) + '.scan.csv'
			*(*pstate).path = extract_path(F)
			print,'save Scan List to ',F
			save_scan_list, pstate, F, error=error
		endif
		end
		
	'frame-button': begin
		scan_list_get_state, pstate, error=err
		if err then begin
			warning, timeout=300, 'scan_list_event','Failed to read "scan-sequence" state.'
			goto, finish
		endif
		s = get_kvs( (*pstate).kvs, (*pstate).state_key, 'frame_id', /hash, error=err)
		if err then begin
			warning, timeout=300, 'scan_list_event','Failed to read active scan "frame number".'
			goto, finish
		endif

		widget_control, (*pstate).frame_id_text, set_value=s
		(*pstate).frame = long(s)

		scan_list_clear, pstate
		load_kvs_scan_list, pstate, error=err
		if err eq 0 then begin		
			update_scan_list_table, pstate

			(*pstate).sel.top = 0
			(*pstate).sel.bottom = 0
			*(*pstate).pselect = (*pstate).sel
			widget_control, (*pstate).table, set_table_select=[-1,(*pstate).sel.top,-1,(*pstate).sel.top]
			notify, 'scan-edit-select', (*pstate).pselect, from=event.top
		endif
		end

	'frame-text': begin
		scan_list_get_state, pstate, error=err
		if err then begin
			warning, timeout=300, 'scan_list_event','Failed to read "scan-sequence" state.'
			widget_control, event.id, set_value=str_tidy((*pstate).frame)
			goto, finish
		endif
		if (*pstate).scan_sequence.active and (*pstate).master then begin
			warning, timeout=300, 'scan_list_event',["Can't change frame number as scan is active,", $
						' and we are the "Master".']
			widget_control, event.id, set_value=str_tidy((*pstate).frame)
			goto, finish
		endif

		widget_control, event.id, get_value=s
		(*pstate).frame = long(s)

		scan_list_clear, pstate
		load_kvs_scan_list, pstate, error=err
		if err eq 0 then begin		
			update_scan_list_table, pstate

			(*pstate).sel.top = 0
			(*pstate).sel.bottom = 0
			*(*pstate).pselect = (*pstate).sel
			widget_control, (*pstate).table, set_table_select=[-1,(*pstate).sel.top,-1,(*pstate).sel.top]
			notify, 'scan-edit-select', (*pstate).pselect, from=event.top
		endif
		end

	'site-mode': begin
		(*pstate).site = event.index
		sites = ['Mel','Per']
		(*pstate).kvs_prefix = 'MM.' + sites[(*pstate).site] + '.'
		end

	'coords-src-mode': begin
		(*pstate).coords_src = event.index
		*(*pstate).pcoords = [(*pstate).coords_src, (*pstate).coords_tgt]
		notify, 'scan-edit-coords', (*pstate).pcoords, from=event.top
		end

	'coords-tgt-mode': begin
		(*pstate).coords_tgt = event.index
		*(*pstate).pcoords = [(*pstate).coords_src, (*pstate).coords_tgt]
		notify, 'scan-edit-coords', (*pstate).pcoords, from=event.top
		end

	'kvs-get-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish

		scan_list_clear, pstate
		load_kvs_scan_list, pstate, error=err
		if err eq 0 then begin		
			update_scan_list_table, pstate

			(*pstate).sel.top = 0
			(*pstate).sel.bottom = 0
			*(*pstate).pselect = (*pstate).sel
			widget_control, (*pstate).table, set_table_select=[-1,(*pstate).sel.top,-1,(*pstate).sel.top]
			notify, 'scan-edit-select', (*pstate).pselect, from=event.top
		endif
		end

	'commit': begin
		widget_control, (*pstate).frame_id_text, get_value=s
		(*pstate).frame = long(s)

		scan_list_get_state, pstate, error=err
		if err then goto, finish

		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'Scan-List event',['You are not the Scan "Master".','This will overwrite the current KVS scan-list.', '', $ 
						'Are you sure?','Hit "Cancel" to abort.'], cancel=cancel
			if cancel then goto, finish
		endif
		save_kvs_scan_list, pstate, error=err
		update_scan_list_table, pstate
		end

	'translate-button': begin
		scan_list_translate, pstate
		update_scan_list_table, pstate
		*(*pstate).pselect = (*pstate).sel
		notify, 'scan-edit-select', (*pstate).pselect, from=event.top
		end

	'edit-button': begin
		scan_edit, group=event.top, TLB=tlb, data=p, index=pindex, frame=(*pstate).pframe, $
					path=*(*pstate).path, sample=(*pstate).psample, coords=[(*pstate).coords_src, (*pstate).coords_tgt], $
					debug=(*pstate).debug, DAQ=(*pstate).pm, port=(*pstate).ps, row=(*pstate).sel.top, $
					kvs_port = (*pstate).kvs, enable_interlace=(*pstate).enable_interlace, kvs_prefix=(*pstate).kvs_prefix
					prefs=(*pstate).prefs
		register_notify, event.top, [ $			
					'scan-list-update', $		; update list display
					'scan-list-select' $		; select a scan list row
					], from=tlb
		(*pstate).lock = 1
		widget_control, (*pstate).lock_check, set_value=(*pstate).lock
		end
		
	'lock-check': begin
		case event.value of
			0: begin
				(*pstate).lock = event.select
				end
			else:
		endcase
		if (*pstate).lock then goto, finish

		scan_list_get_state, pstate, error=err
		if err then begin
			warning, timeout=300, 'scan_list_event','Failed to read "scan-sequence" state.'
			goto, finish
		endif
		if (*pstate).scan_sequence.active eq 0 then goto, finish

;		Note: active is cleared if master=0, so following only can happen if master=1

		s = get_kvs( (*pstate).kvs, (*pstate).state_key, 'frame_id', /hash, error=err)
		if err then begin
			warning, timeout=300, 'scan_list_event','Failed to read active scan "frame number".'
			goto, finish
		endif
		changed = ((*pstate).frame ne long(s))
		if changed eq 0 then goto, finish

		widget_control, (*pstate).frame_id_text, set_value=s
		(*pstate).frame = long(s)

		scan_list_clear, pstate
		load_kvs_scan_list, pstate, error=err
		if err eq 0 then begin		
			update_scan_list_table, pstate

			(*pstate).sel.top = 0
			(*pstate).sel.bottom = 0
			*(*pstate).pselect = (*pstate).sel
			widget_control, (*pstate).table, set_table_select=[-1,(*pstate).sel.top,-1,(*pstate).sel.top]
			notify, 'scan-edit-select', (*pstate).pselect, from=event.top
		endif
		end

	'power-check': begin
		case event.value of
			0: begin
				(*pstate).power_ignore = event.select
				end
			else:
		endcase
		end

	'master-check': begin
		if (*pstate).master eq event.select then goto, finish

		if (*pstate).master then begin
			scan_list_relinquish_master, pstate, error=err
		endif else begin
			scan_list_become_master, pstate, error=err
		endelse

		case event.value of
			0: begin
				(*pstate).master = event.select
				end
			else:
		endcase
		if err then begin
			warning, timeout=300, 'scan_list_event','Error in KVS changing "master" status.'
		endif
		if (*pstate).master eq 0 then goto, finish

		scan_list_get_state, pstate, error=err
		if err then begin
			warning, timeout=300, 'scan_list_event','Failed to read "scan-sequence" state.'
			goto, finish
		endif
		if (*pstate).scan_sequence.active eq 0 then goto, finish

		s = get_kvs( (*pstate).kvs, (*pstate).state_key, 'frame_id', /hash, error=err)
		if err then begin
			warning, timeout=300, 'scan_list_event','Failed to read active scan "frame number".'
			goto, finish
		endif
		changed = ((*pstate).frame ne long(s))
		if changed eq 0 then goto, finish

		widget_control, (*pstate).frame_id_text, set_value=s
		(*pstate).frame = long(s)

		scan_list_clear, pstate
		load_kvs_scan_list, pstate, error=err
		if err eq 0 then begin		
			update_scan_list_table, pstate

			(*pstate).sel.top = 0
			(*pstate).sel.bottom = 0
			*(*pstate).pselect = (*pstate).sel
			widget_control, (*pstate).table, set_table_select=[-1,(*pstate).sel.top,-1,(*pstate).sel.top]
			notify, 'scan-edit-select', (*pstate).pselect, from=event.top
		endif
		end

	'fill-button': begin
		if event.select eq 0 then goto, finish
		if no_list then goto, finish
		n = n_elements(*p)
		nc = (*pstate).columns
		if n lt 2 then goto, finish
		i1 = clip( (*pstate).sel.top, 0, n-1)
		i2 = clip( (*pstate).sel.bottom, 0, n-1)
		if i2 eq i1 then goto, finish
		j1 = clip( (*pstate).sel.left, 0, nc-1)
		j2 = clip( (*pstate).sel.right, 0, nc-1)
		for i=i1+1,i2 do begin
			for j=j1,j2 do begin
				k0 = (*pindex)[i1]
				k = (*pindex)[i]
				p0 = (*(*p)[k0]).pval
				pi = (*(*p)[k]).pval
				case j of
					0: (*pi).active = (*p0).active
					1: (*pi).sample = (*p0).sample
					2: (*pi).gain = (*p0).gain
					3: (*pi).origin.x = (*p0).origin.x
					4: (*pi).origin.y = (*p0).origin.y
					5: (*pi).origin.z = (*p0).origin.z

					6: (*pi).raster.size.x = (*p0).raster.size.x
					7: (*pi).raster.size.y = (*p0).raster.size.y
					8: (*pi).raster.pixel.x = (*p0).raster.pixel.x
					9: (*pi).raster.pixel.y = (*p0).raster.pixel.y

					10: (*pi).raster.dwell = (*p0).raster.dwell
					11: (*pi).raster.interlace = (*p0).raster.interlace

					13: (*pi).comment = (*p0).comment
					else:
				endcase
			endfor
		endfor
		update_scan_list_table, pstate
		nc = (*pstate).columns
		j = ((*pstate).sel.top - 5) > 0
		widget_control, (*pstate).table, set_table_select=[j1,i1,j2,i2]
		widget_control, (*pstate).table, set_table_view=[0,j]
		end
		
	'arrow-up': begin
		if event.select eq 0 then goto, finish
		if no_list then goto, finish
		n = n_elements(*p)
		if n lt 2 then goto, finish
		i1 = clip( (*pstate).sel.top, 0, n-1)
		i2 = clip( (*pstate).sel.bottom, 0, n-1)
		if i1 eq 0 then goto, finish

		for i=i1,i2 do begin
			t = (*pindex)[i-1]
			(*pindex)[i-1] = (*pindex)[i]
			(*pindex)[i] = t
		endfor

		n = n_elements(*p)
		(*pstate).sel.top = clip((*pstate).sel.top-1, 0, n-1)
		(*pstate).sel.bottom = clip((*pstate).sel.bottom-1, 0, n-1) > (*pstate).sel.top
		update_scan_list_table, pstate
		nc = (*pstate).columns
		j = ((*pstate).sel.top - 5) > 0
		widget_control, (*pstate).table, set_table_select=[0,(*pstate).sel.top,nc-1,(*pstate).sel.bottom]
		widget_control, (*pstate).table, set_table_view=[0,j]

		*(*pstate).pselect = (*pstate).sel
		notify, 'scan-edit-select', (*pstate).pselect, from=event.top
		end
		
	'arrow-down': begin
		if event.select eq 0 then goto, finish
		if no_list then goto, finish
		n = n_elements(*p)
		if n lt 2 then goto, finish
		i1 = clip( (*pstate).sel.top, 0, n-1)
		i2 = clip( (*pstate).sel.bottom, 0, n-1)
		if i2 ge n-1 then goto, finish

		for i=i1,i2 do begin
			t = (*pindex)[i+1]
			(*pindex)[i+1] = (*pindex)[i]
			(*pindex)[i] = t
		endfor

		n = n_elements(*p)
		(*pstate).sel.top = clip((*pstate).sel.top+1, 0, n-1)
		(*pstate).sel.bottom = clip((*pstate).sel.bottom+1, 0, n-1)
		update_scan_list_table, pstate
		nc = (*pstate).columns
		j = ((*pstate).sel.top - 5) > 0
		widget_control, (*pstate).table, set_table_select=[0,(*pstate).sel.top,nc-1,(*pstate).sel.bottom]
		widget_control, (*pstate).table, set_table_view=[0,j]

		*(*pstate).pselect = (*pstate).sel
		notify, 'scan-edit-select', (*pstate).pselect, from=event.top
		end
		
	'delete-button': begin
		if no_list then goto, finish
		scan_list_delete, pindex, (*pstate).sel.top, (*pstate).sel.bottom, nshow=ns

		n = (ns gt 0) ? 0 : n_elements(*pindex)
		(*pstate).sel.top = clip((*pstate).sel.top, 0, (n-1)>0)
		(*pstate).sel.bottom = clip((*pstate).sel.bottom, 0, (n-1)>0)
		update_scan_list_table, pstate
		
		*(*pstate).pselect = (*pstate).sel
		notify, 'scan-edit-select', (*pstate).pselect, from=event.top
		end
		
	'clear-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).scan_sequence.active and (*pstate).master then begin
			warning, timeout=300, 'scan_list_event',["Can't clear list as scan is active,", $
						' and we are the "Master".']
			goto, finish
		endif

		scan_list_clear, pstate
		update_scan_list_table, pstate
		end
		
	'start-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_start, ps, p, pindex, pm, pstate, no_list, tlb, phase=(*pstate).phase.pstart, /init
		scan_list_put_state, pstate, /frame, error=err
		end
		
	'skip-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_stop, ps, pstate, event.top, /no_complete
		wait, 15
		scan_list_next, ps, p, pindex, pm, pstate, event.top, phase=(*pstate).phase.pnext, /init
		scan_list_put_state, pstate, error=err
		end
		
	'stop-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_stop, ps, pstate, event.top
		scan_list_put_state, pstate, error=err
		end
		
	'append-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		widget_control, (*pstate).append_text, get_value=s
		(*pstate).changed = 1
		(*pstate).scan_sequence.stroke = fix2(s)
		scan_list_start, ps, p, pindex, pm, pstate, no_list, tlb, /append, phase=(*pstate).phase.pstart, /init
		scan_list_put_state, pstate, /frame, error=err
		end
		
	'append-text': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		widget_control, (*pstate).append_text, get_value=s
		(*pstate).changed = 1
		(*pstate).scan_sequence.stroke = fix2(s)
		widget_control, (*pstate).append_text, set_value=str_tidy((*pstate).scan_sequence.stroke)
		scan_list_put_state, pstate, /frame, error=err
		end
	
	'pause-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300,  'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_pause, ps, pm, no_list, pstate, pause=1
		scan_list_put_state, pstate, error=err
		end
	
	'resume-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_pause, ps, pm, no_list, pstate, pause=0
		scan_list_put_state, pstate, error=err
		end

	'query-button': begin
		widget_control, (*pstate).move_stage_x_text, set_value=str_tidy((*pm).current.position.x)
		widget_control, (*pstate).move_stage_y_text, set_value=str_tidy((*pm).current.position.y)
		widget_control, (*pstate).move_stage_z_text, set_value=str_tidy((*pm).current.position.z)
		end

	'go-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		widget_control, (*pstate).move_stage_x_text, get_value=s
		if s eq '' then begin
			s = str_tidy((*pm).current.position.x)
			widget_control, (*pstate).move_stage_x_text, set_value=s
		endif else if gnumeric(s) eq 0 then begin
			warning, timeout=300, 'scan_list_event','Illegal numeric value for X.'
			goto, finish
		endif
		x = float2(s)
		widget_control, (*pstate).move_stage_y_text, get_value=s
		if s eq '' then begin
			s = str_tidy((*pm).current.position.y)
			widget_control, (*pstate).move_stage_y_text, set_value=s
		endif else if gnumeric(s) eq 0 then begin
			warning, timeout=300, 'scan_list_event','Illegal numeric value for Y.'
			goto, finish
		endif
		y = float2(s)
		widget_control, (*pstate).move_stage_z_text, get_value=s
		if s eq '' then begin
			s = str_tidy((*pm).current.position.z)
			widget_control, (*pstate).move_stage_z_text, set_value=s
		endif else if gnumeric(s) eq 0 then begin
			warning, timeout=300, 'scan_list_event','Illegal numeric value for Z.'
			goto, finish
		endif
		z = float2(s)
		print,'Move stage to X=',x,', Y=',y,', Z=',z

		r = req_vsub( (*pstate).vsub_stage, 'Move', {X:x, Y:y, Z:z}, error=err, message=mess)
		if err then begin
			warning, timeout=300, 'scan_list_event',['Move stage error.','REQ to Stage VSUB returned an error: ',mess]
			goto, finish
		endif
		scan_list_put_state, pstate, error=err
		end

	'preset-mode': begin
		(*pstate).preset_mode = event.index
		t = (*(*pstate).presets)[event.index]
		print,'Set Preset Move to "' + t.name + '"'
		end

	'go-preset-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		r = req_vsub( (*pstate).vsub_stage, 'Move-Preset', (*(*pstate).presets)[(*pstate).preset_mode].name, error=err, message=mess)
		if err then begin
			warning, timeout=300, 'scan_list_event',['Preset Move stage error.','REQ to Stage VSUB returned an error: ',mess]
			goto, finish
		endif
		scan_list_put_state, pstate, error=err
		end

	'home-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		r = req_vsub( (*pstate).vsub_stage, 'Home', error=err, message=mess)
		if err then begin
			warning, timeout=300, 'scan_list_event',['Home stage error.','REQ to Stage VSUB returned an error: ',mess]
			goto, finish
		endif
		end

	'stage-fast-left': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_pan, pstate, event, 'X--'
		end
	'stage-left': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_pan, pstate, event, 'X-'
		end
	'stage-right': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_pan, pstate, event, 'X+'
		end
	'stage-fast-right': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_pan, pstate, event, 'X++'
		end
	'stage-fast-down': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_pan, pstate, event, 'Y--'
		end
	'stage-down': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_pan, pstate, event, 'Y-'
		end
	'stage-up': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_pan, pstate, event, 'Y+'
		end
	'stage-fast-up': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_pan, pstate, event, 'Y++'
		end
	'stage-in': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_pan, pstate, event, 'Z-'
		end
	'stage-out': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

;		for i=0,300 do scan_list_pan, pstate, event, 'Z+'
		scan_list_pan, pstate, event, 'Z+'
		end
	'stage-stop': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		scan_list_pan, pstate, event, 'STOP'
		end
		
	'shutter-open-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		print, 'Open shutter: ',(*pstate).shutter_name[(*pm).endstation]
		r = req_vsub( (*pstate).vsub_safety, 'Shutter-Open', (*pstate).shutter_name[(*pm).endstation], error=err, message=mess)
		if err then begin
			warning, timeout=300, 'scan_list_event',['Shutter open error.','REQ to Safety VSUB returned an error: ',mess]
			goto, finish
		endif else begin
			log_message, (*pstate).vsub_safety, type='INFO', 'scan_list_event, manual Shutter opened.'
		endelse
		end
	'shutter-close-button': begin
		scan_list_get_state, pstate, error=err
		if err then goto, finish
		if (*pstate).master eq 0 then begin
			warning, timeout=300, 'scan-list-event','Need to become "Master" first.'
			goto, finish
		endif

		print, 'Close shutter: ',(*pstate).shutter_name[(*pm).endstation]
		r = req_vsub( (*pstate).vsub_safety, 'Shutter-Close', (*pstate).shutter_name[(*pm).endstation], error=err, message=mess)
		if err then begin
			warning, timeout=300, 'scan_list_event',['Shutter close error.','REQ to Safety VSUB returned an error: ',mess]
			goto, finish
		endif else begin
			log_message, (*pstate).vsub_safety, type='INFO', 'scan_list_event, manual Shutter closed.'
		endelse
		end

	'close-button': begin
		print,'Close scan list ...'
		goto, kill
		end
	else:
endcase

finish:
	widget_control, hourglass=0
	close, 1
	return

bad_state:
	warning,'scan_list_event',['STATE variable has become ill-defined.','Abort Scan List.'],/error
	goto, die
bad_ptr:
	warning,'scan_list_event',['Parameter structure variable has become ill-defined.','Abort Scan List.'],/error
	goto, die

kill:
	print,'Scan List kill, cleanup ...'
	cancel_notify, event.top

	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if (*pstate).local_list then begin
		for i=0,n_elements(*(*pstate).plist)-1 do ptr_free, (*(*pstate).plist)[i]
		ptr_free, (*pstate).plist
	endif
	if ptr_valid( (*pstate).pindex) then ptr_free, (*pstate).pindex
	if ptr_valid( (*pstate).pframe) then ptr_free, (*pstate).pframe
	if ptr_valid( (*pstate).psample) then ptr_free, (*pstate).psample
	if (*pstate).local_pm then ptr_free, (*pstate).pm
	if ptr_valid( (*pstate).pselect) then ptr_free, (*pstate).pselect
	if ptr_valid( (*pstate).pcoords) then ptr_free, (*pstate).pcoords
	if ptr_valid( (*pstate).headings) then ptr_free, (*pstate).headings
	close_vsub, (*pstate).vsub_stage
	close_vsub, (*pstate).vsub_safety
	close_vsub, (*pstate).vsub_excillum
	close_kvs, (*pstate).kvs
	close_comms, (*pstate).comms
	if (*pstate).ztap eq 0 then begin
		close_comms, (*pstate).comms2
		close_comms, (*pstate).comms3
		close_comms, (*pstate).comms4
	endif
	if ptr_valid(pstate) then ptr_free, pstate

die:
	print,'Scan List die ...'
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;-----------------------------------------------------------------

pro scan_list_check_bounds, pm, p, pk, silent=silent, error=error

; Check the bounds (space and velocity) for selected scan *p.

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_check_bounds',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif
if n_elements(silent) eq 0 then silent=0

	error = 1
	if ptr_good(pm) eq 0 then begin
		warning, timeout=300, 'scan_list_check_bounds','pm struct pointer is invalid.'
		return
	endif
	if ptr_good(p) eq 0 then begin
		warning, timeout=300, 'scan_list_check_bounds','Selected scan spec pointer is invalid.'
		return
	endif
	bad = 0

	if ((*p).origin.x lt (*pm).stage.min.x) or ((*p).origin.x gt (*pm).stage.max.x) then begin
		if silent eq 0 then warning, timeout=300, 'scan_list_check_bounds','X origin out of bounds ('+str_tidy((*pm).stage.min.x)+','+str_tidy((*pm).stage.max.x)+')'
		bad = 1
	endif
	if ((*p).origin.y lt (*pm).stage.min.y) or ((*p).origin.y gt (*pm).stage.max.y) then begin
		if silent eq 0 then warning, timeout=300, 'scan_list_check_bounds','Y origin out of bounds ('+str_tidy((*pm).stage.min.y)+','+str_tidy((*pm).stage.max.y)+')'
		bad = 1
	endif
	if ((*p).origin.z lt (*pm).stage.min.z) or ((*p).origin.z gt (*pm).stage.max.z) then begin
		if silent eq 0 then warning, timeout=300, 'scan_list_check_bounds','Z origin out of bounds ('+str_tidy((*pm).stage.min.z)+','+str_tidy((*pm).stage.max.z)+')'
		bad = 1
	endif

	if ((*p).origin.x+(*p).raster.size.x gt (*pm).stage.max.x) then begin
		if silent eq 0 then warning, timeout=300, 'scan_list_check_bounds','X extent out of bounds ('+str_tidy((*pm).stage.max.x)+')'
		bad = 1
	endif
	if ((*p).origin.y+(*p).raster.size.y gt (*pm).stage.max.y) then begin
		if silent eq 0 then warning, timeout=300, 'scan_list_check_bounds','Y extent out of bounds ('+str_tidy((*pm).stage.max.y)+')'
		bad = 1
	endif

	if ((*p).raster.pixel.x/(0.001 * (*p).raster.dwell) gt (*pm).stage.speed.x) then begin
		if silent eq 0 then warning, timeout=300, 'scan_list_check_bounds','X velocity out of bounds ('+str_tidy((*pm).stage.speed.x)+')'
		bad = 1
	endif
	if bad then goto, bad

	error = 0
	return
bad:
	error = 1
	return
end

;-----------------------------------------------------------------

pro scan_list_clear, pstate

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_clear',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	p = (*pstate).plist						; each points to {on:0, key:'', pval:ptr({scan}) }
	pindex = (*pstate).pindex				; display index
	pframe = (*pstate).pframe				; frame details
	psample = (*pstate).psample				; sample details
	n = n_elements(*p)

	for i=0,n-1 do ptr_free, (*p)[i]
	plist = ptr_new()
	*p = plist
	*pindex = [-1]
;	*psample = ''
;	*pframe = define(/maia_frame_spec)
	return
end

;-----------------------------------------------------------------

pro scan_list_delete, pindex, top, bottom, nshow=ns

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_delete',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if (*pindex)[0] eq -1 then begin
		ns = 0
		return
	endif
	n = n_elements(*pindex)
	dead = intarr(n)
	for i=top,bottom do begin
		if (i ge 0) and (i lt n) then dead[i]=1
	endfor
	q = where( dead eq 0, ns)
	if ns gt 0 then begin
		*pindex = (*pindex)[q]
	endif else begin
		*pindex = -1
	endelse
	return
end

;-----------------------------------------------------------------

pro scan_list_check_kvs, pstate, error=err

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_check_kvs',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_retry_kvs, lock_retry_kvs
if n_elements(lock_retry_kvs) eq 0 then lock_retry_kvs=0

	err = 1
	if lock_retry_kvs then return			; scan_list_retry_kvs busy, no multiple entry

	check_kvs, (*pstate).kvs, error=err
	if err then begin
		print, 'scan-list: KVS err, typename='+typename( (*pstate).kvs)
		log_message, (*pstate).kvs, type='ERROR', 'Scan-List, KVS not responding. Retry KVS and VSUB connections ...'
		widget_control, (*pstate).help, set_value='KVS not responding. Retry KVS and VSUB connections ...'
		scan_list_retry_kvs, pstate, error=err
		if err then return	
	endif
	return
end

;-----------------------------------------------------------------

pro scan_list_test_master, pstate, error=err

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_test_master',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	err = 0
	if (*pstate).changed then return
	was_master = (*pstate).master

	scan_list_check_kvs, pstate, error=err
	if err then return	

	current_master = get_kvs( (*pstate).kvs, (*pstate).state_key, 'master_id', /hash, error=err)
	if err then return

	if current_master eq (*pstate).local_id then begin
		if was_master eq 0 then begin
			(*pstate).master = 1
			scan_list_get_state, pstate, /frame, /force
			update_scan_list_table, pstate
			log_message, (*pstate).comms, type='INFO', 'scan_list_test_master: Inherit role as "Master".'
		endif
	endif else begin
		if was_master then begin
			(*pstate).master = 0
			(*pstate).scan_sequence.active = 0
			update_scan_list_table, pstate
			log_message, (*pstate).comms, type='INFO', 'scan_list_test_master: Hand-over role as "Master".'
		endif
	endelse

	widget_control, (*pstate).master_check, set_value=(*pstate).master
	return
end

;-----------------------------------------------------------------

pro scan_list_get_state, pstate, frame=frame, force=force, error=err

; /frame	Also load scan list for frame, if number is different in KVS
; /force	Alawys load frame scan list.
 
COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_get_state',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(frame) eq 0 then frame=0
if n_elements(force) eq 0 then force=0
err = 1

	if frame and (*pstate).master and force then begin
		current_frame = get_kvs( (*pstate).kvs, (*pstate).state_key, 'frame_id', /hash, error=err)
		if err then return

		iframe = long2(current_frame)
		do_load = (iframe ne (*pstate).frame) or force
		if do_load and (iframe ge 0) then begin
			(*pstate).frame = iframe
			widget_control, (*pstate).frame_id_text, set_value=str_tidy(iframe)
			
			scan_list_clear, pstate
			load_kvs_scan_list, pstate, error=err
			if err eq 0 then begin		
				update_scan_list_table, pstate
	
				(*pstate).sel.top = 0
				(*pstate).sel.bottom = 0
				*(*pstate).pselect = (*pstate).sel
				widget_control, (*pstate).table, set_table_select=[-1,(*pstate).sel.top,-1,(*pstate).sel.top]
;				notify, 'scan-edit-select', (*pstate).pselect, from=event.top
			endif
		endif

		(*pstate).lock = 1
		widget_control, (*pstate).lock_check, set_value=(*pstate).lock
	endif

	if (*pstate).changed then return

;	Check UTC of last scan_sequence PUT to KVS. If it's stale, assume that old master has died
;	and assume role as Master here ... (also does a get state)

	stale = 600.
;	stale = 1.0e+9			; for debug, so a break point does not force a change of master

	utc = get_kvs( (*pstate).kvs, (*pstate).state_key, 'utc', /hash, error=err)
	if err eq 0 then begin
		if (systime(1) - utc) gt stale then begin
;			print,'scan_list_get_state: UTC in KVS stale, so assume role as "Master".'
			log_message, (*pstate).comms, type='INFO', 'scan_list_get_state: Assume role as "Master".'
			scan_list_become_master, pstate
			return
		endif
	endif
	
	scan_list_test_master, pstate, error=err
	if err then return

	t = get_kvs( (*pstate).kvs, (*pstate).state_key, 'scan_sequence', /hash, template=(*pstate).scan_sequence, error=err)
	if err then return

	if (*pstate).master eq 0 then t.active = 0
	(*pstate).scan_sequence = t
	return
end

;-----------------------------------------------------------------

pro scan_list_put_state, pstate, frame=frame, error=err

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_put_state',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(frame) eq 0 then frame=0

;	scan_list_test_master, pstate, error=err
;	if err then return

	if (*pstate).master eq 0 then begin
		(*pstate).changed = 0
		return
	endif

	t = systime(1)
	if (*pstate).changed or ((t - (*pstate).last_put) gt 10.) then begin
		set_kvs, (*pstate).kvs, (*pstate).state_key, 'utc', t, /hash, /lower, error=err
		if err eq 0 then (*pstate).last_put = t
	endif

	if (*pstate).changed then begin
		set_kvs, (*pstate).kvs, (*pstate).state_key, 'scan_sequence', (*pstate).scan_sequence, /hash, /lower, error=err
		if err then return
		(*pstate).changed = 0
	endif
	
	if frame then begin
		set_kvs, (*pstate).kvs, (*pstate).state_key, 'frame_id', str_tidy((*pstate).frame), /hash, error=err
		if err then return
	endif
	return
end

;-----------------------------------------------------------------

pro scan_list_become_master, pstate, error=err

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_become_master',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
	scan_list_test_master, pstate, error=err
	if err then return

	if (*pstate).master eq 1 then return

	old_master = get_kvs( (*pstate).kvs, (*pstate).state_key, 'master_id', /hash, error=err)
	if err then return

	if old_master eq (*pstate).local_id then old_master=''

	set_kvs, (*pstate).kvs, (*pstate).state_key, 'master_id', (*pstate).local_id, /hash, error=err
	if err then return

	set_kvs, (*pstate).kvs, (*pstate).state_key, 'pushed_master', old_master, /hash, error=err
	if err then return
	
	(*pstate).master = 1
	scan_list_get_state, pstate, /frame, /force
	update_scan_list_table, pstate
	log_message, (*pstate).comms, type='INFO', 'scan_list_become_master: Become "Master".'

	t = systime(1)
	set_kvs, (*pstate).kvs, (*pstate).state_key, 'utc', t, /hash, /lower, error=err
	if err eq 0 then (*pstate).last_put = t
	return
end

;-----------------------------------------------------------------

pro scan_list_relinquish_master, pstate, error=err

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_relinquish_master',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	(*pstate).changed = 0
	scan_list_test_master, pstate, error=err
	if err then return

	if (*pstate).master eq 0 then return

	temp = get_kvs( (*pstate).kvs, (*pstate).state_key, 'pushed_master', /hash, error=err)
	if err then return

	if temp eq (*pstate).local_id then temp = ''

	pushed_master = get_kvs( (*pstate).kvs, (*pstate).state_key, 'master_id', /hash, error=err)
	if err then return

	set_kvs, (*pstate).kvs, (*pstate).state_key, 'pushed_master', pushed_master, /hash, error=err
	if err then return
	
	set_kvs, (*pstate).kvs, (*pstate).state_key, 'master_id', temp, /hash, error=err
	if err then return

	(*pstate).master = 0
	(*pstate).scan_sequence.active = 0
	update_scan_list_table, pstate
	log_message, (*pstate).comms, type='INFO', 'scan_list_relinquish_master: Relinquish "Master" role.'
	return
end

;-----------------------------------------------------------------

pro scan_list_pan, pstate, event, key

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_pan',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if (event.select eq 0) or (key eq 'STOP') then begin
		r = req_vsub( (*pstate).vsub_stage, 'Stop', 'none', error=err, message=mess)
		if err then begin
			warning, timeout=300, 'scan_list_pan',['Stop stage error.','REQ to Stage VSUB returned an error:',mess]
			goto, finish
		endif
	endif else begin
		r = req_vsub( (*pstate).vsub_stage, 'Pan', key, error=err, message=mess)
		if err then begin
			warning, timeout=300, 'scan_list_pan',['Pan stage error.','REQ to Stage VSUB returned an error:',mess]
			goto, finish
		endif
	endelse

finish:
	return
end

;-----------------------------------------------------------------

pro scan_list_start, ps, p, pindex, pm, pstate, no_list, tlb, append=append, phase=pphase, init=init

; Start a scan after first moving to origin. Similar in 'scan_list_next'.
;
;	"position.dim[0].source" must be set to "enc2" to indicate that the X linear encoder is to be used.
;	Then it will assume index marks for Y axis too and check "encoder.axis[1-2].calib.ststus for "calibrated".
;
; This process divided into "phases" ('skip-button' timer loop will come back here while phase not zero)
;	phase 0	saves scan details to KVS  
;	phase 1	starts move near index marks (if position.dim[0].source = "enc2", i.e. linear encoder mode)
;	phase 2 checks if moving and returns (counts retries in 'loop')
;			also returns if status is 'fault' or 'locked'
;			if not moving, checks stage position against index dest (saved as (*pm).stage.dest).
;			if not on position, retries origin positioning again
;			starts move across index marks (if linear encoder mode)
;	phase 3 checks if moving and returns (counts retries in 'loop')
;			also returns if status is 'fault' or 'locked'
;			if not moving, checks stage position against origin (saved as (*pm).stage.dest).
;			if not on position, retries origin positioning again
;	phase 4	starts move to origin
;	phase 5 checks if moving and returns (counts retries in 'loop')
;			also returns if status is 'fault' or 'locked'
;			if not moving, checks stage position against origin (saved as (*pm).stage.dest).
;			if not on position, retries origin positioning again
;	phase 6	if on position then sets up Maia and starts scan execution and advances phase.
;	phase 7	checks if scan 'scanning', if not returns to try again.
;			if 'scanning' then set flags that sequence has started and zero phase (no more timer reentries).

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_start',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_scan_list_start_1, ii, j, pj, key, stroke, loop
common c_scan_list_start_2, linear_encoder
if n_elements(append) eq 0 then append=0
if n_elements(init) eq 0 then init=0
if n_elements(linear_encoder) eq 0 then linear_encoder=0
if n_elements(pphase) eq 0 then return
if append then begin
	if (*pstate).scan_sequence.active or ((*pstate).scan_sequence.raster_on eq 1) then begin
		warning, timeout=300, 'scan_list_start',['Raster appears to be running, or a sequence has started.', '',  $
				'To use "Append", you must stop existing raster/scan sequence using "Stop" first.']
		return
	endif
endif

if init then *pphase=0						; This will switch off the 'skip-button' timer entry.
phase = *pphase								; Use a local copy of 'phase' so that 'skip-button'
if phase eq 0 then begin					; timer does not enter here while debugging in here.
	loop = 0								; NOTE: this only works for /init pass.
endif										; Set break in 'skip-button' timer to debug following phases here.

(*pstate).changed = ((*pstate).scan_sequence.lock_paused ne 0)
(*pstate).scan_sequence.lock_paused = 0

loop++
if phase eq 0 then goto, phase0
if phase eq 1 then goto, phase1
if phase eq 2 then goto, phase2
if phase eq 3 then goto, phase3
if phase eq 4 then goto, phase4
if phase eq 5 then goto, phase5
if phase eq 6 then goto, phase6
if phase eq 7 then goto, phase7

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

phase0:
	print,'scan_list_start: Phase 0 ...'
	if no_list then goto, finish
	n = n_elements(*pindex)
	if n eq 0 then goto, finish
	if (*pm).endstation-1 ne (*pstate).coords_tgt then begin
		warning, timeout=300, 'scan_list_start',['Stage for selected "Target" coordinates is not available.', $
				'','Start mm_scan_list using a config appropriate to that stage.']
		goto, finish
	endif

	active = intarr(n)
	for i=0,n-1 do begin
		j = (*pindex)[i]
		pj = (*(*p)[j]).pval
		(*pj).raster.stroke = 0								; make sure all set stroke=0 initially
		scan_list_check_bounds, pm, pj, ps, /silent, error=error
		if error then (*pj).active = 1						; set out of bounds ones to OFF
		active[i] = (*pj).active
	endfor
	q = where(active eq 2, nq)								; look for a 'start'	
	if nq ge 1 then ii = q[0]
	if nq eq 0 then begin
		q = where(active eq 0, nq)							; look for first 'on'	
		if nq ge 1 then ii = q[0]
	endif
	if nq eq 0 then begin
		warning, timeout=300, 'scan_list_start',['No scans selected to run, or all have bounds issues.', $
			'','Enable one or more scans, and try again.','', $
			'Enable scans by clicking selected rows in the "Active" column.']
		goto, finish
	endif

	j = (*pindex)[ii]
	pj = (*(*p)[j]).pval
	key = (*(*p)[j]).key
	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, start sequence at item #'+str_tidy(ii)+', key='+key

;	If /append then write 'stroke' into scan struct, else clear stroke.

	if append then begin
		stroke = (*pstate).scan_sequence.stroke
	endif else begin
		stroke = 0
	endelse
	(*pj).raster.stroke = stroke

;	Force a commit of table to KVS --------------------------

	widget_control, (*pstate).frame_id_text, get_value=s
	(*pstate).frame = long(s)

	save_kvs_scan_list, pstate, error=err
	if err then goto, finish

	update_scan_list_table, pstate
	key = (*(*p)[j]).key					; may get created in save to KVS
	phase = 1

;	----------------------------------------------------------
	
phase1:
	if key eq '' then begin
		warning, timeout=300, 'scan_list_start',['Starting scan key is blank.', $
			'','Seems that these scans have not been "committed" to the KVS.']
		goto, finish
	endif

;	First check whether this system uses the linear encoder for X ...
;	"position.dim[0].source" must be set to "enc2" to indicate that the linear encoder is to be used.

	v = socket_command_get( ps, 'source', class='position.dim', /string, chip=-1, n_chips=3, error=err)
	if err then begin
		linear_encoder = 0
		warning, timeout=300, 'scan_list_start',['Get "position.dim[].source" error: '+str_tidy(v)]
	endif else begin
		if (v[0] ne 'enc2') then begin
			linear_encoder = 0
			log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Kandinski Axis 0 source does not use the linear encoder (enc2).'
		endif else begin
			linear_encoder = 1			; *** enable this line to enable use of linear encoder index marks for origin ***
		endelse
	endelse

;	If there is a linear encoder on X axis (i.e. "position.dim[0]" is set to "enc2" in Kandinski)
;	then arm calibration (X,Y axes) and move over the index mark in the correct direction.
;
;	If not, then use old method of "agreeing" on origin position at origin (this uses "Linear_encoder" passed in common).
;	In other words, move to origin, wait to settle and set "encoder.axis[].position" and 
;	"pixel.dim[].origin" to origin position.

	if linear_encoder then begin
;		Need to readback ‘encoder.axis[2].calib.status’ and if NOT “calibrated”, then do the Calibration ...
;		If "calibrated", then set 'phase=4' and goto 'move_to_origin' ...

		v = socket_command_get( ps, 'calib.status', class='encoder.axis', /string, chip=-1, n_chips=3, error=err)
		if err then begin
			linear_encoder = 0
			warning, timeout=300, 'scan_list_start',['Get "calib.status" error: '+str_tidy(v)]
			goto, move_to_origin
		endif

;		axis = 1 (Y), 2 (fine encoder X), 0 (old rotary encoder X).
;		Time resets in about 11 days, which will force a new index calib.

		if (v[2] eq 'calibrated') and (v[1] eq 'calibrated') and (systime(1)-(*pstate).last_index lt 1.0e+6) then begin
			log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Axes "calibrated" so go to origin ...'
			goto, move_to_origin
		endif
	
;		This now set in 'setup.var' ...

;		socket_command_set, ps, 'index.phase', ['phase00','phase11'], class='encoder.axis', chip=[1,2], error=err
;		if err then begin
;			warning, timeout=300, 'scan_list_start',['Set Index Phase error.']
;			goto, finish
;		endif

		ve = socket_command_get( ps, 'calib.position', class='encoder.axis', chip=-1, n_chips=3, error=err)
		log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Phase 1 - calib.position = '+strjoin(string(ve),',')
		if err then begin
			linear_encoder = 0
			warning, timeout=300, 'scan_list_start',['Get "Calib Position" error: '+str_tidy(ve),'','Revert to set origin method.']
			goto, move_to_origin
		endif
		if ((ve[2] lt 5) or (ve[2] gt 645)) then begin
			linear_encoder = 0
			warning, timeout=300, 'scan_list_start',['Read Calib Position error.','Strange axis 2 calib.position found: '+str_tidy(ve[2]),'','Revert to set origin method.']
			goto, move_to_origin
		endif
		if ((ve[1] lt 50.0) or (ve[1] gt 100.)) then begin
			linear_encoder = 0
			warning, timeout=300, 'scan_list_start',['Read Calib Position error.','Strange axis 1 calib.position found: '+str_tidy(ve[1]),'','Revert to set origin method.']
			goto, move_to_origin
		endif

;		Move to calib position, less 2.5 mm ...

		log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Phase 1 - Move to near index ...'
		(*pm).stage.dest = {x: ve[2]-2.5, y: ve[1]-2.5, z: 0.5}
		(*pm).stage.move.status = ''
	
		r = req_vsub( (*pstate).vsub_stage, 'Move', (*pm).stage.dest, seq=seq, error=err, message=mess, errno=errno)
		if err then begin
			warning, timeout=300, 'scan_list_start',['Move to near Index error.','REQ to Stage VSUB returned an error: ',mess,'REQ Errno = '+str_tidy(errno)]
			goto, finish
		endif
		log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Move near Index started, test for on position.'

		wait, 2.0
		(*pstate).changed = 1
		(*pstate).scan_sequence.move.seqno = seq
		phase = 2
		goto, preturn

phase2:
		if (*pm).stage.move.status eq 'moving' then goto, preturn
		onpos = 1
		if abs((*pm).stage.move.x - (*pm).stage.dest.x) gt 0.01 then onpos=0
		if abs((*pm).stage.move.y - (*pm).stage.dest.y) gt 0.01 then onpos=0
		if (onpos eq 0) then begin
			if (loop lt 100) then goto, preturn
			log_message, (*pstate).vsub_stage, type='WARNING', 'scan_list_start, Move to near Index error. Not on position after long timeout. Abort start.'
			help, (*pm).stage.move
			goto, finish
		endif
		err = 0
		if (*pm).stage.move.status ne 'stopped' then err=1
		if err then begin
			log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Move to near Index error? Stage Move status Fault or Locked? Status = '+(*pm).stage.move.status
			help, (*pm).stage.move
			goto, preturn
		endif
		wait, 1.0

		socket_command_set, ps, 'calib.arm', [1,1], class='encoder.axis', chip=[1,2], error=err
		if err then begin
			warning, timeout=300, 'scan_list_start',['Set Calib Arm error.']
			goto, finish
		endif
		wait, 1.0
	
		log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Phase 2 - Move over Index ...'
	
		(*pm).stage.dest.x += 5.
		(*pm).stage.dest.y += 5.
		(*pm).stage.move.status = ''
	
		r = req_vsub( (*pstate).vsub_stage, 'Move', (*pm).stage.dest, seq=seq, error=err, message=mess, errno=errno)
		if err then begin
			warning, timeout=300, 'scan_list_start',['Move over Index error.','REQ to Stage VSUB returned an error: ',mess,'REQ Errno = '+str_tidy(errno)]
			goto, finish
		endif

		wait, 2.0	
		phase = 3
		log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Move past Index started, test for on position ...'
	
		(*pstate).changed = 1
		(*pstate).scan_sequence.move.seqno = seq
		goto, preturn

phase3:
		if (*pm).stage.move.status eq 'moving' then goto, preturn
		onpos = 1
		if abs((*pm).stage.move.x - (*pm).stage.dest.x) gt 0.01 then onpos=0
		if abs((*pm).stage.move.y - (*pm).stage.dest.y) gt 0.01 then onpos=0
		if (onpos eq 0) then begin
			if (loop lt 100) then goto, preturn
			log_message, (*pstate).vsub_stage, type='WARNING', 'scan_list_start, Move over Index error. Not on position after long timeout. Abort start.'
			help, (*pm).stage.move
			goto, finish
		endif
		err = 0
		if (*pm).stage.move.status ne 'stopped' then err=1
		if err then begin
			log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Move over Index error? Stage Move status Fault or Locked? Status = '+(*pm).stage.move.status
			help, (*pm).stage.move
			goto, preturn
		endif

;		Readback ‘encoder.axis[2].calib.status’ to check for “calibrated” ...

		v = socket_command_get( ps, 'calib.status', class='encoder.axis', /string, chip=-1, n_chips=3, error=err)
		if err then begin
			warning, timeout=300, 'scan_list_start',['Get "Index Phase" error: '+str_tidy(v)]
			goto, finish
		endif
		if (v[2] ne 'calibrated') then begin
			log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Failed to trigger X axes calibration.'
			linear_encoder = 0
			goto, move_to_origin
		endif
		if (v[1] ne 'calibrated') then begin
			log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Failed to trigger Y axes calibration.'
			linear_encoder = 0
			goto, move_to_origin
		endif
		(*pstate).last_index = systime(1)
		phase = 4
	endif

move_to_origin:
	phase = 4

;----------------------------------------------------------------------------------------------------------------

phase4:
	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Phase 1 - Move to origin ...'
	(*pm).stage.dest = (*pj).origin
	(*pm).stage.move.status = ''

	r = req_vsub( (*pstate).vsub_stage, 'Move', (*pj).origin, seq=seq, error=err, message=mess, errno=errno)
	if err then begin
		warning, timeout=300, 'scan_list_start',['Move to origin error.','REQ to Stage VSUB returned an error: ',mess,'REQ Errno = '+str_tidy(errno)]

;		For most errors, we assume that the Move failed, and abort (goto finish), unlike in "Next" where we 
;		'preturn' and come back in later (for this 'phase') and try it again.

		goto, finish
	endif
	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Move to origin started, test for on position.'

	wait, 2.0
	(*pstate).changed = 1
	(*pstate).scan_sequence.move.seqno = seq
	phase = 5
	goto, preturn

phase5:
	if (*pm).stage.move.status eq 'moving' then goto, preturn
	onpos = 1
	if abs((*pm).stage.move.x - (*pm).stage.dest.x) gt 0.01 then onpos=0
	if abs((*pm).stage.move.y - (*pm).stage.dest.y) gt 0.01 then onpos=0
	if (onpos eq 0) then begin
		if (loop lt 100) then goto, preturn
		log_message, (*pstate).vsub_stage, type='WARNING', 'scan_list_start, Move to origin error. Not on position after long timeout. Abort start.'
		help, (*pm).stage.move
		goto, finish
	endif
	err = 0
	if (*pm).stage.move.status ne 'stopped' then err=1
	if err then begin
		log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Move to origin error? Stage Move status Fault or Locked? Status = '+(*pm).stage.move.status
		help, (*pm).stage.move
		goto, preturn
	endif
	wait, 1.0

phase6:
	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Phase 6 - Setup Maia and Execute raster ...'

	scan_list_maia_scan, j, ps, p, pindex, pm, pstate, tlb, error=err
	if err then begin
		log_message, (*pstate).vsub_stage, type='ERROR', 'scan_list_start, Setup Maia error.'
		goto, finish
	endif
	wait, 2.0

	print, 'scan_list_start: REQ "Execute-Scan" to Stage ...'
	r = req_vsub( (*pstate).vsub_stage, 'Execute-Scan', key, seq=seq, error=err, message=mess, errno=errno)
	if err then begin
		warning, timeout=300, 'scan_list_start',['Start sequence error.','REQ to Stage VSUB returned an error: ',mess,'REQ Errno = '+str_tidy(errno)]

;		For most errors, we assume that the Execute-Scan failed, and abort (goto finish), unlike in "Next" where we 
;		'preturn' and come back in later (for this 'phase') and try it again.

		goto, finish
	endif
	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Execute scan started ...'

	(*pstate).changed = 1
	(*pstate).scan_sequence.scan.seqno = seq
	phase = 7
	wait, 1.0
	goto, preturn

phase7:
	if (*pm).stage.scan.status eq 'stopped' then goto, preturn
	err = 0
	if (*pm).stage.scan.status ne 'scanning' then err=1
	if err then begin
		print,'scan_list_start: Start scan error? Stage Scan status Fault or Paused?'
		help, (*pm).stage.scan
		goto, preturn
	endif

	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_start, Phase 7 - raster underway, set sequence status parameters.'

	(*pstate).changed = 1
	(*pstate).scan_sequence.scan.index = ii						; from common
	(*pstate).scan_sequence.raster_on = 1
	(*pstate).scan_sequence.power_on = (*pm).status.power		; power on/off at start of sequence
	(*pstate).scan_sequence.stroke = stroke						; from common
	(*pstate).scan_sequence.pause = 0
	(*pstate).scan_sequence.error = 0
	(*pstate).scan_sequence.progress = 0.0
	(*pstate).scan_sequence.time_start = systime(1)				; only set active=1 after start time set.
	(*pstate).scan_sequence.active = 1
	widget_control, (*pstate).append_text, set_value=str_tidy(stroke)
	
	notify,'scan-sequence-started', from=tlb
	phase = 0
	update_scan_list_table, pstate
	goto, preturn

finish:
	phase = 0
	scan_list_maia_stop, ps, pstate, tlb
	update_scan_list_table, pstate

preturn:
	*pphase = phase
	return
end

;-----------------------------------------------------------------

pro scan_list_stop, ps, pstate, tlb, no_complete=no_complete

; /no_complete	if we are going onto next

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_stop',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(no_complete) eq 0 then no_complete=0
*(*pstate).phase.pstart = 0
*(*pstate).phase.pnext = 0
(*pstate).changed = 1
(*pstate).scan_sequence.lock_paused = 0

	r = req_vsub( (*pstate).vsub_stage, 'Stop-Scan', key, error=err, message=mess)
	if err then begin
		warning, timeout=300, 'scan_list_stop',['Stop scan error.','REQ to Stage VSUB returned an error:',mess]
		goto, finish
	endif
	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_stop, Manual stop of scan sequence.'

finish:
	if no_complete eq 0 then begin
		(*pstate).changed = 1
		(*pstate).scan_sequence.raster_on = 0
		(*pstate).scan_sequence.power_on = 0
		(*pstate).scan_sequence.active = 0
		(*pstate).scan_sequence.pause = 0
		(*pstate).scan_sequence.progress = 0.0
		notify,'scan-sequence-complete', from=tlb
	endif

	scan_list_maia_stop, ps, pstate, tlb
	update_scan_list_table, pstate
	return
end

;-----------------------------------------------------------------

pro scan_list_pause, ps, pm, no_list, pstate, pause=pause, soft=soft

; Set 'pause', 'soft' for only a temporary pause that can be cancelled by more beam
; 
; Note:
;	'lock_paused' is used to keep a pause state, if set manually, 
;	so that power=1 does not cancel it.

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_pause',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

;	if (*pstate).scan_sequence.active eq 0 then goto, finish
	if n_elements(pause) eq 0 then pause=1
	if n_elements(soft) eq 0 then soft=0
	if soft and (*pstate).scan_sequence.lock_paused and (pause eq 0) then return

	if pause eq 0 then begin
		scan_list_maia_pause, ps, pstate, pause=pause, error=err
		if err then begin
			warning, timeout=5., 'scan_list_pause',['Resume sequence error.','Maia event flow control returned an error.']
		endif
		scan_list_shutter_pause, pm, pstate, pause=pause, error=error
		if err then begin
			warning, timeout=5., 'scan_list_pause',['Resume sequence error.','Shutter control returned an error.']
		endif
	endif
	wait, 1.

	if pause then begin
		command = 'Pause-Scan'
	endif else begin
		command = 'Resume-Scan'
	endelse

	r = req_vsub( (*pstate).vsub_stage, command, 'none', error=err, message=mess, errno=errno)
	if err then begin
		log_message, (*pstate).vsub_stage, type='ERROR', 'scan_list_pause: '+command+' error. REQ to Stage VSUB returned an error:'+mess+' Errno = '+str_tidy(errno)

;		These errors are harmless, just 'preturn' and wait to try again ...
;		1	'Controller state not RASTER! (#RPAUSE)'
;		?	'Controller state not READY! (#RSTART)'  ???

		if where( errno eq [1,-2]) ne -1 then begin
			goto, cont				; ignore already in Pause state
		endif

;		These errors may be recoverable, popup warning to alert user for manual action.
;		?	'Timeout waiting for controller to init!'

		warning, timeout=10, cancel=cancel, 'scan_list_pause',[command+' error.','REQ to Stage VSUB returned an error:',mess,'REQ Errno = '+str_tidy(errno)]
		if cancel then begin
			scan_list_retry_kvs, pstate, error=err
			if err then begin
				warning, timeout=300, 'scan_list_pause',[command+' error.','REQ to Stage VSUB returned an error:',mess, $
					'Subsequent Retry KVS, VSUBs also failed.','Abort ...']
			endif
		endif
		goto, finish
	endif

cont:
	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_pause, '+command+' success.'
	(*pstate).changed = 1
	(*pstate).scan_sequence.pause = pause
	if (soft eq 0) then (*pstate).scan_sequence.lock_paused = pause
	(*pstate).scan_sequence.time_start = systime(1)

	wait, 3.
	if pause eq 1 then begin
		scan_list_maia_pause, ps, pstate, pause=pause, error=err
		if err then begin
			warning, timeout=5., 'scan_list_pause',['Pause sequence error.','Maia event flow control returned an error.']
		endif
		scan_list_shutter_pause, pm, pstate, pause=pause, error=error
		if err then begin
			warning, timeout=5., 'scan_list_pause',['Pause sequence error.','Shutter control returned an error.']
		endif
	endif

finish:
	return
end

;-----------------------------------------------------------------

pro scan_list_maia_pause, ps, pstate, pause=pause, error=error

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_maia_pause',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
	if n_elements(pause) eq 0 then pause=1

	error = 1
	if (*pstate).test and ((*ps).open eq 0) then begin
		error = 0
		return
	endif

	socket_command_set, ps, 'enable', (pause eq 0), class='event', error=error
	if error then begin
		log_message, (*pstate).comms, type='ERROR', 'scan_list_maia_pause, Error returned from Maia.'
	endif else begin
		log_message, (*pstate).comms, type='INFO', 'scan_list_maia_pause, "event.enable" = '+str_tidy(fix(pause eq 0))
	endelse
	return
end

;-----------------------------------------------------------------

pro scan_list_maia_scan, j, ps, p, pindex, pm, pstate, tlb, error=error

;	Uses "linear_encoder" passed in common from 'scan_list_start'.

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_maia_scan',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_scan_list_start_2, linear_encoder

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

	error = 1
	n = n_elements(*pindex)
	if n eq 0 then return
	pj = (*(*p)[j]).pval
	key = (*(*p)[j]).key

	if (*pj).coordinates ne 'mapper'+str_tidy((*pm).endstation) then begin
		warning, timeout=300, 'scan_list_maia_scan',['Illegal coordinate system to start scan.', $
			'Scan set-up may still need to be "Translated" to mapper coordinates.']
		return
	endif
	if (*pstate).test and ((*ps).open eq 0) then begin
		error = 0
		return
	endif
	if (*pm).version.kandinski lt 7021 then begin
		warning, timeout=300, 'scan_list_maia_scan','Kandinski version too old.'
		return
	endif

	error = 0
	err = 0

;	If "linear_encoder" = 0, then set encoder.axis[].position to match origin.

	org = [(*pj).origin.x, (*pj).origin.y, (*pj).origin.z] 
	if linear_encoder eq 0 then socket_command_set, ps, 'position', org, class='encoder.axis', chip=-1, n_chip=3, error=err & error = error or err
	socket_command_set, ps, 'origin', org, class='pixel.dim', chip=-1, n_chip=3, error=err & error = error or err

	pitch = [(*pj).raster.pixel.x, (*pj).raster.pixel.y] 
	hyst = ([(*pj).raster.pixel.x, (*pj).raster.pixel.y] * 0.1) > [0.001, 0.003]  
	socket_command_set, ps, 'pitch', pitch, class='pixel.dim', chip=-1, n_chip=2, error=err & error = error or err
	socket_command_set, ps, 'hysteresis', hyst, class='pixel.dim', chip=-1, n_chip=2, error=err & error = error or err

	nx = round( float((*pj).raster.size.x) / float((*pj).raster.pixel.x)) > 1
	ny = round( float((*pj).raster.size.y) / float((*pj).raster.pixel.y)) > 1
	socket_command_set, ps, 'coord.extent', [nx,ny], class='pixel.dim', chip=-1, n_chip=2, error=err & error = error or err

	socket_command_set, ps, 'dwell', 0.001 * (*pj).raster.dwell, class='scan', error=err & error = error or err
	if (*pj).project ne '' then begin
		socket_command_set, ps, 'project.next', '"'+(*pj).project+'"', class='blog', error=err & error = error or err
	endif
	tcomment = str_remove( ['"',"'"], (*pj).comment)
	tregion = str_remove( ['"',"'"], (*pj).region)
	socket_command_set, ps, 'sample.name', '"'+(*pj).sample+'"', class='metadata', error=err & error = error or err
	socket_command_set, ps, 'scan.region', '"'+tregion+'"', class='metadata', error=err & error = error or err
	socket_command_set, ps, 'scan.info', '"'+tcomment+'"', class='metadata', error=err & error = error or err
	socket_command_set, ps, 'scan.dwell', 0.001 * (*pj).raster.dwell, class='metadata', error=err & error = error or err
	socket_command_set, ps, 'scan.crossref', '"'+key+'"', class='metadata', error=err & error = error or err
	socket_command_set, ps, 'crossref', '"'+key+'"', class='scan', error=err & error = error or err

	serial = '?'
	sample_type = 'user'
	s = strsplit( (*pj).sample, '-_ ', /extract, count=ns)
	if strupcase(s[0]) eq 'UDIMET' then begin					; For now detector these standard names.
		if ns ge 2 then begin									; How to do this better?
			serial = s[1]
		endif else begin
			serial = '500'
		endelse
		sample_type = 'standard'
	endif
	if strupcase(s[0]) eq 'NIST' then begin
		if ns ge 2 then begin
			serial = s[1]
		endif else begin
			serial='1243'
		endelse
		sample_type = 'standard'
	endif
	
	socket_command_set, ps, 'sample.type', sample_type, class='metadata', error=err & error = error or err
	socket_command_set, ps, 'sample.serial', serial, class='metadata', error=err & error = error or err
	socket_command_set, ps, 'beam.energy', '24100.0', class='metadata', error=err & error = error or err

	if (*pm).version.kandinski gt 8600 then begin
		socket_command_set, ps, 'name', 'dwell.time', class='input.chan', chip=0, error=err & error = error or err
		socket_command_set, ps, 'source', 'in0', class='flux.chan', chip=0, error=err & error = error or err
	endif else begin
		socket_command_set, ps, 'name', 'Maia:dwell.time', class='flux.chan', chip=0, error=err & error = error or err
	endelse
	socket_command_set, ps, 'coeff', 1.0, class='flux.chan', chip=0, error=err & error = error or err
	socket_command_set, ps, 'unit', 'ms', class='flux.chan', chip=0, error=err & error = error or err

	socket_command_set, ps, 'key', 'prefix', class='metadata.datum', chip=0, error=err & error = error or err
	socket_command_set, ps, 'value', (*pstate).kvs_prefix, class='metadata.datum', chip=0, error=err & error = error or err
	socket_command_set, ps, 'key', 'endstation', class='metadata.datum', chip=1, error=err & error = error or err
	socket_command_set, ps, 'value', str_tidy((*pm).endstation), class='metadata.datum', chip=1, error=err & error = error or err

	socket_command_set, ps, 'enable', 1, class='event', error=err & error = error or err
	socket_command_set, ps, 'enable', 1, class='pixel', error=err & error = error or err
	socket_command_set, ps, 'enable', 1, class='photon', error=err & error = error or err
	if error then begin
		warning, timeout=300, 'scan_list_maia_scan',['Error setting up Maia scan parameters.','Abort scan.']
		return
	endif

;	socket_command_set, ps, 'new', 1, class='scan', error=error
	log_warning, 'scan_list_maia_scan','Send Kandinski "newrun" with /fail_on_retry set.'
	socket_command_set, ps, 'newrun', 1, class='blog', error=error, /fail_on_retry
	if error then begin
		warning, timeout=300, 'scan_list_maia_scan','Error during Kandinski "newrun". Abort scan.'
		return
	endif

	error = 0
	return
end

;-----------------------------------------------------------------

pro scan_list_maia_stop, ps, pstate, tlb, error=error

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_maia_stop',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if (*pstate).test and ((*ps).open eq 0) then begin
		error = 0
		return
	endif

	error = 0
	print,'scan_list_maia_stop: Maia "endrun" ...'
	socket_command_set, ps, 'endrun', 1, class='blog', error=error
	if error then begin
		warning, timeout=300, 'scan_list_maia_stop','Error during Kandinski "endrun".'
		return
	endif

;	socket_command_set, ps, 'enable', 0, class='pixel', error=err & error = error or err
;	if error then begin
;		warning, timeout=300, 'scan_list_maia_stop',['Error stopping scan.']
;		return
;	endif

	error = 0
	return
end

;-----------------------------------------------------------------

pro scan_list_shutter_pause, pm, pstate, pause=pause, error=error

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_shutter_pause',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
	if n_elements(pause) eq 0 then pause=1

	error = 1
	if pause then begin
		print, 'Close shutter: ',(*pstate).shutter_name[(*pm).endstation]
		r = req_vsub( (*pstate).vsub_safety, 'Shutter-Close', (*pstate).shutter_name[(*pm).endstation], error=err, message=mess)
		if err then begin
			warning, timeout=300, 'scan_list_shutter_pause',['Shutter pause close error.','REQ to Safety VSUB returned an error: ',mess]
			goto, finish
		endif else begin
			log_message, (*pstate).vsub_safety, type='INFO', 'scan_list_shutter_pause, Shutter closed.'
		endelse
	endif else begin
		print, 'Open shutter: ',(*pstate).shutter_name[(*pm).endstation]
		r = req_vsub( (*pstate).vsub_safety, 'Shutter-Open', (*pstate).shutter_name[(*pm).endstation], error=err, message=mess)
		if err then begin
			warning, timeout=300, 'scan_list_shutter_pause',['Shutter pause open error.','REQ to Safety VSUB returned an error: ',mess]
			goto, finish
		endif else begin
			log_message, (*pstate).vsub_safety, type='INFO', 'scan_list_shutter_pause, Shutter opened.'
		endelse
	endelse
	error = 0
	return

finish:
	error = 1
	return
end

;-----------------------------------------------------------------

pro scan_list_wait_on_move, pstate, condition, no=no, error=error, message=mess

; This and 'scan_list_wait_on_scan' don't seem to work. Need to return to
; widget manager and come back in, which is now done in 'scan_list_start' phases, etc.

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_wait_on_move',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(no) eq 0 then no=0
if n_elements(condition) eq 0 then condition='stopped'

	wait, 4.5
	error = 0
	mess = ''
	done = 0
	loop = 0
	template =  {		status:		'', $			; 'moving', 'stopped', 'paused', 'fault', 'locked'
						progress:	0.0, $			; progress % 
						error:		''}				; error message
	repeat begin
		wait, 0.5
		stat = get_vsub( (*pstate).vsub_stage, 'move', template=template, error=error)
		if (size(stat, /tname) eq 'STRUCT') then begin
			if error then begin
				mess = stat.error
				done = 1
			endif else begin
				if no then begin
					if (stat.status ne condition) and (stat.status ne 'unknown') then done = 1
				endif else begin
					if stat.status eq condition then done = 1
				endelse
				if stat.status eq 'fault' then begin
					error = 1
					mess = stat.error
					done = 1
				endif
				if stat.status eq 'locked' then begin
					error = 1
					mess = 'Locked? ['+stat.error+']'
					done = 1
				endif
			endelse
			sc = condition
			if no then sc='Not '+sc
			print,'Wait on stage Move ('+sc+'): status='+stat.status+', progress='+str_tidy(stat.progress)+', done='+str_tidy(done)+', error=('+str_tidy(error)+') '+mess
		endif else begin
			error = 1
			mess = 'Bad status from Move VSUB'
			done = 1
			log_message, (*pstate).vsub_stage, type='WARNING', 'scan_list_wait_on_move, get_vsub error: '+mess
;			print,'Wait on stage Move: Bad struct returned!'
		endelse
		loop++
	endrep until done or (loop gt 120)

	if error then wait, 5.0

	if loop ge 120 then begin
		sc = condition
		if no then sc='Not '+sc
		warning, timeout=300, 'scan_list_wait_on_move',['Waiting for '+sc+' timeout.','Correct status not found after 1 minute.',mess]
		error = 1
	endif	
	return
end

;-----------------------------------------------------------------

pro scan_list_wait_on_scan, pstate, condition, no=no, error=error, message=mess

; This and 'scan_list_wait_on_move' don't seem to work. Need to return to
; widget manager and come back in, which is now done in 'scan_list_start' phases, etc.

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_wait_on_scan',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(no) eq 0 then no=0
if n_elements(condition) eq 0 then condition='stopped'

	wait, 4.5
	error = 0
	mess = ''
	done = 0
	loop = 0
	template =  {		status:		'', $			; 'moving', 'stopped', 'paused', 'fault'
						key:		'', $			; scan KVS key
						progress:	0.0, $			; progress % 
						duration:	0L, $			; seconds duration
						remaining:	0L, $			; seconds remaining
						stroke:		0L, $			; last 'stroke' completed
						error:		''}				; error message
	repeat begin
		wait, 0.5
		stat = get_vsub( (*pstate).vsub_stage, 'scan', template=template, error=error)
help,stat
		if (size(stat, /tname) eq 'STRUCT') then begin
			if error then begin
				mess = stat.error
				done = 1
			endif else begin
				if no then begin
					if (stat.status ne condition) and (stat.status ne 'unknown') then begin
						done = 1
					endif
				endif else begin
					if stat.status eq condition then begin
						done = 1
					endif
				endelse
				if stat.status eq 'fault' then begin
					error = 1
					mess = stat.error
					done = 1
				endif
			endelse
			sc = condition
			if no then sc='Not '+sc
			print,'Wait on stage Raster ('+sc+'): status='+stat.status+', progress='+str_tidy(stat.progress)+', done='+str_tidy(done)+', error=('+str_tidy(error)+') '+mess
		endif else begin
			error = 1
			mess = 'Bad status from Scan VSUB'
			done = 1
			print,'Wait on stage Raster: Bad struct returned!'
		endelse
		loop++
	endrep until done or (loop gt 120)

	if loop ge 120 then begin
		sc = condition
		if no then sc='Not '+sc
		warning, timeout=300, 'scan_list_wait_on_scan',['Waiting for '+sc+' timeout.','Status not found after 1 minute.',mess]
	endif	
	return
end

;-----------------------------------------------------------------

pro scan_list_next, ps, p, pindex, pm, pstate, tlb, phase=pphase, init=init

;	See notes in 'scan_list_start'

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_next',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_scan_list_next_1, ii, j, pj, key, stroke, loop
common c_scan_list_start_2, linear_encoder

if n_elements(init) eq 0 then init=0
if n_elements(pphase) eq 0 then return
if init then *pphase=0									; This will switch off the 'skip-button' entry.
phase = *pphase											; Use a local copy of 'phase' so that 'skip-button'
if phase eq 0 then begin								; timer does not enter here while debugging in here.
	loop = 0											; NOTE: this only works for /init pass.
														; Set break on 'skip-button' timer for following phase entry here.
	print,'scan_list_next: Issue Maia Endrun 2.'
	scan_list_maia_stop, ps, pstate, tlb
endif
(*pstate).changed = ((*pstate).scan_sequence.lock_paused ne 0)
(*pstate).scan_sequence.lock_paused = 0

n = n_elements(*pindex)
if n eq 0 then goto, finish

loop++													; keep this in common for later Phases
if phase eq 0 then goto, phase0
if phase eq 1 then goto, phase1
if phase eq 2 then goto, phase2
if phase eq 3 then goto, phase3
if phase eq 4 then goto, phase4
if phase eq 5 then goto, phase5

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

phase0:
	print,'scan_list_next: Phase 0 ...'

	ii = (*pstate).scan_sequence.scan.index
	if ii ge n then goto, done
	j = (*pindex)[ii]
	pj = (*(*p)[j]).pval

	veto_more = 0
	if (*pj).active eq 3 then veto_more=1				; was last scan a "STOP" scan?
	(*pj).active = 1									; OFF once done
	if veto_more then goto, done

	set_kvs, (*pstate).kvs, (*(*p)[j]).key, 'ACTIVE', 1, /hash, error=err		; set Hash ACTIVE to OFF for finished scan row
	phase = 1

phase1:
	print,'scan_list_next: Phase 1 ...'

	ii = (*pstate).scan_sequence.scan.index + 1			; put this in common for later Phases
	if ii ge n then goto, done

	while (((*(*(*p)[(*pindex)[ii]]).pval).active eq 1) or ((*(*(*p)[(*pindex)[ii]]).pval).active ge 4)) and (ii lt (n-1)) do ii++		; skip 'off', 'ref1', ref2'
	if ii ge n then goto, done

	j = (*pindex)[ii]									; put these in common for later Phases
	key = (*(*p)[j]).key
	pj = (*(*p)[j]).pval
	(*pj).raster.stroke = 0
	if ((*pj).active ne 0) and ((*pj).active ne 2) and ((*pj).active ne 3) then goto, done
	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_next, continue sequence at item #'+str_tidy(ii)+', key='+key

	if key eq '' then begin
		warning, timeout=300, 'scan_list_next',['Next scan key is blank.', $
			'','Perhaps some scans have not been "committed" to the KVS.']
		goto, finish
	endif

	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_next, Phase 1 - Move to origin ...'
	(*pm).stage.dest = (*pj).origin
	(*pm).stage.move.status = ''

;	Move to origin ...

	r = req_vsub( (*pstate).vsub_stage, 'Move', (*pj).origin, seq=seq, error=err, message=mess, errno=errno)
	if err then begin
		log_message, (*pstate).vsub_stage, type='ERROR', 'scan_list_next: Move to origin error. REQ to Stage VSUB returned an error:'+mess+' Errno = '+str_tidy(errno)

;		These errors are harmless, just 'preturn' and wait to try again ...
;		1	'Begin move not allowed in state ...'
;		2	'Move to load not allowed in state ...'

		if where( errno eq [1,2]) ne -1 then begin
			goto, preturn				; retry move to origin
		endif

;		These errors are fatal, popup warning and finish...
;		3	'load pos (%f) outside soft lims (%f-%f)'

		if where( errno eq [3]) ne -1 then begin
			warning, timeout=10, 'scan_list_next',['Next sequence error - Move to origin error.','REQ to Stage VSUB returned a fatal error:',mess,'REQ Errno = '+str_tidy(errno)]
			goto, finish				; fatal, abort Next
		endif

;		Remaining errors may be recoverable, popup warning and 'preturn' and wait to try again ...
;		?	'Timeout waiting for controller to init!'

		warning, timeout=10, cancel=cancel, 'scan_list_next',['Next sequence error - Move to origin error.','REQ to Stage VSUB returned an error:',mess,'REQ Errno = '+str_tidy(errno)]
		if cancel then begin
			scan_list_retry_kvs, pstate, error=err
			if err then begin
				warning, timeout=300, 'scan_list_next',['Next sequence error - Move to origin error.','REQ to Stage VSUB returned an error:',mess, $
					'Subsequent Retry KVS, VSUBs also failed.','Continue ...']
;				goto, finish
			endif
		endif
		goto, preturn					; retry move to origin
	endif

	(*pstate).changed = 1
	(*pstate).scan_sequence.move.seqno = seq
	phase = 2
	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_next, Move to origin started, test for on position.'
	wait, 2.0
	goto, preturn

phase2:
	print,'scan_list_next: Phase 2 ...'

;	Wait to stop moving ...

	if (*pm).stage.move.status eq 'moving' then goto, preturn
	onpos = 1
	if abs((*pm).stage.move.x - (*pm).stage.dest.x) gt 0.01 then onpos=0
	if abs((*pm).stage.move.y - (*pm).stage.dest.y) gt 0.01 then onpos=0
	if (onpos eq 0) then begin
		if (loop lt 100) then goto, preturn
		log_message, (*pstate).vsub_stage, type='WARNING', 'scan_list_next, Move to origin error. Not on position after long timeout. Retry REQ to origin.'
		help, (*pm).stage.move
		loop = 1
		phase = 1						; retry move to origin
		goto, preturn
	endif
	err = 0
	if (*pm).stage.move.status ne 'stopped' then err=1
	if err then begin
		log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_next, Move to origin error? Stage Move status Fault or Locked? Status = '+(*pm).stage.move.status
		help, (*pm).stage.move
		goto, preturn
	endif
	phase = 3

;	Setup Maia for next scan ...

start_raster:
	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_next, Phase 3 - Setup Maia and Execute raster ...'

phase3:
	scan_list_maia_scan, j, ps, p, pindex, pm, pstate, tlb, error=err
	if err then begin
		log_message, (*pstate).vsub_stage, type='ERROR', 'scan_list_next, Setup Maia error. Abort ...'
		goto, finish
	endif
	phase = 4
	wait, 2.0

;	Execute scan ...

phase4:
	print, 'scan_list_next: REQ "Execute-Scan" to Stage ...'
	r = req_vsub( (*pstate).vsub_stage, 'Execute-Scan', key, seq=seq, error=err, message=mess, errno=errno)
	if err then begin
		log_message, (*pstate).vsub_stage, type='ERROR', 'scan_list_next, Execute-Scan REQ to Stage VSUB returned an error: '+mess+' Errno = '+str_tidy(errno)

;		These errors are harmless, just 'preturn' and wait to try again ...
;		1	'raster init not allowed in state %s'
;		2	'raster start not allowed in state %s'

		if where( errno eq [1,2]) ne -1 then begin
			goto, preturn				; retry execute-raster
		endif

;		These errors are fatal, popup warning and finish...
;		3	'load pos (%f) outside soft lims (%f-%f)'
;		4	'raster parameters rejected by controller'
;		5	'raster start not possible - already complete'

		if where( errno eq [3,4,5]) ne -1 then begin
			warning, timeout=10, 'scan_list_next',['Next sequence error - Execute Scan error.','REQ to Stage VSUB returned a fatal error:',mess,'REQ Errno = '+str_tidy(errno)]
			goto, finish				; fatal, abort Next
		endif

;		Other errors may be recoverable, popup warning and 'preturn' and wait to try again ...
;		7	'Timeout waiting for controller to init!'

		warning, timeout=10, cancel=cancel, 'scan_list_next',['Next sequence error - Execute Scan error.','REQ to Stage VSUB returned an error:',mess,'REQ Errno = '+str_tidy(errno)]
		if cancel then begin
			scan_list_retry_kvs, pstate, error=err
			if err then begin
				warning, timeout=300, 'scan_list_next',['Next sequence error - Execute Scan error.','REQ to Stage VSUB returned an error:',mess, $
					'Subsequent Retry KVS, VSUBs also failed.','Abort Next ...']
			endif
			goto, finish
		endif
		goto, preturn				; retry Execute Scan
	endif

	(*pstate).changed = 1
	(*pstate).scan_sequence.scan.seqno = seq
	phase = 5
	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_next, Raster started, test for "scanning".'
	wait, 1.0
	goto, preturn

phase5:
	print,'scan_list_next: Phase 5 ...'

;	Wait for raster underway ...

	if (*pm).stage.scan.status eq 'stopped' then goto, preturn
	err = 0
	if (*pm).stage.scan.status ne 'scanning' then err=1
	if err then begin
		log_message, (*pstate).vsub_stage, type='WARNING', 'scan_list_next, Start scan error? Stage Scan status Fault or Paused? Status = '+(*pm).stage.scan.status
		help, (*pm).stage.scan
		goto, preturn
	endif

	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_next, Phase 5 - raster underway, set sequence status parameters.'

	(*pstate).changed = 1
	(*pstate).scan_sequence.scan.index = ii				; 'ii' from common, from Phase 1
	(*pstate).scan_sequence.raster_on = 1
	(*pstate).scan_sequence.stroke = 0					; no "append" with next
	(*pstate).scan_sequence.pause = 0
	(*pstate).scan_sequence.error = 0
	(*pstate).scan_sequence.progress = 0.0
	(*pstate).scan_sequence.time_start = systime(1)		; only set active=1 after start time set.
	(*pstate).scan_sequence.active = 1
	widget_control, (*pstate).append_text, set_value=str_tidy(stroke)

	phase = 0
	update_scan_list_table, pstate
	goto, preturn

done:
	log_message, (*pstate).vsub_stage, type='INFO', 'scan_list_next, sequence finished.'
	(*pstate).changed = 1
	(*pstate).scan_sequence.active = 0
	(*pstate).scan_sequence.raster_on = 0
	(*pstate).scan_sequence.pause = 0
	(*pstate).scan_sequence.progress = 0.0

	notify,'scan-sequence-complete', from=tlb

finish:
	if phase ge 3 then scan_list_maia_stop, ps, pstate, tlb
	phase = 0
	update_scan_list_table, pstate

preturn:
	*pphase = phase
	return
end

;-----------------------------------------------------------------

;	Use the Frame Ref 1,2 Finder & Mapper to derive the coordinates transformation.
;	Use this to translate all 'finder' coords (scans, sample refs) to mapper coords.
;	/mirrorX relates to Finder coords.

pro scan_list_translate, pstate, left_handed=left_handed

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_translate',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(left_handed) eq 0 then left_handed=0

	plist = (*pstate).plist					; each points to {on:0, key:'', pval:ptr({scan}) }
	pindex = (*pstate).pindex				; display index
	pframe = (*pstate).pframe				; frame details
	psample = (*pstate).psample				; sample details

	coords = (*pstate).tgt_coords_list

	no_list = 0
	if ptr_valid(plist) eq 0 then return
	if size(*plist,/tname) ne 'POINTER' then begin
		no_list = 1
	endif else begin
		if ptr_valid( (*plist)[0] ) eq 0 then no_list=1
		if no_list eq 0 then if size(*(*plist)[0],/tname) ne 'STRUCT' then no_list=1
		if n_elements(*plist) eq 0 then no_list=1
	endelse
	if no_list then return
	if ptr_good(pindex) eq 0 then return
	if (*pindex)[0] eq -1 then return

	scan_list_tidy, pstate

;	First scan to see what the coordinate system is for scans, not equal to the Target coords
	
	src_coordinates = ''
	n = n_elements(*pindex)
	for i=0,n-1 do begin
		j = (*pindex)[i]
		if j lt 0 then continue
		p = (*(*plist)[j]).pval
		if (*p).coordinates ne coords[(*pstate).coords_tgt] then begin
			src_coordinates = (*p).coordinates
			break
		endif
	endfor
	if src_coordinates eq '' then return

;	Calculate translation/rotation transformation from (R1,R2) to (ref1, ref2) ...

	mirror1 = ( (*pstate).clayton_wrong_left_right and ((*pstate).kvs_prefix eq 'MM.Mel.') )
	mirror2 = ( ((*pstate).clayton_wrong_left_right eq 0) and ((*pstate).kvs_prefix eq 'MM.Mel.') or $
				((*pstate).kvs_prefix eq 'MM.Per.'))

	if src_coordinates eq 'finder' then begin
		R1 = (*pframe).finder.ref1
		R2 = (*pframe).finder.ref2
		mirrorX_in = 0
	endif else if src_coordinates eq 'mapper1' then begin
		R1 = (*pframe).mapper1.ref1
		R2 = (*pframe).mapper1.ref2
		mirrorX_in = mirror1
	endif else if src_coordinates eq 'mapper2' then begin
		R1 = (*pframe).mapper2.ref1
		R2 = (*pframe).mapper2.ref2
		mirrorX_in = mirror2
	endif else return

	if (*pstate).coords_tgt eq 0 then begin			; mapper1
		ref1 = (*pframe).mapper1.ref1 
		ref2 = (*pframe).mapper1.ref2
		mirrorX_out = mirror1
	endif else begin								; mapper2
		ref1 = (*pframe).mapper2.ref1 
		ref2 = (*pframe).mapper2.ref2
		mirrorX_out = mirror2
	endelse

	t =  build_translate_coords( R1,R2, ref1,ref2, scale, mirrorX_in=mirrorX_in, mirrorX_out=mirrorX_out, error=error)
	if error then goto, bad

;	Scans: Translate any 'finder' coords to 'mapper' ...

	n = n_elements(*pindex)
	for i=0,n-1 do begin
		j = (*pindex)[i]
		if j lt 0 then continue
		key = (*(*plist)[j]).key
		p = (*(*plist)[j]).pval
		if (*p).coordinates eq src_coordinates then begin
			r = translate_coords( (*p).origin.x, (*p).origin.y, (*p).origin.z, t, zsurfin=(*p).zsurface)
			r2 = translate_coords( (*p).origin.x+(*p).raster.size.x, (*p).origin.y+(*p).raster.size.y, 0.0, t)

			(*p).origin.x = min([r.origin.x,r2.origin.x])
			(*p).origin.y = min([r.origin.y,r2.origin.y])
			(*p).raster.size.x = max([r.origin.x,r2.origin.x]) - min([r.origin.x,r2.origin.x])
			(*p).raster.size.y = max([r.origin.y,r2.origin.y]) - min([r.origin.y,r2.origin.y])
			(*p).zsurface = r.zsurface
			(*p).coordinates = coords[(*pstate).coords_tgt]
		endif
	endfor

;	Samples: Translate any 'finder' coords to 'mapper' ...

	for i=0,n_elements(*psample)-1 do begin
		if (*psample)[i].coordinates eq src_coordinates then begin
			r = translate_coords( (*psample)[i].reference.ref1.x, (*psample)[i].reference.ref1.y, (*psample)[i].reference.ref1.z, t)
			(*psample)[i].reference.ref1 = r.origin

			r = translate_coords( (*psample)[i].reference.ref2.x, (*psample)[i].reference.ref2.y, (*psample)[i].reference.ref2.z, t)
			(*psample)[i].reference.ref2 = r.origin
			(*psample)[i].coordinates = coords[(*pstate).coords_tgt]
		endif
	endfor
	return

bad:
	warning, timeout=300, 'scan_list_translate',['Bad transformation.','Check Frame Reference coordinates.']
	return
end

;-----------------------------------------------------------------

;	Build translation matrices to transform from (R1,R2) to (ref1,ref2)
;	From C code for Amiga stage routine "Cue.c".
;	Added 'scale' for a non-mm input coordinates scale.

function build_translate_coords, R1,R2, ref1,ref2, scale, left_handed=left_handed, mirrorX_in=mirrorX_in, mirrorX_out=mirrorX_out, error=error

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'build_translate_coords',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
if n_elements(left_handed) eq 0 then left_handed=0
if n_elements(mirrorX_in) eq 0 then mirrorX_in=0
if n_elements(mirrorX_out) eq 0 then mirrorX_out=0
error = 1

	small = 0.01
	if mirrorX_in then begin
		R1.x = -R1.x
		R2.x = -R2.x
	endif
	if mirrorX_out then begin
		ref1.x = -ref1.x
		ref2.x = -ref2.x
	endif

;	Scale of Finder versus Mapper system ...

	dxf = R2.x - R1.x 
	dyf = R2.y - R1.y
	rf = sqrt( dxf*dxf + dyf*dyf)
	dxm = ref2.x - ref1.x 
	dym = ref2.y - ref1.y
	rm = sqrt( dxm*dxm + dym*dym)

	scale = rm/rf
	R1.x *= scale										; new scaled finder refs
	R1.y *= scale
	R2.x *= scale
	R2.y *= scale

	t = {tx1:0.0, ty1:0.0, tx2:0.0, ty2:0.0, tz:0.0, theta:0.0, scale:scale, left_handed:left_handed, mirror:{in:mirrorX_in, out:mirrorX_out}}

	dyo = ref2.y - ref1.y
	dxo = ref2.x - ref1.x
	if (abs(dyo) lt small) and (abs(dxo) lt small) then goto, bad

	dyi = R2.y - R1.y
	if left_handed then dyi = -dyi
	dxi = R2.x - R1.x
	if (abs(dyi) lt small) and (abs(dxi) lt small) then goto, bad

	t.tx1 = -R1.x
	t.ty1 = -R1.y
	if left_handed then t.ty1 = -t.ty1
	t.tx2 = ref1.x
	t.ty2 = ref1.y
	t.theta = atan(dyo,dxo) - atan( dyi, dxi)

	t.tz = ref1.z + R1.z

	help, t
	error = 0
	return, t

bad:
	return, 0
end

;-----------------------------------------------------------------

;	Apply translation to x,y,z, based on 't'.
;	From C code for Amiga stage routine "Cue.c".
;	Added 'scale' for a non-mm input coordinates scale.
;	Added Zsurface bilinear surface coefficients.
;	Added X mirror for IN or OUT coordinates.

function translate_coords, xin,yin,zin, t, zsurfin=zsurfin

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'translate_coords',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	if t.mirror.in then begin
		xin = -xin
	endif

	r = {origin:{x:0.0, y:0.0, z:0.0}, zsurface:fltarr(4)}
	x = t.scale* xin + t.tx1
	y = t.scale* yin
	if t.left_handed then y=-y
	y = y + t.ty1

	theta = t.theta
	xt = x*cos(theta) - y*sin(theta)
	yt = x*sin(theta) + y*cos(theta)

	r.origin.x = xt + t.tx2
	r.origin.y = yt + t.ty2
	r.origin.z = t.tz - zin				; does this assume Z +ve is opposite for Laser and Z stage?

	if t.mirror.out then begin
		r.origin.x = -r.origin.x
	endif

;	While the transformation of X,Y includes scaling, rotation and translation, these will
;	not be used much in Maia Mapper (no scaling and minimal rotation). Ignoring scaling
;	and rotation, the transformation reduces to a simple translation x'=x+ox,y'=y+oy, where
;	ox=tx1+tx2, oy=ty1+ty2. This simplification will be assumed to transform the Z surface
;	parameters.

	if keyword_set(zsurfin) then begin
		if n_elements(zsurfin) eq 4 then begin
			r.zsurface[0] = r.origin.z
			if abs(t.scale-1.) gt 0.05 then goto, bad_scale
			if abs(t.theta) gt 0.05 then goto, bad_theta
			if t.left_handed then goto, bad_hand

			ox = t.tx1 + t.tx2
			oy = t.ty1 + t.ty2
			k = zsurfin
			k0 = k[0] - k[1]*oy - k[2]*ox + k[3]*ox*oy
			k1 = k[1] - k[3]*ox
			k2 = k[2] - k[3]*oy
			k3 = k[3]
			r.zsurface = [t.tz,0.,0.,0.] - [k0,k1,k2,k3]
			print, r.zsurface
		endif
	endif

done:
	return, r

bad_scale:
	warning, timeout=300, 'translate_coords','Z surface transform does not support scaling in translation.'
	goto, done
bad_theta:
	warning, timeout=300, 'translate_coords','Z surface transform does not support significant rotation.'
	goto, done
bad_hand:
	warning, timeout=300, 'translate_coords','Z surface transform does not support left handed coordinates.'
	goto, done
end

;-----------------------------------------------------------------

;	Retry Open the KVS store, plus VSUBs, etc.
;	If something fails, wait 3s and then try all again.
;	'lock_retry_kvs' prevents multiple calls at once, and
;	should be zeroed in main at startup.

pro scan_list_retry_kvs, pstate, error=err

COMPILE_OPT STRICTARR
common c_retry_kvs, lock_retry_kvs
common c_retry_kvs_count, retry_kvs_count
if n_elements(lock_retry_kvs) eq 0 then lock_retry_kvs=0
if n_elements(retry_kvs_count) eq 0 then retry_kvs_count=0

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		err = 1
		lock_retry_kvs = 0

		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning, timeout=300, 'scan_list_retry_kvs',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	err = 1
	if lock_retry_kvs then return			; no multiple entry

start:
	lock_retry_kvs = 1
	retry_kvs_count++
	(*pstate).retry_count++
;	print,'scan_list_retry_kvs: count = ', (*pstate).retry_count, '  ', systime()
	log_message, (*pstate).kvs, type='WARNING', 'scan_list_retry_kvs, Retry kvs, count = '+str_tidy((*pstate).retry_count)+' ('+str_tidy(retry_kvs_count)+')'
	
	close_vsub, (*pstate).vsub_stage
	close_vsub, (*pstate).vsub_safety
	close_vsub, (*pstate).vsub_excillum
	close_kvs, (*pstate).kvs
;	close_comms, (*pstate).comms
	if (*pstate).ztap eq 0 then begin
		close_comms, (*pstate).comms2
		close_comms, (*pstate).comms3
		close_comms, (*pstate).comms4
	endif
	wait, 1.0

;	Can't close and re-open this as it will lose the DEBUG and ztap set-up.

;	comms = open_comms( ztap=(*pstate).ztap, error=err)
;	if err then begin
;		print,'scan_list_retry_kvs',['Failed to open a new comms object.']
;		return
;	endif
;	(*pstate).comms = comms

	if (*pstate).ztap then begin
		comms2 = (*pstate).comms
		comms3 = (*pstate).comms
		comms4 = (*pstate).comms
	endif else begin
		comms2 = open_comms( error=err)
		if err then begin
			log_message, (*pstate).kvs, type='ERROR', 'scan_list_retry_kvs, Failed to open a comms 2 object.'
;			print,'scan_list_retry_kvs',['Failed to open a comms 2 object.']
			goto, loop
		endif
		comms3 = open_comms( error=err)
		if err then begin
			log_message, (*pstate).kvs, type='ERROR', 'scan_list_retry_kvs, Failed to open a comms 3 object.'
;			print,'scan_list_retry_kvs',['Failed to open a comms 3 object.']
			goto, loop
		endif
		comms4 = open_comms( error=err)
		if err then begin
			log_message, (*pstate).kvs, type='ERROR', 'scan_list_retry_kvs, Failed to open a comms 4 object.'
;			print,'scan_list_retry_kvs',['Failed to open a comms 4  object.']
			goto, loop
		endif
	endelse

	(*pstate).comms2 = comms2
	(*pstate).comms3 = comms3
	(*pstate).comms4 = comms4

	kvs = open_kvs( (*pstate).kvs_endpoint, ztap=(*pstate).ztap, comms=comms, error=err)
	if err then begin
		log_message, (*pstate).kvs, type='ERROR', 'scan_list_retry_kvs, Failed to reopen the KVS endpoint: '+ (*pstate).kvs_endpoint
;		print,'scan_list_retry_kvs',['Failed to reopen the KVS endpoint: ', (*pstate).kvs_endpoint]
		goto, loop
	endif
	(*pstate).kvs = kvs

; Open the stage and Safety VSUBs 

	vsub = open_vsub(kvs, (*pstate).stage_endpoints, subscribe=['var.rt.stage','var.change.stage','alert'], mmcl=(*pstate).mmcl_stage, comms=comms3, error=err)
	if err then begin
		log_message, (*pstate).kvs, type='ERROR', 'scan_list_retry_kvs, Failed to reopen the Stage VSUB endpoints: '+strjoin((*pstate).stage_endpoints,' ')
;		print,'scan_list_retry_kvs',['Failed to reopen the Stage VSUB endpoints: ',(*pstate).stage_endpoints]
		goto, loop
	endif
	(*pstate).vsub_stage = vsub

	vsub = open_vsub(kvs, (*pstate).safety_endpoints, subscribe=['var.change.safety','var.rt.safety','alert'], mmcl=(*pstate).mmcl_safety, comms=comms2, error=err)
	if err then begin
		log_message, (*pstate).kvs, type='ERROR', 'scan_list_retry_kvs, Failed to reopen the Safety VSUB endpoints: '+strjoin((*pstate).safety_endpoints,' ')
;		print,'scan_list_retry_kvs',['Failed to reopen the Safety VSUB endpoints: ',(*pstate).safety_endpoints]
		goto, loop
	endif
	(*pstate).vsub_safety = vsub

	vsub = open_vsub(kvs, (*pstate).excillum_endpoints, subscribe=['var.rt.source','var.change.source','alert'], mmcl=(*pstate).mmcl_excillum, comms=comms4, error=err)
	if err then begin
		log_message, (*pstate).kvs, type='ERROR', 'scan_list_retry_kvs, Failed to reopen the Excillum VSUB endpoints: '+strjoin((*pstate).excillum_endpoints,' ')
;		print,'scan_list_retry_kvs',['Failed to reopen the Excillum VSUB endpoints: ',(*pstate).excillum_endpoints]
		goto, loop
	endif
	(*pstate).vsub_excillum = vsub

loop:
	if err then begin
		if (*pstate).retry_count le 10 then begin
			wait, 5
			goto, start
		endif else begin
			log_message, (*pstate).kvs, type='WARNING', 'scan_list_retry_kvs, Bad KVS or VSUB retries (10). Wait and try again ...'
			if retry_kvs_count gt 100 then begin
				timeout = 1200.
				warning, timeout=timeout,'scan_list_retry_kvs',['Bad KVS or VSUB retries (10).', 'Loop count = '+str_tidy(retry_kvs_count),'Loop count getting too large; dwindling resources. Wait longer ...','', $
						'Wait and try again ...'], cancel=cancel
			endif else begin
				timeout = 300.
				warning, timeout=timeout,'scan_list_retry_kvs',['Bad KVS or VSUB retries (10).', 'Loop count = '+str_tidy(retry_kvs_count),'', $
						'Wait and try again ...'], cancel=cancel
			endelse
			if cancel then goto, done
			(*pstate).retry_count = 0
			goto, start

;			lock_retry_kvs = 0
;			(*pstate).retry_count = 0
;			return
		endelse
	endif

done:
	print,'scan_list_retry_kvs: success.'
	lock_retry_kvs = 0
	(*pstate).retry_count = 0
	return
end

;-----------------------------------------------------------------

;	Tidy list by removing any scans not from current frame ...

pro scan_list_tidy, pstate

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'scan_list_tidy',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	widget_control, (*pstate).frame_id_text, get_value=s
	(*pstate).frame = long2(s)

	frame = (*pstate).frame
	kname = (*pstate).kvs_prefix + 'SL.' + 'frame' + strtrim(string(frame),2)
	nf = strlen(kname)

	no_list = 0
	p = (*pstate).plist						; each points to {on:0, key:'', pval:ptr({scan}) }
	pindex = (*pstate).pindex				; display index
	if ptr_valid(p) eq 0 then return
	if size(*p,/tname) ne 'POINTER' then begin
		no_list = 1
	endif else begin
		if ptr_valid( (*p)[0] ) eq 0 then no_list=1
		if no_list eq 0 then if size(*(*p)[0],/tname) ne 'STRUCT' then no_list=1
		if n_elements(*p) eq 0 then no_list=1
	endelse
	if no_list then return

	n = n_elements(*p)
	ni = n_elements(*pindex)
	bad = intarr(n)
	inew = *pindex

	for i=0,n-1 do begin
		key = (*(*p)[i]).key
		if (strmid(key,0,nf) ne kname) and (key ne '') then begin
			ptr_free, (*p)[i]
			bad[i] = 1
			q = where( *pindex eq i, nq)
			if nq gt 0 then begin
				inew[q[0]] = -1
				if q[0] lt ni-1 then begin
					for k=q[0]+1,ni-1 do begin
						inew[k] = inew[k] - 1
					endfor
				endif
			endif
		endif
	endfor
	*pindex = inew

	q = where( bad eq 0, nq)
	if nq gt 0 then begin
		*p = (*p)[q]
	endif else begin
		*p = ptr_new()
	endelse

	q = where( *pindex ge 0, nq)
	if nq gt 0 then begin
		*pindex = (*pindex)[q]
	endif else begin
		*pindex = [-1]
	endelse
	return
end

;-----------------------------------------------------------------

function scan_list_time, p

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
		warning, timeout=300, 'scan_list_time',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0.
	endif
endif

	nx = round( float((*p).raster.size.x) / float((*p).raster.pixel.x)) > 1
	ny = round( float((*p).raster.size.y) / float((*p).raster.pixel.y)) > 1
	
;	Simple options:
;		1. Just set time.min - then stop at this dwell.

	t = 0.001 * (*p).raster.dwell

	time_tot = float(nx)*float(ny)* t

	return, time_tot
end

;-----------------------------------------------------------------

pro load_scan_list, pstate, file

; Read the scan list from 'File'

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'load_scan_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		close, 1
		return
	endif
endif

	if lenchr(file) lt 1 then return
	plist = (*pstate).plist
	if ptr_valid(plist) eq 0 then goto, bad_ptr
	no_list = 0
	if size(*plist,/tname) ne 'POINTER' then begin
		no_list = 1
	endif else begin
		if ptr_valid( (*plist)[0] ) eq 0 then no_list=1
	endelse
	pindex = (*pstate).pindex				; display index
	pframe = (*pstate).pframe				; frame details
	psample = (*pstate).psample				; sample details

	valid_versions = [-1,-2]
	coords = (*pstate).src_coords_list

;	Frame details ...

	ref = define(/maia_frame_spec)
	*pframe = ref
	template3 = define(/maia_sample_spec)
	
	on_ioerror, bad_file
	openr,unit, file, /get_lun
	on_ioerror, bad_io

	s = ''
	readf, unit, s
	str = strsplit( s, ', ', /extract)
	ns = n_elements(str)
	if ns lt 2 then goto, bad_version
	version = fix(str[1])
	q = where( version eq valid_versions, nq)
	if nq lt 1 then goto, bad_version
	i = 0
	
	while( not( EOF(unit)) ) do begin
		readf, unit, s
		if strmid(s,0,1) eq '#' then continue
		str = strsplit( s, ', ', /extract)
		ns = n_elements(str)
		if ns ge 2 then begin
			srest = strjoin( str[1:*], ' ')
			
			case strlowcase(str[0]) of
				'frame-finder-ref1:' : begin
					if ns ge 4 then begin
						(*pframe).finder.ref1 = {x:float(str[1]), y:float(str[2]), z:float(str[3])}
					endif
					end
				'frame-finder-ref2:' : begin
					if ns ge 4 then begin
						(*pframe).finder.ref2 = {x:float(str[1]), y:float(str[2]), z:float(str[3])}
					endif
					end
				'frame-mapper1-ref1:' : begin
					if ns ge 4 then begin
						(*pframe).mapper1.ref1 = {x:float(str[1]), y:float(str[2]), z:float(str[3])}
					endif
					end
				'frame-mapper1-ref2:' : begin
					if ns ge 4 then begin
						(*pframe).mapper1.ref2 = {x:float(str[1]), y:float(str[2]), z:float(str[3])}
					endif
					end
				'frame-mapper2-ref1:' : begin
					if ns ge 4 then begin
						(*pframe).mapper2.ref1 = {x:float(str[1]), y:float(str[2]), z:float(str[3])}
					endif
					end
				'frame-mapper2-ref2:' : begin
					if ns ge 4 then begin
						(*pframe).mapper2.ref2 = {x:float(str[1]), y:float(str[2]), z:float(str[3])}
					endif
					end

				'scan-number:': begin
					p = ptr_new(define(/maia_scan_spec))
					(*p).coordinates = coords[(*pstate).coords_src]
					if n_elements(pt) ge 1 then begin
						pt = [pt,p]
					endif else begin
						pt = p
					endelse
					end
				'sample:': begin
					(*p).sample = srest

					if n_elements( *psample) eq 0 then begin
						*psample = template3
						k = 0
					endif else if ( size(*psample,/tname) ne 'STRUCT') then begin
						*psample = template3
						k = 0
					endif else begin
						q = where( srest eq (*psample).sample, nq)
						if nq eq 0 then begin
							k = n_elements(*psample)
							*psample = [*psample, template3]
						endif else k=q[0]
					endelse
					k_sample = k
			
					sam = template3
					sam.sample = srest
					(*psample)[k] = sam
					end

				'sample-ref1:' : begin
					if ns ge 4 then begin
						(*psample)[k_sample].reference.ref1 = {x:float(str[1]), y:float(str[2]), z:float(str[3])}
					endif
					end
				'sample-ref2:' : begin
					if ns ge 4 then begin
						(*psample)[k_sample].reference.ref2 = {x:float(str[1]), y:float(str[2]), z:float(str[3])}
					endif
					end

				'sample-coords:' : begin
					(*psample)[k_sample].coordinates = str[1]
					end

				'project:': begin
					(*p).project = srest
					end
				'grain:': begin
					(*p).region = srest
					end
				'comment:': begin
					(*p).comment = srest
					end
				'origin:': begin
					if ns ge 4 then begin
						(*p).origin.x = float2(str[1])
						(*p).origin.y = float2(str[2])
						(*p).origin.z = float2(str[3])
					endif
					end
				'interlace:': begin
					(*p).raster.interlace = (*pstate).enable_interlace ? (fix2(str[1]) > 1) : 1
					end
				'size:': begin
					if ns ge 3 then begin
						(*p).raster.size.x = float2(str[1])
						(*p).raster.size.y = float2(str[2])
					endif
					end
				'pixel:': begin
					if ns ge 3 then begin
						(*p).raster.pixel.x = float2(str[1])
						(*p).raster.pixel.y = float2(str[2])
					endif
					end
				'time-min/max:': begin
					if ns ge 2 then begin
						(*p).raster.dwell = float2(str[1])
					endif
					end
				'dwell:': begin
					if ns ge 2 then begin
						(*p).raster.dwell = 1000. * float2(str[1])
					endif
					end
				'coords:' : begin
					(*p).coordinates = str[1]
					end

				else:
			endcase
		endif
	endwhile
	close_file, unit

	no_list = 0
	plist = (*pstate).plist
	if ptr_valid(plist) eq 0 then goto, bad_ptr
	if size(*plist,/tname) ne 'POINTER' then begin
		no_list = 1
	endif else begin
		if ptr_valid( (*plist)[0] ) eq 0 then no_list=1
		if no_list eq 0 then if size(*(*plist)[0],/tname) ne 'STRUCT' then no_list=1
	endelse
	pindex = (*pstate).pindex

	if no_list eq 0 then begin
		n = n_elements(plist)	
		for i=0,n-1 do begin
			ptr_free, (*(*plist)[i]).pval
		endfor
	endif

	widget_control, (*pstate).frame_id_text, get_value=s
	(*pstate).frame = long(s)

	frame = (*pstate).frame
	kname = (*pstate).kvs_prefix + 'SL.' + 'frame' + strtrim(string(frame),2)

	n = n_elements(pt)	
	pl = ptrarr(n)
	index = intarr(n)

	for i=0,n-1 do begin
		pl[i] = ptr_new( {on:0, key:'', pval:ptr_new()})
		(*pl[i]).pval = pt[i]
		(*pl[i]).key = kname + '.scan.' + strtrim(string(i),2)
		(*pl[i]).on = 1
		index[i] = i
	endfor
	*plist = pl
	*pindex = index
	return

bad_file:
	warning, timeout=300, 'load_scan_list',['Error opening scan list file: ',file],/error
	return
bad_io:
	warning, timeout=300, 'load_scan_list','Error reading scan list file.',/error
	return
bad_ptr:
	warning, timeout=300, 'load_scan_list','Bad initial results pointer',/error
	return
bad_version:
	warning, timeout=300,  'load_scan_list', 'Bad file version',/error
	return
end

;-----------------------------------------------------------------

pro save_scan_list, pstate, file, error=error

; Write the scan list *p to file 'F'

COMPILE_OPT STRICTARR
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
		warning, timeout=300, 'save_scan_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		close, 1
		return
	endif
endif

	if lenchr(file) lt 1 then return
	plist = (*pstate).plist
	if ptr_valid(plist) eq 0 then goto, bad_ptr
	no_list = 0
	if size(*plist,/tname) ne 'POINTER' then begin
		no_list = 1
	endif else begin
		if ptr_valid( (*plist)[0] ) eq 0 then no_list=1
	endelse
	if no_list then return
	pindex = (*pstate).pindex				; display index
	pframe = (*pstate).pframe				; frame details
	psample = (*pstate).psample				; sample details
	
	n = 0
	for i=0L,n_elements(*pindex)-1 do begin
		j = (*pindex)[i]
		p = (*(*plist)[j]).pval
		if ptr_good(p) then n = n+1
	endfor
	if n eq 0 then return

	version = -2
	
	on_ioerror, bad_file
	openw,unit, file, /get_lun
	on_ioerror, bad_io

	printf, unit, 'Version:, ' + str_tidy(version)

	r = (*pframe).finder
	printf, unit, 'Frame-Finder-REF1:, ' + str_tidy(r.ref1.x) + ', ' + str_tidy(r.ref1.y) + ', ' + str_tidy(r.ref1.z) 
	printf, unit, 'Frame-Finder-REF2:, ' + str_tidy(r.ref2.x) + ', ' + str_tidy(r.ref2.y) + ', ' + str_tidy(r.ref2.z) 
	r = (*pframe).mapper1
	printf, unit, 'Frame-mapper1-REF1:, ' + str_tidy(r.ref1.x) + ', ' + str_tidy(r.ref1.y) + ', ' + str_tidy(r.ref1.z) 
	printf, unit, 'Frame-mapper1-REF2:, ' + str_tidy(r.ref2.x) + ', ' + str_tidy(r.ref2.y) + ', ' + str_tidy(r.ref2.z) 
	r = (*pframe).mapper2
	printf, unit, 'Frame-mapper2-REF1:, ' + str_tidy(r.ref1.x) + ', ' + str_tidy(r.ref1.y) + ', ' + str_tidy(r.ref1.z) 
	printf, unit, 'Frame-mapper2-REF2:, ' + str_tidy(r.ref2.x) + ', ' + str_tidy(r.ref2.y) + ', ' + str_tidy(r.ref2.z) 


	for i=0L,n_elements(*pindex)-1 do begin
		j = (*pindex)[i]
		p = (*(*plist)[j]).pval
		if ptr_good(p) eq 0 then continue
		
		printf, unit, '# ------------------------------------------------------------------------------'
		printf, unit, 'Scan-Number:, ' + str_tidy(i)
		printf, unit, 'Sample:, ' + (*p).sample

		q = where( (*p).sample eq (*psample).sample, nq)
		if nq gt 0 then begin
			r = (*psample)[q[0]].reference
			printf, unit, 'Sample-REF1:, ' + str_tidy(r.ref1.x) + ', ' + str_tidy(r.ref1.y) + ', ' + str_tidy(r.ref1.z) 
			printf, unit, 'Sample-REF2:, ' + str_tidy(r.ref2.x) + ', ' + str_tidy(r.ref2.y) + ', ' + str_tidy(r.ref2.z) 
			printf, unit, 'Sample-coords:, ' + str_tidy((*psample)[q[0]].coordinates) 
		endif

		printf, unit, 'Project:, ' + (*p).project
		printf, unit, 'Grain:, ' + (*p).region
		printf, unit, 'Comment:, ' + (*p).comment
		
		printf, unit, 'Origin:, ' + str_tidy((*p).origin.x) + ', ' + str_tidy((*p).origin.y)  + ', ' + str_tidy((*p).origin.z) 
		printf, unit, 'Interlace:, ' + str_tidy((*p).raster.interlace) 
		printf, unit, 'Size:, ' + str_tidy((*p).raster.size.x) + ', ' + str_tidy((*p).raster.size.y) 
		printf, unit, 'Pixel:, ' + str_tidy((*p).raster.pixel.x) + ', ' + str_tidy((*p).raster.pixel.y) 

		printf, unit, 'Dwell:, ' + str_tidy( 0.001 * (*p).raster.dwell) 
		printf, unit, 'Coords:, ' + str_tidy((*p).coordinates) 
	endfor

finish:
	close_file, unit
	return

bad_io:
	warning, timeout=300, 'save_scan_list','Error writing results file',/error
	goto, finish
bad_ptr:
	warning, timeout=300, 'save_scan_list','bad results pointer',/error
	goto, finish
bad_file:
	warning, timeout=300, 'save_scan_list',['error opening file: ',file],/error
	goto, finish
end

;-----------------------------------------------------------------

pro load_kvs_scan_list, pstate, protect=protect, error=error

; Read the scan list *plist from the KVS
;	/protect	protect the current scan in Edit.

COMPILE_OPT STRICTARR
error = 1
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
		warning, timeout=300, 'load_kvs_scan_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(protect) eq 0 then protect=0

	error = 1
	no_list = 0
	p = (*pstate).plist						; each points to {on:0, key:'', pval:ptr({scan}) }
	pindex = (*pstate).pindex				; display index
	pframe = (*pstate).pframe				; frame details
	psample = (*pstate).psample				; sample details

	check_kvs, (*pstate).kvs, error=error
	if error then begin
;		if typename( (*pstate).kvs) eq 'PYTHON' then begin
			scan_list_retry_kvs, pstate, error=error
;		endif
	endif
	if error then goto, kvs_fail

	if ptr_valid(p) eq 0 then goto, bad_ptr
	if size(*p,/tname) ne 'POINTER' then begin
		no_list = 1
	endif else begin
		if ptr_valid( (*p)[0] ) eq 0 then no_list=1
		if no_list eq 0 then if size(*(*p)[0],/tname) ne 'STRUCT' then no_list=1
		if n_elements(*p) eq 0 then no_list=1
	endelse

	q = where( *pindex ge 0, nq)
	if no_list or (nq eq 0) then protect=0

	widget_control, (*pstate).frame_id_text, get_value=s
	(*pstate).frame = long2(s)

	frame = (*pstate).frame
	kname = (*pstate).kvs_prefix + 'SL.' + 'frame' + strtrim(string(frame),2)

;	Frame details ...

	kref = kname + '.reference'
	template1 = define(/maia_frame_spec)
	ref = get_kvs( (*pstate).kvs, kref, template=template1, error=error)
	if error then  begin
		set_kvs, (*pstate).kvs, kref, template1, error=error
		if error then goto, bad
		ref = template1
	endif
	*pframe = ref

;	Scan list ...

	klist = kname + '.scan_list'
	list = get_kvs( (*pstate).kvs, klist, error=error)
	if error then begin
		list = ['none']
;		set_kvs, (*pstate).kvs, klist, list, error=error
		if error then goto, bad
	endif
	if list[0] eq 'none' then return

	n1 = n_elements(*p)
	if no_list then n1=0
	n = n_elements(list)
	if (n eq 0) or (list[0] eq '') then return

;	Scans ...

	key_protect = 'none'
	if protect and (((*pstate).sel.top ge 0) and ((*pstate).sel.top lt n1)) then begin
		j = (*pindex)[(*pstate).sel.top]
		key_protect = (*(*p)[j]).key
	endif
	template2 = define(/maia_scan_spec)
	template3 = define(/maia_sample_spec)

	j = n1
	for i=0L,n-1 do begin
		kscan = list[i]
		ishash = strmid(kscan,strlen(kscan)-5,5) eq '.hash'

		s = get_kvs( (*pstate).kvs, kscan, hash=ishash, template=template2, error=err)
		if err then continue
		sample = s.sample
		if (*pstate).enable_interlace eq 0 then s.raster.interlace=1

;		s.raster.dwell = 0.001 * s.raster.dwell			; ******* fix dwell to ms

		if no_list then begin
			key = ''
		endif else begin
			key = strarr(n1 > j)
			for k=0L,(n1>j)-1 do key[k] = (*(*p)[k]).key
		endelse
		q = where( key eq kscan, nq)

		if nq gt 0 then begin
			if kscan eq key_protect then begin			; test if changed externally
				
			endif else begin
				if ptr_valid( (*(*p)[q[0]]).pval) eq 0 then (*(*p)[q[0]]).pval=ptr_new(/allocate_heap)
				*(*(*p)[q[0]]).pval = s
				(*(*p)[q[0]]).key = kscan
				q = where( *pindex eq q[0], nq)
				if nq eq 0 then *pindex = [*pindex, q[0]]
			endelse
		endif else begin
			pt = ptr_new( {on:0, key:'', pval:ptr_new()} )
			(*pt).pval = ptr_new(s)
			(*pt).key = kscan
			(*pt).on = 1
			if j eq 0 then begin
				*p = pt
				*pindex = [j]
			endif else begin
				*p = [*p, pt]
				*pindex = [*pindex, j]
			endelse
;			(*(*p)[j]).pval = ptr_new(s)
;			(*(*p)[j]).key = kscan
;			(*(*p)[j]).on = 1
			no_list = 0
			j++
		endelse

;		Samples ...

		if n_elements( *psample) eq 0 then begin
			*psample = template3
			k = 0
		endif else if ( size(*psample,/tname) ne 'STRUCT') then begin
			*psample = template3
			k = 0
		endif else begin
			q = where( sample eq (*psample).sample, nq)
			if nq eq 0 then begin
				k = n_elements(*psample)
				*psample = [*psample, template3]
			endif else k=q[0]
		endelse

		ksample = kname + '.sample.' + sample
		sam = get_kvs( (*pstate).kvs, ksample, template=template3, error=error)
		if error then begin
			sam = template3
			sam.sample = sample
			set_kvs, (*pstate).kvs, ksample, sam, error=error
			if error then goto, bad
		endif
		(*psample)[k] = sam
	endfor

	q = where( *pindex ge 0, nq)
	if nq gt 0 then *pindex = (*pindex)[q]
	if nq eq 0 then *pindex = [-1]
	error = 0
	return

bad:
	return
bad_ptr:
	warning, timeout=300, 'load_kvs_scan_list',['Parameter structure variable has become ill-defined.','Abort KVS Load.'],/error
	return
kvs_fail:
	warning, timeout=300, 'load_kvs_scan_list',['KVS not reachable.','Abort KVS Load.'],/error
	return
end

;-----------------------------------------------------------------

pro save_kvs_scan_list, pstate, error=error

; Write the scan list *plist to the KVS

COMPILE_OPT STRICTARR
error = 1
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
		warning, timeout=300, 'save_kvs_scan_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	plist = (*pstate).plist					; each points to {on:0, key:'', pval:ptr({scan}) }
	pindex = (*pstate).pindex				; display index
	pframe = (*pstate).pframe				; frame details
	psample = (*pstate).psample				; sample details

	check_kvs, (*pstate).kvs, error=error
	if error then begin
;		if typename( (*pstate).kvs) eq 'PYTHON' then begin
			scan_list_retry_kvs, pstate, error=error
;		endif
	endif
	if error then goto, kvs_fail

	if ptr_valid(plist) eq 0 then goto, bad_ptr
	no_list = 0
	if size(*plist,/tname) ne 'POINTER' then begin
		no_list = 1
	endif else begin
		if ptr_valid( (*plist)[0] ) eq 0 then no_list=1
	endelse
	if no_list then return
	np = n_elements(*plist)
	ns = n_elements(*psample)

	frame = (*pstate).frame
	kname = (*pstate).kvs_prefix + 'SL.' + 'frame' + strtrim(string(frame),2)
	nk = strlen(kname)

	kref = kname + '.reference'
	print,'Save Frame reference: ', kref
	set_kvs, (*pstate).kvs, kref, *pframe, error=error
	if error then goto, bad

	klist = kname + '.scan_list'
	n = n_elements(*pindex)
	keys = ''
	all_keys = strarr(np)
	for i=0L,n-1 do begin
		all_keys[i] = (*(*plist)[i]).key
	endfor
	use_sample = intarr(ns)

	if (n ge 1) and ((*pindex)[0] ne -1) then begin		
		for i=0L,n-1 do begin
			j = (*pindex)[i]
			key = (*(*plist)[j]).key
			if strmid(key,strlen(key)-5,5) ne '.hash' then begin
				key = key + '.hash'
			endif

;			Blank key, cos one was appended ...

			if (key eq '') or (key eq '.hash') then begin
				k = 0
				repeat begin
					key = kname + '.scan.' + strtrim(string(k++),2)	+ '.hash'
					q = where( key eq all_keys, nq)
				endrep until nq eq 0
			endif

;			Check for a key with the wrong frame name (got changed?) ...

			s = strsplit( strmid( key, nk-1), '.', /extract)
			if long(s[0]) ne frame then begin
				k = 0
				repeat begin
					key = kname + '.scan.' + strtrim(string(k++),2) + '.hash'
					q = where( key eq all_keys, nq)
				endrep until nq eq 0				
			endif

			all_keys[j] = key
			p = (*(*plist)[j]).pval

			if ptr_good(p) then begin
				t = *p
;				t.raster.dwell = 1000. * t.raster.dwell			; ******* fix dwell to ms

				if t.coordinates eq '' then t.coordinates='mapper1'
				print,'Save Scan: ', key
				set_kvs, (*pstate).kvs, key, t, /hash, error=error
				if error then goto, bad

				q = where( t.sample eq (*psample).sample, nq)
				if nq gt 0 then use_sample[q[0]] = 1
			endif

			(*(*plist)[j]).key = key
			keys = [keys,key]
		endfor
		list = keys[1:*]

	endif else list=['']

	for i=0,n_elements(*psample)-1 do begin
		if use_sample[i] then begin
			ksample = kname + '.sample.' + (*psample)[i].sample
			if (*psample)[i].coordinates eq '' then (*psample)[i].coordinates='mapper1'
			print,'Save Sample: ', ksample, ' = ', (*psample)[i].sample
			set_kvs, (*pstate).kvs, ksample, (*psample)[i], error=error
			if error then goto, bad
		endif
	endfor

	print,'Save Scan List: ', klist, ' = ', list
	set_kvs, (*pstate).kvs, klist, list, error=error
	if error then goto, bad
	return
bad:
	return
bad_ptr:
	warning, timeout=300, 'save_kvs_scan_list','bad results pointer',/error
	return
kvs_fail:
	warning, timeout=300, 'save_kvs_scan_list',['KVS not reachable.','Abort KVS Save.'],/error
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_scan_list_site, wWidget

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
		warning, timeout=300, 'OnRealize_scan_list_site',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

;(*pstate).site = (*pstate).site_name eq 'Per' ? 1 : 0
widget_control, wWidget, set_combobox_select=(*pstate).site
end

;------------------------------------------------------------------------------------------

pro OnRealize_scan_list_coords_src, wWidget

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
		warning, timeout=300, 'OnRealize_scan_list_coords_src',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=(*pstate).coords_src
end

;------------------------------------------------------------------------------------------

pro OnRealize_scan_list_coords_tgt, wWidget

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
		warning, timeout=300, 'OnRealize_scan_list_coords_tgt',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=(*pstate).coords_tgt
end

;------------------------------------------------------------------------------------------

pro OnRealize_scan_list_presets, wWidget

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
		warning, timeout=300, 'OnRealize_scan_list_presets',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, get_value=s
q = where( strlowcase(strtrim(s,2)) eq 'sample change', nq)
if nq gt 0 then begin
	(*pstate).preset_mode = q[0]
	widget_control, wWidget, set_combobox_select=q[0]
endif
end

;-----------------------------------------------------------------

pro OnRealize_scan_list_Table, wWidget

	top = tlb_id(wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate
	
	w = widget_info( wWidget, /row_heights)
	geom = widget_info( wWidget, /geometry)
	tlb_geom = widget_info( top, /geometry)
	
	(*pstate).table = wWidget
	(*pstate).row_height = w[0]
	(*pstate).rows = 6
	(*pstate).xoffset = tlb_geom.xsize - geom.scr_xsize + 7
	(*pstate).yoffset = tlb_geom.ysize - geom.scr_ysize + 2
	
	update_scan_list_table, pstate
end

;------------------------------------------------------------------------------------------

pro update_scan_list_table, pstate, nsi

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
		warning, timeout=300, 'update_scan_list_table',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(nsi) lt 1 then nsi=(*pstate).sel.top
ns = nsi

case !version.os_family of
	'MacOS': begin
		x_ch_size = !d.x_ch_size * 1.3
		end
	'unix': begin
		x_ch_size = !d.x_ch_size * 1.3
		end
	else: begin
		x_ch_size = !d.x_ch_size
		end
endcase

; NOTE: Also see 'fill-button' code for column order ...

columns = ['Active','Sample','Project','X origin','Y origin','Z origin', $
			'X size','Y size','X pitch','Y pitch', $
			'Dwell', 'Interlace', 'Time', 'Comment', 'Region', 'Key']
nc = n_elements(columns)
widths = [6,15,9,replicate(7,3),replicate(7,2),replicate(7,2),6,8,7,22,12,25] * x_ch_size

plist = (*pstate).plist
if ptr_valid(plist) eq 0 then goto, bad
if size(*plist,/tname) ne 'POINTER' then goto, bad
if ptr_valid( (*plist)[0] ) eq 0 then goto, bad
if size(*(*plist)[0],/tname) ne 'STRUCT' then goto, bad

ps = (*pstate).ps
pm = (*pstate).pm
pindex = (*pstate).pindex
;if (*pindex)[0] lt 0 then goto, bad

n = n_elements(*pindex)
if n eq 0 then goto, bad
ns = ns < (n-1)

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

istart = intarr(n)
for i=0,n-1 do begin
	j = (*pindex)[i]
	p = (*(*plist)[j]).pval
	if (*p).active eq 2 then istart[i] = 1				; START - there can be only one
endfor
qstart = where( istart eq 1, nqstart)
if nqstart gt 1 then begin
	for k=1,nqstart-1 do begin
		i = qstart[k]
		j = (*pindex)[i]
		p = (*(*plist)[j]).pval
		(*p).active = 0									; START - turn extra "START" to "ON"
	endfor
endif
istop = intarr(n)
for i=0,n-1 do begin
	j = (*pindex)[i]
	p = (*(*plist)[j]).pval
	if (*p).active eq 3 then istop[i] = 1				; STOP - there can be only one
endfor
qstop = where( istop eq 1, nqstop)
q = where( (qstop lt qstart[0]) and (nqstop gt 0), nq)
if nq gt 0 then begin
	for k=0,nq-1 do begin
		i = qstop[q[k]]
		j = (*pindex)[i]
		p = (*(*plist)[j]).pval
		(*p).active = 0									; any "STOP" before first "START" cycle to "ON"
	endfor
endif
if nqstop gt 1 then begin
	for k=1,nqstop-1 do begin
		i = qstop[k]
		j = (*pindex)[i]
		p = (*(*plist)[j]).pval
		(*p).active = 0									; STOP - turn extra "STOP" to "ON"
	endfor
endif

t = strarr(nc,n)
active = (*pstate).active
yes_no = ['no','yes']
c = intarr(3,nc,n)
c[0,*,*] = (spec_colour('white',/rgb))[0]
c[1,*,*] = (spec_colour('white',/rgb))[1]
c[2,*,*] = (spec_colour('white',/rgb))[2]
tt = 0.0
no_stop = 1

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

for i=0L,n-1 do begin
	k = 0
	j = (*pindex)[i]
	p = (*(*plist)[j]).pval
	if ptr_good(p) eq 0 then continue
	
	if (*pstate).scan_sequence.active and ((*pstate).scan_sequence.scan.index eq i) then begin
		for l=0,nc-1 do c[*,l,i] = spec_colour('green',/rgb)
	endif
	scan_list_check_bounds, pm, p, ps, /silent, error=error
	if error then begin
		for l=0,nc-1 do c[*,l,i] = spec_colour('yellow',/rgb)
	endif
	ttonly = scan_list_time( p) 
	sttime = time_legend( ttonly)
	if no_stop and ((active[ (*p).active] eq 'START') or (active[ (*p).active] eq 'STOP') or (active[ (*p).active] eq 'ON')) then begin
		tt = tt + ttonly
	endif
	if active[ (*p).active] eq 'STOP' then no_stop = 0

	t[k,i] = active[ (*p).active]  &  k++
	t[k,i] = (*p).sample  &  k++
	t[k,i] = (*p).project  &  k++
	t[k,i] = str_tidy( (*p).origin.x, places=3,length=6)  &  k++
	t[k,i] = str_tidy( (*p).origin.y, places=3,length=6)  &  k++
	t[k,i] = str_tidy( (*p).origin.z, places=3,length=6)  &  k++

	t[k,i] = str_tidy( (*p).raster.size.x, places=3,length=6)  &  k++
	t[k,i] = str_tidy( (*p).raster.size.y, places=3,length=6)  &  k++
	t[k,i] = str_tidy( (*p).raster.pixel.x, places=4,length=6)  &  k++
	t[k,i] = str_tidy( (*p).raster.pixel.y, places=4,length=6)  &  k++

	t[k,i] = str_tidy( (*p).raster.dwell, places=-2)  &  k++
	t[k,i] = str_tidy((*p).raster.interlace)  &  k++
	t[k,i] = sttime  &  k++

	t[k,i] = (*p).comment  &  k++
	t[k,i] = (*p).region  &  k++
	t[k,i] = (*(*plist)[j]).key  &  k++
endfor

stt = time_legend( tt)
widget_control, (*pstate).time_text, set_value = ' '+stt
widget_control, (*pstate).time_text2, set_value = ' '+stt

if ptr_valid( (*pstate).headings) then ptr_free, (*pstate).headings
(*pstate).headings = ptr_new(columns)
(*pstate).columns = nc
(*pstate).rows = n
rows = string( indgen(n))

widget_control, (*pstate).table, set_value = t, column_widths=widths, $
			row_labels = rows, column_labels=columns, $
			table_xsize=(*pstate).columns, table_ysize=n, align=2
;			use_table_select=[0,0,nc-1,n-1]
widget_control, (*pstate).table, background_color=c
goto, done

bad:
	t = strarr(nc,256)
	widget_control, (*pstate).table, set_value = t, $
			row_labels = str_tidy(indgen(256)), column_labels=columns, $
			table_xsize=nc, table_ysize=16, align=2
	ns = 0
done:
	return
	end

;------------------------------------------------------------------------------------------

pro update_scan_list_time, pstate

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
		warning, timeout=300, 'update_scan_list_time',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

plist = (*pstate).plist
if ptr_valid(plist) eq 0 then goto, bad
if size(*plist,/tname) ne 'POINTER' then goto, bad
if ptr_valid( (*plist)[0] ) eq 0 then goto, bad
if size(*(*plist)[0],/tname) ne 'STRUCT' then goto, bad

pindex = (*pstate).pindex
n = n_elements(*pindex)
if n eq 0 then goto, bad
active = (*pstate).active

;            0     1      2      3      4      5
; active = ['ON','OFF','START','STOP','REF1','REF2']

tt = 0.0
ttone = 0.0
stt = 'n/a'
no_stop = 1

for i=0L,n-1 do begin
	k = 0
	j = (*pindex)[i]
	p = (*(*plist)[j]).pval
	if ptr_good(p) eq 0 then continue
	
	ttonly = scan_list_time( p)
	if (*pstate).scan_sequence.active and ((*pstate).scan_sequence.scan.index eq i) then begin
		perc = (*pstate).scan_sequence.progress
		ttonly = ttonly * ( (100.-perc) > 0.)/100.
		ttone = ttonly
	endif
	if no_stop and ((active[ (*p).active] eq 'START') or (active[ (*p).active] eq 'STOP') or (active[ (*p).active] eq 'ON')) then begin
		tt = tt + ttonly
	endif
	if (active[ (*p).active] eq 'STOP') then begin
		no_stop = 0
	endif
endfor

	if ttone gt 0. then stt = time_legend( ttone)
	widget_control, (*pstate).time_text3, set_value = ' '+stt

	stt = time_legend( tt)
	widget_control, (*pstate).time_text, set_value = ' '+stt
	widget_control, (*pstate).time_text2, set_value = ' '+stt

bad:
	return
	end

;--------------------------------------------------------------------------

; Note: If this is executed before compile, it loads the SAV file of same name, which is
;	compiled under IDL 8.5.1. Compile first under 8.7 for testing.

pro maia_scan_list, debug=debug, fake=fake, notimer=notimer, test=test, ztap=use_ztap, per=per

	mm_scan_list, debug=debug, fake=fake, notimer=notimer, test=test, ztap=use_ztap, per=per
	return
end

;--------------------------------------------------------------------------

; Note: If this is executed before compile, it loads the SAV file of same name, which is
;	compiled under IDL 8.5.1. Compile first under 8.7 for testing.

pro mm_scan_list, debug=debug, fake=fake, notimer=notimer, test=test, ztap=use_ztap, per=per

COMPILE_OPT STRICTARR
if n_elements(debug) lt 1 then debug=0
if n_elements(fake) lt 1 then fake=0
if n_elements(notimer) lt 1 then notimer=0
if n_elements(use_ztap) eq 0 then use_ztap=0
if n_elements(test) lt 1 then test=0
if n_elements(per) lt 1 then per=0

	mel_kvs_endpoint = 'tcp://mm-mel-1-cl.it.csiro.au:29320'
	mel_sl_config = 'MM.Mel.SL.1.config'
	per_kvs_endpoint = 'tcp://mm-per-1-kf.it.csiro.au:29320'
	per_sl_config = 'MM.Per.SL.2.config'

	sl_config = mel_sl_config
	kvs_endpoint = mel_kvs_endpoint
	if per then begin
		kvs_endpoint = per_kvs_endpoint
		sl_config = per_sl_config
	endif
	if fake then sl_config = 'MM.fake.SL.config'

	argv = command_line_args( count=argc)
	if argc ge 1 then begin
		if argv[0] eq 'per' then begin
			kvs_endpoint = per_kvs_endpoint
			sl_config = per_sl_config
			goto, cont
		endif else if argv[0] eq 'mel' then begin
			sl_config = mel_sl_config
			kvs_endpoint = mel_kvs_endpoint
			goto, cont
		endif
		kvs_endpoint = argv[0]					; KVS endpoint
	endif
	if argc ge 2 then begin
		sl_config = argv[1]						; config KVS key
	endif
	if argc ge 3 then begin
		q = where( argv[2:*] eq 'ztap', nq)
		if nq ge 1 then use_ztap=1				; switch on ztap = 1
		q = where( argv[2:*] eq 'notimer', nq)
		if nq ge 1 then notimer=1				; switch on notimer = 1
		q = where( argv[2:*] eq 'test', nq)
		if nq ge 1 then test=1					; switch on test mode
	endif
	print,'scan_list: args = ', argv

;	Scan_List (maia_scan_list.sav) loads routines from GeoPIXE.sav
;	These routines are NOT compiled into mm_scan_list.sav

cont:
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
			if file_test(file) eq 0 then begin
				r = dialog_message(['GeoPIXE library not found.','Failed to restore "GeoPIXE.sav".'],/error)
			endif else found=1
		endif else found=1
	endif else found = 1
	if found then restore, file

	startupp, /colours, /maia						; setup IDL

;	warning, 'MM_scan_list',['argv='+strjoin(argv,','), 'config='+sl_config, 'ztap='+string(use_ztap)]

	scan_list, kvs_endpoint=kvs_endpoint, config_key=sl_config, debug=debug, notimer=notimer, ztap=use_ztap, test=test
	return
end

;------------------------------------------------------------------------------------------

pro scan_list, group_leader=group, TLB=tlb, data=plist, path=path, $
				debug=debug, pars=sl_pars, config_key=config_key, test=test, $
				kvs_endpoint=kvs_endpoint, notimer=notimer, ztap=use_ztap, per=per

;	/notimer		to suppress frame and coords KVS read timers
;	/debug			suppress error pop-ups, stop on error
;	kvs_endpoint	will default to KVS entry in 'geopixe.conf' file.

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_retry_kvs, lock_retry_kvs
lock_retry_kvs = 0

common c_errors_1, catch_errors_on
if n_elements(debug) lt 1 then debug=0
catch_errors_on = 1								; enable error CATCHing
if debug then catch_errors_on = 0				; disable error CATCHing
ErrorNo = 0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning, 'scan_list',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif else on_error,0

if n_elements(group) eq 0 then group = 0L
if n_elements(path) lt 1 then path=''
if n_elements(notimer) lt 1 then notimer=0
if n_elements(config_key) eq 0 then config_key = 'MM.Mel.SL.1.config'
if n_elements(use_ztap) eq 0 then use_ztap=0
if n_elements(test) lt 1 then test=0
if n_elements(kvs_endpoint) lt 1 then kvs_endpoint = 'tcp://mm-mel-1-cl.it.csiro.au:29320'
if n_elements(per) lt 1 then per=0

version = '8.6o'
enable_interlace = 0							; enable the interlace raster option
clayton_wrong_left_right = 1					; wrong end-stations (1=Left, 2=Right) in Clayton only
startupp										; load GeoPIXE libraries

pver = python_version( revision=prev)
iver = idl_version( revision=irev)
print,'IDL=',iver,', python=',pver
if ((float2(prev) ge 3.6) and (float2(irev) ge 8.8)) then begin		; IDL 8.8 works with python 3.6+
	print,'IDL 8.8+ and python 3.6+ found'
endif else if ((prev eq '2.7') and (irev eq '8.5')) then begin		; IDL 8.5.1 works with python 2.7
	print,'IDL 8.5 and python 2.7 found'
endif else begin
	warning,'scan_list',['Maia Mapper python library is needed for Scan-List.', $
			'However, python version found = ' + pver, ' which is not compatible with IDL ' + iver+'.', '', $
			'MM Libs need python 2.7 with IDL 8.5.1, or python 3.6+ with IDL 8.8+,', $
			'so you will need to configure for correct IDL + python combination.']
	return
endelse

if per then begin
	kvs_endpoint = 'tcp://mr-05-per.it.csiro.au:29320'
	config_key = 'MM.Per.SL.2.config'
endif
state_key = strip_file_ext(config_key) + '.state.hash'

print,'version = ', version
print,'kvs = ', kvs_endpoint
print,'config_key = ', config_key
print,'test = ', test

prefs = geopixe_defaults( error=err, source='mm_scan_list')
no_kvs = 0
if err eq 0 then begin
	if prefs.kvs.enable eq 0 then begin
		no_kvs = 1
	endif else begin
		if n_elements(kvs_endpoint) eq 0 then kvs_endpoint = prefs.kvs.endpoint
	endelse
endif else if n_elements(kvs_endpoint) eq 0 then no_kvs=1
if no_kvs then begin
	warning,'scan_list',['KVS is not enabled.', $
		'KVS prefs need to be added to your "geopixe.conf" file', $
		'in your home .geopixe directory.']
	return
endif

s = get_login_info()				; make a local ID for this session (use for master mode)
clientname = s.machine_name
username = s.user_name
local_id = clientname + ':' + username + '-' + str_tidy((randomu(seed,1,/ulong))[0])

src_coords_list = ['finder','mapper1','mapper2','mapper1','mapper2']
tgt_coords_list = ['mapper1','mapper2']

;-------------------------------------------------------------------------------------
;
; Open a comms object ... (note this also can occur in 'scan_list_retry_kvs')
;
;	NOTE: By default, this assumes that the rsyslog server is the same as the KVS node.
;	If not, then add a line for "logging server" in 'geopixe.conf'.

server = prefs.logging.server
comms = open_comms( ztap=use_ztap, source='mm_scan_list', server=server, error=err)
if err then begin
	warning,'scan_list',['Failed to open a comms object.','Server = '+server]
	return
endif
save_comms_in_common, comms		; save comms for implicit 'log_message' in 'warning', 'alarm_popup'
;print, 'scan_list started, with logging using server = '+server
log_message, comms, type='INFO', 'scan_list started, with logging using server = '+server
s = get_login_info()
clientname = s.machine_name
username = s.user_name
log_message, comms, type='INFO', 'scan_list, machine='+clientname+', username='+username

; These 'server' and ;'source' entries are redundant, as only the first open_comms set these up ...

if use_ztap then begin
	comms2 = comms
	comms3 = comms
	comms4 = comms
endif else begin
	comms2 = open_comms( source='mm_scan_list2', server=server, error=err)
	if err then begin
		warning,'scan_list',['Failed to open a comms 2 object.']
		return
	endif
	comms3 = open_comms( source='mm_scan_list3', server=server, error=err)
	if err then begin
		warning,'scan_list',['Failed to open a comms 3 object.']
		return
	endif
	comms4 = open_comms( source='mm_scan_list4', server=server, error=err)
	if err then begin
		warning,'scan_list',['Failed to open a comms 4 object.']
		return
	endif
endelse

; Open the Key-value store ... (note this also can occur in 'scan_list_retry_kvs')

print,'Open KVS using endpoint: ', kvs_endpoint
skip_kvs = 0
kvs = open_kvs(kvs_endpoint, comms=comms, ztap=use_ztap, error=err)
if err then begin
	warning,'scan_list',['Failed to open the KVS endpoint: ',kvs_endpoint]
	print,'	Failed to open the KVS endpoint: '+kvs_endpoint
	return
endif

check_kvs, kvs, error=err
if err then begin
	warning,'scan_list',['Check of KVS failed.','Abort.']
	if typename( kvs) ne 'PYTHON' then begin
		warning,'scan_list',['KVS not a valid PYTHON object.','Abort.']
	endif
	close_kvs, kvs
	return
;	skip_kvs = 1
endif

; Read the config in the KVS ...

print,'Open KVS config: ', config_key
if exists_kvs( kvs, config_key) then begin
	endstation = extract_extension( strip_file_ext(config_key))
	if not inumeric(endstation) then begin
		warning,'scan_list',['Error parsing Kandinski config ('+config_key+') for endstation number: ',endstation],/error
		return
	endif
	endstation_index = fix2(endstation)
	config = get_kvs( kvs, config_key, error=err)
	if err eq 0 then begin
		kvs_prefix = config.prefix
		if strmid(kvs_prefix,strlen(kvs_prefix)-1,1) ne '.' then kvs_prefix=kvs_prefix+'.'
		s = strsplit( kvs_prefix, '.', /extract, count=ns)
		site = s[1]

		stage_endpoints = config.endpoints.stage
		kandinski_endpoints = config.endpoints.kandinski
		safety_endpoints = config.endpoints.safety
		excillum_endpoints = config.endpoints.source
		mmcl_stage = 'cmd.req.stage.' + kvs_prefix + 'SL'
		mmcl_safety = 'cmd.req.safety.' + kvs_prefix + 'SL'
		mmcl_excillum = 'cmd.req.source.' + kvs_prefix + 'SL'
		print,'	Stage: endpoints=',stage_endpoints,', mmcl=',mmcl_stage
		print,'	Safety: endpoints=',safety_endpoints,', mmcl=',mmcl_safety
		print,'	Excillum: endpoints=',excillum_endpoints,', mmcl=',mmcl_excillum

		s = strsplit( stage_endpoints, '.', /extract, count=ns)
		if s[ns-1] eq 'endpoints' then begin
			if endstation_index ne long2(s[ns-2]) then begin
				warning,'scan_list',['SL config ('+config_key+') contains inconsistent Stage endstation: ',s[ns-2]],/error
				return
			endif
		endif else begin
			warning,'scan_list',['SL config ('+config_key+') contains strange Stage endpoints: ',stage_endpoints ],/error
			return
		endelse

		if tag_present( 'kandinski', config.endpoints) then begin
			kandinski_config = strip_file_ext( kandinski_endpoints) + '.config
			print,'	Kandinski: config=',kandinski_config
			if exists_kvs( kvs, kandinski_config) then begin
				kconfig = get_kvs( kvs, kandinski_config, error=err)
				if err eq 0 then begin
					if tag_present( 'IP', kconfig) then begin
						ip_maia = kconfig.ip
						port_maia = kconfig.port
						enable_maia = kconfig.enable
					endif else if tag_present( 'HOST', kconfig) then begin
						ip_maia = kconfig.host
						port_maia = 9001
						enable_maia = 1
					endif else begin
						warning,'scan_list',['Error reading Kandinski config: ',kandinski_config],/error
						return
					endelse
				endif else begin
					warning,'scan_list',['Failed to read the Kandinski KVS config: ',kandinski_config],/error
					return
				endelse
			endif else begin
				warning,'scan_list',['Kandinski config does not exist in KVS: ',kandinski_config],/error
				return
			endelse
		endif else begin
			warning,'scan_list',['"Kandinski" endpoints not found in KVS config endpoints: ',config.endpoints],/error
			return
		endelse
	endif else begin
		warning,'scan_list',['Failed to open the ScanList KVS config: ',config_key],/error
		return
	endelse
endif else begin
	warning,'scan_list',['ScanList config does not exist in KVS: ',config_key],/error

	kvs_prefix = 'MM.Mel.'
	site = 'Mel'
;	stage_endpoints = 'MM.test.endpoints'
	stage_endpoints = 'MM.Mel.ST.1.endpoints'
	safety_endpoints = 'MM.Mel.SS.endpoints'
	excillum_endpoints = 'MM.Mel.EX.endpoints'
;	mmcl_stage = 'test.a.b.c.x'
	mmcl_stage = 'cmd.req.stage.MM.Mel.SL'
	mmcl_safety = 'cmd.req.safety.MM.Mel.SL'
	mmcl_excillum = 'cmd.req.source.MM.Mel.SL'
	endstation_index = 1

	ip_maia = 'hymod-262.clayton.csiro.au'
	port_maia = 9001
	enable_maia = 1
	token_maia = '0'
	skip_kvs = 1
;	return
endelse

print,'Test KVS state_key Hash: ', state_key
	scan_sequence = { $
		active:				0, $			; scan list is running (1)
		time_start:			systime(1), $	; time scan started
		pause:				0, $			; raster paused state
		lock_paused: 		0, $			; keep the stage paused
		move: {		seqno:	0L}, $			; 'seqno' sent for move
		scan: {		index:	0, $			; present scan list index
					seqno:	0L}, $			; 'seqno' sent for scan
		raster_on:			0, $			; raster started and 'running'
		power_on:			0, $			; Excillum power was ON and full at start of sequence
		progress:			0.0, $			; % progress through a scan
		error:				0, $			; error in current scan
		stroke:				0L}				; last completed stroke (use for "Append")

if exists_kvs( kvs, state_key) eq 0 then begin
	set_kvs, kvs, state_key, 'master_id', '', /hash, /lower, error=err
	set_kvs, kvs, state_key, 'pushed_master', '', /hash, /lower, error=err
	set_kvs, kvs, state_key, 'frame_id', '-1', /hash, /lower, error=err
	set_kvs, kvs, state_key, 'scan_sequence', scan_sequence, /hash, /lower, error=err
	set_kvs, kvs, state_key, 'utc', systime(1), /hash, /lower, error=err
endif
s = get_kvs( kvs, state_key, 'frame_id', /hash, error=err)
current_frame = (err ? 0: (long2(s) > 0))

isite = 0
if site eq 'Per' then isite=1

if site eq 'Mel' then begin
	shutter_name = clayton_wrong_left_right ? ['none','Left','Right'] : ['none','Right','Left']
endif else begin
	shutter_name = ['none','Right','Left']
endelse

; Open the stage, Safety and Excillum VSUBs (note this also can occur in 'scan_list_retry_kvs')

vsub_stage = open_vsub(kvs, stage_endpoints, subscribe=['var.rt.stage','var.change.stage','alert'], mmcl=mmcl_stage, comms=comms3, ztap=use_ztap, error=err)
if err then begin
	warning,'scan_list',['Failed to open the Stage VSUB endpoints: ',stage_endpoints]
	if skip_kvs eq 0 then return
endif

vsub_safety = open_vsub(kvs, safety_endpoints, subscribe=['var.change.safety','var.rt.safety','alert'], mmcl=mmcl_safety, comms=comms2, ztap=use_ztap, error=err)
if err then begin
	warning,'scan_list',['Failed to open the Safety VSUB endpoints: ',safety_endpoints]
	if skip_kvs eq 0 then return
endif
vsub_excillum = open_vsub(kvs, excillum_endpoints, subscribe=['var.rt.source','var.change.source','alert'], mmcl=mmcl_excillum, comms=comms4, ztap=use_ztap, error=err)
if err then begin
	warning,'scan_list',['Failed to open the Excillum VSUB endpoints: ',excillum_endpoints]
	if skip_kvs eq 0 then return
endif

preset_names = ['none']
presets = {name:'none', x:0.0, Y:0.0, Z:0.0}

r = req_vsub( vsub_stage, 'List-Move-Presets', error=err, message=mess)
if err then begin
	warning,'scan_list_event',['List-Move-Presets stage error.','REQ to Stage VSUB returned an error:',mess]
endif else begin
	n = n_elements(r)
	if n ge 1 then begin
		presets = replicate( {name:'', x:0.0, Y:0.0, Z:0.0}, n)
		for i=0,n-1 do begin
			presets[i].name = r[i]
		endfor
		preset_names = presets.name
	endif
endelse

;-------------------------------------------------------------------------------------
;
; Maia Kandinski control socket
; Use a socket to control Maia set-up and parameter display

print,'Open Maia control socket ...'
ps = ptr_new(/allocate_heap)
*ps = open_socket( ip=ip_maia, port=port_maia, token=token_maia, enable=enable_maia, retries=0, $
						client='Scan_List', connect_timeout=30, error=error)
vkan = 0
if error eq 0 then begin
	vkan = socket_command_get( ps, 'ps.version', class='config', error=error)
	if error then begin
		warning,'scan_list','Error reading config.ps.version" from Kandinski.'
		return
	endif
endif else begin
	warning,'scan_list','Failed to open Maia control socket.'
	print,'	Failed to open Maia control socket.'
	if test eq 0 then return
endelse


;-------------------------------------------------------------------------------------

; Maia Mapper parameters harvested

if enable_interlace eq 0 then begin
	warning,'scan_list',['GeoPIXE Cluster/YLUT handling does not support interlaced files yet.', $
				'Hence, "Interlace" is disabled in ScanList for now.','','Using logging server = '+server]
endif

pm = ptr_new(/allocate_heap)
*pm = {		$	
		status: {	kvs:			0, $					; KVS up
					stage:			0, $					; Stage VSUB server active
					home:			-1, $					; Stage homed (1), not-homed (0), off or homing (-1)
					moving:			0, $					; Stage moving (1), not-moving (0)
					safety:			0, $					; Safety VSUB server active
					blog:			0, $					; Maia connected to blog OK
					discard:		1, $					; Maia/blog discard
					discard_rate:	0.0, $					; Maia/blog discard rate /s
					maia:			0, $					; Maia/Kandinski VSUB server active
					bias:			0.0, $					; Maia detector bias voltage
					temp:			0.0, $					; Maia detector temp (C)
					bpinterlock:	0, $					; Maia interlock status (1=good)
					excillum:		0, $					; Excillum VSUB server active
					power:			0}, $					; Excillum power good (above 95%)
		version: {	kandinski:		vkan}, $				; versions of things
		endstation:					endstation_index, $		; index to end-station (1, 2)
		current: {	position: {		x:	0.0, $				; current source position
									y:	0.0, $				; Y
									z:	0.0}, $				; Z
					newscan: {	new: 0, $					; flags a new (1) scan, not used yet
								origin: {	x: 0.0, $		; origin X
											y: 0.0, $		; Y
											z: 0.0}, $		; Z
								size: {		x: 0.0, $		; scan X size
											y: 0.0}, $		; Y
								zsurface: fltarr(4) }}, $	; Z surface bilinear coefficients
		target: {	position: {		x:	0.0, $				; current target position
									y:	0.0, $				; Y
									z:	0.0}}, $				; Z
		previous: {	position: {		x:	0.0, $				; previous target position
									y:	0.0, $				; Y
									z:	0.0}}, $				; Z
		finder: {	position: {		x:	0.0, $				; finder cursor position
									y:	0.0, $				; Y
									z:	0.0}, $				; Z
					newscan: {	new: 0, $					; flags a new (1) scan, not used yet
								origin: {	x: 0.0, $		; origin X
											y: 0.0, $		; Y
											z: 0.0}, $		; Z
								size: {		x: 0.0, $		; scan X size
											y: 0.0}, $		; Y
								zsurface: fltarr(4)}}, $	; Z surface bilinear coefficients
		mapper1: {	position: {		x:	0.0, $				; mapper 1 stage position
									y:	0.0, $				; Y
									z:	0.0}}, $			; Z
		mapper2: {	position: {		x:	0.0, $				; mapper 2 stage position
									y:	0.0, $				; Y
									z:	0.0}}, $			; Z
		geopixe1: {	position: {		x:	0.0, $				; mapper 1 pos seen in GeoPIXE images
									y:	0.0, $				; Y
									z:	0.0}, $				; Z
					newscan: {	new: 0, $					; GeoPIXE 1 newscan
								origin: {	x: 0.0, $		; origin X
											y: 0.0, $		; Y
											z: 0.0}, $		; Z
								size: {		x: 0.0, $		; scan X size
											y: 0.0}, $		; Y
								zsurface: fltarr(4)}}, $	; Z surface bilinear coefficients (not used)
		geopixe2: {	position: {		x:	0.0, $				; mapper 1 pos seen in GeoPIXE images
									y:	0.0, $				; Y
									z:	0.0}, $				; Z
					newscan: {	new: 0, $					; GeoPIXE 2 newscan
								origin: {	x: 0.0, $		; origin X
											y: 0.0, $		; Y
											z: 0.0}, $		; Z
								size: {		x: 0.0, $		; scan X size
											y: 0.0}, $		; Y
								zsurface: fltarr(4)}}, $	; Z surface bilinear coefficients (not used)

;		Note that (*pm).stage gets used as a whole for collection of limits data from daemon. Take care!

		stage: { 		min: {	x:	0.0, $					; min X position
								y:	0.0, $					; min Y
								z: -5.0}, $					; min Z
						max: {	x:	700.0, $				; max X position
								y:	150.0, $				; max Y
								z:  1.0}, $					; max Z
						speed: { x:	10.0, $					; max X velocity
								y:	10.0, $					; max Y
								z:  10.0}, $				; max Z
				acceleration: {	x:	10.0, $					; max X acceleration
								y:	10.0, $					; max Y
								z:  10.0}, $				; max Z

						move: {	status:	'', $				; moving status
								x:	0.0, $					; stage X
								y:	0.0, $					; stage Y
								z:  0.0}, $					; stage Z
						dest: {	x:	0.0, $					; destination X
								y:	0.0, $					; destination Y
								z:  0.0}, $					; destination Z
						scan: {	status:	'', $				; scanning status
								progress:	0.0}} $			; propgress (%)
		} 
local_pm = 1

; Read the Stage limits from KVS

stage_stub = strmid( stage_endpoints, 0, locate('endpoints', stage_endpoints))
stage_limits = stage_stub + 'limits'
if exists_kvs( kvs, stage_limits) then begin
	limits = get_kvs( kvs, stage_limits, template=(*pm).stage, error=err)
	if err eq 0 then begin
		(*pm).stage = limits
	endif else begin
		warning,'scan_list',['Failed to open the Stage Limits in KVS: ',stage_limits]
		return
	endelse
endif else begin
;	warning,'scan_list',['Stage Limits does not exist in KVS: ',stage_limits]
;	return
endelse

;-------------------------------------------------------------------------------------

no_list = 0
local_list = 0
if ptr_valid(plist) eq 0 then begin
	plist = ptr_new(/alloc)
	local_list = 1
endif
if size(*plist,/tname) ne 'POINTER' then begin
	no_list = 1
endif else begin
	if ptr_valid( (*plist)[0] ) eq 0 then no_list=1
	if no_list eq 0 then if size(*(*plist)[0],/tname) ne 'STRUCT' then no_list=1
endelse
if no_list then begin
	*plist = [ptr_new()]
	local_list = 1
endif
index = [-1]

case !version.os_family of
	'MacOS': begin
		yw = 220
		mode_xsize = 90
		origin_xsize = 90
		text_xsize = 60
		text_xsize2 = 50
		help_xsize = 875
		help_lines = 4
		update_xsize = 110
		led_space = 0
		led_xsize = 70
		led_xsize2 = 85
		led_xsize3 = 70
		end
	'unix': begin
		yw = 252
		mode_xsize = 130
		origin_xsize = 90
		text_xsize = 60
		text_xsize2 = 50
		help_xsize = 875
		help_lines = 4
		update_xsize = 110
		led_space = 0
		led_xsize = 70
		led_xsize2 = 85
		led_xsize3 = 70
		end
	else: begin
		yw = 219
		mode_xsize = 90
		origin_xsize = 90
		text_xsize = 60
		text_xsize2 = 40
		help_xsize = 855
		help_lines = 4
		update_xsize = 80
		led_space = 1
		led_xsize = 60
		led_xsize2 = 75
		led_xsize3 = 60
		end
endcase

active = ['ON','OFF','START','STOP','REF1','REF2']
;axes = ['Stage X','Stage Y','Stage Z','Stage A']
;shapes = ['Rectangular Snake','Elliptical scan']
sites = ['Melbourne','Perth']
if clayton_wrong_left_right and (isite eq 0) then begin
	src_coords = ['Finder Coords','Mapper #1 (left) Coords','Mapper #2 (right) Coords','GeoPIXE #1 (left) Coords','GeoPIXE #2 (right) Coords']
	tgt_coords = ['Mapper #1 (left) Coords','Mapper #2 (right) Coords']
	src_help = 'Select the source of position coordinate updates (e.g. for Ref marks) and scan area selection (e.g. for new scan area/origin definitions or using "Capture" in "Scan Edit") between "Finder Coords" ' + $
			'and "Mapper Coords" for either "1 left" or "2 right" end-station or for image coordinates in GeoPIXE regions on images acquired on either "Mapper" endstation.'
	tgt_help = 'Select the target coordinate system for Translation (e.g. for Mapper Ref marks) and scan area selection (e.g. for new scan area/origin definition) between ' + $
			'"Mapper Coords" for either "1 left" or "2 right" end-station. Usually, the "Target" will be the End-station number as shown in the title bar. Only these "target" coordinates will be accepted by the Stage daemon.'
endif else begin
	src_coords = ['Finder Coords','Mapper #1 (right) Coords','Mapper #2 (left) Coords','GeoPIXE #1 (right) Coords','GeoPIXE #2 (left) Coords']
	tgt_coords = ['Mapper #1 (right) Coords','Mapper #2 (left) Coords']
	src_help = 'Select the source of position coordinate updates (e.g. for Ref marks) and scan area selection (e.g. for new scan area/origin definitions or using "Capture" in "Scan Edit") between "Finder Coords" ' + $
			'and "Mapper Coords" for either "1 right" or "2 left" end-station or for image coordinates in GeoPIXE regions on images acquired on either "Mapper" endstation.'
	tgt_help = 'Select the target coordinate system for Translation (e.g. for Mapper Ref marks) and scan area selection (e.g. for new scan area/origin definition) between ' + $
			'"Mapper Coords" for either "1 right" or "2 left" end-station. Usually, the "Target" will be the End-station number as shown in the title bar. Only these "target" coordinates will be accepted by the Stage daemon.'
endelse

; LEDs             0 (off)                       1 (Red)                       2 (Green)
lnames = geopixe_root + ['images/led-off-12x14.jpeg','images/led-red-12x14.jpeg','images/led-green-12x14.jpeg']

; 	top-level base  --------------------------------------------------------------------------------------

tracking = 1					; later have context-sensitive help window

tlb = widget_base( /column, title='Scan List '+version+' (GeoPIXE '+geopixe_version()+')   Site:"'+kvs_prefix+'", Endstation:'+str_tidy(endstation_index), /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='scan_list_TLB', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=2 ,YPAD=2, /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=1, /base_align_center, /align_center)

s0base = widget_base( tbase, /row, /base_align_center, ypad=1, xpad=2, space=20)
s0base1 = widget_base( s0base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)
label = widget_label( s0base1, value='Site:')
site_mode = widget_combobox( s0base1, value=sites, uname='site-mode', /tracking, scr_xsize=origin_xsize*1.3, $
			Notify_Realize='OnRealize_scan_list_site', $
			uvalue='Select the Maia Mapper site between "Melbourne" and "Perth".', /align_right, sensitive=0)
s0base2 = widget_base( s0base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)
label = widget_label( s0base2, value='Source:')
sample_coords_src_mode = widget_combobox( s0base2, value=src_coords, uname='coords-src-mode', /tracking, scr_xsize=origin_xsize*2, $
			Notify_Realize='OnRealize_scan_list_coords_src', $
			uvalue=src_help, /align_right)
s0base4 = widget_base( s0base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)
label = widget_label( s0base4, value='Target:')
sample_coords_tgt_mode = widget_combobox( s0base4, value=tgt_coords, uname='coords-tgt-mode', /tracking, scr_xsize=origin_xsize*2, $
			Notify_Realize='OnRealize_scan_list_coords_tgt', $
			uvalue=tgt_help, /align_right)

s0base3 = widget_base( s0base, /row, xpad=1, ypad=0, space=2, /align_center, /base_align_center)
button = widget_button( s0base3, value='Scan Frame', uname='frame-button', tracking=tracking, $
			uvalue='Load the Scan List for the current active Scan Frame, for the current "site". If "Lock" is OFF, then sample frame details and the scan list for this frame will be updated periodically.')
frame_id_text = widget_text( s0base3, value=str_tidy(current_frame), uname='frame-text', /tracking, /editable, $
			uvalue='Enter a Sample Frame ID to load (if you are not "Master" or a scan is not active). Hit <return> to load it immediately.', scr_xsize=text_xsize)

; Table  -------------------------------------------------------------------------------------------

t = strarr(19,256)

table = Widget_Table( tbase, UNAME='scan-table', /all_events, value=t, Notify_Realize='OnRealize_scan_list_Table',  $
			X_SCROLL_SIZE=12, Y_SCROLL_SIZE=10, /RESIZEABLE_COLUMNS, alignment=2, tracking=tracking, $
			scr_xsize = help_xsize, $
			uvalue='Scan list table: Select row to edit by clicking on row label; click again to unselect it. A selected row is locked from update changes (Engage "Lock" to lock entire table). Click on "Active" cell to cycle run state ' + $
			'between "ON", "OFF", "START" and "STOP". On/Off enables/disables a scan; Start/Stop brackets a range to run.' )

tab_panel = widget_tab( tbase, location=0, /align_center, uname='tab-panel')

; Edit panel  --------------------------------------------------------------------------------------

edit_base = widget_base( tab_panel, title='    Append and Edit    ', /column, xpad=10, ypad=1, space=2, $
			/align_center, /base_align_center, scr_xsize=help_xsize-7)
bbase = widget_base( edit_base, /row, /base_align_center, ypad=0, xpad=0, space=15)

data_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
button = widget_button( data_base, value='Import', uname='import-button', tracking=tracking, $
					uvalue='Import a Scan List from a file.')
button = widget_button( data_base, value='Export', uname='export-button', tracking=tracking, $
					uvalue='Save the current scan list to a file.')

;k_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
;button = widget_button( k_base, value='Kget', uname='kvs-get-button', tracking=tracking, $
;					uvalue='Get the scan list for the current Frame # from the KVS.')
;button = widget_button( k_base, value='Kput', uname='commit', tracking=tracking, $
;					uvalue='Put the scan list in the KVS associated with the current Frame #.')

edit2_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
button = widget_button( edit2_base, value='Edit', uname='edit-button', tracking=tracking, $
					uvalue='Open Scan Edit window to edit the selected row in the list, or to append or insert new rows. Select a row to edit by clicking on row label. ' + $
					 'The selected row is locked from updates while selected. For more extensive changes, "Lock" the local list before making changes using the "Lock" checkbox (default when Scan Edit is opened).')
lock_check = cw_bgroup2( edit2_base, ['Lock'], /row, xpad=0, ypad=0, space=0, /return_index, /tracking, $
					uname='lock-check', set_value=[1], /nonexclusive, $
					uvalue=['Lock the local copy of the scan list for this Frame number while editing or re-ordering the scan list. ' + $
					'Unlocked you will see the current list for this Frame number updated regularly from the KVS. Note: The row selected for editing is always locked.'])
button = widget_button( edit2_base, value='Fill', uname='fill-button', tracking=tracking, $
					uvalue='Duplicate a value or values of cells down a range of selected rows in selected columns.')

arrows_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
arrow = picture_button( arrows_base, geopixe_root+'images/up-16x14.jpeg', uname='arrow-up', $
			/tracking, uvalue='Move selected scan list row(s) up one position. Select rows by clicking in a cell and dragging up or down. ' + $
			'"Lock" the local list copy before making changes.', /pushbutton_events)
arrow = picture_button( arrows_base, geopixe_root+'images/down-16x14.jpeg', uname='arrow-down', $
			/tracking, uvalue='Move selected scan list rows down one position. Select rows by clicking in a cell and dragging up or down. ' + $
			'"Lock" the local list copy before making changes.', /pushbutton_events)

del_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
button = widget_button( del_base, value='Delete', uname='delete-button', tracking=tracking, $
					uvalue='Delete the selected scan row(s) in the list. Click and drag down a column [except "Active" column] to select rows.')
button = widget_button( del_base, value='Clear', uname='clear-button', tracking=tracking, $
					uvalue='Clear the entire scan list local copy. KVS scan list remains until a "Commit".')

t3_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
label = widget_label( t3_base, value='Remaining time:')
time_text2 = widget_text( t3_base, value='', uname='time-text2', /tracking, editable=0, scr_xsize=text_xsize, $
	uvalue='Shows the total time remaining to execute the selected scan rows, estimated from pixel dwell time for enabled scans and total number of pixels for each. ')

commit_base = widget_base( bbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
button = widget_button( commit_base, value='Translate', uname='translate-button', tracking=tracking, $
			uvalue='Transform all Sample & Scan coordinates to "Target" Mapper coordinates based on the transformation established in "Scan Edit" for Frame Finder and Mapper References. ' + $
			'Both Finder and Mapper Reference points must be defined for Frame first (see "Scan Edit" window). ')
button = widget_button( commit_base, value='Commit', uname='commit', tracking=tracking, $
					uvalue='Commit the current changes to the scan list to the KVS for this Frame number. Use "Lock" to lock the local list copy before making extensive changes to the list. ' + $
					'List must be "translated" before Stage daemon will act on list.')

; Control panel  --------------------------------------------------------------------------------------

control_base = widget_base( tab_panel, title='    Scan Sequence Control    ', /column, xpad=10, ypad=1, space=2, $
				/align_center, /base_align_center, scr_xsize=help_xsize-7)
cbase = widget_base( control_base, /row, /base_align_center, ypad=0, xpad=0, space=14)

scan_base = widget_base( cbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
;label = widget_label( scan_base, value='Scan Sequence:')
start_button = widget_button( scan_base, value='Start', uname='start-button', tracking=tracking, $
					uvalue='"Start" the execution of the scan list, for rows tagged "ON" between rows labelled "START" and "STOP". ' + $
					'Left click "Active" column cell to toggle state between "ON", "OFF", "START" and "STOP". ' + $
					'Lock the list to make local "Active" changes. "Active" will be set to OFF after scan completion. ' + $
					'Note: Translate coordinates to local "Mapper" coordinates first.')
skip_button = widget_button( scan_base, value='Skip', uname='skip-button', tracking=tracking, $
					uvalue='"Skip" the execution of the current scan and continue with next in the list.')
stop_button = widget_button( scan_base, value='Stop', uname='stop-button', tracking=tracking, $
					uvalue='"Stop" the execution of the scan list.')
;label = widget_label( scan_base, value='  ')
master_check = cw_bgroup2( scan_base, ['Master'], /row, xpad=0, ypad=0, space=0, /return_index, /tracking, $
					uname='master-check', set_value=[0], /nonexclusive, $
					uvalue=['Scan-List has "Master" status and can control the scan sequence. Check to request "Master" status. Only the "Master" can start, stop or pause a scan.'])
append_button = widget_button( scan_base, value='Append', uname='append-button', tracking=tracking, $
					uvalue='"Append" to an incomplete first scan (as a new run starting after "stroke") and continue execution of the scan list, for rows tagged "ON" between rows labelled "START" and "STOP". ' + $
					'Left click "Active" column cell to toggle state between "ON", "OFF", "START" and "STOP". ' + $
					'Lock the list to make local "Active" changes. "Active" will be set to OFF after scan completion. ')
append_text = widget_text( scan_base, value='', uname='append-text', /tracking, editable=1, scr_xsize=text_xsize2, $
					uvalue='Shows the completed number of strokes for current/previous scan. "Append" will Start a scan after this stroke number. ')
label = widget_label( scan_base, value='  ')
pause_button = widget_button( scan_base, value='Pause', uname='pause-button', tracking=tracking, $
					uvalue='"Pause" a running scan at end-of-line.')
resume_button = widget_button( scan_base, value='Resume', uname='resume-button', tracking=tracking, $
					uvalue='"Resume" a scan that was "Paused".')

led2 = lonarr(7)
lc0base = widget_base( cbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
led2[0]= picture_button( lc0base, lnames, uname='led20', value=0, /tracking, uvalue='LED20: Scan List - "Green" locked, "Red" unlocked. ' + $
		'Best to "Lock" list ("Append/Edit" tab) before setting "Active" and Starting sequence. ' + $
		'"Locked" means the local copy of list is locked for editing or re-ordering. ' + $
		'Unlocked it will update regularly from KVS. Note: Row selected for editing is always locked.')
label = widget_label( lc0base, value='Lock', /tracking, uvalue='LED20: Scan List - "Green" locked, "Red" unlocked. ' + $
		'Best to "Lock" list ("Append/Edit" tab) before setting "Active" and Starting sequence. ' + $
		'"Locked" means the local copy of list is locked for editing or re-ordering. ' + $
		'Unlocked it will update regularly from KVS. Note: Row selected for editing is always locked.')

lc1base = widget_base( cbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
led2[1]= picture_button( lc1base, lnames, uname='led21', value=0, /tracking, uvalue='LED21: Scanning status - "Green" scanning, "Red" paused, "Off" stopped.')
label = widget_label( lc1base, value='Scan', /tracking, uvalue='LED21: Scanning status - "Green" scanning, "Red" paused, "Off" stopped')

lc2base = widget_base( cbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
led2[6]= picture_button( lc2base, lnames, uname='led26', value=0, /tracking, uvalue='LED26: Beam status - "Green" beam on (power good and shutter open), "Red" beam off (power low or shutter closed), "Off" source off.')
label = widget_label( lc2base, value='Beam', /tracking, uvalue='LED26: Beam status - "Green" beam on (power good and shutter open), "Red" beam off (power low or shutter closed), "Off" source off (or failed to connect).')

t2_base = widget_base( cbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
label = widget_label( t2_base, value='Remain: One:')
time_text3 = widget_text( t2_base, value='', uname='time-text3', /tracking, editable=0, scr_xsize=text_xsize, $
	uvalue='Shows the time remaining to execute the current scan row, estimated from pixel dwell time and total number of pixels remaining. ')
label = widget_label( t2_base, value='All:')
time_text = widget_text( t2_base, value='', uname='time-text', /tracking, editable=0, scr_xsize=text_xsize, $
	uvalue='Shows the total time remaining to execute the selected scan rows, estimated from pixel dwell time for enabled scans and total number of pixels for each. ')

; Manual Stage panel  --------------------------------------------------------------------------------------

stage_base = widget_base( tab_panel, title='    Manual Stage Control    ', /column, xpad=10, ypad=1, space=2, $
				/align_center, /base_align_center, scr_xsize=help_xsize-7)
stbase = widget_base( stage_base, /row, /base_align_center, ypad=0, xpad=0, space=15)

sx_base = widget_base( stbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
label = widget_label( sx_base, value='Pan  X:')
arrow = picture_button( sx_base, geopixe_root+'images/fast-left-24x14.jpeg', uname='stage-fast-left', $
	/tracking, uvalue='Jog stage fast left (-X).', /pushbutton_events)
arrow = picture_button( sx_base, geopixe_root+'images/left-16x14.jpeg', uname='stage-left', $
	/tracking, uvalue='Jog stage slow left (-X).', /pushbutton_events)
arrow = picture_button( sx_base, geopixe_root+'images/stop-16x14.jpeg', uname='stage-stop', $
	/tracking, uvalue='Stop stage jog.', /pushbutton_events)
arrow = picture_button( sx_base, geopixe_root+'images/right-16x14.jpeg', uname='stage-right', $
	/tracking, uvalue='Jog stage slow right (+X).', /pushbutton_events)
arrow = picture_button( sx_base, geopixe_root+'images/fast-right-24x14.jpeg', uname='stage-fast-right', $
	/tracking, uvalue='Jog stage fast right (+X).', /pushbutton_events)
	
sy_base = widget_base( stbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
label = widget_label( sy_base, value='Y:')
;arrow = picture_button( sy_base, geopixe_root+'images/fast-down-24x14.jpeg', uname='stage-fast-down', $
;	/tracking, uvalue='Jog stage fast down (-Y).', /pushbutton_events)
arrow = picture_button( sy_base, geopixe_root+'images/down-16x14.jpeg', uname='stage-down', $
	/tracking, uvalue='Jog stage slow down (-Y).', /pushbutton_events)
arrow = picture_button( sy_base, geopixe_root+'images/stop-16x14.jpeg', uname='stage-stop', $
	/tracking, uvalue='Stop stage jog.', /pushbutton_events)
arrow = picture_button( sy_base, geopixe_root+'images/up-16x14.jpeg', uname='stage-up', $
	/tracking, uvalue='Jog stage slow up (+Y).', /pushbutton_events)
;arrow = picture_button( sy_base, geopixe_root+'images/fast-up-24x14.jpeg', uname='stage-fast-up', $
;	/tracking, uvalue='Jog stage fast up (+Y).', /pushbutton_events)

sz_base = widget_base( stbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
label = widget_label( sz_base, value='Z:')
arrow = picture_button( sz_base, geopixe_root+'images/down-16x14.jpeg', uname='stage-in', $
	/tracking, uvalue='Jog stage slow in (-Z).', /pushbutton_events)
arrow = picture_button( sz_base, geopixe_root+'images/stop-16x14.jpeg', uname='stage-stop', $
	/tracking, uvalue='Stop stage jog.', /pushbutton_events)
arrow = picture_button( sz_base, geopixe_root+'images/up-16x14.jpeg', uname='stage-out', $
	/tracking, uvalue='Jog stage slow out (+Z).', /pushbutton_events)

sm_base = widget_base( stbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
label = widget_label( sm_base, value='Move: X:')
move_stage_x_text = widget_text( sm_base, value='', uname='stage-move-x-text', /tracking, /editable, scr_xsize=origin_xsize, $
	uvalue='Select desired stage X cooordinate (mm). X is horizontal scan axis (0 to 660 mm). Move to X,Y,Z on "Go".')
label = widget_label( sm_base, value=' Y:')
move_stage_y_text = widget_text( sm_base, value='', uname='stage-move-y-text', /tracking, /editable, scr_xsize=origin_xsize, $
	uvalue='Select desired stage y cooordinate (mm). Y is vertical stage axis (0 to 150 mm). Move to X,Y,Z on "Go".')
label = widget_label( sm_base, value=' Z:')
move_stage_z_text = widget_text( sm_base, value='', uname='stage-move-z-text', /tracking, /editable, scr_xsize=origin_xsize, $
	uvalue='Select desired stage Z cooordinate (mm). Z is beam axis (+ along beam axis, towards door) (-1 to +5 mm). Move to X,Y,Z on "Go".')

q_button = widget_button( sm_base, value='?', uname='query-button', tracking=tracking, $
					uvalue='Capture the current XYZ coordinates (from selected coordinate source).')
go_button = widget_button( sm_base, value='Go', uname='go-button', tracking=tracking, $
					uvalue='Go to the XYZ coordinates shown to the left.')
led2[3]= picture_button( sm_base, lnames, uname='led23', value=0, /tracking, uvalue='LED23: Manual Move - "Green" moving, "Red" locked, "off" stopped.')

; Preset Stage moves panel  --------------------------------------------------------------------------------------

preset_base = widget_base( tab_panel, title='    Preset Stage Moves    ', /column, xpad=10, ypad=1, space=2, $
				/align_center, /base_align_center, scr_xsize=help_xsize-7)
ptbase = widget_base( preset_base, /row, /base_align_center, ypad=0, xpad=0, space=15)

pm_base = widget_base( ptbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
home_button = widget_button( pm_base, value='Home', uname='home-button', tracking=tracking, $
					uvalue='Home all stage axes. Take care as the stage will move immediately.')
led2[2]= picture_button( pm_base, lnames, uname='led22', value=0, /tracking, uvalue='LED22: Scan List - "Green" Homed, "Red" Not-Homed, ' + $
		'"Off" connection down or homing.')

label = widget_label( pm_base, value='            ')
label = widget_label( pm_base, value='Preset Move:')

preset_mode = widget_combobox( pm_base, value='  '+preset_names, uname='preset-mode', /tracking, scr_xsize=2*origin_xsize, $
			Notify_Realize='OnRealize_scan_list_presets', $
			uvalue='Select a preset stage position. "sample change" will move the stage to the Sample Change position. Click on "Go" to move there.', /align_right)

;preset_stage_x_text = widget_text( pm_base, value='', uname='preset-move-x-text', /tracking, /editable, scr_xsize=origin_xsize, $
;	uvalue='Preset stage X cooordinate (mm). X is horizontal scan axis. Select preset using the droplist to the left. Move to X,Y,Z on "Go".')
;label = widget_label( pm_base, value=' Y:')
;preset_stage_y_text = widget_text( pm_base, value='', uname='preset-move-y-text', /tracking, /editable, scr_xsize=origin_xsize, $
;	uvalue='Preset stage y cooordinate (mm). Y is vertical stage axis. Select preset using the droplist to the left. Move to X,Y,Z on "Go".')
;label = widget_label( pm_base, value=' Z:')
;preset_stage_z_text = widget_text( pm_base, value='', uname='preset-move-z-text', /tracking, /editable, scr_xsize=origin_xsize, $
;	uvalue='Preset stage Z cooordinate (mm). Z is beam axis (+ towards door). Select preset using the droplist to the left. Move to X,Y,Z on "Go".')

go_preset_button = widget_button( pm_base, value='Go', uname='go-preset-button', tracking=tracking, $
					uvalue='Go to the XYZ coordinates of the selected "Preset Move".')
led2[4]= picture_button( pm_base, lnames, uname='led24', value=0, /tracking, uvalue='LED24: Preset Move - "Green" moving, "Red" locked, "off" stopped.')

; Safety shutter control panel  --------------------------------------------------------------------------------------

safety_base = widget_base( tab_panel, title='    Shutter Control    ', /column, xpad=10, ypad=1, space=2, $
				/align_center, /base_align_center, scr_xsize=help_xsize-7)
stbase = widget_base( safety_base, /row, /base_align_center, ypad=0, xpad=0, space=15)

sm_base = widget_base( stbase, /row, /base_align_center, xpad = 0, ypad=0, space=2)
led2[5]= picture_button( sm_base, lnames, uname='led25', value=0, /tracking, uvalue='LED25: Shutter - "Green" closed, "Red" open, "off" daemon issue.')

label = widget_label( sm_base, value='   Shutter for end-station "'+shutter_name[(*pm).endstation]+'":')

shutter_open_button = widget_button( sm_base, value='Open', uname='shutter-open-button', tracking=tracking, $
					uvalue='Open the shutter, if safety PLC allows it, for this end-station.')
button = widget_button( sm_base, value='Close', uname='shutter-close-button', tracking=tracking, $
					uvalue='Close the shutter for this end-station.')


; Help strip  --------------------------------------------------------------------------------------

hbase = widget_base( tbase, /row, /base_align_center, xpad=0, ypad=0, space=2)

led = lonarr(10)
led_base = widget_base( hbase, /column, xpad=1, ypad=0, space=led_space, /align_center, /base_align_left, scr_xsize=led_xsize)

lr0base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[0]= picture_button( lr0base, lnames, uname='led0', value=0, /tracking, uvalue='LED0: KVS operational status. "Green" operational, "Red" failed to connect, "Off" disabled.')
label = widget_label( lr0base, value='KVS', /tracking, uvalue='LED0: KVS operational status. "Green" operational, "Red" failed to connect, "Off" disabled.')

lr1base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[1] = picture_button( lr1base, lnames, uname='led1', value=0, /tracking, uvalue='LED1: Stage VSUB server active status. "Green" operational, "Red" failed to connect, "Off" disabled.')
label = widget_label( lr1base, value='Stage', /tracking, uvalue='LED1: Stage VSUB server active status. "Green" operational, "Red" failed to connect, "Off" disabled.')

lr2base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[2] = picture_button( lr2base, lnames, uname='led2', value=0, /tracking, uvalue='LED2: Safety VSUB server active status. "Green" operational, "Red" failed to connect, "Off" disabled.')
label = widget_label( lr2base, value='Safety', /tracking, uvalue='LED2: Safety VSUB server active status. "Green" operational, "Red" failed to connect, "Off" disabled.')

lr3base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[3] = picture_button( lr3base, lnames, uname='led3', value=0, /tracking, uvalue='LED3: Maia/Kandinski active status. "Green" operational, "Red" low bias, poor T or bad interlock or failed to connect, "Off" disabled.')
label = widget_label( lr3base, value='Maia', /tracking, uvalue='LED3: Maia/Kandinski active status. "Green" operational, "Red" low bias, poor T or bad interlock or failed to connect (refer to Maia Control), "Off" disabled.')

led2_base = widget_base( hbase, /column, xpad=1, ypad=0, space=led_space, /align_center, /base_align_left, scr_xsize=led_xsize2)

lr4base = widget_base( led2_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[4]= picture_button( lr4base, lnames, uname='led4', value=0, /tracking, uvalue='LED4: List locked status. "Green" locked, "Red" not locked.')
label = widget_label( lr4base, value='Locked', /tracking, uvalue='LED4: List locked status. "Green" locked, "Red" not locked. ' + $
					'"Locked" means the local copy of the scan list is locked for editing or re-ordering. ' + $
					'Unlocked it will update regularly from the KVS. Note: Row selected for editing is always locked.')

lr5base = widget_base( led2_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[5] = picture_button( lr5base, lnames, uname='led5', value=0, /tracking, uvalue='LED5: Stage scanning status. "Green" scanning, "Red" paused, "Off" stopped.')
label = widget_label( lr5base, value='Scanning', /tracking, uvalue='LED5: Stage scanning status. "Green" scanning, "Red" paused, "Off" stopped.')

lr6base = widget_base( led2_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[6] = picture_button( lr6base, lnames, uname='led6', value=0, /tracking, uvalue='LED6: Stage home status. "Green" homed, "Red" not homed, "Off" disconnected or homing.')
label = widget_label( lr6base, value='Homed', /tracking, uvalue='LED6: Stage home status. "Green" homed, "Red" not homed, "Off" disconnected or homing.')

lr7base = widget_base( led2_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[7] = picture_button( lr7base, lnames, uname='led7', value=0, /tracking, uvalue='LED7: Blog connected status. "Green" Maia successfully connected to blog, "Red" Maia failed to connect to blog, "Off" disabled.')
label = widget_label( lr7base, value='Blog', /tracking, uvalue='LED7: Blog connected status. "Green" Maia successfully connected to blog, "Red" Maia failed to connect to blog, "Off" disabled.')

led3_base = widget_base( hbase, /column, xpad=1, ypad=0, space=led_space, /align_center, /base_align_left, scr_xsize=led_xsize3)

lr8base = widget_base( led3_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[8]= picture_button( lr8base, lnames, uname='led8', value=0, /tracking, uvalue='LED8: Excillum VSUB server active status. "Green" operational, "Red" failed to connect, "Off" disabled.')
label = widget_label( lr8base, value='Source', /tracking, uvalue='LED8: Excillum VSUB server active status. "Green" operational, "Red" failed to connect, "Off" disabled. ')

lr9base = widget_base( led3_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[9] = picture_button( lr9base, lnames, uname='led9', value=0, /tracking, uvalue='LED9: Excillum Power, shutter and Maia ready status. "Green" at full power, shutter open and Maia ready; "Red" power is low or off, shutter closed or Maia not ready; "Off" failed to connect.')
label = widget_label( lr9base, value='Ready', /tracking, uvalue='LED9: Excillum Power, shutter and Maia ready status. "Green" at full power, shutter open and Maia ready; "Red" power is low or off, shutter closed or Maia not ready; "Off" failed to connect.')

power_check = cw_bgroup2( led3_base, ['Ignore'], /row, xpad=0, ypad=0, space=0, /return_index, /tracking, $
					uname='power-check', set_value=[0], /nonexclusive, $
					uvalue=['Check box to ignore "Ready" state and continue scan sequence regardless. ' + $
					'Normally, leave the box unchecked so that source "Ready" state can be used to Pause or Resume a scan. ' + $
					'Use "Ignore" to be able to test the stage raster while the source or Maia are off, for example.'])

help = widget_text( hbase, scr_xsize=help_xsize-led_xsize-led_xsize2-led_xsize3-update_xsize-18, ysize=help_lines, /wrap, uname='help', tracking=tracking, $
				uvalue='Help window. Displays context-sensitive information and tips about widgets.', $
				frame=0)
update = widget_text( hbase, scr_xsize=update_xsize, ysize=help_lines, /wrap, uname='update', tracking=tracking, $
				uvalue='Update window. Displays position updates for currently selected "Target" endstation.', $
				frame=0)

state = { $
		path:			ptr_new(path), $		; pointer to current path
		plist:			plist, $				; pointer to array of pointers to scan detail structs
		pindex:			ptr_new(index), $		; pointer to display index
		pframe:			ptr_new(/alloc), $		; pointer to frame details
		psample:		ptr_new(/alloc), $		; pointer to sample details
		ps:				ps, $					; pointer to Kandinski port struct
		pm:				pm, $					; pointer to DAQ parameters struct
		pselect:		ptr_new(/allocate_heap), $	; pointer to select for notify to scan_edit
		pcoords:		ptr_new(/allocate_heap), $	; pointer to coords for notify to scan_edit
		lock:			1, $					; locks the KVS list for this Frame number while editing
		local_list:		local_list, $			; flags local responsibility for plist heap
		local_pm:		local_pm, $				; flags local responsibility for pm heap
		prefs:			prefs, $				; geopixe defaults

		comms:			comms, $				; comms object instance, for KVS
		comms2:			comms2, $				; comms 2 object instance, for Safety VSUB
		comms3:			comms3, $				; comms 3 object instance, for Stage VSUB
		comms4:			comms4, $				; comms 4 object instance, for Excillum VSUB
		local_id:		local_id, $				; Local process ID (node:user-seq')
		kvs:			kvs, $					; key-value store Python object
		retry_count:	0L, $					; counts re-opens of KVS
		last_put: 		systime(1), $			; last PUT of scan-sequence to KVS
		ztap:			use_ztap, $				; Ztap in use
		kvs_prefix:		kvs_prefix, $			; KVS key prefix (e.g. 'MM.Mel.')
		config_key:		config_key, $			; Config KVS key (e.g. 'MM.Per.SL.2.config')
		state_key:		state_key, $			; Scan saequence state KVS key (e.g. 'MM.Per.SL.2.state.hash')
		kvs_endpoint:	kvs_endpoint, $			; KVS endpoint
		clayton_wrong_left_right: clayton_wrong_left_right, $	; flags wrong end-stations (1=Left, 2=Right) only in Clayton
		shutter_name:	shutter_name, $			; names of shutters indexed to endstation
		site_name:		site, $					; site name
		site:			isite, $				; site mode (0=Mel, 1=Per)
		master:			0, $					; flags "Master" mode
		vsub_stage:		vsub_stage, $			; VSUB object for Stage daemon, returns like KVS.
		vsub_safety:	vsub_safety, $			; VSUB object for Safety daemon, returns like KVS.
		vsub_excillum:	vsub_excillum, $		; VSUB object for Excillum daemon, returns like KVS.
		stage_endpoints: stage_endpoints, $		; stage endpoints
		safety_endpoints: safety_endpoints, $	; safety endpoints
		excillum_endpoints: excillum_endpoints, $	; excillum endpoints
		mmcl_stage:		mmcl_stage, $			; MMCL for stage REQ
		mmcl_safety:	mmcl_safety, $			; MMCL for safety REQ
		mmcl_excillum:	mmcl_excillum, $		; MMCL for excillum REQ

		scan_sequence: scan_sequence, $			; scan saequence details from KVS
		changed:		0, $					; have changed scan_sequence, but not 'put' yet.
		last_index: 	1.0d+0, $				; last Calib at Index systime(1)

		phase:	{ $								; phase pointers used for timer execution
			pstart:		ptr_new(0), $			; execution phase for "start"
			pnext:		ptr_new(0)}, $			; execution phase for "index position"

		power_ignore:	0, $					; "power" ignore check box state
		first_push_state: 	1.0d+0, $			; first Push to Excillum state 'on' systime(1)
		last_push_state: 	1.0d+0, $			; last Push to Excillum state 'on' systime(1)
		last_recalibrate: 	1.0d+0, $			; last Excillum Recalibrate systime(1)

		frame:			current_frame, $		; frame #
		coords_src:		0, $					; coords source (0=Finder, 1=Mapper 1, 2=Mapper 2, 3=GeoPIXE 1, 4=GeoPIXE 2)
		coords_tgt:		((*pm).endstation-1)>0, $	; coords tgt (0=Mapper 1, 1=Mapper 2)
		src_coords_list:	src_coords_list, $	; list of src coord labels
		tgt_coords_list:	tgt_coords_list, $	; list of tgt coord labels
		presets:		ptr_new(presets), $		; preset move names
		preset_mode:	0, $					; selected preset move
		
;		limits: { min: { x:	0.0, $				; min X position
;						 y:	0.0, $				; min Y
;						 z: 1.0}, $				; min Z
;				max: {	 x:	700.0, $			; max X position
;						 y:	150.0, $			; max Y
;						 z: -5.0}, $			; max Z
;			velocity: {  x:	10.0, $				; max X velocity
;						 y:	10.0, $				; max Y
;						 z: 10.0}, $			; max Z
;		acceleration: {	 x:	10.0, $				; max X acceleration
;						 y:	10.0, $				; max Y
;						 z: 10.0}}, $			; max Z
		tracking:		tracking, $				; is tracking enabled
		debug:			debug, $				; debug flag
		test:			test, $					; test mode flag
		row_height:		0L, $					; table row height
		xoffset:		0L, $					; X offset for resize
		yoffset:		0L, $					; Y offset for resize
;		shapes:			shapes, $				; shapes list
		active:			active, $				; activation modes for items in list
		enable_interlace:	enable_interlace, $ ; enable interlace > 1

		timer_coords:	1.2, $					; timer for coords update from KVS 
		timer_position:	1.3, $					; timer for position update from stage VSUB
		timer_move:		1.4, $					; timer for move status
		timer_sequence:	2.5, $					; timer for scan sequence monitoring (must be longer than coords, position)
		timer_skip:		2.0, $					; timer for phase execution of start, next, etc.
		timer_shutter:	3.0, $					; timer for shutter status
		timer_frame:	5.0, $					; timer for update of frame lists from KVS
		timer_home:		10.0, $					; timer for home status

		alarm_maia_popup: 0L, $					; TLB for an alarm popup for Maia discard while Raster on
		alarm_stage_popup: 0L, $				; TLB for an alarm popup for frozen stage
		alarm_excillum_popup: 0L, $				; TLB for an alarm popup for bad Excillum state
		led:			intarr(n_elements(led)), $	; LED state array main window
		led2:			intarr(n_elements(led2)), $	; LED state array sequence panel

		table:			table, $				; table ID
		rows:			256, $					; number of rows
		columns:		19, $					; number of colums
		headings:		ptr_new(), $			; pointer to column headings
		sel: {left:-1, top:-1, right:-1, bottom:-1 }, $	; use "(*pstate).sel.top" as current region
		cr_found:		0, $					; to fight MAC IDL bug
		lock_check:		lock_check, $			; Lock edit check box ID
		time_text:		time_text, $			; total time text ID
		time_text2:		time_text2, $			; total time text2 ID
		time_text3:		time_text3, $			; Single time text3 ID
		edit_base:		edit_base, $			; edit base ID
		control_base:	control_base, $			; control base ID
		stage_base:		stage_base, $			; stage base ID
		frame_id_text: 	frame_id_text, $		; frame # ID
		sample_coords_src_mode: sample_coords_src_mode, $	; src coords droplist ID
		sample_coords_tgt_mode: sample_coords_tgt_mode, $	; tgt coords droplist ID
		site_mode:		site_mode, $			; site droplist ID
		preset_mode_id:	preset_mode, $			; presets droplist ID
		move_stage_x_text: move_stage_x_text, $	; stage X move ID
		move_stage_y_text: move_stage_y_text, $	; stage Y move ID
		move_stage_z_text: move_stage_z_text, $	; stage Z move ID
		led_id:			led, $					; LED image IDs main window
		led2_id:		led2, $					; LED image IDs sequence panel
		start_button:	start_button, $			; start button ID
		master_check:	master_check, $			; master mode checkbox ID
;		preset_stage_x_text: preset_stage_x_text, $	; stage X preset move ID
;		preset_stage_y_text: preset_stage_y_text, $	; stage Y preset move ID
;		preset_stage_z_text: preset_stage_z_text, $	; stage Z preset move ID
		home_button:	home_button, $			; home button ID
		q_button:		q_button, $				; q button ID
		go_button:		go_button, $			; go button ID
		skip_button:	skip_button, $			; skip button ID
		shutter_open_button: shutter_open_button, $	; shutter open button
		
		pause_button:	pause_button, $			; pause/resume button ID
		append_text:	append_text, $			; stroke number for an "Append" scan
		update:			update, $				; update pane ID
		help:			help $					; ID of help text
	}

*state.pselect = state.sel
pstate = ptr_new(state, /no_copy)

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=pstate
widget_control, tlb, /realize

register_notify, tlb, ['path', $				; new path
					'scan-list-update', $		; update list display
					'scan-sequence-next', $		; step to next in scan list sequence
					'scan-done' $				; scan finished message
					], from=group

xmanager, 'scan_list', tlb, /no_block

; Seem to need to do this from longer to shorter times (IDL 8.8) ...
; ... or at least AFTER the xmanager call?

if notimer eq 0 then begin
	widget_control, home_button, timer = (*pstate).timer_home
	widget_control, frame_id_text, timer = (*pstate).timer_frame
	widget_control, shutter_open_button, timer = (*pstate).timer_shutter
	widget_control, start_button, timer = (*pstate).timer_sequence
	widget_control, skip_button, timer = (*pstate).timer_skip
	widget_control, go_button, timer = (*pstate).timer_move
	widget_control, q_button, timer = (*pstate).timer_position
	widget_control, sample_coords_src_mode, timer = (*pstate).timer_coords

;	widget_control, move_stage_x_text, timer = 0.2
;	widget_control, move_stage_y_text, timer = 0.3
endif

return
end
