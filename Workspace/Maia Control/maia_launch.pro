pro maia_launch_event, event

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
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
		warning,'maia_launch_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return				; goto, kill
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
pm = (*pstate).pmaia
pr = (*pstate).preadout
ps = (*pstate).psocket
play = (*pstate).playout
pimage = (*pstate).pimage
pp_et = (*pstate).ppspec3
pp_group = (*pstate).ppspec
obj = (*pstate).DevObj

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
			(*pstate).tracking_counter = 4						; suppress detector parameters for a while
		endif else begin
;			s = ((*pm).status.blog_error eq '') ? '' : 'Blog error returned: ' + (*pm).status.blog_error
;			widget_control, (*pstate).help, set_value=['Maia detector launch panel: "Setup" Maia, display data and control detector.', '',s]

			s = ((*pm).status.blog_error eq '') ? 'Maia detector launch panel: "Setup" Maia, display data and control detector.' : 'Blog error returned: ' + (*pm).status.blog_error
			widget_control, (*pstate).help, set_value=[s]
		endelse
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
			'image-crop': begin
				if ptr_valid( event.pointer) then begin
					x = (*event.pointer).x
					y = (*event.pointer).y
					s = ['Box size X=' + str_tidy(x[1]-x[0]+1) + ' Y=' + str_tidy(y[1]-y[0]+1), $
						'Origin X=' + str_tidy(x[0]) + ' Y=' + str_tidy(y[0])]
					widget_control, (*pstate).help, set_value=s
				endif
				goto, finish
				end
			'chart-display-cleared': begin						; only returned from Chart display
				pc = (*pstate).pchart
				(*pc).n  = 0
				(*pc).init = 0
				(*(*pc).p[0]).n = 0
				goto, finish
				end
			'groups-display-cleared': begin
				maia_launch_clear, pstate, event, /groups
				end
			'spectrum-display-cleared': begin
				maia_launch_clear, pstate, event, /spectra
				end
			'spectrum-cal': begin								; from Maia-Setup when Cals sent to Hymod
				notify,'spectrum-display', from=event.top		; redraw Spectra display
				notify,'spectrum-changed', from=event.top		; Spectra-select update
				notify,'groups-display', from=event.top			; redraw Group spectra display
				end
			'spectrum-get-cals': begin							; "Get cals" from spectrum-display
				maia_launch_read_cal, ps, pm, play				; first read from Maia
				maia_launch_update_cal, pp_et, pp_group, pimage, pm, play
				notify,'spectrum-display', from=event.top		; redraw Spectra display
				notify,'spectrum-changed', from=event.top		; Spectra-select update
				notify,'groups-display', from=event.top			; redraw Group spectra display
				end
			'warn-setup': begin
				cols = [0,2,3]
				widget_control, (*pstate).maia_setup_button, set_value={select:cols[*event.pointer]}
				end
			else:
		endcase
		end
	'WIDGET_TIMER': begin
		u = widget_info( event.id, /uname)
		case u of
			'setup-maia-button': begin
				if (*pm).status.blog_error ne '' then widget_control, (*pstate).help, set_value='Blog error returned: ' + (*pm).status.blog_error

				maia_update_parameters, pstate, update=update, error=error
				if update then begin
					*(*pstate).psetup = *(*pstate).pHYMOD_debug 
					notify, 'maia-display', (*pstate).psetup, from=event.top
					notify, 'maia-process', from=event.top
					widget_control, (*pstate).maia_setup_button, set_value={select:hymod_warning_colour( *(*pstate).pHYMOD_debug)}
				endif
				
				s = ['Detector bias = '+str_tidy((*pm).control.bias_monitor, length=5)+' V', $
					'Leakage current = '+str_tidy((*pm).control.leakage, length=5)+' uA', $
					'Detector Temp = '+str_tidy((*pm).control.temp.detector, length=5)+' C']
				if (*pstate).tracking_counter eq 0 then widget_control, (*pstate).status, set_value=s
				(*pstate).tracking_counter = ((*pstate).tracking_counter - 1) > 0
				
				if (*pm).status.blog_error ne '' then begin
					s = 'Blog error returned: ' + (*pm).status.blog_error
					log_message, (*pstate).comms, type='ERROR', 'maia_launch events, "Setup" Maia: '+ s
					widget_control, (*pstate).help, set_value=['Maia detector launch panel: "Setup" Maia, display data and control detector.','', s]
				endif

				if maia_launch_overtemp( pm, mess=mess) and (widget_info( (*pstate).alarm_temp_popup, /valid) eq 0) then begin
					alarm_popup, group=event.top, tlb=atlb, title='Maia: Alarm Condition', message=mess
					(*pstate).alarm_temp_popup = atlb
				endif
				if maia_launch_overleak( pm, mess=mess) and (widget_info( (*pstate).alarm_leakage_popup, /valid) eq 0) then begin
					alarm_popup, group=event.top, tlb=atlb, title='Maia: Alarm Condition', message=mess
					(*pstate).alarm_leakage_popup = atlb
				endif
				if maia_launch_overloss( pm, mess=mess) and (widget_info( (*pstate).alarm_loss_popup, /valid) eq 0) then begin
					alarm_popup, group=event.top, tlb=atlb, title='Maia: Alarm Condition', message=mess
					(*pstate).alarm_loss_popup = atlb
				endif
				if maia_launch_overdisk( pm, mess=mess) and (widget_info( (*pstate).alarm_disk_popup, /valid) eq 0) then begin
					alarm_popup, group=event.top, tlb=atlb, title='Maia: Alarm Condition', message=mess
					(*pstate).alarm_disk_popup = atlb
				endif

				for i=0L,5 do begin
					widget_control, (*pstate).led_id[i], set_value=(*pstate).led[i]
				endfor

				(*pstate).heart[0] = 2 - (*pstate).heart[0]
				widget_control, (*pstate).heart_id[0], set_value=(*pstate).heart[0]
				widget_control, event.id, timer=(*pstate).time_maia	
				end
			'rates-button': begin
				maia_update_parameters2, pstate, update=update, error=error		; these are part of 'maia-rates' notify update
				if update then notify, 'maia-rates', from=event.top
				s1 = 'Run:  ' + str_tidy((*pm).run.number) + '.' + str_tidy((*pm).run.segment) 
				s1 = s1 + '  axes: ' + str_tidy(round((*pm).run.X)) + ', ' + str_tidy(round((*pm).run.Y))+ ', ' + str_tidy(round((*pm).run.Z))
				s2 = 'Project: ' + (*pm).run.project + ', Group: ' + (*pm).run.Group + '  (free: ' + str_tidy( (*pm).control.status.fs_free/1.0e+12) + ' TB)'
				widget_control, (*pstate).run_stats, set_value=[s1,'Rate:   '+rates_string((*pm).run.rate,/bytes),s2]

				case (*pm).run.discard of
					0: widget_control, (*pstate).run_button, set_value={value:"Endrun", select:1}
					1: widget_control, (*pstate).run_button, set_value={value:"Newrun", select:3}
				endcase

				if maia_launch_interlock( pm, ps, downtime=s) and (widget_info( (*pstate).alarm_interlock_popup, /valid) eq 0) then begin
					alarm_popup, group=event.top, tlb=atlb, title='Maia: Alarm Condition', message='Interlock Failed ('+s+' UTC)'
					(*pstate).alarm_interlock_popup = atlb
				endif
				if maia_launch_zerorate( pm, mess=mess) and (widget_info( (*pstate).alarm_rate_popup, /valid) eq 0) then begin
					alarm_popup, group=event.top, tlb=atlb, title='Maia: Alarm Condition', message=mess
					(*pstate).alarm_rate_popup = atlb
				endif

				if n_elements(*(*pm).IC.plist) eq 0 then begin
					*(*pm).IC.plist = 'none'
				endif else begin
					if size(*(*pm).IC.plist, /tname) ne 'STRING' then begin
						*(*pm).IC.plist = 'none'
					endif
				endelse
				if (*pm).IC.pv.name eq '' then (*pm).IC.pv.name = (*(*pm).IC.plist)[0]
				
;				Hard to detect (*pm).run.discard=1 as it may last for a short time.
;				Instead look for a change in run number for the clear. However, (*pm).run.discard=0 will
;				last longer and can be detected for the save.

				if ((*pm).run.number ne (*pm).run.last_run) or  $								; start new run or
						((*pm).run.last_discard eq 0) and ((*pm).run.discard eq 1) then begin	; run ended
					run = (*pm).run.number
					if ((*pm).run.number ne (*pm).run.last_run) then run = (*pm).run.last_run
					
					if (*pm).DA.save and ((*pm).DA.save_path ne '') then begin
						maia_launch_read_da, ps, pm, pimage, /scan, /deadtime, /init		; update image dwell
						maia_launch_read_da_rGamma, ps, pm									; needed here cos not in 'maia_launch_read_da' now
						maia_launch_read_enable, ps, pm, pr, pimage, play					; update image *pactive
						maia_launch_update_image_cal, pimage, pm, play						; update image *pcal, cal
						q = where( *(*pimage).el eq 'Flux', nq)
						if nq gt 0 then begin
							raw_flux = (*(*pimage).image)[*,*,q[0]]
							image_correct_flux, pimage, (*(*pimage).image)[*,*,q[0]], (*pm).IC, *(*pm).IC.plist, /flatten, raw=raw_flux
						endif

						F = (*pm).DA.save_path + str_tidy(run) + '-RT.dai'
						(*pimage).file = F
						(*pimage).source = (*pm).run.group + slash() + str_tidy(run) + '.0'
						(*pimage).source2 = (*pm).run.group + slash() + str_tidy(run) + '.' + str_tidy((*pm).run.segment)
						(*pimage).has_preview = 0
						if ptr_valid((*pimage).preview) then ptr_free, (*pimage).preview
						obj->set_options, deadtime_cal=(*pm).deadtime.cal

						print,'Maia_launch: save RT image: '+F[0]
						write_geopixe_image, pimage, F, /no_null, /check_bounds
						notify, 'image-display', from=event.top
						
;						Clear items set in 'image_correct_flux' after 'write_geopixe_image' ...						
						(*pimage).has_flux = 0
						if ptr_valid((*pimage).flux) then ptr_free, (*pimage).flux
						if ptr_valid((*pimage).raw_flux) then ptr_free, (*pimage).raw_flux
						(*pimage).charge = 0.0
						(*pimage).temp.valid = 0
						log_message, (*pstate).comms, type='INFO', 'maia_launch: RT image saved to = '+F
					endif
				endif

				if ((*pm).run.number ne (*pm).run.last_run)	then begin						; start new run
					(*pimage).flatten = 0
					maia_launch_clear, pstate, event
					maia_launch_read_da, ps, pm, pimage, /scan, /deadtime, /init
					log_message, (*pstate).comms, type='INFO', 'maia_launch: Clear image for a new run ='+str_tidy((*pm).run.number)
				endif

				(*pm).run.last_run = (*pm).run.number
				(*pm).run.last_discard = (*pm).run.discard

				(*pstate).heart[1] = 2 - (*pstate).heart[1]
				widget_control, (*pstate).heart_id[1], set_value=(*pstate).heart[1]
				widget_control, event.id, timer=(*pstate).time_maia2
				end
			'new-chart-button': begin
				maia_update_parameters3, pstate, update=update, error=error
				if (*pm).chart.on then begin
					maia_launch_update_chart, pstate, error=error
				endif else begin
					update=0  &  error=0
				endelse
;				print, 'Maia update times: ',(*pstate).time_maia,(*pstate).time_maia2,(*pstate).time_maia3
;				print, 'Groups update times: ',(*pstate).time_spectra,(*pstate).time_spectra_update
;				print, 'Spectra update times: ',(*pstate).time_ET_spectra,(*pstate).time_ET_spectra_update
;				print, 'DA update times: ',(*pstate).time_da,(*pstate).time_da_update
				if (error eq 0) then begin
					if update then notify, 'chart-display', from=event.top
					widget_control, event.id, timer=(*pstate).time_maia3
				endif

				(*pstate).heart[2] = 2 - (*pstate).heart[2]
				widget_control, (*pstate).heart_id[2], set_value=(*pstate).heart[2]
				end
			'new-spectra-button': begin
				if (*(*(*pstate).pshrmem_ET_spectra).ppar)[2] eq 0 then begin
					maia_update_ET_spectra, pstate, update=update, error=error
					if (*pstate).enable_activity eq 0 then notify, 'maia-rates', from=event.top
					if update then begin
;						print,'time_et: ',(*pstate).time_et_spectra,(*pstate).time_et_spectra_update  
						p = (*pstate).pimage2D
						set_image_minmax, p, (*p).image, (*p).options, /xonly
						notify, 'spectrum-display', from=event.top
						notify, 'et2d-display', from=event.top
					endif
				endif

				(*pstate).heart[3] = 2 - (*pstate).heart[3]
				widget_control, (*pstate).heart_id[3], set_value=(*pstate).heart[3]
				widget_control, event.id, timer=(*pstate).time_ET_spectra	
				end
			'new-groups-button': begin
				if (*pm).number.spectra eq 0 then goto, finish
				if (*(*(*pstate).pshrmem_spectra).ppar)[2] eq 0 then begin
					maia_update_group_spectra, pstate, update=update, error=error
					if update then begin
						notify, 'groups-display', from=event.top
					endif
				endif

				(*pstate).heart[4] = 2 - (*pstate).heart[4]
				widget_control, (*pstate).heart_id[4], set_value=(*pstate).heart[4]
				widget_control, event.id, timer=(*pstate).time_spectra	
				end
			'new-images-button': begin
				if (*(*(*pstate).pshrmem_da).ppar)[2] eq 0 then begin
					maia_update_da2, pstate, update=update, error=error
					if update then begin
						p = (*pstate).pimage
						set_image_minmax, p, (*p).image, (*p).options, /xonly
						notify, 'image-display', from=event.top
					endif
				endif

				(*pstate).heart[5] = 2 - (*pstate).heart[5]
				widget_control, (*pstate).heart_id[5], set_value=(*pstate).heart[5]
				widget_control, event.id, timer=(*pstate).time_da	
				end

;			Log read timers
;	NOTE:	These work under latest IDL, where event loops run independently, and so they can execute a
;			wait read, which only stalls that one event loop, leaving others running OK.
;
;;			'heart0': begin										; Parameters log
;				log_client, unit=mlun[0], plog=plogm[0]
;				widget_control, event.id, timer=(*pstate).log_timer
;				end
			'heart1': begin										; Activity log
;				log_client, unit=blun[0], plog=plogb[0]
;				widget_control, event.id, timer=(*pstate).log_timer
				end
;			'heart2': begin										; Parameters slow log
;				log_client, unit=mlun[1], plog=plogm[1]
;				widget_control, event.id, timer=(*pstate).log_timer
;				end
			'heart3': begin										; ET log
;				log_client, unit=blun[1], plog=plogb[1]
;				widget_control, event.id, timer=(*pstate).log_timer
				end
			'heart4': begin										; Groups log
;				log_client, unit=blun[4], plog=plogb[4]
;				widget_control, event.id, timer=(*pstate).log_timer
				end
			'heart5': begin										; DA log
;				log_client, unit=blun[2], plog=plogb[2]
;				widget_control, event.id, timer=(*pstate).log_timer
				end
		
			'query': begin										; Epics log
;				log_client, unit=blun[3], plog=plogb[3]
;				widget_control, event.id, timer=(*pstate).log_timer
				end
			else:
		endcase
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request Maia-Launch ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'maia-launch-tlb': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				case !version.os_family of
					'MacOS': begin
						xh = (event.x - 580) > 240
						xs = xh
						end
					'unix': begin
						xh = (event.x - 580) > 240
						xs = xh
						end
					else: begin
						xh = (event.x - 585) > 240
						xs = xh
						end
				endcase
				widget_control, (*pstate).run_stats, scr_xsize=xs
				widget_control, (*pstate).help, scr_xsize=xh
				end
			else:
		endcase
		end

	'setup-maia-button': begin
		maia_setup, group_leader=event.top, tlb=tlb, data=play, maia=pm, port=ps, debug=(*pstate).debug, $
			disable=(*pstate).pdisable, readout=pr, path=*(*pstate).path, pshrmem_da=(*pstate).pshrmem_da, $
			pimage = (*pstate).pimage, ppspec=(*pstate).ppspec3, ppgroup=(*pstate).ppspec, default=(*pstate).default
		register_notify, event.top, $
								['path', $					; new path
								'warn-setup', $				; changed warnings on Maia setup window
								'update-elk', $				; update Rate ELK map
								'image-elements', $			; new element list for images
								'spectrum-cal' $			; transmit ET/Group spectra cal
								], from=tlb
		end
		
	'rates-button': begin
		maia_rates, group_leader=event.top, tlb=tlb, layout=play, maia=pm, $
			disable=(*pstate).pdisable, data=(*pstate).prates, mdata=(*pstate).pmrates, path=*(*pstate).path, /tracking
		register_notify, event.top, $
								['path', $					; new path
								'detector-select', $		; detector selection on rates mimic display
								'detector-toggle', $		; detector right-click toggle on rates mimic display
								'warn-setup' $				; changed warnings on Maia setup window
								], from=tlb
		end
		
	'blogd-button': begin
		restore, geopixe_root+'blog_browser.sav'
		blog_browse, group=event.top, /realtime, ip=(*pstate).blog.ip, port=(*pstate).blog.port, $
							version=(*pstate).version, dt=(*pm).deadtime.cal.a
		end
		
	'reset-button': begin
;		socket_retry, ps, error=error
;		(*ps).attempts = 1

		if (*pm).number.spectra ge 1 then begin
			(*(*pstate).pshrmem_spectra).loop = 0
			(*(*(*pstate).pshrmem_spectra).ppar)[2] = 1		; send reset to blog client
		endif
		print,'Set spectra blog client reset flag ...'
		(*(*pstate).pshrmem_ET_spectra).loop = 0
		(*(*(*pstate).pshrmem_ET_spectra).ppar)[2] = 1		; send reset to blog client
		print,'Set ET_spectra blog client reset flag ...'
		(*(*pstate).pshrmem_da).loop = 0
		(*(*(*pstate).pshrmem_da).ppar)[2] = 1				; send reset to blog client
		print,'Set DA blog client reset flag ...'
		(*(*pstate).pshrmem_activity).loop = 0
		(*(*(*pstate).pshrmem_activity).ppar)[2] = 1		; send reset to blog client
		print,'Set activity blog client reset flag ...'
		
		(*pr).quad_enable[*] = 1
		(*pr).RR_enable[*] = 1
		(*pr).event = 1
		print,'Set Maia enables ...'
		socket_command_set, ps, 'enable', (*pr).event, class='event'
		socket_command_set, ps, 'enable', 1, class='photon'
		socket_command_set, ps, 'enable', (*pr).quad_enable, class='readout.quad', chip=-1, n_chips=4
		socket_command_set, ps, 'enable', (*pr).RR_enable, class='readout.clock', chip=-1, n_chips=3
		socket_command_set, ps, 'enable', 1, class='deadtime'
		socket_command_set, ps, 'enable', 1, class='pixel'
		socket_command_set, ps, 'rewrite', 1, class='ddm'

		n_chips = (*pm).n_detectors/32
		
		maia_launch_initial, ps, pm, play, pr, pimage
;		*(*pstate).psetup = *(*pstate).pHYMOD_debug 
;		notify, 'maia-display', (*pstate).psetup, from=event.top
		end
		
	'new-spectra-button': begin
		i = launch_free_activity( (*pstate).activity.display.ET_spectra) 
		p = (*(*pstate).ppspec3)[0]
		emax = (*p).cal.poly[0] + ((*p).size-1) * (*p).cal.poly[1]
		title = 'Maia E Spectra Display'
		if i ge 0 then begin
			spectrum_display, group_leader=event.top, tlb=tlb, path=*(*pstate).path, dpath=*(*pstate).dpath, ehigh=4*emax, $
						spectrum=*(*pstate).ppspec3, title=title, /realtime, layout=(*pstate).layout_file, $
						ppercent=(*pstate).activity.display.ET_spectra[i].ppercent, update_notify='spectrum-display', $
						/highlight
			(*pstate).activity.display.ET_spectra[i].TLB = TLB
		endif else begin
			spectrum_display, group_leader=event.top, tlb=tlb, path=*(*pstate).path, dpath=*(*pstate).dpath, layout=(*pstate).layout_file, $
						spectrum=*(*pstate).ppspec3, title=title, /realtime, update_notify='spectrum-display', $
						ehigh=4*emax, /highlight
		endelse
		register_notify, event.top, $
								['path', $						; new path
								'spectrum-get-cals', $			; transmit ET/Group spectra cal
								'select-highlight', $			; highlight spectrum #
								'spectrum-display-cleared' $	; changed (cleared) chart
;								'warn-setup' $					; changed warnings on Maia setup window
								], from=tlb

		i = launch_free_activity( (*pstate).activity.display.ET_spectraT) 
		p = (*(*pstate).ppspec4)[0]
		emax = (*p).cal.poly[0] + ((*p).size-1) * (*p).cal.poly[1]
		title = 'Maia T Spectra Display'
		if i ge 0 then begin
			spectrum_display, group_leader=event.top, tlb=tlb, path=*(*pstate).path, dpath=*(*pstate).dpath, ehigh=4*emax, $
						spectrum=*(*pstate).ppspec4, title=title, /realtime, layout=(*pstate).layout_file, $
						ppercent=(*pstate).activity.display.ET_spectraT[i].ppercent, update_notify='spectrum-display', $
						/highlight, /offsety
			(*pstate).activity.display.ET_spectraT[i].TLB = TLB
		endif else begin
			spectrum_display, group_leader=event.top, tlb=tlb, path=*(*pstate).path, dpath=*(*pstate).dpath, layout=(*pstate).layout_file, $
						spectrum=*(*pstate).ppspec4, title=title, /realtime, update_notify='spectrum-display', $
						ehigh=4*emax, /highlight
		endelse
		register_notify, event.top, $
								['path', $						; new path
								'select-highlight' $			; highlight spectrum #
;								'warn-setup' $					; changed warnings on Maia setup window
								], from=tlb
		end
		
	'new-groups-button': begin
		if (*pm).number.spectra eq 0 then goto, finish
		i = launch_free_activity( (*pstate).activity.display.spectra) 
		p = (*(*pstate).ppspec)[0]
		emax = (*p).cal.poly[0] + ((*p).size-1) * (*p).cal.poly[1]
		if i ge 0 then begin
			spectrum_display, group_leader=event.top, tlb=tlb, path=*(*pstate).path, dpath=*(*pstate).dpath, $
						spectrum=*(*pstate).ppspec, title='Maia Group Spectra Display', /realtime, ehigh=4*emax, $
						ppercent=(*pstate).activity.display.spectra[i].ppercent, update_notify='groups-display'
			(*pstate).activity.display.spectra[i].TLB = TLB
		endif else begin
			spectrum_display, group_leader=event.top, tlb=tlb, path=*(*pstate).path, dpath=*(*pstate).dpath, ehigh=4*emax, $
						spectrum=*(*pstate).ppspec, title='Maia Group Spectra Display', /realtime, update_notify='groups-display'
		endelse
		register_notify, event.top, $
								['path', $					; new path
								'spectrum-get-cals', $			; transmit ET/Group spectra cal
								'group-display-cleared' $	; changed (cleared) chart
;								'warn-setup' $				; changed warnings on Maia setup window
								], from=tlb
		end
		
	'new-chart-button': begin
		pc = (*pstate).pchart
		ppspec = (*pstate).pchart_spec
		if (n_elements(*ppspec) gt 0) and ((*pc).n ge 2) then begin
			spectrum_display, group_leader=event.top, tlb=tlb, path=*(*pstate).path, dpath=*(*pstate).dpath, $
						spectrum=*(*pstate).pchart_spec, title='Maia Chart Display', /realtime, /chart, $
						ehigh=5000, /show_negatives, /nosav, /no_sum, /no_query, update_notify='chart-display'
			register_notify, event.top, $
								['path', $						; new path
								'chart-display-cleared' $		; changed (cleared) chart
								], from=tlb
		endif
		(*pm).chart.on = 1
		end
		
	'new-et2d-button': begin
		i = launch_free_activity( (*pstate).activity.display.ET2d) 
		title = 'Maia ET 2D Display'
		if i ge 0 then begin
			ET2d_image, group_leader=event.top, tlb=tlb, pimage=(*pstate).pimage2D, pm=pm, $
						title=title, ppercent=(*pstate).activity.display.et2d[i].ppercent
			(*pstate).activity.display.et2d[i].TLB = TLB
		endif else begin
			ET2d_image, group_leader=event.top, tlb=tlb, pimage=(*pstate).pimage2D, pm=pm
		endelse
		register_notify, event.top, $
								['path' $					; new path
;								'warn-setup' $				; changed warnings on Maia setup window
								], from=tlb
		end
		
	'new-images-button': begin
		i = launch_free_activity( (*pstate).activity.display.image) 
		; copy the element list ((*pm).DA.name) to ( *(*pstate).pimage).el) first here ...
		if i ge 0 then begin
			gimage, group_leader=event.top, tlb=tlb, path=*(*pstate).path, dpath=*(*pstate).dpath, $
					/realtime, pimage=(*pstate).pimage, ppercent=(*pstate).activity.display.image[i].ppercent, $
					version=(*pstate).version
			(*pstate).activity.display.image[i].TLB = TLB
		endif else begin
			gimage, group_leader=event.top, tlb=tlb, path=*(*pstate).path, dpath=*(*pstate).dpath, $
					/realtime, pimage=(*pstate).pimage
		endelse
		register_notify, event.top, $
								['path' $					; new path
;								'warn-setup' $				; changed warnings on Maia setup window
								], from=tlb
		end
		
	'new-multi-button': begin
		i = launch_free_activity( (*pstate).activity.display.multi) 
		if i ge 0 then begin
			multi_image, group_leader=event.top, tlb=tlb, pimage=(*pstate).pimage, $
								ppercent=(*pstate).activity.display.multi[i].ppercent
			(*pstate).activity.display.multi[i].TLB = TLB
		endif else begin
			multi_image, group_leader=event.top, tlb=tlb, pimage=(*pstate).pimage
		endelse
		register_notify, event.top, $
								['image-crop' $				; image cropping on image #0
								], from=tlb
		end
		
	'start-button': begin									; not used now, move code to "reset"
		(*pr).quad_enable[*] = 1
		(*pr).RR_enable[*] = 1
		(*pr).event = 1
		print,'Set Maia enables ...'
		socket_command_set, ps, 'enable', (*pr).event, class='event'
		socket_command_set, ps, 'enable', 1, class='photon'
		socket_command_set, ps, 'enable', (*pr).quad_enable, class='readout.quad', chip=-1, n_chips=4
		socket_command_set, ps, 'enable', (*pr).RR_enable, class='readout.clock', chip=-1, n_chips=3
;		socket_command_set, ps, 'enable', 1, class='deadtime'
;		socket_command_set, ps, 'enable', 1, class='deadtime_pp'
		socket_command_set, ps, 'enable', 1, class='pixel'
		socket_command_set, ps, 'enable', 1, class='accum'
		socket_command_set, ps, 'enable', 1, class='activity'

		if ((*pm).n_detectors eq 384) or ((*pm).version.scepter ge 7) then begin
			print,'Set Maia SCEPTER enable ...'
			socket_command_set, ps, 'enable', 1, class='scepter'		; SPI overhead, do not use with old 96!
		endif
		n_chips = (*pm).n_detectors/32
		
		maia_launch_initial, ps, pm, play, pr, pimage
;		*(*pstate).psetup = *(*pstate).pHYMOD_debug 
;		notify, 'maia-display', (*pstate).psetup, from=event.top
		end
		
	'stop-button': begin
;		(*pr).quad_enable = 0
;		(*pr).event = 0
;		socket_command_set, ps, 'enable', (*pr).quad_enable, class='readout.quad', chip=[0,1,2,3], n_chips=4
;		socket_command_set, ps, 'enable', (*pr).event, class='event'
		socket_command_set, ps, 'endrun', 1, class='blog'
;		*(*pstate).psetup = *(*pstate).pHYMOD_debug 
;		notify, 'maia-display', (*pstate).psetup, from=event.top
		end
		
	'run-button': begin
		case (*pm).run.discard of 
			1: begin						; not running so start
				socket_command_set, ps, 'newrun', 1, class='blog'
				end
			0: begin						; running so stop
				socket_command_set, ps, 'endrun', 1, class='blog'
				end
		endcase
		end
		
	'clear-button': begin
		maia_launch_clear, pstate, event
		end
		
	'control-stats': begin
		case event.index of
			2: begin
				if (*pstate).enable_project_select then begin
					tproject = socket_command_get( ps, 'project.next', class='blog', error=err)
					if err eq 0 then (*pm).run.project = tproject
					tgroup = socket_command_get( ps, 'group.next', class='blog', error=err)
					if err eq 0 then (*pm).run.group = tgroup
					text = ['"Project" for next run','"Group" for next run']
					initial_text = [(*pm).run.project, (*pm).run.group]
					help_text = ['"Project" directory tree for blog data storage from next run on.', $
								'"Group" sub-directory for blog data storage from next run on.']
					Help_default = 'Change "Project" or "Group" dirs for blog data. Leave either or both blank to not make a change.'
				endif else begin
					tgroup = socket_command_get( ps, 'group.next', class='blog', error=err)
					if err eq 0 then (*pm).run.group = tgroup
					text = ['"Group" for next run']
					initial_text = [(*pm).run.group]
					help_text = ['"Group" sub-directory for blog data storage from next run on.']
					Help_default = 'Change "Group" dir for blog data. Leave blank to not make a change.'
				endelse

				r = options_popup( title='Blog Storage Options', text=text, initial_text=initial_text, help_text=help_text, $
							help_default=help_default,	min_xsize=200, error=error)
				if error then return

				if (*pstate).enable_project_select then begin
					tproject = r.text[0]				; "Project" directory tree for blog data storage
					tgroup = r.text[1]					; "Group" directory tree for blog data storage
					if (tproject ne '') then begin
						(*pm).run.project = tproject
						socket_command_set, ps, 'project.next', (*pm).run.project, class='blog'
					endif
				endif else begin
					tgroup = r.text[0]					; "Group" directory tree for blog data storage
				endelse
				if tgroup ne '' then begin
					(*pm).run.group = tgroup
					socket_command_set, ps, 'group.next', (*pm).run.group, class='blog'
				endif
				end
			else:
		endcase
		end
	else:
endcase

finish:
	return

done:
	goto, kill

bad_state:
	warning,'maia_launch_event',['STATE variable has become ill-defined.','Abort Batch Sort.'],/error
	goto, kill

kill:
	ErrorNo = 0
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		goto, die
	endif

	log_message, (*pstate).comms, type='INFO', 'maia_launch, Signal clients to die ...'
	print,'Signal clients to die ...'
	; Note that Epics client uses the DA shared memory to receive this ...
	(*(*(*pstate).pshrmem_activity).ppar)[3] = 1			; kill Activity blog_client
	(*(*(*pstate).pshrmem_ET_spectra).ppar)[3] = 1			; kill ET spectra blog_client
	(*(*(*pstate).pshrmem_da).ppar)[3] = 1					; kill DA blog_client
	if (*pm).number.spectra ge 1 then begin
		(*(*(*pstate).pshrmem_spectra).ppar)[3] = 1			; kill Group spectra blog_client
	endif
	; Note that "maia_client_parameters_slow" uses the same shared memory as "maia_client_parameters"
	(*(*(*pstate).pshrmem_pars).ppar)[3] = 1				; kill parameters maia_client
	wait, 2
	
	print,'Kill clients ...'
	for i=0,n_elements((*pstate).blog_client_obj)-1 do begin
		obj_destroy, (*pstate).blog_client_obj[i]
	endfor
	for i=0,n_elements((*pstate).maia_client_obj)-1 do begin
		obj_destroy, (*pstate).maia_client_obj[i]
	endfor
	
	widget_control, hourglass=1
	wait, 3

	log_message, (*pstate).comms, type='INFO', 'maia_launch, close windows and exit.'
	widget_control, event.top, /destroy
	widget_control, hourglass=0

	print,'Close socket ...'
	close_file, (*ps).unit
	if ptr_valid( ps) then ptr_free, ps
	
	if ptr_valid( (*pstate).pshrmem_activity) then ptr_free, (*pstate).pshrmem_activity
	if ptr_valid( (*pstate).pshrmem_spectra) then ptr_free, (*pstate).pshrmem_spectra
	if ptr_valid( (*pstate).pshrmem_ET_spectra) then ptr_free, (*pstate).pshrmem_ET_spectra
	if ptr_valid( (*pstate).pshrmem_ET_spectraT) then ptr_free, (*pstate).pshrmem_ET_spectraT
	if ptr_valid( (*pstate).pshrmem_ET2d) then ptr_free, (*pstate).pshrmem_ET2d
	if ptr_valid( (*pstate).pshrmem_da) then ptr_free, (*pstate).pshrmem_da
	if ptr_valid( (*pstate).pshrmem_pars) then ptr_free, (*pstate).pshrmem_pars
	
	print,'Cleanup Maia Launch ...'
	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).dpath) then ptr_free, (*pstate).dpath
	if ptr_valid( play) then ptr_free, play
	if ptr_valid( pm) then ptr_free, pm
	if ptr_valid( (*pstate).pdisable) then ptr_free, (*pstate).pdisable
	if ptr_valid( play) then ptr_free, play
	if ptr_valid( (*pstate).pseed) then ptr_free, (*pstate).pseed
	if ptr_valid( (*pstate).prates) then ptr_free, (*pstate).prates
	free_spectra, (*pstate).ppspec
	free_spectra, (*pstate).ppspec3
	free_spectra, (*pstate).ppspec4
	free_spectra, (*pstate).pchart_spec
	free_images, (*pstate).pimage
	free_images, (*pstate).pimage2D
	
;	... need to recursively free (*pstate).pchart memory ...
;	... need to free (*pstate).activity ptr array data ...
;	
;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-start.

;	shared_memory_unmap, prefix=(*pstate).prefix_et2_spectra, n_buffers=(*(*pstate).pshrmem_ET_spectra).n_buffers
;	shared_memory_unmap, prefix=(*pstate).prefix_spectra, n_buffers=(*(*pstate).pshrmem_spectra).n_buffers
;	shared_memory_unmap, prefix=(*pstate).prefix_da, n_buffers=(*(*pstate).pshrmem_da).n_buffers

die:
;	heap_gc
	return
end

;--------------------------------------------------------------------------

pro maia_launch_clear, pstate, event, groups=groups, spectra=spectra, images=images

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
		warning,'maia_launch_clear',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(images) lt 1 then images=0
	if n_elements(spectra) lt 1 then spectra=0
	if n_elements(groups) lt 1 then groups=0
	if (images eq 0) and (spectra eq 0) and (groups eq 0) then begin
		images = 1
		spectra = 1
		groups = 1
	endif
	pimage = (*pstate).pimage
	pm  = (*pstate).pmaia

	(*(*pstate).pmrates).detectors[*] = 0.					; clear maximum detector rates

	if images then begin
		pshrmem = (*pstate).pshrmem_da
		if (*pshrmem).error eq 0 then begin	
			(*(*pshrmem).ppar)[2] = 1						; reset DA images blog_client
		endif												; which clears image, pars, counters

		(*pimage).processed = 0
		(*pimage).valid = 0
		(*pimage).bad_xy = 0
		(*pimage).clipped = 0
		(*pimage).bounds.xmin = 0L
		(*pimage).bounds.ymin = 0L
		(*pimage).bounds.xmax = 0L
		(*pimage).bounds.ymax = 0L
		(*pimage).charge = 0.
		(*pimage).temp.total_flux = 0.						; total flux
		(*pimage).temp.total_pixels = 0						; total pixels used
		(*pimage).temp.valid = 0
		notify, 'image-clear-all-marks', from=event.top
		notify, 'image-display', from=event.top
	endif

	if spectra then begin		
		pshrmem = (*pstate).pshrmem_ET_spectra		
		pshrmem4 = (*pstate).pshrmem_ET_spectraT		
		pshrmem2 = (*pstate).pshrmem_ET2d		
		if (*pshrmem).error eq 0 then begin	
			(*pstate).activity.buffers.ET_spectra = 0.		; clear lost buffers			
			(*pstate).rate_last = 0							; clear previous sum used for rates (only from ET data)
			pf = (*pshrmem).pfloat[0]						; clear lost buffers
			(*pf)[1] = 0.									; in shared memory
			pd = (*pshrmem).pdat[0]							; clear E spectra 2D buffer
			(*pd)[*] = 0.0									; in shared memory
			pd4 = (*pshrmem4).pdat[0]						; clear T spectra 2D buffer
			(*pd4)[*] = 0.0									; in shared memory
			notify, 'spectrum-display', from=event.top
			pd2 = (*pshrmem2).pdat[0]						; clear ET2D image buffer
			(*pd2)[*] = 0.0									; in shared memory
			notify, 'et2d-display', from=event.top
			(*(*pshrmem).ppar)[2] = 1						; reset E spectra blog_client
		endif
	endif

	if groups and ((*pm).number.spectra gt 0) then begin
		for i=0L,n_elements(*(*pstate).ppspec)-1 do begin	; clear Group spectra in shared
			(*(*(*(*pstate).ppspec)[i]).data)[*] = 0.0		; memory, which is pointed to by
		endfor												; individual spectra structs
		pshrmem = (*pstate).pshrmem_spectra
		if (*pshrmem).error eq 0 then begin	
			pf = (*pshrmem).pfloat[0]						; clear lost buffers
			(*pf)[1] = 0.									; in shared memory
			(*pf)[2] = 0.									; clear 'duration' time
			(*(*pshrmem).ppar)[2] = 1						; reset Group spectra blog_client
		endif
		notify, 'groups-display', from=event.top
	endif
	return
end

;--------------------------------------------------------------------------

pro maia_launch_hardware_reset, ps, pm

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
		warning,'maia_launch_hardware_reset',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if (*pm).n_detectors eq 96 then begin
;		print,'Set Maia 96 specific parameters ...'
;		if (*pm).version.dam le 2 then begin
;			print,' ... set redirect for older Maia 96 ...'
;			socket_command_set, ps, 'redirect', '4B'x, class='internal.ddm'
;		endif
;		socket_command_set, ps, 'pd', 1.4, class='buffer.offs'
;		socket_command_set, ps, 'td', 1.0, class='buffer.offs'
	endif
	if (*pm).n_detectors eq 384 then begin
;		print,'Set Maia 384 specific parameters ...'
;		socket_command_set, ps, 'pd', 1.5, class='buffer.offs'
;		socket_command_set, ps, 'td', 1.0, class='buffer.offs'
	endif
	
	if ((*pm).version.scepter ge 6) then begin
;		print,'Set Maia SCEPTER 6 parameters ...'
;		socket_command_set, ps, 'tri1', 1, class='scepter', chip=-1, n_chips=n_chips
;		socket_command_set, ps, 'tri2', 1, class='scepter', chip=-1, n_chips=n_chips
;		socket_command_set, ps, 'tria', 0, class='scepter', chip=-1, n_chips=n_chips
	endif
	if ((*pm).version.scepter ge 7) then begin
;		print,'Set Maia SCEPTER 7 parameters ...'
	endif
	
	socket_command_set, ps, 'bias.voltage.rate', (*pm).control.bias_rate, class='detector'
	socket_command_set, ps, 'peltier.current.rate', 0.005, class='detector'
	return
end

;--------------------------------------------------------------------------

pro maia_launch_init_chart, pstate

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
		warning,'maia_launch_init_chart',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

pc = (*pstate).pchart
ppspec = (*pstate).pchart_spec
file = (*pc).file
if n_elements(*ppspec) ge 1 then return
if (*pc).n lt 1 then return

	nv = (*(*pc).p[0]).n
	nc = 32000
	if nv lt 1 then return
	j = (*pc).n-1
	
	pspec = ptrarr(nv)
	for i=0L,nv-1 do begin
		spec = define(/spectrum)
		spec.data = ptr_new( fltarr(nc))
		spec.size = nc
		spec.array = 0
		spec.detector = 7
		spec.station = 0
		spec.channel = i
		spec.cal.order = 1
		spec.cal.units = 'sec'
		spec.cal.poly[0:1] = [0.0,(*pstate).time_maia3]	
		spec.comment = 'Maia on-line strip chart'
		spec.source = 'Maia'
		spec.DevObj = (*pstate).DevObj
		str = replace(' ','_',(*(*(*pc).p[j]).p[i]).name)
		add_n = 0
		if ((*(*(*pc).p[j]).p[i]).index ne 0) then add_n=1
		if i lt nv-1 then begin
			if ((*(*(*pc).p[j]).p[i]).index eq 0) and ((*(*(*pc).p[j]).p[i+1]).index ne 0) then add_n=1
		endif
		if add_n then begin
			sn = str_tidy((*(*(*pc).p[j]).p[i]).index)
			str = str + '[' + sn + ']'
		endif
		spec.label = 'Maia parameter #' + str_tidy(i) + ' /'+str
		pspec[i] = ptr_new(spec, /no_copy)
	endfor
	*ppspec = pspec
return
end

;--------------------------------------------------------------------------

pro maia_launch_initial, ps, pm, play, pr, pimage, default, error=error

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
		warning,'maia_launch_initial',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_null_image_1, max_image_cal
if n_elements(max_image_cal) lt 1 then max_image_cal = 8

;	Set-up 'scratch' variables in Kandisnki from 'info' strings.

@maia_scratch.def

if (*ps).open eq 1 then begin
	socket_command_set, ps, 'key', 'Maia:cal.info', class='scratch.datum', chip=scratch_cal
	socket_command_set, ps, 'key', 'Maia:DA.info', class='scratch.datum', chip=scratch_DA
	socket_command_set, ps, 'key', 'Maia:ROI.info', class='scratch.datum', chip=scratch_ROI
	socket_command_set, ps, 'key', 'Maia:gaintrim.info', class='scratch.datum', chip=scratch_gaintrim
	socket_command_set, ps, 'key', 'Maia:group.info', class='scratch.datum', chip=scratch_group
	socket_command_set, ps, 'key', 'Maia:linearise.info', class='scratch.datum', chip=scratch_linearise
	socket_command_set, ps, 'key', 'Maia:pileup.info', class='scratch.datum', chip=scratch_pileup
	socket_command_set, ps, 'key', 'Maia:throttle.info', class='scratch.datum', chip=scratch_throttle
	socket_command_set, ps, 'key', 'Maia:deadtime.info', class='scratch.datum', chip=scratch_deadtime
	socket_command_set, ps, 'key', 'Maia:ROI.rate', class='scratch.datum', chip=scratch_ROI_rate

	maia_launch_hardware_reset, ps, pm

;	socket_command_set, ps, 'enable', 1, class='photon'
;	socket_command_set, ps, 'enable', 1, class='event'
;	
;	socket_command_set, ps, 'enable', 1, class='pixel'

;	socket_command_set, ps, 'enable', 1, class='event.fake'
;	socket_command_set, ps, 'et.rate', 1000, class='event.fake'
;	socket_command_set, ps, 'pa.rate', 2, class='event.fake'

;	Timers

	socket_command_set, ps, 'period', 0.5, class='timer', chip=0, n_chips=(*pm).number.timer
	socket_command_set, ps, 'enable', 1, class='timer', chip=0, n_chips=(*pm).number.timer

	socket_command_set, ps, 'period', 2.0, class='timer', chip=1, n_chips=(*pm).number.timer
	socket_command_set, ps, 'enable', 1, class='timer', chip=1, n_chips=(*pm).number.timer

;	Set-up Activity accumulator

	gc = intarr((*pm).n_detectors)		; Activity group set to "All"
	gc[*] = 1
	socket_command_set, ps, 'group', gc, channel=12, n_channels=16, class='group.det', chip=-1, n_chips=(*pm).n_detectors

	g = intarr(16)
	g[*] = 0							; ET events
	g[13] = 1
	socket_command_set, ps, 'select.group', g, channel=-1, n_channels=16, class='event'

	g[*] = 0							; Activity (ET 2D handled via ET events now)
	g[12] = 1							; set to monitor ALL groups here (and all detectors via group 12 above)
	socket_command_set, ps, 'select.group', g, channel=-1, n_channels=16, class='activity'
	socket_command_set, ps, 'trigger.rocont', 'timer0', class='activity'
	socket_command_set, ps, 'enable', 1, class='activity'

	g[*] = 0							; dead time
	g[15] = 1
	socket_command_set, ps, 'select.group', g, channel=-1, n_channels=16, class='deadtime'
	socket_command_set, ps, 'trigger.rocont', 'timer1', class='deadtime'
	socket_command_set, ps, 'enable', 1, class='deadtime'

;	socket_command_set, ps, 'select.group', g, channel=-1, n_channels=16, class='deadtime_pp'
;	socket_command_set, ps, 'enable', 1, class='deadtime_pp'

;	Set-up DA accumulator

	g[*] = 0							; DA
	g[14] = 1
	socket_command_set, ps, 'select.group', g, channel=-1, n_channels=16, class='da'
	socket_command_set, ps, 'trigger.rocont', 'patrans', class='da'
	socket_command_set, ps, 'trigger.start', 'paentry', class='da'
	socket_command_set, ps, 'trigger.rostop', 'paexit', class='da'
;	socket_command_set, ps, 'enable', 0, class='da'

;	Set-up Group Spectra accumulators

	if (*pm).number.spectra gt 0 then begin
		for i=0L,(*pm).number.spectra-1 do begin
			g[*] = 0					; spectra
			g[i] = 1
			socket_command_set, ps, 'select.group', g, channel=-1, n_channels=16, class='spectrum', chip=i
			socket_command_set, ps, 'trigger.rocont', 'timer1', class='spectrum', chip=i
			socket_command_set, ps, 'enable', 0, class='spectrum', chip=i
		endfor
	endif
	socket_command_set, ps, 'enable', 1, class='accum'

	if ((*pm).n_detectors eq 384) or ((*pm).version.scepter ge 7) then begin
		print,'Set Maia SCEPTER enable ...'
		socket_command_set, ps, 'enable', 1, class='scepter'		; SPI overhead, do not use with old 96!
	endif

;	Enable the various metadata related variables to be logged ...

	socket_command_set, ps, 'blog.enable', 1, class='info'
	socket_command_set, ps, 'blog.enable', 1, class='metadata'
	socket_command_set, ps, 'blog.enable', 1, class='variable'
	socket_command_set, ps, 'blog.enable', 1, class='scan'
	
	; Read back Kandinski values ...
	
	maia_launch_read_enable, ps, pm, pr, pimage, play
	maia_launch_read_groups, ps, pm, play
	maia_launch_read_cal, ps, pm, play
	maia_launch_read_gaintrim, ps, pm, play
	maia_launch_read_da, ps, pm, pimage, /init
	maia_launch_read_throttle, ps, pm
endif
return
end

;--------------------------------------------------------------------------

function maia_launch_interlock, pm, ps, downtime=down

; Check interlock failure value. Also, if the interlock has recoverd very recently,
; then the downtime is recent, and this is detected as well. Otherwise, we may miss a short
; (less than 0.5 s) recent drop-out.
; Ignore if downtime is still zero, or within 10s of Maia startup.

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
		warning,'maia_launch_interlock',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	bad = 0
	down = 'n/a'
	if ((*pm).control.interlock eq 0) and ((*pm).control.status.link eq 1) then bad=1

	if bad then begin
		v  = socket_command_get( ps, 'bpinterlock.downtime', class='status', /double, error=err)
		if err eq 0 then begin
			(*pm).control.status.bpinterlock_downtime = v[0]
		endif
	endif

	if  (*pm).control.status.bpinterlock_downtime gt 1. then begin
		if ((systime(1) - (*pm).control.status.bpinterlock_downtime) lt 10.) and $
			((systime(1) - (*pm).control.status.bpinterlock_downtime) gt 0.) and $
			(((*pm).control.status.bpinterlock_downtime - (*pm).control.status.main_uptime) gt 10.) then bad=1

			down = date_from_utc( (*pm).control.status.bpinterlock_downtime)
	endif
	return, bad
end

;--------------------------------------------------------------------------

function maia_launch_overdisk, pm, mess=mess

;	If file-system size is non-zero, then test fs_time (time in hours remaining).
;	Ignore '(*pm).control.status.fs_free' for now.

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
		warning,'maia_launch_overdisk',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	over = 0
	mess = ''
	if (*pm).run.discard then return, 0

	if (*pm).control.status.fs_size ne 0 then begin
;		if (*pm).control.status.fs_time gt 0 then begin
;			if (*pm).control.status.fs_time lt 100 then begin
;				over=1
;				mess = 'Storage below 100 hours.'
;			endif
;		endif
		if (*pm).control.status.fs_free lt 0.3 * 1.0e+12 then begin
			over = 1
			mess = 'Storage below 0.3 TB.'
		endif
		if (*pm).control.status.fs_free lt 0.1 * 1.0e+12 then begin
			over = 1
			mess = 'Storage critically low, below 0.1 TB!'
		endif
	endif

	return, over
end

;--------------------------------------------------------------------------

function maia_launch_overleak, pm, mess=mess

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
		warning,'maia_launch_overleak',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	over = 0
	mess = ''
	if (*pm).control.leakage gt 20. then begin
		over = 1
		mess = 'Detector leakage above 20 uA'
	endif

	return, over
end

;--------------------------------------------------------------------------

function maia_launch_overloss, pm, mess=mess

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
		warning,'maia_launch_overloss',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_maia_overloss, last_discard_rate, last_link_erate
if n_elements(last_discard_rate) eq 0 then last_discard_rate=0L
if n_elements(last_link_erate) eq 0 then last_link_erate=0L

	over = 0
	mess = ''
	if (*pm).control.status.discard_rate gt 1000. then begin
		over = 1
		mess = 'High discard rate'
		s = '(run= '+str_tidy((*pm).run.number)+', segment= '+str_tidy((*pm).run.segment)+': rate= ' + str_tidy((*pm).control.status.discard_rate) + '/s)'
		if ((*pm).control.status.discard_rate ne last_discard_rate) then begin
			print, 'maia_launch_overloss: ' + systime() + ' - ' + mess 
			print, '	' + s
		endif
		mess = mess + ' ' + s
		last_discard_rate = (*pm).control.status.discard_rate
	endif
	ter = total( (*pm).control.status.link_erate)
	if ter gt 1000. then begin
		over = 1
		mess = 'High link error rate'
		s = '(run= '+str_tidy((*pm).run.number)+', segment= '+str_tidy((*pm).run.segment)+': rate= ' + str_tidy(ter) + '/s)'
		if ((*pm).control.status.link_erate ne last_link_erate) then begin
			print, 'maia_launch_overloss: ' + systime() + ' - ' + mess 
			print, '	' + s
		endif
		mess = mess + ' ' + s
		last_link_erate = ter
	endif

	return, over
end

;--------------------------------------------------------------------------

function maia_launch_zerorate, pm, mess=mess

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
		warning,'maia_launch_zerorate',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	over = 0
	mess = ''
	if (*pm).run.rate lt 1. then begin
		over = 1
		mess = 'Zero Blog data rate'
	endif

	return, over
end

;--------------------------------------------------------------------------

function maia_launch_overtemp, pm, mess=mess

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
		warning,'maia_launch_overtemp',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	over = 0
	mess = ''
	if (*pm).control.temp.detector gt 60. then begin
		over = 1
		mess = 'Detector temperature above 60 C!'
	endif
	if (*pm).control.temp.water gt 40. then begin
		over = 1
		mess = 'Water temperature above 40 C!'
	endif
;	if (*pm).control.temp.hermes gt 40. then over=1			; some Maia's do not have this, so use 'water'
	if (*pm).control.temp.mosfet gt 65. then begin
		over = 1
		mess = 'MOSFET temperature above 65 C!'
	endif

	if (*pm).control.temp.hymod_FPGA gt 75. then begin
		over = 1
		mess = 'Hymod FPGA temperature above 75 C!'
	endif
	if (*pm).control.temp.hymod_CPU gt 75. then begin
		over = 1
		mess = 'Hymod CPU temperature above 75 C!'
	endif
	if (*pm).control.temp.FPGA gt 75. then begin
		over = 1
		mess = 'DAM FPGA temperature above 75 C!'
	endif

	return, over
end

;--------------------------------------------------------------------------

pro maia_launch_read_cal, ps, pm, play

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
		warning,'maia_launch_read_cal',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

; Cal coeffs. Gain-trim coeffs; also read in maia_update_parameters3 ...
;
; Detector array:
;	CSV table index is used for elements of the detector array, such as in the
;	'select' vector. To convert this to detector number, use the lookup table:
;
;	(*play).data.index						CSV table index to detector number
;	(*play).ref								detector number to CSV table index
	
	index = (*play).data.index
	v  = socket_command_get( ps, 'ecoeff', channel=0, class='cal.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
	if err eq 0 then (*pm).channel.cal.b = v[index]
	v  = socket_command_get( ps, 'ecoeff', channel=1, class='cal.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
	if err eq 0 then (*pm).channel.cal.a = v[index]
return
end

;--------------------------------------------------------------------------

pro maia_launch_read_da, ps, pm, pimage, scan=scan, deadtime=deadtime, init=init, pshrmem=pshrmem

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
		warning,'maia_launch_read_da',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(scan) lt 1 then scan=0
if n_elements(deadtime) lt 1 then deadtime=0
if n_elements(init) lt 1 then init=0
if n_elements(pshrmem) eq 0 then pshrmem=0L

n_detectors = (*ps).n_detectors
n_chips = (*ps).n_detectors/32
obj = (*pm).DevObj

maia_launch_read_da_info, ps, pm, pimage, pshrmem=pshrmem

; Move this to chart update 'maia_update_parameters3' because it's large ...	
; maia_launch_read_da_rGamma, ps, pm

	if ptr_good(pshrmem) then begin
		pdat = (*pshrmem).pdat

		(*pm).DA.on = (*pdat).DA.on
		(*pm).DA.N = (*pdat).DA.N
		(*pm).DA.scale = (*pdat).DA.scale
		for i=0,(*pm).DA.N-1 do begin 
			(*pm).DA.name[i] = string((*pdat).DA.bname[*,i])
		endfor
		
		(*pm).deadtime.on = (*pdat).enable.deadtime
		if (*pm).deadtime.on then begin
			if (*pm).version.software ge 4737 then begin
				(*pm).deadtime.cal = (*pdat).deadtime.cal
				
;				(*pimage).deadtime_cal = (*pm).deadtime.cal				; obsolete in image (now in Maia device read/write)		
				obj->set_options, deadtime_cal=(*pm).deadtime.cal
			endif
		endif
		
		(*pm).scan.on = (*pdat).enable.pixel
		if (*pm).scan.on or (scan eq 0) then begin
			(*pm).scan.X = (*pdat).scan.X
			(*pimage).dwell = (*pdat).scan.dwell
			
			if init then begin
				(*pm).scan.origin.X = (*pdat).scan.origin.X
				(*pm).scan.origin.Y = (*pdat).scan.origin.Y
				(*pm).scan.origin.Xunit = string((*pdat).scan.origin.bXunit)
				(*pm).scan.origin.Yunit = string((*pdat).scan.origin.bYunit)
				(*pm).scan.pitch.X = (*pdat).scan.pitch.X
				(*pm).scan.pitch.Y = (*pdat).scan.pitch.Y 
				(*pm).scan.pitch.Xunit = string((*pdat).scan.pitch.bXunit)
				(*pm).scan.pitch.Yunit = string((*pdat).scan.pitch.bYunit)	
			endif
		endif
		
	endif else begin
		v = socket_command_get( ps, 'enable', class='da', error=err)
		if err eq 0 then (*pm).da.on = v[0]
		
		if (*pm).DA.on then begin
			v = socket_command_get( ps, 'number', class='da.element', error=err)
			if err eq 0 then (*pm).DA.N = v[0] < (*pm).number.DA
			if (*pm).DA.N eq 0 then (*pm).DA.on=0
			
			if (*pm).DA.N gt 0 then begin
				v = socket_command_get( ps, 'name', class='da.element', chip=-1, n_chips=(*pm).number.da, /string, error=err)
				if err eq 0 then (*pm).DA.name[0:(*pm).DA.N-1] = v[0:(*pm).DA.N-1]
					
				v = socket_command_get( ps, 'scale', class='da.element', chip=-1, n_chips=(*pm).number.da, /float, error=err)
				if err eq 0 then (*pm).DA.scale[0:(*pm).DA.N-1] = v[0:(*pm).DA.N-1]
			endif
		endif
		
		v = socket_command_get( ps, 'enable', class='deadtime', error=err)
		if err eq 0 then (*pm).deadtime.on = v[0]
		
		if (*pm).deadtime.on or (deadtime eq 0) then begin	
			if (*pm).version.software ge 4737 then begin
				v = socket_command_get( ps, 'coeff', class='deadtime.time', channel=-1, n_channels=2, /float, error=err)
				if err eq 0 then begin
					(*pm).deadtime.cal.b = v[0]
					(*pm).deadtime.cal.a = v[1]
					obj->set_options, deadtime_cal=(*pm).deadtime.cal
				endif
			endif
		endif
		
		v = socket_command_get( ps, 'enable', class='pixel', error=err)
		if err eq 0 then (*pm).scan.on = v[0]
		
		if (*pm).scan.on or (scan eq 0) then begin
			v = socket_command_get( ps, 'size', class='scan', n_channels=3, channel=-1, error=err)
			if err eq 0 then begin
				(*pm).scan.X = v[0]
				(*pm).scan.Y = v[1]
			endif
			v = socket_command_get( ps, 'dwell', class='scan', error=err)
			if err eq 0 then begin
				(*pimage).dwell.val = 1000. * v[0]
				(*pimage).dwell.on = 1
			endif
		
			if init then begin
				v = socket_command_get( ps, 'origin', class='pixel.dim', n_chips=3, chip=-1, error=err)
				if err eq 0 then begin
					(*pm).scan.origin.X = v[0]
					(*pm).scan.origin.Y = v[1]
				endif
				v = socket_command_get( ps, 'unit', class='position.dim', n_chips=3, chip=-1, error=err)
				if err eq 0 then begin
					(*pm).scan.origin.Xunit = v[0]
					(*pm).scan.origin.Yunit = v[1]
					(*pm).scan.pitch.Xunit = v[0]
					(*pm).scan.pitch.Yunit = v[1]
				endif
		
				v = socket_command_get( ps, 'pitch', class='pixel.dim', n_chips=3, chip=-1, error=err)
				if err eq 0 then begin
					(*pm).scan.pitch.X = v[0]
					(*pm).scan.pitch.Y = v[1]
				endif
;				v = socket_command_get( ps, 'unit', class='position.dim', n_chips=3, chip=-1, error=err)
;				if err eq 0 then begin
;					(*pm).scan.pitch.Xunit = v[0]
;					(*pm).scan.pitch.Yunit = v[1]
;				endif
			endif
		endif
	endelse 
return
end

;--------------------------------------------------------------------------

pro maia_launch_read_da_info, ps, pm, pimage, pshrmem=pshrmem

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
		warning,'maia_launch_read_da_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(pshrmem) eq 0 then pshrmem=0L		; pars shrmem from 'maia_update_parameters'

@maia_scratch.def

; If 'pshrmem' is passed, then copy from *pdat in shared memory.
; Else, read direct from Kandinski during init and to get up-to-date data.

	if ptr_good(pshrmem) then begin
		pdat = (*pshrmem).pdat

;		Flux.chan variables for built-in FC0, FC1 ...

		if ((*pm).version.software ge 6646) then begin
			(*pm).IC0.remote = (*pdat).IC0.remote
			(*pm).IC0.pv.name = string( (*pdat).IC0.pv.bname)
			(*pm).IC0.pv.val = (*pdat).IC0.pv.val
			(*pm).IC0.pv.unit = (*pdat).IC0.pv.unit

			(*pm).IC1.remote = (*pdat).IC1.remote
			(*pm).IC1.pv.name = string( (*pdat).IC1.pv.bname)
			(*pm).IC1.pv.val = (*pdat).IC1.pv.val
			(*pm).IC1.pv.unit = (*pdat).IC1.pv.unit
		endif
				
;		DA info struct string ...

		if (*pm).version.software ge 4737 then begin
			s = string((*pdat).info.da)
			r = unstringify( s, error=err, context='maia_launch_read_da_info 1')
			if err and (strlen(s) gt 0) then begin
				print,'maia_launch_read_da_info: ','failed to decode "da.info" data from shared memory.'
				return
			endif
			goto, cont
		endif
		
	endif else begin
	
;		Flux.chan variables for built-in FC0, FC1 ...
;		Using 'charge_sensitivity' to convert val and string unit to sensitivity in nA/V units,
;		and 'charge_gain_units' to convert to 'val' and floating 'unit' scaling.

		if ((*pm).version.software ge 6646) then begin
			v1 = socket_command_get( ps, 'unit', class='flux.chan', n_chips=2, chip=-1, /string, error=err)
			if err eq 0 then begin
				if v1[0] ne '' then begin			; Maia:scaler.FC0
					(*pm).IC0.remote = 0
					v = socket_command_get( ps, 'coeff', class='flux.chan', chip=0, /float, error=err)
					if err eq 0 then begin
						(*pm).IC0.remote = 1
						(*pm).IC0.pv.val = v[0]
					endif else (*pl)[2]++
					v = socket_command_get( ps, 'name', class='flux.chan', chip=0, /string, error=err)
					if err eq 0 then begin
						(*pm).IC0.pv.name = v[0]
					endif else (*pl)[2]++
					v = socket_command_get( ps, 'unit', class='flux.chan', chip=0, /string, error=err)
					if err eq 0 then begin
						sens = charge_sensitivity( (*pm).IC0.pv.val, v[0])
						val = charge_gain_units( sens, unit=vunit)
						(*pm).IC0.pv.val = val
						(*pm).IC0.pv.unit = vunit
					endif else (*pl)[2]++
				endif
				if v1[1] ne '' then begin			; Maia:scaler.FC1
					(*pm).IC1.remote = 0
					v = socket_command_get( ps, 'coeff', class='flux.chan', chip=1, /float, error=err)
					if err eq 0 then begin
						(*pm).IC1.remote = 1
						(*pm).IC1.pv.val = v[0]
					endif else (*pl)[2]++
					v = socket_command_get( ps, 'name', class='flux.chan', chip=1, /string, error=err)
					if err eq 0 then begin
						(*pm).IC1.pv.name = v[0]
					endif else (*pl)[2]++
					v = socket_command_get( ps, 'unit', class='flux.chan', chip=1, /string, error=err)
					if err eq 0 then begin
						sens = charge_sensitivity( (*pm).IC1.pv.val, v[0])
						val = charge_gain_units( sens, unit=vunit)
						(*pm).IC1.pv.val = val
						(*pm).IC1.pv.unit = vunit
					endif else (*pl)[2]++
				endif
			endif
		endif

;		DA info struct string ...

		if (*pm).version.software ge 4737 then begin
;			v  = socket_command_get( ps, 'info', class='da', /string, error=err)
			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_DA, /string, error=err)
			if err then return
			r = unstringify( v[0], error=err, context='maia_launch_read_da_info 2')
			if err and (strlen(v[0]) gt 0) then begin
				print,'maia_launch_read_da_info: ','failed to decode "da.info" data direct from Kandinski.'
				return
			endif
		endif
	endelse

cont:
	if tag_present('IC',r) then begin

;		Copy IC part of the DA info return struct to (*pm).IC, if NOT remote ...
;		If the selected one is NOT a h/w scaler, also save it as ICE.

		pr = ptr_new(r)
		if (*pm).IC.remote eq 0 then begin
			copy_pointer_data, pr, pm, tag=['IC']
			if ((*pm).IC.pv.name ne 'Maia:scaler.FC0') and ((*pm).IC.pv.name ne 'Maia:scaler.FC1') then begin
				(*pm).ICE.pv = (*pm).IC.pv
			endif
		endif
		ptr_free, pr
		check_plist_maia, (*pm).IC.plist
	endif

;	Use '(*pm).IC.pv.name' to select current IC. Copy from IC0, IC1 for h/w 
;	(which may get updated from Kandinski if changed elsewhere) ...

	if (*pm).IC.pv.name eq 'Maia:scaler.FC0' then begin
		(*pm).IC.remote = (*pm).IC0.remote
		if (*pm).IC.remote then begin
			(*pm).IC.pv = (*pm).IC0.pv
			(*pm).IC.pv.name = 'Maia:scaler.FC0'
		endif
	endif else 	if (*pm).IC.pv.name eq 'Maia:scaler.FC1' then begin
		(*pm).IC.remote = (*pm).IC1.remote
		if (*pm).IC.remote then begin
			(*pm).IC.pv = (*pm).IC1.pv
			(*pm).IC.pv.name = 'Maia:scaler.FC1'
		endif
	endif else if (*pm).IC.pv.name eq 'Maia:dwell.time' then begin
		(*pm).IC.remote = 0
	endif else begin
		(*pm).IC.remote = 0
		(*pm).IC.pv = (*pm).ICE.pv
	endelse

	if tag_present('file',r) then begin
		(*pm).da.file = r.file
	endif
	if tag_present('matrix',r) then begin
		t = (*pimage).matrix
		struct_assign, r.matrix, t
		if ptr_valid( (*pimage).matrix.mdl) then ptr_free, (*pimage).matrix.mdl
		(*pimage).matrix = t							; (*pimage).matrix.mdl inherits pointer address of r.matrix.mdl
	endif												; this is OK as r.matrix.mdl is local, but not freed

;	if tag_present('array',r) then begin
;		*(*pm).DA.array = r.array
;	endif

;	Not stored in Hymod now, only local ...
;	if tag_present('save_path',r) then begin
;		(*pm).da.save_path = r.save_path
;	endif
return
end

;--------------------------------------------------------------------------

pro maia_launch_read_da_rGamma, ps, pm

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
		warning,'maia_launch_read_da_rGamma',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

n_detectors = (*ps).n_detectors

if (*pm).DA.on and ((*pm).version.software ge 5411) then begin
	v = socket_command_get( ps, 'number', class='da.element', error=err)
	if err eq 0 then (*pm).DA.N = v[0] < (*pm).number.DA
	if (*pm).DA.N eq 0 then (*pm).DA.on=0
	if ptr_valid((*pm).DA.parray) eq 0 then (*pm).DA.parray = ptr_new(/allocate_heap)
	
	if (*pm).DA.N gt 0 then begin
		rGamma = fltarr(n_detectors,(*pm).DA.N)
		for j=0L,(*pm).DA.N-1 do begin
			v = socket_command_get( ps, 'geometry', class='da.element', chip=j, channel=-1, n_channels=n_detectors, /float, error=err)
			if err eq 0 then rGamma[*,j] = v[0:n_detectors-1]
		endfor
		*(*pm).DA.parray = {On:1, n_det:n_detectors, rGamma:rGamma}
	endif else begin
		*(*pm).DA.parray = {On:0, n_det:n_detectors}
	endelse
endif
return
end

;--------------------------------------------------------------------------

pro maia_launch_read_pileup, ps, pm

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
		warning,'maia_launch_read_pileup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

v = socket_command_get( ps, 'trange', class='pileup.energy', chip=-1, n_chips=4096, channel=0, error=err)
if err eq 0 then begin
	*(*pm).pileup.limits.plow = v
endif
v = socket_command_get( ps, 'trange', class='pileup.energy', chip=-1, n_chips=4096, channel=1, error=err)
if err eq 0 then begin
	*(*pm).pileup.limits.phigh = v
endif
return
end

;--------------------------------------------------------------------------

pro maia_launch_read_enable, ps, pm, pr, pimage, play, disable=disable, pshrmem=pshrmem

; pshrmem=pshrmem	pointer to shared memory copy to read from, else read direct from Kandinski

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
		warning,'maia_launch_read_enable',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(pshrmem) eq 0 then pshrmem=0L

	maia_launch_read_info, ps, pm, pimage, pshrmem=pshrmem

; Cal coeffs. Gain-trim coeffs; also read in maia_update_parameters3 ...
	
	if ptr_good(pshrmem) then begin
		pdat = (*pshrmem).pdat
		*pr = (*pdat).readout
		(*pm).linear.on = (*pdat).enable.linear
		(*pm).trim.on = (*pdat).enable.gaintrim
		(*pm).pileup.on = (*pdat).enable.pileup
		(*pm).throttle.on = (*pdat).enable.throttle
		(*pm).scan.on = (*pdat).enable.pixel
		(*pm).deadtime.on = (*pdat).enable.deadtime
		(*pm).DA.on = (*pdat).DA.on
		(*pm).ROI.on = (*pdat).enable.ROI
		(*pm).groups.on = (*pdat).enable.groups

		q = where( (*pdat).enable.ECH eq 0, nq)
		if ptr_valid((*pimage).pactive) eq 0 then (*pimage).pactive = ptr_new(/allocate_heap)
		if nq gt 0 then begin
			*(*pimage).pactive = q
		endif else begin
			*(*pimage).pactive = 0
		endelse
		disable = (*pdat).enable.ECH
		
	endif else begin
		v  = socket_command_get( ps, 'enable', class='event', error=err)
		if err eq 0 then (*pr).event = v[0]
		
		v = socket_command_get( ps, 'enable', class='readout.clock', chip=-1, n_chips=3, error=err)
		if err eq 0 then (*pr).rr_enable = v
		
		v = socket_command_get( ps, 'enable', class='readout.quad', chip=-1, n_chips=4, error=err)
		if err eq 0 then (*pr).quad_enable = v
		
		v = socket_command_get( ps, 'enable', class='photon', error=err)
		if err eq 0 then (*pr).photon = v[0]
		
		v = socket_command_get( ps, 'enable', class='accum', error=err)
		if err eq 0 then (*pr).accum = v[0]
		
		v = socket_command_get( ps, 'enable', class='activity', error=err)
		if err eq 0 then (*pr).activity = v[0]
		
		v = socket_command_get( ps, 'enable', class='scepter', error=err)
		if err eq 0 then (*pr).scepter = v[0]
		
		v = socket_command_get( ps, 'enable', class='linearise', error=err)
		if err eq 0 then (*pm).linear.on = v[0]
		if ((*pm).version.software ge 9542) then begin
			v2 = socket_command_get( ps, 'enable', class='linearise2', error=err2)
			if (err2 eq 0) and (err eq 0) then (*pm).linear.on = v[0] or v2[0]
		endif

		v = socket_command_get( ps, 'enable', class='gaintrim', error=err)
		if err eq 0 then (*pm).trim.on = v[0]
		
		v = socket_command_get( ps, 'enable', class='pileup', error=err)
		if err eq 0 then (*pm).pileup.on = v[0]
		
		v = socket_command_get( ps, 'enable', class='throttle', error=err)
		if err eq 0 then (*pm).throttle.on = v[0]
		
		v = socket_command_get( ps, 'enable', class='pixel', error=err)
		if err eq 0 then (*pm).scan.on = v[0]
		
		v = socket_command_get( ps, 'enable', class='deadtime', error=err)
		if err eq 0 then (*pm).deadtime.on = v[0]
		
		v = socket_command_get( ps, 'enable', class='da', error=err)
		if err eq 0 then (*pm).DA.on = v[0]
		
		if (*pm).number.roi gt 0 then begin
			v = socket_command_get( ps, 'enable', class='roi', error=err)
			if err eq 0 then (*pm).ROI.on = v[0]
		endif
		
		if (*pm).number.spectra gt 0 then begin
			v = socket_command_get( ps, 'enable', class='spectrum', chip=-1, n_chips=(*pm).groups.spectra, error=err)
			if err eq 0 then (*pm).groups.on = v[0]	
		endif
		
		v = socket_command_get( ps, 'ECH', class='hermes', chip=-1, channel=-1, error=err)
		if err eq 0 then begin
			q = where( v eq 0, nq)
			if ptr_valid((*pimage).pactive) eq 0 then (*pimage).pactive = ptr_new(/allocate_heap)
			if nq gt 0 then begin
				*(*pimage).pactive = q
			endif else begin
				*(*pimage).pactive = 0
			endelse
			disable = v[(*play).data.index]
		endif
	endelse
	return
end

;--------------------------------------------------------------------------

pro maia_launch_read_gaintrim, ps, pm, play

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
		warning,'maia_launch_read_gaintrim',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

; Gain trim coeffs ...

; Detector array:
;	CSV table index is used for elements of the detector array, such as in the
;	'select' vector. To convert this to detector number, use the lookup table:
;
;	(*play).data.index						CSV table index to detector number
;	(*play).ref								detector number to CSV table index

index = (*play).data.index

v  = socket_command_get( ps, 'ecoeff', channel=0, class='gaintrim.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
if err eq 0 then (*pm).channel.trim.E.b = v[index]
v  = socket_command_get( ps, 'ecoeff', channel=1, class='gaintrim.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
if err eq 0 then (*pm).channel.trim.E.a = v[index]
v  = socket_command_get( ps, 'tcoeff', channel=0, class='gaintrim.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
if err eq 0 then (*pm).channel.trim.T.b = v[index]
v  = socket_command_get( ps, 'tcoeff', channel=1, class='gaintrim.det', chip=-1, n_chips=(*pm).n_detectors, /float, error=err)
if err eq 0 then (*pm).channel.trim.T.a = v[index]
	
return
end

;--------------------------------------------------------------------------

pro maia_launch_read_groups, ps, pl, play

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
		warning,'maia_launch_read_groups',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

@maia_scratch.def

	index = (*play).data.index
	
	(*pl).Groups.group[*].table = 0
	(*pl).Groups.group[*].pileup = 0
	(*pl).Groups.group[*].throttle = 0
	(*pl).Groups.group[*].et_mode = 0
	for i=0L,(*pl).Groups.spectra-1 do begin
		v = socket_command_get( ps, 'group', class='group.det', chip=-1, n_chips=(*pl).n_detectors, channel=i, error=err)
;		help, (*pl).n_detectors, v
		if err eq 0 then (*pl).Groups.group[i].table[0:(*pl).n_detectors-1] = v[index]
	endfor
	for i=(*pl).Groups.spectra,15 do begin
		v = socket_command_get( ps, 'group', class='group.det', chip=-1, n_chips=(*pl).n_detectors, channel=i, error=err)
		if err eq 0 then (*pl).Groups.group[i].table[0:(*pl).n_detectors-1] = v[index]
	endfor

	if (*pl).Groups.spectra gt 0 then begin
		v = socket_command_get( ps, 'select.pileup', class='spectrum', chip=-1, n_chips=(*pl).Groups.spectra, error=err)
		if err eq 0 then (*pl).Groups.group[0:(*pl).Groups.spectra-1].pileup = v	
		v = socket_command_get( ps, 'select.throttle', class='spectrum', chip=-1, n_chips=(*pl).Groups.spectra, error=err)
		if err eq 0 then (*pl).Groups.group[0:(*pl).Groups.spectra-1].throttle = v
	endif

	v = socket_command_get( ps, 'select.pileup', class='event', error=err)
	if err eq 0 then (*pl).Groups.group[13].pileup = v	
	v = socket_command_get( ps, 'select.throttle', class='event', error=err)
	if err eq 0 then (*pl).Groups.group[13].throttle = v	

	v = socket_command_get( ps, 'select.pileup', class='da', error=err)
	if err eq 0 then (*pl).Groups.group[14].pileup = v	
	v = socket_command_get( ps, 'select.throttle', class='da', error=err)
	if err eq 0 then (*pl).Groups.group[14].throttle = v	

	v = socket_command_get( ps, 'select.pileup', class='deadtime', error=err)
	if err eq 0 then (*pl).Groups.group[15].pileup = v	
	v = socket_command_get( ps, 'select.throttle', class='deadtime', error=err)
	if err eq 0 then (*pl).Groups.group[15].throttle = v	
	
	if (*pl).version.software ge 4813 then begin
;		v  = socket_command_get( ps, 'info', class='group', /string, error=err)
		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_group, /string, error=err)
		if err eq 0 then (*pl).groups.file = v[0]
	endif
	
	return
end

;-------------------------------------------------------------------------------------

pro maia_launch_read_info, ps, pm, pimage, pshrmem=pshrmem

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
		warning,'maia_launch_read_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(pshrmem) eq 0 then pshrmem=0L

; Info fields, and decode them ...
; 
; What about scan.info and scan.info[] ?

@maia_scratch.def

maia_launch_read_da_info, ps, pm, pimage, pshrmem=pshrmem

	if ptr_good(pshrmem) then begin
		pdat = (*pshrmem).pdat
		
		if (*pm).version.software ge 4737 then begin	
			if (*pm).number.roi gt 0 then begin
				(*pm).roi.file = string((*pdat).info.roi)
			endif

			s = string((*pdat).info.deadtime)
			r = unstringify( s, error=err, context='maia_launch_read_info 3')
			if err and (strlen(s) gt 0) then begin
				print,'maia_launch_read_info: ','failed to decode "deadtime.info" data.'
			endif else begin
				if tag_present('auto',r) then begin
					(*pm).deadtime.auto = r.auto
				endif
			endelse

			(*pm).cal.file = string((*pdat).info.cal)
			
			s = string((*pdat).info.gaintrim)
			r = unstringify( s, error=err, context='maia_launch_read_info 4')
			if err and (strlen(s) gt 0) then begin
				print,'maia_launch_read_info: ','failed to decode "gaintrim.info" data.'
			endif else begin
				if tag_present('file',r) then begin
					(*pm).trim.file = r.file
				endif
				if tag_present('file2',r) then begin
					(*pm).trim.file2 = r.file2
				endif
			endelse
	
			s = string((*pdat).info.linear)
			r = unstringify( s, error=err, context='maia_launch_read_info 5')
			if err and (strlen(s) gt 0) then begin
				print,'maia_launch_read_info: ','failed to decode "linearise.info" data.'
			endif else begin
				if tag_present('file',r) then begin
					(*pm).linear.file = r.file
				endif
			endelse
	
			s = string((*pdat).info.pileup)
			r = unstringify( s, error=err, context='maia_launch_read_info 6')
			if err and (strlen(s) gt 0) then begin
				print,'maia_launch_read_info: ','failed to decode "pileup.info" data.'
			endif else begin
				if tag_present('file',r) then begin
					(*pm).pileup.file = r.file
				endif
			endelse
	
			s = string((*pdat).info.throttle)
			r = unstringify( s, error=err, context='maia_launch_read_info 7')
			if err and (strlen(s) gt 0) then begin
				print,'maia_launch_read_info: ','failed to decode "throttle.info" data.'
			endif else begin
				if tag_present('file',r) then begin
					(*pm).throttle.file = r.file
				endif
			endelse
		endif
		
		if (*pm).version.software ge 4813 then begin
			(*pm).groups.file = string((*pdat).info.group)
		endif		

	endif else begin
		if (*pm).version.software ge 4737 then begin	
			if (*pm).number.roi gt 0 then begin
;				v  = socket_command_get( ps, 'info', class='roi', /string, error=err)
				v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_ROI, /string, error=err)
				if err eq 0 then (*pm).roi.file = v[0]
			endif
			
;			v  = socket_command_get( ps, 'info', class='deadtime', /string, error=err)
			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_deadtime, /string, error=err)
			if err eq 0 then begin
				r = unstringify( v[0], error=err, context='maia_launch_read_info 8')
				if (err eq 0) and (strlen(v[0]) gt 0) then begin
					if tag_present('auto',r) then begin
						(*pm).deadtime.auto = r.auto
					endif
				endif
			endif
			
;			v  = socket_command_get( ps, 'info', class='cal', /string, error=err)
			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_cal, /string, error=err)
			if err eq 0 then (*pm).cal.file = v[0]
		
;			v  = socket_command_get( ps, 'info', class='gaintrim', /string, error=err)
			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_gaintrim, /string, error=err)
			if err eq 0 then begin
				r = unstringify( v[0], error=err, context='maia_launch_read_info 9')
				if err and (strlen(v[0]) gt 0) then begin
					print,'maia_launch_read_info: ','failed to decode "gaintrim.info" data.'
				endif else begin
					if tag_present('file',r) then begin
						(*pm).trim.file = r.file
					endif
					if tag_present('file2',r) then begin
						(*pm).trim.file2 = r.file2
					endif
				endelse
			endif
			
;			v  = socket_command_get( ps, 'info', class='linearise', /string, error=err)
			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_linearise, /string, error=err)
			if err eq 0 then begin
				r = unstringify( v[0], error=err, context='maia_launch_read_info 10')
				if err and (strlen(v[0]) gt 0) then begin
					print,'maia_launch_read_info: ','failed to decode "linearise.info" data.'
				endif else begin
					if tag_present('file',r) then begin
						(*pm).linear.file = r.file
					endif
				endelse
			endif
			
;			v  = socket_command_get( ps, 'info', class='pileup', /string, error=err)
			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_pileup, /string, error=err)
			if err eq 0 then begin
				r = unstringify( v[0], error=err, context='maia_launch_read_info 11')
				if err and (strlen(v[0]) gt 0) then begin
					print,'maia_launch_read_info: ','failed to decode "pileup.info" data.'
				endif else begin
					if tag_present('file',r) then begin
						(*pm).pileup.file = r.file
					endif
				endelse
			endif
			
;			v  = socket_command_get( ps, 'info', class='throttle', /string, error=err)
			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_throttle, /string, error=err)
			if err eq 0 then begin
				r = unstringify( v[0], error=err, context='maia_launch_read_info 12')
				if err and (strlen(v[0]) gt 0) then begin
					print,'maia_launch_read_info: ','failed to decode "throttle.info" data.'
				endif else begin
					if tag_present('file',r) then begin
						(*pm).throttle.file = r.file
					endif
				endelse
			endif	
		endif
		
		if (*pm).version.software ge 4813 then begin
;			v  = socket_command_get( ps, 'info', class='group', /string, error=err)
			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_group, /string, error=err)
			if err eq 0 then (*pm).groups.file = v[0]
		endif
	endelse
return
end

;--------------------------------------------------------------------------

pro maia_launch_read_throttle, ps, pm

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
		warning,'maia_launch_read_throttle',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	; Throttle factors ...
	
	v  = socket_command_get( ps, 'factor', class='throttle.energy', chip=-1, n_chips=n_elements((*pm).throttle.factors), error=err)
	if err eq 0 then (*pm).throttle.factors = v
	
return
end

;--------------------------------------------------------------------------

pro maia_launch_update_cal, pp_et, pp_group, pimage, pm, play

; Update array energy cals for ET spectra, Group spectra and image pcal arrays.
; 
; Detector array:
;	CSV table index is used for elements of the detector array, such as in the
;	'select' vector. To convert this to detector number, use the lookup table:
;
;	(*play).data.index						CSV table index to detector number
;	(*play).ref								detector number to CSV table index

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
		warning,'maia_launch_update_cal',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	pp = pp_et												; ET spectra
	for i=0L,n_elements(*pp)-1 do begin
		(*(*pp)[i]).cal.units = (abs((*pm).channel[ (*play).ref[i] ].cal.a - 1.0) gt 0.001) ? 'keV' : 'channel'
		(*(*pp)[i]).cal.poly[0:1] = [(*pm).channel[ (*play).ref[i] ].cal.b,(*pm).channel[ (*play).ref[i] ].cal.a]
	endfor
	
;	Should update group masks here into the (*pp).pactive list

	if (*pm).number.spectra gt 0 then begin
		pp = pp_group											; Group spectra
		q = where ( (abs((*pm).channel.cal.a - 1.0) gt 0.001), nq)
		if nq gt 0 then begin
			cal_a = (*pm).channel[q[0]].cal.a
			cal_b = (*pm).channel[q[0]].cal.b
			cal_units = 'keV'
		endif else begin
			cal_a = 1.0
			cal_b = 0.0
			cal_units = 'channel'
		endelse
		for i=0L,n_elements(*pp)-1 do begin
			(*(*pp)[i]).cal.units = cal_units
			(*(*pp)[i]).cal.poly[0:1] = [cal_b,cal_a]
		endfor
	endif
	
	maia_launch_update_image_cal, pimage, pm, play
	return
end

;--------------------------------------------------------------------------

pro maia_launch_update_deadtime, pstate, pm

; Check deadtime.auto and update DTcalA

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
		warning,'maia_launch_update_deadtime',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if ptr_good(pstate, /struct) eq 0 then return
if ptr_good(pm, /struct) eq 0 then return
if (*pm).deadtime.auto eq 0 then return

	ps = (*pstate).psocket
	if ptr_good(ps, /struct) eq 0 then return
	obj = (*pm).DevObj

	tds = (*pm).channel[0].scepter.tds
	if tds ge 8 then return

	ver = (*pm).version.scepter
	if ver ge 5 then begin
		DTcalA = [ 10.2, 5.2, 2.6, 1.32, 0.66, 0.44, 0.33, 0.17]
	endif else begin
		DTcalA = [ 33.8, 16.9, 8.5, 4.2, 2.1, 1.58, 1.06, 0.53]
	endelse

	if abs((*pm).deadtime.cal.a - DTcalA[tds]) gt 0.01 then begin
		(*pm).deadtime.cal.a = DTcalA[tds]
		dt = (*pm).deadtime.cal
		obj->set_options, deadtime_cal=dt
		socket_command_set, ps, 'coeff', [dt.b,dt.a], class='deadtime.time', channel=-1, n_channels=2
	endif
	return
end

;--------------------------------------------------------------------------

pro maia_launch_update_image_cal, pimage, pm, play

; Update array energy cals for image pcal arrays.
; 
; Detector array:
;	CSV table index is used for elements of the detector array, such as in the
;	'select' vector. To convert this to detector number, use the lookup table:
;
;	(*play).data.index						CSV table index to detector number
;	(*play).ref								detector number to CSV table index

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
		warning,'maia_launch_image_cal',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_null_image_1, max_image_cal
if n_elements(max_image_cal) lt 1 then max_image_cal = 8
if ptr_good((*pimage).pactive) eq 0 then return
	
	q = *(*pimage).pactive									; DA images
	nq = n_elements(q)
	if ptr_valid((*pimage).pcal) eq 0 then (*pimage).pcal = ptr_new(/allocate_heap)
	
;	Note this must match normal use of pcal structure ...

	*(*pimage).pcal = replicate( {ORDER:1, UNITS:'', POLY:FLTARR(max_image_cal+1)}, nq)
	pcal = (*pimage).pcal
	
	for i=0L,nq-1 do begin		
		(*pcal)[i].poly[0:1] = [(*pm).channel[(*play).ref[q[i]]].cal.b,(*pm).channel[(*play).ref[q[i]]].cal.a]
		(*pcal)[i].units = 'keV' 
	endfor	

	q = where( abs((*pm).channel.cal.a - 1.) gt 0.01, nq)
	if nq gt 0 then begin
		(*pimage).cal.order = 1
		(*pimage).cal.units = 'keV'
		(*pimage).cal.poly[0:1] = [(*pcal)[0].poly[0],(*pcal)[0].poly[1]]
	endif
	return
end

;--------------------------------------------------------------------------

pro maia_launch_update_chart, pstate, error=err

COMPILE_OPT STRICTARR
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
		warning,'maia_launch_update_chart',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		close_file, lun
		return
	endif
endif

pc = (*pstate).pchart
ppspec = (*pstate).pchart_spec
if n_elements(*ppspec) lt 1 then maia_launch_init_chart, pstate
if n_elements(*ppspec) lt 1 then return
if (*pc).n lt 1 then return
file = (*pc).file
path = fix_path( extract_path(file))
ext = extract_extension(file)
name0 = strip_path( strip_file_ext(file))

	n = n_elements( *ppspec )
	p0 = (*pc).p[0]
	nv = (*p0).n
	nc = (*(*ppspec)[0]).size
	nstep = 1000
	if (n lt 1) or (nv lt 1) then return
	p = (*pc).p[(*pc).n-1]
	time = (*p).time
	date = date_from_utc(time)
	
;	Log current log record to file ...

	err = 1
	s1 = strsplit( date, ' ', /extract, count=ns1)
	ofile = path + name0 +'-'+ s1[2] +'-'+ s1[1] +'-'+ s1[0] +'.'+ ext
	line = ''

	append = file_test(ofile)

	if append and ((*pc).time0 lt 1.) then begin			; need to lookup time0 in current day log file
		on_ioerror, bad_file
		openr, lun, ofile, /get_lun
		on_ioerror, bad_read
		readf, lun, line
		readf, lun, line
		close_file, lun
		s = strsplit( line, ',', /extract)
		(*pc).time0 = utc_from_date( s[1])
	endif

	on_ioerror, bad_file
	openw, lun, ofile, /get_lun, append=append
	on_ioerror, bad_write
	if append eq 0 then begin
		head = strarr(nv)
		(*pc).time0 = time
		
;		N.B. The heading must be the same as the "name" so that the names can be read back from the
;		log file during init in "maia_chart_add" to guarantee continuity in names for each spectrum after
;		a restart of Maia_Control.

		for i=0L,nv-1 do begin
			head[i] = (*(*p0).p[i]).name
			if ((*(*p0).p[i]).index gt 0) or (i lt nv-1) and ((*(*p0).p[(i+1)<(nv-1)]).index gt 0) then begin
				head[i] = head[i]+'['+str_tidy((*(*p0).p[i]).index)+']'
			endif
		endfor
		printf, lun, 'Time(sec), UTC', head, format='(A,",",'+str_tidy(nv)+'(A,:,","))'
	endif
	err = 0
	
	nv = (*p).n
	val = fltarr(nv)
	for i=0L,nv-1 do val[i]=(*(*p).p[i]).val
	printf, lun, time-(*pc).time0, date, val, format='(G,", ",A,",",'+str_tidy(nv)+'(G,:,","))'
	goto, finish
	
bad_file:
	warning,'maia_launch_update_chart','Error opening Maia log file: '+ ofile
	goto, finish
bad_write:
	warning,'maia_launch_update_chart','Error writing to Maia log file: '+ ofile
	goto, finish
bad_read:
	warning,'maia_launch_update_chart','Error reading Maia log file: '+ ofile
	goto, finish

finish:
	on_ioerror, null
	close_file, lun
	
;	Check for spectrum full and shuffle left ...

	if ((*pc).n ge nc) then begin
		print,'maia_launch_update_chart: spectrum full, shuffle left ...'
		nstep = nstep < ((*pc).n-1)
		for i=0L,nv-1 do begin								; copy-shift spectrum
			(*(*(*ppspec)[i]).data)[0:nc-1-nstep] = (*(*(*ppspec)[i]).data)[nstep:nc-1]
			(*(*(*ppspec)[i]).data)[nc-nstep:nc-1] = 0
		endfor
		pnew = [(*pc).p[nstep:*],(*pc).p[0:nstep-1]]		; rotate ptr table
		(*pc).p = pnew
		(*pc).n = ((*pc).n - nstep) > 1
		p = (*pc).p[(*pc).n-1]
	endif

;	thresh = -30.
	for i=0L,nv-1 do begin
;		if (*(*(*pc).p[(*pc).n-1]).p[i]).val gt thresh then begin
			(*(*(*ppspec)[i]).data)[(*pc).n-1] = (*(*p).p[i]).val
;		endif else begin
;			(*(*(*ppspec)[i]).data)[(*pc).n-1] = (*(*(*ppspec)[i]).data)[((*pc).n-2)>0]
;		endelse
	endfor
return
end

;--------------------------------------------------------------------------

pro maia_launch_update_DA_images, pimage, pm

; Update the element list, and other parameters, in the DA image struct.

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
		warning,'maia_launch_update_da_images',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if ptr_good(pimage, /struct) eq 0 then return
if ptr_good(pm, /struct) eq 0 then return
if (*pm).DA.N eq 0 then return

	(*(*pimage).el)[0:(*pm).DA.N-1] = (*pm).DA.name[0:(*pm).DA.N-1]
							
;	This order also in maia_launch_update_DA_images, blog_client_da2, maia_launch (setup), blog_file_da2

	n_extra = n_elements( special_elements(/maia))
	nel = n_elements(*(*pimage).el)
	if (*pm).DA.N lt nel then begin
		nbot = (*pm).DA.N
		nbig = (*pm).DA.N + n_extra
		ntop = nbig < nel
		n_extra = n_extra - (nbig-ntop)
		(*(*pimage).el)[nbot:ntop-1] = (special_elements(/maia))[0:n_extra-1]
		if ntop lt nel then begin
			(*(*pimage).el)[ntop:nel-1] = ' '
		endif
	endif
	
;	Matrix details updated en masse in "maia_launch_read_da_info"
;	Image cal and array cal set-up in "maia_setup_hymod_apply" and "maia_launch_initial"

;	Should check (*pm).scan.origin.Xunit

	(*pimage).scan.origin.x = (*pm).scan.origin.X
	(*pimage).scan.origin.y = (*pm).scan.origin.Y

;	Should check (*pm).scan.pitch.Xunit

	(*pimage).scan.x = (*pm).scan.X * (*pm).scan.pitch.X
	(*pimage).scan.y = (*pm).scan.Y * (*pm).scan.pitch.Y

	if (*pm).pileup.on then (*pimage).pileup = (*pm).pileup.file
	if (*pm).throttle.on then (*pimage).throttle = (*pm).throttle.file
	
;	Bounds are set for a selective 'write_geopixe_image' save of RT images.
;	Now set in maia_update_da2 to actual range observed.

	(*pimage).IC.mode = (*pm).IC.mode
	(*pimage).IC.conversion = (*pm).IC.conversion
	(*pimage).IC.pv = (*pm).IC.pv
	if ptr_valid((*pimage).plist) then ptr_free, (*pimage).plist
	if ptr_valid((*pm).IC.plist) then (*pimage).plist = ptr_new( *(*pm).IC.plist)
	
;	Image scan dwell and origin set in "maia_launch_read_da"

	return
end

;--------------------------------------------------------------------------

pro maia_launch_version1, ps, n_detectors, version, error=error

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
		warning,'maia_launch_version1',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if (*ps).open eq 0 then begin
;	n_detectors = 384
;	print,'Port not open: default to detector channels = ', n_detectors

	(*ps).n_detectors = n_detectors
	(*ps).version = version
	error = 0
	return
endif

(*ps).version = version
v = socket_command_get( ps, 'version', class='config.ps', /long, /quiet, error=error)
(*ps).version = v[0]	
if error then (*ps).version = version

v = socket_command_get( ps, 'channels', class='config', /long, error=error)
if v[0] lt 1 then begin
	warning,'maia_launch version1:',['zero "config.channels" returned.','Will assume default for now.']
	socket_command_mode, /ddm_down
endif else begin
	n_detectors = v[0]
endelse

(*ps).n_detectors = n_detectors > 1
print,'maia_launch Version1: Detector channels = ', n_detectors
return
end

;--------------------------------------------------------------------------

pro maia_launch_version2, ps, pm, error=error

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
		warning,'maia_launch_version2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

lib = geopixe_library(version=lib_ver)
(*pm).identity.library = lib_ver

if (*ps).open eq 0 then begin
	print,'Port not open: default to detector channels = ', (*ps).n_detectors
	(*pm).n_detectors = (*ps).n_detectors
	
	(*pm).version.dam = 1
	error = 0
	return
endif

ident = socket_command_get( ps, 'identity', class='config.dam', /string, error=error)
if error eq 0 then (*pm).identity.dam = ident
ident = socket_command_get( ps, 'identity', class='config.ddm', /string, error=error)
if error eq 0 then (*pm).identity.ddm = ident
;ident = socket_command_get( ps, 'identity', class='config.dbpm', /string, /quiet, error=error)
;if error eq 0 then (*pm).identity.dbpm = ident
ident = socket_command_get( ps, 'identity', class='config.ps', /string, error=error)
if error eq 0 then (*pm).identity.software = ident

ver = socket_command_get( ps, 'revision', class='config.dam', /long, /quiet, error=error)
if error eq 0 then (*pm).version.dam = ver[0]	
ver = socket_command_get( ps, 'revision', class='config.ddm', /long, error=error)
if error eq 0 then (*pm).version.ddm = ver[0]	
ver = socket_command_get( ps, 'revision', class='config.dbpm', /string, /quiet, error=error)
if error eq 0 then (*pm).version.dbpm = long2(ver[0])	

(*pm).version.software = (*ps).version
ver = socket_command_get( ps, 'version', class='config.ps', /long, error=error)
if error eq 0 then (*pm).version.software = ver[0]

if (*pm).version.software lt 4737 then begin
	warning,'maia_launch',['Kandinski version [' +str_tidy((*pm).version.software)+ '] is too old.','Missing "scratch.datum" variables will cause problems.']
endif

ver = socket_command_get( ps, 'revision', class='config.hermes', /long, /quiet, error=error)
if error eq 0 then (*pm).version.hermes = ver[0]
ver = socket_command_get( ps, 'revision', class='config.scepter', /long, /quiet, error=error)
if error eq 0 then (*pm).version.scepter = ver[0]

n = socket_command_get( ps, 'da.number', class='config', /long, error=error)
if error eq 0 then (*pm).number.da = n < 32
if n gt 32 then warning,'maia_launch_version2','Kandinski DA accumulator number greater than 32.'
n = socket_command_get( ps, 'et2d.number', class='config', /long, error=error)
if error eq 0 then begin
	(*pm).number.et2d = n
endif else begin
	(*pm).number.et2d = 0
endelse
n = socket_command_get( ps, 'roi.number', class='config', /long, error=error)
if error eq 0 then begin
	(*pm).number.roi = n
endif else begin
	(*pm).number.roi = 0
endelse
n = socket_command_get( ps, 'spectrum.number', class='config', /long, error=error)
if error eq 0 then begin
	(*pm).number.spectra = n
	(*pm).groups.spectra = n
endif else begin
	(*pm).number.spectra = 0
	(*pm).groups.spectra = 0
endelse
n = socket_command_get( ps, 'timer.number', class='config', /long, error=error)
if error eq 0 then (*pm).number.timer = n

if (*pm).version.software ge 7012 then begin
	maxb = socket_command_get( ps, 'bias.voltage.max', class='detector', /float, error=error)
	if error eq 0 then begin
		(*pm).control.bias_max = (maxb[0] > 20.) < 200.
	endif
	maxb = socket_command_get( ps, 'bias.voltage.rate', class='detector', /float, error=error)
	if error eq 0 then begin
		(*pm).control.bias_rate = (maxb[0] > 0.002) < 0.5
	endif
	minb = socket_command_get( ps, 'bias.voltage.min', class='detector', /float, error=error)
	if error eq 0 then begin
		(*pm).control.bias_min = ((minb[0] < 50.) < ((*pm).control.bias_max-50.)) > 10.
	endif
endif else if (*pm).version.software ge 6900 then begin
	maxb = socket_command_get( ps, 'bias.voltage.max', class='config.detector', /float, error=error)
	if error eq 0 then begin
		(*pm).control.bias_max = (maxb[0] > 20.) < 200.
	endif
	maxb = socket_command_get( ps, 'bias.voltage.rate', class='detector', /float, error=error)
	if error eq 0 then begin
		(*pm).control.bias_rate = (maxb[0] > 0.002) < 0.5
	endif
endif else begin
	maxb = socket_command_get( ps, 'bias.voltage.max', class='config.detector', /float, error=error)
	if error eq 0 then begin
		bias_max = replicate(165.,100)
		(*pm).control.bias_max = (maxb[0] > 1.) < bias_max[(*pm).version.dam]
	endif
endelse
bias = socket_command_get( ps, 'bias.voltage', class='detector', /float, error=error)
if error eq 0 then (*pm).control.bias = (bias[0] > (*pm).control.bias_min) < (*pm).control.bias_max

if (*pm).version.software ge 7012 then begin
	maxp = socket_command_get( ps, 'peltier.current.max', class='detector', /float, error=error)
	if error eq 0 then begin
		(*pm).control.peltier_cool_max = (maxp[0] > 0.1) < 3.
	endif
	minp = socket_command_get( ps, 'peltier.current.min', class='detector', /float, error=error)
	if error eq 0 then begin
		(*pm).control.peltier_bake_max = (-minp[0] > 0.1) < 1.5
	endif
endif else begin
	maxp = socket_command_get( ps, 'peltier.current.range', class='config.detector', channel=-1, n_channel=2, /float, error=error)
	if error eq 0 then begin
		cool_max = [2.9,2.9,2.9,1.9,replicate(2.,100)]
		bake_max = [1.5,1.5,1.5,0.6,replicate(1.,100)]
		(*pm).control.peltier_cool_max = (maxp[1] > 0.1) < cool_max[(*pm).version.dam]
		(*pm).control.peltier_bake_max = (-maxp[0] > 0.1) < bake_max[(*pm).version.dam]	
	endif
endelse
peltier = socket_command_get( ps, 'peltier.current', class='detector', /float, error=error)
if error eq 0 then (*pm).control.peltier = peltier[0] 

if ((*pm).n_detectors ne 96) or ((*pm).version.dam ge 3) then begin
	guard = socket_command_get( ps, 'guard.voltage', class='detector', /float, error=error)
	if error eq 0 then (*pm).control.guard = (guard[0] > 0.1) < (*pm).control.guard_max
endif else guard=0.0

print,'maia_launch Version2:  identity		version'
print,'dam		',(*pm).identity.dam,'		',(*pm).version.dam
print,'ddm		',(*pm).identity.ddm,'		',(*pm).version.ddm
print,'dbpm		',(*pm).identity.dbpm,'		',(*pm).version.dbpm
print,'s/w		',(*pm).identity.software,'		',(*pm).version.software
print,'library		',(*pm).identity.library,'	',lib
print,'hermes			',(*pm).version.hermes
print,'scepter			',(*pm).version.scepter
print,'number of ET2D	',(*pm).number.et2d
print,'          da	',(*pm).number.da
print,'          roi	',(*pm).number.roi
print,'          spectra	',(*pm).number.spectra
print,'          timers	',(*pm).number.timer
print,'bias: min, max = ',(*pm).control.bias_min, (*pm).control.bias_max
print,'peltier: cool_max, bake_max = ',(*pm).control.peltier_cool_max, (*pm).control.peltier_bake_max
return
end

;--------------------------------------------------------------------------

pro maia_control

;	Maia launch (Maia_control.sav) loads routines from GeoPIXE.sav.

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

	maia_launch
	return
end

;--------------------------------------------------------------------------

;pro maia_control_new
;
;maia_launch
;return
;end

;--------------------------------------------------------------------------

pro maia_launch, debug=debug

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
common c_null_image_1, max_image_cal
if n_elements(debug) lt 1 then debug=0
if n_elements(max_image_cal) lt 1 then max_image_cal = 8
common c_debug_linux_bug, first										;@8-18
if n_elements(first) lt 1 then first=3								;@8-18

common c_errors_1, catch_errors_on
catch_errors_on = 1							; enable error CATCHing
if debug then catch_errors_on = 0			; disable error CATCHing
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
		warning,'maia_launch',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET

;	Due to an odd bug with very first pop-up, we try again ...		;@8-18

		if first gt 0 then begin									;@8-18
loop:
			first = (first-1) >0
			Catch, ErrorNo
			if (ErrorNo ne 0) then begin
				Catch, /cancel
				on_error, 1
				help, calls = s
				n = n_elements(s)
				c = 'Call stack: '
				if n gt 2 then c = [c, s[1:n-2]]
				warning,'maia_launch',['IDL run-time error caught.', '', $
						'Error:  '+strtrim(!error_state.name,2), $
						!error_state.msg,'',c], /error
				MESSAGE, /RESET
				if first gt 0 then goto, loop
				return
			endif
		endif else return
	endif
endif else on_error,0

startupp, /colours, /database			; setup IDL

resolve_routine, 'maia_setup', /no_recompile
image_routines							; Load support routines
register_notify							; notification routines
version = maia_version()				; Version text

if database_alive() ne 1 then begin
	warning,'maia_launch','failed to load "geopixe2.sav"'
endif

; Load devices and then set Maia as the default.

define_devices

;-------------------------------------------------------------------------------------
;
; Maia Kandinski control socket
; Use a socket to control Maia set-up and parameter display

default = maia_defaults( error=error, source='maia_launch', /reset)
if error then return

ip_maia = default.maia.ip
name_maia = default.maia.name
port_maia = default.maia.port
obj = obj_new(default.maia.device)	; Maia object: copy kept in *pm, *pstate
enable_maia = default.maia.enable
enable_project_select = default.maia.project_select
n_detectors = default.detectors
log_file = default.logfile			; will in future make use of read-back identity to make file-name
token_maia = '0'

time_maia = 5.0						; update ASIC parameters timer (maia_update_parameters)
time_maia2 = 0.8					; update rates, T, bias (maia_update_parameters2)
time_maia3 = 10.0					; update chart parameters timer (maia_update_parameters3)

enable_activity = 1					; use new Activity accumulator (else get from spectra_ET2)
timeout_processes = default.maia.timeout	; timeout for background processes

; This explicit parameter setting only works because this is a Maia device object ...
; Same goes for explicit 'deadtime_cal' setting elsewhere.

flipX = 0
if ((strmid(default.facility,0,6) eq 'MM.Mel') and (default.endstation eq '1')) or $
			((strmid(default.facility,0,6) eq 'MM.Per') and (default.endstation eq '2')) then begin
	obj->set_options, flip_x = 1
	flipX = 1
	print,'Maia device: force flip.X ON'
endif
if ((strmid(default.facility,0,6) eq 'MM.Mel') and (default.endstation eq '2')) or $
			((strmid(default.facility,0,6) eq 'MM.Per') and (default.endstation eq '1')) then begin
	obj->set_options, flip_x = 0
	flipX = 0
	print,'Maia device: force flip.X OFF'
endif

;-------------------------------------------------------------------------------------
;
;	GeoPIXE prefs, including kvs
;	KVS (and comms) needed to use 'log_message' logging to rsyslog in Maia Mapper.
;	implicit error logging for all 'warning', 'alarm_popup' calls.
;	Not enabled if KVS not enabled in 'geopixe.conf'
;
;	NOTE: By default, this assumes that the rsyslog server is the same as the KVS node.
;	If not, then add a line for "logging server" in 'geopixe.conf'.

path = ''
dpath = ''
prefs = geopixe_defaults( error=err, source='maia_launch')

;	If KVS enabled in the "geopixe.conf" file, then connect to the key-value store (KVS) using
;	the MM ZMQ library tools. This is for Maia Mapper support.

comms = 0
kvs = 0
kvs_prefix = ''
server = ''
if err eq 0 then begin
	if strlen(prefs.path.analysis) gt 0 then path = prefs.path.analysis
	if strlen(prefs.path.data) gt 0 then dpath = prefs.path.data

	if prefs.kvs.enable then begin
		pver = python_version( revision=prev)
		iver = idl_version( revision=irev)
		if ((float2(prev) ge 3.6) and (float2(irev) ge 8.8)) then begin		; IDL 8.8 works with python 3.6+

		endif else if ((prev eq '2.7') and (irev eq '8.5')) then begin		; IDL 8.5.1 works with python 2.7

		endif else begin
			warning,'maia_launch',['KVS has been enabled for Maia Mapper in your "geopixe.conf" file.', $
					'However, python version found = ' + pver, ' which is not compatible with IDL ' + iver+'.', '', $
					'Maia Mapper Libs need python 2.7 with IDL 8.5.1, or python 3.6+ with IDL 8.8+,', $
					'so configure for correct IDL + python combination or disable KVS in the "geopixe.conf" file.','', $
					'Continue without KVS and MM Libs ...']
			prefs.kvs.enable = 0
		endelse
	endif

	if prefs.kvs.enable then begin
		print,'Open KVS using endpoint: ', prefs.kvs.endpoint
		server = prefs.logging.server
		comms = open_comms( server=server, source='maia_launch', error=err)
		if err then begin
			warning,'maia_launch',['Failed to open a MM comms object.','Server = '+server]
		endif else begin
			save_comms_in_common, comms		; save comms for implicit 'log_message' in 'warning', 'alarm_popup'
			print, 'maia_launch started, with logging using server = '+server
			log_message, comms, type='INFO', 'maia_launch started, with logging using server = '+server
			s = get_login_info()
			clientname = s.machine_name
			username = s.user_name
			log_message, comms, type='INFO', 'maia_launch, machine='+clientname+', username='+username
		endelse
		kvs = open_kvs(prefs.kvs.endpoint, comms=comms, error=err)
		if err then begin
			warning,'maia_launch',['Failed to open the KVS endpoint: ',prefs.kvs.endpoint]
		endif
		kvs_prefix = prefs.kvs.prefix
		
		check_kvs, kvs, error=err
		if err then begin
			warning,'maia_launch',['Check of KVS failed.','Will close and ignore KVS.']
			close_kvs, kvs
		endif			
	endif else begin
		kvs = open_kvs( 'null')			; get it compiled, but just return 0L
	endelse
endif

;-------------------------------------------------------------------------------------
;
; Maia control socket and parameters struct

print,'Open Maia control socket ...'
ps = bad_pars_struct( maia_port, make_pars=make_ps)
if make_ps then begin
	*ps = open_socket( ip=ip_maia, port=port_maia, token=token_maia, enable=enable_maia, $
						client='Maia_Control', connect_timeout=30, retries=0, error=error)
	if error ne 0 then begin
		warning,'maia_launch','Failed to open Maia control socket.'
		goto, bad_setup
	endif else begin
		log_message, comms, type='INFO', 'maia_launch startup, Kandinski socket open = '+ip_maia
	endelse
endif

; Read initial version info from Kandinski, including "n_detectors"

maia_launch_version1, ps, n_detectors, default.version, error=error
if error then begin
	warning,'maia_launch','Failed to get initial version info from Maia control socket.'
	goto, bad_setup
endif

; Maia control parameters struct, read 'version', 'identity' back from Maia

pm = bad_pars_struct( maia_pars, make_pars=make_pm)
if make_pm then begin
	print,'Maia_launch: make Maia *pm struct based on n_detectors =',n_detectors
	*pm = define(maia_struct = n_detectors)
endif
(*pm).groups.spectra = default.spectra
(*pm).number.DA = default.DA
(*pm).groups.group[12].table[*] = 1
(*pm).DevObj = obj

maia_launch_version2, ps, pm, error=error
if error then begin
	warning,'maia_launch','Failed to get version/ident info from Maia control socket.'
	goto, bad_setup
endif

; Detector layout struct parameters from file "Maia_ ... .csv"

letters = ['','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']
prefix = 'Maia_' + str_tidy((*pm).n_detectors) + letters[ (*pm).version.dam ]
(*pm).file = 'Maia_' + (*pm).identity.dam + '.parameters.csv'
print,'  Maia identity: ', (*pm).identity.dam
print,'    Maia prefix: ', prefix
print,'  Maia log file: ', (*pm).file
log_message, comms, type='INFO', 'maia_launch, detector identity='+(*pm).identity.dam+', Maia prefix='+prefix

if extract_path(log_file) eq '' then begin
	log_file = extract_path(default.conf) + 'Maia_' + (*pm).identity.dam + '_log.csv'
endif else begin
	tlog_file = extract_path(default.conf) + 'Maia_' + (*pm).identity.dam + '_log.csv'
	warning,'maia_launch',['An override log file path has been set from the Maia.conf file.', $
				'Using log file: "'+log_file+'"','','May be better to remove "log_file" in .Maia.conf file,', $
				'to use a default file-name based on DAM identity.','It will be located in same dir as .Maia.conf file, with path/name:',tlog_file,'', $
				'To append to an existing log-file, rename it to match this name','and move it to this location, and restart Maia-Control.']
endelse
log_message, comms, type='INFO', 'maia_launch startup, Use log file name: '+log_file
print, 'Use log file name: '+log_file

pd = bad_pars_struct( data, make_pars=make_pd)
if make_pd then begin
	layout_file = geopixe_root + prefix + '.csv'
	d = read_detector_layout(layout_file, maia=maia, error=error)
	if error then begin
		warning,'maia_launch','Failed to read "'+layout_file+'" to initialize layout.'
		goto, bad_setup
	endif
	if maia eq 0 then begin
		warning,'maia_launch','"'+layout_file+'" file does not contain required extended columns.'
		goto, bad_setup
	endif
	*pd = d
endif

; Maia disable parameters struct

pe = bad_pars_struct( maia_disable, make_pars=make_pe)
if make_pe then begin
;	file = prefix  + '.enable.csv'
;	disable = read_maia_enable( file, index=(*pd).data.index, error=error)
;	if error then begin
;		warning,'maia_launch','Failed to read "'+file+'" to initialize disables.'
;		disable = intarr(n_detectors)
;	endif
;	*pe = disable
	*pe = intarr(n_detectors)
endif

; Detector rates parameters 

prates = bad_pars_struct( rates, make_pars=make_pr)
if make_pr then begin
;	dr = 11000. * randomu( seed, n_detectors)
;	dr[0] = 123456.
;	dr[1] = 33000.
;	dr[2] = 13000.
;	*prates = dr
	*prates = {detectors:fltarr(n_detectors), groups:fltarr(16)}
endif
pmrates = ptr_new( {detectors:fltarr(n_detectors)})

; Maia readout parameters struct

pr = bad_pars_struct( maia_readout, make_pars=make_pr)
if make_pr then begin
	readout = define(/maia_readout)				; various readout enables
	*pr = readout
endif

pimage_temp = ptr_new(define(/image))
maia_launch_initial, ps, pm, pd, pr, pimage_temp, default, error=error
if error then begin
	warning,'maia_launch','Failed to get version/ident info from Maia control socket.'
	goto, bad_setup
endif

; Initial array energy cal copied into image struct ...

maia_launch_update_image_cal, pimage_temp, pm, pd

q = where ( (abs((*pm).channel.cal.a - 1.0) gt 0.001), nq)
if nq gt 0 then begin
	cal_a = (*pm).channel[q[0]].cal.a				; cal for spectra
	cal_b = (*pm).channel[q[0]].cal.b
	cal_units = 'keV'
endif else begin
	cal_a = 1.0
	cal_b = 0.0
	cal_units = 'channel'
endelse

;-------------------------------------------------------------------------------------
;
; Slave process: Kandinski parameters shared memory.

maia_prefix = default.maia.name + '_'					; maia.name (max chars = 8)
prefix_pars = maia_prefix + 'pars_'						; append local prefix (max chars = 12)
if n_elements(byte(maia_prefix)) gt 9 then warning,'maia_launch','Too many characters in Maia name in conf file.'

template = define(maia_shared1 = n_detectors)	
pshrmem_pars = shared_memory_struct( prefix=prefix_pars, template=template, /init, error=error )
if error then goto, bad_shrmem

b = byte(ip_maia)
n = n_elements(b)
;(*(*pshrmem_pars).ppar)[2:3] = 0
(*(*pshrmem_pars).pbyte)[*] = 0B
(*(*pshrmem_pars).pbyte)[0:n-1] = b						; IP address
(*(*pshrmem_pars).plong)[0] = port_maia					; blog port

(*(*pshrmem_pars).pdat).n_detectors = n_detectors		; initialize shared memory
(*(*pshrmem_pars).pdat).version = (*pm).version			; e.g. values that will not change
(*(*pshrmem_pars).pdat).number = (*pm).number	
(*(*pshrmem_pars).pdat).readout = *pr
(*(*pshrmem_pars).pdat).layout_data = (*pd).data
(*(*pshrmem_pars).pdat).channel = (*pm).channel
(*(*pshrmem_pars).pdat).control = (*pm).control
(*(*pshrmem_pars).pdat).throttle.factors = (*pm).throttle.factors
if ptr_good((*pm).pileup.limits.plow) then begin
	(*(*pshrmem_pars).pdat).pileup.limits.low = *(*pm).pileup.limits.plow
endif
if ptr_good((*pm).pileup.limits.phigh) then begin
	(*(*pshrmem_pars).pdat).pileup.limits.high = *(*pm).pileup.limits.phigh
endif
(*(*pshrmem_pars).pdat).DA.on = (*pm).DA.on
(*(*pshrmem_pars).pdat).DA.N = (*pm).DA.N
if (*pm).DA.on and ptr_good((*pm).DA.parray) then begin
	if tag_present('rGamma',*(*pm).DA.parray) then begin
		(*(*pshrmem_pars).pdat).DA.rGamma = (*(*pm).DA.parray).rGamma
	endif
endif

;-------------------------------------------------------------------------------------
;
; Blog server data socket
; Used by clients to read data from blog
; Initially these parameters will be hardcoded into the clients,
; and the clients simply launched here ...

ip_blog = default.blog.ip
port_blog = default.blog.port
enable_blog = default.blog.enable

;-------------------------------------------------------------------------------------
;
; Use several blog clients to read blog records for spectra, images, etc.
; These run as separate processes (IDL clients initially).
; Use shared memory (no semaphores) to access this data.
; Define the shared memory here, and then start the client processes.
; Set up here as "master", by setting BOTH the buffer_size and n_buffers.
; 
log_message, comms, type='INFO', 'maia_launch startup, Setup all shared memory segments ...'
; 
; Slave process: Blog data shared memory - Spectra for up to 12 spectra groups
; Number of buffers comes from shared memory, set here.

prefix_spectra = maia_prefix + 'groups_'				; append local prefix (max chars = 12)
if n_elements(byte(maia_prefix)) gt 9 then warning,'maia_launch','Too many characters in Maia name in conf file.'
time_spectra = 0.8										; group spectra update time
time_spectra_update = 3.0								; group display update
time_spectra_min = 1.0									; group display update range
time_spectra_max = 5.0
pshrmem_spectra = 0L
pspec = 0L

n_buffers = (*pm).groups.spectra
if n_buffers ge 1 then begin
	buffer_size = 4096L	
	pshrmem_spectra = shared_memory_buffers( prefix=prefix_spectra, n_buffers=n_buffers, $
							buffer_size=buffer_size, /init, error=error)
	if error then goto, bad_shrmem
	b = byte(ip_blog)
	n = n_elements(b)
	;(*(*pshrmem_spectra).ppar)[2:3] = 0
	(*(*pshrmem_spectra).pbyte[0])[*] = 0B
	(*(*pshrmem_spectra).pbyte[0])[0:n-1] = b			; IP address
	(*(*pshrmem_spectra).plong[0])[0] = port_blog		; blog port
endif
		
if n_buffers ge 1 then begin
	pspec = ptrarr(n_buffers)
	for i=0L,n_buffers-1 do begin
		spec = define(/spectrum)
		spec.data = (*pshrmem_spectra).pdat[i]
		spec.size = (*pshrmem_spectra).buffer_size
		spec.pactive = ptr_new(indgen((*pm).n_detectors))		; should get updated from group masks
		spec.array = 1
		spec.detector = 7
		spec.station = 0
		spec.channel = i
		spec.cal.order = 1
		spec.cal.units = cal_units
		spec.cal.poly[0:1] = [cal_b,cal_a]	
		spec.comment = 'Maia on-line Group spectrum monitor'
		spec.source = 'Maia ' + (*pm).identity.DAM
		spec.file = 'Online'
		spec.DevObj = clone_device_object(obj)
		spec.label = 'Maia'+ (*pm).identity.DAM + ' Group #' + str_tidy(i) + ' /E'
		pspec[i] = ptr_new(spec, /no_copy)
	endfor
endif else pspec=ptr_new()


; Slave process: Blog data shared memory - Activity spectra for 384 detector, 16 groups
; Number of buffers comes from shared memory, set here. Data stored in *(*pstate).prates struct

prefix_activity = maia_prefix + 'activity_'				; append local prefix (max chars = 12)
;time_activity = 0.4
;time_activity_min = 0.2
;time_activity_max = 1.0
;time_activity_update = 3.0

n_buffers = 2											; 0 - detectors, 1 - groups
buffer_size = n_detectors	
pshrmem_activity = shared_memory_buffers( prefix=prefix_activity, n_buffers=n_buffers, $
						buffer_size=buffer_size, /init, error=error)
if error then goto, bad_shrmem
b = byte(ip_blog)
n = n_elements(b)
;(*(*pshrmem_activity).ppar)[2:3] = 0
(*(*pshrmem_activity).pbyte[0])[*] = 0B
(*(*pshrmem_activity).pbyte[0])[0:n-1] = b				; IP address
(*(*pshrmem_activity).plong[0])[0] = port_blog			; blog port


; Master: Blog data shared memory - Spectra (2D E spectra [4096,384] for ET2 records
; Number of buffers is set here and sent to the blog Client.

prefix_ET_spectra = maia_prefix + 'et_spec_'			; append local prefix (max chars = 12)
time_ET_spectra = 1.8									; ET spectrum update
time_ET_spectra_update = 3.0							; display update
time_ET_spectra_min = 2.0								; display update range
time_ET_spectra_max = 10.0

n_channels = 4096L										; channels in spectra
n_buffers3 = 1											; number of 2D spectra buffers
buffer_size3 = [n_channels,n_detectors]
pshrmem_ET_spectra = shared_memory_buffers( prefix=prefix_ET_spectra, n_buffers=n_buffers3, $
						buffer_size=buffer_size3, /init, error=error)
if error then goto, bad_shrmem
b = byte(ip_blog)
n = n_elements(b)
;(*(*pshrmem_ET_spectra).ppar)[2:3] = 0
(*(*pshrmem_ET_spectra).pbyte[0])[*] = 0B
(*(*pshrmem_ET_spectra).pbyte[0])[0:n-1] = b			; IP address
(*(*pshrmem_ET_spectra).plong[0])[0] = port_blog		; blog port
		
if n_buffers3 ge 1 then begin
	pspec3 = ptrarr(n_detectors)
	for i=0L,n_detectors-1 do begin
		spec = define(/spectrum)
		spec.data = ptr_new( lonarr(n_channels))
		spec.size = n_channels
		spec.array = 0
		spec.detector = 7
		spec.station = i+1
		spec.channel = i
		spec.cal.order = 1								
		spec.cal.units = (abs((*pm).channel[i].cal.a - 1.0) gt 0.001) ? 'keV' : 'channel'
		spec.cal.poly[0:1] = [(*pm).channel[i].cal.b,(*pm).channel[i].cal.a]
		spec.comment = 'Maia on-line Detector E spectra monitor'
		spec.source = 'Maia ' + (*pm).identity.DAM
		spec.file = 'Online'
		spec.DevObj = clone_device_object(obj)
		spec.label = 'Maia'+ (*pm).identity.DAM + ' Detector #' + str_tidy(i) + ' /E'
		pspec3[i] = ptr_new(spec, /no_copy)
	endfor
endif else pspec3=ptr_new()


; Master: Blog data shared memory - Spectra (2D T spectra [4096,384] for ET2 records
; Number of buffers is set here and sent to the blog Client.
; The pbyte[0] array here is used to store the Throttle spectrum for use by backgnd process.

prefix_ET_spectraT = maia_prefix + 'et_specT_'			; append local prefix (max chars = 12)

n_channels4 = 1024L										; channels in T spectra
n_buffers4 = 1											; number of 2D spectra buffers
buffer_size4 = [n_channels4,n_detectors]
pshrmem_ET_spectraT = shared_memory_buffers( prefix=prefix_ET_spectraT, n_buffers=n_buffers4, $
						buffer_size=buffer_size4, n_byte=4096, /init, error=error)
if error then goto, bad_shrmem
;(*(*pshrmem_ET_spectraT).ppar)[2:3] = 0

if n_buffers4 ge 1 then begin
	pspec4 = ptrarr(n_detectors)
	for i=0L,n_detectors-1 do begin
		spec = define(/spectrum)
		spec.data = ptr_new( lonarr(n_channels4))
		spec.size = n_channels4
		spec.array = 0
		spec.detector = 7
		spec.station = i+1
		spec.channel = i
		spec.cal.order = 1								; need to load Cal from setup.spec
		spec.cal.units = 'channel'						; enables set via Maia_Setup
		spec.cal.poly[0:1] = [0.0,1.0]	
	;	spec.ecal = spec.cal
		spec.comment = 'Maia on-line Detector T spectra monitor'
		spec.source = 'Maia ' + (*pm).identity.DAM
		spec.file = 'Online'
		spec.DevObj = clone_device_object(obj)
		spec.label = 'Maia'+ (*pm).identity.DAM + ' Detector #' + str_tidy(i) + ' /T'
		pspec4[i] = ptr_new(spec, /no_copy)
	endfor
endif else pspec4=ptr_new()


; Master: Blog data shared memory - ET2D (2D E,T maps [256,128,385] for ET2 records
; Separate maps for 'All' and each detector channel.
; Number of buffers is set here and sent to the blog Client.

prefix_ET2d = maia_prefix + 'et2d_'						; append local prefix (max chars = 12)

n_energy = 256
n_time = 128
buffer_size3b = [n_energy, n_time, n_detectors+1]
pshrmem_ET2d = shared_memory_buffers( prefix=prefix_ET2d, n_buffers=1, $
						buffer_size=buffer_size3b, /init, error=error)
if error then goto, bad_shrmem
;(*(*pshrmem_ET2d).ppar)[2:3] = 0
		
if ptr_valid((*pshrmem_ET2d).pdat[0]) then begin
	pimg3 = (*pshrmem_ET2d).pdat[0]
endif else begin
	pimg3 = ptr_new(fltarr(n_energy,n_time,n_detectors+1))
endelse

image = define(/image)
image.n_el = n_detectors+1
image.el = ptr_new(['All',str_tidy(indgen(n_detectors))])
image.image = pimg3
image.xsize = n_energy
image.ysize = n_time
image.xcompress =  (4096L/n_energy) > 1					; for 12 bit E
image.ycompress = (1024L/n_time) > 1					; for 10 bit T
image.array = 0
image.detector = 7
image.comment = 'Maia on-line ET 2D monitor'
image.source = 'Maia'
image.DevObj = clone_device_object(obj)
pimage2D = ptr_new(image, /no_copy)
fix_options, pimage2D
opt = (*pimage2D).options
for i=0,n_elements(*opt)-1 do (*opt)[i].log=1


; Slave: Blog data shared memory - DA Images

prefix_da = maia_prefix + 'da_'						; append local prefix (max chars = 12)
time_da = 0.5										; DA array update time
time_da_update = 1.5								; DA display update time
time_da_min = 0.5									; DA display update range
time_da_max = 5.0

nx = default.image.x								; size of image DA buffer
ny = default.image.y	
ix = 0												; redirection of X axis
iy = 1												; redirection of Y axis
iz = 2												; redirection of Z axis
													; flipX was already set in parent 'obj', but do it again to be explicit

; These explicit parameters only work because this is a Maia device object ...

(*pimage2D).DevObj->set_options, source_x=ix, source_y=iy, source_z=iz, flip_x=flipX
(*pm).DA.axes = [ix,iy,iz]

; This order also in maia_launch_update_DA_images, blog_client_da2, maia_launch (setup)
n_el_maia = n_elements(special_elements(/maia))
nel = (*pm).number.da + n_el_maia
str_elemements = ['none', special_elements(/maia), replicate(' ',nel-(n_el_maia+1)) ]

buffer_size2 = long([nx,ny,nel])	
pshrmem_da = shared_memory_buffers( prefix=prefix_da, n_buffers=1, $
					buffer_size=buffer_size2, /floatd, n_float=50, n_long=1000, /init, error=error)
if error then goto, bad_shrmem
b = byte(ip_blog)
n = n_elements(b)
;(*(*pshrmem_da).ppar)[2:3] = 0
(*(*pshrmem_da).ppar)[6:9] = [ix,iy,iz,flipX]		; axis redirection and X flip
(*(*pshrmem_da).pbyte[0])[*] = 0B
(*(*pshrmem_da).pbyte[0])[0:n-1] = b				; IP address
(*(*pshrmem_da).plong[0])[0] = port_blog			; blog port

; el		will come from DA matrix definition. Copy this to 'image.el' on DA load.
; 			Need to update names from maia_da_element_1 record, via (*(*pshrmem_da).pbyte[1:*])[].
; nel		from shared memory buffer-size - need to pad out el list, if short from DA matrix.
; 			Perhaps use blank element name to indicate end of valid element images.
; pactive	should come from Group(13) selection. Read it back?
; detector	from 'detector' in DA matrix. 
; cal		set here (and in individual spectra) from DA matrix 'pcal' load.
; compress	set this to 8000/xsize, 8000/ysize ?? What about Bigger scan areas?
; 			Need some entry fields on Imaging page for Compress (show max image size).
; 			These need to be sent to background process via shared memory.
; ERROR		throw an error if DA matrix is not a detector array.

if ptr_valid((*pshrmem_da).pdat[0]) then begin
	pimg = (*pshrmem_da).pdat[0]
endif else begin
	pimg = ptr_new(fltarr(nx,ny,nel))
endelse

;p = read_geopixe_image('c4-x-3-m.dai')
;els = *(*p).el
;n_els_maia = n_elements(special_elements(/maia))
;if (*p).n_el lt nel then els=[els,special_elements(/maia),replicate(' ',nel-((*p).n_el+n_els_maia))]
;free_images, p

image = define(/image)
image.n_el = nel
image.n_attributes = 2
image.el = ptr_new(str_elemements)
image.image = pimg
image.xsize = nx
image.ysize = ny
image.xcompress = 1
image.ycompress = 1
image.array = 1
image.detector = 7
image.charge = 0.0									; this indicates to use flux * conv
image.comment = 'Maia on-line DA image monitor'
image.facility = default.facility
image.endstation = default.endstation
image.source = 'Maia Realtime'
image.DevObj = clone_device_object(obj)

if ptr_good((*pimage_temp).pactive) then begin
	image.pactive = ptr_new( *(*pimage_temp).pactive)
endif
if ptr_good((*pimage_temp).pcal) then begin
	image.pcal = ptr_new( *(*pimage_temp).pcal)
endif
image.dwell = (*pimage_temp).dwell
image.scan = (*pimage_temp).scan
image.cal = (*pimage_temp).cal
	t = image.matrix
	struct_assign, (*pimage_temp).matrix, t			; collected during maia_launch_initial
image.matrix = t

pimage = ptr_new(image, /no_copy)
fix_options, pimage

;-------------------------------------------------------------------------------------

; 0 	    1	2	3	   4	 5		6
; activity  ET  DA  Epics  Group Params Slow-params
;
; Launch kandinski clients ...
;
;	Pass the 'server' to the child processes so they can log error messages to rsyslog
;	via the MMlibs Python library. This works fine for Linux, with system Python available.

maia_client_obj = objarr(2)
blog_client_obj = objarr(5)

if enable_maia then begin
	print,'Spawn new kandinski clients ...'
	log_message, comms, type='INFO', 'maia_launch startup, Spawn new kandinski clients ...'
	spawn_maia_clients, n_detectors, obj=maia_client_obj, prefix=maia_prefix, conf=default.conf, server=server, error=err
	if err then goto, kill_processes

; Check maia clients ...

	t = 0.0
	while (*(*pshrmem_pars).ppar)[5] eq 0 do begin
		print,'Wait for Parameters maia client to run ...'
		wait, 0.3
		t = t+0.3
		if t gt timeout_processes then begin
			warning,'maia_launch','Parameters client not running.'
			(*pshrmem_pars).error = 1
			break
		endif
	endwhile
	
;	The Slow process uses the parameters shared memory, and listens to extra ppar[6] variable

	t = 0.0
	print,'Wait for Slow Parameters maia client to run ...'
	while (*(*pshrmem_pars).ppar)[6] eq 0 do begin				; non-standard index used (6)
		print,'Wait for Slow Parameters maia client to run ...'
		wait, 0.3
		t = t+0.3
		if t gt timeout_processes then begin
			warning,'maia_launch','Slow Parameters maia client not running.'
;			(*pshrmem_pars).error = 1
			break
		endif
	endwhile
	
endif

;-------------------------------------------------------------------------------------

; 0 	    1	2	3	   4	 5		6
; activity  ET  DA  Epics  Group Params Slow-params
;
; Launch blog clients ...
;
;	Pass the 'server' to the child processes so they can log error messages to rsyslog
;	via the MMlibs Python library. This works fine for Linux, with system Python available.

if enable_blog then begin
	print,'Spawn new blog clients ...'
	log_message, comms, type='INFO', 'maia_launch startup, Spawn new blog clients ...'
	spawn_blog_clients, obj=blog_client_obj, prefix=maia_prefix, conf=default.conf, server=server, $
						enable_groups=((*pm).groups.spectra gt 0), error=err
	if err then goto, kill_processes

; Check blog clients ...

	t = 0.0
	while (*(*pshrmem_activity).ppar)[5] eq 0 do begin
		print,'Wait for Activity blog client to run ...'
		wait, 0.3
		t = t+0.3
		if t gt timeout_processes then begin
			warning,'maia_launch','Activity client not running.'
			(*pshrmem_activity).error = 1
			break
		endif
	endwhile
	
	t = 0.0
	while (*(*pshrmem_ET_spectra).ppar)[5] eq 0 do begin
		print,'Wait for ET spectra blog client to run ...'
		wait, 0.3
		t = t+0.3
		if t gt timeout_processes then begin
			warning,'maia_launch','ET_spectra client not running.'
			(*pshrmem_ET_spectra).error = 1
			break
		endif
	endwhile
	
	t = 0.0
	print,'Wait for DA images blog client to run ...'
	while (*(*pshrmem_da).ppar)[5] eq 0 do begin
		print,'Wait for DA images blog client to run ...'
		wait, 0.3
		t = t+0.3
		if t gt timeout_processes then begin
			warning,'maia_launch','DA images client not running.'
			(*pshrmem_da).error = 1
			break
		endif
	endwhile

;	The Epics process uses the DA shared memory, and listens to extra ppar[6] variable

	t = 0.0
	print,'Wait for Epics blog client to run ...'
	while (*(*pshrmem_da).ppar)[6] eq 0 do begin				; non-standard index used (6)
		print,'Wait for Epics blog client to run ...'
		wait, 0.3
		t = t+0.3
		if t gt timeout_processes then begin
			warning,'maia_launch','Epics client not running.'
;			(*pshrmem_da).error = 1								; must cooperate with DA
			break
		endif
	endwhile

	t = 0.0
	if (*pm).number.spectra gt 0 then begin
		while (*(*pshrmem_spectra).ppar)[5] eq 0 do begin
			print,'Wait for Group spectra blog client to run ...'
			wait, 0.3
			t = t+0.3
			if t gt timeout_processes then begin
				warning,'maia_launch','Group spectra client not running.'
				(*pshrmem_spectra).error = 1
				break
			endif
		endwhile	
	endif
endif
goto, start_launch

kill_processes:
	print,'Kill client objects ...'
	for i=0,n_elements(blog_client_obj)-1 do begin
		obj_destroy, blog_client_obj[i]
	endfor
	for i=0,n_elements(maia_client_obj)-1 do begin
		obj_destroy, maia_client_obj[i]
	endfor
	goto, bad_bridge

;-------------------------------------------------------------------------------------

start_launch:
log_message, comms, type='INFO', 'maia_launch startup, Start GUI ...'
case !version.os_family of
	'MacOS': begin
       symbol = 'SYMBOL*12'
       large_font = 'Arial*12'
       def_font = 'Geneva*10'
		fnt1 = 'HELVETICA*8'       ;'HELVETICA*9'
		csize = 1.3

		left_xsize = 80
		left_ysize = 685
		right_xsize = 150
		right_ysize = 720
		text_xsize = 120
		text_xsize2 = 140
		button_xsize = 70
		button_ysize = 27
		table_yscroll = 34
		led_space = 2
		heart_space = 2
		rspace = 4
		space1 = 1
		tspace5 = 5
		stats_xsize = 7.5*button_xsize
		help_xsize = 13.5*(button_xsize + 3) - 3 - 98 		; 238
		end
	'unix': begin
       symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
       large_font = '10x20'
       def_font = '6x13'
		fnt1 = 'timr08'             ;'6x12'
		csize = 1.3

		left_xsize = 80
		left_ysize = 685
		right_xsize = 150
		right_ysize = 720
		text_xsize = 120
		text_xsize2 = 140
		button_xsize = 70
		button_ysize = 27
		table_yscroll = 34
		led_space = 2
		heart_space = 2
		rspace = 4
		space1 = 1
		tspace5 = 5
		stats_xsize = 7.5*button_xsize
		help_xsize = 13.5*(button_xsize + 3) - 3 - 98		; 238
		end
	else: begin
       symbol = 'SYMBOL*BOLD*14'
       large_font = 'COURIER*BOLD*10'
       def_font = 'Arial*14'
		fnt1 = 'Helvetica*8'       ;'ARIAL*12'
		csize = 1.3

		left_xsize = 80
		left_ysize = 685
		right_xsize = 150
		right_ysize = 720
		text_xsize = 120
		text_xsize2 = 140
		button_xsize = 70
		button_ysize = 23
		table_yscroll = 34
		led_space = 3
		heart_space = 3
		rspace = 5
		space1 = 1
		tspace5 = 5
		stats_xsize = 5.5*button_xsize + 5
		help_xsize = 11.5*(button_xsize + 5) - 98	; 238
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

;	  select: 0     1      2      3
colours = [ grey, green, violet, violet]		; for ??
colours2 = [grey, green, yellow, red]			; for "buttons"

screen = get_screen_size()
;xoffset = (screen[0]/2 - (help_xsize+238+120)/2) > 0
xoffset = 180
yoffset = 0	;	(screen[1]-28 - 200)/2 > 0

; 	top-level base

s = '  ' + version + '  (' + name_maia + ' [' + (*pm).identity.software + ' ' + str_tidy((*pm).version.software) + '], ' + $
		'Maia: ' + (*pm).identity.dam + ', Lib: ' + str_tidy((*pm).identity.library) + ')  ' + default.facility + ' ' + default.endstation
log_message, comms, type='INFO', 'maia_launch, start GUI:'+s
tlb = widget_base( /column, title='Maia Control'+s, /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='maia-launch-tlb', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=1 ,YPAD=2 ,xoffset=xoffset, $
					yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
label = widget_label( tbase, value='Maia: CSIRO-BNL Advanced Fluorescence Detector and Imaging System')
t0base = widget_base( tbase, /row, xpad=0, ypad=0, space=0, /base_align_top, /align_center)

lbase = widget_base( t0base, /column, xpad=0, ypad=0, space=2, /base_align_center)
row0base = widget_base( lbase, /row, xpad=0, ypad=1, space=0, /base_align_top, /align_center)
led0_base = widget_base( row0base, /column, xpad=1, ypad=1, space=3, /frame, /base_align_left)
label = widget_label( led0_base, value='Status', /align_center)

if !version.os_family eq 'unix' then led0_base = widget_base( led0_base, /column, xpad=0, ypad=1, space=3, /base_align_left)
led = intarr(6)
lnames = geopixe_root + 'images' + path_sep() + ['led-off-12x14.jpeg','led-red-12x14.jpeg','led-green-12x14.jpeg']
led_base = widget_base( led0_base, /column, xpad=1, ypad=0, space=led_space, /align_center, /base_align_left)

lr0base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[0] = (*ps).open+1
led0 = picture_button( lr0base, lnames, uname='led0', value=led[0], /tracking, uvalue='Status LED0: Maia socket is Open status.')
label = widget_label( lr0base, value='Maia socket', /tracking, uvalue='Status LED0: Maia socket is Open status.')

lr1base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led1 = picture_button( lr1base, lnames, uname='led1', /tracking, uvalue='Status LED1: Hymod to DDM link operational status.')
label = widget_label( lr1base, value='DDM link', /tracking, uvalue='Status LED1: Hymod to DDM link operational status.')

lr2base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led2 = picture_button( lr2base, lnames, uname='led2', /tracking, uvalue='Status LED2: Kandinski is connected to Blog status.')
label = widget_label( lr2base, value='Maia to Blog', /tracking, uvalue='Status LED2: Kandinski is connected to Blog status.')

; Note that the LED[3] value needs to reflect all background processes that access BLOG ...

lr3base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
if (*pm).number.spectra gt 0 then begin
	led[3] = 1 + (enable_blog and (((*pshrmem_spectra).error or (*pshrmem_ET_spectra).error or (*pshrmem_activity).error or (*pshrmem_da).error) eq 0))
endif
led3 = picture_button( lr3base, lnames, uname='led3', value=led[3], /tracking, uvalue='Status LED3: Launch panel blog clients appear to be running.')
label = widget_label( lr3base, value='Blog clients', /tracking, uvalue='Status LED3: Launch panel blog clients appear to be running.')

lr4base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[4] = 0
led4 = picture_button( lr4base, lnames, uname='led4', value=led[4], /tracking, uvalue='Status LED4: Hardware interlocks.')
label = widget_label( lr4base, value='Interlock', /tracking, uvalue='Status LED4: Hardware interlocks.')

lr5base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led5 = picture_button( lr5base, lnames, uname='led5', /tracking, uvalue='Status LED5: status of detector bias and temperature (Green: Bias and T good; Red: bias or T bad; Black: Bias and Peltier Off).')
label = widget_label( lr5base, value='Bakeout', /tracking, uvalue='Status LED5: status of detector bakeout plug (Red: CAUTION! Bakeout plug inserted, Peltier drives detector HEATING; Black: Normal operation).')


rbase = widget_base( t0base, /column, xpad=0, ypad=0, space=space1, /base_align_center)

rowbase = widget_base( rbase, /row, xpad=1, ypad=1, space=1, /base_align_top, /align_center)

setup_base = widget_base( rowbase, /column, xpad=1, ypad=1, space=3, /frame, /base_align_center)
label = widget_label( setup_base, value='Setup')

maia_setup_button = state_button( setup_base, value='Maia', uname='setup-maia-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Setup Maia detector parameters, load calibration tables and run tests and calibration procedures.')
button = state_button( setup_base, value='Reset', uname='reset-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Reset Maia and restart blog service sub-processes.')

display_base = widget_base( rowbase, /column, xpad=1, ypad=1, space=3, /frame, /base_align_center)
label = widget_label( display_base, value='Display')

d1base = widget_base( display_base, row=2, xpad=0, ypad=0, space=3, /base_align_center, /align_center)
spectra_button = state_button( d1base, value='ET Spectra', uname='new-spectra-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open a GeoPIXE real-time spectrum window for the display of individual "Detector" spectra gathered from ET blog records.')
rates_button = state_button( d1base, value='Rates', uname='rates-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open the Maia detector count rate and process monitor window.')
images_button = state_button( d1base, value='Image', uname='new-images-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open a GeoPIXE real-time image display.')
blog_browse_button = state_button( d1base, value='Browse', uname='blogd-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open a Blog-Browser window to sample records from the blogd server.')
et2d_image_button = state_button( d1base, value='ET 2D', uname='new-et2d-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open a Maia ET 2D image window to display "E versus T" for events gathered from ET blog records.')
groups_button = state_button( d1base, value='Groups', uname='new-groups-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open a GeoPIXE real-time spectrum window for the display of "Group" spectra.')
multi_images_button = state_button( d1base, value='Multi', uname='new-multi-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open a Maia Multi-Image display.')
chart_button = state_button( d1base, value='Chart', uname='new-chart-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open a chart recorder display to show selected logged parameters.')

control_base = widget_base( rowbase, /column, xpad=1, ypad=1, space=3, /frame, /base_align_center)
label = widget_label( control_base, value='Control')
control_base2 = widget_base( control_base, column=2, xpad=0, ypad=0, space=3, /align_center, /base_align_center)

run_button = state_button( control_base2, value='Newrun', uname='run-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=3, colours=colours2, n_states=4, alt=0, $
					uvalue='Instruct Maia to issue a "newrun" record to blog, or to "endrun" if a run is currently active. ' + $
					'If newrun fails to start, discard may indicate a DDM fault.')
button = state_button( control_base2, value='Clear', uname='clear-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=2, colours=colours2, n_states=4, alt=0, $
					uvalue='Clear ALL local shared spectrum and image memory.')
;button = state_button( control_base2, value='Start', uname='start-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
;					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
;					uvalue='Resume event flow: enable "events".')
;button = state_button( control_base2, value='Endrun', uname='stop-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
;					/freeze, select=3, colours=colours2, n_states=4, alt=0, $
;					uvalue='Pause event flow: disable "events".')
run_stats = widget_list( control_base2, scr_xsize=stats_xsize, uname='control-stats', /tracking, value=strarr(3), $
				uvalue='Display Blog run number and segment and Blog received data rate in bytes. Click on "Group" to change blog data Group.', $
				frame=1, scr_ysize=2*button_ysize+tspace5)

hbase = widget_base( rbase, /row, xpad=0, ypad=0, space=rspace, /base_align_center, /align_center)
status = widget_text( hbase, scr_xsize=0.2*help_xsize-rspace, ysize=3, wrap=0, uname='status', /tracking, $
				uvalue='Detector status field.', frame=1)
help = widget_text( hbase, scr_xsize=0.8*help_xsize-4, ysize=3, /wrap, uname='help', /tracking, $
				uvalue='Context sensitive help: Move cursor over object for help.', frame=1)

picpath = geopixe_root + 'maia' + slash()
pic = picture_button( hbase, picpath + 'Maia-Logo2.png', tracking=0, pushbutton_events=0)	;, xsize=144, ysize=48


rhbase = widget_base( t0base, /column, xpad=0, ypad=0, space=2, /base_align_center)
row0base = widget_base( rhbase, /row, xpad=0, ypad=1, space=0, /base_align_top, /align_center)
heart0_base = widget_base( row0base, /column, xpad=1, ypad=1, space=3, /frame, /base_align_left)
query = widget_label( heart0_base, value=' ', /align_center, uname='query', /tracking, uvalue='  ')

if !version.os_family eq 'unix' then heart0_base = widget_base( heart0_base, /column, xpad=0, ypad=1, space=3, /base_align_left)
heart = intarr(6)
;hnames = ['images/led-off-12x14.jpeg','images/led-red-12x14.jpeg','images/led-green-12x14.jpeg']
heart_base = widget_base( heart0_base, /column, xpad=1, ypad=0, space=heart_space, /align_center, /base_align_left)

rr0base = widget_base( heart_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
heart0 = picture_button( rr0base, lnames, uname='heart0', /tracking, uvalue='Heart beat0: "Maia" loop timer. Update Maia parameters from ' + $
		'shared memory (parameters read from Maia by background processes), LED displays and check for Warning conditions.')
;label = widget_label( rr0base, value='Maia socket', /tracking, uvalue='Heart beat0: Maia socket is Open status.')

rr1base = widget_base( heart_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
heart1 = picture_button( rr1base, lnames, uname='heart1', /tracking, uvalue='Heart beat1: "Rates" loop timer. Update run status, progress and ' + $
		'details, some Warning conditions, save RT images and detect new run started and clear displays.')
;label = widget_label( rr1base, value='DDM link', /tracking, uvalue='Heart beat1: Hymod to DDM link operational status.')

rr2base = widget_base( heart_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
heart2 = picture_button( rr2base, lnames, uname='heart2', /tracking, uvalue='Heart beat2: "Chart" loop timer. Update parameters logged to Chart and ' + $
		'large Maia parameter arrays occasionally (parameters read into shared memory from Maia by background processes).')
;label = widget_label( rr2base, value='Maia to Blog', /tracking, uvalue='Heart beat2: Kandinski is connected to Blog status.')

rr3base = widget_base( heart_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
heart3 = picture_button( rr3base, lnames, uname='heart3', /tracking, uvalue='Heart beat3: "Spectra" loop timer. Update display of spectra and ET2D ' + $
		'display, reading directly from shared memory accumulated by ET background process.')
;label = widget_label( rr3base, value='Blog clients', /tracking, uvalue='Heart beat3: Launch panel blog clients appear to be running.')

rr4base = widget_base( heart_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
heart4 = picture_button( rr4base, lnames, uname='heart4', /tracking, uvalue='Heart beat4: "Groups" loop timer. Update Groups spectra display (if enabled).')
;label = widget_label( rr4base, value='Interlock', /tracking, uvalue='Heart beat4: Hardware interlocks.')

rr5base = widget_base( heart_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
heart5 = picture_button( rr5base, lnames, uname='heart5', /tracking, uvalue='Heart beat5: "Images" loop timer. Update RT Image display, reading directly ' + $
		'from shared memory updated by DA background process.')
;label = widget_label( rr5base, value='Bakeout', /tracking, uvalue='Heart beat5: status of detector bakeout plug (Red: CAUTION! Bakeout plug inserted, Peltier drives detector HEATING; Black: Normal operation).')


state = { $
		path:				ptr_new(path), $			; pointer to current path
		dpath:				ptr_new(dpath), $			; pointer to raw data path
		version:			version, $					; Maia launch version string
		debug:				debug, $					' debug mode enabled for IDLDE use
		default:			default, $					; defaults struct (read from Maia.conf)
		DevObj:				obj, $						; Maia device object
		prefix:				prefix, $					; Maia type prefix
		comms:				comms, $					; MM lib comms Python object (if KVS enabled)
		kvs:				kvs, $						; MM lib KVS Python object (=0 if not enabled)
		kvs_prefix:			kvs_prefix, $				; KVS prefix code string
		layout_file:		layout_file, $				; layout file name
		playout:			pd, $						; pointer to layout struct array
		pmaia:				pm, $						; pointer to maia parameters struct array
		pdisable:			pe, $						; pointer to maia disable detector channels
		preadout:			pr, $						; pointer to readout enables
		psocket:			ps, $						; pointer to maia socket port and command parameters
		prates:				prates, $					; pointer to maia count rates struct
		pmrates:			pmrates, $					; pointer to maia maximum count rates struct
		pchart:				ptr_new( {n:0L, file:log_file, init:0, time0:0.0d+0, p:ptrarr(1000,/allocate_heap)}), $	; pointer to chart parameters heap area
		pchart_spec:		ptr_new( /allocate_heap), $	; pointer to chart spec ptr array
		psetup:				ptr_new( /allocate_heap), $	; pointer to heap for Notify to Maia_setup.
		
		pshrmem_pars:		pshrmem_pars, $				; pointer to shared memory for Maia parameters update
		maia_client_obj:	maia_client_obj, $			; maia client IDL bridge objects
		log_timer:			3., $						; maia and blog client log read timer
		
		rate_last:			fltarr((*pm).n_detectors), $	; last count for rate determ
		time_maia:			time_maia, $				; Maia update slow timer event time interval
		time_maia2:			time_maia2, $				; Maia update fast timer event time interval
		time_maia3:			time_maia3, $				; Maia update slow chart timer interval
		pHYMOD_DEBUG:		ptr_new( /allocate_heap), $	; debug modes (pulser, synth, hermes, scepter, eblk, bake)
		
		blog: { ip:			ip_blog, $					; blogd IP
			port:			port_blog}, $				; blogd port
		blog_client_obj:	blog_client_obj, $			; blog client IDL bridge objects
			
		ppspec:				ptr_new(pspec,/no_copy), $	; pointer to ptr array of on-line Groups spectra 
		pshrmem_spectra:	pshrmem_spectra, $			; pointer to spectra shared memory struct
		prefix_spectra:		prefix_spectra, $			; prefix name string
		time_spectra:		time_spectra, $				; spectra timer event time interval
		time_spectra_min:	time_spectra_min, $			; min buffer read time
		time_spectra_max:	time_spectra_max, $			; max buffer read time
		time_spectra_update: time_spectra_update, $		; display update time

		pshrmem_activity:	pshrmem_activity, $			; pointer to spectra shared memory struct
		prefix_activity:	prefix_activity, $			; prefix name string
;		time_activity:		time_activity, $			; spectra timer event time interval
;		time_activity_min:	time_activity_min, $		; min buffer read time
;		time_activity_max:	time_activity_max, $		; max buffer read time
;		time_activity_update: time_activity_update, $	; display update time

		ppspec3:				ptr_new(pspec3,/no_copy), $	; pointer to ptr array of on-line ET2 Detectors E spectra 
		pshrmem_ET_spectra:		pshrmem_ET_spectra, $		; pointer to spectra shared memory struct
		prefix_ET_spectra:		prefix_ET_spectra, $		; prefix name string
		time_ET_spectra:		time_ET_spectra, $			; spectra timer event time interval
		time_ET_spectra_min:	time_ET_spectra_min, $		; min buffer read time
		time_ET_spectra_max:	time_ET_spectra_max, $		; max buffer read time
		time_ET_spectra_update: time_ET_spectra_update, $	; display update time

		ppspec4:				ptr_new(pspec4,/no_copy), $	; pointer to ptr array of on-line ET2 Detectors T spectra 
		pshrmem_ET_spectraT:	pshrmem_ET_spectraT, $		; pointer to spectra shared memory struct
		prefix_ET_spectraT:		prefix_ET_spectraT, $		; prefix name string
		n_channels4:		n_channels4, $				; # channels in T spectra

		pimage2D:			pimage2D, $					; pointer to on-line ET 2D "images" 
		pshrmem_ET2d:		pshrmem_ET2d, $				; pointer to  ET 2D shared memory struct
		prefix_ET2d:		prefix_ET2d, $				; prefix name string

		pimage:				pimage, $					; pointer to on-line DA images 
		pshrmem_da:			pshrmem_da, $				; pointer to DA images shared memory struct
		prefix_da:			prefix_da, $				; prefix name string
		time_da:			time_da, $					; DA timer event time interval
		time_da_min:		time_da_min, $				; min buffer read time
		time_da_max:		time_da_max, $				; max buffer read time
		time_da_update:		time_da_update, $			; display update time
		maia_IC_name:		'', $						; Maia flux IC PV name string

		tracking:			1, $						; tracking mode
		tracking_counter:	0, $						; tracking counter to suppress detector messages for a while
		pseed:				ptr_new(1L), $				; seed for randomu
		
		led:				led, $						; LED values
		heart:				heart, $					; Heart beat toggles
		
		time: { update: { $
					DA:		systime(/seconds), $		; last DA update 
					Maia:	systime(/seconds), $		; last Maia parameters update
					ET_spectra: systime(/seconds), $	; last spectrum update
					activity: systime(/seconds), $		; last activity update
					spectra: systime(/seconds)}, $		; last group spectra update
				display: { $
					DA:		systime(/seconds), $		; last DA display draw
					Maia:	systime(/seconds), $		; last Maia parameters display
					ET_spectra: systime(/seconds), $	; last spectrum display
					spectra: systime(/seconds)}}, $		; last Groups display
					
; Number indices for various processes:
;	update	DA			0		display:	image 		0	(normal window)
;			ET_spectra	1					ET_spectra	1
;			maia		2					multi		2	(multi image window)

;		activity: { group:	lonarr(16), $				; count rates now stored in "prates" struct
;			detector:		lonarr((*pm).n_detectors), $ ; counts (per second) for each detector
		activity: { $
			chip:			lonarr((*pm).n_detectors/32), $		; counts (per second) for each Scepter
			process: { DA:	0.0, $						; % time on DA update
					ET_spectra: 0.0, $					; % time on ET_spectra updates
					spectra: 0.0, $						; % time on groups updates
					activity: 0.0, $					; % time on activity updates
					maia:	0.0 }, $					; % time on Maia parameter updates
			buffers: { DA:	0.0, $						; % records lost for DA
					ET_spectra: 0.0, $					; % records lost for ET_spectra
					spectra: 0.0 }, $					; % records lost for groups
			display: { $
				ET_spectra: replicate({ TLB: 0L, $		; ET E spectra TLB ID
							ppercent: ptr_new()},64), $ ; ptr to % spectrum draw
				ET_spectraT: replicate({ TLB: 0L, $		; ET T spectra TLB ID
							ppercent: ptr_new()},64), $ ; ptr to % spectrum draw
				ET2d: replicate({ TLB: 0L, $			; ET2d TLB ID
							ppercent: ptr_new()},64), $ ; ptr to % ET 2D draw
				spectra: replicate({ TLB: 0L, $			; groups TLB ID
							ppercent: ptr_new()},64), $ ; ptr to % groups draw
				image: replicate({ TLB: 0L, $			; image TLB ID
							ppercent: ptr_new()},64), $ ; ptr to % image draw
				multi: replicate({ TLB: 0L, $			; multi TLB ID
							ppercent: ptr_new()},64)}}, $ ; ptr to % multi-image draw
		
		enable_activity:	enable_activity, $			; enable new activity accumulator
		enable_project_select:	enable_project_select, $ ; enable "project" change
		
		maia_setup_button:	maia_setup_button, $		; maia setup button ID
		spectra_button:		spectra_button, $			; spectra button ID 
		rates_button:		rates_button, $				; rates button ID
		groups_button:		groups_button, $			; groups button ID
		images_button:		images_button, $			; images button ID
		heart_id:			[heart0,heart1,heart2,heart3,heart4,heart5,query], $	; heart widget IDs
		led_id:				[led0,led1,led2,led3,led4,led5], $	; LED widget IDs
		run_button: 		run_button, $				; run button ID
		run_stats:			run_stats, $				; run stats text widget ID
		status:				status, $					; detector status field
		alarm_interlock_popup:	0L, $					; ID of interlock alarm popup TLB
		alarm_temp_popup:		0L, $					; ID of temp alarm popup TLB
		alarm_leakage_popup:	0L, $					; ID of leakage alarm popup TLB
		alarm_loss_popup:		0L, $					; ID of loss alarm popup TLB
		alarm_disk_popup:		0L, $					; ID of storage alarm popup TLB
		alarm_rate_popup:		0L, $					; ID of blog zero rate alarm popup TLB
		help:				help $						; help text ID
		}

; This code to extend ptr array and initialize chart structs also in Maia_chart_add
; and maia_update_parameters3.

for i=0L,1000-1 do begin
	p1 = ptrarr(10,/allocate_heap)
	for k=0L,10-1 do *p1[k] = {name:'', index:0, val:0.0}
	(*state.pchart).p[i] = ptr_new({n:0L, time:0.0d+0, p:p1})
endfor
state.activity.display.ET_spectra.ppercent = ptrarr(64,/allocate_heap)
state.activity.display.ET_spectraT.ppercent = ptrarr(64,/allocate_heap)
state.activity.display.ET2d.ppercent = ptrarr(64,/allocate_heap)
state.activity.display.spectra.ppercent = ptrarr(64,/allocate_heap)
state.activity.display.image.ppercent = ptrarr(64,/allocate_heap)
state.activity.display.multi.ppercent = ptrarr(64,/allocate_heap)
for i=0L,63 do *state.activity.display.ET_spectra[i].ppercent = 0.0
for i=0L,63 do *state.activity.display.ET_spectraT[i].ppercent = 0.0
for i=0L,63 do *state.activity.display.ET2d[i].ppercent = 0.0
for i=0L,63 do *state.activity.display.spectra[i].ppercent = 0.0
for i=0L,63 do *state.activity.display.image[i].ppercent = 0.0
for i=0L,63 do *state.activity.display.multi[i].ppercent = 0.0

;state.activity.group = long(100000.*randomu(*state.pseed,16))
;state.activity.detector = long(5000.*randomu(*state.pseed,(*pm).n_detectors))

maia_launch_update_cal, state.ppspec3, state.ppspec, state.pimage, pm, pd

;---------------------------------------------------------------------------------------------------

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state)
widget_control, tlb, /realize

widget_control, maia_setup_button, timer=time_maia				; start slow maia update timer
widget_control, rates_button, timer=time_maia2					; start fast maia update timer
widget_control, chart_button, timer=time_maia3					; start slow maia chart update timer
if enable_blog then begin
	if (*pm).number.spectra gt 0 then begin
		if (*pshrmem_spectra).error eq 0 then begin
			widget_control, groups_button, timer=time_spectra			; start group spectra update timer
		endif
	endif
	if (*pshrmem_ET_spectra).error eq 0 then begin
		widget_control, spectra_button, timer=time_ET_spectra		; start detector spectra update timer
	endif
	if (*pshrmem_da).error eq 0 then begin
		widget_control, images_button, timer=time_da				; start DA update timer
	endif

;	The read from the process pipes (in "log_client") causes IDL to hang, so commant out for now ...
;	for i=0,6 do begin
;		widget_control, state.heart_id[i], timer=state.log_timer	; start log timers
;	endfor
endif
register_notify, tlb, ['path', $									; new path
						'select-highlight', $						; highlight spectrum #
						'warn-setup'], $							; warning status from Setup
						from=group

xmanager, 'maia_launch', tlb, /no_block
return

bad_shrmem:
	warning,'maia_launch','error shrmem allocate'
	goto, bad_setup
bad_blog_client:
	warning,'maia_launch','blog client not running'
	goto, bad_setup
bad_bridge:
	warning,'maia_launch',['error launching background processes.', '', $
		'IDL Bridge may have failed due to license restrictions.']
	goto, bad_setup
bad_setup:
	close_file, (*ps).unit
	return
end
