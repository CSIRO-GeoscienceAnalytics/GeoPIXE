pro daq_launch_event, event

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
		warning,'daq_launch_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return				; goto, kill
	endif
endif

@daq_scratch.def

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
pm = (*pstate).pdaq
pr = (*pstate).preadout
ps = (*pstate).psocket
psm = (*pstate).pmsocket
play = (*pstate).playout
pimage = (*pstate).pimage
pp_et = (*pstate).ppspec3
obj = (*pstate).DevObj
axes = ['DACx','DACy','StageX','StageY','StageZ','StageA']

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
			(*pstate).tracking_counter = 4
		endif else begin
			widget_control, (*pstate).help, set_value='DAQ detector launch panel: "Setup" DAQ, display data and control detector.'
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
			'spectrum-display-cleared': begin
				daq_launch_clear, pstate, event, /spectra
				end
			'spectrum-cal': begin								; from DAQ-Setup when Cals sent to Hymod
				daq_launch_update_cal, (*pstate).ppspec3, (*pstate).ppspec, (*pstate).pimage, pm, play
				notify,'spectrum-display', from=event.top		; redraw Spectra display
				notify,'spectrum-changed', from=event.top		; Spectra-select update
				end
			'spectrum-get-cals': begin							; "Get cals" from spectrum-display
				daq_launch_read_cal, ps, pm, play				; first read from DAQ
				daq_launch_update_cal, pp_et, pp_group, pimage, pm, play
				notify,'spectrum-display', from=event.top		; redraw Spectra display
				notify,'spectrum-changed', from=event.top		; Spectra-select update
				end
			'master-set': begin
				(*pstate).lock_master = 1						; lock it to stop timer changing it
				(*pstate).master = *event.pointer
				if (*pstate).master then begin
					v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_master, /string, error=err)
					if err eq 0 then begin
						if v ne (*pstate).local_id then begin
							(*pstate).prev_id = v
						endif else begin
							(*pstate).prev_id = ''
						endelse
					endif
					socket_command_set, ps, 'value', (*pstate).local_id, class='scratch.datum', chip=scratch_master
				endif else begin
					socket_command_set, ps, 'value', (*pstate).prev_id, class='scratch.datum', chip=scratch_master
				endelse
				(*pstate).lock_master = 0
				end
			'scan-sequence-started': begin
				print,'daq_launch: start sequence'
				(*pstate).scan_sequence.active = 1
				(*pstate).scan_sequence.raster.on = 1
				end
			'scan-sequence-complete': begin
				print,'daq_launch: sequence completed'
				(*pstate).scan_sequence.active = 0
				(*pstate).scan_sequence.raster.on = 0
				end
			'warn-setup': begin
				cols = [0,2,3]
				widget_control, (*pstate).daq_setup_button, set_value={select:cols[*event.pointer]}
				end
			else:
		endcase
		end
	'WIDGET_TIMER': begin
		u = widget_info( event.id, /uname)
		case u of
			'setup-daq-button': begin
				daq_update_parameters, pstate, update=update, error=error
				if update then begin
					*(*pstate).psetup = *(*pstate).pHYMOD_debug 
					notify, 'daq-display', (*pstate).psetup, from=event.top
					notify, 'daq-process', from=event.top
					widget_control, (*pstate).daq_setup_button, set_value={select:hymod_warning_colour( *(*pstate).pHYMOD_debug)}
				endif
				
;				s = ['Detector bias = '+str_tidy((*pm).control.bias_monitor, length=5)+' V', $
;					'Leakage current = '+str_tidy((*pm).control.leakage, length=5)+' uA', $
;					'Detector Temp = '+str_tidy((*pm).control.temp.detector, length=5)+' C']
;				if (*pstate).tracking_counter eq 0 then widget_control, (*pstate).help, set_value=s
				(*pstate).tracking_counter = ((*pstate).tracking_counter - 1) > 0
				
;				Test for raster.status to be 'run' (1) or 'pause' (2): If not, do we go to next scan?

				(*pm).control.status.raster_status = daq_launch_raster_status( ps, pm, led=led)
				(*pstate).led[5] = led

				for i=0L,5 do begin
					widget_control, (*pstate).led_id[i], set_value=(*pstate).led[i]
				endfor
				
				(*pstate).scan_sequence.raster.on = ((*pm).control.status.raster_status eq 1) or ((*pm).control.status.raster_status eq 2)
				if (*pstate).scan_sequence.active and ((*pstate).scan_sequence.raster.on eq 0) then begin
					socket_command_set, ps, 'endrun', 1, class='blog', error=err
					if (*pm).control.status.raster_status eq 3 then begin
						warning,'daq_launch',['Error returned from Klee from raster.status.', 'Abort scan sequence.']
						*(*pstate).pcom = 'stop'
						notify, 'scan-command', (*pstate).pcom, from=event.top
					endif else begin
						notify, 'scan-sequence-next', from=event.top
					endelse
					(*pstate).scan_sequence.active = 0
				endif
				widget_control, event.id, timer=(*pstate).time_daq	
				end
			'rates-button': begin
				daq_update_parameters2, pstate, update=update, error=error		; these are part of 'daq-rates' notify update
				if update then notify, 'daq-rates', from=event.top
				s1 = 'Run:  ' + str_tidy((*pm).run.number) + '.' + str_tidy((*pm).run.segment) 
;				s1 = s1 + '  XY: ' + str_tidy(round((*pm).run.position[obj->get_Xaxis()])) + ', ' + str_tidy(round((*pm).run.position[obj->get_Yaxis()]))
				s1 = s1 + '  XY: ' + str_tidy((*pm).run.pixel[obj->get_Xaxis()]) + ', ' + str_tidy((*pm).run.pixel[obj->get_Yaxis()])
				s1 = s1 + ' (' + axes[obj->get_Xaxis()] + ', ' + axes[obj->get_Yaxis()] + ')'
				s2 = 'Project: ' + (*pm).run.project + ', Group: ' + (*pm).run.Group
				widget_control, (*pstate).run_stats, set_value=[s1,'Rate:   '+rates_string((*pm).run.rate,/bytes),s2]

				case (*pm).run.discard of
					0: widget_control, (*pstate).run_button, set_value={value:"Endrun", select:1}
					1: widget_control, (*pstate).run_button, set_value={value:"Newrun", select:3}
				endcase

				if n_elements(*(*pm).IC.plist) eq 0 then begin
					*(*pm).IC.plist = 'none'
				endif else begin
					if size(*(*pm).IC.plist, /tname) ne 'STRING' then begin
						*(*pm).IC.plist = 'none'
					endif
				endelse
				
;				Hard to detect (*pm).run.discard=1 as it may last for a short time.
;				Instead look for a change in run number for the clear. However, (*pm).run.discard=0 will
;				last longer and can be detected for the save.

				if ((*pm).run.number ne (*pm).run.last_run) or  $							; start new run
					((*pm).run.last_discard eq 0) and ((*pm).run.discard eq 1) then begin	; run ended
					run = (*pm).run.number
					if ((*pm).run.number ne (*pm).run.last_run) then run = (*pm).run.last_run
					
					if (*pm).DA.save and ((*pm).DA.save_path ne '') then begin
;						daq_launch_read_da, ps, pm, pimage, /scan, /deadtime, /init		; update image dwell
;						daq_launch_read_da_rGamma, ps, pm								; needed here cos not in 'daq_launch_read_da' now
						daq_launch_read_enable, ps, pm, pr, pimage, play				; update image *pactive
						daq_launch_update_image_cal, pimage, pm, play					; update image *pcal, cal
						q = where( *(*pimage).el eq 'Flux', nq)
						if nq gt 0 then begin
							raw_flux = (*(*pimage).image)[*,*,q[0]]
							image_correct_flux, pimage, (*(*pimage).image)[*,*,q[0]], (*pm).IC, *(*pm).IC.plist, /flatten, raw=raw_flux
						endif

						F = (*pm).DA.save_path + str_tidy(run) + '-RT.dai'
						(*pimage).file = F
						(*pimage).source = (*pm).run.group + str_tidy(run) + '.0'
						(*pimage).source2 = (*pm).run.group + str_tidy(run) + '.' + str_tidy((*pm).run.segment)
						(*pimage).has_preview = 0
						if ptr_valid((*pimage).preview) then ptr_free, (*pimage).preview
						obj->set_options, deadtime_cal=(*pm).deadtime.cal

						write_geopixe_image, pimage, F, /no_null, /check_bounds
						notify, 'image-display', from=event.top
						
;						Clear items set in 'image_correct_flux' and 'write_geopixe_image' ...						
						(*pimage).has_flux = 0
						if ptr_valid((*pimage).flux) then ptr_free, (*pimage).flux
						if ptr_valid((*pimage).raw_flux) then ptr_free, (*pimage).raw_flux
						(*pimage).charge = 0.0
						(*pimage).temp.valid = 0
					endif
				endif

				if ((*pm).run.number ne (*pm).run.last_run)	then begin						; start new run
					(*pimage).flatten = 0
					daq_launch_clear, pstate, event
;					daq_launch_read_da, ps, pm, pimage, /scan, /deadtime, /init
				endif

				(*pm).run.last_run = (*pm).run.number
				(*pm).run.last_discard = (*pm).run.discard
				widget_control, event.id, timer=(*pstate).time_daq2
				end
			'run-button': begin
;				daq_update_parameters3, pstate, update=update, error=error
				widget_control, event.id, timer=(*pstate).time_daq3
				end
			'new-spectra-button': begin
				if (*(*(*pstate).pshrmem_ET_spectra).ppar)[2] eq 0 then begin
					daq_update_ET_spectra, pstate, update=update, error=error
					if (*pstate).enable_activity eq 0 then notify, 'daq-rates', from=event.top
					if update then begin
;						print,'time_et: ',(*pstate).time_et_spectra,(*pstate).time_et_spectra_update  
						p = (*pstate).pimage2D
						set_image_minmax, p, (*p).image, (*p).options
						notify, 'spectrum-display', from=event.top
						notify, 'et2d-display', from=event.top
					endif
				endif
				widget_control, event.id, timer=(*pstate).time_ET_spectra	
				end
			'new-images-button': begin
				if (*(*(*pstate).pshrmem_da).ppar)[2] eq 0 then begin
					daq_update_da2, pstate, update=update, error=error
					if update then begin
						p = (*pstate).pimage
						set_image_minmax, p, (*p).image, (*p).options
						notify, 'image-display', from=event.top
					endif
				endif
				widget_control, event.id, timer=(*pstate).time_da	
				end
			'list-stage-button': begin
				v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_master, /string, error=err)
				if ((*pstate).lock_master eq 0) and (err eq 0) then begin
					current_master = v
;					print, 'current_master = ', v
					if (*pstate).local_id eq current_master then begin
						(*pstate).master = 1
					endif else begin
						(*pstate).master = 0
					endelse
					*(*pstate).pmaster = (*pstate).master
					notify, 'master', (*pstate).pmaster, from=event.top
				endif

				if (*pstate).master then begin
					daq_launch_scan_request, ps, pm, (*pstate).pstage, (*pstate).tlb, (*pstate).pcom,  update=update
					if update then notify, 'scan-list-update', from=event.top					; update scan list display
				endif
				widget_control, (*pstate).list_stage_button, timer=(*pstate).time_scan		; start new scan request timer
				end
			else:
		endcase
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request DAQ-Launch ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'daq-launch-tlb': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_BASE': begin
				case !version.os_family of
					'MacOS': begin
						xh = (event.x - 341) > 450
						xs = xh - 216
						end
					'unix': begin
						xh = (event.x - 341) > 450
						xs = xh - 216
						end
					else: begin
						xh = (event.x - 349) > 450			; 585
						xs = xh - 248
						end
				endcase
				widget_control, (*pstate).run_stats, scr_xsize=xs
				widget_control, (*pstate).help, scr_xsize=xh
				end
			else:
		endcase
		end

	'setup-daq-button': begin
		daq_setup, group_leader=event.top, tlb=tlb, data=play, daq=pm, port=ps, debug=(*pstate).debug, $
			disable=(*pstate).pdisable, readout=pr, path=*(*pstate).path, $		; pshrmem_da=(*pstate).pshrmem_da, $
			default=(*pstate).default, pimage = (*pstate).pimage
		register_notify, event.top, $
								['path', $					; new path
								'warn-setup', $				; changed warnings on DAQ setup window
								'image-elements', $			; new element list for images
								'spectrum-cal' $			; transmit ET spectra cal
								], from=tlb
		end
		
	'rates-button': begin
		daq_rates, group_leader=event.top, tlb=tlb, layout=play, daq=pm, default=(*pstate).default, $
			disable=(*pstate).pdisable, data=(*pstate).prates, path=*(*pstate).path, /tracking
		register_notify, event.top, $
								['path', $					; new path
								'detector-select', $		; detector selection on rates mimic display
								'warn-setup' $				; changed warnings on DAQ setup window
								], from=tlb
		end
		
	'blogd-button': begin
		blog_browser, group=event.top, /realtime, ip=(*pstate).blog.ip, port=(*pstate).blog.port, $
							version=(*pstate).version			;, dt=(*pm).deadtime.cal.a
		end
		
	'reset-button': begin
;		socket_retry, ps, error=error
;		(*ps).attempts = 1

		(*(*pstate).pshrmem_ET_spectra).loop = 0
		(*(*(*pstate).pshrmem_ET_spectra).ppar)[2] = 1		; send reset to blog client
		print,'Set ET_spectra blog client reset flag ...'
;		(*(*pstate).pshrmem_da).loop = 0
;		(*(*(*pstate).pshrmem_da).ppar)[2] = 1				; send reset to blog client
;		print,'Set DA blog client reset flag ...'
		(*(*pstate).pshrmem_activity).loop = 0
		(*(*(*pstate).pshrmem_activity).ppar)[2] = 1		; send reset to blog client
		print,'Set activity blog client reset flag ...'
		
		(*pr).quad_enable[*] = 1
		(*pr).event = 1
		print,'Set DAQ enables ...'
		socket_command_set, ps, 'enable', 1, class='event.blog'
		socket_command_set, ps, 'enable', 1, class='photon'
		socket_command_set, ps, 'enable', (*pr).quad_enable[0], class='scepter.clock'
;		socket_command_set, ps, 'enable', 1, class='deadtime'
		socket_command_set, ps, 'enable', 1, class='pixel'
		
		if ((*pm).version.scepter ge 7) then begin
			print,'Set SCEPTER enable ...'
			socket_command_set, ps, 'enable', 1, class='scepter'		; SPI overhead, do not use with old 96!
		endif
		socket_command_set, ps, 'enable', 1, class='scepter'	
		socket_command_set, ps, 'enable', 1, class='activity.blog'
		socket_command_set, ps, 'enable', 1, class='photon.chan', n_chips=(*ps).n_detectors, chip=-1
		socket_command_set, ps, 'period', 0.5, class='activity'
	
		socket_command_set, ps, 'tsedge', 'leading', class='nimadc.chan', n_chips=4, chip=-1
		socket_command_set, ps, 'cdtinv', 0, class='nimadc.chan', n_chips=4, chip=-1				; 0=Canberra, 1=Ortec
		
		daq_launch_initial, ps, pm, play, pr, pimage
;		*(*pstate).psetup = *(*pstate).pHYMOD_debug 
;		notify, 'daq-display', (*pstate).psetup, from=event.top
		end
		
	'new-spectra-button': begin
		i = launch_free_activity( (*pstate).activity.display.ET_spectra) 
		p = (*(*pstate).ppspec3)[0]
		emax = (*p).cal.poly[0] + ((*p).size-1) * (*p).cal.poly[1]
		title = 'DAQ E Spectra Display'
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
								'spectrum-cal', $				; transmit ET/Group spectra cal
								'spectrum-display-cleared' $	; changed (cleared) chart
;								'warn-setup' $					; changed warnings on DAQ setup window
								], from=tlb

		i = launch_free_activity( (*pstate).activity.display.ET_spectraT) 
		p = (*(*pstate).ppspec4)[0]
		emax = (*p).cal.poly[0] + ((*p).size-1) * (*p).cal.poly[1]
		title = 'DAQ T Spectra Display'
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
								['path' $					; new path
;								'warn-setup' $				; changed warnings on DAQ setup window
								], from=tlb
		end
		
	'new-et2d-button': begin
		i = launch_free_activity( (*pstate).activity.display.ET2d) 
		title = 'DAQ ET 2D Display'
		if i ge 0 then begin
			ET2d_image, group_leader=event.top, tlb=tlb, pimage=(*pstate).pimage2D, pm=pm, $
						title=title, ppercent=(*pstate).activity.display.et2d[i].ppercent
			(*pstate).activity.display.et2d[i].TLB = TLB
		endif else begin
			ET2d_image, group_leader=event.top, tlb=tlb, pimage=(*pstate).pimage2D, pm=pm
		endelse
		register_notify, event.top, $
								['path' $					; new path
;								'warn-setup' $				; changed warnings on DAQ setup window
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
;								'warn-setup' $				; changed warnings on DAQ setup window
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
		
	'list-stage-button': begin
		stage_list, group_leader=event.top, tlb=tlb, data=(*pstate).pstage, daq=pm, port=ps, $
				debug=(*pstate).debug, path=*(*pstate).path, mport=psm, pbeam=(*pstate).pbeam, $
				master=(*pstate).master
		register_notify, event.top, $
								['path', $					; new path
								'master-set', $					; change master state
								'scan-sequence-started', $	; a new scan sequence has started
								'scan-sequence-complete' $	; scan sequence is completed
								], from=tlb
		end

	'stop-stage-button': begin
		socket_command_set, ps, 'stop', 1, class='stage.axis', n_chips=3, chip=-1
		*((*pstate).pcom)='stop'
		notify, 'scan-command', (*pstate).pcom, from=tlb
;		clear various timer sequence flags?
		end
		
	'start-button': begin									; not used now, move code to "reset"
		(*pr).quad_enable[*] = 1
		(*pr).event = 1
		print,'Set DAQ enables ...'
		socket_command_set, ps, 'blog.enable', (*pr).event, class='event'
		socket_command_set, ps, 'enable', 1, class='photon'
		socket_command_set, ps, 'enable', (*pr).quad_enable[0], class='scepter.clock'
		socket_command_set, ps, 'enable', 1, class='pixel'
		socket_command_set, ps, 'enable', 1, class='activity.blog'

		if ((*pm).version.scepter ge 7) then begin
			print,'Set DAQ SCEPTER enable ...'
			socket_command_set, ps, 'enable', 1, class='scepter'		; SPI overhead, do not use with old 96!
		endif
		
		daq_launch_initial, ps, pm, play, pr, pimage
;		*(*pstate).psetup = *(*pstate).pHYMOD_debug 
;		notify, 'daq-display', (*pstate).psetup, from=event.top
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
		daq_launch_clear, pstate, event
		end
		
	'control-stats': begin
		case event.index of
			2: begin
				text = [(*pm).run.group]
				label = ['"Group" for next run']
				string_edit, event.top, title='Specify Blog "Group" subdirectory for next run', $
						label=label, text=text, error=err
				if err eq 0 then begin
					(*pm).run.group = text[0]
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
	warning,'daq_launch_event',['STATE variable has become ill-defined.','Abort Batch Sort.'],/error
	goto, kill

kill:
	ErrorNo = 0
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		goto, die
	endif

	print,'Signal clients to die ...'
	(*(*(*pstate).pshrmem_ET_spectra).ppar)[3] = 1				; kill ET spectra blog_client
	(*(*(*pstate).pshrmem_activity).ppar)[3] = 1				; kill Activity blog_client
;	(*(*(*pstate).pshrmem_da).ppar)[3] = 1						; kill DA blog_client
	(*(*(*pstate).pshrmem_pars).ppar)[3] = 1					; kill ET pars DAQ_client
	wait, 2

	print,'Kill clients ...'
	for i=0,n_elements((*pstate).blog_client_luns)-1 do begin
		if (*pstate).blog_client_luns[i] ne 0 then close_file, (*pstate).blog_client_luns[i], /force
	endfor
	for i=0,n_elements((*pstate).daq_client_luns)-1 do begin
		if (*pstate).daq_client_luns[i] ne 0 then close_file, (*pstate).daq_client_luns[i], /force
	endfor
	
	widget_control, hourglass=1
	wait, 3

	widget_control, event.top, /destroy
	widget_control, hourglass=0

	print,'Close socket ...'
	close_file, (*ps).unit
	if ptr_valid( ps) then ptr_free, ps

	if ptr_valid( (*pstate).pshrmem_ET_spectra) then ptr_free, (*pstate).pshrmem_ET_spectra
	if ptr_valid( (*pstate).pshrmem_ET_spectraT) then ptr_free, (*pstate).pshrmem_ET_spectraT
	if ptr_valid( (*pstate).pshrmem_ET2d) then ptr_free, (*pstate).pshrmem_ET2d
	if ptr_valid( (*pstate).pshrmem_activity) then ptr_free, (*pstate).pshrmem_activity
;	if ptr_valid( (*pstate).pshrmem_da) then ptr_free, (*pstate).pshrmem_da
	if ptr_valid( (*pstate).pshrmem_pars) then ptr_free, (*pstate).pshrmem_pars

	print,'Cleanup DAQ Launch ...'
	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).dpath) then ptr_free, (*pstate).dpath
	if ptr_valid( play) then ptr_free, play
	if ptr_valid( pm) then ptr_free, pm
	if ptr_valid( (*pstate).pdisable) then ptr_free, (*pstate).pdisable
	if ptr_valid( play) then ptr_free, play
	if ptr_valid( (*pstate).pseed) then ptr_free, (*pstate).pseed
	if ptr_valid( (*pstate).prates) then ptr_free, (*pstate).prates
	free_spectra, (*pstate).ppspec3
	free_spectra, (*pstate).ppspec4
	free_images, (*pstate).pimage
	free_images, (*pstate).pimage2D
	
;	... need to free (*pstate).activity ptr array data ...
;	
;	If shared memory is unmapped in this process, then it can't be
;	mapped again with the same name. Hence, only unmap them if the
;	sizes are to change. Then also kill the processes and re-start.

;	shared_memory_unmap, prefix=(*pstate).prefix_et2_spectra, n_buffers=(*(*pstate).pshrmem_ET_spectra).n_buffers
;	shared_memory_unmap, prefix=(*pstate).prefix_da, n_buffers=(*(*pstate).pshrmem_da).n_buffers

die:
;	heap_gc
	return
end

;----------------------------------------------------------------------------------

pro daq_launch_clear, pstate, event, spectra=spectra, images=images

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
		warning,'daq_launch_clear',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(images) lt 1 then images=0
	if n_elements(spectra) lt 1 then spectra=0
	if (images eq 0) and (spectra eq 0) then begin
		images = 1
		spectra = 1
	endif
	pimage = (*pstate).pimage

	if images then begin
		pshrmem = (*pstate).pshrmem_da
		if (*pshrmem).error eq 0 then begin	
			(*(*pshrmem).ppar)[2] = 1						; reset DA images blog_client
		endif												; which clears image and pars
		
		(*pimage).processed = 0
		(*pimage).valid = 0
		(*pimage).bad_xy = 0
		(*pimage).clipped = 0
		(*pimage).bounds.xmin = 0L
		(*pimage).bounds.ymin = 0L
		(*pimage).bounds.xmax = 0L
		(*pimage).bounds.ymax = 0L
		(*pimage).temp.valid = 0
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
			(*pd)[*,*] = 0.0								; in shared memory
			pd4 = (*pshrmem4).pdat[0]						; clear T spectra 2D buffer
			(*pd4)[*,*] = 0.0								; in shared memory
			notify, 'spectrum-display', from=event.top
			pd2 = (*pshrmem2).pdat[0]						; clear ET2D image buffer
			(*pd2)[*,*] = 0.0								; in shared memory
			notify, 'et2d-display', from=event.top
			(*(*pshrmem).ppar)[2] = 1						; reset E spectra blog_client
		endif
	endif
	return
end

;----------------------------------------------------------------------------------

pro daq_launch_initial, ps, pm, play, pr, pimage, default, error=error

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
		warning,'daq_launch_initial',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_null_image_1, max_image_cal
if n_elements(max_image_cal) lt 1 then max_image_cal = 8

;	Set-up 'scratch' variables in Klee.

@daq_scratch.def

if (*ps).open eq 1 then begin
	socket_command_set, ps, 'key', 'DAQ:cal', class='scratch.datum', chip=scratch_cal
	socket_command_set, ps, 'key', 'DAQ:DA', class='scratch.datum', chip=scratch_DA
	socket_command_set, ps, 'key', 'DAQ:ROI', class='scratch.datum', chip=scratch_ROI
	socket_command_set, ps, 'key', 'DAQ:gaintrim', class='scratch.datum', chip=scratch_gaintrim
	socket_command_set, ps, 'key', 'DAQ:group', class='scratch.datum', chip=scratch_group
	socket_command_set, ps, 'key', 'DAQ:linearise', class='scratch.datum', chip=scratch_linearise
	socket_command_set, ps, 'key', 'DAQ:pileup', class='scratch.datum', chip=scratch_pileup
	socket_command_set, ps, 'key', 'DAQ:throttle', class='scratch.datum', chip=scratch_throttle
	socket_command_set, ps, 'key', 'DAQ:deadtime', class='scratch.datum', chip=scratch_deadtime
	socket_command_set, ps, 'key', 'DAQ:velocity', class='scratch.datum', chip=scratch_velocity
	socket_command_set, ps, 'key', 'DAQ:master', class='scratch.datum', chip=scratch_master

	if ((*pm).version.scepter ge 7) then begin
		print,'Set SCEPTER enable ...'
		socket_command_set, ps, 'enable', 1, class='scepter'		; SPI overhead, do not use with old 96!
	endif
	socket_command_set, ps, 'enable', 1, class='scepter.clock'

	socket_command_set, ps, 'enable', 1, class='event.blog'
	socket_command_set, ps, 'enable', 1, class='activity.blog'

	socket_command_set, ps, 'enable', 1, class='beam.photon'
	socket_command_set, ps, 'enable', 1, class='beam'
	socket_command_set, ps, 'enable', 1, class='photon'
	socket_command_set, ps, 'enable', 1, class='photon.chan', n_chips=(*ps).n_detectors, chip=-1
;	socket_command_set, ps, 'period', 0.5, class='activity'

	socket_command_set, ps, 'tsedge', 'leading', class='nimadc.chan', n_chips=4, chip=-1
	socket_command_set, ps, 'cdtinv', 0, class='nimadc.chan', n_chips=4, chip=-1		; 0=Canberra, 1=Ortec
	
	; Read back Klee values ...
	
	v1  = socket_command_get( ps, 'velocity.max', class='stage.axis', chip=-1, n_chip=4, error=err)
	if err eq 0 then begin
		(*pm).stage.max.velocity = v1
	endif
	v1  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_velocity, /string, error=err)
	if err eq 0 then begin
		velocity = unstringify(v1[0])
		if n_elements(velocity) lt 4 then velocity = [2.,2.,2.,2.]
		(*pm).stage.velocity = velocity
	endif
	v1  = socket_command_get( ps, 'energy', class='metadata.beam', /float, error=err)
	if err eq 0 then begin
		(*pm).beam.energy = v1 * 1.0e-6
	endif
	v1  = socket_command_get( ps, 'particle', class='metadata.beam', /string, error=err)
	if err eq 0 then begin
		(*pm).beam.particle = v1
	endif
	v1  = socket_command_get( ps, 'coeff', class='charge', /float, error=err)
	if err eq 0 then begin
		(*pm).beam.charge.scale = v1[0]
		(*pm).IC.pv.val = v1[0]
	endif
	v1  = socket_command_get( ps, 'unit', class='charge', /string, error=err)
	if err eq 0 then begin
		(*pm).beam.charge.unit = v1[0]
		sens = charge_sensitivity( (*pm).IC.pv.val, v1[0], /ionbeam)
		val = charge_gain_units( sens, unit=vunit)
		(*pm).IC.pv.val = val
		(*pm).IC.pv.unit = vunit
	endif
	daq_launch_update_IC, pm
	
	;	Enable the various metadata related variables to be logged ...

;	socket_command_set, ps, 'blog.enable', 1, class='info'
	socket_command_set, ps, 'blog.enable', 1, class='metadata'
	socket_command_set, ps, 'blog.enable', 1, class='variable'
;	socket_command_set, ps, 'blog.enable', 1, class='scan'

	daq_launch_read_enable, ps, pm, pr, pimage, play
;	daq_launch_read_cal, ps, pm, play
;	daq_launch_read_da, ps, pm, pimage, /init
endif
return
end

;--------------------------------------------------------------------------

pro daq_launch_read_enable, ps, pm, pr, pimage, play, disable=disable, pshrmem=pshrmem

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
		warning,'daq_launch_read_enable',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(pshrmem) eq 0 then pshrmem=0L

n_detectors = (*ps).n_detectors
n_chips = (*ps).n_detectors/32

	daq_launch_read_info, ps, pm, pimage, pshrmem=pshrmem

; Cal coeffs. Gain-trim coeffs; also read in daq_update_parameters3 ...
	
	if ptr_good(pshrmem) then begin
		pdat = (*pshrmem).pdat
		*pr = (*pdat).readout
		(*pm).scan.on = (*pdat).enable.pixel
		(*pm).deadtime.on = (*pdat).enable.deadtime
;		(*pm).DA.on = (*pdat).DA.on
;		(*pm).ROI.on = (*pdat).enable.ROI

		q = where( (*pdat).enable.ECH eq 0, nq)
		if ptr_valid((*pimage).pactive) eq 0 then (*pimage).pactive = ptr_new(/allocate_heap)
		if nq gt 0 then begin
			*(*pimage).pactive = q
		endif else begin
			*(*pimage).pactive = 0
		endelse
		disable = (*pdat).enable.ECH
		
	endif else begin
		v = socket_command_get( ps, 'enable', class='scepter.clock', error=err)
		if err eq 0 then (*pr).quad_enable[0] = v[0]
		
		v  = socket_command_get( ps, 'blog.enable', class='event', error=err)
		if err eq 0 then (*pr).event = v[0]
		
		v = socket_command_get( ps, 'enable', class='photon', error=err)
		if err eq 0 then (*pr).photon = v[0]
		
		v = socket_command_get( ps, 'blog.enable', class='activity', error=err)
		if err eq 0 then (*pr).activity = v[0]
		
		v = socket_command_get( ps, 'enable', class='scepter', error=err)
		if err eq 0 then (*pr).scepter = v[0]
			
		v = socket_command_get( ps, 'enable', class='pixel', error=err)
		if err eq 0 then (*pm).scan.on = v[0]
			
	;	v = socket_command_get( ps, 'enable', class='da', error=err)
	;	if err eq 0 then (*pm).DA.on = v[0]
		
	;	if (*pm).number.roi gt 0 then begin
	;		v = socket_command_get( ps, 'enable', class='roi', error=err)
	;		if err eq 0 then (*pm).ROI.on = v[0]
	;	endif
		
		v = socket_command_get( ps, 'enable', class='photon.chan', chip=-1, n_chips=n_detectors, error=err)
		if err eq 0 then begin
			q = where( v eq 1, nq)
			if ptr_valid((*pimage).pactive) eq 0 then (*pimage).pactive = ptr_new(/allocate_heap)
			if nq gt 0 then begin
				*(*pimage).pactive = q
			endif else begin
				*(*pimage).pactive = 0
			endelse
			disable = 1-v[(*play).data.index]
		endif
	endelse
	return
end

;-------------------------------------------------------------------------------------

pro daq_launch_read_info, ps, pm, pimage, pshrmem=pshrmem

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
		warning,'daq_launch_read_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(pshrmem) eq 0 then pshrmem=0L

; Info fields, and decode them ...
; These are filled in shared memory in background processes: 

@daq_scratch.def
obj = (*pm).DevObj

;	daq_launch_read_da_info, ps, pm, pimage, pshrmem=pshrmem

	if ptr_good(pshrmem) then begin
		pdat = (*pshrmem).pdat
		
		if (*pm).version.software ge 7000 then begin	
;			if (*pm).number.roi gt 0 then begin
;				(*pm).roi.file = string((*pdat).info.roi)
;			endif

			s = string((*pdat).info.deadtime)
			r = unstringify( s, error=err)
			if err and (strlen(s) gt 0) then begin
				print,'daq_launch_read_info: ','failed to decode "deadtime info" data.'
			endif else begin
				if tag_present('auto',r) then begin
					(*pm).deadtime.auto = r.auto
				endif
				if tag_present('cal',r) then begin
					(*pm).deadtime.cal = r.cal
					(*pm).deadtime.on = 1
					obj->set_options, deadtime_cal=r.cal
				endif
			endelse

;			(*pm).cal.file = string((*pdat).info.cal)
			
;			s = string((*pdat).info.gaintrim)
;			r = unstringify( s, error=err)
;			if err and (strlen(s) gt 0) then begin
;				print,'daq_launch_read_info: ','failed to decode "gaintrim.info" data.'
;			endif else begin
;				if tag_present('file',r) then begin
;					(*pm).trim.file = r.file
;				endif
;				if tag_present('file2',r) then begin
;					(*pm).trim.file2 = r.file2
;				endif
;			endelse

;			s = string((*pdat).info.linear)
;			r = unstringify( s, error=err)
;			if err and (strlen(s) gt 0) then begin
;				print,'daq_launch_read_info: ','failed to decode "linearise.info" data.'
;			endif else begin
;				if tag_present('file',r) then begin
;					(*pm).linear.file = r.file
;				endif
;			endelse

;			s = string((*pdat).info.pileup)
;			r = unstringify( s, error=err)
;			if err and (strlen(s) gt 0) then begin
;				print,'daq_launch_read_info: ','failed to decode "pileup.info" data.'
;			endif else begin
;				if tag_present('file',r) then begin
;					(*pm).pileup.file = r.file
;				endif
;			endelse
		endif

	endif else begin
		if (*pm).version.software ge 7000 then begin	
;			if (*pm).number.roi gt 0 then begin
;				v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_ROI, /string, error=err)
;				if err eq 0 then (*pm).roi.file = v[0]
;			endif
			
			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_deadtime, /string, error=err)
			if err eq 0 then begin
				r = unstringify( v[0], error=err)
				if (err eq 0) and (strlen(v[0]) gt 0) then begin
					if tag_present('auto',r) then begin
						(*pm).deadtime.auto = r.auto
					endif
					if tag_present('cal',r) then begin
						(*pm).deadtime.cal = r.cal
						(*pm).deadtime.on = 1
					endif
				endif
			endif
			
;			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_cal, /string, error=err)
;			if err eq 0 then (*pm).cal.file = v[0]
		
;			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_gaintrim, /string, error=err)
;			if err eq 0 then begin
;				r = unstringify( v[0], error=err)
;				if err and (strlen(v[0]) gt 0) then begin
;					print,'daq_launch_read_info: ','failed to decode "gaintrim.info" data.'
;				endif else begin
;					if tag_present('file',r) then begin
;						(*pm).trim.file = r.file
;					endif
;					if tag_present('file2',r) then begin
;						(*pm).trim.file2 = r.file2
;					endif
;				endelse
;			endif
			
;			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_linearise, /string, error=err)
;			if err eq 0 then begin
;				r = unstringify( v[0], error=err)
;				if err and (strlen(v[0]) gt 0) then begin
;					print,'daq_launch_read_info: ','failed to decode "linearise.info" data.'
;				endif else begin
;					if tag_present('file',r) then begin
;						(*pm).linear.file = r.file
;					endif
;				endelse
;			endif
		
;			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_pileup, /string, error=err)
;			if err eq 0 then begin
;				r = unstringify( v[0], error=err)
;				if err and (strlen(v[0]) gt 0) then begin
;					print,'daq_launch_read_info: ','failed to decode "pileup.info" data.'
;				endif else begin
;					if tag_present('file',r) then begin
;						(*pm).pileup.file = r.file
;					endif
;				endelse
;			endif	
		endif
	endelse
	return
end

;--------------------------------------------------------------------------

pro daq_launch_read_da_info, ps, pm, pimage, pshrmem=pshrmem

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
		warning,'daq_launch_read_da_info',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(pshrmem) eq 0 then pshrmem=0L		; pars shrmem from 'daq_update_parameters'

@daq_scratch.def
return

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

;		if (*pm).version.software ge 4737 then begin
;			s = string((*pdat).info.da)
;			r = unstringify( s, error=err)
;			if err and (strlen(s) gt 0) then begin
;				print,'daq_launch_read_da_info: ','failed to decode "da.info" data from shared memory.'
;				return
;			endif
;			goto, cont
;		endif
		
	endif else begin
	
;		Flux.chan variables for built-in FC0, FC1 ...
;		Using 'charge_sensitivity' to convert val and string unit to sensitivity in nA/V units,
;		and 'charge_gain_units' to convert to 'val' and floating 'unit' scaling.

		if ((*pm).version.software ge 6646) then begin
			v1 = socket_command_get( ps, 'unit', class='flux.chan', n_chips=2, chip=-1, /string, error=err)
			if err eq 0 then begin
				if v1[0] ne '' then begin			; DAQ:scaler.FC0
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
				if v1[1] ne '' then begin			; DAQ:scaler.FC1
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

;		if (*pm).version.software ge 4737 then begin
;			v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_DA, /string, error=err)
;			if err then return
;			r = unstringify( v[0], error=err)
;			if err and (strlen(v[0]) gt 0) then begin
;				print,'DAQ_launch_read_da_info: ','failed to decode "da.info" data direct from Kandinski.'
;				return
;			endif
;			goto, cont
;		endif
	endelse
	return

cont:
	if tag_present('IC',r) then begin

;		Copy IC part of the DA info return struct to (*pm).IC, if NOT remote ...
;		If the selected one is NOT a h/w scaler, also save it as ICE.

		pr = ptr_new(r)
		if (*pm).IC.remote eq 0 then begin
			copy_pointer_data, pr, pm, tag=['IC']
			if ((*pm).IC.pv.name ne 'DAQ:scaler.FC0') and ((*pm).IC.pv.name ne 'DAQ:scaler.FC1') then begin
				(*pm).ICE.pv = (*pm).IC.pv
			endif
		endif
		ptr_free, pr
		check_plist_daq, (*pm).IC.plist
	endif

;	Use '(*pm).IC.pv.name' to select current IC. Copy from IC0, IC1 for h/w 
;	(which may get updated from Kandinski if changed elsewhere) ...

	if (*pm).IC.pv.name eq 'DAQ:scaler.FC0' then begin
		(*pm).IC.remote = (*pm).IC0.remote
		if (*pm).IC.remote then begin
			(*pm).IC.pv = (*pm).IC0.pv
			(*pm).IC.pv.name = 'DAQ:scaler.FC0'
		endif
	endif else 	if (*pm).IC.pv.name eq 'DAQ:scaler.FC1' then begin
		(*pm).IC.remote = (*pm).IC1.remote
		if (*pm).IC.remote then begin
			(*pm).IC.pv = (*pm).IC1.pv
			(*pm).IC.pv.name = 'DAQ:scaler.FC1'
		endif
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
	if tag_present('save_path',r) then begin
;		(*pm).da.save_path = r.save_path
	endif
	return
end

;--------------------------------------------------------------------------

pro daq_launch_scan_request, ps, pm, plist, tlb, pcom, update=update

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
		warning,'daq_launch_scan_request',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

@daq_scratch.def

	update = 0
	smallQ = 0.0001
	smallP = 0.5

	v  = socket_command_get( ps, 'value', class='scratch.datum', chip=scratch_velocity, /string, error=err)
	if err then return
	velocity = unstringify(v[0])
	if n_elements(velocity) lt 4 then velocity = [2.,2.,2.,2.]

	v0  = socket_command_get( ps, 'key', class='scratch.datum', chip=-1, n_chips=32, /string, error=err)
	if err then return

	q = where(v0 eq 'scan-setup', nq)
	if nq ne 0 then begin

		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=q[0], /string, error=err)
		if (err eq 0) and (v[0] ne '?') and (v[0] ne 'received') then begin

			setup = unstringify( v[0])
			pointer_display, setup
			socket_command_set, ps, 'value', (size(setup,/tname) eq 'STRUCT' ? 'received' : '?'), class='scratch.datum', chip=q[0]

			if size(setup,/tname) eq 'STRUCT' then begin	
				if tag_present('ENERGY',setup) then begin
					socket_command_set, ps, 'beam.energy', setup.energy, class='metadata'
				endif
				if tag_present('BEAM',setup) then begin
					socket_command_set, ps, 'beam.particle', setup.beam, class='metadata'
				endif
				if tag_present('CHARGE_UNIT',setup) then begin
					socket_command_set, ps, 'unit', setup.charge_unit, class='charge'
				endif
				if tag_present('CHARGE_COEFF',setup) then begin
					socket_command_set, ps, 'coeff', setup.charge_coeff, class='charge'
				endif
				if tag_present('DACX_SCALE',setup) then begin
					socket_command_set, ps, 'scale', setup.dacx_scale, class='deflect.axis', chip=0
					socket_command_set, ps, 'unit', 'mm', class='deflect.axis', chip=0
				endif
				if tag_present('DACY_SCALE',setup) then begin
					socket_command_set, ps, 'scale', setup.dacy_scale, class='deflect.axis', chip=1
					socket_command_set, ps, 'unit', 'mm', class='deflect.axis', chip=1
				endif
				if tag_present('STAGEX_VELOCITY',setup) then begin
					velocity[0] = setup.stagex_velocity
				endif
				if tag_present('STAGEY_VELOCITY',setup) then begin
					velocity[1] = setup.stagey_velocity
				endif
				if tag_present('STAGEZ_VELOCITY',setup) then begin
					velocity[2] = setup.stagez_velocity
				endif
				if tag_present('STAGEA_VELOCITY',setup) then begin
					velocity[3] = setup.stagea_velocity
				endif
				sv = stringify(velocity)
				socket_command_set, ps, 'value', sv, class='scratch.datum', chip=scratch_velocity
			endif
		endif
	endif

	q = where(v0 eq 'scan-new', nq)
	if nq ne 0 then begin
		
		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=q[0], /string, error=err)
		if (err eq 0) and (v[0] ne '?') and (v[0] ne 'received') then begin
	
			scan = unstringify( v[0], error=err)
			pointer_display, scan	
			socket_command_set, ps, 'value', (size(scan,/tname) eq 'STRUCT' ? 'received' : '?'), class='scratch.datum', chip=q[0]

			if size(scan,/tname) eq 'STRUCT' then begin	
				if ptr_good( plist) then begin
					n = n_elements( *plist)
					new = *(*plist)[n-1]

					new.blog = 0									; clear some entries, others inherit from last scan
					new.crossref = ''
					new.grain = ''
					new.comment = ''
				endif else begin
					new = define(/scan_spec)
				endelse
				new.active = 0										; set this one to 'ON'
		
				if tag_present('SAMPLE',scan) then new.sample = string(scan.sample)
				if tag_present('GRAIN',scan) then new.grain = string(scan.grain)
				if tag_present('COMMENT',scan) then new.comment = string(scan.comment)
				if tag_present('REF',scan) then new.crossref = string(scan.ref)
		
				if tag_present('XORG',scan) then new.origin.x = scan.xorg
				if tag_present('YORG',scan) then new.origin.y = scan.yorg
				if tag_present('ZORG',scan) then new.origin.z = scan.zorg
				if tag_present('XAXIS',scan) then new.raster.axis.x = scan.xaxis
				if tag_present('YAXIS',scan) then new.raster.axis.y = scan.yaxis
				if tag_present('XSIZE',scan) then new.raster.size.x = scan.xsize
				if tag_present('YSIZE',scan) then new.raster.size.y = scan.ysize
				if tag_present('XPIXEL',scan) then new.raster.pixel.x = scan.xpixel
				if tag_present('YPIXEL',scan) then new.raster.pixel.y = scan.ypixel
	
	;			if tag_present('shape',scan) then new.raster.shape = scan.shape
				if tag_present('step',scan) then new.raster.step_scan = scan.step
				if tag_present('overscan',scan) then new.raster.overscan = scan.overscan
				if tag_present('interlace',scan) then new.raster.interlace = scan.interlace
		
				if tag_present('QMIN',scan) then begin
					new.raster.charge.min = scan.qmin
					if new.raster.charge.min gt smallQ then new.raster.step_scan = 1
				endif
				if tag_present('QMAX',scan) then begin
					new.raster.charge.max = scan.qmax
					if new.raster.charge.max gt smallQ then new.raster.step_scan = 1
				endif
				if tag_present('PMIN',scan) then begin
					new.raster.photons.min = scan.pmin
					if new.raster.photons.min gt smallP then new.raster.step_scan = 1
				endif
				if tag_present('PMAX',scan) then begin
					new.raster.photons.max = scan.pmax
					if new.raster.photons.max gt smallP then new.raster.step_scan = 1
				endif
				if tag_present('TMIN',scan) then new.raster.time.min = scan.tmin
				if tag_present('TMAX',scan) then new.raster.time.max = scan.tmax
				
				p = ptr_new(new,/no_copy)
				if ptr_good( plist) then begin
					*plist = [*plist, p]
				endif else begin
					*plist = [p]
				endelse
				update = 1
			endif
		endif
	endif
		
	q = where(v0 eq 'scan-command', nq)
	if nq ne 0 then begin
		v  = socket_command_get( ps, 'value', class='scratch.datum', chip=q[0], /string, error=err)
		if (err eq 0) and (v[0] ne '') and (v[0] ne '?') and (v[0] ne 'received') then begin

			com = unstringify( v[0])
			pointer_display, com
			socket_command_set, ps, 'value', (size(com,/tname) eq 'STRUCT' ? 'received' : '?'), class='scratch.datum', chip=q[0]

			if size(com,/tname) eq 'STRUCT' then begin	
				if tag_present('start',com) then begin
					*pcom='start'
					notify, 'scan-command', pcom, from=tlb
				endif else if tag_present('stop',com) then begin
					*pcom='stop'
					notify, 'scan-command', pcom, from=tlb
				endif else if tag_present('pause',com) then begin
					*pcom='pause'
					notify, 'scan-command', pcom, from=tlb
				endif else if tag_present('skip',com) then begin
					*pcom='skip'
					notify, 'scan-command', pcom, from=tlb
				endif
			endif
		endif
	endif	
	return
end

;--------------------------------------------------------------------------

function daq_launch_raster_status, ps, pm, led=led

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
		warning,'daq_launch_update_cal',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	led = 0
	i = 3
	if (*pm).version.software gt 6260 then begin
		com = 'status'
		if (*pm).version.software gt 6880 then com = 'status.state'
		v  = socket_command_get( ps, com, class='raster', /string, error=err)
		if err eq 0 then begin
			case v[0] of
				'stop':		begin	i=0 & j=0
					end
				'run_move':	begin	i=1 & j=2	
					end
				'run_dwell': begin	i=1 & j=2
					end
				'run_pause': begin	i=2 & j=0	
					end
				'error':	begin	i=3 & j=1	
					end
				else:		begin 	i=3 & j=1	
					end
			endcase
			led = j
		endif
	endif
	return, i
end

;--------------------------------------------------------------------------

pro daq_launch_update_cal, pp_et, pimage, pm, play

; Update array energy cals for ET spectra and image pcal arrays.
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
		warning,'daq_launch_update_cal',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

;	this still has play re-ordering, not used in DAQ?

	pp = pp_et												; ET spectra
	
	print, 'n_elements( *pp) =', n_elements(*pp)
	print, 'n_elements( (*play).ref) =', n_elements( (*play).ref)

	n = min([n_elements(*pp),n_elements( (*play).ref)])
	for i=0L,n-1 do begin
		(*(*pp)[i]).cal.units = (abs((*pm).channel[ (*play).ref[i] ].cal.a - 1.0) gt 0.001) ? 'keV' : 'channel'
		(*(*pp)[i]).cal.poly[0:1] = [(*pm).channel[ (*play).ref[i] ].cal.b,(*pm).channel[ (*play).ref[i] ].cal.a]
	endfor
	
	daq_launch_update_image_cal, pimage, pm, play
	return
end

;--------------------------------------------------------------------------

pro daq_launch_update_deadtime, pstate, pm

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
		warning,'daq_launch_update_deadtime',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if ptr_good(pstate, /struct) eq 0 then return
if ptr_good(pm, /struct) eq 0 then return
if (*pm).deadtime.auto eq 0 then return

@daq_scratch.def

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

		info = { auto:(*pm).deadtime.auto, cal:dt }
		s = stringify( info)
		socket_command_set, ps, 'value', s, class='scratch.datum', chip=scratch_deadtime, error=err
	endif
	return
end

;--------------------------------------------------------------------------

pro daq_launch_update_IC, pm

; Update IC struct based on Klee beam and charge parameters

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
			warning,'daq_launch_update_IC',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	ranges = ['C', 'mC', 'uC', 'nC', 'pC', 'fC']
	scales = [1.e+6, 1.e+3, 1., 1.e-3, 1.e-6, 1.e-9]
	q = where( (*pm).beam.charge.unit eq ranges, nq)
	if nq gt 0 then begin
		(*pm).IC.mode = 0
		(*pm).IC.conversion = 1.0
		(*pm).IC.pv.name = 'DAQ:scaler.FC0'
		(*pm).IC.pv.val = scales[q[0]] * 1.0e-3
		(*pm).IC.pv.unit = (*pm).beam.charge.scale
	endif
	return
end

;--------------------------------------------------------------------------

pro daq_launch_update_image_cal, pimage, pm, play

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
		warning,'daq_launch_image_cal',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_null_image_1, max_image_cal
if n_elements(max_image_cal) lt 1 then max_image_cal = 8
if ptr_good(pimage) eq 0 then return
if ptr_good((*pimage).pactive) eq 0 then return
	
;	This still has play re-ordering, one-to-one at moment in DAQ, but not stay that way (e.g. start Mesytec at 0?)

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

pro daq_launch_version1, ps, n_detectors, version, error=error

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
		warning,'daq_launch_version1',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if (*ps).open eq 0 then begin
	(*ps).n_detectors = n_detectors
	(*ps).version = version
	error = 0
	return
endif

(*ps).version = version
v = socket_command_get( ps, 'version', class='main', /long, /quiet, error=error)
if error eq 0 then (*ps).version = v[0]

;n_detectors = socket_command_get( ps, 'channels', class='config', /long, error=error)
;n_detectors = n_detectors[0]
;if n_detectors lt 1 then warning,'daq_launch version1:','zero "config.channels" returned.'

(*ps).n_detectors = n_detectors > 1
print,'daq_launch Version1: Detector channels = ', n_detectors
return
end

;--------------------------------------------------------------------------

pro daq_launch_version2, ps, pm, error=error

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
		warning,'daq_launch_version2',['IDL run-time error caught.', '', $
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
	
	(*pm).version.daq = 1
	error = 0
	return
endif

ident = socket_command_get( ps, 'identity', class='main', /string, error=error)
if error eq 0 then (*pm).identity.software = ident

server = socket_command_get( ps, 'server', class='blog', /string, error=error)
if error eq 0 then (*pm).identity.blog = server	

(*pm).version.software = (*ps).version
ver = socket_command_get( ps, 'version', class='main', /long, error=error)
if error eq 0 then (*pm).version.software = ver[0]

ver = socket_command_get( ps, 'revision', class='scepter', /long, /quiet, error=error)
if error eq 0 then (*pm).version.scepter = ver[0]

;nch = socket_command_get( ps, 'channels', class='config', /long, error=error)
;if error eq 0 then (*pm).n_detectors = nch

;n = socket_command_get( ps, 'da.number', class='config', /long, error=error)
;if error eq 0 then (*pm).number.da = n < 32
;if n gt 32 then warning,'daq_launch_version2','Kandinski DA accumulator number greater than 32.'
;n = socket_command_get( ps, 'et2d.number', class='config', /long, error=error)
;if error eq 0 then (*pm).number.et2d = n
;n = socket_command_get( ps, 'roi.number', class='config', /long, error=error)
;if error eq 0 then (*pm).number.roi = n

print,'daq_launch Version2:  identity		version'
print,'daq		',(*pm).identity.daq,'		',(*pm).version.daq
print,'s/w		',(*pm).identity.software,'		',(*pm).version.software
print,'library		',(*pm).identity.library,'	',lib
print,'scepter			',(*pm).version.scepter
;print,'number of ET2D	',(*pm).number.et2d
;print,'          da	',(*pm).number.da
;print,'          roi	',(*pm).number.roi
return
end

;--------------------------------------------------------------------------

pro daq_control

daq_launch
return
end

;--------------------------------------------------------------------------

pro daq_launch, debug=debug

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_working_dir, geopixe_root
common c_null_image_1, max_image_cal
if n_elements(debug) lt 1 then debug=0
if n_elements(max_image_cal) lt 1 then max_image_cal = 8

common c_errors_1, catch_errors_on
catch_errors_on = 1                           ; enable error CATCHing
if debug then catch_errors_on = 0             ; disable error CATCHing
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
		warning,'daq_launch',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif else on_error,0

; DAQ launch (daq_control.sav) loads routines from GeoPIXE.sav.

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

;................................................................................................

startupp, /colours, /database			; setup IDL

resolve_routine, 'daq_setup', /no_recompile
image_routines							; Load support routines
register_notify							; notification routines
version = daq_version()					; Version text

if database_alive() ne 1 then begin
	warning,'daq_launch','failed to load "geopixe2.sav"'
endif

; Load devices and then set DAQ as the default.

define_devices

;-------------------------------------------------------------------------------------
;
; DAQ Klee control socket
; Use a socket to control DAQ set-up and parameter display

default = daq_defaults( error=error, source='daq_launch', /reset)
if error then return

ip_daq = default.daq.ip				; DAQ Klee port
name_daq = default.daq.name
port_daq = default.daq.port
obj = obj_new(default.daq.device)	; DAQ device object: copy kept in *pm, *pstate
enable_daq = default.daq.enable
n_detectors = default.detectors
token_daq = '0'

ip_maia = default.maia.ip			; Slave Maia (only if IP is non-blank)
port_maia = default.maia.port
enable_maia = default.maia.enable
debug_maia = default.maia.debug
name_maia = default.maia.name
token_maia = '0'

time_daq = 3.0						; update ASIC parameters timer (daq_update_parameters)
time_daq2 = 0.4						; update rates, T, bias (daq_update_parameters2)
time_daq3 = 10						; slow update (daq_update_parameters3)

enable_activity = 1					; use new Activity accumulator (else get from spectra_ET2)
timeout_processes = 30.				; timeout for background processes

s = get_login_info()				; make a local ID for this session (use for master mode)
clientname = s.machine_name
username = s.user_name
local_id = clientname + ':' + username

;-------------------------------------------------------------------------------------
;
; daq control socket and parameters struct

print,'Open daq control socket ...'
ps = bad_pars_struct( daq_port, make_pars=make_ps)
if make_ps then begin
	*ps = open_socket( ip=ip_daq, port=port_daq, token=token_daq, enable=enable_daq, $
					 retries=0, client='DAQ-Launch', connect_timeout=30, error=error)
	if error ne 0 then begin
		warning,'daq_launch','Failed to open DAQ control socket.'
		goto, bad_setup
	endif
endif

; Slave Maia control socket and parameters struct

if ip_maia eq '' then enable_maia = 0
psm = bad_pars_struct( maia_port, make_pars=make_psm)
print,'Open slave Maia control socket ...'
if make_psm then begin
	*psm = open_socket( ip=ip_maia, port=port_maia, token=token_maia, enable=enable_maia, $
						client='DAQ-Launch', connect_timeout=30, retries=0, error=error)
	if error ne 0 then begin
		warning,'daq_launch','Failed to open slave Maia control socket.'
		goto, bad_setup
	endif
endif

; Read initial version info from Klee, including "n_detectors"

daq_launch_version1, ps, n_detectors, default.version, error=error
if error then begin
	warning,'daq_launch','Failed to get initial version info from daq control socket.'
	goto, bad_setup
endif

; DAQ control parameters struct, read 'version', 'identity' back from DAQ

pm = bad_pars_struct( daq_pars, make_pars=make_pm)
if make_pm then begin
	print,'DAQ_launch: make DAQ *pm struct based on n_detectors =',n_detectors
	*pm = define(daq_struct = n_detectors)
endif
(*pm).number.DA = default.DA
(*pm).DevObj = obj

daq_launch_version2, ps, pm, error=error
if error then begin
	warning,'daq_launch','Failed to get version/ident info from DAQ control socket.'
	goto, bad_setup
endif

; Check whether the blog server IP that Klee is connected to matches what we plan
; to connect to as blog.

;if strlowcase(default.blog.ip) ne strlowcase((*pm).identity.blog) then begin
;	warning,'daq_launch',['There seems to be a mismatch between:', '', $
;					"Klee's connected blog server: "+(*pm).identity.blog, $
;					'and the blog server you have selected: '+default.blog.ip, '', $
;					'Select "OK" to continue, or "Cancel" to exit.'], $
;					cancel=cancel
;	if cancel then begin
;		print,'Close socket ...'
;		close_file, (*ps).unit
;		if ptr_valid( ps) then ptr_free, ps
;		if ptr_valid( pm) then ptr_free, pm
;		return
;	endif
;endif

; Detector layout struct parameters from file "DAQ_ ... .csv"

prefix = 'DAQ_36'
letters = ['','A','B','C','D','E','F','G','H','I','J','K','L','M']
prefix = prefix + letters[ (*pm).version.daq ]

pd = bad_pars_struct( data, make_pars=make_pd)
if make_pd then begin
	layout_file = prefix + '.csv'
	d = read_detector_layout(layout_file, maia=maia, error=error)
	if error then begin
		warning,'daq_launch','Failed to read "'+layout_file+'" to initialize layout.'
		goto, bad_setup
	endif
	if maia eq 0 then begin
		warning,'daq_launch','"'+layout_file+'" file does not contain required extended columns.'
		goto, bad_setup
	endif
	*pd = d
endif

; DAQ disable parameters struct

pe = bad_pars_struct( daq_disable, make_pars=make_pe)
if make_pe then begin
	*pe = intarr(n_detectors)
endif

; Detector rates parameters 

prates = bad_pars_struct( rates, make_pars=make_pr)
if make_pr then begin
	*prates = {detectors:fltarr(n_detectors)}
endif

; DAQ readout parameters struct
;	perhaps use 'quadrant' to refer to Scepter bank 1 of 16, bank 2 or 16, NIMs
;	NO: use the selection group "quadrant" for this, but not here.
;	Here, just have one 'quadrant' for 'scepter.clock.enable'

pr = bad_pars_struct( daq_readout, make_pars=make_pr)
if make_pr then begin
	readout = define(/daq_readout)
	*pr = readout
endif

pimage_temp = ptr_new(define(/image))
daq_launch_initial, ps, pm, pd, pr, pimage_temp, default, error=error
if error then begin
	warning,'daq_launch','Failed to get version/ident info from DAQ control socket.'
	goto, bad_setup
endif

; Initial array energy cal copied into image struct ...

daq_launch_update_image_cal, pimage_temp, pm, pd

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

daq_prefix = default.daq.name + '_'						; daq.name (max chars = 8)
prefix_pars = daq_prefix + 'pars_'						; append local prefix (max chars = 12)
if n_elements(byte(daq_prefix)) gt 9 then warning,'daq_launch','Too many characters in DAQ name in conf file.'

template = define(daq_shared1 = n_detectors)	
pshrmem_pars = shared_memory_struct( prefix=prefix_pars, template=template, /init, error=error )
if error then goto, bad_shrmem

b = byte(ip_daq)
n = n_elements(b)
;(*(*pshrmem_pars).ppar)[2:3] = 0						; use /init now
(*(*pshrmem_pars).pbyte)[*] = 0B
(*(*pshrmem_pars).pbyte)[0:n-1] = b						; IP address
(*(*pshrmem_pars).plong)[0] = port_daq					; blog port

(*(*pshrmem_pars).pdat).n_detectors = n_detectors		; initialize shared memory
(*(*pshrmem_pars).pdat).version = (*pm).version			; e.g. values that will not change
(*(*pshrmem_pars).pdat).number = (*pm).number	
(*(*pshrmem_pars).pdat).readout = *pr
(*(*pshrmem_pars).pdat).layout_data = (*pd).data
(*(*pshrmem_pars).pdat).channel = (*pm).channel
(*(*pshrmem_pars).pdat).control = (*pm).control
;(*(*pshrmem_pars).pdat).DA.on = (*pm).DA.on
;(*(*pshrmem_pars).pdat).DA.N = (*pm).DA.N
;if (*pm).DA.on and ptr_good((*pm).DA.parray) then begin
;	if tag_present('rGamma',*(*pm).DA.parray) then begin
;		(*(*pshrmem_pars).pdat).DA.rGamma = (*(*pm).DA.parray).rGamma
;	endif
;endif

;-------------------------------------------------------------------------------------
;
; Blog server data socket
; Used by clients to read data from blog
; Port and IP info passed via the shared memory

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
; 
; Slave process: Blog data shared memory - Activity spectra for 36 detector
; Number of buffers comes from shared memory, set here. Data stored in *(*pstate).prates struct

prefix_activity = daq_prefix + 'activity_'				; append local prefix (max chars = 12)
if n_elements(byte(daq_prefix)) gt 9 then warning,'daq_launch','Too many characters in DAQ name in conf file.'
;time_activity = 0.4
;time_activity_min = 0.2
;time_activity_max = 1.0
;time_activity_update = 3.0

; NOTE: only use 1 buffer in DAQ version, not 2 ...

n_buffers = 2											; 0 - detectors, 1 - group name string
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


; Master: Blog data shared memory - Spectra (2D E spectra [4096,36] for ET2 records
; Number of buffers is set here and sent to the blog Client.

prefix_ET_spectra = daq_prefix + 'et_spec_'				; append local prefix (max chars = 12)
time_ET_spectra = 1.8									; ET spectrum update
time_ET_spectra_update = 3.0							; display update
time_ET_spectra_min = 2.0								; display update range
time_ET_spectra_max = 10.0

n_channels = 8192L										; channels in spectra
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
		spec.comment = 'DAQ on-line Detector E spectra monitor'
		spec.source = 'DAQ'
		spec.DevObj = clone_device_object(obj)
		spec.label = 'DAQ Detector #' + str_tidy(i) + ' /E'
		pspec3[i] = ptr_new(spec, /no_copy)
	endfor
endif else pspec3=ptr_new()


; Master: Blog data shared memory - Spectra (2D T spectra [4096,384] for ET2 records
; Number of buffers is set here and sent to the blog Client.
; The pbyte[0] array here is used to store the Throttle spectrum for use by backgnd process.

prefix_ET_spectraT = daq_prefix + 'et_specT_'			; append local prefix (max chars = 12)
print, prefix_ET_spectraT

n_channels4 = 2048L										; channels in T spectra
n_buffers4 = 1											; number of 2D spectra buffers
buffer_size4 = [n_channels4,n_detectors]
pshrmem_ET_spectraT = shared_memory_buffers( prefix=prefix_ET_spectraT, n_buffers=n_buffers4, $
						buffer_size=buffer_size4, n_byte=8192, /init, error=error)
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
		spec.cal.units = 'channel'						; enables set via DAQ_Setup
		spec.cal.poly[0:1] = [0.0,1.0]	
	;	spec.ecal = spec.cal
		spec.comment = 'DAQ on-line Detector T spectra monitor'
		spec.source = 'DAQ'
		spec.DevObj = clone_device_object(obj)
		spec.label = 'DAQ Detector #' + str_tidy(i) + ' /T'
		pspec4[i] = ptr_new(spec, /no_copy)
	endfor
endif else pspec4=ptr_new()


; Master: Blog data shared memory - ET2D (2D E,T maps [512,512] for ET2 records
; Number of buffers is set here and sent to the blog Client.

prefix_ET2d = daq_prefix + 'et2d_'						; append local prefix (max chars = 12)

n_energy = 1024
n_time = 256
buffer_size3b = [n_energy,n_time]
pshrmem_ET2d = shared_memory_buffers( prefix=prefix_ET2d, n_buffers=1, $
						buffer_size=buffer_size3b, /init, error=error)
if error then goto, bad_shrmem
;(*(*pshrmem_ET2d).ppar)[2:3] = 0
		
if ptr_valid((*pshrmem_ET2d).pdat[0]) then begin
	pimg3 = (*pshrmem_ET2d).pdat[0]
endif else begin
	pimg3 = ptr_new(fltarr(n_energy,n_time,1))
endelse

image = define(/image)
image.n_el = 1
image.el = ptr_new('E-T')
image.image = pimg3
image.xsize = n_energy
image.ysize = n_time
image.xcompress =  (8192L/n_energy) > 1					; for 13 bit E
image.ycompress = (2048L/n_time) > 1					; for 11 bit T
image.array = 0
image.detector = 7
image.comment = 'DAQ on-line ET 2D monitor'
image.source = 'DAQ'
image.DevObj = clone_device_object(obj)
pimage2D = ptr_new(image, /no_copy)
fix_options, pimage2D


; Slave: Blog data shared memory - DA Images

prefix_da = daq_prefix + 'da_'						; append local prefix (max chars = 12)
time_da = 0.5										; DA array update time
time_da_update = 1.5								; DA display update time
time_da_min = 0.5									; DA display update range
time_da_max = 5.0

nx = default.image.x								; size of image DA buffer
ny = default.image.y	

; This order also in daq_launch_update_DA_images, blog_client_da2, daq_daq (setup)
n_el_daq = n_elements(special_elements(/maia))
nel = (*pm).number.da + n_el_daq
str_elemements = ['none', special_elements(/maia), replicate(' ',nel-(n_el_daq+1)) ]

buffer_size2 = long([nx,ny,nel])	
pshrmem_da = shared_memory_buffers( prefix=prefix_da, n_buffers=1, $
				buffer_size=buffer_size2, /floatd, n_float=50, n_long=1000, /init, error=error)
if error then goto, bad_shrmem
b = byte(ip_blog)
n = n_elements(b)
;(*(*pshrmem_da).ppar)[2:3] = 0
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
image.comment = 'DAQ on-line DA image monitor'
image.source = 'DAQ Realtime'
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

; Launch klee clients ...

if enable_daq then begin
	print,'Spawn new klee clients ...'
	spawn_klee_clients, n_detectors, units=daq_client_luns, prefix=daq_prefix, conf=default.conf

; Check klee clients ...

	t = 0.0
	while (*(*pshrmem_pars).ppar)[5] eq 0 do begin
		print,'Wait for Parameters DAQ client to run ...'
		wait, 0.3
		t = t+0.3
		if t gt timeout_processes then begin
			warning,'daq_launch','Parameters DAQ client not running.'
			(*pshrmem_pars).error = 1
			break
		endif
	endwhile
	
;	The Slow process uses the parameters shared memory, and listens to extra ppar[6] variable

;	t = 0.0
;	print,'Wait for Slow Parameters DAQ client to run ...'
;	while (*(*pshrmem_pars).ppar)[6] eq 0 do begin				; non-standard index used (6)
;		print,'Wait for Slow Parameters DAQ client to run ...'
;		wait, 0.3
;		t = t+0.3
;		if t gt 3. then begin
;			warning,'daq_launch','Slow Parameters DAQ client not running.'
;;			(*pshrmem_pars).error = 1
;			break
;		endif
;	endwhile
	
endif else daq_client_luns=lonarr(2)

;-------------------------------------------------------------------------------------

; Launch blog clients ...

if enable_blog then begin
	print,'Spawn new clients ...'
	spawn_daq_clients, units=blog_client_luns, prefix=daq_prefix, conf=default.conf

;-------------------------------------------------------------------------------------

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
		if t gt 3. then begin
			warning,'maia_launch','ET_spectra client not running.'
			(*pshrmem_ET_spectra).error = 1
			break
		endif
	endwhile
	
;	t = 0.0
;	print,'Wait for DA images blog client to run ...'
;	while (*(*pshrmem_da).ppar)[5] eq 0 do begin
;		print,'Wait for DA images blog client to run ...'
;		wait, 0.3
;		t = t+0.3
;		if t gt 3. then begin
;			warning,'maia_launch','DA images client not running.'
;			(*pshrmem_da).error = 1
;			break
;		endif
;	endwhile
endif else blog_client_luns=lonarr(3)

;-------------------------------------------------------------------------------------

time_scan = 1.0					; new scan request timer

path = ''
dpath = ''
prefs = geopixe_defaults( error=err, source='daq_launch')
if err eq 0 then begin
	if strlen(prefs.path.analysis) gt 0 then path = prefs.path.analysis
	if strlen(prefs.path.data) gt 0 then dpath = prefs.path.data
endif

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
		rspace = 4
		space1 = 1
		tspace5 = 3
		stats_xsize = 7.5*button_xsize + 20
		help_xsize = 13.5*(button_xsize + 5) - 238
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
		rspace = 4
		space1 = 1
		tspace5 = 3
		stats_xsize = 7.5*button_xsize + 20
		help_xsize = 13.5*(button_xsize + 5) - 238
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
		rspace = 5
		space1 = 1
		tspace5 = 5
		stats_xsize = 5.5*button_xsize - 5
		help_xsize = 11.5*(button_xsize + 5) - 238
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
lnames = geopixe_root + 'images' + path_sep() + ['led-off-12x14.jpeg','led-red-12x14.jpeg','led-green-12x14.jpeg']

screen = get_screen_size()
;xoffset = (screen[0]/2 - (help_xsize+238+120)/2) > 0
xoffset = 180
yoffset = 0	;	(screen[1]-28 - 200)/2 > 0

; 	top-level base

s = '  ' + version + '  (to ' + name_daq + ' ['+(*pm).identity.software + ' ' + str_tidy((*pm).version.software) + ']'
if (*psm).open then s=s + ' + ' + name_maia
s = s + ', Library: ' + str_tidy((*pm).identity.library) + ')'
tlb = widget_base( /column, title='DAQ Control GUI'+s, /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='daq-launch-tlb', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=1 ,YPAD=2 ,xoffset=xoffset, $
					yoffset=yoffset, /base_align_center)
tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)
label = widget_label( tbase, value='CSIRO-BNL Data Acquisition System - DAQ 36')
t0base = widget_base( tbase, /row, xpad=0, ypad=0, space=0, /base_align_top, /align_center)

lbase = widget_base( t0base, /column, xpad=0, ypad=0, space=2, /base_align_center)
row0base = widget_base( lbase, /row, xpad=0, ypad=1, space=0, /base_align_top, /align_center)
led0_base = widget_base( row0base, /column, xpad=1, ypad=1, space=3, /frame, /base_align_left)
label = widget_label( led0_base, value='Status', /align_center)

if !version.os_family eq 'unix' then led0_base = widget_base( led0_base, /column, xpad=0, ypad=1, space=3, /base_align_left)
led = intarr(6)
led_base = widget_base( led0_base, /column, xpad=1, ypad=0, space=led_space, /align_center, /base_align_left)

lr0base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[0] = (*ps).open+1
led0 = picture_button( lr0base, lnames, uname='led0', value=led[0], /tracking, uvalue='Status LED0: DAQ socket is Open status.')
label = widget_label( lr0base, value='DAQ socket', /tracking, uvalue='Status LED0: DAQ socket is Open status.')

lr1base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led1 = picture_button( lr1base, lnames, uname='led1', value=led[1], /tracking, uvalue='Status LED1: Klee is connected to Blog status.')
label = widget_label( lr1base, value='DAQ to Blog', /tracking, uvalue='Status LED1: Klee is connected to Blog status.')

lr2base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led2 = picture_button( lr2base, lnames, uname='led2', value=led[2], /tracking, uvalue='Status LED2: Blog enabled status.')
label = widget_label( lr2base, value='Blog enabled', /tracking, uvalue='Status LED2: Blog enabled status.')

; Note that the LED[3] value needs to reflect all background processes that access BLOG ...

lr3base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
;led[3] = 1 + (enable_blog and (((*pshrmem_ET_spectra).error or (*pshrmem_activity).error or (*pshrmem_da).error) eq 0))
led[3] = 1 + (enable_blog and (((*pshrmem_ET_spectra).error eq 0 ) and ((*pshrmem_activity).error eq 0) and ((*pshrmem_pars).error eq 0)))
led3 = picture_button( lr3base, lnames, uname='led3', value=led[3], /tracking, uvalue='Status LED3: Launch panel blog clients appear to be running.')
label = widget_label( lr3base, value='Blog clients', /tracking, uvalue='Status LED3: Launch panel blog clients appear to be running.')

lr4base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led[4] = (*psm).open+1
led4 = picture_button( lr4base, lnames, uname='led4', value=led[4], /tracking, uvalue='Status LED4: Slave Maia socket is Open status')
label = widget_label( lr4base, value='Slave Maia', /tracking, uvalue='Status LED4: Slave Maia socket is Open status')

lr5base = widget_base( led_base, /row, xpad=0, ypad=0, space=2, /base_align_center, /align_left)
led5 = picture_button( lr5base, lnames, uname='led5', /tracking, uvalue='Status LED5: ')
label = widget_label( lr5base, value='Raster', /tracking, uvalue='Status LED5: Indicates raster "stop" (black), "running" (green) ' + $
					'"error" (red).')


rbase = widget_base( t0base, /column, xpad=0, ypad=0, space=space1, /base_align_center)

rowbase = widget_base( rbase, /row, xpad=1, ypad=1, space=1, /base_align_top, /align_center)

setup_base = widget_base( rowbase, /column, xpad=1, ypad=1, space=3, /frame, /base_align_center)
label = widget_label( setup_base, value='Setup')

daq_setup_button = state_button( setup_base, value='DAQ', uname='setup-daq-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Setup DAQ detector parameters, load calibration tables and run tests and calibration procedures.')
button = state_button( setup_base, value='Reset', uname='reset-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='?')

display_base = widget_base( rowbase, /column, xpad=1, ypad=1, space=3, /frame, /base_align_center)
label = widget_label( display_base, value='Display')

d1base = widget_base( display_base, row=2, xpad=0, ypad=0, space=3, /base_align_center, /align_center)
spectra_button = state_button( d1base, value='ET Spectra', uname='new-spectra-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open a GeoPIXE real-time spectrum window for the display of individual "Detector" spectra gathered from ET blog records.')
rates_button = state_button( d1base, value='Rates', uname='rates-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open the DAQ detector count rate and process monitor window.')
images_button = state_button( d1base, value='Image', uname='new-images-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open a GeoPIXE real-time image display.')
et2d_image_button = state_button( d1base, value='ET 2D', uname='new-et2d-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open a DAQ ET 2D image window to display "E versus T" for events gathered from ET blog records.')
blog_browse_button = state_button( d1base, value='Browse', uname='blogd-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open a Blog-Browser window to sample records from the blogd server.')
multi_images_button = state_button( d1base, value='Multi', uname='new-multi-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open a DAQ Multi-Image display.')

stage_base = widget_base( rowbase, /column, xpad=1, ypad=1, space=3, /frame, /base_align_center)
label = widget_label( stage_base, value='Scan')

list_stage_button = state_button( stage_base, value='List', uname='list-stage-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=0, colours=colours2, n_states=4, alt=0, $
					uvalue='Open the Scan List window to append, edit or control scan specifications and execution.')
stop_stage_button = state_button( stage_base, value='Stop', uname='stop-stage-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=3, colours=colours2, n_states=4, alt=0, $
					uvalue='Stop all Stage axis motors. Will also stop any running scan, if the "Scan List" window is open.')

control_base = widget_base( rowbase, /column, xpad=1, ypad=1, space=3, /frame, /base_align_center)
label = widget_label( control_base, value='Control')
control_base2 = widget_base( control_base, column=2, xpad=0, ypad=0, space=3, /align_center, /base_align_center)

run_button = state_button( control_base2, value='Newrun', uname='run-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=3, colours=colours2, n_states=4, alt=0, $
					uvalue='Instruct DAQ to issue a "newrun" record to blog, or to "endrun" if a run is currently active. ' + $
					'Normally run control will be done via the Scan List execution. See "Start" and "Stop" on the Scan List window.')
button = state_button( control_base2, value='Clear', uname='clear-button', /tracking, xsize=button_xsize, ysize=button_ysize, $
					/freeze, select=2, colours=colours2, n_states=4, alt=0, $
					uvalue='Clear ALL (local?) spectrum and image memory.')

run_stats = widget_list( control_base2, scr_xsize=stats_xsize, uname='control-stats', /tracking, value=strarr(3), $
				uvalue='Display Blog run number and segment and Blog received data rate in bytes. Double click on "Group" to change blog data Group.', $
				frame=0, scr_ysize=2*button_ysize+tspace5)

hbase = widget_base( rbase, /row, xpad=0, ypad=0, space=rspace, /base_align_center, /align_center)
help = widget_text( hbase, scr_xsize=help_xsize, ysize=3, /wrap, uname='help', /tracking, $
				uvalue='Context sensitive help: Move cursor over object for help.', frame=0)

picpath = geopixe_root + 'daq' + slash()
pic = picture_button( hbase, picpath + 'DAQ-Logo.png', tracking=0, pushbutton_events=0)

log_file = prefix + '_log.csv'

state = { $
		path:				ptr_new(path), $			; pointer to current path
		dpath:				ptr_new(dpath), $			; pointer to raw data path
		version:			version, $					; DAQ launch version string
		debug:				debug, $					' debug mode enabled for IDLDE use
		default:			default, $					; defaults struct (read from DAQ.conf)
		DevObj:				obj, $						; DAQ device object
		prefix:				prefix, $					; DAQ file name prefix
		layout_file:		layout_file, $				; layout file name
		local_id:			local_id, $					; local ID string (used for master)
		prev_id:			'', $						; ID of a previous master
		master:				0, $						; flags this instance as the DAQ master
		lock_master:		0, $						; locks master during check set

		pbeam:				ptr_new(1.0), $				; pointer to beam current estimate (nA)
		playout:			pd, $						; pointer to layout struct array
		pdaq:				pm, $						; pointer to daq parameters struct array
		pdisable:			pe, $						; pointer to daq disable detector channels
		preadout:			pr, $						; pointer to readout enables
		psocket:			ps, $						; pointer to daq socket port and command parameters
		pmsocket:			psm, $						; pointer to Maia socket port and command parameters
		prates:				prates, $					; pointer to daq count rates struct
		psetup:				ptr_new( /allocate_heap), $	; pointer to heap for Notify to DAQ_setup.
		pstage:				ptr_new( /allocate_heap), $	; pointer to heap for Scan list.
		pcom:				ptr_new( /allocate_heap), $	; pointer to heap for Notify command to scan_list.
		pmaster:			ptr_new( /allocate_heap), $	; pointer to heap for Notify master to scan_list.
		
		pshrmem_pars:		pshrmem_pars, $				; pointer to shared memory for Maia parameters update
		daq_client_luns:	daq_client_luns, $			; DAQ client logical units
		
		scan_sequence: { $
			active:				0, $					; scan list is running	
			raster: {	on:		0}}, $					; raster started and 'running'		
		
		rate_last:			fltarr((*pm).n_detectors), $	; last count for rate determ
		time_daq:			time_daq, $					; DAQ update slow timer event time interval
		time_daq2:			time_daq2, $				; DAQ update fast timer event time interval
		time_daq3:			time_daq3, $				; DAQ update slow timer interval
		pHYMOD_DEBUG:		ptr_new( /allocate_heap), $	; debug modes (pulser, synth, hermes, scepter, eblk, bake)
		
		blog: { ip:			ip_blog, $					; blogd IP
			port:			port_blog}, $				; blogd port
		blog_client_luns:	blog_client_luns, $			; blog client logical units
			
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
		daq_IC_name:		'', $						; DAQ flux IC PV name string

		time_scan:			time_scan, $				; new scan request timer (list_stage_button)
		tracking:			1, $						; tracking mode
		tracking_counter:	0, $						; tracking counter to suppress detector messages for a while
		pseed:				ptr_new(1L), $				; seed for randomu
		
		led:				led, $						; LED values
		
		time: { update: { $
					DA:		systime(/seconds), $		; last DA update 
					DAQ:	systime(/seconds), $		; last DAQ parameters update
					ET_spectra: systime(/seconds), $	; last spectrum update
					activity: systime(/seconds)}, $		; last activity update
				display: { $
					DA:		systime(/seconds), $		; last DA display draw
					DAQ:	systime(/seconds), $		; last DAQ parameters display
					ET_spectra: systime(/seconds)}}, $	; last spectrum display
					
; Number indices for various processes:
;	update	DA			0		display:	image 		0	(normal window)
;			ET_spectra	1					ET_spectra	1
;			daq			2					multi		2	(multi image window)

		activity: { $
			chip:		0L, $							; counts (per second) for Scepter
			process: { DA:	0.0, $						; % time on DA update
					ET_spectra: 0.0, $					; % time on ET_spectra updates
					activity: 0.0, $					; % time on activity updates
					daq:	0.0 }, $					; % time on DAQ parameter updates
			buffers: { DA:	0.0, $						; % records lost for DA
					ET_spectra: 0.0 }, $				; % records lost for ET_spectra
			display: { $
				ET_spectra: replicate({ TLB: 0L, $		; ET E spectra TLB ID
							ppercent: ptr_new()},64), $ ; ptr to % spectrum draw
				ET_spectraT: replicate({ TLB: 0L, $		; ET T spectra TLB ID
							ppercent: ptr_new()},64), $ ; ptr to % spectrum draw
				ET2d: replicate({ TLB: 0L, $			; ET2d TLB ID
							ppercent: ptr_new()},64), $ ; ptr to % ET 2D draw
				image: replicate({ TLB: 0L, $			; image TLB ID
							ppercent: ptr_new()},64), $ ; ptr to % image draw
				multi: replicate({ TLB: 0L, $			; multi TLB ID
							ppercent: ptr_new()},64)}}, $ ; ptr to % multi-image draw
		
		enable_activity:	enable_activity, $			; enable new activity accumulator
		
		tlb:				tlb, $						; top level base ID
		daq_setup_button:	daq_setup_button, $			; daq setup button ID
		spectra_button:		spectra_button, $			; spectra button ID 
		rates_button:		rates_button, $				; rates button ID
		images_button:		images_button, $			; images button ID
		led_id:				[led0,led1,led2,led3,led4,led5], $	; LED widget IDs
		run_button: 		run_button, $				; run button ID
		list_stage_button:	list_stage_button, $		; list stage button ID
		run_stats:			run_stats, $				; run stats text widget ID
		help:				help $						; help text ID
		}

state.activity.display.ET_spectra.ppercent = ptrarr(64,/allocate_heap)
state.activity.display.ET_spectraT.ppercent = ptrarr(64,/allocate_heap)
state.activity.display.ET2d.ppercent = ptrarr(64,/allocate_heap)
state.activity.display.image.ppercent = ptrarr(64,/allocate_heap)
state.activity.display.multi.ppercent = ptrarr(64,/allocate_heap)
for i=0L,63 do *state.activity.display.ET_spectra[i].ppercent = 0.0
for i=0L,63 do *state.activity.display.ET_spectraT[i].ppercent = 0.0
for i=0L,63 do *state.activity.display.ET2d[i].ppercent = 0.0
for i=0L,63 do *state.activity.display.image[i].ppercent = 0.0
for i=0L,63 do *state.activity.display.multi[i].ppercent = 0.0

daq_launch_update_cal, state.ppspec3, state.pimage, pm, pd

;---------------------------------------------------------------------------------------------------

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

widget_control, daq_setup_button, timer=time_daq					; start slow daq update timer
widget_control, rates_button, timer=time_daq2						; start fast daq update timer
;widget_control, run_button, timer=time_daq3						; start slow daq update timer
widget_control, list_stage_button, timer=time_scan					; start new scan request timer

if enable_blog then begin
	if (*pshrmem_ET_spectra).error eq 0 then begin
		widget_control, spectra_button, timer=time_ET_spectra		; start detector spectra update timer
	endif
;	if (*pshrmem_da).error eq 0 then begin
;		widget_control, images_button, timer=time_da				; start DA update timer
;	endif
endif
register_notify, tlb, ['path', $									; new path
						'warn-setup'], $							; warning status from Setup
						from=group

xmanager, 'daq_launch', tlb, /no_block
return

bad_shrmem:
	warning,'daq_launch','error shrmem allocate'
	goto, bad_setup
bad_blog_client:
	warning,'daq_launch','blog client not running'
	goto, bad_setup
bad_setup:
	close_file, (*ps).unit
	return
end

