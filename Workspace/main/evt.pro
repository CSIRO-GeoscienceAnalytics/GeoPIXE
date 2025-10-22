;
;	EVT file sorting for Images, Spectra, Traverses
;
pro evt_event, event

COMPILE_OPT STRICTARR
common c_prefs_scan, prefs_XY_scan, prefs_X_step, prefs_Y_step, prefs_Resolution
common c_sandia_4, sync, TimerEvent, RTCmask, Dummymask, maxADCs, mpa_x_adc, mpa_y_adc
common aps_4, aps_count_to_charge
common c_geopixe_vm, geopixe_enable_vm
common image_region_window_1, region_window
if n_elements(geopixe_enable_vm) lt 1 then geopixe_enable_vm=1
if n_elements(aps_count_to_charge) lt 1 then aps_count_to_charge=1.

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
		warning,'EVT_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
;widget_control, hourglass=0

if n_elements(prefs_XY_scan) lt 1 then prefs_XY_scan = {X:100.0, Y:100.0}
if n_elements(prefs_X_step) lt 1 then prefs_X_step = {Y:2000.0}
if n_elements(prefs_Y_step) lt 1 then prefs_Y_step = {X:640.0}
if n_elements(prefs_Resolution) lt 1 then prefs_Resolution = {X:0.635, Y:0.08333333}
if n_elements(mpa_x_adc) lt 1 then mpa_x_adc = 0S
if n_elements(mpa_y_adc) lt 1 then mpa_y_adc = 2S

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
p = (*pstate).p
if ptr_valid(p) eq 0 then goto, bad_ptr
if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr

DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

file_ext = [[['*.dam','*.cuts','*.cuts','*.mpdam'],['*.dam','*.region','',''],['*.dam','*.cuts','*.cuts','']], $
		[['*.damg','*.cuts','*.cuts',''],['*.damg','*.cuts','*.cuts',''],['*.damg','*.cuts','*.cuts','']], $
		[['*.damr','*.cuts','*.cuts',''],['*.damr','*.cuts','*.cuts',''],['*.damr','*.cuts','*.cuts','']], $
		[['*.dame','*.cuts','*.cuts',''],['*.dame','*.cuts','*.cuts',''],['*.dame','*.cuts','*.cuts','']], $
		[['*.dams','*.cuts','*.cuts',''],['*.dams','*.cuts','*.cuts',''],['*.dams','*.cuts','*.cuts','']], $
		[['*.damc','*.cuts','*.cuts',''],['*.damc','*.cuts','*.cuts',''],['*.damc','*.cuts','*.cuts','']], $
		[['*.damd','*.cuts','*.cuts',''],['*.damd','*.cuts','*.cuts',''],['*.damd','*.cuts','*.cuts','']], $
		[['*.damx','*.cuts','*.cuts','*.mpdam'],['*.damx','*.cuts','*.cuts',''],['*.damx','*.cuts','*.cuts','']]]

file_title = [['Select DA Matrix file','Select CUTS specification file','Select STIM energy CUTS file','Select MPDA MPDAM file'], $
		['Select DA Matrix file','Select CUTS specification file','Select STIM energy CUTS file','Select ?'], $
		['Select DA Matrix file','Select CUTS specification file','Select STIM energy CUTS file','Select ?']]

case tag_names( event, /structure) of
	'NOTIFY': begin
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
					print,'EVT: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
					path = build_output_path( *(*pstate).dpath, *(*pstate).path, (*p).root, /set)
				endif
				end
			'dpath': begin
				if ptr_valid( event.pointer) then begin
					print,'EVT: new raw data dpath = ',(*event.pointer)
					*(*pstate).dpath = (*event.pointer)
					path = build_output_path( *(*pstate).dpath, *(*pstate).path, (*p).root, /set)
				endif
				end
			'detector-select': begin
				if ptr_valid( event.pointer) then begin
					print,'Received "detector-select" notify : n=',(*event.pointer).n
					k = (*event.pointer).n
					(*p).station = (k - (*(*event.pointer).pdata).start > 0) < (*pstate).max_adcs
;					widget_control_update, event.top, update=0
					widget_control, (*pstate).station, set_combobox_select=(*p).station
					widget_control, (*pstate).enable, set_value=(*p).enable[(*p).station]
					widget_control, (*pstate).type, set_combobox_select=(*p).type[(*p).station]
					widget_control, (*pstate).cal_a, set_value=str_tidy((*p).cal_a[(*p).station], places=8)
					widget_control, (*pstate).cal_b, set_value=str_tidy((*p).cal_b[(*p).station])
					widget_control, (*pstate).mode, set_combobox_select=(*p).mode[(*p).station]
					widget_control, (*pstate).file, set_value=(*p).file[(*p).station]
					set_widget_text, (*pstate).file, (*p).file[(*p).station]
;					widget_control_update, event.top, update=1
				endif
				end
			'detector-load': begin						; layout details loaded elsewhere
				answer = dialog_message( /question, title='Notify Sort EVT of changed detector layout', $
									['Do you want to Enable or Disable detector channels', $
									'based on the good detector channels in the new layout?'])

				if (answer eq 'Yes') and ptr_valid( event.pointer) then begin
;					print,'Received "detector-load" notify.'
					pd = (*event.pointer).pdata
					(*p).enable[*] = 0
					for j=0L,(*pd).n-1 do begin
						i = (*pd).data[j].index - (*pd).start
						if (i ge 0) and (i lt (*pstate).max_adcs) then begin
							(*p).fwhm[i] = (*pd).data[j].fwhm
							(*p).enable[i] = ((*pd).veto) ? ((*pd).data[j].bad lt 1) : ((*pd).data[j].bad lt 2)
						endif
					endfor
					widget_control, (*pstate).enable, set_value=(*p).enable[(*p).station]
				endif
				end
			'detector-fwhm': begin						; layout FWHM thresh details modified elsewhere
				if ptr_valid( event.pointer) then begin
;					print,'Received "detector-fwhm" notify.'
					pd = (*event.pointer).pdata
					(*p).enable[*] = 0
					for j=0L,(*pd).n-1 do begin
						i = (*pd).data[j].index - (*pd).start
						if (i ge 0) and (i lt (*pstate).max_adcs) then begin
							(*p).fwhm[i] = (*pd).data[j].fwhm
							(*p).enable[i] = ((*pd).veto) ? ((*pd).data[j].bad lt 1) : ((*pd).data[j].bad lt 2)
						endif
					endfor
					widget_control, (*pstate).enable, set_value=(*p).enable[(*p).station]
				endif
				end
			'snapshot': begin
				name = 'evt.snap'
				on_ioerror, finish
				openw, lun, name, /xdr, /get_lun
				geom = widget_info( event.top, /geometry)
				on_ioerror, snap_done
				writeu, lun, geom.xoffset, geom.yoffset, geom.scr_xsize, geom.scr_ysize
snap_done:
				close_file, lun
				on_ioerror, null
				end
				
;			Changes made in a device object, may trigger this Notify.
;			Need to reprocess last header 'mp' data in 'evt_check_mp'.

			'device-update': begin
				mp = DevObj->show_header(error=err)
				if err eq 0 then evt_check_mp, pstate, mp, /no_cal_adopt, error=err
				end
				
			'start-evt': begin
				print,'EVT: start-evt ...'
				if ptr_valid( event.pointer) then begin
					psort = (*event.pointer).p
					skip = (*event.pointer).skip
					load = (*event.pointer).load
					(*p).xrange = (*psort).xrange
					(*p).yrange = (*psort).yrange
					(*p).xsize = (*psort).xsize
					(*p).ysize = (*psort).ysize
					(*p).charge = (*psort).charge
					(*p).comment = (*psort).comment
					(*p).evt_file = (*psort).file
					(*p).evt2_file = ''
					(*p).output_file = (*psort).output				; *** after new sort control, Parallel ...
					suppress = (*psort).suppress

					*(*pstate).path = extract_path( (*p).output_file)
					print,'EVT: path = ', *(*pstate).path
					
					active = get_active( p, enable, type, mode, cal_a, cal_b, ecompress, file)

;					widget_control_update, event.top, update=0
					widget_control, (*pstate).comment, set_value=(*p).comment
					widget_control, (*pstate).evt2_file, set_value=''
					set_widget_text, (*pstate).evt_file, (*p).evt_file

					set_widget_text, (*pstate).output_file, (*p).output_file

;					For Maia like data files, pull out the Dwell time for each in the series ...
;					Is the best way to handle changing dwell, or should 'dwell' be added as a new
;					column in the Batch Sort table? 

					head = get_header_info( DevObj, (*p).evt_file, group=event.top, /silent, error=error)
					if error eq 0 then begin
						if head.scan.dwell gt 1.0e-6 then begin
							(*p).dwell = head.scan.dwell 
							widget_control, (*pstate).dwell_id, set_value=str_tidy((*p).dwell)
						endif
						(*p).xorigin = head.scan.x
						(*p).yorigin = head.scan.y
						(*p).zorigin = head.scan.z
						(*p).facility = head.metadata.facility
						(*p).endstation = head.metadata.endstation
						(*p).comment = head.title
						(*p).sample = head.sample
						(*p).grain = head.grain
					endif

					widget_control, (*pstate).xrange, set_value=str_tidy((*p).xrange)
					widget_control, (*pstate).yrange, set_value=str_tidy((*p).yrange)
					widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
					widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)
					print, 'EVT output file: ', (*p).output_file
;					widget_control_update, event.top, update=1

					if (skip eq 0) or (file_test( (*p).output_file) eq 0) then begin
						widget_control, hourglass=1
						print,' evt-start: ', (*p).evt_file
						evt_start, pstate, group=event.top, error=error, suppress=suppress, pprefs=(*pstate).pprefs
						print,' evt return error=',error
					endif else if load then begin
						print, 'skip EVT, but load file: ', (*p).output_file
						pp = read_geopixe_image((*p).output_file)
						(*p).charge = (*pp).charge
						(*pstate).local_images = 0				; 'image' will own this
						(*pp).orphan = 1
						(*pstate).pimage = pp
						notify, 'images', (*pstate).pimage, from=event.top
						error = 0
					endif else begin
						error = 0
					endelse
					widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
					if error eq 0 then begin
						notify, 'path', (*pstate).path, from=event.top
						*(*pstate).pval = (*p).charge 
						notify, 'done-evt', (*pstate).pval, from=event.top
					endif else begin
						notify, 'abort-evt', from=event.top
					endelse
				endif
				end
			'wizard-action': begin
				if ptr_valid( event.pointer) then begin
					if (*event.pointer).window eq 'Sort EVT' then begin
						case (*event.pointer).command of
							'open-test': begin
;								print,'*** Wizard Sort EVT: test if window is open ...',event.top
								pw = (*pstate).pwiz
								*pw = *event.pointer		; make a copy to return 'event.top' as
								(*pw).top = event.top		; there may be mutiple EVT windows open
								(*pw).error = 0
								notify, 'wizard-return', pw
								end

							'sort-setup': begin
								print,'*** Wizard Sort EVT: Set details only ...'
								pw = event.pointer
								pd = (*pw).pdata
								if tag_present('DEVICE', *pd) then evt_set_device, pstate, (*pd).device, event.top
								if tag_present('DEVICE_OPTIONS', *pd) then (*(*pstate).pdev)->set_options, (*pd).device_options
								if tag_present('IMAGE_MODE', *pd) then evt_set_sort_mode, pstate, (*pd).image_mode, event.top
								if tag_present('OUTPUT', *pd) then evt_set_output_file, pstate, (*pd).output, event.top
								if tag_present('BLOG', *pd) then evt_set_evt_file, pstate, (*pd).blog, event.top, /no_cal_adopt
								evt_set_evt2_file, pstate, ''
								if tag_present('PILEUP', *pd) then evt_set_pileup_file, pstate, (*pd).pileup
								if tag_present('THROTTLE', *pd) then evt_set_throttle_file, pstate, (*pd).throttle
								if tag_present('LINEAR', *pd) then evt_set_linear_file, pstate, (*pd).linear
								if tag_present('CONV', *pd) then evt_set_charge_conversion, pstate, (*pd).conv
								if tag_present('CHARGE_MODE', *pd) then evt_set_charge_mode, pstate, (*pd).charge_mode, event.top
								if tag_present('FLUX_SCALER', *pd) then evt_set_preamp_pv, pstate, (*pd).flux_scaler
								if tag_present('GAIN_VALUE', *pd) then evt_set_preamp_sensitivity, pstate, (*pd).gain_value
								if tag_present('GAIN_UNITS', *pd) then evt_set_preamp_units, pstate, (*pd).gain_units
								if tag_present('ARRAY', *pd) then evt_set_array, pstate,  (*pd).array
								if tag_present('TYPE', *pd) then evt_set_type, pstate, (*pd).type, event.top
								if tag_present('CAL', *pd) then evt_getcal, pstate, (*pd).cal, event.top
								if tag_present('PROJ_MODE', *pd) then evt_set_proj_mode, pstate, (*pd).proj_mode, event.top
								if tag_present('DAM', *pd) then evt_set_proj_file, pstate, (*pd).dam

								(*pw).error = 0
								notify, 'wizard-return', pw
								end

							'sort-image': begin
								print,'*** Wizard Sort EVT: Set details and then sort images ...'
								pw = event.pointer
								pd = (*pw).pdata
								if tag_present('DEVICE', *pd) then evt_set_device, pstate, (*pd).device, event.top
								if tag_present('IMAGE_MODE', *pd) then evt_set_sort_mode, pstate, (*pd).image_mode, event.top
								if tag_present('OUTPUT', *pd) then evt_set_output_file, pstate, (*pd).output, event.top
								if tag_present('BLOG', *pd) then evt_set_evt_file, pstate, (*pd).blog, event.top, /no_output, /no_cal_adopt
								evt_set_evt2_file, pstate, ''
								if tag_present('PILEUP', *pd) then evt_set_pileup_file, pstate, (*pd).pileup
								if tag_present('THROTTLE', *pd) then evt_set_throttle_file, pstate, (*pd).throttle
								if tag_present('LINEAR', *pd) then evt_set_linear_file, pstate, (*pd).linear
								if tag_present('CONV', *pd) then evt_set_charge_conversion, pstate, (*pd).conv
								if tag_present('CHARGE_MODE', *pd) then evt_set_charge_mode, pstate, (*pd).charge_mode, event.top
								if tag_present('FLUX_SCALER', *pd) then evt_set_preamp_pv, pstate, (*pd).flux_scaler
								if tag_present('GAIN_VALUE', *pd) then evt_set_preamp_sensitivity, pstate, (*pd).gain_value
								if tag_present('GAIN_UNITS', *pd) then evt_set_preamp_units, pstate, (*pd).gain_units
								if tag_present('ARRAY', *pd) then evt_set_array, pstate,  (*pd).array
								if tag_present('TYPE', *pd) then evt_set_type, pstate, (*pd).type, event.top
								if tag_present('CAL', *pd) then evt_getcal, pstate, (*pd).cal, event.top
								if tag_present('PROJ_MODE', *pd) then evt_set_proj_mode, pstate, (*pd).proj_mode, event.top
								if tag_present('DAM', *pd) then evt_set_proj_file, pstate, (*pd).dam
								skip = 1
								load = 0
								if tag_present('SKIP', *pd) then skip = (*pd).skip			; skip sort if output file exists
								if tag_present('LOAD', *pd) then load = (*pd).load			; load image output file

								use = (*pw).qual1											; add this tag to filename
								dont = (*pw).qual2											; strip this off existing filename
								if (use ne '') or (dont ne '') then begin
									pt = extract_path((*p).output_file)
									T = strip_path( strip_file_ext( (*p).output_file))
									ext = extract_extension( (*p).output_file)
									if dont ne '' then T = strip_file_m( T, ending='-'+dont)
									if use ne '' then begin
										T = strip_file_m( T, ending='-'+use)
										if (locate('-'+use,T) eq -1) then T = T + '-'+use
									endif
									if lenchr(pt) lt 1 then pt = *(*pstate).path
									T = pt + T + '.'+ ext
									evt_set_output_file, pstate, T, event.top
								endif
								err = 1

								exists = file_test( (*p).output_file)
								if exists and (load or skip) then begin						; load existing images
									pp = read_geopixe_image( (*p).output_file, error=error)
									if error eq 0 then begin
										(*pstate).pimage = pp
										*(*pstate).path = extract_path( (*p).output_file)
										notify, 'path', (*pstate).path, from=event.top
										notify, 'images', (*pstate).pimage, from=event.top
										err = 0
									endif else skip = 0
								endif else skip = 0
								if (skip eq 0) then begin									; sort raw data into images
									verify = 0
									if tag_present('VERIFY', *pd) then verify=(*pd).verify
									(*p).flux = 0.0
									widget_control, hourglass=1
									evt_start, pstate, group=event.top, pprefs=(*pstate).pprefs, $
													verify=verify, /notify, file_return=sret, error=err
									pp = (*pstate).pimage
								endif

								(*pw).error = err
								if tag_present('OUTPUT', *pd) then (*pd).output = (*pp).file								; file-name
								if tag_present('CHARGE', *pd) then (*pd).charge = (*pp).charge						; return a new charge
								if tag_present('PNEW', *pd) then begin						; some file paths have changed
									*(*pd).pnew = {pfiles:ptr_new(), stats:{processed:(*pp).processed, clipped:(*pp).clipped, valid:(*pp).valid, bad_xy:(*pp).bad_xy}}
									if n_elements(sret) gt 0 then (*(*pd).pnew).pfiles = ptr_new( sret,/no_copy)
								endif
								notify, 'wizard-return', pw
								end
								
							else: begin
								warning,'EVT: Notify',['Unknown wizard command: '+(*event.pointer).command, $
										'Make sure GeoPIXE version is compatible with Wizard.']
							endelse
						endcase
					endif
				endif
				end
			else:
		endcase
		goto, finish
		end
	'WIDGET_TRACKING': begin
		if widget_info( event.id, /valid) eq 0 then return
		widget_control, event.id, get_uvalue=s
		if event.enter eq 1 then begin
			if size(s,/tname) eq 'STRING' then begin
				widget_control, (*pstate).help, set_value=s
			endif else if size(s,/tname) eq 'STRUCT' then begin
				t = tag_names( s)
				q = where( t eq 'HELP')
				if q[0] ne -1 then begin
					if size(s.Help,/tname) eq 'STRING' then begin
						widget_control, (*pstate).help, set_value=s.Help
					endif
				endif
			endif
		endif else begin
			widget_control, (*pstate).help, set_value=' '
		endelse
		goto, finish
		end
	'WIDGET_TIMER': begin
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request evt ...'
		goto, kill
		end
	else:
endcase

;	Read text widgets and update ...

	widget_control, (*pstate).evt_file, get_value=s
	(*p).evt_file = s
	widget_control, (*pstate).evt2_file, get_value=s
	(*p).evt2_file = s
	widget_control, (*pstate).throttle_file, get_value=s
	(*p).throttle_file = s
	widget_control, (*pstate).pileup_file, get_value=s
	if ((*p).sort_mode eq 1) and ((*p).XANES_dir eq 0) then (*p).xanes_energies_file = s else (*p).pileup_file = s
	widget_control, (*pstate).linearize_file, get_value=s
	(*p).linearize_file = s
	widget_control, (*pstate).output_file, get_value=s
	(*p).output_file = s

	widget_control, (*pstate).sample, get_value=s
	(*p).sample = s
	widget_control, (*pstate).grain, get_value=s
	(*p).grain = s
	widget_control, (*pstate).comment, get_value=s
	(*p).comment = s

	widget_control, (*pstate).xrange, get_value=s
	(*p).xrange = long2(s)
	widget_control, (*pstate).yrange, get_value=s
	(*p).yrange = long2(s)
	widget_control, (*pstate).zrange, get_value=s
	(*p).zrange = long2(s)
	widget_control, (*pstate).xoffset, get_value=s
	(*p).xoffset = long2(s)
	widget_control, (*pstate).yoffset, get_value=s
	(*p).yoffset = long2(s)
	widget_control, (*pstate).x_sub_range, get_value=s
	(*p).x_sub_range = long2(s)
	widget_control, (*pstate).y_sub_range, get_value=s
	(*p).y_sub_range = long2(s)
	widget_control, (*pstate).xsize, get_value=s
	(*p).xsize = float2(s)
	widget_control, (*pstate).ysize, get_value=s
	(*p).ysize = float2(s)
	widget_control, (*pstate).zsize, get_value=s
	(*p).zsize = float2(s)
	widget_control, (*pstate).zorigin, get_value=s
	(*p).zorigin = float2(s)

	widget_control, (*pstate).charge_conversion, get_value=s
	(*p).charge_conversion = float2(s)
	aps_count_to_charge = (*p).charge_conversion 
	widget_control, (*pstate).charge, get_value=s
	(*p).charge = float2(s)
	widget_control, (*pstate).step_size, get_value=s
	(*p).step_size = float2(s)
	widget_control, (*pstate).step_count, get_value=s
	(*p).step_count = long2(s)

	widget_control, (*pstate).cal_a, get_value=s
	(*p).cal_a[(*p).station] = float2(s)
	widget_control, (*pstate).cal_b, get_value=s
	(*p).cal_b[(*p).station] = float2(s)
	widget_control, (*pstate).file, get_value=s
	(*p).file[(*p).station] = s
	widget_control, (*pstate).el_select_text, get_value=s
	(*p).el_select = s
	widget_control, (*pstate).energy_file_text, get_value=s
	(*p).xanes_energies_file = s

;	device =		new device index
;
;	sort_mode = 	0	images
;					1	EXAFS spectrum	
;					2	XANES stack
;
;	xy_mode = 		0	Normal XY scan
;					1	X step scan
;					2	traverse step X,Y
;					3	Y step scan
;					4	XY-E (Z) XANES 3D scan
;					5	XY-Theta (Z) Tomo 3D scan
;
;	step_mode = 	0	Toggle bit X advance
;					1	Station count advance
;					2	Event count advance
;
;	mode =			0	DA projections
;					1	CUTs
;					2	mean energy STIM CUTs
;					3	MPDA method
;
;	Actually, step_mode=1 and step_mode=2 are really the same thing, but the default
;	station number is 0 for step_mode=1, and 2 for step_mode=2.
;	Note that these station numbers start at 0, but on GUI are shown starting from 1 (for MPsys).

;widget_control, event.top, update=0
uname = widget_info( event.id, /uname)
case uname of

	'start_button': begin
		widget_control, hourglass=1
		(*p).flux = 0.0
		evt_start, pstate, group=event.top, pprefs=(*pstate).pprefs, /verify
		end

	'export_DA_button': begin
		matrix = read_da( (*p).file[0], phases=phase_dai, pcorr=pcorr, mpda=mpda, eDA=eDA, error=err)
		if err eq 0 then begin
			if mpda then begin
				warning,'Evt event',"Can't export an MPDA matrix file for real-time."
			endif else begin
				export_DA2, matrix, group=event.top, pileup=(*p).pileup_file, throttle=(*p).throttle_file, linear=(*p).linearize_file
			endelse
		endif
		end

	'export_button': begin
		widget_control, hourglass=1
		evt_start, pstate, group=event.top, /export, pprefs=(*pstate).pprefs, /verify
		end

	'evt_TLB': begin
	;	print,' the timer event.'
	;	widget_control, event.top, timer=2.0
		end

	'close_button': begin
		print,'Close evt ...'
		goto, kill
		end

	'command_file_button':begin
		evt_start, pstate, group=event.top, /gencom, pprefs=(*pstate).pprefs, /verify
		end

	'update_button': begin
	  if ((*p).sort_mode eq 0)  then begin
		F = (*p).output_file
		widget_control, /hourglass
		pp = read_geopixe_image(F, version=version)
		if ptr_valid(pp) then begin
			*(*pstate).path = extract_path(F)
			if ptr_valid( (*pstate).pimage) then begin
				if (*(*pstate).pimage).orphan eq 1 then begin
					(*pstate).local_images = 1
					(*(*pstate).pimage).orphan = 0
				endif
				if ((*pstate).pimage ne pp) and ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
			endif
			(*pstate).pimage = pp
			(*pstate).local_images = 1
			(*pp).orphan = 0

			(*pp).charge = (*p).charge
			(*pp).IC.conversion = (*p).charge_conversion
			
			(*pp).scan.x = (*p).xsize * 0.001
			(*pp).scan.y = (*p).ysize * 0.001
			(*pp).sample = (*p).sample
			(*pp).grain = (*p).grain
			(*pp).comment = (*p).comment

			if version gt -2 then begin
				(*pp).xcompress = (*p).xcompress + 1
				(*pp).ycompress = (*p).ycompress + 1
				(*pp).xstep = (*p).step_count
				(*pp).xstep_on = ((*p).xy_mode ne 0) ? 1 : 0
			endif
			if version gt -16 then (*pp).ystep = (*p).xy_mode eq 3

			if version gt -3 then begin
				(*pp).matrix.file = (*p).file[(*p).station]
			endif

			if version gt -4 then begin
				(*pp).step_events = ((*p).step_mode eq 2) ? 1 : 0
			endif

			if version gt -5 then begin
;				(*pp).events = (*p).events
			endif

			if version gt -6 then begin
				(*pp).step_toggle = ((*p).step_mode eq 0) ? 1 : 0
				(*pp).toggle_bit = (*pstate).bit_numbers[(*p).step_bit]
				(*pp).toggle_station = (*p).step_station
			endif

			if version gt -7 then begin
;				(*pp).type ? = (*p).type ? 			; (ie. 0=conc, 1=mineral fraction)
			endif

			if version gt -8 then begin
				(*pp).channel = (*p).station
				(*pp).detector = (*p).type[(*p).station]
			endif

			write_geopixe_image, pp, F
			notify, 'path', (*pstate).path, from=event.top
		endif
	  endif
	  end

	'template_button': begin
		widget_control_update, event.top, /save
	  if ((*p).sort_mode eq 0) then begin
		path = *(*pstate).path
		F = file_requester( filter = ['*.dai','*.dai.*'], path=path, group=event.top, $
			title='Load image parameters from a .DAI file', fix_filter=0, /image, preview_routine='image_geopixe_preview')
		if F eq '' then goto, finish
		
		; This will also apply "set_options" on device parameters ...
		img = read_geopixe_image(F, /header, error=error)
		
		if error then goto, finish
		if ptr_valid(img) then begin
			widget_control, hourglass=1
			*(*pstate).path = extract_path(F)
			
			i = find_device( (*img).DevObj->name(), objects=*(*p).pDevObjList)
			if i ge 0 then begin
				DevObj = (*(*p).pDevObjList)[i]					; current device object

				options = (*img).DevObj->get_options(error=error)
				if error eq 0 then DevObj->set_options, options
				head = (*img).DevObj->show_header(error=error)	; will not work unless a full get_header_info has been done
				if error eq 0 then DevObj->set_header, head
			endif
			(*p).device = i > 0
			*(*pstate).pdev = DevObj
			notify, 'device', (*pstate).pdev, from=event.top

			evt_set_array, pstate, (*img).array					; is it a detector array mode DAI

			if DevObj->multi_files() then begin
				widget_control, (*pstate).evt2_base, map=1, scr_ysize=(*pstate).evt_base_ysize
				widget_control, (*pstate).evt2_base2, scr_ysize=(*pstate).evt_base_ysize
				widget_control, (*pstate).evt_button, set_value='First'+((((*p).sort_mode eq 1) and (*p).XANES_dir)?' Dir':' File:')
			endif else begin
				b = byte(DevObj->extension())
				if n_elements(b) gt 1 then begin
					t = string(b[1:*])
				endif else begin
					t = 'evt'
				endelse
				t = t + ((((*p).sort_mode eq 1) and (*p).XANES_dir)?' Dir':' File:')
				widget_control, (*pstate).evt2_base, map=0, scr_ysize=1
				widget_control, (*pstate).evt2_base2, scr_ysize=1
				widget_control, (*pstate).evt_button, set_value=t
			endelse
			if DevObj->linear() then begin
				widget_control, (*pstate).linearize_base, map=1, scr_ysize=(*pstate).evt_base_ysize
				widget_control, (*pstate).linearize_base2, scr_ysize=(*pstate).evt_base_ysize
			endif else begin
				widget_control, (*pstate).linearize_base, map=0, scr_ysize=1
				widget_control, (*pstate).linearize_base2, scr_ysize=1
			endelse
			if DevObj->throttle() then begin
				widget_control, (*pstate).throttle_base, map=1, scr_ysize=(*pstate).evt_base_ysize
				widget_control, (*pstate).throttle_base2, scr_ysize=(*pstate).evt_base_ysize
			endif else begin
				widget_control, (*pstate).throttle_base, map=0, scr_ysize=1
				widget_control, (*pstate).throttle_base2, scr_ysize=1
			endelse
			if DevObj->pileup() then begin
				widget_control, (*pstate).pileup_base, map=1, scr_ysize=(*pstate).evt_base_ysize
				widget_control, (*pstate).pileup_base2, scr_ysize=(*pstate).evt_base_ysize
			endif else begin
				widget_control, (*pstate).pileup_base, map=0, scr_ysize=1
				widget_control, (*pstate).pileup_base2, scr_ysize=1
			endelse
			DevObj->render_options, (*pstate).device_option_mode_base
			widget_control, (*pstate).device_option_mode_base, map=DevObj->show_sort_options(), scr_ysize=DevObj->get_sort_ysize()

			widget_control, (*pstate).station, set_combobox_select=(*p).station
			widget_control, (*pstate).device_mode, set_combobox_select=(*p).device
			
			(*p).energy_proxy_axis = (*img).energy_proxy_axis
			(*p).xanes_energies_file = (*img).energies_file
			widget_control, (*pstate).da_xanes_base1a, map=0
			widget_control, (*pstate).proxy_axis, set_combobox_select=(*p).energy_proxy_axis
			set_widget_text, (*pstate).energy_file_text, (*p).xanes_energies_file

			if (*img).xstep_on eq 0 then begin
				(*p).xy_mode = 0
				(*p).step_mode = 0
				widget_control, (*pstate).xy_mode, set_combobox_select=0
				widget_control, (*pstate).stepmode_base, map=0, scr_ysize=1
				widget_control, (*pstate).step_mode, set_combobox_select=0
				widget_control, (*pstate).size_base, map=0
				widget_control, (*pstate).da_xanes_base1b, map=1
				widget_control, (*pstate).da_xanes_base2, map=((*p).energy_proxy_axis gt 0)
			endif else begin
				if (*img).step_toggle eq 1 then begin
					(*p).xy_mode = 1
					(*p).step_mode = 0
					(*p).step_bit = where((*img).toggle_bit eq (*pstate).bit_numbers) > 0
				endif else if (*img).step_events eq 1 then begin
					(*p).xy_mode = 1
					(*p).step_mode = 2
				endif else begin
					(*p).xy_mode = 1
					(*p).step_mode = 1
				endelse
				if (*img).ystep then (*p).xy_mode=3
				(*p).step_station = (*img).toggle_station
				(*p).step_count = (*img).xstep
				widget_control, (*pstate).size_base, map=1, scr_ysize=(*pstate).xybase_ysize

				widget_control, (*pstate).xy_mode, set_combobox_select=(*p).xy_mode
				widget_control, (*pstate).step_mode, set_combobox_select=(*p).step_mode
				widget_control, (*pstate).step_count, set_value=str_tidy((*p).step_count)
				widget_control, (*pstate).step_station, set_combobox_select=(*p).step_station
				on_off = 0
				if (*p).step_mode eq 0 then on_off=1
				widget_control, (*pstate).stepbit_base, map=on_off
				widget_control, (*pstate).stepcount_base, map=1-on_off
;				geo = widget_info( (*pstate).stepmode_base, /geometry)
				widget_control, (*pstate).stepmode_base, map=1, scr_ysize=(*pstate).stepmode_ysize
				widget_control, (*pstate).size_base, map=1

				widget_control, (*pstate).da_xanes_base1b, map=0
				widget_control, (*pstate).da_xanes_base2, map=0
			endelse

			enable_cluster = DevObj->cluster()
			if enable_cluster eq 0 then (*p).cluster = 0
			cluster_OK = ((*p).sort_mode eq 0) or (((*p).sort_mode eq 2) and (((*p).xy_mode eq 0) or ((*p).xy_mode eq 4)))
			widget_control, (*pstate).cluster_id, sensitive=enable_cluster and cluster_OK

			(*p).station = (*img).channel
			(*p).array = (*img).array
			list = adc_list_device( DevObj, max_adcs = (*pstate).max_adcs)
			widget_control, (*pstate).station, set_value=list, set_combobox_select=(*p).station
			widget_control, (*pstate).array, set_combobox_select=(*p).array
			widget_control, (*pstate).detector_layout_base, map=(*p).array

			if (*img).array eq 1 then begin
				(*p).enable[*] = 0
				(*p).enable[ *(*img).pactive] = 1
				widget_control, (*pstate).enable, set_value=(*p).enable[(*p).station], sensitive=1
			endif else begin
				widget_control, (*pstate).enable, set_value=(*p).enable[(*p).station], sensitive=0
			endelse

			mode = (*img).mode
			if strlowcase(extract_extension((*img).matrix.file)) eq 'mpdam' then mode=3
			if (mode eq 3) or (mode eq 4) then widget_control, (*pstate).da_xanes_base1b, map=0

			if (*p).array eq 1 then begin
				chan = *(*img).pactive
				n_active = n_elements(chan)
				for j=0L,n_active-1 do begin
					i = chan[j]
					cal_ab, (*(*img).pcal)[j], ca,cb,cu
					(*p).cal_a[i] = ca
					(*p).cal_b[i] = cb
				endfor
				widget_control, (*pstate).cal_a, set_value=str_tidy((*p).cal_a[(*p).station], places=8)
				widget_control, (*pstate).cal_b, set_value=str_tidy((*p).cal_b[(*p).station])
				for i=0L,(*pstate).max_adcs-1 do begin
					(*p).ecompress[i] = (*img).ecompress
					(*p).type[i] = (*img).detector
					(*p).mode[i] = mode
					(*p).file[i] = (*img).matrix.file
				endfor
				*(*pstate).pval = (*p).type[0]
				notify, 'evt-type', (*pstate).pval, from=event.top
				*(*pstate).pval2 = (*p).mode[0]
				notify, 'evt-mode', (*pstate).pval2, from=event.top

				widget_control, (*pstate).type, set_combobox_select=(*p).type[(*p).station]
				widget_control, (*pstate).mode, set_combobox_select=(*p).mode[(*p).station]
				widget_control, (*pstate).file, set_value=(*p).file[(*p).station]
				widget_control, (*pstate).base_new_MPDA, map=(mode eq 3)
				widget_control, (*pstate).base_export, map=(mode ne 3)
				set_widget_text, (*pstate).file, (*p).file[(*p).station]
			endif else begin
				i = (*p).station
				cal_ab, (*img).cal, ca,cb,cu
				(*p).cal_a[i] = ca
				(*p).cal_b[i] = cb
				(*p).ecompress[i] = (*img).ecompress
				(*p).type[i] = (*img).detector
				(*p).mode[i] = mode
				(*p).file[i] = (*img).matrix.file
				*(*pstate).pval = (*p).type[i]
				notify, 'evt-type', (*pstate).pval, from=event.top
				*(*pstate).pval2 = (*p).mode[i]
				notify, 'evt-mode', (*pstate).pval2, from=event.top

				widget_control, (*pstate).cal_a, set_value=str_tidy((*p).cal_a[i], places=8)
				widget_control, (*pstate).cal_b, set_value=str_tidy((*p).cal_b[i])
				widget_control, (*pstate).type, set_combobox_select=(*p).type[i]
				widget_control, (*pstate).mode, set_combobox_select=(*p).mode[i]
				widget_control, (*pstate).file, set_value=(*p).file[i]
				widget_control, (*pstate).base_new_MPDA, map=(mode eq 3)
				widget_control, (*pstate).base_export, map=(mode ne 3)
				set_widget_text, (*pstate).file, (*p).file[i]
			endelse
			widget_control, (*pstate).file, sensitive=(mode ne 4)

			(*p).sample = (*img).sample
			(*p).grain = (*img).grain
			(*p).comment = (*img).comment
			widget_control, (*pstate).sample, set_value=(*p).sample
			widget_control, (*pstate).grain, set_value=(*p).grain
			widget_control, (*pstate).comment, set_value=(*p).comment
			(*p).facility = (*img).facility
			(*p).endstation = (*img).endstation

			(*p).evt_file = (*img).source
			widget_control, (*pstate).evt_file, set_value=(*p).evt_file
			set_widget_text, (*pstate).evt_file, (*p).evt_file

			(*p).evt2_file = (*img).source2
			widget_control, (*pstate).evt2_file, set_value=(*p).evt2_file
			set_widget_text, (*pstate).evt2_file, (*p).evt2_file

			(*p).throttle_file = (*img).throttle
			widget_control, (*pstate).throttle_file, set_value=(*p).throttle_file
			set_widget_text, (*pstate).throttle_file, (*p).throttle_file

			(*p).pileup_file = (*img).pileup
			widget_control, (*pstate).pileup_file, set_value=(*p).pileup_file
			set_widget_text, (*pstate).pileup_file, (*p).pileup_file

			(*p).linearize_file = (*img).linearize
			widget_control, (*pstate).linearize_file, set_value=(*p).linearize_file
			set_widget_text, (*pstate).linearize_file, (*p).linearize_file

			active = get_active( p, enable, type, mode, cal_a, cal_b, ecompress, file)

			T = strip_file_ext((*img).file)
			if (mode eq 1) then T = strip_file_m( T, ending='-cuts') + '-cuts'
			if (mode eq 3) then T = strip_file_m( T, ending='-MPDA') + '-MPDA'
			(*p).output_file = T + '.'+ (*pstate).outputs[(*p).sort_mode]
			set_widget_text, (*pstate).output_file, (*p).output_file

			fi = find_file2((*p).evt_file)
			if (fi[0] ne '') then begin
				*(*pstate).dpath = extract_path( (*p).evt_file)
				notify, 'dpath', (*pstate).dpath, from=event.top
				*(*pstate).path = build_output_path( (*p).evt_file, (*p).output_file, (*p).root, /set)
			endif
			*(*pstate).path = extract_path( (*p).output_file)
			notify, 'path', (*pstate).path, from=event.top
			notify, 'root', (*p).root, from=event.top
			(*p).xsize = (*img).scan.x * 1000.0
			(*p).ysize = (*img).scan.y * 1000.0
			(*p).xorigin = (*img).scan.origin.x
			(*p).yorigin = (*img).scan.origin.y

			(*p).xrange = (*img).original_xsize * (*img).xcompress
			(*p).yrange = (*img).original_ysize * (*img).ycompress
			(*p).xcompress = ((*img).xcompress - 1) > 0
			(*p).ycompress = ((*img).ycompress - 1) > 0

			(*p).image_mode = (*img).sub_region
			(*p).xoffset = (*img).xoffset
			(*p).yoffset = (*img).yoffset
			if ((*p).xoffset ne 0) or ((*p).yoffset ne 0) then (*p).image_mode=1
			(*p).x_sub_range = (*img).x_sub_range
			(*p).y_sub_range = (*img).y_sub_range
			if (*p).x_sub_range eq 0 then (*p).x_sub_range = (*img).xsize * (*p).xcompress
			if (*p).y_sub_range eq 0 then (*p).y_sub_range = (*img).ysize * (*p).ycompress
			
			(*p).charge = (*img).charge
			(*p).charge_mode = (*img).IC.mode
			(*p).charge_conversion = (*img).IC.conversion
			(*p).preamp.pv = (*img).IC.pv.name
			(*p).preamp.val = (*img).IC.pv.val
			(*p).preamp.unit = (*img).IC.pv.unit
			if ptr_valid((*img).plist) then begin
				if n_elements(*(*img).plist) gt 0 then begin
					*(*p).pic_list = *(*img).plist
					if (*p).preamp.pv eq '' then (*p).preamp.pv = (*(*p).pic_list)[0]
				endif
			endif
			evt_check_pvlist, (*p).pic_list, DevObj

			(*p).flux = 0.0
			if ptr_valid( (*img).flux) and (*img).has_flux then begin
				(*p).flux = total(*(*img).flux)
				if (*p).flux gt 1.0e-10 then begin
					if (*img).IC.mode gt 0 then begin
						if (*img).charge eq 0.0 then begin
							(*p).charge = (*p).flux * (*img).IC.conversion
							print,'                 charge zero, set it to flux * conv = ',(*p).charge
						endif else begin
							if abs((*img).IC.conversion - (*img).charge / (*p).flux) gt 1.0e-9 then begin
								(*p).charge_conversion = (*img).charge / (*p).flux
								print,'                 set IC.conversion = image.charge / total_flux = ',(*img).IC.conversion
							endif
						endelse
					endif
				endif
			endif else begin
				(*p).flux = 0.0
			endelse
			aps_count_to_charge = (*p).charge_conversion
			widget_control, (*pstate).charge_conversion, set_value=str_tidy((*p).charge_conversion)

			(*p).dwell = (*img).dwell.val
			(*p).use_dwell = (*img).dwell.on
			(*p).flatten = (*img).flatten
			case (*p).charge_mode of
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
			
			ionbeam = DevObj->ionbeam()
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

		;	widget_control, (*pstate).ic_val_mode, set_value='   '+str_tidy(ic_vals)
			widget_control, (*pstate).ic_unit_mode, set_value='   '+ic_units
			widget_control, (*pstate).charge_mode, set_value=qmodes, set_uvalue=qhelp
			widget_control, (*pstate).charge_mode, set_combobox_select=(*p).charge_mode

			widget_control, (*pstate).charge_conversion, set_value=str_tidy((*p).charge_conversion)
			widget_control, (*pstate).dwell_base, sensitive=(*p).use_dwell
			widget_control, (*pstate).dwell_id, set_value=str_tidy((*p).dwell)
			widget_control, (*pstate).ic_pv_mode, set_value='   '+*(*p).pic_list
			q = where( (*p).preamp.pv eq *(*p).pic_list, nq)
			widget_control, (*pstate).ic_pv_mode, set_combobox_select=(nq ne 0) ? q[0] : 0

			l = locate('time', strlowcase((*p).preamp.pv))
			val = (*p).preamp.val
			unit = (*p).preamp.unit
			ival = find_charge_val_unit_index( val, unit, iunit=iunit, time=(l ge 0), /write_back)
			(*p).preamp.val = val
			(*p).preamp.unit = unit
			widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
			widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit
			OK = (locate('.time',(*p).preamp.pv) lt 0)
			widget_control, (*pstate).ic_val_mode, sensitive=OK
			widget_control, (*pstate).ic_unit_mode, sensitive=OK

			widget_control, (*pstate).flatten_id, set_value=(*p).flatten			
			widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
			widget_control, (*pstate).xrange, set_value=str_tidy((*p).xrange)
			widget_control, (*pstate).yrange, set_value=str_tidy((*p).yrange)
			widget_control, (*pstate).xcompress, set_combobox_select=(*p).xcompress
			widget_control, (*pstate).ycompress, set_combobox_select=(*p).ycompress
			widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
			widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)
			widget_control, (*pstate).image_mode, set_combobox_select=(*p).image_mode
			widget_control, (*pstate).xoffset, set_value=str_tidy((*p).xoffset)
			widget_control, (*pstate).yoffset, set_value=str_tidy((*p).yoffset)
			widget_control, (*pstate).x_sub_range, set_value=str_tidy((*p).x_sub_range)
			widget_control, (*pstate).y_sub_range, set_value=str_tidy((*p).y_sub_range)
			if (*p).image_mode ne 0 then begin
				widget_control, (*pstate).offset_base, map=1, scr_ysize=(*pstate).offset_ysize
			endif else begin
				widget_control, (*pstate).offset_base, map=0, scr_ysize=1
			endelse

			if (*p).xy_mode ne 0 then begin
				if ((*p).xy_mode eq 3) and ((*p).ysize gt 0.01) then begin
					um_per_step = prefs_Resolution.Y
					sy = (*p).ysize / float((*p).yrange)
					ny = round( sy / um_per_step )
					(*p).step_size_index = clip(ny-1,0,29)
					(*p).step_size = um_per_step * float(ny)
					widget_control, (*pstate).step_size_drop, set_combobox_select = (*p).step_size_index
					widget_control, (*pstate).step_size, set_value=str_tidy((*p).step_size)
				endif else if ((*p).xsize gt 0.01) then begin
					um_per_step = prefs_Resolution.X
					sx = (*p).xsize / float((*p).xrange)
					nx = round( sx / um_per_step )
					(*p).step_size_index = clip(nx-1,0,29)
					(*p).step_size =  um_per_step * float(nx)
					widget_control, (*pstate).step_size_drop, set_combobox_select = (*p).step_size_index
					widget_control, (*pstate).step_size, set_value=str_tidy((*p).step_size)
				endif
			endif

			free_images, img
			notify, 'evt-load', from=event.top
		endif
	  endif else if (*p).sort_mode eq 2 then begin
		path = *(*pstate).path
		F = file_requester( filter = ['*.xan'], path=path, group=event.top, $
			title='Load image parameters from a .XAN file', fix_filter=0, /image, preview_routine='image_geopixe_preview')
		if F eq '' then goto, finish
		
		; This will also apply "set_options" on device parameters ...
		img = read_geopixe_image(F, /xanes, /header, error=error)
		
		if error then goto, finish
		if ptr_valid(img) then begin
			widget_control, hourglass=1
			*(*pstate).path = extract_path(F)
			
			i = find_device( (*img).DevObj->name(), objects=*(*p).pDevObjList)
			if i ge 0 then begin
				DevObj = (*(*p).pDevObjList)[i]					; current device object

				options = (*img).DevObj->get_options(error=error)
				if error eq 0 then DevObj->set_options, options
				head = (*img).DevObj->show_header(error=error)
				if error eq 0 then DevObj->set_header, head
			endif
			(*p).device = i > 0
			*(*pstate).pdev = DevObj
			notify, 'device', (*pstate).pdev, from=event.top

			if DevObj->multi_files() then begin
				widget_control, (*pstate).evt2_base, map=1, scr_ysize=(*pstate).evt_base_ysize
				widget_control, (*pstate).evt2_base2, scr_ysize=(*pstate).evt_base_ysize
				widget_control, (*pstate).evt_button, set_value='First'+((((*p).sort_mode eq 1) and (*p).XANES_dir)?' Dir':' File:')
			endif else begin
				b = byte(DevObj->extension())
				if n_elements(b) gt 1 then begin
					t = string(b[1:*])
				endif else begin
					t = 'evt'
				endelse
				t = t + ((((*p).sort_mode eq 1) and (*p).XANES_dir)?' Dir':' File:')
				widget_control, (*pstate).evt2_base, map=0, scr_ysize=1
				widget_control, (*pstate).evt2_base2, scr_ysize=1
				widget_control, (*pstate).evt_button, set_value=t
			endelse
			if DevObj->linear() then begin
				widget_control, (*pstate).linearize_base, map=1, scr_ysize=(*pstate).evt_base_ysize
				widget_control, (*pstate).linearize_base2, scr_ysize=(*pstate).evt_base_ysize
			endif else begin
				widget_control, (*pstate).linearize_base, map=0, scr_ysize=1
				widget_control, (*pstate).linearize_base2, scr_ysize=1
			endelse
			if DevObj->throttle() then begin
				widget_control, (*pstate).throttle_base, map=1, scr_ysize=(*pstate).evt_base_ysize
				widget_control, (*pstate).throttle_base2, scr_ysize=(*pstate).evt_base_ysize
			endif else begin
				widget_control, (*pstate).throttle_base, map=0, scr_ysize=1
				widget_control, (*pstate).throttle_base2, scr_ysize=1
			endelse
			if DevObj->pileup() then begin
				widget_control, (*pstate).pileup_base, map=1, scr_ysize=(*pstate).evt_base_ysize
				widget_control, (*pstate).pileup_base2, scr_ysize=(*pstate).evt_base_ysize
			endif else begin
				widget_control, (*pstate).pileup_base, map=0, scr_ysize=1
				widget_control, (*pstate).pileup_base2, scr_ysize=1
			endelse
			DevObj->render_options, (*pstate).device_option_mode_base
			widget_control, (*pstate).device_option_mode_base, map=DevObj->show_sort_options(), scr_ysize=DevObj->get_sort_ysize()

			widget_control, (*pstate).station, set_combobox_select=(*p).station
			widget_control, (*pstate).device_mode, set_combobox_select=(*p).device
			
			(*p).xy_mode = ((*img).stack_type eq 1) ? 5 : 4
			(*p).step_mode = 0
			widget_control, (*pstate).xy_mode, set_combobox_select=(*p).xy_mode
			widget_control, (*pstate).stepmode_base, map=0, scr_ysize=1
			widget_control, (*pstate).step_mode, set_combobox_select=0
			widget_control, (*pstate).size_base, map=0
			widget_control, (*pstate).da_xanes_base1b, map=0
			widget_control, (*pstate).proxy_axis, set_combobox_select=(*p).energy_proxy_axis
			if (*p).xy_mode eq 4 then begin
				widget_control, (*pstate).zorigin_base, map=1
				widget_control, (*pstate).zcompress_base, map=0
				widget_control, (*pstate).da_xanes_base1a, map=1
				widget_control, (*pstate).da_xanes_base2, map=1
			endif else if (*p).xy_mode eq 5 then begin
				widget_control, (*pstate).zorigin_base, map=0
				widget_control, (*pstate).zcompress_base, map=1
				widget_control, (*pstate).da_xanes_base1a, map=0
				widget_control, (*pstate).da_xanes_base2, map=0
			endif

			(*p).station = (*img).channel
			(*p).array = (*img).array
			list = adc_list_device( DevObj, max_adcs = (*pstate).max_adcs)
			widget_control, (*pstate).station, set_value=list, set_combobox_select=(*p).station
			widget_control, (*pstate).array, set_combobox_select=(*p).array
			widget_control, (*pstate).detector_layout_base, map=(*p).array

			if (*img).array eq 1 then begin
				(*p).enable[*] = 0
				(*p).enable[ *(*img).pactive] = 1
				widget_control, (*pstate).enable, set_value=(*p).enable[(*p).station], sensitive=1
			endif else begin
				widget_control, (*pstate).enable, set_value=(*p).enable[(*p).station], sensitive=0
			endelse

			enable_cluster = DevObj->cluster()
			if enable_cluster eq 0 then (*p).cluster = 0
			cluster_OK = ((*p).sort_mode eq 0) or (((*p).sort_mode eq 2) and (((*p).xy_mode eq 0) or ((*p).xy_mode eq 4)))
			widget_control, (*pstate).cluster_id, sensitive=enable_cluster and cluster_OK

			mode = (*img).mode
			if (*p).array eq 1 then begin
				chan = *(*img).pactive
				n_active = n_elements(chan)
				for j=0L,n_active-1 do begin
					i = chan[j]
					cal_ab, (*(*img).pcal)[j], ca,cb,cu
					(*p).cal_a[i] = ca
					(*p).cal_b[i] = cb
				endfor
				widget_control, (*pstate).cal_a, set_value=str_tidy((*p).cal_a[(*p).station], places=8)
				widget_control, (*pstate).cal_b, set_value=str_tidy((*p).cal_b[(*p).station])
				for i=0L,(*pstate).max_adcs-1 do begin
					(*p).ecompress[i] = (*img).ecompress
					(*p).type[i] = (*img).detector
					(*p).mode[i] = mode
					(*p).file[i] = (*img).matrix.file
				endfor
				*(*pstate).pval = (*p).type[0]
				notify, 'evt-type', (*pstate).pval, from=event.top
				*(*pstate).pval2 = (*p).mode[0]
				notify, 'evt-mode', (*pstate).pval2, from=event.top

				widget_control, (*pstate).type, set_combobox_select=(*p).type[(*p).station]
				widget_control, (*pstate).mode, set_combobox_select=(*p).mode[(*p).station]
				widget_control, (*pstate).file, set_value=(*p).file[(*p).station]
				set_widget_text, (*pstate).file, (*p).file[(*p).station]
			endif else begin
				i = (*p).station
				cal_ab, (*img).cal, ca,cb,cu
				(*p).cal_a[i] = ca
				(*p).cal_b[i] = cb
				(*p).ecompress[i] = (*img).ecompress
				(*p).type[i] = (*img).detector
				(*p).mode[i] = mode
				(*p).file[i] = (*img).matrix.file
				*(*pstate).pval = (*p).type[i]
				notify, 'evt-type', (*pstate).pval, from=event.top
				*(*pstate).pval2 = (*p).mode[i]
				notify, 'evt-mode', (*pstate).pval2, from=event.top

				widget_control, (*pstate).cal_a, set_value=str_tidy((*p).cal_a[i], places=8)
				widget_control, (*pstate).cal_b, set_value=str_tidy((*p).cal_b[i])
				widget_control, (*pstate).type, set_combobox_select=(*p).type[i]
				widget_control, (*pstate).mode, set_combobox_select=(*p).mode[i]
				widget_control, (*pstate).file, set_value=(*p).file[i]
				set_widget_text, (*pstate).file, (*p).file[i]
			endelse

			(*p).sample = (*img).sample
			(*p).grain = (*img).grain
			(*p).comment = (*img).comment
			widget_control, (*pstate).sample, set_value=(*p).sample
			widget_control, (*pstate).grain, set_value=(*p).grain
			widget_control, (*pstate).comment, set_value=(*p).comment

			(*p).evt_file = (*img).source
			widget_control, (*pstate).evt_file, set_value=(*p).evt_file
			set_widget_text, (*pstate).evt_file, (*p).evt_file

			(*p).evt2_file = (*img).source2
			widget_control, (*pstate).evt2_file, set_value=(*p).evt2_file
			set_widget_text, (*pstate).evt2_file, (*p).evt2_file

			(*p).throttle_file = (*img).throttle
			widget_control, (*pstate).throttle_file, set_value=(*p).throttle_file
			set_widget_text, (*pstate).throttle_file, (*p).throttle_file

			(*p).pileup_file = (*img).pileup
			widget_control, (*pstate).pileup_file, set_value=(*p).pileup_file
			set_widget_text, (*pstate).pileup_file, (*p).pileup_file

			(*p).linearize_file = (*img).linearize
			widget_control, (*pstate).linearize_file, set_value=(*p).linearize_file
			set_widget_text, (*pstate).linearize_file, (*p).linearize_file

			active = get_active( p, enable, type, mode, cal_a, cal_b, ecompress, file)

			T = strip_file_ext((*img).file)
			if (mode eq 1) then T = strip_file_m( T, ending='-cuts') + '-cuts'
			if (mode eq 3) then T = strip_file_m( T, ending='-MPDA') + '-MPDA'
			(*p).output_file = T + '.'+ (*pstate).outputs[(*p).sort_mode]
			set_widget_text, (*pstate).output_file, (*p).output_file

			fi = find_file2((*p).evt_file)
			if (fi[0] ne '') then begin
				*(*pstate).dpath = extract_path( (*p).evt_file)
				notify, 'dpath', (*pstate).dpath, from=event.top
				*(*pstate).path = build_output_path( (*p).evt_file, (*p).output_file, (*p).root, /set)
			endif
			*(*pstate).path = extract_path( (*p).output_file)
			notify, 'path', (*pstate).path, from=event.top
			notify, 'root', (*p).root, from=event.top
			(*p).xsize = (*img).scan.x * 1000.0
			(*p).ysize = (*img).scan.y * 1000.0
			(*p).zsize = (*img).scan.z * 1000.0
			(*p).xorigin = (*img).scan.origin.x
			(*p).yorigin = (*img).scan.origin.y
			(*p).zorigin = (*img).scan.origin.z

			(*p).xrange = (*img).original_xsize * (*img).xcompress
			(*p).yrange = (*img).original_ysize * (*img).ycompress
			(*p).zrange = (*img).original_zsize * (*img).zcompress
			(*p).xcompress = ((*img).xcompress - 1) > 0
			(*p).ycompress = ((*img).ycompress - 1) > 0
			(*p).zcompress = ((*img).zcompress - 1) > 0

			(*p).xanes_energies_file = (*img).energies_file
			(*p).el_select = (*img).el
	
			(*p).image_mode = (*img).sub_region
			(*p).xoffset = (*img).xoffset
			(*p).yoffset = (*img).yoffset
			if ((*p).xoffset ne 0) or ((*p).yoffset ne 0) then (*p).image_mode=1
			(*p).x_sub_range = (*img).x_sub_range
			(*p).y_sub_range = (*img).y_sub_range
			if (*p).x_sub_range eq 0 then (*p).x_sub_range = (*img).xsize * (*p).xcompress
			if (*p).y_sub_range eq 0 then (*p).y_sub_range = (*img).ysize * (*p).ycompress
			
			(*p).charge = (*img).charge
			(*p).charge_mode = (*img).IC.mode
			(*p).charge_conversion = (*img).IC.conversion
			(*p).preamp.pv = (*img).IC.pv.name
			(*p).preamp.val = (*img).IC.pv.val
			(*p).preamp.unit = (*img).IC.pv.unit
			if ptr_valid((*img).plist) then begin
				if n_elements(*(*img).plist) gt 0 then begin
					*(*p).pic_list = *(*img).plist
					if (*p).preamp.pv eq '' then (*p).preamp.pv = (*(*p).pic_list)[0]
				endif
			endif
			evt_check_pvlist, (*p).pic_list, DevObj

			(*p).flux = 0.0
			if ptr_valid( (*img).flux) and (*img).has_flux then begin
				(*p).flux = total(*(*img).flux)
				if (*p).flux gt 1.0e-10 then begin
					if (*img).IC.mode gt 0 then begin
						if (*img).charge eq 0.0 then begin
							(*p).charge = (*p).flux * (*img).IC.conversion
							print,'                 charge zero, set it to flux * conv = ',(*p).charge
						endif else begin
							if abs((*img).IC.conversion - (*img).charge / (*p).flux) gt 1.0e-9 then begin
								(*p).charge_conversion = (*img).charge / (*p).flux
								print,'                 set IC.conversion = image.charge / total_flux = ',(*img).IC.conversion
							endif
						endelse
					endif
				endif
			endif else begin
				(*p).flux = 0.0
			endelse
			aps_count_to_charge = (*p).charge_conversion
			widget_control, (*pstate).charge_conversion, set_value=str_tidy((*p).charge_conversion)

			(*p).dwell = (*img).dwell.val
			(*p).use_dwell = (*img).dwell.on
			(*p).flatten = (*img).flatten
			widget_control, (*pstate).charge_mode, set_combobox_select=(*p).charge_mode
			case (*p).charge_mode of
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
			widget_control, (*pstate).charge_conversion, set_value=str_tidy((*p).charge_conversion)
			widget_control, (*pstate).dwell_base, sensitive=(*p).use_dwell
			widget_control, (*pstate).dwell_id, set_value=str_tidy((*p).dwell)
			widget_control, (*pstate).ic_pv_mode, set_value='   '+*(*p).pic_list
			q = where( (*p).preamp.pv eq *(*p).pic_list, nq)
			widget_control, (*pstate).ic_pv_mode, set_combobox_select=(nq ne 0) ? q[0] : 0

			l = locate('time', strlowcase((*p).preamp.pv))
			val = (*p).preamp.val
			unit = (*p).preamp.unit
			ival = find_charge_val_unit_index( val, unit, iunit=iunit, time=(l ge 0), /write_back)
			(*p).preamp.val = val
			(*p).preamp.unit = unit
			widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
			widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit
			OK = (locate('.time',(*p).preamp.pv) lt 0)
			widget_control, (*pstate).ic_val_mode, sensitive=OK
			widget_control, (*pstate).ic_unit_mode, sensitive=OK

			widget_control, (*pstate).flatten_id, set_value=(*p).flatten			
			widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
			widget_control, (*pstate).xrange, set_value=str_tidy((*p).xrange)
			widget_control, (*pstate).yrange, set_value=str_tidy((*p).yrange)
			widget_control, (*pstate).xcompress, set_combobox_select=(*p).xcompress
			widget_control, (*pstate).ycompress, set_combobox_select=(*p).ycompress
			widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
			widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)

			widget_control, (*pstate).zrange, set_value=str_tidy((*p).zrange)
			widget_control, (*pstate).zcompress, set_combobox_select=(*p).zcompress
			widget_control, (*pstate).zsize, set_value=str_tidy((*p).zsize)
			widget_control, (*pstate).zorigin, set_value=str_tidy((*p).zorigin)
			set_widget_text, (*pstate).energy_file_text, (*p).xanes_energies_file
			set_widget_text, (*pstate).el_select_text, (*p).el_select
			
			widget_control, (*pstate).image_mode, set_combobox_select=(*p).image_mode
			widget_control, (*pstate).xoffset, set_value=str_tidy((*p).xoffset)
			widget_control, (*pstate).yoffset, set_value=str_tidy((*p).yoffset)
			widget_control, (*pstate).x_sub_range, set_value=str_tidy((*p).x_sub_range)
			widget_control, (*pstate).y_sub_range, set_value=str_tidy((*p).y_sub_range)
			if (*p).image_mode ne 0 then begin
				widget_control, (*pstate).offset_base, map=1, scr_ysize=(*pstate).offset_ysize
			endif else begin
				widget_control, (*pstate).offset_base, map=0, scr_ysize=1
			endelse

			free_images, img
			notify, 'evt-load', from=event.top
		endif
	  endif
	  widget_control_update, event.top, /rest
	  end

	'sort_mode': begin
		evt_set_sort_mode, pstate, event.value, event.top
		end

	'device_mode': begin
		(*p).device = event.index
		DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object
		name = DevObj->name()
		evt_set_device, pstate, name, event.top
		end

	'cluster-option': begin
		case event.value of
			0: begin
				if DevObj->cluster() eq 0 then begin
					warning,'EVT','Cluster processing only available for Maia device for now.'
					(*p).cluster = 0
				endif else begin
					(*p).cluster = event.select
				endelse
				end
			else:
		endcase
		end
		
	'batch_button': begin
		batch_sort, group_leader=event.top, dpath=*(*pstate).dpath, path=*(*pstate).path, root=(*p).root, $
						mode=(*p).mode[(*p).station], type=(*p).type[(*p).station], TLB=tlb, device=DevObj
		register_notify, event.top, ['start-evt', $		; start sort from Batch_Sort
							'batch-filter', $			; digital filter to Image
							'batch-operations-open', $	; open Image operations window
							'batch-rgb-open', $			; open RGB Image window
							'batch-save'], $			; save images, HTML to Image
						from=tlb
		end

	'evt_button': begin
		dpath = *(*pstate).dpath
		file = (*p).evt_file
		dir_mode = (((*p).sort_mode eq 1) and (*p).XANES_dir)
		F = file_requester( /read, filter = ((*p).sort_mode eq 1) ? '*' : ('*'+DevObj->extension()), $
					path=dpath, group=event.top, file=file, dir=dir_mode, updir=4, $
					title='Select the source list-mode data', fix_filter=0, multi_char=DevObj->multi_char(), $
					numeric=(DevObj->multi_files() and (DevObj->extension() eq '') and (DevObj->multi_char() eq '.')) )
		if F ne '' then begin
			evt_set_evt_file, pstate, F[0], event.top
			evt_set_evt2_file, pstate, ''
			notify, 'dpath', (*pstate).dpath, from=event.top
		endif
		end
	'evt_file': begin
		widget_control, event.id, get_value=s
		(*p).evt_file = s
		file = find_file2( (*p).evt_file)
		if lenchr(file[0]) gt 0 then begin
			evt_set_evt_file, pstate, file[0], event.top
			evt_set_evt2_file, pstate, ''
		endif
		end
	'evt2_button': begin
		dpath = *(*pstate).dpath
		file = (*p).evt2_file
		if extract_path( file) ne extract_path( (*p).evt_file) then begin
			file = ''
			dpath = extract_path( (*p).evt_file)
		endif
		F = file_requester( /read, filter = ((*p).sort_mode eq 1) ? '*' : '*'+DevObj->extension(), $
					path=dpath, group=event.top, file=file, dir=(((*p).sort_mode eq 1) and (*p).XANES_dir), $
					title='Select the last list-mode data', fix_filter=0, multi_char=DevObj->multi_char(), /latest, $
					numeric=(DevObj->multi_files() and (DevObj->extension() eq '') and (DevObj->multi_char() eq '.')) )
		if F ne '' then begin
			evt_set_evt2_file, pstate, F
		endif
		end
	'evt2_file': begin
		widget_control, event.id, get_value=s
		evt_set_evt2_file, pstate, s
		end

	'throttle_button': begin
		file = (*p).throttle_file
		path = *(*pstate).path
		F = file_requester( /read, filter = ['*.throttle.var','*.txt'], file=file[0], $
					path=path, group=event.top, updir=3, $
					title='Select the throttle factors file', fix_filter=0)
		if F ne '' then begin
			evt_set_throttle_file, pstate, F
		endif
		end
	'throttle_file': begin
		widget_control, event.id, get_value=s
		evt_set_throttle_file, pstate, s
		end

	'pileup_button': begin
		if ((*p).sort_mode eq 1) and ((*p).XANES_dir eq 0) then begin
			file1 = (*p).xanes_energies_file
			title = 'Select the XANES energies file'
		endif else begin
			file1 = (*p).pileup_file
			title = 'Select the pileup time limits file'
		endelse
		file = file1
		path = *(*pstate).path
		F = file_requester( /read, filter = ['*.pileup.var','*.txt'], file=file[0], $
					path=path, group=event.top, title=title, fix_filter=0, updir=3)
		if F ne '' then begin
			evt_set_pileup_file, pstate, F
		endif
		end
	'pileup_file': begin
		widget_control, event.id, get_value=s
		evt_set_pileup_file, pstate, s
		end

	'linearize_button': begin
		file = (*p).linearize_file
		path = *(*pstate).path
		F = file_requester( /read, filter = ['*.linear.var','*.linear'], file=file[0], $
					path=path, group=event.top, updir=3, $
					title='Select the linearization function file', fix_filter=0)
		if F ne '' then begin
			evt_set_linear_file, pstate, F
		endif
		end
	'linearize_file': begin
		widget_control, event.id, get_value=s
		(*p).linearize_file = s
		end

	'output_button': begin
		file = (*p).output_file
		path = *(*pstate).path
		F = file_requester( /write, filter = '*.'+(*pstate).outputs[(*p).sort_mode], $
			path=path, group=event.top, file=file, $
			title='Select the output file', fix_filter=1)
		if F ne '' then begin
			evt_set_output_file, pstate, F[0], event.top
		endif
		end
	'output_file': begin
		widget_control, event.id, get_value=F
		(*p).output_file = F
		if strlen(F) gt 0 then begin
			evt_set_output_file, pstate, F[0], event.top
		endif
		end

	'sample': begin
		widget_control, event.id, get_value=s
		(*p).sample = s
		end
	'grain': begin
		widget_control, event.id, get_value=s
		(*p).grain = s
		end
	'comment': begin
		widget_control, event.id, get_value=s
		(*p).comment = s
		end

	'xy_mode': begin
		evt_set_xy_mode, pstate, event.index, event.top
		end

	'step_mode': begin
		(*p).step_mode = event.index
		on_off = 0
		if event.index eq 0 then on_off=1
		widget_control, (*pstate).stepbit_base, map=on_off
		widget_control, (*pstate).stepcount_base, map=1-on_off
		end
	'step_station': begin
		(*p).step_station = event.index
		end
	'step_count': begin
;		widget_control, event.id, get_value=s
;		(*p).step_count = long(s)
		end
	'step_size_drop': begin
		(*p).step_size_index = event.index
		widget_control_update, event.top, update=0
		case (*p).xy_mode of
			1: begin
				um_per_step = prefs_Resolution.X
				(*p).step_size = um_per_step * float(1 + event.index)
				widget_control, (*pstate).step_size, set_value=str_tidy((*p).step_size)
				(*p).xsize = (*p).xrange * (*p).step_size
				widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
				(*p).ysize = (*p).yrange * (*p).step_size
				widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)
				end

			2: begin
				um_per_step = prefs_Resolution.X
				(*p).step_size = um_per_step * float(1 + event.index)
				widget_control, (*pstate).step_size, set_value=str_tidy((*p).step_size)
				(*p).xsize = (*p).xrange * (*p).step_size
				widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
				end

			3: begin
				um_per_step = prefs_Resolution.X
				(*p).step_size = um_per_step * float(1 + event.index)
				widget_control, (*pstate).step_size, set_value=str_tidy((*p).step_size)
				(*p).ysize = (*p).yrange * (*p).step_size
				widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)
				(*p).xsize = (*p).xrange * (*p).step_size
				widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
				end
			else:
		endcase
		widget_control_update, event.top, update=1
		end
	'step_size': begin
		if (*p).sort_mode ne 2 then begin
			um_per_step = prefs_Resolution.X
			if ((*p).xy_mode eq 3) then um_per_step = prefs_Resolution.X
			widget_control, event.id, get_value=s
			i = (long((float(s[0]) / um_per_step) + 0.5) - 1 < 29)
			(*p).step_size_index = i
			widget_control, (*pstate).step_size_drop, set_combobox_select=i
			s = um_per_step * float(i+1)
			(*p).step_size = s
			widget_control, (*pstate).step_size, set_value=str_tidy((*p).step_size)
		endif
		if ((*p).sort_mode eq 2) or ((*p).xy_mode ne 3) then begin
			(*p).xsize = (*p).xrange * (*p).step_size
			widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
		endif else if ((*p).xy_mode eq 3) then begin
			(*p).ysize = (*p).yrange * (*p).step_size
			widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)
		endif
		end
	'step_bit': begin
		(*p).step_bit = event.index
		end

	'x_range': begin
;		widget_control, event.id, get_value=s
;		(*p).xrange = long(s)
		if ((*p).xy_mode ne 0) then begin
			(*p).xsize = (*p).xrange * (*p).step_size
			widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
		endif
		end
	'y_range': begin
;		widget_control, event.id, get_value=s
;		(*p).yrange = long(s)
		if ((*p).xy_mode eq 1) or ((*p).xy_mode eq 3) then begin
			(*p).ysize = (*p).yrange * (*p).step_size
			widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)
		endif
		end
	'z_range': begin
;		widget_control, event.id, get_value=s
;		(*p).zrange = long(s)
		end
	'z_origin': begin
;		widget_control, event.id, get_value=s
;		(*p).zorigin = long(s)
		end

	'image_mode': begin
		(*p).image_mode = event.index
		widget_control_update, event.top, update=0
		if (*p).image_mode ne 0 then begin
			widget_control, (*pstate).offset_base, map=1, scr_ysize=(*pstate).offset_ysize
		endif else begin
			widget_control, (*pstate).offset_base, map=0, scr_ysize=1
		endelse
		widget_control_update, event.top, update=1
		end
	'x_offset': begin
;		widget_control, event.id, get_value=s
;		(*p).xoffset = long2(s)
		end
	'y_offset': begin
;		widget_control, event.id, get_value=s
;		(*p).yoffset = long2(s)
		end
	'x_sub_range': begin
;		widget_control, event.id, get_value=s
;		(*p).x_sub_range = long2(s)
		end
	'y_sub_range': begin
;		widget_control, event.id, get_value=s
;		(*p).y_sub_range = long2(s)
		end
	'get-window-region': begin
		if n_elements(region_window) gt 0 then begin
			(*p).xoffset = region_window.offset.x
			(*p).yoffset = region_window.offset.y
			(*p).x_sub_range = region_window.range.x
			(*p).y_sub_range = region_window.range.y
			widget_control, (*pstate).xoffset, set_value=str_tidy((*p).xoffset)
			widget_control, (*pstate).yoffset, set_value=str_tidy((*p).yoffset)
			widget_control, (*pstate).x_sub_range, set_value=str_tidy((*p).x_sub_range)
			widget_control, (*pstate).y_sub_range, set_value=str_tidy((*p).y_sub_range)
		endif
		end

	'steprange_button': begin
		if (*p).xy_mode ne 0 then begin
			range = range_select( event.top)
			if range.error then goto, finish
			scan = 1000. * abs(range.stop - range.start)
			pixels = round(scan / (*p).step_size)
			(*p).charge = float(pixels * range.count) * 1.0e-6 * range.charge
			widget_control, (*pstate).charge, set_value = str_tidy((*p).charge)
			if ((*p).xy_mode eq 1) or ((*p).xy_mode eq 2) then begin
				(*p).xrange = pixels
				(*p).xsize = scan
				widget_control, (*pstate).xrange, set_value=str_tidy(pixels)
				widget_control, (*pstate).xsize, set_value=str_tidy(scan)
			endif else if ((*p).xy_mode eq 3) then begin
				(*p).yrange = pixels
				(*p).ysize = scan
				widget_control, (*pstate).yrange, set_value=str_tidy(pixels)
				widget_control, (*pstate).ysize, set_value=str_tidy(scan)
			endif
		endif
		end
	'setsize_button': begin
		if (*p).xy_mode ne 0 then begin
			(*p).xsize = (*p).xrange * (*p).step_size
			widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
		endif
		if ((*p).xy_mode eq 1) or ((*p).xy_mode eq 3) then begin
			(*p).ysize = (*p).yrange * (*p).step_size
			widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)
		endif
		end

	'x_size': begin
;		widget_control, event.id, get_value=s
;		(*p).xsize = long2(s)
		end
	'y_size': begin
;		widget_control, event.id, get_value=s
;		(*p).ysize = long2(s)
		end
	'z_size': begin
;		widget_control, event.id, get_value=s
;		(*p).zsize = long2(s)
		end
	'x_compress': begin
		(*p).xcompress = event.index
		end
	'y_compress': begin
		(*p).ycompress = event.index
		end
	'z_compress': begin
		(*p).zcompress = event.index
		end

	'charge': begin
		widget_control, event.id, get_value=s 
		if gnumeric(s) eq 0 then begin
			warning,'evt','illegal character in numeric string.'
			goto, finish
		endif
		evt_set_charge, pstate, float2(s)
		end
	'charge-mode': begin
		evt_set_charge_mode, pstate, event.index, event.top 
		end
	'charge-conversion': begin
;		widget_control_update, event.top, update=0
		widget_control, event.id, get_value=s
		(*p).charge_conversion = float(s)
		aps_count_to_charge = (*p).charge_conversion 
		if (*p).flux ne 0. then begin
			(*p).charge = (*p).flux * (*p).charge_conversion 
			widget_control, (*pstate).charge, set_value=str_tidy((*p).charge)
		endif	
;		widget_control_update, event.top, update=1
		end
	'charge-scan': begin
		dpath = *(*pstate).dpath
		file = find_file2( (*p).evt_file)
		if lenchr(file[0]) eq 0 then begin
			file = strip_path( (*p).evt_file)
			F = file_requester( /read, filter = ((*p).sort_mode eq 1) ? '*' : ('*'+DevObj->extension()), $
						path=dpath, group=event.top, file=file, dir=(((*p).sort_mode eq 1) and (*p).XANES_dir), $
						title='Select the source list-mode data', fix_filter=0, $
						numeric=(DevObj->multi_files() and (DevObj->extension() eq '')) )
			if F ne '' then begin
				set_widget_text, (*pstate).evt_file, F[0]
				(*p).evt_file = F[0]
				*(*pstate).dpath = extract_path(F[0])
				notify, 'dpath', (*pstate).dpath, from=event.top
				(*p).flux = 0.0
	
				if strlowcase(strip_path(strip_file_ext((*p).output_file, double=((*p).sort_mode eq 1)))) ne  $
								strlowcase(strip_path(strip_file_ext((*p).evt_file),/keep)) then begin
	
					*(*pstate).path = build_output_path( (*p).evt_file, (*p).output_file, (*p).root)
	;				notify, 'path', (*pstate).path, from=event.top
	
					active = get_active( p, enable, type, mode, cal_a, cal_b, ecompress, file)
					T = strip_file_ext((*p).evt_file)
					if (mode eq 1) then T = strip_file_m( T, ending='-cuts') + '-cuts'
					if (mode eq 3) then T = strip_file_m( T, ending='-MPDA') + '-MPDA'
					(*p).output_file = *(*pstate).path + strip_path(T,/keep) + '.'+ (*pstate).outputs[(*p).sort_mode]
					set_widget_text, (*pstate).output_file, (*p).output_file
				endif
	
				dir_mode = ((*p).sort_mode eq 1) and (*p).XANES_dir
				mp = get_header_info( DevObj, (*p).evt_file, group=event.top, /silent, dir_mode=dir_mode, error=error)
				if (error eq 0) then evt_check_mp, pstate, mp, /no_cal_adopt
			endif else goto, finish
		endif
		
		evt_flux, pstate, event.top, p				; scan data for PVs and pop-up
		end

	'ic-pv-mode': begin
		evt_set_preamp_pv, pstate, (*(*p).pic_list)[event.index]
		(*p).flux = 0.0
		end
	'ic-preamp-mode': begin
		if ((*p).preamp.pv eq '') and (n_elements(*(*p).pic_list) gt 0) then (*p).preamp.pv=(*(*p).pic_list)[0]
		(*p).preamp.val = (*pstate).ic_vals[event.index]
		(*p).flux = 0.0
		end
	'ic-preamp-unit-mode': begin
		if ((*p).preamp.pv eq '') and (n_elements(*(*p).pic_list) gt 0) then (*p).preamp.pv=(*(*p).pic_list)[0]
		(*p).preamp.unit = (*pstate).ic_vunits[event.index]
		(*p).flux = 0.0
		end
	'dwell': begin
		widget_control, (*pstate).dwell_id, get_value=s
		(*p).dwell = float2(s)
		(*p).flux = 0.0
		end

	'flatten': begin
		(*p).flatten = event.select
		end

	'station': begin
		(*p).station = event.index
;#		widget_control, event.top, update=0
		widget_control, (*pstate).enable, set_value=(*p).enable[(*p).station]
		widget_control, (*pstate).type, set_combobox_select=(*p).type[(*p).station]
		widget_control, (*pstate).cal_a, set_value=str_tidy((*p).cal_a[(*p).station], places=8)
		widget_control, (*pstate).cal_b, set_value=str_tidy((*p).cal_b[(*p).station])
		widget_control, (*pstate).mode, set_combobox_select=(*p).mode[(*p).station]
		set_widget_text, (*pstate).file, (*p).file[(*p).station]
	;	if widget_info(event.top,/update) eq 0 then widget_control, event.top, update=1
		end

	'array': begin
		evt_set_array, pstate, event.index 
		end

	'detector-layout': begin
		detector_layout, group_leader=event.top, TLB=tlb, path=(*pstate).detector_path, data=(*p).layout
		register_notify, event.top, ['detector-select','detector-load','detector-fwhm','evt-load'], from=tlb
		end

	'enable': begin
		(*p).enable[(*p).station] = event.select
		end		
	'type': begin
		evt_set_type, pstate, event.index, event.top
		end
	'cal_a': begin
		widget_control, event.id, get_value=s
		(*p).cal_a[(*p).station] = float(s)
		end
	'cal_b': begin
		widget_control, event.id, get_value=s
		(*p).cal_b[(*p).station] = float(s)
		end
	'da_cal_button': begin
		if (*p).mode[(*p).station] eq 0 then begin				; DA mode
			da = read_da( (*p).file[(*p).station], error=error)
			if error then begin
				file = *(*pstate).path + strip_path( (*p).file[(*p).station])
				da = read_da( file, error=error)
				if error eq 0 then begin
					set_active, pstate, (*p).station, file
				endif
			endif
			if error then begin
				warning, 'EVT', ['Error reading DA matrix file:',(*p).file[(*p).station]]
			endif
			if error eq 0 then begin
				(*p).cal_a[(*p).station] = da.cal_orig.a
				(*p).cal_b[(*p).station] = da.cal_orig.b
				(*p).ecompress[(*p).station] = da.ecompress > 1
				widget_control, (*pstate).cal_a, set_value=str_tidy( (*p).cal_a[(*p).station], places=8)
				widget_control, (*pstate).cal_b, set_value=str_tidy( (*p).cal_b[(*p).station])
				da = 0
			endif
		endif
		end
	'getcal_button': begin
		path = *(*pstate).path
		F = file_requester( /read, filter = '*.spec', path=path, group=event.top, $
					title='Load spectrum calibration from a SPEC file', fix_filter=0, preview_routine='spectrum_preview')
		if F ne '' then begin
			evt_getcal, pstate, F
		endif
		end
	'mode': begin
		modes = ['DA','CUTS','STIM','MPDA','CUBE','ALL']
		evt_set_proj_mode, pstate, modes[event.index], event.top
		if event.index eq 4 then begin
			warning,'evt_event',['Take care using XYE datacubes, which can result in', $
							'extremely large data arrays, even using just 200 energy steps', $
							'on the non-linear compressed E axis.','', $
							'Consider using a sub-region Window within the scan', $
							'(see "Scan" tab) to reduce array dimensions.']
		endif
		end

	'file_button': begin
		file = (*p).file[(*p).station]
		path = *(*pstate).path
		filt = file_ext[(*p).mode[(*p).station],(*p).sort_mode,(*p).type[(*p).station]]
		F = file_requester( /read, filter=filt, path=path, group=event.top, file=file[0], $
					title=file_title[(*p).mode[(*p).station],(*p).sort_mode], fix_filter=0, $
					preview_routine='image_DA_preview')
		if F ne '' then begin
;			*(*pstate).path = extract_path(F)			; often in config elsewhere now
			evt_set_proj_file, pstate, F[0]
		endif
		end
	'file': begin
		widget_control, event.id, get_value=F
		if strlen(F) gt 0 then begin
			evt_set_proj_file, pstate, F[0]
		endif
		end
	'new_button': begin
		if (*p).mode[(*p).station] eq 3 then begin
			initial_file = ['','','']
			if extract_extension( (*p).file[(*p).station]) eq 'mpdam' then begin
				mpdam = read_mpdam( (*p).file[(*p).station], error=err)
				if err eq 0 then begin
					initial_file = [mpdam.phases, mpdam.correct, (*p).file[(*p).station]]
				endif
			endif
			file = ['Phase maps DAI','Correct Yields','New MPDAM file']
			filter = ['*.dai','*.correct','*.mpdam']
			help_file = ['Select the phase maps image DAI file appropriate for this data-set.', $
					'Select the Correct Yields .CORRECT setup file, which selects the DA matrices for all phases.', $
					'Select the path and file-name of the .MPDAM file to create.']
			r = options_popup( event.top, title='Create a MPDAM Setup File',file=file, filter=filter, initial_file=initial_file, $
				help_file=help_file, min_xsize=450, path=*(*pstate).path, debug=0, error=error)
			if error eq 0 then begin
				ofile = strip_file_ext(r.file[2]) + '.mpdam'
				write_mpdam, ofile, r.file[0], r.file[1], error=err
				if err then goto, finish
				evt_set_proj_file, pstate, ofile
			endif
		endif
		end

	'el_select_button': begin
		end
	'xanes-el-select': begin
		widget_control, event.id, get_value=F
		if strlen(F) gt 0 then begin
			(*p).el_select = F[0]
			set_widget_text, (*pstate).el_select_text, (*p).el_select
		endif
		end
	'collapse': begin
		(*p).collapse_energy = event.select
		end		

	'proxy-axis': begin
		evt_set_proxy_axis, pstate, event.index, event.top
		end
	
	'energy_file_button': begin
		file = (*p).xanes_energies_file
		path = *(*pstate).path
		F = file_requester( /read, filter = '*.csv', path=path, group=event.top, file=file[0], $
					title='Select energies file', fix_filter=0)
		if F ne '' then begin
			*(*pstate).path = extract_path(F)
			(*p).xanes_energies_file = F[0]
			set_widget_text, (*pstate).energy_file_text, (*p).xanes_energies_file
		endif
		end
	'energies-file': begin
		widget_control, event.id, get_value=F
		if strlen(F) gt 0 then begin
			(*p).xanes_energies_file = F[0]
			set_widget_text, (*pstate).energy_file_text, (*p).xanes_energies_file
		endif
		end
	else:
endcase

finish:
	active = get_active( p, enable, type, mode, cal_a, cal_b, ecompress, file)
	widget_control, (*pstate).enable, set_value=(*p).enable[(*p).station]

	if (strlen((*p).output_file) gt 0) and (strlen((*p).evt_file) gt 0) then begin
		if strlowcase(strip_path(strip_file_ext(strip_file_m((*p).output_file, ending='-cuts'), double=((*p).sort_mode eq 1)))) ne  $
							strlowcase(strip_path(strip_file_ext((*p).evt_file),/keep)) or  $
							((mode eq 1) and (locate('-cuts',(*p).output_file) eq -1)) or  $
							((mode eq 0) and (locate('-cuts',(*p).output_file) ne -1)) then begin
			T = strip_file_ext(strip_file_m((*p).output_file, ending='-cuts'), double=((*p).sort_mode eq 1))
			if ((mode eq 1) and (locate('-cuts',T) eq -1)) then T = T + '-cuts'
			if (mode eq 3) then T = strip_file_m( T, ending='-MPDA') + '-MPDA'
			pt = extract_path((*p).output_file)
			if lenchr(pt) lt 1 then pt = *(*pstate).path
			(*p).output_file = pt + strip_path(T) + '.'+ (*pstate).outputs[(*p).sort_mode]
			set_widget_text, (*pstate).output_file, (*p).output_file
		endif
	endif

	close_file, lun
	widget_control, hourglass=0
	;if widget_info(event.top,/update) eq 0 then widget_control, event.top, update=1
	return

bad_state:
	warning,'evt',['STATE variable has become ill-defined.','Abort Sort EVT.'],/error
	goto, kill
bad_ptr:
	warning,'evt',['Parameter structure variable has become ill-defined.','Abort Sort EVT.'],/error
	goto, kill
bad_mpdam:
	warning,'evt','Error writing the MPDAM file,',/error
	goto, finish

kill:
	cancel_notify, event.top
	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid((*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid((*pstate).dpath) then ptr_free, (*pstate).dpath
	if ptr_valid((*pstate).detector_path) then ptr_free, (*pstate).detector_path
	if ptr_valid((*pstate).pdev) then ptr_free, (*pstate).pdev
	if ptr_valid((*pstate).pdet) then ptr_free, (*pstate).pdet
	if ptr_valid((*pstate).pval) then ptr_free, (*pstate).pval
	if ptr_valid((*pstate).pval2) then ptr_free, (*pstate).pval2
	if ptr_valid((*pstate).pwiz) then ptr_free, (*pstate).pwiz
	if ptr_valid((*pstate).pspec) then begin
		if (*(*(*pstate).pspec)[0]).orphan eq 1 then begin
			(*pstate).local_spectra = 1
			(*(*(*pstate).pspec)[0]).orphan = 0
		endif
		if ((*pstate).local_spectra eq 1) then free_spectra, (*pstate).pspec
	endif
	if ptr_valid((*pstate).pimage) then begin
		if (*(*pstate).pimage).orphan eq 1 then begin
			(*pstate).local_images = 1
			(*(*pstate).pimage).orphan = 0
		endif
		if ((*pstate).local_images eq 1) then free_images, (*pstate).pimage
	endif
	
die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
return
end

;------------------------------------------------------------------------------------------

pro evt_check_mp, pstate, mp, error=error, no_cal_adopt=no_cal_adopt
	
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
			warning,'evt_check_mp',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif
	
	if n_elements(mp) eq 0 then goto, bad
	if n_elements(no_cal_adopt) lt 1 then no_cal_adopt=0
	if size(mp,/tname) ne 'STRUCT' then goto, bad
	if n_elements(pstate) eq 0 then goto, bad
	if ptr_valid(pstate) eq 0 then goto, bad
	if size(*pstate,/tname) ne 'STRUCT' then goto, bad
	p = (*pstate).p
	if ptr_valid(p) eq 0 then goto, bad
	if size(*p,/tname) ne 'STRUCT' then goto, bad
	error = 1
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object
	
	if mp.scan.sort_mode eq 0 then begin
		if mp.scan.on then begin
			if (mp.scan.mode eq 0) then begin
				if ((*p).xy_mode ne 0) and (((*p).xy_mode ne 4) and ((*p).xy_mode ne 5)) then (*p).xy_mode=0
			endif else begin
				(*p).xy_mode = mp.scan.mode
			endelse
			if mp.scan.n_steps gt 0 then (*p).step_size_index = clip(mp.scan.n_steps-1,0,29)
			if mp.scan.step_size gt 0.0 then (*p).step_size = mp.scan.step_size
			(*p).xorigin = mp.scan.x
			(*p).yorigin = mp.scan.y
			(*p).zorigin = mp.scan.z
			(*p).xoffset = 0L
			(*p).yoffset = 0L
			
			case (*p).xy_mode of
				0: begin									; XY scan mode
					if mp.scan.y_pixels gt 0 then (*p).yrange = mp.scan.y_pixels
					if mp.scan.x_pixels gt 0 then (*p).xrange = mp.scan.x_pixels
					if mp.scan.z_pixels gt 0 then (*p).zrange = mp.scan.z_pixels
					if mp.scan.y_mm gt 0.0 then (*p).ysize = mp.scan.y_mm * 1000.
					if mp.scan.x_mm gt 0.0 then (*p).xsize = mp.scan.x_mm * 1000.
					if mp.scan.z_mm gt 0.0 then (*p).zsize = mp.scan.z_mm * 1000.
					widget_control, (*pstate).stepmode_base, map=0, scr_ysize=1
					widget_control, (*pstate).step_mode, set_combobox_select=0
					widget_control, (*pstate).size_base, map=0
					end
				1: begin									; X step mode
					if mp.scan.y_pixels gt 0 then (*p).yrange = mp.scan.y_pixels
					(*p).ysize = mp.scan.y_mm * 1000.
					widget_control, (*pstate).step_mode, set_combobox_select=(*p).step_mode
					on_off = 0
					if (*p).step_mode eq 0 then on_off=1
					widget_control, (*pstate).stepbit_base, map=on_off
					widget_control, (*pstate).stepcount_base, map=1-on_off
;					geo = widget_info( (*pstate).stepmode_base, /geometry)
					widget_control, (*pstate).stepmode_base, map=1, scr_ysize=(*pstate).stepmode_ysize
					widget_control, (*pstate).size_base, map=1
					end
				3: begin									; Y step mode
					if mp.scan.x_pixels gt 0 then (*p).xrange = mp.scan.x_pixels
					(*p).xsize = mp.scan.x_mm * 1000.
					widget_control, (*pstate).step_mode, set_combobox_select=(*p).step_mode
					on_off = 0
					if (*p).step_mode eq 0 then on_off=1
					widget_control, (*pstate).stepbit_base, map=on_off
					widget_control, (*pstate).stepcount_base, map=1-on_off
;					geo = widget_info( (*pstate).stepmode_base, /geometry)
					widget_control, (*pstate).stepmode_base, map=1, scr_ysize=(*pstate).stepmode_ysize
					widget_control, (*pstate).size_base, map=1
					end
				4: begin									; XYE scan mode
					if mp.scan.y_pixels gt 0 then (*p).yrange = mp.scan.y_pixels
					if mp.scan.x_pixels gt 0 then (*p).xrange = mp.scan.x_pixels
					if mp.scan.z_pixels gt 0 then (*p).zrange = mp.scan.z_pixels
					if mp.scan.y_mm gt 0.0 then (*p).ysize = mp.scan.y_mm * 1000.
					if mp.scan.x_mm gt 0.0 then (*p).xsize = mp.scan.x_mm * 1000.
					if mp.scan.z_mm gt 0.0 then (*p).zsize = mp.scan.z_mm * 1000.
					widget_control, (*pstate).stepmode_base, map=0, scr_ysize=1
					widget_control, (*pstate).step_mode, set_combobox_select=0
					widget_control, (*pstate).size_base, map=0
					end
				5: begin									; XY-theta scan mode
					if mp.scan.y_pixels gt 0 then (*p).yrange = mp.scan.y_pixels
					if mp.scan.x_pixels gt 0 then (*p).xrange = mp.scan.x_pixels
					if mp.scan.z_pixels gt 0 then (*p).zrange = mp.scan.z_pixels
					if mp.scan.y_mm gt 0.0 then (*p).ysize = mp.scan.y_mm * 1000.
					if mp.scan.x_mm gt 0.0 then (*p).xsize = mp.scan.x_mm * 1000.
					if mp.scan.z_mm gt 0.0 then (*p).zsize = mp.scan.z_mm * 1000.
					widget_control, (*pstate).stepmode_base, map=0, scr_ysize=1
					widget_control, (*pstate).step_mode, set_combobox_select=0
					widget_control, (*pstate).size_base, map=0
					end
				else:
			endcase

			widget_control, (*pstate).xy_mode, set_combobox_select=(*p).xy_mode
			widget_control, (*pstate).step_size_drop, set_combobox_select = (*p).step_size_index
			widget_control, (*pstate).step_size, set_value=str_tidy((*p).step_size)
			widget_control, (*pstate).xrange, set_value=str_tidy((*p).xrange)
			widget_control, (*pstate).yrange, set_value=str_tidy((*p).yrange)
			widget_control, (*pstate).zrange, set_value=str_tidy((*p).zrange)
			widget_control, (*pstate).zorigin, set_value=str_tidy((*p).zorigin)
			widget_control, (*pstate).xsize, set_value=str_tidy((*p).xsize)
			widget_control, (*pstate).ysize, set_value=str_tidy((*p).ysize)
			widget_control, (*pstate).zsize, set_value=str_tidy((*p).zsize)
		endif
		
;		if n_elements(mp.cal) gt 1 then evt_set_array, pstate,  1
		first = 1
		cal_adopt = 0
		for i=0L,min([(*pstate).max_adcs,n_elements(mp.cal)])-1 do begin
			if mp.cal[i].on then begin
				if first and (no_cal_adopt eq 0) then begin
					answer = dialog_message( /question, title='Detector calibration parameters in event file', /center, $
									['Do you want to set the detector calibration parameters', $
									'based on the values loaded along with the list-mode header?'])
					first = 0
					if answer eq 'Yes' then cal_adopt = 1
				endif
				if cal_adopt then begin
					cal_ab, {poly: [mp.cal[i].b,mp.cal[i].a], units:mp.cal[i].units, order:1}, ca,cb
					(*p).cal_a[i] = ca
					(*p).cal_b[i] = cb
					if (*p).array then (*p).enable[i] = 1
					if mp.detector[i] ge 0 then (*p).type[i] = mp.detector[i]
				endif
			endif
		endfor
		widget_control, (*pstate).cal_a, set_value=str_tidy((*p).cal_a[(*p).station], places=8)
		widget_control, (*pstate).cal_b, set_value=str_tidy((*p).cal_b[(*p).station])
		widget_control, (*pstate).enable, set_value=(*p).enable[(*p).station]
		widget_control, (*pstate).type, set_combobox_select=(*p).type[(*p).station]
	endif
	if mp.charge gt 1.0e-6 then begin
		evt_set_charge, pstate, mp.charge
	endif
	if mp.sensitivity gt 1.0e-30 then begin
		val = charge_gain_units( mp.sensitivity, units=unit)
		evt_set_preamp_sensitivity, pstate, val
		evt_set_preamp_units, pstate, unit
		if mp.IC_name ne '' then begin
			evt_set_preamp_pv, pstate, mp.IC_name
			evt_set_charge_mode, pstate, 1, (*pstate).tlb
		endif
	endif
	if mp.scan.dwell gt 1.0e-6 then begin
		(*p).dwell = mp.scan.dwell 
		widget_control, (*pstate).dwell_id, set_value=str_tidy((*p).dwell)
	endif
	if mp.sample ne '' then begin
		(*p).sample = mp.sample
		widget_control, (*pstate).sample, set_value=(*p).sample
	endif
	if mp.grain ne '' then begin
		(*p).grain = mp.grain
		widget_control, (*pstate).grain, set_value=(*p).grain
	endif
	if mp.title ne '' then begin
		(*p).comment = mp.title
		widget_control, (*pstate).comment, set_value=(*p).comment
	endif
	(*p).facility = mp.metadata.facility
	(*p).endstation = mp.metadata.endstation
	if ((mp.pileup.file ne '') or (mp.throttle.file ne '')) and (no_cal_adopt eq 0) then begin
		answer = dialog_message( /question, title='Pileup and/or Throttle file-names in data file header', /center, $
					['Do you want to use these to set the "Pileup" and "Throttle" fields?'])
		if answer eq 'Yes' then begin
			evt_set_pileup_file, pstate, mp.pileup.file, /preserve_path
			evt_set_throttle_file, pstate, mp.throttle.file, /preserve_path
		endif
	endif

;	Deadtime cal is one Device parameter set in both header and device sort_options ...
	DevObj->update_device_from_header
	error = 0
	return
	
bad:
	error = 1
	return
end

;------------------------------------------------------------------------------------------

pro evt_check_pvlist, plist, obj

COMPILE_OPT STRICTARR
if ptr_valid( plist) eq 0 then return
if n_elements( *plist) eq 0 then *plist='none'
	
	obj->check_pv_list, plist
	return
end

;------------------------------------------------------------------------------------------

pro evt_flux, pstate, tlb, p, quiet=quiet, error=error

	if n_elements(quiet) lt 1 then quiet=0
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object
	
	image_mode = 1
	flux_scan, DevObj,(*p).evt_file, PV_list, IC_name, IC_val, IC_vunit, dwell=dwell, $
				image_mode=image_mode, error=error, group=tlb, no_pv=no_pv, use_dwell=use_dwell, $
				suppress=quiet, dir_mode=(((*p).sort_mode eq 1) and (*p).XANES_dir)
				
	(*p).use_dwell = use_dwell
	if use_dwell then (*p).dwell = dwell
	widget_control, (*pstate).dwell_base, sensitive=use_dwell
	widget_control, (*pstate).dwell_id, set_value=str_tidy(dwell)
	if quiet then return
	if no_pv and (error eq 0) then begin
		(*p).charge_mode = 2
		evt_set_charge_mode, pstate, (*p).charge_mode, tlb
	endif
	
	if error eq 0 then begin
		if n_elements(PV_list) ne 0 then begin
			*(*p).pic_list = PV_list
			evt_check_pvlist, (*p).pic_list, DevObj
		endif
		evt_set_preamp_pv, pstate, IC_name
		evt_set_preamp_sensitivity, pstate, IC_val
		evt_set_preamp_units, pstate, IC_vunit
	endif else begin
		if quiet eq 0 then warning,'evt_flux','No Flux sensitivity data found for this data-type.'
	endelse
	return
end

;------------------------------------------------------------------------------------------

function get_active, p, enable, type, mode, cal_a, cal_b, ecompress, file, alert=alert

	if n_elements(alert) lt 1 then alert=0
	if ptr_valid(p) eq 0 then return, -1
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	if (*p).array eq 1 then begin
		q = where( (*p).enable eq 1)
		if q[0] ne -1 then begin
			active = q
		endif else begin
			active = 0
			(*p).enable[0] = 1
			if alert then warning,'get_active','There are no ADC channels "enabled".'
		endelse
	endif else begin
		active = (*p).station
	endelse
	active = active < (n_elements((*p).type)-1)
	
	type = (*p).type[active[0]]
	mode = (*p).mode[active[0]]
	file = (*p).file[active[0]]

	ecompress = (*p).ecompress[active[0]]			; ?

	cal_a = (*p).cal_a
	cal_b = (*p).cal_b

	return, active
	end

;------------------------------------------------------------------------------------------

pro evt_getcal, pstate, F, tlb

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
		warning,'evt_getcal',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(F) lt 1 then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

;	*(*pstate).path = extract_path(F)			; no: cal in config elsewhere sometimes.
	if (*p).array eq 1 then begin
		i1 = 0
		i2 = (*pstate).max_adcs-1
		for i=i1,i2 do begin
			(*p).enable[i] = 0
		endfor
	endif else begin
		i1 = (*p).station
		i2 = (*p).station
	endelse
	pp = read_spec(F)
	npp = n_elements(pp)
	n = intarr(npp)
	for j=0L,npp-1 do begin
		if ptr_good(pp[j]) then n[j] = (*pp[j]).station + adc_offset_device((*pp[j]).DevObj)
	endfor
	print,'evt_getcal: n=',n

;	Note that "(*pp[0]).DevObj->start_adc()" is equivalent
;	to "1 + adc_offset_device((*pp[0]).DevObj)".

	use_station = 1
	if size(pp[0],/tname) eq 'POINTER' then begin
		for i=0L,(*pstate).max_adcs-1 do begin
			j = i < (npp-1)
			if use_station then begin
				q = where( i + (*pp[0]).DevObj->start_adc() eq n)
				j = q[0]
			endif
			if j ge 0 then begin
				if ptr_valid(pp[j]) then begin
					cal_ab, (*pp[j]).cal, ca,cb,cu
					(*p).cal_a[i] = ca
					(*p).cal_b[i] = cb
					(*p).ecompress[i] = (*pp[j]).ecompress > 1
					(*p).FWHM[i] = (*pp[j]).FWHM
					if (*p).array then (*p).enable[i] = 1
				endif
			endif
		endfor
		for j=0L,npp-1 do begin
			free_spectrum, pp[j]
		endfor
	endif
	widget_control, (*pstate).cal_a, set_value=str_tidy((*p).cal_a[(*p).station], places=8)
	widget_control, (*pstate).cal_b, set_value=str_tidy((*p).cal_b[(*p).station])
	widget_control, (*pstate).enable, set_value=(*p).enable[(*p).station]

	*(*pstate).pdet = {enable:(*p).enable, FWHM:(*p).FWHM}
	notify, 'detector-get', (*pstate).pdet, from=(*pstate).tlb
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_array, pstate, array

;	Set the device

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
		warning,'evt_set_array',['IDL run-time error caught.', '', $
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
	if n_elements(array) eq 0 then return

	(*p).array = array
	widget_control, (*pstate).enable, sensitive=((*p).array ne 0)
	widget_control, (*pstate).detector_layout_base, map=(*p).array
	widget_control, (*pstate).array, set_combobox_select=(*p).array
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_charge_conversion, pstate, conv

;	Set the beam flux charge conversion

COMPILE_OPT STRICTARR
ErrorNo = 0
common aps_4, aps_count_to_charge
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
		warning,'evt_set_charge_conversion',['IDL run-time error caught.', '', $
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
	if n_elements(conv) eq 0 then return

	widget_control, (*pstate).charge_conversion, set_value=str_tidy(conv)
	(*p).charge_conversion = float2(conv)
	aps_count_to_charge = (*p).charge_conversion 
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_charge, pstate, charge

;	Set the beam charge

COMPILE_OPT STRICTARR
ErrorNo = 0
common aps_4, aps_count_to_charge
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
		warning,'evt_set_charge',['IDL run-time error caught.', '', $
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
	if n_elements(charge) eq 0 then return

	(*p).charge = charge
	if ptr_valid((*pstate).pimage) then (*(*pstate).pimage).charge = (*p).charge
	set_widget_text, (*pstate).charge, str_tidy((*p).charge)

	if (*p).flux ne 0. then begin
		(*p).charge_conversion  = (*p).charge / (*p).flux 
		aps_count_to_charge = (*p).charge_conversion 
		widget_control, (*pstate).charge_conversion, set_value=str_tidy((*p).charge_conversion)
	endif	
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_charge_mode, pstate, mode, tlb

;	Set the flux mode

COMPILE_OPT STRICTARR
ErrorNo = 0
common aps_4, aps_count_to_charge
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
		warning,'evt_set_charge_mode',['IDL run-time error caught.', '', $
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
	if n_elements(mode) eq 0 then return

	(*p).charge_mode = mode
	widget_control, (*pstate).charge_mode, set_combobox_select=(*p).charge_mode
;#	widget_control, tlb, update=0
	case (*p).charge_mode of
		0: begin
			widget_control, (*pstate).ic_base, map=0
			widget_control, (*pstate).scan_button, sensitive=0
			widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
			widget_control, (*pstate).ic_base2, scr_ysize=1
			end
		1: begin
			if ((*p).preamp.pv eq '') and (n_elements(*(*p).pic_list) gt 0) then (*p).preamp.pv=(*(*p).pic_list)[0]
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
	;if widget_info(tlb,/update) eq 0 then widget_control, tlb, update=1
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_device, pstate, device, tlb

;	Set the device

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
		warning,'evt_set_device',['IDL run-time error caught.', '', $
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

	if n_elements(device) eq 0 then return
	newDevObj = obj_new(device)
	if obj_valid(newDevObj) eq 0 then begin
		warning,'evt_set_device','Illegal device = ' + string(device)
		return
	endif
	
	j = find_device( newDevObj->name(), objects=*(*p).pDevObjList)
	if j lt 0 then goto, finish

	(*p).device = j
	widget_control, (*pstate).device_mode, set_combobox_select=(*p).device
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object
	*(*pstate).pdev = DevObj
	notify, 'device', (*pstate).pdev, from=tlb
;#	widget_control, tlb, update=0

	evt_set_array, pstate, DevObj->array_default()

	evt_check_pvlist, (*p).pic_list, DevObj
	widget_control, (*pstate).ic_pv_mode, set_value='   '+*(*p).pic_list

	enable_cluster = DevObj->cluster()
	if enable_cluster eq 0 then (*p).cluster = 0
	widget_control, (*pstate).cluster_id, sensitive=enable_cluster
	if DevObj->multi_files() then begin
		widget_control, (*pstate).evt2_base, map=1, scr_ysize=(*pstate).evt_base_ysize
		widget_control, (*pstate).evt2_base2, scr_ysize=(*pstate).evt_base_ysize
		widget_control, (*pstate).evt_button, set_value='First'+((((*p).sort_mode eq 1) and (*p).XANES_dir)?' Dir':' File:')
		widget_control, (*pstate).evt2_button, set_value='Last'+((((*p).sort_mode eq 1) and (*p).XANES_dir)?' Dir':' File:')
	endif else begin
		b = byte(DevObj->extension())
		if n_elements(b) gt 1 then begin
			t = string(b[1:*])
		endif else begin
			t = 'evt'
		endelse
		t = t + ((((*p).sort_mode eq 1) and (*p).XANES_dir)?' Dir':' File:')
		widget_control, (*pstate).evt2_base, map=0, scr_ysize=1
		widget_control, (*pstate).evt2_base2, scr_ysize=1
		widget_control, (*pstate).evt_button, set_value=t
	endelse
	if DevObj->linear() then begin
		widget_control, (*pstate).linearize_base, map=1, scr_ysize=(*pstate).evt_base_ysize
		widget_control, (*pstate).linearize_base2, scr_ysize=(*pstate).evt_base_ysize
	endif else begin
		widget_control, (*pstate).linearize_base, map=0, scr_ysize=1
		widget_control, (*pstate).linearize_base2, scr_ysize=1
	endelse
	if DevObj->throttle() then begin
		widget_control, (*pstate).throttle_base, map=1, scr_ysize=(*pstate).evt_base_ysize
		widget_control, (*pstate).throttle_base2, scr_ysize=(*pstate).evt_base_ysize
	endif else begin
		widget_control, (*pstate).throttle_base, map=0, scr_ysize=1
		widget_control, (*pstate).throttle_base2, scr_ysize=1
	endelse
	if DevObj->pileup() then begin
		widget_control, (*pstate).pileup_base, map=1, scr_ysize=(*pstate).evt_base_ysize
		widget_control, (*pstate).pileup_base2, scr_ysize=(*pstate).evt_base_ysize
	endif else begin
		widget_control, (*pstate).pileup_base, map=0, scr_ysize=1
		widget_control, (*pstate).pileup_base2, scr_ysize=1
	endelse
	DevObj->render_options, (*pstate).device_option_mode_base
	widget_control, (*pstate).device_option_mode_base, map=DevObj->show_sort_options(), scr_ysize=DevObj->get_sort_ysize()

	list = adc_list_device( DevObj, max_adcs = (*pstate).max_adcs)
	widget_control, (*pstate).station, set_value=list, set_combobox_select=(*p).station

	if ((*p).xy_mode ne 0) then begin
		(*p).xy_mode = 0
		widget_control, (*pstate).xy_mode, set_combobox_select=0
		widget_control, (*pstate).stepmode_base, map=0, scr_ysize=1
		widget_control, (*pstate).size_base, map=(1 - ((*p).xy_mode eq 0))
	endif
	
	ionbeam = DevObj->ionbeam()
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

;	widget_control, (*pstate).ic_val_mode, set_value='   '+str_tidy(ic_vals)
	widget_control, (*pstate).ic_unit_mode, set_value='   '+ic_units
	widget_control, (*pstate).charge_mode, set_value=qmodes, set_uvalue=qhelp
	widget_control, (*pstate).charge_mode, set_combobox_select=(*p).charge_mode

	l = locate('time', strlowcase((*p).preamp.pv))
	val = (*p).preamp.val
	unit = (*p).preamp.unit
	ival = find_charge_val_unit_index( val, unit, iunit=iunit, /write_back, time=(l ge 0), ionbeam=ionbeam)
	(*p).preamp.val = val
	(*p).preamp.unit = unit
	widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
	widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit

	OK = (locate('.time',(*p).preamp.pv) lt 0)
	widget_control, (*pstate).ic_val_mode, sensitive=OK
	widget_control, (*pstate).ic_unit_mode, sensitive=OK

	;if widget_info(tlb,/update) eq 0 then widget_control, tlb, update=1

finish:
	obj_destroy, newDevObj
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_evt_file, pstate, F, tlb, no_cal_adopt=no_cal_adopt, no_output=no_output

;	Set the input EVT file-name and make associated changes

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
		warning,'evt_set_evt_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(F) lt 1 then return
	if n_elements(no_cal_adopt) lt 1 then no_cal_adopt=0
	if n_elements(no_output) lt 1 then no_output=0
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	set_widget_text, (*pstate).evt_file, F[0]
	(*p).evt_file = F[0]
	*(*pstate).dpath = extract_path(F[0])
	(*p).flux = 0.0

;	if strlowcase(strip_path(strip_file_ext((*p).output_file, double=((*p).sort_mode eq 1)))) ne  $
;					strlowcase(strip_path(strip_file_ext((*p).evt_file),/keep)) then begin
	if not no_output then begin
;		*(*pstate).path = build_output_path( (*p).evt_file, (*p).output_file, (*p).root, /set)
		*(*pstate).path = build_output_path( (*p).evt_file, (*p).output_file, (*p).root)

		active = get_active( p, enable, type, mode, cal_a, cal_b, ecompress, file)
		T = strip_file_ext((*p).evt_file)
		if (mode eq 1) then T = strip_file_m( T, ending='-cuts') + '-cuts'
		if (mode eq 3) then T = strip_file_m( T, ending='-MPDA') + '-MPDA'
		if DevObj->multi_files() and (DevObj->multi_char() ne '.') then begin
			T = strip_file_m( T, ending=DevObj->multi_char() + ((adc_offset_device(DevObj) eq -1) ? '0' : '1'))
		endif
		if DevObj->embed_detector() then begin
			m = locate_last( DevObj->multi_char(), T)		; "_" before detector number, after strip off sequence 0 above
			if m gt 0 then begin							; assumes now that mutli_char is before det# too.
				T = strmid( T,0,m)
			endif
		endif
		(*p).output_file = *(*pstate).path + strip_path(T,/keep) + '.'+ (*pstate).outputs[(*p).sort_mode]
		set_widget_text, (*pstate).output_file, (*p).output_file
	endif

	widget_control, /hourglass
	dir_mode = ((*p).sort_mode eq 1) and (*p).XANES_dir
	mp = get_header_info( DevObj, (*p).evt_file, /silent, dir_mode=dir_mode, error=error)
	if (error eq 0) then evt_check_mp, pstate, mp, no_cal_adopt=no_cal_adopt
	
	evt_flux, pstate, tlb, p, /quiet			; get dwell only
	widget_control, hourglass=0
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_evt2_file, pstate, F

;	Set the input EVT2 file-name 

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
		warning,'evt_set_evt2_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(F) lt 1 then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	set_widget_text, (*pstate).evt2_file, F[0]
	(*p).evt2_file = F[0]
	(*p).flux = 0.0
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_linear_file, pstate, F

;	Set the input throttle file-name 

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
		warning,'evt_set_linear_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(F) lt 1 then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	set_widget_text, (*pstate).linearize_file, F
	(*p).linearize_file = F[0]
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_pileup_file, pstate, F, preserve_path=preserve_path

;	Set the input pileup file-name 
;	/preserve_path  do not change path if file-name remains the same

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
		warning,'evt_set_pileup_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(F) lt 1 then return
	if n_elements(preserve_path) lt 1 then preserve_path=0
	
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	if ((*p).sort_mode eq 1) and ((*p).XANES_dir eq 0) then begin
		(*p).xanes_energies_file = F[0]
		set_widget_text, (*pstate).pileup_file, (*p).xanes_energies_file
	endif else begin
		if (strip_path(F[0]) ne strip_path((*p).pileup_file)) or (preserve_path eq 0) then begin
			(*p).pileup_file = F[0]
		endif
		set_widget_text, (*pstate).pileup_file, (*p).pileup_file
	endelse
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_preamp_pv, pstate, pv

;	Set the preamp/IC PV

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
		warning,'evt_set_preamp_pv',['IDL run-time error caught.', '', $
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
	if n_elements(pv) lt 1 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	evt_check_pvlist, (*p).pic_list, DevObj
	widget_control, (*pstate).ic_pv_mode, set_value='   '+*(*p).pic_list

	(*p).preamp.pv = pv
	q = where( (*p).preamp.pv eq *(*p).pic_list, nq)
	i = (nq ne 0) ? q[0] : 0
	(*p).preamp.pv = (*(*p).pic_list)[i]
	widget_control, (*pstate).ic_pv_mode, set_combobox_select=i

	OK = (locate('.time',(*p).preamp.pv) lt 0)
	widget_control, (*pstate).ic_val_mode, sensitive=OK
	widget_control, (*pstate).ic_unit_mode, sensitive=OK
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_preamp_sensitivity, pstate, val

;	Set the preamp gain value/multiplier

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
		warning,'evt_set_preamp_sensitivity',['IDL run-time error caught.', '', $
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
	if n_elements(val) lt 1 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	l = locate('time', strlowcase((*p).preamp.pv))
	(*p).preamp.val = val
	val2 = (*p).preamp.val
	unit2 = (*p).preamp.unit
	ival = find_charge_val_unit_index( val2, unit2, iunit=iunit, /write_back, time=(l ge 0))
	(*p).preamp.val = val2
	(*p).preamp.unit = unit2
	widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
	widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_preamp_units, pstate, unit

;	Set the preamp gain units

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
		warning,'evt_set_preamp_units',['IDL run-time error caught.', '', $
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
	if n_elements(unit) lt 1 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	l = locate('time', strlowcase((*p).preamp.pv))
	(*p).preamp.unit = unit
	val2 = (*p).preamp.val
	unit2 = (*p).preamp.unit
	ival = find_charge_val_unit_index( val2, unit2, iunit=iunit, time=(l ge 0), /write_back)
	(*p).preamp.val = val2
	(*p).preamp.unit = unit2
	widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
	widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_output_file, pstate, F, tlb

;	Set the output DAI file-name and make associated changes

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
		warning,'evt_set_output_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(F) lt 1 then return
	if F[0] eq '' then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	active = get_active( p, enable, type, mode, cal_a, cal_b, ecompress, file)

	T = strip_file_ext(F, double=((*p).sort_mode eq 1))
	if (mode eq 1) then T = strip_file_m( T, ending='-cuts') + '-cuts'
	if (mode eq 3) then T = strip_file_m( T, ending='-MPDA') + '-MPDA'
	F2 = T + '.'+ (*pstate).outputs[(*p).sort_mode]
	set_widget_text, (*pstate).output_file, F2

	(*p).output_file = F2
	*(*pstate).path = build_output_path( (*p).evt_file, (*p).output_file, (*p).root, /set)

	*(*pstate).dpath = extract_path( (*p).evt_file[0])
	notify, 'dpath', (*pstate).dpath, from=tlb
	notify, 'path', (*pstate).path, from=tlb
	notify, 'root', (*p).root, from=tlb
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_proj_mode, pstate, mode, tlb

;	Set the projection mode = (DA, CUTS, STIM, MPDA) 

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
		warning,'evt_set_proj_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(mode) lt 1 then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	q = where( mode eq ['DA','CUTS','STIM','MPDA','CUBE','ALL'])
	if (*p).array eq 1 then begin
		i1 = 0
		i2 = (*pstate).max_adcs-1
	endif else begin
		i1 = (*p).station
		i2 = (*p).station
	endelse
	for i=i1,i2 do begin
		(*p).mode[i] = q[0]
	endfor
;	if (*p).sort_mode eq 1 then begin
;		widget_control, (*pstate).file, sensitive=((*p).mode[(*p).station] ne 0)
;	endif else begin
		widget_control, (*pstate).file, sensitive=(q[0] ne 4)
;	endelse
	widget_control, (*pstate).da_xanes_base1b, map=((*p).mode[(*p).station] ne 3) and ((*p).mode[(*p).station] ne 4) and ((*p).mode[(*p).station] ne 5)
	widget_control, (*pstate).da_xanes_base2, map=(((*p).xy_mode eq 4) or (((*p).xy_mode eq 0) and ((*p).mode[(*p).station] ne 3) and ((*p).energy_proxy_axis gt 0)))
	widget_control, (*pstate).base_new_MPDA, map=((*p).mode[(*p).station] eq 3)
	widget_control, (*pstate).base_export, map=((*p).mode[(*p).station] ne 3)
	widget_control, (*pstate).mode, set_combobox_select=(*p).mode[(*p).station]
	widget_control, (*pstate).base_proj_file, map=((*p).mode[(*p).station] ne 4) and ((*p).mode[(*p).station] ne 5)

	*(*pstate).pval = (*p).mode[(*p).station]
	notify, 'evt-mode', (*pstate).pval, from=tlb
	return
	end
	
;------------------------------------------------------------------------------------------

pro evt_set_proj_file, pstate, F

;	Set the projection (DA, Cuts, ...) file name

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
		warning,'evt_set_proj_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(F) lt 1 then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	if (*p).array eq 1 then begin
		i1 = 0
		i2 = (*pstate).max_adcs-1
	endif else begin
		i1 = (*p).station
		i2 = (*p).station
	endelse
	for i=i1,i2 do begin
		(*p).file[i] = F
	endfor
	set_widget_text, (*pstate).file, F

	return
	end
	
;------------------------------------------------------------------------------------------

pro evt_set_proxy_axis, pstate, mode, tlb

;	Set the sort mode

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
		warning,'evt_set_proxy_axis',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(mode) lt 1 then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	(*p).energy_proxy_axis = mode
	case (*p).xy_mode of
		0: begin
			widget_control, (*pstate).da_xanes_base1a, map=0
			widget_control, (*pstate).da_xanes_base1b, map=1
			widget_control, (*pstate).da_xanes_base2, map=((*p).energy_proxy_axis gt 0)
			end
		4: begin
			widget_control, (*pstate).da_xanes_base1a, map=1
			widget_control, (*pstate).da_xanes_base1b, map=0
			widget_control, (*pstate).da_xanes_base2, map=1
			end
		else: begin
			widget_control, (*pstate).da_xanes_base1a, map=0
			widget_control, (*pstate).da_xanes_base1b, map=0
			widget_control, (*pstate).da_xanes_base2, map=0
			end
	endcase

	widget_control, (*pstate).proxy_axis, set_combobox_select=(*p).energy_proxy_axis
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_sort_mode, pstate, mode, tlb

;	Set the sort mode

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
		warning,'evt_set_sort_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(mode) lt 1 then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	(*p).sort_mode = mode
;#	widget_control, tlb, update=0
	if (*p).sort_mode eq 2 then begin
		evt_set_charge_mode, pstate, 1, tlb
		evt_set_xy_mode, pstate, 4, tlb
		widget_control, (*pstate).from_button, set_value='  From XAN  '
		widget_control, (*pstate).zorigin_base, map=1
		widget_control, (*pstate).zcompress_base, map=0
	endif else begin
		evt_set_xy_mode, pstate, 0, tlb
		widget_control, (*pstate).from_button, set_value='  From DAI  '
	endelse
	case (*p).xy_mode of
		0: begin
			widget_control, (*pstate).da_xanes_base1a, map=0
			widget_control, (*pstate).da_xanes_base1b, map=((*p).mode[(*p).station] ne 4)
			widget_control, (*pstate).da_xanes_base2, map=((*p).energy_proxy_axis gt 0)
			end
		4: begin
			widget_control, (*pstate).da_xanes_base1a, map=1
			widget_control, (*pstate).da_xanes_base1b, map=0
			widget_control, (*pstate).da_xanes_base2, map=1
			end
		else: begin
			widget_control, (*pstate).da_xanes_base1a, map=0
			widget_control, (*pstate).da_xanes_base1b, map=0
			widget_control, (*pstate).da_xanes_base2, map=0
			end
	endcase

	widget_control, (*pstate).mode, set_value=(*pstate).projection_modes[*,(*p).sort_mode], $
							set_combobox_select=(*p).mode[(*p).station]
;	if (*p).sort_mode eq 1 then begin
;		widget_control, (*pstate).file, sensitive = ((*p).mode[(*p).station] ne 0)
;	endif else begin
		widget_control, (*pstate).file, sensitive=1
;	endelse
	if DevObj->multi_files() then begin
		widget_control, (*pstate).evt_button, set_value='First'+((((*p).sort_mode eq 1) and (*p).XANES_dir)? ' Dir' : ' File:')
		widget_control, (*pstate).evt2_button, set_value='Last'+((((*p).sort_mode eq 1) and (*p).XANES_dir)? ' Dir' : ' File:')
	endif else begin
		b = byte(DevObj->extension())
		if n_elements(b) gt 1 then begin
			t = string(b[1:*])
		endif else begin
			t = 'evt'
		endelse
		t = t + ((((*p).sort_mode eq 1) and (*p).XANES_dir)? ' Dir':' File:')
		widget_control, (*pstate).evt_button, set_value=t
	endelse
	enable_cluster = DevObj->cluster()
	if enable_cluster eq 0 then (*p).cluster = 0
	cluster_OK = ((*p).sort_mode eq 0) or (((*p).sort_mode eq 2) and (((*p).xy_mode eq 0) or ((*p).xy_mode eq 4)))
	widget_control, (*pstate).cluster_id, sensitive=enable_cluster and cluster_OK
	
	if (*p).sort_mode eq 1 then begin
		widget_control, (*pstate).scan_window_base, map=0
		widget_control, (*pstate).xybase, map=0, scr_ysize=1
		widget_control, (*pstate).ypars_base, map=0, scr_ysize=1
		widget_control, (*pstate).xpars_base, map=0, scr_ysize=1
		if (*p).XANES_dir eq 0 then begin
			widget_control, (*pstate).pileup_button, set_value='Energies:'
			set_widget_text, (*pstate).pileup_file, (*p).xanes_energies_file
		endif else begin
			widget_control, (*pstate).pileup_button, set_value='Pileup:'
			set_widget_text, (*pstate).pileup_file, (*p).pileup_file
		endelse
		widget_control, (*pstate).from_button, sensitive=0
		widget_control, (*pstate).batch_button, sensitive=0
		widget_control, (*pstate).update_button, sensitive=0
	endif else begin
		widget_control, (*pstate).scan_window_base, map=1
		widget_control, (*pstate).xybase, map=1, scr_ysize=(*pstate).xybase_ysize
		widget_control, (*pstate).ypars_base, map=1, scr_ysize=(*pstate).ypars_ysize
		widget_control, (*pstate).xpars_base, map=1, scr_ysize=(*pstate).xpars_ysize
		widget_control, (*pstate).pileup_button, set_value='Pileup:'
		set_widget_text, (*pstate).pileup_file, (*p).pileup_file
		widget_control, (*pstate).from_button, sensitive=1
		widget_control, (*pstate).batch_button, sensitive=1
		widget_control, (*pstate).update_button, sensitive=1
	endelse
	;if widget_info(tlb,/update) eq 0 then widget_control, tlb, update=1
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_throttle_file, pstate, F, preserve_path=preserve_path

;	Set the input throttle file-name 

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
		warning,'evt_set_throttle_file',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(F) lt 1 then return
	if n_elements(preserve_path) lt 1 then preserve_path=0

	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	if (strip_path(F[0]) ne strip_path((*p).throttle_file)) or (preserve_path eq 0) then begin
		(*p).throttle_file = F[0]
	endif
	set_widget_text, (*pstate).throttle_file, (*p).throttle_file
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_type, pstate, type, tlb

;	Set the data type

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
		warning,'evt_set_type',['IDL run-time error caught.', '', $
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
	if n_elements(type) eq 0 then return

	if (*p).array eq 1 then begin
		i1 = 0
		i2 = (*pstate).max_adcs-1
	endif else begin
		i1 = (*p).station
		i2 = (*p).station
	endelse
	for i=i1,i2 do begin
		(*p).type[i] = type
	endfor
	widget_control, (*pstate).type, set_combobox_select=type
	*(*pstate).pval = (*p).type[(*p).station]
	notify, 'evt-type', (*pstate).pval, from=tlb
	return
end

;------------------------------------------------------------------------------------------

pro evt_set_xy_mode, pstate, mode, tlb

;	Set the XY mode

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
		warning,'evt_set_xy_mode',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	if n_elements(mode) lt 1 then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

		(*p).xy_mode = mode
		widget_control, (*pstate).xy_mode, set_combobox_select=mode
;#		widget_control, tlb, update=0
		case (*p).xy_mode of
			0: begin
				widget_control, (*pstate).ypars_base, map=1, scr_ysize=(*pstate).ypars_ysize
				widget_control, (*pstate).zpars_base, map=0, scr_ysize=1
				widget_control, (*pstate).stepmode_base, map=0, scr_ysize=1
				widget_control, (*pstate).stepdrop_base, map=1
				widget_control, (*pstate).size_base, map=0, scr_ysize=1
				widget_control, (*pstate).da_xanes_base1a, map=0
				widget_control, (*pstate).da_xanes_base1b, map=1
				widget_control, (*pstate).da_xanes_base2, map=1
				end
			1: begin
				widget_control, (*pstate).ypars_base, map=1, scr_ysize=(*pstate).ypars_ysize
				widget_control, (*pstate).zpars_base, map=0, scr_ysize=1
				widget_control, (*pstate).stepmode_base, map=1, scr_ysize=(*pstate).stepmode_ysize
				widget_control, (*pstate).stepdrop_base, map=1
				widget_control, (*pstate).size_base, map=1, scr_ysize=(*pstate).xybase_ysize
				widget_control, (*pstate).da_xanes_base1a, map=0
				widget_control, (*pstate).da_xanes_base1b, map=0
				widget_control, (*pstate).da_xanes_base2, map=0
				end
			2: begin
				widget_control, (*pstate).ypars_base, map=0, scr_ysize=1
				widget_control, (*pstate).zpars_base, map=0, scr_ysize=1
				widget_control, (*pstate).stepmode_base, map=0, scr_ysize=1
				widget_control, (*pstate).stepdrop_base, map=0
				widget_control, (*pstate).size_base, map=1, scr_ysize=(*pstate).xybase_ysize
				widget_control, (*pstate).da_xanes_base1a, map=0
				widget_control, (*pstate).da_xanes_base1b, map=0
				widget_control, (*pstate).da_xanes_base2, map=0
				end
			3: begin
				widget_control, (*pstate).ypars_base, map=1, scr_ysize=(*pstate).ypars_ysize
				widget_control, (*pstate).zpars_base, map=0, scr_ysize=1
				widget_control, (*pstate).stepmode_base, map=1, scr_ysize=(*pstate).stepmode_ysize
				widget_control, (*pstate).stepdrop_base, map=1
				widget_control, (*pstate).size_base, map=1, scr_ysize=(*pstate).xybase_ysize
				widget_control, (*pstate).da_xanes_base1a, map=0
				widget_control, (*pstate).da_xanes_base1b, map=0
				widget_control, (*pstate).da_xanes_base2, map=0
				end
			4: begin
				widget_control, (*pstate).ypars_base, map=1, scr_ysize=(*pstate).ypars_ysize
				widget_control, (*pstate).zpars_base, map=1, scr_ysize=(*pstate).zpars_ysize
				widget_control, (*pstate).stepmode_base, map=0, scr_ysize=1
				widget_control, (*pstate).stepdrop_base, map=0
				widget_control, (*pstate).size_base, map=0, scr_ysize=1
				widget_control, (*pstate).zorigin_base, map=1
				widget_control, (*pstate).zcompress_base, map=0
				widget_control, (*pstate).da_xanes_base1a, map=1
				widget_control, (*pstate).da_xanes_base1b, map=0
				widget_control, (*pstate).da_xanes_base2, map=1
				end
			5: begin
				widget_control, (*pstate).ypars_base, map=1, scr_ysize=(*pstate).ypars_ysize
				widget_control, (*pstate).zpars_base, map=1, scr_ysize=(*pstate).zpars_ysize
				widget_control, (*pstate).stepmode_base, map=0, scr_ysize=1
				widget_control, (*pstate).stepdrop_base, map=0
				widget_control, (*pstate).size_base, map=0, scr_ysize=1
				widget_control, (*pstate).zorigin_base, map=0
				widget_control, (*pstate).zcompress_base, map=1
				widget_control, (*pstate).da_xanes_base1a, map=0
				widget_control, (*pstate).da_xanes_base1b, map=0
				widget_control, (*pstate).da_xanes_base2, map=0
				end
			else:
		endcase
		enable_cluster = DevObj->cluster()
		if enable_cluster eq 0 then (*p).cluster = 0
		cluster_OK = ((*p).sort_mode eq 0) or (((*p).sort_mode eq 2) and (((*p).xy_mode eq 0) or ((*p).xy_mode eq 4)))
		widget_control, (*pstate).cluster_id, sensitive=enable_cluster and cluster_OK
	;	if widget_info(tlb,/update) eq 0 then widget_control, tlb, update=1
	return
end

;------------------------------------------------------------------------------------------

pro set_active, pstate, active, file

if n_elements(pstate) eq 0 then return
if ptr_valid(pstate) eq 0 then return
if size(*pstate,/tname) ne 'STRUCT' then return
if n_elements(active) lt 1 then return
if n_elements(file) lt 1 then return
p = (*pstate).p
if ptr_valid(p) eq 0 then return
DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

if (*p).array eq 1 then begin
	(*p).file[*] = file
	widget_control, (*pstate).file, set_value=file
endif else begin
	(*p).file[active] = file
	if active eq (*p).station then widget_control, (*pstate).file, set_value=file
endelse

return
end

;------------------------------------------------------------------------------------------

pro OnRealize_evt_charge_conversion, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).IC_base2, /geometry)
(*pstate).IC_base2_ysize = geo.ysize
return
end

;------------------------------------------------------------------------------------------

pro OnRealize_evt_charge_pv_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).IC_base1, /geometry)
(*pstate).IC_base1_ysize = geo.ysize
;#widget_control, top, update=0

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	q = where( (*p).preamp.pv eq *(*p).pic_list, nq)
	widget_control, wWidget, set_combobox_select=(nq ne 0) ? q[0] : 0
	widget_control, (*pstate).ic_base, map=((*p).charge_mode ne 0 )
	if (*p).charge_mode eq 1 then begin
		widget_control, (*pstate).ic_base1, map=1, scr_ysize=(*pstate).ic_base1_ysize
	endif else begin
		widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
	endelse
endif
;if widget_info(top,/update) eq 0 then widget_control, top, update=1
end

;------------------------------------------------------------------------------------------

pro OnRealize_evt_charge_preamp_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
;	Done with units now ...
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_evt_charge_unit_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	l = locate('time', strlowcase((*p).preamp.pv))
	val = (*p).preamp.val
	unit = (*p).preamp.unit
	ival = find_charge_val_unit_index( val, unit, iunit=iunit, time=(l ge 0), /write_back)
	(*p).preamp.val = val
	(*p).preamp.unit = unit
	widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
	widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit

	OK = (locate('.time',(*p).preamp.pv) lt 0)
	widget_control, (*pstate).ic_val_mode, sensitive=OK
	widget_control, (*pstate).ic_unit_mode, sensitive=OK
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_evt_charge_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	widget_control, wWidget, set_combobox_select=(*p).charge_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_evt_station, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).station = wWidget

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	list = adc_list_device( DevObj)
	widget_control, (*pstate).station, set_value=list, $
						set_combobox_select=(*(*pstate).p).station
endif
end


;-----------------------------------------------------------------

pro OnRealize_device_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p

if ptr_valid( p) then begin
	widget_control, wWidget, set_combobox_select=(*p).device

	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object
	*(*pstate).pdev = DevObj
	evt_set_array, pstate, DevObj->array_default()
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_evt_energies_file, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	set_widget_text, (*pstate).energy_file_text, (*(*pstate).p).xanes_energies_file
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_evt_file, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	set_widget_text, (*pstate).evt_file, (*(*pstate).p).evt_file
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_evt2_file, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).evt_base, /geometry)
(*pstate).evt_base_ysize = geo.ysize
;#widget_control, top, update=0

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object
	
	if DevObj->multi_files() eq 0 then begin
		widget_control, (*pstate).evt2_base, map=0, scr_ysize=1
		widget_control, (*pstate).evt2_base2, scr_ysize=1
	endif
	set_widget_text, (*pstate).evt2_file, (*(*pstate).p).evt2_file
endif
;if widget_info(top,/update) eq 0 then widget_control, top, update=1
end

;------------------------------------------------------------------------------------------

pro OnRealize_pileup_file, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate
;#widget_control, top, update=0

geo = widget_info( (*pstate).evt_base, /geometry)
(*pstate).evt_base_ysize = geo.ysize

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

	if DevObj->pileup() eq 0 then begin
		widget_control, (*pstate).pileup_base, map=0, scr_ysize=1
		widget_control, (*pstate).pileup_base2, scr_ysize=1
	endif
	set_widget_text, (*pstate).pileup_file, (*(*pstate).p).pileup_file
endif
;if widget_info(top,/update) eq 0 then widget_control, top, update=1
end

;------------------------------------------------------------------------------------------

pro OnRealize_linearize_file, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate
;#widget_control, top, update=0

geo = widget_info( (*pstate).evt_base, /geometry)
(*pstate).evt_base_ysize = geo.ysize

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object
	
	if DevObj->linear() eq 0 then begin
		widget_control, (*pstate).linearize_base, map=0, scr_ysize=1
		widget_control, (*pstate).linearize_base2, scr_ysize=1
	endif
	set_widget_text, (*pstate).linearize_file, (*(*pstate).p).linearize_file
endif
;if widget_info(top,/update) eq 0 then widget_control, top, update=1
end

;------------------------------------------------------------------------------------------

pro OnRealize_throttle_file, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).evt_base, /geometry)
(*pstate).evt_base_ysize = geo.ysize
;#widget_control, top, update=0

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object
	
	if DevObj->throttle() eq 0 then begin
		widget_control, (*pstate).throttle_base, map=0, scr_ysize=1
		widget_control, (*pstate).throttle_base2, scr_ysize=1
	endif
	set_widget_text, (*pstate).throttle_file, (*(*pstate).p).throttle_file
endif
;if widget_info(top,/update) eq 0 then widget_control, top, update=1
end

;------------------------------------------------------------------------------------------

pro OnRealize_EVT_array, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).array
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_output_file, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	set_widget_text, (*pstate).output_file, (*(*pstate).p).output_file
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_proxy_axis, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).energy_proxy_axis
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_xy_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).xy_mode
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_evt_image_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).offset_base, /geometry)
(*pstate).offset_ysize = geo.ysize
;#widget_control, top, update=0

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).image_mode

	if (*(*pstate).p).image_mode eq 0 then begin
		widget_control, (*pstate).offset_base, map=0, scr_ysize=1
	endif
endif
;if widget_info(top,/update) eq 0 then widget_control, top, update=1
end

;------------------------------------------------------------------------------------------

pro OnRealize_step_mode, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).stepmode_base, /geometry)
(*pstate).stepmode_ysize = geo.ysize
;#widget_control, top, update=0

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).step_mode

	if ((*(*pstate).p).xy_mode eq 0) or ((*(*pstate).p).xy_mode eq 4) or ((*(*pstate).p).xy_mode eq 5) then begin
		widget_control, (*pstate).stepmode_base, map=0, scr_ysize=1
		widget_control, (*pstate).size_base, map=0, scr_ysize=1
	endif
endif
;if widget_info(top,/update) eq 0 then widget_control, top, update=1
end

;------------------------------------------------------------------------------------------

pro OnRealize_step_station, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).step_station
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_step_size, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).step_size_index
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_step_bit, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).step_bit
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_xcompress, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).xcompress
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_ycompress, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).xpars_base, /geometry)
(*pstate).xpars_ysize = geo.ysize
geo = widget_info( (*pstate).ypars_base, /geometry)
(*pstate).ypars_ysize = geo.ysize
geo = widget_info( (*pstate).xybase, /geometry)
(*pstate).xybase_ysize = geo.ysize
;#widget_control, top, update=0

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).ycompress

	if (*(*pstate).p).xy_mode eq 2 then begin
		widget_control, (*pstate).ypars_base, map=0, scr_ysize=1
	endif
	if (*(*pstate).p).sort_mode eq 1 then begin
		widget_control, (*pstate).xybase, map=0, scr_ysize=1
		widget_control, (*pstate).xpars_base, map=0, scr_ysize=1
		widget_control, (*pstate).ypars_base, map=0, scr_ysize=1
	endif
endif
;if widget_info(top,/update) eq 0 then widget_control, top, update=1
end

;------------------------------------------------------------------------------------------

pro OnRealize_zcompress, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).xpars_base, /geometry)
(*pstate).xpars_ysize = geo.ysize
geo = widget_info( (*pstate).ypars_base, /geometry)
(*pstate).ypars_ysize = geo.ysize
geo = widget_info( (*pstate).xybase, /geometry)
(*pstate).xybase_ysize = geo.ysize
;#widget_control, top, update=0

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).zcompress
endif
;if widget_info(top,/update) eq 0 then widget_control, top, update=1
end

;------------------------------------------------------------------------------------------

pro OnRealize_zorigin, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).zpars_base, /geometry)
(*pstate).zpars_ysize = geo.ysize
end

;------------------------------------------------------------------------------------------

pro OnRealize_type, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).type[(*(*pstate).p).station]
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).mode[(*(*pstate).p).station]
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_station_file, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	set_widget_text, wWidget, (*(*pstate).p).file[(*(*pstate).p).station]
endif
end

;------------------------------------------------------------------------------------------

pro evt, group_leader=group, TLB=tlb, image=image, $
		xoffset=xoffset, yoffset=yoffset, pars=p, path=path, test=test, _extra=extra, $
		dpath=dpath, pprefs=pprefs

COMPILE_OPT STRICTARR
common c_prefs_scan, prefs_XY_scan, prefs_X_step, prefs_Y_step, prefs_Resolution
common c_geopixe_adcs, geopixe_max_adcs
common c_working_dir, geopixe_root
common aps_4, aps_count_to_charge
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=384
if n_elements(aps_count_to_charge) lt 1 then aps_count_to_charge=1.0

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
		warning,'EVT',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
startupp

; Old - these will become obsolete, should be absorbed into mpsys device ...
if n_elements(prefs_XY_scan) lt 1 then prefs_XY_scan = {X:100.0, Y:100.0}
if n_elements(prefs_X_step) lt 1 then prefs_X_step = {Y:2000.0}
if n_elements(prefs_Y_step) lt 1 then prefs_Y_step = {X:640.0}
if n_elements(prefs_Resolution) lt 1 then prefs_Resolution = {X:0.635, Y:0.08333333}

if n_elements(group) lt 1 then group=0
if n_elements(image) lt 1 then image=0
;if n_elements(spectra) lt 1 then spectra=0
;if n_elements(traverse) lt 1 then traverse=0
if n_elements(path) lt 1 then path=''
if n_elements(dpath) lt 1 then dpath=path
if n_elements(test) lt 1 then test=0
sort_mode = 0
if image then sort_mode=0
;if spectra then sort_mode=1
prefs = geopixe_defaults( error=err, source='evt')

if ptr_good( pprefs,/struct) eq 0 then begin
	if strlen(path) gt 0 then prefs.path.analysis = path
	if strlen(dpath) gt 0 then prefs.path.data = dpath
	prefs.test = test
;	prefs.debug = debug
;	prefs.slave = realtime
	pprefs = ptr_new(prefs)
endif

define_devices, titles=device_titles, names=device_names
register_notify

; Set these in main EVT event routine too ...
;XANES_mode_tag = ' File'		; for single DIR XANES
;XANES_dir = 0
XANES_mode_tag = ' Dir'			; for DIR XANES
XANES_dir = 1

case !version.os_family of
	'MacOS': begin
		symbol = 'SYMBOL*12'
		large_font = 'Arial*12'
;@2		widget_control, default_font='Geneva*10'		; set font for all windows
		xw = 432
		yo = 10
		space1 = 1
		space2 = 2
		space5 = 5
		space10 = 10
		space15 = 15
		from_xsize = 65
		device_xsize = 260
		evt_button_xsize = 55
		evt_file_xsize = 236
		evt_file_xsize2 = 270
		sample_xsize = 113
		comment_xsize = 266
		xy_mode_xsize = 240
		stepmode_base_width = 305
		stepmode_xsize = 290
		steps_xsize = 90
		distance_xsize = 90
		xrange_xsize = 55
		charge_xsize = 80
		step_range_dummy_xsize = 52
		station_xsize = 57
		type_xsize = 100
		cal_xsize = 70
		mode_xsize = 260
		file_xsize = 320
		map_base_xsize = 305
		pad5 = 5
		help_xsize = 422
		tab_xsize = 412
		charge_xsize2 = 195
		encoder_xsize = 175
		com_xsize = 40
		update_xsize = 55
		end
	'unix': begin
		symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
		large_font = '10x20'
;@2		widget_control, default_font='6x13'				; set font for all windows
		xw = 432
		yo = 0
		space1 = 0
		space2 = 2
		space5 = 4
		space10 = 7
		space15 = 12
		from_xsize = 65
		device_xsize = 320
		evt_button_xsize = 75
		evt_file_xsize = 286
		evt_file_xsize2 = 327
		sample_xsize = 143
		comment_xsize = 342
		xy_mode_xsize = 260
		stepmode_base_width = 385
		stepmode_xsize = 220
		steps_xsize = 120
		distance_xsize = 90
		xrange_xsize = 55
		charge_xsize = 80
		step_range_dummy_xsize = 82
		station_xsize = 67
		type_xsize = 100
		cal_xsize = 110
		mode_xsize = 260
		file_xsize = 327
		map_base_xsize = 385
		pad5 = 5
		help_xsize = 422
		tab_xsize = 412
		charge_xsize2 = 195
		encoder_xsize = 175
		com_xsize = 40
		update_xsize = 55
		end
	else: begin
		symbol = 'SYMBOL*BOLD*14'
		large_font = 'COURIER*BOLD*10'
	;	widget_control, default_font='Arial*14'			; set font for all windows
		xw = 357
		yo = 0
		space1 = 1
		space2 = 2
		space5 = 5
		space10 = 10
		space15 = 15
		from_xsize = 65
		device_xsize = 242
		evt_button_xsize = 55
		evt_file_xsize = 236
		evt_file_xsize2 = 264
		sample_xsize = 113
		comment_xsize = 270
		xy_mode_xsize = 245
		stepmode_base_width = 305
		stepmode_xsize = 298
		steps_xsize = 90
		distance_xsize = 90
		xrange_xsize = 55
		charge_xsize = 80
		step_range_dummy_xsize = 13
		station_xsize = 67
		type_xsize = 100
		cal_xsize = 70
		mode_xsize = 240
		file_xsize = 255	; 240
		map_base_xsize = 305
		pad5 = 7
		help_xsize = 343	; 326
		tab_xsize = 336
		charge_xsize2 = 195
		encoder_xsize = 175
		com_xsize = 40
		update_xsize = 55
		end
endcase

if n_elements(xoffset) lt 1 then begin
	screen = get_screen_size()
	xoffset = (screen[0] - xw) > 0
endif
if n_elements(yoffset) lt 1 then yoffset = yo

p = bad_pars_struct( p, make_pars=make_p)

max_adcs = 384 > geopixe_max_adcs

if make_p then begin
	DevObjList = instance_device_objects( device_names, error=err)
;	DevObjList = open_device_objects(error=err, name=device_names, title=device_titles)
	if err then begin
		warning,'EVT',['Failed to open Device Objects.','Missing "xxx_device__define.sav" files in "/interface" ?']
		return
	endif
	if obj_valid(DevObjList[0]) eq 0 then begin
		warning,'EVT',['Failed to open Device Objects.','Obj array invalid.']
		return
	endif
	
;	Note that this struct is kept in the Image (*pstate).pevt, and must be freed there.

	pars = {	$
		device:		0, $							; list-mode device index
		pDevObjList:	ptr_new(DevObjList), $		; pointer to device obj array
		evt_file:	'', $							; EVT file name
		evt2_file:	'', $							; final EVT file name
		throttle_file: '', $						; throttle file name (if used)
		pileup_file: '', $							; pileup file name (if used)
		xanes_energies_file: '', $					; XANES energies file name (if used), 2D / 3D (sort_mode=2) modes
		collapse_energy:	1, $					; collapse the energy planes down onto DA matrix stack list
		energy_proxy_axis: 0, $						; axis controlling energy index (0=none, 1=X, 2=Y) in 2D (xanes=0) mode 
		el_select: '', $							; selected XANES element
		linearize_file: '', $						; linearization file name (if used)
		output_file: '', $							; Output file name
		root:		ptr_new( /allocate_heap), $		; root info for paths
		sample:		'', $							; sample
		grain:		'', $							; grain
		comment:	'', $							; comment
		facility: '', $								; facility string
		endstation: '', $							; endstation name
		sort_mode:	sort_mode, $					; sort_mode
		xanes_dir:	XANES_dir, $					; XANES dir mode
		xy_mode:	0, $							; xy scan mode (XY scan)
		step_mode:	0, $							; step advance mode (toggle bit)
		step_station:	0, $						; step station number index
		step_count:	100000L, $						; step count
		step_bit:	0, $							; step bit number index
		step_size:	prefs_Resolution.X, $			; step size (microns)
		step_size_index: 0, $						; step size droplist index
		xrange:		256L, $							; X range (pixels)
		yrange:		256L, $							; Y range
		zrange:		1L, $							; Z range
		xcompress:	0, $							; X compress index
		ycompress:	0, $							; Y compress index
		zcompress:	0, $							; Z compress index
		image_mode:	0, $							; image mode index (0=full, 1=sub-region)
		xoffset:	0L, $							; X offset (pixels) to region sub-image
		yoffset:	0L, $							; Y offset
		x_sub_range:	0L, $						; X sub-range (pixels) of sub-image
		y_sub_range:	0L, $						; Y sub-range
		xsize:		0.0, $							; X size (microns)
		ysize:		0.0, $							; Y size (microns)
		zsize:		0.0, $							; Z size (microns, mdeg)
		xorigin:	0.0, $							; X scan origin (mm)
		yorigin:	0.0, $							; Y scan origin (mm)
		zorigin:	0.0, $							; Z scan origin (mm, deg)
		charge:		0.0, $							; total charge (uC) (no default)
		charge_mode:	0, $						; charge mode (direct ons, IC with PV, IC no PV)
		charge_conversion:	aps_count_to_charge, $	; conversion from flux to charge
		flux:		0.0, $							; total IC count
		preamp: {	pv:		'', $					; Epics PV name for IC
					val:	1, $					; Preamp multiplier
					unit:	0.0 }, $				; Preamp range scaling factor
		pic_list:	ptr_new(/allocate_heap), $		; List of previous IC PVs
		dwell:		0.0, $							; dwell time (ms)
		use_dwell:	0, $							; use dwell to convert IC rate to IC count
		flatten:	1, $							; flatten image based on flux trends
		station:	0, $							; active station  index
		array:		0, $							; single detector by default
		cluster: 0, $								; cluster processing
		layout:		ptr_new(/allocate_heap), $		; detector layout pop-up window data

		enable:		intarr(max_adcs), $				; enable station
		type:		intarr(max_adcs), $				; type (PIXE)
		cal_a:		replicate(1.0,max_adcs), $		; cal A
		cal_b:		fltarr(max_adcs), $				; cal B
		FWHM:		fltarr(max_adcs), $				; FWHM (eV)
		ecompress:	replicate(1,max_adcs), $		; E compression factor
		mode:		intarr(max_adcs), $				; mode (DA) index
		file:		replicate(' <--- select file ',max_adcs) $	; file name
	}
	pars.enable[0] = 1
	*p = pars

	device_name = (*pprefs).default.device
	if device_name ne '' then begin
		for i=0,n_elements( *(*p).pDevObjList)-1 do begin
			odev = (*(*p).pDevObjList)[i]
			if odev.name() eq device_name then (*p).device = i
		endfor
	endif
endif else begin
	device_names = list_device_objects( title=device_titles)
endelse

(*p).charge_conversion = aps_count_to_charge
path = build_output_path( dpath, path, (*p).root, /set)

DevObj = (*(*p).pDevObjList)[(*p).device]			; current device object

if image and ((*p).sort_mode eq 1) then (*p).sort_mode=0
;if spectra and ((*p).sort_mode ne 1) then (*p).sort_mode=1

sort_modes = ['Image','EXAFS','3D Stack']
xy_modes = [' Scan in X and Y ',' Stage stepping in X ',' Linear stage Traverse in XY ', $
			' Stage stepping in Y ', ' Scan in XY and Energy (Z) ', ' Scan in XY and Tomo angle (Z) ']
image_modes = ['Image full area','Image select sub-region (no XY compress)']
step_modes = ['Advance by Toggle Bit','Advance by ADC Count','Advance by Event Count']
data_types = ' ' + detector_types()
projection_modes = [	['Dynamic Analysis (DA)','Spectrum CUTs (ROIs)','CUT mean energy (STIM)','Multiphase DA (MPDA)','XYE (compressed E) datacube','All Counts'], $	; image
			['Dynamic Analysis (DA)','Spectrum CUTs (ROIs)','CUT mean energy (STIM)',' ',' ',' '], $				; EXAFS
			['Dynamic Analysis (DA)','Spectrum CUTs (ROIs)','CUT mean energy (STIM)',' ',' ',' ']]					; 3D stack
proxy_modes = ['None','X axis','Y axis']
; compressions must be just integers starting at 1
compressions = ' '+str_tidy(indgen(100)+1)
station_numbers = '   ' + strtrim(indgen(max_adcs)+1,2) + '   '
bit_numbers = ['   12   ','   11   ','   10   ','   9   ']
outputs = ['dai','xanes.csv','xan']

ionbeam = DevObj->ionbeam()
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

width = 320

; 	top-level base

tlb = widget_base( /column, title='Sort EVT Files', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, _extra=extra, uname='evt_TLB', /base_align_center, $
					xpad=0, ypad=space2, space=space2, xoffset=xoffset, yoffset=yoffset )
tbase = widget_base( tlb, /column, xpad=1, ypad=0, space=space2, /base_align_center)

; sort type buttons

sbase = widget_base( tbase, /row, /base_align_center, ypad=1, space=space10)
from_button = widget_button( sbase, value='  From DAI  ', uname='template_button', /tracking, $
					uvalue='Load sort parameters for image (or 3D stack) from a previous ' + $
					'DAI (or XAN) file. Use this to repeat a sort, or as a template for a new data-set. ' + $
					'Select "Image" (DAI) or "3D Stack" (XAN) first.', scr_xsize=from_xsize)
toggle = cw_bgroup2( sbase, sort_modes, /row, ypad=0, /exclusive, /no_release, /return_index, /tracking, $
					uname='sort_mode', set_value=(*p).sort_mode, $
					uvalue='Select type of data to extract from event files to build images, EXAFS spectra or a 3D image stack (XANES or Tomo). ' + $
					'Spectra from regions can be extracted using the EVT button on the "Image Regions" window.')

; device selection

tweek = 4

dbase = widget_base( tbase, /row, /base_align_center, xpad=3, ypad=0, space=space5, /align_center)

lab = widget_label( dbase, value='Device:')
device_mode = widget_combobox( dbase, value=device_titles, uname='device_mode', /tracking, $
					notify_realize='OnRealize_device_mode', xsize=device_xsize, $
					uvalue='Select input device driver for the list-mode (event-by-event) file(s), or pixel format files.')

batch_button = widget_button( dbase, value='Batch', uname='batch_button', /tracking, $
					uvalue='Batch operation of Sort EVT: Set-up "Sort EVT" first for the first run in a series. Make sure input and output file paths are correct. ' + $
							'THEN open "Batch Sort" and select runs to process. Start using "Start" on the batch window.')	;, scr_xsize=18)

tab_panel = widget_tab( tbase, location=0, /align_center, uname='tab-panel')
tab_names = ['Files','Device','Scan','Flux','DA / E.Cal']

; ----------- files ------------------------------------------------------------------------------

files_base = widget_base( tab_panel, title=tab_names[0], /column, xpad=1, ypad=1, scr_xsize=tab_xsize, space=space1, /base_align_right, /align_right)
lab = widget_label( files_base, value='Data Files and Corrections', /align_center)

; EVT file

name = 'EVT File:'

boff=21

evt_base = widget_base( files_base, /row, /base_align_center, xpad=2, ypad=0, space=space2, /align_right)

if DevObj->multi_files() then begin
	name = 'First' + ((((*p).sort_mode eq 1) and XANES_dir)?' Dir':' File:')
	evt2_base = widget_base( files_base, /row, /base_align_center, xpad=2, ypad=0, space=space2, /align_right, map=1)
	evt2_base2 = evt2_base
endif else begin
	evt2_base = widget_base( files_base, /row, /base_align_center, xpad=2, ypad=0, space=space2, /align_right, map=0)
	evt2_base2 = evt2_base
endelse

evt_button = widget_button( evt_base, value=name, uname='evt_button', /tracking, $
					uvalue='Browse to select the list-mode file to sort, or the "First" file in a series of list-mode/pixel data files. ' + $
					'If the "output" is to be on a different path, select "Output" below.', scr_xsize=evt_button_xsize)
evt_file = widget_text( evt_base, value=(*p).evt_file, uname='evt_file', /tracking, /editable, $
					notify_realize='OnRealize_evt_file', $
					uvalue='Enter a file-name for the list-mode file to sort, or the "First" file in a series of list-mode/pixel data files; or click on the button to the left. ' + $
					'If the "output" is to be on a different path, select "Output" below.', $
					scr_xsize=evt_file_xsize2)

evt2_button = widget_button( evt2_base2, value='Last'+((((*p).sort_mode eq 1) and XANES_dir)?' Dir':' File:'), uname='evt2_button', /tracking, $
					uvalue='Browse to select the "Last" list-mode/pixel data file to sort. Leave blank to sort all.', scr_xsize=evt_button_xsize)
evt2_file = widget_text( evt2_base2, value=(*p).evt2_file, uname='evt2_file', /tracking, /editable, $
					notify_realize='OnRealize_evt2_file', $
					uvalue='Enter a file-name for the "Last" list-mode/pixel data file to sort; or click on the button to the left. Leave blank to sort all.', $
					scr_xsize=evt_file_xsize2)

pileup_base = widget_base( files_base, /row, /base_align_center, xpad=2, ypad=0, space=space2, /align_right, map=DevObj->pileup())
pileup_base2 = pileup_base

; pileup, or the XANES energy list file ...
pileup_button = widget_button( pileup_base2, value='Pileup:', uname='pileup_button', /tracking, $
					uvalue='Browse to select the "pileup" file for this EVT file, to filter out pileup, if appropriate, which contains upper and lower T limits for each E. Or, a "XANES energies" file in XANES mode, which contains the energy list.', scr_xsize=evt_button_xsize)
pileup_file = widget_text( pileup_base2, value=(*p).pileup_file, uname='pileup_file', /tracking, /editable, $
					notify_realize='OnRealize_pileup_file', $
					uvalue='Enter a file-name for the "pileup" file for this EVT file, to filter out pileup, if appropriate, which contains upper and lower T limits for each E. Or, a "XANES energies" file in XANES mode, which contains the energy list.', $
					scr_xsize=evt_file_xsize2)

throttle_base = widget_base( files_base, /row, /base_align_center, xpad=2, ypad=0, space=space2, /align_right, map=DevObj->throttle())
throttle_base2 = throttle_base

throttle_button = widget_button( throttle_base2, value='Throttle:', uname='throttle_button', /tracking, $
					uvalue='Browse to select the "throttle" file for this EVT file, to correct for Throttle losses, if appropriate.', scr_xsize=evt_button_xsize)
throttle_file = widget_text( throttle_base2, value=(*p).throttle_file, uname='throttle_file', /tracking, /editable, $
					notify_realize='OnRealize_throttle_file', $
					uvalue='Enter a file-name for the "throttle" file for this EVT file, to correct for Throttle losses, if appropriate; or click on the button to the left.', $
					scr_xsize=evt_file_xsize2)

linearize_base = widget_base( files_base, /row, /base_align_center, xpad=2, ypad=0, space=space2, /align_right, map=DevObj->linear())
linearize_base2 = linearize_base

linearize_button = widget_button( linearize_base2, value='Linearize:', uname='linearize_button', /tracking, $
					uvalue='Browse to select the "linearize" file for this EVT file, to linearize gain on input, if appropriate.', scr_xsize=evt_button_xsize)
linearize_file = widget_text( linearize_base2, value=(*p).linearize_file, uname='linearize_file', /tracking, /editable, $
					notify_realize='OnRealize_linearize_file', $
					uvalue='Enter a file-name for the "linearize" file for this EVT file, to linearize gain on input, if appropriate; or click on the button to the left. ' + $
					'Maia corrects linearization internally now, so this should not be needed.', $
					scr_xsize=evt_file_xsize2)

; Sample, grain and comment details

samplebase = widget_base( files_base, /row, /base_align_center, xpad=2, ypad=1, space=space2)
lab = widget_label( samplebase, value='Sample:')

sample = widget_text( samplebase, uname='sample', /editable, value=string((*p).sample), $
					uvalue='Sample name string.',scr_xsize=sample_xsize, /tracking)

lab = widget_label( samplebase, value='  Grain:')

grain = widget_text( samplebase, uname='grain', /editable, value=string((*p).grain), $
					uvalue='Grain or analysis region name string.',scr_xsize=sample_xsize, /tracking)

commentbase = widget_base( files_base, /row, /base_align_center, xpad=2, ypad=0, space=space2, /align_right)
lab = widget_label( commentbase, value='Comment:')

comment = widget_text( commentbase, uname='comment', /editable, value=string((*p).comment), $
					uvalue='Enter any comment text to be saved with the output file.', $
					scr_xsize=comment_xsize, /tracking)


; ----------- device ------------------------------------------------------------------------------

device_base = widget_base( tab_panel, title=tab_names[1], /column, xpad=1, ypad=1, scr_xsize=tab_xsize, space=space1, /base_align_center, /align_center)
lab = widget_label( device_base, value='Device Specific Parameters')

;	Device options

maiabase = widget_base( device_base, /base_align_center, map=1, space=1, xpad=0, ypad=0)

device_option_mode_base = widget_base( maiabase, /column, /frame,  space=1, xpad=0, ypad=1, /base_align_center, xsize=stepmode_base_width)

; Render sort options in render_options method in object, else set Y size to 1

DevObj->render_options, device_option_mode_base
widget_control, device_option_mode_base, map=DevObj->show_sort_options(), scr_ysize=DevObj->get_sort_ysize()


; ----------- scan ------------------------------------------------------------------------------

scan_base = widget_base( tab_panel, title=tab_names[2], /column, xpad=1, ypad=1, scr_xsize=tab_xsize, space=space1, /base_align_center, /align_center)
lab = widget_label( scan_base, value='Beam / Sample Scanning')

xybase = widget_base( scan_base, /column, xpad=2, ypad=0, /base_align_center, /align_center, map=((*p).sort_mode ne 1))

xymode_base = widget_base( xybase, /row, map=1, xpad=0, ypad=1, /base_align_center, /align_center)

;	XY scan mode droplist

lab = widget_label( xymode_base, value='XY Scan Mode:')
xy_mode = widget_combobox( xymode_base, value=xy_modes, uname='xy_mode', /tracking, $
					notify_realize='OnRealize_xy_mode', $
					uvalue='Select scan mode: beam/sample scanning in both X and Y, ' + $
					'stage stepping in X (or Y) and scanning in the other, ' + $
					'stage stepping in both X and Y along a traverse line, and ' + $
					'XY scan combined with Energy or Tomo angle scan (Z).', xsize=xy_mode_xsize)

; ... step mode ....

stepbase = widget_base( scan_base, /column, /base_align_center, map=1, space=1, xpad=0, ypad=0)

stepmode_base = widget_base( stepbase, /column, /frame,  space=1, xpad=0, ypad=1, /base_align_center, xsize=stepmode_base_width)
lab = widget_label( stepmode_base, value='Step Advance Parameters')

;	step advance modes droplist

step_mode = widget_combobox( stepmode_base, value=step_modes, uname='step_mode', /tracking, $
					notify_realize='OnRealize_step_mode', $
					uvalue='Indicates how step advance is encoded. Choose between advance ' + $
					'using the toggling of a high-order ADC bit, counts in another ADC, or ' + $
					'an event count in the data ADC.',xsize=stepmode_xsize)

stepbase0 = widget_base( stepmode_base, column=2, xpad=0, ypad=0, space=space1, /base_align_right, /align_center)
stepbase1 = widget_base( stepbase0, /row, /align_right, /base_align_center, xpad=0, ypad=0)
lab = widget_label( stepbase1, value='Station:')

; 	step station droplist

step_station = widget_combobox( stepbase1, value=station_numbers, uname='step_station', /tracking, $
					notify_realize='OnRealize_step_station', $
					uvalue='Select station (i.e. ADC) with step advance information.',xsize=station_xsize)

stepbase2 = widget_base( stepbase0, xpad=0, ypad=0, /align_right)
stepcount_base = widget_base( stepbase2, /row, map=0, /base_align_center, /align_right, xpad=0, ypad=0)
lab = widget_label( stepcount_base, value='  Count:')
step_count = widget_text( stepcount_base, uname='step_count', /editable, value=str_tidy((*p).step_count), $
					uvalue='Number of counts in selected ADC station per step.',scr_xsize=distance_xsize, /tracking)

stepbit_base = widget_base( stepbase2, /row, map=1, /base_align_center, /align_right, xpad=0, ypad=0)
lab = widget_label( stepbit_base, value='        Bit:')

;	step bit droplist

step_bit = widget_combobox( stepbit_base, value=bit_numbers, uname='step_bit', /tracking, $
					notify_realize='OnRealize_step_bit',$
					uvalue='Number of toggle bit (0-12) that encodes a stage step advance.',xsize=station_xsize)

stepdrop_base = widget_base( stepbase0, /row, /base_align_center, /align_right, xpad=0, ypad=0)
lab = widget_label( stepdrop_base, value='Steps:')

; 	step step-size droplist

step_size_drop = widget_combobox( stepdrop_base, value='  '+str_tidy(indgen(30)+1)+'  ', uname='step_size_drop', /tracking, $
					notify_realize='OnRealize_step_size',$
					uvalue='Select step size in sample stage stepper units.',xsize=station_xsize)

stepbase4 = widget_base( stepbase0, /row, /base_align_center, /align_right, xpad=0, ypad=0)
lab = widget_label( stepbase4, value='      Distance:')

; 	step step-size floating text

step_size = widget_text( stepbase4, uname='step_size', /editable, value=str_tidy((*p).step_size), $
					uvalue='Size of each step in microns. For X or Y stepping this is related ' + $
					'to the stepper increment. For a traverse enter the distance travelled ' + $
					'per point along the traverse line.',scr_xsize=distance_xsize, /tracking)

;.......................

;	XY range, compress

parsbase = widget_base( scan_base, /column, space=1, xpad=2, ypad=1, /base_align_right, /align_center)
xpars_base = widget_base( parsbase, column=3, xpad=0, ypad=0, space=space2, /base_align_right)

xbase = widget_base( xpars_base, /row, /base_align_center, xpad=0, ypad=0, space=1)
lab = widget_label( xbase, value='X range:')
xrange = widget_text( xbase, uname='x_range', /editable, /tracking, value=str_tidy((*p).xrange), $
					uvalue='Total number of X pixels in a scan or steps along a traverse. Select a sub-region using the "selected sub-region" controls below.', scr_xsize=xrange_xsize)

xcbase = widget_base( xpars_base, /row, /base_align_center, xpad=0, ypad=0, space=1)
lab = widget_label( xcbase, value='X compress:', scr_xsize=xrange_xsize+10)
xcompress = widget_combobox( xcbase, value=compressions, uname='x_compress', /tracking, $
					notify_realize='OnRealize_xcompress', scr_xsize=xrange_xsize, $
					uvalue='Compression factor (binning) for X (1=no compression).')

xsbase = widget_base( xpars_base, /row, /base_align_center, xpad=0, ypad=0, space=1)
lab = widget_label( xsbase, value='X size:')
xsize = widget_text( xsbase, uname='x_size', /editable, /tracking, value=str_tidy((*p).xsize), $
					scr_xsize=xrange_xsize, uvalue='X size of scan area in microns, or the total distance ' + $
					'travelled in a traverse.')

ypars_base = widget_base( parsbase, column=3, xpad=0, ypad=0, space=space2, /base_align_right)

ybase = widget_base( ypars_base, /row, /base_align_center, xpad=0, ypad=0, space=1)
lab = widget_label( ybase, value='Y range:')
yrange = widget_text( ybase, uname='y_range', /editable, /tracking, value=str_tidy((*p).yrange), $
					uvalue='Total number of Y pixels in a scan. Select a sub-region using the "selected sub-region" controls below.', scr_xsize=xrange_xsize)

ycbase = widget_base( ypars_base, /row, /base_align_center, xpad=0, ypad=0, space=1)
lab = widget_label( ycbase, value='Y compress:', scr_xsize=xrange_xsize+10)
ycompress = widget_combobox( ycbase, value=compressions, uname='y_compress', /tracking, $
					notify_realize='OnRealize_ycompress', scr_xsize=xrange_xsize, $
					uvalue='Compression factor (binning) for Y (1=no compression).')

ysbase = widget_base( ypars_base, /row, /base_align_center, xpad=0, ypad=0, space=1)
lab = widget_label( ysbase, value='Y size:')
ysize = widget_text( ysbase, uname='y_size', /editable, /tracking, value=str_tidy((*p).ysize), $
					scr_xsize=xrange_xsize, uvalue='Y size of scan area in microns (or eV/mdeg for non-spatial axis).')

zpars_base = widget_base( parsbase, column=3, xpad=0, ypad=0, space=space2, /base_align_right, map=(((*p).xy_mode eq 4) or ((*p).xy_mode eq 5)))

zbase = widget_base( zpars_base, /row, /base_align_center, xpad=0, ypad=0, space=1)
lab = widget_label( zbase, value='Z range:')
zrange = widget_text( zbase, uname='z_range', /editable, /tracking, value=str_tidy((*p).zrange), $
					uvalue='Total number of Z pixels in a scan, for Energy or Tomo angle.', scr_xsize=xrange_xsize)

zoptions_base = widget_base( zpars_base, xpad=0, ypad=0)

zorigin_base = widget_base( zoptions_base, /row, /base_align_center, xpad=0, ypad=0, space=1, map=((*p).xy_mode eq 4))
lab = widget_label( zorigin_base, value='   Z origin:', scr_xsize=xrange_xsize+10)
zorigin = widget_text( zorigin_base, uname='z_origin', /editable, /tracking, value=str_tidy((*p).zorigin), notify_realize='OnRealize_zorigin', $
					uvalue='Origin of Z axis for a scan, for Energy or Tomo angle.', scr_xsize=xrange_xsize)

zcompress_base = widget_base( zoptions_base, /row, /base_align_center, xpad=0, ypad=0, space=1, map=((*p).xy_mode eq 5))
lab = widget_label( zcompress_base, value='Z compress:', scr_xsize=xrange_xsize+10)
zcompress = widget_combobox( zcompress_base, value=compressions, uname='z_compress', /tracking, $
					notify_realize='OnRealize_zcompress', scr_xsize=xrange_xsize, $
					uvalue='Compression factor (binning) for Z (Energy, Tomo) (1=no compression).')

zsbase = widget_base( zpars_base, /row, /base_align_center, xpad=0, ypad=0, space=1)
lab = widget_label( zsbase, value='Z size:')
zsize = widget_text( zsbase, uname='z_size', /editable, /tracking, value=str_tidy((*p).zsize), $
					scr_xsize=xrange_xsize, uvalue='Z size of scan, for energy, angle (mdeg or eV).')

;cbase = widget_base( parsbase, /row, /base_align_center, xpad=2, ypad=0, /center)
size_base = widget_base( parsbase, /row, /base_align_center, /align_center, xpad=0, ypad=0, space=80, map=(1 - ( (*p).xy_mode eq 0))  )
;dummy = widget_label(size_base, value=' ', scr_xsize=20)
step_range_button = widget_button( size_base, value='Step Range', uname='steprange_button', /tracking, $
					uvalue='In X (or Y) step mode, use this pop-up to set the X (or Y) range in the stepping direction.')
;dummy = widget_label(size_base, value=' ',scr_xsize=step_range_dummy_xsize)
size_button = widget_button( size_base, value='Calculate Size', uname='setsize_button', /tracking, $
					uvalue='In X (or Y) step mode, set the X and Y sizes from the X,Y range values and the step-size, assuming square pixels.')

scan_window_base = widget_base( scan_base, /column, space=1, xpad=2, ypad=1, /base_align_right, /align_center, $
					map=((*p).sort_mode ne 1))
lab = widget_label( scan_window_base, value='Offset / Windowing of Scan', /align_center)

sub_region_mode = ((*p).xoffset ne 0) or ((*p).yoffset ne 0)
(*p).image_mode = sub_region_mode
off_base = widget_base( scan_window_base, /row, xpad=0, ypad=0, space=space2, /base_align_right, /align_center)
lab = widget_label( off_base, value='Image Mode:')
image_mode = widget_combobox( off_base, value=image_modes, uname='image_mode', /tracking, $
					notify_realize='OnRealize_evt_image_mode', $
					uvalue='Select image mode: Make image for "full area", or ' + $
					'Select a window or "sub-region" to image. The latter uses no compression, to enable full resolution ' + $
					'for a limited area.', xsize=xy_mode_xsize)

offset_base = widget_base( scan_window_base, /row, xpad=0, ypad=0, space=space2, /base_align_top, /align_center, map=(sub_region_mode))
off_base2 = widget_base( offset_base, column=2, xpad=0, ypad=0, space=space2, /base_align_right, /align_center )
offx_base = widget_base( off_base2, /row, xpad=3, ypad=0, space=space2, /base_align_right)
lab = widget_label( offx_base, value='X offset:')
xoffset = widget_text( offx_base, uname='x_offset', /editable, /tracking, value=str_tidy((*p).xoffset), $
					scr_xsize=xrange_xsize, uvalue='X offset of window to image in total area (express in non-compressed pixels). ' + $
					'Use "Box" shape on image window to determine Window coordinates.')
offy_base = widget_base( off_base2, /row, xpad=3, ypad=0, space=space2, /base_align_right)
lab = widget_label( offy_base, value='Y offset:')
yoffset = widget_text( offy_base, uname='y_offset', /editable, /tracking, value=str_tidy((*p).yoffset), $
					scr_xsize=xrange_xsize, uvalue='Y offset of window to image in total area (express in non-compressed pixels).'+ $
					'Use "Box" shape on image window to determine Window coordinates.')
offxr_base = widget_base( off_base2, /row, xpad=3, ypad=0, space=space2, /base_align_right)
lab = widget_label( offxr_base, value='X sub-range:')
x_sub_range = widget_text( offxr_base, uname='x_sub_range', /editable, /tracking, value=str_tidy((*p).x_sub_range), $
					scr_xsize=xrange_xsize, uvalue='X range of window to image from total area (express in non-compressed pixels).'+ $
					'Use "Box" shape on image window to determine Window coordinates.')
get_window_button = widget_button( offset_base, value='Get', uname='get-window-region', /tracking, $
						uvalue='Get the previous "Box" shape region from the "Image" window to use as the Windowed Sub-region here.')

offyr_base = widget_base( off_base2, /row, xpad=3, ypad=0, space=space2, /base_align_right)
lab = widget_label( offyr_base, value='Y sub-range:')
y_sub_range = widget_text( offyr_base, uname='y_sub_range', /editable, /tracking, value=str_tidy((*p).y_sub_range), $
					scr_xsize=xrange_xsize, uvalue='Y range of window to image from total area (express in non-compressed pixels).'+ $
					'Use "Box" shape on image window to determine Window coordinates.')


; ----------- flux ------------------------------------------------------------------------------

flux_base = widget_base( tab_panel, title=tab_names[3], /column, xpad=1, ypad=1, scr_xsize=tab_xsize, space=space1, /base_align_center, /align_center)
lab = widget_label( flux_base, value='Beam Flux / Charge Set-up')

c0base = widget_base( flux_base, /row, /base_align_center, xpad=2, ypad=1, space=5)
charge_mode = widget_combobox( c0base, value=qmodes, uname='charge-mode', $
					notify_realize='OnRealize_evt_charge_mode', /tracking, uvalue=qhelp, scr_xsize=tab_xsize-30)

IC_base = widget_base( flux_base, /column, /frame, xpad=4, ypad=1, space=space1, /base_align_right, map=((*p).charge_mode ne 0))
IC_base1 = widget_base( IC_base, /column, xpad=0, ypad=0, space=space1, /base_align_right, map=((*p).charge_mode eq 1))
;lab = widget_label( IC_base, value='Ion Chamber / Charge Integrator Parameters', /align_center)

evt_check_pvlist, (*p).pic_list, DevObj

	s1base = widget_base( IC_base1, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s1base, value='Scaler channel:')
	ic_pv_mode = widget_combobox( s1base, value='   '+*(*p).pic_list, uname='ic-pv-mode', /tracking, $
					notify_realize='OnRealize_evt_charge_pv_mode', $
					uvalue='Select the Epics scaler PV used to record upstream ion-counter.',scr_xsize=charge_xsize2)

	s2base = widget_base( IC_base1, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s2base, value='Preamp Sensitivity:')
	ic_val_mode = widget_combobox( s2base, value='   '+str_tidy(ic_vals), uname='ic-preamp-mode', /tracking, scr_xsize=charge_xsize2, $
					notify_realize='OnRealize_evt_charge_preamp_mode', $
					uvalue="Select the ion chamber preamp sensitivity multiplier.")

	s3base = widget_base( IC_base1, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s3base, value='Preamp Scale Units:')
	ic_unit_mode = widget_combobox( s3base, value='   '+ic_units, uname='ic-preamp-unit-mode', /tracking, scr_xsize=charge_xsize2, $
					notify_realize='OnRealize_evt_charge_unit_mode', $
					uvalue="Select the ion chamber preamp scale units.")

	dwell_base = widget_base( IC_base1, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( dwell_base, value='Dwell time (ms):')
	dwell_wID = widget_text( dwell_base, uname='dwell', /editable, value=str_tidy((*p).dwell), $
						uvalue='Enter the dwell time per pixel (ms) if it is fixed. This is for converting count-rate to counts per pixel. ' + $
						'It may be found by the device driver, especially for cases when it varies per pixel.', $
						scr_xsize=charge_xsize2, /tracking)

	ic_base2 = widget_base( IC_base, /row, /base_align_center, xpad=0, ypad=1, space=2)
	scan_button = widget_button( ic_base2, value='Scan data for PVs', uname='charge-scan', /tracking, sensitive=((*p).charge_mode eq 1), $
						uvalue='Open raw data file(s) and scan for Dwell and Epics PVs for IC rate and preamplifier settings. ' + $
						'This is done automatically when new data files are selected.')
	lab = widget_label( ic_base2, value='     Conversion (Q/IC):')
	charge_conversion = widget_text( ic_base2, uname='charge-conversion', /editable, /tracking, value=str_tidy((*p).charge_conversion), $
					notify_realize='OnRealize_evt_charge_conversion', $
					uvalue='Conversion factor from integrated flux (IC count) to charge (uC) for scan.', scr_xsize=charge_xsize)
						
cbase = widget_base( flux_base, /row, /base_align_center, xpad=2, ypad=1, space=5)
lab = widget_label( cbase, value='Equivalent charge (Q uC):')
charge = widget_text( cbase, uname='charge', /editable, /tracking, value=str_tidy((*p).charge), $
					uvalue='Total integrated charge for scan (uC). If the device maps charge across image '+ $
					'then this will be filled in after the sort. If the device does not, enter the total '+ $
					'charge for the map (assumed uniform).', scr_xsize=charge_xsize)

; ----------- DA/Cal ------------------------------------------------------------------------------

da_base = widget_base( tab_panel, title=tab_names[4], /column, xpad=1, ypad=1, scr_xsize=tab_xsize, space=space1, /base_align_center, /align_center)
lab = widget_label( da_base, value='Data Sorting Method and Energy Calibration')

statbase = widget_base( da_base, /row, xpad=2, ypad=1, space=1, /base_align_center, /align_center)

;	station to map droplist

val = adc_list_device( DevObj, max_adcs=max_adcs)

lab = widget_label( statbase, value='Channel:')

station = widget_combobox(statbase, uname='station', scr_xsize=station_xsize+5, $
			NOTIFY_REALIZE='OnRealize_evt_station', $
			value=val, /tracking, uvalue='Select ADC channel to display and edit parameters (' + $
			'displayed below). In "Single Detector" mode, this is also the ADC channel to process for images. ' + $
			'Click "?" to select active channel in "detector layout".')

lab = widget_label( statbase, value='   ')

;	Select single / array detectors

array = widget_combobox( statbase, value=[' Single Detector ',' Detector Array '], uname='array', /tracking, $
					notify_realize='OnRealize_EVT_array', $
					uvalue='Select between sorting a Single detector on one ADC channel, or an array of detectors on Multiple ADC channels. ' + $
					'For Detector Arrays, enable channels using "Enable" below, or using "Get" to retrieve good spectra and their Cals.')

detector_layout_base = widget_base( statbase, /row, xpad=0, ypad=0, space=0, /base_align_center, map=(*p).array)

button = widget_button( detector_layout_base, value='?', uname='detector-layout', /tracking, $
						uvalue='Open the detector layout pop-up window to select detector elements in an array, ' + $
						'and show status and properties.')

;.......................

;	stations

	si = str_tidy((*p).station)

	station_base = widget_base( da_base, /column, /frame, /align_center, /base_align_center, xsize=map_base_xsize, ypad=1)
	base1 = widget_base( station_base, /column, /base_align_right, space=space1, xpad=0, ypad=0)

	base1b = widget_base( base1, /row, /base_align_center, ypad=0, space=40)
	enable = cw_bgroup2( base1b, ['Enable'], /row, set_value=[(*p).enable[(*p).station]], sensitive=(*p).array, $
						/return_index, uname='enable',/ nonexclusive, /tracking, $
						uvalue='Enable sorting for this ADC channel or detector. This brings up a selection pop-up window for detector arrays.', ypad=0)
	typebase1 = widget_base( base1b, /row, /base_align_center, xpad=0, ypad=0)
	lab = widget_label( typebase1, value='Data type:')
	type = widget_combobox( typebase1, value=data_types, uname='type', /tracking, notify_realize='OnRealize_type', $
					uvalue='Select the type of data stored on this detector or ADC channel.',xsize=type_xsize)

	base1c = widget_base( base1, /row, /base_align_center, space=space5, ypad=0)
	lab = widget_label( base1c, value='Cal A:')
	cal_a = widget_text( base1c, uname='cal_a', /editable, /tracking, value=str_tidy((*p).cal_a[(*p).station], places=8), $
						uvalue='Calibration gain for this detector/ADC channel. This gain must be in "keV/ch" and match the compression ' + $
						'used on the spectrum used to make the DA matrix. Use 1.0 for channels.',scr_xsize=cal_xsize)
	lab = widget_label( base1c, value='B:')
	cal_b = widget_text( base1c, uname='cal_b', /editable, /tracking, value=str_tidy((*p).cal_b[(*p).station]), $
						uvalue='Calibration offset for this detector/ADC channel. This offset must be in "keV" and match the compression ' + $
						'used on the spectrum used to make the DA matrix. Use 0.0 for channels.',scr_xsize=cal_xsize)
	button = widget_button( base1c, value='Get', uname='getcal_button', /tracking, $
						uvalue='Load the energy calibration(s) from a SPEC file, for this ADC only in "Single detector" ' + $
						'mode, or containing all good detectors in "Detector Array" mode (enable spectra channels present, disable others).')
	button = widget_button( base1c, value='DA', uname='da_cal_button', /tracking, $
						uvalue='Use the energy cal from the fit used to generate the DA matrix ' + $
						'(if using the Dynamic Analysis method). Do not use this in "Detector Array" mode.')

	base1e = widget_base( base1, /row, /base_align_center, /align_center, ypad=0)
	lab = widget_label( base1e, value='Projection:')
	mode = widget_combobox( base1e, value=projection_modes[*,(*p).sort_mode], uname='mode', /tracking, $
						notify_realize='OnRealize_mode', $
						uvalue='Select the projection method for this ADC, or all enabled ADCs in "Detector Array" mode. ' + $
						'Choose between "Dynamic Analysis", spectrum energy "Cuts", "STIM mean energy", "Multiphase DA", "XYE data cube" or "All Counts".',xsize=mode_xsize)

	base_proj_file = widget_base( base1, /row, /base_align_center, ypad=0, map=((*p).mode[(*p).station] ne 4) and ((*p).mode[(*p).station] ne 5))
	button = widget_button( base_proj_file, value='File:', uname='file_button', /tracking, $
						uvalue='Browse to select a file-name for the projection method for this ADC, or all in "Detector Array" mode.')
	file = widget_text( base_proj_file, value=(*p).file[(*p).station], uname='file', /tracking, /editable, $
						notify_realize='OnRealize_station_file', $
						uvalue='Enter a file-name for the projection method, ' + $
						'or click on the "File" button to the left to browse for the projection file.',scr_xsize=file_xsize-35)

	base_billboard = widget_base( base_proj_file)
	base_new_MPDA = widget_base( base_billboard, /row, /base_align_center, xpad=0, ypad=0, map=((*p).mode[(*p).station] eq 3))
	new_button = widget_button( base_new_MPDA, value='New', uname='new_button', /tracking, $
						uvalue='Creates a new .MPDAM file. Browse to select a phase map .DAI file and a corrections matrix .CORRECT file.')
	base_export = widget_base( base_billboard, /row, /base_align_center, xpad=0, ypad=0, map=((*p).mode[(*p).station] ne 3))
	export_button = widget_button( base_export, value='Exp', uname='export_DA_button', /tracking, $
						uvalue='Export the chosen DA matrix file to a binary file to be used for real-time processing.')

	da_xanes_base0 = widget_base( da_base, xpad=0, ypad=0, /base_align_center)
	da_xanes_base1a = widget_base( da_xanes_base0, /column, xpad=0, ypad=0, space=space2, /base_align_center, map=((*p).xy_mode eq 4))

	dbase4a = widget_base( da_xanes_base1a, /row, /base_align_center, ypad=0, space=15)
	collapse = cw_bgroup2( dbase4a, ['Collapse E'], /row, set_value=[(*p).collapse_energy], $
						/return_index, uname='collapse',/ nonexclusive, /tracking, $
						uvalue='Enable collapsing the energy/angle pixel planes down onto the energy list in the DA matrix stack.', ypad=0)
	dbase4ab = widget_base( dbase4a, /row, /base_align_center, ypad=0)
	button = widget_button( dbase4ab, value='XANES Element:', uname='el_select_button', /tracking, $
						uvalue='Select the XANES element from all elements used in the DA matrix.')
	el_select_text = widget_text( dbase4ab, value=(*p).el_select, uname='xanes-el-select', /tracking, /editable, $
;						notify_realize='OnRealize_xanes_element', $
						uvalue='Enter the name of the XANES element to use for the stack.',scr_xsize=cal_xsize)

	da_xanes_base1b = widget_base( da_xanes_base0, /column, xpad=0, ypad=0, space=space2, /base_align_center, map=((*p).mode[(*p).station] ne 3) and ((*p).mode[(*p).station] ne 4) and (((*p).xy_mode eq 0)))

	dbase4b = widget_base( da_xanes_base1b, /row, /base_align_center, ypad=0, space=15)
	lab = widget_label( dbase4b, value='Energy proxy axis:')
	proxy_axis = widget_combobox( dbase4b, value=proxy_modes, uname='proxy-axis', /tracking, $
						notify_realize='OnRealize_proxy_axis', $
						uvalue='Select the Proxy Axis that provides the index into an Energies File list, or "None" to not use an energy proxy.',xsize=120)

	da_xanes_base2 = widget_base( da_base, /column, xpad=0, ypad=0, space=space2, /base_align_center, map=(((*p).xy_mode eq 4) or (((*p).xy_mode eq 0) and ((*p).mode[(*p).station] ne 3) and ((*p).mode[(*p).station] ne 4) and((*p).energy_proxy_axis gt 0))))

	dbase5 = widget_base( da_xanes_base2, /row, /base_align_center, ypad=0)
	button = widget_button( dbase5, value='Energies:', uname='energy_file_button', /tracking, $
						uvalue='Browse to select a file-name for the XANES energies table for the (energy + angle) pixel steps on the Proxy Axis (Z in 3D stack mode).')
	energy_file_text = widget_text( dbase5, value=(*p).xanes_energies_file, uname='energies-file', /tracking, /editable, $
						notify_realize='OnRealize_evt_energies_file', $
						uvalue='Enter a file-name for the XANES energies table for the (energy + angle) pixel steps on the Proxy Axis (Z in 3D stack mode), ' + $
						'or click on "File" to the left to browse for the file.',scr_xsize=file_xsize-20)

;---------- end tab ----------------------------------------------------------------------------------------------------

obase = widget_base( tbase, /row, /base_align_center, ypad=1, space=space5)
button = widget_button( obase, value='Output:', uname='output_button', /tracking, $
					uvalue='Browse to select the output file name. This will be set automatically after the EVT file is selected. Click on "Output" to select a different output directory, or to edit the filename.', $
					scr_xsize=55)
output_file = widget_text( obase, value=(*p).evt_file, uname='output_file', /tracking, /editable, $
					notify_realize='OnRealize_output_file', $
					uvalue='Enter a file-name for the output file. This will be set automatically after the EVT file is selected. Click on "Output" to select a different output directory, or to edit the filename.', $
					scr_xsize=file_xsize+24)

bbase = widget_base( tbase, /row, /base_align_center, ypad=0, space=space5)
button = widget_button( bbase, value='Start', uname='start_button', /tracking, $
					uvalue='Start sorting the list-mode data file with the chosen settings. ' + $
					'All settings will be saved in the header of the output DAI file.')

flatten_id = cw_bgroup2( bbase, ['Flatten'], /row, set_value=[(*p).flatten], /return_index, uname='flatten',/ nonexclusive, /tracking, $
						uvalue='Enable correction of the images at the end of the sort to correct for flux variation.', xpad=0, ypad=0)
	
enable_cluster = DevObj->cluster()
if enable_cluster eq 0 then (*p).cluster = 0
cluster_id = cw_bgroup2( bbase, ['Cluster'], /row, set_value=[(*p).cluster], sensitive=enable_cluster, $
						/return_index, uname='cluster-option',/ nonexclusive, /tracking, xpad=0, ypad=0, $
						uvalue=['"Cluster" enables parallel processing using multiple cores or a local configured computer cluster.'])
				
update_button = widget_button( bbase, value='Update', uname='update_button', /tracking, xsize=update_xsize, $
					uvalue='Write updated settings back to the Output. NOTE: ' + $
					'Critical parameters related to data format cannot be changed. ' + $
					'Alternatively, edit lines in "Image Properties and History" window.')

;button = widget_button( bbase, value='Export', uname='export_button', /tracking, $
;					uvalue='Write EVT file out in a standard form (blog data file) for CMIT processor tests. ' + $
;					'Note that this file may be significantly larger than the original EVT file.')

button = widget_button( bbase, value='C*', uname='command_file_button', /tracking, xsize=com_xsize, $
					uvalue='Generate a "GeoPIXE Command File" (.gcf) to use as a template for execution by GeoPIXE in a processing pipeline ' + $
					'to process raw data into images.')

button = widget_button( bbase, value='Close', uname='close_button', /tracking, $
					uvalue='Exit the EVT sort popup window. All parameters will be recovered ' + $
					'if you reopen the Sort EVT window from the same parent window.')

;.................................................................................

help = widget_text( tbase, scr_xsize=help_xsize, ysize=4, /wrap, uname='HELP', /tracking, $
				uvalue='Help window. Displays info about widgets.',frame=0)

state = {  $
		tlb:			tlb, $				; top level base
		path:			ptr_new(path), $	; pointer to current path
		dpath:			ptr_new(dpath), $	; pointer to current path
		detector_path:	ptr_new(geopixe_root), $	; pointer to current path for detector pop-up
		test:			test, $				; test mode
		pprefs:			pprefs, $			; preferences
		pimage:			ptr_new(), $		; pointer to images for notify 'images'
		pspec:			ptr_new(), $		; pointer to spectra for notify 'spectra'
		pdev:			ptr_new(/allocate_heap), $	; pointer to device to pass to batch
		pdet:			ptr_new(/allocate_heap), $	; pointer to detector_layout to notify of get, ...
		pval:			ptr_new(/allocate_heap), $	; pointer general parameter passing
		pval2:			ptr_new(/allocate_heap), $	; pointer general parameter passing
		pwiz:			ptr_new(/allocate_heap), $	; pointer for return from Wizard window 'open-test'

		local_spectra:		0, $			; flags local responsibility for spectra
		local_images:		0, $			; flags local responsibility for images
		p:				p, $				; pointer to parameters
		ic_vals:		ic_vals, $			; IC vals list
		ic_units:		ic_units, $			; IC units string list
		ic_vunits:		ic_vunits, $		; scale units for units list

		bit_numbers:	long(bit_numbers), $	; toggle bit number lookup table
		compressions:	long(compressions), $	; compression value lookup table
		projection_modes: projection_modes, $	; projection mode strings
		outputs:		outputs, $			; output file extensions
		tab_names:		tab_names, $		; tab names

		batch_button:	batch_button, $		; batch button ID
		sort_mode:		toggle, $			; sort mode toggle buttons ID
		device_mode:	device_mode, $		; device droplist ID
		cluster_id:		cluster_id, $		; cluster checkbox ID
		from_button:	from_button, $		; from template button ID
		evt_file:		evt_file, $			; EVT file-name text ID
		evt_button:		evt_button, $		; EVT file button ID
		evt2_file:		evt2_file, $		; Last EVT file-name text ID
		evt2_button:	evt2_button, $		; EVT2 file button ID
		evt_base:		evt_base, $			; First EVT file-name base widget ID
		evt_base_ysize:	0, $				; evt_base height
		evt2_base:		evt2_base, $		; Last EVT file-name base mapped widget ID
		evt2_base2:		evt2_base2, $		; Last EVT file-name base ysized widget ID
		throttle_base:	throttle_base, $	; throttle file-name base mapped widget ID
		throttle_base2:	throttle_base2, $	; throttle file-name base ysized widget ID
		throttle_file:	throttle_file, $	; throttle file-name text ID
		throttle_button: throttle_button, $	; throttle file button ID
		pileup_base:	pileup_base, $		; pileup file-name base mapped widget ID
		pileup_base2:	pileup_base2, $		; pileup file-name base ysized widget ID
		pileup_file:	pileup_file, $		; pileup file-name text ID
		pileup_button:	pileup_button, $	; pileup file button ID
		linearize_base:	linearize_base, $		; linearize file-name base mapped widget ID
		linearize_base2: linearize_base2, $		; linearize file-name base ysized widget ID
		linearize_file:	linearize_file, $		; linearize file-name text ID
		linearize_button: linearize_button, $	; linearize file button ID
		output_file:	output_file, $		; Output file-name text ID
		sample:			sample, $			; sample name text ID
		grain:			grain, $			; grain name text ID
		comment:		comment, $			; comment text ID
		xybase:			xybase, $			; XY scan mode base ID to map (for sort-mode 1)
		xybase_ysize:	0, $				; XY scan base Y size for mapping
		xymode_base:	xymode_base, $		; XY scan mode base ID to map
		xy_mode:		xy_mode, $			; XY mode droplist ID
		stepmode_base:	stepbase, $			; step advance mode base ID to map
		stepmode_ysize:	0, $				; scr_ysize for stepmode base
		step_mode:		step_mode, $		; step mode droplist ID
		step_station:	step_station, $		; step station droplist ID
		stepcount_base:	stepcount_base, $	; step count base ID to map
		step_count:		step_count, $		; step count text ID
		stepbit_base:	stepbit_base, $		; step bit base ID to map
		step_bit:		step_bit, $			; step bit droplist ID
		step_size:		step_size, $		; step size float text ID
		stepdrop_base:	stepdrop_base, $	; step size droplist base ID
		step_size_drop:	step_size_drop, $	; step size droplist ID
		scan_window_base: scan_window_base, $	; scan windowing base ID

		xrange:			xrange, $			; X range text ID
		yrange:			yrange, $			; Y range text ID
		zrange:			zrange, $			; Z range text ID
		xpars_base:		xpars_base, $		; X range, etc. base ID
		xpars_ysize:	0, $				; X size of Y base
		ypars_base:		ypars_base, $		; Y range, etc. base ID
		ypars_ysize:	0, $				; Y size of Y base
		zpars_base:		zpars_base, $		; Z range, etc. base ID
		zpars_ysize:	0, $				; Y size of Z base
		xcompress:		xcompress, $		; X compress droplist ID
		ycompress:		ycompress, $		; Y compress droplist ID
		zcompress:		zcompress, $		; Z compress droplist ID
		xsize:			xsize, $			; X size text ID
		ysize:			ysize, $			; Y size text ID
		zsize:			zsize, $			; Z size text ID
		zorigin:		zorigin, $			; Z origin text ID
		zorigin_base:	zorigin_base, $		; Z origin map base ID
		zcompress_base:	zcompress_base, $	; Z compress map base ID
		
		size_base:		size_base, $		; set size button base ID
		da_xanes_base1a: da_xanes_base1a, $	; XANES items (Xanes element) on DA tab base ID
		da_xanes_base1b: da_xanes_base1b, $	; XANES items (proxy axis) on DA tab base ID
		da_xanes_base2:	da_xanes_base2, $	; XANES items (energies file) on DA tab base ID
		proxy_axis:		proxy_axis, $		; XANES proxy axis droplist ID
		base_proj_file: base_proj_file, $	; base for Projection file, and buttons

		image_mode:		image_mode, $		; image mode droplist ID
		offset_base:	offset_base, $		; XY sub-region select base
		offset_ysize:	0L, $				; offset/range base Y size
		xoffset:		xoffset, $			; X sub-range offset text ID
		yoffset:		yoffset, $			; Y sub-range offset text ID
		x_sub_range:	x_sub_range, $		; X sub-range pixel range text ID
		y_sub_range:	y_sub_range, $		; Y sub-range pixel range text ID
		
		charge_mode:	charge_mode, $		; Charge mode droplist ID
		charge:			charge, $			; charge text ID
		charge_conversion: charge_conversion, $	; charge conversion text ID
		scan_button:	scan_button, $		; scan PVs button ID
		IC_base:		IC_base, $			; Ion chamber base ID
		IC_base1:		IC_base1, $			; Ion chamber base1 ID
		IC_base2:		IC_base2, $			; Conversion base ID
		IC_base1_ysize:	0, $				; Y size of IC_base1
		IC_base2_ysize:	0, $				; Y size of IC_base2
		ic_pv_mode:		ic_pv_mode, $		; IC PV selector ID
		ic_val_mode:	ic_val_mode, $		; IC value selector ID
		ic_unit_mode:	ic_unit_mode, $		; IC unit selector ID
		dwell_ID:		dwell_wID, $		; dwell widget ID
		dwell_base:		dwell_base, $		; dwell base ID
		flatten_id:		flatten_id, $		; flatten toggle ID
		update_button:	update_button, $	; update button ID
				
		station:		station, $			; station droplist ID
		device_option_mode_base:	device_option_mode_base, $	; device options map base ID
		device_option_mode_base_ysize: 0, $	; device_base Y size 
		array:			array, $			; select single of detector array droplist ID
		detector_layout_base: detector_layout_base, $	; base for detector layout pop-up "?" button

		max_adcs:		max_adcs, $			; max number of ADcs
		station_base:	station_base, $		; station parameters base ID to map
		enable:			enable, $			; enable checkmark ID
		type:			type, $				; type droplist ID
		cal_a:			cal_a, $			; cal A text ID
		cal_b:			cal_b, $			; cal B text ID
		mode:			mode, $				; sort mode droplist ID
		file:			file, $				; file name text ID
		new:			new_button, $		; new button ID
		el_select_text:	el_select_text, $	; el select text ID
		energy_file_text: energy_file_text, $ ; XANES energies file text ID
		collapse:		collapse, $			; collapse energies check-box ID
		base_new_MPDA:	base_new_MPDA, $	; mapping base for "New" button
		base_export:	base_export, $		; mapping base for "Exp" button

		help:			help $				; help text ID
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

register_notify, tlb, ['path', $		; new path
					'dpath', $			; new raw data path
					'done-save', $		; batch images/html saved
					'done-filter', $	; batch filters applied
					'snapshot'], $		; snapshots?
				from=group
register_notify, tlb, ['wizard-action']	; global notify from a wizard

; The axis redirection used in some device objects (e.g. Maia, FalconX, UQ iXRF, DAQ, ...) 
; means that changes to an axis will need to look-up values stored in the object for the
; data-file header and then update other values shown in EVT. This is enabled here
; for all devices. If they don't support this, it will be ignord. It will return an
; event 'device-update' to this TLB.

print,'Register notify on all devices ...'
for i=0,n_elements((*(*p).pDevObjList))-1 do begin
	obj = (*(*p).pDevObjList)[i]
	help, obj
	obj->register_notify, tlb, 'device-update', from=tlb
endfor
xmanager, 'evt', tlb, /no_block

return
end
