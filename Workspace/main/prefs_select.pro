; Now obsolete - use "geopixe.conf" file and geopixe_defaults() call
;
;	select = prefs_select( group, path, old_prefs=old_prefs)
;
;	group		parent widget
;	path		default path
;	old_prefs	current preferences states
;	prefs		new preferences
;
;----------------------------------------------------------------------

pro prefs_select_event, Event

common c_working_dir, geopixe_root
if n_elements(geopixe_root) lt 1 then geopixe_root=''

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	case tag_names( event,/structure) of
		'WIDGET_TRACKING': begin
			widget_control, event.id, get_uvalue=s
			if size(s,/tname) eq 'STRING' then begin
				if event.enter eq 1 then begin
					widget_control, (*pstate).help, set_value=s
				endif else begin
					widget_control, (*pstate).help, set_value='Context sensitive help is displayed here. Move cursor over a widget to get help on it.'
				endelse
			endif
			goto, finish
			end
		else:
	endcase

	uname = widget_info( event.id, /uname)

	case uname of

		'prefs_select_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				widget_control, event.top, /destroy
				return
			endif
			end

		'XY-scan-X-scale-text': begin
			widget_control, (*pstate).XY_X_size_text, get_value=s
			if gnumeric(s) eq 0 then goto, bad
			(*(*pstate).p).XY_scan.X = float(s)
			end

		'XY-scan-Y-scale-text': begin
			widget_control, (*pstate).XY_Y_size_text, get_value=s
			if gnumeric(s) eq 0 then goto, bad
			(*(*pstate).p).XY_scan.Y = float(s)
			end

		'X-step-Y-scale-text': begin
			widget_control, (*pstate).Xstep_Y_size_text, get_value=s
			if gnumeric(s) eq 0 then goto, bad
			(*(*pstate).p).X_step.Y = float(s)
			end

		'Y-step-X-scale-text': begin
			widget_control, (*pstate).Ystep_X_size_text, get_value=s
			if gnumeric(s) eq 0 then goto, bad
			(*(*pstate).p).Y_step.X = float(s)
			end

		'X-stepper-text': begin
			widget_control, (*pstate).Xstep_microns_text, get_value=s
			if gnumeric(s) eq 0 then goto, bad
			(*(*pstate).p).resolution.X = float(s)
			end

		'Y-stepper-text': begin
			widget_control, (*pstate).Ystep_microns_text, get_value=s
			if gnumeric(s) eq 0 then goto, bad
			(*(*pstate).p).resolution.Y = float(s)
			end

		'X-stepper-origin': begin
			widget_control, (*pstate).Xorigin_text, get_value=s
			if gnumeric(s) eq 0 then goto, bad
			(*(*pstate).p).origin.X = float(s)
			end

		'Y-stepper-origin': begin
			widget_control, (*pstate).Yorigin_text, get_value=s
			if gnumeric(s) eq 0 then goto, bad
			(*(*pstate).p).origin.Y = float(s)
			end

		'mpa3-x-adc': begin
			(*(*pstate).p).mpa3.x = event.index
			end

		'mpa3-y-adc': begin
			(*(*pstate).p).mpa3.y = event.index
			end

		'startup-options': begin
			(*(*pstate).p).startup.(event.value) = event.select
			end

		'origin-options': begin
			(*(*pstate).p).origin.auto = event.select
			widget_control, (*pstate).origin_base, sensitive = 1-(*(*pstate).p).origin.auto
			end
			
		'startup-directory-text': begin
			widget_control, (*pstate).startup_text, get_value=s
			(*(*pstate).p).directory = s
			end

		'browse-button': begin
			F = file_requester( /read, /must_exist, /dir, $
					title='select default startup path', path=(*(*pstate).p).directory, $
					group=event.top)
			if F ne '' then begin
				(*(*pstate).p).directory = F
				widget_control, (*pstate).startup_text, set_value=F
			endif
			end

		'raw-data-directory-text': begin
			widget_control, (*pstate).raw_data_text, get_value=s
			(*(*pstate).p).data_dir = s
			end

		'browse-raw-button': begin
			F = file_requester( /read, /must_exist, /dir, $
					title='select default raw data path', path=(*(*pstate).p).data_dir, $
					group=event.top)
			if F ne '' then begin
				(*(*pstate).p).data_dir = F
				widget_control, (*pstate).raw_data_text, set_value=F
			endif
			end

		'load': begin
			F = file_requester( /read, /must_exist, filter = '*.prefs', $
					title='Load Preferences from File', path=geopixe_root, $
					group=event.top, /fix_filter)
			if F ne '' then begin
				load_prefs_select, pstate, F
				prefs_select_update, pstate
			endif
			end
		'save': begin
			F = file_requester( /write, filter = '*.prefs', $
					title='Save Preferences to File', path=geopixe_root, $
					group=event.top, /fix_filter)
			if F ne '' then begin
				prefs_select_get, pstate
				save_prefs_select, pstate, F
			endif
			end

		'defaults': begin
			(*(*pstate).p).XY_scan.X = 	100.0			; XY scan maximum  range (microns)
			(*(*pstate).p).XY_scan.Y = 	100.0
			(*(*pstate).p).X_step.Y = 	2000.0			; X-step maximum Y range (microns)
			(*(*pstate).p).Y_step.X = 	640.0			; Y-step maximum X range (microns)
			(*(*pstate).p).Resolution.X=	0.635		; X stepper resolution (microns)
			(*(*pstate).p).Resolution.Y= 0.0833333		; Y stepper resolution
			(*(*pstate).p).mpa3.X = 0					; X ADC with mpa3 list-mode
			(*(*pstate).p).mpa3.Y = 2					; Y ADC with mpa3 list-mode
			(*(*pstate).p).startup.spectrum = 1			; Startup options
			(*(*pstate).p).startup.identify = 0
			(*(*pstate).p).startup.image_clone = 0
			(*(*pstate).p).startup.sort_EVT = 1
			(*(*pstate).p).startup.PIXE_Fit = 1
			(*(*pstate).p).startup.Regions = 0
			(*(*pstate).p).directory = ''				; startup-directory
			(*(*pstate).p).data_dir = ''				; raw data directory
			(*(*pstate).p).error = 0

			prefs_select_update, pstate
			end

		'cancel': begin
			(*(*pstate).p).error = 1
			widget_control, event.top, /destroy
			return
			end
		'ok': begin
			prefs_select_get, pstate, error=error
			if error then goto, finish
			widget_control, event.top, /destroy
			return
			end
		else:
	endcase

finish:
	return

bad:
	warning,'prefs_select_event',['Illegal numeric value encountered.','Re-enter numeric value.']
	goto, finish
end

;------------------------------------------------------------------------------------------

pro prefs_select_get, pstate, error=error

	error = 0
	if ptr_valid( (*pstate).p) eq 0 then return

	widget_control, (*pstate).XY_X_size_text, get_value=s
	if gnumeric(s) then begin
		(*(*pstate).p).XY_scan.X = float(s)
	endif else begin
		warning,'prefs_select_get',['Illegal text in value for XY scan X size','Re-enter numeric value.']
		error = 1
	endelse
	widget_control, (*pstate).XY_Y_size_text, get_value=s
	if gnumeric(s) then begin
		(*(*pstate).p).XY_scan.Y = float(s)
	endif else begin
		warning,'prefs_select_get',['Illegal text in value for XY scan Y size','Re-enter numeric value.']
		error = 1
	endelse
	widget_control, (*pstate).Xstep_Y_size_text, get_value=s
	if gnumeric(s) then begin
		(*(*pstate).p).X_step.Y = float(s)
	endif else begin
		warning,'prefs_select_get',['Illegal text in value for X-step Y size','Re-enter numeric value.']
		error = 1
	endelse
	widget_control, (*pstate).Ystep_X_size_text, get_value=s
	if gnumeric(s) then begin
		(*(*pstate).p).Y_step.X = float(s)
	endif else begin
		warning,'prefs_select_get',['Illegal text in value for Y-step X size','Re-enter numeric value.']
		error = 1
	endelse
	widget_control, (*pstate).Xstep_microns_text, get_value=s
	if gnumeric(s) then begin
		(*(*pstate).p).resolution.X = float(s)
	endif else begin
		warning,'prefs_select_get',['Illegal text in value for X stepper resolution','Re-enter numeric value.']
		error = 1
	endelse
	widget_control, (*pstate).Ystep_microns_text, get_value=s
	if gnumeric(s) then begin
		(*(*pstate).p).resolution.Y = float(s)
	endif else begin
		warning,'prefs_select_get',['Illegal text in value for Y stepper resolution','Re-enter numeric value.']
		error = 1
	endelse
	widget_control, (*pstate).Xorigin_text, get_value=s
	if gnumeric(s) then begin
		(*(*pstate).p).origin.X = float(s)
	endif else begin
		warning,'prefs_select_get',['Illegal text in value for X origin','Re-enter numeric value.']
		error = 1
	endelse
	widget_control, (*pstate).Yorigin_text, get_value=s
	if gnumeric(s) then begin
		(*(*pstate).p).origin.Y = float(s)
	endif else begin
		warning,'prefs_select_get',['Illegal text in value for Y origin','Re-enter numeric value.']
		error = 1
	endelse
	widget_control, (*pstate).startup_text, get_value=s
	(*(*pstate).p).directory = s
	widget_control, (*pstate).raw_data_text, get_value=s
	(*(*pstate).p).data_dir = s
	return
end

;------------------------------------------------------------------------------------------

pro prefs_select_update, pstate

	if ptr_valid( (*pstate).p) eq 0 then return

	widget_control, (*pstate).XY_X_size_text, set_value=string( (*(*pstate).p).XY_scan.X)
	widget_control, (*pstate).XY_Y_size_text, set_value=string( (*(*pstate).p).XY_scan.Y)
	widget_control, (*pstate).Xstep_Y_size_text, set_value=string( (*(*pstate).p).X_step.Y)
	widget_control, (*pstate).Ystep_X_size_text, set_value=string( (*(*pstate).p).Y_step.X)
	widget_control, (*pstate).Xstep_microns_text, set_value=string( (*(*pstate).p).resolution.X)
	widget_control, (*pstate).Ystep_microns_text, set_value=string( (*(*pstate).p).resolution.Y)
	widget_control, (*pstate).mpa3_X_ADC, set_combobox_select=(*(*pstate).p).mpa3.x
	widget_control, (*pstate).mpa3_Y_ADC, set_combobox_select=(*(*pstate).p).mpa3.y
	widget_control, (*pstate).Xorigin_text, set_value=string( (*(*pstate).p).origin.X)
	widget_control, (*pstate).Yorigin_text, set_value=string( (*(*pstate).p).origin.Y)

	widget_control, (*pstate).startup_text, set_value= (*(*pstate).p).directory
	widget_control, (*pstate).raw_data_text, set_value= (*(*pstate).p).data_dir

	widget_control, (*pstate).options, set_value=[(*(*pstate).p).startup.spectrum,(*(*pstate).p).startup.identify, $
			(*(*pstate).p).startup.image_clone,(*(*pstate).p).startup.sort_evt,(*(*pstate).p).startup.pixe_fit,(*(*pstate).p).startup.regions]
	widget_control, (*pstate).origin_options, set_value=[(*(*pstate).p).origin.auto]
	widget_control, (*pstate).origin_base, sensitive = 1-(*(*pstate).p).origin.auto
	return
end

;-----------------------------------------------------------------

pro save_prefs_select, pstate, file

	file = strip_file_ext(file) + '.prefs'
	on_ioerror, bad_open
	openw, lun, file, /xdr, /get_lun

	on_ioerror, bad_io
	version = -5
	writeu,lun, version
	writeu,lun, (*(*pstate).p)

done:
	close_file, lun
	return

bad_open:
	warning,'save_prefs_select','error opening file: '+file
	goto, done
bad_io:
	warning,'save_prefs_select','error writing file: '+file
	goto, done
end

;-----------------------------------------------------------------

pro load_prefs_select, pstate, file, prefs=prefs, error=error

	error = 1
	on_ioerror, bad_open
	openr, lun, file, /xdr, /get_lun
	on_ioerror, bad_io

	valid = [-1,-2,-3,-4,-5]

	version = 0
	readu,lun, version
	q = where( version eq valid)
	if q[0] eq -1 then goto, bad_io

	prefs = {  $
				XY_scan: {X:	100.0, $		; XY scan maximum  range (microns)
						  Y:	100.0 }, $
				X_step: { Y:	2000.0	}, $	; X-step maximum Y range (microns)
				Y_step: { X:	640.0 }, $		; Y-step maximum X range (microns)
				MPA3:  {  X:	0, $			; X ADC to use with mpa3 list-mode
						  Y:	2}, $			; Y ADC to use with mpa3 list-mode
				Resolution: {X:	0.635, $		; X stepper resolution (microns)
							Y:	0.0833333 }, $	; Y stepper resolution
				Origin: { X:	0.0, $			; stage X origin (mm)
							Y:	0.0, $			; stage Y origin
							auto:	0}, $		; use origin data in DAI files not the none here
				startup: { spectrum: 1, $		; Startup options
						identify: 0, $
						image_clone: 0, $
						sort_EVT: 1, $
						PIXE_Fit: 1, $
						Regions: 0 }, $
				directory: '', $				; startup-directory
				data_dir: '', $					; raw data directory
				slave:		0, $				; image slave
				error: 0 }

	case version of
		-1: begin
			prefs1 = {  $
				XY_scan: {X:	100.0, $		; XY scan maximum  range (microns)
						  Y:	100.0 }, $
				X_step: { Y:	2000.0	}, $	; X-step maximum Y range (microns)
				Y_step: { X:	640.0 }, $		; Y-step maximum X range (microns)
				Resolution: {X:	0.635, $		; X stepper resolution (microns)
							Y:	0.0833333 }, $	; Y stepper resolution
				startup: { spectrum: 1, $		; Startup options
						identify: 0, $
						image_clone: 0, $
						sort_EVT: 1, $
						PIXE_Fit: 1, $
						Regions: 0 }, $
				directory: '', $				; startup-directory
				error: 0 }
			end
		-2: begin
			prefs1 = {  $
				XY_scan: {X:	100.0, $		; XY scan maximum  range (microns)
						  Y:	100.0 }, $
				X_step: { Y:	2000.0	}, $	; X-step maximum Y range (microns)
				Y_step: { X:	640.0 }, $		; Y-step maximum X range (microns)
				Resolution: {X:	0.635, $		; X stepper resolution (microns)
							Y:	0.0833333 }, $	; Y stepper resolution
				mpa3:  {  X:	0, $			; X ADC to use with mpa3 list-mode
						  Y:	2}, $			; Y ADC to use with mpa3 list-mode
				startup: { spectrum: 1, $		; Startup options
						identify: 0, $
						image_clone: 0, $
						sort_EVT: 1, $
						PIXE_Fit: 1, $
						Regions: 0 }, $
				directory: '', $				; startup-directory
				error: 0 }
			end
		-3: begin
			prefs1 = {  $
				XY_scan: {X:	100.0, $		; XY scan maximum  range (microns)
						  Y:	100.0 }, $
				X_step: { Y:	2000.0	}, $	; X-step maximum Y range (microns)
				Y_step: { X:	640.0 }, $		; Y-step maximum X range (microns)
				MPA3:  {  X:	0, $			; X ADC to use with mpa3 list-mode
						  Y:	2}, $			; Y ADC to use with mpa3 list-mode
				Resolution: {X:	0.635, $		; X stepper resolution (microns)
							Y:	0.0833333 }, $	; Y stepper resolution
				Origin: { X:	0.0, $			; stage X origin (mm)
						  Y:	0.0}, $			; stage Y origin
				startup: { spectrum: 1, $		; Startup options
						identify: 0, $
						image_clone: 0, $
						sort_EVT: 1, $
						PIXE_Fit: 1, $
						Regions: 0 }, $
				directory: '', $				; startup-directory
				error: 0 }
			end
		-4: begin
			prefs1 = {  $
				XY_scan: {X:	100.0, $		; XY scan maximum  range (microns)
						  Y:	100.0 }, $
				X_step: { Y:	2000.0	}, $	; X-step maximum Y range (microns)
				Y_step: { X:	640.0 }, $		; Y-step maximum X range (microns)
				MPA3:  {  X:	0, $			; X ADC to use with mpa3 list-mode
						  Y:	2}, $			; Y ADC to use with mpa3 list-mode
				Resolution: {X:	0.635, $		; X stepper resolution (microns)
							Y:	0.0833333 }, $	; Y stepper resolution
				Origin: { X:	0.0, $			; stage X origin (mm)
						  Y:	0.0}, $			; stage Y origin
				startup: { spectrum: 1, $		; Startup options
						identify: 0, $
						image_clone: 0, $
						sort_EVT: 1, $
						PIXE_Fit: 1, $
						Regions: 0 }, $
				directory: '', $				; startup-directory
				data_dir: '', $					; raw data directory
				error: 0 }
			end
		-5: begin
			prefs1 = {  $
				XY_scan: {X:	100.0, $		; XY scan maximum  range (microns)
						  Y:	100.0 }, $
				X_step: { Y:	2000.0	}, $	; X-step maximum Y range (microns)
				Y_step: { X:	640.0 }, $		; Y-step maximum X range (microns)
				MPA3:  {  X:	0, $			; X ADC to use with mpa3 list-mode
						  Y:	2}, $			; Y ADC to use with mpa3 list-mode
				Resolution: {X:	0.635, $		; X stepper resolution (microns)
							Y:	0.0833333 }, $	; Y stepper resolution
				Origin: { X:	0.0, $			; stage X origin (mm)
							Y:	0.0, $			; stage Y origin
							auto:	0}, $		; use origin data in DAI files not the none here
				startup: { spectrum: 1, $		; Startup options
						identify: 0, $
						image_clone: 0, $
						sort_EVT: 1, $
						PIXE_Fit: 1, $
						Regions: 0 }, $
				directory: '', $				; startup-directory
				data_dir: '', $					; raw data directory
				error: 0 }
			end
		else: goto, bad_io
	endcase

	readu,lun, prefs1

	prefs.xy_scan = prefs1.xy_scan
	prefs.x_step = prefs1.x_step
	prefs.y_step = prefs1.y_step
	prefs.resolution = prefs1.resolution
	prefs.startup = prefs1.startup
	prefs.directory = prefs1.directory
	
	case version of
		-2: begin
			prefs.mpa3 = prefs1.mpa3
			end
		-3: begin
			prefs.mpa3 = prefs1.mpa3
			prefs.origin.x = prefs1.origin.x
			prefs.origin.x = prefs1.origin.x
			end
		-4: begin
			prefs.mpa3 = prefs1.mpa3
			prefs.origin.x = prefs1.origin.x
			prefs.origin.x = prefs1.origin.x
			prefs.data_dir = prefs1.data_dir
			end
		-5: begin
			prefs.mpa3 = prefs1.mpa3
			prefs.origin = prefs1.origin
			prefs.data_dir = prefs1.data_dir
			end
		else:
	endcase
	
	error = 0
	if n_elements(pstate) eq 0 then goto, done
	if ptr_valid(pstate) eq 0 then goto, done
	if size(*pstate,/tname) ne 'STRUCT' then goto, done
	
	*(*pstate).p = prefs
	
done:
	close_file, lun
	return

bad_open:
	warning,'load_prefs_select','error opening file: '+file
	goto, done
bad_io:
	warning,'load_prefs_select','error reading file: '+file
	goto, done
end

;------------------------------------------------------------------------------------------

pro OnRealize_prefs_mpa3_x_adc, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).mpa3.x
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_prefs_mpa3_y_adc, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).mpa3.y
endif
end

;-----------------------------------------------------------------

function prefs_select, group, path=path, old_prefs=old_prefs, init=init, slave=slave

; Now obsolete - use "geopixe.conf" file and geopixe_defaults() call
;	Select preferences
;	Path is path for prefs files, not GeoPIXE data

common c_working_dir, geopixe_root
if n_elements(geopixe_root) lt 1 then geopixe_root=''

	if n_elements(init) lt 1 then init=0
	if init eq 0 then begin
		if n_elements(group) lt 1 then return, 0
		if widget_info( group, /valid) eq 0 then return, 0
	endif
	if n_elements(path) lt 1 then path=geopixe_root
	if n_elements(slave) lt 1 then slave=0

	prefs = {  $
			XY_scan: {X:	100.0, $		; XY scan maximum  range (microns)
					  Y:	100.0 }, $
			X_step: { Y:	2000.0	}, $	; X-step maximum Y range (microns)
			Y_step: { X:	640.0 }, $		; Y-step maximum X range (microns)
			MPA3:  {  X:	0, $			; X ADC to use with mpa3 list-mode
					  Y:	2}, $			; Y ADC to use with mpa3 list-mode
			Resolution: {X:	0.635, $		; X stepper resolution (microns)
						Y:	0.0833333 }, $	; Y stepper resolution
			Origin: { X:	0.0, $			; stage X origin (mm)
					  Y:	0.0, $			; stage Y origin
					  auto:	0}, $			; use origin data in DAI files not the none here
			startup: { spectrum: 1, $		; Startup options
					identify: 0, $
					image_clone: 0, $
					sort_EVT: 1, $
					PIXE_Fit: 1, $
					Regions: 0 }, $
			directory: '', $				; startup-directory
			data_dir: '', $					; raw data directory
			slave:		slave, $			; image started as a slave window
			error: 0 }
	if init then begin
		load_prefs_select, 0, path+'GeoPIXE.prefs', prefs=prefs2, error=error
		if error eq 0 then prefs=prefs2
		prefs.slave = slave
		return, prefs						; defaults
	endif

	if n_elements(old_prefs) gt 0 then begin
		if size(old_prefs,/tname) eq 'STRUCT' then begin
			prefs = old_prefs
		endif
	endif

	xsize = 300
	ysize = 500
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title='Preferences', /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='prefs_select_TLB', /base_align_center, $
					xpad=10, ypad=10, space=2					  ,/modal, /floating)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=15, /base_align_center)
	obase = widget_base( tbase, /row, /base_align_top, xpad=0, ypad=0, space=30)


	lbase = widget_base( obase, /column, xpad=0, ypad=0, space=2, /base_align_right)
	lab = widget_label( lbase, value='Full Scale Scan Dimensions')

	l1base = widget_base( lbase, /row, /base_align_right, xpad=0, ypad=0, space=5)
	lab = widget_label( l1base, value='XY scan X size:')
	text1 = widget_text( l1base, value=string(prefs.XY_scan.X), uname='XY-scan-X-scale-text', /tracking, /editable, $
					uvalue='Enter the maximum size of the X scan (microns), when in XY scan mode. ' + $
					'Defines scan X size for maximum "scale=1.00" in Mpsys MP file.', scr_xsize=70)

	l2base = widget_base( lbase, /row, /base_align_right, xpad=0, ypad=0, space=5)
	lab = widget_label( l2base, value='XY scan Y size:')
	text2 = widget_text( l2base, value=string(prefs.XY_scan.Y), uname='XY-scan-Y-scale-text', /tracking, /editable, $
					uvalue='Enter the maximum size of the Y scan (microns), when in XY scan mode. ' + $
					'Defines scan Y size for maximum "scale=1.00" in Mpsys MP file.', $
					scr_xsize=70)

	l3base = widget_base( lbase, /row, /base_align_right, xpad=0, ypad=0, space=5)
	lab = widget_label( l3base, value='X-step Y size:')
	text3 = widget_text( l3base, value=string(prefs.X_step.Y), uname='X-step-Y-scale-text', /tracking, /editable, $
					uvalue='Enter the maximum size of the Y scan (microns), when in "Stage stepping in X" mode.', $
					scr_xsize=70)

	l4base = widget_base( lbase, /row, /base_align_right, xpad=0, ypad=0, space=5)
	lab = widget_label( l4base, value='Y-step X size:')
	text4 = widget_text( l4base, value=string(prefs.Y_step.X), uname='Y-step-X-scale-text', /tracking, /editable, $
					uvalue='Enter the maximum size of the X scan (microns), when in "Stage stepping in Y" mode.', $
					scr_xsize=70)

	l5base = widget_base( lbase, /row, /base_align_right, xpad=0, ypad=0, space=5)
	lab = widget_label( l5base, value='X stepper resolution:')
	text5 = widget_text( l5base, value=string(prefs.resolution.X), uname='X-stepper-text', /tracking, /editable, $
					uvalue='Enter the step distance (microns) for one step of the X stage stepper for "Stage stepping in X" mode.', $
					scr_xsize=70)

	l6base = widget_base( lbase, /row, /base_align_right, xpad=0, ypad=0, space=5)
	lab = widget_label( l6base, value='Y stepper resolution:')
	text6 = widget_text( l6base, value=string(prefs.resolution.Y), uname='Y-stepper-text', /tracking, /editable, $
					uvalue='Enter the step distance (microns) for one step of the Y stage stepper for "Stage stepping in Y" mode.', $
					scr_xsize=70)

	origin_base = widget_base( lbase, /column, /base_align_right, xpad=0, ypad=0, space=2, sensitive=1-prefs.origin.auto)
	l61base = widget_base( origin_base, /row, /base_align_right, xpad=0, ypad=0, space=5)
	lab = widget_label( l61base, value='X absolute origin:')
	text61 = widget_text( l61base, value=string(prefs.origin.x), uname='X-stepper-origin', /tracking, /editable, $
					uvalue='Enter the physical origin (mm) for the X stage. This will be added to image coordinates for box selections', $
					scr_xsize=70)

	l62base = widget_base( origin_base, /row, /base_align_right, xpad=0, ypad=0, space=5)
	lab = widget_label( l62base, value='Y absolute origin:')
	text62 = widget_text( l62base, value=string(prefs.origin.y), uname='Y-stepper-origin', /tracking, /editable, $
					uvalue='Enter the physical origin (mm) for the Y stage. This will be added to image coordinates for box selections', $
					scr_xsize=70)

	origin_options = cw_bgroup2( lbase, ['Auto origin from DAI file'], /column, $
					xpad=0, ypad=0, space=0, /return_index, /tracking, $
					uname='origin-options', set_value=[prefs.origin.auto], /nonexclusive, $
					uvalue=['Set this option to make use of origin data saved in DAI files (acquired AFTER April 25, 2010).'])

	l7base = widget_base( lbase, /row, /base_align_right, xpad=0, ypad=0, space=5)
	lab = widget_label( l7base, value='Default X ADC:')
	mpa3_x_adc = widget_combobox( l7base, value=strtrim(string(indgen(16)+1),2), uname='mpa3-x-adc', /tracking, /align_right, $
					notify_realize='OnRealize_prefs_mpa3_x_adc', $
					uvalue='Select the ADC that contains X data when sorting certain list-mode files.', xsize=70)

	l8base = widget_base( lbase, /row, /base_align_right, xpad=0, ypad=0, space=5)
	lab = widget_label( l8base, value='Default Y ADC:')
	mpa3_y_adc = widget_combobox( l8base, value=strtrim(string(indgen(16)+1),2), uname='mpa3-y-adc', /tracking, /align_right, $
					notify_realize='OnRealize_prefs_mpa3_y_adc', $
					uvalue='Select the ADC that contains Y data when sorting certain list-mode files.', xsize=70)


	rbase = widget_base( obase, /column, xpad=0, ypad=0, space=2, /base_align_left)
	lab = widget_label( rbase, value='Window Start-up Options')

	options = cw_bgroup2( rbase, ['Spectrum Display','Line Identify','Image Clone','Sort EVT','PIXE/SXRF Fit','Image Regions'], /column, $
					xpad=0, ypad=0, space=0, /return_index, /tracking, $
					uname='startup-options', set_value=[prefs.startup.spectrum, prefs.startup.identify, prefs.startup.image_clone, $
						prefs.startup.sort_evt, prefs.startup.pixe_fit, prefs.startup.regions], /nonexclusive, $
					uvalue=['Open a Spectrum Display window on GeoPIXE start-up.','Open a Line Identification window on GeoPIXE start-up.', $
					'Open a Image Clone window on GeoPIXE start-up.','Open a Sort EVT window on GeoPIXE start-up.', $
					'Open a PIXE/SXRF Fit window on GeoPIXE start-up.','Open a Image Regions window on GeoPIXE start-up.'])


	sdbase = widget_base( tbase, /row, /base_align_right, /align_right, xpad=0, ypad=0, space=5)
	lab = widget_label( sdbase, value='Start-up Directory:')
	sdtext = widget_text( sdbase, value=prefs.directory, uname='startup-directory-text', /tracking, /editable, $
					uvalue='Enter the initial default directory path for output from GeoPIXE.', $
					scr_xsize=180)
	button = widget_button( sdbase, xsize=50, value='Browse', uname='browse-button',/tracking,uvalue='Browse for the initial default directory path for output from GeoPIXE.')


	sd2base = widget_base( tbase, /row, /base_align_right, /align_right, xpad=0, ypad=0, space=5)
	lab = widget_label( sd2base, value='Raw Data Directory:')
	sd2text = widget_text( sd2base, value=prefs.data_dir, uname='raw-data-directory-text', /tracking, /editable, $
					uvalue='Enter the initial default directory path for raw data input into GeoPIXE.', $
					scr_xsize=180)
	button = widget_button( sd2base, xsize=50, value='Browse', uname='browse-raw-button',/tracking,uvalue='Browse for the initial default directory path for raw data input into GeoPIXE.')


	bbase = widget_base( tbase, /row, /base_align_center, /align_center, ypad=1, space=2)
	button = widget_button( bbase, xsize=36, value='Load', uname='load',/tracking,uvalue='Load preference parameters from a PREFS file.')
	button = widget_button( bbase, xsize=36, value='Save', uname='save',/tracking,uvalue='Save preference parameters to a PREFS file.')
	lab = widget_label( bbase, value=' ')
	button = widget_button( bbase, xsize=48, value='Defaults', uname='defaults',/tracking,uvalue='Set preference parameters to the defaults.')
	lab = widget_label( bbase, value=' ')
	button = widget_button( bbase, xsize=43, value='Cancel', uname='cancel',/tracking,uvalue='Cancel preferences and return.')
	button = widget_button( bbase, xsize=27, value='OK', uname='ok',/tracking,uvalue='Use the selected preferences. Remember to SAVE the preferences for them to apply the next time GeoPIXE is run.')

	help = widget_text( tlb, scr_xsize=330, ysize=3, /wrap, uname='help', /tracking, $
				uvalue='Help window. Displays info about widgets.',frame=0)

	p = ptr_new( prefs )			; current prefs

	state = {	$
			path:				path, $					; pointer to current path

			XY_X_size_text:		text1, $				; widget ID of XY scan X size text box
			XY_Y_size_text:		text2, $				; widget ID of XY scan Y size text box
			Xstep_Y_size_text:	text3, $				; widget ID of X-step scan Y size text box
			Ystep_X_size_text:	text4, $				; widget ID of Y-step scan X size text box
			Xstep_microns_text:	text5, $				; widget ID of X-step resolution text box
			Ystep_microns_text:	text6, $				; widget ID of Y-step resolution text box
			Xorigin_text:		text61, $				; widget ID of X stage origin text box
			Yorigin_text:		text62, $				; widget ID of Y stage origin text box
			mpa3_X_ADC:			mpa3_x_adc, $			; widget ID of mpa3 X ADC droplist
			mpa3_Y_ADC:			mpa3_y_adc, $			; widget ID of mpa3 Y ADC droplist
			origin_base:		origin_base, $			; ID of origin base
			origin_options:		origin_options, $		; ID of origin auto options

			startup_text:		sdtext, $				; widget ID of startup-directory text box
			raw_data_text:		sd2text, $				; widget ID of raw data directory text box

			options:			options, $				; widget ID of options CW
			help:				help, $					; widget ID of help text

			p:					p $						; pointer to selection
		}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	xmanager, 'prefs_select', tlb				;	, /no_block

	r = *p
	return, r
end
