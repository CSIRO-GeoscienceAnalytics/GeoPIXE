;
; Modal pop-up window to confirm flux selections.
;----------------------------------------------------------------------

pro generic_flux_select_event, Event

COMPILE_OPT STRICTARR
common c_generic_flux_select_1, last_detector, last_num, last_unit, last_time

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	uname = widget_info( event.id, /uname)

	case tag_names( event,/structure) of
		'WIDGET_TRACKING': begin
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
		else:
	endcase

	case uname of
		'generic_flux_select_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				widget_control, event.top, /destroy
				return
			endif
			end
		'detector-mode': begin
			(*(*pstate).p).flux = event.index
			end
		'num-mode': begin
			(*(*pstate).p).sense_num = event.index
			end
		'unit-mode': begin
			(*(*pstate).p).sense_unit = event.index
			end
		'time-mode': begin
			(*(*pstate).p).live_time = event.index
			end
		'dwell': begin
			widget_control, (*pstate).dwell, get_value=s
			(*(*pstate).p).dwell = float(s)
			print,'set dwell = ',float(s)
			end
		'xrange': begin
			widget_control, (*pstate).xrange, get_value=s
			(*(*pstate).p).xrange = float(s)
			end
		'yrange': begin
			widget_control, (*pstate).yrange, get_value=s
			(*(*pstate).p).yrange = float(s)
			end

;		'up': begin
;			(*(*pstate).p).charge = (*(*pstate).p).charge * 10.
;			widget_control, (*pstate).charge_widget, set_value=string((*(*pstate).p).charge)
;			end
;		'down': begin
;			(*(*pstate).p).charge = (*(*pstate).p).charge / 10.
;			widget_control, (*pstate).charge_widget, set_value=string((*(*pstate).p).charge)
;			end

		'ok': begin
			if widget_info((*pstate).dwell,/valid) then begin
				widget_control, (*pstate).dwell, get_value=s
				(*(*pstate).p).dwell = float(s)
			endif
			if widget_info((*pstate).xrange,/valid) then begin
				widget_control, (*pstate).xrange, get_value=s
				(*(*pstate).p).xrange = fix(s)
			endif
			if widget_info((*pstate).yrange,/valid) then begin
				widget_control, (*pstate).yrange, get_value=s
				(*(*pstate).p).yrange = fix(s)
			endif
			last_detector = (*pstate).detector[ (*(*pstate).p).flux ]
			last_num = (*pstate).num[ (*(*pstate).p).sense_num ]
			last_unit = (*pstate).unit[ (*(*pstate).p).sense_unit ]
			last_time = (*pstate).time[ (*(*pstate).p).live_time ]
			(*(*pstate).p).error = 0
			widget_control, event.top, /destroy
			return
			end
		'cancel': begin
			(*(*pstate).p).error = 1
			widget_control, event.top, /destroy
			return
			end
		else:
	endcase

finish:
	return
end

;-----------------------------------------------------------------

pro OnRealize_generic_flux_select_detector, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).flux
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_generic_flux_select_num, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).sense_num
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_generic_flux_select_unit, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).sense_unit
endif
end

;------------------------------------------------------------------------------------------

function generic_flux_select, group, detector, num, unit, time, $
			dwell=dwell, dead=dead, livetime_title=livetime_title, error=error

;	Select IC counter sensitivity and units.
;	dwell		only show dwell time if an argument is present
;	dead		0=no 1=show livetime/deadtime droplist
;	livetime_title title to show against livetime/deadtime droplist

COMPILE_OPT STRICTARR
common c_generic_flux_select_1, last_detector, last_num, last_unit, last_time
if n_elements(last_detector) lt 1 then last_detector=''
if n_elements(last_num) lt 1 then last_num=''
if n_elements(last_unit) lt 1 then last_unit=''
if n_elements(last_time) lt 1 then last_time=''

	error = 1
	if n_elements(group) lt 1 then return, 0
	if n_elements(detector) lt 1 then return, 0
	if n_elements(num) lt 1 then return, 0
	if n_elements(unit) lt 1 then return, 0
	if n_elements(time) lt 1 then return, 0
	if n_elements(dead) lt 1 then dead=1
	if n_elements(range) lt 1 then range=0
	if n_elements(livetime_title) lt 1 then livetime_title='Select Live Time:'
	detector = replace('	',' ',detector)
	
	idetector = 0
	inum = 0
	iunit = 0
	itime = 0
	q = where(detector eq last_detector, nq)
	if nq gt 0 then idetector=q[0]
	q = where(num eq last_num, nq)
	if nq gt 0 then inum=q[0]
	q = where(unit eq last_unit, nq)
	if nq gt 0 then iunit=q[0]
	q = where(time eq last_time, nq)
	if nq gt 0 then itime=q[0]

	xsize = 120
	ysize = 80
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title='Select IC Scaler and Sensitivity', /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='generic_flux_select_TLB', /base_align_center, $
					/modal, /floating, xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_right)

	s1base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s1base, value='Select IC Scaler channel:')
	detector_mode = widget_combobox( s1base, value=detector, uname='detector-mode', /tracking, $
					notify_realize='OnRealize_generic_flux_select_detector', $
					uvalue='Select the scaler PV used to record upstream ion-counter.',scr_xsize=210)

	s2base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s2base, value='Select Preamp Sensitivity Set:')
	num_mode = widget_combobox( s2base, value='   '+str_tidy(num), uname='num-mode', /tracking, scr_xsize=180, $
					notify_realize='OnRealize_generic_flux_select_num', $
					uvalue="Select the PV that holds the ion chamber preamp sensitivity, if it's available. Otherwise, set preamp sensitivity.")

	s3base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s3base, value='Select Preamp Units:')
	unit_mode = widget_combobox( s3base, value='   '+unit, uname='unit-mode', /tracking, scr_xsize=180, $
					notify_realize='OnRealize_generic_flux_select_unit', $
					uvalue="Select the PV that holds the ion chamber preamp units, if it's available. Otherwise, set preamp units.")

	if dead then begin
		s4base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
		lab = widget_label( s4base, value=livetime_title)
		time_mode = widget_combobox( s4base, value=time, uname='time-mode', scr_xsize=210, /tracking, $
					uvalue='Select the PV used for Live-time or Dead-time correction.')
	endif else time_mode=0L
	
	if n_elements(dwell) ge 1 then begin
		s5base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
		lab = widget_label( s5base, value='Enter dwell time (ms):')
		dwell_widget = widget_text( s5base, uname='dwell', /editable, value=string(dwell), $
						uvalue='Enter the dwell time per pixel (ms). This is for converting count-rate to counts per pixel.', $
						scr_xsize=100, /tracking)
	endif else begin
		dwell_widget = 0L
		dwell = 1.0
	endelse
	
	if range then begin
		s6base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
		lab = widget_label( s6base, value='Enter X range:')
		xrange_widget = widget_text( s6base, uname='xrange', /editable, value='0', $
						uvalue='Enter the X pixel range. This is for gating flux to only within valid image pixel range.', $
						scr_xsize=100, /tracking)
		s7base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
		lab = widget_label( s7base, value='Enter Y range:')
		yrange_widget = widget_text( s7base, uname='yrange', /editable, value='0', $
						uvalue='Enter the Y pixel range. This is for gating flux to only within valid image pixel range.', $
						scr_xsize=100, /tracking)
	endif else begin
		xrange_widget = 0L
		yrange_widget = 0L
	endelse
	
	bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=5, /base_align_center)
;	button = widget_button( bbase, value='x10', uname='up')
;	button = widget_button( bbase, value='/10', uname='down')
	button = widget_button( bbase, value='Cancel', uname='cancel')
	lab = widget_label( bbase, value='     ')
	button = widget_button( bbase, value='OK', uname='ok')

	help = widget_text( tbase, scr_xsize=350, ysize=4, /wrap, uname='HELP', /tracking, $
				uvalue='Help window. Displays info about widgets.',frame=0)


	p = ptr_new( {flux:idetector, sense_num:inum, sense_unit:iunit, live_time:itime, $
					dwell:dwell, xrange:0, yrange:0, error:1})

	state = {	$
				p:				p, $						; pointer to selection
				detector:		detector, $					; list of detector PVs
				num:			num, $						; list of nums or num sense PVs
				unit:			unit, $						; list of units or sense unit PVs
				time:			time, $						; list of time PVs
				detector_mode:	detector_mode, $			; widget ID of detector droplist
				num_mode:		num_mode, $					; widget ID of num droplist
				time_mode:		time_mode, $				; widget ID of time droplist
				dwell:			dwell_widget, $				; widget ID of dwell text box
				xrange:			xrange_widget, $			; optional X range widget
				yrange:			yrange_widget, $			; optional Y range widget
				unit_mode:		unit_mode, $				; widget ID of unit droplist
				help:			help $						; help widget ID
			}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	xmanager, 'generic_flux_select', tlb, no_block=0

	error = (*p).error
	if error eq 0 then dwell=(*p).dwell
	r = *p
	return, r
end
