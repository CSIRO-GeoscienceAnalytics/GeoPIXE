;
;	select = import_select( group, title=title, /debug, /device_only)
;
;	Select source lab (device) and file format from lists, and return the OPT
;	struct for spectrum_load.

;	select		returns the OPT struct for spectrum_load
;				and the device name for

;	group			group leader (optional)
;	title			title string (optional)
;	/device_only 	select the device only for XANES energies import
;	/debug			debug mode, not modal for testing, but returns straight away.

;----------------------------------------------------------------------

pro import_select_event, Event

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
		warning,'import_select_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

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
				widget_control, (*pstate).help, set_value='Select data class and file format in the lists above.'
			endelse
			return
			end
		'WIDGET_TIMER': begin
	;		print,' got a timer event; update text reads ...'
			return
			end
		'WIDGET_KILL_REQUEST': begin
			widget_control, event.top, /destroy
			return
			end
		else:
	endcase

	uname = widget_info( event.id, /uname)
	case uname of

		'type-mode': begin
			(*pstate).type = event.value
			type_select = (*pstate).type
			q = where( ((*pstate).labs.type eq type_select) or ((*pstate).labs.type eq 9)) > 0
			lab_select = (*pstate).labs[q[0]].lab
			(*pstate).lab = lab_select
			labs = (*pstate).labs[q].title
			(*(*pstate).p).device = (*pstate).labs[q[0]].name
			uval = {labs:(*pstate).labs[q], help:'Select the lab or data file class'}
			widget_control, (*pstate).lab_list, set_value=labs, set_uvalue=uval

			q = where( (*pstate).formats.lab eq lab_select) > 0
			format_select = (*pstate).formats[q[0]].opt
			(*(*pstate).p).opt = format_select
			formats = (*pstate).formats[q].title
			uval = {formats:(*pstate).formats[q], help:'Select file format for the selected lab or data class.'}
			if widget_info( (*pstate).format_list, /valid) then begin
				widget_control, (*pstate).format_list, set_value=formats, set_uvalue=uval
			endif
			end

		'import_select_lab_list': begin
			widget_control, (*pstate).lab_list, get_uvalue=u
			labs = u.labs
			lab_select = labs[event.index].lab
			(*pstate).lab = lab_select
			(*(*pstate).p).device = labs[event.index].name
			q = where( (*pstate).formats.lab eq lab_select, nq)
			if nq eq 0 then begin
				format_select = (*pstate).def_format.opt
				formats = (*pstate).def_format.title
				uval = {formats:(*pstate).def_format, help:'No import options for this device class, so selection will default to native GeoPIXE spectrum read.'}
			endif else begin
				format_select = (*pstate).formats[q[0]].opt
				formats = (*pstate).formats[q].title
				uval = {formats:(*pstate).formats[q], help:'Select file format for the selected lab or data class.'}
			endelse
			(*(*pstate).p).opt = format_select
			if widget_info( (*pstate).format_list, /valid) then begin
				widget_control, (*pstate).format_list, set_value=formats, set_uvalue=uval
			endif
			end

		'import_select_format_list': begin
			widget_control, (*pstate).format_list, get_uvalue=u
			formats = u.formats
			format_select = formats[event.index].opt
			(*(*pstate).p).opt = format_select
			end

		'append': begin
			case event.value of
				0: begin									; append check-box
					(*(*pstate).p).append = event.select
					end
				1: begin									; generate GCF check-box
					(*(*pstate).p).generate = event.select
					end
				else:
			endcase
			end

		'ok': begin
			(*(*pstate).p).error = 0
			widget_control, event.top, /destroy
			if (*pstate).local then widget_control, (*pstate).group, /destroy
			return
			end

		'cancel': begin
			(*(*pstate).p).error = 1
			widget_control, event.top, /destroy
			if (*pstate).local then widget_control, (*pstate).group, /destroy
			return
			end
		else:
	endcase

	return
end

;--------------------------------------------------------------------------

pro OnRealize_import_select_device, wWidget

COMPILE_OPT STRICTARR

	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate
	widget_control, wWidget, get_uvalue=u
	labs = u.labs

	q1 = where( labs.name eq (*pstate).device_name, nq1)
	if nq1 gt 0 then begin
		widget_control, wWidget, set_list_select=q1[0]

		lab_select = labs[q1[0]].lab
		(*pstate).lab = lab_select
	endif
	widget_control, (*pstate).toggle, set_value=(*pstate).type
	return
end

;--------------------------------------------------------------------------

pro OnRealize_import_select_format, wWidget

COMPILE_OPT STRICTARR

	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, get_uvalue=pstate

	q2 = where( (*pstate).formats.lab eq (*pstate).lab, nq2)
	if nq2 eq 0 then begin
		format_select = (*pstate).def_format.opt
		formats = (*pstate).def_format.title
		uval = {formats:(*pstate).def_format, help:'No import options for this device class, so selection will default to native GeoPIXE spectrum read.'}
	endif else begin
		format_select = (*pstate).formats[q2[0]].opt
		formats = (*pstate).formats[q2].title
		uval = {formats:(*pstate).formats[q2], help:'Select file format for the selected lab or data class.'}
	endelse
	(*(*pstate).p).opt = format_select
	widget_control, wWidget, set_value=formats, set_uvalue=uval
	widget_control, wWidget, set_list_select=0
	return
end

;-----------------------------------------------------------------

function import_select, group, debug=debug, title=title, device_only=device_only

;	Select source lab (device) and file format from lists, and return the OPT
;	struct for spectrum_load.
;	
;	group			group leader (optional)
;	title			title string (optional)
;	/device_only 	select the device only for XANES energies import
;	/debug			debug mode, not modal for testing, but returns straight away.

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
		warning,'import_select',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, {error:1}
	endif
endif
common c_import_select_1, p_import_select

	if n_elements(title) lt 1 then title='Import Spectrum File(s)'
	if n_elements(device_only) lt 1 then device_only=0
	if n_elements(debug) lt 1 then debug=0
	if n_elements(group) lt 1 then group=0L
	if widget_info(group,/valid) eq 0 then begin
		group = widget_base( scr_xsize=1, scr_ysize=1)
		local = 1
	endif else local=0
	if debug then begin
		modal = 0
		floating = 0
	endif else begin
		modal = 1
		floating = 1
	endelse
	
	device_name = ''
	geopixe = geopixe_defaults( error=error, source='import_select')
	if error eq 0 then device_name = geopixe.default.device
	
	define_devices
	opt = list_device_imports()
	name = list_device_objects( title=titles, beam_type=beam_type)
	
	if n_elements(name) eq 0 then return, 0

	;	lab		index to lab/device
	;	type	type of lab 	(0: NMP, 1: SXRF, 9: misc)
	
	first = 1
	n_obj = n_elements(name)
	for i=0L,n_obj-1 do begin									; lab, device list
		type = (beam_type[i] - 1) > 0
		if type eq 2 then type=9
		item = { name: name[i], title: titles[i], lab:i+1, type:type}
		if first then begin
			lab = item
			first = 0
		endif else begin
			lab = [lab, item]
		endelse
	endfor
	gobj = obj_new('GENERIC_DEVICE')
	type = (gobj->beam_type() - 1) > 0			; add generic device
	if type eq 2 then type=9
	item = { name:'GENERIC_DEVICE', title: gobj->title(), lab:n_obj+1, type:type}
	if first then begin
		lab = item
		first = 0
	endif else begin
		lab = [lab, item]
	endelse

	first = 1
	for i=0L,n_elements(opt)-1 do begin			; import select list
		q = where( opt[i].device_name eq lab.name, nq)
		if nq gt 0 then begin
			item = { title: opt[i].title, lab:q[0]+1, opt:opt[i]}
			if first then begin
				format = item
				first = 0
			endif else begin
				format = [format, item]
			endelse
		endif
	endfor
	def_format = { title: '--- none found ---', lab:n_elements(name)+1, opt:0L }
	
	if ptr_good(p_import_select) then begin
		p = p_import_select
		
		device_name = (*p).device
		q = where( lab.name eq device_name, nq)
		type_select = lab[q[0]].type
		if type_select eq 9 then type_select = 0
		
		q = where( (lab.type eq type_select) or (lab.type eq 9)) > 0
		lab_select = lab[q[0]].lab
		labs = lab[q].title
		uval_dev = {labs:lab[q], help:'Select the source data device class'}

		q = where( format.lab eq lab_select, nq)
		if nq eq 0 then begin
			format_select = def_format.opt
			formats = def_format.title
			uval_list = {formats:def_format, help:'No import options for this device class, so selection will default to native GeoPIXE spectrum read.'}
		endif else begin
			format_select = format[q[0]].opt
			formats = format[q].title
			uval_list = {formats:format[q], help:'Select file format for the selected device or data class.'}
		endelse
	endif else begin
		q = where( lab.name eq device_name, nq)
		type_select = lab[q[0]].type
		if type_select eq 9 then type_select = 0
	
		q = where( (lab.type eq type_select) or (lab.type eq 9)) > 0
		lab_select = lab[q[0]].lab
		labs = lab[q].title
		uval_dev = {labs:lab[q], help:'Select the source data device class'}
	
		q = where( format.lab eq lab_select, nq)
		if nq eq 0 then begin
			format_select = def_format.opt
			formats = def_format.title
			uval_list = {formats:def_format, help:'No import options for this device class, so selection will default to native GeoPIXE spectrum read.'}
		endif else begin
			format_select = format[q[0]].opt
			formats = format[q].title
			uval_list = {formats:format[q], help:'Select file format for the selected device or data class.'}
		endelse

		p = ptr_new( {	opt:		format_select, $	; format index selection
						device:		device_name, $		; device name
						append:		0, $				; request append
						generate:	0, $				; generate a GeoPIXE Command File
						error:		0  } )				; error return
		p_import_select = p
	endelse

	xsize = 300
	ysize = 500
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='import_select_TLB', /base_align_center, $
					modal=modal, floating=floating, $
					xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center)

	toggle = cw_bgroup2( tbase, ['Particle Beam (PIXE,SEM)','Synchrotron (XRF)'], /row, ypad=0, $
					/exclusive, /no_release, /return_index, /tracking, $
					uname='type-mode', set_value=0, uvalue='Select type of data source between PIXE/SEM and SXRF.')

	label = widget_label( tbase, value='1.  Data Source or Device Class')
	Lab_List = Widget_List(tbase, UNAME='import_select_lab_list',  value = labs, /tracking, uvalue = uval_dev, $
				NOTIFY_REALIZE='OnRealize_import_select_device', scr_xsize=320 ,scr_ysize=240)

	if device_only eq 0 then begin
		label = widget_label( tbase, value='2.  Select File Type or Operation')
		Format_List = Widget_List(tbase, UNAME='import_select_format_list', value = Formats, /tracking, uvalue=uval_list, $
				NOTIFY_REALIZE='OnRealize_import_select_format', scr_xsize=320 ,scr_ysize=120)
	
;		append = cw_bgroup2( tbase, ['Append spectra','Generate Command File'], /row, set_value=[0,0], sensitive=1, $
;							uvalue=['Append loaded spectra to existing spectra in Spectrum Display.','Generate a GeoPIXE Command File.'], ypad=0, $

		append = cw_bgroup2( tbase, ['Append spectra'], /row, set_value=[0], sensitive=1, $
							uvalue='Append loaded spectra to existing spectra in Spectrum Display.', ypad=0, $
							/return_index, uname='append',/ nonexclusive, /tracking)
	endif else begin
		Format_List = 0L
	endelse
	
	bbase = widget_base( tbase, /row, /base_align_center, /align_center, ypad=1, space=2)
	button = widget_button( bbase, value='Cancel', uname='cancel',/tracking, uvalue='Cancel load.')
	label = widget_label( bbase, value='', scr_xsize=30)
	button = widget_button( bbase, value='OK', uname='ok',/tracking, uvalue='Load spectrum file(s) with these selected attributes.')

	help = widget_text( tbase, scr_xsize=320, ysize=2, /wrap, uname='HELP', /tracking, $
				uvalue='Help window - displays information about widgets. Move cursor over each widget to get help.',frame=0)

	state = {	$
			labs:			lab, $					; lab struct
			formats:		format, $				; format struct
			def_format:		def_format, $			; fallback format
			device_name: 	device_name, $			; device name to pre-select
			p:				p, $					; selection
			type:			type_select, $			; data type selection
			lab:			lab_select, $			; lab selection
			group:			group, $				; group leader
			local:			local, $				; local group leader
			help:			help, $					; context help widget ID
			toggle:			toggle, $				; source widget ID
			lab_list:		lab_list, $				; lab list widget ID
			format_list:	format_list $			; format list widget ID
		}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	if debug then begin
		xmanager, 'import_select', tlb, /no_block
	endif else begin
		xmanager, 'import_select', tlb
	endelse

	r = *p			; could not pass *p back. It became "undefined" under IDL 8.0
	return, r
end
