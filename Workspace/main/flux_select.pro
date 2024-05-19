;
;	Flux_select, like Flux panel on EVT
;
pro flux_select_event, event

COMPILE_OPT STRICTARR
common aps_4, aps_count_to_charge

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
		warning,'flux_select_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
p = (*pstate).p
if ptr_valid(p) eq 0 then goto, bad_ptr
if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr

; Maia specific features
cluster_on = (*p).obj->cluster()

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
	'WIDGET_TIMER': begin
	;	print,' got a timer event; update text reads ...'
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request evt ...'
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'charge-mode': begin
		(*p).charge_mode = event.index
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
		end
		
	'charge-conversion': begin
		widget_control, event.id, get_value=s
		(*p).charge_conversion = float2(s)
		aps_count_to_charge = (*p).charge_conversion 
		end
		
	'charge-scan': begin
		dpath = *(*pstate).dpath
		file = find_file2( (*p).evt_file)
		if lenchr(file[0]) eq 0 then goto, finish
		
		image_mode = 1
		flux_scan, (*p).obj,(*p).evt_file, PV_list, IC_name, IC_val, IC_vunit, dwell=dwell, $
					image_mode=image_mode, error=error, group=event.top, no_pv=no_pv, use_dwell=use_dwell
		if IC_name eq '' then goto, finish			; cancelled pop-up
		if no_pv then begin
			(*p).charge_mode = 2
			widget_control, (*pstate).charge_mode, set_combobox_select=2
		endif
		widget_control, (*pstate).ic_base, map=((*p).charge_mode ne 0 )
		widget_control, (*pstate).scan_button, sensitive=((*p).charge_mode eq 1 )
		if (*p).charge_mode eq 1 then begin
			widget_control, (*pstate).ic_base1, map=1, scr_ysize=(*pstate).ic_base1_ysize
		endif else begin
			widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
		endelse
		if (*p).charge_mode ne 0 then begin
			widget_control, (*pstate).ic_base2, scr_ysize=(*pstate).ic_base2_ysize
		endif else begin
			widget_control, (*pstate).ic_base2, scr_ysize=1
		endelse
		(*p).use_dwell = use_dwell
;		if use_dwell then (*p).dwell = dwell
		if dwell gt 0.0001 then (*p).dwell = dwell
		widget_control, (*pstate).dwell_base, sensitive=use_dwell
		widget_control, (*pstate).dwell_id, set_value=str_tidy(dwell)
		if error eq 0 then begin
			if n_elements(PV_list) ne 0 then begin
				*(*p).pic_list = PV_list
			endif
			widget_control, (*pstate).ic_pv_mode, set_value=*(*p).pic_list
			(*p).preamp.pv = IC_name
			q = where( (*p).preamp.pv eq *(*p).pic_list, nq)
			widget_control, (*pstate).ic_pv_mode, set_combobox_select=(nq ne 0) ? q[0] : 0

			l = locate('time', strlowcase((*p).preamp.pv))
			(*p).preamp.val = IC_val
			(*p).preamp.unit = IC_vunit
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
		endif else begin
			warning,'evt','No Flux sensitivity data found for this data-type.'
		endelse
		end

	'ic-pv-mode': begin
		(*p).preamp.pv = (*(*p).pic_list)[event.index]
		(*p).flux = 0.0
		OK = (locate('.time',(*p).preamp.pv) lt 0)
		widget_control, (*pstate).ic_val_mode, sensitive=OK
		widget_control, (*pstate).ic_unit_mode, sensitive=OK
		end
	'ic-preamp-mode': begin
		(*p).preamp.val = (*pstate).ic_vals[event.index]
		(*p).flux = 0.0
		end
	'ic-preamp-unit-mode': begin
		(*p).preamp.unit = (*pstate).ic_vunits[event.index]
		(*p).flux = 0.0
		end
		
	'dwell': begin
		widget_control, (*pstate).dwell_id, get_value=s
		(*p).dwell = float2(s)
		(*p).flux = 0.0
		end

	'cluster': begin
		(*p).cluster = event.select
		end

	'from-spec-button': begin
		print,'From ...'
		path = *(*pstate).path
		F = file_requester( filter = '*.spec', path=path, group=event.top, $
					title='Select the source SPEC file', fix_filter=1, /within_modal )
		if F ne '' then begin
			*(*pstate).path = extract_path(F[0])
			psa = read_spec( F[0], /header, error=err)
			if err then goto, finish
			ps = psa[0]
			
			if (*ps).DevObj->name() eq (*p).obj->name() then begin
				options = (*ps).DevObj->get_options()
				(*p).obj->set_options, options
			endif

			(*p).charge_mode = (*ps).IC.mode
			(*p).charge_conversion = (*ps).IC.conversion
			(*p).use_dwell = (*ps).dwell.on
;			if (*p).use_dwell then (*p).dwell = (*ps).dwell.val
			if (*ps).dwell.val gt 0.0001 then (*p).dwell = (*ps).dwell.val
			(*p).preamp.pv = (*ps).IC.pv.name
			(*p).preamp.val = (*ps).IC.pv.val
			(*p).preamp.unit = (*ps).IC.pv.unit

			l = locate('time', strlowcase((*p).preamp.pv))
			val = (*p).preamp.val
			unit = (*p).preamp.unit
			ival = find_charge_val_unit_index( val, unit, iunit=iunit, time=(l ge 0), /write_back)
			(*p).preamp.val = val
			(*p).preamp.unit = unit

			if ptr_good((*ps).plist) then begin
				*(*p).pic_list = *(*ps).plist
				widget_control, (*pstate).ic_pv_mode, set_value=*(*p).pic_list
			endif else begin
				*(*p).pic_list = ['none']
			endelse
			widget_control, (*pstate).charge_mode, set_combobox_select=(*p).charge_mode
			widget_control, (*pstate).charge_conversion, set_value=str_tidy((*p).charge_conversion)
			widget_control, (*pstate).ic_base, map=((*p).charge_mode ne 0 )
			widget_control, (*pstate).scan_button, sensitive=((*p).charge_mode eq 1 )
			if (*p).charge_mode eq 1 then begin
				widget_control, (*pstate).ic_base1, map=1, scr_ysize=(*pstate).ic_base1_ysize
			endif else begin
				widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
			endelse
			if (*p).charge_mode ne 0 then begin
				widget_control, (*pstate).ic_base2, scr_ysize=(*pstate).ic_base2_ysize
			endif else begin
				widget_control, (*pstate).ic_base2, scr_ysize=1
			endelse
;			widget_control, (*pstate).dwell_base, sensitive=(*p).use_dwell
			widget_control, (*pstate).dwell_base, sensitive=1
			widget_control, (*pstate).dwell_id, set_value=str_tidy((*p).dwell)
			q = where( (*p).preamp.pv eq *(*p).pic_list, nq)
			widget_control, (*pstate).ic_pv_mode, set_combobox_select=(nq ne 0) ? q[0] : 0
			widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
			widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit
			OK = (locate('.time',(*p).preamp.pv) lt 0)
			widget_control, (*pstate).ic_val_mode, sensitive=OK
			widget_control, (*pstate).ic_unit_mode, sensitive=OK
		endif
		end

	'from-dai-button': begin
		print,'From ...'
		path = *(*pstate).path
		F = file_requester( filter = '*.dai', path=path, group=event.top, $
					title='Select the source DAI file', fix_filter=1, /within_modal )
		if F ne '' then begin
			*(*pstate).path = extract_path(F[0])
			ps = read_geopixe_image( F[0], /header, error=err)
			if err then goto, finish
			
			if (*ps).DevObj->name() eq (*p).obj->name() then begin
				options = (*ps).DevObj->get_options()
				(*p).obj->set_options, options
			endif
			
			help, (*ps).IC, /str	
			(*p).charge_mode = (*ps).IC.mode
			(*p).charge_conversion = (*ps).IC.conversion
			(*p).use_dwell = (*ps).dwell.on
			if (*p).use_dwell then (*p).dwell = (*ps).dwell.val
			(*p).preamp.pv = (*ps).IC.pv.name
			(*p).preamp.val = (*ps).IC.pv.val
			(*p).preamp.unit = (*ps).IC.pv.unit
;			(*p).preamp.val = IC_val
;			(*p).preamp.unit = IC_vunit

			l = locate('time', strlowcase((*p).preamp.pv))
			val = (*p).preamp.val
			unit = (*p).preamp.unit
			ival = find_charge_val_unit_index( val, unit, iunit=iunit, time=(l ge 0), /write_back)
			(*p).preamp.val = val
			(*p).preamp.unit = unit

			if ptr_good((*ps).plist) then begin
				*(*p).pic_list = *(*ps).plist
				widget_control, (*pstate).ic_pv_mode, set_value=*(*p).pic_list
			endif else begin
				*(*p).pic_list = ['none']
			endelse
			
			if ptr_valid( (*ps).flux) and (*ps).has_flux then begin
				tflux = total(*(*ps).flux)
				if tflux gt 1.0e-10 then begin
					if (*ps).IC.mode gt 0 then begin
						if (*ps).charge eq 0.0 then begin
;							(*p).charge = tflux * (*ps).IC.conversion
						endif else begin
							if abs((*ps).IC.conversion - (*ps).charge / tflux) gt 1.0e-9 then begin
								(*p).charge_conversion = (*ps).charge / tflux
							endif
						endelse
					endif
				endif
			endif
			
			widget_control, (*pstate).charge_mode, set_combobox_select=(*p).charge_mode
			widget_control, (*pstate).charge_conversion, set_value=str_tidy((*p).charge_conversion)
			widget_control, (*pstate).ic_base, map=((*p).charge_mode ne 0 )
			widget_control, (*pstate).scan_button, sensitive=((*p).charge_mode eq 1 )
			if (*p).charge_mode eq 1 then begin
				widget_control, (*pstate).ic_base1, map=1, scr_ysize=(*pstate).ic_base1_ysize
			endif else begin
				widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
			endelse
			if (*p).charge_mode ne 0 then begin
				widget_control, (*pstate).ic_base2, scr_ysize=(*pstate).ic_base2_ysize
			endif else begin
				widget_control, (*pstate).ic_base2, scr_ysize=1
			endelse
;			widget_control, (*pstate).dwell_base, sensitive=(*p).use_dwell
			widget_control, (*pstate).dwell_base, sensitive=1
			widget_control, (*pstate).dwell_id, set_value=str_tidy((*p).dwell)
			q = where( (*p).preamp.pv eq *(*p).pic_list, nq)
			widget_control, (*pstate).ic_pv_mode, set_combobox_select=(nq ne 0) ? q[0] : 0
			widget_control, (*pstate).ic_val_mode, set_combobox_select=ival
			widget_control, (*pstate).ic_unit_mode, set_combobox_select=iunit
			OK = (locate('.time',(*p).preamp.pv) lt 0)
			widget_control, (*pstate).ic_val_mode, sensitive=OK
			widget_control, (*pstate).ic_unit_mode, sensitive=OK
		endif
		end

	'ok-button': begin
		print,'OK ...'
		widget_control, (*pstate).charge_conversion, get_value=s
		(*p).charge_conversion = float2(s)
		aps_count_to_charge = (*p).charge_conversion 
		if (*p).charge_mode eq 0 then (*p).charge_conversion = 0.0
		widget_control, (*pstate).dwell_id, get_value=s
		(*p).dwell = float2(s)
		(*p).error = 0
		goto, kill
		end

	'cancel-button': begin
		print,'Close flux_select ...'
		(*p).error = 1
		goto, kill
		end

	else:
endcase

finish:
	widget_control, hourglass=0
	return

bad_state:
	warning,'evt',['STATE variable has become ill-defined.','Abort Sort EVT.'],/error
	goto, kill
bad_ptr:
	warning,'evt',['Parameter structure variable has become ill-defined.','Abort Sort EVT.'],/error
	goto, kill

kill:
	cancel_notify, event.top
	if ptr_good(pstate, /struct) eq 0 then goto, die

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
return
end

;------------------------------------------------------------------------------------------

pro OnRealize_flux_select_charge_conversion, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).IC_base2, /geometry)
(*pstate).IC_base2_ysize = geo.ysize
return
end

;------------------------------------------------------------------------------------------

pro OnRealize_flux_select_charge_pv_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

geo = widget_info( (*pstate).IC_base1, /geometry)
(*pstate).IC_base1_ysize = geo.ysize

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	q = where( (*p).preamp.pv eq *(*p).pic_list, nq)
	if nq ne 0 then begin
		widget_control, wWidget, set_combobox_select=q[0]
	endif
	widget_control, (*pstate).ic_base, map=((*p).charge_mode ne 0 )
	if (*p).charge_mode eq 1 then begin
		widget_control, (*pstate).ic_base1, map=1, scr_ysize=(*pstate).ic_base1_ysize
	endif else begin
		widget_control, (*pstate).ic_base1, map=0, scr_ysize=1
	endelse
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_flux_select_charge_preamp_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
;	Done in units routine now ..
;	q = where( abs((*p).preamp.val - (*pstate).ic_vals) lt 0.01, nq)
;	if nq ne 0 then begin
;		widget_control, wWidget, set_combobox_select=q[0]
;	endif
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_flux_select_charge_unit_mode, wWidget

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

pro OnRealize_flux_select_charge_mode, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	widget_control, wWidget, set_combobox_select=(*p).charge_mode
endif
end

;------------------------------------------------------------------------------------------

pro flux_select, group_leader=group, TLB=tlb, device=obj, $
		xoffset=xoffset, yoffset=yoffset, pars=p, path=path, debug=debug, _extra=extra, $
		dpath=dpath, evt_file=evt_file, charge_mode=charge_mode, sensitivity=sensitivity, $
		flux_pv=flux_pv, conv=conv, silent=silent

COMPILE_OPT STRICTARR
common c_geopixe_adcs, geopixe_max_adcs
common c_working_dir, geopixe_root
common aps_4, aps_count_to_charge
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=384
if n_elements(aps_count_to_charge) lt 1 then aps_count_to_charge=0.0

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

define_devices
if n_elements(group) lt 1 then group=0L
if n_elements(path) lt 1 then path=''
if n_elements(dpath) lt 1 then dpath=path
if n_elements(debug) lt 1 then debug=0
if n_elements(sensitivity) lt 1 then sensitivity=0.0
if n_elements(evt_file) lt 1 then evt_file=''
if n_elements(obj) lt 1 then return
if n_elements(silent) lt 1 then silent=0

case !version.os_family of
	'MacOS': begin
		symbol = 'SYMBOL*12'
		large_font = 'Arial*12'
;@2		widget_control, default_font='Geneva*10'		; set font for all windows
		xw = 393
		yh = 300
		space1 = 1
		space2 = 2
		space5 = 5
		space10 = 10
		space15 = 15
		charge_xsize = 100
		pad5 = 5
		help_xsize = 322
		tab_xsize = 412
		charge_xsize2 = 180
		dev_xsize = 400
		end
	'unix': begin
		symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
		large_font = '10x20'
;@2		widget_control, default_font='6x13'				; set font for all windows
		xw = 392
		yh = 300
		space1 = 0
		space2 = 2
		space5 = 4
		space10 = 7
		space15 = 12
		charge_xsize = 100
		pad5 = 5
		help_xsize = 402
		tab_xsize = 412
		charge_xsize2 = 180
		dev_xsize = 400
		end
	else: begin
		symbol = 'SYMBOL*BOLD*14'
		large_font = 'COURIER*BOLD*10'
	;	widget_control, default_font='Arial*14'			; set font for all windows
		xw = 357
		yh = 300
		space1 = 1
		space2 = 2
		space5 = 5
		space10 = 10
		space15 = 15
		charge_xsize = 80
		pad5 = 7
		help_xsize = 326
		tab_xsize = 336
		charge_xsize2 = 195
		dev_xsize = 330		;313
		end
endcase

if n_elements(xoffset) lt 1 then begin
	screen = get_screen_size()
	xoffset = 0.5*(screen[0] - xw) > 0
endif
if n_elements(yoffset) lt 1 then begin
	screen = get_screen_size()
	yoffset = 0.5*(screen[1] - yh) > 0
endif

p = bad_pars_struct( p, make_pars=make_p)

if make_p then begin
	pars = {	$
		obj:		obj, $							; device object
		evt_file:	evt_file, $						; first EVT file name
		charge_mode:	1, $						; charge mode (direct ions, IC with PV, IC no PV)
		charge_conversion:	aps_count_to_charge, $	; conversion from flux to charge
		flux:		0.0, $							; total IC count
		preamp: {	pv:		'', $					; Epics PV name for IC
					val:	1, $					; Preamp multiplier
					unit:	0.0 }, $				; Preamp range scaling factor
		pic_list:	ptr_new(/allocate_heap), $		; List of previous IC PVs
		dwell:		0.0, $							; dwell time (ms)
		use_dwell:	0, $							; use dwell to convert IC rate to IC count
		cluster:	0, $							; cluster enable flag
		error:		0 $								; flags error return
	}
	*p = pars
endif
if n_elements(charge_mode) lt 1 then charge_mode=(*p).charge_mode
if n_elements(flux_pv) lt 1 then flux_pv=(*p).preamp.pv
(*p).charge_conversion = aps_count_to_charge
if n_elements(conv) lt 1 then conv=(*p).charge_conversion
(*p).evt_file = evt_file
(*p).obj = obj
(*p).charge_mode = charge_mode
(*p).preamp.pv = flux_pv
(*p).charge_conversion = conv
if (*p).preamp.pv eq '' then (*p).preamp.pv = 'none'
(*p).error = 0

ionbeam = obj->ionbeam()
charge_gain_unit_lists, ic_vals, ic_units, ic_vunits, ionbeam=ionbeam
if sensitivity ne 0.0 then begin
	val = charge_gain_units( sensitivity, units=unit)
	(*p).preamp.val = val
	(*p).preamp.unit = unit
	print,'flux_select: found sensitivity = ',sensitivity,', val,unit=',val,unit
endif
obj->check_pv_list, (*p).pic_list
if silent then return
(*p).error = 1

flux_scan, obj,(*p).evt_file, PV_list, IC_name, IC_val, IC_vunit, $
				no_pv=no_pv, dwell=dwell, error=error, $
				/image_mode, group=group, use_dwell=use_dwell, /suppress
if error eq 0 then begin
	(*p).use_dwell = use_dwell
	(*p).dwell = dwell
	if no_pv eq 0 then begin
		if n_elements(PV_list) ne 0 then begin
			*(*p).pic_list = PV_list
			if (*p).preamp.pv eq '' then (*p).preamp.pv = PV_list[0]
		endif
	endif
	if IC_name ne '' then (*p).preamp.pv = IC_name
	if IC_val ne 0.0 then (*p).preamp.val = IC_val
	if IC_vunit ne 0.0 then (*p).preamp.unit = IC_vunit
endif

obj->check_pv_list, (*p).pic_list
cluster_on = obj->cluster()

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

tlb = widget_base( /column, title='Flux PV Select', /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, _extra=extra, uname='flux_select_TLB', /base_align_center, $
					xpad=0, ypad=space2, space=space2, xoffset=xoffset, yoffset=yoffset, floating=(debug eq 0) )
tbase = widget_base( tlb, /column, xpad=1, ypad=0, space=space2, /base_align_center)

flux_base = widget_base( tbase, /column, xpad=1, ypad=1, scr_xsize=tab_xsize, space=space1, /base_align_center, /align_center)
lab = widget_label( flux_base, value='Beam Flux / Charge Set-up')

c0base = widget_base( flux_base, /row, /base_align_center, xpad=2, ypad=1, space=5)
charge_mode = widget_combobox( c0base, value=qmodes, $
					notify_realize='OnRealize_flux_select_charge_mode', uname='charge-mode', /tracking, $
					uvalue=qhelp, scr_xsize=tab_xsize-30)

IC_base = widget_base( flux_base, /column, /frame, xpad=4, ypad=1, space=space1, scr_xsize=dev_xsize, /base_align_right, map=((*p).charge_mode ne 0))
lab = widget_label( IC_base, value='Ion Chamber / Charge Integrator Parameters', /align_center)

IC_base1 = widget_base( IC_base, /column, xpad=0, ypad=0, space=space1, /base_align_right, map=((*p).charge_mode eq 1))

if n_elements( *(*p).pic_list) lt 1 then *(*p).pic_list=['none']

	s1base = widget_base( IC_base1, /row, xpad=1, ypad=0, space=2, /base_align_center)
	lab = widget_label( s1base, value='Scaler channel:')
	ic_pv_mode = widget_combobox( s1base, value=*(*p).pic_list, uname='ic-pv-mode', /tracking, $
					notify_realize='OnRealize_flux_select_charge_pv_mode', $
					uvalue='Select the Epics scaler PV used to record upstream ion-counter.',scr_xsize=charge_xsize2)

	s2base = widget_base( IC_base1, /row, xpad=1, ypad=0, space=2, /base_align_center)
	lab = widget_label( s2base, value='Preamp Sensitivity:')
	ic_val_mode = widget_combobox( s2base, value='   '+str_tidy(ic_vals), uname='ic-preamp-mode', /tracking, scr_xsize=charge_xsize2, $
					notify_realize='OnRealize_flux_select_charge_preamp_mode', $
					uvalue="Select the ion chamber preamp sensitivity multiplier.")

	s3base = widget_base( IC_base1, /row, xpad=1, ypad=0, space=2, /base_align_center)
	lab = widget_label( s3base, value='Preamp Scale Units:')
	ic_unit_mode = widget_combobox( s3base, value='   '+ic_units, uname='ic-preamp-unit-mode', /tracking, scr_xsize=charge_xsize2, $
					notify_realize='OnRealize_flux_select_charge_unit_mode', $
					uvalue="Select the ion chamber preamp scale units.")

	dwell_base = widget_base( IC_base1, /row, xpad=1, ypad=0, space=2, /base_align_center)
	lab = widget_label( dwell_base, value='Dwell time (ms):')
	dwell_wID = widget_text( dwell_base, uname='dwell', /editable, value=str_tidy((*p).dwell), $
						uvalue='Enter the dwell time per pixel (ms) if it is fixed. This is for converting count-rate to counts per pixel. ' + $
						'It may be found by the device driver, especially for cases when it varies.', $
						scr_xsize=charge_xsize2, /tracking)

	ic_base2 = widget_base( IC_base, /row, /base_align_center, xpad=1, ypad=1, space=2)
	scan_button = widget_button( ic_base2, value='Scan data for PVs', uname='charge-scan', /tracking, sensitive=((*p).charge_mode eq 1), $
						uvalue='Open raw data file(s) and scan for Epics PVs for IC rate and preamplifier settings.')
	lab = widget_label( ic_base2, value='     Conversion (Q/IC):')
	charge_conversion = widget_text( ic_base2, uname='charge-conversion', /editable, /tracking, value=str_tidy((*p).charge_conversion), $
					notify_realize='OnRealize_flux_select_charge_conversion', $
					uvalue='Conversion factor from integrated flux (IC count) to charge (uC) for scan.', scr_xsize=charge_xsize)
						
bbase = widget_base( tbase, /row, /base_align_center, ypad=0, space=space5)

bbase2 = widget_base( bbase, /row, /base_align_center, ypad=0, xpad=0, space=1)
lab = widget_label( bbase2, value='From:')
button = widget_button( bbase2, value='SPEC', uname='from-spec-button', /tracking, $
					uvalue='Restore IC parameters (no device parameters) from a SPEC spectrum file header.')
button = widget_button( bbase2, value='DAI', uname='from-dai-button', /tracking, $
					uvalue='Restore IC parameters and device parameters from a DAI image file header.')

lab = widget_label( bbase, value='     ')

enable_cluster = obj->cluster()
if enable_cluster eq 0 then (*p).cluster = 0
cluster_id = cw_bgroup2( bbase, ['Cluster'], /row, set_value=[(*p).cluster], sensitive=enable_cluster, /return_index, uname='cluster',/ nonexclusive, /tracking, $
						uvalue='Enable the list-mode processing of data on a computer cluster.', xpad=0, ypad=0, $
						map=cluster_on)
					
;lab = widget_label( bbase, value='     ')

;.......................

;	Device options

maiabase = widget_base( flux_base, /base_align_center, map=1, space=1, xpad=0, ypad=0)

device_option_mode_base = widget_base( maiabase, /column, /frame,  space=1, xpad=2, ypad=1, /base_align_center, scr_xsize=dev_xsize)

; Render sort options in render_options method in object, else set Y size to 1

obj->render_options, device_option_mode_base
widget_control, device_option_mode_base, map=obj->show_sort_options(), scr_ysize=obj->get_sort_ysize()

;.......................


button = widget_button( bbase, value='OK', uname='ok-button', /tracking, $
					uvalue='Accept parameters and close window.')

;.................................................................................

help = widget_text( tbase, scr_xsize=help_xsize, ysize=4, /wrap, uname='HELP', /tracking, $
				uvalue='Help window. Displays info about widgets.',frame=0)

state = {  $
		path:			ptr_new(path), $	; pointer to current path
		dpath:			ptr_new(dpath), $	; pointer to current path
		debug:			debug, $			; debug mode
		p:				p, $				; pointer to parameters
		ic_vals:		ic_vals, $			; IC vals list
		ic_units:		ic_units, $			; IC units string list
		ic_vunits:		ic_vunits, $		; scale units for units list

		charge_mode:	charge_mode, $		; Charge mode droplist ID
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
		cluster_id:		cluster_id, $		; cluster enable ID
				
		help:			help $				; help text ID
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

;register_notify, tlb, ['path', $		; new path
;					'dpath', $			; new raw data path
;					'snapshot'], $		; snapshots?
;				from=group

if debug then begin
	xmanager, 'flux_select', tlb, /no_block
endif else begin
	xmanager, 'flux_select', tlb
endelse

return
end
