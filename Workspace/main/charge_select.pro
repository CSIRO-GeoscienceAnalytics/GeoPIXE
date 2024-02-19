;
; Modal pop-up window to confirm "charge" based on total IC counts.
;
;	charge = charge_select( group, charge)
;
;	group		parent widget
;
;----------------------------------------------------------------------

pro charge_select_event, Event

COMPILE_OPT STRICTARR

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate
	p = (*pstate).p
	
	uname = widget_info( event.id, /uname)

	case tag_names( event,/structure) of
		'NOTIFY': begin
;			case event.tag of
;
;				else:
;			endcase
			end
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
				if abs((*p).new.charge - (*p).new.conv * (*p).flux) gt 0.001 then begin
					s = 'NOTE: New "Charge" is not consistent with the new "Conv" and "Flux" (IC count) values.'
				endif else s=''
				widget_control, (*pstate).help, set_value=s
			endelse
			goto, finish
			end
		'WIDGET_TIMER': begin
		;	print,' got a timer event; update text reads ...'
			end
		else:
	endcase

	case uname of

		'charge_select_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				(*p).error = 1
				widget_control, event.top, /destroy
				return
			endif
			end
			
		'new-conv': begin
			widget_control, (*pstate).new_conv_widget, get_value=s
			if gnumeric(s[0]) then (*p).new.conv = float(strtrim(s[0],2))
			(*p).new.charge = (*p).flux * (*p).new.conv
			widget_control, (*pstate).new_conv_widget, set_value=str_tidy((*p).new.conv)
			widget_control, (*pstate).new_charge_widget, set_value=str_tidy((*p).new.charge)
			end
			
		'new-charge': begin
			widget_control, (*pstate).new_charge_widget, get_value=s
			if gnumeric(s[0]) then (*p).new.charge = float(strtrim(s[0],2))
			(*p).new.conv = (*p).new.charge / (*p).flux 
			widget_control, (*pstate).new_conv_widget, set_value=str_tidy((*p).new.conv)
			widget_control, (*pstate).new_charge_widget, set_value=str_tidy((*p).new.charge)
			end

		'options-mode': begin
			(*p).mode = event.index
			case (*p).mode of
				0: begin
					(*p).new.conv = (*p).present.conv
					(*p).new.charge = (*p).flux * (*p).new.conv
					widget_control, (*pstate).new_conv_widget, set_value=str_tidy((*p).new.conv)
					widget_control, (*pstate).new_charge_widget, set_value=str_tidy((*p).new.charge)
					widget_control, (*pstate).new_conv_widget, /editable
					widget_control, (*pstate).new_charge_widget, editable=0
					end
				1: begin
					(*p).new.charge = (*p).present.charge
					(*p).new.conv = (*p).new.charge / (*p).flux 
					widget_control, (*pstate).new_conv_widget, set_value=str_tidy((*p).new.conv)
					widget_control, (*pstate).new_charge_widget, set_value=str_tidy((*p).new.charge)
					widget_control, (*pstate).new_conv_widget, editable=0
					widget_control, (*pstate).new_charge_widget, /editable
					end
			endcase
			end
			
		'ok': begin
			error = 0
			case (*p).mode of
				0: begin
					widget_control, (*pstate).new_conv_widget, get_value=s
					if gnumeric(s[0]) then begin
						(*p).new.conv = float(strtrim(s[0],2))
					endif else begin
						warning,'charge_select_event',['Illegal character in New Conv.', $
									'Correct "Conv" entry.', $
									'Then try "OK" again.']
						error = 1
					endelse
					(*p).new.charge = (*p).flux * (*p).new.conv
					widget_control, (*pstate).new_conv_widget, set_value=str_tidy((*p).new.conv)
					widget_control, (*pstate).new_charge_widget, set_value=str_tidy((*p).new.charge)
					widget_control, (*pstate).new_conv_widget, /editable
					widget_control, (*pstate).new_charge_widget, editable=0
					end
				1: begin
					widget_control, (*pstate).new_charge_widget, get_value=s
					if gnumeric(s[0]) then begin
						(*p).new.charge = float(strtrim(s[0],2))
					endif else begin
						warning,'charge_select_event',['Illegal character in New Charge.', $
									'Correct "Charge" entry.', $
									'Then try "OK" again.']
						error = 1
					endelse
					(*p).new.conv = (*p).new.charge / (*p).flux 
					widget_control, (*pstate).new_conv_widget, set_value=str_tidy((*p).new.conv)
					widget_control, (*pstate).new_charge_widget, set_value=str_tidy((*p).new.charge)
					widget_control, (*pstate).new_conv_widget, editable=0
					widget_control, (*pstate).new_charge_widget, /editable
					end
			endcase
			(*p).error = error
			if error eq 0 then begin
				widget_control, event.top, /destroy
				return
			endif
			end
			
		'cancel': begin
			(*p).error = 1
			widget_control, event.top, /destroy
			return
			end
		else:
	endcase

finish:
	return
end

;-----------------------------------------------------------------

pro charge_select, group, title=title, conv=conv, charge=charge, flux=flux, debug=debug, error=error

;	Confirm/calculate charge or IC to charge conversion factor.

common aps_4, aps_count_to_charge

COMPILE_OPT STRICTARR
	if n_elements(flux) lt 1 then return
	if n_elements(conv) lt 1 then conv=1.0
	if n_elements(title) lt 1 then title='Confirm Charge or IC to Charge Conversion'
	if n_elements(debug) lt 1 then debug=0
	if (n_elements(group) lt 1) and (debug eq 0) then return
	if n_elements(group) lt 1 then group=0L

	if debug then begin
		modal = 0
		floating = 0
	endif else begin
		modal = 1
		floating = 1
	endelse
	
	case !version.os_family of
		'MacOS': begin
;			symbol = 'SYMBOL*12'
;			large_font = 'Arial*12'
;			widget_control, default_font='Geneva*10'		; set font for all windows
			button_xsize = 100
			help_xsize = 265
			left_xsize = 60
			end
		'unix': begin
;			symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
;			large_font = '10x20'
;			widget_control, default_font='6x13'				; set font for all windows
			button_xsize = 100
			help_xsize = 265
			left_xsize = 60
			end
		else: begin
;			symbol = 'SYMBOL*BOLD*14'
;			large_font = 'COURIER*BOLD*10'
		;	widget_control, default_font='Arial*14'			; set font for all windows
			button_xsize = 100
			help_xsize = 265
			left_xsize = 60
			end
	endcase

	xsize = 120
	ysize = 80
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='charge_select_TLB', /base_align_center, $
					modal=modal, floating=floating, xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_right)

	r1base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( r1base, value='Flux:')
	count_widget = widget_text( r1base, uname='flux', value=string(flux), $
					uvalue='The total flux (integrated IC count) for the selected image or spectrum.',scr_xsize=button_xsize, /tracking)
	lab = widget_label( r1base, value='  Total IC Count',scr_xsize=button_xsize)

	opt = ['  Use "Conv" to set "Charge"','  Use "Charge" to set "Conv"']
	options = widget_combobox( tbase, value=opt, uname='options-mode', /tracking, scr_xsize=help_xsize, $
					uvalue='To determine charge from "Conv", select "Use Conv to set Charge" and enter "Conv". ' + $
					'To determine conv from "Charge", select "Use Charge to set Conv" and enter "Charge".')

	r2base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( r2base, value='         Present:',scr_xsize=button_xsize)
	lab = widget_label( r2base, value='       Change to:',scr_xsize=button_xsize)

	conv1 = charge / flux
	charge1 = charge
	conv2 = conv
	charge2 = flux * conv
	
	r3base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( r3base, value='Conv:')
	present_conv_widget = widget_text( r3base, uname='present-conv', value=string(conv1), $
					uvalue='The present Conversion factor calculated from the "Flux" (IC count) and present "Charge".',scr_xsize=button_xsize, /tracking)
	new_conv_widget = widget_text( r3base, uname='new-conv', value=string(conv2), /editable, $
					uvalue='The new Conversion factor, entered or calculated.',scr_xsize=button_xsize, /tracking)

	r4base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( r4base, value='Charge:')
	present_charge_widget = widget_text( r4base, uname='present-charge', value=string(charge1), $
					uvalue='The present Charge.',scr_xsize=button_xsize, /tracking)
	new_charge_widget = widget_text( r4base, uname='new-charge', value=string(charge2), $
					uvalue='The new Charge, entered or calculated.',scr_xsize=button_xsize, /tracking)

	bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	button = widget_button( bbase, value='Cancel', uname='cancel',scr_xsize=button_xsize, $
					uvalue='Cancel and keep original settings.', /tracking)
;	lab = widget_label( bbase, value='     ')
	button = widget_button( bbase, value='OK', uname='ok',scr_xsize=button_xsize, $
					uvalue='Accept new "Conversion" factor entered or calculated from ratio of "Flux" (IC count) to "Charge" and exit.', /tracking)

	if abs(charge1 - conv1 * flux) gt 0.001 then begin
		s = 'NOTE: Present "Charge" is not consistent with the present "Conv" and "Flux" (IC count) values.'
	endif else s=''
	help = widget_text( tbase, scr_xsize=help_xsize, ysize=4, /wrap, uname='HELP', /tracking, value=s, frame=0)

	p = ptr_new(  {	flux:			flux, $							; flux
					mode:			0, $							; mode (0="use conv to set charge", 1="use charge to set conv")
					error:			0, $							; error flag
					present:	{	conv:			conv1, $		; present conv
									charge:			charge1 }, $	; present charge
					new:		{	conv:			conv2, $		; new conv
									charge:			charge2 }} )	; new charge

	state = {	$
				p:						p, $						; pointer to selection
				count_widget:			count_widget, $				; widget ID of flux text
				options: 				options, $					; widget ID of options droplist
				present_conv_widget:	present_conv_widget, $		; widget ID of present conv text
				new_conv_widget:		new_conv_widget, $			; widget ID of new conv text
				present_charge_widget:	present_charge_widget, $	; widget ID of present charge text
				new_charge_widget:		new_charge_widget, $		; widget ID of new charge text
				help:					help $						; help widget ID
			}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	if debug then begin
		xmanager, 'charge_select', tlb, /no_block
	endif else begin
		xmanager, 'charge_select', tlb
	endelse

	conv = (*p).new.conv
	charge = (*p).new.charge
	error = (*p).error
	return
end
