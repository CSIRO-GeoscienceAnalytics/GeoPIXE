;
; IDL Event Callback Procedures
; cal_eventcb
;
; Generated on:	07/12/99 17:04.23
;
;-----------------------------------------------------------------

pro PostCreate_Cal_Top, wWidget, xlow, xhigh, path=path, _EXTRA=_VWBExtra_

top = tlb_id( wWidget)
child = widget_info( top, /child)

if n_elements(path) lt 1 then path=''
max_cal = 8

cal = {	order:1, $				; cal order (after apply)
	    units: '', $			; cal units (after apply)
	    poly: fltarr(max_cal), $ ; cal poly (after apply)
	    X: [0,0], $				; markers
	    E: [0.0,0.0] }			; marked energies

state = { $
	xlow: xlow, $				; low X marker
	xhigh: xhigh, $				; high X marker
	elow: 6.4, $				; low e
	ehigh: 15.74, $				; high e
	elow_direct: 0, $			; user typed elow direct
	ehigh_direct: 0, $			; user typed ehigh direct
	a: 1.0, $					; A
	b: 0.0, $					; B
	ab_direct: 0, $				; user typed a,b direct
	units: 'channels', $		; units
	line1: 0, $					; line 1 index
	line2: 0, $					; line 2 index
	z1: 0, $					; Z1
	z2: 0, $					; Z2
	get_adc:		0, $		; which ADC to "Get" (0=any)

	low_e_text: 0L, $			; low_e_text id
	high_e_text: 0L, $			; high_e_text id
	element_1_text: 0L, $		; element_1_text id
	element_2_text: 0L, $		; element_2_text id
	line_1_combobox: 0L, $		; line_1_combobox id
	line_2_combobox: 0L, $		; line_2_combobox id
	cal_a_text: 0L, $			; cal_a_text id
	cal_b_text: 0L, $			; cal_b_text id
	cal_units_text: 0L, $		; cal_units_text id
	get_adc_combobox:  0L, $	; droplist ID
	help: 0L, $					; help text id
	path: ptr_new(path), $		; path
	p: ptr_new(cal) }			; will point to state

widget_control, child, set_uvalue=ptr_new(state, /no_copy)
end

;-----------------------------------------------------------------

pro OnDestroy_Cal_TLB, wWidget

cancel_notify, wWidget
end

;-----------------------------------------------------------------

pro OnKill_Cal_TLB, Event

print,'OnKill_Cal: destroy widget tree ...'
widget_control, event.top, /destroy
end

;-----------------------------------------------------------------

pro cal_update_state, pstate

	widget_control, (*pstate).elow_text, get_value=s
	(*p).e_low = float(s[0])
	widget_control, (*pstate).ehigh_text, get_value=s
	(*p).e_high = float(s[0])

return
end

;-----------------------------------------------------------------
; NOTIFY Callback Procedure.
;
;   {NOTIFY, ID:0L, TOP:0L, HANDLER:0L, TAG:t, POINTER:p, FROM:f }
;
;-----------------------------------------------------------------

pro OnNotify_Cal, Event

;print,'OnNotify_Cal: tag = ',event.tag

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case event.tag of

	'path': begin
		if ptr_valid( event.pointer) then begin
;			print,'CAL: new path = ',(*event.pointer)
			*(*pstate).path = (*event.pointer)
		endif
		end

	'cal-x': begin		; returned from 'spectrum_display'

		if ptr_valid( event.pointer) then begin
			(*pstate).xlow = (*event.pointer).low
			(*pstate).xhigh = (*event.pointer).high
;			print,'CAL:  notify, xlow,xhigh = ',(*pstate).xlow, (*pstate).xhigh
			recalc_ab, pstate
		endif
		end

	else: begin
		print,'OnNotify_Cal: unknown tag = ',event.tag
		end
endcase
end

;-----------------------------------------------------------------
; Tracking Callback Procedure.
;
;   {WIDGET_TRACKING, ID:0L, TOP:0L, HANDLER:0L, ENTER:0 }
;
;   ENTER is 1 if the tracking event is an entry event, and 0 if it
;       is an exit event.
;-----------------------------------------------------------------

pro OnTracking_Cal, Event

widget_control, event.id, get_uvalue=message
if n_elements(message) lt 1 then return
if size(message,/tname) ne 'STRING' then return

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if event.enter eq 1 then begin
	widget_control, (*pstate).help, set_value=message
endif else begin
	widget_control, (*pstate).help, set_value=' '
endelse
end
;
;-----------------------------------------------------------------

pro OnRealize_Element_1_text, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).element_1_text = wWidget
(*pstate).z1 = 0
end

;-----------------------------------------------------------------

pro OnRealize_Cal_Help, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).help = wWidget
end

;-----------------------------------------------------------------
; Text Insert Character Callback Procedure.
;
;   {WIDGET_TEXT_CH, ID:0L, TOP:0L, HANDLER:0L, TYPE:0, OFFSET:0L,
;       CH:0B }
;
;   OFFSET is the (zero-based) insertion position that will result
;       after the character is inserted. CH is the ASCII value of the
;       character.
;-----------------------------------------------------------------

pro OnInsert_Element_1_text, Event

common c_xray_lines_1, xray_lines

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).elow = 0.0
widget_control, (*pstate).low_e_text, set_value = ''
(*pstate).elow_direct = 0
(*pstate).ab_direct = 0

if event.type ne 0 then begin
	return
endif
if event.ch ne 10 then begin
	return
endif

widget_control, event.id, get_value=s
(*pstate).z1 = atomic_number(s)
if ((*pstate).z1 gt 0) and ((*pstate).line1 gt 0) then begin
	(*pstate).elow = line_energy( (*pstate).z1, xray_lines[(*pstate).line1])
;	print,'  z, line, elow=', (*pstate).z1,xray_lines[(*pstate).line1],(*pstate).elow
	widget_control, (*pstate).low_e_text, set_value = string((*pstate).elow)
	recalc_ab, pstate

	(*pstate).units = 'keV'
	widget_control, (*pstate).cal_units_text, set_value=(*pstate).units
endif
end

;-----------------------------------------------------------------

pro OnInsert_Low_E_text, Event

common c_xray_lines_1, xray_lines

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

widget_control, (*pstate).element_1_text, set_value=' '
widget_control, (*pstate).line_1_combobox, set_combobox_select=0
(*pstate).line1 = 0
(*pstate).z1 = 0
(*pstate).elow_direct = 1
(*pstate).ab_direct = 0

if event.type ne 0 then begin
	return
endif
if event.ch ne 10 then begin
	return
endif

widget_control, (*pstate).low_e_text, get_value=s
(*pstate).elow = float(s)
print,'  elow=',(*pstate).elow
print,'  z, line, elow=', (*pstate).z1,xray_lines[(*pstate).line1],(*pstate).elow
recalc_ab, pstate
end

;-----------------------------------------------------------------

pro OnInsert_Element_2_text, Event

  common c_xray_lines_1, xray_lines

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).ehigh = 0.0
widget_control, (*pstate).high_e_text, set_value = ''
(*pstate).ehigh_direct = 0
(*pstate).ab_direct = 0

if event.type ne 0 then begin
	return
endif
if event.ch ne 10 then begin
	return
endif

widget_control, event.id, get_value=s
(*pstate).z2 = atomic_number(s)
if ((*pstate).z2 gt 0) and ((*pstate).line2 gt 0) then begin
	(*pstate).ehigh = line_energy( (*pstate).z2, xray_lines[(*pstate).line2])
	print,'  z, line, ehigh=', (*pstate).z2,xray_lines[(*pstate).line2],(*pstate).ehigh
	widget_control, (*pstate).high_e_text, set_value = string((*pstate).ehigh)
	recalc_ab, pstate

	(*pstate).units = 'keV'
	widget_control, (*pstate).cal_units_text, set_value=(*pstate).units
endif
end

;-----------------------------------------------------------------

pro OnInsert_High_E_text, Event

common c_xray_lines_1, xray_lines

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

widget_control, (*pstate).element_2_text, set_value=' '
widget_control, (*pstate).line_2_combobox, set_combobox_select=0
(*pstate).line2 = 0
(*pstate).z2 = 0
(*pstate).ehigh_direct = 1
(*pstate).ab_direct = 0

if event.type ne 0 then begin
	return
endif
if event.ch ne 10 then begin
	return
endif

widget_control, (*pstate).high_e_text, get_value=s
(*pstate).ehigh = float(s)
print,'  z, line, ehigh=', (*pstate).z2,xray_lines[(*pstate).line2],(*pstate).ehigh
recalc_ab, pstate
end

;-----------------------------------------------------------------

pro OnInsert_Cal_A_text, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

widget_control, (*pstate).element_1_text, set_value = ' '
widget_control, (*pstate).element_2_text, set_value = ' '
widget_control, (*pstate).line_1_combobox, set_combobox_select=0
widget_control, (*pstate).line_2_combobox, set_combobox_select=0
widget_control, (*pstate).low_e_text, set_value=' '
widget_control, (*pstate).high_e_text, set_value=' '
(*pstate).line1 = 0
(*pstate).z1 = 0
(*pstate).line2 = 0
(*pstate).z2 = 0
(*pstate).elow_direct = 0
(*pstate).ehigh_direct = 0
(*pstate).ab_direct = 1

if event.type ne 0 then begin
	return
endif
if event.ch ne 10 then begin
	return
endif

widget_control, (*pstate).cal_a_text, get_value = s
(*pstate).a = float(s)

(*pstate).elow = (*pstate).a * (*pstate).xlow + (*pstate).b
(*pstate).ehigh = (*pstate).a * (*pstate).xhigh + (*pstate).b
print,'  a,b=',(*pstate).a,(*pstate).b,'  elow,ehigh=',(*pstate).elow,(*pstate).ehigh
widget_control, (*pstate).low_e_text, set_value = string((*pstate).elow)
widget_control, (*pstate).high_e_text, set_value = string((*pstate).ehigh)
end

;-----------------------------------------------------------------

pro OnInsert_Cal_B_text, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

widget_control, (*pstate).element_1_text, set_value = ' '
widget_control, (*pstate).element_2_text, set_value = ' '
widget_control, (*pstate).line_1_combobox, set_combobox_select=0
widget_control, (*pstate).line_2_combobox, set_combobox_select=0
widget_control, (*pstate).low_e_text, set_value=' '
widget_control, (*pstate).high_e_text, set_value=' '
(*pstate).line1 = 0
(*pstate).z1 = 0
(*pstate).line2 = 0
(*pstate).z2 = 0
(*pstate).elow_direct = 0
(*pstate).ehigh_direct = 0
(*pstate).ab_direct = 1

if event.type ne 0 then begin
	return
endif
if event.ch ne 10 then begin
	return
endif

widget_control, (*pstate).cal_b_text, get_value = s
(*pstate).b = float(s)

(*pstate).elow = (*pstate).a * (*pstate).xlow + (*pstate).b
(*pstate).ehigh = (*pstate).a * (*pstate).xhigh + (*pstate).b
print,'  a,b=',(*pstate).a,(*pstate).b,'  elow,ehigh=',(*pstate).elow,(*pstate).ehigh
widget_control, (*pstate).low_e_text, set_value = string((*pstate).elow)
widget_control, (*pstate).high_e_text, set_value = string((*pstate).ehigh)
end

;-----------------------------------------------------------------

pro recalc_ab, pstate

(*pstate).a = ((*pstate).ehigh - (*pstate).elow) / ((*pstate).xhigh - (*pstate).xlow)
(*pstate).b = (*pstate).elow - (*pstate).a * (*pstate).xlow

widget_control, (*pstate).cal_a_text, set_value = string((*pstate).a)
widget_control, (*pstate).cal_b_text, set_value = string((*pstate).b)
return
end

;-----------------------------------------------------------------

pro OnRealize_Line_1_combobox, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).line_1_combobox = wWidget
end

;-----------------------------------------------------------------
; Droplist Select Item Callback Procedure.
;
;   {WIDGET_combobox, ID:0L, TOP:0L, HANDLER:0L, INDEX:0L }
;
;   INDEX returns the index of the selected item. This can be used to
;       index the array of names originally used to set the widget's
;       value.
;-----------------------------------------------------------------

pro OnSelect_Line_1_combobox, Event

  common c_xray_lines_1, xray_lines

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).line1 = event.index
widget_control, (*pstate).element_1_text, get_value=s
(*pstate).z1 = atomic_number(s)
(*pstate).elow_direct = 0
(*pstate).ab_direct = 0

if ((*pstate).z1 gt 0) and ((*pstate).line1 gt 0) then begin
	(*pstate).elow = line_energy( (*pstate).z1, xray_lines[(*pstate).line1])
	print,'  z, line, elow=', (*pstate).z1,xray_lines[(*pstate).line1],(*pstate).elow
	widget_control, (*pstate).low_e_text, set_value = string((*pstate).elow)
	recalc_ab, pstate

	(*pstate).units = 'keV'
	widget_control, (*pstate).cal_units_text, set_value=(*pstate).units
endif
end

;-----------------------------------------------------------------

pro OnRealize_Line_2_combobox, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).line_2_combobox = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_Get_ADC_combobox, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).get_adc_combobox = wWidget
end

;-----------------------------------------------------------------
; Droplist Select Item Callback Procedure.
;
;   {WIDGET_combobox, ID:0L, TOP:0L, HANDLER:0L, INDEX:0L }
;
;   INDEX returns the index of the selected item. This can be used to
;       index the array of names originally used to set the widget's
;       value.
;-----------------------------------------------------------------

pro OnSelect_Line_2_combobox, Event

  common c_xray_lines_1, xray_lines

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).line2 = event.index
widget_control, (*pstate).element_2_text, get_value=s
(*pstate).z2 = atomic_number(s)
(*pstate).ehigh_direct = 0
(*pstate).ab_direct = 0

if ((*pstate).z2 gt 0) and ((*pstate).line2 gt 0) then begin
	(*pstate).ehigh = line_energy( (*pstate).z2, xray_lines[(*pstate).line2])
	print,'  z, line, ehigh=', (*pstate).z2,xray_lines[(*pstate).line2],(*pstate).ehigh
	widget_control, (*pstate).high_e_text, set_value = string((*pstate).ehigh)
	recalc_ab, pstate

	(*pstate).units = 'keV'
	widget_control, (*pstate).cal_units_text, set_value=(*pstate).units
endif
end

;-----------------------------------------------------------------

pro OnSelect_Get_ADC_combobox, Event

  common c_xray_lines_1, xray_lines

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).get_adc = event.index
end

;-----------------------------------------------------------------

pro OnRealize_Low_E_text, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).low_e_text = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_Element_2_text, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).element_2_text = wWidget
(*pstate).z2 = 0
end

;-----------------------------------------------------------------

pro OnRealize_High_E_text, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).high_e_text = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_Cal_A_text, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).cal_a_text = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_Cal_B_text, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).cal_b_text = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_Cal_Units_text, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).cal_units_text = wWidget
end

;-----------------------------------------------------------------
; Text Insert Character Callback Procedure.
;
;   {WIDGET_TEXT_CH, ID:0L, TOP:0L, HANDLER:0L, TYPE:0, OFFSET:0L,
;       CH:0B }
;
;   OFFSET is the (zero-based) insertion position that will result
;       after the character is inserted. CH is the ASCII value of the
;       character.
;-----------------------------------------------------------------

pro OnInsert_Cal_Units_text, Event

if event.ch ne 10 then return
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

widget_control, event.id, get_value=s
(*pstate).units = s
print,' units=',(*pstate).units
end

;-----------------------------------------------------------------
; Activate Button Callback Procedure.
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.
;-----------------------------------------------------------------

pro OnButton_Cal_Apply, Event, all=all, RA=RA

; Apply cal to current spectrum, or to "All", or
; Re-cal All to re-assign the energy of the marked peaks to the new energies.

common c_xray_lines_1, xray_lines
if n_elements(all) lt 1 then all=0
if n_elements(RA) lt 1 then RA=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if (*pstate).ab_direct eq 0 then begin
	if (*pstate).elow_direct eq 0 then begin
		widget_control, (*pstate).element_1_text, get_value=s
		(*pstate).z1 = atomic_number(s)
		if ((*pstate).z1 gt 0) and ((*pstate).line1 gt 0) then begin
			(*pstate).elow = line_energy( (*pstate).z1, xray_lines[(*pstate).line1])
			print,'  z, line, elow=', (*pstate).z1,xray_lines[(*pstate).line1],(*pstate).elow
			widget_control, (*pstate).low_e_text, set_value = string((*pstate).elow)
		endif
	endif else begin
		widget_control, (*pstate).low_e_text, get_value=s
		(*pstate).elow = float(s[0])
	endelse
	if (*pstate).ehigh_direct eq 0 then begin
		widget_control, (*pstate).element_2_text, get_value=s
		(*pstate).z2 = atomic_number(s)
		if ((*pstate).z2 gt 0) and ((*pstate).line2 gt 0) then begin
			(*pstate).ehigh = line_energy( (*pstate).z2, xray_lines[(*pstate).line2])
			print,'  z, line, ehigh=', (*pstate).z2,xray_lines[(*pstate).line2],(*pstate).ehigh
			widget_control, (*pstate).high_e_text, set_value = string((*pstate).ehigh)
		endif
	endif else begin
		widget_control, (*pstate).high_e_text, get_value=s
		(*pstate).ehigh = float(s[0])
	endelse
	recalc_ab, pstate
endif
widget_control, (*pstate).cal_a_text, get_value = s
(*pstate).a = float(s)
widget_control, (*pstate).cal_b_text, get_value = s
(*pstate).b = float(s)
widget_control, (*pstate).cal_units_text, get_value = s
(*pstate).units = s

(*(*pstate).p).order = 1
(*(*pstate).p).units = (*pstate).units
(*(*pstate).p).poly[1] = (*pstate).a
(*(*pstate).p).poly[0] = (*pstate).b
(*(*pstate).p).X = [(*pstate).xlow,(*pstate).xhigh] 
(*(*pstate).p).E = [(*pstate).elow,(*pstate).ehigh] 

if all then begin
	notify, 'cal-ab-all', (*pstate).p, from=event.top
endif else if RA then begin
	if (*pstate).ab_direct then begin
		notify, 'cal-ab-RA-AB', (*pstate).p, from=event.top
	endif else begin
		notify, 'cal-ab-RA', (*pstate).p, from=event.top
	endelse
endif else begin
	notify, 'cal-ab', (*pstate).p, from=event.top
endelse
end

;-----------------------------------------------------------------

pro OnButton_Cal_Get, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

path = *(*pstate).path
F = file_requester( /read, filter = '*.spec', $
		/must_exist, path=path, group=event.top, $
		title='Load energy calibration from a SPEC file', fix_filter=0)
if F ne '' then begin
	p = read_spec(F, /header, find=(*pstate).get_adc)
	if ptr_valid(p) then begin
		p = p[0]

		(*pstate).a = (*p).cal.poly[1]
		(*pstate).b = (*p).cal.poly[0]
		(*pstate).units = (*p).cal.units
		widget_control, (*pstate).cal_a_text, set_value = string((*pstate).a)
		widget_control, (*pstate).cal_b_text, set_value = string((*pstate).b)
		widget_control, (*pstate).cal_units_text, set_value = (*pstate).units

		(*pstate).elow = (*pstate).a * (*pstate).xlow + (*pstate).b
		(*pstate).ehigh = (*pstate).a * (*pstate).xhigh + (*pstate).b
		print,'  a,b=',(*pstate).a,(*pstate).b,'  elow,ehigh=',(*pstate).elow,(*pstate).ehigh
		widget_control, (*pstate).low_e_text, set_value = string((*pstate).elow)
		widget_control, (*pstate).high_e_text, set_value = string((*pstate).ehigh)

		widget_control, (*pstate).element_1_text, set_value = ' '
		widget_control, (*pstate).element_2_text, set_value = ' '
		widget_control, (*pstate).line_1_combobox, set_combobox_select=0
		widget_control, (*pstate).line_2_combobox, set_combobox_select=0
		(*pstate).line1 = 0
		(*pstate).z1 = 0
		(*pstate).line2 = 0
		(*pstate).z2 = 0
		(*pstate).ab_direct = 1

		free_spectrum, p
	endif
endif
end

;-----------------------------------------------------------------

pro OnButton_Cal_keV, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).units = 'keV'
widget_control, (*pstate).cal_units_text, set_value=(*pstate).units
end

;-----------------------------------------------------------------

pro OnButton_Cal_Close, Event

print,'OnButton_Cal_Close: destroy widget tree ...'
widget_control, event.top, /destroy
end

;-----------------------------------------------------------------
;
; Empty stub procedure used for autoloading.
;
pro cal_eventcb
end
