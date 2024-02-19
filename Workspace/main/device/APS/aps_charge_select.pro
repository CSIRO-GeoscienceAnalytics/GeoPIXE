;
; Modal pop-up window to confirm "charge" based on total IC counts.
;
;	charge = APS_charge_select( group, charge)
;
;	group		parent widget
;
;----------------------------------------------------------------------

pro APS_charge_select_event, Event

COMPILE_OPT STRICTARR

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	uname = widget_info( event.id, /uname)

	case uname of

		'APS_charge_select_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				widget_control, event.top, /destroy
				return
			endif
			end
		'charge': begin
			widget_control, (*pstate).charge_widget, get_value=s
			if gnumeric(s[0]) then (*(*pstate).p).charge = float(strtrim(s[0],2))
			(*(*pstate).p).conversion = (*(*pstate).p).charge / (*(*pstate).p).fluence
			widget_control, (*pstate).conversion_widget, set_value=str_tidy((*(*pstate).p).conversion)
			end
		'conversion': begin
			widget_control, (*pstate).conversion_widget, get_value=s
			if gnumeric(s[0]) then (*(*pstate).p).conversion = float(strtrim(s[0],2))
			(*(*pstate).p).charge = (*(*pstate).p).conversion * (*(*pstate).p).fluence
			widget_control, (*pstate).charge_widget, set_value=str_tidy((*(*pstate).p).charge)
			end
		'count': begin
			widget_control, (*pstate).count_widget, get_value=s
			end
		'query': begin
			case event.value of
				0: begin
					(*(*pstate).p).use_flux = event.select
					end
				else:
			endcase
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
			error = 0
			widget_control, (*pstate).charge_widget, get_value=s
			if gnumeric(s[0]) then begin
				(*(*pstate).p).charge = float(strtrim(s[0],2))
			endif else begin
				warning,'APS_charge_select_event',['Illegal character in Charge.', $
							'Correct Charge entry.', $
							'Then try "OK" again.']
				error = 1
			endelse
			widget_control, (*pstate).count_widget, get_value=s
			if gnumeric(s[0]) then begin
				(*(*pstate).p).fluence = float(strtrim(s[0],2))
			endif else begin
				warning,'APS_charge_select_event',['Illegal character in IC Count.', $
							'Correct IC Count entry.', $
							'Then try "OK" again.']
				error = 1
			endelse

			if error eq 0 then begin
				widget_control, event.top, /destroy
				return
			endif
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

function APS_charge_select, group, head, title=title, silent=silent, $
			flux_question=flux_question

;	Confirm/calculate APS fluence and return as equivalent 'charge'.

common aps_4, aps_count_to_charge
if n_elements(flux_question) lt 1 then flux_question=0

COMPILE_OPT STRICTARR
	if n_elements(silent) lt 1 then silent=0
	if n_elements(group) lt 1 then return, 0
	if n_elements(head) lt 1 then return, 0
	if n_elements(title) lt 1 then title='IC Confirm Charge'

;	Need to extend the structure, if it does not contain "use_flux". 
;	Take care to include all Inherited tags.

	tags = tag_names(head)
	q = where(tags eq 'USE_FLUX',nq)
	if nq eq 0 then begin
		head2 = {aps_head2, INHERITS aps_head, conversion:0.0, use_flux:0}
		for i=0L,n_elements(tags)-1 do begin
			head2.(i) = head.(i)
		endfor
		head = head2
		head.conversion = aps_count_to_charge
	endif
	
	if n_elements(aps_count_to_charge) gt 0 then begin
		head.charge = aps_count_to_charge * head.fluence
		print,'APS_count_to_charge = ', aps_count_to_charge
	endif
	if n_elements(aps_count_to_charge) lt 1 then aps_count_to_charge=1.
	if silent then return, head

	xsize = 120
	ysize = 80
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='APS_charge_select_TLB', /base_align_center, $
					/modal, /floating, xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_right)

	s2base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s2base, value='Total IC Count Found:')
	count_widget = widget_text( s2base, uname='count', value=string(head.fluence), $
					uvalue='The total integrated IC count over image area used as flux measure.',scr_xsize=100, tracking=0)

	if flux_question then begin
		query = cw_bgroup2( tbase, ['Use flux to flatten image'], /column, xpad=0, ypad=0, space=0, $
					/return_index, /tracking, $
					uname='query', set_value=[head.use_flux], /nonexclusive, $
					uvalue=['Enable using the flux image to normalize out the effects of flux variations in images.'])
	endif else query = 0L
	
	s2bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s2bbase, value='Conversion Q / IC count:')
	conversion_widget = widget_text( s2bbase, uname='conversion', /editable, value=string(head.conversion), $
					uvalue='Conversion factor Q/IC from selected total ion chamber count to charge (uC).',scr_xsize=100, tracking=0)

	s1base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s1base, value='"charge" Q:')
	charge_widget = widget_text( s1base, uname='charge', /editable, value=string(head.charge), $
					uvalue='Enter the total integrated "charge" equivalent. Hint: Note the ratio of total IC count ' + $
					'to "charge" for a standard, to scale observed IC count to estimate a "charge" here.', $
					scr_xsize=100, tracking=0)

	bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=5, /base_align_center)
;	button = widget_button( bbase, value='x10', uname='up')
;	button = widget_button( bbase, value='/10', uname='down')
	button = widget_button( bbase, value='Cancel', uname='cancel')
	lab = widget_label( bbase, value='     ')
	button = widget_button( bbase, value='OK', uname='ok')

	p = ptr_new( head)

	state = {	$
				p:				p, $						; pointer to selection
				charge_widget:	charge_widget, $			; widget ID of charge text
				conversion_widget: conversion_widget, $		; widget ID of conversion factor text
				query_widget:	query, $					; widget ID of query options checkbox
				count_widget:	count_widget $				; widget ID of DT text
			}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	xmanager, 'APS_charge_select', tlb

	head = *p
	if head.fluence gt 1.0e-6 then begin
		aps_count_to_charge = head.charge / head.fluence
		head.conversion = aps_count_to_charge
		print,'APS_count_to_charge = ', aps_count_to_charge
	endif

	return, head
end
