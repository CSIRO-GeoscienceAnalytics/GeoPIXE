;
; Modal pop-up window to confirm LMF charge.
;
;	charge = OM_charge_select( group, charge)
;
;	group		parent widget
;
;----------------------------------------------------------------------

pro OM_charge_select_event, Event

COMPILE_OPT STRICTARR

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	uname = widget_info( event.id, /uname)

	case uname of

		'OM_charge_select_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				widget_control, event.top, /destroy
				return
			endif
			end
		'charge': begin
			widget_control, (*pstate).cwidget, get_value=s
			if gnumeric(s[0]) then (*(*pstate).p).charge = float(strtrim(s[0],2))
			end
		'deadtime': begin
			widget_control, (*pstate).dtwidget, get_value=s
			if gnumeric(s[0]) then (*(*pstate).p).deadtime = float(strtrim(s[0],2))
			end
		'up': begin
			(*(*pstate).p).charge = (*(*pstate).p).charge * 10.
			widget_control, (*pstate).cwidget, set_value=string((*(*pstate).p).charge)
			end
		'down': begin
			(*(*pstate).p).charge = (*(*pstate).p).charge / 10.
			widget_control, (*pstate).cwidget, set_value=string((*(*pstate).p).charge)
			end

		'ok': begin
			error = 0
			widget_control, (*pstate).cwidget, get_value=s
			if gnumeric(s[0]) then begin
				(*(*pstate).p).charge = float(strtrim(s[0],2))
			endif else begin
				warning,'OM_charge_select_event',['Illegal character in Charge.', $
							'Correct Charge entry.', $
							'Then try "OK" again.']
				error = 1
			endelse
			widget_control, (*pstate).dtwidget, get_value=s
			if gnumeric(s[0]) then begin
				(*(*pstate).p).deadtime = float(strtrim(s[0],2))
			endif else begin
				warning,'OM_charge_select_event',['Illegal character in DeadTime.', $
							'Correct DeadTime entry.', $
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

function OM_charge_select, group, charge, live=live, dead=dead

;	Confirm LMF charge extraction

COMPILE_OPT STRICTARR
	if n_elements(group) lt 1 then return, 0
	if n_elements(charge) lt 1 then charge=1.0
	if n_elements(live) lt 1 then live={elapsed:1.0, live:1.0}
	live_fraction = live.live/live.elapsed
	deadtime = (1.0 - live_fraction) * 100.

	xsize = 120
	ysize = 80
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title='Edit LMF Charge', /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='OM_charge_select_TLB', /base_align_center, $
					/modal, /floating, xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_right)

	s1base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s1base, value='LMF Charge Found (uC):')
	cwidget = widget_text( s1base, uname='charge', /editable, value=string(charge), $
					uvalue='Enter or modify the integrated charge extracted from the LMF file..',scr_xsize=100, tracking=0)

	s2base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s2base, value='LMF Dead Time (%):')
	dtwidget = widget_text( s2base, uname='deadtime', /editable, value=string(deadtime), $
					uvalue='Enter or modify the deadtime extracted from the LMF file..',scr_xsize=100, tracking=0)

	bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=5, /base_align_center)
	button = widget_button( bbase, value='x10', uname='up')
	button = widget_button( bbase, value='/10', uname='down')
	lab = widget_label( bbase, value='  ')
	button = widget_button( bbase, value='Cancel', uname='cancel')
	button = widget_button( bbase, value='OK', uname='ok')

	p = ptr_new( {charge:charge, deadtime:deadtime, error:0 })

	state = {	$
			p:			p, $					; pointer to selection
			cwidget:	cwidget, $				; widget ID of charge text
			dtwidget:	dtwidget $				; widget ID of DT text
			}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	xmanager, 'OM_charge_select', tlb					;, /no_block

	dead = (*p).deadtime*0.01
	return, (*p).charge * (1.0 - dead)
end
