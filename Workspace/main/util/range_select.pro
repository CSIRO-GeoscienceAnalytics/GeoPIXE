;
; Modal pop-up window to enter step range.
;
;	range = range_select( group)
;
;	group		parent widget
;
;----------------------------------------------------------------------

pro range_select_event, Event

COMPILE_OPT STRICTARR

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	uname = widget_info( event.id, /uname)

	case uname of

		'range_select_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				(*(*pstate).p).error = 1
				widget_control, event.top, /destroy
				return
			endif
			end
		'start': begin
			widget_control, (*pstate).start, get_value=s
			if fnumeric(s) then (*(*pstate).p).start = float(strtrim(s,2))
			end
		'stop': begin
			widget_control, (*pstate).stop, get_value=s
			if fnumeric(s) then (*(*pstate).p).stop = float(strtrim(s,2))
			end
		'count': begin
			widget_control, (*pstate).count, get_value=s
			if fnumeric(s) then (*(*pstate).p).count = round(float(strtrim(s,2)))
			end
		'charge': begin
			charges = [1.,0.1,0.01,0.001]
			(*(*pstate).p).charge = charges[event.index]
			end
		'ok': begin
			error = 0
			widget_control, (*pstate).start, get_value=s
			if fnumeric(s) then begin
				(*(*pstate).p).start = float(strtrim(s,2))
			endif else begin
				warning,'range_select_event','Illegal character in Start position.'
				error = 1
			endelse
			if fnumeric(s) then begin
				(*(*pstate).p).start = float(strtrim(s,2))
			endif else begin
				warning,'range_select_event',['Illegal character in Start Position.', $
							'Correct Start Position entry.', $
							'Then try "OK" again.']
				error = 1
			endelse
			widget_control, (*pstate).stop, get_value=s
			if fnumeric(s) then begin
				(*(*pstate).p).stop = float(strtrim(s,2))
			endif else begin
				warning,'range_select_event',['Illegal character in Stop Position.', $
							'Correct Stop Position entry.', $
							'Then try "OK" again.']
				error = 1
			endelse
			widget_control, (*pstate).count, get_value=s
			if fnumeric(s) then begin
				(*(*pstate).p).count = round(float(strtrim(s,2)))
			endif else begin
				warning,'range_select_event',['Illegal character in Scaler Count.', $
							'Correct Scaler Count entry.', $
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

function range_select, group

;	Enter step range positions

COMPILE_OPT STRICTARR
	if n_elements(group) lt 1 then return, 0

	xsize = 150
	ysize = 150
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title='Enter Step Range', /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='range_select_TLB', /base_align_center, $
					/modal, /floating, xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_right)

	s1base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s1base, value='Start Stage Position (mm):')
	start = widget_text( s1base, uname='start', /editable, value='    0.000', $
					uvalue='Enter the stage position at start of step image run.',scr_xsize=100, tracking=0)

	s2base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s2base, value='Stop Stage Position (mm):')
	stop = widget_text( s2base, uname='stop', /editable, value='    0.000', $
					uvalue='Enter the stage position at end of step image run.',scr_xsize=100, tracking=0)

	s3base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s3base, value='Scaler Counts per Line:')
	count = widget_text( s3base, uname='count', /editable, value='  100000', $
					uvalue='Enter the pre-scaler count value used to advance the stage stepping.',scr_xsize=100, tracking=0)

	s4base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s4base, value='Charge per Pulse:')
	charge = widget_combobox( s4base, value=['   1 pC','   0.1 pC','   0.01 pC','   0.001 pC'], uname='charge', tracking=0, $
;					notify_realize='OnRealize_range_select_charge', $
					uvalue='Charge per scaler count (pC).',scr_xsize=100)

	bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=5, /base_align_center)
	button = widget_button( bbase, value='Cancel', uname='cancel')
	button = widget_button( bbase, value='OK', uname='ok')

	p = ptr_new( {start:0.0, stop:0.0, count:0L, charge:1.0, error:0 })

	state = {	$
			p:		p, $					; pointer to selection
			count:	count, $				; widget ID of scaler count
			charge:	charge, $				; widget ID of charge per pulse droplist
			start:	start, $				; widget ID of start text
			stop:	stop $					; widget ID of stop text
			}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	xmanager, 'range_select', tlb					;, /no_block

	r = *p
	return, r
end
