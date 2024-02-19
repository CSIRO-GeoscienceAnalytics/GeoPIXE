;
; Modal pop-up window to enter throttle parameters.
;
;	throttle = throttle2_select( group, tot)
;
;	Enter throttle parameters
;	'tot' 		total of spectrum
;	'group'		parent widget
;
;----------------------------------------------------------------------

pro throttle2_select_event, Event

COMPILE_OPT STRICTARR

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	uname = widget_info( event.id, /uname)

	case uname of

		'throttle2_select_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				(*(*pstate).p).error = 1
				widget_control, event.top, /destroy
				return
			endif
			end
		'time': begin
			widget_control, (*pstate).time, get_value=s
			if fnumeric(s) then (*(*pstate).p).time = float(strtrim(s,2))
			(*(*pstate).p).rate = (*(*pstate).p).total * (*(*pstate).p).detectors / (*(*pstate).p).time
			widget_control, (*pstate).rate, set_value=string((*(*pstate).p).rate*1.e-6)
			(*(*pstate).p).factor = (*(*pstate).p).rate / (*(*pstate).p).max
			widget_control, (*pstate).factor, set_value=string((*(*pstate).p).factor)
			end
		'detectors': begin
			widget_control, (*pstate).detectors, get_value=s
			if fnumeric(s) then (*(*pstate).p).detectors = round(float(strtrim(s,2)))
			(*(*pstate).p).rate = (*(*pstate).p).total * (*(*pstate).p).detectors / (*(*pstate).p).time
			widget_control, (*pstate).rate, set_value=string((*(*pstate).p).rate*1.e-6)
			(*(*pstate).p).factor = (*(*pstate).p).rate / (*(*pstate).p).max
			widget_control, (*pstate).factor, set_value=string((*(*pstate).p).factor)
			end
		'max': begin
			widget_control, (*pstate).max, get_value=s
			if fnumeric(s) then (*(*pstate).p).max = float(strtrim(s,2))*1.e6
			(*(*pstate).p).factor = (*(*pstate).p).rate / (*(*pstate).p).max
			widget_control, (*pstate).factor, set_value=string((*(*pstate).p).factor)
			end
		'rate': begin
			widget_control, (*pstate).rate, get_value=s
			if fnumeric(s) then (*(*pstate).p).rate = float(strtrim(s,2))*1.e6
			(*(*pstate).p).factor = (*(*pstate).p).rate / (*(*pstate).p).max
			widget_control, (*pstate).factor, set_value=string((*(*pstate).p).factor)
			end
		'ok': begin
			error = 0
;			widget_control, (*pstate).time, get_value=s
;			if fnumeric(s) then begin
;				(*(*pstate).p).time = float(strtrim(s,2))
;			endif else begin
;				warning,'throttle2_select_event','Illegal character in Time.'
;				error = 1
;			endelse
;			(*(*pstate).p).rate = (*(*pstate).p).total * (*(*pstate).p).detectors / (*(*pstate).p).time
			widget_control, (*pstate).max, get_value=s
			if fnumeric(s) then begin
				(*(*pstate).p).max = float(strtrim(s,2))*1.e6
			endif else begin
				warning,'throttle2_select_event','Illegal character in Max rate.'
				error = 1
			endelse
			widget_control, (*pstate).rate, get_value=s
			if fnumeric(s) then begin
				(*(*pstate).p).rate = float(strtrim(s,2))*1.e6
			endif else begin
				warning,'throttle2_select_event','Illegal character in Rate.'
				error = 1
			endelse
			(*(*pstate).p).factor = (*(*pstate).p).rate / (*(*pstate).p).max

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

function throttle2_select, group, tot

;	Enter throttle parameters
;	'tot' total of spectrum

COMPILE_OPT STRICTARR
	if n_elements(group) lt 1 then return, 0

	xsize = 150
	ysize = 150
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title='Enter Throttle Pars', /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='throttle2_select_TLB', /base_align_center, $
					/modal, /floating, xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_right)

;	s1base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
;	lab = widget_label( s1base, value='Spectrum total (one detector):')
;	total_id = widget_label( s1base, value=string(tot),scr_xsize=100)

;	s1bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
;	lab = widget_label( s1bbase, value='Total number of detectors:')
;	detectors_id = widget_text( s1bbase, uname='detectors', /editable, value='          32', $
;					uvalue='Enter the number of detectors enabled in the array.',scr_xsize=100, tracking=0)

;	s2base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
;	lab = widget_label( s2base, value='Spectrum time (sec):')
;	time_id = widget_text( s2base, uname='time', /editable, value='          0.0', $
;					uvalue='Enter the integrated time (sec) for the pilot spectrum.',scr_xsize=100, tracking=0)

	s3base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s3base, value='Total count rate (MHz):')
	rate_id = widget_text( s3base, uname='rate', /editable, value='          5.0', $
					uvalue='Enter the expected total count-rate (M/s).',scr_xsize=100, tracking=0)

	s4base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s4base, value='Maximum count rate (MHz):')
	max_id = widget_text( s4base, uname='max', /editable, value='          2.0', $
					uvalue='Enter the maximum allowed count-rate (MHz).',scr_xsize=100, tracking=0)

	s5base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s5base, value='Throttle factor:')
	factor_id = widget_label( s5base, value='                ',scr_xsize=100)

	bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=5, /base_align_center)
	button = widget_button( bbase, value='Cancel', uname='cancel')
	button = widget_button( bbase, value='OK', uname='ok')

	p = ptr_new( {total:tot, time:0.0, max:2.e6, rate:5.e6, factor:2.5, detectors:96, error:0 })

	state = {	$
			p:		p, $					; pointer to selection
;			total:	total_id, $				; widget ID of spectrum total
;			detectors:	detectors_id, $		; widget ID of number of detectors
;			time:	time_id, $				; widget ID of acqusition time
			max:	max_id, $				; widget ID of maximum count rate text
			rate:	rate_id, $				; widget ID of count rate text
			factor:	factor_id $				; widget ID of throttle factor text
			}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	xmanager, 'throttle2_select', tlb					;, /no_block

	r = *p
	return, r
end
