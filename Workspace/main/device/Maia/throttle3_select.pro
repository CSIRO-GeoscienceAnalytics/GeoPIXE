;
; Non-modal pop-up window to manipulate throttle parameters.
;
;	throttle = throttle3_select( group, tot)
;
;	Enter throttle parameters
;	'tot' 		total of spectrum
;	'group'		parent widget
;
;----------------------------------------------------------------------

pro throttle3_select_event, Event

COMPILE_OPT STRICTARR
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
		warning,'throttle3_select_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'spectrum-view': begin				; new View range
				if ptr_valid( event.pointer) then begin
					cal_ab, (*(*pstate).pspec).cal, a,b,u, error=error
;					if error eq 0 then begin
						(*pstate).remote_view[0] = ((*event.pointer).low*a + b) > 0.32
						(*pstate).remote_view[1] = ((*event.pointer).high*a + b) > (*pstate).remote_view[0]
						
						(*pstate).view = (*pstate).remote_view
						widget_control, (*pstate).low_e_id, set_value=str_tidy((*pstate).view[0])
						widget_control, (*pstate).high_e_id, set_value=str_tidy((*pstate).view[1])
;					endif
				endif
				goto, finish
				end
			'spectrum-fit': begin				; new spectrum displayed
				if ptr_valid( event.pointer) then begin
					if ptr_valid((*event.pointer).pspec) then begin
						(*pstate).pspec = (*event.pointer).pspec
					endif
				endif
				goto, finish
				end
			else: begin
				goto, finish
				end
		endcase
		end
;	'WIDGET_TRACKING': begin
;		widget_control, event.id, get_uvalue=s
;		if size(s,/tname) eq 'STRING' then begin
;			if event.enter eq 1 then begin
;				widget_control, (*pstate).help, set_value=s
;			endif else begin
;				if (*pstate).mode eq 0 then begin
;					widget_control, (*pstate).help, set_value='Click on a line to mark it on the spectrum. ' + $
;							'Adjust lines visible with Threshold slider.'
;				endif else begin
;					widget_control, (*pstate).help, set_value='LEFT click an element to mark its lines on the spectrum. ' + $
;							'Use "Filter", "Detector" to modify relative intensities. ' + $
;							'RIGHT click will enable the element in "X-ray Spectrum Fit". '
;				endelse
;			endelse
;		endif
;		goto, finish
;		end
	'WIDGET_KILL_REQUEST': begin
		goto, kill
		end
	else:
endcase

	uname = widget_info( event.id, /uname)

	case uname of

		'throttle-factor': begin
			widget_control, (*pstate).throttle_id, get_value=s
			if fnumeric(s) eq 0 then goto, bad_number
			(*pstate).factor = float(s)
			end
			
		'low-e': begin
			widget_control, (*pstate).low_e_id, get_value=s
			if fnumeric(s) eq 0 then goto, bad_number
			(*pstate).view[0] = float(s)
			end
		'high-e': begin
			widget_control, (*pstate).low_e_id, get_value=s
			if fnumeric(s) eq 0 then goto, bad_number
			(*pstate).view[1] = float(s)
			end
			
		'use-view': begin
			(*pstate).view = (*pstate).remote_view
			widget_control, (*pstate).low_e_id, set_value=str_tidy((*pstate).view[0])
			widget_control, (*pstate).high_e_id, set_value=str_tidy((*pstate).view[1])
			end
			
		'throttle-view': begin
			widget_control, (*pstate).throttle_id, get_value=s
			if fnumeric(s) eq 0 then goto, bad_number
			p = (*pstate).pspec
			siz = (*p).size < n_elements( *(*pstate).pthrottle)
			(*pstate).factor = float(strtrim(s,2))
			cal_ab, (*p).cal, a,b,u, error=error
;			if error then goto, finish
			view = round(((*pstate).view-b)/a)
			throttle = build_throttle( p, (*pstate).factor, view=view)
			(*(*pstate).pthrottle)[view[0]:view[1]] = throttle[view[0]:view[1]]
			(*(*pstate).pthrottle) = (*(*pstate).pthrottle) > 1
			
			if max(throttle[view[0]:view[1]]) ge 16 then warning,'throttle_spectrum',['Maximum 4 bit throttle exceeded','Truncate Throttle back to 4 bits.']
			if max(throttle[view[0]:view[1]]) eq 1 then warning,'throttle_spectrum','no throttling needed'
			(*(*pstate).pthrottle) = (*(*pstate).pthrottle) < 15

			if (*p).n_fit gt 0 then begin
				for j=0L,(*p).n_fit-1 do begin
					free_spectrum, (*p).fit[j]
				endfor
			endif
			
			sfit = define(/spectrum)
			sfit.source = (*p).source
			sfit.cal = (*p).cal
			sfit.label = 'Throttle factor spectrum'
			sfit.comment = 'Throttle spectrum overlay'
			sfit.size = siz
			sfit.data = ptr_new( (*(*pstate).pthrottle)[0:siz-1])
			
			(*p).fit[0] = ptr_new(sfit, /no_copy)	; overlay the throttle array
			
			sfit = define(/spectrum)
			sfit.source = (*p).source
			sfit.cal = (*p).cal
			sfit.label = 'Throttled down spectrum'
			sfit.comment = 'Throttled spectrum'
			sfit.size = siz
			sfit.data = ptr_new( (*(*p).data)[0:siz-1]/float((*(*pstate).pthrottle)[0:siz-1]) )
			
			(*p).fit[1] = ptr_new(sfit)				; overlay the throttled spectrum
			(*p).n_fit = 2
			
			print,'    View: Total = ',total((*(*p).data)[view[0]:view[1]]),'  throttled = ',total((*sfit.data)[view[0]:view[1]])
			print,'Spectrum: Total = ',total(*(*p).data),'  throttled = ',total(*sfit.data)
			
			notify, (*pstate).update_notify, from=event.top 
			end
			
		'save': begin
			p = (*pstate).pspec
			siz = (*p).size < n_elements( *(*pstate).pthrottle)
			save_file = strip_file_ext(strip_path((*p).file))+'.throttle.var'
			path = extract_path((*p).file)
			
			F = file_requester( /write, filter = ['*.throttle.var','*.txt'], path=path, $
						title='Save Throttle Spectrum', file = save_file, group=event.top, /fix_filter)
			if F ne '' then begin
				close, 1
				openw, 1, F
				printf, 1, '# Throttle file'
				printf, 1, '# Generated by Throttle3_select, '+systime()
				printf, 1, '# Source: ' + (*p).file + ' ...'
				printf, 1, '#'
				for j=0L,siz-1 do begin
					if (*(*pstate).pthrottle)[j] ne 1 then begin
						s = 'throttle.energy[' + str_tidy(j) + '].factor'
						printf, 1, s, (*(*pstate).pthrottle)[j], format='(A,1x,I4)'
					endif
				endfor
				printf, 1, '#end'
				close, 1
 
 				widget_control, event.top, /destroy
				return
			endif
			end
			
		'cancel': begin
			widget_control, event.top, /destroy
			return
			end
		else:
	endcase

finish:
	return
	
bad_number:
	warning,'throttle3_select_event','Illegal number entered.',/error
	goto, finish
	
bad_state:
	warning,'throttle3_select_event',['STATE variable has become ill-defined.','Abort identify2.'],/error
	goto, kill

kill:
	print,'Kill throttle3_select ...'
	cancel_notify, event.top

	if n_elements(pstate) lt 1 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	ptr_free, (*pstate).pthrottle

die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return	
end

;-----------------------------------------------------------------

pro throttle3_select, pspec, group_leader=group, tlb=tlb, view=view, $
						update_notify=update_notify

;	Enter throttle parameters
;	'pspec'		pointer to spectrum
;	'view'		view range (energy)

COMPILE_OPT STRICTARR
ErrorNo = 0
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
		warning,'throttle3_select',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
	if n_elements(pspec) lt 1 then return
	if ptr_valid(pspec) eq 0 then return
	if n_elements(group) lt 1 then return
	if n_elements(view) lt 1 then view=[1.0,100.]
	if n_elements(update_notify) lt 1 then update_notify='spectrum-display'

	cal_ab, (*pspec).cal, a,b,u, error=error
	if error then begin
		view_low = 'View:  Low:'
		view_high = '   High:'
		range_text = 'Enter factor for this range:'
	endif else begin
		view_low = 'View:  low E:'
		view_high = '   high E:'
		range_text = 'Enter factor for this E range:'
	endelse
	
	xsize = 150
	ysize = 150
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title='Set Throttle in View', /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='throttle3_select_TLB', /base_align_center, xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center)

	s2base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s2base, value=view_low)
	low_e_id = widget_text( s2base, uname='low-e', /editable, value=str_tidy(view[0]), $
					uvalue='Enter the low View Energy (keV) or click "Read View".',scr_xsize=50, tracking=0)
	lab = widget_label( s2base, value=view_high)
	high_e_id = widget_text( s2base, uname='high-e', /editable, value=str_tidy(view[1]), $
					uvalue='Enter the high View Energy (keV) or click "Read View".',scr_xsize=50, tracking=0)
;	lab = widget_label( s2base, value=' ')
;	button = widget_button( s2base, value='Use View', uname='use-view')

	s1bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=2)
	lab = widget_label( s1bbase, value=range_text)
	throttle_id = widget_text( s1bbase, uname='throttle-factor', /editable, value='10.0', $
					uvalue='Enter the throttle factor to apply to the View range.',scr_xsize=80, tracking=0)
	lab = widget_label( s1bbase, value=' ')
	button = widget_button( s1bbase, value=' Throttle ', uname='throttle-view')

	bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=50, /base_align_center)
	button = widget_button( bbase, value='Cancel', uname='cancel')
	button = widget_button( bbase, value='Save Throttle spectrum', uname='save')

	state = {	$
			pspec:			pspec, $				; pointer to spectrum
			pthrottle:		ptr_new(intarr(4096)), $	; throttle factors spectrum
			factor:			10.0, $					; throttle factor 
			view:			view, $					; View range
			remote_view:	view, $					; remote View range changed in spectrum display
			update_notify:	update_notify, $		; notify spectrum update string
			throttle_id:	throttle_id, $			;widget ID of throttle factor
			low_e_id:		low_e_id, $				;widget ID of View low E
			high_e_id:		high_e_id $			;widget ID of View high E
			}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	register_notify, tlb, ['spectrum-view','spectrum-fit'], from=group
	
	xmanager, 'throttle3_select', tlb, /no_block

	return
end
