;
; Modal pop-up window to select DA export parameters.
;
;	select = export_da_select( group, file=file, big_endian=big_endian, mode=mode)
;
;	Enter export_da parameters
;	'group'			parent widget
;	'big_endian' 	to select big endian binary format target processor (e.g. VME)
;	'mode' 			select format mode
;;
;----------------------------------------------------------------------

pro export_da_select_event, Event

COMPILE_OPT STRICTARR

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			else:
		endcase
		end
	'WIDGET_TRACKING': begin
		widget_control, event.id, get_uvalue=s
		if size(s,/tname) eq 'STRING' then begin
			if event.enter eq 1 then begin
;				widget_control, (*pstate).help, set_value=s
			endif else begin
;				widget_control, (*pstate).help, set_value=' '
			endelse
		endif
		goto, finish
		end
	'WIDGET_TIMER': begin
	;	print,' got a timer event; update text reads ...'
		end
	else:
endcase

	uname = widget_info( event.id, /uname)

	case uname of

		'export_da_select_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				(*(*pstate).p).error = 1
				widget_control, event.top, /destroy
				return
			endif
			end
		'pileup': begin
			widget_control, (*pstate).pileup, get_value=s
			(*(*pstate).p).pileup = strtrim(s,2)
			end
		'pileup-button': begin
			file = ''
			path = extract_path( (*(*pstate).p).file)
			if lenchr((*(*pstate).p).pileup) gt 0 then begin
				file = find_file2( (*(*pstate).p).pileup)
				path = extract_path( file[0])
			endif
			F = file_requester( /read, filter = '*.txt', $
					/must_exist, group=event.top, path=path, $
					title='Select Pileup limits file', /fix_filter)
			if F ne '' then begin
				n = lenchr(F)
				k = lenchr(strip_path(F))
				widget_control, (*pstate).pileup, set_value=F, set_text_select=[n-k,k]
				widget_control, (*pstate).pileup, set_value=F, set_text_select=[n-1,0]
				(*(*pstate).p).pileup = F
			endif
			end
		'throttle': begin
			widget_control, (*pstate).throttle, get_value=s
			(*(*pstate).p).throttle = strtrim(s,2)
			end
		'throttle-button': begin
			file = ''
			path = extract_path( (*(*pstate).p).file)
			if lenchr((*(*pstate).p).throttle) gt 0 then begin
				file = find_file2( (*(*pstate).p).throttle)
				path = extract_path( file[0])
			endif
			F = file_requester( /read, filter = '*.txt', $
					/must_exist, group=event.top, path=path, $
					title='Select throttle factors file', /fix_filter)
			if F ne '' then begin
				n = lenchr(F)
				k = lenchr(strip_path(F))
				widget_control, (*pstate).throttle, set_value=F, set_text_select=[n-k,k]
				widget_control, (*pstate).throttle, set_value=F, set_text_select=[n-1,0]
				(*(*pstate).p).throttle = F
			endif
			end
		'linear': begin
			widget_control, (*pstate).linear, get_value=s
			(*(*pstate).p).linear = strtrim(s,2)
			end
		'linear-button': begin
			file = ''
			path = extract_path( (*(*pstate).p).file)
			if lenchr((*(*pstate).p).linear) gt 0 then begin
				file = find_file2( (*(*pstate).p).linear)
				path = extract_path( file[0])
			endif
			F = file_requester( /read, filter = ['*.linear.var','*.linear'], $
					/must_exist, group=event.top, path=path, $
					title='Select Linearization file', /fix_filter)
			if F ne '' then begin
				n = lenchr(F)
				k = lenchr(strip_path(F))
				widget_control, (*pstate).linear, set_value=F, set_text_select=[n-k,k]
				widget_control, (*pstate).linear, set_value=F, set_text_select=[n-1,0]
				(*(*pstate).p).linear = F
			endif
			end
		'mode': begin
			(*(*pstate).p).mode = event.index
			end
		'options': begin
			(*(*pstate).p).options[event.value] = event.select
			end
		'ok': begin
			widget_control, (*pstate).pileup, get_value=s
			if fnumeric(s) then begin
				(*(*pstate).p).pileup = strtrim(s,2)
			endif
			widget_control, (*pstate).throttle, get_value=s
			if fnumeric(s) then begin
				(*(*pstate).p).throttle = strtrim(s,2)
			endif
			widget_control, event.top, /destroy
			return
			end
		'cancel': begin
			widget_control, event.top, /destroy
			(*(*pstate).p).error = 1
			return
			end
		else:
	endcase

finish:
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_export_da_mode, wWidget

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).mode
endif
end

;-----------------------------------------------------------------

function export_da_select, group, big_endian=big_endian, mode=mode, pileup=pileup, throttle=throttle, linear=linear

;	Select export_da parameters
;	'big_endian' to select big endian binary format target processor (e.g. VME)
;	'mode' select format mode
;	
;	'linear' suppressed for now, as we don't have real-time tools to support the 
;	v2 Maia Linearization scheme, which uses per chip Linearization functions.

COMPILE_OPT STRICTARR
	if n_elements(group) lt 1 then return, {error:1}
	if n_elements(big_endian) lt 1 then big_endian=1
	if n_elements(mode) lt 1 then mode=1
	if n_elements(pileup) lt 1 then pileup=''
	if n_elements(throttle) lt 1 then throttle=''
	if n_elements(linear) lt 1 then linear=''

	xsize = 150
	ysize = 150
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title='Enter Export Options', /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='export_da_select_TLB', /base_align_center, $
					/modal, /floating, xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_right)

	s1base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s1base, value='Output format:')
	mode_id = widget_combobox( s1base, value=['Maia DMT ASCII','DMX binary','iThemba DMM binary'], uname='mode', /tracking, $
					notify_realize='OnRealize_export_da_mode', $
					uvalue='Select the export DA output format.',scr_xsize=240)

	s2base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s2base, value='Binary Format options:')
	options_id = cw_bgroup2( s2base, ['Big endian'], /column, xpad=0, ypad=0, space=0, $
					/return_index, /tracking, xsize=200, $
					uname='options', set_value=[big_endian], /nonexclusive, $
					uvalue=['For binary formats, use big endian format for VME target processor. Disable for PC processor.'])

	s3base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s3base, value='Pileup limits file:')
	pileup_id = widget_text( s3base, uname='pileup', /editable, value=pileup, $
					uvalue='Enter the file-name for the pileup limits for pileup rejection.',scr_xsize=200, tracking=0)
	button = widget_button( s3base, value='Load', uname='pileup-button')

	s4base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
	lab = widget_label( s4base, value='Throttle factors file:')
	throttle_id = widget_text( s4base, uname='throttle', /editable, value=throttle, $
					uvalue='Enter the file-name for the throttling factors for high count rates.',scr_xsize=200, tracking=0)
	button = widget_button( s4base, value='Load', uname='throttle-button')

;	s5base = widget_base( tbase, /row, xpad=0, ypad=0, space=2, /base_align_center)
;	lab = widget_label( s5base, value='Linearization file:')
;	linear_id = widget_text( s5base, uname='linear', /editable, value=linear, $
;					uvalue='Enter the file-name for the E signal gain linearization correction function.',scr_xsize=200, tracking=0)
;	button = widget_button( s5base, value='Load', uname='linear-button')

	bbase = widget_base( tbase, /row, xpad=0, ypad=0, space=5, /base_align_center)
	button = widget_button( bbase, value='Cancel', uname='cancel')
	button = widget_label( bbase, value='         ')
	button = widget_button( bbase, value='OK', uname='ok')

;	p = ptr_new( {mode:mode, options:[big_endian], pileup:pileup, throttle:throttle, linear:linear, error:0 })
	p = ptr_new( {mode:mode, options:[big_endian], pileup:pileup, throttle:throttle, error:0 })

	state = {	$
			p:		p, $					; pointer to selection
			mode:	mode_id, $				; widget ID of format type droplist
			options: options_id, $			; widget ID of options
			pileup:	pileup_id, $			; widget ID of pileup file-name
;			linear:	linear_id, $			; widget ID of linearization file-name
			throttle: throttle_id $			; widget ID of throttle file-name
			}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize
	
	set_widget_text, pileup_id, pileup
	set_widget_text, throttle_id, throttle
;	set_widget_text, linear_id, linear

	xmanager, 'export_da_select', tlb					;, /no_block

	r = *p
	return, r
end
