pro warning_popup_event, event

COMPILE_OPT STRICTARR
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
		warning,'warning_popup_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return				; goto, kill
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, bad_state
if ptr_valid(pstate) eq 0 then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state
p = (*pstate).p

case tag_names( event,/structure) of
	'WIDGET_TIMER': begin
		goto, kill
		end
	'WIDGET_KILL_REQUEST': begin
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'ok-button': begin
		goto, kill
		end
	'cancel-button': begin
		(*p).cancel = 1
		goto, kill
		end
	else:
endcase

finish:
	return

bad_state:
	warning,'warning_popup_event',['STATE variable has become ill-defined.','Abort warning popup.'],/error
	goto, kill

kill:
	ErrorNo = 0
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		goto, die
	endif
	widget_control, event.top, /destroy
	if (*pstate).local then widget_control, (*pstate).group, /destroy

die:
	return
end

;-------------------------------------------------------------------------------------

pro warning_popup, routine, messi, error=error, info=info, cancel=cancel, $
					group_leader=group, timeout=timeout, debug=debug, output=output

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
			warning,'warning_popup',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif else on_error,0
	if n_elements(group) lt 1 then group=0L
	if n_elements(debug) lt 1 then debug=0
	if n_elements(error) lt 1 then error=0
	if n_elements(info) lt 1 then info=0
	if n_elements(messi) lt 1 then messi='<unknown message>'
	if n_elements(timeout) lt 1 then timeout=0.
	mess = messi
	if size(mess,/tname) ne 'STRING' then mess=string(mess)

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

	prefix = 'WARNING in'
	if error then prefix = 'ERROR in'
	if info then prefix = 'Information from'
	
	title = prefix + ' routine ' + routine
	
	gprint,level=2, output=output, '========================= '+title+': ', mess

	case !version.os_family of
		'MacOS': begin
			ch_scale = 1.2
			retain = 2
			end
		'unix': begin
			ch_scale = 1.25
			retain = 2
			end
		else: begin
			ch_scale = 1.0
			retain = 1
			end
	endcase

	nl = n_elements(mess)
	ns = 1
	for i=0,nl-1 do ns = ns > strlen(mess[i])
	w = ns * !d.x_ch_size * ch_scale
	width = 300. > (w + 50.)
	height = 120 + 50*nl
	device, get_screen_size=screen
	xoff = screen[0]/2 - width/2
	yoff = screen[1]/2 - height/2
	
	tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, $
						group_leader=group, uname='warning-popup-tlb', modal=modal, floating=floating, $
						/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=1 ,YPAD=4, /base_align_center, $
						xoffset=xoff, yoffset=yoff)
	
	tbase = widget_base( tlb, /column, xpad=1, ypad=1, space=2, /base_align_center, /align_center)
	
	text = widget_text( tbase, value=mess, scr_xsize=width, ysize=nl, uname='text', tracking=0, frame=0)	;, /wrap)
	
	bbase = widget_base( tbase, /row, xpad=1, ypad=1, space=2, /base_align_center, /align_center)
	
	ok_Button = Widget_Button( bbase, UNAME='ok-button', /ALIGN_CENTER, VALUE='OK', tracking_events=0)
	
	if arg_present(cancel) then begin
		label = widget_label( bbase, value='        ', /align_center)

		cancel_Button = Widget_Button( bbase, UNAME='cancel-button', /ALIGN_CENTER, VALUE='Cancel', tracking_events=0)
	endif else cancel_button=0L
	
	p = ptr_new( {cancel: 0} )
	
	state = { $
			text:			mess, $					; warning text
			on:				1, $					; colour button ON
			local:			local, $				; local group leader
			group:			group, $				; group_leader
			p:				p, $					; pointer to selection
			ok_button:		ok_button, $			; OK button ID
			cancel_button:	cancel_button $			; cancel button ID
			}
	
	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize
	
	if timeout gt 0. then begin
		widget_control, ok_button, timer=timeout	; start popup timeout timer
	endif

	register_notify, tlb, ['cancel' $				; cancel message
						], from=group
	
	if debug then begin
		xmanager, 'warning_popup', tlb, /no_block
	endif else begin
		xmanager, 'warning_popup', tlb
	endelse

	cancel = (*p).cancel
	return
end




