pro alarm_popup_event, event

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
		warning,'alarm_popup_event',['IDL run-time error caught.', '', $
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

case tag_names( event,/structure) of
	'NOTIFY': begin
		case event.tag of
			'cancel': begin
				goto, kill
				end
			else:
		endcase
		goto, finish
		end
	'WIDGET_TIMER': begin
		u = widget_info( event.id, /uname)
		case u of
			'alarm-button': begin
				(*pstate).on = 1 - (*pstate).on
				col = (*pstate).on ? 2 : 3
				widget_control, event.id, set_value = {select:col}	
				widget_control, event.id, timer=1.0
				end
			else:
		endcase
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		goto, kill
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'alarm-button': begin
		goto, kill
		end
	else:
endcase

finish:
	return

done:
	goto, kill

bad_state:
	warning,'alarm_popup_event',['STATE variable has become ill-defined.','Abort alarm popup.'],/error
	goto, kill

kill:
	ErrorNo = 0
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		goto, die
	endif
	widget_control, event.top, /destroy

die:
	return
end

;-------------------------------------------------------------------------------------

pro alarm_popup, group=group, title=title, message=mess, tlb=tlb, debug=debug

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
		warning,'alarm_popup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif else on_error,0
if n_elements(debug) lt 1 then debug=0
if n_elements(title) lt 1 then title='Alarm Condition'
if n_elements(mess) lt 1 then mess='Interlock Failure!'

common c_logging_comms, logging_comms
if n_elements(logging_comms) lt 1 then logging_comms=0
log_message, logging_comms, type='WARNING', title+': '+strjoin( mess, ', ')

grey = spec_colour('l.grey')
green = spec_colour('green')
yellow = spec_colour('yellow')
lblue = spec_colour('l.blue')
red = spec_colour('red')
orange = spec_colour('orange')
dgreen = spec_colour('d.green')
blue = spec_colour('blue')
violet = spec_colour('violet')
pink = spec_colour('pink')
black = spec_colour('black')
white = spec_colour('white')

;	  select: 0     1      2      3
colours = [ grey, green, violet, violet]		; for ??
colours2 = [grey, green, yellow, red]			; for "buttons"

width = 600
message = mess[0]
ns = strlen(message)
cs = 5.
ts = 4.
if ns gt 32 then begin
	width = fix(width * 2)
	cs = cs * 32./float(ns)
	ts = (ts * 32./float(ns)) > 1.5
endif else if ns gt 16 then begin
	width = fix(width * (ns/16.))
endif
height = 180
device, get_screen_size=screen
xoff = screen[0]/2 - width/2
yoff = screen[1]/2 - height/2

tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, $
					group_leader=group, uname='maia-alarm-tlb', $
					/TLB_SIZE_EVENTS, SPACE=2 ,XPAD=1 ,YPAD=4, /base_align_center, $
					xoffset=xoff, yoffset=yoff)

tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center, /align_center)

alarm_button = state_button( tbase, value=message, uname='alarm-button', xsize=width, ysize=height, $
					/freeze, select=2, colours=colours2, charsize=cs, charthick=ts, n_states=4, alt=0 )

state = { $
		message:		message, $				; alarm message
		on:				1, $					; colour button ON
		alarm_button:	alarm_button $			; alarm button ID
		}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize

widget_control, alarm_button, timer=1.0			; start alarm flash timer

register_notify, tlb, ['cancel' $				; cancel message
					], from=group

xmanager, 'alarm_popup', tlb, /no_block
return
end




