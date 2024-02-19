pro command_event, event

common c_global, catch_errors_on
ErrorNo = 0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		ErrArray = ['command_event: Error Caught','Error Number:'+strtrim(!error,2),!Err_String]
		a = dialog_message(ErrArray, /Error)
		goto, finish
	endif
endif
  widget_control, hourglass=0

	child = widget_info( event.handler, /child)
	widget_control, child, get_uvalue=pstate
	type = tag_names(event,/structure)

	action = widget_info( event.id, /uname)

	case action of
		'Command':	begin
			widget_control, (*pstate).command, get_value=t
			if( n_elements(t) gt 0) then begin
				if strlen(t[0]) gt 0 then begin
				;	print,'command=',t[0]
					ErrorNo = execute(t[0])
					if (ErrorNo eq 0) then begin
						Catch, /cancel
						ErrArray = ['command: Error Caught','Error Number:'+strtrim(!error,2),!Err_String]
						a = dialog_message(ErrArray, /Error)
						goto, finish
					endif
				endif
			endif
			end
	endcase

finish:
	widget_control, hourglass=0
	return
end
; ---------------------------------------------------------------------------------
;
; ---- widget definition module ----

pro command, group=group

; note: the "group" keywords and arguments ensure that this widget dies when the
; parent widget dies.

@icommon.def
if n_elements(catch_errors_on) lt 1 then catch_errors_on = 1

ErrorNo = 0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		ErrArray = ['command: Error Caught','Error Number:'+strtrim(!error,2),!Err_String]
		a = dialog_message(ErrArray, /Error)
		return
	endif
endif

tlb = widget_base(column=1, title='IDL Command', group_leader=group)

base1 = widget_base(tlb, column=1, /align_center)

command = widget_text(base1, /editable,  xsize=140, /align_center, uname='Command')

widget_control, tlb, /realize

state = { $
		command:			command, $				; command entry/all_events,
		list:				0 }				 		;

widget_control, base1, set_uvalue=ptr_new(state, /no_copy)

xmanager, 'command', tlb, Event='command_event', /no_block
end

; ---------------------------------------------------------------------------------
