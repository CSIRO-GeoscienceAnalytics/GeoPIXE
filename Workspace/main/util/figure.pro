pro figure_event, event

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
		warning,'figure_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
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
				'figure-file': begin

					end
				else:
			endcase
			return
			end
		'WIDGET_KILL_REQUEST': begin
			print,'Kill request figure ...'
			goto, kill
			end
		else:
	endcase
	
	uname = widget_info( event.id, /uname)
	case uname of
		'figure_TLB': begin
;			OnSize_Image_Table, Event
			end
		else:
	endcase
	return

bad_state:
	warning,'figure_event',['STATE variable has become ill-defined.','Abort ...'],/error
	goto, kill

kill:
	cancel_notify, event.top
	if ptr_good(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

die:
	widget_control, event.top, /destroy
	return
end

;------------------------------------------------------------------------------------------

pro figure, file, tlb=tlb, group=group, title=title

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
		warning,'figure',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(file) eq 0 then file='wizard/wizard_depth-depth-curve.png'
if n_elements(title) eq 0 then title='Figure'
if n_elements(group) eq 0 then begin
	no_group = 1
	group = 0L
endif else no_group=0
startupp

case !version.os_family of
	'MacOS': begin
		end
	'unix': begin
		end
	else: begin
		end
endcase
		
; 	top-level base

tlb = widget_base( /column, title=title, group=group, /TLB_KILL_REQUEST_EVENTS, $
					uname='figure_TLB', /base_align_center, xpad=0, ypad=0, space=0 )
tbase = widget_base( tlb, /column, xpad=2, ypad=2, space=2, /base_align_center)

fig = picture_button( tbase, file, error=err)
if err or (widget_info(fig, /valid) eq 0) then begin
	widget_control, tlb, /destroy
	return
endif

state = {	file:		file, $				; figure file-name

			tlb:		tlb, $				; top-level base ID
			group:		group $				; group leader ID
		}
			
child = widget_info( tlb, /child)
pstate = ptr_new(state, /no_copy)
widget_control, child, set_uvalue=pstate
widget_control, tlb, /realize

if no_group then begin
	register_notify, tlb, 'figure-file'
endif else begin
	register_notify, tlb, 'figure-file', from=Group
endelse

xmanager, 'figure', tlb, /no_block
return
end
		