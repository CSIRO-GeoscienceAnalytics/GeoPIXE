;
; IDL Event Callback Procedures
; spectrum_history_eventcb
;
; Generated on:	01/17/100 13:42.18
;
;-----------------------------------------------------------------
; TLB_KILL_REQUEST_EVENTS Callback Procedure.
;
;   {WIDGET_KILL_REQUEST, ID:0L, TOP:0L, HANDLER:0L }
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.
;-----------------------------------------------------------------

pro OnKill_spectrum_history, Event

COMPILE_OPT STRICTARR
cancel_notify, event.top

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

; if ptr_valid((*pstate).pstuff) then ptr_free, (*pstate).pstuff

widget_control, event.top, /destroy
end

;-----------------------------------------------------------------
; NOTIFY Callback Procedure.
;
;   {NOTIFY, ID:0L, TOP:0L, HANDLER:0L, TAG:t, POINTER:p, FROM:f }
;
;	TAG	string showing the notification name, as registered.
;	POINTER	pointer passed as a general argument (can be null).
;	FROM	id of widget sending the notify (or zero).
;-----------------------------------------------------------------

pro OnNotify_spectrum_history, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case event.tag of
	'spectra-changed': begin
		if ptr_valid( event.pointer) then begin
			(*pstate).p = event.pointer
			(*pstate).show = current_history_spec( (*pstate).p )
			Update_spectrum_history, pstate
		endif
		end

;	'image-show-element': begin
;		if ptr_valid( event.pointer) then begin
;			(*pstate).show = *event.pointer
;			Update_spectrum_history, pstate
;		endif
;		end

	else: begin
		print,'OnNotify_spectrum_history: unknown tag = ',event.tag
		end
endcase

finish:
end

;-----------------------------------------------------------------
; Tracking Callback Procedure.
;
;   {WIDGET_TRACKING, ID:0L, TOP:0L, HANDLER:0L, ENTER:0 }
;
;   ENTER is 1 if the tracking event is an entry event, and 0 if it
;       is an exit event.
;-----------------------------------------------------------------

pro OnTracking_spectrum_history, Event

COMPILE_OPT STRICTARR

end

;-----------------------------------------------------------------
; TLB_SIZE_EVENTS Callback Procedure.
;
;   {WIDGET_BASE, ID:0L, TOP:0L, HANDLER:0L, X:0, Y:0 }
;
;   The X and Y fields return the new width of the base, not
;       including any frame provided by the window manager.
;-----------------------------------------------------------------

pro OnSize_spectrum_history, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

; Use integer arithmetic for y to ensure a whole number of rows

x = event.x - (*pstate).xoffset
y = event.y - (*pstate).yoffset

;n = ((event.y - (*pstate).yoffset + 9)/ (*pstate).row_height) - 2
;y = (n + 2) * (*pstate).row_height

widget_control, (*pstate).list, scr_xsize=x, scr_ysize=y
end

;-----------------------------------------------------------------

pro OnRealize_spectrum_history, wWidget

COMPILE_OPT STRICTARR
top = tlb_id(wWidget)
child = widget_info( top, /child)
state = {	$
			p:			ptr_new(), $ ; pointer to image data
			plist:		ptr_new(), $ ; pointer to list array
			pindex:		ptr_new(), $ ; pointer to list index array
			show:		0, $		; current element to show
			list:		0L, $		; List ID

			row_height:	0, $		; table row height
			xoffset:	0, $		; offset in xsize for resize
			yoffset:	0 }			; offset in ysize for resize

;w = widget_info( wWidget, /row_heights)
w = 18
geom = widget_info( wWidget, /geometry)
tlb_geom = widget_info( top, /geometry)

state.list = wWidget
state.row_height = w[0]
state.yoffset = tlb_geom.ysize - geom.scr_ysize
state.xoffset = tlb_geom.xsize - geom.scr_xsize

widget_control, child, set_uvalue=ptr_new(state, /no_copy)
end

;-----------------------------------------------------------------
; List Select Item Callback Procedure.
;
;   {WIDGET_LIST, ID:0L, TOP:0L, HANDLER:0L, INDEX:0L, CLICKS:0L}
;
;   INDEX returns the index of the selected item. This index can be
;       used to subscript the array of names originally used to set
;       the widget's value. The CLICKS field returns either 1 or 2,
;       depending upon how the list item was selected. If the list
;       item is double-clicked, CLICKS is set to 2.
;-----------------------------------------------------------------

pro OnSelect_spectrum_history, Event

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
		warning,'OnSelect_spectrum_history',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, done
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

;case event.index of
; 	0:	begin				;
; 		end
;	else: begin
;		end
;endcase

done:
end

;-----------------------------------------------------------------
; Activate Button Callback Procedure.
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.
;-----------------------------------------------------------------

pro OnButton_spectrum_history_Close, Event

COMPILE_OPT STRICTARR
OnKill_spectrum_history, Event
end

;-----------------------------------------------------------------

function current_history_spec, p

COMPILE_OPT STRICTARR
if ptr_valid(p) eq 0 then return, 0
if n_elements(*p) eq 0 then return, 0

	n = n_elements(*p)
	for i=0L,n-1 do begin
		if (*(*p)[i]).show eq 1 then return, i
	endfor
	return, 0
end

;-----------------------------------------------------------------

pro Update_spectrum_history, pstate

COMPILE_OPT STRICTARR

list = ['']
p = (*pstate).p
if ptr_valid(p) eq 0 then goto, show
List = spectrum_details( p, show=(*pstate).show)

show:
	*(*pstate).plist = list
;	*(*pstate).pindex = index
	*(*pstate).pindex = indgen(n_elements(list))
	widget_control, (*pstate).list, set_value = list, set_combobox_select = (*pstate).show
return
end
;
; Empty stub procedure used for autoloading.
;
pro spectrum_history_eventcb
end

