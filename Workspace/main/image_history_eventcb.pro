;
; IDL Event Callback Procedures
; image_history_eventcb
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

pro OnKill_Image_history, Event

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

pro OnNotify_Image_history, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case event.tag of
	'images-changed': begin

		if ptr_valid( event.pointer) then begin
			(*pstate).p = event.pointer
			(*pstate).show = 0
			Update_Image_history, pstate
		endif
		end

	'image-show-element': begin

		if ptr_valid( event.pointer) then begin
			(*pstate).show = *event.pointer
			Update_Image_history, pstate
		endif
		end

	'image-display': begin

			Update_Image_history, pstate
		end

	else: begin
		print,'OnNotify_Image_history: unknown tag = ',event.tag
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

pro OnTracking_Image_history, Event

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

pro OnSize_Image_history, Event

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

pro OnRealize_Image_history, wWidget

COMPILE_OPT STRICTARR
top = tlb_id(wWidget)
child = widget_info( top, /child)
state = {	$
			p:			ptr_new(), $ ; pointer to image data
			plist:		ptr_new(), $ ; pointer to list array
			pindex:		ptr_new(), $ ; pointer to list index array
			show:		0, $		; current element to show
			list:		0L, $		; List ID
			stats:		0, $		; enable statistics

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

pro Update_Image_history, pstate

COMPILE_OPT STRICTARR

List = 'Bad image pointer'
index = [0]
p = (*pstate).p
if ptr_good(p,/struct) eq 0 then goto, show

list = image_details( p, show=(*pstate).show, stats=(*pstate).stats, index=index)

show:
	*(*pstate).plist = list
	*(*pstate).pindex = index
	widget_control, (*pstate).list, set_value = list, $
			set_combobox_select = (*pstate).show
return
end

;
; Empty stub procedure used for autoloading.
;
pro image_history_eventcb
end

