;-----------------------------------------------------------------

; BUTTON_EVENTS Callback Procedure.
;
;   {WIDGET_DRAW, ID:0L, TOP:0L, HANDLER:0L, TYPE: 0, X:0, Y:0,
;       PRESS:0B, RELEASE:0B, CLICKS:0}
;
;   TYPE returns a value that describes the type of draw widget
;       interaction that generated an event: 0 - Button Press, 1 -
;       Button Release, 2 - Motion, 3 - Viewport Moved, 4 -
;       Visibility Changed (Expose)
;   PRESS returns a bitmask of which button was pressed:
;		1 left, 2 centre, 4 right.
;--------------------------------------------------------------------

pro OnButton_simple_image, Event, return_event

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
		warning,'OnButton_simple_image',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

return_event = 0
possible = ['DOWN','UP','MOTION','SCROLL']
if event.type gt 2 then begin
	print,'OnButton_simple_image: Found illegal button type'
	return
endif
;print,'event: type=',event.type, '  press=',event.press

child = widget_info( event.handler, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, finish
if ptr_valid(pstate) eq 0 then goto, finish
if size(*pstate,/tname) ne 'STRUCT' then goto, finish
p = (*pstate).pmark
if ptr_valid(p) eq 0 then goto, finish

case possible[ event.type] of
	'DOWN': begin
		(*pstate).left_button = 0
		(*pstate).right_button = 0
		if (event.press AND 1B) ne 0 then begin
			print,'OnButton_simple_image: left mouse button press ...'
			(*pstate).left_button = 1
		endif
		if (event.press AND 4B) ne 0 then begin
			print,'OnButton_simple_image: right mouse button press ...'
			(*pstate).right_button = 1
		endif
		(*pstate).id = -1
		pixel_simple_image_to_xy, pstate, event.x,event.y, x,y
		print,'zoomed mouse x,y=',x,y,', event.x,y=',event.x,event.y

		if (*pstate).right_button then begin
;			if near_simple_image_xy( x,y, (*p).x,(*p).y) eq -1 then begin
				clear_simple_image_all_markers, pstate
				goto, finish
;			endif
		endif

		if (*p).present then begin
			(*pstate).id = near_simple_image_xy( x,y, (*p).x,(*p).y)
			print,'near ',(*pstate).id
			if (*pstate).id eq -1 then goto, finish
		endif else if (*pstate).right_button then begin
			goto, finish
		endif else begin
			(*pstate).id = -1
			(*p).x = [x,x+1,x+1,x, x]
			(*p).y = [y,y,y-1,y-1, y]
			wset, (*pstate).wid
			plot_simple_image_box, pstate
		endelse

		widget_control, event.id, draw_motion_events=1
		end

	'MOTION': begin
		if (*pstate).left_button eq 0 then begin		; button off, but Motion On means Cursor
			pixel_simple_image_to_xy, pstate, event.x,event.y, x,y
	;		print,'Motion: event.x,y = ',event.x,event.y,' x,y= =',long(round(x)), long(round(y))

			return_event = {SIMPLE_IMAGE_CURSOR, ID:event.handler, TOP:event.top, $
							HANDLER:0L, CURSOR:{SIMLE_IMAGE_CURSOR_XY, x:long(round(x)), y:long(round(y))} }
			goto, finish
		endif
		
		xc = 0
		yc = 0
		pixel_simple_image_to_xy, pstate, event.x,event.y, x,y

;		print,'motion: x,y= ', x,y
		clear_simple_image_box, pstate
		case (*pstate).id of
			-1: begin						; initial drag out
				(*p).x[1:2] = x
				(*p).y[0:1] = y
				xc = mean((*p).x[0:3])
				yc = mean((*p).y[0:3])
				(*p).x[4] = xc
				(*p).y[4] = yc
				end
			0: begin
				(*p).x[0] = x  &  (*p).x[3] = x
				(*p).y[0:1] = y
				xc = mean((*p).x[0:3])
				yc = mean((*p).y[0:3])
				(*p).x[4] = xc
				(*p).y[4] = yc
				end
			1: begin
				(*p).x[1:2] = x
				(*p).y[0:1] = y
				xc = mean((*p).x[0:3])
				yc = mean((*p).y[0:3])
				(*p).x[4] = xc
				(*p).y[4] = yc
				end
			2: begin
				(*p).x[1:2] = x
				(*p).y[2:3] = y
				xc = mean((*p).x[0:3])
				yc = mean((*p).y[0:3])
				(*p).x[4] = xc
				(*p).y[4] = yc
				end
			3: begin
				(*p).x[0] = x  &  (*p).x[3] = x
				(*p).y[2:3] = y
				xc = mean((*p).x[0:3])
				yc = mean((*p).y[0:3])
				(*p).x[4] = xc
				(*p).y[4] = yc
				end
			4: begin						; centre
				dx = x - (*p).x[4]
				dy = y - (*p).y[4]
				(*p).x = [ (*p).x[0:3]+dx, x]
				(*p).y = [ (*p).y[0:3]+dy, y]
				xc = x
				yc = y
				end
		endcase
		wset, (*pstate).wid
		plot_simple_image_box, pstate

more:
		end

	'UP': begin
		xy_simple_image_to_pixel, pstate, (*p).x,(*p).y, px,py
		(*p).px = px
		(*p).py = py
		print,'UP: kill q!
		print,'x,y=',(*p).x,(*p).y
		print,'px,py=',(*p).px,(*p).py
		if (*pstate).right_button eq 0 then begin
			return_event = {SIMPLE_IMAGE_CROP, ID:event.handler, TOP:event.top, $
				HANDLER:0L, CROP:{CROP, x:(*p).x[0:1], y:(*p).y[1:2]} }
		endif else begin
			return_event = {SIMPLE_IMAGE_CROP, ID:event.handler, TOP:event.top, $
				HANDLER:0L, CROP:{CROP, x:[0.,0.], y:[0.,0.]} }
		endelse
		widget_control, event.id, draw_motion_events=0
		(*pstate).left_button = 0
		(*pstate).right_button = 0
		end
endcase

finish:
	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die
	return

die:
	widget_control, event.top, /destroy
	return
end

