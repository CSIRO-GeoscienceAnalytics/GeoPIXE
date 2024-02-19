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
;     1 left, 2 centre, 4 right.
;--------------------------------------------------------------------

pro OnButton_time_amp, Event

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
       warning,'OnButton_time_amp',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       goto, finish
    endif
endif

possible = ['DOWN','UP','MOTION','SCROLL']
if event.type gt 2 then begin
    print,'OnButton_time_amp: Found illegal button type'
    return
endif
;print,'event: type=',event.type, '  press=',event.press

child = widget_info( event.top, /child)
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
         print,'OnButton_time_amp: left mouse button press ...'
         (*pstate).left_button = 1
       endif
       if (event.press AND 4B) ne 0 then begin
         print,'OnButton_time_amp: right mouse button press ...'
         (*pstate).right_button = 1
       endif
       (*pstate).id = -1
       pixel_time_amp_to_xy, pstate, event.x,event.y, x,y
       (*pstate).oldx = x
       (*pstate).oldy = y
       xy_time_amp_to_pixel, pstate, [x,(*p).x], [y,(*p).y], px,py

       if (*pstate).right_button then begin
         if near_time_amp_xy( px[0],py[0], px[1:*],py[1:*]) eq -1 then begin
          clear_time_amp_all_markers, pstate
;          notify, 'time_amp-analyze-clear', from=event.top
          goto, finish
         endif
       endif

       if (*p).present then begin
         (*pstate).id = near_time_amp_xy( px[0],py[0], px[1:*],py[1:*])
         if (*pstate).id eq -1 then goto, finish
       endif else if (*pstate).right_button then begin
         goto, finish
       endif else begin
         (*pstate).id = 4
         xoff = ([0,1,2,3,4,2,2]-4.)*(*pstate).ax
         yoff = [0,0,0,0,0,10,-10] *(*pstate).ay
		 if (*pstate).logX then begin
         	(*p).x = 10^( alog10(x) + xoff)
		 endif else begin
         	(*p).x = x + xoff
		 endelse
		 if (*pstate).logY then begin
         	(*p).y = 10^( alog10(y) + yoff)
		 endif else begin
         	(*p).y = y + yoff
		 endelse
         wset, (*pstate).wid2
         plot_time_amp_spline, pstate
       endelse

       if ptr_valid( (*pstate).px) then ptr_free, (*pstate).px
       if ptr_valid( (*pstate).py) then ptr_free, (*pstate).py
       (*pstate).px = ptr_new( (*p).x)
       (*pstate).py = ptr_new( (*p).y)

       widget_control, event.id, draw_motion_events=1
       notify, 'time_amp-analyze-clear', from=event.top
       end

    'MOTION': begin
;     print,'Motion: id=',(*pstate).id, '  event.x,y= ', event.x,event.y
;     if (*pstate).id lt 0 then begin
;      widget_control, event.id, draw_motion_events=0
;      goto, finish
;     endif
       pixel_time_amp_to_xy, pstate, event.x,event.y, x,y
       if (x eq (*p).x[(*pstate).id]) and (y eq (*p).y[(*pstate).id]) then goto, more

    ;  print,'motion: spline, (*pstate).id=',(*pstate).id,' ...'
       clear_time_amp_spline, pstate
         case (*pstate).id of
          0: begin                           ; start
			  sy = (*pstate).ax/(*pstate).ay
			  x2 = (*pstate).logX ? alog10(x) : x
			  y2 = (*pstate).logY ? alog10(y) : y
			  px = (*pstate).logX ? alog10(*(*pstate).px) : *(*pstate).px
			  py = (*pstate).logY ? alog10(*(*pstate).py) : *(*pstate).py
			  ox = (*pstate).logX ? alog10((*pstate).oldx) : (*pstate).oldx
			  oy = (*pstate).logY ? alog10((*pstate).oldy) : (*pstate).oldy
              r1 = sqrt( (px[4]-ox)^2 + (sy*(py[4]-oy))^2 )
              r2 = sqrt( (px[4]-x2)^2 + (sy*(py[4]-y2))^2 )
              theta1 = atan( sy*(oy-py[4]), ox-px[4])
              theta2 = atan( sy*(y2-py[4]), x2-px[4])
              x1 = px - px[4]
              y1 = sy*(py - py[4])
              rotatev, x1,y1, 0.0,0.0, -theta1, xr,yr
              xr = xr * (r2/r1)
              rotatev, xr,yr, 0.0,0.0, theta2, xt,yt
              (*p).x = (*pstate).logX ? 10^(xt + alog10((*p).x[4])) : xt + (*p).x[4]
              (*p).y = (*pstate).logY ? 10^(yt/sy + alog10((*p).y[4])) : yt/sy + (*p).y[4]
              end
          4: begin                           ; end
			  sy = (*pstate).ax/(*pstate).ay
			  x2 = (*pstate).logX ? alog10(x) : x
			  y2 = (*pstate).logY ? alog10(y) : y
			  px = (*pstate).logX ? alog10(*(*pstate).px) : *(*pstate).px
			  py = (*pstate).logY ? alog10(*(*pstate).py) : *(*pstate).py
			  ox = (*pstate).logX ? alog10((*pstate).oldx) : (*pstate).oldx
			  oy = (*pstate).logY ? alog10((*pstate).oldy) : (*pstate).oldy
              r1 = sqrt( (px[0]-ox)^2 + (sy*(py[0]-oy))^2 )
              r2 = sqrt( (px[0]-x2)^2 + (sy*(py[0]-y2))^2 )
              theta1 = atan( sy*(oy-py[0]), ox-px[0])
              theta2 = atan( sy*(y2-py[0]), x2-px[0])
              x1 = px - px[0]
              y1 = sy*(py - py[0])
              rotatev, x1,y1, 0.0,0.0, -theta1, xr,yr
              xr = xr * (r2/r1)
              rotatev, xr,yr, 0.0,0.0, theta2, xt,yt
              (*p).x = (*pstate).logX ? 10^(xt + alog10((*p).x[0])) : xt + (*p).x[0]
              (*p).y = (*pstate).logY ? 10^(yt/sy + alog10((*p).y[0])) : yt/sy + (*p).y[0]
              end
          5: begin                           ; width 1
              theta1 = atan( (*(*pstate).py)[5]-(*(*pstate).py)[2], (*(*pstate).px)[5]-(*(*pstate).px)[2])
              tx = [(*(*pstate).px), x]
              ty = [(*(*pstate).py), y]
              rotatev, tx,ty, (*(*pstate).px)[2],(*(*pstate).py)[2], -theta1, xr,yr
              w = xr[7] - xr[2]
              xr[5] = xr[7]
              xr[6] = xr[2] - w
              rotatev, xr,yr, (*(*pstate).px)[2],(*(*pstate).py)[2], theta1, tx,ty
              (*p).x = tx[0:6]
              (*p).y = ty[0:6]
              end
          6: begin                           ; width 2
              theta1 = atan( (*(*pstate).py)[6]-(*(*pstate).py)[2], (*(*pstate).px)[6]-(*(*pstate).px)[2])
              tx = [(*(*pstate).px), x]
              ty = [(*(*pstate).py), y]
              rotatev, tx,ty, (*(*pstate).px)[2],(*(*pstate).py)[2], -theta1, xr,yr
              w = xr[7] - xr[2]
              xr[6] = xr[7]
              xr[5] = xr[2] - w
              rotatev, xr,yr, (*(*pstate).px)[2],(*(*pstate).py)[2], theta1, tx,ty
              (*p).x = tx[0:6]
              (*p).y = ty[0:6]
              end
          2: begin                           ; mid (move width handles too)
              dx = x - (*(*pstate).px)[2]
              dy = y - (*(*pstate).py)[2]
              (*p).x[2] = x
              (*p).y[2] = y
              (*p).x[5] = (*(*pstate).px)[5] + dx
              (*p).y[5] = (*(*pstate).py)[5] + dy
              (*p).x[6] = (*(*pstate).px)[6] + dx
              (*p).y[6] = (*(*pstate).py)[6] + dy
              end
          else: begin
              (*p).x[(*pstate).id] = x
              (*p).y[(*pstate).id] = y
              end
         endcase
       wset, (*pstate).wid2
       plot_time_amp_spline, pstate

more:
       s = legend_time_amp_string( pstate, /position)
       widget_control, (*pstate).help, set_value=s
       end

    'UP': begin
       print,'UP: kill q!
;       print,'x=',(*p).x,format='(A7,7F)'
;       print,'y=',(*p).y,format='(A7,7F)'

       widget_control, event.id, draw_motion_events=0
;     notify, 'time_amp-analyze-mark', from=event.top
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
