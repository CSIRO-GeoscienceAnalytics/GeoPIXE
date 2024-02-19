;------------------------------------------------------------------

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

pro OnButton_corr, Event

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
		warning,'OnButton_corr',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

possible = ['DOWN','UP','MOTION','SCROLL']
if event.type gt 2 then begin
	print,'OnButton_corr: Found illegal button type'
	return
endif
;print,'event: type=',event.type, '  press=',event.press

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(pstate) eq 0 then goto, finish
if ptr_valid(pstate) eq 0 then goto, finish
if size(*pstate,/tname) ne 'STRUCT' then goto, finish
p = (*pstate).pmark
if ptr_good(p) eq 0 then goto, finish

case possible[ event.type] of
	'DOWN': begin
		(*pstate).left_button = 0
		(*pstate).right_button = 0
		if (event.press AND 1B) ne 0 then begin
;			print,'OnButton_corr: left mouse button press ...'
			(*pstate).left_button = 1
		endif
		if (event.press AND 4B) ne 0 then begin
;			print,'OnButton_corr: right mouse button press ...'
			(*pstate).right_button = 1
		endif
		(*pstate).id = -1
		pixel_corr_to_xy, pstate, event.x,event.y, x,y

		if (*pstate).right_button then begin
			if near_corr_xy( x,y, (*p).x,(*p).y) eq -1 then begin
				clear_corr_all_markers, pstate
				if (*pstate).highlight eq 0 then notify, 'corr-analyze-clear', from=event.top
				goto, finish
			endif
		endif

		if (*p).present then begin
			(*pstate).id = near_corr_xy( x,y, (*p).x,(*p).y)
;			print,'near ',(*pstate).id
			if (*pstate).id eq -1 then goto, finish
		endif else if (*pstate).right_button then begin
			goto, finish
		endif else begin
			(*pstate).id = -1
			circle_diam, x-3.,y-3., x,y, xs,ys, n=10
			(*p).x[1:*] = xs[0:9]
			(*p).y[1:*] = ys[0:9]
			(*p).x[0] = mean(xs)
			(*p).y[0] = mean(ys)
;			help,p
;			print,'corr initial circle x,y=',(*p).x,(*p).y
			wset, (*pstate).wid2
			plot_corr_spline, pstate
;			print,'corr after plot circle x,y=',(*p).x,(*p).y
		endelse

		widget_control, event.id, draw_motion_events=1
		if (*pstate).highlight eq 0 then notify, 'corr-analyze-clear', from=event.top
		end

	'MOTION': begin
;		print,'Motion: id=',(*pstate).id
;		help,p
;		print,'corr current entry circle x,y=',(*p).x,(*p).y
		xc = 0
		yc = 0
		pixel_corr_to_xy, pstate, event.x,event.y, x,y
;		print, 'Corr: mouse x,y=',event.x,event.y,' to=',x,y

		clear_corr_spline, pstate
		if (*pstate).id eq -1 then begin							; drag expand
			circle_diam, (*p).x[1],(*p).y[1], x,y, xs,ys, n=10
			(*p).x[1:*] = xs[0:9]
			(*p).y[1:*] = ys[0:9]
			xc = mean((*p).x[1:*])
			yc = mean((*p).y[1:*])
			(*p).x[0] = xc
			(*p).y[0] = yc
;			print,'corr circle x,y=',(*p).x,(*p).y
		endif else if (*pstate).id eq 0 then begin					; centre
			dx = x - (*p).x[0]
			dy = y - (*p).y[0]
			(*p).x = [ x, (*p).x[1:*]+dx]
			(*p).y = [ y, (*p).y[1:*]+dy]
			xc = x
			yc = y
		endif else begin										; control points
			(*p).x[(*pstate).id] = x
			(*p).y[(*pstate).id] = y
			xc = mean((*p).x[1:*])
			yc = mean((*p).y[1:*])
			(*p).x[0] = xc
			(*p).y[0] = yc
		endelse
		wset, (*pstate).wid2
		plot_corr_spline, pstate

more:
		s = legend_corr_string( pstate, /position)
		widget_control, (*pstate).help, set_value=s
		end

	'UP': begin
		xy_corr_to_conc, pstate, (*p).x,(*p).y, cx,cy, /nozoom
		(*p).cx = cx
		(*p).cy = cy
		print,'UP: kill q!
		print,'x,y=',(*p).x,(*p).y
		print,'cx,cy=',(*p).cx,(*p).cy

		widget_control, event.id, draw_motion_events=0
		notify, 'corr-analyze-mark', from=event.top
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

