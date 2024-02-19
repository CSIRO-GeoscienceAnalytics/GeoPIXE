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

pro OnButton_pca_cluster, Event

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
		warning,'OnButton_pca_cluster',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

possible = ['DOWN','UP','MOTION','SCROLL']
if event.type gt 2 then begin
	print,'OnButton_pca_cluster: Found illegal button type'
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
			print,'OnButton_pca_cluster: left mouse button press ...'
			(*pstate).left_button = 1
		endif
		if (event.press AND 4B) ne 0 then begin
			print,'OnButton_pca_cluster: right mouse button press ...'
			(*pstate).right_button = 1
		endif
		(*pstate).id = -1
		pixel_pca_cluster_to_xy, pstate, event.x,event.y, x,y
		print,'zoomed mouse x,y=',x,y

		if (*pstate).right_button then begin
			if near_pca_cluster_xy( x,y, (*p).x,(*p).y) eq -1 then begin
				clear_pca_cluster_all_markers, pstate
				notify, 'corr-analyze-clear', from=event.top
				goto, finish
			endif
		endif

		if (*p).present then begin
			(*pstate).id = near_pca_cluster_xy( x,y, (*p).x,(*p).y)
			print,'near ',(*pstate).id
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
			wset, (*pstate).wid2
			plot_pca_cluster_spline, pstate
		endelse

		widget_control, event.id, draw_motion_events=1
		notify, 'corr-analyze-clear', from=event.top
		end

	'MOTION': begin
		print,'Motion: id=',(*pstate).id
;		if (*pstate).id lt 0 then begin
;			widget_control, event.id, draw_motion_events=0
;			goto, finish
;		endif
		xc = 0
		yc = 0
		pixel_pca_cluster_to_xy, pstate, event.x,event.y, x,y

		print,'motion: x,y= ', x,y
		clear_pca_cluster_spline, pstate
		if (*pstate).id eq -1 then begin							; drag expand
			circle_diam, (*p).x[1],(*p).y[1], x,y, xs,ys, n=10
			(*p).x[1:*] = xs[0:9]
			(*p).y[1:*] = ys[0:9]
			xc = mean((*p).x[1:*])
			yc = mean((*p).y[1:*])
			(*p).x[0] = xc
			(*p).y[0] = yc
		endif else if (*pstate).id eq 0 then begin					; centre
			dx = x - (*p).x[0]
			dy = y - (*p).y[0]
			(*p).x = [ x, (*p).x[1:*]+dx]
			(*p).y = [ y, (*p).y[1:*]+dy]
			xc = x
			yc = y
		endif else begin											; control points
			(*p).x[(*pstate).id] = x
			(*p).y[(*pstate).id] = y
			xc = mean((*p).x[1:*])
			yc = mean((*p).y[1:*])
			(*p).x[0] = xc
			(*p).y[0] = yc
		endelse
		wset, (*pstate).wid2
		plot_pca_cluster_spline, pstate

more:
		s = legend_pca_cluster_string( pstate, /position)
		widget_control, (*pstate).help, set_value=s
		end

	'UP': begin
		xy_pca_cluster_to_conc, pstate, (*p).x,(*p).y, cx,cy, /nozoom
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

