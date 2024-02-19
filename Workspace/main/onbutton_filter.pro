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

pro OnButton_filter, pstate, Event

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
		warning,'OnButton_filter',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

possible = ['DOWN','UP','MOTION','SCROLL']
if event.type gt 2 then begin
	print,'OnButton_filter: Found illegal button type'
	return
endif
;print,'event: type=',event.type, '  press=',event.press

if n_elements(pstate) eq 0 then goto, finish
if ptr_valid(pstate) eq 0 then goto, finish
if size(*pstate,/tname) ne 'STRUCT' then goto, finish

case possible[ event.type] of
	'DOWN': begin
		wset, (*pstate).wid
		xnorm = !x.s[1] * alog10((*pstate).energy) + !x.s[0]
		d = xnorm * (*pstate).width
;		print, '(*pstate).energy=',(*pstate).energy,' norm=',xnorm,' d=',d
		minx = clip( (min(d) - 4), 0, (*pstate).width)
		maxx = clip( (max(d) + 4), 0, (*pstate).width)
		device,copy=[ minx,0, maxx-minx+1,(*pstate).height, minx,0, (*pstate).pix]

		xnorm = float(event.x)/float((*pstate).width)
		e = clip( 10.^((xnorm - !x.s[0])/!x.s[1]), 0.1, 1.0e5)
		(*pstate).energy = e

		if ((*pstate).energy gt 10.^!x.crange[0]) and ((*pstate).energy lt 10.^!x.crange[1]) then begin
			plots, [(*pstate).energy,(*pstate).energy], 10.^(!y.crange), color=spec_colour('orange')
		endif

		widget_control, event.id, draw_motion_events=1
		end

	'MOTION': begin
		wset, (*pstate).wid
		xnorm = !x.s[1] * alog10((*pstate).energy) + !x.s[0]
		d = xnorm * (*pstate).width
;		print, '(*pstate).energy=',(*pstate).energy,' norm=',xnorm,' d=',d
		minx = clip( (min(d) - 4), 0, (*pstate).width)
		maxx = clip( (max(d) + 4), 0, (*pstate).width)
		device,copy=[ minx,0, maxx-minx+1,(*pstate).height, minx,0, (*pstate).pix]

		xnorm = float(event.x)/float((*pstate).width)
		e = clip( 10.^((xnorm - !x.s[0])/!x.s[1]), 0.1, 1.0e5)
		(*pstate).energy = e
;		print, 'event.x=',event.x,'  norm=',xnorm,'  e=',e

		if ((*pstate).energy gt 10.^!x.crange[0]) and ((*pstate).energy lt 10.^!x.crange[1]) then begin
			plots, [(*pstate).energy,(*pstate).energy], 10.^(!y.crange), color=spec_colour('orange')
		endif
		end

	'UP': begin
		widget_control, event.id, draw_motion_events=0
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

