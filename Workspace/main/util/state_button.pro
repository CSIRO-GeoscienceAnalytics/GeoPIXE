
function state_button_event, event

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
       warning,'state_button',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','Check plugins for errors.'], /error
       MESSAGE, /RESET
       return, 0
    endif
endif else on_error,0

if widget_info( event.id, /valid) eq 0 then return, 0
child = widget_info( event.handler, /child)
widget_control, child, get_uvalue=pstate

eventname = tag_names( event, /structure_name)
if eventname eq 'WIDGET_TRACKING' then begin
    event.id = event.handler
    return, event
endif
if eventname eq 'WIDGET_TIMER' then begin
    event.id = event.handler
    return, event
endif
;									press	release
; type 0 'press'      	'left'		  1		  1
;      1 'release'      'right' 	  4		  4

if event.type eq 0 then begin					; 'press' down event
	(*pstate).press = 1
	state_button_realize, event.handler
	return, 0
endif
if (*pstate).silent then goto, finish			; this is a silent state_button

; help, event, /struct

(*pstate).press = 0
if event.modifiers ne 0 then begin				; shift to clear
    if (*pstate).freeze eq 0 then begin
	    (*pstate).select = 0
	    (*pstate).alt = 0
	endif
endif else if ((event.release eq 1) and ((*pstate).alt eq 0)) or $
          ((event.release eq 4) and ((*pstate).alt eq 1)) or $
          ((event.release eq 4) and ((*pstate).select eq 0)) then begin
    if (*pstate).freeze eq 0 then begin
	    (*pstate).select = (*pstate).select + 1
	endif
endif

if event.release eq 1 then begin
    if (*pstate).freeze eq 0 then begin
		(*pstate).alt = 0
    	(*pstate).select = ((*pstate).select) mod (*pstate).n_states
	endif
endif
if (*pstate).right and (event.release eq 4) then begin
    if (*pstate).freeze eq 0 then begin
		(*pstate).alt = 1
	    (*pstate).select = ((*pstate).select) mod (*pstate).n_alt_states
	endif
    if (*pstate).select eq 0 then (*pstate).alt=0
endif

;	If a freeze button, then subvert a right mouse click to send an alt select

if ((*pstate).freeze eq 1) and (event.release eq 4) then begin
	return_event = {STATE_BUTTON, ID:event.handler, TOP:event.top, $
       HANDLER: 0L, SELECT:1, ALT:1 }
endif else begin
	return_event = {STATE_BUTTON, ID:event.handler, TOP:event.top, $
       HANDLER: 0L, SELECT:(*pstate).select, ALT:(*pstate).alt }
endelse

;help, return_event

state_button_realize, event.handler

if (*pstate).altpro ne '' then begin
    call_procedure, (*pstate).altpro, return_event
    return, 0
endif

if (*pstate).altfunc ne '' then begin
    result = call_function( (*pstate).altfunc, return_event)
    return, result
endif

finish:
    return, return_event
end

;-----------------------------------------------------------------------------

pro state_button_set, id, value

; Set attributes:
;	select		the colour index for button
;	value		the text string
;	alt			enable alt (right click) colour index
;	mode		0 normal, 1 use mode_colour, not select index
;	colour		set this colour only in mode=1 state
;	highlight	draw highlight central spot
	
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
       warning,'state_button',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','Check plugins for errors.'], /error
       MESSAGE, /RESET
       return
    endif
endif else on_error,0

child = widget_info( id, /child)
widget_control, child, get_uvalue=pstate

if size(value,/tname) eq 'STRUCT' then begin
    tags = tag_names(value)
    if n_elements(tags) gt 0 then begin
       for i=0L, n_elements(tags)-1 do begin
         case tags[i] of
          'MODE': begin
              (*pstate).mode = value.mode
              end
          'COLOUR': begin
              (*pstate).mode_colour = value.colour
              end
          'SELECT': begin
              (*pstate).select = value.select
              end
          'HIGHLIGHT': begin
              (*pstate).highlight = value.highlight
              end
          'VALUE': begin
              (*pstate).value = value.value
              end
          'ALT': begin
              (*pstate).alt = value.alt
              end
          'LABEL': begin						; seems to be not used, use "value"
              (*pstate).value = value.label
              end
          else:
         endcase
       endfor
    endif
endif else begin
    (*pstate).value = value
endelse

state_button_realize, id
return
end

;-----------------------------------------------------------------------------

function state_button_get, id

child = widget_info( id, /child)
widget_control, child, get_uvalue=pstate

result = (*pstate).select

return, result
end

;-----------------------------------------------------------------------------

pro state_button_realize, id

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
       warning,'state_button',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','Check plugins for errors.'], /error
       MESSAGE, /RESET
       return
    endif
endif else on_error,0

child = widget_info( id, /child)
widget_control, child, get_uvalue=pstate

oldID = !d.window
window, /free, xsize=(*pstate).xsize, ysize=(*pstate).ysize, /pixmap
pixID = !d.window

case !version.os_family of
    'MacOS': begin
       csize = 1.0 * (*pstate).csize
	   cthick = (*pstate).cthick
       end
    'unix': begin
       csize = 1.1 * (*pstate).csize
	   cthick = (*pstate).cthick
       end
    else: begin
       csize = 0.95 * (*pstate).csize
	   cthick = (*pstate).cthick
       end
endcase

x1 = 1							; 2
x2 = (*pstate).xsize - 2
y1 = 1							; 2
y2 = (*pstate).ysize - 3		; 2
xs = (*pstate).xsize
ys = (*pstate).ysize
buttonx = [x1, x1, x2, x2, x1]
buttony = [y1, y2, y2, y1, y1]
if (*pstate).press or (*pstate).silent then begin
	darkx = [0, 0, xs, x2, x1, x1, 0]
	darky = [0, ys, ys, y2, y2, y1,0]
	lightx = [0, x1, x2, x2, xs, xs, 0]
	lighty = [0, y1, y1, y2, ys,  0, 0]
endif else begin
	lightx = [0, 0, xs, x2, x1, x1, 0]
	lighty = [0, ys, ys, y2, y2, y1,0]
	darkx = [0, x1, x2, x2, xs, xs, 0]
	darky = [0, y1, y1, y2, ys,  0, 0]
endelse
if (*pstate).shape eq 1 then begin
	circle_diam, x1,(y1+y2)/2, x2,(y1+y2)/2, shapex,shapey
endif else if (*pstate).shape eq 2 then begin
	shapex = buttonx
	shapey = buttony
endif
if (*pstate).highlight then begin
	xh = (*pstate).xsize/2
	yh = (*pstate).ysize/2
	highx = clip( xh + [-2, 2, 2, -2, -2], 0, xs-1)
	highy = clip( yh + [-3, -3, 1, 1, -3]-1, 0, ys-1)
	highx2 = clip( xh + [-1, 1, 1, -1, -1], 0, xs-1)
	highy2 = clip( yh + [-2, -2, 0, 0, -2]-1, 0, ys-1)
endif

nc = n_elements((*pstate).state_colours)
c = ((*pstate).select + (*pstate).alt*(*pstate).n_states) < (nc-1)
if (*pstate).mode eq 0 then begin
	col = (*pstate).state_colours[c]
endif else if (*pstate).mode eq 1 then begin
	col = (*pstate).mode_colour
endif else col = 0
POLYFILL, buttonx, buttony, Color=col, /Device
POLYFILL, darkx, darky, Color=(*pstate).colours[2], /Device
POLYFILL, lightx, lighty, Color=(*pstate).colours[1], /Device
if (*pstate).highlight then begin
	POLYFILL, highx, highy, Color=(*pstate).colours[1], /Device
	POLYFILL, highx2, highy2, Color=spec_colour('pink'), /Device
endif

; Ignore font parameters for now. Just use default font,
; and then set size using (*pstate).csize in vector form.

;old_pf = !p.font
;!p.font = 0         ; as it was
;!p.font = 1
;device, get_current_font=old_font
;if (*pstate).font ne '' then device, set_font=(*pstate).font
;device, set_font='System*12'
;device, set_font='Helvetica Bold', /tt_font

x = (xs+1)/2.
y = ys/2. - (*pstate).yoff

;c = (*pstate).colours[0]
;if ((*pstate).select ge (*pstate).n_states) or (*pstate).alt then c=(*pstate).colours[3]

c = (*pstate).label_colours[c]
if (*pstate).shape ge 1 then PLOTS, shapex, shapey, Color=c, /Device
if csize gt 0.1 then begin
	xyouts, x-1,y+1, (*pstate).value, /device, align=0.5, color=c, charsize=csize, charthick=cthick
endif

;device, set_font=old_font, /tt_font
;device, set_font=old_font
;!p.font = old_pf

widget_control, (*pstate).drawID, get_value=wid
(*pstate).wid = wid
wset,wid

device, copy=[0,0,(*pstate).xsize,(*pstate).ysize, 0,0, pixID]
wdelete, pixID

wset, oldID
return
end

;-----------------------------------------------------------------------------

function state_button, parent, n_states=n_states, colours=state_coloursi, $
         frame_colours=coloursi, xsize=xsize, ysize=ysize, tracking=tracking, $
         value=value, uname=uname, uvalue=uvalue, font=font, shape=shape, $
         event_pro=event_pro, event_func=event_func, select=initial, $
         n_alt_states=n_alt_states, alt=alt, silent=silent, right=right, $
         charsize=csize, charthick=cthick, xoffset=xoff, yoffset=yoff, freeze=freeze, _extra=extra

; Create a state-button, which has a set of colours that it cycles through when pressed.
; The colours are set using the 'colours' keyword. 'Frame_colours' can also be set to frame the button.
; They are normally set to creat 3D buttons, using the colours: black, highlight, shadow, white.
; Then colours 0 and 3 are normally used to label text, depending on contrast with the button colour.
; The alt colours come after the set of n_states normal colours in the colours list.
;
; 'n_state'			# of states the button cycles through
; 'select'			initial state setting for the button
; 'n_alt_states'	# of alt states, when right mouse button pressed and /right set
; 'alt'				sets initial alt state (makes text white too)
; /right			enable right mouse button to toggle to alt state
;
; 'xsize', 'ysize'	set the size of the button in pixels.
; 'value'			is a value for a state-button to be displayed as a label.
; 'shape'			shape top draw (0=none, 1=circle, 2=square)
; 'uvalue'			is a uvalue for a state-button.
; 'uname'			is a uname for a state-button.
; /tracking			enable tracking events
; 'font'			set the font to use
; 'xoffset', 'yoffset'	sets TLB base position offset
; 'charsize'		charsize for value text in button
;
; /silent			does not change or pass on press events
; 'event_pro'		alternate pro to execute to process an event
; 'event_func'		alternate function to execute to process an event, its return is passed on
; /freeze			event reported, but state does not change on mouse click

; Font is not working properly, and is ignored.
; Set font size using 'charsize' keyword parameter.
; Must change font to work like direct Draw widget character selection.

COMPILE_OPT STRICTARR
COMMON COLORS, R_orig, G_orig, B_orig, R_curr, G_curr, B_curr

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
       warning,'state_button',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','Check plugins for errors.'], /error
       MESSAGE, /RESET
       return, 0
    endif
endif else on_error,0

highlight = spec_colour('white')
shadow = spec_colour('d.grey')
black = spec_colour('black')
white = spec_colour('white')

grey = spec_colour('l.grey')
green = spec_colour('green')
yellow = spec_colour('yellow')
lblue = spec_colour('l.blue')
red = spec_colour('red')
orange = spec_colour('orange')
dgreen = spec_colour('d.green')
blue = spec_colour('blue')
violet = spec_colour('violet')
;                      					        select: 0   1    2       3    4       5    6      7        8    9
if n_elements(state_coloursi) lt 1 then state_coloursi = [grey,green,yellow,red,lblue,orange,white,dgreen,blue,violet]
if n_elements(coloursi) lt 1 then coloursi = [black, highlight, shadow, white]
colours = coloursi
state_colours = state_coloursi
if n_elements(value) lt 1 then value=''
if n_elements(event_pro) lt 1 then altpro=''
if n_elements(event_func) lt 1 then altfunc=''
if n_elements(initial) lt 1 then initial=0
if n_elements(silent) lt 1 then silent=0
if n_elements(freeze) lt 1 then freeze=0
if n_elements(shape) lt 1 then shape=0
if n_elements(tracking) lt 1 then tracking=0
if n_elements(alt) lt 1 then alt=0
if n_elements(n_states) lt 1 then n_states=3
if n_elements(right) lt 1 then right=0
if n_elements(n_alt_states) gt 0 then if n_alt_states gt 0 then right=1
if n_elements(n_alt_states) lt 1 then n_alt_states=n_states
if n_elements(font) lt 1 then begin
    if !version.os_family eq 'MacOS' then begin
       font = 'HELVETICA*BOLD*11'
    endif else begin
       font = 'ARIAL*BOLD*14'
    endelse
endif
if n_elements(xsize) lt 1 then xsize=22
if n_elements(ysize) lt 1 then ysize=18
if n_elements(xoff) lt 1 then xoff=0
if n_elements(yoff) lt 1 then yoff=0
if right and (n_alt_states+n_states gt 16) then begin
    r = 16.0 / float(n_alt_states + n_states)
    n_states = fix(r*n_states)
    n_alt_states = fix(r*n_alt_states)
endif
if right then state_colours[n_states] = state_colours[0]
if n_elements(csize) lt 1 then csize=1.0
if n_elements(cthick) lt 1 then cthick=1.0

contrast0 = sqrt( (float(r_orig[state_colours])-float(r_orig[colours[0]]))^2 + (float(g_orig[state_colours])-float(g_orig[colours[0]]))^2 + $
				(float(b_orig[state_colours])-float(b_orig[colours[0]]))^2)
contrast3 = sqrt( (float(r_orig[state_colours])-float(r_orig[colours[3]]))^2 + (float(g_orig[state_colours])-float(g_orig[colours[3]]))^2 + $
				(float(b_orig[state_colours])-float(b_orig[colours[3]]))^2)
label_colours = state_colours
label_colours[*] = colours[0]
q = where(contrast3 gt 1.42*contrast0, nq)
if nq gt 0 then label_colours[q] = colours[3]

  if !version.os_family eq 'unix' then begin
    retain=2
  endif else begin
    retain=1
  endelse

tlb = widget_base( parent, uname=uname, uvalue=uvalue, $
         event_func='state_button_event', notify_realize='state_button_realize', $
         pro_set_value='state_button_set', func_get_value='state_button_get', $
         xoffset=xoff, yoffset=yoff, _extra=extra)

drawID = widget_draw( tlb, xsize=xsize, ysize=ysize, retain=retain, $
		/button_events, tracking=tracking)

state = {   select:     initial, $       ; current select state
         n_states:      n_states, $      ; total number of states
         n_alt_states:  n_alt_states, $  ; total number of right states
         drawID:		drawID, $        ; draw widget ID
         wid:			0, $             ; window index for draw
         mode:			0, $			 ; colour mode (0=normal, 1="Set" any "Colour")
		 mode_colour:	110, $			 ; colour to use in mode=1
		 highlight:		0, $			 ; highlighted attribute (central spot)
         xsize:			xsize, $         ; x size
         ysize:			ysize, $         ; y size
         yoff:			5, $             ; text Y offset from centre
         value:			value, $         ; text value
         font:			font, $          ; font
         csize:			csize, $         ; char size
         cthick:		cthick, $        ; char line thickness
         press:			0, $			 ; flags button press ON
         silent:		silent, $        ; silent = no response
         freeze:		freeze, $        ; freeze = response but no state change
         shape:			shape, $		 ; draw a shape (0=none, 1=circle, 2=square)
         right:			right, $         ; use right mouse button as alt
         alt:			alt, $           ; alt set
         state_colours: state_colours, $ ; selected colour indices
         colours:		colours, $       ; text, highlight colour indices
         label_colours:	label_colours, $ ; colour for text label, depending on state colour
         altpro:		altpro, $        ; alt event pro
         altfunc:		altfunc }        ; alt event function

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)

return, tlb
end
