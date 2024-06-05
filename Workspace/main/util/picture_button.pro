;+
; picture_button
;
; id = picture_button( parent, file, Colours=colours, XSize=xsize, $
;   YSize=ysize, UValue=uvalue, Font=font, value=value, tracking=tracking, $
;   Event_Pro=newprocedure, Event_Func=newfunction, uname=uname, fill=fill, $
;   pushbutton_events=pushbutton_events)
;
; parent		parent widget
; file			image file name (multiple files to change image by value [0,1,2,...] )
; 				(if dimensions=3 then assume 'file' is image data for one image)
;
; Keywords:

; colours		colour indices for black, highlight, shadow, white
; xsize			xsize of image to force resize of image
; ysize			ysize of image to force resize of image
; fill			fill a border around image, using colour of pic[1,1]
; EVENT_FUNC	The name of an event handler function for this button.
; EVENT_PRO		The name of an event handler procedure for this button
; FONT			This is the name of a hardware font to be used for the button value.
; UVALUE		A user value. Used in the normal way.
; Uname			A user name. Used in the normal way.
; tracking		produce tracking events
; pushbutton_events	generate both release events (select=0) as well as press
;
; EVENT STRUCTURE:
;
;   event = {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:1}
;
; MODIFICATION HISTORY:
;
;   Based on color-button written by: David Fanning, April 1996.
;   Given to attendees of IDL training courses.
;	Re-written as picture-button by CG Ryan, Jan, 2008.
;-

FUNCTION picture_button_EVENTS, event

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
       warning,'picture_button',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','Check plugins for errors.'], /error
       MESSAGE, /RESET
       return, 0
    endif
endif else on_error,0

child = WIDGET_INFO(event.handler, /Child)
WIDGET_CONTROL, child, get_uvalue=pstate

	eventname = tag_names(event,/structure_name)
	if eventname eq 'WIDGET_TRACKING' then begin
	    event.id = event.handler
	    return, event
	endif
	if eventname eq 'WIDGET_TIMER' then begin
	    event.id = event.handler
	    return, event
	endif

   ; Is this a button press event?

IF event.type EQ 0 THEN BEGIN
    (*pstate).press = 1
   picture_button_REALIZE, event.handler

	if (*pstate).pushbutton_events then begin
		returnEvent = {WIDGET_BUTTON, ID:event.handler, TOP:event.top, HANDLER:0L, SELECT:1}

		IF ((*pstate).altPro NE '') THEN BEGIN
		   Call_Procedure, (*pstate).altPro, returnEvent
		   RETURN, 0
		ENDIF
		IF ((*pstate).altFunc NE '') THEN BEGIN
		   result = CALL_FUNCTION((*pstate).altFunc, returnEvent)
		   RETURN, 0
		ENDIF

		return, returnEvent
	endif

   RETURN, 0
ENDIF

   ; Pretend this is a button event. Make up a BUTTON event structure.

returnEvent = {WIDGET_BUTTON, ID:event.handler, TOP:event.top, HANDLER:0L, SELECT:1}
(*pstate).press = 0
picture_button_REALIZE, event.handler

if (*pstate).pushbutton_events then begin
	returnEvent.select = 0
endif

   ; If there is alternate event handler, send event to it.

IF ((*pstate).altPro NE '') THEN BEGIN
   Call_Procedure, (*pstate).altPro, returnEvent
   RETURN, 0
ENDIF
IF ((*pstate).altFunc NE '') THEN BEGIN
   result = CALL_FUNCTION((*pstate).altFunc, returnEvent)
   RETURN, 0
ENDIF

   ; Send it to the regular event handler.

RETURN, returnEvent
END

; *******************************************************************

PRO picture_button_SET, id, thisValue

   ; image index value

child = WIDGET_INFO(id, /Child)
WIDGET_CONTROL, child, get_uvalue=pstate

(*pstate).value = clip( thisValue, 0, n_elements(*(*pstate).pppic)-1)

*(*pstate).ppic = *(*(*pstate).pppic)[(*pstate).value]
(*pstate).order = 0
(*pstate).dim = dimensions(*(*pstate).ppic)

   ; Rebuild the button in the draw widget.

picture_button_REALIZE, id
END

; *******************************************************************

FUNCTION picture_button_GET, id

   ; The value of the button is the text.

child = WIDGET_INFO(id, /Child)
WIDGET_CONTROL, child, get_uvalue=pstate

retvalue = (*pstate).value
RETURN, retvalue
END

; *******************************************************************

PRO picture_button_REALIZE, id

   ; This is where the button is drawn in the draw widget.

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
       warning,'picture_button',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','Check plugins for errors.'], /error
       MESSAGE, /RESET
       return
    endif
endif else on_error,0

child = WIDGET_INFO(id, /Child)
WIDGET_CONTROL, child, get_uvalue=pstate

   ; Make the draw widget the current graphics window.

WIDGET_CONTROL, (*pstate).drawID, Get_Value=wid
(*pstate).wid = wid
WSET, wid

if (*pstate).dim eq 3 then begin
	device, decomposed=1
	tv, *(*pstate).ppic, /true, order=(*pstate).order
	device, decomposed=0
endif else begin
  	tvlct, r,g,b, /get
  	tvlct, (*pstate).r, (*pstate).g, (*pstate).b
	tv, *(*pstate).ppic
  	tvlct, r,g,b
endelse

x1 = 2
x2 = (*pstate).xsize - 2
y1 = 2
y2 = (*pstate).ysize - 2
xs = (*pstate).xsize
ys = (*pstate).ysize
if (*pstate).press then begin
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

POLYFILL, darkx, darky, Color=(*pstate).index[2], /Device
POLYFILL, lightx, lighty, Color=(*pstate).index[1], /Device

END

; *******************************************************************

FUNCTION picture_button, parent, file, Colours=colours, XSize=xsize, $
   YSize=ysize, UValue=uvalue, Font=font, value=value, tracking=tracking, $
   Event_Pro=newprocedure, Event_Func=newfunction, uname=uname, fill=fill, $
   pushbutton_events=pushbutton_events

; parent		parent widget
; file			image file name (multiple files to change image by value [0,1,2,...] )
; 				(if dimensions(file)=3 then assume 'file' is image data for one image)
; value			index into image array
; colours		colour indices for black, highlight, shadow, white
; xsize			xsize of image to force resize of image
; ysize			ysize of image to force resize of image
; fill			fill a border around image, using colour of pic[1,1]
; EVENT_FUNC	The name of an event handler function for this button.
; EVENT_PRO		The name of an event handler procedure for this button
; FONT			This is the name of a hardware font to be used for the button value.
; UVALUE		A user value. Used in the normal way.
; Uname			A user name. Used in the normal way.
; /tracking		produce tracking events
; /pushbutton_events	generate both release events (select=0) as well as press

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
       warning,'picture_button',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','Check plugins for errors.'], /error
       MESSAGE, /RESET
       return, 0
    endif
endif else on_error,0

IF N_elements(parent) lt 1 THEN MESSAGE, 'Must pass parent ID as argument.'
IF N_ELEMENTS(value) EQ 0 THEN value = 0
IF N_ELEMENTS(uvalue) EQ 0 THEN uvalue = 0
IF N_ELEMENTS(fill) EQ 0 THEN fill = 0
IF N_ELEMENTS(pushbutton_events) EQ 0 THEN pushbutton_events = 0
IF N_ELEMENTS(tracking) EQ 0 THEN tracking = 0
IF N_ELEMENTS(uname) EQ 0 THEN uname = ''
if n_elements(font) lt 1 then begin
    if !version.os_family eq 'MacOS' then begin
       font = 'HELVETICA*BOLD*11'
    endif else begin
       font = 'ARIAL*BOLD*14'
    endelse
endif
IF N_ELEMENTS(newprocedure) EQ 0 THEN newprocedure = ''
IF N_ELEMENTS(newfunction) EQ 0 THEN newfunction = ''

nf = n_elements(file)
if nf lt 1 then begin
	print,'picture_button: No picture supplied.'
	return, 0L
endif
if file_test(file) eq 0 then begin
	print,'picture_button: File not found: '+file
	return, 0L
endif

dim = dimensions(file)
if (nf eq 1) and (dim eq 3) then begin					; single image data already
	pic = file
	ppic = ptr_new(file)
	r = 0
	g = 0
	b = 0
	order = 0
endif else begin										; image file name(s)
	ppic = ptrarr(nf, /allocate_heap)
	value = clip( value, 0, nf-1)
	r = 0
	g = 0
	b = 0
	for i=0L, nf-1 do begin
		*ppic[i] = read_image( file[i], r,g,b)
		if dimensions(*ppic[i]) lt 2 then begin
			print,'picture_button: Error reading picture file: '+file[i]
			return, 0L
		endif
	endfor
	pic = *ppic[value]
	order = 0
	dim = dimensions(pic)
endelse

  if !version.os_family eq 'unix' then begin
    retain=2
  endif else begin
    retain=1
  endelse

if dim ge 3 then begin
	pic = pic[0:2,*,*]
	px = n_elements(pic[0,*,0])
	py = n_elements(pic[0,0,*])
	if fill gt 0 then begin
		t = replicate(pic[1,1], 3, px+2*fill, py+2*fill)
		t[*, fill:fill+px-1, fill:fill+py-1] = pic
		pic = t
		px = px+2*fill
		py = py+2*fill
	endif
	if (n_elements(xsize) ne 0) and (n_elements(ysize) ne 0) then pic = congrid(pic,3,xsize,ysize,/interp)
endif else begin
	px = n_elements(pic[*,0])
	py = n_elements(pic[0,*])
	if fill gt 0 then begin
		t = replicate(pic[1,1], px+2*fill, py+2*fill)
		t[fill:fill+px-1, fill:fill+py-1] = pic
		pic = t
		px = px+2*fill
		py = py+2*fill
	endif
	if (n_elements(xsize) ne 0) and (n_elements(ysize) ne 0) then pic = congrid(pic,xsize,ysize,/interp)
endelse

IF N_ELEMENTS(xsize) EQ 0 THEN xsize = px
IF N_ELEMENTS(ysize) EQ 0 THEN ysize = py

highlight = spec_colour('white')
shadow = spec_colour('d.grey')
black = spec_colour('black')
white = spec_colour('white')
if n_elements(colours) lt 1 then colours = [black, highlight, shadow, white]

IF N_ELEMENTS(colours) NE 4 THEN $
   MESSAGE, 'COLORINDEX keyword must be 4-element array.'

cw_tlb = WIDGET_BASE(parent, UValue=uvalue, Event_Func='picture_button_EVENTS', $
		Pro_Set_Value='picture_button_SET', Func_Get_Value='picture_button_GET', Uname=uname, $
   		Notify_Realize='picture_button_REALIZE')

base1 = widget_base(cw_tlb, /column, space=0, xpad=0, ypad=0)

drawID = WIDGET_DRAW(base1, XSize=xsize, YSize=ysize, Button_Events=1, $
				tracking=tracking, retain=retain)

state = { drawID:drawID, $ ;        Draw widget ID.
         wid:0, $ ;                Window index number of draw widget.
         ppic: ptr_new(pic), $		; ptr to current pic
         file:file, $				; file names
         pppic: ptr_new(ppic), $	; ptr to ptrarr of pics
         press:0, $					button press state
         pushbutton_events: pushbutton_events, $
         order:order, $	;			order of planes?
         dim: dim, $				dimensions of pic
         r: r, $					red colour map (if indexed)
         g: g, $					green colour map (if indexed)
         b: b, $					blue colour map (if indexed)
         xsize:xsize, $ ;          XSIZE of draw widget.
         ysize:ysize, $ ;          YSIZE of draw widget.
         value:value, $ ;          The index into multiple images.
         font:font, $ ;            The hardware font to use on the button.
         altPro:newprocedure, $ ;  The name of an alternate procedure to send event to.
         altFunc:newfunction, $ ;  The name of an alternate function to send event to.
         index:colours} ;            The index array that specifies button values.

WIDGET_CONTROL, base1, Set_UValue=ptr_new(state, /No_Copy)
RETURN, cw_tlb
END
