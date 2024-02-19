; $Id: cw_bgroup2.pro,v 1.19 1998/01/15 18:41:29 scottm Exp $
;
; Copyright (c) 1992-1998, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	cw_bgroup2
;
; PURPOSE:
;	cw_bgroup2 is a compound widget that simplifies creating
;	a base of buttons. It handles the details of creating the
;	proper base (standard, exclusive, or non-exclusive) and filling
;	in the desired buttons. Events for the individual buttons are
;	handled transparently, and a cw_bgroup2 event returned. This
;	event can return any one of the following:
;		- The Index of the button within the base.
;		- The widget ID of the button.
;		- The name of the button.
;		- An arbitrary value taken from an array of User values.
;
; CATEGORY:
;	Compound widgets.
;
; CALLING SEQUENCE:
;		Widget = cw_bgroup2(Parent, Names)
;
;	To get or set the value of a cw_bgroup2, use the GET_VALUE and
;	SET_VALUE keywords to WIDGET_CONTROL. The value of a cw_bgroup2
;	is:
;
;		-----------------------------------------------
;		Type		Value
;		-----------------------------------------------
;		normal		None
;		exclusive   	Index of currently set button
;		non-exclusive	Vector indicating the position
;				of each button (1-set, 0-unset)
;		-----------------------------------------------
;
;
; INPUTS:
;       Parent:		The ID of the parent widget.
;	Names:		A string array, containing one string per button,
;			giving the name of each button.
;
; KEYWORD PARAMETERS:
;
;	BUTTON_UVALUE:	An array of user values to be associated with
;			each button and returned in the event structure.
;	COLUMN:		Buttons will be arranged in the number of columns
;			specified by this keyword.
;	EVENT_FUNCT:	The name of an optional user-supplied event function
;			for buttons. This function is called with the return
;			value structure whenever a button is pressed, and
;			follows the conventions for user-written event
;			functions.
;	EXCLUSIVE:	Buttons will be placed in an exclusive base, with
;			only one button allowed to be selected at a time.
;	FONT:		The name of the font to be used for the button
;			titles. If this keyword is not specified, the default
;			font is used.
;	FRAME:		Specifies the width of the frame to be drawn around
;			the base.
;	IDS:		A named variable into which the button IDs will be
;			stored, as a longword vector.
;	LABEL_LEFT:	Creates a text label to the left of the buttons.
;	LABEL_TOP:	Creates a text label above the buttons.
;	MAP:		If set, the base will be mapped when the widget
;			is realized (the default).
;	NONEXCLUSIVE:	Buttons will be placed in an non-exclusive base.
;			The buttons will be independent.
;	NO_RELEASE:	If set, button release events will not be returned.
;	RETURN_ID:	If set, the VALUE field of returned events will be
;			the widget ID of the button.
;	RETURN_INDEX:	If set, the VALUE field of returned events will be
;			the zero-based index of the button within the base.
;			THIS IS THE DEFAULT.
;	RETURN_NAME:	If set, the VALUE field of returned events will be
;			the name of the button within the base.
;	ROW:		Buttons will be arranged in the number of rows
;			specified by this keyword.
;	SCROLL:		If set, the base will include scroll bars to allow
;			viewing a large base through a smaller viewport.
; *	SENSITIVE	Sensitivity state for the widget.
;	SET_VALUE:	The initial value of the buttons. This is equivalent
;			to the later statement:
;
;			WIDGET_CONTROL, widget, set_value=value
;
;	SPACE:		The space, in pixels, to be left around the edges
;			of a row or column major base. This keyword is
;			ignored if EXCLUSIVE or NONEXCLUSIVE are specified.
; *	TRACKING	Enable tracking for this compound widget.
;	UVALUE:		The user value to be associated with the widget.
; *	UNAME		User name to be associated with the widget.
;	XOFFSET:	The X offset of the widget relative to its parent.
;	XPAD:		The horizontal space, in pixels, between children
;			of a row or column major base. Ignored if EXCLUSIVE
;			or NONEXCLUSIVE are specified.
;	XSIZE:		The width of the base.
;	X_SCROLL_SIZE:	The width of the viewport if SCROLL is specified.
;	YOFFSET:	The Y offset of the widget relative to its parent.
;	YPAD:		The vertical space, in pixels, between children of
;			a row or column major base. Ignored if EXCLUSIVE
;			or NONEXCLUSIVE are specified.
;	YSIZE:		The height of the base.
;	Y_SCROLL_SIZE:	The height of the viewport if SCROLL is specified.
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; SIDE EFFECTS:
;	This widget generates event structures with the following definition:
;
;		event = { ID:0L, TOP:0L, HANDLER:0L, SELECT:0, VALUE:0 }
;
;	The SELECT field is passed through from the button event. VALUE is
;	either the INDEX, ID, NAME, or BUTTON_UVALUE of the button,
;	depending on how the widget was created.
;
; RESTRICTIONS:
;	Only buttons with textual names are handled by this widget.
;	Bitmaps are not understood.
;
; MODIFICATION HISTORY:
;	15 June 1992, AB
;	7 April 1993, AB, Removed state caching.
;	6 Oct. 1994, KDB, Font keyword is not applied to the label.
;       10 FEB 1995, DJC  fixed bad bug in event procedure, getting
;                         id of stash widget.
;	11 April 1995, AB Removed Motif special cases.
;	31 April 2003, CGR Added sensitive, uname, tracking.
;-


pro cw_bgroup2_SETV, id, value

  ON_ERROR, 2						;return to caller

  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, get_uvalue=pstate

  case (*pstate).type of
    0: message,'unable to set plain button group value'
    1: begin
	  WIDGET_CONTROL, SET_BUTTON=0, (*pstate).ids[(*pstate).excl_pos]
	  (*pstate).excl_pos = value
	  WIDGET_CONTROL, /SET_BUTTON, (*pstate).ids[value]
	end
    2: begin
	  n = n_elements(value)-1
	  for i = 0, n do begin
	    (*pstate).nonexcl_curpos[i] = value[i]
	    WIDGET_CONTROL, (*pstate).ids[i], SET_BUTTON=value[i]
	  endfor
	end
  endcase
end



function cw_bgroup2_GETV, id, value

  ON_ERROR, 2						;return to caller

  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, get_uvalue=pstate

  case (*pstate).type of
    0: message,'unable to get plain button group value'
    1: ret = (*pstate).excl_pos
    2: ret = (*pstate).nonexcl_curpos
  endcase

  return, ret
end



function cw_bgroup2_EVENT, ev

  if tag_names( ev,/structure) eq 'WIDGET_TRACKING' then begin
  	return, { WIDGET_TRACKING, ID:ev.id, TOP:ev.top, $
  					HANDLER:0L, ENTER:ev.enter }
  endif

  WIDGET_CONTROL, ev.handler, GET_UVALUE=stash
  WIDGET_CONTROL, stash, get_uvalue=pstate
 ; WIDGET_CONTROL, ev.id, get_uvalue=uvalue
  uvalue = fix( widget_info( ev.id, /uname))

  ret = 1			;Assume we return a struct
  case (*pstate).type of
    0:
    1: if (ev.select eq 1) then begin
	  (*pstate).excl_pos = uvalue
	ENDIF else begin
	  if ((*pstate).no_release ne 0) then ret = 0
	ENDELSE
    2: begin
	  ; Keep track of the current state
	  (*pstate).nonexcl_curpos[uvalue] = ev.select
          if ((*pstate).no_release ne 0) and (ev.select eq 0) then ret = 0
	end
  endcase

  if ret then begin		;Return a struct?
      ret = { ID:(*pstate).base, TOP:ev.top, HANDLER:0L, SELECT:ev.select, $
	       VALUE:(*pstate).ret_arr[uvalue] }
      efun = (*pstate).efun
      if efun ne '' then return, CALL_FUNCTION(efun, ret) $
      else return, ret
  endif else begin		;Trash the event
      return, 0
  endelse
end



function cw_bgroup2, parent, names, $
	BUTTON_UVALUE = button_uvalue, COLUMN=column, EVENT_FUNCT = efun, $
	EXCLUSIVE=excl, FONT=font, FRAME=frame, IDS=ids, LABEL_TOP=label_top, $
	LABEL_LEFT=label_left, MAP=map, $
	NONEXCLUSIVE=nonexcl, NO_RELEASE=no_release, RETURN_ID=return_id, $
	RETURN_INDEX=return_index, RETURN_NAME=return_name, $
	ROW=row, SCROLL=scroll, SET_VALUE=sval, SPACE=space, UVALUE=uvalue, $
	XOFFSET=xoffset, XPAD=xpad, XSIZE=xsize, X_SCROLL_SIZE=x_scroll_size,$
	YOFFSET=yoffset, YPAD=ypad, YSIZE=ysize, Y_SCROLL_SIZE=y_scroll_size, $
	uname=uname, tracking=tracking, sensitive=sensitive


  IF (N_PARAMS() ne 2) THEN MESSAGE, 'Incorrect number of arguments'

  ON_ERROR, 2						;return to caller

  ; Set default values for the keywords
  version = WIDGET_INFO(/version)
  if (version.toolkit eq 'OLIT') then def_space_pad = 4 else def_space_pad = 3
  IF (N_ELEMENTS(column) eq 0) 			then column = 0
  IF (N_ELEMENTS(excl) eq 0) 			then excl = 0
  IF (N_ELEMENTS(frame) eq 0)			then frame = 0
  IF (N_ELEMENTS(map) eq 0)				then map=1
  IF (N_ELEMENTS(nonexcl) eq 0)			then nonexcl = 0
  IF (N_ELEMENTS(no_release) eq 0)		then no_release = 0
  IF (N_ELEMENTS(row) eq 0)				then row = 0
  IF (N_ELEMENTS(scroll) eq 0)			then scroll = 0
  IF (N_ELEMENTS(space) eq 0)			then space = def_space_pad
  IF (N_ELEMENTS(uvalue) eq 0)			then uvalue = 0
  IF (N_ELEMENTS(xoffset) eq 0)			then xoffset=0
  IF (N_ELEMENTS(xpad) eq 0)			then xpad = def_space_pad
  IF (N_ELEMENTS(xsize) eq 0)			then xsize = 0
  IF (N_ELEMENTS(x_scroll_size) eq 0)	then x_scroll_size = 0
  IF (N_ELEMENTS(yoffset) eq 0)			then yoffset=0
  IF (N_ELEMENTS(ypad) eq 0)			then ypad = def_space_pad
  IF (N_ELEMENTS(ysize) eq 0)			then ysize = 0
  IF (N_ELEMENTS(y_scroll_size) eq 0)	then y_scroll_size = 0
  IF (N_ELEMENTS(uname) eq 0)			then uname = ''
  IF (N_ELEMENTS(tracking) eq 0)		then tracking = 0
  IF (N_ELEMENTS(sensitive) eq 0)		then sensitive = 1


  top_base = 0L
  if (n_elements(label_top) ne 0) then begin
	next_base = WIDGET_BASE(parent, XOFFSET=xoffset, YOFFSET=yoffset, Xpad=0, Ypad=0, Space=0, /COLUMN, sensitive=sensitive)
	if(keyword_set(font)) then begin
		junk = WIDGET_TEXT(next_base, value=label_top, font=font, uvalue=uvalue, tracking=tracking)
	endif else begin
		junk = WIDGET_TEXT(next_base, value=label_top, uvalue=uvalue, tracking=tracking)
	endelse
	top_base = next_base
  endif else begin
	next_base = parent
  endelse
  if (n_elements(label_left) ne 0) then begin
	next_base = WIDGET_BASE(next_base, XOFFSET=xoffset, YOFFSET=yoffset, Xpad=0, Ypad=0, Space=0, /ROW)
	if(keyword_set(font)) then begin
		junk = WIDGET_TEXT(next_base, value=label_left, font=font, uvalue=uvalue, tracking=tracking)
	endif else begin
		junk = WIDGET_TEXT(next_base, value=label_left, uvalue=uvalue, tracking=tracking)
	endelse
	if (top_base eq 0L) then top_base = next_base
  endif

  ; We need some kind of outer base to hold the users UVALUE

  if (top_base eq 0L) then begin
	top_base = WIDGET_BASE(parent, XOFFSET=xoffset, YOFFSET=yoffset, Xpad=0, Ypad=0, Space=0, sensitive=sensitive)
	next_base = top_base
  endif
  If (top_base EQ next_base) THEN begin
	next_base = WIDGET_BASE(top_base, Xpad=0, Ypad=0, Space=0)
  endif

  ; Set top level base attributes

  WIDGET_CONTROL, top_base, MAP=map, $
	FUNC_GET_VALUE='cw_bgroup2_GETV', PRO_SET_VALUE='cw_bgroup2_SETV', $
	SET_UVALUE=uvalue, set_uname=uname

  ; The actual button holding base
  base = WIDGET_BASE(next_base, COLUMN=column, EXCLUSIVE=excl, FRAME=frame, $
	NONEXCLUSIVE=nonexcl, ROW=row, SCROLL=scroll, SPACE=space, $
	XPAD=xpad, XSIZE=xsize, X_SCROLL_SIZE=x_scroll_size, $
	YPAD=ypad, YSIZE=ysize, Y_SCROLL_SIZE=y_scroll_size, $
	EVENT_FUNC='cw_bgroup2_EVENT', $
	UVALUE=WIDGET_INFO(top_base, /child))


  n = n_elements(names)
  uv = uvalue
  if n_elements(uvalue) lt n then uv = replicate(uvalue, n)
  ids = lonarr(n)
  for i = 0, n-1 do begin
    if (n_elements(font) eq 0) then begin
      ids[i] = WIDGET_BUTTON(base, value=names[i], Uname=i, uvalue=uv[i], tracking=tracking)
    endif else begin
      ids[i] = WIDGET_BUTTON(base, value=names[i], FONT=font, Uname=i, uvalue=uv[i], tracking=tracking)
    endelse
  endfor

  ; Keep the state info in the real (inner) base UVALUE.
  ; Pick an event value type:
  ;	0 - Return ID
  ;	1 - Return INDEX
  ;	2 - Return NAME
  ret_type = 1
  if KEYWORD_SET(RETURN_ID) then ret_type = 0
  if KEYWORD_SET(RETURN_NAME) then ret_type = 2
  if KEYWORD_SET(BUTTON_UVALUE) then ret_type = 3
    case ret_type of
      0: ret_arr = ids
      1: ret_arr = indgen(n)
      2: ret_arr = names
      3: ret_arr = button_uvalue
    endcase
  type = 0
  if (excl ne 0) then type = 1

  if (nonexcl ne 0) then type = 2
  if n_elements(efun) le 0 then efun = ''
  state = { type:type, $	; 0-Standard, 1-Exclusive, 2-Non-exclusive
	    base: top_base, $	; cw_bgroup2 base...
	    ret_arr:ret_arr, $	; Vector of event values
	    efun : efun, $	; Name of event fcn
	    nonexcl_curpos:intarr(n), $	; If non-exclus, tracks state
	    excl_pos:0, $			; If exclusive, current button
	    ids:ids, $			; Ids of buttons
	    no_release:no_release }
  WIDGET_CONTROL, WIDGET_INFO(top_base, /CHILD), SET_UVALUE=ptr_new(state, /NO_COPY)

  if (n_elements(sval) ne 0) then cw_bgroup2_SETV, top_base, sval

  return, top_base
END
