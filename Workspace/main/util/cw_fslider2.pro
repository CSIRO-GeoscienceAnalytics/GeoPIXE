; $Id: cw_fslider2.pro,v 1.12 1998/07/15 20:03:30 alan Exp $
;
; Copyright (c) 1992-1998, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	cw_fslider2
;
; PURPOSE:
;	The standard slider provided by the WIDGET_SLIDER() function is
;	integer only. This compound widget provides a floating point
;	slider.
;
; CATEGORY:
;	Compound widgets.
;
; CALLING SEQUENCE:
;	widget = cw_fslider2(Parent)
;
; INPUTS:
;       Parent:		The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	DRAG:		Set this keyword to zero if events should only
;				be generated when the mouse is released. If it is
;				non-zero, events will be generated continuously
;				when the slider is adjusted. Note: On slow systems,
;				/DRAG performance can be inadequate. The default
;				is DRAG=0.
;   EDIT:		Set this keyword to make the slider label be
;				editable. The default is EDIT=0.
;	FORMAT:		Provides the format in which the slider value is
;				displayed. This should be a format as accepted by
;				the STRING procedure. The default is FORMAT='(G13.6)'
;	FRAME:		Set this keyword to have a frame drawn around the
;				widget. The default is FRAME=0.
;	MAXIMUM:	The maximum value of the slider. The default is
;				MAXIMUM=100.
;	MINIMUM:	The minimum value of the slider. The default is
;				MINIMUM=0.
;	SCROLL		Sets the SCROLL keyword to the WIDGET_SLIDER underlying
;				this compound widget. Unlike WIDGET_SLIDER, the
;				value given to SCROLL is taken in the floating units
;				established by MAXIMUM and MINIMUM, and not in pixels.
; *	SENSITIVE	Sensitivity state for the widget.
;	SUPPRESS_VALUE:	If true, the current slider value is not displayed.
;				The default is SUPPRESS_VALUE=0.
;	TITLE:		The title of slider. (The default is no title.)
; *	TRACKING	Enable tracking for this compound widget.
; *	UNAME		User name to be associated with the widget.
;	UVALUE:		The user value for the widget.
;	VALUE:		The initial value of the slider
;	VERTICAL:	If set, the slider will be oriented vertically.
;				The default is horizontal.
;	LAYOUT		For horizontal slider: 0 column, 1 row label/slider layout.
;	XSIZE:		For horizontal sliders, sets the length.
;	YSIZE:		For vertical sliders, sets the height.
;
; OUTPUTS:
;	The ID of the created widget is returned.
;
; SIDE EFFECTS:
;	This widget generates event structures containing a field
;	named value when its selection thumb is moved. This is a
;	floating point value.
;
; PROCEDURE:
;	WIDGET_CONTROL, id, SET_VALUE=value can be used to change the
;		current value displayed by the widget.  Optionally, the
;		value supplied to the SET_VALUE keyword can be a three
;		element vector consisting of [value, minimum, maximum]
;		in order to change the minimum and maximum values as
;		well as the slider value itself.
;
;	WIDGET_CONTROL, id, GET_VALUE=var can be used to obtain the current
;		value displayed by the widget.  The maximum and minimum
;		values of the slider can also be obtained by calling the
;		FSLIDER2_GET_VALUE function directly (rather than the standard
;		usage through the WIDGET_CONTROL interface) with the optional
;		keyword MINMAX:
;			sliderVals = FSLIDER2_GET_VALUE(id, /MINMAX)
;		When called directly with the MINMAX keyword, the return
;		value of FSLIDER2_GET_VALUE is a three element vector
;		containing [value, minimum, maximum].
;
;
; MODIFICATION HISTORY:
;	April 2, 1992, SMR and AB
;		Based on the RGB code from XPALETTE.PRO, but extended to
;		support color systems other than RGB.
;	5 January 1993, Mark Rivers, Brookhaven National Labs
;		Added EDIT keyword.
;   7 April 1993, AB, Removed state caching.
;	28 July 1993, ACY, set_value: check labelid before setting text.
;	3 October 1995, AB, Added SCROLL keyword.
;	15 July 1998, ACY, Added ability to set and get minimum and maximum.
;	31 April 2003, Chris Ryan, CSIRO
;		Added SENSITIVE, UNAME, TRACKING, LAYOUT
;-


PRO FSLIDER2_SET_VALUE, id, value

  ; Set the value of both the slider and the label
  ON_ERROR, 2						;return to caller

  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, get_uvalue=pstate

  IF (N_ELEMENTS(value) GE 3) THEN BEGIN
 	(*pstate).bot = value[1]
 	(*pstate).top = value[2]
 	; set value to scalar for remainder of routine to preserve
 	; compatability with computations for scalar argument to
 	; FSLIDER2_SET_VALUE
 	value = value[0]
 	value = value > (*pstate).bot
 	value = value < (*pstate).top
  ENDIF

  WIDGET_CONTROL, (*pstate).slideid, $
	SET_VALUE = round(float((*pstate).big) * $
		(float(value) - (*pstate).bot) / (float((*pstate).top) - (*pstate).bot))
  IF ((*pstate).labelid NE 0) THEN $
  	WIDGET_CONTROL, (*pstate).labelid, $
		SET_VALUE = STRING(FLOAT(value), format=(*pstate).format)
END



FUNCTION FSLIDER2_GET_VALUE, id, MINMAX=minmax

  ; Return the value of the slider
  ON_ERROR, 2						;return to caller

  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, get_uvalue=pstate

; First read the label in case it has been edited and NOT <returned>

    WIDGET_CONTROL, (*pstate).labelid, GET_VALUE = tmp

    value = float(tmp[0])
    value = value > (*pstate).bot
    value = value < (*pstate).top
    ;Update the slider, set new value
    WIDGET_CONTROL, (*pstate).slideid, $
	SET_VALUE = round(float((*pstate).big) * $
		(value - (*pstate).bot) / (float((*pstate).top) - (*pstate).bot))

    ; Update the label so it has desired format
    WIDGET_CONTROL, (*pstate).labelid, $
           SET_VALUE=STRING(value, format=(*pstate).format)

; Now return floating slider value

  WIDGET_CONTROL, (*pstate).slideid, GET_VALUE = tmp
  ret = ((tmp / float((*pstate).big)) * (float((*pstate).top) - (*pstate).bot)) + (*pstate).bot

  IF (KEYWORD_SET(minmax)) THEN ret=[ret, (*pstate).bot, (*pstate).top]
  return, ret
END


;-----------------------------------------------------------------------------

FUNCTION fslider2_event, ev

  if tag_names( ev,/structure) eq 'WIDGET_TRACKING' then begin
  	return, { WIDGET_TRACKING, ID:ev.id, TOP:ev.top, $
  					HANDLER:0L, ENTER:ev.enter }
  endif

  ; Retrieve the structure from the child that contains the sub ids
  parent=ev.handler
  stash = WIDGET_INFO(parent, /CHILD)
  WIDGET_CONTROL, stash, get_uvalue=pstate

  ; See which widget was adjusted, the slider or the label

  value=0.0
  drag=0
  if (ev.id eq (*pstate).slideid) then begin
    ; Get the non-adjusted value
    WIDGET_CONTROL, (*pstate).slideid, GET_VALUE = nonadj
    ; Compute the floating point value
    value = ((nonadj / float((*pstate).big)) * (float((*pstate).top) - (*pstate).bot)) + (*pstate).bot
    drag = ev.drag
;    print,'slide: nonadj=', nonadj,' value=', value
    ; Update label
    IF ((*pstate).labelid NE 0) THEN $
      WIDGET_CONTROL, (*pstate).labelid, $
           SET_VALUE=STRING(value, format=(*pstate).format)

  endif else if (ev.id eq (*pstate).labelid) then begin

    WIDGET_CONTROL, (*pstate).labelid, GET_VALUE = tmp

    value = float(tmp[0])
    value = value > (*pstate).bot
    value = value < (*pstate).top
    ;Update the slider, set new value
    WIDGET_CONTROL, (*pstate).slideid, $
	SET_VALUE = float((*pstate).big) * $
		(value - (*pstate).bot) / (float((*pstate).top) - (*pstate).bot)

    drag = 0
    ; Update the label so it has desired format
    WIDGET_CONTROL, (*pstate).labelid, $
           SET_VALUE=STRING(value, format=(*pstate).format)
  endif

  RETURN, { ID:parent, TOP:ev.top, HANDLER:0L, VALUE:value, DRAG:drag }
END

;-----------------------------------------------------------------------------

FUNCTION cw_fslider2, parent, $
		DRAG = drag, $
        EDIT = edit, $
		FRAME = frame, $
		MAXIMUM = max, $
		MINIMUM = min, $
		SCROLL = scroll, $
		SUPPRESS_VALUE = sup, $
		TITLE = title, $
		VALUE = val, $
		VERTICAL = vert, $
		XSIZE = xsize, $
		YSIZE = ysize, $
		FORMAT=format, $
		UVALUE = uval, $
		Uname = uname, $
		tracking = tracking, $
		sensitive = sensitive, $
		layout = layout, $
		_extra = extra

  IF (N_PARAMS() EQ 0) THEN MESSAGE, 'cw_fslider2: Incorrect number of arguments'

  ON_ERROR, 2						; return to caller

  big = 30000L						; size of widget_slide

  ; Defaults for keywords
  IF NOT (KEYWORD_SET(drag))  THEN drag = 0
  IF NOT (KEYWORD_SET(edit))  THEN edit = 0
  IF NOT (KEYWORD_SET(frame)) THEN frame = 0
  IF N_ELEMENTS(max) EQ 0     THEN max = 100.0
  IF N_ELEMENTS(min) EQ 0     THEN min = 0.0
  IF NOT (KEYWORD_SET(scroll)) THEN scroll = big/100 ELSE $
	  scroll = ABS(LONG(round((float(scroll) / (max - min)) * big))) > 1
  IF NOT (KEYWORD_SET(sup))   THEN sup = 0
  IF NOT (KEYWORD_SET(title)) THEN title = ""
  IF NOT (KEYWORD_SET(uval))  THEN uval = 0
  IF (N_ELEMENTS(uname) EQ 0)  THEN uname = ''
  IF NOT (KEYWORD_SET(tracking))  THEN tracking = 0
  IF (N_ELEMENTS(sensitive) eq 0)		then sensitive = 1
  IF N_ELEMENTS(val) EQ 0     THEN val = min
  IF NOT (KEYWORD_SET(format))  THEN format='(G13.6)'
  IF (N_ELEMENTS(layout) EQ 0)  THEN layout = 0

  state = {slideid:0L, labelid:0L, top:max, bot:min, format:format, big:big }

  ; Motif 1.1 and newer sliders react differently to XSIZE and YSIZE
  ; keywords than Motif 1.0 or OpenLook. These defs are for horizontal sliders
  version = WIDGET_INFO(/version)
  newer_motif = (version.style eq 'Motif') and (version.release ne '1.0')

  ; The sizes of the parts depend on keywords and whether or not the
  ; float slider is vertical or horizontal
  ;these are display specific and known to be inherently evil
  sld_thk = 16
  chr_wid = 7
  IF (KEYWORD_SET(vert)) THEN BEGIN
    if (newer_motif) then begin
      if (not KEYWORD_SET(xsize)) then xsize = 0
    endif else begin
      title_len = STRLEN(title) * chr_wid
      xsize = (sld_thk * 1.4) + title_len	; Take label into account
    endelse
    IF NOT (KEYWORD_SET(ysize)) THEN ysize = 100
    l_yoff = ysize / 2
  ENDIF ELSE BEGIN					;horizontal slider
    vert = 0
    tmp = not keyword_set(xsize)
    if (newer_motif) then begin
      if (tmp) then xsize = 0
      IF NOT (KEYWORD_SET(ysize)) THEN ysize = 0
    endif else begin
      if (tmp) then xsize = 100
      IF (TITLE NE '') THEN sld_thk = sld_thk + 21
      ysize = sld_thk		; Make the slider not waste label space
    endelse
    l_yoff = 0
  ENDELSE

  if (vert and layout) then begin
    mainbase = WIDGET_BASE(parent, FRAME = frame, /column, uname = uname, sensitive=sensitive, _extra=extra)
    mainchild = WIDGET_BASE(mainbase, xpad=0, ypad=0, space=0, /row, /align_center)
    labelbase = mainbase
  endif else if (vert) then begin
    mainbase = WIDGET_BASE(parent, FRAME = frame, /ROW, uname = uname, sensitive=sensitive, _extra=extra)
    mainchild = WIDGET_BASE(mainbase, xpad=0, ypad=0, space=0, /row)
    labelbase = WIDGET_BASE(mainchild)
  endif else if (layout) then begin
    mainbase = WIDGET_BASE(parent, FRAME = frame, /COLUMN, uname = uname, sensitive=sensitive, _extra=extra)
    mainchild = WIDGET_BASE(mainbase, xpad=0, ypad=0, space=3, /row, /base_align_center, /align_center)
    labelbase = mainchild
  endif else begin
    mainbase = WIDGET_BASE(parent, FRAME = frame, /COLUMN, uname = uname, sensitive=sensitive, _extra=extra)
    mainchild = WIDGET_BASE(mainbase, xpad=0, ypad=0, space=0, /column)
    labelbase = mainchild
  endelse

  WIDGET_CONTROL, mainbase, SET_UVALUE = uval, EVENT_FUNC = 'fslider2_event', $
	PRO_SET_VALUE='FSLIDER2_SET_VALUE', $
	FUNC_GET_VALUE='FSLIDER2_GET_VALUE'


  IF (sup EQ 0) THEN begin
    ; Only build the label if suppress_value is FALSE
    nf = strlen(state.format)
    xn = (nf gt 3) ? (fix( strmid( state.format, 2, nf-3))>2 ) : (10)
    state.labelid = WIDGET_TEXT(labelbase, YOFFSET = l_yoff, $
		VALUE = STRING(FLOAT(val), format=state.format), edit=edit, XSIZE=xn, tracking=tracking, uvalue=uval)
  endif ELSE state.labelid = 0

;	help,big,val,state.bot,state.top,scroll,drag,float(big) * $
;			(float(val) - state.bot) / $
;			(float(state.top) - state.bot)

    state.slideid = WIDGET_SLIDER(mainchild, $
		TITLE = TITLE, $
		XSIZE = xsize, $
		YSIZE = ysize, $
		/SUPPRESS_VALUE, $
		MINIMUM = 0, $
		MAXIMUM = big, $
		VALUE = float(big) * $
			(float(val) - state.bot) / $
			(float(state.top) - state.bot), $
		VERTICAL = vert, $
		DRAG=drag, $
		SCROLL=scroll, $
		uvalue = uval, $
		tracking = tracking )


  WIDGET_CONTROL, WIDGET_INFO(mainbase, /CHILD), SET_UVALUE=ptr_new(state, /NO_COPY)
  RETURN, mainbase

END
