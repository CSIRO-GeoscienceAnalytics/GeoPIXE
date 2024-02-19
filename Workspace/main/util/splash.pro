;
; Splash page modal popup
;
; Call sequence:
;
; Splash,	group_leader=group, $			; groupleader, for modal action
;			title = 'Title string', $		; popup window title
;			picture24 = 'file24.jpeg', $	; jpeg file for 24 bit screen
;			picture8 = 'file8.gif', $		; gif file for 8 bit screen
;			select = select, $				; selection returned
;			buttons = ['button string 1','button string 2']	; etc.
;			timeout = time					; disappear after a time (default=1 sec)
;
;	picture24 can also specify a 24-bit PNG file (extension .png).
;	Later: will permit better choices of image file types.
;
; Notes:
; 1.	BOTH image files should be loaded, so that the .SAV file
;		contains both 8 and 24-bit versions.
;
; 2.	Use the group_leader to force the popup to be /modal and /floating.
;		If not supplied a dummy one is realized.

; 3.	If buttons is used, then buttons in splash will be labelled using these
;		strings, and as many buttons as strings will be created.
;
;	If pictures are not proivided, any in common (from previous splash, e.g. loaded
;	with database in "geopixe2.sav") will be used.
;
;-----------------------------------------------------------------
; TLB_KILL_REQUEST_EVENTS Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_KILL_REQUEST, ID:0L, TOP:0L, HANDLER:0L }
;
;-----------------------------------------------------------------
pro OnKill_splash_TLB, Event

; This behaves badly on some flavoirs of Linux, so we explicitly call
; OnDestroy_Splash if there is a tiny fake parent window (local_tlb=1)

widget_control, event.top, get_uvalue=pstate

if (*pstate).local_tlb eq 0 then begin
	if widget_info(event.top,/valid) then widget_control, event.top, /destroy
endif else begin
	OnDestroy_splash_TLB, event.top
endelse
end

;-----------------------------------------------------------------
pro OnDestroy_splash_TLB, wWidget

; This is triggered by OnKill_splash, but before it has finished.

widget_control, wWidget, get_uvalue=pstate

if (*pstate).local_tlb then begin
	info = widget_info((*pstate).tlb,/valid)
	if widget_info((*pstate).tlb,/valid) then widget_control, (*pstate).tlb, /destroy
endif
end

;-----------------------------------------------------------------
; Notify Realize Callback Procedure.
; Argument:
;   wWidget - ID number of specific widget.
;
;-----------------------------------------------------------------
pro OnRealize_splash_draw, wWidget

  common c_splash24, got_24, pic_24, bit24, order_24
  common c_splash8, got_8, pic_8, r,g,b

top = tlb_id( wWidget)
widget_control, top, get_uvalue=pstate

widget_control, wWidget, get_value = wid
(*pstate).wid = wid

if !d.n_colors gt 256 then begin
	if got_24 ne 1 then begin
		print,'Splash_draw: No pic_24 found.'
	endif else begin
		print,'Splash_draw: draw pic_24 ...order=',order_24
		device, decomposed=1
		wset, (*pstate).wid
		tv, pic_24, /true, order=order_24
		device, decomposed=0
	endelse
endif else begin
	if got_8 ne 1 then begin
		print,'Splash_draw: No pic_8 found.'
	endif else begin
		print,'Splash_draw: draw pic_8 ...'
		wset, (*pstate).wid
	  	tvlct, r,g,b
		tv, pic_8
	endelse
endelse

if (*pstate).fresh then begin
	print, 'Draw splash text afresh ...'
	tv_text, 50,(*pstate).ysize-80, 'GeoPIXE', transparent=[0,0,0], font='Helvetica Bold Italic', align=0, color=spec_colour('green'), charsize=7
	tv_text, (*pstate).xsize-25,190, 'Quantitative PIXE/SXRF', transparent=[0,0,0], font='Helvetica Bold Italic', align=1, color=spec_colour('white'), charsize=3
	tv_text, (*pstate).xsize-25,150, 'Imaging and Analysis', transparent=[0,0,0], font='Helvetica Bold Italic', align=1, color=spec_colour('white'), charsize=3
	tv_text, (*pstate).xsize-485,110, 'Original code by ...', transparent=[0,0,0], font='Helvetica Italic', align=0, color=spec_colour('white'), charsize=1.5
	tv_text, (*pstate).xsize-465,90, 'C.G. Ryan, M. Jensen, ', transparent=[0,0,0], font='Helvetica Italic', align=0, color=spec_colour('white'), charsize=1.5
	tv_text, (*pstate).xsize-445,70, 'B.E. Etschmann, D.R. Cousens', transparent=[0,0,0], font='Helvetica Italic', align=0, color=spec_colour('white'), charsize=1.5
	tv_text, (*pstate).xsize-465,50, 'CSIRO Mineral Resources', transparent=[0,0,0], font='Helvetica Italic', align=0, color=spec_colour('white'), charsize=1.5
	tv_text, (*pstate).xsize-445,30, '... now Open Source', transparent=[0,0,0], font='Helvetica bold Italic', align=0, color=spec_colour('white'), charsize=1.4
	if !d.n_colors gt 256 then begin
		print,'Splash_draw: Read back final pic_24.'
		pic_24 = tvrd(0,0,!d.x_size,!d.y_size,true=1)
		order_24 = 0
	endif
	wait, 4
endif
return
end

;----------------------------------------------------------------------

pro splash_TLB_event, Event

  widget_control, event.top, get_uvalue=pstate
  wWidget =  Event.top
  uname = widget_info( event.id, /uname)

  buttonname = strarr(4)
  for i=0L,3 do begin
	buttonname[i] = strcompress( 'Button_' + string(i), /remove_all)
  endfor

  q = where( buttonname eq uname)
  if q[0] ne -1 then begin						; was a button
	n = q[0]
	(*(*pstate).pselect).select = n
	OnKill_splash_TLB, Event
	return
  endif

  case uname of

	'splash_TLB': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
			OnKill_splash_TLB, Event
			return
		endif
		end
	else:
  endcase
end

;----------------------------------------------------------------------

pro splash, GROUP_LEADER=wGroup, title=title, picture24=file24, $
		picture8=file8, select=select, buttons=buttons, _EXTRA=_VWBExtra_, $
		timeout=time

COMPILE_OPT STRICTARR, HIDDEN

  common c_splash24, got_24, pic_24, bit24, order_24
  common c_splash8, got_8, pic_8, r,g,b

  if n_elements(got_24) lt 1 then got_24=0
  if n_elements(got_8) lt 1 then got_8=0
  if n_elements(buttons) lt 1 then buttons = ['Continue']
  if n_elements(time) lt 1 then time=0
  select = -2
  info = {select:-2}
  local_tlb = 0
  if n_elements(wGroup) lt 1 then begin
  	local_tlb = 1
  	wGroup = widget_base(scr_xsize=1,scr_ysize=1,xpad=0,ypad=0,map=0)
  	widget_control, wGroup, /realize
  print,'local TLB = ', wGroup
  endif

  case !version.os_family of
	'MacOS': begin
		retain = 2
		end
	'unix': begin
		retain = 2
		end
	else: begin
		retain = 1
 		end
  endcase

  fresh = 0
  if !d.n_colors gt 256 then begin
  	bit24 = 1
  endif else begin
  	bit24 = 0
  endelse
  if got_24 ne 1 then begin
    fresh = 1
	if n_elements(file24) lt 1 then begin
		print,'splash: No picture24 supplied.'
		file24 = '..\Database Build\geopixe-profile.png'
	endif
	if strpos( strlowcase(file24), '.png') ne -1 then begin
		print,'splash: read 24-bit png file ...'
	  	pic_24 = read_png( file24)
	  	order_24 = 0
	endif else begin
		print,'splash: read 24-bit jpeg file ...'
	  	read_jpeg, file24, pic_24
	  	order_24 = 0
	endelse
	got_24 = 1
  endif
  if got_8 ne 1 then begin
    fresh = 1
	if n_elements(file8) lt 1 then begin
		print,'splash: No picture8 supplied.'
		file8 = '..\Data Build\geopixe-profile.gif'
	endif
	print,'splash: read 8-bit gif file ...'
  	read_gif, file8, pic_8, r,g,b
;  	r = r[0:127]
;  	g = g[0:127]						; are these a problem?
;  	b = b[0:127]
  	r = r
  	g = g								; are these a problem?
  	b = b
	got_8 = 1
  endif
  if bit24 then begin
	xsize = n_elements(pic_24[0,*,0])
	ysize = n_elements(pic_24[0,0,*])
  endif else begin
	xsize = n_elements(pic_8[*,0])
	ysize = n_elements(pic_8[0,*])
  endelse

  if n_elements(title) lt 1 then title='Splash title goes here ...'
  if n_elements(wGroup) ge 1 then begin
  	modal = 1
  	floating = 1
  endif else begin
  	wGroup = 0
	modal = 0
	floating = 0
  endelse

  device, get_screen_size=sz
  xoff = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
  yoff = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

  splash_TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='splash_TLB',  $
       	floating=floating, modal=modal, xoffset=xoff, yoffset=yoff,  $
		/ALIGN_CENTER ,/BASE_ALIGN_right ,TITLE=title ,SPACE=10,  $
		COLUMN=1, xpad=10, ypad=10)
  print,'splash TLB = ', splash_TLB


  Top_base = Widget_Base(splash_TLB, UNAME='Top_base' ,SPACE=1 , $
  		xpad = 1, ypad = 1, /ROW)


  button_base = Widget_Base(splash_TLB, UNAME='button_base' ,/row, $
  		space=5, xpad=1, ypad=1, /base_align_right)


  draw = widget_draw( Top_base, xsize=xsize, ysize=ysize, retain=retain, $
  		notify_realize='OnRealize_splash_Draw', uname='Splash_Draw')


  for i=0L,(n_elements(buttons)<4)-1 do begin
	uname = strcompress( 'Button_' + string(i), /remove_all)
  	button = Widget_Button(Button_base, UNAME=uname, VALUE=buttons[i])
  	if i eq 0 then timer=button
  endfor

  p = ptr_new(info)

  print,'Splash: Fresh set to ', fresh
  state = { $
  		pselect:	p, $				; pointer to return value
		local_tlb:	local_tlb, $		; do we need to kill the tlb too
		tlb:		wGroup, $			; the tlb id
		xsize:		xsize, $			; draw X size
		ysize:		ysize, $			; draw Y size
		fresh:		fresh, $			; newly created (1), or stored in sav data (0)
		wid:		0 }					; draw widget ID
  help, state

  widget_control, splash_tlb, set_uvalue=ptr_new(state, /no_copy)

  Widget_Control, /REALIZE, splash_TLB
  if time gt 0.01 then widget_control, timer, timer=float(time)

  XManager, 'splash_TLB', splash_TLB, event_handler='splash_TLB_event', $
  					cleanup='OnDestroy_Splash_TLB'
  info = *p
  ptr_free, p

  select = info.select
  return
end

