function simple_image_event, event

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
	   warning,'simple_image',['IDL run-time error caught.', '', $
		  'Error:  '+strtrim(!error_state.name,2), $
		  !Error_state.msg,'',c], /error
	   MESSAGE, /RESET
	   goto, finish
	endif
endif
catch_errors_on = 0

if widget_info( event.id, /valid) eq 0 then return, 0L
child = widget_info( event.handler, /child)
widget_control, child, get_uvalue=pstate

if size(pstate,/tname) ne 'POINTER' then goto, bad_state
if size(*pstate,/tname) ne 'STRUCT' then goto, bad_state

p = (*pstate).p
if ptr_good( p, /struct) eq 0 then goto, bad_ptr
return_event = 0L
xanes_stack_test, p, xanes, n_el, el, el_xanes
ganged = (*pstate).ganged

case tag_names( event,/structure) of
	'WIDGET_TRACKING': begin
;		help,event,/struct
		if event.id eq (*pstate).cursor_id then begin		; from Draw with cursor ON
			widget_control, event.id, draw_motion_events=event.enter
;			print,'Simple: Draw, Motion = ', event.enter
		endif else begin
			widget_control, event.id, get_uvalue=s
			widget_control, event.handler, set_uvalue=s
			event.id = event.handler
			return_event = event
		endelse
		goto, finish
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of

	'element-combobox': begin
		(*pstate).n = event.index
		i = (*pstate).n
		pimg = (*p).image
		opt = (*p).options
		if ptr_valid( pimg ) eq 0 then goto, finish
		
		widget_control, (*pstate).bottom_slider, set_value = fix((*opt)[i].bottom)
		widget_control, (*pstate).top_slider, set_value = fix((*opt)[i].top)
		widget_control, (*pstate).Zscale_mode, set_combobox_select = (((*opt)[i].log >0)<2)
		draw_simple_images, pstate
		end

	'image-draw': begin
		onbutton_simple_image, event, return_event
		end

	'zscale-mode-combobox': begin
		i = (*pstate).n
		pimg = (*p).image
		opt = (*p).options
		if ptr_valid( opt ) eq 0 then goto, finish
		
		if ganged then begin
			for j=0,n_elements(*opt)-1 do (*opt)[j].log = event.index
		endif else begin
			(*opt)[i].log = event.index
		endelse
		draw_simple_images, pstate
		notify, 'image-display', from=event.top
		end

	'top-slider': begin
;		print,'Top: ', event.value
		i = (*pstate).n
		pimg = (*p).image
		opt = (*p).options
		if ptr_valid( pimg ) eq 0 then goto, finish
		
		if ganged then begin
			for j=0,n_elements(*opt)-1 do (*opt)[j].top = float(event.value) 
		endif else begin
			(*opt)[i].top = float(event.value) 
		endelse
		draw_simple_images, pstate
		notify, 'image-display', from=event.top
		end

	'bottom-slider': begin
		i = (*pstate).n
		pimg = (*p).image
		opt = (*p).options
		if ptr_valid( pimg ) eq 0 then goto, finish
		
		if ganged then begin
			for j=0,n_elements(*opt)-1 do (*opt)[j].bottom = float(event.value) 
		endif else begin
			(*opt)[i].bottom = float(event.value) 
		endelse
		draw_simple_images, pstate
		notify, 'image-display', from=event.top
		end
	else:
endcase

finish:
	return, return_event

bad_state:
	warning,'simple_image_event',['STATE variable has become ill-defined.','Abort Fit Setup.'],/error
	return_event = 0L
	goto, finish
bad_ptr:
	warning,'simple_image_event',['Parameter structure variable has become ill-defined.','Abort.'],/error
	return_event = 0L
	goto, finish
end

;-----------------------------------------------------------------------------

pro simple_image_set, id, vset

child = widget_info( id, /child)
widget_control, child, get_uvalue=pstate

;p = (*pstate).p
;if ptr_good( p, /struct) eq 0 then return
;xanes_stack_test, p, xanes, n_el, el, el_xanes

if size(vset,/tname) eq 'STRUCT' then begin
	tags = tag_names(vset)
	redisplay = 0
	setview = 0
	if n_elements(tags) gt 0 then begin
		for i=0L, n_elements(tags)-1 do begin
			case tags[i] of
				'WIDTH': begin							; new width (whole window)
					(*pstate).width = vset.width - (*pstate).dw
					widget_control, (*pstate).draw, scr_xsize=(*pstate).width
					widget_control, (*pstate).Element_combobox, scr_xsize= (*pstate).el_base + 0.1*((*pstate).width+(*pstate).dw-(*pstate).wmin)
					widget_control, (*pstate).top_slider, scr_xsize= 47 + 0.37*((*pstate).width+(*pstate).dw-(*pstate).wmin)
					widget_control, (*pstate).bottom_slider, scr_xsize= 47 + 0.37*((*pstate).width+(*pstate).dw-(*pstate).wmin)
					widget_control, (*pstate).Zscale_Mode, scr_xsize= (*pstate).mode_base + 0.16*((*pstate).width+(*pstate).dw-(*pstate).wmin)
					setview = 1
					redisplay = 1
					end
				'HEIGHT': begin							; new height (whole window)
					(*pstate).height = vset.height - (*pstate).dh
					widget_control, (*pstate).draw, scr_ysize=(*pstate).height
					setview = 1
					redisplay = 1
					end
				'IMAGE': begin							; new image pointer
					if ptr_valid(vset.image) eq 0 then begin
						warning,'simple_image_set','Bad image pointer passed.'
					endif else begin
						(*pstate).p = vset.image
						p = (*pstate).p
						xanes_stack_test, p, xanes, n_el, el, el_xanes
						widget_control, (*pstate).Element_combobox, set_value=el
						(*pstate).N = (*pstate).N < (n_el -1)
						widget_control, (*pstate).Element_combobox, set_combobox_select=(*pstate).N
						redisplay = 1
					endelse
					end
				'ELEMENT': begin
					(*pstate).N = vset.element < (n_el - 1)
					widget_control, (*pstate).Element_combobox, set_combobox_select=(*pstate).N
					redisplay = 1
					end
				'DISPLAY': begin						; trigger redisplay
					redisplay = 1
					end
				'SHAPE': begin							; overlay shape (device coords)
					if tag_present('X',vset.shape) then begin
						*(*pstate).shape.px = vset.shape.x
					endif
					if tag_present('Y',vset.shape) then begin
						*(*pstate).shape.py = vset.shape.y
					endif
					redisplay = 1
					end
				'LIST'	: begin
					(*pstate).N = (*pstate).N < (n_el - 1)
					widget_control, (*pstate).Element_combobox, set_value=el
					redisplay = 1
					end
				'CLEAR': begin							; clear marker area
					(*pstate).crop.on = 0
					redisplay = 1
					end
				'CROP': begin
					p = (*pstate).p
					if size(p,/tname) ne 'POINTER' then return
					if ptr_valid(p) eq 0 then return
					if size(*p,/tname) ne 'STRUCT' then return
					(*pstate).crop.x[0] = clip(min(vset.crop.x), 0, (*(*pstate).p).xsize-1)
					(*pstate).crop.x[1] = clip(max(vset.crop.x), 0, (*(*pstate).p).xsize-1)
					(*pstate).crop.y[0] = clip(min(vset.crop.y), 0, (*(*pstate).p).ysize-1)
					(*pstate).crop.y[1] = clip(max(vset.crop.y), 0, (*(*pstate).p).ysize-1)
					(*pstate).crop.on = 1
					if (*pstate).crop.y[1] le (*pstate).crop.y[0] then (*pstate).crop.on = 0
					if (*pstate).crop.x[1] le (*pstate).crop.x[0] then (*pstate).crop.on = 0
					print, (*pstate).crop
					redisplay = 1
					end
				else:
			endcase
		endfor
		if setview then begin
			set_simple_image_view, pstate
			redisplay = 0
		endif
		if redisplay then begin
			p = (*pstate).p
			if size(p,/tname) ne 'POINTER' then return
			if ptr_valid(p) eq 0 then return
			if size(*p,/tname) ne 'STRUCT' then return
			i = (*pstate).n
			pimg = (*p).image
			opt = (*p).options
			if ptr_valid( pimg ) eq 0 then return
			widget_control, (*pstate).bottom_slider, set_value = fix((*opt)[i].bottom)
			widget_control, (*pstate).top_slider, set_value = fix((*opt)[i].top)
			widget_control, (*pstate).Zscale_mode, set_combobox_select = (((*opt)[i].log >0)<2)
			draw_simple_images, pstate
		endif
	endif
endif
return
end

;-----------------------------------------------------------------------------

function simple_image_get, id

child = widget_info( id, /child)
widget_control, child, get_uvalue=pstate

return, (*pstate).percent
end

;-----------------------------------------------------------------

pro OnDestroy_Simple_Image, wWidget

parent = widget_info( wWidget, /parent)
widget_control, parent, get_uvalue=pstate

free_simple_image_state, pstate
return
end

;-----------------------------------------------------------------

pro OnRealize_Simple_Image_Element, wWidget

parent = widget_info( wWidget, /parent)
parent = widget_info( parent, /parent)
widget_control, parent, get_uvalue=pstate

widget_control, wWidget, set_combobox_select = (*pstate).N
end

;-----------------------------------------------------------------

pro OnRealize_Simple_Image_Zscale_Mode, wWidget

parent = widget_info( wWidget, /parent)
parent = widget_info( parent, /parent)
widget_control, parent, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p ) eq 0 then return	
i = (*pstate).n
pimg = (*p).image
opt = (*p).options

if ptr_good(opt) then widget_control, wWidget, set_combobox_select = (*opt)[i].log
end

;-----------------------------------------------------------------

pro OnRealize_Simple_Image, wWidget

COMPILE_OPT STRICTARR

; Can't use usually trick of searching up for tlb_top, as this finds top, not the
; top of THIS CW. Instead, we've stored the child widget ID in uvalue of this Draw widget.

parent = widget_info( wWidget, /parent)
widget_control, parent, get_uvalue=pstate

widget_control, wWidget, get_value=wid
wset,wid
(*pstate).wid = wid

if (*pstate).box_select then begin
	window, /free, xsize=(*pstate).width, ysize=(*pstate).height, /pixmap
	(*pstate).pix = !d.window
endif

if ptr_valid( (*pstate).p) then draw_simple_images, pstate
return
end

;------------------------------------------------------------------------------------------

function simple_image, parent, top_tlb=top_tlb, pimage=pimage, Nimage=Nimage, width=width, height=height, $
					tracking=tracking, box_select=box_select, uvalue=uvalue, cursor=cursor, $
					border=with_border, ganged=ganged, _EXTRA=_VWBExtra_

; CW simple image display, 'Nimage' is the initial element to display
; width, height are for overall, need to calculate the smaller draw size internally.
; 
; 'parent' is the parent widget (base)
; 'top_tlb' is the parent window's TLB
; 
; /border	to add axes and plot image within the axes box
; /ganged	to gang all min/max and Z-scale controls
; /box_select	to allow a box shape for zoom
; /cursor	to ...
;
; Several parameters come via the Extras mechanism:
; xoffset, yoffset are for use in a bulletin board 'parent' base
; uname, uvalue, xpad, ypad, space, xoffset, yoffset

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
	   Catch, /cancel
	   on_error, 1
	   help, calls = s
	   n = n_elements(s)
	   c = 'Call stack: '
	   if n gt 2 then c = [c, s[1:n-2]]
	   warning,'simple_image',['IDL run-time error caught.', '', $
		  'Error:  '+strtrim(!error_state.name,2), $
		  !Error_state.msg,'',c], /error
	   MESSAGE, /RESET
	   return, 0L
	endif
endif

if n_elements(parent) lt 1 then return,0L
if n_elements(tracking) lt 1 then tracking=0
if n_elements(with_border) lt 1 then with_border=0
if n_elements(cursor) lt 1 then cursor=0
if n_elements(ganged) lt 1 then ganged=0
if n_elements(uvalue) lt 1 then uvalue=''
if n_elements(Nimage) lt 1 then Nimage=0
if n_elements(box_select) lt 1 then box_select=0
if n_elements(width) lt 1 then width=200
if n_elements(height) lt 1 then height=200
simple_image_routines

pimage = bad_pars_struct( pimage, make_pars=make_pd)
if make_pd then begin
	warning,'simple_image','No "pimage" supplied for image.'
	return, 0L
endif

if ptr_good( pimage) then begin
	xanes_stack_test, pimage, xanes, n_el, el, el_xanes
endif else begin
	n_el = 1
	el = [' ']
endelse

case !version.os_family of
	'MacOS': begin
		retain=2
		dw = 5
		dh = 27
		wmin = 187
		xsize_element = 48
		xsize_analyze_mode = 38
		end
	'unix': begin
		retain=2
		dw = 5
		dh = 27
		wmin = 187
		xsize_element = 48
		xsize_analyze_mode = 38
		end
	else: begin
		retain=1
		dw = 5
		dh = 27
		wmin = 187
		xsize_element = 45
		xsize_analyze_mode = 34
		end
endcase
xsize_slide = 47 + 0.37*(width-wmin)				; 2* 0.37 + (0.16 + 0.1 below) = 1.0
													; these factors in _set routine too.

border = [0.08,0.06,0.97,0.96]

mark = {present:0, x:fltarr(5), y:fltarr(5), px:fltarr(5), py:fltarr(5)  }	; box

state = {	p:				pimage, $				; pointer to image data struct
			N:				Nimage, $				; element to display
;			Zmode:			0, $					; Z scale mode value
			low:			0, $					; low slider
			high:			100, $					; high slider
			wid:			0L, $					; draw window #
			pix:			0L, $					; pixmap window id
			crop:	{ on:	0, $					; crop is enabled
					x:		intarr(2), $			; crop X range
					y:		intarr(2) }, $			; crop Y range
			width:			width-dw, $				; draw area
			height:			height-dh, $			;
			with_border:	with_border, $			; add a border to image plot
			border:			border, $				; border to image plot
			dw:				dw, $					; adjust scr size to get Draw area size
			dh:				dh, $					;
			wmin:			wmin, $					; min w
			el_base:		xsize_element, $		; X size min of element
			mode_base:		xsize_analyze_mode, $	; X size min of mode
			cursor:			cursor, $				; cursor mode on/off
			ganged:			ganged, $				; gang controls together

			pmark:			ptr_new( mark, /no_copy), $	; spline 10 marker
			id:				0, $					; id of point being moved
			left_button:	0, $					; flags left mouse button
			right_button:	0, $					; right mouse
			box_select:		box_select, $			; enable draw select box
			last_time:		systime(/seconds), $	; time last
			percent:		0.0, $					; % time on display
			
			shape: {px:	ptr_new(/allocate_heap), $	; pointer to overlay shape X coords (device)
					py:	ptr_new(/allocate_heap)}, $	; pointer to overlay shape Y coords (device)
						
			Draw:			0L, $					; draw widget ID
			Element_combobox: 0L, $					; element combobox ID
			Bottom_Slider:	0L, $					; bottom slider ID
			Top_Slider:		0L, $					; top slider ID
			Zscale_Mode:	0L, $					; Z scale Mode ID
			cursor_id:		0L $					; ID of draw with cursor
	   }
pstate = ptr_new(state, /no_copy)

tlb = widget_base( parent, pro_set_value='simple_image_set', func_get_value='simple_image_get', $
			event_func='simple_image_event', /column, uvalue='test base ...', _EXTRA=_VWBExtra_ )

Image_Draw_Base = Widget_Base( TLB, UNAME='image-draw-base', SPACE=0, XPAD=0, YPAD=0, /column, uvalue=pstate)

Draw = Widget_Draw(Image_Draw_Base, UNAME='image-draw', SCR_XSIZE=width-dw, SCR_YSIZE=height-dh,  $
			NOTIFY_REALIZE='OnRealize_Simple_Image', tracking=cursor, uvalue=uvalue,  $
			KILL_NOTIFY='OnDestroy_Simple_Image', SCROLL=0, retain=retain, BUTTON_EVENTS=box_select)
(*pstate).draw = draw
if cursor then (*pstate).cursor_id = draw

Image_Button_Top_Base = Widget_Base(Image_Draw_Base, UNAME='image-button-top-base', SPACE=1, XPAD=0, YPAD=0, /ROW, $
			/align_center, /base_align_center)

Element_combobox = widget_combobox(Image_Button_Top_Base, UNAME='element-combobox', $
			NOTIFY_REALIZE='OnRealize_Simple_Image_Element',  $
			VALUE=el, tracking=tracking, xsize=xsize_element + 0.1*(width-wmin), $
			uvalue='Droplist to select the element image to display.')
(*pstate).Element_combobox = Element_combobox

Bottom_Slider = widget_slider( Image_Button_Top_Base, minimum=0, maximum=100, /drag, tracking=tracking, $
			uvalue='Adjust minimum image value to display, as a % of max.', uname='bottom-slider', $
;			NOTIFY_REALIZE='OnRealize_Simple_Image_Bottom_Slider', $
			xsize=xsize_slide, value=0, /suppress_value)
(*pstate).Bottom_Slider = Bottom_Slider

Zscale_Mode = widget_combobox(Image_Button_Top_Base, xsize=xsize_analyze_mode + 0.16*(width-wmin),  $
			NOTIFY_REALIZE='OnRealize_Simple_Image_Zscale_Mode',  $
			UNAME='zscale-mode-combobox', VALUE=[ 'L linear  ', 'G log', 'S sqrt' ], tracking=tracking, $
			uvalue='Select image Z scale display mode between Linear, Log and Sqrt. Adjust limits with sliders.')
;Zscale_Mode = 0L
(*pstate).Zscale_Mode = Zscale_Mode

Top_Slider = widget_slider( Image_Button_Top_Base, minimum=0, maximum=300, /drag, tracking=tracking, $
;			NOTIFY_REALIZE='OnRealize_Simple_Image_Top_Slider', $
			uvalue='Adjust maximum image value to display, as a % of max.', uname='top-slider', $
			xsize=xsize_slide, value=100, /suppress_value)
(*pstate).Top_Slider = Top_Slider

widget_control, tlb, set_uname=uname, set_uvalue=uvalue

return, tlb
end


