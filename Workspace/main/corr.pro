pro corr_event, Event

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
		warning,'corr_Event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

  wWidget =  Event.top
  widget_control, hourglass=0
;  help, event,/structure

	case tag_names( event,/structure) of
		'NOTIFY': begin
			OnNotify_corr, event
			return
			end
		'WIDGET_TRACKING': begin
			OnTracking_corr, Event
			return
			end
		else:
	endcase

  uname = widget_info( event.id, /uname)

  case uname of

	'corr_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_KILL_REQUEST': begin
				OnKill_corr, Event
				end
			'WIDGET_BASE': begin
				OnSize_corr, Event
				end
			else:
		endcase
		end

	'corr_Draw': begin
		if( Event.type eq 3 )then begin
			OnViewport_corr, Event
		endif else begin
			OnButton_corr, Event
		endelse
		end

	'Element_comboboxx': begin
		OnSelect_corr_Elementx, Event
		end

	'Element_comboboxy': begin
		OnSelect_corr_Elementy, Event
		end

	'Full_Button': begin
		OnButton_corr_Full, Event
		end

	'Zoom_In_Button': begin
		OnButton_corr_Zoom_In, Event
		end

	'Zoom_Out_Button': begin
		OnButton_corr_Zoom_Out, Event
		end

	'Low_Slider': begin
		OnMove_corr_Low, Event
		end

	'High_Slider': begin
		OnMove_corr_High, Event
		end

	'Zaxis_combobox': begin
		OnSelect_corr_Zaxis, Event
		end

	'Y_low_Slider': begin
		OnMove_corr_Y, Event
		end

	'X_low_Slider': begin
		OnMove_corr_X, Event
		end

	'Y_high_Slider': begin
		OnMove_corr_Y, Event, /high
		end

	'X_high_Slider': begin
		OnMove_corr_X, Event, /high
		end

	'Y_smooth_Slider': begin
		OnMove_corr_Y_smooth, Event
		end

	'X_smooth_Slider': begin
		OnMove_corr_X_smooth, Event
		end

	'xlog_button': begin
		OnSelect_corr_Xlog, Event
		end

	'ylog_button': begin
		OnSelect_corr_Ylog, Event
		end

	'highlight_button': begin
		OnSelect_corr_Highlight, Event
		end

	'Save_GIF_Menu': begin
		corr_Save_GIF, Event
		end

	'Save_PNG_Menu': begin
		corr_Save_GIF, Event, /PNG
		end

	'Export_Menu': begin
		corr_export, Event, /jpeg
		end

	'Clear_Spline_Menu': begin
		corr_clear_spline, Event
		end

	'Clear_QC_Menu': begin
		corr_clear_qc, Event
		end

	'Analyze_Spline_Menu': begin
		corr_analyze_spline, Event, /fresh
		end

	'Analyze_Spline_refine_Menu': begin
		corr_analyze_spline, Event, fresh=0
		end

	'Analyze_Spline_exclude_Menu': begin
		corr_analyze_spline, Event, fresh=0, /exclude
		end

	'Exit_Menu': begin
		corr_Exit, Event
		end

	'Clone_Menu': begin
		corr_Clone, Event
		end

	'Clone_Menu_1': begin
		corr_Clone, Event, 1
		end

	'Clone_Menu_3': begin
		corr_Clone, Event, 3
		end

	'Clone_Menu_7': begin
		corr_Clone, Event, 7
		end

	'Clone_Menu_11': begin
		corr_Clone, Event, 11
		end

	'Colour_Table': begin
		corr_Colours, Event
		end

	'Default_Colour_Table': begin
		corr_Colours, Event, 5
		end

	'Grey_Scale': begin
		corr_Colours, Event, 0
		end

	'Invert_Colour_Table': begin
		corr_Invert_Colours, Event
		end

	'Linear_Luminance': begin
		corr_Linear_Luminance, Event
		end

	else:
  endcase
	widget_control, hourglass=0
end

;-----------------------------------------------------------------------------------

pro corr, GROUP_LEADER=wGroup, TLB=corr_TLB, pimages=pimages, qregion=qregion, $
						path=path, title=title, _EXTRA=_VWBExtra_

COMPILE_OPT STRICTARR
common c_errors_1, catch_errors_on

if n_elements(wGroup) lt 1 then wGroup=0L
if n_elements(path) lt 1 then path=''
if n_elements(title) lt 1 then title='Associations'


ErrorNo = 0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'corr',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

  	corr_routines					    ; Load support routines
  	corr_eventcb						; Load event callback routines
  	register_notify

  case !version.os_family of
	'MacOS': begin
		symbol = 'SYMBOL*12'
		large_font = 'Arial*12'
		def_font = 'Geneva*10'
		draw_trim = 15
		scr_trim = 21
		help1_xsize = 56
		mode_xsize = 68
		slide_xsize = 112
		button_height = 20
		xsize_element = 95
		smooth_slide_xsize = 72
		bot_slide_xsize = 73
		end
	'unix': begin
		symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
		large_font = '10x20'
		def_font = '6x13'
		draw_trim = 0
		scr_trim = 15
		help1_xsize = 56
		mode_xsize = 70
		slide_xsize = 112
		button_height = 29
		xsize_element = 95
		smooth_slide_xsize = 52
		bot_slide_xsize = 81
		end
	else: begin
		symbol = 'SYMBOL*BOLD*14'
		large_font = 'COURIER*BOLD*10'
		def_font = 'Arial*14'
		draw_trim = 0
		scr_trim = 15
		help1_xsize = 61					; 45
		mode_xsize = 50
		slide_xsize = 140					; 140
		button_height = 21
		xsize_element = 72					; 62
		smooth_slide_xsize = 68				; 70
		bot_slide_xsize = 79				; 73
 		end
  endcase
;@2  widget_control, default_font=def_font        ; set font for all windows

pixx = 280
pixy = 280
margin = {low:70, high:20, bottom:35, top:20}

use_gif = 0
if extract(!version.release,0,2) eq '5.3' then use_gif=1	; enable GIF output, suppress PNG
															; use PNG only with IDL 5.4 (mirror bug)
  pimages = bad_pars_struct( pimages, make_pars=no_images)
  if no_images then begin
;	warning,'Corr',['There are no images to display.','Load images first.']
;	return
  endif
  qregion = bad_pars_struct( qregion, make_pars=no_q)
  if no_q then qregion = ptr_new()

  corr_TLB = Widget_Base( GROUP_LEADER=wGroup,  $
      UNAME='corr_TLB' ,/TLB_KILL_REQUEST_EVENTS, /base_align_center  $
      ,/TLB_SIZE_EVENTS ,TITLE=title ,SPACE=2 ,XPAD=1 ,YPAD=1  $
      ,COLUMN=1 ,MBAR=corr_TLB_MBAR, _EXTRA=_VWBExtra_)

  if !version.os_family eq 'unix' then begin
	retain=2
  endif else begin
	retain=1
  endelse

	w = pixx + margin.low + margin.high
	h = pixy + margin.bottom + margin.top
	print,'Corr: request w,h=',w,h
	
  corr_draw_tab = widget_tab( corr_TLB, location=3, /align_center, uname='tab-panel')	; , scr_xsize=help_xsize

  corr_Draw_Base = Widget_Base(corr_draw_tab, title=' Association ', UNAME='corr_Draw_Base' ,SPACE=0, XPAD=0 ,YPAD=0, $
  		/column, /base_align_center)

  corr_Draw = Widget_Draw(corr_Draw_Base, UNAME='corr_Draw', SCR_XSIZE=w+scr_trim, SCR_YSIZE=h+scr_trim,  $
      	x_scroll_SIZE=w+scr_trim, y_scroll_SIZE=h+scr_trim, NOTIFY_REALIZE='OnRealize_corr',  $
		KILL_NOTIFY='OnDestroy_corr', /SCROLL, XSIZE=w+draw_trim, YSIZE=h+draw_trim, RETAIN=retain, $
		/BUTTON_EVENTS, /VIEWPORT_EVENTS)


  corr_DrawX_Base = Widget_Base(corr_draw_tab, title=' X Histogram ', UNAME='corr_DrawX_Base' ,SPACE=0, XPAD=0 ,YPAD=0, $
  		/column, /base_align_center)

  corr_DrawX = Widget_Draw(corr_DrawX_Base, UNAME='corr_DrawX', SCR_XSIZE=w+scr_trim, SCR_YSIZE=h+scr_trim,  $
      	NOTIFY_REALIZE='OnRealize_corrX',  XSIZE=w+draw_trim, YSIZE=h+draw_trim, RETAIN=retain)


  corr_DrawY_Base = Widget_Base(corr_draw_tab, title=' Y Histogram ', UNAME='corr_DrawY_Base' ,SPACE=0, XPAD=0 ,YPAD=0, $
  		/column, /base_align_center)

  corr_DrawY = Widget_Draw(corr_DrawY_Base, UNAME='corr_DrawY', SCR_XSIZE=w+scr_trim, SCR_YSIZE=h+scr_trim,  $
      	NOTIFY_REALIZE='OnRealize_corrY',  XSIZE=w+draw_trim, YSIZE=h+draw_trim, RETAIN=retain)



  corr_Button_Base = Widget_Base(corr_TLB, UNAME='corr_Button_Base'  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,column=1, /align_center, /base_align_center)


  corr_Button_Base1 = Widget_Base(corr_Button_Base, UNAME='corr_Button_Base1'  $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,/row, /align_center)


  corr_Button_Base2 = Widget_Base(corr_Button_Base1, UNAME='corr_Button_Base2'  $
      ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/column, /align_center, /base_align_center)


  corr_Button_Top_Base = Widget_Base(corr_Button_Base2, UNAME='corr_Button_Top_Base'  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_center, /base_align_bottom)


  corr_Button_Bot_Base = Widget_Base(corr_Button_Base2, UNAME='corr_Button_Bot_Base'  $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_center, /base_align_bottom)


  corr_Button_Bot2_Base = Widget_Base(corr_Button_Base2, UNAME='corr_Button_Bot_Base'  $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_center, /base_align_bottom)


  corr_Help1_Base = Widget_Base(corr_Button_Base, UNAME='corr_Help1_Base', map=1  $
      ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/row, /align_center)


  corr_Help2_Base = Widget_Base(corr_Button_Base1, UNAME='corr_Help2_Base', map=0  $
      ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/row, /align_center)


  PostCreate_corr_Base, corr_Draw_Base, corr_Help1_Base, corr_Help2_Base, path=path, $
  			pimages=pimages, qregion=qregion, parent=wGroup, clone=clone, $
  			width=pixx, height=pixy, margin=margin, _EXTRA=_VWBExtra_

  PostCreate_corr, corr_Draw, _EXTRA=_VWBExtra_


;  label = widget_text( corr_Button_Top_Base, value='Log', xsize=3, /align_bottom, frame=0)

  Element_comboboxy = Widget_combobox(corr_Button_Top_Base,  $
      UNAME='Element_comboboxy' ,NOTIFY_REALIZE='OnRealize_Corr_Elementy',  $
      VALUE=[ '  ', ' Sm   ' ], tracking_events=1, /align_bottom, xsize=xsize_element, $
      uvalue='Droplist to select the element to display on the Y axis.')

  Y_smooth_Slider = widget_slider( corr_Button_Top_Base, minimum=0, maximum=10, /drag, /tracking, $
	uvalue='Adjust the pre-smooth for the Y element data. This is applied before binning associations into the 2D histogram.', uname='Y_smooth_Slider', $
	xsize=smooth_slide_xsize, value=1, NOTIFY_REALIZE='OnRealize_corr_Y_smooth_Slider', /align_bottom)

  space = widget_base(corr_Button_Top_Base, xsize=5)

  Full_Button = Widget_Button(corr_Button_Top_Base, UNAME='Full_Button'  $
      ,/ALIGN_CENTER ,VALUE='0', tracking_events=1, font=large_font, /align_bottom, $
      uvalue='Show original size of Associations 2D histogram in window.',xsize=20,ysize=button_height)

  Zoom_In_Button = Widget_Button(corr_Button_Top_Base, UNAME='Zoom_In_Button'  $
      ,/ALIGN_CENTER ,VALUE='+', tracking_events=1, font=large_font, /align_bottom, $
      uvalue='Magnify Associations 2D histogram x2.',xsize=20,ysize=button_height)

  Zoom_Out_Button = Widget_Button(corr_Button_Top_Base, UNAME='Zoom_Out_Button'  $
      ,/ALIGN_CENTER ,VALUE='-', tracking_events=1, font=large_font, /align_bottom, $
      uvalue='Demagnify Assocations 2D histogram x2.',xsize=20,ysize=button_height)

  space = widget_base(corr_Button_Top_Base, xsize=5)

  X_smooth_Slider = widget_slider( corr_Button_Top_Base, minimum=0, maximum=10, /drag, /tracking, $
	uvalue='Adjust the pre-smooth for the X element data. This is applied before binning associations into the 2D histogram.', uname='X_smooth_Slider', $
	xsize=smooth_slide_xsize, value=1, NOTIFY_REALIZE='OnRealize_corr_X_smooth_Slider', /align_bottom)

  Element_comboboxx = Widget_combobox(corr_Button_Top_Base, /align_bottom,  $
      UNAME='Element_comboboxx' ,NOTIFY_REALIZE='OnRealize_Corr_Elementx',  $
      VALUE=[ '  ', ' Sm   ' ], tracking_events=1, xsize=xsize_element, $
      uvalue='Droplist to select the element to display on the X axis.')


  ylog_base = widget_base(corr_Button_Bot_Base, /nonexclusive, xsize=15, /row, /align_top, SPACE=0 ,XPAD=0 ,YPAD=0)
  ylog_button = widget_button( ylog_base, value='', /tracking, uvalue='Enable/disable log Y scale.', $
  				uname='ylog_button', xsize=20, ysize=20, /align_top, $
  				NOTIFY_REALIZE='OnRealize_corr_Ylog')

  Y_Slider = widget_slider( corr_Button_Bot_Base, minimum=0, maximum=100, /drag, /tracking, $
	uvalue='Adjust the low end of the Y element range to display.', uname='Y_low_Slider', $
	xsize=bot_slide_xsize, value=0, NOTIFY_REALIZE='OnRealize_corr_Y_Slider', /align_bottom)

  Y_Slider_top = widget_slider( corr_Button_Bot_Base, minimum=0, maximum=100, /drag, /tracking, $
	uvalue='Adjust the high end of the Y element range to display.', uname='Y_high_Slider', $
	xsize=bot_slide_xsize, value=100, NOTIFY_REALIZE='OnRealize_corr_Y_Slider_top', /align_bottom)

  space = widget_base(corr_Button_Bot_Base, xsize=5)

  X_Slider = widget_slider( corr_Button_Bot_Base, minimum=0, maximum=100, /drag, /tracking, $
	uvalue='Adjust the low end of the X element range to display.', uname='X_low_Slider', $
	xsize=bot_slide_xsize, value=0, NOTIFY_REALIZE='OnRealize_corr_X_Slider', /align_bottom)

  X_Slider_top = widget_slider( corr_Button_Bot_Base, minimum=0, maximum=100, /drag, /tracking, $
	uvalue='Adjust the high end of the X element range to display.', uname='X_high_Slider', $
	xsize=bot_slide_xsize, value=100, NOTIFY_REALIZE='OnRealize_corr_X_Slider_top', /align_bottom)

  xlog_base = widget_base(corr_Button_Bot_Base, /nonexclusive, xsize=15, /row, /align_top, SPACE=0 ,XPAD=0 ,YPAD=0)
  xlog_button = widget_button( xlog_base, value='', /tracking, uvalue='Enable/disable log X scale.', $
  				uname='xlog_button', xsize=20, ysize=20, /align_top, $
  				NOTIFY_REALIZE='OnRealize_corr_Xlog')


  Low_Slider = widget_slider( corr_Button_Bot2_Base, minimum=0, maximum=100, /drag, /tracking, $
	uvalue='Adjust minimum Z scale of 2D histogram to display, as % of max.', uname='Low_Slider', $
	xsize=slide_xsize, value=0, NOTIFY_REALIZE='OnRealize_corr_Low_Slider', /align_bottom)

  Zaxis_comboboxx = Widget_combobox(corr_Button_Bot2_Base, /align_bottom,  $
      UNAME='Zaxis_combobox' , NOTIFY_REALIZE='OnRealize_corr_Zaxis', $
      VALUE=[ 'Linear Z', 'Sqrt Z', 'Log Z' ], tracking_events=1, xsize=xsize_element, $
      uvalue='Droplist to select the Z axis scale mode for the 2D histgram display.')

  highlight_base = widget_base(corr_Button_Bot2_Base, /nonexclusive, xsize=15, /row, /align_top, SPACE=0 ,XPAD=0 ,YPAD=0)
  highlight_button = widget_button( highlight_base, value='', /tracking, uvalue='Enable/disable green highlight of selected pixels. ' + $
  				'Click and drag to draw a spline shape to select pixels. Right-click to clear spline. Use "Analyze" menus to analyze within spline.', $
  				uname='highlight_button', xsize=20, ysize=20, /align_top, $
  				NOTIFY_REALIZE='OnRealize_corr_highlight')

  High_Slider = widget_slider( corr_Button_Bot2_Base, minimum=0, maximum=100, /drag, /tracking, $
	uvalue='Adjust maximum Z scale of 2D histogram to display, as % of max.', uname='High_Slider', $
	xsize=slide_xsize, value=100, NOTIFY_REALIZE='OnRealize_corr_High_Slider', /align_bottom)


  Help_Text1 = Widget_Text(corr_Help1_Base, UNAME='Help_Text1', /wrap $
      ,NOTIFY_REALIZE='OnRealize_corr_Help1',XSIZE=help1_xsize, frame=0 ,YSIZE=3, $
      tracking_events=1, uvalue='Help window to show help prompts for widgets.')


  Help_Text2 = Widget_Text(corr_Help2_Base, UNAME='Help_Text2', /wrap $
      ,NOTIFY_REALIZE='OnRealize_corr_Help2',scr_XSIZE=1, frame=0 ,scr_YSIZE=102, ysize=5, $
      tracking_events=1, uvalue='Help window to show help prompts for widgets.')


 ; File menus

  W_MENU_0 = Widget_Button(corr_TLB_MBAR, UNAME='W_MENU_0' ,/MENU  $
      ,VALUE='File')


if use_gif then begin
	  W_MENU_3 = Widget_Button(W_MENU_0, UNAME='Save_GIF_Menu' ,/SEPARATOR  $
	      ,VALUE='Save as GIF')
endif else begin
	  W_MENU_3b = Widget_Button(W_MENU_0, UNAME='Save_PNG_Menu' ,/SEPARATOR  $
	      ,VALUE='Save as PNG')
endelse

  W_MENU_4 = Widget_Button(W_MENU_0, UNAME='Export_Menu' ,/SEPARATOR  $
	      ,VALUE='Export')


  W_MENU_6 = Widget_Button(W_MENU_0, UNAME='Exit_Menu' ,/SEPARATOR  $
      ,VALUE='Exit')


; Display menus

  W_MENU_20 = Widget_Button(corr_TLB_MBAR, UNAME='W_MENU_20'  $
      ,/MENU ,VALUE='Display')


  W_MENU_21 = Widget_Button(W_MENU_20, UNAME='Clone_Menu', VALUE='Clone', /menu)

  W_MENU_211 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_1', VALUE='1 Clone')

  W_MENU_212 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_3', VALUE='3 Clone')

  W_MENU_213 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_7', VALUE='7 Clone')

  W_MENU_214 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_11', VALUE='11 Clone')


  W_MENU_23 = Widget_Button(W_MENU_20, UNAME='Colours_Menu', VALUE='Colours',/SEPARATOR, /menu )

  W_MENU_2310 = Widget_Button(W_MENU_23, UNAME='Colour_Table', VALUE='Colour Table' )

  W_MENU_231 = Widget_Button(W_MENU_23, UNAME='Default_Colour_Table', VALUE='Default Colours' )

  W_MENU_232 = Widget_Button(W_MENU_23, UNAME='Grey_Scale', VALUE='Grey Scale')

  W_MENU_233 = Widget_Button(W_MENU_23, UNAME='Invert_Colour_Table', VALUE='Invert Colour Table',/SEPARATOR  )

  W_MENU_234 = Widget_Button(W_MENU_23, UNAME='Linear_Luminance', VALUE='Linear Luminance' )


  W_MENU_22 = Widget_Button(W_MENU_20, UNAME='Clear_Spline_Menu', VALUE='Clear Spline', /separator)


; Analyze menus

  W_MENU_40 = Widget_Button(corr_TLB_MBAR, UNAME='W_MENU_40'  $
      ,/MENU ,VALUE='Analyze')


  W_MENU_401 = Widget_Button(W_MENU_40, UNAME='Analyze_Spline_Menu', VALUE='Include points within Spline' )

  W_MENU_402 = Widget_Button(W_MENU_40, UNAME='Analyze_Spline_refine_Menu', VALUE='Further refine within Spline' )

  W_MENU_403 = Widget_Button(W_MENU_40, UNAME='Analyze_Spline_exclude_Menu', VALUE='Reject selection within Spline' )

  W_MENU_404 = Widget_Button(W_MENU_40, UNAME='Clear_QC_Menu', VALUE='Clear selected pixels', /separator)

  Widget_Control, /REALIZE, corr_TLB

  set_corr_map_help, corr_TLB

  if wGroup ne 0 then begin
  	register_notify, corr_TLB, $
  				['images-changed', $			; new Images loaded
				'image-display', $				; images display/processing changes
				'images', $						; new images
				'path', $						; new path
				'image-region-select', $		; region selected
				'image-analyze-q', $			; new q array
				'image-analyze-all-clear', $	; clear q array
				'corr-analyze-clear', $			; pass on notify of qc destined for image
				'corr-analyze-spline', $		; pass on notify of qc cleared destined for image
  				'corr-display' $				; corr display needs updating (smooth...)
		  		], from=wGroup
  endif

  XManager, 'corr', corr_TLB, /no_block

end
;

