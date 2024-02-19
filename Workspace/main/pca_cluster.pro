pro pca_cluster_event, Event

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
		warning,'pca_cluster_Event',['IDL run-time error caught.', '', $
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
			OnNotify_pca_cluster, event
			return
			end
		'WIDGET_TRACKING': begin
			OnTracking_pca_cluster, Event
			return
			end
		else:
	endcase

  uname = widget_info( event.id, /uname)

  case uname of

	'pca_cluster_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_KILL_REQUEST': begin
				OnKill_pca_cluster, Event
				end
			'WIDGET_BASE': begin
				OnSize_pca_cluster, Event
				end
			else:
		endcase
		end

	'pca_cluster_tool_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_KILL_REQUEST': begin
				OnKill_pca_cluster, Event
				end
			else:
		endcase
		end

	'pca_cluster_Draw': begin
		if( Event.type eq 3 )then begin
			OnViewport_pca_cluster, Event
		endif else begin
			OnButton_pca_cluster, Event
		endelse
		end

	'Element_comboboxx': begin
		OnSelect_pca_cluster_Elementx, Event
		end

	'Element_comboboxy': begin
		OnSelect_pca_cluster_Elementy, Event
		end

	'Full_Button': begin
		OnButton_pca_cluster_Full, Event
		end

	'Zoom_In_Button': begin
		OnButton_pca_cluster_Zoom_In, Event
		end

	'Zoom_Out_Button': begin
		OnButton_pca_cluster_Zoom_Out, Event
		end

	'Low_Slider': begin
		OnMove_pca_cluster_Low, Event
		end

	'High_Slider': begin
		OnMove_pca_cluster_High, Event
		end

	'Threshold_Slider': begin
		OnMove_pca_cluster_threshold, Event
		end

	'Average_High_Slider': begin
		OnMove_pca_cluster_average_high, Event
		end

	'Average_Low_Slider': begin
		OnMove_pca_cluster_average_low, Event
		end

	'Show_options': begin
		OnSelect_pca_cluster_highlight, event
		end

	'Class_Max_Slider': begin
		OnMove_pca_cluster_class_max, Event
		end

	'Class_Slider': begin
		OnMove_pca_cluster_class, Event
		end

	'Components_Slider': begin
		OnMove_pca_cluster_Components, Event
		end

	'Cluster_Source': begin
		OnMove_pca_cluster_source, Event
		end

	'Zaxis_combobox': begin
		OnSelect_pca_cluster_Zaxis, Event
		end

	'Cluster_Show_Button': begin
		OnSelect_pca_cluster_class_show, Event
		end
 
	'Cluster_Select_Button': begin
		pca_cluster_select_Cluster, Event
		end
 
	'Y_low_Slider': begin
		OnMove_pca_cluster_Y, Event
		end

	'X_low_Slider': begin
		OnMove_pca_cluster_X, Event
		end

	'Y_high_Slider': begin
		OnMove_pca_cluster_Y, Event, /high
		end

	'X_high_Slider': begin
		OnMove_pca_cluster_X, Event, /high
		end

	'Smooth_Slider': begin
		OnMove_pca_cluster_smooth, Event
		end

	'Border_Slider': begin
		OnMove_pca_cluster_border, Event
		end

	'xlog_button': begin
		OnSelect_pca_cluster_Xlog, Event
		end

	'ylog_button': begin
		OnSelect_pca_cluster_Ylog, Event
		end

	'Normalize_Button': begin
		OnSelect_pca_cluster_normalize, Event
		end

	'Save_GIF_Menu': begin
		pca_cluster_Save_GIF, Event
		end

	'Save_PNG_Menu': begin
		pca_cluster_Save_GIF, Event, /PNG
		end

	'Export_Menu': begin
		pca_cluster_export, Event, /cgm
		end

	'Clear_Spline_Menu': begin
		pca_cluster_clear_spline, Event
		end

	'PCA_Button': begin
		pca_cluster_Analyze_PCA, event
		end 

	'Plot_PCA_Eigen': begin
		pca_cluster_plot_eigen, event
		end 
		
	'Cluster_Button': begin
		pca_cluster_Analyze_Clusters, event
		end 

	'Analyze_Spline_Menu': begin
		pca_cluster_analyze_spline, Event
		end

	'Exit_Menu': begin
		pca_cluster_Exit, Event
		end

	'Clone_Menu': begin
		pca_cluster_Clone, Event
		end

	'Clone_Menu_1': begin
		pca_cluster_Clone, Event, 1
		end

	'Clone_Menu_3': begin
		pca_cluster_Clone, Event, 3
		end

	'Clone_Menu_7': begin
		pca_cluster_Clone, Event, 7
		end

	'Clone_Menu_11': begin
		pca_cluster_Clone, Event, 11
		end

	'Colour_Table': begin
		pca_cluster_Colours, Event
		end

	'Default_Colour_Table': begin
		pca_cluster_Colours, Event, 5
		end

	'Grey_Scale': begin
		pca_cluster_Colours, Event, 0
		end

	'Invert_Colour_Table': begin
		pca_cluster_Invert_Colours, Event
		end

	'Linear_Luminance': begin
		pca_cluster_Linear_Luminance, Event
		end

	else:
  endcase
	widget_control, hourglass=0
end

;-----------------------------------------------------------------------------------

pro pca_cluster, GROUP_LEADER=wGroup, TLB=pca_cluster_TLB, xoffset=xoffset, $
		yoffset=yoffset, pimages=pimages, qregion=qregion, path=path

COMPILE_OPT STRICTARR
common c_errors_1, catch_errors_on

if n_elements(wGroup) lt 1 then wGroup=0L
if n_elements(path) lt 1 then path=''

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
		warning,'pca_cluster',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

  	pca_cluster_routines					    ; Load support routines
  	pca_cluster_eventcb						; Load event callback routines
  	register_notify

  case !version.os_family of
	'MacOS': begin
		symbol = 'SYMBOL*12'
		large_font = 'Arial*12'
		def_font = 'Geneva*10'
		draw_trim = 15
		scr_trim = 21
		help1_xsize = 52
		mode_xsize = 68
		slide_xsize = 96
		button_height = 20
		xsize_element = 95
		smooth_slide_xsize = 72
		bot_slide_xsize = 95
		select_xsize = 45
		end
	'unix': begin
		symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
		large_font = '10x20'
		def_font = '6x13'
		draw_trim = 0
		scr_trim = 15
		help1_xsize = 52
		mode_xsize = 70
		slide_xsize = 98
		button_height = 29
		xsize_element = 95
		smooth_slide_xsize = 52
		bot_slide_xsize = 97
		select_xsize = 48
		end
	else: begin
		symbol = 'SYMBOL*BOLD*14'
		large_font = 'COURIER*BOLD*10'
		def_font = 'Arial*14'
		draw_trim = 0
		scr_trim = 15
		help1_xsize = 52					; 45
		mode_xsize = 50
		slide_xsize = 94					; 140
		button_height = 21
		xsize_element = 72					; 62
		smooth_slide_xsize = 68				; 70
		bot_slide_xsize = 95				; 73
		select_xsize = 45
 		end
  endcase
;@2  widget_control, default_font=def_font        ; set font for all windows

pixx = 280
pixy = 280
margin = {low:70, high:20, bottom:35, top:20}

w = 0
h = 0
xoff = 0
yoff = 0
if widget_info( wGroup, /valid) then begin
	geom = widget_info( wGroup, /geometry)
	w = geom.scr_xsize
	h = geom.scr_ysize
	xoff = geom.xoffset
	yoff = geom.yoffset
endif
screen = get_screen_size()
if n_elements(xoffset) lt 1 then begin
	screen = get_screen_size()
	xoffset = ((xoff + w) < (screen[0]-34 - 300)) > 0
endif
if n_elements(yoffset) lt 1 then begin
	screen = get_screen_size()
	yoffset = ((yoff) < (screen[1]-28 - 400)) > 0
	yoffset2 = yoffset + 412
endif else yoffset2=yoffset

use_gif = 0
if extract(!version.release,0,2) eq '5.3' then use_gif=1	; enable GIF output, suppress PNG
															; use PNG only with IDL 5.4 (mirror bug)
  pimages = bad_pars_struct( pimages, make_pars=no_images)
  xanes = 0
  if no_images then begin
  	el = 'PC ' + strtrim(string(indgen(100)),2)
;	warning,'pca_cluster',['There are no images to display.','Load images first.']
;	return
  endif else begin
  	xanes_stack_test, pimages, xanes, n_el, el, el_xanes
  	el = 'PC ' + strtrim(string(indgen(n_el)),2)
  endelse
  qregion = bad_pars_struct( qregion, make_pars=no_q)
  if no_q then qregion = ptr_new()

  pca_cluster_TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='pca_cluster_TLB' , $
  		/TLB_KILL_REQUEST_EVENTS, /base_align_center, /TLB_SIZE_EVENTS ,TITLE='PCA/Clusters', $
  		SPACE=2, XPAD=1, YPAD=1, /COLUMN, MBAR=pca_cluster_TLB_MBAR, xoffset=xoffset, yoffset=yoffset)

  pca_cluster_tool_TLB = Widget_Base( GROUP_LEADER=pca_cluster_TLB, UNAME='pca_cluster_tool_TLB' , $
  		/TLB_KILL_REQUEST_EVENTS, /base_align_center ,TITLE='PCA/Clusters', $
  		SPACE=2, XPAD=1, YPAD=1, /COLUMN, xoffset=xoffset, yoffset=yoffset2)

  if !version.os_family eq 'unix' then begin
	retain=2
  endif else begin
	retain=1
  endelse

  pca_cluster_Draw_Base = Widget_Base(pca_cluster_TLB, UNAME='pca_cluster_Draw_Base' ,SPACE=0  $
      ,XPAD=0 ,YPAD=0 ,/column, /base_align_center)

	w = pixx + margin.low + margin.high
	h = pixy + margin.bottom + margin.top
	print,'pca_cluster: request w,h=',w,h
	
	pca_cluster_Draw = Widget_Draw(pca_cluster_Draw_Base, UNAME='pca_cluster_Draw'  $
;      ,SCR_XSIZE=w+scr_trim ,SCR_YSIZE=h+scr_trim  $
      ,x_scroll_SIZE=w+scr_trim ,y_scroll_SIZE=h+scr_trim  $
      ,NOTIFY_REALIZE='OnRealize_pca_cluster'  $
      ,KILL_NOTIFY='OnDestroy_pca_cluster' ,/SCROLL ,XSIZE=w+draw_trim  $
      ,YSIZE=h+draw_trim ,RETAIN=retain ,/BUTTON_EVENTS,/VIEWPORT_EVENTS)


	pca_cluster_Button_Base = Widget_Base(pca_cluster_tool_TLB, UNAME='pca_cluster_Button_Base'  $
		,SPACE=1 ,XPAD=0 ,YPAD=0 ,column=1, /align_center, /base_align_center)

	pca_cluster_Button_Base1 = Widget_Base(pca_cluster_Button_Base, UNAME='pca_cluster_Button_Base1'  $
		,SPACE=2 ,XPAD=0 ,YPAD=0 ,/row, /base_align_bottom, /align_center)
		
	pca_cluster_Button_Base2 = Widget_Base(pca_cluster_Button_Base, UNAME='pca_cluster_Button_Base2'  $
		,SPACE=2 ,XPAD=0 ,YPAD=0 ,/row, /base_align_top, /align_center)
		
		
	pca_cluster_X_Base = Widget_Base(pca_cluster_Button_Base1, UNAME='pca_cluster_X_Base'  $
		,SPACE=1 ,XPAD=1 ,YPAD=1 ,/column, /base_align_left, /frame)
	  
	label = widget_label( pca_cluster_X_Base, value='X', /align_center)

	pca_cluster_X_Base1 = Widget_Base(pca_cluster_X_Base, SPACE=2 ,XPAD=0 ,YPAD=0 ,/row, /align_center)

	Element_comboboxx = Widget_combobox(pca_cluster_X_Base1,  $
		UNAME='Element_comboboxx' ,NOTIFY_REALIZE='OnRealize_pca_cluster_Elementx',  $
		VALUE=el, tracking_events=1, xsize=xsize_element, $
		uvalue='Droplist to select the PCA component to display on the X axis.')

;	xlog_base = widget_base(pca_cluster_X_Base1, /nonexclusive, xsize=15, /row, /align_top, SPACE=0 ,XPAD=0 ,YPAD=0)
;	xlog_button = widget_button( xlog_base, value='', /tracking, uvalue='Enable/disable log X scale.', $
;		uname='xlog_button', xsize=20, ysize=20, /align_top, $
;		NOTIFY_REALIZE='OnRealize_pca_cluster_Xlog')

  	X_Slider_top = widget_slider( pca_cluster_X_Base, minimum=0, maximum=100, /drag, /tracking, $
		uvalue='Adjust the high end of the X PCA component range to display.', uname='X_high_Slider', $
		xsize=bot_slide_xsize, value=100, NOTIFY_REALIZE='OnRealize_pca_cluster_X_Slider_top', /align_top)

	X_Slider = widget_slider( pca_cluster_X_Base, minimum=0, maximum=100, /drag, /tracking, $
		uvalue='Adjust the low end of the X PCA component range to display.', uname='X_low_Slider', $
		xsize=bot_slide_xsize, value=0, NOTIFY_REALIZE='OnRealize_pca_cluster_X_Slider', /align_top)


	pca_cluster_Z_Base = Widget_Base(pca_cluster_Button_Base1, UNAME='pca_cluster_Z_Base'  $
		,SPACE=1 ,XPAD=1 ,YPAD=1 ,/column, /align_top, /base_align_center, /frame)
	  
	Zaxis_comboboxx = Widget_combobox(pca_cluster_Z_Base, UNAME='Zaxis_combobox' ,   $
		VALUE=[ 'Linear Z', 'Sqrt Z', 'Log Z' ], tracking_events=1, xsize=xsize_element+20, $
		uvalue='Droplist to select the Z axis scale mode for the 2D histgram display.', $
		NOTIFY_REALIZE='OnRealize_pca_cluster_Zaxis')

	pca_cluster_Z_Base1 = Widget_Base(pca_cluster_Z_Base, SPACE=2 ,XPAD=0 ,YPAD=0 ,/row, /base_align_bottom)

	Full_Button = Widget_Button(pca_cluster_Z_Base1, UNAME='Full_Button'  $
		,/ALIGN_CENTER ,VALUE='0', tracking_events=1, font=large_font, $
		uvalue='Show original size of Associations 2D histogram in window.',xsize=20,ysize=button_height)

	Zoom_In_Button = Widget_Button(pca_cluster_Z_Base1, UNAME='Zoom_In_Button'  $
		,/ALIGN_CENTER ,VALUE='+', tracking_events=1, font=large_font, $
		uvalue='Magnify Associations 2D histogram x2.',xsize=20,ysize=button_height)

	Zoom_Out_Button = Widget_Button(pca_cluster_Z_Base1, UNAME='Zoom_Out_Button'  $
		,/ALIGN_CENTER ,VALUE='-', tracking_events=1, font=large_font, $
		uvalue='Demagnify Assocations 2D histogram x2.',xsize=20,ysize=button_height)

	High_Slider = widget_slider( pca_cluster_Z_Base, minimum=0, maximum=100, /drag, /tracking, $
		uvalue='Adjust maximum Z scale of 2D histogram to display, as % of max.', uname='High_Slider', $
		xsize=slide_xsize, value=100, NOTIFY_REALIZE='OnRealize_pca_cluster_High_Slider')

	Low_Slider = widget_slider( pca_cluster_Z_Base, minimum=0, maximum=100, /drag, /tracking, $
		uvalue='Adjust minimum Z scale of 2D histogram to display, as % of max.', uname='Low_Slider', $
		xsize=slide_xsize, value=0, NOTIFY_REALIZE='OnRealize_pca_cluster_Low_Slider')


	pca_cluster_Y_Base = Widget_Base(pca_cluster_Button_Base1, UNAME='pca_cluster_Y_Base'  $
		,SPACE=1 ,XPAD=1 ,YPAD=1 ,/column, /base_align_left, /frame)
	  
	label = widget_label( pca_cluster_Y_Base, value='Y', /align_center)

	pca_cluster_Y_Base1 = Widget_Base(pca_cluster_Y_Base, SPACE=2 ,XPAD=0 ,YPAD=0 ,/row, /align_center)

	Element_comboboxy = Widget_combobox(pca_cluster_Y_Base1,  $
		UNAME='Element_comboboxy' ,NOTIFY_REALIZE='OnRealize_pca_cluster_Elementy',  $
		VALUE=el, tracking_events=1, xsize=xsize_element, $
		uvalue='Droplist to select the PCA component to display on the Y axis.')

;	ylog_base = widget_base(pca_cluster_Y_Base1, /nonexclusive, xsize=15, /row, /align_top, SPACE=0 ,XPAD=0 ,YPAD=0)
;	ylog_button = widget_button( ylog_base, value='', /tracking, uvalue='Enable/disable log Y scale.', $
;		uname='ylog_button', xsize=20, ysize=20, /align_top, $
;		NOTIFY_REALIZE='OnRealize_pca_cluster_Ylog')

	Y_Slider_top = widget_slider( pca_cluster_Y_Base, minimum=0, maximum=100, /drag, /tracking, $
		uvalue='Adjust the high end of the Y PCA component range to display.', uname='Y_high_Slider', $
		xsize=bot_slide_xsize, value=100, NOTIFY_REALIZE='OnRealize_pca_cluster_Y_Slider_top')

	Y_Slider = widget_slider( pca_cluster_Y_Base, minimum=0, maximum=100, /drag, /tracking, $
		uvalue='Adjust the low end of the Y PCA component range to display.', uname='Y_low_Slider', $
		xsize=bot_slide_xsize, value=0, NOTIFY_REALIZE='OnRealize_pca_cluster_Y_Slider')


	pca_cluster_pca_Base = Widget_Base(pca_cluster_Button_Base2, UNAME='pca_cluster_pca_Base'  $
		,SPACE=1 ,XPAD=1 ,YPAD=1 ,/column, /align_top, /base_align_center, /frame)
	  
	label = widget_label( pca_cluster_pca_Base, value='PCA  Analysis', /align_center)

	pca_cluster_pca_Base0 = Widget_Base(pca_cluster_pca_Base, SPACE=1 ,XPAD=0 ,YPAD=0 ,/column, /base_align_right, /align_center)

	pca_cluster_pca_Base1 = Widget_Base(pca_cluster_pca_Base0, SPACE=5 ,XPAD=0 ,YPAD=0 ,/row, /base_align_bottom)

	label = widget_label( pca_cluster_pca_Base1, value='Smooth images:')

	smooth_Slider = Widget_combobox( pca_cluster_pca_Base1,  $
		UNAME='Smooth_Slider' ,NOTIFY_REALIZE='OnRealize_pca_cluster_smooth_Slider',  $
		VALUE=strtrim(string(indgen(20)),2), tracking_events=1, xsize=select_xsize, $
		uvalue='Apply boxcar smooth of this value to all images prior to PCA analysis.')

	pca_cluster_pca_Base1a = Widget_Base(pca_cluster_pca_Base0, SPACE=5 ,XPAD=0 ,YPAD=0 ,/row, /base_align_bottom)

	label = widget_label( pca_cluster_pca_Base1a, value='Ignore borders:')

	border_Slider = Widget_combobox( pca_cluster_pca_Base1a,  $
		UNAME='Border_Slider' ,NOTIFY_REALIZE='OnRealize_pca_cluster_Border_Slider',  $
		VALUE=strtrim(string(indgen(20)),2), tracking_events=1, xsize=select_xsize, $
		uvalue='Ignore border pixel margin to this width (pixels on all sides) during PCA analysis.')

	pca_cluster_pca_Base2 = Widget_Base(pca_cluster_pca_Base, UNAME='pca_cluster_pca_Base2'  $
		,SPACE=1 ,XPAD=4 ,YPAD=2 ,/column, /align_center, /base_align_right, /frame)
	  
	pca_cluster_pca_Base3 = Widget_Base(pca_cluster_pca_Base2, SPACE=5 ,XPAD=0 ,YPAD=0 ,/row, /base_align_center, /align_center)

	label = widget_label( pca_cluster_pca_Base3, value=(xanes ? 'Normalize spectra ': 'Normalize elements '))

	normalize_base = widget_base(pca_cluster_pca_Base3, /nonexclusive, xsize=15, /row, /align_top, SPACE=0 ,XPAD=0 ,YPAD=0)
	normalize_button = widget_button( normalize_base, value='', /tracking, uvalue='Enable/disable normalization of spectra prior to PCA.', $
		uname='Normalize_Button', xsize=20, ysize=20, /align_top, NOTIFY_REALIZE='OnRealize_pca_cluster_normalize')

	pca_cluster_pca_Base4 = Widget_Base(pca_cluster_pca_Base2, SPACE=5 ,XPAD=0 ,YPAD=0 ,/row, /base_align_bottom)

	label = widget_label( pca_cluster_pca_Base4, value='Average High %:')

	average_high_Slider = Widget_combobox(pca_cluster_pca_Base4, sensitive=xanes,  $
		UNAME='Average_High_Slider' ,NOTIFY_REALIZE='OnRealize_pca_cluster_average_high_Slider',  $
		VALUE=strtrim(string(indgen(20)),2), tracking_events=1, xsize=select_xsize, $
		uvalue='Average this % of spectrum channels at high energy end for pixel spectrum scale normalization.')

	pca_cluster_pca_Base5 = Widget_Base(pca_cluster_pca_Base2, SPACE=5 ,XPAD=0 ,YPAD=0 ,/row, /base_align_bottom)

	label = widget_label( pca_cluster_pca_Base5, value='Average Low %:')

	average_low_Slider = Widget_combobox(pca_cluster_pca_Base5, sensitive=xanes,  $
		UNAME='Average_Low_Slider' ,NOTIFY_REALIZE='OnRealize_pca_cluster_average_low_Slider',  $
		VALUE=strtrim(string(indgen(20)),2), tracking_events=1, xsize=select_xsize, $
		uvalue='Average this % of spectrum channels at low energy end for pixel spectrum background subtraction.')

	pca_cluster_pca_Base6 = Widget_Base(pca_cluster_pca_Base2, SPACE=5 ,XPAD=0 ,YPAD=0 ,/row, /base_align_bottom)

	label = widget_label( pca_cluster_pca_Base6, value='Threshold (%):')

	threshold_Slider = Widget_combobox(pca_cluster_pca_Base6, sensitive=xanes,  $
		UNAME='Threshold_Slider' ,NOTIFY_REALIZE='OnRealize_pca_cluster_threshold_Slider',  $
		VALUE=strtrim(string(indgen(100)),2), tracking_events=1, xsize=select_xsize, $
		uvalue='Adjust the threshold for significant pixels, as % of maximum.')

	pca_cluster_pca_Base7 = Widget_Base(pca_cluster_pca_Base, SPACE=2 ,XPAD=0 ,YPAD=0 ,/row, /base_align_bottom)

	PCA_Button = Widget_Button(pca_cluster_pca_Base7, UNAME='PCA_Button'  $
		,/ALIGN_CENTER ,VALUE='Generate PCA', tracking_events=1, $
		uvalue='Analyze pixel spectra or element maps using PCA and display selected (X,Y) 2-component projections.')

	select_PCA_Button = Widget_Button(pca_cluster_pca_Base7, UNAME='Plot_PCA_Eigen'  $
		,/ALIGN_CENTER ,VALUE='Plot Eigen', tracking_events=1, $
		uvalue='Plot the PCA returned Eigen values and the vectors for the PC 0-3 projection axes.')

	pca_cluster_show_Base = Widget_Base(pca_cluster_Button_Base2, UNAME='pca_cluster_show_Base'  $
		,SPACE=1 ,XPAD=1 ,YPAD=1 ,/column, /align_top, /base_align_center)
	  
	label = widget_label( pca_cluster_show_Base, value='Highlight Points', /align_center)

	pca_cluster_show_Base1 = Widget_Base(pca_cluster_show_Base, SPACE=5 ,XPAD=0 ,YPAD=0 ,/row, /base_align_bottom)

	show_options = cw_bgroup2( pca_cluster_show_Base1, ['Spline','Class'], /row, set_value=[0,1], $
				/return_index, uname='Show_options',/ nonexclusive, /tracking, $
				uvalue=['Enable highlighting of spline selected points from previous spline (pink) on PCA plots.', $
						'Enable highlighting of cluster class selected points (green) on PCA plots.'], xpad=0, ypad=0, space=1)

	pca_cluster_cluster_Base = Widget_Base(pca_cluster_show_Base, UNAME='pca_cluster_cluster_Base'  $
		,SPACE=1 ,XPAD=1 ,YPAD=1 ,/column, /align_top, /base_align_right, /frame)
	  
	label = widget_label( pca_cluster_cluster_Base, value='Cluster  Analysis', /align_center)

	pca_cluster_cluster_Base1 = Widget_Base(pca_cluster_cluster_Base, SPACE=5 ,XPAD=0 ,YPAD=0 ,/row, /base_align_bottom)

	label = widget_label( pca_cluster_cluster_Base1, value='Number of clusters:')

	class_max_Slider = Widget_combobox( pca_cluster_cluster_Base1,  $
		UNAME='Class_Max_Slider' ,NOTIFY_REALIZE='OnRealize_pca_cluster_class_max_Slider',  $
		VALUE=strtrim(string(indgen(120)),2), tracking_events=1, xsize=select_xsize, $
		uvalue='Maximum number of clusters to search for using K-means clustering.')

	cluster_source_ID = Widget_combobox( pca_cluster_cluster_Base,  $
		UNAME='Cluster_Source' ,NOTIFY_REALIZE='OnRealize_pca_cluster_source', /align_center, $
		VALUE=['from pixel spectra','from PCA results'], tracking_events=1, $		;, xsize=select_xsize, $
		uvalue='Select the source for clustering from (i) the raw pixel spectra data (smoothed in PCA routine), (ii) the results of the PCA analysis.')

	pca_cluster_cluster_Base2 = Widget_Base(pca_cluster_cluster_Base, SPACE=5 ,XPAD=0 ,YPAD=0 ,/row, /base_align_bottom)

	label = widget_label( pca_cluster_cluster_Base2, value='PCA components:')

	components_Slider = Widget_combobox( pca_cluster_cluster_Base2,  $
		UNAME='Components_Slider' ,NOTIFY_REALIZE='OnRealize_pca_cluster_components_Slider',  $
		VALUE=strtrim(string(indgen(100)),2), tracking_events=1, xsize=select_xsize, $
		uvalue='Number of PCA component perspectives to use for cluster analysis.')

	cluster_Button = Widget_Button(pca_cluster_cluster_Base, UNAME='Cluster_Button'  $
		,/ALIGN_CENTER ,VALUE='K-means Cluster Analysis', tracking_events=1, $
		uvalue='Analyze (i) the image pixel spectra or (ii) selected components of the PCA results (according to the "from ..." droplist) to ' + $
				'search for up to the selected number of clusters. For both, assumes that "Generate PCA" has been completed first.')

	pca_cluster_cluster_Base3 = Widget_Base(pca_cluster_cluster_Base, SPACE=5 ,XPAD=0 ,YPAD=0 ,/row, /base_align_bottom)

	label = widget_label( pca_cluster_cluster_Base3, value='Class:')

	class_Slider = Widget_combobox( pca_cluster_cluster_Base3,  $
		UNAME='Class_Slider' ,NOTIFY_REALIZE='OnRealize_pca_cluster_class_Slider',  $
		VALUE=strtrim(string(indgen(100)),2), tracking_events=1, xsize=select_xsize, $
		uvalue='Select the class index to select pixels on the images, using the "Select" button.')

	cluster_show_Button = Widget_Button(pca_cluster_cluster_Base3, UNAME='Cluster_Show_Button'  $
		,/ALIGN_CENTER ,VALUE='Show', tracking_events=1, $
		uvalue='Show the data points on the PCA XY plot corresponding to the class index indicated to the left.')

	cluster_select_Button = Widget_Button(pca_cluster_cluster_Base, UNAME='Cluster_Select_Button'  $
		,/ALIGN_CENTER ,VALUE='Highlight Class on Images', tracking_events=1, $
		uvalue='Highlight the pixels on the images in green corresponding to the class index indicated above.')


  PostCreate_pca_cluster_Base, pca_cluster_Draw_Base, pca_cluster_tool_TLB, $
  			path=path, pimages=pimages, qregion=qregion, parent=wGroup, clone=clone, $
  			width=pixx, height=pixy, margin=margin, _EXTRA=_VWBExtra_

  PostCreate_pca_cluster, pca_cluster_Draw, _EXTRA=_VWBExtra_


  Help_Text1 = Widget_Text(pca_cluster_tool_TLB, UNAME='Help_Text1', /wrap $
      ,NOTIFY_REALIZE='OnRealize_pca_cluster_Help1',XSIZE=help1_xsize, frame=0 ,YSIZE=4, $
      tracking_events=1, uvalue='Help window to show help prompts for widgets.')


 ; File menus

  W_MENU_0 = Widget_Button(pca_cluster_TLB_MBAR, UNAME='W_MENU_0' ,/MENU  $
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

  W_MENU_20 = Widget_Button(pca_cluster_TLB_MBAR, UNAME='W_MENU_20'  $
      ,/MENU ,VALUE='Display')


;  W_MENU_21 = Widget_Button(W_MENU_20, UNAME='Clone_Menu', VALUE='Clone', /menu)
;
;  W_MENU_211 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_1', VALUE='1 Clone')
;
;  W_MENU_212 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_3', VALUE='3 Clone')
;
;  W_MENU_213 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_7', VALUE='7 Clone')
;
;  W_MENU_214 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_11', VALUE='11 Clone')


  W_MENU_23 = Widget_Button(W_MENU_20, UNAME='Colours_Menu', VALUE='Colours',/SEPARATOR, /menu )

  W_MENU_2310 = Widget_Button(W_MENU_23, UNAME='Colour_Table', VALUE='Colour Table' )

  W_MENU_231 = Widget_Button(W_MENU_23, UNAME='Default_Colour_Table', VALUE='Default Colours' )

  W_MENU_232 = Widget_Button(W_MENU_23, UNAME='Grey_Scale', VALUE='Grey Scale')

  W_MENU_233 = Widget_Button(W_MENU_23, UNAME='Invert_Colour_Table', VALUE='Invert Colour Table',/SEPARATOR  )

  W_MENU_234 = Widget_Button(W_MENU_23, UNAME='Linear_Luminance', VALUE='Linear Luminance' )


  W_MENU_22 = Widget_Button(W_MENU_20, UNAME='Clear_Spline_Menu', VALUE='Clear Spline', /separator)


; Analyze menus

  W_MENU_40 = Widget_Button(pca_cluster_TLB_MBAR, UNAME='W_MENU_40'  $
      ,/MENU ,VALUE='Analyze')


  W_MENU_401 = Widget_Button(W_MENU_40, UNAME='Analyze_Spline_Menu', VALUE='Within Spline' )


  Widget_Control, /REALIZE, pca_cluster_TLB
  Widget_Control, /REALIZE, pca_cluster_tool_TLB

  set_pca_cluster_map_help, pca_cluster_tool_TLB

  if wGroup ne 0 then begin
  	register_notify, pca_cluster_TLB, $
  				['images-changed', $				; new Images loaded
				'image-display', $					; images display/processing changes
				'images', $							; new images
				'path', $							; new path
				'image-region-select', $			; region selected
				'image-analyze-q', $				; new q array
				'image-analyze-all-clear', $		; clear q array
				'corr-analyze-clear', $				; pass on notify of qc destined for image
				'corr-analyze-spline', $			; pass on notify of qc cleared destined for image
  				'corr-display' $					; pca_cluster display needs updating (e.g. smooth...)
		  		], from=wGroup
  endif

  XManager, 'pca_cluster', pca_cluster_TLB, /no_block
  XManager, 'pca_cluster', pca_cluster_tool_TLB, event='pca_cluster_event', /no_block

end
;

