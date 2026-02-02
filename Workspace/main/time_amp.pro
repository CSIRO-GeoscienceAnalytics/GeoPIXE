pro time_amp_event, Event

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
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
		warning,'time_amp_Event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	wWidget =  Event.top
	widget_control, hourglass=0
;	help, event,/structure
	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	case tag_names( event,/structure) of
		'NOTIFY': begin
			OnNotify_time_amp, event
			return
			end
		'WIDGET_TRACKING': begin
			OnTracking_time_amp, Event
			return
			end
		else:
	endcase

  uname = widget_info( event.id, /uname)

  case uname of

	'time_amp_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_KILL_REQUEST': begin
				time_amp_exit, Event
				end
			'WIDGET_BASE': begin
				print,'time-amp: resize disabled; skip_resize=',(*pstate).skip_resize
;				OnSize_time_amp, Event
				end
			else:
		endcase
		end

	'time_amp_Draw': begin
		if( Event.type eq 3 )then begin
			OnViewport_time_amp, Event
		endif else begin
			OnButton_time_amp, Event
		endelse
		end

;	'Detector_combobox': begin
;		OnSelect_time_amp_detector, Event
;		end

	'station-slider': begin
		time_amp_detector_slider, Event
		end

	'station-text': begin
		time_amp_detector_text, Event
		end

	'station-up-one': begin
		time_amp_detector_up, event
		end

	'events': begin
		OnSelect_time_amp_events, Event
		end

	'Low_X_Slider': begin
		OnMove_time_amp_Low_X, Event
		end

	'High_X_Slider': begin
		OnMove_time_amp_High_X, Event
		end

	'Low_Y_Slider': begin
		OnMove_time_amp_Low_Y, Event
		end

	'High_Y_Slider': begin
		OnMove_time_amp_High_Y, Event
		end

	'xlog_button': begin
		OnSelect_time_amp_Xlog, Event
		end

	'ylog_button': begin
		OnSelect_time_amp_Ylog, Event
		end

	'hymod-load-gaintrim-file': begin
		time_amp_get_gaintrim, Event
		end

	'hymod-gaintrim-T-off-up-fast': begin
		if event.select then begin
			if (*pstate).detector lt 0 then goto, finish
			(*(*pstate).ptrim).T[ (*pstate).detector].b += 25
			print, (*(*pstate).ptrim).T[ (*pstate).detector].b, (*(*pstate).ptrim).T[ (*pstate).detector].a
			draw_time_amps, pstate
		endif
		end
		
	'hymod-gaintrim-T-off-up': begin
		if event.select then begin
			if (*pstate).detector lt 0 then goto, finish
			(*(*pstate).ptrim).T[ (*pstate).detector].b += 5
			print, (*(*pstate).ptrim).T[ (*pstate).detector].b, (*(*pstate).ptrim).T[ (*pstate).detector].a
			draw_time_amps, pstate
		endif
		end
		
	'hymod-gaintrim-T-off-down': begin
		if event.select then begin
			if (*pstate).detector lt 0 then goto, finish
			(*(*pstate).ptrim).T[ (*pstate).detector].b -= 5
			print, (*(*pstate).ptrim).T[ (*pstate).detector].b, (*(*pstate).ptrim).T[ (*pstate).detector].a
			draw_time_amps, pstate
		endif
		end
		
	'hymod-gaintrim-T-off-down-fast': begin
		if event.select then begin
			if (*pstate).detector lt 0 then goto, finish
			(*(*pstate).ptrim).T[ (*pstate).detector].b -= 25
			print, (*(*pstate).ptrim).T[ (*pstate).detector].b, (*(*pstate).ptrim).T[ (*pstate).detector].a
			draw_time_amps, pstate
		endif
		end
		
	'hymod-gaintrim-T-gain-up-fast': begin
		if event.select then begin
			if (*pstate).detector lt 0 then goto, finish
			(*(*pstate).ptrim).T[ (*pstate).detector].a *= 1.05
			print, (*(*pstate).ptrim).T[ (*pstate).detector].b, (*(*pstate).ptrim).T[ (*pstate).detector].a
			draw_time_amps, pstate
		endif
		end
		
	'hymod-gaintrim-T-gain-up': begin
		if event.select then begin
			if (*pstate).detector lt 0 then goto, finish
			(*(*pstate).ptrim).T[ (*pstate).detector].a *= 1.01
			print, (*(*pstate).ptrim).T[ (*pstate).detector].b, (*(*pstate).ptrim).T[ (*pstate).detector].a
			draw_time_amps, pstate
		endif
		end
		
	'hymod-gaintrim-T-gain-down': begin
		if event.select then begin
			if (*pstate).detector lt 0 then goto, finish
			(*(*pstate).ptrim).T[ (*pstate).detector].a *= 0.99
			print, (*(*pstate).ptrim).T[ (*pstate).detector].b, (*(*pstate).ptrim).T[ (*pstate).detector].a
			draw_time_amps, pstate
		endif
		end
		
	'hymod-gaintrim-T-gain-down-fast': begin
		if event.select then begin
			if (*pstate).detector lt 0 then goto, finish
			(*(*pstate).ptrim).T[ (*pstate).detector].a *= 0.95
			print, (*(*pstate).ptrim).T[ (*pstate).detector].b, (*(*pstate).ptrim).T[ (*pstate).detector].a
			draw_time_amps, pstate
		endif
		end
		
	'save-gaintrim-table': begin
		time_amp_save_gaintrim, Event
		end

	'extract-time-amp': begin
		time_amp_extract, Event, device='MAIA_DEVICE'
		end

	'extract-time-amp-daq': begin
		time_amp_extract, Event, device='DAQ_DEVICE' 
		end

	'save-pileup-table': begin
		time_amp_save_pileup, Event
		end

	'Save_GIF_Menu': begin
		time_amp_Save_GIF, Event
		end

	'Save_PNG_Menu': begin
		time_amp_Save_GIF, Event, /PNG
		end

	'Export_Menu': begin
;		time_amp_export, Event
		end

	'Clear_Spline_Menu': begin
		time_amp_clear_spline, Event
		end

	'Overlay_Pileup_Menu': begin
		time_amp_overlay_pileup, Event
		end
		
	'Analyze_Spline_Menu': begin
		time_amp_analyze_spline, Event
		end

	'Exit_Menu': begin
		time_amp_Exit, Event
		end

	'Clone_Menu': begin
		time_amp_Clone, Event
		end

	'Clone_Menu_1': begin
		time_amp_Clone, Event, 1
		end

	'Clone_Menu_3': begin
		time_amp_Clone, Event, 3
		end

	'Clone_Menu_7': begin
		time_amp_Clone, Event, 7
		end

	'Clone_Menu_11': begin
		time_amp_Clone, Event, 11
		end

	'Colour_Table': begin
		time_amp_Colours, Event
		end

	'Default_Colour_Table': begin
		time_amp_Colours, Event, 5
		end

	'Grey_Scale': begin
		time_amp_Colours, Event, 0
		end

	'Invert_Colour_Table': begin
		time_amp_Invert_Colours, Event
		end

	'Linear_Luminance': begin
		time_amp_Linear_Luminance, Event
		end

	else:
  endcase
finish:
	widget_control, hourglass=0
end

;-----------------------------------------------------------------------------------

pro time_amp, GROUP_LEADER=wGroup, TLB=time_amp_TLB, qregion=qregion, $
						path=path, _EXTRA=_VWBExtra_, pileup=pileup, device=obj

; Plot E-T plot for Maia data. Maia device is hardwired in this routine.
; For more general use, will need to passs device and get notified of 
; (i) change of device, (ii) change of spectra/image and hence device.

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_errors_1, catch_errors_on
common c_geopixe_adcs, geopixe_max_adcs
if n_elements(geopixe_max_adcs) lt 1 then geopixe_max_adcs=384

if n_elements(wGroup) lt 1 then wGroup=0L
if n_elements(path) lt 1 then path=''
if ptr_valid(path) then path=*path
if n_elements(obj) lt 1 then obj=obj_new('MAIA_DEVICE')

ErrorNo = 0
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'time_amp',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

  	time_amp_routines					    ; Load support routines
  	time_amp_eventcb						; Load event callback routines
	define_devices

  case !version.os_family of
	'MacOS': begin
		symbol = 'SYMBOL*12'
		large_font = 'Arial*12'
;@2		widget_control, default_font='Geneva*10'		; set font for all windows
		draw_trim = 15
		scr_trim = 21
		help1_xsize = 94
		slide_xsize = 185
		button_height = 20
		xsize_detector = 44
		end
	'unix': begin
		symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
		large_font = '10x20'
;@2		widget_control, default_font='6x13'				; set font for all windows
		draw_trim = 0
		scr_trim = 15
		help1_xsize = 95
		slide_xsize = 170
		button_height = 29
		xsize_detector = 81
		end
	else: begin
		symbol = 'SYMBOL*BOLD*14'
		large_font = 'COURIER*BOLD*10'
;		widget_control, default_font='Arial*14'				; set font for all windows
		draw_trim = 0
		scr_trim = 15
		help1_xsize = 100
		slide_xsize = 161
		button_height = 20
		xsize_detector = 44
 		end
  endcase

use_gif = 0
if extract(!version.release,0,2) eq '5.3' then use_gif=1	; enable GIF output, suppress PNG
															; use PNG only with IDL 5.4 (mirror bug)
  qregion = bad_pars_struct( qregion, make_pars=no_q)
  if no_q then qregion = ptr_new()

  time_amp_TLB = Widget_Base( GROUP_LEADER=wGroup,  $
      UNAME='time_amp_TLB' ,/TLB_KILL_REQUEST_EVENTS, /base_align_center  $
      ,/TLB_SIZE_EVENTS ,TITLE='Time Amplitude' ,SPACE=2 ,XPAD=1 ,YPAD=1  $
      ,COLUMN=1 ,MBAR=time_amp_TLB_MBAR, _EXTRA=_VWBExtra_)

  if !version.os_family eq 'unix' then begin
	retain = 2
  endif else begin
	retain = 1
  endelse

  time_amp_Draw_Base = Widget_Base(time_amp_TLB, UNAME='time_amp_Draw_Base' ,SPACE=0  $
      ,XPAD=0 ,YPAD=0 ,ROW=1)

  time_amp_Draw = Widget_Draw(time_amp_Draw_Base, UNAME='time_amp_Draw'  $
      ,NOTIFY_REALIZE='OnRealize_time_amp'  $
      ,KILL_NOTIFY='OnDestroy_time_amp'  ,XSIZE=600+draw_trim  $
      ,YSIZE=500+draw_trim ,RETAIN=retain ,/BUTTON_EVENTS)
      ;,/VIEWPORT_EVENTS, /SCROLL)


  time_amp_Col_Base = Widget_Base(time_amp_TLB, UNAME='time_amp_Col_Base'  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,column=1, /align_center, /base_align_center)


  time_amp_Row_Base1 = Widget_Base(time_amp_Col_Base, UNAME='time_amp_Row_Base1'  $
      ,SPACE=20 ,XPAD=0 ,YPAD=0 ,/row, /align_center)


  time_amp_Left_Base2 = Widget_Base(time_amp_Row_Base1, UNAME='time_amp_Left_Base2'  $
      ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/column, /align_center, /base_align_center)


  time_amp_Top_Base = Widget_Base(time_amp_Left_Base2, UNAME='time_amp_Top_Base'  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_center, /base_align_center)


  time_amp_Mid_Base = Widget_Base(time_amp_Left_Base2, UNAME='time_amp_Bot_Base'  $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_right, /base_align_bottom)


  time_amp_Bot_Base = Widget_Base(time_amp_Left_Base2, UNAME='time_amp_Bot_Base'  $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_right, /base_align_bottom)


  time_amp_Help1_Base = Widget_Base(time_amp_Col_Base, UNAME='time_amp_Help1_Base', map=1  $
      ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/row, /align_center)


;  time_amp_Help2_Base = Widget_Base(time_amp_Row_Base1, UNAME='time_amp_Help2_Base', map=0  $
;      ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/row, /align_center)
  time_amp_Help2_Base = 0L


  time_amp_Right_Base2 = Widget_Base(time_amp_Row_Base1, UNAME='time_amp_Right_Base2'  $
      ,SPACE=10 ,XPAD=0 ,YPAD=0 ,/column, /align_center, /base_align_right)


  time_amp_Top_Base2 = Widget_Base(time_amp_Right_Base2, UNAME='time_amp_Top_Base2'  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_center, /base_align_center)


  time_amp_Mid_Base2 = Widget_Base(time_amp_Right_Base2, UNAME='time_amp_Bot_Base2'  $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_right, /base_align_bottom)


  time_amp_Bot_Base2 = Widget_Base(time_amp_Right_Base2, UNAME='time_amp_Bot_Base2'  $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_right, /base_align_bottom)



  PostCreate_time_amp_Base, time_amp_Draw_Base, time_amp_Help1_Base, time_amp_Help2_Base, path=path, $
 			qregion=qregion, parent=wGroup, clone=clone, device=obj, _EXTRA=_VWBExtra_

  PostCreate_time_amp, time_amp_Draw, _EXTRA=_VWBExtra_


  label = widget_label( time_amp_Top_Base, value='Detector:')


;  Detector_combobox = Widget_combobox(time_amp_Top_Base,  $
;      UNAME='Detector_combobox' ,  $
;      VALUE=['all',' '+str_tidy(indgen(384))+' '], tracking_events=1, /align_bottom, $
;      NOTIFY_REALIZE='OnRealize_time_amp_detector', $
;     uvalue='Select the detector to display time-amplitude data for, or combine "all" detector data in one display.')


  station_text = widget_text( time_amp_Top_Base, uname='station-text', /editable, /tracking, value='all', $
				      NOTIFY_REALIZE='OnRealize_time_amp_detector_text', $
					scr_xsize=75, uvalue='Select ADC channel to display Time-amplitude data for.' + $
					'Select "All" to display data for all detector channels.')

  station_slider = widget_slider( time_amp_Top_Base, /suppress_value, minimum=0, maximum=geopixe_max_adcs, value=0, /drag, scr_xsize=75, $
				      NOTIFY_REALIZE='OnRealize_time_amp_detector_slider', $
					uname='station-slider', /tracking, uvalue='Select ADC channel to display Time-amplitude data for.' + $
					'Select "All" to display data for all detector channels.')

  if !version.os_family eq 'unix' then begin
  		arrow = picture_button( time_amp_Top_Base, geopixe_root + 'images/right-16x14.jpeg', uname='station-up-one', $
					/tracking, uvalue='Increment detector channel by +1.', /pushbutton_events)
  endif

  space = widget_base(time_amp_Top_Base, xsize=10)

  label = widget_label( time_amp_Top_Base, value='Events:')

  s = '100000'
  events = widget_text( time_amp_Top_Base, value=s, uname='events', /tracking, /editable, $
					notify_realize='onrealize_time_amp_events', $
					uvalue='Select the number of events to extract. Start small (100,000) as plotting "All" is slow. Then select a single detector channel ' + $
					'and increase to a larger count (e.g. 1,000,000) and Extract blog data again.')

  label = widget_label( time_amp_Mid_Base, value='Amp (X):')

  Low_X_Slider = widget_slider( time_amp_Mid_Base, minimum=0, maximum=100, /drag, /tracking, $
	uvalue='Adjust minimum X scale to display, as % of max.', uname='Low_X_Slider', $
	NOTIFY_REALIZE='OnRealize_time_amp_Low_X_Slider', $
	xsize=slide_xsize, value=0, /align_bottom)

  High_X_Slider = widget_slider( time_amp_Mid_Base, minimum=0, maximum=100, /drag, /tracking, $
	uvalue='Adjust maximum X scale to display, as % of max.', uname='High_X_Slider', $
	NOTIFY_REALIZE='OnRealize_time_amp_High_X_Slider', $
	xsize=slide_xsize, value=100, /align_bottom)

  xlog_base = widget_base(time_amp_Mid_Base, /nonexclusive, xsize=15, /row, /align_center, SPACE=0 ,XPAD=0 ,YPAD=0)
  xlog_button = widget_button( xlog_Base, value='', /tracking, uvalue='Enable/disable log X scale.', $
  				NOTIFY_REALIZE='OnRealize_time_amp_Xlog', $
  				uname='xlog_button', xsize=20, ysize=20, /align_top)

  label = widget_label( time_amp_Bot_Base, value='Time (Y):')

  Low_Y_Slider = widget_slider( time_amp_Bot_Base, minimum=0, maximum=100, /drag, /tracking, $
	uvalue='Adjust minimum X scale to display, as % of max.', uname='Low_Y_Slider', $
	NOTIFY_REALIZE='OnRealize_time_amp_Low_Y_Slider', $
	xsize=slide_xsize, value=0, /align_bottom)

  High_Y_Slider = widget_slider( time_amp_Bot_Base, minimum=0, maximum=100, /drag, /tracking, $
	uvalue='Adjust maximum X scale to display, as % of max.', uname='High_Y_Slider', $
	NOTIFY_REALIZE='OnRealize_time_amp_High_Y_Slider', $
	xsize=slide_xsize, value=100, /align_bottom)

  ylog_base = widget_base(time_amp_Bot_Base, /nonexclusive, xsize=15, /row, /align_center, SPACE=0 ,XPAD=0 ,YPAD=0)
  ylog_button = widget_button( ylog_Base, value='', /tracking, uvalue='Enable/disable log Y scale.', $
 				NOTIFY_REALIZE='OnRealize_time_amp_Ylog', $
 				uname='ylog_button', xsize=20, ysize=20, /align_top)


  Help_Text1 = Widget_Text(time_amp_Help1_Base, UNAME='Help_Text1', /wrap $
      ,XSIZE=help1_xsize, frame=0 ,YSIZE=3, $
	  NOTIFY_REALIZE='OnRealize_time_amp_Help1', $
      tracking_events=1, uvalue='Help window to show help prompts for widgets.')

;  Help_Text2 = Widget_Text(time_amp_Help2_Base, UNAME='Help_Text2', /wrap $
;      ,scr_XSIZE=1, frame=0 ,scr_YSIZE=102, ysize=5, $
;      NOTIFY_REALIZE='OnRealize_time_amp_Help2', $
;      tracking_events=1, uvalue='Help window to show help prompts for widgets.')

  button = widget_button( time_amp_Top_Base2, value='Load T GainTrim', uname='hymod-load-gaintrim-file', /tracking, uvalue='Load T gain-trim from a file. Select the same file used in Hymod to generate ' + $
  			'this blog data. You will be able to adjust these values using the arrow buttons below. Remember to save a new T gaintrim file using the File menu.')

  label = widget_label( time_amp_Mid_Base2, value='Adj T Offset:')
  arrow = picture_button( time_amp_Mid_Base2, geopixe_root + 'images/fast-left-24x14.jpeg', uname='hymod-gaintrim-T-off-down-fast', $
				/tracking, uvalue='Adjust selected detector T Gain-Trim offset value: Decrement selected channel T gain-trim offsets by -25. ' + $
				'Select channel to be adjusted using the detector selector on the left. Monitor the new position within the pileup field. ' + $
				'Use the "Save Adjusted T GainTrim" menu to save a new T gaintrim file.', /pushbutton_events)
  arrow = picture_button( time_amp_Mid_Base2, geopixe_root + 'images/left-16x14.jpeg', uname='hymod-gaintrim-T-off-down', $
				/tracking, uvalue='Adjust selected detector T Gain-Trim offset value: Decrement selected channel T gain-trim offsets by -5. ' + $
				'Select channel to be adjusted using the detector selector on the left. Monitor the new position within the pileup field. ' + $
				'Use the "Save Adjusted T GainTrim" menu to save a new T gaintrim file.', /pushbutton_events)
  arrow = picture_button( time_amp_Mid_Base2, geopixe_root + 'images/right-16x14.jpeg', uname='hymod-gaintrim-T-off-up', $
				/tracking, uvalue='Adjust selected detector T Gain-Trim offset value: Increment selected channel T gain-trim offsets by +5. ' + $
				'Select channel to be adjusted using the detector selector on the left. Monitor the new position within the pileup field. ' + $
				'Use the "Save Adjusted T GainTrim" menu to save a new T gaintrim file.', /pushbutton_events)
  arrow = picture_button( time_amp_Mid_Base2, geopixe_root + 'images/fast-right-24x14.jpeg', uname='hymod-gaintrim-T-off-up-fast', $
				/tracking, uvalue='Adjust selected detector T Gain-Trim offset value: Increment selected channel T gain-trim offsets by +25. ' + $
				'Select channel to be adjusted using the detector selector on the left. Monitor the new position within the pileup field. ' + $
				'Use the "Save Adjusted T GainTrim" menu to save a new T gaintrim file.', /pushbutton_events)

  label = widget_label( time_amp_Bot_Base2, value='Adj T Gain:')
  arrow = picture_button( time_amp_Bot_Base2, geopixe_root + 'images/fast-left-24x14.jpeg', uname='hymod-gaintrim-T-gain-down-fast', $
				/tracking, uvalue='Adjust selected detector T Gain-Trim gain value: Decrement selected channel T gain-trim gain by -5%. ' + $
				'Select channel to be adjusted using the detector selector on the left. Monitor the new position within the pileup field. ' + $
				'Use the "Save Adjusted T GainTrim" menu to save a new T gaintrim file.', /pushbutton_events)
  arrow = picture_button( time_amp_Bot_Base2, geopixe_root + 'images/left-16x14.jpeg', uname='hymod-gaintrim-T-gain-down', $
				/tracking, uvalue='Adjust selected detector T Gain-Trim gain value: Decrement selected channel T gain-trim gain by -1%. ' + $
				'Select channel to be adjusted using the detector selector on the left. Monitor the new position within the pileup field. ' + $
				'Use the "Save Adjusted T GainTrim" menu to save a new T gaintrim file.', /pushbutton_events)
  arrow = picture_button( time_amp_Bot_Base2, geopixe_root + 'images/right-16x14.jpeg', uname='hymod-gaintrim-T-gain-up', $
				/tracking, uvalue='Adjust selected detector T Gain-Trim gain value: Increment selected channel T gain-trim gain by +1%. ' + $
				'Select channel to be adjusted using the detector selector on the left. Monitor the new position within the pileup field. ' + $
				'Use the "Save Adjusted T GainTrim" menu to save a new T gaintrim file.', /pushbutton_events)
  arrow = picture_button( time_amp_Bot_Base2, geopixe_root + 'images/fast-right-24x14.jpeg', uname='hymod-gaintrim-T-gain-up-fast', $
				/tracking, uvalue='Adjust selected detector T Gain-Trim gain value: Increment selected channel T gain-trim gain by +5%. ' + $
				'Select channel to be adjusted using the detector selector on the left. Monitor the new position within the pileup field. ' + $
				'Use the "Save Adjusted T GainTrim" menu to save a new T gaintrim file.', /pushbutton_events)


 ; File menus

  W_MENU_0 = Widget_Button(time_amp_TLB_MBAR, UNAME='W_MENU_0' ,/MENU  $
      ,VALUE='File')

  W_MENU_1 = Widget_Button(W_MENU_0, UNAME='extract-time-amp'  $
	      ,VALUE='Extract Time-Amp Maia 384')

  W_MENU_1b = Widget_Button(W_MENU_0, UNAME='extract-time-amp-daq'  $
	     	,VALUE='Extract Time-Amp DAQ 36')

if use_gif then begin
	  W_MENU_3 = Widget_Button(W_MENU_0, UNAME='Save_GIF_Menu' ,/SEPARATOR  $
	      ,VALUE='Save as GIF')
endif else begin
	  W_MENU_3b = Widget_Button(W_MENU_0, UNAME='Save_PNG_Menu' ,/SEPARATOR  $
	      ,VALUE='Save as PNG')
endelse

  W_MENU_2 = Widget_Button(W_MENU_0, UNAME='save-pileup-table'  $
	      ,VALUE='Save Pile-up Table')

  W_MENU_3 = Widget_Button(W_MENU_0, UNAME='save-gaintrim-table'  $
	      ,VALUE='Save T gaintrim file')

;  W_MENU_4 = Widget_Button(W_MENU_0, UNAME='Export_Menu' ,/SEPARATOR  $
;	      ,VALUE='Export as CSV')


  W_MENU_6 = Widget_Button(W_MENU_0, UNAME='Exit_Menu' ,/SEPARATOR  $
      ,VALUE='Exit')


; Display menus

  W_MENU_20 = Widget_Button(time_amp_TLB_MBAR, UNAME='W_MENU_20'  $
      ,/MENU ,VALUE='Display')


  W_MENU_21 = Widget_Button(W_MENU_20, UNAME='Clone_Menu_1', VALUE='Clone')

;  W_MENU_21 = Widget_Button(W_MENU_20, UNAME='Clone_Menu', VALUE='Clone', /menu)

;  W_MENU_211 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_1', VALUE='1 Clone')

;  W_MENU_212 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_3', VALUE='3 Clone')

;  W_MENU_213 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_7', VALUE='7 Clone')

;  W_MENU_214 = Widget_Button(W_MENU_21, UNAME='Clone_Menu_11', VALUE='11 Clone')


;  W_MENU_23 = Widget_Button(W_MENU_20, UNAME='Colours_Menu', VALUE='Colours',/SEPARATOR, /menu )

;  W_MENU_2310 = Widget_Button(W_MENU_23, UNAME='Colour_Table', VALUE='Colour Table' )

;  W_MENU_231 = Widget_Button(W_MENU_23, UNAME='Default_Colour_Table', VALUE='Default Colours' )

;  W_MENU_232 = Widget_Button(W_MENU_23, UNAME='Grey_Scale', VALUE='Grey Scale')

;  W_MENU_233 = Widget_Button(W_MENU_23, UNAME='Invert_Colour_Table', VALUE='Invert Colour Table',/SEPARATOR  )

;  W_MENU_234 = Widget_Button(W_MENU_23, UNAME='Linear_Luminance', VALUE='Linear Luminance' )

  W_MENU_22 = Widget_Button(W_MENU_20, UNAME='Clear_Spline_Menu', VALUE='Clear Spline', /separator)

  W_MENU_222 = Widget_Button(W_MENU_20, UNAME='Overlay_Pileup_Menu', VALUE='Overlay Maia Pileup File', /separator)


; Analyze menus

;  W_MENU_40 = Widget_Button(time_amp_TLB_MBAR, UNAME='W_MENU_40'  $
;      ,/MENU ,VALUE='Analyze')


;  W_MENU_401 = Widget_Button(W_MENU_40, UNAME='Analyze_Spline_Menu', VALUE='Within Spline' )


  Widget_Control, /REALIZE, time_amp_TLB

  ;set_time_amp_map_help, time_amp_TLB
  set_time_amp_pileup, time_amp_TLB, pileup

  if wGroup ne 0 then begin
  	register_notify, time_amp_TLB, [ $
				'path', $						; new path
	            'image-update-time' $		       ; q region changed, from Image via Spectrum
		  		], from=wGroup
  endif

  XManager, 'time_amp', time_amp_TLB, /no_block

end


