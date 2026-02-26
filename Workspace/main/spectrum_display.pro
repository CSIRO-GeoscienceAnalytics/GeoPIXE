;
; Spectrum display event routines

pro Spectrum_Display_event, Event

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
         warning,'Spectrum_display_event',['IDL run-time error caught.', '', $
              'Error:  '+strtrim(!error_state.name,2), $
              !Error_state.msg,'',c], /error
         MESSAGE, /RESET
         return
       endif
    endif

  wWidget =  Event.top
  widget_control, hourglass=0

  case tag_names( event,/structure) of
    'NOTIFY': begin
       OnNotify_Spectrum, event
       return
       end
    'WIDGET_TRACKING': begin
       OnTracking_Spectrum, Event
       return
       end
    else:
  endcase

  uname = widget_info( event.id, /uname)

  case uname of

    'Spectrum_TLB': begin
       case Tag_Names(Event, /STRUCTURE_NAME) of
         'WIDGET_KILL_REQUEST': begin
		 	OnKill_Spectrum, Event
          end
         'WIDGET_BASE': begin
;		 	help,event,/str
			OnSize_Spectrum, Event
          end
         'WIDGET_TIMER': begin
		 	OnTimer_Spectrum, Event
          end
       endcase
       end

    'Y_Legend_Draw': begin
       end

    'Spectrum_Draw': begin
		if( Event.type eq 3 )then begin
			OnViewport_Spectrum, Event
		endif else begin
			OnButton_Spectrum, Event
		endelse
		end

    'Select_Button': begin
            OnButton_Select, Event
       end

    'Marker_combobox': begin
            OnSelect_Marker, Event
       end

    'Analyze_Button': begin
       OnButton_Spectrum_Analyze, Event
       end

    'Analyze_Menu_Button': begin
       OnButton_Spectrum_Analyze, Event
       end

    'Identify_Button': begin
            OnButton_Identify, Event
       end

    'Identify_Menu': begin
            OnButton_Identify, Event
       end

    'Full_Button': begin
            OnButton_Full, Event
       end

    'Rescale_Button': begin
            OnButton_Rescale, Event
       end

    'Rescale_combobox': begin
            OnSelect_Rescale, Event
       end

    'Up_Button': begin
            OnButton_Up, Event
       end

    'Down_Button': begin
            OnButton_Down, Event
       end

    'Shrink_Button': begin
            OnButton_Shrink, Event
       end

    'Expand_Button': begin
            OnButton_Expand, Event
       end

    'Widen_Button': begin
            OnButton_Widen, Event
       end

    'Zoom_Button': begin
            OnButton_Zoom, Event
       end

    'Log_Button': begin
            OnButton_Log, Event
       end

    'Clear_Button': begin
            OnButton_Clear, Event
       end

    'Show_Errors_Option': begin
            OnButton_show_errors, Event
       end

    'Show_Diff_Option': begin
            OnButton_show_diff, Event
       end

	'query-button':begin
		geopixe_browser, 'Help/GeoPIXE-Users-Guide.htm', title='GeoPIXE Users Guide', group=event.top, key='Spectrum Display Window'
		end

    'load_menu': begin
            Spectrum_Load, Event, opt=list_device_imports(find='generic_geopixe_spec')
       end

    'load_append_menu': begin
            Spectrum_Load, Event, /append, opt=list_device_imports(find='generic_geopixe_spec')
       end

    'load_trav_menu': begin
            Spectrum_Load, Event, opt=list_device_imports(find='generic_geopixe_trav')
       end

    'load_da_menu': begin
            Spectrum_DA_Load, Event
       end

    'import_spectra_menu': begin
            Spectrum_Import, Event
       end

    'import_energies_menu': begin
            Spectrum_Import_energies, Event
       end

    'save_menu': begin
            Spectrum_Save, Event
       end

    'save_trav_menu': begin
            Spectrum_Save, Event, /trav
       end

    'export_menu': begin
            Spectrum_Export, Event
       end

    'export_fit_menu': begin
            Spectrum_Export, Event, /fits
       end

    'gif_menu': begin
            Spectrum_GIF, Event
       end

    'plot_menu': begin
            Spectrum_Plot_Group, Event, /separate
       end

    'plot_group_menu': begin
            Spectrum_Plot_Group, Event
       end

    'plot_group_png_menu': begin
            Spectrum_Plot_Group, Event, /png
       end

    'plot_group_jpeg_menu': begin
            Spectrum_Plot_Group, Event, /jpeg
       end

    'plot_group_cgm_menu': begin
            Spectrum_Plot_Group, Event, /cgm
       end

    'plot_group_cgm_white_menu': begin
            Spectrum_Plot_Group, Event, /cgm, /white
       end

    'plot_group_wmf_menu': begin
            Spectrum_Plot_Group, Event, /wmf
       end

    'plot_group_wmf_white_menu': begin
            Spectrum_Plot_Group, Event, /wmf, /white
       end

    'Convert_MDA_Menu': begin
       aps_to_list, group=event.top
       end

    'print_menu': begin
            Spectrum_Print, Event
       end

    'exit_menu': begin
            Spectrum_Exit, Event
       end

    'select_menu': begin
            OnButton_Select, Event
       end

    'history_menu': begin
            Spectrum_Menu_history, Event
       end

    'cal_menu': begin
            Spectrum_Menu_Cal, Event
       end

    'clear_cal_menu': begin
            Spectrum_Menu_Clear_Cal, Event
       end

    'get_cal_spec_menu': begin
            Spectrum_Menu_Get_Cal, Event
       end

    'get_cal_image_menu': begin
            Spectrum_Menu_Get_Cal_image, Event
       end

    'identify_mark_menu': begin
            Spectrum_Menu_Identify, Event
       end

    'X0-X5_mark_menu': begin
            Spectrum_Mark_X0X5, Event
       end

    'cal_mark_menu': begin
            Spectrum_Mark_Cal, Event
       end

    'view_mark_menu': begin
            Spectrum_Mark_View, Event
       end

    'element_mark_menu': begin
            Spectrum_Mark_Element, Event
       end

    'cut_mark_menu': begin
            Spectrum_Mark_Cut, Event
       end

    'pileup_menu': begin
            Spectrum_Mark_Pileup, Event
       end

    'pileup_image_menu': begin
            Spectrum_Mark_Pileup_Image, Event
       end

    'escapes_menu': begin
            Spectrum_Mark_Escapes, Event
       end

    'escapes_Si_menu': begin
            Spectrum_Mark_Escapes, Event, /Si
       end

    'clear_mark_menu': begin
            Spectrum_Clear_Mark, Event
       end

    'negatives_menu': begin
            Spectrum_Show_Negatives, Event
       end

    'smooth_menu': begin
            Spectrum_Smooth, Event
       end

    'median_menu': begin
            Spectrum_Median, Event
       end

    'compress_menu': begin
            Spectrum_Compress, Event
       end

    'duplicate_menu': begin
            Spectrum_Duplicate, Event
       end

    'add_menu': begin
            Spectrum_Add, Event
       end

    'add_map_menu': begin
            Spectrum_Add, Event, /map_cal
       end

    'fold_menu': begin
            Spectrum_Fold, Event, /bit12
       end

    'fold2_menu': begin
            Spectrum_Fold, Event, /bit12, /bit11
       end

    'correct_throttle_menu': begin
            Spectrum_correct_throttle, Event
       end

    'throttle_menu': begin
            Spectrum_throttle, Event
       end

    'Plugin': begin
       Spectrum_Process_Plugin, Event
       end
       
    'Reload-Plugins': begin
       Spectrum_Reload_Plugins, Event
    	end

    'pixe_fit_menu': begin
            Spectrum_Fit_Setup, Event
       end

    'pige_fit_window': begin
            Spectrum_Fit_Setup, Event, /gamma
       end

    'fit_results_window': begin
            Spectrum_Fit_Results, Event
       end

    'error_function_menu': begin
            Spectrum_Error_Function, Event
       end

    'gauss_function_menu': begin
            Spectrum_Gauss_Function, Event
       end

    'do_fit_function_menu': begin
            Spectrum_Do_Curve_Fit, Event
       end

    'do_fit_function_menu': begin
            Spectrum_Do_Curve_Fit, Event
       end

    'background_menu': begin
            Spectrum_Menu_Background, Event
       end

    'Image_Display_Menu': begin
            gImage, group=Event.top
       end

    'command_menu': begin
            Command, group=Event.top
       end

    'evt_menu': begin
            Spectrum_EVT, event
       end

    'cut_setup_menu': begin
            Spectrum_Cut_Setup, Event
       end

    'Time_Amp_Menu': begin
            Spectrum_Time_Amp, Event
       end

    'yield_calc_window': begin
            Spectrum_Yield_Calc, Event
       end

	'depth_profile_calc_window': begin
            Spectrum_depth_Calc, Event
       end

    'Edit_Filters_Menu': begin
            Spectrum_Edit_Filters, Event
       end

    'Edit_Detectors_Menu': begin
            Spectrum_Edit_Detectors, Event
       end

	'detector_map_menu': begin
		spectrum_detector_map, event
		end

    'Blog_Browser_Menu': begin
            Spectrum_Blog_Browser, Event
       end

	'Help_Recent': begin
		geopixe_browser, 'Help/Recent News.htm', title='Recent News', group=event.top, /centre
		end

	'Help_Overview': begin
		geopixe_browser, 'Help/GeoPIXE Overview.htm', title='GeoPIXE Overview', group=event.top
		end

	'Help_Worked_examples': begin
		geopixe_browser, 'Help/GeoPIXE Worked Examples Open Notes.htm', title='GeoPIXE Worked Examples', group=event.top
		end

	'Help_Maia_User': begin
   		geopixe_browser, 'Help/Maia-384-user-help.htm', title='Maia 384 User Guide', group=event.top, key='Table of Contents'
       end

    'Help_User': begin
		geopixe_browser, 'Help/GeoPIXE-Users-Guide.htm', title='GeoPIXE Users Guide', group=event.top, key='Table of Contents'
       end

    'Help_Query': begin
		restore, geopixe_root+'idl_query.sav'
       IDL_query_geopixe
       end

    else:
  endcase

  widget_control, hourglass=0
end

;-------------------------------------------------------------------------------------

pro Spectrum_display, GROUP_LEADER=wGroup, TLB=Spectrum_TLB, path=path, $
       xoffset=xoffset, yoffset=yoffset, nosav=nosav, test=test, $
       start_identify=start_identify, start_pixe=start_pixe, realtime=realtime, $
       dpath=dpath, ppercent=ppercent, title=stitle, update_notify=update_notify, $
       no_clear=no_clear, show_negatives=show_negatives, no_sum=no_sum, $
       no_query=no_query, chart=chart, offsety=offsety, _EXTRA=_VWBExtra_

; Note 'spectrum' keyword passed on to Postcreate routine via 'extras'.

  COMPILE_OPT STRICTARR
  common c_spectrum_start, spectrum_compiled
  common c_working_dir, geopixe_root
  common c_geopixe_vm, geopixe_enable_vm
  if n_elements(geopixe_enable_vm) lt 1 then geopixe_enable_vm=1

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
       warning,'Spectrum_display',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
  endif else on_error,0

  if n_elements(wGroup) lt 1 then wGroup=0
  if n_elements(path) lt 1 then path=''
  if n_elements(dpath) lt 1 then dpath=path
  if n_elements(nosav) lt 1 then nosav=0
  if n_elements(test) lt 1 then test=0
  if n_elements(chart) lt 1 then chart=0
  if n_elements(no_sum) lt 1 then no_sum=0
  if n_elements(no_query) lt 1 then no_query=0
  if n_elements(realtime) lt 1 then realtime=0
  if n_elements(ppercent) eq 0 then ppercent=ptr_new(0.0)
  if n_elements(start_identify) lt 1 then start_identify=0
  if n_elements(start_PIXE) lt 1 then start_PIXE=0
  if n_elements(show_negatives) lt 1 then show_negatives=0
  start_up = 0
  if start_identify or start_PIXE then start_up=1
  if n_elements(update_notify) eq 0 then update_notify='spectrum-display'
  if n_elements(no_clear) lt 1 then no_clear=0
  if n_elements(layout) lt 1 then layout=''
  if n_elements(offsety) lt 1 then offsety=0
  stitle1 = 'Spectrum Display'
  if realtime then stitle1='Maia Spectrum Display'
  if n_elements(stitle) lt 1 then stitle=stitle1
  stitle = stitle + '  (GeoPIXE ' + geopixe_version() + ')'

  if n_elements( spectrum_compiled) lt 1 then begin
    spectrum_routines
    t = strip_clip()						; compile SNIP routines
    error_function							; default curve fit function (pro)
    register_notify
    spectrum_select_eventcb
    spectrum_display_eventcb				; Load event callback routines
    spectrum_compiled = 1
  endif
  startupp, /colours						; setup IDL graphics

  !p.charsize = 1.0
  case !version.os_family of
    'MacOS': begin
       symbol = 'SYMBOL*12'
       large_font = 'Arial*12'
;@2       widget_control, default_font='Geneva*10'   ; set font for all windows
       draw_trim = 15
       scr_trim = 21
       xoff = 10
       yoff = 335
       xsize_select = 45
       xsize_marker = 115
       xsize_analyze = 20
       xsize_ident = 20
       xsize_full = 35
       xsize_rescale = 45
       xsize_rescale2 = 70
       xsize_up = 27
       xsize_down = 37
       xsize_shrink = 45
       xsize_expand = 45
       xsize_widen = 45
       xsize_log = 50
       ysize_button = 23
       button_space = 1
       y_trim = 16
       legend_xsize = 57
       end
    'unix': begin
       symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
       large_font = '10x20'
 ;@2      widget_control, default_font='6x13'          ; set font for all windows
       draw_trim = 0
       scr_trim = 30
       xoff = 0
       yoff = 470
       xsize_select = 50
       xsize_marker = 90
       xsize_analyze = 24
       xsize_ident = 24
       xsize_full = 35
       xsize_rescale = 54
       xsize_rescale2 = 70
       xsize_up = 27
       xsize_down = 37
       xsize_shrink = 47
       xsize_expand = 48
       xsize_widen = 44
       xsize_log = 56
       ysize_button = 23
       button_space = 1
       y_trim = 26
       legend_xsize = 39
       end
    else: begin
       symbol = 'SYMBOL*BOLD*14'
       large_font = 'COURIER*BOLD*10'
    ;  widget_control, default_font='Arial*14'          ; set font for all windows
       draw_trim = 0
       scr_trim = 21        ; 15
       xoff = 0
       yoff = 425
       xsize_select = 40
       xsize_marker = 90
       xsize_analyze = 22
       xsize_ident = 22
       xsize_full = 26
       xsize_rescale = 48
       xsize_rescale2 = 60
       xsize_up = 23
       xsize_down = 37
       xsize_shrink = 38
       xsize_expand = 44
       xsize_widen = 41
       xsize_log = 45
       ysize_button = 23
       button_space = 3
       y_trim = 20         	 ; 16
	   legend_xsize = 45		; 57
       end
endcase

;  if (n_elements(xoffset) lt 1) and (n_elements(yoffset) lt 1) then begin
;   on_ioerror, cont
;   openr, lun, 'spectrum_display.snap', /xdr, /get_lun
;   xoff2 = 0.0
;   yoff2 = 0.0
;   xsize = 0.0
;   ysize = 0.0
;   readu, lun, xoff2, yoff2, xsize, ysize
;   xoffset = xoff2 > 0.0
;   yoffset = (yoff2) > 0.0
;   xsize = xsize > 0.0
;   ysize = ysize > 0.0
;cont:
;   close_file, lun
;  endif

  if n_elements(yoffset) lt 1 then begin
    screen = get_screen_size()
    yoffset = (screen[1]-28 - yoff*(1+offsety)) > 0
  endif
  if n_elements(xoffset) lt 1 then xoffset = xoff

  add_plugins = 0
  plugins = Spectrum_Load_Plugins( error=error)
  if error eq 0 then add_plugins = 1

  Spectrum_TLB = Widget_Base( GROUP_LEADER=wGroup, xoffset=xoffset, yoffset=yoffset, $
      UNAME='Spectrum_TLB' ,/TLB_KILL_REQUEST_EVENTS, _EXTRA=_VWBExtra_ $
      ,/TLB_SIZE_EVENTS ,TITLE=stitle ,SPACE=button_space ,XPAD=2 ,YPAD=2  $
      ,COLUMN=1 ,MBAR=Spectrum_TLB_MBAR)


  Draw_Base = Widget_Base(Spectrum_TLB, UNAME='Draw_Base' ,SPACE=0  $
      ,XPAD=0 ,YPAD=0 ,ROW=1)

  PostCreate_Draw_Base, Draw_Base, path=path, plugins=plugins, test=test, $
              start_identify=start_identify, start_pixe=start_pixe, update_notify=update_notify, $
              dpath=dpath, realtime=realtime, ppercent=ppercent, $
              show_negatives=show_negatives, chart=chart, _EXTRA=_VWBExtra_

  if n_elements(xsize) lt 1 then begin
    xsize2 = 600
  endif else begin
    xsize2 = xsize - 108
  endelse
  if n_elements(ysize) lt 1 then begin
    ysize2 = 285
  endif else begin
    ysize2 = ysize - 125
  endelse
  xsize3 = 2400

  if !version.os_family eq 'unix' then begin
    retain = 2
  endif else begin
    retain = 1
  endelse

  Y_Legend_Draw = Widget_Draw(Draw_Base, UNAME='Y_Legend_Draw'  $
      ,SCR_XSIZE=legend_xsize+scr_trim ,SCR_YSIZE=ysize2 ,NOTIFY_REALIZE='OnRealize_Legend'  $
      ,RETAIN=retain)

  Spectrum_Draw = Widget_Draw(Draw_Base, UNAME='Spectrum_Draw'  $
      ,SCR_XSIZE=xsize2+scr_trim ,SCR_YSIZE=ysize2+scr_trim  $
      ,NOTIFY_REALIZE='OnRealize_Spectrum'  $
      ,KILL_NOTIFY='OnDestroy_Spectrum' ,/SCROLL ,XSIZE=xsize3+draw_trim  $
      ,YSIZE=ysize2 ,RETAIN=retain ,/BUTTON_EVENTS,/VIEWPORT_EVENTS)


  PostCreate_Spectrum, Spectrum_Draw, _EXTRA=_VWBExtra_


  Button_Base = Widget_Base(Spectrum_TLB, UNAME='Button_Base'  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,/ROW, /base_align_center)

  Select_Button = Widget_Button(Button_Base, UNAME='Select_Button'  $
      ,/ALIGN_CENTER ,VALUE='Select', xsize=xsize_select, tracking_events=1, $
      uvalue='Popup window to select spectra to display and edit.', ysize=ysize_button)

  spc = widget_label(button_base, value='', xsize=1)

  Marker_combobox = Widget_Combobox(Button_Base,  $
      UNAME='Marker_combobox' ,NOTIFY_REALIZE='OnRealize_Marker'  $
      ,VALUE=[ 'Identify Line', 'X0-X5', 'Cal 0,1', 'View 0,1', 'Cut 0,1', 'Mark element' ], $
      tracking_events=1, xsize=xsize_marker, ysize=ysize_button, $
      uvalue='Droplist to select current active set of markers. Position markers starting with right marker.')


  spc = widget_label(button_base, value='', xsize=1)

if no_sum eq 0 then begin
  Analyze_Button = Widget_Button( Button_Base, UNAME='Analyze_Button', ysize=ysize_button  $
      ,/ALIGN_CENTER ,VALUE='S', xsize=xsize_analyze, tracking_events=1, font=symbol, $
      uvalue='Integrate counts between Cut or Xn markers. Results are show in the "Cuts Setup" window.')
endif else Analyze_Button=0L

if no_query eq 0 then begin
  Identify_Button = Widget_Button(Button_Base, UNAME='Identify_Button', ysize=ysize_button  $
      ,/ALIGN_CENTER ,VALUE='?', tracking_events=1, xsize=xsize_ident, $
      uvalue='Open identification window, for X-ray line identification. Position "Identify" marker on a peak for identification.')
endif else Identify_Button=0L


  spc = widget_label(button_base, value='', xsize=button_space)

  Full_Button = Widget_Button(Button_Base, UNAME='Full_Button', ysize=ysize_button  $
      ,/ALIGN_CENTER ,VALUE='Full', tracking_events=1, xsize=xsize_full, $
      uvalue='Display full spectrum.')


  Shrink_Button = Widget_Button(button_base, UNAME='Shrink_Button', ysize=ysize_button  $
      ,/ALIGN_CENTER ,VALUE='Shrink', tracking_events=1, xsize=xsize_shrink, $
      uvalue='X; Shrink X axis about middle energy displayed; increase energy range displayed.')


  Expand_Button = Widget_Button(button_base, UNAME='Expand_Button', ysize=ysize_button  $
      ,/ALIGN_CENTER ,VALUE='Expand', tracking_events=1, xsize=xsize_expand, $
      uvalue='X: Expand X axis about middle energy displayed; decrease energy range displayed.')


  Widen_Button = Widget_Button(button_base, UNAME='Widen_Button', ysize=ysize_button $
      ,/ALIGN_CENTER ,VALUE='Widen', tracking_events=1, xsize=xsize_widen, $
      uvalue='X: Widen X axis; expand between the VIEW markers. Set View markers starting with right marker.')


;  Zoom_Button = Widget_Button(button_base, UNAME='Zoom_Button', ysize=ysize_button  $
;      ,/ALIGN_CENTER ,VALUE='Zoom', tracking_events=1, $
;      uvalue='X: Zoom XY axes; drag out a box to define new X,Y view.')


  spc = widget_label(button_base, value='', xsize=button_space)

;  Rescale_Button = Widget_Button(button_base, UNAME='Rescale_Button', ysize=ysize_button  $
;      ,/ALIGN_CENTER ,VALUE='Rescale', tracking_events=1, xsize=xsize_rescale, $
;      uvalue='Y: Rescale spectrum Y axis to show all. Will also suppress zero.')


  rescale_combobox = widget_combobox(Button_Base,  $
      UNAME='Rescale_combobox' ,NOTIFY_REALIZE='OnRealize_Rescale'  $
      ,VALUE=[ 'Fixed','Auto','Norm','Cut' ], $
      tracking_events=1, xsize=xsize_rescale2, ysize=ysize_button, $
      uvalue='Y: Droplist to select Y scale mode: Fixed (Manual), Auto, Norm (normalize to max), Cut (normalize to cut range sum).')


  Up_Button = Widget_Button(button_base, UNAME='Up_Button', ysize=ysize_button  $
      ,/ALIGN_CENTER ,VALUE='Up', tracking_events=1, xsize=xsize_up, $
      uvalue='Y: Bump spectrum up; reduce top counts per channel displayed.')


  Down_Button = Widget_Button(button_base, UNAME='Down_Button', ysize=ysize_button  $
      ,/ALIGN_CENTER ,VALUE='Down', tracking_events=1, xsize=xsize_down, $
      uvalue='Y: Bump spectrum down; increase top counts per channel displayed.')


  Log_Button = Widget_Button(button_base, UNAME='Log_Button', ysize=ysize_button  $
      ,/ALIGN_CENTER ,VALUE='Log/Lin', tracking_events=1, xsize=xsize_log, $
      uvalue='Y: Toggle between log and linear vertical scale.')


  spc = widget_label(button_base, value='', xsize=1)

  if realtime then begin
  	if no_clear eq 0 then begin
	  	clear_Button = Widget_Button(button_base, UNAME='Clear_Button', ysize=ysize_button  $
	      ,/ALIGN_CENTER ,VALUE='Clear', tracking_events=1, xsize=xsize_log, $
	      uvalue='Clear ALL regions spectra.')
	endif
  endif else begin
  	errors_option = cw_bgroup2( Button_base, ['Errors'], /row, xpad=0, ypad=0, space=0, $
					/return_index, /tracking, uname='Show_Errors_Option', set_value=[0], /nonexclusive, $
					uvalue=['Y: Show spectra histograms with/without error bars. Data with error bars (traverse) will always show error bars.'])


  	diff_option = cw_bgroup2( Button_base, ['Diff'], /row, xpad=0, ypad=0, space=0, $
					/return_index, /tracking, uname='Show_Diff_Option', set_value=[0], /nonexclusive, $
					uvalue=['Y: Show the difference between data and the first overlay (back).'])
  endelse

  Help_Base = Widget_Base(Spectrum_TLB, UNAME='Help_Base'  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,/ROW, /base_align_center)

  Help_Text = Widget_Text(Help_Base, UNAME='Help_Text' $
      ,NOTIFY_REALIZE='OnRealize_Help',scr_XSIZE=legend_xsize+xsize2+scr_trim, frame=0 ,YSIZE=1)

  query_button2 = Widget_Button(Help_Base, UNAME='query-button', xsize=15, ysize=20,  $
      /ALIGN_CENTER ,VALUE='?', /tracking_events, uvalue='Jump to the help on this window in the GeoPIXE Users Guide.')


  W_MENU_1 = Widget_Button(Spectrum_TLB_MBAR, UNAME='file_menu' ,/MENU  $
      ,VALUE='File')

if realtime eq 0 and chart eq 0 then begin
  W_MENU_11 = Widget_Button(W_MENU_1, UNAME='load_menu' ,VALUE='Load SPEC')

  W_MENU_11b = Widget_Button(W_MENU_1, UNAME='load_append_menu' ,VALUE='Append SPEC')

  W_MENU_16 = Widget_Button(W_menu_1, UNAME='import_menu',/MENU, VALUE='Import')

  W_MENU_16a = Widget_Button(W_menu_16, UNAME='import_spectra_menu', VALUE='Spectra')

  W_MENU_16b = Widget_Button(W_menu_16, UNAME='import_energies_menu', VALUE='Energies from list-mode')

  W_MENU_13 = Widget_Button(W_MENU_1, UNAME='load_da_menu' ,/SEPARATOR, VALUE='Load DA Matrix')

  W_MENU_12 = Widget_Button(W_MENU_1, UNAME='load_trav_menu' ,VALUE='Load TRAV')
endif

  W_MENU_14 = Widget_Button(W_MENU_1, UNAME='save_menu' , /separator, VALUE='Save SPEC')

if realtime eq 0 and chart eq 0 then begin
  W_MENU_15 = Widget_Button(W_MENU_1, UNAME='save_trav_menu' , VALUE='Save TRAV')
endif

  W_MENU_17 = Widget_Button(W_menu_1, UNAME='export' ,/MENU, /separator, VALUE='Export')

  W_MENU_171 = Widget_Button(W_MENU_17, UNAME='export_menu' , VALUE='CSV Table')

  W_MENU_171b = Widget_Button(W_MENU_17, UNAME='export_fit_menu' , VALUE='CSV Table w/ Overlays')

  W_MENU_172a = Widget_Button(W_MENU_17, UNAME='plot_group_jpeg_menu', VALUE='JPEG plots')

  W_MENU_172b = Widget_Button(W_MENU_17, UNAME='plot_group_png_menu', VALUE='PNG plots')

  W_MENU_172 = Widget_Button(W_MENU_17, UNAME='plot_group_cgm_menu', VALUE='CGM plots')

;  W_MENU_173 = Widget_Button(W_MENU_17, UNAME='plot_group_cgm_white_menu', VALUE='CGM plot on white')

  W_MENU_174 = Widget_Button(W_MENU_17, UNAME='plot_group_wmf_menu', VALUE='WMF plots')

;  W_MENU_175 = Widget_Button(W_MENU_17, UNAME='plot_group_wmf_white_menu', VALUE='WMF plot on white')

;  W_MENU_176 = Widget_Button(W_MENU_17, UNAME='gif_menu' , VALUE='GIF plot on black')

if realtime eq 0 and chart eq 0 then begin
  W_MENU_17b = Widget_Button(W_menu_1, UNAME='convert' ,/MENU, VALUE='Convert')

  W_MENU_17b1 = Widget_Button(W_MENU_17b, UNAME='Convert_MDA_Menu' , VALUE='APS MDA to LST')
endif

  W_MENU_18 = Widget_Button(W_MENU_1, UNAME='plot_menu' ,/SEPARATOR  $
      ,VALUE='Print Plot')

;  W_MENU_19 = Widget_Button(W_MENU_1, UNAME='plot_group_menu'   $
 ;     ,VALUE='Plot Group')

;  W_MENU_19b = Widget_Button(W_MENU_1, UNAME='print_menu' ,VALUE='Print',/separator)

  W_MENU_19z = Widget_Button(W_MENU_1, UNAME='exit_menu' ,/SEPARATOR  $
      ,VALUE='Exit')


  W_MENU_2 = Widget_Button(Spectrum_TLB_MBAR, UNAME='display_menu'  $
      ,/MENU ,VALUE='Display')

  W_MENU_21 = Widget_Button(W_MENU_2, UNAME='select_menu', VALUE='Spectrum Select' )

 if chart eq 0 then begin
  W_MENU_22 = Widget_Button(W_MENU_2, UNAME='cal_menu', VALUE='Calibrate Energy' )

  W_MENU_22c = Widget_Button(W_MENU_2, UNAME='get_cal_spec_menu', VALUE='Get ALL Energy Cals from SPEC' )

  W_MENU_22e = Widget_Button(W_MENU_2, UNAME='get_cal_image_menu', VALUE='Get ALL Energy Cals from DAI' )

  W_MENU_22d = Widget_Button(W_MENU_2, UNAME='clear_cal_menu', VALUE='Clear ALL Energy Cals' )

  W_MENU_22a = Widget_Button(W_MENU_2, UNAME='Identify_Menu', VALUE='Identify X-rays' )
 endif

  W_MENU_23 = Widget_Button(W_MENU_2, UNAME='mark_menu' ,/MENU  $
      ,VALUE='Mark',/SEPARATOR )


  W_MENU_231 = Widget_Button(W_MENU_23, UNAME='identify_mark_menu'  $
      ,VALUE='Identify')

  W_MENU_232 = Widget_Button(W_MENU_23, UNAME='X0-X5_mark__menu' ,VALUE='X0 - X5')

  W_MENU_233 = Widget_Button(W_MENU_23, UNAME='cal_mark_menu' ,VALUE='Cal 0,1')

  W_MENU_234 = Widget_Button(W_MENU_23, UNAME='view_mark_menu' ,VALUE='View 0,1')

  W_MENU_235 = Widget_Button(W_MENU_23, UNAME='element_mark_menu' ,VALUE='Element')

  W_MENU_236 = Widget_Button(W_MENU_23, UNAME='cut_mark_menu' ,VALUE='Cut 0,1')


  W_MENU_24 = Widget_Button(W_MENU_2, UNAME='clear_mark_menu', VALUE='Clear All Markers')


if chart eq 0 then begin
   W_MENU_25 = Widget_Button(W_MENU_2, UNAME='pileup1_menu', VALUE='Pileup', /separator, /menu)

  W_MENU_251 = Widget_Button(W_MENU_25, UNAME='pileup_menu', VALUE='from Single Spectrum')

  W_MENU_252 = Widget_Button(W_MENU_25, UNAME='pileup_image_menu', VALUE='using Image products')


  W_MENU_26 = Widget_Button(W_MENU_2, UNAME='escapes_menu', VALUE='Ge Escapes')

  W_MENU_26b = Widget_Button(W_MENU_2, UNAME='escapes_Si_menu', VALUE='Si Escapes')
endif

if (realtime eq 0) or show_negatives or chart then begin
  W_MENU_27 = Widget_Button(W_MENU_2, UNAME='negatives_menu', VALUE='Show Negative',/SEPARATOR )
endif

plugin_menus_root = 0L
plugin_menus = 0L
if chart eq 0 then begin
  plugin_menus_root = Widget_Button(Spectrum_TLB_MBAR, UNAME='process_menu', /MENU ,VALUE='Process')

  if realtime eq 0 then begin
  W_MENU_31 = Widget_Button(plugin_menus_root, UNAME='smooth_menu', VALUE='Smooth')

  W_MENU_31b = Widget_Button(plugin_menus_root, UNAME='median_menu', VALUE='Median Filter')

  W_MENU_32 = Widget_Button(plugin_menus_root, UNAME='compress_menu', VALUE='Compress')

  W_MENU_32a = Widget_Button(plugin_menus_root, UNAME='duplicate_menu', VALUE='Duplicate')

  W_MENU_32b = Widget_Button(plugin_menus_root, UNAME='add_menu', VALUE='Add')

  W_MENU_32b = Widget_Button(plugin_menus_root, UNAME='add_map_menu', VALUE='Add (re-map cal)')

  W_MENU_32c = Widget_Button(plugin_menus_root, UNAME='fold_menu', VALUE='Fold (bit #12)')

  W_MENU_32c = Widget_Button(plugin_menus_root, UNAME='fold2_menu', VALUE='Fold (bits #11,12)')
  endif

  W_MENU_32d = Widget_Button(plugin_menus_root, UNAME='correct_throttle_menu', VALUE='Correct Throttle')

	W_MENU_34 = Widget_Button(plugin_menus_root, UNAME='Reload-Plugins', VALUE='Reload User Plugins', /separator )
	
    plugin_menus = Widget_Button(plugin_menus_root, UNAME='W_MENU_33', /menu, VALUE='User Plugins' )

	if add_plugins then begin
	  for i=0L,n_elements((*plugins).title)-1 do begin
	    W_MENU_330 = Widget_Button(plugin_menus, UNAME='Plugin', VALUE=(*plugins).title[i], $
	         uvalue=(*plugins).list[i] )
	  endfor
	endif else plugin_menus=0L

  W_MENU_4 = Widget_Button(Spectrum_TLB_MBAR, UNAME='analyze_menu'  $
      ,/MENU ,VALUE='Analyze')


  W_MENU_40 = Widget_Button(W_MENU_4, UNAME='Analyze_Menu_Button', VALUE='Analyze Cut')

  W_MENU_41 = Widget_Button(W_MENU_4, UNAME='PIXE_menu', /MENU ,VALUE='X-ray Spectrum', /separator)

  W_MENU_411 = Widget_Button(W_MENU_41, UNAME='pixe_fit_menu' ,VALUE='Fit setup')

  if realtime eq 0 then begin
  W_MENU_42 = Widget_Button(W_MENU_4, UNAME='curve_fit_menu', /MENU ,VALUE='Fit Curve')

  W_MENU_421 = Widget_Button(W_MENU_42, UNAME='fit_function_menu', /MENU, VALUE='Function')

  W_MENU_4211 = Widget_Button(W_MENU_421, UNAME='error_function_menu' ,VALUE='Error function')

  W_MENU_4212 = Widget_Button(W_MENU_421, UNAME='gauss_function_menu' ,VALUE='Gaussian function')


  W_MENU_422 = Widget_Button(W_MENU_42, UNAME='do_fit_function_menu', VALUE='Perform Fit')

;  W_MENU_43 = Widget_Button(W_MENU_4, UNAME='background_menu', VALUE='Adjust Background',/SEPARATOR )
  endif

  W_MENU_41b = Widget_Button(W_MENU_4, UNAME='throttle_menu', VALUE='Throttle', /separator)


  W_MENU_5 = Widget_Button(Spectrum_TLB_MBAR, UNAME='window_menu'  $
      ,/MENU ,VALUE='Window')

;  W_MENU_51 = Widget_Button(W_MENU_5, UNAME='Image_Display_Menu' , VALUE='Image Display')

  W_MENU_52 = Widget_Button(W_MENU_5, UNAME='select_menu', VALUE='Spectrum Select' )
  W_MENU_52a = Widget_Button(W_MENU_5, UNAME='history_menu', VALUE='Spectrum Properties and History' )
  W_MENU_52b = Widget_Button(W_MENU_5, UNAME='cal_menu', VALUE='Calibrate Energy')
  W_MENU_52c = Widget_Button(W_MENU_5, UNAME='Identify_Menu', VALUE='Identify X-rays' )

  W_MENU_53 = Widget_Button(W_MENU_5, UNAME='cut_setup_menu', VALUE='Cuts Setup' )
  W_MENU_53b = Widget_Button(W_MENU_5, UNAME='detector_map_menu', VALUE='Detector Map' )

  W_MENU_54 = Widget_Button(W_MENU_5, UNAME='pixe_fit_menu' ,VALUE='X-ray Spectrum Fit',/separator)

  W_MENU_55 = Widget_Button(W_MENU_5, UNAME='fit_results_window' ,VALUE='Fit Results')
  W_MENU_55a = Widget_Button(W_MENU_5, UNAME='yield_calc_window' ,VALUE='PIXE/SXRF Yield Calculation')
  W_MENU_55a2 = Widget_Button(W_MENU_5, UNAME='depth_profile_calc_window' ,VALUE='PIXE/SXRF Depth Profile Calculation')

  if realtime eq 0 then begin
  W_MENU_55b = Widget_Button(W_MENU_5, UNAME='Time_Amp_Menu', VALUE='Time Amplitude display',/SEPARATOR )
;  if test then W_MENU_54b = Widget_Button(W_MENU_5, UNAME='pige_fit_window' ,VALUE='PIGE Fit')

  W_MENU_54b = Widget_Button(W_MENU_5, UNAME='pige_fit_window' ,VALUE='PIGE Fit')
  endif

  W_MENU_56 = Widget_Button(W_MENU_5, UNAME='Edit_Filters_Menu', VALUE='Edit Filters',/SEPARATOR )
  W_MENU_57 = Widget_Button(W_MENU_5, UNAME='Edit_Detectors_Menu', VALUE='Edit Detectors' )

  if realtime eq 0 then begin
  W_MENU_58 = Widget_Button(W_MENU_5, UNAME='Blog_Browser_Menu', VALUE='Blog Browser' )
  endif
endif

; Help menus

  W_MENU_70 = Widget_Button(Spectrum_TLB_MBAR, UNAME='W_MENU_70', /MENU ,VALUE='Help')

  W_MENU_70x = Widget_Button(W_MENU_70, UNAME='Help_Recent' ,VALUE="Recent News")

  W_MENU_70a = Widget_Button(W_MENU_70, UNAME='Help_Overview' ,VALUE="GeoPIXE Overview")

  W_MENU_70b = Widget_Button(W_MENU_70, UNAME='Help_Worked_examples' ,VALUE="GeoPIXE Worked Examples Guide")

  W_MENU_71 = Widget_Button(W_MENU_70, UNAME='Help_User' ,VALUE="GeoPIXE User's Guide")

  W_MENU_71b = Widget_Button(W_MENU_70, UNAME='Help_Maia_User' ,VALUE="Maia User's Guide")

  W_MENU_72 = Widget_Button(W_MENU_70, UNAME='Help_Query' ,VALUE="Query IDL Environment")

  Widget_Control, /REALIZE, Spectrum_TLB
  if start_up then widget_control, Spectrum_TLB, Timer=0.1

  if wGroup ne 0L then begin
    register_notify, Spectrum_TLB, [update_notify, $	  ; spectra display parameters
                    'images', $          				 ; pass on notify of new images
                    'spectra', $         				 ; new spectra loaded
                    'mark-e', $							; mark line energy from Identify (pass to setup-filter)
                    'image-region-select', $		    ; new region
					'image-region-delete', $			; to delete a selected spectrum (region) row
					'image-regions', $					; regions pointers, if valid w/ spectra
	                'image-spectrum-throttle', $   		; notify from image_table spectrum throttle to spectrum_display
                    'image-clone', $        			 ; new image clones
	                'image-update-time', $		       ; q region changed, to pass on to Time Amp
	                'detector-select', $				; detector selected on rates (Maia) window
                    'images', $           				; new images loaded
                    'correct-image-pileup', $		   ; subtract pileup from images
                    'time-amp-pileup', $     		  ; pileup table results (from time_amp)
                    'path', $              				; new path
                    'dpath' $              				; new raw data path path
                    ], from=wGroup
  endif
  register_notify, Spectrum_TLB, 'snapshot'
  register_notify, Spectrum_TLB, ['wizard-action']		; global notify from a wizard

  XManager, 'Spectrum_Display', Spectrum_TLB, /no_block

	child = widget_info( Spectrum_TLB, /child)
	widget_control, child, get_uvalue=pstate
	(*pstate).tlb = Spectrum_TLB
	(*pstate).plugin_menus = plugin_menus
	(*pstate).plugin_menus_root = plugin_menus_root	
	return
end
