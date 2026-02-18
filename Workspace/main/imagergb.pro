;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	06/28/99 15:17.29
;
pro ImageRGB_event, Event

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
		warning,'ImageRGB_Event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

  wWidget =  Event.top
  widget_control, hourglass=0
;  help, event,/structure

	case tag_names( event,/structure) of
		'NOTIFY': begin
			OnNotify_ImageRGB, event
			return
			end
		'WIDGET_TRACKING': begin
			OnTracking_ImageRGB, Event
			return
			end
		else:
	endcase

  uname = widget_info( event.id, /uname)

  case uname of

	'ImageRGB_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_KILL_REQUEST': begin
				OnKill_ImageRGB, Event
				end
			'WIDGET_BASE': begin
				OnSize_ImageRGB, Event
				end
			else:
		endcase
		end

	'ImageRGB_Draw': begin
		if( Event.type eq 3 )then begin
			OnViewport_ImageRGB, Event
		endif else begin
;			OnButton_ImageRGB, Event
		endelse
		end

	'Element_combobox1': begin
		OnSelect_ImageRGB_Element, Event, 1
		end

	'Element_combobox2': begin
		OnSelect_ImageRGB_Element, Event, 2
		end

	'Element_combobox3': begin
		OnSelect_ImageRGB_Element, Event, 3
		end

	'Max_Text1': begin
		OnSelect_ImageRGB_Max, Event, 1
		end

	'Max_Text2': begin
		OnSelect_ImageRGB_Max, Event, 2
		end

	'Max_Text3': begin
		OnSelect_ImageRGB_Max, Event, 3
		end

	'Full_Button': begin
		OnButton_ImageRGB_Full, Event
		end

	'Zoom_In_Button': begin
		OnButton_ImageRGB_Zoom_In, Event
		end

	'Zoom_Out_Button': begin
		OnButton_ImageRGB_Zoom_Out, Event
		end

    'Compare': begin
       ImageRGB_Compare, Event
       end

    'Compare3': begin
       ImageRGB_Compare3, Event
       end

	'Save_GIF_Menu': begin
		ImageRGB_Save_GIF, Event
		end

	'Save_JPEG_Menu': begin
		ImageRGB_Save_JPEG, Event
		end

	'Save_PNG_Menu': begin
		ImageRGB_Save_GIF, Event, /PNG
		end

    'Save_CGM_Menu': begin
       ImageRGB_Export, Event, /CGM
       end

    'Save_WMF_Menu': begin
       ImageRGB_Export, Event, /WMF
       end

    'Save_PNG_Menu2': begin
       ImageRGB_Export, Event, /PNG
       end

    'Save_JPEG_Menu2': begin
       ImageRGB_Export, Event, /JPEG
       end

	'Plot_Menu': begin
	;	ImageRGB_plot_results, Event
		end

	'Print_Menu': begin
		ImageRGB_Export, Event
		end

	'Exit_Menu': begin
		ImageRGB_Exit, Event
		end

	'Plot_Results_Menu': begin
	;	ImageRGB_results_Plot, Event
		end

	'learn-start': begin
		ImageRGB_Learn, Event, /first
		end

	'learn-next': begin
		ImageRGB_Learn, Event
		end

	'learn-save': begin
		ImageRGB_Learn, Event, /save
		end

	'learn-restore': begin
		ImageRGB_Learn, Event, /restore
		end

	'learn-execute': begin
		ImageRGB_Learn, Event, /execute
		end

	'query-button':begin
		geopixe_browser, 'Help/GeoPIXE-Users-Guide.htm', title='GeoPIXE Users Guide', group=event.top, key='RGB Three Element Images'
		end

	else:
  endcase
	widget_control, hourglass=0
end

;-----------------------------------------------------------------------------------

pro ImageRGB, GROUP_LEADER=wGroup, TLB=ImageRGB_TLB, path=path, _EXTRA=_VWBExtra_, $
			pimages=pimages

COMPILE_OPT STRICTARR
common c_errors_1, catch_errors_on
common c_gif, use_gif

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
		warning,'ImageRGB',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

  	ImageRGB_routines					    ; Load support routines
  	ImageRGB_eventcb						; Load event callback routines

  case !version.os_family of
	'MacOS': begin
		symbol = 'SYMBOL*12'
		large_font = 'Arial*12'
;@2		widget_control, default_font='Geneva*10'		; set font for all windows
		draw_trim = 15
		scr_trim = 21
		help1_xsize = 88
		button_height = 21
		element_xsize = 73
		element_xsize2 = 69
		end
	'unix': begin
		symbol = '-adobe-symbol-medium-r-normal--0-0-100-100-p-0-adobe-fontspecific'
		large_font = '10x20'
;@2		widget_control, default_font='6x13'				; set font for all windows
		draw_trim = 0
		scr_trim = 15
		help1_xsize = 43
		button_height = 29
		element_xsize = 73
		element_xsize2 = 69
		end
	else: begin
		symbol = 'SYMBOL*BOLD*14'
		large_font = 'COURIER*BOLD*10'
	;	widget_control, default_font='Arial*14'				; set font for all windows
		draw_trim = 0
		scr_trim = 15
		help1_xsize = 280			; 
		button_height = 21
		element_xsize = 63			; 
		element_xsize2 = 63			; 
		end
  endcase

  pimages = bad_pars_struct( pimages, make_pars=no_images)
  if no_images then begin
;	warning,'ImageRGB',['There are no images to display.','Load images first.']
;	return
  endif
  xsize = 256
  ysize = 256
  if ptr_valid(pimages) then begin
  	if n_elements( *pimages) gt 0 then begin
	  xsize = (*pimages).xsize
	  ysize = (*pimages).ysize
	endif
  endif

  ImageRGB_TLB = Widget_Base( GROUP_LEADER=wGroup,  $
      UNAME='ImageRGB_TLB' ,/TLB_KILL_REQUEST_EVENTS  $
      ,/TLB_SIZE_EVENTS ,TITLE='Image RGB' ,SPACE=2 ,XPAD=1 ,YPAD=1  $
      ,COLUMN=1 ,MBAR=ImageRGB_TLB_MBAR, _EXTRA=_VWBExtra_)

  if !version.os_family eq 'unix' then begin
	retain=2
  endif else begin
	retain=1
  endelse


  ImageRGB_Draw_Base = Widget_Base(ImageRGB_TLB, UNAME='ImageRGB_Draw_Base' ,SPACE=0  $
      ,XPAD=0 ,YPAD=0 ,ROW=1)

  ImageRGB_Draw = Widget_Draw(ImageRGB_Draw_Base, UNAME='ImageRGB_Draw'  $
      ,SCR_XSIZE=(xsize<600)+scr_trim ,SCR_YSIZE=(ysize<600)+scr_trim  $
      ,NOTIFY_REALIZE='OnRealize_ImageRGB'  $
      ,KILL_NOTIFY='OnDestroy_ImageRGB' ,/SCROLL ,XSIZE=(xsize<1000)+draw_trim  $
      ,YSIZE=(ysize<1000)+draw_trim ,RETAIN=retain ,/BUTTON_EVENTS,/VIEWPORT_EVENTS)


  ImageRGB_Button_Base = Widget_Base(ImageRGB_TLB, UNAME='ImageRGB_Button_Base'  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,column=1, /align_center)


  ImageRGB_Button_Base1 = Widget_Base(ImageRGB_Button_Base, UNAME='ImageRGB_Button_Base1'  $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,/row, /align_center)


  ImageRGB_Button_Base2 = Widget_Base(ImageRGB_Button_Base1, UNAME='ImageRGB_Button_Base2'  $
      ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/column, /align_center)


  ImageRGB_Button_Top_Base = Widget_Base(ImageRGB_Button_Base2, UNAME='ImageRGB_Button_Top_Base'  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_center, /base_align_center)


  ImageRGB_Button_Bot_Base = Widget_Base(ImageRGB_Button_Base2, UNAME='ImageRGB_Button_Bot_Base'  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,ROW=1, /align_left, /base_align_center)


  ImageRGB_Help1_Base = Widget_Base(ImageRGB_Button_Base, UNAME='ImageRGB_Help1_Base', map=1  $
      ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/row, /align_center)


  ImageRGB_Help2_Base = Widget_Base(ImageRGB_Button_Base1, UNAME='ImageRGB_Help2_Base', map=0  $
      ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/row, /align_center)


  PostCreate_ImageRGB_Base, ImageRGB_Draw_Base, ImageRGB_Help1_Base, $
		ImageRGB_Help2_Base, path=path, parent=wGroup, $
		pimages=pimages, _EXTRA=_VWBExtra_

  PostCreate_ImageRGB, ImageRGB_Draw, _EXTRA=_VWBExtra_


  Element_combobox1 = Widget_combobox(ImageRGB_Button_Top_Base,  $
      UNAME='Element_combobox1' ,NOTIFY_REALIZE='OnRealize_Element1',  $
      VALUE=[ '  '], tracking_events=1, scr_xsize=element_xsize, $
      uvalue='Droplist to select the element for ImageRGB to display in RED.')

  Element_combobox2 = Widget_combobox(ImageRGB_Button_Top_Base,  $
      UNAME='Element_combobox2' ,NOTIFY_REALIZE='OnRealize_Element2',  $
      VALUE=[ '  '], tracking_events=1, scr_xsize=element_xsize, $
      uvalue='Droplist to select the element for ImageRGB to display in GREEN.')

  Element_combobox3 = Widget_combobox(ImageRGB_Button_Top_Base,  $
      UNAME='Element_combobox3' ,NOTIFY_REALIZE='OnRealize_Element3',  $
      VALUE=[ '  '], tracking_events=1, scr_xsize=element_xsize, $
      uvalue='Droplist to select the element for ImageRGB to display in BLUE.')

  space1 = widget_base(ImageRGB_Button_Top_Base, xsize=2)

  Full_Button = Widget_Button(ImageRGB_Button_Top_Base, UNAME='Full_Button'  $
      ,/ALIGN_CENTER ,VALUE='o', tracking_events=1, font=large_font, $
      uvalue='Show original size of ImageRGB in window.',xsize=20,ysize=button_height)


  Zoom_In_Button = Widget_Button(ImageRGB_Button_Top_Base, UNAME='Zoom_In_Button'  $
      ,/ALIGN_CENTER ,VALUE='+', tracking_events=1, font=large_font, $
      uvalue='Magnify ImageRGB view x2.',xsize=20,ysize=button_height)


  Zoom_Out_Button = Widget_Button(ImageRGB_Button_Top_Base, UNAME='Zoom_Out_Button'  $
      ,/ALIGN_CENTER ,VALUE='-', tracking_events=1, font=large_font, $
      uvalue='Demagnify ImageRGB view x2.',xsize=20,ysize=button_height)


  Max_Text1 = Widget_Text(ImageRGB_Button_Bot_Base, UNAME='Max_Text1', /edit, $
      NOTIFY_REALIZE='OnRealize_ImageRGB_Max1',scr_XSIZE=element_xsize2, frame=1, $
      tracking_events=1, uvalue='Display maximum value (1-300) for this element.')

  Max_Text2 = Widget_Text(ImageRGB_Button_Bot_Base, UNAME='Max_Text2', /edit, $
      NOTIFY_REALIZE='OnRealize_ImageRGB_Max2',scr_XSIZE=element_xsize2, frame=1, $
      tracking_events=1, uvalue='Display maximum value (1-300) for this element.')

  Max_Text3 = Widget_Text(ImageRGB_Button_Bot_Base, UNAME='Max_Text3', /edit, $
      NOTIFY_REALIZE='OnRealize_ImageRGB_Max3',scr_XSIZE=element_xsize2, frame=1, $
      tracking_events=1, uvalue='Display maximum value (1-300) for this element.')


  Help_Text1 = Widget_Text(ImageRGB_Help1_Base, UNAME='Help_Text1', /wrap $
      ,NOTIFY_REALIZE='OnRealize_ImageRGB_Help1',scr_XSIZE=help1_xsize-19, frame=0 ,YSIZE=3, $
      tracking_events=1, uvalue='Help window to show help prompts for widgets.')

; Must use 'widget_text' here as 'widget_button' cannot be sized small enough when mapped off in 'map_help' routine.

  query_button1 = Widget_text(ImageRGB_Help1_Base, UNAME='query-button', scr_xsize=15, scr_ysize=20, /frame, /all_events,  $
      /ALIGN_CENTER ,VALUE='?', /tracking_events, uvalue='Jump to the help on this window in the GeoPIXE Users Guide.')

  Help_Text2 = Widget_Text(ImageRGB_Help2_Base, UNAME='Help_Text2', /wrap $
      ,NOTIFY_REALIZE='OnRealize_ImageRGB_Help2',scr_XSIZE=1, frame=0 ,scr_YSIZE=1, ysize=3, $
      tracking_events=1, uvalue='Help window to show help prompts for widgets.')

; Must use 'widget_text' here as 'widget_button' cannot be sized small enough when mapped off in 'map_help' routine.

  query_button2 = Widget_text(ImageRGB_Help2_Base, UNAME='query-button', scr_xsize=1, scr_ysize=20, /frame, /all_events,  $
      /ALIGN_CENTER ,VALUE='?', /tracking_events, uvalue='Jump to the help on this window in the GeoPIXE Users Guide.')


 ; File menus

  W_MENU_0 = Widget_Button(ImageRGB_TLB_MBAR, UNAME='W_MENU_0' ,/MENU  $
      ,VALUE='File')

  W_MENU_1 = Widget_Button(W_MENU_0, UNAME='Compare' ,VALUE='Compare images #2', font=def_font)

  W_MENU_1b = Widget_Button(W_MENU_0, UNAME='Compare3' ,VALUE='Compare images #3', font=def_font)

  if n_elements(use_gif) lt 1 then use_gif=0

  W_MENU_3 = Widget_Button(W_MENU_0, UNAME='Export_Menu' ,VALUE='Export', /menu, /separator, font=def_font)

  W_MENU_3b = Widget_Button(W_MENU_3, UNAME='Simple_Menu' ,VALUE='Simple image', /menu, /separator, font=def_font)

  W_MENU_3d = Widget_Button(W_MENU_3b, UNAME='Save_JPEG_Menu', VALUE='Save as JPEG')
  if use_gif then begin
	  W_MENU_3c = Widget_Button(W_MENU_3b, UNAME='Save_GIF_Menu', VALUE='Save as GIF')
  endif else begin
	  W_MENU_3c = Widget_Button(W_MENU_3b, UNAME='Save_PNG_Menu', VALUE='Save as PNG')
  endelse

  W_MENU_3e = Widget_Button(W_MENU_3, UNAME='Plot_Menu' ,VALUE='Image plot', /menu, font=def_font)

  W_MENU_36 = Widget_Button(W_MENU_3e, UNAME='Save_JPEG_Menu2', VALUE='Current Image as JPEG', font=def_font)

;  W_MENU_35 = Widget_Button(W_MENU_3e, UNAME='Save_PNG_Menu2', VALUE='Current Image as PNG', font=def_font)

  W_MENU_32 = Widget_Button(W_MENU_3e, UNAME='Save_CGM_Menu', VALUE='Currect Image as CGM', /separator, font=def_font)

  W_MENU_34 = Widget_Button(W_MENU_3e, UNAME='Save_WMF_Menu', VALUE='Current Image as WMF', font=def_font)

  W_MENU_4 = Widget_Button(W_MENU_0, UNAME='Print_Menu' ,VALUE='Print' ,/SEPARATOR, font=def_font)

  W_MENU_6 = Widget_Button(W_MENU_0, UNAME='Exit_Menu' ,/SEPARATOR  $
      ,VALUE='Exit')


; Learn menus

  W_MENU_20 = Widget_Button(ImageRGB_TLB_MBAR, UNAME='W_MENU_20'  $
      ,/MENU ,VALUE='Learn')

  W_MENU_21 = Widget_Button(W_MENU_20, UNAME='learn-start', VALUE='Start', font=def_font)

  W_MENU_22 = Widget_Button(W_MENU_20, UNAME='learn-next', VALUE='Next', font=def_font)

  W_MENU_23 = Widget_Button(W_MENU_20, UNAME='learn-save',/SEPARATOR, VALUE='Save', font=def_font)

  W_MENU_24 = Widget_Button(W_MENU_20, UNAME='learn-restore', VALUE='Restore', font=def_font)

  W_MENU_25 = Widget_Button(W_MENU_20, UNAME='learn-execute',/SEPARATOR, VALUE='Execute', font=def_font)


; ImageRGB Processing menus

;  W_MENU_50 = Widget_Button(ImageRGB_TLB_MBAR, UNAME='W_MENU_50'  $
;      ,/MENU ,VALUE='Process')


;  W_MENU_51 = Widget_Button(W_MENU_50, UNAME='Median_Menu-2', VALUE='2' )


; Window menus

;  W_MENU_60 = Widget_Button(ImageRGB_TLB_MBAR, UNAME='W_MENU_60'  $
;      ,/MENU ,VALUE='Window')


;  W_MENU_61 = Widget_Button(W_MENU_60, UNAME='Plot_Results_Menu' ,VALUE='Plot Results')


  Widget_Control, /REALIZE, ImageRGB_TLB

  register_notify, ImageRGB_TLB, $
  				['images-changed', $					; new ImageRGBs loaded
				'image-display', $						; pass on notify on spectra
				'image-analyze-mark', $					; new shape changed
				'batch-save', $							; receive from batch
				'path' $								; new path
		  		], from=wGroup
  register_notify, ImageRGB_TLB, ['wizard-action']		; global notify from a wizard

  XManager, 'ImageRGB', ImageRGB_TLB, /no_block

	child = widget_info( ImageRGB_TLB, /child)
	widget_control, child, get_uvalue=pstate
	
	(*pstate).query1 = query_button1
	(*pstate).query2 = query_button2

  set_RGB_map_help, ImageRGB_TLB

end
;

