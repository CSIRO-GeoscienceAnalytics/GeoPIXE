;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	06/30/99 14:45.19
;
pro Spectrum_Select_event, Event

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
		warning,'Spectrum_select_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

  wWidget =  Event.top
  widget_control, hourglass=0
  ;help, event,/structure

	case tag_names( event,/structure) of
		'NOTIFY': begin
			OnNotify_Select, event
			return
			end
		'WIDGET_TRACKING': begin
	        OnTracking_Select, Event
			return
			end
		else:
	endcase

  uname = widget_info( event.id, /uname)

  case uname of

	'Select_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_KILL_REQUEST': begin
				OnKill_Select, Event
				end
			'WIDGET_BASE': begin
				OnSize_Select, Event
				end
			else:
		endcase
		end
	'Select_Table': begin
;		help,event,/struct
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_TABLE_CH': begin
				OnChangeValue_Select, Event
				end
			'WIDGET_TABLE_CELL_SEL': begin
				OnCellSelect_Select, Event
				end
			else:
		endcase
		end
	'Prev_Button': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
			OnButton_Select_Prev, Event
		endif
		end
	'Next_Button': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
			OnButton_Select_Next, Event
		endif
		end
	'Array_Button': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
			OnButton_Select_Sel, Event
		endif
		end
	'Display_Button': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
			OnButton_Display, Event
		endif
		end
	'R_Button': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
			OnButton_Display_Random, Event
		endif
		end
	'All_Button': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
			OnButton_Display_All, Event
		endif
		end
	'One_Button': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
			OnButton_Display_One, Event
		endif
		end
	'ShowFit_Button': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
			OnButton_ShowFit, Event
		endif
		end
	'Close_Button': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
			OnButton_Close_Select, Event
		endif
		end
	'Highlight_Mode_Combobox': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_COMBOBOX' )then begin
			OnSelect_Highlight_Select, Event
		endif
		end

	'Delete_Spec_Button': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
			OnButton_Delete_by_mode_Select, Event
		endif
		end
	'Delete_Mode_Combobox': begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_COMBOBOX' )then begin
			OnSelect_Delete_Mode_Select, Event
		endif
		end
		
;	'No_XY_Button': begin
;		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
;			OnButton_NoXY_Spec_Select, Event
;		endif
;		end
;	'No_Odd_Button': begin
;		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
;			OnButton_NoOdd_Spec_Select, Event
;		endif
;		end
;	'No_E_Button': begin
;		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
;			OnButton_NoE_Spec_Select, Event
;		endif
;		end
;	'No_T_Button': begin
;		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
;			OnButton_NoT_Spec_Select, Event
;		endif
;		end
;	'No_Q_Button': begin
;		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
;			OnButton_NoQ_Spec_Select, Event
;		endif
;		end
;	'Delete_Fit_Button': begin
;		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
;			OnButton_Delete_Fit_Select, Event
;		endif
;		end
	else:
  endcase

	widget_control, hourglass=0
return
end

;--------------------------------------------------------------------------------------

pro Spectrum_Select, GROUP_LEADER=wGroup, TLB=Select_TLB, spectrum=p, xoffset=xoffset, yoffset=yoffset, $
		realtime=realtime, show=pshow, update_notify=update_notify, _EXTRA=_VWBExtra_

spectrum_routines
spectrum_select_eventcb				    ; Load event callback routines

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
		warning,'Spectrum_select',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	n = 6
  if ptr_good(p) eq 0 then goto, bad_skip
  if ptr_valid(p) eq 0 then goto, bad_ptr
  if n lt 1 then goto, bad_ptr
  if ptr_valid((*p)[0]) eq 0 then goto, bad_ptr
	n = n_elements(*p)

bad_skip:
  if n_elements(wGroup) lt 1 then wGroup = 0L
  if n_elements(realtime) lt 1 then realtime = 0
  if n_elements(update_notify) eq 0 then update_notify='spectrum-display'
  if n_elements(pshow) lt 1 then pshow=ptr_new(replicate(1,n))

case !version.os_family of
	'MacOS': begin
		fnt = 'COURIER*BOLD*10'
		xsize_mode = 70
		xsize_display = 50
		xsize_fit = 52
		xsize_all = 26
		xsize_one = 28
		xsize_R = 18
		xsize_prev = 34
		xsize_next = 34
		xsize_sel = 59
		xsize_del_mode = 140
		xsize_delspec = 50
		xsize_delfit1 = 46
		xsize_delfit2 = 84
		xsize_noxy = 40
		xsize_not = 34
		xs = 5
		yw = 73
		xw = 496
		end
	'unix': begin
		fnt = '6x10'
		xsize_mode = 70
		xsize_display = 50
		xsize_fit = 52
		xsize_all = 26
		xsize_one = 28
		xsize_R = 18
		xsize_prev = 34
		xsize_next = 34
		xsize_sel = 59
		xsize_del_mode = 140
		xsize_delspec = 50
		xsize_delfit1 = 46
		xsize_delfit2 = 84
		xsize_noxy = 40
		xsize_not = 34
		xs = 5
		yw = 73
		xw = 496
		end
	else: begin
		fnt = 'COURIER*10'
		xsize_mode = 67
		xsize_display = 44
		xsize_fit = 47
		xsize_all = 21
		xsize_one = 29
		xsize_R = 18
		xsize_prev = 31
		xsize_next = 31
		xsize_sel = 46
		xsize_del_mode = 120
		xsize_delspec = 45
		xsize_delfit1 = 38
		xsize_delfit2 = 84
		xsize_noxy = 40
		xsize_not = 32
		xs = 0
		yw = 45
		xw = 496
		end
endcase

	height = 108+(n<6)*18
	w = 0
	h = 0
	xoff = 0
	yoff = 0
	xo = 0
	yo = 0
	screen = get_screen_size()
	if widget_info( wGroup, /valid) then begin
		geom = widget_info( wGroup, /geometry)
		w = geom.scr_xsize + xs
		h = geom.scr_ysize
		xoff = geom.xoffset
		yoff = geom.yoffset
		if yoff + h + height lt (screen[1]-28) then begin
			xo = xoff < xo
			yo = yoff + h
		endif else begin
			xo = ((xoff + w) < (screen[0]-34 - xw)) > 0
			yo = (screen[1]-yw - height) > 0
		endelse
	endif
	if n_elements(xoffset) lt 1 then begin
		xoffset = xo
	endif
	if n_elements(yoffset) lt 1 then begin
		yoffset = yo
	endif

  Select_TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='Select_TLB'  $
      ,KILL_NOTIFY='OnDestroy_Select' ,/TLB_KILL_REQUEST_EVENTS  $
      ,/TLB_SIZE_EVENTS ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER, xoffset=xoffset, yoffset=yoffset  $
      ,TITLE='Spectrum Select' ,SPACE=2 ,XPAD=2 ,YPAD=2 ,COLUMN=1)


	s = { label:'', sample:'', grain:'', comment:'', charge:0.0, $
		size:0, cal_a:0.0, cal_b:0.0, units:'', station:0, $
		x:0.0, y:0.0, z:0.0, $
		theta:0.0, phi:0.0, scanx:0.0, scany:0.0, filter:0, seq:'', $
		elow:0.0, ehigh:0.0, ylow:0.0, yhigh:0.0, ylog:0 }
 	t = replicate( s, 100)

  Select_Table = Widget_Table(Select_TLB, UNAME='Select_Table'  $
      ,NOTIFY_REALIZE='OnRealize_Select_Table' ,/EDITABLE ,/all_events $
      ,Y_SCROLL_SIZE=6 ,value=t ,/row_major	$	;	, font=fnt $
      ,/RESIZEABLE_COLUMNS, alignment=2, scr_xsize=580 )

  PostCreate_Select_Table, Select_Table, spectrum=p, show=pshow, $
  		update_notify=update_notify, realtime=realtime, _EXTRA=_VWBExtra_


  Select_Button_Base = Widget_Base(Select_TLB,  $
      UNAME='Select_Button_Base' ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER  $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,ROW=1)


  Mode_combobox = Widget_combobox( Select_Button_Base,  $
      UNAME='Highlight_Mode_Combobox' ,NOTIFY_REALIZE='OnRealize_Select_Highlight',  $
      VALUE=[ 'Colour','Highlight'], xsize=xsize_mode)


  Spacer_1 = Widget_Base(Select_Button_Base, xsize=3)


  Display_Button = Widget_Button(Select_Button_Base, xsize=xsize_display,  $
      UNAME='Display_Button' ,/ALIGN_CENTER ,VALUE='Display')

  All_Button = Widget_Button(Select_Button_Base, xsize=xsize_all,  $
      UNAME='All_Button' ,/ALIGN_CENTER ,VALUE='All')

  One_Button = Widget_Button(Select_Button_Base, xsize=xsize_one,  $
      UNAME='One_Button' ,/ALIGN_CENTER ,VALUE='One')

  if realtime then begin
  	Random_Button = Widget_Button(Select_Button_Base, xsize=xsize_R,  $
		UNAME='R_Button' ,/ALIGN_CENTER ,VALUE='R')
  endif

  ShowFit_Button = Widget_Button(Select_Button_Base, xsize=xsize_fit,  $
      UNAME='ShowFit_Button' ,/ALIGN_CENTER ,VALUE='Overlay')


  Spacer_2 = Widget_Base(Select_Button_Base, xsize=3)


  Prev_Button = Widget_Button(Select_Button_Base, xsize=xsize_prev, UNAME='Prev_Button'  $
      ,/ALIGN_CENTER ,VALUE='Prev')


  Next_Button = Widget_Button(Select_Button_Base, xsize=xsize_next, UNAME='Next_Button'  $
      ,/ALIGN_CENTER ,VALUE='Next')


  Spacer_3 = Widget_Base(Select_Button_Base, xsize=3)

  array_Button = Widget_Button(Select_Button_Base, xsize=xsize_sel, UNAME='Array_Button'  $
      ,/ALIGN_CENTER ,VALUE='Array...')


  Spacer_3 = Widget_Base(Select_Button_Base, xsize=3)

;del_fit_name = 'Delete Overlay'
;xsize_delfit = xsize_delfit2

if realtime eq 0 then begin

  Delete_Spec_Button = Widget_Button(Select_Button_Base, UNAME='Delete_Spec_Button'  $
      ,/ALIGN_CENTER ,VALUE='Delete:', xsize=xsize_delspec)


  Delete_combobox = Widget_combobox( Select_Button_Base,  $
      UNAME='Delete_Mode_Combobox' ,  $
      VALUE=[ 'all XY and T','all XY spectra','all T spectra','all E spectra','all Odd detectors', $
      			'all Even detectors','Selected range','Displayed spectra','Fit overlays','Charge values', $
      			'Not selected range','Not displayed spectra','No valid calibration','Zero spectra'], xsize=xsize_del_mode)


  Spacer_3 = Widget_Base(Select_Button_Base, xsize=3)


;  NoXY_Button = Widget_Button(Select_Button_Base, UNAME='No_XY_Button'  $
;      ,/ALIGN_CENTER ,VALUE='No XY', xsize=xsize_noxy)
;
;
;  NoE_Button = Widget_Button(Select_Button_Base, UNAME='No_E_Button'  $
;      ,/ALIGN_CENTER ,VALUE='No E', xsize=xsize_not)
;
;
;  NoT_Button = Widget_Button(Select_Button_Base, UNAME='No_T_Button'  $
;      ,/ALIGN_CENTER ,VALUE='No T', xsize=xsize_not)
;
;
;  NoOdd_Button = Widget_Button(Select_Button_Base, UNAME='No_Odd_Button'  $
;      ,/ALIGN_CENTER ,VALUE='No Odd', xsize=xsize_noxy+5)
;
;
;  NoQ_Button = Widget_Button(Select_Button_Base, UNAME='No_Q_Button'  $
;      ,/ALIGN_CENTER ,VALUE='No Q', xsize=xsize_not)
;
;
;  del_fit_name = 'Del Fit'
;  xsize_delfit = xsize_delfit1
endif

;  Delete_Fit_Button = Widget_Button(Select_Button_Base, UNAME='Delete_Fit_Button'  $
;      ,/ALIGN_CENTER ,VALUE=del_fit_name, xsize=xsize_delfit)

;  Close_Button = Widget_Button(Select_Button_Base,  $
;      UNAME='Close_Button' ,/ALIGN_CENTER ,VALUE='Close')


  Widget_Control, /REALIZE, Select_TLB

  register_notify, Select_TLB, $
  				['spectra-changed', $		; new spectra loaded (Spectrum_Display, Image_Table, EVT)
				'select-update', $			; changed selected spectra (detector)
				'select-highlight', $		; point to spectrum to select one with mouse
				'array-select', $			; detector channel selection from 'detector_select'
				'image-region-delete', $	; to delete a selected spectrum (region) row
				'image-region-select' $		; region selected (Image_Table)
				], from=wGroup

  register_notify, Select_TLB, ['wizard-action']		; global notify from a wizard

  XManager, 'Spectrum_Select', Select_TLB  ,CLEANUP='OnDestroy_Select'  ,/no_block

	return

bad_ptr:
	warning,'spectrum_select','Bad input pointer or invalid spectrum data.',/error
end
