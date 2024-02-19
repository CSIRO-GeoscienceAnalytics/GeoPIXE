pro Cal_event, Event

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
       warning,'Cal_event',['IDL run-time error caught.', '', $
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
         OnNotify_Cal, event
         return
         end
       'WIDGET_TRACKING': begin
         OnTracking_Cal, Event
         return
         end
       'WIDGET_KILL_REQUEST': begin
         OnKill_Cal_TLB, Event
         return
         end
       else:
    endcase

  uname = widget_info( event.id, /uname)

  case uname of
    'Element_1_text': begin
       if (event.type ge 0) and (event.type le 2) then begin
               OnInsert_Element_1_text, Event
       endif
       end
    'Line_1_combobox': begin
       OnSelect_Line_1_combobox, Event
       end
    'E_Low_text': begin
       if (event.type ge 0) and (event.type le 2) then begin
         OnInsert_Low_E_text, Event
       endif
       end
    'Element_2_text': begin
       if (event.type ge 0) and (event.type le 2) then begin
         OnInsert_Element_2_text, Event
       endif
       end
    'Line_2_combobox': begin
       OnSelect_Line_2_combobox, Event
       end
    'E_High_text': begin
       if (event.type ge 0) and (event.type le 2) then begin
         OnInsert_High_E_text, Event
       endif
       end
    'A_Text': begin
       if (event.type ge 0) and (event.type le 2) then begin
         OnInsert_Cal_A_text, Event
       endif
       end
    'B_text': begin
       if (event.type ge 0) and (event.type le 2) then begin
         OnInsert_Cal_B_text, Event
       endif
       end
    'Units_text': begin
       if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_CH' )then begin
         OnInsert_Cal_Units_text, Event
       endif
       end
    'Get_button': begin
       if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
         OnButton_Cal_Get, Event
       endif
       end
    'keV_button': begin
       if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
         OnButton_Cal_keV, Event
       endif
       end
    'Get_ADC_combobox': begin
       OnSelect_Get_ADC_combobox, Event
       end
    'Apply_button': begin
       if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
         OnButton_Cal_Apply, Event
       endif
       end
    'Apply_all_button': begin
       if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
         OnButton_Cal_Apply, Event, /all
       endif
       end
    'Apply_RA_button': begin
       if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
         OnButton_Cal_Apply, Event, /RA
       endif
       end
    'Close_button': begin
       if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
         OnButton_Cal_Close, Event
       endif
       end
    else:
  endcase

    widget_control, hourglass=0
return
end

;--------------------------------------------------------------------------------------

pro Cal, GROUP_LEADER=wGroup, TLB=Cal_TLB, xlow=xlow, xhigh=xhigh, path=path, xoffset=xoffset, yoffset=yoffset, _EXTRA=_VWBExtra_

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
       warning,'Cal',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

  if n_elements(wGroup) lt 1 then wGroup=0L
  if n_elements(xlow) lt 1 then xlow=0
  if n_elements(xhigh) lt 1 then xhigh=0
  if n_elements(path) lt 1 then path=''

case !version.os_family of
    'MacOS': begin
       yw = 170
       end
    'unix': begin
       yw = 216
       end
    else: begin
       yw = 167
       end
endcase

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
    xoffset = ((xoff + w - 410) < (screen[0]-34 - 410)) > 0
endif
if n_elements(yoffset) lt 1 then begin
    screen = get_screen_size()
    yoffset = ((yoff - yw) < (screen[1]-28 - 159)) > 0
endif

 Resolve_Routine, 'cal_eventcb'     ; Load event callback routines

  common c_xray_lines_1, xray_lines

  xray_lines = [ ' ', 'Ka1', 'Kb1', 'La1', 'Lb1', 'Lb2', 'Lg1', $
       'Ka1 + Ka1', 'Kb1 + Kb1', 'Ka1 + Kb1', 'La1 + La1', $
       'Lb1 + Lb1', 'Ka1 Si-esc', 'Ka1 Ge-esc', 'Ka1 Ge-esc2', $
       'La1 Si-esc', 'La1 Ge-esc', 'La1 Ge-esc2' ]

  Cal_TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='Cal_TLB'  $
      ,/TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset  $
      ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER ,TITLE='Energy Calibration' ,SPACE=1  $
      ,COLUMN=1)


  Top_base = Widget_Base(Cal_TLB, UNAME='Top_base' ,SPACE=1 ,ROW=1)

  PostCreate_Cal_Top, Top_base, xlow, xhigh, path=path, _EXTRA=_VWBExtra_


  Low_E_base = Widget_Base(Top_base, UNAME='Low_E_base' ,FRAME=1  $
      ,COLUMN=1)


  Line_1_base = Widget_Base(Low_E_base, UNAME='Line_1_base' ,SPACE=3  $
      ,ROW=1)


  Element_1_text = Widget_Text(Line_1_base, UNAME='Element_1_text'  $
      ,NOTIFY_REALIZE='OnRealize_Element_1_text' ,/EDITABLE  $
      ,VALUE=' ' ,XSIZE=5 ,YSIZE=1, /tracking, /all_events, $
      uvalue='Element name for low energy Cal marker. (Enter name and hit <return>.)')


  Line_1_combobox = Widget_combobox(Line_1_base,  $
      UNAME='Line_1_combobox' $
      ,NOTIFY_REALIZE='OnRealize_Line_1_combobox' ,VALUE=xray_lines, /tracking, $
      uvalue='X-ray line mneumonic for low energy Cal marker.' )


  E_Low_text = Widget_Text(Low_E_base, UNAME='E_Low_text' $
      ,NOTIFY_REALIZE='OnRealize_Low_E_text' ,/EDITABLE  $
      ,VALUE=' ' ,XSIZE=10 ,YSIZE=1, /tracking, /all_events, $
      uvalue='X-ray energy for low energy Cal marker.')


  High_E_base = Widget_Base(Top_base, UNAME='High_E_base' ,FRAME=1  $
      ,COLUMN=1)


  Line_2_base = Widget_Base(High_E_base, UNAME='Line_2_base' ,SPACE=3  $
      ,ROW=1)


  Element_2_text = Widget_Text(Line_2_base, UNAME='Element_2_text'  $
      ,NOTIFY_REALIZE='OnRealize_Element_2_text' ,/EDITABLE  $
      ,VALUE=' ' ,XSIZE=5 ,YSIZE=1, /tracking, /all_events, $
      uvalue='Element name for high energy Cal marker. (Enter name and hit <return>.)')


  Line_2_combobox = Widget_combobox(Line_2_base,  $
      UNAME='Line_2_combobox' $
      ,NOTIFY_REALIZE='OnRealize_Line_2_combobox' ,VALUE=xray_lines, /tracking, $
      uvalue='X-ray line mneumonic for high energy Cal marker.' )


  E_High_text = Widget_Text(High_E_base, UNAME='E_High_text'  $
      ,NOTIFY_REALIZE='OnRealize_High_E_text'  $
      ,/EDITABLE ,VALUE=' ' ,XSIZE=10  $
      ,YSIZE=1, /tracking, /all_events, $
      uvalue='X-ray energy for high energy Cal marker.')


  Cal_AB_base = Widget_Base(Top_base, UNAME='Cal_AB_base' ,COLUMN=1)

  Cal_A_base = Widget_Base(Cal_AB_base, UNAME='Cal_A_base' ,ROW=1)

  A_label = Widget_Label(Cal_A_base, UNAME='A_label' ,/ALIGN_LEFT  $
      ,VALUE='A:')


  A_Text = Widget_Text(Cal_A_base, UNAME='A_Text'   $
      ,NOTIFY_REALIZE='OnRealize_Cal_A_text' ,/EDITABLE  $
      ,VALUE='1.0000000' ,XSIZE=15 ,YSIZE=1, /tracking, /all_events, $
      uvalue='A (gain) calibration constant.')


  Cal_B_base = Widget_Base(Cal_AB_base, UNAME='Cal_B_base' ,ROW=1)

  B_label = Widget_Label(Cal_B_base, UNAME='B_label' ,/ALIGN_LEFT  $
      ,VALUE='B:')


  B_text = Widget_Text(Cal_B_base, UNAME='B_text' $
      ,NOTIFY_REALIZE='OnRealize_Cal_B_text' ,/EDITABLE  $
      ,VALUE='0.0000000',XSIZE=15 ,YSIZE=1, /tracking, /all_events, $
      uvalue='B (offset) calibration constant.')


  Button_base = Widget_Base(Cal_TLB, UNAME='Button_base'  $
      ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER ,SPACE=1 ,ROW=1 )


  Units_label = Widget_Label(Button_base, UNAME='Units_label', VALUE='Units:')


  Units_text = Widget_Text(Button_base, UNAME='Units_text'  $
      ,NOTIFY_REALIZE='OnRealize_Cal_Units_text' ,/EDITABLE  $
      ,VALUE='channels' ,XSIZE=7 ,YSIZE=1, /tracking, $
      uvalue='Energy units for calibration (eg. "keV"). (Enter name and hit <return>.)')

  keV_button = Widget_Button(Button_base, UNAME='keV_button'  $
      ,/ALIGN_CENTER ,VALUE='keV', /tracking, $
      uvalue='Set the calibration units text to "keV".')


  Spacer = Widget_Label(Button_base, VALUE=' ', xsize=5)


  Get_button = Widget_Button(Button_base, UNAME='Get_button'  $
      ,/ALIGN_CENTER ,VALUE='Get', /tracking, $
      uvalue='Get the calibration A,B parameters from a spectrum SPEC file.')

 	obj = obj_new('MAIA_DEVICE')
   svals = ['any',strtrim( adc_list_device(obj),2)]

  Get_ADC_combobox = Widget_combobox(button_base,  $
      UNAME='Get_ADC_combobox' $
      ,NOTIFY_REALIZE='OnRealize_Get_ADC_combobox' ,VALUE=svals, /tracking, $
      uvalue='Use this ADC # in a "Get" to extract the correct energy calibration A,B.' )


  Spacer = Widget_Label(Button_base, VALUE=' ', xsize=5)


  lab = Widget_Label(Button_base, VALUE='Apply:')

  Apply_button = Widget_Button(Button_base, UNAME='Apply_button'  $
      ,/ALIGN_CENTER ,VALUE='One', /tracking, $
      uvalue='Set the energy calibration for the currently displayed spectrum.')

  Apply_all_button = Widget_Button(Button_base, UNAME='Apply_all_button'  $
      ,/ALIGN_CENTER ,VALUE='All', /tracking, $
      uvalue='Set the energy calibration for ALL loaded spectra.')

  Apply_RA_button = Widget_Button(Button_base, UNAME='Apply_RA_button'  $
      ,/ALIGN_CENTER ,VALUE='RA', /tracking, $
      uvalue='Set all calibrations to "Re-Assign" peaks to new energies.')


  Spacer = Widget_Label(Button_base, VALUE=' ', xsize=5)

  Close_button = Widget_Button(Button_base, UNAME='Close_button'  $
      ,/ALIGN_CENTER ,VALUE='Close', /tracking, $
      uvalue='Close the window.')

  help = widget_text( Cal_TLB, xsize=65, ysize=1, /wrap, uname='HELP', /tracking, $
          uvalue='Help window. Displays info about widgets.',frame=0, $
          notify_realize='OnRealize_Cal_Help')

  Widget_Control, /REALIZE, Cal_TLB

  register_notify, Cal_TLB, ['cal-x', $        ; new cal X markers
          'path'], $   ; new path
          from=wGroup

  XManager, 'Cal', Cal_TLB  ,CLEANUP='OnDestroy_Cal_TLB', /no_block

  if obj_valid(obj) then obj_destroy, obj
end
