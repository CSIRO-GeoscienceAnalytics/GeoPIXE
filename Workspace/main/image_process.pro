;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on: 01/17/100 13:42.18
;
pro Image_Process_event, Event

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
       warning,'Image_process_event',['IDL run-time error caught.', '', $
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
         OnNotify_Image_Process, event
         return
         end
       'WIDGET_TRACKING': begin
         OnTracking_Image_Process, Event
         return
         end
       else:
    endcase

  uname = widget_info( event.id, /uname)

  case uname of

    'Image_Process_TLB': begin
       case Tag_Names(Event, /STRUCTURE_NAME) of
         'WIDGET_KILL_REQUEST': begin
          OnKill_Image_Process, Event
          end
         'WIDGET_BASE': begin
          OnSize_Image_Process, Event
          end
         else:
       endcase
       end

    'Image_Process_List': begin
       OnSelect_Image_Process, Event
       end

    'Image_Process_Execute': begin
       OnButton_Image_Process_Execute, Event
       end

    'Image_Process_Undo': begin
       OnButton_Image_Process_Undo, Event
       end

    'Image_Process_Get': begin
       OnButton_Image_Process_Get, Event
       end

    'Image_Process_Close': begin
       OnButton_Image_Process_Close, Event
       end
    else:
  endcase

    widget_control, hourglass=0
end

;-------------------------------------------------------------------------------------------------------

pro Image_Process, GROUP_LEADER=wGroup, TLB=Image_Process_TLB, plugins=plugins, $
          xoffset=xoffset, yoffset=yoffset, path=path, get_file=get_file, $
          pimage=pimage, return_list=return_list, _EXTRA=_VWBExtra_

image_process_eventcb     ; Load event callback routines

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
       warning,'Image_process',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

if n_elements(wGroup) lt 1 then wGroup=0L
if n_elements(path) lt 1 then path = ptr_new(/allocate_heap)
if n_elements(get_file) lt 1 then get_file = ptr_new(/allocate_heap)
if n_elements(pimage) lt 1 then pimage = ptr_new()
tuv = {path:path, get_file:get_file, pimage:pimage}

; Note: This list must contain all variants of processing commands used (here and in the main Image
; window) if the "Get" button is to find commands used to apply them elsewhere.

	List=[  $
  		'Gaussian smooth 1.0', $          ; 0
		'Gaussian smooth 1.5', $          ; 1
		'Gaussian smooth 2', $            ; 2
		'Gaussian smooth 3', $            ; 3
		'Gaussian smooth 5', $            ; 4
		'Gaussian smooth 10', $            ; 50
		'Boxcar smooth 2', $              ; 5
		'Boxcar smooth 3', $              ; 6
		'Boxcar smooth 5', $              ; 7
		'Boxcar smooth 10', $             ; 8
		'Median filter 2', $              ; 9
		'Median filter 3', $              ; 10
		'Median filter 5', $              ; 11
		'Median filter 10', $              ; 39
		'Roberts edge enhance', $          ; 12
		'Sobel edge enhance', $             ; 13
		'Erode 2', $                 ; 14
		'Erode 5', $                 ; 15
		'Erode 10', $                 ; 51
		'Dilate 2', $                  ; 16
		'Dilate 5', $                  ; 17
		'Dilate 10', $                  ; 52
		'Clear border 1', $               ; 18
		'Clear border 2', $               ; 19
		'Clear border 5', $               ; 20
		'Clear border 10', $                 ; 21
		'Clear sides 1', $               ; 46
		'Clear sides 2', $               ; 47
		'Clear sides 5', $               ; 48
		'Clear sides 10', $               ; 49
		'* Correct zero pixels', $		; 40
		'Correct Y ripples', $			; 41

;   	  'Kill region', $               ; 22
     '* Shift even rows +0.5', $             ; 42
     '* Shift even rows -0.5', $             ; 43
     '* Shift odd rows +0.5', $             ; 44
     '* Shift odd rows +1', $             ; 23
     '* Shift odd rows +2', $             ; 24
     '* Shift odd rows +5', $             ; 25
     '* Shift odd rows -0.5', $             ; 45
     '* Shift odd rows -1', $             ; 26
     '* Shift odd rows -2', $             ; 27
     '* Shift odd rows -5', $             ; 28
     '* Shift all rows +1', $             ; 29
     '* Shift all rows +2', $             ; 30
     '* Shift all rows +5', $             ; 31
     '* Shift all rows -1', $             ; 32
     '* Shift all rows -2', $             ; 33
     '* Shift all rows -5', $             ; 34
     '* Shift odd columns +1', $             ; 53
     '* Shift odd columns -1' $             ; 54
;     'Repair some missing rows', $         ; 35
;     'Repair a double counted row' $       ; 36
;     'Repair some missing columns', $     ; 37
;     'Repair a double counted column' $     ; 38
      ]

  arg=[  $
     1.0, $                      ; 0
     1.5, $                      ; 1
     2.0, $                      ; 2
     3.0, $                      ; 3
     5.0, $                      ; 4
     10.0, $                      ; 50
     2.0, $                      ; 5
     3.0, $                      ; 6
     5.0, $                      ; 7
     10.0, $                     ; 8
     2.0, $                      ; 9
     3.0, $                      ; 10
     5.0, $                      ; 11
     10.0, $                      ; 39
       0.0, $                        ; 12
       0.0, $                        ; 13
       2.0, $                        ; 14
       5.0, $                        ; 15
       10.0, $                        ; 51
       2.0, $                        ; 16
       5.0, $                        ; 17
       10.0, $                        ; 52
       1.0, $                        ; 18
       2.0, $                        ; 19
       5.0, $                        ; 20
       10.0, $                     ; 21
       1.0, $                     ; 46
       2.0, $                     ; 47
       5.0, $                     ; 48
       10.0, $                     ; 49
	   0.0, $						; 40
	   0.0, $							; 41

;     0.0, $                       ; 22
     0.5, $                       ; 42
     -0.5, $                       ; 43
     0.5, $                       ; 44
     1.0, $                       ; 23
     2.0, $                       ; 24
     5.0, $                       ; 25
     -0.5, $                       ; 45
     -1.0, $                      ; 26
     -2.0, $                      ; 27
     -5.0, $                      ; 28
     1.0, $                       ; 29
     2.0, $                       ; 30
     5.0, $                       ; 31
     -1.0, $                      ; 32
     -2.0, $                      ; 33
     -5.0, $                      ; 34
     1.0, $                      ; 53
     -1.0 $                      ; 54
;     0, $                     ; 35
;     1 $                       ; 36
;     2, $                     ; 37
;     3 $                       ; 38
      ]

  routine = [ $
     'Image_Process_Gaussian', $     ; 0
     'Image_Process_Gaussian', $     ; 1
     'Image_Process_Gaussian', $     ; 2
     'Image_Process_Gaussian', $     ; 3
     'Image_Process_Gaussian', $     ; 4
     'Image_Process_Gaussian', $     ; 50
     'Image_Process_Boxcar', $     ; 5
     'Image_Process_Boxcar', $     ; 6
     'Image_Process_Boxcar', $     ; 7
     'Image_Process_Boxcar', $     ; 8
     'Image_Process_Median', $     ; 9
     'Image_Process_Median', $     ; 10
     'Image_Process_Median', $     ; 11
     'Image_Process_Median', $     ; 39
     'Image_Process_Roberts', $   ; 12
     'Image_Process_Sobel', $       ; 13
     'Image_Process_Erode', $       ; 14
     'Image_Process_Erode', $       ; 15
     'Image_Process_Erode', $       ; 51
     'Image_Process_Dilate', $     ; 16
     'Image_Process_Dilate', $     ; 17
     'Image_Process_Dilate', $     ; 52
     'Image_Process_Clear', $       ; 18
     'Image_Process_Clear', $       ; 19
     'Image_Process_Clear', $       ; 20
     'Image_Process_Clear', $       ; 21
     'Image_Process_Clear_sides', $       ; 46
     'Image_Process_Clear_sides', $       ; 47
     'Image_Process_Clear_sides', $       ; 48
     'Image_Process_Clear_sides', $       ; 49
	 'Image_Process_Zeroes', $		; 40
	 'Image_Process_correct_Y_ripples', $	; 41

;       'Image_Process_Suppress_Region', $ ; 22
       'Image_Process_Shift_even_rows', $     ; 42
       'Image_Process_Shift_even_rows', $     ; 43
       'Image_Process_Shift_odd_rows', $     ; 44
       'Image_Process_Shift_odd_rows', $     ; 23
       'Image_Process_Shift_odd_rows', $     ; 24
       'Image_Process_Shift_odd_rows', $     ; 25
       'Image_Process_Shift_odd_rows', $     ; 45
       'Image_Process_Shift_odd_rows', $     ; 26
       'Image_Process_Shift_odd_rows', $     ; 27
       'Image_Process_Shift_odd_rows', $     ; 28
       'Image_Process_Shift_all', $     ; 29
       'Image_Process_Shift_all', $     ; 30
       'Image_Process_Shift_all', $     ; 31
       'Image_Process_Shift_all', $     ; 32
       'Image_Process_Shift_all', $     ; 33
       'Image_Process_Shift_all', $     ; 34
       'Image_Process_Shift_odd_columns', $     ; 53
       'Image_Process_Shift_odd_columns' $     ; 54
;       'Image_Process_Missing_Lines', $   ; 35
;       'Image_Process_Missing_Lines' $       ; 36
;       'Image_Process_Missing_Lines', $   ; 37
;       'Image_Process_Missing_Lines' $       ; 38
     ]

;  Add any plugins ...

  if ptr_valid(plugins) then begin
    list = [list,(*plugins).title]
    arg = [arg,replicate(0,n_elements((*plugins).list))]
    routine = [routine,(*plugins).list]
  endif

  uval = {list:list, routine:routine, arg:arg}

	 if arg_present(return_list) then begin
	 	return_list = uval
		return
	endif

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
    xoffset = ((xoff + w/3) < (screen[0]-34 - 192)) > 0
endif
if n_elements(yoffset) lt 1 then begin
    screen = get_screen_size()
    yoffset = ((yoff + h) < (screen[1]-28 - 306)) > 0
endif

  Image_Process_TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='Image_Process_TLB'  $
       ,/TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset  $
      ,/TLB_SIZE_EVENTS ,TITLE='Image Processing' ,SPACE=0 ,XPAD=0  $
      ,YPAD=0 , /COLUMN ,/BASE_ALIGN_CENTER, uvalue=tuv, _EXTRA=_VWBExtra_)

  base1 = widget_base( Image_process_tlb, /column, space=3, xpad=3, ypad=3 ,/BASE_ALIGN_CENTER)

  Image_Process_List = Widget_List(base1, UNAME='Image_Process_List',  $
       value = List, uvalue=uval, NOTIFY_REALIZE='OnRealize_Image_Process', $
       scr_xsize=185 ,scr_ysize=180)
;     XSIZE=30 ,YSIZE=15)

  Button_Base = Widget_Base(base1, UNAME='Button_Base',  $
       /ALIGN_CENTER ,/BASE_ALIGN_CENTER,  $
       SPACE=5, XPAD=0, YPAD=0, /ROW)


  Image_Process_Button = Widget_Button(Button_Base, UNAME='Image_Process_Undo'  $
      ,/ALIGN_CENTER ,VALUE='Undo Last')

  Image_Process_Button = Widget_Button(Button_Base, UNAME='Image_Process_Get'  $
      ,/ALIGN_CENTER ,VALUE='Get')

  Image_Process_Close = Widget_Button(Button_Base, UNAME='Image_Process_Close'  $
       ,/ALIGN_CENTER ,VALUE='Close')


  Widget_Control, /REALIZE, Image_Process_TLB

  register_notify, Image_Process_TLB,  $
              ['images-changed', $          ; new images pointer
              'batch-filter' $           ; batch application of "Get" button
              ], from=wGroup
  register_notify, Image_Process_TLB, ['wizard-action']	; global notify from a wizard

  XManager, 'Image_Process', Image_Process_TLB ,/no_block

end
