;
; spectrum_history    display history information for selected element image
;
pro spectrum_history_event, Event

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
		warning,'spectrum_history_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	wWidget =  Event.top
	widget_control, hourglass=0
  
	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate	
	if ptr_good(pstate,/struct) eq 0 then goto, bad_state
  
	case tag_names( event,/structure) of
		'NOTIFY': begin
			OnNotify_spectrum_history, event
			return
			end
		'WIDGET_TRACKING': begin
			OnTracking_spectrum_history, Event
			return
			end
		else:
	endcase

  uname = widget_info( event.id, /uname)

  case uname of

	'spectrum_history_TLB': begin
		case Tag_Names(Event, /STRUCTURE_NAME) of
			'WIDGET_KILL_REQUEST': begin
				OnKill_spectrum_history, Event
				end
			'WIDGET_BASE': begin
				OnSize_spectrum_history, Event
				end
			else:
		endcase
		end
	'spectrum_history_List': begin
		p = (*(*pstate).p)[0]
		if ptr_good(p[0],/struct) eq 0 then goto, bad_ptr
		if (event.clicks eq 2) and (event.index lt n_elements(*(*pstate).pindex)) then begin
			index = (*(*pstate).pindex)[event.index]
			case index of
;				45: begin
;					text = [(*p).sample, (*p).grain, (*p).comment]
;					label = ['Sample','Grain','Comment']
;					string_edit, event.top, title='Edit Image Property Strings',label=label, text=text, error=err
;					if err eq 0 then begin
;						(*p).sample = text[0]
;						(*p).grain = text[1]
;						(*p).comment = text[2]
;					endif
;					end
;				70: begin
;					text = str_tidy([(*p).scan.x, (*p).scan.y])
;					label = ['Scan X size (mm)','Scan Y size (mm)']
;					string_edit, event.top, title='Edit Image Properties',label=label, text=text, /numeric, error=err
;					if err eq 0 then begin
;						(*p).scan.x = float2(text[0])
;						(*p).scan.y = float2(text[1])
;					endif
;					end
;				90: begin
;					text = str_tidy([(*p).scan.origin.x, (*p).scan.origin.y])
;					label = ['Scan X origin (mm)','Scan Y origin (mm)']
;					string_edit, event.top, title='Edit Image Properties',label=label, text=text, /numeric, error=err
;					if err eq 0 then begin
;						(*p).scan.origin.x = float2(text[0])
;						(*p).scan.origin.y = float2(text[1])
;					endif
;					end
;				170: begin
;					image_flux_charge, p, xanes=xanes, charge=charge, conv=conv, flux=flux, error=error
;					if error eq 0 then begin
;						charge_select, event.top, title='Flux to Charge conversion', conv=conv, charge=charge, flux=flux, error=error
;						if error eq 0 then begin
;							(*p).charge = charge
;							(*p).IC.conversion = conv
;							(*p).temp.valid = 0
;						endif
;					endif
;					end
				else: begin
					if index gt 1000 then begin
						warning,'spectrum_history_event',['Change the selected spectrum property in the "Spectrum Select" table.']
					endif else begin
						warning,'spectrum_history_event',['Change the selected spectrum property in the "Spectrum Select" table.']
					endelse
					end
			endcase
			Update_spectrum_history, pstate
		endif
		end
	else:
  endcase
  return
  
bad_state:
	warning,'spectrum_history_event',['STATE variable has become ill-defined.','Abort window.'],/error
	goto, kill
bad_ptr:
	warning,'spectrum_history_event',['Parameter structure variable has become ill-defined.','Abort window.'],/error
	goto, kill

kill:
	widget_control, hourglass=0
	OnKill_spectrum_history, Event
end

;-------------------------------------------------------------------------------------------------------

pro spectrum_history, GROUP_LEADER=wGroup, TLB=spectrum_history_TLB, xoffset=xoffset, $
		yoffset=yoffset, pspectra=pspectra, show=show, _EXTRA=_VWBExtra_

 spectrum_history_eventcb     ; Load event callback routines

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
		warning,'spectrum_history',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(wGroup) lt 1 then wGroup=0L
;if n_elements(pspectra) lt 1 then pspectra=ptr_new()
if ptr_good(pspectra) eq 0 then return
if ptr_good( (*pspectra)[0],/struct) eq 0 then return
if n_elements(show) lt 1 then show=0

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
	xoffset = ((xoff + w) < (screen[0]-34 - 192)) > 0
endif
if n_elements(yoffset) lt 1 then begin
	screen = get_screen_size()
	yoffset = ((yoff + h-425) < (screen[1]-28 - 236)) > 0
endif

  spectrum_history_TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='spectrum_history_TLB'  $
       ,/TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset  $
      ,/TLB_SIZE_EVENTS ,TITLE='Spectrum History' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3 ,COLUMN=1 ,/BASE_ALIGN_CENTER, _EXTRA=_VWBExtra_)

  spectrum_history_List = Widget_List(spectrum_history_TLB, UNAME='spectrum_history_List',  $
		value = List, NOTIFY_REALIZE='OnRealize_spectrum_history', $
		scr_xsize=400 ,scr_ysize=350)

  Widget_Control, /REALIZE, spectrum_history_TLB

child = widget_info( spectrum_history_TLB, /child)
widget_control, child, get_uvalue=pstate

(*pstate).plist = ptr_new(list)
(*pstate).pindex = ptr_new([0])
if ptr_valid(pspectra) then (*pstate).p = pspectra
if n_elements(show) gt 0 then (*pstate).show = show
Update_spectrum_history, pstate

  register_notify, spectrum_history_TLB, $
  				['spectra-changed', $		; new spectra loaded (Spectrum_Display, Image_Table, EVT)
				'select-update'], $			; changed selected spectra (detector)
  				from=wGroup

  XManager, 'spectrum_history', spectrum_history_TLB ,/no_block

end
