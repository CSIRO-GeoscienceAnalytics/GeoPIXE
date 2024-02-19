pro file_select_event, event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case tag_names( event,/structure) of
;	'NOTIFY': begin
;		case event.tag of
;			else: begin
;				goto, finish
;				end
;		endcase
;		end
;	'WIDGET_TRACKING': begin
;		widget_control, event.id, get_uvalue=s
;		if size(s,/tname) eq 'STRING' then begin
;			if event.enter eq 1 then begin
;				widget_control, (*pstate).help, set_value=s
;			endif else begin
;				widget_control, (*pstate).help, set_value=' '
;			endelse
;		endif
;		goto, finish
;		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill Identify ...'
;		cancel_notify, event.top
		widget_control, event.top, /destroy
		return
		end
	else:
endcase

uname = widget_info( event.id, /uname)
case uname of
	'Identify_TLB': begin					; resize
		print,event.x,event.y
		w = (event.x - 12) > 220
		h = (event.y - 125) > 145
		print,w,h
		widget_control, (*pstate).list, scr_xsize=w, scr_ysize=h
		widget_control, (*pstate).help, scr_xsize=w+6
		if (w gt 390) and ((*pstate).tiny eq 1) then begin
			widget_control, (*pstate).ptable, set_value=0
			(*pstate).tiny = 0
			widget_control, (*pstate).list, scr_xsize=382, scr_ysize=180
			widget_control, (*pstate).help, scr_xsize=390
		endif
		if (w le 300) and ((*pstate).tiny eq 0) then begin
			widget_control, (*pstate).ptable, set_value=1
			(*pstate).tiny = 1
			widget_control, (*pstate).list, scr_xsize=220, scr_ysize=145
			widget_control, (*pstate).help, scr_xsize=226
		endif
		end
	'filesel_cw': begin
		help, event, /structure
		if event.done eq 1 then *((*pstate).pfile) = event.value
		if event.done ge 1 then begin
			widget_control, event.top, /destroy
			return
		endif
		end
	else:
endcase

finish:
return
end

;-----------------------------------------------------------
function file_select, group=wGroup, fix_filter=fix_filter, $
	path=path, filter=filter, title=title, file=file, $
	read=read, write=write, _EXTRA=_VWBExtra_

  if n_elements(title) lt 1 then title='Select a file'
  if n_elements(fix_filter) lt 1 then fix_filter=0
  if n_elements(path) lt 1 then path=''
  if n_elements(file) lt 1 then file=''
  if n_elements(filter) lt 1 then filter='All Files'
  if n_elements(multiple) lt 1 then multiple=0
  if n_elements(write) lt 1 then write=0
  modal = 1
  if n_elements(wGroup) lt 1 then wGroup = 0L
  if widget_info( long(wGroup), /valid_id) eq 0 then modal=0

  TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='file_select_TLB',  $
       /TLB_KILL_REQUEST_EVENTS, $
       modal=modal,  $
       /TLB_SIZE_EVENTS ,TITLE=title ,SPACE=3 ,XPAD=3,  $
       YPAD=3 ,COLUMN=1 ,/BASE_ALIGN_CENTER, _EXTRA=_VWBExtra_)

  select = cw_filesel( TLB, filename=file, filter=filter, $
  	fix_filter=fix_filter, multiple=multiple, path=path, $
  	uname='filesel_cw')

  pfile = ptr_new('')

  state = {	filesel:	select, $
  			pfile:		pfile $			; pointer to file name
  		  }
  child = widget_info( tlb, /child)
  widget_control, child, set_uvalue=ptr_new(state, /no_copy)

  Widget_Control, /REALIZE, TLB

  XManager, 'file_select', TLB , /no_block

	r = *pfile
	return, r
end
