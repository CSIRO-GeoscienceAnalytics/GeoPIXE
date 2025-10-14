;
; IDL Event Callback Procedures
; image_process_eventcb
;
; Generated on:	01/17/100 13:42.18
;
;-----------------------------------------------------------------
; TLB_KILL_REQUEST_EVENTS Callback Procedure.
;
;   {WIDGET_KILL_REQUEST, ID:0L, TOP:0L, HANDLER:0L }
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.
;-----------------------------------------------------------------

pro OnKill_Image_Process, Event

; cancel_notify, event.top

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

notify, 'image-operations-closed', from=event.top

if n_elements(pstate) eq 0 then return
if ptr_valid(pstate) eq 0 then return
if size(*pstate,/tname) ne 'STRUCT' then return

if ptr_valid((*pstate).p) then ptr_free, (*pstate).p

widget_control, event.top, /destroy
;heap_gc, /verbose
end

;-----------------------------------------------------------------
; NOTIFY Callback Procedure.
;
;   {NOTIFY, ID:0L, TOP:0L, HANDLER:0L, TAG:t, POINTER:p, FROM:f }
;
;	TAG	string showing the notification name, as registered.
;	POINTER	pointer passed as a general argument (can be null).
;	FROM	id of widget sending the notify (or zero).
;-----------------------------------------------------------------

pro OnNotify_Image_Process, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case event.tag of

	'images-changed': begin
		p = event.pointer									; pointer to other 'image'
		if ptr_valid(p) eq 0 then goto, finish
		(*pstate).pimage = p
		end

	'batch-filter': begin
		print,'Image-process: batch-filter (get) ...........................................................'
		first = 1
		if ptr_valid( event.pointer) then begin
			if size(*event.pointer,/tname) eq 'STRUCT' then first=(*event.pointer).first
		endif
		if (*event.pointer).do_it then begin
			(*pstate).p2 = ptr_new(1)
			OnButton_Image_Process_Get, Event, first=first
		endif else (*pstate).p2 = ptr_new(0)
		notify, 'done-filter', (*pstate).p2, from=event.top
		return
		end

	'wizard-action': begin
		if ptr_valid( event.pointer) then begin
			if (*event.pointer).window eq 'Image Operations' then begin
				case (*event.pointer).command of
					'open-test': begin
;						print,'*** Wizard Image Process: test if window is open ...'
						pw = (*pstate).pwiz
						*pw = *event.pointer
						(*pw).top = event.top
						(*pw).error = 0
						notify, 'wizard-return', pw
						end

					else: begin
						warning,'image: Notify',['Unknown wizard command: '+(*event.pointer).command, $
								'Make sure GeoPIXE version is compatible with Wizard.']
					endelse
				endcase
			endif
		endif
		end
	else: begin
		print,'OnNotify_Image_Process: unknown tag = ',event.tag
		end
endcase

finish:
end

;-----------------------------------------------------------------
; Tracking Callback Procedure.
;
;   {WIDGET_TRACKING, ID:0L, TOP:0L, HANDLER:0L, ENTER:0 }
;
;   ENTER is 1 if the tracking event is an entry event, and 0 if it
;       is an exit event.
;-----------------------------------------------------------------

pro OnTracking_Image_Process, Event

end

;-----------------------------------------------------------------
; TLB_SIZE_EVENTS Callback Procedure.
;
;   {WIDGET_BASE, ID:0L, TOP:0L, HANDLER:0L, X:0, Y:0 }
;
;   The X and Y fields return the new width of the base, not
;       including any frame provided by the window manager.
;-----------------------------------------------------------------

pro OnSize_Image_Process, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

; Use integer arithmetic for y to ensure a whole number of rows

x = event.x - (*pstate).xoffset
y = event.y - (*pstate).yoffset

;n = ((event.y - (*pstate).yoffset + 9)/ (*pstate).row_height) - 2
;y = (n + 2) * (*pstate).row_height

widget_control, (*pstate).list, scr_xsize=x, scr_ysize=y
end

;-----------------------------------------------------------------

pro OnRealize_Image_Process, wWidget

top = tlb_id(wWidget)
widget_control, top, get_uvalue=tuv

child = widget_info( top, /child)
state = {	$
			p:			ptr_new(), $ ; pointer to use to send 'image-process'
			p2:			ptr_new(), $ ; pointer to use to send 'image-process'
			p3:			ptr_new(), $ ; pointer to use to send 'image-process'

			list:		0L, $		; List ID
			path:		tuv.path, $					; current path
			get_file:	tuv.get_file, $				; current get file name
			pimage:		tuv.pimage, $				; pointer to images
			pget:		ptr_new(/allocate_heap), $	; previous get settings
			pmodel:		ptr_new(/allocate_heap), $	; get pimg parameters
			pwiz:		ptr_new(/allocate_heap), $	; wizard memory

			row_height:	0, $		; table row height
			xoffset:	0, $		; offset in xsize for resize
			yoffset:	0 }			; offset in ysize for resize

;w = widget_info( wWidget, /row_heights)
w = 18
geom = widget_info( wWidget, /geometry)
tlb_geom = widget_info( top, /geometry)

state.list = wWidget
state.row_height = w[0]
state.yoffset = tlb_geom.ysize - geom.scr_ysize
state.xoffset = tlb_geom.xsize - geom.scr_xsize

widget_control, child, set_uvalue=ptr_new(state, /no_copy)
end

;-----------------------------------------------------------------
; List Select Item Callback Procedure.
;
;   {WIDGET_LIST, ID:0L, TOP:0L, HANDLER:0L, INDEX:0L, CLICKS:0L}
;
;   INDEX returns the index of the selected item. This index can be
;       used to subscript the array of names originally used to set
;       the widget's value. The CLICKS field returns either 1 or 2,
;       depending upon how the list item was selected. If the list
;       item is double-clicked, CLICKS is set to 2.
;-----------------------------------------------------------------

pro OnSelect_Image_Process, Event

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
		warning,'OnSelect_image_process',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, done
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

widget_control, event.id, get_uvalue=uv
if event.index ge n_elements( uv.routine) then goto, done

if ptr_valid((*pstate).p) then ptr_free, (*pstate).p

(*pstate).p = ptr_new( {  $
				name: 		'Process', $
				mode:		0, $							; use current image
				operation:	uv.routine[event.index], $
				arg1:		uv.arg[event.index] }, /no_copy)

notify, 'image-process', (*pstate).p,  from=event.top

done:
end

;-----------------------------------------------------------------

pro OnButton_Image_Process_Get, Event, first=first

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
		warning,'OnButton_Image_Process_Get',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, done
	endif
endif
if n_elements(first) lt 1 then first=1
image_geopixe_preview

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if ptr_valid((*pstate).pimage) eq 0 then goto, done
p = (*pstate).pimage

path = extract_path(*(*pstate).get_file)
if strlen(path) lt 1 then path=*(*pstate).path
file = strip_path(*(*pstate).get_file)

if first then begin
	F = file_requester( /read, filter = '*.dai', /translate, updir=3, $
			/must_exist, /fix_filter, /image, path=path, file=file, group=event.top, $
			title='Get Image Operations from file', preview_routine='image_geopixe_preview')
	if F eq '' then goto, done

	*(*pstate).get_file = F
	pimg = read_geopixe_image(F)
	copy_pointer_data, (*pimg).history, phistory, /init
	*(*pstate).pmodel = {el:*(*pimg).el, options:*(*pimg).options, phistory:phistory}
	free_images, pimg

	el_names = strcompress( *(*p).el, /remove_all)
	titles = ['Display Range','Image Processing']
	heads = ['Image Processing Select','Select Elements','Apply Settings from File']

	select = export_select( event.top, el_names, titles, $
				old_select=*(*pstate).pget, path=*(*pstate).path, title=heads)
	if select.error then goto, done
	*(*pstate).pget = select
endif

if n_elements(*(*pstate).pmodel) lt 1 then goto, done
select = *(*pstate).pget

widget_control, (*pstate).list, get_uvalue=uv, /hourglass

for i=0L,(*p).n_el-1 do begin
	j = where( strtrim((*(*p).el)[i],2) eq strtrim((*(*pstate).pmodel).el,2))
	j = j[0]
	if (j ne -1) and (select.el_enable[i] eq 1) then begin

		if select.mode_enable[0] then begin				; apply display range settings

			(*(*p).options)[i].bottom = ((*(*pstate).pmodel).options)[j].bottom
			(*(*p).options)[i].top = ((*(*pstate).pmodel).options)[j].top
			(*(*p).options)[i].log = ((*(*pstate).pmodel).options)[j].log
		endif

		phist = (*(*(*pstate).pmodel).phistory)[j]		; apply digital filters from history

		if ptr_valid(phist) and select.mode_enable[1] then begin
			nhist = n_elements(*phist)
			if nhist ge 1 then begin
				for k=0L,nhist-1 do begin
					image_build_op, i, *(*p).el, uv, (*phist)[k], ops
				endfor
			endif
		endif
	endif
endfor

; Need to think about whether we need to introduce a "priority" in the order of these operations.
; For example, should we do a global "Correct Zero pixels" before individual median filters?
; Or should that be the other way round?
; Should all operations performed on an image be tagged by a sequence number, so we can do them again
; in the same order?

if n_elements(ops) ge 1 then begin
	(*pstate).p = ptr_new( ops, /no_copy)
	notify, 'image-process', (*pstate).p,  from=event.top
endif else begin
	notify, 'image-display', from=event.top
endelse
done:
end

;-----------------------------------------------------------------

pro OnButton_Image_Process_Undo, Event

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
		warning,'OnButton_Image_Process_Undo',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, done
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid((*pstate).p) then ptr_free, (*pstate).p
(*pstate).p = ptr_new( { $
		name:		'process', $
		mode:		0, $						; use current image
		operation:	'Image_Restore_Undo', $		; name of operation
		arg1:		0 $							; arg 1
		}, /no_copy)

notify, 'image-process', (*pstate).p,  from=event.top

done:
end

;-----------------------------------------------------------------
; Activate Button Callback Procedure.
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.
;-----------------------------------------------------------------

pro OnButton_Image_Process_Execute, Event

end

;-----------------------------------------------------------------
; Activate Button Callback Procedure.
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.
;-----------------------------------------------------------------

pro OnButton_Image_Process_Close, Event

OnKill_Image_Process, Event
end
;
; Empty stub procedure used for autoloading.
;
pro image_process_eventcb
end
