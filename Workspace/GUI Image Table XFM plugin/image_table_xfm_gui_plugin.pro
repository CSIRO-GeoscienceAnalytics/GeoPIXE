
;	Plugin to add an XFM tab to the Image Table GUI

function image_table_xfm_gui_plugin_event, Event

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
		warning,'image_table_xfm_gui_plugin_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif

	child = widget_info( event.handler, /child)
	widget_control, child, get_uvalue=pstate
	pstate_parent = (*pstate).pstate_parent

	if ptr_good(pstate_parent, /struct) eq 0 then goto, bad_state
	if ptr_good(pstate, /struct) eq 0 then goto, bad_state

	p = (*pstate_parent).p
	no_regions = 0
	if ptr_valid(p) eq 0 then goto, bad_ptr
	if size(*p,/tname) ne 'POINTER' then begin
		no_regions = 1
	endif else begin
		if ptr_valid( (*p)[0] ) eq 0 then no_regions=1
		if no_regions eq 0 then if size(*(*p)[0],/tname) ne 'STRUCT' then no_regions=1
	endelse
	if no_regions eq 0 then obj = (*(*p)[0]).DevObj

	case tag_names( event,/structure) of
		'WIDGET_TRACKING': begin
			return, { WIDGET_TRACKING, ID:event.id, TOP:event.top, HANDLER:0L, ENTER:event.enter }
			end
		else:
	endcase

	uname = widget_info( event.id, /uname)

	case uname of
		'XFM_Button': begin
			if no_regions then goto, finish
			OnButton_Image_Table_Export_XFM, Event
			end
	
		else: return, event
	endcase

finish:
	return, 0L

bad_state:
	warning,'image_table_xfm_gui_plugin_event',['STATE variable has become ill-defined.','Abort Image Table.'],/error
	goto, finish
bad_ptr:
	warning,'image_table_xfm_gui_plugin_event',['Parameter structure variable has become ill-defined.','Abort Image Table.'],/error
	goto, finish
end

;-----------------------------------------------------------------

pro OnButton_Image_Table_Export_XFM, Event

;	XFM export of region bounds 

COMPILE_OPT STRICTARR

;	This was written for parent originally, so pstate is that of Image_Table parent

	child = widget_info( event.handler, /child)
	widget_control, child, get_uvalue=pstate_local
	pstate = (*pstate_local).pstate_parent

	OnButton_Image_Table_Save, Event		; save regions file first

	default = geopixe_defaults( source='Image Table XFM Export')

;	Originally used 'custom' mechanism. Now just an Image Table GUI plugin.

;	if default.custom.enable eq 0 then return
;	if default.custom.lab ne 'XFM' then return

	path = fix_path( default.custom.path)
;	file = strip_file_ext(default.custom.file) + '.csv'

	p = *(*pstate).p
	region_file = (*p[0]).region_file
	file2 = strip_file_ext( region_file) + '.csv'
	file1 = strip_path(file2)

		child2 = widget_info( (*pstate).group, /child)
		widget_control, child2, get_uvalue=pstate_image
		pimg = (*pstate_image).p

		r = image_absolute( pimg, absolute=0, error=err)
		cx = r.absolute.size.x / r.uncompressed.size.x
		cy = r.absolute.size.y / r.uncompressed.size.y

	close_file, 1
	on_ioerror, bad_open
	F = fix_path(path) + file1
	openw,1, F
	on_ioerror, bad_write

;	absx, absy are pixel coords in full scan
;	These * cx,cy are the scan coords (mm).

	for j=0L,(*pstate).n-1 do begin
		pj = p[j]

		pm = (*pj).pmark[0]
		if ptr_valid(pm) eq 0 then goto, bad_region

		r = image_absolute( pimg, crop={x:[min((*pm).x),max((*pm).x)], y:[min((*pm).y),max((*pm).y)]}, error=err)
		if err eq 0 then begin
			ox = r.absolute.org.x
			oy = r.absolute.org.y
			sx = r.absolute.size.x
			sy = r.absolute.size.y

			printf,1, (*pj).note, sx,ox, sy,oy, format='(A,",,",F7.3,",",F7.3,",,,",F7.3,",",F7.3)'
		endif
	endfor

done:
	on_ioerror, null
	close_file, 1

	file_copy, F, file2, /overwrite

	warning,'OnButton_Image_Table_Export_XFM',['Coordinates written to files:', F[0],file2], /info
	return
	
bad_open:
	warning,'OnButton_Image_Table_Export_XFM',['Failed to open file: '+F[0],'Check that file is not open in Excell.']
	goto, done
bad_write:
	warning,'OnButton_Image_Table_Export_XFM',['Bad write to file: '+F[0],'Check that file is not open in Excell.']
	goto, done
bad_region:
	warning,'OnButton_Image_Table_Export_XFM',['Bad region pointer.']
	goto, done
end

;---------------------------------------------------------------------

function image_table_xfm_gui_plugin, tab_panel, pstate_parent

;	Plugin to add a XFM tab to the Image Table GUI
;	with an export button, to export a table of coordinates of regions, to be import
;	into XFM stage controls.

; ---------------- xfm  -------------------------------------------------------------------

	geo = widget_info( tab_panel, /geometry)

	xfm_base = widget_base( tab_panel, title=' XFM ', /column, xpad=0, ypad=1, space=3, $
								/align_center, /base_align_center, scr_xsize=geo.scr_xsize)

	h0base = widget_base( xfm_base, /row, /base_align_center, ypad=0, xpad=0, space=10)

	xfm_base2 = Widget_Base( h0base, UNAME='Image_Table_HButton_Base' ,/ALIGN_CENTER ,/BASE_ALIGN_CENTER, SPACE=1 ,XPAD=0 ,YPAD=0 , /ROW, uvalue='')

	XFM_Button = Widget_Button(xfm_base2, /tracking, UNAME='XFM_Button' ,/ALIGN_CENTER , $
			VALUE='Export Coords', uvalue='Export region coordinates to a CSV file to be imported into the XFM Scan Script spreadsheet.' )

  
	WIDGET_CONTROL, xfm_base, EVENT_FUNC = 'image_table_xfm_gui_plugin_event'
  
	state = {	pstate_parent:	pstate_parent, $	; pstate in image_table parent	

			group:		tab_panel, $		; tab parent TLB
			tlb:		xfm_base }			; TLB ID
  
	child = widget_info( xfm_base, /child)
	pstate = ptr_new(state, /no_copy)
	widget_control, child, set_uvalue=pstate

	return, xfm_base
end
