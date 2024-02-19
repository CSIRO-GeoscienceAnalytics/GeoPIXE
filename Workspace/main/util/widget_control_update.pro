pro widget_control_update, tlb, update=update, save_xy=save_xy, rest_xy=rest_xy

; Endeavour to use the widget_control, update= procedure, but stop the
; random jumping of windows back to their original XY offset.  
;
; Normally, use update=0 and update=1 to bracket widget rendering. But if
; nesting is expected (e.g. file_requester used within bracket), then can use
; /save and /rest (not nested) to force a save and restore of window position.

COMPILE_OPT STRICTARR
common c_widget_update_1, widget_update_ytrim
common c_widget_update_2, widget_update_xoff, widget_update_yoff 
common c_widget_update_3, widget_save_xoff, widget_save_yoff 

if widget_info( tlb, /valid) eq 0 then return
if n_elements(update) lt 1 then update=1
if n_elements(save_xy) lt 1 then save_xy=0
if n_elements(rest_xy) lt 1 then rest_xy=0

info = widget_info(tlb,/update)
geom = widget_info( tlb, /geometry)

if n_elements( widget_update_ytrim) lt 1 then begin
	widget_control, tlb, xoffset = geom.xoffset, yoffset=geom.yoffset
	geom2 = widget_info( tlb, /geometry)
	widget_update_ytrim = geom2.yoffset - geom.yoffset
	widget_update_xoff = 0
	widget_update_yoff = 0
	widget_save_xoff = 0
	widget_save_yoff = 0
endif

if save_xy then begin
	widget_save_xoff = geom.xoffset
	widget_save_yoff = geom.yoffset - widget_update_ytrim
	widget_control, tlb, xoffset = widget_save_xoff, yoffset=widget_save_yoff, update=0
	return
endif 
if rest_xy then begin
	widget_control, tlb, xoffset = widget_save_xoff, yoffset=widget_save_yoff, /update
	return
endif

if (info eq 1) and (update eq 0) then begin
	widget_update_xoff = geom.xoffset
	widget_update_yoff = geom.yoffset - widget_update_ytrim
endif 

widget_control, tlb, update=update,  $
			xoffset = widget_update_xoff, yoffset=widget_update_yoff
return
end

