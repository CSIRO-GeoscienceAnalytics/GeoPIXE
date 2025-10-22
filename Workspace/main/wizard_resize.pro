pro wizard_resize_widget, id, dx,dy

COMPILE_OPT STRICTARR

	if widget_info( id, /valid) eq 0 then return
	child = widget_info( id, /all_children)

	for i=0,n_elements(child)-1 do begin
		wizard_resize_widget, child[i], dx,dy
	endfor

	widget_control, id, get_uvalue=uv
	print,'Resize Uname = ', widget_info( id, /uname)
	
	if size(uv,/tname) eq 'STRUCT' then begin
		help,uv
		geom = widget_info( id, /geometry)
		if tag_present('XRESIZE',uv) then begin
			delta = round( uv.xresize * dx)

			if widget_info( id, /uname) eq 'results-table' then begin
				print,'Resize table X by ',delta,' to ', geom.scr_xsize + delta
			endif

			if delta ne 0 then begin
				widget_control, id, scr_xsize = (geom.scr_xsize + delta) > 10
			endif
		endif
		if tag_present('YRESIZE',uv) then begin
			delta = round( uv.yresize * dy)

			if widget_info( id, /uname) eq 'results-table' then begin
				print,'Resize table Y by ',delta,' to ', geom.scr_ysize + delta
			endif

			if delta ne 0 then begin
				widget_control, id, scr_ysize = (geom.scr_ysize + delta) > 10
			endif
		endif
	endif

;	for i=0,n_elements(child)-1 do begin
;		wizard_resize_widget, child[i], dx,dy
;	endfor

	return
end

;----------------------------------------------------------------------------------------

pro wizard_resize, tlb, oldx=oldx, oldy=oldy, minx=minx, miny=miny

; Resize all widgets in a heirachy flagged with 'xresize:1' or 'yresize:1' in the uvalue struct.
; Determine change in TLB geom scr size from previously ('oldx', 'oldy') as supplied.
; Don't allow TLB geom any smaller than 'minx', 'miny'.
;
; Remember to use ‘widget_info( event.top, /geometry)’ to update TLB size (stored elsewhere)
; after the resize has finished.

COMPILE_OPT STRICTARR
ErrorNo = 0
error = 1
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
       warning,'wizard_resize',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	print,'Resize Uname = ', widget_info( tlb, /uname)

if n_elements(tlb) eq 0 then return
if n_elements(oldx) eq 0 then return
if n_elements(oldy) eq 0 then return
if n_elements(minx) eq 0 then minx=200
if n_elements(miny) eq 0 then miny=100

	geom = widget_info( tlb, /geometry)
	dx = (geom.scr_xsize > minx) - oldx
	dy = (geom.scr_ysize > miny) - oldy

	wizard_resize_widget, tlb, dx,dy
	return
end
