;
; IDL Event Callback Procedures
; corr_eventcb
;
;-----------------------------------------------------------------

pro Corr_Analyze_Spline, Event, fresh=fresh, exclude=exclude

; fresh = 1	new spline selection
;		  0	refine existing (i.e. intersection)
; /exclude	remove selected pixels from selection

COMPILE_OPT STRICTARR
if n_elements(exclude) eq 0 then exclude=0
if n_elements(fresh) eq 0 then fresh=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_good(p) eq 0 then goto, done
if ptr_valid((*pstate).pmark) eq 0 then goto, done

xanes_stack_test, p, xanes, n_el, el, el_xanes

qc = make_corr_mask( pstate, fresh=fresh, exclude=exclude)

if qc[0] ne -1 then begin
	if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
	(*pstate).qc = ptr_new( qc, /no_copy)

	if ptr_valid( (*pstate).pspline) then ptr_free, (*pstate).pspline
	(*pstate).pspline = ptr_new( {elx:el[(*pstate).corr_x], ely:el[(*pstate).corr_y], $
						qc:(*pstate).qc, pmark:(*pstate).pmark }, /no_copy)

	notify, 'corr-analyze-spline', (*pstate).pspline, from=event.top
	if (*pstate).highlight then draw_corrs, pstate
endif

done:
end

;-----------------------------------------------------------------

pro Corr_Clear_Spline, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if (*pstate).highlight eq 0 then begin
	clear_corr_all_markers, pstate

	notify, 'corr-analyze-clear', from=event.top
endif
end

;-----------------------------------------------------------------

pro Corr_Clear_qc, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc

notify, 'corr-analyze-clear', from=event.top
if (*pstate).highlight then draw_corrs, pstate
end

;-----------------------------------------------------------------

pro corr_Clone, Event, n

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
		warning,'corr_clone',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(n) lt 1 then n=1
geom = widget_info( event.top, /geometry)

xoffset = intarr(n)
yoffset = intarr(n)
for i=0L,n-1 do begin
	if (n eq 1) and (i eq 0) then begin
		xoffset[0] = geom.xoffset + 50
		yoffset[0] = geom.yoffset + 50
	endif else if i lt 3 then begin
		xoffset[i] = geom.xoffset + (i+1)*(geom.scr_xsize+2)
		yoffset[i] = geom.yoffset
	endif else if i lt 7 then begin
		xoffset[i] = geom.xoffset + (i-3)*(geom.scr_xsize+2)
		yoffset[i] = geom.yoffset + geom.scr_ysize+2
	endif else if i lt 11 then begin
		xoffset[i] = geom.xoffset + (i-7)*(geom.scr_xsize+2)
		yoffset[i] = geom.yoffset + 2*(geom.scr_ysize+2)
	endif else if i lt 15 then begin
		xoffset[i] = geom.xoffset + (i-11)*(geom.scr_xsize+2)
		yoffset[i] = geom.yoffset + 3*(geom.scr_ysize+2)
	endif
endfor

path = *(*pstate).path

for i=0L,n-1 do begin
	corr, group_leader=event.top, TLB=tlb, path=path, pimages=(*pstate).p, qregion=(*pstate).q, $
				xoffset=xoffset[i], yoffset=yoffset[i]

	register_notify, event.top, $
 				['images', $					; new images loaded somewhere
		  		'images-changed', $				; pass on notify of images changed
				'path', $						; new path
				'image-region-select', $		; region selected
				'image-analyze-q', $			; new q array
				'image-analyze-all-clear', $	; clear q array
				'corr-analyze-clear', $			; pass on notify of qc cleared destined for image
				'corr-analyze-spline', $		; pass on notify of qc destined for image
		  		'corr-display' $				; pass on notify of corr changed (e.g. colours)
				], from=tlb
endfor
end

;-----------------------------------------------------------------

pro corr_Colours, Event, n

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(n) gt 0 then begin
	loadct, n, bottom=16, ncolors=100
endif else begin
	xloadct, group=event.top, /modal, bottom=16, ncolors=100
endelse

load_spec_colours
draw_corrs, pstate
notify, 'corr-display', from=event.top
end

;-----------------------------------------------------------------

pro corr_Exit, Event

print,'corr_Exit ...'
OnKill_corr, event

end

;-----------------------------------------------------------------

pro Corr_Export, Event, cgm=cgm, wmf=wmf, eps=eps, jpeg=jpeg

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(cgm) eq 0 then cgm=0
if n_elements(wmf) eq 0 then wmf=0
if n_elements(eps) eq 0 then eps=0
if n_elements(jpeg) eq 0 then jpeg=0
if ptr_valid( (*pstate).p) eq 0 then goto, done

p = (*pstate).p
if ptr_good( p) eq 0 then return
xanes_stack_test, p, xanes, n_el, el, el_xanes, z_found=z_found

select = plot_corr_select( event.top, cgm=cgm, wmf=wmf, eps=eps, old_select=*(*pstate).pexport, $
					jpeg=jpeg, path=*(*pstate).path, corr_pstate=pstate )
if select.error then goto, done

;goto, done     ; use this bypass if testing plot_image_select without /modal, etc.

name = 'Save Export Association as '

if select.plot.type eq 'CGM' then begin
	file = find_file2( (*p).file)
	path = extract_path( file[0])
	if lenchr(path) eq 0 then path = *(*pstate).path
    file = strip_path( strip_file_ext( (*p).file)) + '-corr' + '.cgm'

    file = file_requester( /write, filter='*.cgm', path=path, $
         file=file, title=name+'CGM', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
    file = strip_file_ext(file) + '.cgm'
    file = strip_file_m( file, /element)

    plot_corr, pstate, /cgm, file=file, options=select.plot

endif else if select.plot.type eq 'METAFILE' then begin
	file = find_file2( (*p).file)
	path = extract_path( file[0])
	if lenchr(path) eq 0 then path = *(*pstate).path
    file = strip_path( strip_file_ext( (*p).file)) + '-corr' + '.wmf'

    file = file_requester( /write, filter='*.wmf', path=path, $
         file=file, title=name+'WMF', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
    file = strip_file_ext(file) + '.wmf'
    file = strip_file_m( file, /element)

    plot_corr, pstate, /wmf, file=file, options=select.plot

endif else if select.plot.type eq 'JPEG' then begin
   	file = find_file2( (*p).file)
   	path = extract_path( file[0])
   	if lenchr(path) eq 0 then path = *(*pstate).path
   	file = strip_path( strip_file_ext( (*p).file)) + '-corr' + '.jpg'

   	file = file_requester( /write, filter='*.jpg', path=*(*pstate).path, $
   		file=file, title=name+'JPEG', group=event.top, /fix_filter)
   	if strlen(file) lt 1 then goto, done
   	file = strip_file_ext(file) + '.jpg'
   	file = strip_file_m( file, /element)

   	plot_corr, pstate, /jpeg, file=file, options=select.plot

   endif else if select.plot.type eq 'PNG' then begin
   	file = find_file2( (*p).file)
   	path = extract_path( file[0])
   	if lenchr(path) eq 0 then path = *(*pstate).path
   	file = strip_path( strip_file_ext( (*p).file)) + '-corr' + '.png'

   	file = file_requester( /write, filter='*.png', path=*(*pstate).path, $
   		file=file, title=name+'PNG', group=event.top, /fix_filter)
   	if strlen(file) lt 1 then goto, done
   	file = strip_file_ext(file) + '.png'
   	file = strip_file_m( file, /element)

   	plot_corr, pstate, /png, file=file, options=select.plot

endif else if select.plot.type eq 'PS' then begin
	file = find_file2( (*p).file)
	path = extract_path( file[0])
	if lenchr(path) eq 0 then path = *(*pstate).path
    file = strip_path( strip_file_ext( (*p).file)) + '-corr' + '.eps'

    file = file_requester( /write, filter='*.eps', path=path, $
         file=file, title=name+'EPS', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
    file = strip_file_ext(file) + '.eps'
    file = strip_file_m( file, /element)

    plot_corr, pstate, /eps, file=file, options=select.plot

endif else if select.plot.type eq 'CSV' then begin
	warning,'Corr_Export','Not implemented yet.'
	goto, done
	
endif else begin
    plot_corr, pstate, options=select.plot
endelse

*(*pstate).pexport = {plot:select.plot}

done:
end

;-----------------------------------------------------------------

pro corr_Invert_Colours, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

tvlct, ro,go,bo, /get
rr = reverse(ro[16:115])
gg = reverse(go[16:115])
bb = reverse(bo[16:115])
tvlct, rr,gg,bb, 16

load_spec_colours
draw_corrs, pstate
notify, 'corr-display', from=event.top
end

;-----------------------------------------------------------------

pro corr_Linear_Luminance, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

tvlct, rr,gg,bb, /get
r = rr[16:115]
g = gg[16:115]
b = bb[16:115]

s = float(r) + float(2.2*g) + float(b)
if s[0] gt s[99] then begin
	L = (256./100.)*(99. - findgen(100))*4.2
endif else begin
	L = (256./100.)*findgen(100)*4.2
endelse
f = L/s
rr[16:115] = byte(f*r < 255)
gg[16:115] = byte(f*g < 255)
bb[16:115] = byte(f*b < 255)

tvlct, rr[16:115],gg[16:115],bb[16:115], 16

load_spec_colours
draw_corrs, pstate
notify, 'corr-display', from=event.top
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

pro corr_Save_GIF, Event, png=png

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(png) lt 1 then png=0
p = (*pstate).p
if ptr_valid(p) eq 0 then return
xanes_stack_test, p, xanes, n_el, el, el_xanes, z_found=z_found

if png then begin
	ext = '.png'
	title = 'PNG file to Write'
endif else begin
	ext = '.gif'
	title = 'GIF file to Write'
endelse
s = strtrim( el[(*pstate).corr_x],2) + '-' + strtrim( el[(*pstate).corr_y],2)

file = find_file2( (*p).file)
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
gif_file = strip_path( strip_file_ext( (*p).file)) + '-' + s + '-corr' + ext

F = file_requester( /write, filter = '*'+ext, path=path, $
			title=title, file = gif_file, group=event.top, fix_filter=1)
if F ne '' then begin
;	widget_control, /hourglass

	b = make_corr_tvb( pstate, (*pstate).corr_x, (*pstate).corr_y)
	window, /free, xsize=n_elements(b[*,0]), ysize=n_elements(b[0,*]), /pixmap
	tpix = !d.window
	tv, b
	plot_corr_spline, pstate, /wide, /bare
	xyouts,0.8,0.02,el[(*pstate).corr_x],charsize=1.8,charthick=8.0,color=16,/norm
	xyouts,0.01,0.8,el[(*pstate).corr_y],charsize=1.8,charthick=8.0,color=16,/norm
	xyouts,0.8,0.02,el[(*pstate).corr_x],charsize=1.8,charthick=1.7,color=spec_colour('green'),/norm
	xyouts,0.01,0.8,el[(*pstate).corr_y],charsize=1.8,charthick=1.7,color=spec_colour('green'),/norm
	b = color_quan( tvrd(true=3), 3, rcol,gcol,bcol, colors=128)
	if n_elements(b) gt 1 then begin
		if png then begin
			write_png, F, b, rcol,gcol,bcol
		endif else begin
			write_gif, F, b, rcol,gcol,bcol
		endelse
	endif else begin
		print,'corr_save_GIF:  Nothing to save!  No TV byte data.'
	endelse
	if tpix ne 0 then wdelete, tpix
endif

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

pro OnButton_corr_Full, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then return

set_corr_view, pstate, event.top, /full
end
;
;-----------------------------------------------------------------

pro OnButton_corr_Zoom_In, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done

set_corr_view, pstate, event.top, zoom=+1

done:
end
;
;-----------------------------------------------------------------

pro OnButton_corr_Zoom_Out, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done

set_corr_view, pstate, event.top, zoom=-1

done:
end
;
;-----------------------------------------------------------------
; Note: operation here depends on whether this is a clone or not.

pro OnDestroy_corr, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

print,'OnDestroy_corr: Cleanup pixemaps ...'
if n_elements(pstate) eq 0 then return
if ptr_valid(pstate) eq 0 then return
if size(*pstate,/tname) ne 'STRUCT' then return

	free_corr_state, pstate

return
end

;-----------------------------------------------------------------

pro OnKill_corr, Event

print,'OnKill_corr: destroy widget tree ...'
cancel_notify, event.top

widget_control, event.top, /destroy
;heap_gc, /verbose
end

;-----------------------------------------------------------------
; Slider Callback Procedure.
;
;   {WIDGET_SLIDER, ID:0L, TOP:0L, HANDLER:0L, VALUE:0,  DRAG:0}
;
;	VALUE returns the new value of the slider.
;	DRAG returns integer 1
;		if the slider event was generated as part of a drag operation,
;		or zero if the event was generated when the user had finished
;		positioning the slider.
;-----------------------------------------------------------------

pro OnMove_corr_Low, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).Low = float(event.value)
draw_corrs, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_corr_High, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).High = float(event.value)
draw_corrs, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_corr_Y, Event, high=high

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(high) lt 1 then high=0

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

if high then begin
	(*pstate).ytop = float(event.value)
endif else begin
	(*pstate).ybottom = float(event.value)
endelse
p = (*pstate).pmark
conc_corr_to_xy, pstate, (*p).cx,(*p).cy, x,y, /nozoom
(*p).x = x
(*p).y = y
draw_corrs, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_corr_X, Event, high=high

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if n_elements(high) lt 1 then high=0

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

if high then begin
	(*pstate).xtop = float(event.value)
endif else begin
	(*pstate).xbottom = float(event.value)
endelse
p = (*pstate).pmark
conc_corr_to_xy, pstate, (*p).cx,(*p).cy, x,y, /nozoom
(*p).x = x
(*p).y = y
draw_corrs, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_corr_X_smooth, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).x_smooth = float(event.value)
draw_corrs, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_corr_Y_smooth, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).y_smooth = float(event.value)
draw_corrs, pstate

done:
end

;-----------------------------------------------------------------
; NOTIFY Callback Procedure.
;
;   {NOTIFY, ID:0L, TOP:0L, HANDLER:0L, TAG:t, POINTER:p, FROM:from }
;
;	TAG	string showing the notification name, as registered.
;	POINTER	pointer passed as a general argument (can be null).
;	FROM	id of widget sending the notify (or 0).
;-----------------------------------------------------------------

pro OnNotify_corr, Event

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
		warning,'OnNotify_corr',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

;print,'OnNotify_corr: tag = ',event.tag

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

;print,'******* corr_Evenctcb: notify: tag=',event.tag,' from=',event.from,' to=',event.top

case event.tag of

	'corr-display': begin	; returned from 'corr_select'

		; print,' Notified of display changed.'
		draw_corrs, pstate
		end

	'image-display': begin	; returned from 'image'

		; print,' Notified of display changed.'
		draw_corrs, pstate
		end

	'path': begin
		if ptr_valid( event.pointer) eq 0 then goto, finish
		print,'corr: new path = ',(*event.pointer)
		*(*pstate).path = (*event.pointer)
		end

	'image-region-select': begin							; returned from 'corr_table'

		if ptr_valid( event.pointer) then begin
			p = (*event.pointer).pregion
			if ptr_valid(p) then begin
				if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q
				if (*p).mode eq 1 then begin
					pi = (*pstate).p
					p2 = do_translate_region( p, ixoffset=(*pi).xoffset, iyoffset=(*pi).yoffset, ixcompress=(*pi).xcompress, $
							iycompress=(*pi).ycompress, ixsize=(*pi).xsize, error=error)

					xanes_stack_test, pi, xanes, n_el, el, el_xanes
					if ptr_valid( (*p).pmark[0]) then begin
						*(*pstate).pmark = *(*p).pmark[0]
					endif else begin
						clear_corr_all_markers, pstate
					endelse 
					q = where( (*p).elx eq el)
					if q[0] ne -1 then begin
						(*pstate).corr_x = q[0]
						widget_control, (*pstate).element_idx, set_combobox_select=(*pstate).corr_x
					endif
					q = where( (*p).ely eq el)
					if q[0] ne -1 then begin
						(*pstate).corr_y = q[0]
						widget_control, (*pstate).element_idy, set_combobox_select=(*pstate).corr_y
					endif
					if ptr_valid( (*p).q) then (*pstate).qc = ptr_new( *(*p2).q)
				endif else begin
					if ptr_valid( (*p).q) then (*pstate).q = ptr_new( *(*p).q)
					clear_corr_all_markers, pstate
				endelse
				draw_corrs, pstate
			endif
		endif
		end

	'corr-analyze-clear': begin								; returned from other 'corr'

		clear_corr_spline, pstate
		if (*pstate).highlight then begin
			ptr_free, (*pstate).qc 
			(*pstate).qc = ptr_new()
			draw_corrs, pstate
		endif
   		end

	'corr-analyze-spline': begin							; returned from other 'corr'

		if (*pstate).highlight then begin
			if ptr_good( event.pointer) then begin
				(*pstate).pspline = ptr_new( *event.pointer)
				(*pstate).qc = ptr_new( *(*event.pointer).qc)
			endif
			draw_corrs, pstate
		endif
		end

	'images-changed': begin									; returned from other apps

		p = event.pointer									; pointer to new 'images'
		if ptr_valid(p) eq 0 then goto, finish

		(*pstate).p = p
		(*pstate).file = (*p).file
		if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q

		set_corr_view, pstate, event.top
		end

;	'images': begin											; returned from other apps

;		p = event.pointer									; pointer to new 'images'
;		if ptr_valid(p) eq 0 then goto, finish

;		(*pstate).p = p
;		(*pstate).file = (*p).file
;		if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q

;		set_corr_view, pstate, event.top
;		end

	'image-analyze-q': begin								; returned from 'image' clone

		if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
		if ptr_valid( event.pointer) then begin
			pq = *event.pointer
			if ptr_valid(pq.q) then (*pstate).q = ptr_new( *pq.q)
			draw_corrs, pstate
		endif
		end

	'image-analyze-all-clear': begin						; returned from 'image' clone

		if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
		draw_corrs, pstate
		end

	else: begin
	;	print,'OnNotify_corr: unknown tag = ',event.tag
		end
endcase

finish:
	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die
	return

die:
	widget_control, event.top, /destroy
	return
	end

;-----------------------------------------------------------------

pro OnRealize_corr_Help1, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).help1 = wWidget
(*pstate).help = wWidget
end
;
;-----------------------------------------------------------------

pro OnRealize_corr_Help2, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).help2 = wWidget
end
;
;-----------------------------------------------------------------

pro OnRealize_corr, wWidget

;	The association draw area

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

draw_trim = 0
scr_trim = 15
if !version.os_family eq 'MacOS' then begin
	draw_trim = 15
	scr_trim = 21
endif

widget_control, wWidget, get_value=wid2
wset,wid2
(*pstate).wid2 = wid2
(*pstate).draw2 = wWidget

w = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
h = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top

(*pstate).w = (w + scr_trim) < 600
(*pstate).h = (h + scr_trim) < 600
widget_control, (*pstate).draw2, draw_xsize=w+draw_trim, draw_ysize=h+draw_trim, $
		scr_xsize=(*pstate).w, scr_ysize=(*pstate).h

(*pstate).dw =		16
(*pstate).dh =		0

window, /free, xsize=w, ysize=h, /pixmap
(*pstate).pix = !d.window
window, /free, xsize=w, ysize=h, /pixmap
(*pstate).pix2 = !d.window

(*pstate).count++
if ptr_valid( (*pstate).p) and ((*pstate).count ge 3) then begin
	if n_elements( *(*pstate).p) gt 0 then begin
		draw_corrs, pstate
	endif
endif
end

;-----------------------------------------------------------------

pro OnRealize_corrX, wWidget

;	The X histogram draw area

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

draw_trim = 0
scr_trim = 15
if !version.os_family eq 'MacOS' then begin
	draw_trim = 15
	scr_trim = 21
endif

widget_control, wWidget, get_value=widX
wset,widX
(*pstate).widX = widX
(*pstate).drawX = wWidget

w = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
h = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top

(*pstate).w = (w + scr_trim) < 600
(*pstate).h = (h + scr_trim) < 600
widget_control, (*pstate).drawX, draw_xsize=w+draw_trim, draw_ysize=h+draw_trim, $
		scr_xsize=(*pstate).w, scr_ysize=(*pstate).h

(*pstate).dw =		16
(*pstate).dh =		0

(*pstate).count++
if ptr_valid( (*pstate).p) and ((*pstate).count ge 3) then begin
	if n_elements( *(*pstate).p) gt 0 then begin
		draw_corrs, pstate
	endif
endif
end

;-----------------------------------------------------------------

pro OnRealize_corrY, wWidget

;	The Y histogram draw area

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

draw_trim = 0
scr_trim = 15
if !version.os_family eq 'MacOS' then begin
	draw_trim = 15
	scr_trim = 21
endif

widget_control, wWidget, get_value=widY
wset, widY
(*pstate).widY = widY
(*pstate).drawY = wWidget

w = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
h = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top

(*pstate).w = (w + scr_trim) < 600
(*pstate).h = (h + scr_trim) < 600
widget_control, (*pstate).drawY, draw_xsize=w+draw_trim, draw_ysize=h+draw_trim, $
		scr_xsize=(*pstate).w, scr_ysize=(*pstate).h

(*pstate).dw =		16
(*pstate).dh =		0

(*pstate).count++
if ptr_valid( (*pstate).p) and ((*pstate).count ge 3) then begin
	if n_elements( *(*pstate).p) gt 0 then begin
		draw_corrs, pstate
	endif
endif
end

;-----------------------------------------------------------------

pro OnRealize_Corr_Elementx, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_good(p) eq 0 then return
xanes_stack_test, p, xanes, n_el, el, el_xanes

(*pstate).element_idx = wWidget
if ptr_valid( (*pstate).p) then begin
	if n_elements( *p) gt 0 then begin
		widget_control, wWidget, set_value=el
	endif
endif
widget_control, wWidget, set_combobox_select = (*pstate).corr_x
end

;-----------------------------------------------------------------

pro OnRealize_Corr_Elementy, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_good(p) eq 0 then return
xanes_stack_test, p, xanes, n_el, el, el_xanes

(*pstate).element_idy = wWidget
if ptr_valid( p) then begin
	if n_elements( *p) gt 0 then begin
		widget_control, wWidget, set_value=el
	endif
endif
widget_control, wWidget, set_combobox_select = (*pstate).corr_y
end

;-----------------------------------------------------------------

pro OnRealize_corr_Low_Slider, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Low_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_corr_High_Slider, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).High_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_corr_Y_Slider, wWidget

; print,'onrealize_bottom slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Y_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_corr_Y_Slider_top, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Y_slider_top = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_corr_X_Slider, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).X_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_corr_X_Slider_top, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).X_slider_top = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_corr_X_smooth_Slider, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).X_smooth_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_corr_Y_smooth_Slider, wWidget

; print,'onrealize_bottom slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Y_smooth_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_corr_Xlog, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Xlog_button = wWidget
widget_control, wWidget, set_button=0
end

;-----------------------------------------------------------------

pro OnRealize_corr_Ylog, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Ylog_button = wWidget
widget_control, wWidget, set_button=0
end

;-----------------------------------------------------------------

pro OnRealize_corr_Zaxis, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

widget_control, wWidget, set_combobox_select=2
end

;-----------------------------------------------------------------

pro OnRealize_corr_Highlight, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).highlight_button = wWidget
widget_control, wWidget, set_button=0
end

;-----------------------------------------------------------------
; Droplist Select Item Callback Procedure.
;
;   {WIDGET_combobox, ID:0L, TOP:0L, HANDLER:0L, INDEX:0L }
;
;   INDEX returns the index of the selected item. This can be used to
;       index the array of names originally used to set the widget's
;       value.
;-----------------------------------------------------------------

pro OnSelect_corr_Zaxis, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Zaxis = event.index

draw_corrs, pstate

s = legend_corr_string(pstate)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------

pro OnSelect_corr_Elementx, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).corr_x = event.index

draw_corrs, pstate

s = legend_corr_string(pstate)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------

pro OnSelect_corr_Elementy, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).corr_y = event.index

draw_corrs, pstate

s = legend_corr_string(pstate)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------

pro OnSelect_corr_Xlog, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).logx = float(event.select)
p = (*pstate).pmark
conc_corr_to_xy, pstate, (*p).cx,(*p).cy, x,y, /nozoom
(*p).x = x
(*p).y = y
draw_corrs, pstate

done:
end

;-----------------------------------------------------------------

pro OnSelect_corr_Ylog, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).logy = float(event.select)
p = (*pstate).pmark
conc_corr_to_xy, pstate, (*p).cx,(*p).cy, x,y, /nozoom
(*p).x = x
(*p).y = y
draw_corrs, pstate

done:
end

;-----------------------------------------------------------------

pro OnSelect_corr_Highlight, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).highlight = event.select
draw_corrs, pstate

done:
end

;-----------------------------------------------------------------
; TLB_SIZE_EVENTS Callback Procedure.
;
;   {WIDGET_BASE, ID:0L, TOP:0L, HANDLER:0L, X:0, Y:0 }
;
;   The X and Y fields return the new width of the base, not
;       including any frame provided by the window manager.
;-----------------------------------------------------------------

pro OnSize_corr, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

case !version.os_family of
	'MacOS': begin
		draw_trim = 15
		scr_trim = 21
		end
	'unix': begin
		draw_trim = 0
		scr_trim = 15
		end
	else: begin
		draw_trim = 0
		scr_trim = 15
		end
endcase

w = ((event.x - (*pstate).scr_xsize_off) > (256 + scr_trim)) < ((*pstate).width + scr_trim)
h = ((event.y - (*pstate).scr_ysize_off) > (64 + scr_trim)) < ((*pstate).height + scr_trim)

(*pstate).w = w
(*pstate).h = h
map_corr_help, pstate

totx = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
toty = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top

w = ((event.x - (*pstate).scr_xsize_off) > (256 + scr_trim)) < (totx + scr_trim)
h = ((event.y - (*pstate).scr_ysize_off) > (64 + scr_trim)) < (toty + scr_trim)

; Note that setting "draw_xsize=(*pstate).width, draw_ysize=(*pstate).height" is redundant,
; but necessary to keep the scrolling window working ...

widget_control, (*pstate).draw2, scr_xsize=w, scr_ysize=h, $
			draw_xsize=totx+draw_trim, draw_ysize=toty+draw_trim

widget_control, (*pstate).drawX, scr_xsize=w, scr_ysize=h
widget_control, (*pstate).drawY, scr_xsize=w, scr_ysize=h

(*pstate).w = w
(*pstate).h = h
(*pstate).xview = w - (*pstate).dw
(*pstate).yview = h - (*pstate).dh

(*pstate).position = [float((*pstate).margin.low)/float(totx), float((*pstate).margin.bottom)/float(toty), $
		1.-(float((*pstate).margin.high)/float(totx)), 1.-(float((*pstate).margin.top)/float(toty))]

draw_corrs, pstate
return
end

;-----------------------------------------------------------------
; Tracking Callback Procedure.
;
;   {WIDGET_TRACKING, ID:0L, TOP:0L, HANDLER:0L, ENTER:0 }
;
;   ENTER is 1 if the tracking event is an entry event, and 0 if it
;       is an exit event.
;-----------------------------------------------------------------

pro OnTracking_corr, Event

COMPILE_OPT STRICTARR
widget_control, event.id, get_uvalue=message
if n_elements(message) lt 1 then return
if size(message,/tname) ne 'STRING' then return

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
if widget_info( (*pstate).help, /valid) eq 0 then goto, done

if event.enter eq 1 then begin
	widget_control, (*pstate).help, set_value=message
endif else begin
	s = legend_corr_string(pstate)
	widget_control, (*pstate).help, set_value=s
endelse

done:
end
;
;-----------------------------------------------------------------
; VIEWPORT_EVENTS Callback Procedure.
;
;   {WIDGET_DRAW, ID:0L, TOP:0L, HANDLER:0L, TYPE: 0, X:0, Y:0,
;       PRESS:0B, RELEASE:0B, CLICKS:0}
;
;   TYPE returns a value that describes the type of draw widget
;       interaction that generated an event: 0 - Button Press, 1 -
;       Button Release, 2 - Motion, 3 - Viewport Moved, 4 -
;       Visibility Changed (Expose)
;
;-----------------------------------------------------------------

pro OnViewport_corr, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).xlow = event.x
(*pstate).ylow = event.y
end

;-----------------------------------------------------------------

pro PostCreate_corr_Base, wWidget, Help1_Base, Help2_Base, $
					parent=parent, path=path, _EXTRA=_VWBExtra_, $
					pimages=pimages, qregion=qregion, width=pixx, height=pixy, $
					margin=margin

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
		warning,'Postcreate_corr_base',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(parent) lt 1 then parent=0
if n_elements(path) lt 1 then path=''
if n_elements(pixx) lt 1 then pixx=256
if n_elements(pixy) lt 1 then pixy=256
if n_elements(margin) lt 1 then margin={low:80, high:20, bottom:50, top:20}

totx = pixx + margin.low + margin.high
toty = pixy + margin.bottom + margin.top
position = [float(margin.low)/float(totx), float(margin.bottom)/float(toty), $
			1.-(float(margin.high)/float(totx)), 1.-(float(margin.top)/float(toty))]
print,position
w = pixx
h = pixy
vx = pixx
vy = pixy

mark = {present:0, x:fltarr(11), y:fltarr(11), cx:fltarr(11), cy:fltarr(11)  }	; spline 10 marker

state = {	p:			pimages, $		; pointer to Images pointer array
			file:		'', $			; current corr filename
			path:		ptr_new(path), $ ; current path

			b:			ptr_new(), $	; pointer to screen corr byte array
			q:			qregion, $		; Q region mask array
			image_x:	ptr_new(), $	; pointer to X image (pre-smoothed)
			image_y:	ptr_new(), $	; pointer to Y image (pre-smoothed)
			qc:			ptr_new(), $	; qc mask array to return to Image
			pspline:	ptr_new(), $	; pointer to struct for notify 'corr-analyze-spline'

			pmark:		ptr_new( mark, /no_copy), $	; spline 10 marker
			id:			0, $			; id of point being moved

			pexport:	ptr_new(/allocate_heap), $	; heap for export parameters

			wid2:		0L, $			; draw 2 window id		draw area
			draw2:		0L, $			; draw 2 widget ID
			pix:		0L, $			; pixmap window id
			pix2:		0L, $			; pixmap 2 window id

			widX:		0L, $			; draw X window id		histogram areas
			drawX:		0L, $			; draw X widget ID
			widY:		0L, $			; draw Y window id
			drawY:		0L, $			; draw Y widget ID
			count:		0, $			; count of realized draw widgets

			element_idx:	0L, $		; element droplist ID
			element_idy:	0L, $		; element droplist ID
			help1:		0L, $			; help 1 text widget ID
			help2:		0L, $			; help 2 text widget ID
			help:		0L, $			; current help text widget ID
			help1_base:	Help1_Base, $	; base to map for help 1
			help2_base:	Help2_Base, $	; base to map for help 2
			Low_slider:	0L, $			; Low Z scale slider ID
			High_slider: 0L, $			; High Z scale slider ID
			X_slider:	0L, $			; X low-end slider ID
			Y_slider: 	0L, $			; Y low-end slider ID
			X_slider_top:	0L, $		; X high-end slider ID
			Y_slider_top: 	0L, $		; Y high-end slider ID
			X_smooth_slider:	0L, $	; X smooth slider ID
			Y_smooth_slider:	0L, $	; Y smooth slider ID
			xlog_button:	0L, $		; X log toggle button ID
			ylog_button:	0L, $		; Y log toggle button ID
			highlight_button:	0L, $	; highlight toggle button ID

			corr_x:		0, $			; current corr displayed
			corr_y:		0, $			; current corr displayed
			logx:		0, $			; X log scale
			logy:		0, $			; Y log scale
			highlight:	0, $			; highlight selected pixels
			low:		0, $			; low Z %
			high:		100, $			; high Z %
			Zaxis:		2, $			; Z axis linear (0), sqrt (1), Log 92)
			xbottom:	0, $			; low  X display
			ybottom:	0, $			; low  Y display
			xtop:		100, $			; high  X display
			ytop:		100, $			; high  Y display
			x_smooth:	1, $			; pre-smooth for X display
			y_smooth:	1, $			; pre-smooth for Y display
			maxx:		0.0, $			; maxmimum of X data
			maxy:		0.0, $			; maxmimum of X data
			minx:		0.0, $			; minimum of Y data
			miny:		0.0, $			; minimum of Y data
			range:		{x:[0.0,0.0], y:[0.0,0.0]}, $	; range actually displayed
			zoom:		0, $			; zoom factor
			left_button:	0, $		; flags left mouse button
			right_button:	0, $		; right mouse

			width:		w, $			; width of pixmap		(pixels)
			height:		h, $			; height of pixmap		(pixels)
			owidth:		w, $			; original width of pixmap		(pixels)
			oheight:	h, $			; original height of pixmap		(pixels)
			margin:		margin, $		; margins around pixmap area in plot
			position:	position, $		; plot window position
			w:			0, $			; scr width of draw
			h:			0, $			; scr height of draw

			xview:		vx, $			; X view width			(pixels)
			yview:		vy, $			; Y view width			(pixels)
			xlow:		0, $			; X view low			(pixels)
			ylow:		0, $			; Y view low			(pixels)

			dw:			0, $			; width difference/border
			dh:			0, $			; height difference/border
			scr_xsize_off:	0, $		; resize offsets (see also map_help
			scr_ysize_off:	0, $
			tlb_width:	0, $			; tlb scr_xsize (initial)
			tlb_height:	0 }				; tlb scr_ysize (initial)

finish:
	top = tlb_id( wWidget)
	child = widget_info( top, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	return
end

;-----------------------------------------------------------------

pro PostCreate_corr, wWidget, _EXTRA=_VWBExtra_

widget_control, wWidget, set_draw_view=[0,0]
end

;-----------------------------------------------------------------
;
; Empty stub procedure used for autoloading.
;

pro corr_eventcb
end
