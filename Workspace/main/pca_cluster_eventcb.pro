;
; IDL Event Callback Procedures
; pca_cluster_eventcb
;
;-----------------------------------------------------------------

pro pca_cluster_Analyze_PCA, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_good(p) eq 0 then goto, done

(*pstate).class_on = 0				; disable the green class overlay

Analyze_pca, pstate, error=err
if err then goto, done

draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro pca_cluster_Analyze_Clusters, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_good(p) eq 0 then goto, done

Analyze_clusters, pstate, error=err
if err then goto, done

done:
end

;-----------------------------------------------------------------

pro pca_cluster_plot_eigen, event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_good(p) eq 0 then goto, done
pca = (*pstate).pca
if ptr_good(pca.presults) eq 0 then goto, done
els = *(*p).el

xsize = 400
ysize = 300
n = n_elements(*pca.peigen)
window,0,xsize=xsize,ysize=ysize
print,'Eigen[0:'+str_tidy(n)+']=',(*pca.peigen)[0:n-1]
!p.title='Eigen values / Variance'
!x.title = 'Component #'
!y.title = ''
mx = max(*pca.peigen)
plot, *pca.peigen, yrange=[0.001,1.]*mx, /ylog

window,1,xsize=xsize,ysize=ysize
print,'Coeff[*,0]=',(*pca.pcoeff)[*,0]
!p.title='Coeff 0'
;!x.title = 'E (keV)'
;x = *(*p).pz_coords
!x.title = 'Index'
x = indgen(n_elements((*pca.pcoeff)[*,0]))
plot,x,(*pca.pcoeff)[*,0]

window,2,xsize=xsize,ysize=ysize
print,'Coeff[*,1]=',(*pca.pcoeff)[*,1]
!p.title='Coeff 1'
plot,x,(*pca.pcoeff)[*,1]

window,3,xsize=xsize,ysize=ysize
print,'Coeff[*,2]=',(*pca.pcoeff)[*,2]
!p.title='Coeff 2'
plot,x,(*pca.pcoeff)[*,2]

window,4,xsize=xsize,ysize=ysize
print,'Coeff[*,3]=',(*pca.pcoeff)[*,3]
!p.title='Coeff 3'
plot,x,(*pca.pcoeff)[*,3]

for i=4,n_elements((*pca.pcoeff)[0,*])-1 do begin
	print,'Coeff[*,'+str_tidy(i)+']=',(*pca.pcoeff)[*,i]
endfor

scale = 20.
nx = n_elements( (*pca.pcoeff)[0,*])
ny = n_elements( (*pca.pcoeff)[*,0])
window,5, xsize=scale*nx + 80, ysize=scale*(ny+1)
tv, 16B + bytscl(  congrid( transpose( abs(*pca.pcoeff)), nx*scale,ny*scale), top=99), 80,scale

!p.charsize = 1.0
for i=0, n_elements( *pca.pqel)-1 do begin
	xyouts, 5, 2+scale*i + scale, str_tidy(i), /device, color=spec_colour('white'), align=0
	xyouts, 25, 2+scale*i + scale, els[ (*pca.pqel)[i] ], /device, color=spec_colour('white'), align=0
endfor
for i=0, nx-1 do begin
	xyouts, 80+scale*i+2, 2, str_tidy(i), /device, color=spec_colour('white'), align=0
endfor

done:
	return
end

;-----------------------------------------------------------------

pro pca_cluster_select_Cluster, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_good(p) eq 0 then goto, done
if ptr_valid((*pstate).pmark) eq 0 then goto, done
pca = (*pstate).pca
if ptr_good(pca.presults) eq 0 then goto, done
if ptr_good(pca.pclass) eq 0 then goto, done

xanes_stack_test, p, xanes, n_el, el, el_xanes
el = 'PC ' + strtrim(string(indgen(n_el)),2)

qp = where( *pca.pclass eq (*pstate).class, nqp)				; index into PCA results list

if nqp gt 0 then begin
	qc = (*(*pstate).pca.pq)[qp]								; index into original image space

	if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
	(*pstate).qc = ptr_new( qc, /no_copy)

	if ptr_valid( (*pstate).pspline) then ptr_free, (*pstate).pspline
	(*pstate).pspline = ptr_new( {elx:el[(*pstate).pca_cluster_x], ely:el[(*pstate).pca_cluster_y], $
						qc:(*pstate).qc, pmark:(*pstate).pmark }, /no_copy)

	notify, 'corr-analyze-spline', (*pstate).pspline, from=(*pstate).tlb
endif

done:
	draw_pca_clusters, pstate
	return
end

;-----------------------------------------------------------------

pro pca_cluster_Analyze_Spline, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_good(p) eq 0 then goto, done
if ptr_valid((*pstate).pmark) eq 0 then goto, done

xanes_stack_test, p, xanes, n_el, el, el_xanes
el = 'PC ' + strtrim(string(indgen(n_el)),2)

; qc	index in image apace
; qpca	index in local data points list

qc = make_pca_cluster_mask( pstate, pca_index=qp)
*(*pstate).qpca = qp											; save this

if qc[0] ne -1 then begin
	if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
	(*pstate).qc = ptr_new( qc, /no_copy)

	if ptr_valid( (*pstate).pspline) then ptr_free, (*pstate).pspline
	(*pstate).pspline = ptr_new( {elx:el[(*pstate).pca_cluster_x], ely:el[(*pstate).pca_cluster_y], $
						qc:(*pstate).qc, pmark:(*pstate).pmark }, /no_copy)

	notify, 'corr-analyze-spline', (*pstate).pspline, from=event.top
endif

done:
	draw_pca_clusters, pstate
	return
end

;-----------------------------------------------------------------

pro pca_cluster_Clear_Spline, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

clear_pca_cluster_all_markers, pstate

notify, 'corr-analyze-clear', from=event.top
end

;-----------------------------------------------------------------

pro pca_cluster_Clone, Event, n

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
		warning,'pca_cluster_clone',['IDL run-time error caught.', '', $
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
	pca_cluster, group_leader=event.top, TLB=tlb, path=path, pimages=(*pstate).p, qregion=(*pstate).q, $
				xoffset=xoffset[i], yoffset=yoffset[i]

	register_notify, event.top, $
 				['images', $					; new images loaded somewhere
		  		'images-changed', $				; pass on notify of images changed
				'path', $						; new path
				'image-region-select', $		; region selected
				'image-analyze-q', $			; new q array
				'image-analyze-all-clear', $	; clear q array
				'corr-analyze-clear', $			; pass on notify of qc destined for image
				'corr-analyze-spline', $		; pass on notify of qc cleared destined for image
		  		'corr-display' $				; pass on notify of corr changed (e.g. colours)
				], from=tlb
endfor
end

;-----------------------------------------------------------------

pro pca_cluster_Colours, Event, n

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(n) gt 0 then begin
	loadct, n, bottom=16, ncolors=100
endif else begin
	xloadct, group=event.top, /modal, bottom=16, ncolors=100
endelse

load_spec_colours
draw_pca_clusters, pstate
notify, 'corr-display', from=event.top
end

;-----------------------------------------------------------------

pro pca_cluster_Exit, Event

print,'pca_cluster_Exit ...'
OnKill_pca_cluster, event

end

;-----------------------------------------------------------------

pro pca_cluster_Export, Event, cgm=cgm, wmf=wmf, eps=eps

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if n_elements(cgm) eq 0 then cgm=0
if n_elements(wmf) eq 0 then wmf=0
if n_elements(eps) eq 0 then eps=0
if ptr_valid( (*pstate).p) eq 0 then goto, done

p = (*pstate).p
if ptr_good( p) eq 0 then return
xanes_stack_test, p, xanes, n_el, el, el_xanes, z_found=z_found

select = plot_pca_cluster_select( event.top, cgm=cgm, wmf=wmf, eps=eps, old_select=*(*pstate).pexport, $
					path=*(*pstate).path, pca_cluster_pstate=pstate )
if select.error then goto, done

;goto, done     ; use this bypass if testing plot_image_select without /modal, etc.

name = 'Save Export Association as '

if select.plot.type eq 'CGM' then begin
	file = find_file2( (*p).file)
	path = extract_path( file[0])
	if lenchr(path) eq 0 then path = *(*pstate).path
    file = strip_path( strip_file_ext( (*p).file)) + '-pca_cluster' + '.cgm'

    file = file_requester( /write, filter='*.cgm', path=path, $
         file=file, title=name+'CGM', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
    file = strip_file_ext(file) + '.cgm'
    file = strip_file_m( file, /element)

    plot_pca_cluster, pstate, /cgm, file=file, options=select.plot

endif else if select.plot.type eq 'METAFILE' then begin
	file = find_file2( (*p).file)
	path = extract_path( file[0])
	if lenchr(path) eq 0 then path = *(*pstate).path
    file = strip_path( strip_file_ext( (*p).file)) + '-pca_cluster' + '.wmf'

    file = file_requester( /write, filter='*.wmf', path=path, $
         file=file, title=name+'WMF', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
    file = strip_file_ext(file) + '.cgm'
    file = strip_file_m( file, /element)

    plot_pca_cluster, pstate, /wmf, file=file, options=select.plot

endif else if select.plot.type eq 'PS' then begin
	file = find_file2( (*p).file)
	path = extract_path( file[0])
	if lenchr(path) eq 0 then path = *(*pstate).path
    file = strip_path( strip_file_ext( (*p).file)) + '-pca_cluster' + '.eps'

    file = file_requester( /write, filter='*.eps', path=path, $
         file=file, title=name+'EPS', group=event.top, /fix_filter)
    if strlen(file) lt 1 then goto, done
    file = strip_file_ext(file) + '.cgm'
    file = strip_file_m( file, /element)

    plot_pca_cluster, pstate, /eps, file=file, options=select.plot

endif else begin

    plot_pca_cluster, pstate, options=select.plot
endelse

*(*pstate).pexport = {plot:select.plot}

done:
end

;-----------------------------------------------------------------

pro pca_cluster_Invert_Colours, Event

COMPILE_OPT STRICTARR

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

tvlct, ro,go,bo, /get
rr = reverse(ro[16:115])
gg = reverse(go[16:115])
bb = reverse(bo[16:115])
tvlct, rr,gg,bb, 16

load_spec_colours
draw_pca_clusters, pstate
notify, 'corr-display', from=event.top
end

;-----------------------------------------------------------------

pro pca_cluster_Linear_Luminance, Event

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
draw_pca_clusters, pstate
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

pro pca_cluster_Save_GIF, Event, png=png

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
s = strtrim( el[(*pstate).pca_cluster_x],2) + '-' + strtrim( el[(*pstate).pca_cluster_y],2)

file = find_file2( (*p).file)
path = extract_path( file[0])
if lenchr(path) eq 0 then path = *(*pstate).path
gif_file = strip_path( strip_file_ext( (*p).file)) + '-' + s + '-pca_cluster' + ext

F = file_requester( /write, filter = '*'+ext, path=path, $
			title=title, file = gif_file, group=event.top, fix_filter=1)
if F ne '' then begin
;	widget_control, /hourglass

	b = make_pca_cluster_tvb( pstate, (*pstate).pca_cluster_x, (*pstate).pca_cluster_y)
	window, /free, xsize=n_elements(b[*,0]), ysize=n_elements(b[0,*]), /pixmap
	tpix = !d.window
	tv, b
	plot_pca_cluster_spline, pstate, /wide
	xyouts,0.8,0.02,el[(*pstate).pca_cluster_x],charsize=1.8,charthick=8.0,color=16,/norm
	xyouts,0.01,0.8,el[(*pstate).pca_cluster_y],charsize=1.8,charthick=8.0,color=16,/norm
	xyouts,0.8,0.02,el[(*pstate).pca_cluster_x],charsize=1.8,charthick=1.7,color=spec_colour('green'),/norm
	xyouts,0.01,0.8,el[(*pstate).pca_cluster_y],charsize=1.8,charthick=1.7,color=spec_colour('green'),/norm
	b = color_quan( tvrd(true=3), 3, rcol,gcol,bcol, colors=128)
	if n_elements(b) gt 1 then begin
		if png then begin
			write_png, F, b, rcol,gcol,bcol
		endif else begin
			write_gif, F, b, rcol,gcol,bcol
		endelse
	endif else begin
		print,'pca_cluster_save_GIF:  Nothing to save!  No TV byte data.'
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

pro OnButton_pca_cluster_Full, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then return

set_pca_cluster_view, pstate, event.top, /full
end
;
;-----------------------------------------------------------------

pro OnButton_pca_cluster_Zoom_In, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done

set_pca_cluster_view, pstate, event.top, zoom=+1

done:
end
;
;-----------------------------------------------------------------

pro OnButton_pca_cluster_Zoom_Out, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) eq 0 then goto, done

set_pca_cluster_view, pstate, event.top, zoom=-1

done:
end
;
;-----------------------------------------------------------------
; Note: operation here depends on whether this is a clone or not.

pro OnDestroy_pca_cluster, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

print,'OnDestroy_pca_cluster: Cleanup pixemaps ...'
if n_elements(pstate) eq 0 then return
if ptr_valid(pstate) eq 0 then return
if size(*pstate,/tname) ne 'STRUCT' then return

	free_pca_cluster_state, pstate

return
end

;-----------------------------------------------------------------

pro OnKill_pca_cluster, Event

print,'OnKill_pca_cluster: destroy widget tree ...'
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

pro OnMove_pca_cluster_Low, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).Low = float(event.value)
draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_pca_cluster_High, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).High = float(event.value)
draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_pca_cluster_Y, Event, high=high

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
conc_pca_cluster_to_xy, pstate, (*p).cx,(*p).cy, x,y, /nozoom
(*p).x = x
(*p).y = y
draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_pca_cluster_X, Event, high=high

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
conc_pca_cluster_to_xy, pstate, (*p).cx,(*p).cy, x,y, /nozoom
(*p).x = x
(*p).y = y
draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_pca_cluster_border, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).border = event.index

done:
end

;-----------------------------------------------------------------

pro OnMove_pca_cluster_smooth, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).smooth = event.index

done:
end

;-----------------------------------------------------------------

pro OnMove_pca_cluster_threshold, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).threshold = event.index
;draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_pca_cluster_average_high, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).average_high = event.index
;draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_pca_cluster_average_low, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).average_low = event.index
;draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_pca_cluster_class_max, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).class_max = event.index
;draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_pca_cluster_class, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).class = event.index
(*pstate).class_on = 1
draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro OnMove_pca_cluster_Components, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).Components = event.index

done:
end

;-----------------------------------------------------------------

pro OnMove_pca_cluster_source, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).source = event.index

widget_control, (*pstate).components_base, map=((*pstate).source eq 1)

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

pro OnNotify_pca_cluster, Event

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
		warning,'OnNotify_pca_cluster',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

;print,'OnNotify_pca_cluster: tag = ',event.tag

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

;print,'******* pca_cluster_Evenctcb: notify: tag=',event.tag,' from=',event.from,' to=',event.top

case event.tag of

	'corr-display': begin	; returned from 'pca_cluster_select'

		; print,' Notified of display changed.'
		;draw_pca_clusters, pstate
		end

	'image-display': begin	; returned from 'image'

		; print,' Notified of display changed.'
		;draw_pca_clusters, pstate
		end

	'path': begin
		if ptr_valid( event.pointer) eq 0 then goto, finish
		print,'pca_cluster: new path = ',(*event.pointer)
		*(*pstate).path = (*event.pointer)
		end

	'image-region-select': begin							; returned from 'pca_cluster_table'

		if ptr_valid( event.pointer) then begin
			p = (*event.pointer).pregion
			if ptr_valid(p) then begin
				if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q
				if (*p).mode eq 1 then begin
					pi = (*pstate).p
					xanes_stack_test, pi, xanes, n_el, el, el_xanes
					el = 'PC ' + strtrim(string(indgen(n_el)),2)
					if ptr_valid( (*pstate).pmark) then ptr_free, (*pstate).pmark
					if ptr_valid( (*p).pmark[0]) then (*pstate).pmark = ptr_new( *(*p).pmark[0])
					q = where( (*p).elx eq el)
					if q[0] ne -1 then begin
						(*pstate).pca_cluster_x = q[0]
						widget_control, (*pstate).element_idx, set_combobox_select=(*pstate).pca_cluster_x
					endif
					q = where( (*p).ely eq el)
					if q[0] ne -1 then begin
						(*pstate).pca_cluster_y = q[0]
						widget_control, (*pstate).element_idy, set_combobox_select=(*pstate).pca_cluster_y
					endif
					t = (*(*pstate).pca.qref)[*(*p).q]
					q = where( t ge 0, nq)
					if nq gt 0 then begin
						*(*pstate).qpca = t[q]
					endif else *(*pstate).qpca = -1					
				endif else begin
					if ptr_valid( (*p).q) then (*pstate).q = ptr_new( *(*p).q)
				endelse
				draw_pca_clusters, pstate
			endif
		endif
		end

	'corr-analyze-clear': begin								; returned from other 'pca_cluster'

		clear_pca_cluster_spline, pstate
   		end

	'images-changed': begin											; returned from other apps

		p = event.pointer									; pointer to new 'images'
		if ptr_valid(p) eq 0 then goto, finish

		(*pstate).p = p
		(*pstate).file = (*p).file
		if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q

		set_pca_cluster_view, pstate, event.top
		end

;	'images': begin											; returned from other apps

;		p = event.pointer									; pointer to new 'images'
;		if ptr_valid(p) eq 0 then goto, finish

;		(*pstate).p = p
;		(*pstate).file = (*p).file
;		if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q

;		set_pca_cluster_view, pstate, event.top
;		end

	'image-analyze-q': begin								; returned from 'image' clone

		if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
		if ptr_valid( event.pointer) then begin
			pq = *event.pointer
			if ptr_valid(pq.q) then (*pstate).q = ptr_new( *pq.q)
			draw_pca_clusters, pstate
		endif
		end

	'image-analyze-all-clear': begin						; returned from 'image' clone

		if ptr_valid((*pstate).q) then ptr_free, (*pstate).q
		;draw_pca_clusters, pstate
		end

	else: begin
	;	print,'OnNotify_pca_cluster: unknown tag = ',event.tag
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

pro OnRealize_pca_cluster_Help1, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).help1 = wWidget
(*pstate).help = wWidget
end
;
;-----------------------------------------------------------------

pro OnRealize_pca_cluster_Help2, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).help2 = wWidget
end
;
;-----------------------------------------------------------------

pro OnRealize_pca_cluster, wWidget

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

;if ptr_valid( (*pstate).p) then begin
;	if n_elements( *(*pstate).p) gt 0 then begin
;		draw_pca_clusters, pstate
;	endif
;endif
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_Border_Slider, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_good(p) eq 0 then return

(*pstate).border_Slider = wWidget
(*pstate).border = 2
widget_control, wWidget, set_combobox_select = 2
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_Elementx, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_good(p) eq 0 then return
xanes_stack_test, p, xanes, n_el, el, el_xanes
el = 'PC ' + strtrim(string(indgen(n_el)),2)

(*pstate).element_idx = wWidget
if ptr_valid( (*pstate).p) then begin
	if n_elements( *p) gt 0 then begin
		widget_control, wWidget, set_value=el
	endif
endif
widget_control, wWidget, set_combobox_select = (*pstate).pca_cluster_x
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_Elementy, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_good(p) eq 0 then return
xanes_stack_test, p, xanes, n_el, el, el_xanes
el = 'PC ' + strtrim(string(indgen(n_el)),2)

(*pstate).element_idy = wWidget
if ptr_valid( p) then begin
	if n_elements( *p) gt 0 then begin
		widget_control, wWidget, set_value=el
	endif
endif
widget_control, wWidget, set_combobox_select = (*pstate).pca_cluster_y
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_Low_Slider, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Low_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_High_Slider, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).High_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_Y_Slider, wWidget

; print,'onrealize_bottom slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Y_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_Y_Slider_top, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Y_slider_top = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_X_Slider, wWidget

; print,'onrealize_top slider ...'
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).X_slider = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_X_Slider_top, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).X_slider_top = wWidget
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_threshold_Slider, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).threshold_slider = wWidget
widget_control, wWidget, set_combobox_select = (*pstate).threshold
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_average_high_Slider, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).average_high_Slider = wWidget
widget_control, wWidget, set_combobox_select = (*pstate).average_high
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_average_low_Slider, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).average_low_Slider = wWidget
widget_control, wWidget, set_combobox_select = (*pstate).average_low
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_smooth_Slider, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).smooth_slider = wWidget
widget_control, wWidget, set_combobox_select = (*pstate).smooth
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_source, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).cluster_source_ID = wWidget
(*pstate).source = 1
widget_control, wWidget, set_combobox_select = (*pstate).source
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_class_max_Slider, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).class_max_slider = wWidget
widget_control, wWidget, set_combobox_select = (*pstate).class_max
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_class_Slider, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).class_slider = wWidget
widget_control, wWidget, set_combobox_select = (*pstate).class
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_components_Slider, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
parent = widget_info( wWidget, /parent)
widget_control, child, get_uvalue=pstate

(*pstate).components_Slider = wWidget
(*pstate).components_base = parent
(*pstate).components = 5
widget_control, wWidget, set_combobox_select = (*pstate).components
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_Xlog, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Xlog_button = wWidget
widget_control, wWidget, set_button=0
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_Ylog, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Ylog_button = wWidget
widget_control, wWidget, set_button=0
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_Zaxis, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_good(p) eq 0 then return

(*pstate).Zaxis_mode = wWidget
(*pstate).Zaxis = 1
widget_control, wWidget, set_combobox_select = 1
end

;-----------------------------------------------------------------

pro OnRealize_pca_cluster_normalize, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).normalize_button = wWidget
widget_control, wWidget, set_button=1
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

pro OnSelect_pca_cluster_Zaxis, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).Zaxis = event.index

draw_pca_clusters, pstate

s = legend_pca_cluster_string(pstate)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------

pro OnSelect_pca_cluster_Elementx, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_valid( p) eq 0 then return
pca = (*pstate).pca
if ptr_good(pca.presults) eq 0 then return

nx = n_elements( (*pca.presults)[0,*])

(*pstate).pca_cluster_x = event.index < (nx-1)

draw_pca_clusters, pstate

s = legend_pca_cluster_string(pstate)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------

pro OnSelect_pca_cluster_Elementy, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate
p = (*pstate).p
if ptr_valid( p) eq 0 then return
pca = (*pstate).pca
if ptr_good(pca.presults) eq 0 then return

ny = n_elements( (*pca.presults)[0,*])

(*pstate).pca_cluster_y = event.index < (ny-1)

draw_pca_clusters, pstate

s = legend_pca_cluster_string(pstate)
widget_control, (*pstate).help, set_value=s

done:
end

;-----------------------------------------------------------------

pro OnSelect_pca_cluster_Xlog, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).logx = float(event.select)
p = (*pstate).pmark
conc_pca_cluster_to_xy, pstate, (*p).cx,(*p).cy, x,y, /nozoom
(*p).x = x
(*p).y = y
draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro OnSelect_pca_cluster_Ylog, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).logy = float(event.select)
p = (*pstate).pmark
conc_pca_cluster_to_xy, pstate, (*p).cx,(*p).cy, x,y, /nozoom
(*p).x = x
(*p).y = y
draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro OnSelect_pca_cluster_normalize, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).normalize = event.select

done:
end

;-----------------------------------------------------------------

pro OnSelect_pca_cluster_highlight, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).highlight[event.value] = event.select
draw_pca_clusters, pstate

done:
end

;-----------------------------------------------------------------

pro OnSelect_pca_cluster_class_show, Event

COMPILE_OPT STRICTARR
child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

p = (*pstate).p
if ptr_valid( p) eq 0 then goto, done

(*pstate).class_on = 1
draw_pca_clusters, pstate

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

pro OnSize_pca_cluster, Event

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
map_pca_cluster_help, pstate

totx = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
toty = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top

w = ((event.x - (*pstate).scr_xsize_off) > (256 + scr_trim)) < (totx + scr_trim)
h = ((event.y - (*pstate).scr_ysize_off) > (64 + scr_trim)) < (toty + scr_trim)

; Note that setting "draw_xsize=(*pstate).width, draw_ysize=(*pstate).height" is redundant,
; but necessary to keep the scrolling window working ...

widget_control, (*pstate).draw2, scr_xsize=w, scr_ysize=h, $
			draw_xsize=totx+draw_trim, draw_ysize=toty+draw_trim

(*pstate).w = w
(*pstate).h = h
(*pstate).xview = w - (*pstate).dw
(*pstate).yview = h - (*pstate).dh

(*pstate).position = [float((*pstate).margin.low)/float(totx), float((*pstate).margin.bottom)/float(toty), $
		1.-(float((*pstate).margin.high)/float(totx)), 1.-(float((*pstate).margin.top)/float(toty))]
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

pro OnTracking_pca_cluster, Event

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
	s = legend_pca_cluster_string(pstate)
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

pro OnViewport_pca_cluster, Event

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

(*pstate).xlow = event.x
(*pstate).ylow = event.y
end

;-----------------------------------------------------------------

pro PostCreate_pca_cluster_Base, wWidget, tools, $
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
		warning,'Postcreate_pca_cluster_base',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(parent) lt 1 then parent=0L
if n_elements(tlb) lt 1 then tlb=0L
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
			file:		'', $			; current pca_cluster filename
			path:		ptr_new(path), $ ; current path

			b:			ptr_new(), $	; pointer to screen pca_cluster byte array
			q:			qregion, $		; Q region mask array
			image_x:	ptr_new(), $	; pointer to X image (pre-smoothed)
			image_y:	ptr_new(), $	; pointer to Y image (pre-smoothed)
			qpca:		ptr_new(/allocate_heap), $	; qc mask array as index into cut-down PCA image data
			qc:			ptr_new(), $	; qc mask array to return to Image
			pspline:	ptr_new(), $	; pointer to struct for notify 'corr-analyze-spline'

			pmark:		ptr_new( mark, /no_copy), $	; spline 10 marker
			id:			0, $			; id of point being moved

			pexport:	ptr_new(/allocate_heap), $	; heap for export parameters

			tlb:		0L, $			; Draw TLB ID
			tools:		0L, $			; Tool bar TLB ID
			wid2:		0L, $			; draw 2 window id
			draw2:		0L, $			; draw 2 widget ID
			pix:		0L, $			; pixmap window id
			pix2:		0L, $			; pixmap 2 window id

			element_idx:	0L, $		; element droplist ID
			element_idy:	0L, $		; element droplist ID
			help1:		0L, $			; help 1 text widget ID
			help:		0L, $			; current help text widget ID
			Low_slider:	0L, $			; Low Z scale slider ID
			High_slider: 0L, $			; High Z scale slider ID
			X_slider:	0L, $			; X low-end slider ID
			Y_slider: 	0L, $			; Y low-end slider ID
			X_slider_top:	0L, $		; X high-end slider ID
			Y_slider_top: 	0L, $		; Y high-end slider ID
			xlog_button:	0L, $		; X log toggle button ID
			ylog_button:	0L, $		; Y log toggle button ID
			Zaxis_mode:		0L, $		; Z axis mode ID
			threshold_slider:	0L, $	; threshold slider ID
			normalize_button:	0L, $	; PCA normalize toggle button ID
			average_high_slider:	0L, $	; average spectrum high-E slider ID
			average_low_slider:	0L, $	; average spectrum low-E slider ID
			smooth_slider:	0L, $		; smooth images slider ID
			class_max_slider: 0L, $		; maximum number of classes to look for slider ID
			components_slider: 0L, $	; number of PCA components to use for cluster analysis slider ID
			components_base: 0L, $		; compoents base ID
			class_slider: 0L, $			; index of class to select pixels for slider ID
			border_slider: 0L, $		; number of border pixels to ignore slider ID
			cluster_source_ID: 0L, $	; cluster source data mode ID
			
			pca_cluster_x:		0, $	; current pca_cluster displayed
			pca_cluster_y:		1, $	; current pca_cluster displayed
			logx:		0, $			; X log scale
			logy:		0, $			; Y log scale
			low:		0, $			; low Z %
			high:		100, $			; high Z %
			Zaxis:		0, $			; Z axis linear (0), sqrt (1), log (2)
			xbottom:	0, $			; low  X display
			ybottom:	0, $			; low  Y display
			xtop:		100, $			; high  X display
			ytop:		100, $			; high  Y display
			threshold:	10, $			; threshold for PCA
			normalize:	1, $			; enable nornalize in PCA
			average_low:	10, $		; average these low-E channels
			average_high:	10, $		; average these high E channels
			smooth:		0, $			; pre-smooth for images prior to PCA
			class_max:	5, $			; max number of classes to look for
			components: 5, $			; number of PCA components to use for cluster analysis 
			class:		0, $			; index of class to select pixels for
			class_on:	0, $			; indicates the display of class pixel overlay in green
			border:		0, $			; number of border pixels in margin to ignore from PCA analysis 
			source:		0, $			; selected source of data for clustering
			pca: {	presults:	ptr_new(/allocate_heap), $	; pca results (for q)
					pimage:		ptr_new(/allocate_heap), $	; images for sub-set (smoothed)
					pq:			ptr_new(/allocate_heap), $	; valid pixels in full image q index array
					pqs:		ptr_new(/allocate_heap), $	; valid pixels in cut-down image q index array
					qref:		ptr_new(/allocate_heap), $	; cross reference from normal pixels to subset pixel index
					perror:		ptr_new(/allocate_heap), $	; errors for sub-set (smoothed)
					pqse:		ptr_new(/allocate_heap), $	; valid pixels in cut-down err at half resolution
					pqel:		ptr_new(/allocate_heap), $	; valid elements q index array
					pclass:		ptr_new(/allocate_heap), $	; cluster class results (for q)
					source:		0, $						; flags source of clusters as PCA (1) or pixel data (0)
					pcoeff:		ptr_new(/allocate_heap), $	; pca coeff
					peigen:		ptr_new(/allocate_heap), $	; pca eigen
					pvar:		ptr_new(/allocate_heap)}, $	; pca var
			highlight:	[0,1], $		; highlight spline (pink) or class (green) sxelected points on PCA plots
			
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
	pstate = ptr_new(state, /no_copy)
	top = tlb_id( wWidget)
	(*pstate).tlb = top
	(*pstate).tools = tools
	child = widget_info( top, /child)
	widget_control, child, set_uvalue=pstate

	child2 = widget_info( tools, /child)
	widget_control, child2, set_uvalue=pstate
	return
end

;-----------------------------------------------------------------

pro PostCreate_pca_cluster, wWidget, _EXTRA=_VWBExtra_

widget_control, wWidget, set_draw_view=[0,0]
end

;-----------------------------------------------------------------
;
; Empty stub procedure used for autoloading.
;

pro pca_cluster_eventcb
end
