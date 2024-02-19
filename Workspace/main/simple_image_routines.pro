;
;	simple_image_Routines, for simple_image.pro
;
;-----------------------------------------------------------------

pro clear_simple_image_all_markers, pstate

clear_simple_image_box, pstate
clear_simple_image_box, pstate, /init, /zero

return
end

;-----------------------------------------------------------------
; Copy rectangle from 'from' to 'to' to clear a box shape.
; By default, copies from '(*pstate).pix' to '(*pstate).wid'.

pro clear_simple_image_box, pstate, init=init, zero=zero, from=from, to=to

if n_elements(init) lt 1 then init=0
if n_elements(zero) lt 1 then zero=0
if n_elements(from) lt 1 then from = (*pstate).pix
if n_elements(to) lt 1 then to = (*pstate).wid
p = (*pstate).pmark

if init eq 0 then begin
	x = (*p).x  &  y = (*p).y
	xy_simple_image_to_pixel, pstate, x,y, px,py
	wset, to
	minx = clip( (min(px) - 4), 0, (*pstate).width)
	miny = clip( (min(py) - 4), 0, (*pstate).height)
	maxx = clip( (max(px) + 4), 0, (*pstate).width)
	maxy = clip( (max(py) + 4), 0, (*pstate).height)
;	print,'clear_box: [',minx,miny, maxx-minx+1,maxy-miny+1,'] from=',from,' to=',to
	device,copy=[ minx,miny, maxx-minx+1,maxy-miny+1, minx,miny, from]
endif

if zero then begin
	(*p).x[*] = 0.0
	(*p).y[*] = 0.0
	(*p).px[*] = 0.0
	(*p).py[*] = 0.0
endif
(*p).present = 0
return
end

;-----------------------------------------------------------------
; Draw the current image ((*pstate).n) on the draw widget

pro draw_simple_images, pstate

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
		warning,'draw_simple_images',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

tin  = systime(/seconds)
wset, (*pstate).wid
p = (*pstate).p
if ptr_valid( p) eq 0 then goto, fin
if ptr_valid( (*p).image ) eq 0 then goto, fin

i = (*pstate).n
pimg = (*p).image
opt = (*p).options
sx = (*p).xsize
sy = (*p).ysize
crop = (*pstate).crop

if crop.on then begin
	xl = crop.x[0] >0
	xh = crop.x[1] < (sx-1)
	yl = crop.y[0] >0
	yh = crop.y[1] < (sy-1)
	sx = xh - xl +1
	sy = yh - yl +1
endif else begin
	xl = 0
	xh = sx - 1
	yl = 0
	yh = sy - 1
endelse

if (*pstate).with_border then begin				; if using a plot axes border
	fxl = (*pstate).border[0]
	fxh = (*pstate).border[2]
	fyl = (*pstate).border[1]
	fyh = (*pstate).border[3]
endif else begin
	fxl = 0.0
	fxh = 1.0
	fyl = 0.0
	fyh = 1.0
endelse
img1 = smart_congrid( (*pimg)[xl:xh,yl:yh,i], (fxh-fxl)*(*pstate).width, (fyh-fyl)*(*pstate).height, interp=(*opt)[i].interp, /minus_one)

build_image_scale, (*opt)[i], low, high, image=img1, output=img2

b = bytscl( img2, top=99, min=low, max=high) + 16B
erase

; plot image ...
tv, b, fxl*(*pstate).width, fyl*(*pstate).height					; plot image

; plot axes ...
if (*pstate).with_border then begin	
	pos = [fxl*(*pstate).width,fyl*(*pstate).height,fxh*(*pstate).width,fyh*(*pstate).height]
	plot, [0,0],[0,0], position=pos, /device, /nodata, /noerase, xrange=[xl,xh]*(*p).xcompress, yrange=[yl,yh]*(*p).ycompress, $
		charsize=float((*pstate).width)/500., xstyle=1,ystyle=1
		
; plot optional overlay shape ...
	if ptr_good( (*pstate).shape.px) and ptr_good( (*pstate).shape.px) then begin
		np = min( [n_elements(*(*pstate).shape.px),n_elements(*(*pstate).shape.py)])
		x = [ (*(*pstate).shape.px)[0:np-1], (*(*pstate).shape.px)[0] ]
		y = [ (*(*pstate).shape.py)[0:np-1], (*(*pstate).shape.py)[0] ]
		plots, x,y, color=spec_colour('orange')
	endif
endif

if (*pstate).box_select then begin
	wset, (*pstate).pix
	device,copy=[0,0,(*pstate).width,(*pstate).height, 0,0,(*pstate).wid]	
	clear_simple_image_box, pstate, /init
	wset, (*pstate).wid
	if crop.on eq 0 then plot_simple_image_box, pstate
endif

fin:
	t = systime(/seconds)
	(*pstate).percent = 100.*(t-tin)/(t-(*pstate).last_time)
;	print, 'ET-2D: ',t-tin,(t-(*pstate).last_time), (*pstate).percent
	(*pstate).last_time = t
	return
end

;-----------------------------------------------------------------
; Build list of box handles (in simple_image display coords).

pro simple_image_handles, pstate, px,py, n

p = (*pstate).pmark

;  0-3 are control points, 4 is centre handle

px = (*p).x
py = (*p).y

n = n_elements(px)

maxx = max(px)
maxy = max(py)
if (max([maxx,maxy]) eq 0) then n=0

return
end

;-----------------------------------------------------------------

pro free_simple_image_state, pstate

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
		warning,'Free_simple_image_state',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(pstate) eq 0 then return
if ptr_valid(pstate) eq 0 then return
if size(*pstate,/tname) ne 'STRUCT' then return

if (*pstate).box_select then begin
	if (*pstate).pix ge 0 then wdelete, (*pstate).pix
	(*pstate).pix = -1
endif

ptr_free, pstate
return
end

;--------------------------------------------------------------------

; Which is closest index of 'x,y' vectors to point ,'xp,yp'
; This version accepts vectors 'x', 'y' and returns the closest index, else -1

function near_simple_image_xy, xp,yp, x,y, reverse=reverse

if n_elements(reverse) lt 1 then reverse=0

r = sqrt( float(x-xp)*float(x-xp) + float(y-yp)*float(y-yp) )
i = indgen(n_elements(r))

q = where( r lt 20)			; close proximity
if q[0] eq -1 then return, -1
r = r[q]				; list of all in close proximity
i = i[q]				; indices of these

q = sort(r)				; sort in ascending distance order
r = r[q]
i = i[q]

if reverse then begin
	result = max(i)			; larger indices first
endif else begin
	result = i[0]			; smaller distance first
endelse

return, result
end

;-----------------------------------------------------------------
;
; Convert pixel x,y position to image 'x,y'
;
pro pixel_simple_image_to_xy, pstate, px,py, x,y

COMPILE_OPT STRICTARR

x = zoom_simple_image(pstate, px, /x, /down)
y = zoom_simple_image(pstate, py, /y, /down)
return
end
;
;-----------------------------------------------------------------
; Plot handles at vector positions x,y

pro plot_simple_image_handles, pstate, x,y, color

if n_elements(data) lt 1 then data=0
if n_elements(x) lt 1 then return
boxx = [-1,1,1,-1,-1]
boxy = [-1,-1,1,1,-1]
scale = 2
for i=0L,n_elements(x)-1 do begin
	px = x[i] + boxx * scale
	py = y[i] + boxy * scale
	px = clip( px, 0, (*pstate).width-1)
	py = clip( py, 0, (*pstate).height-1)
	plots, px,py, device=1, color=color
endfor
return
end

;-----------------------------------------------------------------
; Plot current box marker

pro plot_simple_image_box, pstate

p = (*pstate).pmark

;  4 is centre handle, 0-3 are control points

maxx = max((*p).x)
maxy = max((*p).y)
if (max([maxx,maxy]) eq 0) then return

; circle_vertices returns the 100 box interpolation points

x = [(*p).x[0:3],(*p).x[0]]
y = [(*p).y[0:3],(*p).y[0]]
;print,'Vertices (data) x,y=',x,y
x1 = (*p).x[0:4]
y1 = (*p).y[0:4]

; plot handles

color = spec_colour('green')
xy_simple_image_to_pixel, pstate, x1,y1, px,py
plot_simple_image_handles, pstate, px,py, color

; plot the box

xy_simple_image_to_pixel, pstate, x,y, px,py
;print,'Vertices (screen) px,py=',px,py
px = clip( px, 0, (*pstate).width-1)
py = clip( py, 0, (*pstate).height-1)
plots, px,py, device=1, color=color

(*p).present = 1
return
end

;-----------------------------------------------------------------------------------
; Set the view size for new simple_images.
; Do this after a resize (initialy too?) to set Draw widget size and pix;
; assumes that these have been set by resize routine: scale, width, height.

pro set_simple_image_view, pstate

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
		warning,'Set_simple_image_view',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	draw_trim = 0
	scr_trim = 15
	if !version.os_family eq 'MacOS' then begin
		draw_trim = 15
		scr_trim = 21
	endif

	if (*pstate).box_select then begin
		allocate_pixmap, (*pstate).width, (*pstate).height, new_wid=wid, old_wid=(*pstate).pix, error=error
		if error then goto, bad_pix
		(*pstate).pix = wid
	endif

	widget_control, (*pstate).draw, scr_xsize=(*pstate).width, scr_ysize=(*pstate).height
	draw_simple_images, pstate
	return
	
bad_pix:
	warning,'set_simple_image_view',['Failed to allocate pixmap.','Disable drag shape.']
	(*pstate).box_select = 0
	return
end

;-----------------------------------------------------------------
;
; Convert image 'x,y' to pixel position 'px,py'
; If compress, ignore zoom, scale x,y by compress.

pro xy_simple_image_to_pixel, pstate, x,y, px,py

px = 0
py = 0
if ptr_valid( (*pstate).p) eq 0 then return

px = zoom_simple_image( pstate, x, /x)
py = zoom_simple_image( pstate, y, /y)
return
end

;-----------------------------------------------------------------

function zoom_simple_image, pstate, n, down=down, x=x, y=y

if n_elements(down) eq 0 then down=0
if n_elements(x) eq 0 then x=0
if n_elements(y) eq 0 then y=0
if (x eq 0) and (y eq 0) then x=1

if x then begin
	if (*pstate).crop.on then begin
		x0 = (*pstate).crop.x[0]
		x1 = (*pstate).crop.x[1]
	endif else begin
		x0 = 0
		x1 = (*(*pstate).p).xsize-1
	endelse
	if (*pstate).with_border then begin
		z0 = long((*pstate).border[0]*(*pstate).width)
		z1 = long((*pstate).border[2]*(*pstate).width)
	endif else begin
		z0 = 0
		z1 = (*pstate).width-1
	endelse
	a = (x1 - x0) / float(z1 - z0)
	b = x0 - a*z0
endif else begin
	if (*pstate).crop.on then begin
		y0 = (*pstate).crop.y[0]
		y1 = (*pstate).crop.y[1]
	endif else begin
		y0 = 0
		y1 = (*(*pstate).p).ysize-1
	endelse
	if (*pstate).with_border then begin
		z0 = long((*pstate).border[1]*(*pstate).height)
		z1 = long((*pstate).border[3]*(*pstate).height)
	endif else begin
		z0 = 0
		z1 = (*pstate).height-1
	endelse
	a = (y1 - y0) / float(z1 - z0)
	b = y0 - a*z0
endelse
if down then begin
	r = float(n) * a + b
endif else begin
	r = (float(n)-b)/a
endelse
return, r
end

;------------------------------------------------------------------------------

; Stub routine for autoloading ...

pro simple_image_routines
end
