;
;	time_amp_Routines, for time_amp.pro
;
;-----------------------------------------------------------------

pro clear_time_amp_all_markers, pstate

clear_time_amp_spline, pstate
clear_time_amp_spline, pstate, /init, /zero

return
end

;-----------------------------------------------------------------
; Copy rectangle from 'from' to 'to' to clear a spline shape.
; By default, copies from '(*pstate).pix' to '(*pstate).wid2'.

pro clear_time_amp_spline, pstate, init=init, zero=zero, from=from, to=to

if n_elements(init) lt 1 then init=0
if n_elements(zero) lt 1 then zero=0
if n_elements(from) lt 1 then from = (*pstate).pix
if n_elements(to) lt 1 then to = (*pstate).wid2
p = (*pstate).pmark

if init eq 0 then begin
	spline_time_amp_vertices, pstate, x,y,n
	xy_time_amp_to_pixel, pstate, x,y, px,py
	wset, to
	minx = clip( (min(px) - 4), 0, (*pstate).w)
	miny = clip( (min(py) - 4), 0, (*pstate).h)
	maxx = clip( (max(px) + 4), 0, (*pstate).w)
	maxy = clip( (max(py) + 4), 0, (*pstate).h)
;	print,'clear_spline: [',minx,miny, maxx-minx+1,maxy-miny+1,'] from=',from,' to=',to
	device,copy=[ minx,miny, maxx-minx+1,maxy-miny+1, minx,miny, from]
endif

if zero then begin
	(*p).x[*] = 0.0
	(*p).y[*] = 0.0
endif
(*p).present = 0
return
end

;-----------------------------------------------------------------
; Draw the current time_amp ((*pstate).p) on the draw widget

pro draw_time_amps, pstate

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
		warning,'Draw_time_amps',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

p = (*pstate).p
if ptr_valid(p) eq 0 then return
if n_elements( *p) eq 0 then return
if size(*p,/tname) ne 'STRUCT' then return

(*pstate).skip_resize = 1	; to stop resize events following plot (in IDL 8.5)

wset, (*pstate).wid2
erase
!p.charsize = 1.2	;1.0
!p.charthick = 1.	;1.0
!p.linestyle = 0
!p.multi = 0
!p.psym = 0
!p.thick = 1.		;3.0
!p.title = ''
!x.charsize = 1.	;2.0
!x.style = 1
!x.thick = 1.		;2.0
!x.ticks = 0
!x.title = ''
!y.charsize = 1.	;2.0
!y.style = 1
!y.thick = 1.		;2.0
!y.ticks = 0
!y.title = ''
!p.background = spec_colour('black')
!p.color = spec_colour('white')
green = spec_colour('green')
;!p.background = spec_colour('white')
;!p.color = spec_colour('black')
;green = spec_colour('d.green')

; To use q here, need to know the Xsize, Ysize of the image area

e = (*p).e
t = (*p).t
x = (*p).x
y = (*p).y
ste = (*p).ste
show_all = 1

if ptr_valid((*pstate).q) and ((*pstate).nx gt 0) and ((*pstate).ny gt 0) then begin
	if ptr_valid((*pstate).q) then begin
		mask = bytarr((*pstate).nx,(*pstate).ny)
		mask[*(*pstate).q] = 1

		q1 = where((x lt (*pstate).nx) and (y lt (*pstate).ny))
		if q1[0] eq -1 then goto, done

		q2 = where(mask[x[q1],y[q1]] eq 1)
		if q2[0] eq -1 then goto, done

		e = e[q1[q2]]
		t = t[q1[q2]]
		x = x[q1[q2]]
		y = y[q1[q2]]
		ste = ste[q1[q2]]
	endif
endif

if (*pstate).detector ge 0 then begin
	q = where(ste eq (*pstate).detector)
	if q[0] ne -1 then begin
		e = e[q]
		t = t[q]
		x = x[q]
		y = y[q]
		ste = ste[q]
	endif else begin
		e = [0,1]
		t = [0,1]
		x = [0,1]
		y = [0,1]
		ste = [0,0]
		show_all = 0
		goto, the_plot
	endelse
endif
t0 = (t - (*(*pstate).ptrim0).T[ ste].b ) /  (*(*pstate).ptrim0).T[ ste].a
t = (t0 * (*(*pstate).ptrim).T[ ste].a ) + (*(*pstate).ptrim).T[ ste].b

xmin = min(e)
xmax = max(e)
ymin = min(t)
ymax = max(t)
(*pstate).maxx = xmax
(*pstate).maxy = ymax

;xr = (xmax - xmin) > 0.01*xmax
;yr = (ymax - ymin) > 0.01*ymax
;xl = xmin - 0.03*xr + (*pstate).lowX*xr/100. > 0.
;if (*pstate).logX then xl = xl > 0.001*xr
;xh = (xmax + 0.03*xr - (100-(*pstate).highX)*xr/100.) > (xl + 0.01*xr)
;yl = ymin - 0.03*yr + (*pstate).lowY*yr/100. > 0.
;if (*pstate).logY then yl = yl > 0.001*yr
;yh = (ymax + 0.03*yr - (100-(*pstate).highY)*yr/100.) > (yl + 0.01*yr)
;(*pstate).xrange = [xl,xh]
;(*pstate).yrange = [yl,yh]

xl = (*pstate).lowX
if (*pstate).logX then xl = xl > 1
yl = (*pstate).lowY
if (*pstate).logY then yl = yl > 1
(*pstate).xrange = [xl, (*pstate).highX] * 4095./100.
(*pstate).yrange = [yl, (*pstate).highY] * 1023./100.

the_plot:
	if n_elements(e) lt 2 then goto, done
	!p.title = 'Time Amplitude (E-T) plot'
	!x.title = 'E amplitude (channel)'
	!y.title = 'T time (channel)'
	
	plot, e, t, /nodata, xlog=(*pstate).logx, ylog=(*pstate).logy, $
			xrange=(*pstate).xrange, yrange=(*pstate).yrange, xstyle=1, ystyle=1, $
			position=(*pstate).position
	if show_all then oplot, e, t, psym=5, color=green, symsize=0.5

	(*pstate).ax = (!x.crange[1]-!x.crange[0])/((*pstate).w*(!x.window[1]-!x.window[0]))
	(*pstate).bx = !x.crange[0] - (*pstate).ax * (*pstate).w*!x.window[0]
	(*pstate).ay = (!y.crange[1]-!y.crange[0])/((*pstate).h*(!y.window[1]-!y.window[0]))
	(*pstate).by = !y.crange[0] - (*pstate).ay * (*pstate).h*!y.window[0]

	draw_time_amp_pileup, pstate
	
	wset, (*pstate).pix
	device,copy=[0,0,(*pstate).w,(*pstate).h, 0,0, (*pstate).wid2]
	
	clear_time_amp_spline, pstate, /init
	wset, (*pstate).wid2
	plot_time_amp_spline, pstate

done:
	(*pstate).skip_resize = 0
	return
end

;-----------------------------------------------------------------

pro draw_time_amp_pileup, pstate

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
		warning,'draw_time_amp_pileup',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

p = (*pstate).p
if ptr_valid(p) eq 0 then return
if n_elements( *p) eq 0 then return
if size(*p,/tname) ne 'STRUCT' then return

wset, (*pstate).wid2

q = where( (*pstate).limits[1,*] ne 0, nq)
if nq eq 0 then return

n = max(q) + 1
x = indgen(n)
y1 = (*pstate).limits[0,0:n-1]
y2 = (*pstate).limits[1,0:n-1]

; plot the spline

orange = spec_colour('orange')	;'red')

x = clip(x[q],!x.crange[0],!x.crange[1])
y1 = clip(y1[q],!y.crange[0],!y.crange[1])
y2 = clip(y2[q],!y.crange[0],!y.crange[1])

xy_time_amp_to_pixel, pstate, x,y1, px,py
plots, px,py, /device, color=orange
xy_time_amp_to_pixel, pstate, x,y2, px,py
plots, px,py, /device, color=orange
return
end

;-----------------------------------------------------------------

pro free_time_amp_state, pstate

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
		warning,'Free_time_amp_state',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(pstate) ne 0 then begin
	if (*pstate).pix ge 0 then wdelete, (*pstate).pix
;	if (*pstate).pix2 ge 0 then wdelete, (*pstate).pix2
	(*pstate).pix = -1
;	(*pstate).pix2 = -1

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).p) then ptr_free, (*pstate).p
	if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q
	if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
endif

return
end

;-----------------------------------------------------------------

function legend_time_amp_string, pstate, position=position

;return,' do this later ...'

if n_elements(pstate) lt 1 then return,''
if n_elements(position) lt 1 then position=0
p = (*pstate).p
if ptr_valid(p) eq 0 then return, ''

if position then begin
	pm = (*pstate).pmark
	x = (*pm).x[0]
	y = (*pm).y[0]
	sx = string(x)
	sy = string(y)

	Note = 'Spline centre '

endif else begin
	mx = (*pstate).maxx
	my = (*pstate).maxy
	sx = string(mx)
	sy = string(my)

	Note = 'Display top '
endelse

s = note + 'Amp (X) = ' + sx
s = [s, note + 'Time (Y) = ' + sy ]
return, s
end

;-----------------------------------------------------------------

function make_time_amp_mask, pstate

qc = -1

spline_time_amp_vertices, pstate, x,y, n
if n eq 0 then return, qc

q = polyfillv( x,y, 256, 256)

mask = bytarr( 256, 256)
mask[q] = 1

;conc_time_amp_to_xy, pstate, *(*pstate).image_x, *(*pstate).image_y, x,y, /nozoom
qc = where( mask[x,y] eq 1)

return, qc
end

;-----------------------------------------------------------------

pro map_time_amp_help, pstate

if (*pstate).w gt 600 then begin
	if (*pstate).help eq (*pstate).help2 then goto, more

	(*pstate).help = (*pstate).help2
	widget_control, (*pstate).help1_base, map=0
	widget_control, (*pstate).help1, scr_ysize=1
	widget_control, (*pstate).help2_base, map=1

	case !version.os_family of
		'MacOS': begin
			(*pstate).scr_xsize_off =	1
			(*pstate).scr_ysize_off =	106
			end
		'unix': begin
			(*pstate).scr_xsize_off =	1
			(*pstate).scr_ysize_off =	152
			end
		else: begin
			(*pstate).scr_xsize_off =	8
			(*pstate).scr_ysize_off =	121
			end
	endcase
endif else begin
	if (*pstate).help eq (*pstate).help1 then goto, more

	(*pstate).help = (*pstate).help1
	widget_control, (*pstate).help2_base, map=0
	widget_control, (*pstate).help2, scr_xsize=1
	widget_control, (*pstate).help1, ysize=3
	widget_control, (*pstate).help1_base, map=1

	case !version.os_family of
		'MacOS': begin
			(*pstate).scr_xsize_off =	1
			(*pstate).scr_ysize_off =	170
			end
		'unix': begin
			(*pstate).scr_xsize_off =	8
			(*pstate).scr_ysize_off =	212
			end
		else: begin
			(*pstate).scr_xsize_off =	8
			(*pstate).scr_ysize_off =	171
			end
	endcase
endelse

more:
	case !version.os_family of
		'unix': begin
			xoff = 324
			end
		else: begin
			xoff = 404		;+120
			end
	endcase

if (*pstate).help eq (*pstate).help2 then begin
	widget_control, (*pstate).help2, scr_xsize=((*pstate).w - xoff)
endif
end

;--------------------------------------------------------------------

; Which is closest index of 'x,y' vectors to point ,'xp,yp'
; This version accepts vectors 'x', 'y' and returns the closest index, else -1

function near_time_amp_xy, xp,yp, x,y, reverse=reverse, margin=m

if n_elements(reverse) lt 1 then reverse=0
if n_elements(m) lt 1 then m=5

r = sqrt( float(x-xp)*float(x-xp) + float(y-yp)*float(y-yp) )
i = indgen(n_elements(r))

q = where( r lt m)			; close proximity
if q[0] eq -1 then return, -1
r = r[q]					; list of all in close proximity
i = i[q]					; indices of these

q = sort(r)					; sort in ascending distance order
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
pro pixel_time_amp_to_xy, pstate, px,py, x,y

COMPILE_OPT STRICTARR

x = (*pstate).ax * px + (*pstate).bx
y = (*pstate).ay * py + (*pstate).by
if (*pstate).logX then x=10^x
if (*pstate).logY then y=10^y

return
end
;
;-----------------------------------------------------------------
; Plot handles at vector positions x,y

pro plot_time_amp_handles, pstate, x,y, color

if n_elements(x) lt 1 then return
boxx = [-1,1,1,-1,-1]
boxy = [-1,-1,1,1,-1]
scale = 2
for i=0L,n_elements(x)-1 do begin
	px = clip( x[i] + boxx * scale, 0, (*pstate).w-1)
	py = clip( y[i] + boxy * scale, 0, (*pstate).h-1)
	plots, px,py, /device, color=color
endfor

return
end

;-----------------------------------------------------------------
; Plot current spline marker

pro plot_time_amp_spline, pstate, wide=wide

if n_elements(wide) eq 0 then wide=0
p = (*pstate).pmark

;  0 is low handle, 1 is curvature handle, 2 is high handle

maxx = max((*p).x)
maxy = max((*p).y)
if (max([maxx,maxy]) eq 0) then return

; spline_time_amp_vertices returns the spline interpolation points

spline_time_amp_vertices, pstate, x,y,n
if n lt 1 then return

; plot handles
;print,'x=',(*p).x,format='(A7,7F)'
;print,'y=',(*p).y,format='(A7,7F)'

color = spec_colour('red')
xy_time_amp_to_pixel, pstate, (*p).x,(*p).y, px,py
if (wide eq 0) then plot_time_amp_handles, pstate, px,py, color

; plot the spline

xy_time_amp_to_pixel, pstate, x,y, px,py

if wide then begin
	plots, px,py, /device, color=spec_colour('red'),thick=3.0			; b/w figures
endif else begin
	plots, px,py, /device, color=color
endelse

(*p).present = 1
return
end

;-----------------------------------------------------------------
; Build list of spline vertices (in time_amp display coords).

pro spline_time_amp_vertices, pstate, x,y, n

p = (*pstate).pmark

;  5 point spline curve

time_amp_shape, (*p).x,(*p).y, x,y

n = n_elements(x)

maxx = max((*p).x)
maxy = max((*p).y)
if (max([maxx,maxy]) eq 0) then n=0

return
end

;-----------------------------------------------------------------

pro set_time_amp_map_help, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

map_time_amp_help, pstate
end

;-----------------------------------------------------------------
;
; Convert image 'x,y' to pixel position 'px,py'
; If compress, ignore zoom, scale x,y by compress.

pro xy_time_amp_to_pixel, pstate, x,y, px,py

px = 0
py = 0
if ptr_valid( (*pstate).p) eq 0 then return

x1 = x
y1 = y
if (*pstate).logX then x1=alog10(x1)
if (*pstate).logY then y1=alog10(y1)

px = round((x1-(*pstate).bx)/ (*pstate).ax)
py = round((y1-(*pstate).by)/ (*pstate).ay)

return
end

;------------------------------------------------------------------------------

; Stub routine for autoloading ...

pro time_amp_routines
end
