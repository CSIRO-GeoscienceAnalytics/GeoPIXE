;
;	corr_Routines, for corr.pro
;
;-----------------------------------------------------------------

pro clear_corr_all_markers, pstate

clear_corr_spline, pstate
clear_corr_spline, pstate, /init, /zero

return
end

;-----------------------------------------------------------------
; Copy rectangle from 'from' to 'to' to clear a spline shape.
; By default, copies from '(*pstate).pix' to '(*pstate).wid2'.

pro clear_corr_spline, pstate, init=init, zero=zero, from=from, to=to

if n_elements(init) lt 1 then init=0
if n_elements(zero) lt 1 then zero=0
if n_elements(from) lt 1 then from = (*pstate).pix
if n_elements(to) lt 1 then to = (*pstate).wid2
p = (*pstate).pmark

totx = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
toty = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top

if init eq 0 then begin
	spline_corr_vertices, pstate, x,y,n
	xy_corr_to_pixel, pstate, x,y, px,py
	wset, to
	minx = clip( (min(px) - 4), 0, totx)
	miny = clip( (min(py) - 4), 0, toty)
	maxx = clip( (max(px) + 4), 0, totx)
	maxy = clip( (max(py) + 4), 0, toty)
;	print,'clear_spline: [',minx,miny, maxx-minx+1,maxy-miny+1,'] from=',from,' to=',to
	device,copy=[ minx,miny, maxx-minx+1,maxy-miny+1, minx,miny, from]
endif

if zero then begin
	(*p).x[*] = 0.0
	(*p).y[*] = 0.0
	(*p).cx[*] = 0.0
	(*p).cy[*] = 0.0
endif
if ptr_good(p) then (*p).present = 0
return
end

;-----------------------------------------------------------------
;
; Convert conc position 'cx,cy' to image 'x,y'
; NOTE: if formula changed here, change 'xy_corr_to_conc' too.

pro conc_corr_to_xy, pstate, cx,cy, x,y, veto=veto, save_max=save_max, $
						range=range, nozoom=nozoom, sx=w, sy=h, noclip=noclip

x = 0.0
y = 0.0
if ptr_valid( (*pstate).p) eq 0 then return
if n_elements(veto) lt 1 then veto=0
if n_elements(noclip) lt 1 then noclip=0
if n_elements(nozoom) lt 1 then nozoom=0
if n_elements(save_max) lt 1 then save_max=0
if nozoom then begin
	w = (*pstate).owidth
	h = (*pstate).oheight
endif else begin
	w = (*pstate).width
	h = (*pstate).height
endelse

if save_max then begin
	pimg = (*(*pstate).p).image
	nx = n_elements((*pimg)[*,0,0])
	ny = n_elements((*pimg)[0,*,0])
	x1 = 0.02*nx
	x2 = 0.98*nx < (nx-1)
	y1 = 0.02*ny
	y2 = 0.98*ny < (ny-1)
	if (size(cx))[0] eq 2 then begin				; ignore border pixels
		xmax = 1.05*max(cx[x1:x2,y1:y2])
	endif else begin
		xmax = 1.05*max(cx)
	endelse
	if (size(cy))[0] eq 2 then begin
		ymax = 1.05*max(cy[x1:x2,y1:y2])
	endif else begin
		ymax = 1.05*max(cy)
	endelse
	(*pstate).maxx = xmax
	(*pstate).maxy = ymax
endif else begin
	xmax = (*pstate).maxx
	ymax = (*pstate).maxy
endelse
if veto then begin
	q = where(cy gt ymax*1.0E-5)
	if q[0] ne -1 then xmax = xmax < 1.05*max(cx[q])
	q = where(cx gt xmax*1.0E-5)
	if q[0] ne -1 then ymax = ymax < 1.05*max(cy[q])
	if save_max then begin
		(*pstate).maxx = xmax
		(*pstate).maxy = ymax
	endif
endif

if (*pstate).logx then begin
	xmax1 = xmax
	xmax = xmax1 / 10^( 5.0 - 0.05*(*pstate).xtop)
	xmin = xmax1 / 10^( 5.0 - 0.045*(*pstate).xbottom) < xmax
	x = long( (w-1) * (alog10((cx)/xmin)/alog10(xmax/xmin) > 0.0) )
endif else begin
	xmax1 = xmax
	xmax = xmax1 * (*pstate).xtop/100.
	xmin = xmax1 * (*pstate).xbottom/100.
	x = long( (w-1) * (((cx)-xmin)/(xmax-xmin) > 0.0) )
endelse
if (*pstate).logy then begin
	ymax1 = ymax
	ymax = ymax1 / 10^( 5.0 - 0.05*(*pstate).ytop)
	ymin = ymax1 / 10^( 5.0 - 0.045*(*pstate).ybottom) < ymax
	y = long( (h-1) * (alog10((cy)/ymin)/alog10(ymax/ymin) > 0.0) )
endif else begin
	ymax1 = ymax
	ymax = ymax1 * (*pstate).ytop/100.
	ymin = ymax1 * (*pstate).ybottom/100.
	y = long( (h-1) * (((cy)-ymin)/(ymax-ymin) > 0.0) )
endelse

if save_max then begin
	(*pstate).minx = xmin
	(*pstate).miny = ymin
endif

range = {x:[xmin,xmax], y:[ymin,ymax]}

if noclip eq 0 then begin
	x = clip( x, 0, w-1)
	y = clip( y, 0, h-1)
endif

x = reform(x, n_elements(x), /overwrite)
y = reform(y, n_elements(y), /overwrite)
return
end

;-----------------------------------------------------------------
; Build list of spline handles (in corr display coords).

pro corr_handles, pstate, px,py, n, use_conc=use_conc

if n_elements(use_conc) lt 1 then use_conc=0

p = (*pstate).pmark

;  1-10 (or 1-32) are control points, 0 is centre handle

if use_conc then begin
	cx = (*p).cx
	cy = (*p).cy
	conc_corr_to_xy, pstate, cx,cy, px,py, /nozoom
endif else begin
	px = (*p).x
	py = (*p).y
endelse

n = n_elements(px)

maxx = max(px)
maxy = max(py)
if (max([maxx,maxy]) eq 0) then n=0

return
end

;-----------------------------------------------------------------
; Draw the current corr ((*pstate).corr) on the draw widget

pro draw_corrs, pstate

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
		warning,'Draw_corrs',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

p = (*pstate).p
if ptr_good(p) eq 0 then return
if ptr_valid( (*p).image ) eq 0 then return
xanes_stack_test, p, xanes, n_el, el, el_xanes

wset, (*pstate).wid2
tb = !p.background
!p.background = 16				; use image colour base as erase background
erase
!p.background = tb
if n_elements( (*(*p).image)[0,0,*]) lt 1 then return

!p.charsize = 1.0
!p.charthick = 1.0
!p.linestyle = 0
!p.multi = 0
!p.psym = 0
!p.thick = 1.0
!p.title = ''
!x.charsize = 1.0
!x.style = 1
!x.thick = 1.0
!x.ticks = 0
!x.title = ''
!y.charsize = 1.0
!y.style = 1
!y.thick = 1.0
!y.ticks = 0
!y.title = ''
tvlct, ro,go,bo, /get
if long(ro[16])+long(go[16])+long(bo[16]) lt 384 then begin
	!p.background = spec_colour('black')
	!p.color = spec_colour('white')
endif else begin
	!p.background = spec_colour('white')
	!p.color = spec_colour('black')
endelse

b = make_corr_tvb( pstate, (*pstate).corr_x, (*pstate).corr_y, range=range)
if n_elements(b) le 1 then return
(*pstate).range = range

tv, b, (*pstate).margin.low, (*pstate).margin.bottom, /device		; show corr

!p.position = (*pstate).position
;print,!p.position
plot,[0,0],[0,0],xrange=range.x,yrange=range.y, /noerase,/nodata, $
		xlog=(*pstate).logx,ylog=(*pstate).logy, xticklen=-0.01,yticklen=-0.01, charsize=1.2
!p.position = [0.,0.,0.,0.]
;print,'Corr: actual w,h=',!d.x_size,!d.y_size

xyouts,0.985,0.015,el[(*pstate).corr_x],charsize=1.8,charthick=15.0,color=16,/norm, align=1.0
xyouts,0.985,0.015,el[(*pstate).corr_x],charsize=1.8,charthick=1.7,color=spec_colour('green'),/norm, align=1.0
xyouts,0.015,0.94,el[(*pstate).corr_y],charsize=1.8,charthick=15.0,color=16,/norm
xyouts,0.015,0.94,el[(*pstate).corr_y],charsize=1.8,charthick=1.7,color=spec_colour('green'),/norm

if ptr_valid ((*pstate).b) then ptr_free, (*pstate).b
(*pstate).b = ptr_new( b, /no_copy)

wset, (*pstate).pix
w = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
h = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top
device,copy=[0,0,w,h, 0,0,(*pstate).wid2]

;p = (*pstate).pmark
;conc_corr_to_xy, pstate, (*p).cx,(*p).cy, x,y, /nozoom
;(*p).x = x
;(*p).y = y

clear_corr_spline, pstate, /init
wset, (*pstate).wid2
plot_corr_spline, pstate, /use_conc
return
end

;-----------------------------------------------------------------

pro free_corr_state, pstate

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
		warning,'Free_corr_state',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(pstate) eq 0 then return
if ptr_valid(pstate) eq 0 then return
if size(*pstate,/tname) ne 'STRUCT' then return

	if (*pstate).pix ge 0 then wdelete, (*pstate).pix
	if (*pstate).pix2 ge 0 then wdelete, (*pstate).pix2
	(*pstate).pix = -1
	(*pstate).pix2 = -1

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).b) then ptr_free, (*pstate).b
	if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q
	if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
	if ptr_valid( (*pstate).image_x) then ptr_free, (*pstate).image_x
	if ptr_valid( (*pstate).image_y) then ptr_free, (*pstate).image_y
return
end

;-----------------------------------------------------------------

function legend_corr_string, pstate, position=position

if n_elements(pstate) eq 0 then return,''
if ptr_valid(pstate) eq 0 then return,''
if size(*pstate,/tname) ne 'STRUCT' then return,''
if n_elements(position) lt 1 then position=0
p = (*pstate).p
if ptr_good(p) eq 0 then return, ''
xanes_stack_test, p, xanes, n_els, el, el_xanes, z_found=z_found

ex = el[(*pstate).corr_x < (n_els-1)]
ey = el[(*pstate).corr_y < (n_els-1)]

if position then begin
	pm = (*pstate).pmark
	x = (*pm).x[0]
	y = (*pm).y[0]
	xy_corr_to_conc, pstate, x,y, mx,my, /nozoom
	sx = str_tidy(mx)
	sy = str_tidy(my)

	Note = 'Spline centre '

endif else begin
	mx = (*pstate).range.x[1]
	my = (*pstate).range.y[1]
	sx = str_tidy(mx)
	sy = str_tidy(my)

	Note = 'Display top '
endelse

special = special_elements()
if (*p).type eq 1 then begin
	unitsx = ''
	unitsy = ''
	stylex = ' fraction'
	styley = ' fraction'
endif else if (*p).type eq 2 then begin
	unitsx = ''
	unitsy = ''
	stylex = ' counts'
	styley = ' counts'
endif else begin
	unitsx = ' ppm'
	unitsy = ' ppm'
	stylex = xanes ? '' : ' conc'
	styley = xanes ? '' : ' conc'
	q = where(ex eq special,nq)
	if nq ne 0 then begin
		unitsx = ''
		stylex = ''
	endif else begin
		if mx gt 999.9 then begin
			sx = str_tidy(mx/10000.)
			unitsx = ' wt%'
		endif
	endelse
	q = where(ey eq special,nq)
	if nq ne 0 then begin
		unitsy = ''
		styley = ''
	endif else begin
		if my gt 999.9 then begin
			sy = str_tidy(my/10000.)
			unitsy = ' wt%'
		endif
	endelse
endelse

s = note + 'X (' + ex + stylex + ') = ' + sx + unitsx
s = [s, note + 'Y (' + ey + styley + ') = ' + sy + unitsy]
;s = [s, 'Use Export plot ("File" menu) to show detailed axes']
return, s
end

;-----------------------------------------------------------------
; Make the byte array to TV to the draw area

function make_corr_tvb, pstate, ix,iy, nozoom=nozoom, xmax=xmax, ymax=ymax, xmin=xmin, ymin=ymin, $
			xtgt=xtgt, ytgt=ytgt, compress=compress, low=low, high=high, range=range

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
		warning,'Make_corr_TVB',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0B
	endif
endif
if n_elements(nozoom) lt 1 then nozoom=0
if n_elements(xmax) lt 1 then xmax=0
if n_elements(ymax) lt 1 then ymax=0
if n_elements(xmin) lt 1 then xmin=0
if n_elements(ymin) lt 1 then ymin=0
if n_elements(xtgt) lt 1 then xtgt=0
if n_elements(ytgt) lt 1 then ytgt=0
b = 0B
p = (*pstate).p
if ptr_valid( p) eq 0 then return, b

imgx = interelement_transform( p, ix, error=nox_interelement)
imgy = interelement_transform( p, iy, error=noy_interelement)

pimg = (*p).image
xanes_stack_test, p, xanes, n_el, el, el_xanes

image_flux_charge, p, xanes=xanes, charge=charge, pixels=pixels, error=err
if err then begin
	print,'Make_corr_TVB: "image_flux_charge" error; return.'
	return, 0B
endif
special = special_elements()
ex = el[ix]
ey = el[iy]

if nox_interelement then begin
	cx = ((*pimg)[*,*,ix] > 0.0)
endif else begin
	cx = imgx[*,*] > 0.0
endelse
if noy_interelement then begin
	cy = ((*pimg)[*,*,iy] > 0.0)
endif else begin
	cy = imgy[*,*] > 0.0
endelse

qf = where( (*(*p).temp.charge_map) gt 0.0, nqf, complement=qz, ncomplement=nqz)
q = where( ex eq special, nq)
el_code, ex, el, z, shell, bad, error
;z = atomic_number( ex)
if (nq eq 0) and ( z ne 0) then begin
	cx = cx / (*(*p).temp.charge_map)
	if nqf eq 0 then begin
		print,'Make_corr_TVB: all pixels, zero charge; return.'
		return, 0B
	endif
	if nqz gt 0 then begin
		cx[qz] = 0.0
	endif
endif
q = where( ey eq special, nq)
el_code, ey, el, z, shell, bad, error
;z = atomic_number( ey)
if (nq eq 0) and ( z ne 0) then begin
	cy = cy / (*(*p).temp.charge_map)
	if nqf eq 0 then begin
		print,'Make_corr_TVB: all pixels, zero charge; return.'
		return, 0B
	endif
	if nqz gt 0 then begin
		cy[qz] = 0.0
	endif
endif

if (*pstate).x_smooth gt 0 then cx = smooth2( cx, (*pstate).x_smooth > 2)
if (*pstate).y_smooth gt 0 then cy = smooth2( cy, (*pstate).y_smooth > 2)

if ptr_valid( (*pstate).image_x) then ptr_free, (*pstate).image_x
if ptr_valid( (*pstate).image_y) then ptr_free, (*pstate).image_y
(*pstate).image_x = ptr_new(cx)
(*pstate).image_y = ptr_new(cy)

; (*pstate).q	shows pixels selected in a normal region (show X,Y only for this subset)

if ptr_valid( (*pstate).q) then begin
	if (*(*pstate).q)[0] ne -1 then begin
		q_to_xy, *((*pstate).q), (*p).xsize, xq,yq
		cx = cx[xq,yq]
		cy = cy[xq,yq]
	endif
endif

conc_corr_to_xy, pstate, cx,cy, x,y, /veto, /save_max, range=range, sx=sx,sy=sy

q = where( (cx lt range.x[1]) and (cx gt range.x[0]) and (cy lt range.y[1]) and (cy gt range.y[0]), nq)
if nq gt 0 then begin
	x = x[q]
	y = y[q]
endif else begin
	return, 0B						; Nov 2011 ??
;	x = 0
;	y = 0
endelse

h = hist_2d( x,y, min1=0,min2=0, max1=sx-1,max2=sy-1)
if (*pstate).high eq 0 then begin
	hmax = image_weighted_max( h, threshold=0.01, scope=1000, nothing_remains=nothing)
endif else begin
	hmax = image_weighted_max( h, threshold=0.003, scope=100, nothing_remains=nothing)
endelse
if nothing then return, 0B
low = (*pstate).Low * hmax / 100. > 0
high = ((*pstate).High * hmax / 100. > (low + 0.005*hmax)) > 1

case (*pstate).Zaxis of
	1: begin
		low = sqrt(low)
		high = sqrt(high)
		h = sqrt(h)
	end
	2: begin
		low = alog10( (10*low) > 1.)
		high = alog10( 10*high)
		h = alog10( (10*h) > 1.)
	end
	else:
endcase

b = bytscl( h, top=99, min=low, max=high) + 16B

; Highlight points from a previous 'analyze spline' view

if (*pstate).highlight then begin
	if ptr_good( (*pstate).qc) then begin
		qp = *(*pstate).qc
		if n_elements(qp) ge 1 then begin
			if qp[0] ne -1 then begin
				conc_corr_to_xy, pstate, cx[qp],cy[qp], xp,yp
				b[xp,yp] = spec_colour('green')
			endif
		endif
	endif
endif

compress = 1
if nozoom then begin
	if ytgt ne 0 then begin
		compress = min( [ compress, float(ytgt) / float((*p).ysize) ])
	endif
	if xtgt ne 0 then begin
		compress = min( [ compress, float(xtgt) / float((*p).xsize) ])
	endif

	if ymin ne 0 then begin
		if ymin gt (*p).ysize*compress then begin
			compress = max( [ compress, float(ymin) / float((*p).ysize) ])
		endif
	endif
	if xmin ne 0 then begin
		if xmin gt (*p).xsize*compress then begin
			compress = max( [ compress, float(xmin) / float((*p).xsize) ])
		endif
	endif
	if ymax ne 0 then begin
		if ymax lt (*p).ysize*compress then begin
			compress = min( [ compress, float(ymax) / float((*p).ysize) ])
		endif
	endif
	if xmax ne 0 then begin
		if xmax lt (*p).xsize*compress then begin
			compress = min( [ compress, float(xmax) / float((*p).xsize) ])
		endif
	endif

	if compress ne 1 then begin
		b = smart_congrid( b, (*p).xsize*compress, (*p).ysize*compress, /interp)
	endif
endif else begin
	compress = float((*pstate).width) / float((*p).xsize)
	if ((*pstate).zoom ne 0) then begin
		b = smart_congrid( b, (*pstate).width, (*pstate).height, /interp)
	endif
endelse

return, b
end

;-----------------------------------------------------------------

function make_corr_mask, pstate, fresh=fresh, exclude=exclude

; fresh = 1	new spline selection
;		  0	refine existing (i.e. intersection)
; /exclude	remove selected pixels from selection

COMPILE_OPT STRICTARR
if n_elements(exclude) eq 0 then exclude=0
if n_elements(fresh) eq 0 then fresh=1
if ptr_good( (*pstate).qc) eq 0 then fresh=1
if (fresh eq 1) then exclude=0
qc = -1

spline_corr_vertices, pstate, x,y, n
if n eq 0 then return, qc
p = (*pstate).p

mask = bytarr( (*pstate).owidth, (*pstate).oheight)
q = polyfillv( x,y, (*pstate).owidth, (*pstate).oheight)	; index in corr plot space

conc_corr_to_xy, pstate, *(*pstate).image_x, *(*pstate).image_y, x,y, /nozoom

; x,y are now the coordinates in this 2D histgram of all element conc points

mask[q] = 1													; new 2-element spline field
qc = where( mask[x,y] eq 1)									; index in image pixel list space for this field

index = bytarr( n_elements(x))								; index in image pixel list space

if (fresh eq 0) then begin
	index[ *(*pstate).qc] = 1								; existing pixel selection
	if exclude then begin									; use spline to veto some pixels
		index[qc] = 0
		sel = 1
	endif else begin										; use spline to refine selection to union
		index[qc] += 1										; of this and previous shape
		sel = 2
	endelse
	qc = where( index eq sel)								; index in image pixel list space
endif

if (*p).bounds.valid then begin
	q2 = bounds_mask( p, /reject)							; reject indices in image space
	qc = veto( q2, qc)
endif

return, qc
end

;-----------------------------------------------------------------

pro map_corr_help, pstate

if (*pstate).w gt 560 then begin
	if (*pstate).help eq (*pstate).help2 then goto, more

	(*pstate).help = (*pstate).help2
	widget_control, (*pstate).help1_base, map=0
	widget_control, (*pstate).help1, scr_ysize=1
	widget_control, (*pstate).help2_base, map=1
endif else begin
	if (*pstate).help eq (*pstate).help1 then goto, more

	(*pstate).help = (*pstate).help1
	widget_control, (*pstate).help2_base, map=0
	widget_control, (*pstate).help2, scr_xsize=1
	widget_control, (*pstate).help1, ysize=3
	widget_control, (*pstate).help1_base, map=1
endelse

more:
if (*pstate).w gt 560 then begin
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
			(*pstate).scr_xsize_off =	10		; 8
			(*pstate).scr_ysize_off =	118		; 106
			end
	endcase
endif else begin
	case !version.os_family of
		'MacOS': begin
			(*pstate).scr_xsize_off =	1
			(*pstate).scr_ysize_off =	138
			end
		'unix': begin
			(*pstate).scr_xsize_off =	8
			(*pstate).scr_ysize_off =	180
			end
		else: begin
			(*pstate).scr_xsize_off =	8
			(*pstate).scr_ysize_off =	167		; 140
			end
	endcase
endelse

case !version.os_family of
	'unix': begin
		xoff = 324
		end
	else: begin
		xoff = 286			; 284
		end
endcase

if (*pstate).help eq (*pstate).help2 then begin
	widget_control, (*pstate).help2, scr_xsize=((*pstate).w - xoff)
endif
end

;--------------------------------------------------------------------

; Which is closest index of 'x,y' vectors to point ,'xp,yp'
; This version accepts vectors 'x', 'y' and returns the closest index, else -1

function near_corr_xy, xp,yp, x,y, reverse=reverse

if n_elements(reverse) lt 1 then reverse=0

r = sqrt( float(x-xp)*float(x-xp) + float(y-yp)*float(y-yp) )
i = indgen(n_elements(r))

q = where( r lt 5)			; close proximity
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
pro pixel_corr_to_xy, pstate, px,py, x,y

COMPILE_OPT STRICTARR

x = clip( zoom_corr(pstate,px - (*pstate).margin.low, /down), 0,(*pstate).width-1)
y = clip( zoom_corr(pstate,py - (*pstate).margin.bottom, /down), 0,(*pstate).height-1)

return
end
;
;-----------------------------------------------------------------
; Plot handles at vector positions x,y

pro plot_corr_handles, pstate, x,y, color, data=data

if n_elements(data) lt 1 then data=0
if n_elements(x) lt 1 then return
boxx = [-1,1,1,-1,-1]
boxy = [-1,-1,1,1,-1]
scale = 2
for i=0L,n_elements(x)-1 do begin
	px = x[i] + boxx * scale
	py = y[i] + boxy * scale
	if data then begin
		px = clip( px, (*pstate).minx, (*pstate).maxx)
		py = clip( py, (*pstate).miny, (*pstate).maxy)
	endif else begin
		px = (*pstate).margin.low + clip( px-(*pstate).margin.low, 0, (*pstate).width-1)
		py = (*pstate).margin.bottom + clip( py-(*pstate).margin.bottom, 0, (*pstate).height-1)
	endelse
	plots, px,py, device=1-data, data=data, color=color
endfor

return
end

;-----------------------------------------------------------------
; Plot current spline marker

pro plot_corr_spline, pstate, compress=compress,wide=wide, data=data, $
						weightx=wtx, weighty=wty, use_conc=use_conc, bare=bare

if n_elements(data) eq 0 then data=0
if n_elements(use_conc) eq 0 then use_conc=0
if n_elements(wide) eq 0 then wide=0
if n_elements(wtx) eq 0 then wtx=0
if n_elements(wty) eq 0 then wty=0
if n_elements(bare) eq 0 then bare=0
p = (*pstate).pmark
if ptr_good(p) eq 0 then return

;  0 is centre handle, 1-* are control points

maxx = max((*p).x)
maxy = max((*p).y)
if (max([maxx,maxy]) eq 0) then return

; circle_vertices returns the 100 spline interpolation points

spline_corr_vertices, pstate, x,y,n, use_conc=use_conc
if n lt 1 then return

; plot handles

color = spec_colour('green')
corr_handles, pstate, x1,y1,n1, use_conc=use_conc

if data eq 0 then begin
	if use_conc then begin
		x1[0] = mean(x1)
		y1[0] = mean(y1)
		(*p).x = x1
		(*p).y = y1
;		print,'plot changed x,y=',(*p).x,(*p).y
	endif
	xy_corr_to_pixel, pstate, x1,y1, px,py, compress=compress, bare=bare
	if (wide eq 0) then plot_corr_handles, pstate, px,py, color
endif
;print,'handles: x1,y1=',x1,y1
;print,'pixels: px,py=',px,py

; plot the spline

if data then begin
	xy_corr_to_conc, pstate, x,y, px,py, /nozoom
	px = clip( px, (*pstate).minx, (*pstate).maxx)
	py = clip( py, (*pstate).miny, (*pstate).maxy)
	if wtx then px = px/10000.
	if wty then py = py/10000.
endif else begin
	xy_corr_to_pixel, pstate, x,y, px,py, compress=compress, bare=bare
endelse

if wide then begin
	plots, px,py, device=1-data, data=data, color=spec_colour('green'),thick=3.0			; b/w figures
endif else begin
	plots, px,py, device=1-data, data=data, color=color
endelse

(*p).present = 1
return
end

;-----------------------------------------------------------------

pro set_corr_map_help, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

map_corr_help, pstate
end

;-----------------------------------------------------------------------------------
; Set the view size and zoom for new corrs.
; If this is a clone, then assume that shapes already cleared.
; For zoom= and /full, don't change the tlb size, or the element.
;
; /clone	for a clone
; /full		for zoom to full corr
; zoom=+1,-1	for zoom in,out

pro set_corr_view, pstate, top, clone=clone, full=full, zoom=izoom, no_change=no_change

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
		warning,'Set_corr_view',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(clone) lt 1 then clone=0
	if n_elements(full) lt 1 then full=0
	if n_elements(izoom) lt 1 then izoom=0
	if n_elements(no_change) lt 1 then no_change=0

	draw_trim = 0
	scr_trim = 15
	if !version.os_family eq 'MacOS' then begin
		draw_trim = 15
		scr_trim = 21
	endif

	p = (*pstate).p
	if ptr_good( p) eq 0 then return
	xanes_stack_test, p, xanes, n_el, el, el_xanes

	old_zoom = (*pstate).zoom
	if clone eq 0 then begin
		if no_change eq 0 then begin
			(*pstate).zoom = ( (izoom eq 0) or (full eq 1) ) ? 0 : ((*pstate).zoom + izoom)
		endif
		(*pstate).width = zoom_corr( pstate, (*pstate).owidth)
		(*pstate).height = zoom_corr( pstate, (*pstate).oheight)
	endif

	w = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
	h = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top

	allocate_pixmap, w, h, new_wid=wid, old_wid=(*pstate).pix, error=error
	if error then goto, bad_pix
	(*pstate).pix = wid

	allocate_pixmap, w, h, new_wid=wid, old_wid=(*pstate).pix2, error=error
	if error then goto, bad_pix
	(*pstate).pix2 = wid

	if (full eq 0) and (izoom eq 0) and (no_change eq 0) then begin
		(*pstate).corr_x = 0
		(*pstate).corr_y = 1
		widget_control, (*pstate).element_idx, set_value=el, $
			set_combobox_select = 0
		widget_control, (*pstate).element_idy, set_value=el, $
			set_combobox_select = 1

		(*pstate).xbottom = 0
		(*pstate).xbottom = 0
		(*pstate).x_smooth = 1
		(*pstate).y_smooth = 1
		(*pstate).low = 0
		(*pstate).high = 100
		widget_control, (*pstate).X_slider, set_value=0
		widget_control, (*pstate).Y_slider, set_value=0
		widget_control, (*pstate).X_smooth_slider, set_value=3
		widget_control, (*pstate).Y_smooth_slider, set_value=3
		widget_control, (*pstate).Low_slider, set_value=0
		widget_control, (*pstate).High_slider, set_value=100
	endif

	if (izoom eq 0) and (no_change eq 0) then begin
		(*pstate).w = (w + scr_trim) < 600
		(*pstate).h = (h + scr_trim) < 600
	endif else begin
		(*pstate).w = (( (*pstate).w) > (256 + scr_trim)) < (w + scr_trim)
		(*pstate).h = (( (*pstate).h) > (64 + scr_trim)) < (h + scr_trim)
	endelse
	map_corr_help, pstate

	widget_control, (*pstate).draw2, draw_xsize=w+draw_trim, $
		draw_ysize=h+draw_trim, scr_xsize=(*pstate).w, scr_ysize=(*pstate).h

	if (clone eq 0) and (full eq 0) and (izoom eq 0) then begin
;		clear_all_markers, pstate
	endif
	goto, fix_position
	
bad_pix:
	(*pstate).zoom = old_zoom
	(*pstate).width = zoom_corr( pstate, (*pstate).owidth)
	(*pstate).height = zoom_corr( pstate, (*pstate).oheight)

fix_position:
	totx = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
	toty = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top
	(*pstate).position = [float((*pstate).margin.low)/float(totx), float((*pstate).margin.bottom)/float(toty), $
			1.-(float((*pstate).margin.high)/float(totx)), 1.-(float((*pstate).margin.top)/float(toty))]

	draw_corrs, pstate
	return
end

;-----------------------------------------------------------------
; Build list of spline vertices (in corr display coords).

pro spline_corr_vertices, pstate, x,y, n, use_conc=use_conc

if n_elements(use_conc) lt 1 then use_conc=0

p = (*pstate).pmark

;  1-10 (or 1-32) are control points, 0 is centre handle

if use_conc then begin
	cx = (*p).cx[1:*]
	cy = (*p).cy[1:*]
	conc_corr_to_xy, pstate, cx,cy, px,py, /nozoom, /noclip
endif else begin
	px = (*p).x[1:*]
	py = (*p).y[1:*]
endelse

spline_shape, px,py, x,y

n = n_elements(x)

maxx = max(px)
maxy = max(py)
if (max([maxx,maxy]) eq 0) then n=0

w = (*pstate).width
h = (*pstate).height
x = clip( x, 0, w-1)
y = clip( y, 0, h-1)
return
end
 
;-----------------------------------------------------------------
;
; Convert image 'x,y' to conc position 'cx,cy'
; NOTE: if formula changed here, change 'conc_corr_to_xy' too.

pro xy_corr_to_conc, pstate, x,y, cx,cy, nozoom=nozoom

cx = 0.0
cy = 0.0
if ptr_valid( (*pstate).p) eq 0 then return
if n_elements(nozoom) lt 1 then nozoom=0
if nozoom then begin
	w = (*pstate).owidth
	h = (*pstate).oheight
endif else begin
	w = (*pstate).width
	h = (*pstate).height
endelse

xmax = (*pstate).maxx
ymax = (*pstate).maxy
if (*pstate).logx then begin
	xmax1 = xmax
	xmax = xmax1 / 10^( 5.0 - 0.05*(*pstate).xtop)
	xmin = xmax1 / 10^( 5.0 - 0.045*(*pstate).xbottom) < xmax
	cx = alog10(xmin) + (x/(w-1))*alog10(xmax/xmin)
	cx = 10.^cx
endif else begin
	xmax1 = xmax
	xmax = xmax1 * (*pstate).xtop/100.
	xmin = xmax1 * (*pstate).xbottom/100.
	cx = x*(xmax-xmin)/(w-1) + xmin
endelse
if (*pstate).logy then begin
	ymax1 = ymax
	ymax = ymax1 / 10^( 5.0 - 0.05*(*pstate).ytop)
	ymin = ymax1 / 10^( 5.0 - 0.045*(*pstate).ybottom) < ymax
	cy = alog10(ymin) + (y/(h-1))*alog10(ymax/ymin)
	cy = 10.^cy
endif else begin
	ymax1 = ymax
	ymax = ymax1 * (*pstate).ytop/100.
	ymin = ymax1 * (*pstate).ybottom/100.
	cy = y*(ymax-ymin)/(h-1) + ymin
endelse

return
end

;-----------------------------------------------------------------
;
; Convert image 'x,y' to pixel position 'px,py'
; If compress, ignore zoom, scale x,y by compress.

pro xy_corr_to_pixel, pstate, x,y, px,py, compress=compress, bare=bare

px = 0
py = 0
if ptr_valid( (*pstate).p) eq 0 then return
if n_elements(bare) eq 0 then bare=0

xoff = (*pstate).margin.low
yoff = (*pstate).margin.bottom
if bare then begin
	xoff = 0
	yoff = 0
endif

if n_elements(compress) eq 0 then begin
	px = zoom_corr( pstate, x) + xoff
	py = zoom_corr( pstate, y) + yoff
endif else begin
	px = fix(compress * x) + xoff
	py = fix(compress * y) + yoff
endelse
return
end

;-----------------------------------------------------------------

function zoom_corr, pstate, n, down=down

if n_elements(down) eq 0 then down=0

if down eq 0 then begin
	if (*pstate).zoom gt 0 then begin
		x = n * 2^((*pstate).zoom)
	endif else if (*pstate).zoom lt 0 then begin
		x = n / 2^(-(*pstate).zoom)
	endif else begin
		x = n
	endelse
endif else begin
	if (*pstate).zoom gt 0 then begin
		x = n / 2^((*pstate).zoom)
	endif else if (*pstate).zoom lt 0 then begin
		x = n * 2^(-(*pstate).zoom)
	endif else begin
		x = n
	endelse
endelse

return, x
end

;------------------------------------------------------------------------------

; Stub routine for autoloading ...

pro corr_routines
end
