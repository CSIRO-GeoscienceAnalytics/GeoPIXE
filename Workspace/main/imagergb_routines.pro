;
;	ImageRGB_Routines, for ImageRGB.pro
;
;-----------------------------------------------------------------
; Draw the current Images on the draw widget

pro draw_ImageRGBs, pstate

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
		warning,'Draw_ImageRGBs',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

p = (*pstate).p
if ptr_good( p) eq 0 then return
if ptr_valid( (*p).Image ) eq 0 then return

; Set-up a 3D byte array for True colour image.
; Fill it with selected RGB colour maps.

wset, (*pstate).wid2

b = make_RGB_true( pstate)
;erase
if n_elements(b) gt 1 then begin
	device, decomposed=1
	tv, b, true=1
	device, decomposed=0
endif

;	Plot mark, using image window parameters, modified by local RGB window zoom ...

if ptr_good( (*pstate).pstate_parent) then begin
	pstate2 = (*pstate).pstate_parent				; image window pstate
	zt = (*pstate2).zoom
	(*pstate2).zoom = (*pstate).zoom
	plot_mark, pstate2
	(*pstate2).zoom = zt
endif

wset, (*pstate).pix
device,copy=[0,0,(*pstate).width,(*pstate).height, 0,0,(*pstate).wid2]
return
end

;-----------------------------------------------------------------

pro free_ImageRGB_state, pstate

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
		warning,'Free_ImageRGB_state',['IDL run-time error caught.', '', $
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
	if ptr_valid((*pstate).pwiz) then ptr_free, (*pstate).pwiz

return
end

;-----------------------------------------------------------------

function legend_RGB_string, pstate

COMPILE_OPT STRICTARR

if ptr_valid((*pstate).p) eq 0 then return,'Bad image pointer'

p = (*pstate).p
if ptr_good( p, /struct) eq 0 then return, 'null image'
palt = (*pstate).palt
palt2 = (*pstate).palt2
good_alt = ptr_good(palt)
good_alt2 = ptr_good(palt2)
xanes_stack_test, p, xanes, n_el, el, el_xanes
n_el2 = 0
n_el3 = 0
if good_alt then xanes_stack_test, palt, xanes2, n_el2, el2, el_xanes2
if good_alt2 then xanes_stack_test, palt2, xanes3, n_el3, el3, el_xanes3

(*pstate).image <= (n_el + n_el2 + n_el3-1)
if good_alt2 eq 0 then (*pstate).image <= (n_el + n_el2-1)
if good_alt eq 0 then (*pstate).image <= (n_el-1)
(*pstate).image2 <= (n_el + n_el2 + n_el3-1)
if good_alt2 eq 0 then (*pstate).image2 <= (n_el + n_el2-1)
if good_alt eq 0 then (*pstate).image2 <= (n_el-1)
(*pstate).image3 <= (n_el + n_el2 + n_el3-1)
if good_alt2 eq 0 then (*pstate).image3 <= (n_el + n_el2-1)
if good_alt eq 0 then (*pstate).image3 <= (n_el-1)

sizes = [n_el, n_el+n_el2, n_el+n_el2+n_el3]
i = (*pstate).image
q = where( i ge sizes, nset)

case nset of
	0: begin
		sel1 = el[i]
		end
	1: begin
		p = (*pstate).palt
		i = i - n_el
		sel1 = el2[i]
		end
	2: begin
		p = (*pstate).palt2
		i = i - (n_el + n_el2)
		sel1 = el3[i]
		end
	else:
endcase

pimg = (*p).Image
opt = (*p).options
sx = (*p).xsize
sy = (*p).ysize

top1 = ((*opt)[i].max * (*opt)[i].top / 100.)

s1 = string(top1)
if (*p).type eq 1 then begin
	charge_per_pixel = 1.0
	units1 = ''
	style1 = 'fraction'
endif else if (*p).type eq 2 then begin
	units1 = ''
	style1 = 'counts'
endif else begin
	units1 = 'ppm'
	style1 = 'conc'
	charge_per_pixel = (*p).charge / (sx * sy)
	top1 = top1 / charge_per_pixel
	s1 = str_tidy(top1)
	if top1 gt 999.9 then begin
		s1 = str_tidy(top1/10000.)
		units1 = 'wt%'
	endif
endelse

i = (*pstate).image2
q = where( i ge sizes, nset)
case nset of
	0: begin
		sel2 = el[i]
		end
	1: begin
		p = (*pstate).palt
		i = i - n_el
		sel2 = el2[i]
		end
	2: begin
		p = (*pstate).palt2
		i = i - (n_el + n_el2)
		sel2 = el3[i]
		end
	else:
endcase

pimg = (*p).Image
opt = (*p).options
if ptr_valid(opt) eq 0 then return,'no options'
sx = (*p).xsize
sy = (*p).ysize

top2 = ((*opt)[i].max * (*opt)[i].top / 100.)

s2 = string(top2)
if (*p).type eq 1 then begin
	charge_per_pixel = 1.0
	units2 = ''
	style2 = 'fraction'
endif else if (*p).type eq 2 then begin
	units2 = ''
	style2 = 'counts'
endif else begin
	units2 = 'ppm'
	style2 = 'conc'
	charge_per_pixel = (*p).charge / (sx * sy)
	top2 = top2 / charge_per_pixel
	s2 = str_tidy(top2)
	if top2 gt 999.9 then begin
		s2 = str_tidy(top2/10000.)
		units2 = 'wt%'
	endif
endelse

i = (*pstate).image3
q = where( i ge sizes, nset)
case nset of
	0: begin
		sel3 = el[i]
		end
	1: begin
		p = (*pstate).palt
		i = i - n_el
		sel3 = el2[i]
		end
	2: begin
		p = (*pstate).palt2
		i = i - (n_el + n_el2)
		sel3 = el3[i]
		end
	else:
endcase

pimg = (*p).Image
opt = (*p).options
sx = (*p).xsize
sy = (*p).ysize

top3 = ((*opt)[i].max * (*opt)[i].top / 100.)

s3 = string(top3)
if (*p).type eq 1 then begin
	charge_per_pixel = 1.0
	units3 = ''
	style3 = 'fraction'
endif else if (*p).type eq 2 then begin
	units3 = ''
	style3 = 'counts'
endif else begin
	units3 = 'ppm'
	style3 = 'conc'
	charge_per_pixel = (*p).charge / (sx * sy)
	top3 = top3 / charge_per_pixel
	s3 = str_tidy(top3)
	if top3 gt 999.9 then begin
		s3 = str_tidy(top3/10000.)
		units3 = 'wt%'
	endif
endelse

sl1 = 'Max ' + sel1 + ' (' + style1 + '; RED) = ' + s1 + ' ' + units1 + ' (zoom=' + str_tidy((*pstate).zoom) + ')'
sl2 = 'Max ' + sel2 + ' (' + style2 + '; GREEN) = ' + s2 + ' ' + units2
sl3 = 'Max ' + sel3 + ' (' + style3 + '; BLUE) = ' + s3 + ' ' + units3
s = [sl1,sl2,sl3]

return, s
end

;-----------------------------------------------------------------
; Make the byte array to TV to the draw area
;
; Use /nozoom for non-screen applications.
;
; Clip using xmin,xmax, ymin,ymax.
; Rescale onto different output size using /nozoom, xtgt, ytgt

function make_RGB_tvb, pstate, ii, nozoom=nozoom, xmax=xmax, ymax=ymax, xmin=xmin, ymin=ymin, $
			xtgt=xtgt, ytgt=ytgt, compress=compress, enhance=enhance, colour=colouri, centroids=centroids

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
			warning,'Make_RGB_TVB',['IDL run-time error caught.', '', $
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
	if n_elements(centroids) lt 1 then centroids=0
		if n_elements(colouri) lt 1 then begin
			colour = 255B
			dcolour = 255B
		endif else if n_elements(colouri) eq 2 then begin
			colour = colouri[0]
			dcolour = colouri[1]
		endif else begin
			colour = colouri
			dcolour = colouri
		endelse
	
	i = ii
	b = 0B
	if ptr_valid( (*pstate).pstate_parent) then begin
		ps = (*(*pstate).pstate_parent).p
		qc = (*(*pstate).pstate_parent).qc
		corr_on = (*(*pstate).pstate_parent).corr_on
	endif else begin
		if n_elements(args) eq 0 then return, b
		ps = args.p
		qc = args.qc
		corr_on = args.corr_on
		centroids = 0					; no pstate access
	endelse

	p = (*pstate).p
	if ptr_good( p) eq 0 then return, b
	palt = (*pstate).palt
	palt2 = (*pstate).palt2
	good_alt = ptr_good(palt)
	good_alt2 = ptr_good(palt2)
	xanes_stack_test, p, xanes, n_el, el, el_xanes
	n_el2 = 0
	n_el3 = 0
	if good_alt then xanes_stack_test, palt, xanes2, n_el2, el2, el_xanes2
	if good_alt2 then xanes_stack_test, palt2, xanes3, n_el3, el3, el_xanes3
	
	i <= (n_el + n_el2 + n_el3-1)
	if good_alt2 eq 0 then i <= (n_el + n_el2-1)
	if good_alt eq 0 then i <= (n_el-1)
	
	sizes = [n_el, n_el+n_el2, n_el+n_el2+n_el3]
	q = where( i ge sizes, nset)
	case nset of
		1: begin
			p = (*pstate).palt
			i = i - n_el
			end
		2: begin
			p = (*pstate).palt2
			i = i - (n_el + n_el2)
			end
		else:
	endcase
	
	pimg = (*p).Image
	opt = (*p).options
	
	if ptr_valid( pimg ) eq 0 then return, b
	if (*opt)[i].top le (*opt)[i].bottom then (*opt)[i].top = (*opt)[i].bottom + 1.0
	
	enhance_spots = 0
	if (var_type(enhance) eq 8) then begin				; must be a 'struct' 
		if enhance.spots.on then begin
	
			was_el = enhance.spots.elements
			s = strsplit( enhance.spots.elements, ' 	', /extract)
			ns = n_elements(s)
			q = where( (*(*p).el)[i] eq s, nq)
			if nq eq 0 then begin
				z1 = intarr(ns)
				for j=0,ns-1 do begin
					el_code, s[j], el1, z, shell1, bad1, error1
					z1[j] = z
				endfor
				el_code, (*(*p).el)[i], el2, z2, shell2, bad2, error2
				q = where( z1 eq z2, nq)
			endif
			if (nq ge 1) then enhance_spots = 1
		endif
	endif
	
	low = (*opt)[i].bottom * (*opt)[i].max / 100.
	high = (*opt)[i].top * (*opt)[i].max / 100.
	compress = 1
	sx0 = n_elements( (*pimg)[*,0,0])
	sy0 = n_elements( (*pimg)[0,*,0])
	sx = sx0
	sy = sy0
	
	if (xmax ne 0) or (ymax ne 0) then begin
		if xmax eq 0 then xmax = sx0-1
		if ymax eq 0 then ymax = sy0-1
		sx = (xmax - xmin +1) < sx0
		sy = (ymax - ymin +1) < sy0
		xl = xmin > 0
		yl = ymin > 0
		xh = xmax < (sx0-1)
		yh = ymax < (sy0-1)
	endif else begin
		xl = 0
		xh = sx - 1
		yl = 0
		yh = sy - 1
	endelse
	
	build_image_scale, (*opt)[i], low, high, image=(*pimg)[xl:xh,yl:yh,i], output=img
	
	b = bytscl( img, top=255, min=low, max=high)
	
	if var_type(centroids) eq 8 then begin				; must be a 'struct'
		was_el = centroids.element
		pr = (*(*pstate).pstate_parent).pregions
		nreg = n_elements(*pr)
		q = where( centroids.element eq *(*p).el, nq)
		if nq eq 0 then begin
			el_code, centroids.element, el1, z1, shell1, bad1, error1
			el_code, *(*p).el, el2, z2, shell2, bad2, error2
			q = where( z1 eq z2, nq)
			if z1 eq 0 then centroids=0
		endif
		if (nq ge 1) then begin
			i = q[0]
		endif else centroids=0
		if (var_type(centroids) ne 8) then begin
			warning,'Make_RGB_TVB',['Centroid element "'+was_el+'" not found.','', $
					'Ignoring centroids ...']
		endif
	endif

	do_corr_highlight = 0
	bc = bytarr( sx0, sy0)

;	For each region, mark out a circle centred on the centroid.
;	If circles overlap, remove the intersections, and only show the outer envelope.
;	Comments below for "clear" (pixels enclosed by circles) and "line" (the circle shape/outline).

	if (var_type(centroids) eq 8) then begin			; must be a 'struct' 
		if (nreg ge 1) then begin
			r = clip( 15 * (float(sy)/700), 8, 25)
			do_corr_highlight = 1
			for j=1,nreg-1 do begin
				x = (*(*(*pr)[j]).centroid)[i].x
				y = (*(*(*pr)[j]).centroid)[i].y
				absx = (*(*pr)[j]).xoffset + x * (*(*pr)[j]).xcompress
				absy = (*(*pr)[j]).yoffset + y * (*(*pr)[j]).ycompress
				px = (absx - (*p).xoffset) / (*p).xcompress
				py = (absy - (*p).yoffset) / (*p).ycompress
				print,j,'  centroid: ',x,y, '  abs: ', absx,absy, '  pixel: ', px,py

				circle_diam, px-r,py-r, px+r,py+r, xc,yc, n=100
				qf = polyfillv( xc,yc, sx0, sy0)
				bc[qf] = bc[qf] + 1						; clear fill
				bc[xc,yc] = bc[xc,yc] + 100				; circle surround

				x0 = (min(xc)-2) > 0					; use a sub-region for speed ...
				x1 = (max(xc)+2) < (sx0-1)
				y0 = (min(yc)-2) > 0
				y1 = (max(yc)+2) < (sy0-1)
				bt = bc[x0:x1,y0:y1]
				q = where( bt eq 102, nq)				; line on prev clear
				if nq gt 0 then bt[q]=2
				q = where( bt eq 3, nq)					; clear on prev clear
				if nq gt 0 then bt[q]=2
				q = where( bt eq 121, nq)				; clear on prev line
				if nq gt 0 then bt[q]=2
				q = where( bt eq 103, nq)				; line+clear on prev clear
				if nq gt 0 then bt[q]=2
				q = where( bt eq 220, nq)				; line on prev line
				if nq gt 0 then bt[q]=120
				q = where( bt eq 221, nq)				; line+clear on prev line
				if nq gt 0 then bt[q]=120
		
				q = where( bt eq 1, nq)					; final clear
				if nq gt 0 then bt[q]=2
				q = where( bt eq 101, nq)				; this line on this clear
				if nq gt 0 then bt[q]=120
				q = where( bt eq 100, nq)				; final line
				if nq gt 0 then bt[q]=120
				bc[x0:x1,y0:y1] = bt					; copy sub-region back

			endfor
			q = where( (bc eq 0) and (shift(bc,1,0) eq 2), nq)	; missing circle pixels (horiz)
			if nq gt 0 then bc[q] = 120
			q = where( (bc eq 2) and (shift(bc,1,0) eq 0), nq)
			if nq gt 0 then bc[q] = 120
			q = where( (bc eq 0) and (shift(bc,0,1) eq 2), nq)	; missing circle pixels (vert)
			if nq gt 0 then bc[q] = 120
			q = where( (bc eq 2) and (shift(bc,0,1) eq 0), nq)
			if nq gt 0 then bc[q] = 120

			q = where( bc eq 2, nq)						; zero clear
			if nq gt 0 then bc[q]=0
			q = where( bc eq 120, nq)					; final line
			if nq gt 0 then bc[q]=100
		endif
	endif

	if ptr_valid( qc) and corr_on then begin		
		if (*qc)[0] ne -1 then begin
			bc[*qc] = 100
;			if highlight_only then b[*]=0				; only highlight, not image
			do_corr_highlight = 1
		endif
	endif
	bc = bc[xl:xh,yl:yh]

	if nozoom then begin
		if (ytgt ne 0) and (xtgt ne 0) then begin
			compress = min( [ float(xtgt) / float(sx), float(ytgt) / float(sy) ])
		endif

;		Hot spots tend to disappear for compress<1 or on big plots
;		Use 'dilate' to expand them so that they remain visible.

		if enhance_spots then begin
			if compress lt 1. then begin
				n = round((1./compress) + 0.4)
				s = round_kernel(n)
				b = dilate(b,s,/gray)
			endif else if sy gt 400 then begin
				n = ceil(sy/400.)
				s = round_kernel(n)
				b = dilate(b,s,/gray)
			endif
		endif
	
		if compress ne 1 then begin
			b = smart_congrid( b, sx*compress, sy*compress)
			if do_corr_highlight then begin
				bc = smart_congrid( bc, sx*compress, sy*compress)
			endif
		endif
	endif else begin
		if ((*pstate).zoom ne 0) then begin
			compress = float((*pstate).width) / float(sx)

;			Hot spots tend to disappear for compress<1 or on big plots
;			Use 'dilate' to expand them so that they remain visible.

			if enhance_spots then begin
				if compress lt 1. then begin
					n = round((1./compress) + 0.4)
					s = round_kernel(n)
					b = dilate(b,s,/gray)
				endif
			endif
	
			b = smart_congrid( b, (*pstate).width, (*pstate).height)
			if do_corr_highlight then begin
				bc = smart_congrid( bc, (*pstate).width, (*pstate).height)
			endif
		endif
	endelse
	
	if do_corr_highlight then begin
		q = where( bc ge 50)		
		if q[0] ne -1 then b[q] = colour
		q = where( (bc ge 20) and (bc lt 50))
		if q[0] ne -1 then b[q] = dcolour
	endif
	return, b
end

;-----------------------------------------------------------------

function make_RGB_true, pstate, nozoom=nozoom, xmax=xmax, ymax=ymax, xmin=xmin, ymin=ymin, $
				xtgt=xtgt, ytgt=ytgt, enhance=enhance, centroids=centroids

COMPILE_OPT STRICTARR
b = 0B
p = (*pstate).p
if ptr_valid(p) eq 0 then return, b
pimg = (*p).Image

bt = make_RGB_tvb( pstate, (*pstate).image, nozoom=nozoom, xmax=xmax, ymax=ymax, xmin=xmin, ymin=ymin, xtgt=xtgt, ytgt=ytgt, enhance=enhance, centroids=centroids)
if n_elements(bt) le 1 then return, 0B
sx = n_elements(bt[*,0])
sy = n_elements(bt[0,*])

b = bytarr(3,sx,sy)
b[0,*,*] = bt

bt = make_RGB_tvb( pstate, (*pstate).image2, nozoom=nozoom, xmax=xmax, ymax=ymax, xmin=xmin, ymin=ymin, xtgt=xtgt, ytgt=ytgt, enhance=enhance, centroids=centroids)
if n_elements(bt) gt 1 then begin
	b[1,*,*] = bt
endif
bt = make_RGB_tvb( pstate, (*pstate).image3, nozoom=nozoom, xmax=xmax, ymax=ymax, xmin=xmin, ymin=ymin, xtgt=xtgt, ytgt=ytgt, enhance=enhance, centroids=centroids)
if n_elements(bt) gt 1 then begin
	b[2,*,*] = bt
endif

return, b
end

;------------------------------------------------------------------

pro map_RGB_help, pstate

COMPILE_OPT STRICTARR
if (*pstate).w gt 450 then begin
	if (*pstate).help eq (*pstate).help2 then goto, more

	(*pstate).help = (*pstate).help2
	widget_control, (*pstate).help1_base, map=0
	widget_control, (*pstate).help1, scr_ysize=1
	widget_control, (*pstate).query1, scr_ysize=1
	widget_control, (*pstate).query2, scr_xsize=15
	widget_control, (*pstate).help2_base, map=1
	widget_control, (*pstate).help2, ysize=3
endif else begin
	if (*pstate).help eq (*pstate).help1 then goto, more

	(*pstate).help = (*pstate).help1
	widget_control, (*pstate).help2_base, map=0
	widget_control, (*pstate).help2, scr_xsize=1
	widget_control, (*pstate).query2, scr_xsize=1
	widget_control, (*pstate).query1, scr_ysize=20
	widget_control, (*pstate).help1, ysize=3
	widget_control, (*pstate).help1_base, map=1
	widget_control, (*pstate).help2, scr_ysize=1
endelse

more:
print,'map_RGB_help: w,h=', (*pstate).w, (*pstate).h
if (*pstate).w gt 450 then begin
	case !version.os_family of
		'MacOS': begin
			(*pstate).scr_xsize_off =	1
			(*pstate).scr_ysize_off =	64
			end
		'unix': begin
			(*pstate).scr_xsize_off =	1
			(*pstate).scr_ysize_off =	96
			end
		else: begin
			(*pstate).scr_xsize_off =	8
			(*pstate).scr_ysize_off =	59		; 65
			end
	endcase
endif else begin
	case !version.os_family of
		'MacOS': begin
			(*pstate).scr_xsize_off =	1
			(*pstate).scr_ysize_off =	81
			end
		'unix': begin
			(*pstate).scr_xsize_off =	8
			(*pstate).scr_ysize_off =	133
			end
		else: begin
			(*pstate).scr_xsize_off =	8
			(*pstate).scr_ysize_off =	83
			end
	endcase
endelse

case !version.os_family of
	'unix': begin
		xoff = 296
		end
	else: begin
		xoff = 293			; 274
		end
endcase

if (*pstate).help eq (*pstate).help2 then begin
	widget_control, (*pstate).help2, scr_xsize=((*pstate).w - xoff)
endif
end

;-----------------------------------------------------------------

function point_ImageRGB, pstate, opt=opt, nx=nx, ny=ny

COMPILE_OPT STRICTARR
	p = (*pstate).p
	if ptr_valid( p) eq 0 then return, 0L
	if ptr_valid( (*p).options ) eq 0 then return, 0L

	pimg = (*p).Image
	opt = (*p).options
	nx = (*p).xsize
	ny = (*p).ysize

	return, pimg
end

;-----------------------------------------------------------------

pro set_RGB_map_help, wWidget

COMPILE_OPT STRICTARR
top = tlb_id( wWidget)
if widget_info( top, /valid) eq 0 then return
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

map_RGB_help, pstate
end

;-----------------------------------------------------------------
; Set the view size and zoom for new ImageRGBs.
; For zoom= and /full, don't change the tlb size, or the element.
;
; /full		for zoom to full ImageRGB
; zoom=+1,-1	for zoom in,out

pro set_RGB_view, pstate, top, full=full, zoom=izoom, no_change=no_change, $
					realize=realize

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
		warning,'Set_RGB_view',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(full) lt 1 then full=0
	if n_elements(izoom) lt 1 then izoom=0
	if n_elements(no_change) lt 1 then no_change=0
	if n_elements(realize) lt 1 then realize=0

	draw_trim = 0
	scr_trim = 15
	if !version.os_family eq 'MacOS' then begin
		draw_trim = 15
		scr_trim = 21
	endif

	p = (*pstate).p
	if ptr_good( p) eq 0 then return
	palt = (*pstate).palt
	palt2 = (*pstate).palt2
	good_alt = ptr_good(palt)
	good_alt2 = ptr_good(palt2)
	if good_alt2 and (good_alt eq 0) then begin
		palt = palt2
		palt2 = ptr_new()
		good_alt = 1
		good_alt2 = 0
	endif
	xanes_stack_test, p, xanes, n_el, el, el_xanes
	n_el2 = 0
	n_el3 = 0
	if good_alt then xanes_stack_test, palt, xanes2, n_el2, el2, el_xanes2
	if good_alt2 then xanes_stack_test, palt2, xanes3, n_el3, el3, el_xanes3
	
	zoom0 = 0
	if (full eq 0) and (izoom eq 0) and (no_change eq 0) then begin
	  	if n_elements( *p) gt 0 then begin
			if ((*p).xsize gt 1600) or ((*p).ysize gt 1600) then begin
			   	r = max([ (*p).xsize/1600., (*p).ysize/1600.])
			   	zoom0 = -( 1 + fix(alog(r)/alog(2)))
			endif
		endif
	endif

	new = {zoom:(*pstate).zoom, width:(*pstate).width, height:(*pstate).height, w:(*pstate).w, h:(*pstate).h }
	if no_change eq 0 then begin
		new.zoom = ( (izoom eq 0) or (full eq 1) ) ? zoom0 : ((*pstate).zoom + izoom)
	endif
	(*pstate).zoom = new.zoom
	w = 300
	h = 300
  	if n_elements( *p) gt 0 then begin
		w = (*p).xsize
		h = (*p).ysize
	endif
	new.width = zoom( pstate, w)
	new.height = zoom( pstate, h)
	new.zoom = (*pstate).zoom
	if (izoom eq 0) and (no_change eq 0) then begin
		new.w = (new.width + scr_trim) < 600
		new.h = (new.height + scr_trim) < 600
	endif else begin
		new.w = (( new.w) > (256 + scr_trim)) < (new.width + scr_trim)
		new.h = (( new.h) > (64 + scr_trim)) < (new.height + scr_trim)
	endelse

	widget_control, (*pstate).draw2, draw_xsize=new.width+draw_trim, $
		draw_ysize=new.height+draw_trim, scr_xsize=new.w, scr_ysize=new.h

	if widget_info( (*pstate).draw2, /valid) eq 0 then begin
		warning,'set_RGB_view',['Failed to allocate memory for larger image.','IDL Draw ID has become undefined.', $
			'','Can not recover Draw widget.', 'Will need to close window.','', $
			'Retry RGB Image window open,','and avoid excessive Zoom in "+".']
		free_imageRGB_state, pstate
		if n_elements(top) ge 1 then widget_control, top, /destroy		
		return
	endif
		
	allocate_pixmap, new.width, new.height, new_wid=wid, old_wid=(*pstate).pix, error=error
	if error then goto, bad_pix
	(*pstate).pix = wid

	allocate_pixmap, new.width, new.height, new_wid=wid, old_wid=(*pstate).pix2, error=error
	if error then goto, bad_pix
	(*pstate).pix2 = wid

	if (full eq 0) and (izoom eq 0) and (no_change eq 0) and (realize eq 0) then begin
		(*pstate).Image = 0
		(*pstate).Image2 = 0
		(*pstate).Image3 = 0
		s = el
		if good_alt then s = [el,el2]
		if good_alt2 then s = [s,el3]
		widget_control, (*pstate).element_id1, set_value=s, set_combobox_select = 0
		widget_control, (*pstate).element_id2, set_value=s, set_combobox_select = 0
		widget_control, (*pstate).element_id3, set_value=s, set_combobox_select = 0
	endif

	(*pstate).zoom = new.zoom
	(*pstate).width = new.width
	(*pstate).height = new.height
	(*pstate).w = new.w
	(*pstate).h = new.h
	
	if widget_info( (*pstate).max_id1, /valid) then begin
		(*pstate).image <= (n_el + n_el2 + n_el3-1)
		if good_alt2 eq 0 then (*pstate).image <= (n_el + n_el2-1)
		if good_alt eq 0 then (*pstate).image <= (n_el-1)
		(*pstate).image2 <= (n_el + n_el2 + n_el3-1)
		if good_alt2 eq 0 then (*pstate).image2 <= (n_el + n_el2-1)
		if good_alt eq 0 then (*pstate).image2 <= (n_el-1)
		(*pstate).image3 <= (n_el + n_el2 + n_el3-1)
		if good_alt2 eq 0 then (*pstate).image3 <= (n_el + n_el2-1)
		if good_alt eq 0 then (*pstate).image3 <= (n_el-1)

		sizes = [n_el, n_el+n_el2, n_el+n_el2+n_el3]
		q = where( (*pstate).image ge sizes, ns)
		(*pstate).Image1Alt = ns
		q = where( (*pstate).image2 ge sizes, ns)
		(*pstate).Image2Alt = ns
		q = where( (*pstate).image3 ge sizes, ns)
		(*pstate).Image3Alt = ns

		case (*pstate).Image1Alt of
			0: begin
				opt = (*p).options
				n = (*pstate).Image
				end
			1: begin
				opt = (*palt).options
				n = (*pstate).Image - n_el
				end
			2: begin
				opt = (*palt2).options
				n = (*pstate).Image - (n_el + n_el2)
				end
		endcase
		widget_control, (*pstate).max_id1, set_value=str_tidy( (*opt)[n].top)

		case (*pstate).Image2Alt of
			0: begin
				opt = (*p).options
				n = (*pstate).Image2
				end
			1: begin
				opt = (*palt).options
				n = (*pstate).Image2 - n_el
				end
			2: begin
				opt = (*palt2).options
				n = (*pstate).Image2 - (n_el + n_el2)
				end
		endcase
		widget_control, (*pstate).max_id2, set_value=str_tidy( (*opt)[n].top)

		case (*pstate).Image3Alt of
			0: begin
				opt = (*p).options
				n = (*pstate).Image3
				end
			1: begin
				opt = (*palt).options
				n = (*pstate).Image3 - n_el
				end
			2: begin
				opt = (*palt2).options
				n = (*pstate).Image3 - (n_el + n_el2)
				end
		endcase
		widget_control, (*pstate).max_id3, set_value=str_tidy( (*opt)[n].top)
	endif
	
	if (realize eq 0) then map_RGB_help, pstate
	if ptr_valid( p) then draw_ImageRGBs, pstate
	return
	
bad_pix:
	widget_control, (*pstate).draw2, draw_xsize=(*pstate).width+draw_trim, $
		draw_ysize=(*pstate).height+draw_trim, scr_xsize=(*pstate).w, scr_ysize=(*pstate).h
	return
end

;------------------------------------------------------------------------------

pro set_RGB_display_range, options, el, opt, type, display_mode, charge_per_pixel, $
		zlow=zlow, zhigh=zhigh, style=style, qscale=qscale, units=units

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
		warning,'set_RGB_display_range',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	qscale = 1.0
	if type eq 1 then begin
		build_image_scale, opt, zlow, zhigh, root=display_mode,label=label
		charge_per_pixel = 1.0
		units = ''
		style = label 	+ ((display_mode eq 1) ? 'variance' : 'fraction')
	endif else if type eq 3 then begin
		build_image_scale, opt, zlow, zhigh, root=display_mode,label=label
		charge_per_pixel = 1.0
		units = ''
		style = label 	;+ ((display_mode eq 1) ? 'variance' : 'energy')
	endif else if type eq 2 then begin
		build_image_scale, opt, zlow, zhigh, root=display_mode,label=label
		units = ''
		style = label 	+ ((display_mode eq 1) ? 'variance' : 'counts')
	endif else begin
		if strlowcase( strmid(el,0,4)) eq 'back' then begin
			build_image_scale, opt, zlow, zhigh, root=display_mode, $
					scale=10000., check_wt=0, wt=wt,label=label
			units = ''
			style = label 	+ ((display_mode eq 1) ? 'variance' : 'intensity')
			qscale = 0.0001
		endif else begin
			build_image_scale, opt, zlow, zhigh, root=display_mode, $
					scale=1./charge_per_pixel, check_wt=(options.ppmOnly eq 0), wt=wt,label=label
			units = ' (ppm)'
			style = label 	+ ((display_mode eq 1) ? 'variance' : 'conc')
			qscale = charge_per_pixel
		endelse
		if wt then begin
			units = ' (wt%)'
			qscale = qscale * 10000.
		endif
	endelse

;	print,'style=',style,' units=',units,' qscale=',qscale
;	print,'zlow=',zlow,'  zhigh=',zhigh
;	print,' bottom=',opt.bottom,'  top=',opt.top

return
end

;------------------------------------------------------------------------------

; Stub routine for autoloading ...

pro ImageRGB_routines
end
