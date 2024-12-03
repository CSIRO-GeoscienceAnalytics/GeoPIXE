pro plot_corr, pstate, cgm=cgm, ps=ps, wmf=wmf, eps=eps, screen=screen, $
		file=file, options=options, jpeg=jpeg, png=png

; Plot corr association within axes.
;
; screen	flags output to existing screen window
;
; The plot 'options' have the form (see plot_corr_select):
;		{	Type:			'CGM'	CGM, METAFILE, PRINTER, ...
;			Crop:			0		crop images to shape
;			White:			1		for white background
;			SymSize:		0.5		symbol size
;			CharSize:		1.1		character size
;			CharThick:		0.8		character line thickness
;			LineThick:		1.4		drawing line thickness
;			ColourTable:	5		colour table
;			Invert:			1		to invert colour table
;			LabelAxes:		1		to label XY axes
;			ZaxisLegend:	1		to place a Z axis legend
;			DistLegend		1		for a distance legend
;			ppmOnly			1		for ppm and no wt%
;			Title: { on:	1		for a title
;				mode:		0,1,2	for filename, sample&grain, text
;				text:		'' }	optional text
;			DistPos:		0,1,2	for bottom, top, outside
;			DistColour:		0,1		for normal, reversed
;			LabelPos:		0,1...	for outside, top-left, top-right, bot-left, bot-right
;			LabelColour:	0,1		for normal, reversed
;			ConcMaxMode:	0		for Auto and Nice
;			ManualMax:		100.	max conc scale, in manual mode
;			ShowShape:		0,1		to draw the shape
;			Absolute:		0  		absolute distance scales with origin
;			centroids: { on: 0, $	plot region centroids for selected element ON
;					element: ''} }	select centroid element name

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
			warning,'plot_images',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			goto, cont
		endif
	endif
	common c_window_12, window_12_open
	if n_elements(window_12_open) lt 1 then window_12_open = 0

	if ptr_valid((*pstate).p) eq 0 then return
	p = (*pstate).p
	xanes_stack_test, p, xanes, n_el, el_names, el_xanes

	if n_elements(cgm) lt 1 then cgm = 0
	if n_elements(ps) lt 1 then ps = 0
	if n_elements(wmf) lt 1 then wmf = 0
	if n_elements(eps) lt 1 then eps = 0
	if n_elements(jpeg) lt 1 then jpeg = 0
	if n_elements(png) lt 1 then png = 0
	if n_elements(screen) lt 1 then screen = 0
	if n_elements(file) lt 1 then begin
		if cgm then file = 'Corr.cgm'
		if wmf then file = 'Corr.wmf'
		if eps then file = 'Corr.eps'
		if jpeg then file = 'Corr.jpg'
		if png then file = 'Corr.png'
	endif

	used_non_screen = 0
	xwin = !d.name
	b_hist = make_corr_tvb( pstate, (*pstate).corr_x, (*pstate).corr_y, $
				low=zlow, high=zhigh, range=range )
	if n_elements(b_hist) le 1 then return
	orgx = 0
	orgy = 0
	sx = n_elements(b_hist[*,0])
	sy = n_elements(b_hist[0,*])
	el1 = el_names[(*pstate).corr_x]
	el2 = el_names[(*pstate).corr_y]

	if (cgm or wmf or eps or jpeg or png) then begin
		F = file
		ext = extract_extension(file)
		F = strip_file_ext(file) + '-' + el1 + '-' + el2 + '.'+ext
	endif

	if cgm then begin
		used_non_screen = 1
		set_device, 'CGM', white=options.white, file=F, error=error
		if error then goto, cont
	endif else if wmf then begin
		used_non_screen = 1
		set_device, 'METAFILE', white=options.white, file=F, error=error
		if error then goto, cont
	endif else if eps then begin
		used_non_screen = 1
		set_device, 'PS', white=options.white, /landscape, /small, file=F, error=error
		if error then goto, cont
	endif else if png then begin
		used_non_screen = 0
		set_device, 'Z', white=options.white, aspect=(float(sy)/float(sx)), error=error
		if error then goto, cont
		screen = 1
	endif else if jpeg then begin
		used_non_screen = 0
		set_device, 'Z', /true, white=options.white, aspect=(float(sy)/float(sx)), error=error
		if error then goto, cont
		screen = 1
	endif else begin
		if screen eq 0 then begin
			if new_dialog_printersetup(/portrait) then begin
				used_non_screen = 1
			endif
		endif

		if (used_non_screen eq 0) then begin
;			if (screen eq 1) then begin
				xsize = 700
				ysize = 600
				
				set_device, xwin, white=options.white
				if !d.name eq xwin then begin
;					window,0, xsize=xsize, ysize=ysize
					if !d.name eq xwin then begin
						Catch, ErrorNo
						if (ErrorNo ne 0) then begin
							Catch, /cancel
							window_12_open = 0
						endif
						if window_12_open eq 0 then begin
							r = sqrt( (float(sx)/float(sy)) / (float(xsize)/float(ysize)) )
							xsize = xsize * r
							ysize = ysize / r
							window, 12, xsize=xsize, ysize=ysize, retain=retain
							window_12_open = 1
						endif
						wset, 12
						xsize = !d.x_size
						ysize = !d.y_size
					endif
				endif
;			endif else begin
;				xsize = !d.x_size
;				ysize = !d.y_size
;			endelse
		endif
	endelse

	!p.charsize = options.CharSize
	sz = !p.charsize
	!p.symsize = options.SymSize

	on_ioerror, cont
	default_plot, thick, athick, csize, cthick, thick_scale=options.LineThick, aspect=aspect

	!p.symsize = !p.symsize * csize/sz
	cthick = cthick * options.CharThick/options.LineThick
	tick = 0.5 * options.CharSize

	x_ch_size = !d.x_ch_size * csize				; default character dimensions
	y_ch_size = !d.y_ch_size * csize				; in device units

	if (!d.name eq 'METAFILE') then y_ch_size=y_ch_size/1.6	; fix flaw in metadata device

;----------------------------------------------------------------------------

; For test plot on screen, can TV plot with an offset now?
; What about a scaling? If not, will need to congrid one.

	rd = 4.0											; bottom

	leg_width = 3.										; as a number of X chars
	leg_height = 0.90									; as a fraction of ypix
	label_size = 1.6

	if options.ZaxisLegend then begin
		rbx = 7.5 + (2.0+leg_width)						; right (normal text)
		rby = 1.0										; right (rotated text)
	endif else if options.LabelPos eq 0 then begin
		rbx = 5.5										; no Z legend, but
		rby = 0.0										; label element outside
	endif else begin
		rbx = 2.0										; no Z legend and no label outside
		rby = 0.0										;
	endelse

	if options.LabelAxes then begin
		rax = 9.5		; left (normal text)			; number of characters in border
		ray = 1.0		; left (rotated text)
	endif else begin
		rax = 1.0		; left (normal text)
		ray = 0.0		; left (rotated text)
	endelse

	if options.title.on then begin
		rc = 3.0										; top (with title)
	endif else begin
		rc = 2.0										; top (no title)
	endelse

	aborder = rax * x_ch_size + ray * y_ch_size			; must use Y_CH for rotated text
	bborder = rbx * x_ch_size + rby * y_ch_size			; must use Y_CH for rotated text
	cborder = rc * y_ch_size
	dborder = rd * y_ch_size

	xborder = aborder + bborder
	yborder = cborder + dborder

	dx_size = !d.x_size
	dy_size = !d.y_size

	xmax = float(dx_size - xborder) > 10.
	ymax = float(dy_size - yborder) > 10.				; max size for image (device units)

	gamma = min([ xmax / sx, ymax / sy ])

	xpix = round(gamma * sx)							; image size (device units)
	ypix = round(gamma * sy)							; aspect maintained
	h_aspect = float(sy)/float(sx)

	xtot = (xborder + xpix) < dx_size					; total device units used
	ytot = (yborder + ypix) < dy_size

	xcentre = (dx_size - xtot)/2						; offset over unused pixels
	ycentre = (dy_size - ytot)/2

	xoff = round(xcentre + aborder)						; offset to plot window
	yoff = round(ycentre + rd * y_ch_size)

	xleg = leg_width * x_ch_size						; legend box (device coords)
	yleg = leg_height * ypix - 1.8*y_ch_size

	if options.LabelAxes then begin
		xloff = xoff + xpix + 2.0*x_ch_size				; offset to legend box (device)
	endif else begin
		xloff = xoff + xpix + 0.8*x_ch_size				; less with no axis labels
	endelse
	yloff = yoff + ypix - yleg

	erase												; erase before double plots
	if eps then begin
		polyfill,[0,1,1,0],[0,0,1,1], /norm, color=!p.background
	endif

back:
	nxpix = float(xpix) / !d.x_size						; image area in Normal coords
	nypix = float(ypix) / !d.y_size
	nxoff = float(xoff) / !d.x_size						; offset in Normal coords
	nyoff = float(yoff) / !d.y_size
	w = [nxoff,nyoff,nxoff+nxpix,nyoff+nypix]			; position plot window

	el = el1 + '-' + el2
	elz = el
	if xanes then elz=el_xanes
	top_title = ''
	case options.title.mode of
		0: begin
			top_title = strip_path( (*p).source)
			end
		1: begin
			top_title = (*p).sample + ' - ' + (*p).grain + ' - ' + el
			end
		2: begin
			top_title = options.title.text
			end
	endcase
	!p.title = ''

	nxleg = float(fix(xleg)) / !d.x_size				; legend area in Normal coords
	nyleg = float(fix(yleg)) / !d.y_size
	nxloff = float(fix(xloff)) / !d.x_size				; offset in Normal coords
	nyloff = float(fix(yloff)) / !d.y_size
	wl = [nxloff,nyloff,nxloff+nxleg,nyloff+nyleg]		; position plot window

	bar = bytarr(xleg,100)
	for i=0L,99 do bar[*,i] = byte(16 + i)				; colours
	label = ''

	print,'start options ...'
	if options.ZaxisLegend then begin
		style = label + 'frequency'

		print,'style=',style
		print,'zlow=',zlow,'  zhigh=',zhigh

;		Check the number of colour steps. If it is small, then we need to have
;		legend steps on these boundaries, and as nice round numbers.

		steps = intarr(100)
		tvlct, r,g,b, /get
		red = r
		green = g
		blue = b
		ns = count_steps( r[16:115], steps=q)
		steps[q] = 1
		ns = count_steps( g[16:115], steps=q)
		steps[q] = 1
		ns = count_steps( b[16:115], steps=q)
		steps[q] = 1
		step = where(steps eq 1)
		n_steps = n_elements(step)

		!y.title = ''
		!y.title = el + ' '
		if (*pstate).Zaxis eq 1 then !y.title = !y.title + ' Sqrt'
		!y.title = !y.title + ' ' + style
		!x.title = ''
		!p.title = ''

		if used_non_screen then begin
			tv, bar, xloff,yloff, xsize=xleg,ysize=yleg, /device
		endif else begin
			b2 = smart_congrid(bar,xleg+0.5,yleg+0.5)
			tv, b2, xloff,yloff,  /device
		endelse

		plots,[xloff,xloff+xleg,xloff+xleg,xloff,xloff], $
			[yloff,yloff,yloff+yleg,yloff+yleg,yloff], /device, thick=athick

;		Should also suppress X labels and ticks, and left Y ticks, and put Y label on RIGHT.
;		On RIGHT is a problem. Will need to do this manually.
;
;		plot,[0,0],[0,0], xrange=[0,100],yrange=[zlow,zhigh], /nodata, position=wl, $
;				xlog=0,ylog=0, xstyle=13,ystyle=1, xthick=athick,ythick=athick, $
;				charthick=cthick, thick=thick, charsize=csize, $
;				yticklen=1.0, /noerase

		if n_steps gt 16 then begin							; small number of colour steps
			if h_aspect lt 0.6 then begin
				zinc = one_sig_figure( zhigh/2.5, /positive, nice=nice)
			endif else begin
				zinc = one_sig_figure( zhigh/5., /positive, nice=nice)
			endelse
		endif

		az = yleg/(zhigh-zlow)
		bz = - az*zlow

		z = one_sig_figure( zlow, nice=nice) - one_sig_figure( zlow, nice=nice, /positive)
		n = 0
		ylast = 0
		while z le zhigh do begin
			if z ge zlow then begin
				dz = az*z + bz
				zs = z
				ez = zinc
				s = build_result(zs,ez,0.0)
				ns = strlen(s)
				plots, xloff+[0,xleg], yloff+[dz,dz], /device, thick=thick
				if yloff+dz gt ylast+1.2*y_ch_size then begin
					xyouts, xloff+xleg+0.5*x_ch_size, yloff+dz-y_ch_size/2, /device, $
								s, charsize=csize, charthick=cthick
					ylast = yloff+dz
				endif
				if n lt ns then n=ns
			endif
			z = z + zinc
		endwhile
		xyouts, xloff+xleg+(n+0.5)*x_ch_size+y_ch_size, yloff+yleg/2, /device, align=0.5, $
				!y.title, charsize=csize, charthick=cthick, orientation=90
	endif

	xm = range.x[1]
	ym = range.y[1]
	xm0 = range.x[0]
	ym0 = range.y[0]
	wtx = 0
	wty = 0
	case (*p).type of
		0: begin
			x_units = '(ppm)'
			y_units = '(ppm)'
			if (xm gt 3000.) then begin
				xm = xm / 10000.
				xm0 = xm0 / 10000.
				x_units = '(wt%)'
				wtx = 1
			endif
			if (ym gt 3000.) then begin
				ym = ym / 10000.
				ym0 = ym0 / 10000.
				y_units = '(wt%)'
				wty = 1
			endif
			end
		1: begin
			x_units = '(fraction)'
			y_units = '(fraction)'
			end
		2: begin
			x_units = '(counts)'
			y_units = '(counts)'
			end
		else: begin
			x_units = ''
			y_units = ''
			end
	endcase

	if options.LabelAxes then begin
		!x.title = el1 + ' ' + x_units + ''
		!y.title = el2 + ' ' + y_units + ''
		!x.style = 1
		!y.style = 1
	endif else begin
		!x.title = ''
		!y.title = ''
		!x.style = 13
		!y.style = 13
	endelse

	if used_non_screen then begin
		tv, b_hist, xoff,yoff, xsize=xpix,ysize=ypix, /device
	endif else begin
		b2 = smart_congrid(b_hist,xpix,ypix)
		tv, b2, xoff,yoff,  /device
	endelse

	if h_aspect gt 1.6 then begin
		xti = one_sig_figure( (xm)/2.5, /positive)
		yti = one_sig_figure( (ym)/5., /positive)
	endif else if h_aspect lt 0.6 then begin
		xti = one_sig_figure( (xm)/5., /positive)
		yti = one_sig_figure( (ym)/2.5, /positive)
	endif else begin
		xti = one_sig_figure( (xm)/5., /positive)
		yti = one_sig_figure( (ym)/5., /positive)
	endelse

	plot,[0,0],[0,0], xrange=[xm0,xm],yrange=[ym0,ym], /nodata, position=w, $
			xlog=(*pstate).logx,ylog=(*pstate).logy, xstyle=xstyle,ystyle=ystyle, xthick=athick,ythick=athick, $
			charthick=cthick, thick=thick, charsize=csize, $
			xticklen=-0.015*tick, yticklen=-0.015*tick*h_aspect, /noerase, $
			xtickinterval=xti, ytickinterval=yti
	plots,[nxoff,nxoff+nxpix,nxoff+nxpix,nxoff,nxoff], $
			[nyoff,nyoff,nyoff+nypix,nyoff+nypix,nyoff],/norm, thick=athick

	if options.ShowShape then begin
		plot_corr_spline, pstate, /data, weightx=wtx, weighty=wty, /use_conc
	endif

	if options.title.on and (strlen(top_title) ge 1) then begin
		xyouts, xoff+xpix/2, yoff+ypix+0.8*y_ch_size, /device, top_title, align=0.5, $
				charsize=1.2*csize, charthick=1.1*cthick
	endif

	n = strlen(el)
	offset = 0.4*label_size
	blank_on = 1
	c = !p.color
	if options.LabelColour eq 1 then begin
		c = !p.background
		blank_on = 0
	endif
	case options.LabelPos of
		0: begin
			xl = xloff														; outside
			yl = yoff
			blank_on = 0
			c = !p.color
			end
		1: begin
			xl = xoff + offset*x_ch_size									; top left
			yl = yoff + ypix - offset*y_ch_size - y_ch_size*label_size
			end
		2: begin
			xl = xoff + offset*x_ch_size									; bottom left
			yl = yoff + offset*y_ch_size
			end
		3: begin
			xl = xoff + xpix - offset*x_ch_size - n*x_ch_size*label_size	; top right
			yl = yoff + ypix - offset*y_ch_size - y_ch_size*label_size
			end
		4: begin
			xl = xoff + xpix - offset*x_ch_size - (n+0.5)*x_ch_size*label_size	; bottom right
			yl = yoff + offset*y_ch_size
			end
	endcase
	xv = xl - 0.2*label_size*x_ch_size + [0,(n+0.5)*x_ch_size,(n+0.5)*x_ch_size,0]*label_size
	yv = yl - 0.2*label_size*y_ch_size + [0,0,y_ch_size,y_ch_size]*label_size
	if blank_on then polyfill, xv,yv, /device, color=!p.background
	xyouts, xl,yl, elz, /device, charsize=label_size*csize, charthick=label_size*cthick, color=c

	if used_non_screen then begin
		device,/close								; a file was open
		on_ioerror, cont1
		if !d.name ne 'PRINTER' then device, file='null'
cont1:
;		if wmf then begin
;			on_ioerror, cont2
;			set_plot, xwin
;cont2:
;			on_ioerror, cont
;			file_move, Ftemp, F, /overwrite, /verbose
;		endif
	endif

	if png then begin
		c = tvrd()
		write_png, F, c, red,green,blue
	endif else if jpeg then begin
		c = tvrd( /true)
		write_jpeg, F, c, /true, quality=100
	endif

;-------------------------------------------------------------------------------------------

cont:
	if used_non_screen or png or jpeg then set_plot, xwin
	!p.charsize = 1.0
	!p.title = ''
	!x.title = ''
	!y.title = ''
	!p.thick = 1.0
	!p.charthick = 1.0
	return
end
