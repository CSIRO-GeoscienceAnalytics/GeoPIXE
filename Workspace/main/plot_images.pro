pro plot_images, pstate, cgm=cgm, wmf=wmf, eps=eps, screen=screen, file=file, png=png, jpeg=jpeg, $
				select=selecti, options=options, crop=crop

; Plot images in boxes
;
; For /CGM and /WMF, output a single image per file (append element name to file name).
; For normal plots, then do 2 images per page in portrait.
;
; The plot 'options' have the form (see define(/plot_options)):
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
;			centroids: { on: 0		plot region centroids for selected element ON
;					element: ''}	select centroid element name
;			enhance: { 
;					spots: { on: 0, 	 enhance hot-spots
;						elements: ''	 select enhance element names (1 to 3, sep by spaces)
;			max_area:		 0			use a maximum display area, strip borders
;			separate:		0		plot separate spectra, or common axes
;			landscape:		0		Landscape orientation
;			Learn: { on:	0		to use a "Learn" RGB file for planes
;					file:	'' }	"Learn" filename
;			ShowALLregions:	0}		show ALL regions in plot
;		}

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
	if n_elements(wmf) lt 1 then wmf = 0
	if n_elements(eps) lt 1 then eps = 0
	if n_elements(png) lt 1 then png = 0
	if n_elements(jpeg) lt 1 then jpeg = 0
	if n_elements(screen) lt 1 then screen = 0
	if n_elements(selecti) lt 1 then begin
		select = intarr( n_el)
		select[(*pstate).image] = 1
	endif else begin
		select = selecti
	endelse
	if n_elements(file) lt 1 then begin
		if cgm then file = 'Images.cgm'
		if wmf then file = 'Images.wmf'
		if eps then file = 'Images.eps'
		if png then file = 'Images.png'
		if jpeg then file = 'Images.jpg'
	endif
	if n_elements(options) lt 1 then options = (*(*pstate).pexport).plot

	used_non_screen = 0
	portrait = 1						; portrait plot
	landscape = 0
	multi = 0							; multiple images per page (2)
	again = 0							; set this to veto printer dialog, second time around
	xwin = !d.name
	absolute = 0
	if options.absolute eq 1 then absolute=1
	nice = 0
	if options.ConcMaxMode eq 0 then nice=1
	case !version.os_family of
		'MacOS': begin
			retain = 2
			end
		'unix': begin
			retain = 2
			end
		else: begin
			retain = 1
			end
	endcase

	qselect = where(select eq 1)
	if qselect[0] eq -1 then goto, cont
	nq = n_elements(qselect)
	iselect = 0

more:
	qplot = qselect[iselect]
	pimg = point_image( pstate, opt=opt, nx=sx, ny=sy)
	if ptr_valid(pimg) eq 0 then goto, cont

	sx0 = sx
	sy0 = sy
	orgx = 0
	orgy = 0
	if options.Crop and (n_elements(crop) ge 1) then begin
		if size(crop,/tname) eq 'STRUCT' then begin
			sx = (crop.x[1]<(sx0-1)) - (crop.x[0]>0) +1
			sy = (crop.y[1]<(sy0-1)) - (crop.y[0]>0) +1
			orgx = crop.x[0] > 0
			orgy = crop.y[0] > 0
		endif
	endif

	if (cgm or wmf or eps or png or jpeg) then begin
		F = file
		ext = extract_extension(file)
		s = options.Crop ? '-crop-' : '-'
		F = strip_file_ext(file) + s + strcompress( el_names[qplot], /remove_all) + '.'+ext
		Ftemp = extract_path(file) +ext+'.temp'
	endif

	if cgm then begin
		used_non_screen = 1
		set_device, 'CGM', white=options.white, file=F, error=error
		if error then goto, cont
		multi = 0
	endif else if wmf then begin
		used_non_screen = 1
;		set_device, 'METAFILE', white=options.white, file=Ftemp, error=error
		set_device, 'METAFILE', white=options.white, file=F, error=error
		if error then goto, cont
		multi = 0
	endif else if eps then begin
		used_non_screen = 1
		set_device, 'PS', white=options.white, /landscape, file=F, error=error	;, /small
		if error then goto, cont
		multi = 0
	endif else if png then begin
		used_non_screen = 0
		set_device, 'Z', white=options.white, aspect=(float(sy)/float(sx)), error=error
		if error then goto, cont
		multi = 0
		screen = 1
	endif else if jpeg then begin
		used_non_screen = 0
		set_device, 'Z', /true, white=options.white, aspect=(float(sy)/float(sx)), error=error
		if error then goto, cont
		multi = 0
		screen = 1
	endif else begin
		multi = 1
		if (again eq 0) and (nq lt 2) then begin
			portrait = 0
			landscape = 1
			multi = 0
		endif

		if screen eq 0 then begin
			if new_dialog_printersetup(portrait=portrait, landscape=landscape, again=again) then begin
				used_non_screen = 1
			endif
		endif

		if used_non_screen eq 0 then begin
			xsize = 700
			ysize = 600
			multi = 0

			set_device, xwin, white=options.white
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
	endelse

;	loadct, options.ColourTable, bottom=16, ncolors=100
;	load_spec_colours

	scale = (multi) ? 0.71 : 1.0
	!p.charsize = scale * options.CharSize
	sz = !p.charsize
	!p.symsize = scale * options.SymSize

	on_ioerror, cont
	default_plot, thick, athick, csize, cthick, thick_scale=scale*options.LineThick, aspect=aspect

	!p.symsize = !p.symsize * csize/sz
	cthick = cthick * options.CharThick/options.LineThick
	tick = 0.5 * scale * options.CharSize

;----------------------------------------------------------------------------

; If (multi eq 1) and (nq ge 2) then plot 2 images on this page, else plot just one.
; Loop back for nq gt 2, and set_device again (with new file name).
;
; For test plot on screen, can TV plot with an offset now?
; What about a scaling? If not, will need to congrid one.

	rd = 4.0											; bottom

	leg_width = 3.										; as a number of X chars
	leg_height = 0.90									; as a fraction of ypix
	label_size = 1.6

	adjust = 1. / sqrt((sx/sy) > 1.)					; adjust for large X/Y aspect
	csize = csize *adjust
	thick = thick *adjust
	athick = athick *adjust
	cthick = cthick *adjust

	x_ch_size = !d.x_ch_size * csize					; default character dimensions
	y_ch_size = !d.y_ch_size * csize					; in device units

	if (!d.name eq 'METAFILE') then y_ch_size=y_ch_size/1.6	; fix flaw in metadata device

	print,'csize,label_size,aspect,x_ch_size,y_ch_size=',csize,label_size,aspect,x_ch_size,y_ch_size

	if options.ZaxisLegend then begin
		rbx = 9.5 + (2.0+leg_width)						; right (normal text)
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
	if multi then dy_size = dy_size/2					; split area for 2 plots

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

	el = strtrim(el_names[qplot],2)
	top_title = ''
	case options.title.mode of
		0: begin
			top_title = strip_path( (*p).source)
			end
		1: begin
			top_title = (*p).sample 
			if (*p).grain ne '' then top_title = top_title + ' - ' + (*p).grain
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
	save_opt = 0
	tvlct, red,green,blue, /get

	print,'start options ...'
	if options.ZaxisLegend then begin		; ++++++++++++++++
		qscale = 1.0
		charge_per_pixel = (*p).charge / (sx0*sy0)
		if (*p).type eq 1 then begin
			build_image_scale, (*opt)[qplot], zlow, zhigh, root=(*pstate).display_mode,label=label
			charge_per_pixel = 1.0
			units = ''
			style = label + (((*pstate).display_mode eq 1) ? 'variance' : 'fraction')
		endif else if (*p).type eq 3 then begin
			build_image_scale, (*opt)[qplot], zlow, zhigh, root=(*pstate).display_mode,label=label
			charge_per_pixel = 1.0
			units = ''
			style = label + (((*pstate).display_mode eq 1) ? 'variance' : 'energy')
		endif else if (*p).type eq 2 then begin
			build_image_scale, (*opt)[qplot], zlow, zhigh, root=(*pstate).display_mode,label=label
			units = ''
			style = label + (((*pstate).display_mode eq 1) ? 'variance' : 'counts')
		endif else begin
			if strlowcase( strmid(el,0,4)) eq 'back' then begin
				build_image_scale, (*opt)[qplot], zlow, zhigh, root=(*pstate).display_mode, $
						scale=10000., check_wt=0, wt=wt,label=label
				units = ''
				style = label + (((*pstate).display_mode eq 1) ? 'variance' : 'intensity')
				qscale = 0.0001
			endif else begin
				build_image_scale, (*opt)[qplot], zlow, zhigh, root=(*pstate).display_mode, $
						scale=1./charge_per_pixel, check_wt=(options.ppmOnly eq 0), wt=wt,label=label
				units = ' (ppm)'
				style = label + (((*pstate).display_mode eq 1) ? 'variance' : 'conc')
				qscale = charge_per_pixel
			endelse
			if wt then begin
				units = ' (wt%)'
				qscale = qscale * 10000.
			endif
		endelse
		print,'style=',style,' units=',units,' qscale=',qscale
		print,'zlow=',zlow,'  zhigh=',zhigh
		print,' bottom=',(*opt)[qplot].bottom,'  top=',(*opt)[qplot].top

;		Check the number of colour steps. If it is small, then we need to have
;		legend steps on these boundaries, and as nice round numbers.

		steps = intarr(100)
		ns = count_steps( red[16:115], steps=q)
		steps[q] = 1
		ns = count_steps( green[16:115], steps=q)
		steps[q] = 1
		ns = count_steps( blue[16:115], steps=q)
		steps[q] = 1
		step = where(steps eq 1)
		n_steps = n_elements(step)

		if options.ConcMaxMode eq 2 then begin				; manual maximum range
			save_opt = 1
			save_bot = (*opt)[qplot].bottom
			save_top = (*opt)[qplot].top

			case (*opt)[qplot].log of
				0: begin
					(*opt)[qplot].top = options.ManualMax * 100. * qscale / ((*pstate).display_mode ? sqrt((*opt)[qplot].max) : (*opt)[qplot].max)
					zhigh = options.ManualMax
					end
				1: begin
					m = 5./100.
					c = alog10(((*pstate).display_mode ? sqrt((*opt)[qplot].max) : (*opt)[qplot].max)/qscale) - 5.
					(*opt)[qplot].top = (alog10(options.ManualMax)-c)/m
					zhigh = alog10(options.ManualMax)
					end
				2: begin
					m = sqrt(((*pstate).display_mode ? sqrt((*opt)[qplot].max) : (*opt)[qplot].max)/qscale) / 100.
					c = 0.
					(*opt)[qplot].top = (sqrt(options.ManualMax)-c)/m
					zhigh = sqrt(options.ManualMax)
					end
			endcase
			if n_steps le 16 then zinc = zhigh / float(n_steps)

		endif else if n_steps le 16 then begin
			save_opt = 1
			save_bot = (*opt)[qplot].bottom
			save_top = (*opt)[qplot].top

			zinc = one_sig_figure( (zhigh-zlow)/n_steps, nice=(nice and (n_steps eq 10)) )
			zlow = round(zlow / zinc) * zinc
			z = zlow + indgen(n_steps+2)*zinc
			zhigh = z[n_steps]
			if h_aspect lt 0.6 then zinc = 2.*zinc
			case (*opt)[qplot].log of
				0: begin
					(*opt)[qplot].bottom = zlow * qscale * 100. / ((*pstate).display_mode ? sqrt((*opt)[qplot].max) : (*opt)[qplot].max)
					(*opt)[qplot].top = zhigh * qscale * 100. / ((*pstate).display_mode ? sqrt((*opt)[qplot].max) : (*opt)[qplot].max)
					end
				1: begin
					m = 5./100.
					c = alog10(((*pstate).display_mode ? sqrt((*opt)[qplot].max) : (*opt)[qplot].max)/qscale) - 5.
					(*opt)[qplot].bottom = (zlow-c)/m
					(*opt)[qplot].top = (zhigh-c)/m
					end
				2: begin
					m = sqrt(((*pstate).display_mode ? sqrt((*opt)[qplot].max) : (*opt)[qplot].max)/qscale) / 100.
					c = 0.
					(*opt)[qplot].bottom = (zlow-c)/m
					(*opt)[qplot].top = (zhigh-c)/m
					end
			endcase
		endif
		print,'zlow=',zlow,'  zhigh=',zhigh
		print,' bottom=',(*opt)[qplot].bottom,'  top=',(*opt)[qplot].top

		!y.title = ''
		if options.ppmOnly eq 0 then !y.title = el + ' ' + style + units
		!x.title = ''
		!p.title = ''

		if used_non_screen then begin
			tv, bar, xloff,yloff, xsize=xleg,ysize=yleg, /device
		endif else begin
			b2 = smart_congrid(bar,xleg+0.5,yleg+0.5)
			tv, b2, xloff+0.5,yloff+0.5,  /device
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
				case (*opt)[qplot].log of
					0: begin
						zs = z
						ez = zinc
						end
					1: begin
						zs = 10.^z
						ez = 10.^(z+zinc) - zs
						end
					2: begin
						zs = z^2
						ez = (z+zinc)^2 - zs
						end
				endcase
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

	endif			; ++++++ end Z legend  ++++++++++++++

;	sx				cropped pixel X size (sx0 original pixel size)
;	scan.origin.x	scan X origin (mm)
;	scan.x			scan X size (mm)

;	All these use the /nozoom option, as we don't target display buffers.
;	Use xtgt, ytgt to force rescaling to these dimensions, only for display 0 window.
;	For non-screen devices, using scaling on the TV procedure.

	centroids=0
	if options.Centroids.On then centroids = options.centroids

	if options.Crop then begin
		if used_non_screen then begin
			b = make_tvb( pstate, qplot, /nozoom, xmin=crop.x[0],xmax=crop.x[1],ymin=crop.y[0],ymax=crop.y[1], $
								compress=compress, centroids=centroids)
		endif else begin
			b = make_tvb( pstate, qplot, /nozoom, xmin=crop.x[0],xmax=crop.x[1],ymin=crop.y[0],ymax=crop.y[1], $
								xtgt=xpix, ytgt=ypix, compress=compress, centroids=centroids)
		endelse
		print,'Make_TVB Compress = ',compress
	endif else begin
		if used_non_screen then begin
			b = make_tvb( pstate, qplot, /nozoom, centroids=centroids)
		endif else begin
			b = make_tvb( pstate, qplot, /nozoom, xtgt=xpix, ytgt=ypix, compress=compress, centroids=centroids)
			print,'Make_TVB Compress = ',compress
		endelse
	endelse

	if used_non_screen then begin
		tv, b, xoff,yoff, xsize=xpix,ysize=ypix, /device
	endif else begin
		tv, b, xoff,yoff,  /device
	endelse

	if options.Crop then begin
		r = image_absolute( p, crop=crop, absolute=absolute, error=err)
	endif else begin
		if options.ShowALLregions then begin
			nreg = n_elements( *(*pstate).pregions)
			if nreg ge 1 then begin
				copy_pointer_data, (*pstate).pmark[0], pmt, /init 
				ttype = (*pstate).analyze_type[0]

				pm = (*pstate).pmark[0] 
				for i=0,nreg-1 do begin
					pr = (*(*(*pstate).pregions)[i]).pmark[0]
					(*pstate).analyze_type[0] = (*(*(*pstate).pregions)[i]).analyze_type[0]
					t = *(*pm)[(*pstate).analyze_type[0]]
					struct_assign, *pr, t
					*(*pm)[(*pstate).analyze_type[0]] = t

					plot_mark, pstate, /wide, xoff=xoff,yoff=yoff, $
						xscale=float(xpix)/zoom(pstate,sx0), yscale=float(ypix)/zoom(pstate,sy0)
				endfor
				copy_pointer_data, pmt, (*pstate).pmark[0] 
				(*pstate).analyze_type[0] = ttype
			endif
		endif else if options.ShowShape then begin
			plot_mark, pstate, /wide, xoff=xoff,yoff=yoff, $
				xscale=float(xpix)/zoom(pstate,sx0), yscale=float(ypix)/zoom(pstate,sy0)
		endif

		r = image_absolute( p, absolute=absolute, error=err)
	endelse
;	pointer_display,r
	if err ne 0 then goto, cont

	xm0 = r.absolute.org.x
	ym0 = r.absolute.org.y
	xm = r.absolute.size.x
	ym = r.absolute.size.y
;	pox = r.pixel.org.x
;	poy = r.pixel.org.y
;	px = r.pixel.size.x
;	py = r.pixel.size.y
;	uox = r.uncompressed.org.x
;	uoy = r.uncompressed.org.y
;	usx = r.uncompressed.size.x
;	usy = r.uncompressed.size.y

	image_axes_units, p, xunits=xunits, yunits=yunits
	if (xm lt 0.001) or (ym lt 0.001) then begin
		x_units = 'pixels'
		y_units = 'pixels'
		xm = sx
		ym = sy
	endif else begin
		x_units = xunits
		y_units = yunits
		if xunits eq 'keV' then begin
			x_units = 'E steps'
			xm = sx
		endif else begin
			if (xm lt 0.3) and (absolute eq 0) then begin
				xm = xm * 1000.
				case x_units of
					'mm': x_units = 'microns'
					else: x_units = 'microns'
				endcase
			endif
		endelse
		if yunits eq 'keV' then begin
			y_units = 'E steps'
			ym = sy
		endif else begin
			if (ym lt 0.3) and (absolute eq 0) then begin
				ym = ym * 1000.
				case y_units of
					'mm': y_units = 'microns'
					else: y_units = 'microns'
				endcase
			endif
		endelse
	endelse
	
	if options.LabelAxes then begin
		!x.title = 'X (' + x_units + ')'
		!y.title = 'Y (' + y_units + ')'
		!x.style = 1
		!y.style = 1
	endif else begin
		!x.title = ''
		!y.title = ''
		!x.style = 13
		!y.style = 13
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

	plot,[0,0],[0,0], xrange=r.range.x,yrange=r.range.y, /nodata, position=w, $
			xlog=0,ylog=0, xstyle=xstyle,ystyle=ystyle, xthick=athick,ythick=athick, $
			charthick=cthick, thick=thick, charsize=csize, $
			xticklen=-0.015*tick, yticklen=-0.015*tick*h_aspect, /noerase, $
			xtickinterval=xti, ytickinterval=yti
	plots,[nxoff,nxoff+nxpix,nxoff+nxpix,nxoff,nxoff], $
			[nyoff,nyoff,nyoff+nypix,nyoff+nypix,nyoff],/norm, thick=athick

	if options.title.on and (strlen(top_title) ge 1) then begin
		xyouts, xoff+xpix/2, yoff+ypix+0.8*y_ch_size, /device, top_title, align=0.5, $
				charsize=1.2*csize, charthick=1.1*cthick
	endif

	if options.DistLegend and ((x_units ne 'pixels') or (y_units ne 'pixels')) then begin
		if x_units eq 'mm' then xm = xm*1000.
		length = [0.1,1.,10.,100.,1000.,10000.]
		q = reverse( where( length lt xm))
		xs = length[q[0]]
		ls = xpix * xs / float(xm)
		c = !p.color

		case options.DistPos of
			0: begin														; bottom
				xp = xoff + 0.03*xpix
				yp = yoff + 0.03*ypix*(1./h_aspect > 1.)
				xl = xp + ls + 0.02*xpix
				yl = yp
				xv = [xp,xp+ls,xp+ls,xp]
				yv = [yp,yp,yp+0.02*ypix*(1./h_aspect > 1.),yp+0.02*ypix*(1./h_aspect > 1.)]
				if options.DistColour eq 1 then c=!p.background
				end
			1: begin														; top
				xp = xoff + 0.03*xpix
				yp = yoff + ypix - 0.03*ypix*(1./h_aspect > 1.)
				xl = xp + ls + 0.02*xpix
				yl = yp - y_ch_size*0.8
				xv = [xp,xp+ls,xp+ls,xp]
				yv = [yp,yp,yp-0.02*ypix*(1./h_aspect > 1.),yp-0.02*ypix*(1./h_aspect > 1.)]
				if options.DistColour eq 1 then c=!p.background
				end
			2: begin														; outside
				xp = xoff + 0.03*xpix
				yp = yoff - 0.03*ypix*(1./h_aspect > 1.)
				xl = xp + ls + 0.02*xpix
				yl = yp - y_ch_size*0.8
				xv = [xp,xp+ls,xp+ls,xp]
				yv = [yp,yp,yp-0.02*ypix*(1./h_aspect > 1.),yp-0.02*ypix*(1./h_aspect > 1.)]
				end
		endcase
		polyfill, xv,yv, /device, color=c
		xyouts, xl,yl, str_tidy(round(xs)), /device, $
				charthick=cthick, charsize=0.9*csize, color=c
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
			xl = xoff + xpix - offset*x_ch_size - n*x_ch_size*label_size	; bottom right
			yl = yoff + offset*y_ch_size
			end
	endcase
	xv = xl - 0.2*label_size*x_ch_size + [0,n*x_ch_size,n*x_ch_size,0]*1.2*label_size
	yv = yl - 0.2*label_size*y_ch_size + [0,0,y_ch_size,y_ch_size]*1.2*label_size
	if blank_on then polyfill, xv,yv, /device, color=!p.background
	xyouts, xl,yl, el, /device, charsize=label_size*csize, charthick=label_size*cthick, color=c

	select[qplot] = 0

	if save_opt then begin
		(*opt)[qplot].bottom = save_bot
		(*opt)[qplot].top = save_top
	endif

	if multi then begin								; plot the second one ...
		multi = 0
		yoff = yoff + dy_size
		yloff = yloff + dy_size
		qplot = qselect[1]
		goto, back
	endif

	if used_non_screen then begin
		again = 1
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

	if used_non_screen or png or jpeg then begin
		iselect++
		if iselect lt nq then goto, more					; more pages
	endif

;-------------------------------------------------------------------------------------------

cont:
	on_ioerror, cont
	if used_non_screen or png or jpeg then set_plot, xwin
done:
	!p.charsize = 1.0
	!p.title = ''
	!x.title = ''
	!y.title = ''
	!p.thick = 1.0
	!p.charthick = 1.0
	return
end

