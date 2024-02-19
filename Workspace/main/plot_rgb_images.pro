pro plot_RGB_images, pstate, cgm=cgm, wmf=wmf, eps=eps, png=png, jpeg=jpeg, screen=screen, file=file, $
				options=options, crop=crop

; Plot RGB images in boxes
;
; For /CGM and /WMF, output a single RGB image per file (append 3 element names to file name).
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
;					file:	'' }}	"Learn" filename

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
			warning,'plot_RGB_images',['IDL run-time error caught.', '', $
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
	pstate2 = (*pstate).pstate_parent
	xanes_stack_test, p, xanes, n_el, el_names, el_xanes
	palt = (*pstate).palt
	palt2 = (*pstate).palt2
	good_alt = ptr_good(palt)
	good_alt2 = ptr_good(palt2)
	n_el2 = 0
	n_el3 = 0
	if good_alt then xanes_stack_test, palt, xanes2, n_el2, el2, el_xanes2
	if good_alt2 then xanes_stack_test, palt2, xanes3, n_el3, el3, el_xanes3

	if n_elements(cgm) lt 1 then cgm = 0
	if n_elements(wmf) lt 1 then wmf = 0
	if n_elements(eps) lt 1 then eps = 0
	if n_elements(png) lt 1 then png = 0
	if n_elements(jpeg) lt 1 then jpeg = 0
	if n_elements(screen) lt 1 then screen = 0
	if n_elements(full_area) lt 1 then full_area = 0
	if n_elements(file) lt 1 then begin
		if cgm then file = 'Images RGB.cgm'
		if wmf then file = 'Images RGB.wmf'
		if eps then file = 'Images RGB.eps'
		if png then file = 'Images RGB.png'
		if jpeg then file = 'Images RGB.jpg'
	endif
	if n_elements(options) lt 1 then options = (*(*pstate).pexport).plot

	used_non_screen = 0
	portrait = 0						; portrait plot
	landscape = 1
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
	iLearn = 0
	old_image = (*pstate).image						; save current RGB indices, 
	old_image2 = (*pstate).image2					; as they get changed for Learn mode
	old_image3 = (*pstate).image3					; and are passed on via *pstate

more:
	if ptr_valid( (*p).options ) eq 0 then fix_options, p
	pimg = (*p).image
	opt = (*p).options
	sx = (*p).xsize
	sy = (*p).ysize
	if ptr_valid(pimg) eq 0 then goto, cont
	sizes = [n_el, n_el+n_el2, n_el+n_el2+n_el3]
	el = strarr(3)

	if options.learn.on then begin
		if ptr_good( (*pstate).plearn) eq 0 then return
		q = where( (*(*pstate).plearn)[iLearn].r eq el_names, nq)
		if nq eq 0 then goto, learn_skip
		i = q[0]
		image1 = i
	endif else begin
		i = (*pstate).image									; Red
		i <= (n_el + n_el2 + n_el3-1)
		if good_alt2 eq 0 then i <= (n_el + n_el2-1)
		if good_alt eq 0 then i <= (n_el-1)
		image1 = i
	endelse

	q = where( i ge sizes, nset)
	case nset of
		1: begin
			p = (*pstate).palt
			i = i - n_el
			el[0] = el2[i]
			opt1 = (*palt).options
			end
		2: begin
			p = (*pstate).palt2
			i = i - (n_el + n_el2)
			el[0] = el3[i]
			opt1 = (*palt2).options
			end
		else: begin
			el[0] = el_names[i]
			opt1 = opt
			end
	endcase

	if options.learn.on then begin
		q = where( (*(*pstate).plearn)[iLearn].g eq el_names, nq)
		if nq eq 0 then goto, learn_skip
		i = q[0]
		image2 = i
	endif else begin
		i = (*pstate).image2								; Green
		i <= (n_el + n_el2 + n_el3-1)
		if good_alt2 eq 0 then i <= (n_el + n_el2-1)
		if good_alt eq 0 then i <= (n_el-1)
		image2 = i
	endelse

	q = where( i ge sizes, nset)
	case nset of
		1: begin
			p = (*pstate).palt
			i = i - n_el
			el[1] = el2[i]
			opt2 = (*palt).options
			end
		2: begin
			p = (*pstate).palt2
			i = i - (n_el + n_el2)
			el[1] = el3[i]
			opt2 = (*palt2).options
			end
		else: begin
			el[1] = el_names[i]
			opt2 = opt
			end
	endcase

	if options.learn.on then begin
		q = where( (*(*pstate).plearn)[iLearn].b eq el_names, nq)
		if nq eq 0 then goto, learn_skip
		i = q[0]
		image3 = i
	endif else begin
		i = (*pstate).image3								; Blue
		i <= (n_el + n_el2 + n_el3-1)
		if good_alt2 eq 0 then i <= (n_el + n_el2-1)
		if good_alt eq 0 then i <= (n_el-1)
		image3 = i
	endelse

	q = where( i ge sizes, nset)
	case nset of
		1: begin
			p = (*pstate).palt
			i = i - n_el
			el[2] = el2[i]
			opt3 = (*palt).options
			end
		2: begin
			p = (*pstate).palt2
			i = i - (n_el + n_el2)
			el[2] = el3[i]
			opt3 = (*palt2).options
			end
		else: begin
			el[2] = el_names[i]
			opt3 = opt
			end
	endcase
	(*pstate).image = image1				; update RGB image indices in *pstate,
	(*pstate).image2 = image2				; as they get used in 'make_RGB_true' and 'make_RGB_tvb'
	(*pstate).image3 = image3

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

;	Need to use (*pstate).image, .image2, .image3 for 3 elements ...

	if (cgm or wmf or eps or png or jpeg) then begin
		F = file
		ext = extract_extension(file)
		s = options.Crop ? '-crop-' : '-'
		F = strip_file_ext(file) + s + strjoin(strcompress(el,/remove_all),'-') + '.'+ext
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
;		set_device, 'Z', /true, white=options.white, aspect=float(!d.y_size)/float(!d.x_size), error=error
		set_device, 'Z', /true, white=options.white, aspect=(float(sy)/float(sx)), error=error
		if error then goto, cont
		multi = 0
		screen = 1
	endif else if jpeg then begin
		used_non_screen = 0
;		set_device, 'Z', /true, white=options.white, aspect=float(!d.y_size)/float(!d.x_size), error=error
		set_device, 'Z', /true, white=options.white, aspect=(float(sy)/float(sx)), error=error
		if error then goto, cont
		multi = 0
		screen = 1
	endif else begin
;		multi = 1
;		if (again eq 0) and (nq lt 2) then begin
			portrait = 0
			landscape = 1
			multi = 0
;		endif

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

	full_area = options.max_area						; cover maximum area (aspect preserved)
														; only kicks in as all trimmings get disabled

	if full_area then begin
		rd = 0.0
	endif else begin
		rd = 4.0										; bottom
	endelse

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
;		rbx = 9.5 + (2.0+leg_width)						; right (normal text)
		rbx = 16.										; right (normal text)
		rby = 1.0										; right (rotated text)
	endif else if options.LabelPos eq 0 then begin
		rbx = 8.0										; no Z legend, but
		rby = 0.0										; label element outside
	endif else if full_area then begin
		rbx = 0.0										; no Z legend and no label outside
		rby = 0.0										; now cover maximum area
	endif else begin
		rbx = 2.0										; no Z legend and no label outside
		rby = 0.0										;
	endelse

	if options.LabelAxes then begin
		rax = 9.5		; left (normal text)			; number of characters in border
		ray = 1.0		; left (rotated text)
	endif else if full_area then begin
		rax = 0.0		; left (normal text)
		ray = 0.0		; left (rotated text)
	endif else begin
		rax = 1.0		; left (normal text)
		ray = 0.0		; left (rotated text)
	endelse

	if options.title.on then begin
		rc = 3.0										; top (with title)
	endif else if full_area then begin
		rc = 0.0										; top (no title)
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
			b = make_RGB_true( pstate, /nozoom, xmin=crop.x[0],xmax=crop.x[1], ymin=crop.y[0],ymax=crop.y[1], $
								enhance=options.enhance, centroids=centroids)
		endif else begin
			b = make_RGB_true( pstate, /nozoom, xmin=crop.x[0],xmax=crop.x[1],ymin=crop.y[0],ymax=crop.y[1], $
								xtgt=xpix, ytgt=ypix, enhance=options.enhance, centroids=centroids)
		endelse
	endif else begin
		if used_non_screen then begin
			b = make_RGB_true( pstate, /nozoom, enhance=options.enhance, centroids=centroids)
		endif else begin
			b = make_RGB_true( pstate, /nozoom, xtgt=xpix, ytgt=ypix, enhance=options.enhance, centroids=centroids)
		endelse
	endelse

	if used_non_screen then begin
		tv, b, xoff,yoff, xsize=xpix,ysize=ypix, true=1, /device
	endif else begin
		device, decomposed=1
		tv, b, xoff,yoff, true=1,  /device
		device, decomposed=0
	endelse

	if options.ShowShape and (options.crop eq 0) then begin
		pstate2 = (*pstate).pstate_parent				; image window pstate
		zt = (*pstate2).zoom
		(*pstate2).zoom = 0
		plot_mark, pstate2, /wide, xoff=xoff,yoff=yoff, $
					xscale=float(xpix)/sx0,yscale=float(ypix)/sy0
		(*pstate2).zoom = zt
	endif

	if options.Crop then begin
		r = image_absolute( p, crop=crop, absolute=absolute, error=err)
	endif else begin
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

	n = total(strlen(el)) + 2
	offsetx = 0.5*label_size
	offsety = 0.3*label_size
	blank_on = 1
	c = [spec_colour('red'),spec_colour('green'),spec_colour('d.blue')]
;	c = [spec_colour('red'),spec_colour('green'),options.White ? spec_colour('blue') : spec_colour('m.blue')]
	if options.LabelColour eq 1 then blank_on = 0
	case options.LabelPos of
		0: begin
			xl = xloff														; outside
			yl = yoff
			blank_on = 0
			xlinc = 0.0
			ylinc = 1.2
			align = 0
			sign = +1.0
			end
		1: begin
			xl = xoff + offsetx*x_ch_size									; top left
			yl = yoff + ypix - 0.5*offsety*y_ch_size - y_ch_size*label_size
			xlinc = 1.0
			ylinc = 0.0
			align = 0
			sign = +1.0
			end
		2: begin
			xl = xoff + offsetx*x_ch_size									; bottom left
			yl = yoff + offsety*y_ch_size
			xlinc = 1.0
			ylinc = 0.0
			align = 0
			sign = +1.0
			end
		3: begin
			xl = xoff + xpix - offsetx*x_ch_size								; top right
			yl = yoff + ypix - 0.5*offsety*y_ch_size - y_ch_size*label_size
			xlinc = -1.0
			ylinc = 0.0
			align = 1
			sign = -1.0
			end
		4: begin
			xl = xoff + xpix - offsetx*x_ch_size								; bottom right
			yl = yoff + offsety*y_ch_size
			xlinc = -1.0
			ylinc = 0.0
			align = 1
			sign = -1.0
			end
	endcase
	nel = reverse(strlen(el))
	dxl = fltarr(4,3)
	for i=0,2 do begin
		dxl[*,i] = (sign)*(-0.2*label_size*x_ch_size + [0,nel[i]*x_ch_size,nel[i]*x_ch_size,0]*1.1*label_size)
	endfor
	dyl = -0.2*label_size*y_ch_size + [0,0,y_ch_size,y_ch_size]*1.1*label_size
	xlinc = xlinc * nel * 1.1*label_size*x_ch_size
	ylinc = ylinc * label_size*y_ch_size

	for i=0,2 do begin
		if blank_on then polyfill, xl+dxl[*,i],yl+dyl, /device, color=!p.background
		xyouts, xl,yl, el[2-i], /device, charsize=label_size*csize, charthick=label_size*cthick, color=c[2-i], align=align
		xl = xl + xlinc[i]
		yl = yl + ylinc
	endfor

	nxleg = float(fix(xleg)) / !d.x_size				; legend area in Normal coords
	nyleg = float(fix(yleg)) / !d.y_size
	nxloff = float(fix(xloff)) / !d.x_size				; offset in Normal coords
	nyloff = float(fix(yloff)) / !d.y_size
;	wl = [nxloff,nyloff,nxloff+nxleg,nyloff+nyleg]		; position plot window

;	bar = bytarr(xleg,100)
;	for i=0L,99 do bar[*,i] = byte(16 + i)				; colours
;	save_opt = 0
	tvlct, red,green,blue, /get

	print,'start options ...'
	if options.ZaxisLegend then begin		; ++++++++++++++++
		charge_per_pixel = (*p).charge / (sx0*sy0)

		set_RGB_display_range, options, el[0], (*opt1)[image1], (*p).type, (*pstate2).display_mode, charge_per_pixel, $
					zlow=zlow0, zhigh=zhigh0, style=style0, qscale=qscale0, units=units0

		set_RGB_display_range, options, el[1], (*opt2)[image2], (*p).type, (*pstate2).display_mode, charge_per_pixel, $
					zlow=zlow1, zhigh=zhigh1, style=style1, qscale=qscale1, units=units1

		set_RGB_display_range, options, el[2], (*opt3)[image3], (*p).type, (*pstate2).display_mode, charge_per_pixel, $
					zlow=zlow2, zhigh=zhigh2, style=style2, qscale=qscale2, units=units2

		print, el[0], ' ', str_tidy(zlow0, places=-2,length=4), ' ', str_tidy(zhigh0, places=-2,length=4),' ', style0, units0
		print, el[1], ' ', str_tidy(zlow1, places=-2,length=4), ' ', str_tidy(zhigh1, places=-2,length=4), ' ',style1, units1
		print, el[2], ' ', str_tidy(zlow2, places=-2,length=4), ' ', str_tidy(zhigh2, places=-2,length=4), ' ',style2, units2

		xs = 0.8
		label_size2 = xs*label_size*0.6
		cs = csize*xs
		dy = 1.3*y_ch_size*label_size2
		dx = x_ch_size*label_size2

		xl = xloff + 4*dx
		yl = yloff + yleg - y_ch_size
		xyouts, xl,yl, 'Min', /device, charsize=cs, charthick=cthick, align=0
		xl = xl + 6*dx
		xyouts, xl,yl, 'Max', /device, charsize=cs, charthick=cthick, align=0

		xl = xloff 
		yl = yl - dy
		xyouts, xl,yl, str_tidy(el[0],length=3), /device, color=spec_colour('red'), charsize=cs, charthick=cthick, align=0
		xl = xl + 4*dx
		xyouts, xl,yl, str_tidy(zlow0, places=-2,length=4), /device, charsize=cs, charthick=cthick, align=0
		xl = xl + 6*dx
		xyouts, xl,yl, str_tidy(zhigh0, places=-2,length=4), /device, charsize=cs, charthick=cthick, align=0

		xl = xloff 
		yl = yl - dy
		xyouts, xl,yl, str_tidy(el[1],length=3), /device, color=spec_colour('green'), charsize=cs, charthick=cthick, align=0
		xl = xl + 4*dx
		xyouts, xl,yl, str_tidy(zlow1, places=-2,length=4), /device, charsize=cs, charthick=cthick, align=0
		xl = xl + 6*dx
		xyouts, xl,yl, str_tidy(zhigh1, places=-2,length=4), /device, charsize=cs, charthick=cthick, align=0

		xl = xloff 
		yl = yl - dy
		xyouts, xl,yl, str_tidy(el[2],length=3), /device, color=spec_colour('d.blue'), charsize=cs, charthick=cthick, align=0
		xl = xl + 4*dx
		xyouts, xl,yl, str_tidy(zlow2, places=-2,length=4), /device, charsize=cs, charthick=cthick, align=0
		xl = xl + 6*dx
		xyouts, xl,yl, str_tidy(zhigh2, places=-2,length=4), /device, charsize=cs, charthick=cthick, align=0

		xl = xloff 
		yl = yl - 2.*dy
		xyouts, xl,yl, style0+units0, /device, color=spec_colour('red'), charsize=cs, charthick=cthick, align=0
		yl = yl - dy
		xyouts, xl,yl, style1+units1, /device, color=spec_colour('green'), charsize=cs, charthick=cthick, align=0
		yl = yl - dy
		xyouts, xl,yl, style2+units2, /device, color=spec_colour('d.blue'), charsize=cs, charthick=cthick, align=0


	endif			; ++++++ end Z legend  ++++++++++++++

;	if save_opt then begin
;		(*opt)[qplot].bottom = save_bot
;		(*opt)[qplot].top = save_top
;	endif

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
;		if nq gt 1 then goto, more					; more pages
	endif

	if png then begin
		device, decomposed=1
		c = tvrd( true=1)
		write_png, F, c
		device, decomposed=0
	endif else if jpeg then begin
		device, decomposed=1
		c = tvrd( true=1)
		write_jpeg, F, c, true=1, quality=100
		device, decomposed=0
	endif

;-------------------------------------------------------------------------------------------

cont:
	on_ioerror, cont
	if used_non_screen or (png or jpeg) then set_plot, xwin

learn_skip:
	if options.learn.on and (iLearn lt n_elements( *(*pstate).plearn)-1) then begin
		iLearn++
		goto, more
	endif
	(*pstate).image = old_image					; restore current RGB indices
	(*pstate).image2 = old_image2				; in case they were changed for Learn mode
	(*pstate).image3 = old_image3

done:
	!p.charsize = 1.0
	!p.title = ''
	!x.title = ''
	!y.title = ''
	!p.thick = 1.0
	!p.charthick = 1.0
	return
end

