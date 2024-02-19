pro plot_spectra, pstate, cgm=cgm, wmf=wmf, eps=eps, screen=screen, $
				select=selecti, options=options, file=file, png=png, jpeg=jpeg

; Plot spectra or line profiles in boxes
; 'profile' selected on basis of error bars for now (see below).

; The plot 'options' have the form:
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
;			enhance: { $
;					spots: { on: 0	enhance hot-spots
;					elements: ''}}	select enhance element names (1 to 3, sep by spaces)
;			max_area: 0				use a maximum display area, strip borders
;			separate:		0 		plot separate spectra, or common axes
;			landscape:		0 		Landscape orientation

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
			warning,'Plot_spectra',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			goto, cont
		endif
	endif

	if ptr_valid((*pstate).p) eq 0 then return
	if ptr_valid( (*(*pstate).p)[0] ) eq 0 then return

	if n_elements(cgm) lt 1 then cgm = 0
	if n_elements(wmf) lt 1 then wmf = 0
	if n_elements(eps) lt 1 then eps = 0
	if n_elements(png) lt 1 then png = 0
	if n_elements(jpeg) lt 1 then jpeg = 0
	if n_elements(screen) lt 1 then screen = 0
	if n_elements(selecti) lt 1 then begin
		select = intarr( n_elements((*(*pstate).p)[*]))
		j = current_plot( pstate)
		select[j] = 1
	endif else begin
		select = selecti
	endelse
	if n_elements(file) lt 1 then begin
		if cgm then file = 'Spectra.cgm'
		if wmf then file = 'Spectra.wmf'
		if eps then file = 'Spectra.eps'
		if png then file = 'Images.png'
		if jpeg then file = 'Images.jpg'
	endif
	if n_elements(options) lt 1 then options = (*(*pstate).pexport).plot

	used_non_screen = 0
	landscape = options.landscape
	portrait = 1-landscape						; portrait plot
	separate = options.separate
	xwin = !d.name

more:
	np = n_elements(*(*pstate).p)
	qselect = where(select eq 1)
	if qselect[0] eq -1 then goto, cont
	nq = n_elements(qselect)
	p = (*(*pstate).p)[ qselect < (np-1)]
	printer = 0

	if cgm then begin
		used_non_screen = 1
		set_device, 'CGM', white=options.white, file=file, error=error
		if error then goto, cont
	endif else if wmf then begin
		used_non_screen = 1
		set_device, 'METAFILE', white=options.white, file=file, error=error
		if error then goto, cont
	endif else if eps then begin
		used_non_screen = 1
		set_device, 'PS', white=options.white, /landscape, /small, file=file, error=error
		if error then goto, cont
	endif else if png then begin
		used_non_screen = 0
		set_device, 'Z', white=options.white, portrait=portrait, landscape=landscape, error=error
		if error then goto, cont
		multi = 0
		screen = 1
	endif else if jpeg then begin
		used_non_screen = 0
		set_device, 'Z', /true, white=options.white, portrait=portrait, landscape=landscape, error=error
		if error then goto, cont
		multi = 0
		screen = 1
	endif else begin
		if screen eq 0 then begin
			if new_dialog_printersetup(portrait=portrait, landscape=landscape) then begin
				used_non_screen = 1
				printer = 1
			endif
		endif

		if used_non_screen eq 0 then begin
			if landscape then begin
				if nq eq 1 then begin
					xsize = 700
					ysize = 450
				endif else begin
					xsize = 700
					ysize = 600
				endelse
			endif else begin
				if nq eq 1 then begin
					xsize = 700
					ysize = 600
				endif else begin
					xsize = 500
					ysize = 800
				endelse
			endelse

			set_device, xwin, white=options.white
			used_non_screen = 0
			if !d.name eq xwin then begin
				window,0, xsize=xsize, ysize=ysize
			endif
		endif
	endelse

	scale = 1.0
	if printer then begin
		scale = (landscape) ? ((nq ge 2) ? 1.0 : 1.2) : ((nq ge 2) ? 1.4 : 1.0)
	endif else if screen then begin
		scale = (landscape) ? 0.9 : ((nq ge 2) ? 1.7 : 1.0)
	endif

	!p.charsize = scale * options.CharSize
	sz = !p.charsize
	!p.symsize = scale * options.SymSize

	on_ioerror, cont
	default_plot, thick, athick, csize, cthick, thick_scale=options.LineThick, aspect=aspect

	!p.symsize = !p.symsize * csize/sz
	cthick = cthick * options.CharThick/options.LineThick
	tick = 0.5 * scale * options.CharSize

	x_ch_size = !d.x_ch_size * csize				; default character dimensions
	y_ch_size = !d.y_ch_size * csize				; in device units
	ny_ch_size = y_ch_size/!d.y_size

;----------------------------------------------------------------------------

	profile=0
	if (*p[0]).has_errors then profile=1			; !!!!!!

	wlo = 0.19
	whi = 0.96
	base = 0.10
	top = 0.92
	if cgm then begin
		if landscape then begin
			if nq eq 1 then begin
				base = 0.30
				top = 0.82
			endif
		endif else begin
			if nq ge 2 then begin
				wlo = 0.4
				whi = 0.85
			endif
		endelse
	endif
	if wmf then begin
		if landscape then begin
			if nq eq 1 then begin
				base = 0.16
				top = 0.85
			endif
		endif else begin
			base = 0.10
			top = 0.93
			wlo = 0.37
			whi = 0.68
		endelse
	endif

	h = (top-base)/nq
	h2 = h
	if separate then h2 = (h2-3.5*ny_ch_size) > 0.03
	csize2 = 0.9 * csize
	xlen = 0.015
	ylen = 0.013 * nq
	if landscape then begin
		xlen = xlen * 0.7
		ylen = ylen * 1.4
	endif
	lid = top

	erase
	polyfill,[wlo,whi,whi,wlo,wlo],[base,base,top,top,base],/norm,color=!p.background

	!p.title = ''
	if options.title.on then begin
		case options.title.mode of
			0: begin
				!p.title = strip_path( (*p[0]).source)
				end
			1: begin
				!p.title = (*p[0]).sample
				if (*p[0]).grain ne '' then !p.title = !p.title + ' - ' + (*p[0]).grain
				end
			2: begin
				!p.title = options.title.text
				end
		endcase
	endif

	if separate then begin
		noxnames = 0
		noxlabel = 0
	endif else begin
		noxnames = 1
		noxlabel = 1
	endelse
	noylabel = 0
	if nq gt 3 then noylabel = 1
	lasty = noylabel
	for i=0L,nq-1 do begin
;		noxlabel = 0
		if i eq nq-1 then begin
			noxnames = 0
			noxlabel = 0
		endif

		if i eq nq/2 then begin
			noylabel = 0
		endif else begin
			noylabel = lasty
		endelse
;		ymin = (*pstate).ylow
;		yhigh = (*pstate).yhigh
		ymin = (*p[i]).ylow					;*** y scale
		yhigh = (*p[i]).yhigh
		if profile then begin
			if strlowcase((*p[i]).cal.units) eq 'channel' then begin
				xtitle = 'Channel'
				ytitle = 'Concentration (ppm)'
			endif else begin
				xtitle = 'Distance (' + (*p[i]).cal.units + ')'
				ytitle = 'Concentration (ppm)'
			endelse
			ylog = (*p[i]).log
;			ylog = 0
;			ymin = 0.0
		endif else begin
			if strlowcase((*p[i]).cal.units) eq 'channel' then begin
				xtitle = 'Channel'
				ytitle = 'Counts per channel'
			endif else begin
				xtitle = 'Energy (' + (*p[i]).cal.units + ')'
				ytitle = 'Counts per channel'
			endelse
			ylog = (*p[i]).log
		endelse
		if separate then begin
			a = (*p[i]).cal.poly[1]
			b = (*p[i]).cal.poly[0]
			elow = (*p[i]).elow * (float((*pstate).view)/float((*pstate).width))
			ehigh = (*p[i]).ehigh * (float((*pstate).view)/float((*pstate).width))
		endif else begin
			elow = (*pstate).vlow * (*pstate).a + (*pstate).b
			ehigh = (*pstate).vhigh * (*pstate).a + (*pstate).b
		endelse

		if separate then begin
			plot_spec, p[i], elow,ehigh, bot=ymin, top=yhigh, window=[wlo,lid-h2,whi,lid], $
								noxnames=noxnames, noxlabel=noxlabel,noylabel=noylabel, $
								/noerase, xticklen=xlen, yticklen=ylen, bw=bw, $
								xlabel=xtitle, ylabel=ytitle, ylog=ylog, charsize=csize, charthick=cthick, $
								thick=thick, axisthick=athick
		endif else begin
			plot_spec, p[i], elow,ehigh, bot=ymin, top=yhigh, window=[wlo,lid-h,whi,lid], $
								noxnames=noxnames, noxlabel=noxlabel,noylabel=noylabel, $
								/noerase, xticklen=xlen, yticklen=ylen, bw=bw, $
								xlabel=xtitle, ylabel=ytitle, ylog=ylog, charsize=csize, charthick=cthick, $
								thick=thick, axisthick=athick
		endelse

		label = shorten_label((*p[i]).label, length=25)
		ss = 1.0
		if strlen(label) le 5 then ss=1.3
		x = !x.window[1] - 0.02
		y = !y.window[1] - 1.5*ny_ch_size*ss

;;  20id_0042-spec-region-20
;mark,'Au',/L,3500,/invert,scale=1.5*h2
;mark,'W',/L,550000,scale=1.5*h2
;mark,'Yb',/L,700,/invert,scale=1.5*h2
;mark,'Pb',/L,1400,/invert,scale=1.5*h2
;
;mark,'Mn',8000,/invert,scale=h2
;mark,'Fe',2500000,scale=h2
;mark,'Ni',3000,/invert,scale=h2
;mark,'Cu',120000,scale=h2
;mark,'Zn',59000,scale=h2
;mark,'Ga',30000,scale=h2
;mark,'As',65000,scale=h2
;mark,'Br',280000,scale=h2
;mark,'Sr',1500000,scale=h2,/middle

;;  20id_0139-spec-region-19
;mark,'Au',/L,500,/invert,scale=1.5*h2
;mark,'W',/L,110000,scale=1.5*h2
;;mark,'Yb',/L,140,/invert,scale=1.5*h2
;mark,'Pb',/L,200,/invert,scale=1.5*h2
;
;mark,'Mn',800,/invert,scale=h2
;mark,'Fe',400000,scale=h2
;mark,'Ni',400,/invert,scale=h2
;mark,'Cu',24000,scale=h2
;mark,'Zn',11800,scale=h2
;mark,'Ga',6000,scale=h2
;mark,'As',20000,scale=h2
;mark,'Br',56000,scale=h2
;mark,'Sr',200000,scale=h2,/middle

;;  20id_0082-spec-region-22
;mark,'Au',/L,10000,scale=1.5*h2

;;  pncid_0082-spec-region-0
;mark,'Au',/L,250,/invert,scale=1.5*h2
;;mark,'W',/L,250,scale=1.5*h2, /invert
;;mark,'Yb',/L,140,/invert,scale=1.5*h2
;mark,'Fe',80000,scale=h2
;mark,'Ni',6000,scale=h2
;mark,'Cu',13000,scale=h2
;mark,'Zn',30000,scale=h2
;mark,'Ga',70000,scale=h2
;mark,'As',100000,scale=h2
;mark,'Rb',100000,scale=h2
;mark,'Pb',/L,100,/invert,scale=1.5*h2
;mark,'Br',42000,scale=h2

;  pncid_0082-spec-det-1
;mark,'Au',/L,20000,/invert,scale=1.5*h2
;mark,'Au',/L,10000,scale=1.5*h2
;mark,'W',/L,250,scale=1.5*h2, /invert
;mark,'Yb',/L,140,/invert,scale=1.5*h2
;mark,'Fe',8000000,scale=h2
;mark,'Ni',200000,scale=h2
;mark,'Cu',500000,scale=h2
;mark,'Zn',1200000,scale=h2
;mark,'Ga',300000,scale=h2
;mark,'As',8000000,scale=h2
;mark,'Se',60000,/invert,scale=h2
;mark,'Rb',6000000,scale=h2
;mark,'Pb',/L,7000,/invert,scale=1.5*h2
;mark,'Br',56000,scale=h2

;;	maia Nov-2008 320 det60 fit
;mark,'Ti',80,/invert,scale=h2
;mark,'Mn',80,/invert,scale=h2
;mark,'Fe',10000,scale=h2
;mark,'Cu',1700,scale=h2
;mark,'Zn',770,scale=h2
;mark,'Ga',370,scale=h2
;mark,'As',10000,scale=h2
;mark,'Br',2000,scale=h2
;mark,'Rb',700,scale=h2
;mark,'Sr',2700,scale=h2
;mark,'Y',6000,scale=h2
;mark,'Zr',14000,scale=h2
;mark,'Nb',2700,scale=h2
;mark,'Hf',32,/L,/invert,scale=1.4*h2
;mark,'Th',16,/L,/invert,scale=1.5*h2

		if options.title.on then begin
			xyouts,x,y,label, charthick=cthick*ss, alignment=1.0,/norm,charsize=csize*ss
		endif
		lid = lid-h
		if nq gt 4 then noylabel = 1
		!p.title = ''
	endfor

;----------------------------------------------------------------------------

cont:
	if used_non_screen then begin
		device, /close
		on_ioerror, cont1
		if !d.name ne 'PRINTER' then device, file='null'
cont1:
	endif

	tvlct, red,green,blue, /get
	if png then begin
		c = tvrd()
		write_png, file, c, red,green,blue
	endif else if jpeg then begin
		c = tvrd( /true)
		write_jpeg, file, c, /true, quality=100
	endif

;-------------------------------------------------------------------------------------------

cont2:
	on_ioerror, cont2
	if used_non_screen or png or jpeg then set_plot, xwin
done:
	!p.linestyle = 0
	!p.multi = 0
	!p.psym = 0
	!x.charsize = 1.0
	!x.style = 1
	!x.thick = 1.0
	!x.ticks = 0
	!y.charsize = 1.0
	!y.style = 1
	!y.thick = 1.0
	!y.ticks = 0

	!p.charsize = 1.0
	!p.title = ''
	!x.title = ''
	!y.title = ''
	!p.thick = 1.0
	!p.charthick = 1.0
	!p.charsize=1.0
	!p.position = [0,0,0,0]
	!p.color = spec_colour('white')
	!p.background = spec_colour('black')
	return
end
