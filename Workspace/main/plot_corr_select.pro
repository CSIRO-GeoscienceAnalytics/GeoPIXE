;
;	select = plot_corr_select( group, el_names, path=path, old_select=old_select, $
;		cgm=cgm, wmf=wmf, eps=eps, csv=csv, just_one=just_one, corr_pstate=corr_pstate, $
;		crop=crop
;
;	Select plot options, and elements.
;
;	'select' is the selection, of the form:
;		{plot:plot, error:0 }
;
;	plot is of the form:
;		Note: keep this format consistent with other definitions of PLOT file parameters
;		e.g. as in plot_corr. In other words, keep this struct length.
;
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
;		}
;
;	group		parent widget
;	old_select	current selection states
;	select		new selection states
;
;----------------------------------------------------------------------

pro plot_corr_select_event, Event

	child = widget_info( event.top, /child)
	widget_control, child, get_uvalue=pstate

	case tag_names( event,/structure) of
		'WIDGET_TRACKING': begin
			widget_control, event.id, get_uvalue=s
			if size(s,/tname) eq 'STRING' then begin
				if event.enter eq 1 then begin
					widget_control, (*pstate).help, set_value=s
				endif else begin
					widget_control, (*pstate).help, set_value='Context sensitive help is displayed here. Move cursor over widget to get help on it.'
				endelse
			endif
			goto, finish
			end
		else:
	endcase

	uname = widget_info( event.id, /uname)

	case uname of

		'plot_corr_select_TLB': begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' )then begin
				widget_control, event.top, /destroy
				return
			endif
			end

		'output-mode': begin
			(*(*pstate).p).plot.type = (*pstate).plot_types[event.index]
			end
		'line-thick': begin
			(*(*pstate).p).plot.LineThick = (*pstate).line_thicknesses[event.index]
			end
		'char-thick': begin
			(*(*pstate).p).plot.CharThick = (*pstate).char_thicknesses[event.index]
			end
		'char-size': begin
			(*(*pstate).p).plot.CharSize = (*pstate).char_sizes[event.index]
			end
		'background': begin
			(*(*pstate).p).plot.White = event.index
			end
		'element-label-position': begin
			(*(*pstate).p).plot.LabelPos = event.index
			end
		'element-label-colour': begin
			(*(*pstate).p).plot.LabelColour = event.index
			end

		'load-colour-table': begin
			xloadct, group=event.top, /modal, bottom=16, ncolors=100
			end
		'invert-table': begin
			(*(*pstate).p).plot.Invert = event.select
			end

		'options': begin
			case event.value of
				0: (*(*pstate).p).plot.LabelAxes = event.select
				1: (*(*pstate).p).plot.ZaxisLegend = event.select
				2: (*(*pstate).p).plot.ShowShape = event.select
				3: (*(*pstate).p).plot.title.on = event.select
				else:
			endcase
			end

		'title-options': begin
			if event.select then (*(*pstate).p).plot.title.mode = event.value
			end

		'title-text': begin
			widget_control, (*pstate).text, get_value=s
			(*(*pstate).p).plot.title.text = s
			end

		'load': begin
			F = file_requester( /read, /must_exist, filter = '*.plot', $
					title='Load Assoc Plot Options from File', path=(*pstate).path, $
					group=event.top, /fix_filter)
			if F ne '' then begin
				load_plot_corr_select, pstate, F
				plot_corr_select_update, pstate
			endif
			end
		'save': begin
			F = file_requester( /write, filter = '*.plot', $
					title='Save Assoc Plot Options to File', path=(*pstate).path, $
					group=event.top, /fix_filter)
			if F ne '' then begin
				save_plot_corr_select, pstate, F
			endif
			end

		'preview': begin
			widget_control, (*pstate).text, get_value=s
			(*(*pstate).p).plot.title.text = s

			if size(*((*pstate).corr_pstate),/tname) eq 'STRUCT' then begin
				plot_corr, (*pstate).corr_pstate, options=(*(*pstate).p).plot, /screen
			endif
			end

		'defaults': begin
;			(*(*pstate).p).plot.type = 'CGM'
			(*(*pstate).p).plot.White =  ((*(*pstate).p).plot.type eq 'PRINTER') ? 1 : 0
			(*(*pstate).p).plot.SymSize = 0.5
			(*(*pstate).p).plot.CharSize = 1.8
			(*(*pstate).p).plot.CharThick = 1.4
			(*(*pstate).p).plot.LineThick = 1.4
			(*(*pstate).p).plot.ColourTable = 5
			(*(*pstate).p).plot.Invert = 0
			(*(*pstate).p).plot.LabelAxes = 1
			(*(*pstate).p).plot.ZaxisLegend = 1
			(*(*pstate).p).plot.title.on = 1
			(*(*pstate).p).plot.title.mode = 0
			(*(*pstate).p).plot.title.text = ''
			(*(*pstate).p).plot.LabelPos = 0
			(*(*pstate).p).plot.LabelColour = 0
			(*(*pstate).p).plot.ShowShape = 1

			plot_corr_select_update, pstate
			end

		'cancel': begin
			(*(*pstate).p).error = 1
			widget_control, event.top, /destroy
			return
			end
		'ok': begin
			widget_control, (*pstate).text, get_value=s
			(*(*pstate).p).plot.title.text = s
			widget_control, event.top, /destroy
			return
			end
		else:
	endcase

finish:
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_background3, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).plot.white
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_char_size3, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	q = sort( abs((*pstate).char_sizes - (*(*pstate).p).plot.CharSize))
	(*(*pstate).p).plot.CharSize = (*pstate).char_sizes[q[0]]

	widget_control, wWidget, set_combobox_select=q[0]
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_char_thick3, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	q = sort( abs((*pstate).char_thicknesses - (*(*pstate).p).plot.CharThick))
	(*(*pstate).p).plot.CharThick = (*pstate).char_thicknesses[q[0]]

	widget_control, wWidget, set_combobox_select=q[0]
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_element_label_colour3, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).plot.LabelColour
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_element_label_position3, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).plot.LabelPos
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_line_thick3, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	q = sort( abs((*pstate).line_thicknesses - (*(*pstate).p).plot.LineThick))
	(*(*pstate).p).plot.LineThick = (*pstate).line_thicknesses[q[0]]

	widget_control, wWidget, set_combobox_select=q[0]
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_output_mode3, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	q = where( (*pstate).plot_types eq (*(*pstate).p).plot.type)
	if q[0] ne -1 then widget_control, wWidget, set_combobox_select=q[0]
endif
end

;------------------------------------------------------------------------------------------

pro plot_corr_select_update, pstate

	if ptr_valid( (*pstate).p) eq 0 then return

	q = where( (*pstate).plot_types eq (*(*pstate).p).plot.type)
	if q[0] ne -1 then widget_control, (*pstate).output_mode, set_combobox_select=q[0]

	q = sort( abs((*pstate).char_thicknesses - (*(*pstate).p).plot.CharThick))
	(*(*pstate).p).plot.CharThick = (*pstate).char_thicknesses[q[0]]
	widget_control, (*pstate).char_thick, set_combobox_select=q[0]

	q = sort( abs((*pstate).char_sizes - (*(*pstate).p).plot.CharSize))
	(*(*pstate).p).plot.CharSize = (*pstate).char_sizes[q[0]]
	widget_control, (*pstate).char_size, set_combobox_select=q[0]

	q = sort( abs((*pstate).line_thicknesses - (*(*pstate).p).plot.LineThick))
	(*(*pstate).p).plot.LineThick = (*pstate).line_thicknesses[q[0]]
	widget_control, (*pstate).line_thick, set_combobox_select=q[0]

	widget_control, (*pstate).background, set_combobox_select=(*(*pstate).p).plot.white
	widget_control, (*pstate).element_label_pos, set_combobox_select=(*(*pstate).p).plot.LabelPos
	widget_control, (*pstate).element_label_colour, set_combobox_select=(*(*pstate).p).plot.LabelColour

	options = [ (*(*pstate).p).plot.LabelAxes, (*(*pstate).p).plot.ZaxisLegend, $
				(*(*pstate).p).plot.ShowShape, (*(*pstate).p).plot.title.on]
	widget_control, (*pstate).options, set_value=options

	widget_control, (*pstate).title_options, set_value=(*(*pstate).p).plot.title.mode
	widget_control, (*pstate).text, set_value=(*(*pstate).p).plot.title.text
	if (*pstate).invert ne 0L then widget_control, (*pstate).invert, set_value=(*(*pstate).p).plot.invert
	return
end

;-----------------------------------------------------------------

pro save_plot_corr_select, pstate, file

	file = strip_file_ext(file) + '.plot'
	on_ioerror, bad_open
	openw, lun, file, /xdr, /get_lun

	on_ioerror, bad_io
	version = -2
	writeu,lun, version
	writeu,lun, (*(*pstate).p).plot

done:
	close_file, lun
	return

bad_open:
	warning,'save_plot_corr_select','error opening file: '+file
	goto, done
bad_io:
	warning,'save_plot_corr_select','error writing file: '+file
	goto, done
end

;-----------------------------------------------------------------

pro load_plot_corr_select, pstate, file

	plot_options = load_plot_options( file, error=error)
	if error then return

	(*(*pstate).p).plot = plot_options
	return
end

;-----------------------------------------------------------------

function plot_corr_select, group, path=path, old_select=old_select, $
		cgm=cgm, wmf=wmf, eps=eps, csv=csv, corr_pstate=corr_pstate

;	Select plot options, and elements.
;
;	'select' is the selection, of the form:
;		{plot:plot, error:0 }
;
;	plot is of the form:
;		Note: keep this format consistent with other definitions of PLOT file parameters
;		e.g. as in plot_spectrum_select. In other words, keep this struct length.
;
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

	if n_elements(group) lt 1 then return, 0
	if n_elements(path) lt 1 then path=''
	if n_elements(cgm) lt 1 then cgm=0
	if n_elements(wmf) lt 1 then wmf=0
	if n_elements(eps) lt 1 then eps=0
	if n_elements(csv) lt 1 then csv=0
	if n_elements(corr_pstate) lt 1 then corr_pstate=ptr_new()
	n_els = n_elements(el_names)

	if n_elements(old_select) gt 0 then begin
		plot_options = old_select.plot
	endif else begin
		plot_options = define(/plot_options)
	endelse
	plot_options.type = 'PRINTER'
	if cgm then plot_options.type = 'CGM'
	if wmf then plot_options.type = 'METAFILE'
	if eps then plot_options.type = 'PS'
	if csv then plot_options.type = 'CSV'

	select = {plot:plot_options, error:0 }

	output_titles = ['Computer Graphics Metafile (CGM)','Windows Metafile (WMF)', 'Encapsulated PostScript', $
					'Default Printer','Spreadsheet CSV File']
	char_sizes = 0.4 + 0.2*findgen(19)
	line_thicknesses = 0.4 + 0.2*findgen(19)
	char_thicknesses = 0.4 + 0.2*findgen(19)
	plot_types = ['CGM','METAFILE','PS','PRINTER','CSV']

	xsize = 300
	ysize = 500
	device, get_screen_size=sz
	xoffset = max([0, fix( (sz[0]-xsize) / 2.0)-20 ])
	yoffset = max([0, fix( (sz[1]-ysize) / 2.0)-60 ])

	tlb = widget_base( /column, title='Plot Assoc Select', /TLB_KILL_REQUEST_EVENTS, xoffset=xoffset, yoffset=yoffset, $
					group_leader=group, uname='plot_corr_select_TLB', /base_align_center, $
					/modal, /floating, $
					xpad=10, ypad=10, space=5)
	tbase = widget_base( tlb, /column, xpad=0, ypad=0, space=2, /base_align_center)

	obase = widget_base( tbase, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( obase, value='Output Format:')
	output_mode = widget_combobox( obase, value=output_titles, uname='output-mode', /tracking, $
					notify_realize='OnRealize_output_mode3', $
					uvalue='Select the output format/device for the Image plot.', xsize=200)

	cbase = widget_base( tbase, /row, /base_align_center, xpad=0, ypad=0, space=5)
	c1base = widget_base( cbase, /column, /base_align_right, xpad=0, ypad=0, space=0)
	c2base = widget_base( cbase, /column, /base_align_right, xpad=0, ypad=0, space=0)

	c11base = widget_base( c1base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( c11base, value='Char Thick:')
	charthick = widget_combobox( c11base, value=str_tidy(char_thicknesses), uname='char-thick', /tracking, $
					notify_realize='OnRealize_char_thick3', $
					uvalue='Select the line thickness for Characters in the Image plot. ', xsize=70)
	c12base = widget_base( c1base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( c12base, value='Char Size:')
	charsize = widget_combobox( c12base, value=str_tidy(char_sizes), uname='char-size', /tracking, $
					notify_realize='OnRealize_char_size3', $
					uvalue='Select the size for Characters in the Image plot.', xsize=70)

	c21base = widget_base( c2base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( c21base, value='Line Thick:')
	linethick = widget_combobox( c21base, value=str_tidy(line_thicknesses), uname='line-thick', /tracking, $
					notify_realize='OnRealize_line_thick3', $
					uvalue='Select the thickness of plot lines in the Image plot.', xsize=70)
	c22base = widget_base( c2base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( c22base, value='Background:')
	background = widget_combobox( c22base, value=['Black','White'], uname='background', /tracking, $
					notify_realize='OnRealize_background3', $
					uvalue='Select the colour of the plot background.', xsize=70)

	invert = 0L
;	ctbase = widget_base( tbase, /row, /base_align_center, xpad=0, ypad=0, space=5)
;	lab = widget_label( ctbase, value='Colour Table:')
;	colour_table_button = widget_button( ctbase, value='Select Table', uname='load-colour-table', /tracking, $
;					uvalue='Select a colour table from the pop-up list.', scr_xsize=90)
;	Invert = cw_bgroup2( ctbase, ['Invert Table'], /column, xpad=0, ypad=0, space=0, $
;					/return_index, /tracking, $
;					uname='invert-table', set_value=select.plot.invert, /nonexclusive, $
;					uvalue=['Invert the selected colour table. i.e. Lowest colour indices will be used for highest image pixel values.'])

	dbase = widget_base( tbase, /row, /base_align_center, xpad=0, ypad=0, space=5)
	d1base = widget_base( dbase, /column, /base_align_left, xpad=0, ypad=0, space=0)
	d2base0 = widget_base( dbase, /column, /base_align_left, xpad=0, ypad=0, space=3)
	d2base = widget_base( d2base0, /column, /base_align_left, xpad=1, ypad=2, space=0, /frame)

	options = cw_bgroup2( d1base, ['Label XY Axes','Z Axis Legend','Show Shape','Plot Title'], /column, $
					xpad=0, ypad=0, space=0, /return_index, /tracking, $
					uname='options', set_value=[select.plot.LabelAxes, select.plot.ZaxisLegend, $
					select.plot.ShowShape,select.plot.title.on], /nonexclusive, $
					uvalue=['Include labels on the X and Y axes.','Include a Z axis/histogram value legend.', $
					'Draw the current spline selection shape on the output Association plot.', $
					'Include a title above the plot. The type of title can be selected in the box to the right.'])

	lab = widget_label( d2base, value='TITLE',/align_center)
	title_options = cw_bgroup2( d2base, ['File Name','Sample and Grain','User Text (below)'], /column, $
					xpad=0, ypad=0, space=0, /return_index, /tracking, $
					uname='title-options', set_value=select.plot.title.mode, /exclusive, $
					uvalue=['Use the image file name as a plot title.', $
					'Use the Sample name and Grain code as a plot title.', $
					'Use the user text string entered below as a plot title.'])

	d22base = widget_base( d2base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( d22base, value='Text:')
	text = widget_text( d22base, value=select.plot.title.text, uname='title-text', /tracking, /editable, $
					uvalue='Enter a text string to use as a plot title, if "User Text" is selected above.', $
					scr_xsize=100)

	t2base = widget_base( tbase, /column, /align_center, /base_align_right, xpad=0, ypad=0, space=2)

	fbase = widget_base( t2base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( fbase, value='Elements Label Position:')
	element_label_pos = widget_combobox( fbase, value=['Outside','Top Left','Bottom Left','Top Right','Bottom Right'], $
					uname='element-label-position', $
					/tracking, notify_realize='OnRealize_element_label_position3', $
					uvalue='Select the location of the element mneumonic labels.', xsize=100)

	f2base = widget_base( t2base, /row, /base_align_center, xpad=0, ypad=0, space=5)
	lab = widget_label( f2base, value='Element Label Colour:')
	element_label_col = widget_combobox( f2base, value=['Normal','Reversed'], uname='element-label-colour', $
					/tracking, notify_realize='OnRealize_element_label_colour3', $
					uvalue='Select the colour of the Element Labels: "Reversed" or "Normal", which is same as axes.', xsize=100)

	gbase = widget_base( t2base, /column, /base_align_right, xpad=1, ypad=2, space=1, /frame)

	bbase = widget_base( tbase, /row, /base_align_center, /align_center, ypad=1, space=2)
	button = widget_button( bbase, xsize=36, value='Load', uname='load',/tracking,uvalue='Load Plot Corr parameters from a PLOT file.')
	button = widget_button( bbase, xsize=36, value='Save', uname='save',/tracking,uvalue='Save Plot Corr parameters to a PLOT file.')
	lab = widget_label( bbase, value=' ')
	button = widget_button( bbase, xsize=48, value='Defaults', uname='defaults',/tracking,uvalue='Set plot parameters to the defaults.')
	button = widget_button( bbase, xsize=48, value='Preview', uname='preview',/tracking,uvalue='Preview the export plot on the screen.')
	lab = widget_label( bbase, value=' ')
	button = widget_button( bbase, xsize=43, value='Cancel', uname='cancel',/tracking,uvalue='Cancel plot and return.')
	button = widget_button( bbase, xsize=27, value='OK', uname='ok',/tracking,uvalue='Proceed with the file requester and export plot.')

	help = widget_text( tlb, scr_xsize=280, ysize=4, /wrap, uname='help', /tracking, $
				uvalue='Help window. Displays info about widgets.',frame=0)

	p = ptr_new( select )			; current selection

	state = {	$
			path:				path, $					; pointer to current path

			char_sizes:			char_sizes, $			; list of char sizes
			char_thicknesses:	char_thicknesses, $		; list of character thicknesses
			line_thicknesses:	line_thicknesses, $		; list of line thicknesses
			plot_types:			plot_types, $			; plot type codes

			text:				text, $					; widget ID of text box
			output_mode:		output_mode, $			; widget ID of plot mode
			char_thick:			charthick, $			; widget ID of char thick
			char_size:			charsize, $				; widget ID of char size
			line_thick:			linethick, $			; widget ID of line thick
			background:			background, $			; widget ID of background mode
			title_options:		title_options, $		; widget ID of title options CW
			options:			options, $				; widget ID of options CW
			invert:				invert, $				; widget ID of invert CW
			element_label_pos:	element_label_pos, $	; widget ID of element label position
			element_label_colour: element_label_col, $	; widget ID of element label colour
			help:				help, $					; widget ID of help text

			corr_pstate:		corr_pstate, $			; state struct from parent Image
			p:					p $						; pointer to selection
		}

	child = widget_info( tlb, /child)
	widget_control, child, set_uvalue=ptr_new(state, /no_copy)
	widget_control, tlb, /realize

	xmanager, 'plot_corr_select', tlb					, /no_block

	r = *p
	return, r
end
