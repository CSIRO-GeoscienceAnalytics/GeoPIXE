function load_plot_options, file, error=error

	COMPILE_OPT STRICTARR
	ErrorNo = 0
	error = 1
	plot_options2 = 0
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
			warning,'load_plot_options',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, plot_options2
		endif
	endif

	on_ioerror, bad_open
	openr, lun, file, /xdr, /get_lun
	on_ioerror, bad_io

	valid = [-1,-2,-3,-4,-5]

	version = 0
	readu,lun, version
	q = where( version eq valid)
	if q[0] eq -1 then goto, bad_io

	case version of
		-1: begin
			plot_options = {  $
				Type:			'CGM', $	; CGM, METAFILE, PRINTER, ...
				Crop:			0, $		; Crop to shape
				White:			0, $		; black background
				SymSize:		0.5, $		; symbol size
				CharSize:		1.1, $		; character size
				CharThick:		0.8, $		; character line thickness
				LineThick:		1.4, $		; drawing line thickness
				ColourTable:	5, $		; colour table
				Invert:			0, $		; no invert colour table
				LabelAxes:		1, $		; yes, label XY axes
				ZaxisLegend:	1, $		; yes, place a Z axis legend
				DistLegend:		0, $		; for a distance legend
				ppmOnly:		0, $		; ppm only, no wt%
				Title: { on:	1, $		; for a title
					mode:		0, $		; for filename
					text:		'' }, $		; optional text
				DistPos:		0, $		; for bottom distance position
				DistColour:		0, $		; for bottom distance colour
				LabelPos:		0, $		; for outside element label
				LabelColour:	0, $		; for element label colour
				ConcMaxMode:	0, $		; for auto range, 1-2-5 maxima with 10 step colours
				ManualMax:		100., $		; max conc scale in manual mode
				ShowShape:		1 $			; show the shape
			}
			end

		-2: begin
			plot_options = {  $
				Type:			'CGM', $	; CGM, METAFILE, PRINTER, ...
				Crop:			0, $		; Crop to shape
				White:			0, $		; black background
				SymSize:		0.5, $		; symbol size
				CharSize:		1.1, $		; character size
				CharThick:		0.8, $		; character line thickness
				LineThick:		1.4, $		; drawing line thickness
				ColourTable:	5, $		; colour table
				Invert:			0, $		; no invert colour table
				LabelAxes:		1, $		; yes, label XY axes
				ZaxisLegend:	1, $		; yes, place a Z axis legend
				DistLegend:		0, $		; for a distance legend
				ppmOnly:		0, $		; ppm only, no wt%
				Title: { on:	1, $		; for a title
					mode:		0, $		; for filename
					text:		'' }, $		; optional text
				DistPos:		0, $		; for bottom distance position
				DistColour:		0, $		; for bottom distance colour
				LabelPos:		0, $		; for outside element label
				LabelColour:	0, $		; for element label colour
				ConcMaxMode:	0, $		; for auto range, 1-2-5 maxima with 10 step colours
				ManualMax:		100., $		; max conc scale in manual mode
				ShowShape:		1 $			; show the shape
			}
			end

		-3: begin
			plot_options = {  $
				Type:			'CGM', $	; CGM, METAFILE, PRINTER, ...
				Crop:			0, $		; Crop to shape
				White:			0, $		; black background
				SymSize:		0.5, $		; symbol size
				CharSize:		1.1, $		; character size
				CharThick:		0.8, $		; character line thickness
				LineThick:		1.4, $		; drawing line thickness
				ColourTable:	5, $		; colour table
				Invert:			0, $		; no invert colour table
				LabelAxes:		1, $		; yes, label XY axes
				ZaxisLegend:	1, $		; yes, place a Z axis legend
				DistLegend:		0, $		; for a distance legend
				ppmOnly:		0, $		; ppm only, no wt%
				Title: { on:	1, $		; for a title
					mode:		0, $		; for filename
					text:		'' }, $		; optional text
				DistPos:		0, $		; for bottom distance position
				DistColour:		0, $		; for bottom distance colour
				LabelPos:		0, $		; for outside element label
				LabelColour:	0, $		; for element label colour
				ConcMaxMode:	0, $		; for auto range, 1-2-5 maxima with 10 step colours
				ManualMax:		100., $		; max conc scale in manual mode
				ShowShape:		1, $		; show the shape
				Absolute:		0  $		; absolute distance scales with origin
			}
			end

		-4: begin
			plot_options = {  $
				Type:			'CGM', $	; CGM, METAFILE, PRINTER, ...
				Crop:			0, $		; Crop to shape
				White:			0, $		; black background
				SymSize:		0.5, $		; symbol size
				CharSize:		1.4, $		; character size
				CharThick:		1.2, $		; character line thickness
				LineThick:		1.2, $		; drawing line thickness
				ColourTable:	5, $		; colour table
				Invert:			0, $		; no invert colour table
				LabelAxes:		1, $		; yes, label XY axes
				ZaxisLegend:	1, $		; yes, place a Z axis legend
				DistLegend:		0, $		; for a distance legend
				ppmOnly:		0, $		; ppm only, no wt%
				Title: { on:	1, $		; for a title
					mode:		0, $		; for filename
					text:		'' }, $		; optional text
				DistPos:		0, $		; for bottom distance position
				DistColour:		0, $		; for bottom distance colour
				LabelPos:		0, $		; for outside element label
				LabelColour:	0, $		; for element label colour
				ConcMaxMode:	0, $		; for auto range, 1-2-5 maxima with 10 step colours
				ManualMax:		100., $		; max conc scale in manual mode
				ShowShape:		1, $		; show the shape
				Absolute:		0, $,		; absolute distance scales with origin
				centroids: { on: 0, $		; plot region centroids for selected element ON
					element:	''}, $		; select centroid element name
				enhance: { $
					spots: { on: 0, $		; enhance hot-spots
						elements: ''}}, $	; select enhance element names (1 to 3, sep by spaces)
				max_area: 0, $				; use a maximum display area, strip borders
				separate:		0, $		; plot separate spectra, or common axes
				landscape:		0}			; Landscape orientation
			end

		-5: begin
			plot_options = define(/plot_options)
			end
	endcase

	readu,lun, plot_options

	plot_options2 = define(/plot_options)
	struct_assign, plot_options, plot_options2
	error = 0

done:
	close_file, lun
	return, plot_options2
	
bad_open:
	warning,'load_plot_options','error opening file: '+file
	goto, done
bad_io:
	warning,'load_plot_options','error reading file: '+file
	goto, done
end
