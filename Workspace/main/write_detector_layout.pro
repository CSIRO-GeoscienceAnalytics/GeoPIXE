	pro write_detector_layout, file, data=p, error=error
;
;	Write a detector specification CSV file.
;	from struct pointed to by 'p'.
;	
;	Tokens:
;	Title		title string
;	N			number of detectors
;	Shape		shape of detectors (0=circular, 1=square)
;	Symmetry	Detector array symmetry steps for 360 degree (default is 4).
;	Reorient	Re-orient or rotate the detector by 360/symmetry degrees
;	Veto		veto detector channels based on FWHM threshold
;	Threshold	FWHM threshold (eV)
;	Data		last token, flags start of detector data
;	Ref			back reference table from detector number to CSV index
;	
;	List of all CSV table indices referenced by ref, with 'start' offset:
;		ref = (*playout).ref[ (*playout).start + indgen((*playout).N) ]
;
;	Detector index corrected for non-zero 'start':
;		(*playout).data[i].index - (*playout).start
;
;	For each detector element, on a separate comma-separated line:
;	index, X, Y, Z, width, height, tilt, bad, FWHM
;
;	where:
;	index		is the detector number, starting at zero.
;	X,Y			is the position (mm) of the centre of the detector element,
;				relative to the middle of the detector array (u,v coords).
;	Z			is the out of plane offset for the detector element (mm) (zero for planar detector array)
;	Width		width (mm)
;	Height		height (mm)
;	Tilt		tilt angle of normal for element relative to the normal for
;				the centre of the detector array, defined as 'towards the centre of the array'
;	Bad			flags a bad element (0=good, 1=bad, >1 may indicate bad modes)
;	FWHM		FWHM for Mn Ka (eV) for this detector element
;
; Extended Maia columns:
;	Hermes		which Hermes does pad belong to
;	Quadrant	which quadrant is it in
;	Radial		which radial group is it in
;	Column		which column
;	Row			which row

	COMPILE_OPT STRICTARR

	error = 1
	if n_elements(p) eq 0 then goto, bad_ptr
	if ptr_valid(p) eq 0 then goto, bad_ptr
	if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr
	if n_elements(file) lt 1 then goto, bad_file
	if lenchr(file) lt 1 then goto, bad_file

    tags = tag_names((*p).data[0])
	maia = 0
	if n_elements(tags) gt 9 then maia=1
	
	F = strip_file_ext(file) + '.csv'
	on_ioerror, bad_file
	openw,unit, F, /get_lun
	on_ioerror,err

	printf, unit, '# This file produced by program: "write_detector_layout.pro"'
	printf, unit, 'title,', (*p).title
	printf, unit, 'N,', (*p).n, ',Number of detectors'
	printf, unit, 'Start,', (*p).start, ',Start at this # (default: detector numbers start at 0)
	printf, unit, 'Shape,', (*p).shape, ',Shape of detectors (0=circular, 1=square)'
	printf, unit, 'Symmetry,', (*p).symmetry, ',Detector array symmetry steps for 360 degree (default is 4)'
	printf, unit, 'Reorient,', (*p).reorient, ',Re-orient or rotate by "reorient*360/symmetry" degrees'
	printf, unit, 'MirrorX,', (*p).mirrorX, ',Mirror the layout in X'
	printf, unit, 'MirrorY,', (*p).mirrorY, ',Mirror the layout in Y'
	printf, unit, 'Veto,', (*p).veto, ',Veto channels with excessive FWHM'
	printf, unit, 'Threshold,', (*p).threshold, ',FWHM threshold (eV)'

	if maia then begin
		printf, unit, '# ID, X,Y offsets from centre (mm), Z offset from planar (mm), Width, Height (mm), Tilt (degrees), bad (0=good 1=poor 2=dead), FWHM (eV), Hermes, Quadrant, Radial, Column, Row'
		printf, unit, 'Data, X, Y, Z, width, height, tilt, bad, FWHM, Hermes, Quadrant, Radial, Column, Row'
		f = "(I4,',',F8.3,',',F8.3,',',F8.3,',',F8.3,',',F8.3,',',F8.3,',',I3,',',F8.3,',',I5,',',I5,',',I5,',',I5,',',I5)"
	endif else begin
		printf, unit, '# ID, X,Y offsets from centre (mm), Z offset from planar (mm), Width, Height (mm), Tilt (degrees), bad (0=good, 1=poor, 2=dead), FWHM (eV)'
		printf, unit, 'Data, X, Y, Z, width, height, tilt, bad, FWHM'
		f = "(I4,',',F8.3,',',F8.3,',',F8.3,',',F8.3,',',F8.3,',',F8.3,',',I3,',',F8.3)"
	endelse
	
	for j=0L,(*p).n-1 do begin
		k = (*p).data[j].index
		x = (*p).data[j].x
		y = (*p).data[j].y
		z = (*p).data[j].z
		w = (*p).data[j].width
		h = (*p).data[j].height
		tilt = (*p).data[j].tilt
		bad = (*p).data[j].bad
		fwhm = (*p).data[j].fwhm
		hermes = (*p).data[j].hermes
		quadrant = (*p).data[j].quadrant
		radial = (*p).data[j].radial
		column = (*p).data[j].column
		row = (*p).data[j].row
		if maia then begin
			printf, unit, format=f, k, x,y,z, w,h, tilt, bad, fwhm, Hermes, Quadrant, Radial, Column, Row
		endif else begin
			printf, unit, format=f, k, x,y,z, w,h, tilt, bad, fwhm
		endelse
	endfor
	error = 0
	
finish:
	close_file, unit
	return

err:
	warning, 'read_detector_layout', 'Error writing file.'
	goto, finish
bad_ptr:
	warning, 'read_detector_layout', 'Bad detector data pointer.'
	goto, finish
bad_file:
	warning, 'read_detector_layout', 'Error opening file.'
	goto, finish
	end

