	function read_detector_layout, file, maia=maia, error=error
; 
;	Read in a detector specification CSV file.
;	If not found, then return error=1.
;	maia returns 1 if extended Maia columns are found in CSV
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
;
;	Ref			return back reference table from detector number to CSV index
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
;	Hermes, Quadrant, Radial, Column, Row (extended fields for Maia)

	COMPILE_OPT STRICTARR

	error = 1
	maia = 0
	mp = 0
	if n_elements(file) lt 1 then file='detector.csv'
	if lenchr(file) lt 1 then goto, bad_file

	F = strip_file_ext(file) + '.csv'
	on_ioerror,re_try
	openr,unit,F,/get_lun
	goto, cont

re_try:
	F = strip_path(F)
	on_ioerror,bad_file
	openr,unit,F,/get_lun
	on_ioerror,err

cont:
	mp = 0
	N = 0
	title = ''
	threshold = 0.0
	veto = 0
	reorient = 0
	symmetry = 4
	mirrorX = 0
	mirrorY = 0
	shape = 0
	start = 0

	line = ''

	while EOF(unit) eq 0 do begin
		readf, unit, line
		if (lenchr(line) gt 0) and (extract(line,0,0) ne '#') then begin
			set_separators, ','
			chop_string, line, str, n_str
			s = strlowcase(str[0])

			case s of
				'title': begin
					title = str[1]
					end
				'n': begin
					N = fix(str[1])
					end
				'start': begin
					start = fix(str[1])
					end
				'shape': begin
					shape= fix(str[1])
					end
				'symmetry': begin
					symmetry= fix(str[1])
					end
				'reorient': begin
					reorient= fix(str[1])
					end
				'mirrorx': begin
					mirrorX= fix(str[1])
					end
				'mirrory': begin
					mirrorY= fix(str[1])
					end
				'veto': begin
					veto= fix(str[1])
					end
				'threshold': begin
					threshold= float(str[1])
					end
				'data': begin
					goto, get_data
					end
				else:
			endcase
		endif
	endwhile

	goto, err

get_data:
	if n lt 1 then goto, err

; Take care here. The order of parameters in these structs (esp. n_str=14) is used elsewhere.
; Do not insert or delete parameters. Add new ones to the end.

	case n_str of
		9: begin
			data1 = {index:0, x:0.0, y:0.0, z:0.0, width:0.0, height:0.0, tilt:0.0, bad:0, FWHM:0.0}
			pad = define(/maia_pad)
			data = replicate( pad, N)
			k = 0
			while (k lt N) and (EOF(unit) eq 0) do begin
				on_ioerror, comment9
				readf, unit, data1
				data[k].index = data1.index
				data[k].x = data1.x
				data[k].y = data1.y
				data[k].z = data1.z
				data[k].width = data1.width
				data[k].height = data1.height
				data[k].tilt = data1.tilt
				data[k].bad = data1.bad
				data[k].fwhm = data1.fwhm
				k = k+1
comment9:	
			endwhile
			end

		14: begin
			pad = define(/maia_pad)
			data = replicate( pad, N)
			k = 0
			while (k lt N) and (EOF(unit) eq 0) do begin
				on_ioerror, comment14
				readf, unit, pad
				data[k] = pad
				k = k+1
comment14:	
			endwhile
			maia = 1
			end
		else: goto, err
	endcase
	on_ioerror, null

; Note that detector number may start at 1 (or more), not 0, so need to allow
; space in ref table for larger numbers.

	ref = intarr( max(data.index)+1 )	; create back index from detector number
	ref[ data.index] = indgen(N)		; to layout table index
										; allow extra room for non-zero 'start'

	mp = define(maia_layout=N)
	mp.title =		title 			; comment string
	mp.file =		F 				; file name
	mp.start =		start 			; detector pad number start/offset (def 0)
	mp.shape =		shape 			; shape of detectors (0=circ, 1=square)
	mp.threshold =	threshold 		; FWHM threshold (eV)
	mp.symmetry =	symmetry 		; symmetry modulo number
	mp.mirrorX =	mirrorX 		; mirror in X
	mp.mirrorY =	mirrorY 		; mirror in Y
	mp.reorient =	reorient 		; Re-orient/rotate the detector by 90 degrees
	mp.veto =		veto 			; veto high FWHM above threshold
	mp.data =		data 			; pad coordinate data (mm)
	mp.ref =		ref 			; cross-reference from detector # to table indx.

	error = 0
	
finish:
	close_file, unit
	return, mp

err:
;	warning, 'read_detector_layout', 'Serious format error in layout file.'
	mp = 0
	goto, finish
bad_file:
;	warning, 'read_detector_layout', 'File "'+file+'" not found.'
	mp = 0
	goto, finish
	end

