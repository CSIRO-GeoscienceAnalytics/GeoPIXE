;
;  phase_distance Image plugin routine
;  -----------------------------
;
;  All Image Plugin routines MUST be named with "_image__plugin.pro"
;  at the end of the file name. For a new "Fred" plugin, copy and rename this file
;  to "Fred_image_plugin.pro" and edit the first line to:
;  "pro fred_image_plugin, p, i, title=title, history=history"
;
;  Plugins should be compiled in IDLDE and saved as a SAV file.
;  Only compile routines for ONE plugin and save using the command:
;
;  "SAVE, /routines, filename='fred_image_plugin.sav'"
;
;  for a "fred_image_plugin" plugin.
;
;  To ensure that only one routine exists in a file, exit IDLDE and start it again to compile
;  and save a plugin.
;
;  NOTE: It is important to ensure that ONLY routines for ONE plugin is in each SAV file.
;  Otherwise, unexpected results may result when the SAV files are restored at run-time.
;
;
;  The plugin SAV files will then be loaded automatically when GeoPIXE.sav runs,
;  if the plugin SAV files are located in the same directory as GeoPIXE.sav.
;
;  Plugin arguments:
;	p		pointer to the GeoPIXE image structure for the present loaded images
;	i		the number of the presently displayed element.
;
;  keywords:
;	history		return a history string along with the image result.
;	title		just return the title string (to go in the menu).
;
;  On return to GeoPIXE, it is assumed that only the contents of the selected element image have
;  been changed, and the sizes of images, their total number and element names all remain unchanged.
;  Avoid tinkering with image structure parameters, as strange things may happen.
;
;----------------------------------------------------------------------------------------------

pro phase_distance_image_plugin, p, ii, title=title, history=history

;	Calculate the "phase distance" to the following correct-yields R matrix components

COMPILE_OPT STRICTARR

all	= 1										; (=1) do all frames, or (=0) just current frame ('ii')

if arg_present(title) then begin
	title = (all ? '* ' : '')+'phase_distance Image Plugin'	; return the menu title for this plugin
	return									; "*" means it applies to all images
endif
if all then i=1L else i=ii

if all eq 0 then image_save_undo, p, i		; this will save image in undo buffer
											; this routine is part of GeoPIXE
											; not used for "*" all images option
											;
;...............................................................................................
;
; Now comes your code here. Use the image data "(*pimg)[ix,iy,iel]" as source.
; Make use of these parameters from the image structure. In debugging, you might
; find the routine "pointer_display, p" useful to display the 'p' struct.

pimg = (*p).image							; pointer to the image arrays for all elements
opt = (*p).options							; display ranges, etc.
nx = (*p).xsize								; X size of image in pixels
ny = (*p).ysize								; Y size of image in pixels
ca = (*p).cal.poly[1]						; energy calibration energy per channel
cb = (*p).cal.poly[0]						; energy calibration offset
cunits = (*p).cal.units						; energy calibration units string
charge = (*p).charge						; integrated charge for images (uC)

; Determine if this is an image of elements or a XANES stack
;	xanes		1 if XANES stack, 0 for element images
;	n_el		# elements (XANES=0), # stack plane energies (XANES=1)
;	el			elements or xanes energy labels
;	el_xanes	element whose edge we have scanned for XANES

xanes_stack_test, p, xanes, n_el, el, el_xanes
if xanes eq 0 then return

;...............................................................................................

if ny lt 3 then return
if n_el lt 2 then return

if all then begin
	drop = ['Use inter-frame image cross-correlation','Read image shift values from CSV file']
	help_drop = 'Select between (i) correcting the image stack shift and generating a file of X,Y shift values, ' + $
				'or (ii) reading a previous shift CSV file and applying it to the current image stack.'
	map_file = [0,1]
	file = 'Shifts'
	help_file = 'Select file of X,Y image shifts per frame/energy step.'
	r = options_popup( title='Image Shift Correction Options', drop=drop, file=file, $
			path=extract_path((*p).file), map_file=map_file, help_drop=help_drop, $
			help_file=help_file, filter='*.csv', error=error)
	if error then return
	use_file_values = r.drop[0]
	if use_file_values then begin
		e = get_xanes_energies( r.file[0], do_xanes=shift_ok, x=x, y=y)
		if shift_ok eq 0 then return
	endif else begin
		file = strip_file_ext( (*p).file) + '-shift.csv'
		print,'Save shift results per frame to file ',file
		on_ioerror, bad
		openw, lun, file, /get_lun
		printf, lun, '# X,Y image shift for stack or image: '+(*p).file
		printf, lun, el[0], ',', 0.0, ',', 0.0		; dummy for first image, i=0
		x = fltarr(n_el)
		y = fltarr(n_el)
	endelse
endif

scan:
	if (all eq 0) or (use_file_values eq 0) then begin
;		N.B. Can't use these with copy back of image2_shifted
;		image1 = median( (*pimg)[*,*,0], 5<(nx/2))
;		image2 = median( (*pimg)[*,*,i], 5<(nx/2))
;		image1 = median( (*pimg)[*,*,0], 3>(nx/100))
;		image2 = median( (*pimg)[*,*,i], 3>(nx/100))

		image1 = (*pimg)[*,*,0]				; compare absolute shift relative for frame #0
		image2 = (*pimg)[*,*,i]
		img_phase_distance, image1, image2, xshift, yshift, image2_shifted, /cm	;, /sobel
		print, 'i,el, shift X,Y=', i,'   ',el[i], xshift, yshift

		(*pimg)[*,*,i] = image2_shifted		; copy back shifted image2
		x[i] = xshift
		y[i] = yshift

		printf, lun, el[i], ',', x[i], ',', y[i]
	endif
	
	if all then begin
		if (use_file_values eq 0) then begin
			i = i+1
			if (i lt n_el) then goto, scan
			goto, finish
		endif else begin
			i = 1L
			goto, correct
		endelse
	endif else goto, finish

correct:
	(*pimg)[*,*,i] = shift_image( (*pimg)[*,*,i], -x[i],-y[i])

	if all then begin
			i = i+1
			if (i lt n_el) then goto, correct
	endif else goto, done
	
finish:
	if use_file_values then goto, done
	close_file, lun
	
	window,0
	!p.title = 'Integral image shift results'
	!x.title = 'X shift'
	!y.title = 'Y shift'
	plot,x,y,psym=3
	warning,'phase_distance_image_plugin',['Saved cummulative shift results per frame to file: ',file],/info

; A simple history record for this plugin.
; No need to add the plugin name here; this is done by GeoPIXE.

done:
	history = 'phase_distanceed position using "phase_distance image" plugin'
	return

;...............................................................................................

bad:
	warning,'phase_distance_image_plugin','Error writing shift output file '+file
	if (use_file_values eq 0) then close_file, lun
	return
end

