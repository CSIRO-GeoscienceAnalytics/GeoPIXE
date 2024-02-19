;
;  flux Image plugin routine
;  -----------------------------
;
;  All Image flux routines MUST be named with "_image__plugin.pro"
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

pro detector_histogram_image_plugin, p, i, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = 'Detector Histogram Plugin'	; return the menu title for this plugin
	return
endif

;image_save_undo, p, i						; this will save image in undo buffer
;											; this routine is part of GeoPIXE

np = n_elements(p)
if np lt 1 then return
if ptr_good( p, /struct) eq 0 then return
if ptr_good( (*p).hist) eq 0 then goto, bad_hist

;...............................................................................................
;
; Now comes your code here. Use the image data "(*pimg)[ix,iy,iel]" as source,
; and form a new image called "img". The example below will just do a simple smooth:
; Make use of these parameters from the image structure.

pimg = (*p).image							; pointer to the image arrays for all elements
sx = (*p).xsize								; X size of image in pixels
sy = (*p).ysize								; Y size of image in pixels
n_el = (*p).n_el							; number of elements/images
el_name = (*(*p).el)[i]						; name of this image
ca = (*p).cal.poly[1]						; energy calibration energy per channel
cb = (*p).cal.poly[0]						; energy calibration offset
cunits = (*p).cal.units						; energy calibration units string
charge = (*p).charge						; integrated charge for images (uC)

	h = 100. * *(*p).hist
	
	window, 0
	!p.title = 'Events per Detector'
	!x.title = 'Detector Channel'
	!y.title = '# Events'
	n = n_elements(h)
	x = indgen(n)
	y = h[x]
	plot, x,y, /nodata
	oplot, x,y, psym=10, color=spec_colour('green')
	
;	A simple history record for this plugin.
;	No need to add the plugin name here; this is done by GeoPIXE.

;	history = 'flux found'
	return
	
bad_hist:
	warning,'Detector Histogram plugin','No "hist" array found.'

;...............................................................................................

return
end

