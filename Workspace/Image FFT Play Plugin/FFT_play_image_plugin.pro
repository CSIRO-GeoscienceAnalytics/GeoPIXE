;
;  Null Image plugin routine
;  -----------------------------
;
;  All Image template routines MUST be named with "_image__plugin.pro"
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

pro FFT_play_image_plugin, p, i, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = 'FFT Play Plugin'				; return the menu title for this plugin
	return
endif

;image_save_undo, p, i						; this will save image in undo buffer
;											; this routine is part of GeoPIXE

;...............................................................................................
;
; Now comes your code here. Use the image data "(*pimg)[ix,iy,iel]" as source,
; and form a new image called "img". The example below will just do a simple smooth:
; Make use of these parameters from the image structure.

pimg = (*p).image							; pointer to the image arrays for all elements
sx = (*p).xsize								; X size of image in pixels
sy = (*p).ysize								; Y size of image in pixels
ca = (*p).cal.poly[1]						; energy calibration energy per channel
cb = (*p).cal.poly[0]						; energy calibration offset
cunits = (*p).cal.units						; energy calibration units string
charge = (*p).charge						; integrated charge for images (uC)

xanes_stack_test, p, xanes, n_el, el, el_xanes

; Use this space for some misc test actions on any element,
; and store result back in the 'Back' image plane, element #0

f = fft( (*pimg)[*,*,i], /center)			; complex FFT, centred

; Try spots on Y axis only, to filter horizontal stripes ...

centre = {x:1381, y:924}
r = 5
circle_diam, centre.x-r, centre.y-r, centre.x+r, centre.y+r, x,y
q = polyfillv( x,y, sx,sy)
f[q] = 0.
centre = {x:1381, y:104}
r = 5
circle_diam, centre.x-r, centre.y-r, centre.x+r, centre.y+r, x,y
q = polyfillv( x,y, sx,sy)
f[q] = 0.

centre = {x:1381, y:820}
r = 3
circle_diam, centre.x-r, centre.y-r, centre.x+r, centre.y+r, x,y
q = polyfillv( x,y, sx,sy)
f[q] = 0.
centre = {x:1381, y:207}
r = 3
circle_diam, centre.x-r, centre.y-r, centre.x+r, centre.y+r, x,y
q = polyfillv( x,y, sx,sy)
f[q] = 0.

centre = {x:1381, y:668}
r = 5
circle_diam, centre.x-r, centre.y-r, centre.x+r, centre.y+r, x,y
q = polyfillv( x,y, sx,sy)
f[q] = 0.
centre = {x:1381, y:356}
r = 5
circle_diam, centre.x-r, centre.y-r, centre.x+r, centre.y+r, x,y
q = polyfillv( x,y, sx,sy)
f[q] = 0.

; Try centre Y axis, above and below ...

mid = sy/2
width = 2.
bx = [-1.,1.,1.,-1.,-1.]
by = [50,50,510,510,50]

x = 1381 + bx*width
y = mid + by
q = polyfillv( x,y, sx,sy)
f[q] = 0.

x = 1381 + bx*width
y = mid - by
q = polyfillv( x,y, sx,sy)
f[q] = 0.

(*pimg)[*,*,0] = abs(fft( f, /center, /inverse))


;(*pimg)[*,*,0] = abs(fft( (*pimg)[*,*,i], /center))

; A simple history record for this plugin.
; No need to add the plugin name here; this is done by GeoPIXE.

history = 'FFT Play of plane ' + str_tidy(i)

;...............................................................................................

return
end

