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

pro flux_image_plugin, p, i, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = 'flux Image Plugin'			; return the menu title for this plugin
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
n_el = (*p).n_el							; number of elements/images
el_name = (*(*p).el)[i]						; name of this image
ca = (*p).cal.poly[1]						; energy calibration energy per channel
cb = (*p).cal.poly[0]						; energy calibration offset
cunits = (*p).cal.units						; energy calibration units string
charge = (*p).charge						; integrated charge for images (uC)

if (*p).has_flux then begin
	flux = *(*p).flux
	name = 'Flux'

	q = where( (flux) ge 0.5*max(flux), nq)
	q2 = where( ((flux) lt 0.5*max(flux)) and ((flux) gt 0.), nq2)
	q3 = where( (flux) eq 0., nq3)
	print, 'average flux = ', mean(flux)
	print, 'fraction of pixels > max/2 = ', float(nq)/(float(sx)*float(sy))
	print, 'fraction of pixels < max/2 and not zero = ', float(nq2)/(float(sx)*float(sy))
	print, 'fraction of zero pixels = ', float(nq3)/(float(sx)*float(sy))

	window,0, xsize=500, ysize=400, retain=retain
	top = max(flux)
	binsiz = 0.01 * top
	h = histogram( flux, min=0., max=top, omin=omin,omax=omax, binsize=binsiz, locations=x)
	x = x+binsiz/2.

	!p.color = spec_colour('black')
	!p.background = spec_colour('white')
	!p.title = 'Flux/Charge distribution (corrected for DT)'
	!x.title = 'Flux/Charge per pixel'
	!y.title = 'Frequency'
	!p.charsize = 1.0	;1.2
	!p.charthick = 1.0
	!p.thick = 1.0
	erase
	plot, x,h, xrange=[(omin-binsiz)>0,omax+0.3], /nodata, /ylog, yrange=[0.5,1.03*max(h)], xstyle=1,ystyle=1
	oplot, x,h+0.1, color=spec_colour('green'), thick=2.

	av = total( x*h) / total(h)
	q3 = reverse(sort(h))
	mode = x[q3[0]]	

	xyouts,0.90,0.87, 'Max flux = '+str_tidy(omax,places=1), /norm,align=1

	(*pimg)[*,*,0] = flux	
	(*(*p).el)[0] = name
	
;	A simple history record for this plugin.
;	No need to add the plugin name here; this is done by GeoPIXE.

	history = 'flux found'
endif else begin
	print,'flux plugin: No "flux" array found.'
	history = 'No flux found'
endelse

;...............................................................................................

return
end

