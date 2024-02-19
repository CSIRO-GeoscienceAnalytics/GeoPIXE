;
;  export_flat Image plugin routine
;  -----------------------------
;
;  All Image conc_offset routines MUST be named with "_image__plugin.pro"
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

pro export_flat_image_plugin, p, i, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Export Flat Image Plugin'		; return the menu title for this plugin
	return
endif

;...............................................................................................
;
; Now comes your code here. Use the image data "(*pimg)[ix,iy,iel]" as source,
; and form a new image called "img". The example below will just do a simple smooth:
; Make use of these parameters from the image structure.

pimg = (*p).image							; pointer to the image arrays for all elements
var = (*p).error							; pointer to 'var' (different size!)
sx = (*p).xsize								; X size of image in pixels
sy = (*p).ysize								; Y size of image in pixels
n_el = (*p).n_el							; number of elements/images
el_name = (*(*p).el)[i]						; name of this image
ca = (*p).cal.poly[1]						; energy calibration energy per channel
cb = (*p).cal.poly[0]						; energy calibration offset
cunits = (*p).cal.units						; energy calibration units string
charge = (*p).charge						; integrated charge for images (uC)

	history = 'Exported flat'
	
	title = 'Select Element Images to Export'
	select = element_select( 0L, *(*p).el, title=title, path=extract_path((*p).file) )
	q = where(select eq 1)
	if q[0] eq -1 then return
	
	nq = n_elements(q)
	el = (*(*p).el)[q]
	
	file = strip_file_ext( (*p).file) + '.dat'
	info = strip_file_ext( (*p).file) + '.csv'
	on_ioerror, done
	openw, lun, file, /get_lun
	openw, lun2, info, /get_lun
	
	printf, lun2, 'file, ' + (*p).file
	printf, lun2, 'Xsize, ' + str_tidy( (*p).xsize)
	printf, lun2, 'Ysize, ' + str_tidy( (*p).ysize)
	printf, lun2, 'Nel, ' + str_tidy( nq)
	printf, lun2, 'Names, ' + strjoin( el, ', ')

	writeu, lun, (*p).xsize, (*p).ysize, nq
	for i=0,nq-1 do begin
		writeu, lun, (*pimg)[*,*,q[i]]
	endfor
	
done:
	close_file, lun
	close_file, lun2
	return

bad:
	warning,'export_flat_image_plugin','Error exporting data.'
	goto, done

;...............................................................................................

end

