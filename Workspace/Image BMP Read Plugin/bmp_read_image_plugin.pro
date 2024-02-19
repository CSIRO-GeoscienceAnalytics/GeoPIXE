;
;  BMP read Image plugin routine
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

pro bmp_read_image_plugin, p, i, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = 'BMP read Plugin'				; return the menu title for this plugin
	return
endif

image_save_undo, p, i						; this will save image in undo buffer
											; this routine is part of GeoPIXE

;...............................................................................................
;
; Now comes your code here. Use the image data "(*pimg)[ix,iy,iel]" as source,
; and form a new image called "img". The example below will just do a simple smooth:
; Make use of these parameters from the image structure.

F = dialog_pickfile(file='C:\Software\Data\Rio\Weipa\weipa1bmp_FeK.bmp', $
					filter='*.bmp', /multiple)
if F[0] eq '' then return

r = query_bmp(F[0],info)
if r eq 0 then return

n_el = n_elements(F)
sx = info.dimensions[0]
sy = info.dimensions[1]
image = fltarr(sx,sy,n_el)
el = strarr(n_el)

for n=0,n_el-1 do begin
	r = read_bmp(F[n])
	image[*,*,n] = r

	j = locate_last('_',F[n])
	k = locate_last('.',F[n])
	el[n] = strmid(F[n],j+1,k-j-1)
endfor

(*p).n_el = n_el
(*p).xsize = sx
(*p).ysize = sy

ptr_free, (*p).image
(*p).image = ptr_new( image, /no_copy)		; pointer to the image arrays for all elements

ptr_free, (*p).el
(*p).el = ptr_new( el, /no_copy)

(*p).cal.poly[1] = 1.0
(*p).cal.poly[0] = 0.0
(*p).cal.units = 'channel'
(*p).charge = 0.0

; A simple history record for this plugin.
; No need to add the plugin name here; this is done by GeoPIXE.

history = 'BMP read'

;...............................................................................................

return
end

