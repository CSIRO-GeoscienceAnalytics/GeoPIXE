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

pro video_play_image_plugin, p, ii, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = 'Video Play Plugin'				; return the menu title for this plugin
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
select = (*(*p).el)[ii]						; element name

path = fix_path( extract_path((*p).file))
name = strip_file_ext( strip_path((*p).file))+'-'+select

files = ['664-SbL-corner-top-left.dai', '664-SbL-top.dai', '664-SbL-corner-top-right.dai', '664-SbL-right.dai', $
		'664-SbL-corner-bot-right.dai', '664-SbL-bot.dai', '664-SbL-corner-bot-left.dai', '664-SbL-left.dai']

border = {origin: {x:188, y:700}, size: {x:1496, y:1132}}
norm =   {origin: {x:1190, y:730}, size: {x:157, y:127}}

fps = 5
kernel = gaussian_kernel(2.5)

xl = border.origin.x
xh = border.origin.x + border.size.x-1
yl = border.origin.y
yh = border.origin.y + border.size.y-1

oVid = IDLffVideoWrite( path + name+'.mp4', FORMAT='mp4')
vidStream = oVid.AddVideoStream( border.size.x/2, border.size.y/2, fps)
frame = bytarr( 3, border.size.x/2, border.size.y/2)

for i=0,n_elements(files)-1 do begin
	p1 = read_geopixe_image( path+files[i], error=err)
	if err then begin
		warning,'','Bad image file read.'
		goto, done
	endif
	charge = (*p1).charge													; integrated charge for images (uC)
	charge_per_pixel = charge / (float((*p1).xsize)*float((*p1).ysize))
	pimg = (*p1).image

	j = (where( *(*p1).el eq select, nq))[0]
	sum = total( (*(*p1).image)[norm.origin.x:norm.origin.x+norm.size.x-1, norm.origin.y:norm.origin.y+norm.size.y-1, j]) / charge_per_pixel
	if i eq 0 then sum0=sum
	f = sum0/sum
	print,i,f,'  ',files[i]

	img0 = convol( f * (*pimg)[xl:xh,yl:yh, j], kernel, /edge_truncate)

	img0 = smart_congrid( img0, border.size.x/2, border.size.y/2)

	build_image_scale, (*(*p).options)[ii], low, high, image=img0, output=img, root=0

;	b = bytscl( img, top=99, min=low, max=high) + 16B
	b = bytscl( img, min=low, max=high)
	for k=0,2 do frame[k,*,*] = b

	for k=0,4 do r = oVid.Put(vidStream, frame)

	free_images, p1
endfor


; A simple history record for this plugin.
; No need to add the plugin name here; this is done by GeoPIXE.

;history = 'FFT Play of plane ' + str_tidy(i)

;...............................................................................................

done:
	free_images, p1
	obj_destroy, oVid
	return
end

