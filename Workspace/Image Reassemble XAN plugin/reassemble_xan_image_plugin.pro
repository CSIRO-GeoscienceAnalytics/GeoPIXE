;
;  reassemble_xan Image plugin routine
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
;  "SAVE, /routines, filename='reassemble_xan_image_plugin.sav'"
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

pro reassemble_xan_image_plugin, p, i, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Reassemble XAN Plugin'		; return the menu title for this plugin
	return
endif

;image_save_undo, p, i						; this will save image in undo buffer
											; this routine is part of GeoPIXE

;...............................................................................................
;
; Now comes your code here. Use the image data "(*pimg)[ix,iy,iel]" as source,
; and form a new image called "img". The example below will just do a simple smooth:
; Make use of these parameters from the image structure.

; Determine if this is an image of elements or a XANES stack
;	xanes		1 if XANES stack, 0 for element images
;	n_el		# elements (XANES=0), # stack plane energies (XANES=1)
;	el			elements or xanes energy labels
;	el_xanes	element whose edge we have scanned for XANES

xanes_stack_test, p, xanes, n_el, el, el_xanes

if xanes eq 0 then return
if n_el lt 2 then return

nx = (*p).xsize								; X size of image in pixels
ny = (*p).ysize								; Y size of image in pixels
nz = (*p).zsize								; Z (E) size in planes
pz = *(*p).pz_coords						; energies
charge = (*p).charge						; integrated charge for images (uC)
nxe = (nx+1)/2								; size of error map
nye = (ny+1)/2

;...............................................................................................

file2 = file_requester(/read, filter=['*.xan','*.xan.*'], title='Select XANES stack to re-assemble with current')

p2 = read_geopixe_image( file2, error=err)
if err then  return

nx2 = (*p2).xsize							; X size of image in pixels
ny2 = (*p2).ysize							; Y size of image in pixels
nz2 = (*p2).zsize							; Z (E) size in planes
pz2 = *(*p2).pz_coords						; energies
charge2 = (*p2).charge						; integrated charge for images (uC)

if (*p2).ixanes ne (*p).ixanes then return
if (nx ne nx2) or (ny ne ny2) then return	

nz3 = nz + nz2
if (*p).has_flux and (*p2).has_flux then begin
	flux = fltarr(nx,ny,nz3)
	flux[*,*,0:nz-1] = *(*p).flux
	flux[*,*,nz:nz3-1] = *(*p2).flux
	*(*p).flux = flux
endif
if (*p).has_dwell and (*p2).has_dwell then begin
	*(*p).dwell_map = *(*p).dwell_map + *(*p2).dwell_map
endif
if (*p).has_errors and (*p2).has_errors then begin
	errors = fltarr(nxe,nye,nz3)
	errors[*,*,0:nz-1] = *(*p).error
	errors[*,*,nz:nz3-1] = *(*p2).error
	*(*p).error = errors
endif
image = fltarr(nx,ny,nz3)
image[*,*,0:nz-1] = *(*p).image
image[*,*,nz:nz3-1] = *(*p2).image
*(*p).image = image

if ptr_valid( (*p).options) and ptr_valid( (*p2).options) then begin
	*(*p).options = [ *(*p).options, *(*p2).options] 
endif
if ptr_valid( (*p).escale) and ptr_valid( (*p2).escale) then begin
	*(*p).escale = [ *(*p).escale, *(*p2).escale] 
endif
if ptr_valid( (*p).history) and ptr_valid( (*p2).history) then begin
	*(*p).history = [ *(*p).history, *(*p2).history] 
endif

(*p).zsize = nz3
*(*p).pz_coords = [ *(*p).pz_coords, *(*p2).pz_coords]
(*p).processed = (*p).processed + (*p2).processed
(*p).valid = (*p).valid + (*p2).valid
(*p).bad_xy = (*p).bad_xy + (*p2).bad_xy
(*p).clipped = (*p).clipped + (*p2).clipped

;	Also need to merge dead-fraction, pileup maps, count-rates

(*p).stack_type = 0
free_images, p2

;	Flatten images to combined flux 3D map

image_flux_flatten_stack, p, flux

; A simple history record for this plugin.
; No need to add the plugin name here; this is done by GeoPIXE.

history = 'Re-assembled XAN with "'+strip_path(file2) + '"'

;...............................................................................................

return
end

