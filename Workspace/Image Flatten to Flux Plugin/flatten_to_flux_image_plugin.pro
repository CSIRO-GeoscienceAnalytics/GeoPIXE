;
;  subtract Image plugin routine
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

pro flatten_to_flux_image_plugin, p, ii, title=title, history=history

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = '* Flatten to Flux Image Plugin'		; return the menu title for this plugin
	return											; "*" means it applies to all images
endif

;image_save_undo, p, i						; this will save image in undo buffer
;											; this routine is part of GeoPIXE

;...............................................................................................
;
; Now comes your code here. Use the image data "(*pimg)[ix,iy,iel]" as source,
; and form a new image called "img". The example below will just do a simple smooth:
; Make use of these parameters from the image structure.

pimg = (*p).image							; pointer to the image arrays for all elements
opt = (*p).options							; display ranges, etc.
nx = (*p).xsize								; X size of image in pixels
ny = (*p).ysize								; Y size of image in pixels
n_el = (*p).n_el							; number of elements/images
el_name = *(*p).el							; name of images
ca = (*p).cal.poly[1]						; energy calibration energy per channel
cb = (*p).cal.poly[0]						; energy calibration offset
cunits = (*p).cal.units						; energy calibration units string
charge = (*p).charge						; integrated charge for images (uC)

if (*p).has_flux eq 0 then goto, bad_element

base = 0.05									; ignore pixels where Flux is less than
											; this fraction of average, or larger than
											; the inverse of this times average
											; 'flux_flatten' has a hard minimum of 0.001

drop = ['Do not modify pixels outside norm range','Zero pixels outside norm range','Replace pixels outside norm range with interpolate']
help_drop = 'Select what to do with invalid flux/charge pixels (outside of range [base,huge], where base is the fraction of average): (i) Do not modify them, (ii) Zero them, ' + $
			'or (iii) Replace them with a local interpolated value.'
text = ['Base fraction','Number of good neighbours']
initial_text = ['0.05','2']
help_text = ['Fraction of average to accept as a valid flux/charge. Outside of range [base,huge] image will not be normalized (see Droplist options).', $
		'Minimum number of good pixel neighbours to use to generate a local average to recover a "zero" pixel (if "interpolate" option is used).']
Help_default = 'Set what to do with pixels with poor flux/charge, which depart signicantly from the average. Set "base" as the minimum fraction of the average to accept. Accept in the range [base,huge].'
r = options_popup( title='Normalize to Flux Options', text=text, initial_text=initial_text, help_text=help_text, $
			help_default=help_default, drop=drop, help_drop=help_drop, min_xsize=400, error=error)
if error then return
	
base = float2(r.text[0])
neighbours = long2(r.text[1])
mode = r.drop[0]
nxy = long(nx) * long(ny)

print,'Flatten to Flux; base = ',base
scale =  flux_flatten( *(*p).flux, base=base, qbad=qb)
nqb = n_elements(qb)

for i=0,n_el-1 do begin
	(*pimg)[*,*,i] = (*pimg)[*,*,i] * scale
endfor
if (*p).has_flux then begin
	*(*p).flux = *(*p).flux * scale
endif

if (mode eq 1) and (qb[0] ne -1) then begin					; zero dodgy flux pixels
	for i=0,n_el-1 do begin
		(*pimg)[qb + i*nxy] = 0.0
	endfor
	if (*p).has_flux then begin
		(*(*p).flux)[qb] = 0.0
	endif
	s = str_tidy(nqb)
	history = 'Flatten to "Flux" plugin, base='+str_tidy(base)+' (zero '+s+' bad pixels)'

endif else if (mode eq 2) and (qb[0] ne -1) then begin		; interpolate dodgy flux pixels
	print,'Interpolate zeroes, for pixels = ',nqb
	image_correct_zero, qb, pimg, (*p).flux, neighbours=neighbours, remain=qz, /verbose
	if qz[0] ne -1 then begin
		print,'Second pass, interpolate zeroes, for pixels = ',n_elements(qz)
		image_correct_zero, qz, pimg, (*p).flux, neighbours=neighbours, remain=qz, /verbose
	endif
	if qz[0] ne -1 then begin
		print,'Third pass, interpolate zeroes, for pixels = ',n_elements(qz)
		image_correct_zero, qz, pimg, (*p).flux, neighbours=neighbours, remain=qz, /verbose
	endif
	nqz = n_elements(qz)
	print,'Zeroes remain = ',nqz
	s = str_tidy(nqb)
	s2 = str_tidy(nqz)
	history = 'Flatten to "Flux" plugin, base='+str_tidy(base)+' (interpolate '+s+' bad pixels, '+s2+' remain)'

endif else begin
	s = str_tidy(nqb)
	history = 'Flatten to "Flux" plugin, base='+str_tidy(base)+' ('+s+' bad pixels not changed)'
endelse

; A simple history record for this plugin.
; No need to add the plugin name here; this is done by GeoPIXE.

done:
	return

;...............................................................................................

bad_element:
	warning,'flatten_to_flux_image_plugin','Could not find "Flux".'
	return
end

