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

pro subtract_image_plugin, p, ii, title=title, history=history

COMPILE_OPT STRICTARR
common c_subtract_image_plugin, rlast

if arg_present(title) then begin
	title = '* Subtract Image Plugin'		; return the menu title for this plugin
	return
endif

if n_elements(rlast) ge 1 then begin
	initial_text = [rlast.text[0],rlast.text[1],rlast.text[2],rlast.text[3]]
endif else begin
	initial_text = ['','','0.1','0']
endelse

text = ['Src map name', 'Subtract map name', 'Subtract ratio', 'Dest map index']
help_text = ['Enter the element name for the Source image.', 'Enter the element name for the image to subtract.', $
			'Enter the scaling factor to apply to the Subtract image before subtraction from the Source.','Enter the Index for the Dest image plane.']
Help_default = 'Enter the Source, Subtract image element names and the Destination image plane index.'
r = options_popup( title='Subtract images', text=text, initial_text=initial_text, help_text=help_text, $
		help_default=help_default, error=error)
if error then return
rlast = r

src = r.text[0]
sub = r.text[1]
f = float2( r.text[2])
dest = fix2( r.text[3])

;image_save_undo, p, i						; this will save image in undo buffer
;											; this routine is part of GeoPIXE

;...............................................................................................
;
; Now comes your code here. Use the image data "(*pimg)[ix,iy,iel]" as source,
; and form a new image called "img". The example below will just do a simple smooth:
; Make use of these parameters from the image structure.

pimg = (*p).image							; pointer to the image arrays for all elements
perr = (*p).error							; pointer to the varuance arrays for all elements
opt = (*p).options							; display ranges, etc.
sx = (*p).xsize								; X size of image in pixels
sy = (*p).ysize								; Y size of image in pixels
ca = (*p).cal.poly[1]						; energy calibration energy per channel
cb = (*p).cal.poly[0]						; energy calibration offset
cunits = (*p).cal.units						; energy calibration units string
charge = (*p).charge						; integrated charge for images (uC)

; Test whether this is an element or XANES stack of images, and set XANES true if so,
; and n_el=number of planes, el_name-names of planes. el_xanes=element used for XANES.

xanes_stack_test, p, xanes, n_el, el_name, el_xanes

; An example of subtracting image with weighting.
; Result is saved back in image #0 (replacing "Back").

q = where( src eq el_name, nq)
if (nq eq 0) then return
i1 = q[0]
q = where( sub eq el_name, nq)
if (nq eq 0) then return
i2 = q[0]
print,'Subtract ',el_name[i2],' (scaled by ',f,') from ',el_name[i1]
print,'   put result in index: ',dest,', which now has: ',el_name[dest]

(*pimg)[*,*,dest] = (*pimg)[*,*,i1] - f*(*pimg)[*,*,i2]
(*perr)[*,*,dest] = (*perr)[*,*,i1] + f*(*perr)[*,*,i2]
(*opt)[dest].max = max( (*pimg)[*,*,dest])

s = el_name[i1]+'-'+el_name[i2]
if xanes eq 0 then (*(*p).el)[dest] = s

; A simple history record for this plugin.
; No need to add the plugin name here; this is done by GeoPIXE.

history = 'subtract "'+s+'" * '+strtrim(strip_trail_zero(string(f)),2)+' plugin'

;...............................................................................................

return
end

