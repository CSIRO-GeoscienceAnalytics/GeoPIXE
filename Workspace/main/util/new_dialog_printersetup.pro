function new_dialog_printersetup, portrait=portrait, landscape=landscape, $
				white=white, again=again

; Fix up the device output size correctly

if n_elements(portrait) eq 0 then portrait=0
if n_elements(landscape) eq 0 then landscape=0
if n_elements(white) eq 0 then white=1
if n_elements(again) eq 0 then again=0
if landscape eq 0 then portrait=1

xwin = !d.name

set_device, 'PRINTER', white=white, portrait=portrait, landscape=landscape

r = 1
if again eq 0 then begin
	r = dialog_printersetup()
	if (r eq 0) then begin
		set_plot, xwin
		return, 0
	endif
endif

help, /dev, output=str

land = 0
pos = strpos(strupcase(str), 'ORIENTATION')
ptr = where(pos ne -1, count)
if (count gt 0) then begin
    pos = strpos(strupcase(str(ptr(0))), 'LANDSCAPE')
    land = (pos(0) ne -1)
endif

scale = 1.
pos = strpos(strupcase(str), 'SCALE')
ptr = where(pos ne -1, count)
if (count gt 0) then begin
    pos = strpos(str(ptr(0)), ':')
    if (pos(0) ne -1) then scale = float(strmid(str(ptr(0)), pos(0)+1, 20))
endif

device, get_page_size=dsize
xsize = dsize[0] / (!d.x_px_cm) - 0.6
ysize = dsize[1] / (!d.y_px_cm) - 0.6

if land then begin
	device, /landscape, xsize=xsize, ysize=ysize, xoffset=0.3,yoffset=0.3
endif else begin
	device, /portrait, xsize=xsize, ysize=ysize, xoffset=0.3,yoffset=0.3
endelse

return, r
END
