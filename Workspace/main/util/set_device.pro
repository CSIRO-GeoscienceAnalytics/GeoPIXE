pro set_device, devi, portrait=portrait, landscape=landscape, aspect=aspect, $
			file=file, white=white, small=small, square=square, error=error, true=true

common c_metafile_1, scale_wmf
if n_elements(scale_wmf) lt 1 then scale_wmf=1.0
common c_zbuffer_1, scale_z
if n_elements(scale_z) lt 1 then scale_z=1.0

if n_elements(portrait) lt 1 then portrait=0
if n_elements(landscape) lt 1 then landscape=0
if n_elements(white) lt 1 then white=0
if n_elements(small) lt 1 then small=0
if n_elements(square) lt 1 then square=0
if n_elements(true) lt 1 then true=0
if n_elements(aspect) lt 1 then aspect = landscape ? 0.7 : 1.3
dev = strupcase(devi)

tvlct, r,g,b, /get

if (dev ne 'WIN') and (dev ne 'X')  then begin		; and (dev ne 'Z')
	set_plot, dev, /copy
endif else begin
	set_plot, dev
endelse
on_ioerror, bad_file

if (!d.name eq 'PRINTER') or (!d.name eq 'COLOUR_PRINTER') then begin

	device, /index_color
	if portrait then device, /portrait
	if landscape then device, /landscape

endif else if (!d.name eq 'PS') then begin			; assumed to be colour EPS

	if n_elements(file) lt 1 then file='plot.eps'
	device, /encapsulated, preview=2, file=file

	if landscape then begin
		if small then begin
			device, /color, bits_per_pixel=8, $			; A5
			    /portrait, xsize=16.0,xoffset=0.0,ysize=11.0,yoffset=0.0, $
			    pre_xsize=16.0, pre_ysize=11.0
		endif else begin
			device, /color, bits_per_pixel=8, $			; A4
			    /landscape,xsize=24.6,xoffset=0.0,ysize=16.0,yoffset=24.6, $
			    pre_xsize=12.3, pre_ysize=8.0
		endelse

;		device, /color, bits_per_pixel=8, $			; 35 mm slide
;		    /landscape,xsize=27.94,xoffset=0.0,ysize=18.62,yoffset=27.94
	endif else begin
		yh = square ? 16.0 : 24.6
		device, /color, bits_per_pixel=8, $			; A4
		    /portrait,xsize=16.0,xoffset=0.0,ysize=yh,yoffset=0.0, $
			   pre_xsize=8.0, pre_ysize=12.3

;		device, /color, bits_per_pixel=8, $			; 35 mm slide
;		    /portrait,xsize=18.62,xoffset=0.0,ysize=27.94,yoffset=0.0
	endelse

;	tvlct, r,g,b
;	loadct, 5, bottom=16, ncolors=100
;	load_spec_colours

endif else if (!d.name eq 'CGM') then begin

	if n_elements(file) lt 1 then file='plot.cgm'
	device, file=file, colors=256

endif else if (!d.name eq 'Z') then begin

	asp = ((aspect > 0.1) < 10.0)
	xsize = 15000 / sqrt(asp)
	ysize = xsize * asp
	device, set_resolution=[xsize,ysize]
	if true then begin
		device, set_pixel_depth=24
	endif else begin
		device, set_pixel_depth=8
	endelse
	device, decomposed=0
	scale_z = ((10. / sqrt(asp)) < 25.) > 10.

endif else if (!d.name eq 'METAFILE') then begin

	if n_elements(file) lt 1 then file='plot.wmf'
	device, xsize=2000, ysize=2000, file=file
	aspect = float(!d.y_size) / float(!d.x_size)
	xsize = 15000 / !d.x_px_cm
	ysize = aspect*xsize
;	scale_wmf = 10000. / 707.
	scale_wmf = 11.
	device, xsize=xsize, ysize=ysize
endif

if white or (!d.name eq 'PRINTER') then begin
	!p.color = spec_colour('black')
	!p.background = spec_colour('white')
endif else begin
	!p.color = spec_colour('white')
	!p.background = spec_colour('black')
endelse
error = 0
return

bad_file:
	warning,'Error opening file: '+file
	error = 1
	return
end
