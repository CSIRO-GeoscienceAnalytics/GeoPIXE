pro default_plot, thick, athick, csize, cthick, $
							thick_scale=scale, aspect=aspect

; inputs:
;	thick_scale	optional scaling for all thicknesses
;
; outputs:
;	thick		line thickness
;	athick		axes thickness
;	csize		char size
;	cthick		char thick
;
;	aspect		aspect ratio = distance X/Y for equal norm values

common c_metafile_1, scale_wmf
if n_elements(scale_wmf) lt 1 then scale_wmf=1.0
common c_zbuffer_1, scale_z
if n_elements(scale_z) lt 1 then scale_z=1.0
if n_elements(scale) lt 1 then scale = 1.0

if (!d.name eq 'PRINTER') or (!d.name eq 'COLOUR_PRINTER') then begin
	thick = 6.0 * scale
	cthick = 8.0 * scale
	athick = 8.0 * scale
	csize = pcharsize(1.0)
endif else if (!d.name eq 'PS') then begin
	thick = 2.0 * scale
	cthick = 1.5 * scale
	athick = 2.0 * scale
	csize = pcharsize(1.0)
endif else if (!d.name eq 'CGM') then begin
	thick = 1.4 * scale
	cthick = 1.2 * scale
	athick = 1.5 * scale
	csize = pcharsize(1.0)
endif else if (!d.name eq 'METAFILE') then begin
	thick = 1.0 * scale * scale_wmf
	cthick = 1.0 * scale * scale_wmf
	athick = 1.0 * scale * scale_wmf
	csize = pcharsize(1.0)
endif else if (!d.name eq 'Z') then begin
	thick = 1.5 * scale * scale_Z
	cthick = 1.0 * scale * scale_Z
	athick = 1.0 * scale * scale_Z
	csize = pcharsize(1.0)
endif else begin
	thick = 1.6 * scale
	cthick = 1.1 * scale
	athick = 1.1 * scale
	csize = pcharsize(1.0)
endelse

aspect = float(!d.x_size) / float(!d.y_size)
return
end
