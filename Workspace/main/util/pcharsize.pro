function pcharsize, scale

common c_metafile_1, scale_wmf
if n_elements(scale_wmf) lt 1 then scale_wmf=1.0

	prt_factor = 0.77	; to correct printer plots for correct character size
;	ps_factor = 0.52	; to correct printer plots for correct character size
	ps_factor = 0.77	; to correct printer plots for correct character size
	cgm_factor = 0.45	; to correct CGM plots for correct character size
;	wmf_factor = 0.69	; to correct WMF plots for correct character size
	wmf_factor = 1.5	; to correct WMF plots for correct character size

	if n_elements(scale) eq 0 then scale = 1.0

	charsize = scale * !p.charsize

	if (!d.name eq 'PRINTER') or (!d.name eq 'COLOUR_PRINTER') then begin
		charsize = charsize * prt_factor
	endif else if (!d.name eq 'PS') then begin
		charsize = charsize * ps_factor
	endif else if (!d.name eq 'CGM')  then begin
		charsize = charsize * cgm_factor
	endif else if (!d.name eq 'METAFILE')  then begin
		charsize = charsize * wmf_factor * scale_wmf
	endif else if (!d.name eq 'Z')  then begin
		charsize = charsize * float(!d.x_size) / 1100.
	endif else begin
		charsize = charsize * float(!d.x_size) / 940.
	endelse

	return, charsize
end
