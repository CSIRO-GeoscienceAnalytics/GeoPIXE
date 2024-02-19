	function pseudo_match, img, r,g,b, bottom=bottom, ncolors=ncolors, true=true
;
;	Take 24-bit 'img' and map it onto the color-table 'r,g,b'.
;	Return the img in pseudo color.
;
;	If no color table is specified, use current table.
;	Use only the colour table part from bottom to bottom+ncolors-1.

	if n_elements(true) eq 0 then true = 3
	if n_elements(bottom) eq 0 then bottom = 0
	if n_elements(ncolors) eq 0 then ncolors = n_elements(r)-bottom
	if n_params(0) eq 1 then tvlct, r,g,b, /get

	img2 = color_quan( img, true, red,green,blue, colors=ncolors)

	p = bytarr(ncolors)
	for i=0L,ncolors-1 do begin
		dr = red[i] - r[bottom:bottom+ncolors-1]
		dg = green[i] - g[bottom:bottom+ncolors-1]
		db = blue[i] - b[bottom:bottom+ncolors-1]

		q = sort( dr*dr + dg*dg + db*db)
		p[i] = bottom + q[0]
	endfor

	return, p[img2]
	end
