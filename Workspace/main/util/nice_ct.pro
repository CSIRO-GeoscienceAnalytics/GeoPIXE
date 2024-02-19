	pro nice_ct, bottom=bottom, ncolors=ncolors
;
;	Gets colours (128) from Browse_colormap.dat, which is created by
;	save_colours.pro
;
	if n_elements(bottom) lt 1 then bottom=0
	if n_elements(ncolors) lt 1 then ncolors=256

	common colors, r_orig, g_orig, b_orig, $
			r_curr, g_curr, b_curr
;
	n_map_colours = 128
	openr,1,'Browse_colormap.dat'
	readf,1,n_map_colours
;	print,'	Number of colour map colours set to ', n_map_colours
	r1 = bytarr(n_map_colours)
	g1 = bytarr(n_map_colours)
	b1 = bytarr(n_map_colours)
;
	rb = 0B
	gb = 0B
	bb = 0B
	for i=0L,n_map_colours-1 do begin
	    readf,1, rb,gb,bb
	    r1[i] = rb
	    g1[i] = gb
	    b1[i] = bb
	endfor
	close,1
;
	r = congrid(r1[15:*],ncolors-1)
	g = congrid(g1[15:*],ncolors-1)
	b = congrid(b1[15:*],ncolors-1)

	r_curr = r_orig
	g_curr = g_orig
	b_curr = b_orig
	r_curr[bottom:bottom] = r1[0]
	g_curr[bottom:bottom] = g1[0]
	b_curr[bottom:bottom] = b1[0]
	r_curr[bottom+1:bottom+ncolors-1] = r
	g_curr[bottom+1:bottom+ncolors-1] = g
	b_curr[bottom+1:bottom+ncolors-1] = b

	tvlct, r_curr,g_curr,b_curr
	return
	end
