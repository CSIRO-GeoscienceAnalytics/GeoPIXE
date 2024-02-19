pro plot_contour_image, z,x,y, xtitle=xtitle, ytitle=ytitle, title=title, $
			multi=multi, nlevels=nlevels

if n_elements(xtitle) gt 0 then !x.title=xtitle
if n_elements(ytitle) gt 0 then !y.title=ytitle
if n_elements(title) gt 0 then !p.title=title
if n_elements(nlevels) lt 1 then nlevels=12
if n_elements(multi) gt 0 then begin
	multi_mode = 1
	!p.multi = [0,multi[0],multi[1]]
endif else multi_mode=0
nx = n_elements(x)
ny = n_elements(y)

erase_first = 0
if !p.multi[0] eq 0 then erase_first=1

zinc = one_sig_figure( (max(z)-min(z))/float(nlevels>2) )
zlow = round( min(z) / zinc) * zinc
zlevels = zlow + findgen(nlevels+2)*zinc

contour, z, x,y, /follow, levels=zlevels, xstyle=1,ystyle=1, noerase=1-erase_first

PX = !X.WINDOW * !D.X_VSIZE
PY = !Y.WINDOW * !D.Y_VSIZE
; Desired size of image in pixels.
SX = PX[1] - PX[0] + 1
SY = PY[1] - PY[0] + 1

wx = sx/nx
wy = sy/ny
cz = congrid(z, sx+wx,sy+wy, /center, /interp)
cz = cz[wx/2:sx+wx/2,wy/2:sy+wy/2]

b = bytscl( cz, top=99, /NaN) + 16B
tv, b, px[0], py[0]

if multi_mode then !p.multi[0] = 0

tx = congrid(x,sx,1,/interp)
ty = congrid(y,sy,1,/interp)
cz2 = congrid(z, sx,sy, /interp)
contour, cz2, tx,ty, levels=zlevels, /follow, xstyle=1,ystyle=1, noerase=erase_first

if multi_mode then !p.multi[0] = !p.multi[1]*!p.multi[2]-1
return
end
