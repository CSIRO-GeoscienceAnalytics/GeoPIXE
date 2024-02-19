pro histobox, i, d, horizontal=horizontal, vertical=vertical, margin=margin, no_trim=no_trim, red=red

if n_elements(vertical) lt 1 then vertical = 0
if n_elements(horizontal) lt 1 then horizontal = 0
if horizontal eq 0 then vertical=1
if n_elements(margin) lt 1 then margin = 0.02
if n_elements(no_trim) lt 1 then no_trim = 0
if n_elements(red) lt 1 then red = 0

h = 0.5-margin
if horizontal then begin
	h = (0.5-margin) / ((!d.y_size * !y.s[1] * 0.98) < 1.0)		; must be a pixel at least
	y = [i-h,i-h,i+h,i+h,i-h] + 0.5
	x = [0.,d,d,0.,0.]
endif else begin
	h = (0.5-margin) / ((!d.x_size * !x.s[1] * 0.98) < 1.0)		; must be a pixel at least
	x = [i-h,i+h,i+h,i-h,i-h] + 0.5
	y = [0.,0.,d,d,0.]
endelse
if red then begin
	green = 'red'
	dgreen = 'red'
	lgrey = 'd.grey'
endif else begin
	green = 'green'
	dgreen = 'd.green'
	lgrey = 'l.grey'
endelse
polyfill, x,y, color=spec_colour(green)
if no_trim eq 0 then begin
	oplot, x[0:2],y[0:2], color=spec_colour(dgreen), thick=1
	oplot, x[2:4],y[2:4], color=spec_colour(lgrey), thick=1
endif
return
end

;---------------------------------------------------------------------------------------------

pro histogram_plot, data, x=xin, labels=labels, title=title, mdata=mdata, $
			vertical=vertical, horizontal=horizontal, credits=credits

; Histogram plot (filled), vertical or horizontal
; 
; 	data		data for green histogram bars
;	mdata		max data for red histogram bars (if present)
; 	x			x coords for bars (in /vert), y coords in /horiz
; 	labels		labels for each histogram bin
; 	/vertical	vertical histogram bars
; 	/horizontal	horizontal format histogram
;	credits		norm fraction of width for title labels (/horizontal)
;				fraction of height in /vertical

;startupp,/colours
;window,0,xsize=500,ysize=250
if n_elements(vertical) lt 1 then vertical = 0
if n_elements(horizontal) lt 1 then horizontal = 0
if horizontal eq 0 then vertical=1
if n_elements(data) lt 1 then data = [4.3,9.8,13.4,10.2,1.3,0.7,3.4,34.]
if n_elements(labels) lt 1 then labels = ['Image Display','Multi Display','Spectra Display','DA Update', $
		'Spectra Update','Maia Update','DA Buffers','Spectra Buffers']
if n_elements(title) lt 1 then title = '% Time / Buffer Fill'
		
!p.color = spec_colour('white')
!p.background = spec_colour('black')
!p.title = ''
!x.title = ''
!y.title = ''
csize = 1.

nd = n_elements(data)
xtop = 1.1*max(data) > 20.
if n_elements(mdata) eq nd then xtop = xtop > 1.1*max(mdata)

x1 = indgen(nd+1)
if n_elements(xin) gt 0 then x1=[xin,max(xin)+1]
dx = x1 - shift(x1,1)
dx = 0.5*min(dx[1:*])
x = fltarr(2*nd+3)
y = fltarr(2*nd+3)
if horizontal then begin
	if n_elements(credits) lt 1 then credits = 0.29
	margin = 0.3
	pos = [credits,0.09,0.98,0.97]
	plot, [0,0],[0,0], /nodata, xrange=[0.,xtop],yrange=[x1[0]-margin,x1[nd]+margin], $
		xstyle=1, ystyle=1, yticks=nd, ytickv=findgen(nd+1), ytickname=replicate(' ',nd+1), $
		position=pos, charsize=csize, charthick=1.0, yticklen=0.0001
endif else begin
	if n_elements(credits) lt 1 then credits = 0.09
	margin = max(x1)*0.02
	pos = [0.14,credits,0.97,0.97]
;	plot, [0,0],[0,0], /nodata, yrange=[0.,xtop],xrange=[xorg-margin,nd+margin+xorg], $
;		xstyle=1, ystyle=1, xticks=(nd<59), xtickv=findgen((nd+1)<60), xtickname=replicate(' ',(nd+1)<60), $
;		position=pos, charsize=csize, charthick=1.0, xticklen=0.0001
	plot, [0,0],[0,0], /nodata, yrange=[0.,xtop],xrange=[x1[0]-margin,x1[nd]+margin]-dx, $
		xstyle=1, ystyle=1, position=pos, charsize=csize, charthick=1.0, xticklen=0.0001
endelse

j = 0
x[j] = x1[0]
y[j] = 0.
j = j+1
for i=0L,nd-1 do begin
	x[j] = x1[i]
	y[j] = data[i]
	x[j+1] = x1[i+1]
	y[j+1] = data[i]
	j = j+2
endfor
x[j] = x1[nd]
y[j] = 0.
x[j+1] = x[0]
y[j+1] = y[0]
if nd gt 100 then no_trim=1 else no_trim=0

if horizontal then begin
;	polyfill, y,x, color=spec_colour('green')
;	oplot, y,x, color=spec_colour('d.green'), thick=1
	if n_elements(mdata) gt 0 then begin
		for i=0L,nd-1 do begin
			histobox, x1[i], mdata[i], /horizontal, margin=0.015, no_trim=no_trim, /red
		endfor 
	endif
	for i=0L,nd-1 do begin
		histobox, x1[i], data[i], /horizontal, margin=0.015, no_trim=no_trim
	endfor 
endif else begin
;	polyfill, x,y, color=spec_colour('green')
;	oplot, x,y, color=spec_colour('d.green'), thick=1
	if n_elements(mdata) gt 0 then begin
		for i=0L,nd-1 do begin
			histobox, x1[i]-dx, mdata[i], /vertical, margin=0.01, no_trim=no_trim, /red
		endfor 
	endif
	for i=0L,nd-1 do begin
		histobox, x1[i]-dx, data[i], /vertical, margin=0.01, no_trim=no_trim
	endfor 
endelse
if horizontal then begin
;	plot, [0,0],[0,0], /nodata, /noerase, xrange=[0.,xtop],yrange=[x1[0]-margin,x1[nd]+margin], $
;		xstyle=1, ystyle=1, yticks=nd, ytickv=findgen(nd+1), ytickname=replicate(' ',nd+1), $
;		position=pos, charsize=csize, charthick=1.0, yticklen=0.01
	plot, [0,0],[0,0], /nodata, /noerase, xrange=[0.,xtop],yrange=[x1[0]-margin,x1[nd]+margin], $
		xstyle=1, ystyle=1, ytickv=findgen(nd)+0.5, ytickname=[labels,' '], yticks=nd, $
		position=pos, charsize=csize, charthick=1.0, yticklen=0.0001
	xyouts, pos[2]-0.03,pos[1]+0.05, /norm, title, charsize=1.1*csize, align=1
endif else begin
	!y.title = title
;	plot, [0,0],[0,0], /nodata, /noerase, yrange=[0.,xtop],xrange=[x1[0]-margin,x1[nd]+margin], $
;		xstyle=1, ystyle=1, xticks=(nd<59), xtickv=findgen((nd+1)<60), xtickname=replicate(' ',(nd+1)<60), $
;		position=pos, charsize=csize, charthick=1.0
;	plot, [0,0],[0,0], /nodata, /noerase, yrange=[0.,xtop],xrange=[x1[0]-margin,x1[nd]+margin], $
;		xstyle=1, ystyle=1, xtickv=findgen((nd+1)<60)+0.5, xtickname=[str_tidy(indgen((nd)<59)),' '], xticks=nd<59, $
;		position=pos, charsize=csize, charthick=1.0, xticklen=0.0001
	plot, [0,0],[0,0], /nodata, /noerase, yrange=[0.,xtop],xrange=[x1[0]-margin,x1[nd]+margin]-dx, $
		xstyle=1, ystyle=1, position=pos, charsize=csize, charthick=1.0
	plot, [0,0],[0,0], /nodata, /noerase, yrange=[0.,xtop],xrange=[x1[0]-margin,x1[nd]+margin]-dx, $
		xstyle=1, ystyle=1, position=pos, charsize=csize, charthick=1.0, xticklen=0.0001
endelse
return
end



