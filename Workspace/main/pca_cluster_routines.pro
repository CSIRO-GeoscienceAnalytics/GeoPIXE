;
;	pca_cluster_Routines, for corr.pro
;
;--------------------------------------------------------------------------------

pro Analyze_pca, pstate, error=err

; Do PCA analysis on the current image data

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'Analyze_pca',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_pca_1, old_select
err = 1

p = (*pstate).p
if ptr_good( p) eq 0 then return
pimg = (*p).image								; pointer to the image arrays for all elements
if ptr_good( pimg) eq 0 then return
perror = (*p).error								; pointer to the var arrays for all elements
if ptr_good( perror) eq 0 then return
pca = (*pstate).pca

sx = long((*p).xsize)							; X size of image in pixels
sy = long((*p).ysize)							; Y size of image in pixels
threshold = float((*pstate).threshold)/100.		; relative pixel intensities to consider
r_top = float((*pstate).average_high)/100.		; fraction of top energies to normalize to
r_bot = float((*pstate).average_low)/100.		; fraction of low energies to normalize to

xanes_stack_test, p, xanes, n_el, el, el_xanes

if (*pstate).border gt 0 then begin
	b = (*pstate).border < clip( min([sx,sy])/2 - 1, 1, 30)
	mask = bytarr(sx,sy)
	mask[*] = 1
	mask[*, 0:b-1] = 0
	mask[*, sy-b:sy-1] = 0
	mask[0:b-1, *] = 0
	mask[sx-b:sx-1, *] = 0
	q = where( mask eq 1, nq)					; q index into full image
endif else begin
	nq = sx*sy
	q = lindgen(nq)
endelse
charge_per_pixel = (*p).charge / (float(sx) * float(sy))

if ptr_valid( (*pstate).q) then begin			; if an image region is active on image, only use these pixels
	if (*(*pstate).q)[0] ne -1 then begin
		nq = n_elements( *(*pstate).q)
		q = *(*pstate).q
	endif
endif

;	Use only selected element planes ...

if xanes then begin
	qe = lindgen(n_el)
	nqe = n_el
endif else begin
	select = element_select( (*pstate).tlb, el, old_select=old_select, path=(*pstate).path, title='Select images for PCA analysis')
	qe = where( select eq 1, nqe)
	if nqe eq 0 then return
	old_select = select
endelse

; Can we deal with a restricted part of the whole data to conserve memory?

q_to_xy, q, sx, x,y								; x,y coordinates of all selected points
minx = min(x)
maxx = max(x)
miny = min(y)
maxy = max(y)
mask = bytarr(sx,sy)
mask[q] = 1
smask = mask[minx:maxx,miny:maxy]
qs = where( smask eq 1, nqs)					; qs index into subset image
if nqs eq 0 then return

qref = lonarr(sx*sy)							; cross reference from normal pixels to subset pixels
qref[*] = -1
qref[q] = qs

image = (*pimg)[minx:maxx,miny:maxy,qe]			; image subset
sxs = maxx-minx+1
sys = maxy-miny+1

q_to_xy, qs, sxs, xs,ys							; x,y coordinates of all selected points
minxe = (minx+1)/2
maxxe = (maxx+1)/2
minye = (miny+1)/2
maxye = (maxy+1)/2
sxse = maxxe-minxe+1
syse = maxye-minye+1
qse = (xs+1)/2 + syse * ((ys+1)/2) 				; qse index into subset err at half resolution

error = (*perror)[minxe:maxxe,minye:maxye,qe]	; error subset

; Smooth image data, if required

if ((*pstate).smooth ge 2) and ((maxx-minx) gt 2*(*pstate).smooth) then begin	
	for i=0,nqe-1 do begin
		image[*,*,i] = smooth( image[*,*,i], (*pstate).smooth > 2)
	endfor
endif
image = reform(temporary(image), sxs*sys, nqe) / charge_per_pixel	

if ((*pstate).smooth ge 4) and ((maxxe-minxe) gt 2*(*pstate).smooth) then begin	
	for i=0,nqe-1 do begin	
		error[*,*,i] = smooth( error[*,*,i], ((*pstate).smooth/2) > 2)
	endfor
endif
error = reform(temporary( sqrt(error>0.)), sxse*syse, nqe) / (4.*charge_per_pixel)

; What to do with variance images?
; Can pcomp handle NaN (!values.f_nan) or zero as a way to ignore element data?
	

; Scale each pixel spectrum to normalize top of XANES at high E

if (*pstate).normalize then begin
	if xanes then begin
		n_top = round(n_el * r_top) > 1
		n_bot = round(n_el * r_bot) > 1
		if float(el[qe[0]]) gt float(el[qe[nqe-1]]) then begin			; first energy is largest
			n1 = 0L
			n2 = n_top-1
			n3 = nqe-n_bot-2
			n4 = nqe-1
		endif else begin												; first energy is smallest
			n1 = nqe-n_top-2
			n2 = nqe-1
			n3 = 0L
			n4 = n_bot-1
		endelse
		
		av_bot = fltarr(nqs)
		for i=n3,n4 do begin
			av_bot[*] = av_bot[*] + image[qs,i]
		endfor
		av_bot = av_bot / n_bot											; average of bottom energies
		
		; Subtract down by av_bot
		
		for i=0,nqe-1 do begin
			image[qs,i] = image[qs,i] - av_bot
		endfor
		
		av_top = fltarr(nqs)
		for i=n1,n2 do begin
			av_top[*] = av_top[*] + image[qs,i]
		endfor
		av_top = av_top / n_top											; average of top energies
		
		; Threshold to ignore weak XANES signals
		
		thresh = threshold * max( av_top )
		q1 = where( av_top gt thresh, nq1)
		if nq1 eq 0 then goto, no_valid
		
		; Scale down by av_top
		
		for i=0,nqe-1 do begin
			image[qs[q1],i] = image[qs[q1],i] / av_top[q1]
		endfor
	
		q = q[q1]														; indices back into full images
		nq = nq1
		qs = qs[q1]
		nqs = nq1
		data = transpose(image[qs,*])
		
	endif else begin
;		ave = fltarr(nq)
;		for i=0,nqe-1 do begin
;			ave[*] = ave[*] + image[q,qe[i]]
;		endfor
;		scale = max(ave) / ave

		data = transpose(image[qs,*])
		data = standardize( temporary(data))
	endelse	
endif else begin

	data = transpose(image[qs,*])
endelse

result = pcomp( data, coefficients=coeff, eigenvalues=eigen, variances=var, /double)

*pca.pimage = image						; cut down image, selected elements only
*pca.pq = q								; index into full image
*pca.pqs = qs							; index into cut-down image
*pca.qref = qref						; cross reference from normal pixels to subset pixel index
*pca.perror = error						; cut down error, selected elements only
*pca.pqse = qse							; index into cut-down err at half resolution
*pca.pqel = qe							; index into elements to use
*pca.presults = transpose(result)		; PCA results (for cut down image, selected elements only)
*pca.pcoeff = coeff
*pca.peigen = reform(eigen)
*pca.pvar = reform(var)
err = 0
return

no_valid:
	warning,'Analyze_pca','No valid pixels found.'
	return
end

;--------------------------------------------------------------------------------

pro Analyze_clusters, pstate, error=err

; Do Cluster analysis on the current image data, or PCA results

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'Analyze_clusters',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
common c_pca_1, old_select
err = 1

p = (*pstate).p
if ptr_good( p) eq 0 then return
pimg = (*p).image									; pointer to the image arrays for all elements
if ptr_good( pimg) eq 0 then return
pca = (*pstate).pca
perror = (*p).error

sx = long((*p).xsize)								; X size of image in pixels
sy = long((*p).ysize)								; Y size of image in pixels
nxy = sx*sy

n_clusters = (*pstate).class_max					; number of clusters to try

xanes_stack_test, p, xanes, n_el, el, el_xanes

if (*pstate).source eq 0 then begin					; raw data source
	if xanes then begin
		qe = lindgen(n_el)
		nqe = n_el
	endif else begin
;		select = element_select( (*pstate).tlb, el, old_select=old_select, path=(*pstate).path, title='Select images for Cluster analysis')
		qe = where( old_select eq 1, nqe)
		if nqe eq 0 then return
;		old_select = select
	endelse

	n = n_elements( (*pca.pimage)[*,0])
	n2 = n_elements( (*pca.perror)[*,0])
	data = fltarr( nqe, n)
	eweight = fltarr(nqe)							; devise element weight based on conc/err average
	for i=0,nqe-1 do begin	
		data[i,*] = (*pca.pimage)[ *pca.pqs + n*i]
		err = (*pca.perror)[ *pca.pqse + n2*i]
		r = (data[i,*] > 0.) / err
		qt = where( err lt 0.1, nqt)				; ignore zero pixels
		if nqt gt 0 then r[qt]=0.0
		eweight[i] = total( r) / nxy
	endfor
	eweight = eweight / max(eweight)
	data = standardize( data)
	print,' Element weights =',eweight
	weights = clust_wts( data, n_clusters=n_clusters, n_iterations=20, variable_wts=eweight)
	class = cluster( data, weights)
endif else begin
	data = transpose( *pca.presults)				; PCA results as source
	data = data[0:(*pstate).components-1,*]
	data = standardize( data)
	weights = clust_wts( data, n_clusters=n_clusters, n_iterations=20, variable_wts=(*pca.peigen)[0:(*pstate).components-1])
	class = cluster( data, weights)
endelse

*pca.pclass = reform(class)
(*pstate).pca.source = (*pstate).source

for i=0,n_clusters-1 do begin
	q = where( *pca.pclass eq i, nq)
	print,'Class ',i,',  # data points = ',nq
endfor
err = 0
return

no_valid:
	warning,'Analyze_clusters','No valid pixels found.'
	return
end

;-----------------------------------------------------------------

pro clear_pca_cluster_all_markers, pstate

clear_pca_cluster_spline, pstate
clear_pca_cluster_spline, pstate, /init, /zero

return
end

;-----------------------------------------------------------------
; Copy rectangle from 'from' to 'to' to clear a spline shape.
; By default, copies from '(*pstate).pix' to '(*pstate).wid2'.

pro clear_pca_cluster_spline, pstate, init=init, zero=zero, from=from, to=to

if n_elements(init) lt 1 then init=0
if n_elements(zero) lt 1 then zero=0
if n_elements(from) lt 1 then from = (*pstate).pix
if n_elements(to) lt 1 then to = (*pstate).wid2
p = (*pstate).pmark

totx = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
toty = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top

if init eq 0 then begin
	spline_pca_cluster_vertices, pstate, x,y,n
	xy_pca_cluster_to_pixel, pstate, x,y, px,py
	wset, to
	minx = clip( (min(px) - 4), 0, totx)
	miny = clip( (min(py) - 4), 0, toty)
	maxx = clip( (max(px) + 4), 0, totx)
	maxy = clip( (max(py) + 4), 0, toty)
;	print,'clear_spline: [',minx,miny, maxx-minx+1,maxy-miny+1,'] from=',from,' to=',to
	device,copy=[ minx,miny, maxx-minx+1,maxy-miny+1, minx,miny, from]
endif

if zero then begin
	(*p).x[*] = 0.0
	(*p).y[*] = 0.0
	(*p).cx[*] = 0.0
	(*p).cy[*] = 0.0
endif
(*p).present = 0
return
end

;-----------------------------------------------------------------
;
; Convert conc position 'cx,cy' to image 'x,y'
; NOTE: if formula changed here, change 'xy_pca_cluster_to_conc' too.

pro conc_pca_cluster_to_xy, pstate, cx,cy, x,y, veto=veto, save_max=save_max, $
						range=range, nozoom=nozoom, sx=w, sy=h, noclip=noclip

x = 0.0
y = 0.0
if ptr_valid( (*pstate).p) eq 0 then return
if n_elements(veto) lt 1 then veto=0
if n_elements(noclip) lt 1 then noclip=0
if n_elements(nozoom) lt 1 then nozoom=0
if n_elements(save_max) lt 1 then save_max=0
if nozoom then begin
	w = (*pstate).owidth
	h = (*pstate).oheight
endif else begin
	w = (*pstate).width
	h = (*pstate).height
endelse

if save_max then begin
	pimg = (*(*pstate).p).image
	nx = n_elements((*pimg)[*,0,0])
	ny = n_elements((*pimg)[0,*,0])
	x1 = 0.02*nx
	x2 = 0.98*nx < (nx-1)
	y1 = 0.02*ny
	y2 = 0.98*ny < (ny-1)
	if (size(cx))[0] eq 2 then begin				; ignore border pixels
		xmax = 1.05*max(cx[x1:x2,y1:y2])
		xmin = min(cx[x1:x2,y1:y2])
	endif else begin
		xmax = 1.05*max(cx)
		xmin = min(cx)
	endelse
	if (size(cy))[0] eq 2 then begin
		ymax = 1.05*max(cy[x1:x2,y1:y2])
		ymin = min(cy[x1:x2,y1:y2])
	endif else begin
		ymax = 1.05*max(cy)
		ymin = min(cy)
	endelse
	xmin = (xmin lt 0.) ? 1.05*xmin : 0.5*xmin
	ymin = (ymin lt 0.) ? 1.05*ymin : 0.5*ymin
	(*pstate).maxx = xmax
	(*pstate).maxy = ymax
	(*pstate).minx = xmin
	(*pstate).miny = ymin
endif else begin
	xmax = (*pstate).maxx
	ymax = (*pstate).maxy
	xmin = (*pstate).minx
	ymin = (*pstate).miny
endelse
if veto then begin
;	q = where(cy gt ymax*1.0E-5)
;	if q[0] ne -1 then xmax = xmax < 1.05*max(cx[q])
;	q = where(cx gt xmax*1.0E-5)
;	if q[0] ne -1 then ymax = ymax < 1.05*max(cy[q])
;	if save_max then begin
;		(*pstate).maxx = xmax
;		(*pstate).maxy = ymax
;	endif
endif

if (*pstate).logx then begin
	xmax1 = xmax
	xmax = xmax1 / 10^( 5.0 - 0.05*(*pstate).xtop)
	xmin = xmax1 / 10^( 5.0 - 0.045*(*pstate).xbottom) < xmax
	x = long( (w-1) * (alog10((cx)/xmin)/alog10(xmax/xmin) > 0.0) )
endif else begin
	a = (xmax - xmin) / 100.
	b = xmin 
	xmax = a * float((*pstate).xtop) + b
	xmin = a * float((*pstate).xbottom) + b
	x = long( (w-1) * (((cx)-xmin)/(xmax-xmin) > 0.0) )
endelse
if (*pstate).logy then begin
	ymax1 = ymax
	ymax = ymax1 / 10^( 5.0 - 0.05*(*pstate).ytop)
	ymin = ymax1 / 10^( 5.0 - 0.045*(*pstate).ybottom) < ymax
	y = long( (h-1) * (alog10((cy)/ymin)/alog10(ymax/ymin) > 0.0) )
endif else begin
	a = (ymax - ymin) / 100.
	b = ymin 
	ymax = a * float((*pstate).ytop) + b
	ymin = a * float((*pstate).ybottom) + b
	y = long( (h-1) * (((cy)-ymin)/(ymax-ymin) > 0.0) )
endelse

;if save_max then begin
;	(*pstate).minx = xmin
;	(*pstate).miny = ymin
;endif

range = {x:[xmin,xmax], y:[ymin,ymax]}

if noclip eq 0 then begin
	x = clip( x, 0, w-1)
	y = clip( y, 0, h-1)
endif

x = reform(x, n_elements(x), /overwrite)
y = reform(y, n_elements(y), /overwrite)
return
end

;-----------------------------------------------------------------
; Build list of spline handles (in pca_cluster display coords).

pro pca_cluster_handles, pstate, px,py, n, use_conc=use_conc

if n_elements(use_conc) lt 1 then use_conc=0

p = (*pstate).pmark

;  1-10 (or 1-32) are control points, 0 is centre handle

if use_conc then begin
	cx = (*p).cx
	cy = (*p).cy
	conc_pca_cluster_to_xy, pstate, cx,cy, px,py, /nozoom
endif else begin
	px = (*p).x
	py = (*p).y
endelse

n = n_elements(px)

maxx = max(px)
maxy = max(py)
if (max([maxx,maxy]) eq 0) then n=0

return
end

;-----------------------------------------------------------------
; Draw the current pca_cluster ((*pstate).pca_cluster) on the draw widget

pro draw_pca_clusters, pstate

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'Draw_pca_clusters',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

p = (*pstate).p
if ptr_good(p) eq 0 then return
if ptr_valid( (*p).image ) eq 0 then return
if ptr_good((*pstate).pca.presults) eq 0 then return

wset, (*pstate).wid2
tb = !p.background
!p.background = 16				; use image colour base as erase background
erase
!p.background = tb

xanes_stack_test, p, xanes, n_els, el, el_xanes, z_found=z_found
el = 'PC ' + strtrim(string(indgen(n_els)),2)

!p.charsize = 1.0
!p.charthick = 1.0
!p.linestyle = 0
!p.multi = 0
!p.psym = 0
!p.thick = 1.0
!p.title = ''
!x.charsize = 1.0
!x.style = 1
!x.thick = 1.0
!x.ticks = 0
!x.title = ''
!y.charsize = 1.0
!y.style = 1
!y.thick = 1.0
!y.ticks = 0
!y.title = ''
tvlct, ro,go,bo, /get
if long(ro[16])+long(go[16])+long(bo[16]) lt 384 then begin
	!p.background = spec_colour('black')
	!p.color = spec_colour('white')
endif else begin
	!p.background = spec_colour('white')
	!p.color = spec_colour('black')
endelse

b = make_pca_cluster_tvb( pstate, (*pstate).pca_cluster_x, (*pstate).pca_cluster_y, range=range)
;if n_elements(b) le 1 then return

t =  (*pstate).range
struct_assign, range, t
(*pstate).range = t

if n_elements(b) gt 1 then begin
	tv, b, (*pstate).margin.low, (*pstate).margin.bottom, /device		; show pca_cluster
endif

!p.position = (*pstate).position
print,!p.position
plot,[0,0],[0,0],xrange=range.x,yrange=range.y, /noerase,/nodata, $
		xlog=(*pstate).logx,ylog=(*pstate).logy, xticklen=-0.01,yticklen=-0.01, charsize=1.2
!p.position = [0.,0.,0.,0.]
print,'pca_cluster: actual w,h=',!d.x_size,!d.y_size

xyouts,0.985,0.015,el[(*pstate).pca_cluster_x],charsize=1.8,charthick=15.0,color=16,/norm, align=1.0
xyouts,0.985,0.015,el[(*pstate).pca_cluster_x],charsize=1.8,charthick=1.7,color=spec_colour('green'),/norm, align=1.0
xyouts,0.015,0.94,el[(*pstate).pca_cluster_y],charsize=1.8,charthick=15.0,color=16,/norm
xyouts,0.015,0.94,el[(*pstate).pca_cluster_y],charsize=1.8,charthick=1.7,color=spec_colour('green'),/norm

if ptr_valid ((*pstate).b) then ptr_free, (*pstate).b
(*pstate).b = ptr_new( b, /no_copy)

wset, (*pstate).pix
w = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
h = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top
device,copy=[0,0,w,h, 0,0,(*pstate).wid2]

;p = (*pstate).pmark
;conc_pca_cluster_to_xy, pstate, (*p).cx,(*p).cy, x,y, /nozoom
;(*p).x = x
;(*p).y = y

clear_pca_cluster_spline, pstate, /init
wset, (*pstate).wid2
plot_pca_cluster_spline, pstate, /use_conc
return
end

;-----------------------------------------------------------------

pro free_pca_cluster_state, pstate

ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'Free_pca_cluster_state',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(pstate) eq 0 then return
if ptr_valid(pstate) eq 0 then return
if size(*pstate,/tname) ne 'STRUCT' then return

	if (*pstate).pix ge 0 then wdelete, (*pstate).pix
	if (*pstate).pix2 ge 0 then wdelete, (*pstate).pix2
	(*pstate).pix = -1
	(*pstate).pix2 = -1

	if ptr_valid( (*pstate).path) then ptr_free, (*pstate).path
	if ptr_valid( (*pstate).b) then ptr_free, (*pstate).b
;	if ptr_valid( (*pstate).q) then ptr_free, (*pstate).q
	if ptr_valid( (*pstate).qc) then ptr_free, (*pstate).qc
	if ptr_valid( (*pstate).image_x) then ptr_free, (*pstate).image_x
	if ptr_valid( (*pstate).image_y) then ptr_free, (*pstate).image_y
	if ptr_valid( (*pstate).pspline) then ptr_free, (*pstate).pspline
	if ptr_valid( (*pstate).pmark) then ptr_free, (*pstate).pmark
	if ptr_valid( (*pstate).pexport) then ptr_free, (*pstate).pexport

	if ptr_valid( (*pstate).pca.presults) then ptr_free, (*pstate).pca.presults
	if ptr_valid( (*pstate).pca.pimage) then ptr_free, (*pstate).pca.pimage
	if ptr_valid( (*pstate).pca.perror) then ptr_free, (*pstate).pca.perror
	if ptr_valid( (*pstate).pca.pq) then ptr_free, (*pstate).pca.pq
	if ptr_valid( (*pstate).pca.pqs) then ptr_free, (*pstate).pca.pqs
	if ptr_valid( (*pstate).pca.pqel) then ptr_free, (*pstate).pca.pqel
	if ptr_valid( (*pstate).pca.pclass) then ptr_free, (*pstate).pca.pclass
	if ptr_valid( (*pstate).pca.pcoeff) then ptr_free, (*pstate).pca.pcoeff
	if ptr_valid( (*pstate).pca.peigen) then ptr_free, (*pstate).pca.peigen
	if ptr_valid( (*pstate).pca.pvar) then ptr_free, (*pstate).pca.pvar

return
end

;-----------------------------------------------------------------

function legend_pca_cluster_string, pstate, position=position

if n_elements(pstate) eq 0 then return,''
if ptr_valid(pstate) eq 0 then return,''
if size(*pstate,/tname) ne 'STRUCT' then return,''
if n_elements(position) lt 1 then position=0
p = (*pstate).p
if ptr_good(p) eq 0 then return, ''
xanes_stack_test, p, xanes, n_els, el, el_xanes, z_found=z_found
el = 'PC ' + strtrim(string(indgen(n_els)),2)

ex = el[(*pstate).pca_cluster_x < (n_els-1)]
ey = el[(*pstate).pca_cluster_y < (n_els-1)]

if position then begin
	pm = (*pstate).pmark
	x = (*pm).x[0]
	y = (*pm).y[0]
	xy_pca_cluster_to_conc, pstate, x,y, mx,my, /nozoom
	sx = str_tidy(mx)
	sy = str_tidy(my)

	Note = 'Spline centre '

endif else begin
	mx = (*pstate).range.x[1]
	my = (*pstate).range.y[1]
	sx = str_tidy(mx)
	sy = str_tidy(my)

	Note = 'Display top '
endelse

special = special_elements()
if (*p).type eq 1 then begin
	unitsx = ''
	unitsy = ''
	stylex = ' fraction'
	styley = ' fraction'
endif else if (*p).type eq 2 then begin
	unitsx = ''
	unitsy = ''
	stylex = ' counts'
	styley = ' counts'
endif else begin
	unitsx = ' '
	unitsy = ' '
	stylex = xanes ? '' : ' conc'
	styley = xanes ? '' : ' conc'
endelse

s = note + 'X (' + ex + stylex + ') = ' + sx + unitsx
s = [s, note + 'Y (' + ey + styley + ') = ' + sy + unitsy]
;s = [s, 'Use Export plot ("File" menu) to show detailed axes']
return, s
end

;-----------------------------------------------------------------
; Make the byte array to TV to the draw area

function make_pca_cluster_tvb, pstate, ix,iy, nozoom=nozoom, xmax=xmax, ymax=ymax, xmin=xmin, ymin=ymin, $
			xtgt=xtgt, ytgt=ytgt, compress=compress, low=low, high=high, range=range

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'Make_pca_cluster_TVB',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0B
	endif
endif
if n_elements(nozoom) lt 1 then nozoom=0
if n_elements(xmax) lt 1 then xmax=0
if n_elements(ymax) lt 1 then ymax=0
if n_elements(xmin) lt 1 then xmin=0
if n_elements(ymin) lt 1 then ymin=0
if n_elements(xtgt) lt 1 then xtgt=0
if n_elements(ytgt) lt 1 then ytgt=0
b = 0B
p = (*pstate).p
if ptr_valid( p) eq 0 then return, b
pca = (*pstate).pca
if ptr_good(pca.presults) eq 0 then return, b

cx = reform((*pca.presults)[*,ix])
cy = reform((*pca.presults)[*,iy])

if ptr_valid( (*pstate).image_x) then ptr_free, (*pstate).image_x
if ptr_valid( (*pstate).image_y) then ptr_free, (*pstate).image_y
(*pstate).image_x = ptr_new(cx)
(*pstate).image_y = ptr_new(cy)

conc_pca_cluster_to_xy, pstate, cx,cy, x,y, /veto, /save_max, range=range, sx=sx,sy=sy

q = where( (cx lt range.x[1]) and (cx gt range.x[0]) and (cy lt range.y[1]) and (cy gt range.y[0]), nq)
if nq gt 0 then begin
	x = x[q]
	y = y[q]
endif else begin
	return, 0B						; Nov 2011 ??
;	x = 0
;	y = 0
endelse

h = hist_2d( x,y, min1=0,min2=0, max1=sx-1,max2=sy-1)
if (*pstate).high eq 0 then begin
	hmax = image_weighted_max(h,threshold=0.01, scope=1000, nothing_remains=nothing)
endif else begin
	hmax = image_weighted_max(h,threshold=0.003, scope=100, nothing_remains=nothing)
endelse
if nothing then return, 0B
low = (*pstate).Low * hmax / 100. > 0
high = ((*pstate).High * hmax / 100. > (low + 0.005*hmax)) > 1

case (*pstate).Zaxis of
	1: begin
		low = sqrt(low)
		high = sqrt(high)
		h = sqrt(h)
	end
	2: begin
		low = alog10( (10*low) > 1.)
		high = alog10( 10*high)
		h = alog10( (10*h) > 1.)
	end
	else:
endcase

b = bytscl( h, top=99, min=low, max=high) + 16B

; Highlight class. This does not depend on whether we have built classes from PCA
; or pixel spectra as in the latter case we only select the same data index as for
; PCA (done in "Analyze_clusters"). NOTE: This means we need the PCA done even if we
; do clusters from pixel spectra.

if (*pstate).class_on and (*pstate).highlight[1] then begin
	qp = where( (*pca.pclass) eq (*pstate).class, nqp)
	if nqp gt 0 then begin
		conc_pca_cluster_to_xy, pstate, cx[qp],cy[qp], xp,yp
		b[xp,yp] = spec_colour('green')
	endif
endif

; Highlight points from a previous 'analyze spline' view

if (*pstate).highlight[0] then begin
	qp = *(*pstate).qpca
	if n_elements(qp) ge 1 then begin
		if qp[0] ne -1 then begin
			conc_pca_cluster_to_xy, pstate, cx[qp],cy[qp], xp,yp
			b[xp,yp] = spec_colour('pink')
		endif
	endif
endif

compress = 1
if nozoom then begin
	if ytgt ne 0 then begin
		compress = min( [ compress, float(ytgt) / float((*p).ysize) ])
	endif
	if xtgt ne 0 then begin
		compress = min( [ compress, float(xtgt) / float((*p).xsize) ])
	endif

	if ymin ne 0 then begin
		if ymin gt (*p).ysize*compress then begin
			compress = max( [ compress, float(ymin) / float((*p).ysize) ])
		endif
	endif
	if xmin ne 0 then begin
		if xmin gt (*p).xsize*compress then begin
			compress = max( [ compress, float(xmin) / float((*p).xsize) ])
		endif
	endif
	if ymax ne 0 then begin
		if ymax lt (*p).ysize*compress then begin
			compress = min( [ compress, float(ymax) / float((*p).ysize) ])
		endif
	endif
	if xmax ne 0 then begin
		if xmax lt (*p).xsize*compress then begin
			compress = min( [ compress, float(xmax) / float((*p).xsize) ])
		endif
	endif

	if compress ne 1 then begin
		b = smart_congrid( b, (*p).xsize*compress, (*p).ysize*compress, /interp)
	endif
endif else begin
	compress = float((*pstate).width) / float((*p).xsize)
	if ((*pstate).zoom ne 0) then begin
		b = smart_congrid( b, (*pstate).width, (*pstate).height, /interp)
	endif
endelse

return, b
end

;-----------------------------------------------------------------

function make_pca_cluster_mask, pstate, pca_index=qp

COMPILE_OPT STRICTARR
qc = -1
qp = -1

spline_pca_cluster_vertices, pstate, xs,ys, n
if n eq 0 then return, qc
p = (*pstate).p

s = 2^(*pstate).zoom
q = polyfillv( xs,ys, (*pstate).owidth, (*pstate).oheight)		; index in pca_cluster plot space

mask = bytarr( (*pstate).owidth, (*pstate).oheight)
mask[q] = 1														; mask image in pca plot space

conc_pca_cluster_to_xy, pstate, *(*pstate).image_x, *(*pstate).image_y, x,y, /nozoom

qp = where( mask[x,y] eq 1)										; index in PCA data point list

qc = (*(*pstate).pca.pq)[qp]									; index into original image space

if (*p).bounds.valid then begin
	q2 = bounds_mask( p, /reject)								; reject indices in image space
	qc = veto( q2, qc)
endif

return, qc
end

;-----------------------------------------------------------------

pro map_pca_cluster_help, pstate

	case !version.os_family of
		'MacOS': begin
			(*pstate).scr_xsize_off =	1
			(*pstate).scr_ysize_off =	138
			end
		'unix': begin
			(*pstate).scr_xsize_off =	8
			(*pstate).scr_ysize_off =	180
			end
		else: begin
			(*pstate).scr_xsize_off =	8
			(*pstate).scr_ysize_off =	8			;167
			end
	endcase
end

;--------------------------------------------------------------------

; Which is closest index of 'x,y' vectors to point ,'xp,yp'
; This version accepts vectors 'x', 'y' and returns the closest index, else -1

function near_pca_cluster_xy, xp,yp, x,y, reverse=reverse

if n_elements(reverse) lt 1 then reverse=0

r = sqrt( float(x-xp)*float(x-xp) + float(y-yp)*float(y-yp) )
i = indgen(n_elements(r))

q = where( r lt 5)			; close proximity
if q[0] eq -1 then return, -1
r = r[q]				; list of all in close proximity
i = i[q]				; indices of these

q = sort(r)				; sort in ascending distance order
r = r[q]
i = i[q]

if reverse then begin
	result = max(i)			; larger indices first
endif else begin
	result = i[0]			; smaller distance first
endelse

return, result
end

;-----------------------------------------------------------------
;
; Convert pixel x,y position to image 'x,y'
;
pro pixel_pca_cluster_to_xy, pstate, px,py, x,y

COMPILE_OPT STRICTARR

x = clip( zoom_pca_cluster(pstate,px - (*pstate).margin.low, /down), 0,(*pstate).width-1)
y = clip( zoom_pca_cluster(pstate,py - (*pstate).margin.bottom, /down), 0,(*pstate).height-1)

return
end
;
;-----------------------------------------------------------------
; Plot handles at vector positions x,y

pro plot_pca_cluster_handles, pstate, x,y, color, data=data

if n_elements(data) lt 1 then data=0
if n_elements(x) lt 1 then return
boxx = [-1,1,1,-1,-1]
boxy = [-1,-1,1,1,-1]
scale = 2
for i=0L,n_elements(x)-1 do begin
	px = x[i] + boxx * scale
	py = y[i] + boxy * scale
	if data then begin
		px = clip( px, (*pstate).minx, (*pstate).maxx)
		py = clip( py, (*pstate).miny, (*pstate).maxy)
	endif else begin
		px = (*pstate).margin.low + clip( px-(*pstate).margin.low, 0, (*pstate).width-1)
		py = (*pstate).margin.bottom + clip( py-(*pstate).margin.bottom, 0, (*pstate).height-1)
	endelse
	plots, px,py, device=1-data, data=data, color=color
endfor

return
end

;-----------------------------------------------------------------
; Plot current spline marker

pro plot_pca_cluster_spline, pstate, compress=compress,wide=wide, data=data, $
						weightx=wtx, weighty=wty, use_conc=use_conc

if n_elements(data) eq 0 then data=0
if n_elements(use_conc) eq 0 then use_conc=0
if n_elements(wide) eq 0 then wide=0
if n_elements(wtx) eq 0 then wtx=0
if n_elements(wty) eq 0 then wty=0
p = (*pstate).pmark

;  0 is centre handle, 1-* are control points

maxx = max((*p).x)
maxy = max((*p).y)
if (max([maxx,maxy]) eq 0) then return

; circle_vertices returns the 100 spline interpolation points

spline_pca_cluster_vertices, pstate, x,y,n, use_conc=use_conc
if n lt 1 then return

; plot handles

color = spec_colour('green')
pca_cluster_handles, pstate, x1,y1,n1, use_conc=use_conc

if data eq 0 then begin
	if use_conc then begin
		x1[0] = mean(x1)
		y1[0] = mean(y1)
		(*p).x = x1
		(*p).y = y1
	endif
	xy_pca_cluster_to_pixel, pstate, x1,y1, px,py, compress=compress
	if (wide eq 0) then plot_pca_cluster_handles, pstate, px,py, color
endif
;print,'handles: x=',x1

; plot the spline

if data then begin
	xy_pca_cluster_to_conc, pstate, x,y, px,py, /nozoom
	px = clip( px, (*pstate).minx, (*pstate).maxx)
	py = clip( py, (*pstate).miny, (*pstate).maxy)
	if wtx then px = px/10000.
	if wty then py = py/10000.
endif else begin
	xy_pca_cluster_to_pixel, pstate, x,y, px,py, compress=compress
endelse

if wide then begin
	plots, px,py, device=1-data, data=data, color=spec_colour('green'),thick=3.0			; b/w figures
endif else begin
	plots, px,py, device=1-data, data=data, color=color
endelse

(*p).present = 1
return
end

;-----------------------------------------------------------------

pro set_pca_cluster_map_help, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

map_pca_cluster_help, pstate
end

;-----------------------------------------------------------------------------------
; Set the view size and zoom for new pca_clusters.
; If this is a clone, then assume that shapes already cleared.
; For zoom= and /full, don't change the tlb size, or the element.
;
; /clone	for a clone
; /full		for zoom to full pca_cluster
; zoom=+1,-1	for zoom in,out

pro set_pca_cluster_view, pstate, top, clone=clone, full=full, zoom=izoom, no_change=no_change

ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'Set_pca_cluster_view',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(clone) lt 1 then clone=0
	if n_elements(full) lt 1 then full=0
	if n_elements(izoom) lt 1 then izoom=0
	if n_elements(no_change) lt 1 then no_change=0

	draw_trim = 0
	scr_trim = 15
	if !version.os_family eq 'MacOS' then begin
		draw_trim = 15
		scr_trim = 21
	endif

	p = (*pstate).p
	if ptr_good( p) eq 0 then return
	xanes_stack_test, p, xanes, n_el, el, el_xanes
	el = 'PC ' + strtrim(string(indgen(n_el)),2)

	old_zoom = (*pstate).zoom
	if clone eq 0 then begin
		if no_change eq 0 then begin
			(*pstate).zoom = ( (izoom eq 0) or (full eq 1) ) ? 0 : ((*pstate).zoom + izoom)
		endif
		(*pstate).width = zoom_pca_cluster( pstate, (*pstate).owidth)
		(*pstate).height = zoom_pca_cluster( pstate, (*pstate).oheight)
	endif

	w = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
	h = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top

	allocate_pixmap, w, h, new_wid=wid, old_wid=(*pstate).pix, error=error
	if error then goto, bad_pix
	(*pstate).pix = wid

	allocate_pixmap, w, h, new_wid=wid, old_wid=(*pstate).pix2, error=error
	if error then goto, bad_pix
	(*pstate).pix2 = wid

	if (full eq 0) and (izoom eq 0) and (no_change eq 0) then begin
		(*pstate).pca_cluster_x = 0
		(*pstate).pca_cluster_y = 1
		widget_control, (*pstate).element_idx, set_value=el, $
			set_combobox_select = 0
		widget_control, (*pstate).element_idy, set_value=el, $
			set_combobox_select = 1

		(*pstate).xbottom = 0
		(*pstate).xbottom = 0
		(*pstate).low = 0
		(*pstate).high = 100
		widget_control, (*pstate).X_slider, set_value=0
		widget_control, (*pstate).Y_slider, set_value=0
		widget_control, (*pstate).Low_slider, set_value=0
		widget_control, (*pstate).High_slider, set_value=100
	endif

	if (izoom eq 0) and (no_change eq 0) then begin
		(*pstate).w = (w + scr_trim) < 600
		(*pstate).h = (h + scr_trim) < 600
	endif else begin
		(*pstate).w = (( (*pstate).w) > (256 + scr_trim)) < (w + scr_trim)
		(*pstate).h = (( (*pstate).h) > (64 + scr_trim)) < (h + scr_trim)
	endelse
	map_pca_cluster_help, pstate

	widget_control, (*pstate).draw2, draw_xsize=w+draw_trim, $
		draw_ysize=h+draw_trim, scr_xsize=(*pstate).w, scr_ysize=(*pstate).h

	if (clone eq 0) and (full eq 0) and (izoom eq 0) then begin
;		clear_all_markers, pstate
	endif
	goto, fix_position
	
bad_pix:
	(*pstate).zoom = old_zoom
	(*pstate).width = zoom_pca_cluster( pstate, (*pstate).owidth)
	(*pstate).height = zoom_pca_cluster( pstate, (*pstate).oheight)

fix_position:
	totx = (*pstate).width + (*pstate).margin.low + (*pstate).margin.high
	toty = (*pstate).height + (*pstate).margin.bottom + (*pstate).margin.top
	(*pstate).position = [float((*pstate).margin.low)/float(totx), float((*pstate).margin.bottom)/float(toty), $
			1.-(float((*pstate).margin.high)/float(totx)), 1.-(float((*pstate).margin.top)/float(toty))]

	draw_pca_clusters, pstate
	return
end

;-----------------------------------------------------------------
; Build list of spline vertices (in pca_cluster display coords).

pro spline_pca_cluster_vertices, pstate, x,y, n, use_conc=use_conc

if n_elements(use_conc) lt 1 then use_conc=0

p = (*pstate).pmark

;  1-10 (or 1-32) are control points, 0 is centre handle

if use_conc then begin
	cx = (*p).cx[1:*]
	cy = (*p).cy[1:*]
	conc_pca_cluster_to_xy, pstate, cx,cy, px,py, /nozoom, /noclip
endif else begin
	px = (*p).x[1:*]
	py = (*p).y[1:*]
endelse

spline_shape, px,py, x,y

n = n_elements(x)

maxx = max(px)
maxy = max(py)
if (max([maxx,maxy]) eq 0) then n=0

w = (*pstate).width
h = (*pstate).height
x = clip( x, 0, w-1)
y = clip( y, 0, h-1)
return
end

;-----------------------------------------------------------------
;
; Convert image 'x,y' to conc position 'cx,cy'
; NOTE: if formula changed here, change 'conc_pca_cluster_to_xy' too.

pro xy_pca_cluster_to_conc, pstate, x,y, cx,cy, nozoom=nozoom

cx = 0.0
cy = 0.0
if ptr_valid( (*pstate).p) eq 0 then return
if n_elements(nozoom) lt 1 then nozoom=0
if nozoom then begin
	w = (*pstate).owidth
	h = (*pstate).oheight
endif else begin
	w = (*pstate).width
	h = (*pstate).height
endelse

xmax = (*pstate).maxx
ymax = (*pstate).maxy
xmin = (*pstate).minx
ymin = (*pstate).miny
if (*pstate).logx then begin
	xmax1 = xmax
	xmax = xmax1 / 10^( 5.0 - 0.05*(*pstate).xtop)
	xmin = xmax1 / 10^( 5.0 - 0.045*(*pstate).xbottom) < xmax
	cx = alog10(xmin) + (x/(w-1))*alog10(xmax/xmin)
	cx = 10.^cx
endif else begin
	a = (xmax - xmin) / 100.
	b = xmin 
	xmax = a * float((*pstate).xtop) + b
	xmin = a * float((*pstate).xbottom) + b
	cx = x*(xmax-xmin)/(w-1) + xmin
endelse
if (*pstate).logy then begin
	ymax1 = ymax
	ymax = ymax1 / 10^( 5.0 - 0.05*(*pstate).ytop)
	ymin = ymax1 / 10^( 5.0 - 0.045*(*pstate).ybottom) < ymax
	cy = alog10(ymin) + (y/(h-1))*alog10(ymax/ymin)
	cy = 10.^cy
endif else begin
	a = (ymax - ymin) / 100.
	b = ymin 
	ymax = a * float((*pstate).ytop) + b
	ymin = a * float((*pstate).ybottom) + b
	cy = y*(ymax-ymin)/(h-1) + ymin
endelse

return
end

;-----------------------------------------------------------------
;
; Convert image 'x,y' to pixel position 'px,py'
; If compress, ignore zoom, scale x,y by compress.

pro xy_pca_cluster_to_pixel, pstate, x,y, px,py, compress=compress

px = 0
py = 0
if ptr_valid( (*pstate).p) eq 0 then return

if n_elements(compress) eq 0 then begin
	px = zoom_pca_cluster( pstate, x) + (*pstate).margin.low
	py = zoom_pca_cluster( pstate, y) + (*pstate).margin.bottom
endif else begin
	px = fix(compress * x) + (*pstate).margin.low
	py = fix(compress * y) + (*pstate).margin.bottom
endelse

return
end

;-----------------------------------------------------------------

function zoom_pca_cluster, pstate, n, down=down

if n_elements(down) eq 0 then down=0

if down eq 0 then begin
	if (*pstate).zoom gt 0 then begin
		x = n * 2^((*pstate).zoom)
	endif else if (*pstate).zoom lt 0 then begin
		x = n / 2^(-(*pstate).zoom)
	endif else begin
		x = n
	endelse
endif else begin
	if (*pstate).zoom gt 0 then begin
		x = n / 2^((*pstate).zoom)
	endif else if (*pstate).zoom lt 0 then begin
		x = n * 2^(-(*pstate).zoom)
	endif else begin
		x = n
	endelse
endelse

return, x
end

;------------------------------------------------------------------------------

; Stub routine for autoloading ...

pro pca_cluster_routines
end
