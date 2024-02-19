pro mark, el, y, k=k, l=l, m=m, charsize=charsize, invert=invert, $
		side=side, middle=middle, scale=scale

; Mark lines for element 'z', at position 'y'

default_plot, thick, athick, csize, cthick

if n_params(0) lt 2 then return
if n_elements(k) eq 0 then k=0
if n_elements(l) eq 0 then l=0
if n_elements(m) eq 0 then m=0
if n_elements(side) eq 0 then side=0
if n_elements(middle) eq 0 then middle=0
if n_elements(invert) eq 0 then invert=0
if n_elements(scale) eq 0 then scale=1.0
if n_elements(charsize) eq 0 then charsize=csize
if (l eq 0) and (m eq 0) then k=1

z = atomic_number(el)
if z lt 1 then return

e = all_lines(z,k=k,l=l,m=m,rel=rel)
rel = rel/max(rel)

if k then begin
	q = reverse(sort(rel))
	e = e[q]
	rel = rel[q]
	q = where((e ge e[0]) or (rel gt 0.03))
	e = e[q]
	rel = rel[q]
endif

ex = clip( e, !x.crange[0], !x.crange[1])
dir = 1 - 2*invert
ny = data_to_norm( y, /Y)
for i=0L,n_elements(e)-1 do begin
	top = y
	bot = norm_to_data( ny - dir*0.05*scale*(rel[i]^(0.33) > 0.1), /Y) > 0.1
	plots,[ex[i],ex[i]],[top,bot],thick=thick
endfor
plots,[min(ex),max(ex)],[y,y],thick=thick

q = reverse(sort(rel))
ex = ex[q]
rel = rel[q]
xn = data_to_norm( ex[0],/x)
yn = data_to_norm( y, /y)

if invert then begin
	if side then begin
		q = sort(ex)
		xs = data_to_norm( min(ex),/x) - 0.002
		ys = yn
		align = 1.0
	endif else begin
		xs = xn - 0.002
		ys = yn - 0.002 - char_height(csize)
		align = 0.0
	endelse
endif else begin
	if side then begin
		q = sort(ex)
		xs = data_to_norm( min(ex),/x) - 0.002
		ys = yn - 0.5*char_height(csize)
		align = 1.0
	endif else begin
		if middle then begin
			xs = data_to_norm( 0.5*(min(ex)+max(ex)),/x) - 0.002
			ys = yn + 0.002
			align = 0.5
		endif else begin
			xs = xn	- 0.001
			ys = yn + 0.004
			align = 0.0
		endelse
	endelse
endelse
xyouts, xs,ys, element_name(z), /norm, charsize=csize, charthick=cthick, align=align

return
end
