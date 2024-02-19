pro tv_text, x,y, text, font=font, charsize=charsize, $
		transparent=c, align=align, _extra=extras

; Generate text string using current font, with antialiasing,
; then write it into current device as /TRUE /DECOMPOSED bitmap.
; Optionally, set a transparent colour trans=[r,g,b]
;
; Should add alpha channel to this sometime, pass on to tv_paste ...

if n_elements(text) lt 1 then return
if n_elements(charsize) lt 1 then charsize = 10
if n_elements(align) lt 1 then align = 0
if n_elements(font) lt 1 then font = 'Helvetica'

old_window = !d.window
!p.charsize = charsize
x_ch_size = !d.x_ch_size * !p.charsize			; character dimensions
y_ch_size = !d.y_ch_size * !p.charsize			; in device units
xsize = 10. * x_ch_size * lenchr(text)
ysize = 20. * y_ch_size + 100

window, xsize=xsize, ysize=ysize, /pixmap
!p.font = 1
device, set_font=font, /tt_font

if align eq 0 then begin
	xt = 0
	yt = 10. * y_ch_size
	x1 = x
endif else if align eq 1 then begin
	xt = xsize
	yt = 10. * y_ch_size
	x1 = (x - xsize/10.) > 0
endif else begin
	xt = xsize/2
	yt = 10. * y_ch_size
	x1 = (x - 0.5*xsize/10.) > 0
endelse

xyouts, xt,yt, text, /device, charsize=10.*charsize, align=align, _extra=extras

b = tvrd(0,0,xsize,ysize, true=1)

b2 = bytarr(3,xsize/10,ysize/10)
for k=0L,2 do begin
	for j=0L,(ysize/10)-1 do begin
		for i=0L,(xsize/10)-1 do begin
			b2[k,i,j] = byte(total(long(b[k,i*10:i*10+9,j*10:j*10+9]))/100)
		endfor
	endfor
endfor
wdelete

wset, old_window
tv_paste, b2, x1,(y - yt/10) > 0, transparent=c

return
end
