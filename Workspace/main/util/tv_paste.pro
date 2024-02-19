pro tv_paste, data, x,y, transparent=c

; Paste the true-colour image data 'data' onto the graphics device at 'x,y'
; Optionally, set a transparent colour = [r,g,b]
; Data must have true=1 order [3,*,*]
;
; Should add Alpha channel to this sometime ...

if (size(data))[0] ne 3 then return
if n_elements(data[*,0,0]) lt 3 then return
if n_elements(x) lt 1 then x=0
if n_elements(y) lt 1 then y=0

device, decomposed=1
b = data
if n_elements(c) ge 3 then begin
	q = where( (data[0,*,*] ne c[0]) or (data[1,*,*] ne c[1]) or (data[2,*,*] ne c[2]), nq)
	sx = n_elements(data[0,*,0])
	sy = n_elements(data[0,0,*])
	nxy = sx*sy
	if (nq gt 0) and (nq lt nxy) then begin
		if (sx+x le !d.x_size) and (sy+y le !d.y_size) then begin
			b = tvrd(x,y,sx,sy,true=1)
			b[3*q+0] = data[3*q+0]
			b[3*q+1] = data[3*q+1]
			b[3*q+2] = data[3*q+2]
		endif else begin
			sx2 = min([sx,!d.x_size-x])
			sy2 = min([sy,!d.y_size-y])
			b2 = tvrd(x,y,sx2,sy2,true=1)
			b[*,0:sx2-1,0:sy2-1] = b2
			b[3*q+0] = data[3*q+0]
			b[3*q+1] = data[3*q+1]
			b[3*q+2] = data[3*q+2]
		endelse
	endif
endif

tv, b, x,y, /true

device, decomposed=0
return
end
