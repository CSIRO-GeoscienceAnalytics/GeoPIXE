pro image_increment, image, new, xoffset=xoffseti, yoffset=yoffseti, plane=planei, $
						xcompress=xcompressi, ycompress=ycompressi, xborder=xb, yborder=yb, $
						bottom_clear_veto=bottom_clear_veto, top_clear_veto=top_clear_veto

; Increment 'image' by 'new', optionally compressed by 'xcompress', 'ycompress'
; starting at 'yoffset'. 'xoffset', 'yoffset' are defined in pre-compressed coordinates.
; If 'plane' specified, then only act on this plane of 3D array.
; 
; 'xb', 'yb' leave these border widths clear in final image, unless
; /bottom_clear_veto	don't clear bottom border
; /top_clear_veto		don't clear top border
; 
; Take care with remainder rows if compressing, i.e. rows at bottom and top
; outside of compress period. Add extra rows to bottom to make up remainder to
; whole compress Y step.
; 
; If this is for a stripe in the middle of an image, then use the *_clear_veto' flags
; to avoid clearing those borders.

COMPILE_OPT STRICTARR
if n_elements(image) eq 0 then return
ptr_mode = 0
if ptr_valid(image[0]) then begin
	ptr_mode = 1
	if n_elements(*image) lt 2 then return
endif
if n_elements(new) eq 0 then return
;if (size(new))[0] ne 2 then return
if n_elements(xcompressi) eq 0 then xcompressi=1
if n_elements(ycompressi) eq 0 then ycompressi=1
if n_elements(xoffseti) eq 0 then xoffseti=0
if n_elements(yoffseti) eq 0 then yoffseti=0
if n_elements(xb) eq 0 then xb=0
if n_elements(yb) eq 0 then yb=0
if n_elements(bottom_clear_veto) eq 0 then bottom_clear_veto=0
if n_elements(top_clear_veto) eq 0 then top_clear_veto=0
xoffset = xoffseti > 0
yoffset = yoffseti > 0
xcompress = xcompressi > 1
ycompress = ycompressi > 1

if ptr_mode then begin
	if ((size(*image))[0] ne 2) and ((size(*image))[0] ne 3) then return
	nix = n_elements((*image)[*,0,0])
	niy = n_elements((*image)[0,*,0])
	niz = ((size(*image))[0] eq 3) ? n_elements((*image)[0,0,*]) : 1
endif else begin
	if ((size(image))[0] ne 2) and ((size(image))[0] ne 3) then return
	nix = n_elements(image[*,0,0])
	niy = n_elements(image[0,*,0])
	niz = ((size(image))[0] eq 3) ? n_elements(image[0,0,*]) : 1
endelse
nnx = n_elements(new[*,0])
nny = n_elements(new[0,*])
if n_elements(planei) ne 0 then begin
	plane = planei < (niz - 1)
endif else plane=0L

; Determine padding to fill out rows to the full compress pitch

right = xoffset + nnx								; effective number of columns
mright = (right mod xcompress)
if mright ne 0 then mright = xcompress-mright		; columns to fill it out to match compressed grid
left = xoffset
mleft = (left mod xcompress)

top = yoffset + nny									; effective number of rows
mtop = (top mod ycompress)
if mtop ne 0 then mtop = ycompress-mtop				; rows to fill it out to match compressed grid
bot = yoffset
mbot = (bot mod ycompress)

; Form filled out array

if (mleft ne 0) or (mright ne 0) or (mbot ne 0) or (mtop ne 0) then begin
	new2 = new[0,0]
	new2[0] = 0
	new2 = replicate( new2[0], nnx + mleft + mright, nny + mbot + mtop)
	new2[mleft:mleft+nnx-1,mbot:mbot+nny-1] = new
	nnx = nnx + mleft + mright
	nny = nny + mbot + mtop
	xoffset = xoffset-mleft
	yoffset = yoffset-mbot
endif else begin
	new2 = new
endelse

; Compress filled out array (should be able to use rebin)

nnx = nnx/xcompress
nny = nny/ycompress
new2 = rebin( new2, nnx, nny)
new2 = new2 * float(xcompress) * float(ycompress)

; Determine coords in destination 'image'

xoff = xoffset/xcompress
yoff = yoffset/ycompress
x2l = 0
xl = xoff
if xl lt xb then begin
	x2l = xb-xl
	xl = xb
endif
if xl ge nix then return
xh = xoff + nnx-1
x2h = nnx-1
if xh gt (nix-1)-xb then begin
	x2h = (nnx-1) - (xh-((nix-1)-xb))
	xh = (nix-1)-xb
endif

y2l = 0
yl = yoff
ybb = bottom_clear_veto ? 0 : yb
if yl lt ybb then begin
	y2l = ybb-yl
	yl = ybb
endif
if yl ge niy then return
yh = yoff + nny-1
y2h = nny-1
ybb = top_clear_veto ? 0 : yb
if yh gt (niy-1)-ybb then begin
	y2h = (nny-1) - (yh-((niy-1)-ybb))
	yh = (niy-1)-ybb
endif

; Increment into 'image' if coords fit
 
if (xh ge xl) and (yh ge yl) then begin
	if ptr_mode then begin
		(*image)[xl:xh,yl:yh,plane] = (*image)[xl:xh,yl:yh,plane] + new2[x2l:x2h,y2l:y2h]
	endif else begin
		image[xl:xh,yl:yh,plane] = image[xl:xh,yl:yh,plane] + new2[x2l:x2h,y2l:y2h]
	endelse
endif
return
end


