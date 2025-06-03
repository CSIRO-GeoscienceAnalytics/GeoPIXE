pro image_preview, file, preview=preview

; Provide preview image and data for 'file_requester'

COMPILE_OPT STRICTARR

preview = 0L
list = 'Not a valid image file'
if n_elements(file) lt 1 then return
R=0L
G=0L
B=0L

img = read_image( file, R,G,B)
if img[0] eq -1 then return

s = size(img)
if s[0] eq 2 then begin
	D3 = 0
	nx = n_elements(img[*,0])
	ny = n_elements(img[0,*])
endif else if s[0] eq 3 then begin
	D3 = 1
	nx = n_elements(img[0,*,0])
	ny = n_elements(img[0,0,*])
endif else goto, just_details

;------------------------------------------------------------
; Details string array:

List = [ 'Image file']
s = D3 ? 'Interleaved 24/32 bit image file' : 'Colour-mapped image file'
List = [List,'  '+s]
List = [List,'Size:','  ' + str_tidy(nx) + ' x ' + str_tidy(ny) + ' pixels']
;------------------------------------------------------------
; Preview image:
	
	scale = max([float(nx)/200, float(ny)/200]) 
	nxp = long(nx/scale)
	nyp = long(ny/scale)
	if D3 then begin
;		img = smart_congrid( img, 3, nxp, nyp)
		preview = {details:list, image:img}
	endif else begin
;		img = smart_congrid( img, nxp, nyp)
		preview = {details:list, image:img, R:R, G:G, B:B}
	endelse
	return
	
just_details:
	preview = {details:list}
	return
end
