pro image_geopixe_preview, file, preview=preview, _extra=extra

; Provide preview image and data for 'file_requester' from 'file'.
; Return preview data in 'preview' struct.
; /ignore	ignore nulls issue in dai read.

COMPILE_OPT STRICTARR
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
		warning,'image_geopixe_preview',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
define_devices

preview = 0L
if n_elements(file) lt 1 then return
if n_elements(discard) lt 1 then discard=0

p = read_geopixe_image( file, /header, version=version, error=error, _extra=extra)
if error ne 0 then return
if ptr_valid(p) eq 0 then return

;------------------------------------------------------------
; Details string array:

list = image_details( p, show=0, /brief)

;------------------------------------------------------------
; Preview image:

if (*p).has_preview then begin
	img = *(*p).preview
endif else begin
	if long((*p).xsize) * long((*p).ysize) ge 600L*600L then goto, just_details
	free_images, p
	p = read_geopixe_image( file, error=error, _extra=extra)
	if error ne 0 then goto, just_details
	if ptr_valid(p) eq 0 then goto, just_details
	
	scale = max([float((*p).xsize)/200, float((*p).ysize)/200]) 
	nxp = long((*p).xsize/scale)
	nyp = long((*p).ysize/scale)
	border = max([nxp,nyp]/50) > 1
	img = smart_congrid( (*(*p).image)[*,*,1]>0, nxp, nyp)
	if (*p).n_el gt 2 then begin
		for i=2,(4<(*p).n_el)-1 do begin
			img = img + smart_congrid( (*(*p).image)[*,*,i]>0, nxp, nyp)
		endfor
	endif
	img[0:border-1,*] = 0
	img[nxp-border:nxp-1,*] = 0
	img[*,0:border-1] = 0
	img[*,nyp-border:nyp-1] = 0
	(*p).preview = ptr_new(img)
	(*p).has_preview = 1
endelse
free_images, p

	preview = {details:list, image:img}
	return
	
just_details:
	preview = {details:list}
	return

end
