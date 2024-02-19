function read_bmp_images, file, header=header, error=error
;
;	Read the images file 'file'
;
;	Return 'p', a pointer (or pointer array) pointing to
;	Image structs, containing the image details
;	and data.
;
;	/header	just read header information, not images.
;

ErrorNo = 0
error = 1
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
		warning,'read_bmp_images',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_io
	endif
endif

if n_elements(file) lt 1 then goto, bad_file
if n_elements(header) lt 1 then header=0
p = 0

image = define(/image)
image.DevObj = obj_new('GENERIC_DEVICE')

r = query_bmp(file[0],info)
if r eq 0 then goto, bad_io

n_el = n_elements(file)
sx = info.dimensions[0]
sy = info.dimensions[1]
img = fltarr(sx,sy,n_el)
el = strarr(n_el)

;image.scan.x = ?			; image size (mm)
;image.scan.y = ?

b = byte(file)
j = -1
if n_el gt 1 then begin
	for m=0L,n_elements(b[*,0])-1 do begin
		for i=1L,n_el-1 do begin
			if b[m,i] ne b[m,0] then goto, more
		endfor
		j = m
	endfor
endif

more:
image.source = file[0]
if j gt 0 then begin
	image.source = strmid(file[0],0,j+1)
endif

trim = 0
ns = strlen(image.source)
if strmid(image.source,ns-1,1) eq '_' then trim=1
if strmid(image.source,ns-1,1) eq '-' then trim=1
if trim then image.source = strmid(image.source,0,ns-1)
image.source = image.source + '.' + extract_extension(file[0])
image.file = image.source

for i=0L,n_el-1 do begin
	ok = query_bmp(file[i],info)
	if ok then begin
		r = read_bmp(file[i])
		img[*,*,i] = r

		k = locate_last('.',file[i])
		el[i] = strmid(file[i],j+1,k-j-1)
		if i eq 0 then begin
			src = strmid(file[i],0,j-1)
		endif
	endif
endfor

image.n_el = n_el
image.el = ptr_new(el, /no_copy)
image.xsize = sx
image.ysize = sy

image.cal.poly[1] = 1.0
image.cal.poly[0] = 0.0
image.cal.units = 'channel'
image.charge = float(sx * sy)			; makes average concs equal to pixel counts
image.type = 2

mdl = fltarr(image.n_el)
image.matrix.mdl = ptr_new(mdl, /no_copy)

opt = define( /options_image)
options = replicate( opt, n_el)	

history = ptrarr( n_el)

if header then goto, wrap_up

for i=0L,n_el-1 do begin
	options[i].min = min(img[*,*,i])
	options[i].max = max(img[*,*,i])
endfor

;------- make pointers and finish -------------------------------

wrap_up:
	image.options = ptr_new( options, /no_copy)
	image.history = ptr_new( history, /no_copy)
	image.image = ptr_new( img, /no_copy)

	p = ptr_new( image, /no_copy)
	error = 0

finish:
	close,1
	return, p

bad_extra:
	print,'read_bmp_images: no extra data'
	goto, finish
bad_io:
	print,'read_bmp_images: I/O error'
	goto, error
bad_file:
	print,'read_bmp_images: no file name supplied'
	goto, usage
bad_version:
	print,'read_bmp_images: bad version number'
	goto, error
bad_user:
	print,'read_bmp_images: bad user mode'
	goto, error

usage:
	print,'read_bmp_images: Usage: p = read_bmp_images(file)'
	print,'		where "p" is pointer to image struct'
	print,'		and "file" is the name of the input file'
	goto, error

error:
	p = 0
	error = 1
	goto, finish
end
