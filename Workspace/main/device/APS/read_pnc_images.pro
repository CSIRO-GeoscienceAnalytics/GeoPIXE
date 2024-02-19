function read_pnc_images, file, header=header, error=error
;
;	Read the PNC-CAT images file 'file'
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
		warning,'read_pnc_images',['IDL run-time error caught.', '', $
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
image.DevObj = obj_new('APS_LST_DEVICE')

on_ioerror, bad_file
openr, lun, file, /get_lun
on_ioerror, bad_io
line = ''
head = ''

more:
	readf, lun, line
	c = strmid( line, 0, 1)
	if c eq '#' then begin
		head = line
		goto, more
	endif

str = strsplit(line, '* 	', count=count, /extract)
if count lt 5 then goto, bad_extra
n_el = fix(str[2])
sx = fix(str[3])
sy = fix(str[4])

data = replicate( {x:0.0, y:0.0, e:fltarr(n_el)}, sx, sy)
readf, lun, data

img = fltarr(sx,sy,n_el)
for i=0L,n_el-1 do begin
	img[*,*,i] = data[*,*].e[i]
endfor

b = byte(head)
q = where((b eq 32) and (shift(b,1) ne 32) and (shift(b,-1) ne 32))
if q[0] ne -1 then b[q] = byte('.')
head = string(b)
str = strsplit(head, '# 	', count=count, /extract)
if count lt n_el+2 then goto, bad_columns
el = str[2:*]

image.scan.x = abs( (data[sx-1,0].x - data[0,0].x) ) / 1000.		; image size (mm)
image.scan.y = abs( (data[0,sy-1].y - data[0,0].y) ) / 1000.

image.source = file[0]
image.file = image.source

image.n_el = n_el
image.el = ptr_new(el, /no_copy)
image.xsize = sx
image.ysize = sy

image.cal.poly[1] = 1.0
image.cal.poly[0] = 0.0
image.cal.units = 'channel'
image.charge = float(sx * sy)			; makes average concs equal to pixel counts
; later want to extract 'IO' total
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
	close_file, lun
	return, p

bad_extra:
	print,'read_pnc_images: no extra data'
	goto, finish
bad_columns:
	print,'read_pnc_images: bad number of columns'
	goto, finish
bad_io:
	print,'read_pnc_images: I/O error'
	goto, error
bad_file:
	print,'read_pnc_images: no file name supplied'
	goto, usage
bad_version:
	print,'read_pnc_images: bad version number'
	goto, error
bad_user:
	print,'read_pnc_images: bad user mode'
	goto, error

usage:
	print,'read_pnc_images: Usage: p = read_pnc_images(file)'
	print,'		where "p" is pointer to image struct'
	print,'		and "file" is the name of the input file'
	goto, error

error:
	p = 0
	error = 1
	goto, finish
end
