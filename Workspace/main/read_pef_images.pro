function read_pef_images, file, header=header, error=error
;
;	Read the images file 'file'
;
;	Return 'p', a pointer (or pointer array) pointing to
;	Image structs, containing the image details
;	and data.
;
;	/header	just read header information, not images.
;

COMPILE_OPT STRICTARR
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

	raw = python.import('rawpy')

	d = raw.imread(file[0])
	rgb = d.postprocess( gamma=[1,1], no_auto_bright=1, output_bps=16)
	r = d.close()

;	if r eq 0 then goto, bad_io
	if n_elements(rgb[*,0,0]) ne 3 then goto, bad_io

	n_el = 3
	sx = n_elements(rgb[0,*,0])
	sy = n_elements(rgb[0,0,*])
	img = fltarr(sx,sy,n_el)
	el = ['Red','Green','Blue']

;	image.scan.x = ?							; image size (mm)
;	image.scan.y = ?

	image.source = file[0]
	image.file = image.source
	
	for i=0L,n_el-1 do begin
		img[*,*,i] = rgb[i,*,*]
	endfor
	
	image.n_el = n_el
	image.el = ptr_new(el, /no_copy)
	image.xsize = sx
	image.ysize = sy
	
	image.cal.poly[1] = 1.0
	image.cal.poly[0] = 0.0
	image.cal.units = 'channel'
	image.charge = float(sx * sy)				; makes average concs equal to pixel counts
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

bad_io:
	print,'read_bmp_images: I/O error'
	goto, error
bad_file:
	print,'read_bmp_images: no file name supplied'
	goto, usage

usage:
	print,'read_pef_images: Usage: p = read_ref_images(file)'
	print,'		where "p" is pointer to image struct'
	print,'		and "file" is the name of the input file'
	goto, error

error:
	p = 0
	error = 1
	goto, finish
end
