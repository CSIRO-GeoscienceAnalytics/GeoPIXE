pro write_chimage, p, file, select=select
;
;	Write the images given by pointer 'p' to file 'file'
;	in PC binary for "Chimage".
;
;	'p'	is a pointer pointing to image structs, containing the images
;		details and data.
;	select	is an optional list of switches to select elements (1=on, 0=off)
;

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
		warning,'write_chimage',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, error
	endif
endif

version = -1L										; .chi version number

if n_elements(file) lt 1 then goto, bad_file
if n_elements(p) lt 1 then goto, usage
if ptr_valid(p) eq 0 then goto, usage

n_els = n_elements( *(*p).el)
if (n_elements(select) eq 0) or (n_elements(select) ne n_els) then begin
	select = replicate(1,n_els)
	q = where( ( strmid(*(*p).el,0,4) eq 'Back') or ( *(*p).el eq 'Sum'))
	if q[0] ne -1 then select[q] = 0
endif
qselect = where( select eq 1)
if qselect[0] eq -1 then return
nqselect = n_elements(qselect)

on_ioerror, bad_io
close, 1
s = strip_file_ext( file) + '.chi'
openw, 1, s

xesize = 0L
yesize = 0L
if ptr_valid( (*p).error) then begin
	xesize = n_elements( (*(*p).error)[*,0,0] )
	yesize = n_elements( (*(*p).error)[0,*,0] )
endif else begin
	(*p).has_errors=0
endelse

writeu,1, version

ns = strlen( (*p).source)							; source file name
nsa = strlen( (*p).sample)							; sample name
ng = strlen( (*p).grain)							; grain code, or analysis point name
nc = strlen( (*p).comment)							; comment string
writeu,1, ns, nsa, ng, nc

if ns gt 0 then begin
	b = byte( (*p).source)							; source file name
	writeu,1, b
endif
if nsa gt 0 then begin
	b = byte( (*p).sample)							; sample name
	writeu,1, b
endif
if ng gt 0 then begin
	b = byte( (*p).grain)							; grain code, analysis point name
	writeu,1, b
endif
if nc gt 0 then begin
	b = byte( (*p).comment)							; comment
	writeu,1, b
endif

writeu,1, nqselect									; number of images and element names
nsel = strlen( (*(*p).el)[qselect])				; length of all element strings
writeu,1, nsel

for i=0L,nqselect-1 do begin
	if nsel[i] gt 0 then begin
		b = byte( (*(*p).el)[qselect[i]])					; element name
		writeu,1, b
	endif
endfor

writeu, 1, (*p).type								; type of image (0=ppm.uC, 1=mineral fraction, 2=counts)
writeu, 1, (*p).detector			        	 	; detector, data type (0=PIXE, 1=PIGE, ...)

writeu, 1, (*p).xcompress, (*p).ycompress			; integral compression factors (1=none)
writeu,1, (*p).xsize, (*p).ysize					; x,y size of image arrays (after compression)
writeu,1, xesize, yesize							; x,y size of variance image arrays

writeu,1, (*p).charge								; total image integrated charge (uC)
writeu,1, (*p).scan.x, (*p).scan.y					; X,Y size of scan in mm
writeu, 1, (*p).corrected							; flags that image has been corrected (yields)
writeu, 1, (*p).original_xsize, (*p).original_ysize	; original (post-compress, pre-scale) size
writeu, 1, (*p).scaled_x, (*p).scaled_y				; image scaling factors

writeu, 1, (*p).has_errors							; flags the presence of an error array

writeu, 1, (*(*p).image)[*,*,qselect]								; the image array (image[xsize,ysize,n_el])
if (*p).has_errors eq 1 then writeu,1, (*(*p).error)[*,*,qselect]	; the variance array (error[xesize,yesize,n_el])


;writeu,1, (*p).cal.poly[1], (*p).cal.poly[0]
;writeu,1, long((*p).ecompress)
;writeu,1, (*p).processed,(*p).valid, (*p).bad_xy, (*p).clipped

;writeu,1, (*p).matrix.label
;writeu,1, (*p).matrix.charge
;writeu,1, (*(*p).matrix.mdl)[qselect]

;writeu, 1, (*p).ystep
;writeu, 1, (*p).xstep_on
;writeu, 1, (*p).xstep
;writeu, 1, (*p).matrix.file
;writeu, 1, (*p).step_events
;writeu, 1, (*p).events
;writeu, 1, (*p).step_toggle, (*p).toggle_bit, (*p).toggle_station
;writeu, 1, (*p).channel

;writeu, 1, (*p).show_back
;writeu, 1, (*p).device
;writeu, 1, (*p).source2


Finish:
	close,1
	return

bad_io:
	warning,'write_chimage','I/O error.'
	goto, error
bad_file:
	warning,'write_chimage','No file name supplied'
	goto, usage

usage:
	print,'write_chimage: Usage: write_chimage, p, file'
	print,'		where "p" is pointer to image struct'
	print,'		and "file" is the name of the input file'
	goto, error

error:
	goto, finish
end
