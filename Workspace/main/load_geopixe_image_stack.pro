function load_geopixe_image_stack, elname, path=path, dir=dir, group=group, $
				flatten=flatten0, error=err

; Read a stack of GeoPIXE image DAI files from path 'path' and assemble
; a XANES image stack for element 'elname'.
; 
; Returns a pointer to a 'stack' struct as defined in "define.pro" /stack.
; 
; Uses multiple file select to select a range of DAI files in a single Dir.
; 
; Set /Dir to select directories instead. 
; In this case, uses multiple select to select a range of dirs; 
; it will read a DAI file of the same name as each Dir.
;
; /flatten	redo flatten of each plane to 'Flux0'.

now = file_expand_path('.')
p = 0L
if n_elements(path) lt 1 then path=now
if n_elements(dir) lt 1 then dir=0
if n_elements(flatten0) lt 1 then flatten0=0
flatten = flatten0
err = 1
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'load_geopixe_image_stack',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif
if dir then begin
	f0 = file_requester(path=path, title='Select dirs containing DAI files', /dir, /multiple)
	name = strip_path(f0)
	if inumeric(name[0]) then begin
		q = sort(long2(name))
		name = name[q]
		f0 = f0[q]
	endif
	if name[0] eq '' then return,0

	f = f0 + slash() + name + '.dai'
	if file_test(f[0], /read) eq 0 then begin	; plain DAI filename not found, look for tagged ones ...
		f1 = file_search( f0[0], name[0]+'*.dai')
		if n_elements(f1) gt 1 then begin
			sel = element_select( group, strip_path(f1), title='Select name variant', /off)
			q = where( sel eq 1, nq)
			if nq eq 0 then return,0
			f1 = f1[q[0]]
		endif
		n = strlen( strip_file_ext(f[0]))
		tag = strmid( strip_file_ext( f1), n)
		f = f0 + slash() + name + tag + '.dai'
	endif
	
endif else begin
	f = file_requester(path=path, title='Select DAI files', filter='*.dai', fix_filter=0, /multiple)
endelse

; Need to also read file of energies (if not in headers) ...

nf = n_elements(f)								; number of stack planes
if (nf lt 1) or (f[0] eq '') then return, 0L

p = ptr_new( define(/stack))					; 3D stack data struct

p0 = read_geopixe_image( f[0])					; first image
if ptr_good(p0) eq 0 then begin
	warning,'load_geopixe_image_stack',['Error reading first image plane:', $
				f[0], 'Abort ...']
	goto, fin
endif
obj = (*p0).DevObj

n_el = (*p0).n_el
(*p).source = (*p0).source
(*p).source2 = (*p0).source2
(*p).throttle = (*p0).throttle
(*p).pileup = (*p0).pileup
(*p).linearize = (*p0).linearize
(*p).DevObj = clone_device_object(obj)
(*p).file = (*p0).file
(*p).sample = (*p0).sample
(*p).grain = (*p0).grain
(*p).comment = (*p0).comment
(*p).cal = (*p0).cal
(*p).ecompress = (*p0).ecompress
(*p).scan.x = (*p0).scan.x
(*p).scan.y = (*p0).scan.y
(*p).scan.origin.x = (*p0).scan.origin.x
(*p).scan.origin.y = (*p0).scan.origin.y
if ptr_good((*p0).px_coords) then (*p).px_coords = ptr_new(*(*p0).px_coords)
(*p).x_coord_units = (*p0).x_coord_units
if ptr_good((*p0).py_coords) then (*p).py_coords = ptr_new(*(*p0).py_coords)
(*p).y_coord_units = (*p0).y_coord_units

; (*p).pz_coords = ptr_new( ??)
; (*p).z_coord_units = ??
;(*p).maia = (*p0).maia

(*p).matrix = (*p0).matrix
if ptr_good((*p0).matrix.mdl) then (*p).matrix.mdl = ptr_new( *(*p0).matrix.mdl)
; do we need to inherit history from the Xanes element plane?
(*p).show_back = (*p0).show_back
(*p).xcompress = (*p0).xcompress
(*p).ycompress = (*p0).ycompress
(*p).original_xsize = (*p0).original_xsize
(*p).original_ysize = (*p0).original_ysize
(*p).bounds = (*p0).bounds
(*p).xsize = (*p0).xsize
(*p).ysize = (*p0).ysize
nx0 = (*p).xsize
ny0 = (*p).ysize
nxe0 = (nx0+1)/2
nye0 = (ny0+1)/2
(*p).zsize = nf
(*p).scaled_x = (*p0).scaled_x
(*p).scaled_y = (*p0).scaled_y
(*p).xoffset = (*p0).xoffset
(*p).yoffset = (*p0).yoffset
(*p).ystep = (*p0).ystep
(*p).xstep_on = (*p0).xstep_on
(*p).xstep = (*p0).xstep
(*p).step_events = (*p0).step_events
(*p).step_toggle = (*p0).step_toggle
(*p).toggle_bit = (*p0).toggle_bit
(*p).toggle_station = (*p0).toggle_station
(*p).events = (*p0).events
(*p).type = (*p0).type
(*p).mode = (*p0).mode
(*p).channel = (*p0).channel
(*p).detector = (*p0).detector
(*p).array = (*p0).array
if ptr_good((*p0).pactive) then (*p).pactive = ptr_new( *(*p0).pactive)
if ptr_good((*p0).pcal) then (*p).pcal = ptr_new( *(*p0).pcal)
(*p).charge = (*p0).charge
(*p).IC = (*p0).IC
(*p).dwell = (*p0).dwell
if ptr_good((*p0).plist) then (*p).plist = ptr_new( *(*p0).plist)
(*p).has_flux = (*p0).has_flux
(*p).flatten = (*p0).flatten
(*p).has_errors = (*p0).has_errors

if n_elements(elname) lt 1 then begin
	if n_elements(group) lt 1 then goto, bad_el
	names = *(*p0).el
	select = element_select( group, names, old_select=intarr((*p0).n_el), $
			title='Select XANES element')
	q = where(select eq 1, nq)
	if nq lt 1 then goto, fin
	elname = names[q[0]]
endif

q = where( elname eq *(*p0).el, nq)				; q[0] is index to 'elname' in image[] array
if nq eq 0 then goto, bad_el
(*p).el = elname
(*p).ixanes = q[0]

if flatten then begin
	q0 = where( *(*p0).el eq 'Flux0', nq0)			; flatten based on "Flux0" image
	if nq0 eq 0 then begin
		warning,'load_geopixe_image_stack',['No "Flux0" plane found for (re-)flatten.', $
				'Will continue, but ignore /flatten request.']
		flatten = 0
	endif else begin
		base = 0.05									; ignore pixels where Flux0 is less than
													; this fraction of average, or larger than
													; the inverse of this times average
													
		scale =  flux_flatten( (*(*p0).image)[0:nx0-1,0:ny0-1,q0[0]], base=base)
		
		for j=0,n_el-1 do begin
			(*(*p0).image)[0:nx0-1,0:ny0-1,j] = (*(*p0).image)[0:nx0-1,0:ny0-1,j] * scale
		endfor
		*(*p0).flux = (*(*p0).image)[0:nx0-1,0:ny0-1,q0[0]]
		(*p0).has_flux = 1
		(*p0).flatten = 1
		(*p).flatten = (*p0).flatten
	endelse
endif

if (*p0).has_flux then begin
	flux = *(*p0).flux							; first plane of flux maps
endif											; normalize others to this one via 'charge'

stack = fltarr( nx0, ny0, nf)
stack[0:nx0-1,0:ny0-1,0] = (*(*p0).image)[0:nx0-1,0:ny0-1,q[0]]				; first plane of stack
energy = fltarr( nf)
label = strarr( nf)

if (*p0).has_errors then begin
	error = fltarr( nxe0, nye0, nf)
	error[0:nxe0-1,0:nye0-1,0] = (*(*p0).error)[0:nxe0-1,0:nye0-1,q[0]]		; first plane of error
endif
energy[0] = (*p0).energy						; energy of first plane
label[0] = (*p0).source

do_shift = 0
if nf gt 1 then begin
	for i=1L,nf-1 do begin
		if ptr_valid(p0) then free_images, p0
		p0 = read_geopixe_image( f[i])					; next image
		if ptr_good(p0) eq 0 then begin
			warning,'load_geopixe_image_stack',['Error reading image plane:', f[i], $
						'Skip and continue ("Cancel" to abort) ...'], cancel=cancel
			if cancel then goto, fin
			continue
		endif
			
		nx = min([nx0, (*p0).xsize])
		ny = min([ny0, (*p0).ysize])					; in case size differs a little (e.g. RT load)
		nxe = min([nxe0, ((*p0).xsize+1)/2])
		nye = min([nye0, ((*p0).ysize+1)/2])

		q = where( elname eq *(*p0).el, nq)				; q[0] is index to 'elname' in image[] array
		if nq eq 0 then goto, bad_el

;		May need to add 'flatten' code if (*p0).flatten is not 1
		
		if flatten then begin
			q0 = where( *(*p0).el eq 'Flux0', nq0)			; flatten based on "Flux0" image
			if nq0 eq 0 then begin
				warning,'load_geopixe_image_stack',['No "Flux0" plane found for (re-)flatten.', $
						'Will continue, but ignore /flatten request.']
				flatten = 0
			endif else begin
				base = 0.05									; ignore pixels where Flux0 is less than
															; this fraction of average, or larger than
															; the inverse of this times average
															
				scale =  flux_flatten( (*(*p0).image)[0:nx0-1,0:ny0-1,q0[0]], base=base)
				
				for j=0,n_el-1 do begin
					(*(*p0).image)[0:nx0-1,0:ny0-1,j] = (*(*p0).image)[0:nx0-1,0:ny0-1,j] * scale
				endfor
				(*p0).flatten = 1
			endelse
		endif
		
;		N.B. Image size MUST remain unchanged along stack for this to work ...
		
		if do_shift then begin
			tix = total( (*(*p0).image)[ *(*ps[0]).q + ishift*pixeln])
			xav = total( float(x) * (*(*p0).image)[ *(*ps[0]).q + ishift*pixeln]) / tix
			yav = total( float(y) * (*(*p0).image)[ *(*ps[0]).q + ishift*pixeln]) / tix
			centroid = {x:xav, y:yav}
			print, 'file: ',f[i],'  centroid = ',centroid

;		Correct image for shift from centroid0 to centroid ...

			dx = centroid.x - centroid0.x				; total shifts
			dy = centroid.y - centroid0.y
			
			print,'********','correct shift = ',dx,dy
			print,''
			for j=0,(*p0).n_el-1 do begin
				(*(*p0).image)[ *,*,j] = shift_image( (*(*p0).image)[ *,*,j], -dx,-dy) 
			endfor
		endif
		
		stack[0:nx-1,0:ny-1,i] = ((*p).charge/(*p0).charge) *	$		; next plane of stack
						(*(*p0).image)[0:nx-1,0:ny-1,q[0]]				; normalized by charge

		if (*p0).has_errors then begin
			error[0:nxe-1,0:nye-1,i] = ((*p).charge/(*p0).charge) *	$	; next plane of errors
						(*(*p0).error)[0:nxe-1,0:nye-1,q[0]]			; normalized by charge
		endif
		energy[i] = (*p0).energy						; energy of next plane
		label[i] = (*p0).source							; source data file label
;		print,'load stack: i,label=',i,'  ',label[i]
		(*p).source2 = (*p0).source2
		if (*p).source2 eq '' then (*p).source2 = (*p0).source
	endfor
endif

(*p).file = strip_file_ext(f[0]) + '-' + strip_path(f[nf-1])

if (*p).has_flux then begin
	(*p).flux = ptr_new( flux, /no_copy)
endif
if (*p).has_errors then begin
	(*p).error = ptr_new( error, /no_copy)
endif
(*p).image = ptr_new( stack, /no_copy)
if max(energy) gt 0.001 then begin
	(*p).pz_coords = ptr_new( energy, /no_copy)
	(*p).z_coord_units = 'keV'
endif
(*p).pz_label = ptr_new( label, /no_copy)

opt = define( /options_image)
options = replicate( opt, nf)	
escale = options
history = ptrarr( nf)
(*p).options = ptr_new( options, /no_copy)
if (*p).has_errors then (*p).escale = ptr_new( escale, /no_copy)
(*p).history = ptr_new( history, /no_copy)

set_image_minmax, p, (*p).image, (*p).options
if ((*p).has_errors eq 1) and (ptr_valid((*p).error) eq 1 ) then begin
	set_image_minmax, p, (*p).error, (*p).escale
endif
if flatten then (*p).flatten=1
err = 0

fin:
	if ptr_valid(p0) then free_images, p0
	return, p
	
bad_el:
	warning,'load_geopixe_image_stack','no element selected.',/error
	err = 1
	goto, fin
end
