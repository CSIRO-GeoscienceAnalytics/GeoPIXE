pro write_geopixe_image, p, file, no_display=no_display, no_null=no_null, $
			check_bounds=check_bounds, cluster=cluster, error=err, dump=dump, no_delete=no_delete

;	Write the images given by pointer 'p' to file 'file'
;	If (*p).xanes tag present, then write a XANES stack file.
;
;	'p' is a pointer pointing to
;	image  structs, containing the images details
;	and data.
;
;	/no_display		do not save display records
;	/no_null		veto saving any blank element names and associated image data
;	/check_bounds	trim image array size down to include only within "bounds"
;	/dump			dump a simple binary format '.bin' file.

COMPILE_OPT STRICTARR
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
		warning,'write_geopixe_image',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

; Remember to check for XANES specific code, or Image only code ...
; dump=1 for binary dump code.

version = -57L							; .dai/ .xan version number
err = 1

if n_elements(file) lt 1 then goto, bad_file
if n_elements(no_display) lt 1 then no_display=0
if n_elements(no_null) lt 1 then no_null=0
if n_elements(check_bounds) lt 1 then check_bounds=0
if n_elements(cluster) lt 1 then cluster=0
if n_elements(dump) lt 1 then dump=0
if n_elements(no_delete) lt 1 then no_delete=0

; A type of xanes=1, means a 3D stack

xanes_stack_test, p, xanes, n_el, el, el_xanes, z_found=z_found
if n_el lt 1 then begin
	gprint,'write_geopixe_image: No elements to write.'
	return
endif
(*p).version = version

if no_null then begin
	q = where( lenchr(el) ne 0, nq)
endif else begin
	q = indgen(n_el)
	nq = n_el
endelse
if (nq eq 0) then begin
	gprint,'write_geopixe_image: No "non-null" data to write.'
	return
endif
 
if ptr_valid( (*p).error) eq 0 then (*p).has_errors=0
if (*p).has_preview eq 0 then ptr_free, (*p).preview
if (ptr_valid( (*p).preview) eq 0) and (cluster eq 0) and (dump eq 0) then begin
	scale = max([float((*p).xsize)/200, float((*p).ysize)/200]) 
	nxp = long((*p).xsize/scale)
	nyp = long((*p).ysize/scale) > 2
	border = max([nxp,nyp]/50) > 1
	preview = smart_congrid( (*(*p).image)[*,*,q[1<(nq-1)]]>0, nxp, nyp)
	if n_el gt 2 then begin
		for i=2,(4<nq)-1 do begin
			preview = preview + smart_congrid( (*(*p).image)[*,*,q[i]]>0, nxp, nyp)
		endfor
	endif
	trimx = border < nxp/5
	trimy = border < nyp/5
	if trimx gt 0 then begin
		preview[0:trimx-1,*] = 0
		preview[nxp-trimx:nxp-1,*] = 0
	endif
	if trimy gt 0 then begin
		preview[*,0:trimy-1] = 0
		preview[*,nyp-trimy:nyp-1] = 0
	endif
	(*p).preview = ptr_new(preview, /no_copy)
	(*p).has_preview = 1
endif

on_ioerror, null
close, 1
on_ioerror, bad_open

;	Save to a file-name with ".temp" suffix. Rename (move) to final name
;	once file is successfully saved.

if cluster eq 0 then begin
	file_temp = file[0] + '.temp'
endif else begin
	file_temp = file[0]
endelse

; If use /compress then image files are about 10% smaller,
; but saving takes even longer.
; Need /compress here and in read_geopixe_image.pro

if dump then begin
	openw, 1, file_temp					; simple dump format file
endif else begin
	openw, 1, file_temp, /XDR			;, /compress
endelse
(*p).file = file
on_ioerror, bad_io

; /check_bounds is intended to be used with Maia-control to save DA RT images.
; It was assumed that the scan sizes ((*p).scan.x, (*p).scan.y) are correct FOR THE
; BOUNDS region, not the full xsize, ysize allocated in shared memory. And this also
; assumes that the full scan is completed and so the bounds are the full image.
; But if the scan is aborted early, then the bounds values will be less in Y, and
; a new scan.y value will need to be estimated.
; In this application it ignores offsets, which are not used in Maia Control.

if check_bounds then begin
	xmax = long((*p).bounds.xmax < ((*p).xsize-1))
	ymax = long((*p).bounds.ymax < ((*p).ysize-1))
	xemax = xmax/2
	yemax = ymax/2

	xsize = xmax+1
	ysize = ymax+1
	xesize = xemax+1
	yesize = yemax+1
	scanx = (*p).scan.x
	scany = (*p).scan.y
	if (*p).original_xsize gt xsize then begin
		scanx = scanx * float(xsize)/float((*p).original_xsize)
	endif
	if (*p).original_ysize gt ysize then begin
		scany = scany * float(ysize)/float((*p).original_ysize)
	endif
	original_xsize = xsize
	original_ysize = ysize
	sub_region = 0
	xoffset = 0L
	yoffset = 0L 
	x_sub_range = xsize * (*p).xcompress
	y_sub_range = ysize * (*p).ycompress
endif else begin
	xmax = (*p).xsize-1
	ymax = (*p).ysize-1
	xsize = (*p).xsize
	ysize = (*p).ysize
	scanx = (*p).scan.x
	scany = (*p).scan.y
	original_xsize = (*p).original_xsize
	original_ysize = (*p).original_ysize
	xoffset = (*p).xoffset
	yoffset = (*p).yoffset
;	if xanes eq 0 then begin
		sub_region = (*p).sub_region
		x_sub_range = (*p).x_sub_range
		y_sub_range = (*p).y_sub_range
		if x_sub_range eq 0 then x_sub_range = original_xsize * (*p).xcompress
		if y_sub_range eq 0 then y_sub_range = original_ysize * (*p).ycompress
;	endif
	xesize = 0L
	yesize = 0L
	if ptr_valid( (*p).error) then begin
		xesize = n_elements( (*(*p).error)[*,0,0] )
		yesize = n_elements( (*(*p).error)[0,*,0] )
	endif
	xemax = xesize-1
	yemax = yesize-1
endelse

gprint, 'write_geopixe_image: write header details ...'
writeu,1, (*p).version

if dump eq 0 then writeu,1, (*p).source
writeu,1, (*p).charge								; what about code below that can modify charge?
writeu,1, xsize, ysize

writeu,1, xesize, yesize							; added to ver=-48 /dump

if dump eq 0 then begin
	writeu,1, xoffset,yoffset 
	writeu,1, sub_region 
	writeu,1, x_sub_range, y_sub_range 
endif
	
if dump eq 0 then begin
	writeu,1, (*p).cal.poly[1], (*p).cal.poly[0]
	writeu,1, (*p).cal.units
	writeu,1, long((*p).ecompress)

	writeu,1, (*p).processed,(*p).valid, (*p).bad_xy, (*p).clipped

	writeu,1, scanx, scany
	writeu,1, (*p).scan.origin.x, (*p).scan.origin.y
endif

if dump eq 0 then begin
	nxc = 0L
	nyc = 0L
	if ptr_good((*p).px_coords) then nxc = min( [xsize, n_elements(*(*p).px_coords)])
	if ptr_good((*p).py_coords) then nyc = min( [ysize, n_elements(*(*p).py_coords)])
	writeu,1, nxc, nyc
	if nxc gt 1 then writeu,1, double( (*(*p).px_coords)[0:nxc-1])
	if nyc gt 1 then writeu,1, double( (*(*p).py_coords)[0:nyc-1])
	writeu,1, (*p).x_coord_units, (*p).y_coord_units
endif

if xanes and (dump eq 0) then begin
	nzc = 0L
	if ptr_good((*p).pz_coords) then nzc = min( [n_el, n_elements(*(*p).pz_coords)])
	writeu,1, nzc
	if nzc gt 1 then writeu,1, (*(*p).pz_coords)[0:nzc-1]			; floats
	writeu,1, (*p).z_coord_units

	nz2 = 0L
	if ptr_good((*p).pz_label) then nz2 = min( [n_el, n_elements(*(*p).pz_label)])
	writeu,1, nz2
	if nz2 gt 1 then writeu,1, (*(*p).pz_label)[0:nz2-1]
	
	writeu,1, (*p).scan.z, (*p).scan.origin.z	
endif

if dump eq 0 then begin
	writeu,1, (*p).sample
	writeu,1, (*p).grain
	writeu,1, (*p).comment
	writeu,1, (*p).facility
	writeu,1, (*p).endstation
	
	if (*p).matrix.label eq '' then (*p).matrix.label = (*p).matrix.file
	writeu,1, (*p).matrix.label
endif

writeu,1, nq
if dump eq 0 then begin
	if xanes then begin
		writeu,1, el_xanes, (*p).ixanes
		writeu,1, (*p).energies_file
		writeu,1, (*p).stack_type
	endif else begin
		writeu,1, el[q]
		writeu,1, (*p).energy_proxy_axis
		writeu,1, (*p).energies_file
	endelse
endif else begin
	if xanes then begin
		if ptr_good((*p).pz_label) then begin
			nz2 = min( [nq, n_elements(*(*p).pz_label)])
			b1 = byte(((*(*p).pz_label))[0:nz2-1])
		endif else b1 = byte(string(indgen(nq)))
		b = bytarr(8,nq)
		nb = n_elements(b1[*,0])
		if nb lt 8 then begin
			b[0:nb-1,*] = b1[0:nb-1,*]
		endif else begin
			b[0:7,*] = b1[0:7,*]
		endelse		
	endif else begin
		b1 = byte(el[q])
		b = bytarr(8,nq)
		nb = n_elements(b1[*,0])
		if nb lt 8 then begin
			b[0:nb-1,*] = b1[0:nb-1,*]
		endif else begin
			b[0:7,*] = b1[0:7,*]
		endelse		
	endelse
	writeu,1, b
endelse

if dump eq 0 then begin
	writeu,1, (*p).matrix.charge
	if (xanes eq 0) and ((*p).matrix.label ne '') and ((*p).matrix.charge ne 0.0) then begin
		mdl = ptr_good((*p).matrix.mdl) ? *(*p).matrix.mdl : 0.0
		nmdl = n_elements(mdl)
		mdl1 = fltarr( max([nmdl,(*p).n_el]) )
		mdl1[0:nmdl-1] = mdl
		mdl2 = mdl1[q]
		writeu,1, mdl2
	endif

	writeu, 1, (*p).ystep
	writeu, 1, (*p).xstep_on
	writeu, 1, (*p).xcompress, (*p).ycompress
	if xanes then writeu, 1, (*p).zcompress
	writeu, 1, (*p).xstep
	writeu, 1, (*p).matrix.file
	writeu, 1, (*p).step_events
	writeu, 1, (*p).events
	writeu, 1, (*p).step_toggle, (*p).toggle_bit, (*p).toggle_station
	writeu, 1, (*p).type
	writeu, 1, (*p).mode
	writeu, 1, (*p).channel, (*p).detector
	
	if xanes eq 0 then writeu, 1, (*p).corrected
	
	writeu, 1, (*p).has_errors
	writeu, 1, original_xsize, original_ysize
	if xanes then writeu, 1, (*p).original_zsize
	writeu, 1, (*p).scaled_x, (*p).scaled_y
	writeu, 1, (*p).show_back

	device_name = (*p).DevObj->name()
	writeu, 1, device_name
	writeu, 1, (*p).source2
	writeu, 1, (*p).throttle
	writeu, 1, (*p).pileup
	
	bounds = (*p).bounds
	if check_bounds then bounds.valid=0
	writeu, 1, bounds
	writeu, 1, (*p).linearize

; Maia object specific data now written at end of write, before images.
; writeu, 1, (*p).maia.encoder_y_correct, (*p).maia.x_margin
; writeu, 1, (*p).maia.clear.x, (*p).maia.clear.y

	writeu, 1, (*p).array
	if (*p).array eq 1 then begin
		writeu, 1, n_elements(*(*p).pactive)			; n_active
		writeu, 1, *(*p).pactive
		writeu, 1, *(*p).pcal
	
		if (xanes eq 0) then begin
			if ptr_good((*p).hist) then begin
				nh = n_elements(*(*p).hist)
				writeu, 1, nh
				if nh gt 0 then writeu, 1, *(*p).hist
			endif else begin
				writeu, 1, 0L
			endelse
		endif
	endif
endif

; Check consistency of flux, IC.conversion and charge ...

if (*p).has_flux then begin
	if (*p).IC.conversion eq 0.0 then (*p).IC.conversion = 1.0
	total_flux = total(*(*p).flux)
	print,'version -26: total(flux)=',total_flux,', conv=',(*p).IC.conversion,', flux*conv=',total_flux*(*p).IC.conversion,', charge=',(*p).charge
	if total_flux gt 1.0e-6 then begin
		if abs((*p).IC.conversion - 1.0) lt 0.001 then begin			; PIXE
;			(*p).IC.conversion = (*p).charge / total_flux
		endif else begin												; SXRF
			(*p).charge = total_flux * (*p).IC.conversion
		endelse
		print,'             total(flux)=',total_flux,', conv=',(*p).IC.conversion,', flux*conv=',total_flux*(*p).IC.conversion,', charge=',(*p).charge
	endif
endif

if dump eq 0 then begin
	writeu, 1, (*p).IC.mode
	if (*p).IC.mode ne 0 then begin
		writeu, 1, (*p).IC
	endif
	np = 0L
	if ptr_valid((*p).plist) then np = n_elements(*(*p).plist)
	writeu, 1, np
	if np gt 0 then begin
		writeu, 1, *(*p).plist
	endif
	writeu, 1, (*p).dwell
	writeu, 1, (*p).flatten

	dummy = {a:1.0, b:0.0}						; was deadtime_cal, now obsolete (in Device now)
	writeu, 1, dummy

	if xanes eq 0 then begin
		writeu, 1, (*p).energy
		writeu, 1, (*p).n_attributes
	endif
	
	writeu, 1, (*p).has_preview
	if (*p).has_preview eq 1 then begin
		gprint, 'write_geopixe_image: write preview ...'
		writeu,1, n_elements( (*(*p).preview)[*,0]), n_elements( (*(*p).preview)[0,*]) 
		writeu,1, *(*p).preview
	endif

;-------------- object specific parameter write -----------------

; 	Now all device specific options are stored and managed by the Object,
; 	and not explicitly known by GeoPIXE. This includes the Maia options.

	if dump eq 0 then begin
		gprint, 'write_geopixe_image: write device optional data ...'
		(*p).DevObj->write_options, 1, error=err2
		if err2 then goto, bad_obj_io
	endif

;----------------------------------------------------------------

	writeu, 1, (*p).has_flux
	if (*p).has_flux eq 1 then begin
		gprint, 'write_geopixe_image: write flux ...'
		if xanes then begin
			writeu, 1, (n_elements((*(*p).flux)[0,0,*]) eq 1) ? 1 : nq
			if n_elements((*(*p).flux)[0,0,*]) eq 1 then begin
				temp = (*(*p).flux)[0:xmax,0:ymax,0]
				writeu, 1, temp
			endif else begin
				for i=0L,nq-1 do begin
					gprint,'	',i
					temp = (*(*p).flux)[0:xmax,0:ymax,q[i]]
					writeu, 1, temp
				endfor
			endelse
		endif else begin
			writeu,1, (*(*p).flux)[0:xmax,0:ymax]
			if ptr_good( (*p).raw_flux) eq 0 then goto, bad_flux_io
			writeu,1, (*(*p).raw_flux)[0:xmax,0:ymax]
		endelse
	endif
	
	writeu, 1, (*p).has_dead
	if (*p).has_dead eq 1 then begin
		gprint, 'write_geopixe_image: write dead_fraction ...'
		writeu,1, (*(*p).dead_fraction)[0:xmax,0:ymax]
	endif

	writeu, 1, (*p).has_dwell
	if (*p).has_dwell eq 1 then begin
		gprint, 'write_geopixe_image: write dwell map ...'
		writeu,1, (*(*p).dwell_map)[0:xmax,0:ymax]
	endif
	
	writeu, 1, (*p).has_pileup
	if (*p).has_pileup eq 1 then begin
		gprint, 'write_geopixe_image: write pileup map ...'
		writeu,1, (*(*p).pileup_map)[0:xmax,0:ymax]
	endif
	
	writeu, 1, (*p).has_rates
	if (*p).has_rates eq 1 then begin
		gprint, 'write_geopixe_image: write count-rate map ...'
		writeu,1, (*(*p).count_rate_map)[0:xmax,0:ymax]
	endif
endif

if dump eq 0 then begin
	writeu, 1, (*p).has_phase
	if (*p).has_phase eq 1 then begin
		gprint, 'write_geopixe_image: write phase map ...'
		n_comp = n_elements( (*(*p).phase)[0,0,*])
		writeu, 1, n_comp
		if n_comp gt 0 then begin
			for i=0L,n_comp-1 do begin
				gprint,'	',i
				writeu,1, (*(*p).phase)[0:xmax,0:ymax,i]
			endfor
		endif
	endif

	writeu, 1, (*p).has_yield
	if (*p).has_yield eq 1 then begin
		gprint, 'write_geopixe_image: write yield map ...'
		nyi = n_elements( (*(*p).yield)[0,0,*])
		writeu, 1, nyi
		if nyi gt 0 then begin
			for i=0L,nyi-1 do begin
				gprint,'	',i
				writeu,1, (*(*p).yield)[0:xemax,0:yemax,i]
			endfor
		endif
	endif
endif

gprint, 'write_geopixe_image: write all images ...'
for i=0L,nq-1 do begin
	gprint,'	',i
	temp = (*(*p).image)[0:xmax,0:ymax,q[i]]
	writeu, 1, temp
endfor

if (*p).has_errors eq 1 then begin						; added to ver=-48 /dump
	gprint, 'write_geopixe_image: write all variance images ...'
	for i=0L,nq-1 do begin
		gprint,'	',i
		temp = (*(*p).error)[0:xemax,0:yemax,q[i]]
		writeu,1, temp
	endfor
endif
gprint, 'write_geopixe_image: write all images finished.'

;----------- Extra stuff for display, history -------------------

if dump or no_display then goto, done

gprint, 'write_geopixe_image: write display ranges ...'
writeu,1, (*(*p).options)[q].bottom, (*(*p).options)[q].top
writeu,1, (*(*p).options)[q].log

if (*p).has_errors eq 1 then begin
	writeu,1, (*(*p).escale)[q].bottom, (*(*p).escale)[q].top
	writeu,1, (*(*p).escale)[q].log
endif

gprint, 'write_geopixe_image: write history data ...'
for i=0L,nq-1 do begin
	n = 0L
	if ptr_valid( (*p).history) then begin
		if ptr_valid( (*(*p).history)[q[i]] ) then begin
			n = n_elements( *(*(*p).history)[q[i]] )
		endif
	endif

;	print,' write ',n,' history records for element ',(*(*p).el)[i]
	writeu,1, n
	if n gt 0 then begin
;		print,' history = ', *(*(*p).history)[i]
		writeu,1, *(*(*p).history)[q[i]]
	endif
endfor

done:
	err = 0

;----------------------------------------------------------------

finish:
	close_file,1

	if (err eq 0) and (cluster eq 0) then begin
		gprint,'write_geopixe_image: File saved. Rename from ='+file_temp
		gprint,'write_geopixe_image:                      to ='+file
		file_move, file_temp, file, /overwrite, /verbose
		
		f = file_search( file+'.*')							; delete .dai.0, .dai.1, ... temp files
		if no_delete then begin
			gprint,'write_geopixe_image: Deletion of cluster "stripes" is suppressed for now.'
		endif else begin
			if f[0] ne '' then file_delete, f, /quiet
		endelse
	endif
	return

bad_open:
	gprint,'write_geopixe_image: Error opening file: "'+file_temp[0]+'"'
	goto, finish
bad_io:
	gprint,'write_geopixe_image: I/O error'
	goto, finish
bad_obj_io:
	gprint,'write_geopixe_image: Object specific I/O error'
	goto, finish
bad_flux_io:
	gprint,'write_geopixe_image: No "raw flux" map found. Abort Image Save.'
	goto, finish
bad_file:
	gprint,'write_geopixe_image: no file name supplied'
	goto, usage

usage:
	print,'write_geopixe_image: Usage: write_geopixe_image, p, file'
	print,'		where "p" is pointer to image struct'
	print,'		and "file" is the name of the input file'
	goto, finish
end
