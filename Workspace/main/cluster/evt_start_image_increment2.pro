pro evt_start_image_increment2, pimg, p, init=init, flatten=flatten, path=path

;	Increment image 'pimg' by 'p' image, flux, charge, etc. data ...
;	(not really device independent code as it is now)
;
;	/init		expand image to full size for first call,
;				but preserve the yoffset for this image as global yoffset
;	/flatten	complete image by doing global 'flatten'
;	
;	First test if either has valid full arrays. Often segment 0 files
;	will have zero data (old Maia data-sets?) and hence may be missing
;	flux arrays, etc. May need to adopt arrays from next image.
;	
;	Preserve most of the pimg parameters in the final combined images.
;	This includes the xoffset, yoffset from the first image.
;	Remember that offsets are NOT compressed.

COMPILE_OPT STRICTARR
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
		warning,'evt_start_image_increment2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	if n_elements(init) eq 0 then init=0
	if n_elements(flatten) eq 0 then flatten=0
	if n_elements(xanes) eq 0 then xanes=0
	if n_elements(path) eq 0 then path=''

	xanes_stack_test, pimg, xanes, n_el, el, el_xanes, z_found=z_found
	if n_el lt 1 then begin
		gprint,'evt_start_image_increment2: No elements to increment.'
		return
	endif

	if flatten then begin

;		For a 'stack_type' of 2, which means that 'collapse_energy' was set and blank energy planes 
;		have not been collapsed yet, collapse planes onto set used in DA matrix stack.
;		Else look to see if there are energy planes that are not used, and remove them 
;		from 'image' and the Z axis position list 'pz_coords' set from 'translate_energy2'.
;		This is done here in cluster mode, and not in 'da_evt_stack'.
;		NOTE: This only works for Y axis as slowest axis for YLUT to be valid.

		if tag_present( 'stack_type', *pimg) then begin
			if (((*pimg).stack_type eq 0) or ((*pimg).stack_type eq 2)) and ptr_valid((*pimg).pz_coords) then begin
				zrange3 = n_elements( (*(*pimg).image)[0,0,*])
				file = file_requester(/read, filter='*.'+extract_extension((*pimg).matrix.file), path=path, $
							title='Select matrix file for XANES energies', file=(*pimg).matrix.file, /skip_if_exists)
				if file eq '' then begin
					warning,'','Failed to find matrix file "'+(*pimg).matrix.file+'" for reconstruction.'
					return
				endif
				matrix = read_da( file, eDA=eDA, error=err)
				if err eq 0 then begin
					n_energy = n_elements(eDA)
				endif else begin
					gprint,'evt_start_image_increment2: Error reading matrix file.'
					n_energy = zrange3
					(*pimg).stack_type = 0
				endelse
				if ((*pimg).stack_type eq 2) then begin					; collapse energy to DA planes
					xrange3 = n_elements( (*(*pimg).image)[*,0,0])
					yrange3 = n_elements( (*(*pimg).image)[0,*,0])
					xrange3e = (xrange3+1)/2
					yrange3e = (yrange3+1)/2

					translate_energy2 = *(*pimg).pz_coords
					e_lookup = uintarr(zrange3)
					for j=0,zrange3-1 do begin
						e_lookup[j] = binary_search( eDA, translate_energy2[j])
					endfor		

					image2 = fltarr(xrange3,yrange3,n_energy)
					image2_error = fltarr(xrange3e,yrange3e,n_energy)
					flux2 = fltarr(xrange3,yrange3,n_energy)
					pz = fltarr(n_energy)
					sum = pz
					for i=0,zrange3-1 do begin
						image2[*,*,e_lookup[i]] += (*(*pimg).image)[*,*,i]
						image2_error[*,*,e_lookup[i]] += (*(*pimg).error)[*,*,i]
						flux2[*,*,e_lookup[i]] += (*(*pimg).flux)[*,*,i]
						pz[e_lookup[i]] +=  translate_energy2[i] * total((*(*pimg).flux)[*,*,i])
						sum[e_lookup[i]] += total((*(*pimg).flux)[*,*,i])
					endfor
					qz = where( sum gt 1.e-10, nqz)
					if nqz gt 0 then begin
						pz = pz[qz] / sum[qz]
						(*(*pimg).image) = image2[*,*,qz]
						(*(*pimg).error) = image2_error[*,*,qz]
						(*(*pimg).flux) = flux2[*,*,qz]
					endif
					qe = sort( pz)
					(*(*pimg).image) = (*(*pimg).image)[*,*,qe]
					(*(*pimg).error) = (*(*pimg).error)[*,*,qe]
					(*(*pimg).flux) = (*(*pimg).flux)[*,*,qe]
					*(*pimg).pz_coords = pz[qe]
					(*pimg).zsize = n_elements(pz)
				endif else begin
					good = intarr(zrange3)
					ave = total((*(*pimg).flux)) / zrange3				; remove blank planes from XANES stack
					for i=0,zrange3-1 do begin
						sum = total( (*(*pimg).flux)[*,*,i])
						if sum gt 0.01 * ave then good[i] = 1
					endfor
					qz = where( good eq 1, nqz)
					if nqz gt 0 then begin
						pz = (*(*pimg).pz_coords)[qz]
						qe = sort( pz)
						(*(*pimg).image) = (*(*pimg).image)[*,*,qz[qe]]
						(*(*pimg).error) = (*(*pimg).error)[*,*,qz[qe]]
						(*(*pimg).flux) = (*(*pimg).flux)[*,*,qz[qe]]
						*(*pimg).pz_coords = pz[qe]
						(*pimg).zsize = nqz
					endif
				endelse
			endif
		endif

;		Flatten image according to flux variation. Flux is assumed to be already corrected for
;		dead-time and pile-up. This Flatten is assumed to be done last after all images are combined.

		if (*pimg).has_flux then begin
			flux = *(*pimg).flux
			charge = (*pimg).charge
			if xanes then begin
				image_correct_flux_stack, pimg, flux, (*pimg).IC, *(*pimg).plist, charge=charge, /flatten
			endif else begin
				raw = *(*pimg).raw_flux
				image_correct_flux, pimg, flux, (*pimg).IC, *(*pimg).plist, charge=charge, $
						/flatten, raw=raw, /cluster_pass
			endelse
		endif
		if (*pimg).has_phase then begin
			*(*pimg).phase = check_phase( *(*pimg).phase)				;@3-16
		endif
		return
	endif

;	The original size is the total size in cluster mode, which may be a region window size
;	The offsets for the full image (or the partial 'window' image) are preserved in the first /init image

	if (*pimg).sub_region then begin				; sub-region, with no compression
		xs = (*pimg).x_sub_range
		ys = (*pimg).y_sub_range
	endif else begin								; normal image
		xs = (*pimg).original_xsize
		ys = (*pimg).original_ysize
	endelse

	if init then begin		
		image = fltarr(xs, ys, n_el)
		if (*pimg).has_errors then begin
			image_error = fltarr((xs+1)/2, (ys+1)/2, n_el)
		endif
		if (*pimg).has_flux then begin
			if xanes then begin
				flux = fltarr(xs, ys, n_el)
			endif else begin
				flux = fltarr(xs, ys)
				raw_flux = fltarr(xs, ys)
			endelse
		endif
		if (*pimg).has_dead then begin
			dead_fraction = fltarr(xs, ys)
		endif
		if (*pimg).has_dwell then begin
			dwell_map = fltarr(xs, ys)
		endif
		if (*pimg).has_pileup then begin
			pileup_map = fltarr(xs, ys)
		endif
		if (*pimg).has_rates then begin
			count_rate_map = fltarr(xs, ys)
		endif
		if (*pimg).has_phase then begin
			n_comp = n_elements( (*(*pimg).phase)[0,0,*])
			phase = fltarr(xs, ys, n_comp)
		endif
		if (*pimg).has_yield then begin
			yield = fltarr((xs+1)/2, (ys+1)/2, n_el-(*pimg).n_attributes)
		endif
		
;		Do image assignments one plane at a time to avoid huge memory allocations.
;		Note that 'y1' starts at zero, and we preserve the yoffset for this first '/init' image.
;		Remember that offsets are NOT compressed.

		y1 = 0L < (ys - 1)
		y2 = (y1 + (*pimg).ysize-1) < (ys - 1)
		dy = (y2-y1) < ((*pimg).ysize - 1)
		y2 = y2 < (y1 + dy)
		for i=0,n_el-1 do begin
			image[*,y1:y2,i] = (*(*pimg).image)[*,0:dy,i]
		endfor
		ptr_free, (*pimg).image
		(*pimg).image = ptr_new(image,/no_copy)	
		if (*pimg).has_errors then begin
			y1e = (y1+1)/2 < ((ys+1)/2 - 1)
			y2e = ((y2+1)/2) < ((ys+1)/2 - 1) 
			dye = (y2e-y1e) < (((*pimg).ysize+1)/2 - 1)
			y2e = y2e < (y1e + dye)
			for i=0,n_el-1 do begin
				image_error[*,y1e:y2e,i] = (*(*pimg).error)[*,0:dye,i]
			endfor
			ptr_free, (*pimg).error
			(*pimg).error = ptr_new(image_error,/no_copy)
		endif
		if (*pimg).has_flux then begin
			if xanes then begin
				flux[*,y1:y2,*] = (*(*pimg).flux)[*,0:dy,*]
				ptr_free, (*pimg).flux
				(*pimg).flux = ptr_new(flux,/no_copy)
			endif else begin
				flux[*,y1:y2] = (*(*pimg).flux)[*,0:dy]
				ptr_free, (*pimg).flux
				(*pimg).flux = ptr_new(flux,/no_copy)
				if ptr_good((*pimg).raw_flux) then begin
					raw_flux[*,y1:y2] = (*(*pimg).raw_flux)[*,0:dy]
				endif
				ptr_free, (*pimg).raw_flux
				(*pimg).raw_flux = ptr_new(raw_flux,/no_copy)
			endelse
		endif
		if (*pimg).has_dead then begin
			dead_fraction[*,y1:y2] = (*(*pimg).dead_fraction)[*,0:dy]
			ptr_free, (*pimg).dead_fraction
			(*pimg).dead_fraction = ptr_new(dead_fraction,/no_copy)
		endif
		if (*pimg).has_dwell then begin
			dwell_map[*,y1:y2] = (*(*pimg).dwell_map)[*,0:dy]
			ptr_free, (*pimg).dwell_map
			(*pimg).dwell_map = ptr_new(dwell_map,/no_copy)
		endif
		if (*pimg).has_pileup then begin
			pileup_map[*,y1:y2] = (*(*pimg).pileup_map)[*,0:dy]
			ptr_free, (*pimg).pileup_map
			(*pimg).pileup_map = ptr_new(pileup_map,/no_copy)
		endif
		if (*pimg).has_rates then begin
			count_rate_map[*,y1:y2] = (*(*pimg).count_rate_map)[*,0:dy]
			ptr_free, (*pimg).count_rate_map
			(*pimg).count_rate_map = ptr_new(count_rate_map,/no_copy)
		endif
		if (*pimg).has_phase then begin
			for i=0,n_comp-1 do begin
				phase[*,y1:y2,i] = (*(*pimg).phase)[*,0:dy,i]
			endfor
			ptr_free, (*pimg).phase
			(*pimg).phase = ptr_new(phase,/no_copy)
		endif
		if (*pimg).has_yield then begin
			for i=0,(n_el-(*pimg).n_attributes)-1 do begin
				yield[*,y1e:y2e,i] = (*(*pimg).yield)[*,0:dye,i]
			endfor
			ptr_free, (*pimg).yield
			(*pimg).yield = ptr_new(yield,/no_copy)
		endif
		(*pimg).xsize = xs
		(*pimg).ysize = ys

		if ptr_good((*pimg).py_coords) then begin
			y_coords = dblarr(ys)
			if n_elements(*(*pimg).py_coords) eq y2-y1+1 then begin
				y_coords[y1:y2] = *(*pimg).py_coords
			endif else begin
				print, 'evt_start_image_increment2: py_coords (init) number do not match.'
			endelse
			(*pimg).py_coords = ptr_new(y_coords,/no_copy)
		endif

;		Need to preserve the original /init image Y and X offsets

		if (*pimg).has_preview and ptr_good((*pimg).preview) then begin
			(*pimg).has_preview = 0
			ptr_free, (*pimg).preview
		endif
		return
	endif
	
;	Make arrays if first image file lacks these ...

	if (*pimg).has_flux eq 0 then begin
		if (*p).has_flux then begin
			if xanes then begin
				(*pimg).flux = ptr_new( fltarr(xs, ys, n_el))
				(*pimg).has_flux = 1
			endif else begin
				(*pimg).flux = ptr_new( fltarr(xs, ys))
				(*pimg).raw_flux = ptr_new( fltarr(xs, ys))
				(*pimg).has_flux = 1
			endelse
		endif
	endif
	if ((*pimg).has_dead eq 0) then begin
		if (*p).has_dead then begin
			(*pimg).dead_fraction = ptr_new( fltarr(xs, ys))
			(*pimg).has_dead = 1
		endif
	endif
	if ((*pimg).has_dwell eq 0) then begin
		if (*p).has_dwell then begin
			(*pimg).dwell_map = ptr_new( fltarr(xs, ys))
			(*pimg).has_dwell = 1
		endif
	endif
	if ((*pimg).has_pileup eq 0) then begin
		if (*p).has_pileup then begin
			(*pimg).pileup_map = ptr_new( fltarr(xs, ys))
			(*pimg).has_pileup = 1
		endif
	endif
	if ((*pimg).has_rates eq 0) then begin
		if (*p).has_rates then begin
			(*pimg).count_rate_map = ptr_new( fltarr(xs, ys))
			(*pimg).has_rates = 1
		endif
	endif
	if (*pimg).has_phase eq 0 then begin
		if (*p).has_phase then begin
			n_comp = n_elements( (*(*pimg).phase)[0,0,*])
			(*pimg).phase = ptr_new( fltarr(xs, ys, n_comp))
			(*pimg).has_phase = 1
		endif
	endif
	if (*pimg).has_yield eq 0 then begin
		if (*p).has_yield then begin
			(*pimg).yield = ptr_new( fltarr((xs+1)/2, (ys+1)/2, n_el-(*pimg).n_attributes))
			(*pimg).has_yield = 1
		endif
	endif
	if xanes eq 0 then begin
		if ptr_good((*pimg).hist) eq 0 then begin
			if ptr_good((*p).hist) then begin
				(*pimg).hist = ptr_new( *(*p).hist)
				(*(*pimg).hist)[*] = 0.0
			endif else begin
				(*pimg).hist = ptr_new( fltarr(384))
			endelse
		endif
	endif
	if ptr_good((*pimg).plist) eq 0 then begin
		if ptr_good((*p).plist) then begin
			(*pimg).plist = ptr_new( *(*p).plist)
			(*pimg).ic = (*p).ic
			(*pimg).dwell = (*p).dwell
		endif
	endif

	if ptr_good((*pimg).py_coords) eq 0 then begin
		if ptr_good((*p).py_coords) then begin
			y_coords = dblarr(ys)
			(*pimg).py_coords = ptr_new(y_coords,/no_copy)
		endif
	endif

;	Increment images (and flux, hist, error) with next image data ...
;	Do image increments one plane at a time to avoid huge memory allocations.
;	Do not assemble preview images - this will be done on write when one is not found.
;	
;	The Y offset for each new image stripe must subtract the first image (*pimg).yoffset.
;	Remember that offsets are NOT compressed.

	yo = ((*p).yoffset - (*pimg).yoffset) / (*pimg).ycompress
	y1 = yo < (ys - 1)
	y2 = (y1 + (*p).ysize-1) < (ys - 1)
	dy = (y2-y1) < ((*p).ysize - 1)
	y2 = y2 < (y1 + dy)
	for i=0,n_el-1 do begin
		(*(*pimg).image)[*,y1:y2,i] += (*(*p).image)[*,0:dy,i]
	endfor
	
	if (*pimg).has_errors and (*p).has_errors then begin
		y1e = (y1+1)/2 < ((ys+1)/2 - 1)
		y2e = ((y2+1)/2) < ((ys+1)/2 - 1) 
		dye = (y2e-y1e) < (((*p).ysize+1)/2 - 1)
		y2e = y2e < (y1e + dye)
		for i=0,n_el-1 do begin
			(*(*pimg).error)[*,y1e:y2e,i] += (*(*p).error)[*,0:dye,i]
		endfor
	endif
	
	if (*pimg).has_flux and (*p).has_flux then begin
		if xanes then begin
			(*(*pimg).flux)[*,y1:y2,*] += (*(*p).flux)[*,0:y2-y1,*]
		endif else begin
			(*(*pimg).flux)[*,y1:y2] += (*(*p).flux)[*,0:y2-y1]
			if ptr_good((*pimg).raw_flux) and ptr_good((*p).raw_flux) then begin
				(*(*pimg).raw_flux)[*,y1:y2] += (*(*p).raw_flux)[*,0:dy]
			endif
		endelse
	endif
	if (*pimg).has_dead and (*p).has_dead then begin
		(*(*pimg).dead_fraction)[*,y1:y2] += (*(*p).dead_fraction)[*,0:y2-y1]
	endif
	if (*pimg).has_dwell and (*p).has_dwell then begin
		(*(*pimg).dwell_map)[*,y1:y2] += (*(*p).dwell_map)[*,0:y2-y1]
	endif
	if (*pimg).has_pileup and (*p).has_pileup then begin
		(*(*pimg).pileup_map)[*,y1:y2] += (*(*p).pileup_map)[*,0:y2-y1]
	endif
	if (*pimg).has_rates and (*p).has_rates then begin
		(*(*pimg).count_rate_map)[*,y1:y2] >= (*(*p).count_rate_map)[*,0:y2-y1]		; accept larger
	endif
	if (*pimg).has_phase and (*p).has_phase then begin
		n_comp = n_elements( (*(*pimg).phase)[0,0,*])
		for i=0,n_comp-1 do begin
			(*(*pimg).phase)[*,y1:y2,i] += (*(*p).phase)[*,0:y2-y1,i]
		endfor
	endif
	
;	Combine average yield maps. Accept maximum in a pixel to avoid zero ones.
	if (*pimg).has_yield and (*p).has_yield then begin
		for i=0,(n_el-(*pimg).n_attributes)-1 do begin
			(*(*pimg).yield)[*,y1e:y2e,i] = (*(*pimg).yield)[*,y1e:y2e,i] > (*(*p).yield)[*,0:dye,i]	;@10-16

;			Test for overlap, and divide by 2 there to maintain average.		; This was very bad!
;			nex = long((xs+1)/2) 
;			nexy = nex * long((ys+1)/2)											;@3-16
;			(*(*pimg).yield)[*,y1e:y2e,i] += (*(*p).yield)[*,0:dye,i]
;			q = where( ((*(*pimg).yield)[*,y1e:y2e,i] gt 1.0e-6) and ((*(*p).yield)[*,0:dye,i] gt 1.0e-6), nq)
;			if nq gt 0 then (*(*pimg).yield)[q + y1e*nex + i*nexy] /= 2
		endfor
	endif
	if xanes eq 0 then begin
		if ptr_good((*pimg).hist) and ptr_good((*p).hist) then begin
			*(*pimg).hist += *(*p).hist
		endif
	endif
	
;	Add charge, as charge is calculated in da_evt from flux for stripe only ...	
	(*pimg).charge += (*p).charge

	(*pimg).valid += (*p).valid
	(*pimg).processed += (*p).processed
	(*pimg).bad_xy += (*p).bad_xy
	(*pimg).clipped += (*p).clipped
	
	if (*p).processed gt 0 then begin
		if (*p).bounds.xmin ge 0 then (*pimg).bounds.xmin = (*pimg).bounds.xmin < (*p).bounds.xmin		; these are non-offset units
		if (*p).bounds.xmax ge 0 then (*pimg).bounds.xmax = (*pimg).bounds.xmax > (*p).bounds.xmax
		if (*p).bounds.ymin ge 0 then (*pimg).bounds.ymin = (*pimg).bounds.ymin < (*p).bounds.ymin
		if (*p).bounds.ymax ge 0 then (*pimg).bounds.ymax = (*pimg).bounds.ymax > (*p).bounds.ymax
	endif
	
;	Accumulate y_coords, as these get split between stripes.

	if ptr_good((*p).py_coords) then begin
		if n_elements(*(*p).py_coords) eq y2-y1+1 then begin
			(*(*pimg).py_coords)[y1:y2] = *(*p).py_coords
		endif else begin
			print, 'evt_start_image_increment2: py_coords (increment) number do not match.'
		endelse
	endif

;	Fix source file names to be the smallest and largest segment files ...
;	This is NOT a device-indepedent way to do this. Will need to revise

	n1 = long2( extract_extension( (*pimg).source))							; revise later, device independent way
	n1b = long2( extract_extension( (*p).source))
	if n1b lt n1 then begin
		(*pimg).source = strip_file_ext((*pimg).source) + '.' + str_tidy(n1b)
	endif
	n2 = long2( extract_extension( (*pimg).source2))
	n2b = long2( extract_extension( (*p).source2))
	if n2b gt n2 then begin
		(*pimg).source2 = strip_file_ext((*pimg).source2) + '.' + str_tidy(n2b)
	endif
	return
end
