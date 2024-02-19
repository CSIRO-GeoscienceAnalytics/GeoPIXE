pro export_images_csv, pstate, file=file, first=first, event=event, plot=plot

;   Export images as a comma or tab-separated ascii file
;	If 'plot' specified, then just plot this region ID of region table only.

	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	p = (*pstate).p
	if ptr_valid(p) eq 0 then return
	xanes_stack_test, p, xanes, n_els, el, el_xanes
	if n_elements(plot) eq 0 then plot=0
	
	if plot then begin
		title = 'Select Element Images to Export '
		if ptr_good((*pstate).pselect2) then select = (*(*pstate).pselect2).select
	
		select = element_select( event.top, el, old_select=select, title=title, path=*(*pstate).path )
		q = where(select eq 1)
		if q[0] eq -1 then return
	
		collapse = 1
		clip_zero  = 0
		mode = 1
		xy = 1
		option = 2
		tab = 0
	
		*(*pstate).pselect2 = {select:select, mode:mode, option:option, collapse:collapse, clip_zero:clip_zero, tab:tab}
		goto, more
	endif
	
	if first then begin
		title = 'Select Element Images to Export '
		if ptr_good((*pstate).pselect2) then select = (*(*pstate).pselect2).select
	
		select = element_select( event.top, el, old_select=select, title=title, path=*(*pstate).path )
		q = where(select eq 1)
		if q[0] eq -1 then return
	
		help_default = 'Select Export mode.'
		explanation = 'There are two modes of Export: "Table" mode (export whole image rectangle, optionally zeroing outside region shape) and "XY" mode (list X,Y pixel coordinates and column(s) for image data). ' + $
				'Each selected element will output a separate export table. A further pop-up will provide more options.'
		drop = ['"Table" mode (export whole image rectangle)','"XY" mode (X,Y coordinates and image data)']
		help_drop = 'Select "Table" for whole image (options permit to zero outside of shape). Select "XY" mode for a list of X,Y pixel coordinates and column(s) for image or region(s) data.'
	
		r = options_popup( event.top, title='Image Export options', drop=drop, help_drop=help_drop, path=*(*pstate).path, $
				help_default=help_default, explanation=explanation, min_xsize=400, error=error)
		if error then return
	
		mode = r.drop[0]
		collapse = 0
		clip_zero  = 0
		tab = 0
	
		if mode eq 0 then begin
			help_default = 'Select Table Export options and click "OK" to export.'
			explanation = 'Select options for Table export mode. Exports all image pixels and optionally clear/zero pixels outside shape.'
			drop = ['Pixels within current Shape displayed on image','All image pixels']
			help_drop = 'Select the pixels to export: All pixels or pixels in the current Shape. For the latter, pixels outside the shape will be zeroed on output.'
			check = ['Clip negatives','Tab delimited file']
			help_check = 'Select options for clipping negative image values at zero and the output file format, which defaults to comma delimited CSV.'
			initial_check = [0,0]
		
			r = options_popup( event.top, title='Table Export options', drop=drop, help_drop=help_drop, path=*(*pstate).path, $
					help_default=help_default, check=check, help_check=help_check, initial_check=initial_check, $
					explanation=explanation, min_xsize=400, error=error)
			if error then return
		
			option = r.drop[0]
			clip_zero  = r.check[0]
			tab = r.check[1]
		endif else begin
			help_default = 'Select XY Export options and click "OK" to export.'
			explanation = 'Select options for XY list export mode. Export all pixels, those in current shape or region(s), and optionally collapse and average along the non-E axis for images with an Energy Proxy Axis.'
			drop = ['Pixels within current Shape displayed on image','All image pixels','Pixels for current Image Region','Pixels for All Image Regions']
			help_drop = 'Select the pixels to export: All pixels or pixels in the current Shape, or use the Image Regions table and ' + $
				'export the current region pixels or pixels for all regions as separate files (or columns for XANES). For regions, they can be shapes or Association highlighted pixels.'
			check = ['Collapse / average non-E axis','Clip negatives','Tab delimited file']
			help_check = 'Select options for averaging, clipping negatives at zero and output file format, which defaults to comma delimited CSV. ' + $
				'Note: "Collapse" is restricted to the case of a non-zero "Energy Proxy Axis" (e.g. XANES).'
			initial_check = [1,0,0]
		
			r = options_popup( event.top, title='XY Export options', drop=drop, help_drop=help_drop, path=*(*pstate).path, $
					help_default=help_default, check=check, help_check=help_check, initial_check=initial_check, $
					explanation=explanation, min_xsize=400, error=error)
			if error then return
		
			option = r.drop[0]
			collapse = r.check[0]
			clip_zero  = r.check[1]
			tab = r.check[2]
		endelse
	
		*(*pstate).pselect2 = {select:select, mode:mode, option:option, collapse:collapse, clip_zero:clip_zero, tab:tab}
	endif
	
	select = (*(*pstate).pselect2).select			; elements
	mode = (*(*pstate).pselect2).mode				; Table (0) or XY (1) mode
	option = (*(*pstate).pselect2).option			; Export option (Table: 0/pixels in shape, 1/all pixels; XY: 0/pixels in shape, 1/all pixels, 
													;				2/pixels for currnt region, 3/pixels for all regions)
	collapse = (*(*pstate).pselect2).collapse		; collapse non-E axis
	clip_zero = (*(*pstate).pselect2).clip_zero		; Output only positive values
	tab = (*(*pstate).pselect2).tab					; tab delimited output file
	
	xy = (mode eq 1)								; XY mode selected

more:
	ctab = tab ? string(9B) : ','
	
	q = where(select eq 1)
	if q[0] eq -1 then return
	
	path = extract_path(file)
	if (plot eq 0) and (file_test(path,/dir) eq 0) then begin
		safe_file_mkdir, path, error=error
		if error then begin
			warning,'export_images_csv',['Illegal directory name:',path]
		endif
	endif
	
	nq = n_elements(q)
	scounts = strlowcase( special_elements(/counts))
	ctype = bytarr(nq)
	for i=0,nq-1 do begin
		qt = where( el[q[i]] eq scounts, nqt1)
		if nqt1 ge 1 then ctype[i] = 1
	endfor
	
	image_flux_charge, p, xanes=xanes, charge=charge, pixels=pixels, conv=conv, flux=flux

;	'image_flux_charge' uses the whole image array
;	'pixels' refers to main 'image' array, not 'error'

	if charge lt 1.0e-10 then charge = 1.0
	charge_per_pixel = charge / float(pixels)
	nt = long((*p).xsize) * long((*p).ysize)

;	In XY mode, pop-up a few more options for collapse/average on non-E axes, etc.
;	for the moment XY mode is synonymous with use_regions.

	if xy then begin
		case option of
			0: begin									; current shape
				shape = 1
				use_region = 0
				all_regions = 0
				end
			1: begin									; all image pixels
				shape = 0
				use_region = 0
				all_regions = 0
				end
			2: begin									; current region table row
				shape = 0
				use_region = 1
				all_regions = 0
				end
			3: begin									; all region table rows
				shape = 0
				use_region = 1
				all_regions = 1
				end
		endcase
	endif else begin
		case option of
			0: begin									; current shape
				shape = 1
				use_region = 0
				all_regions = 0
				end
			1: begin									; all image pixels
				shape = 0
				use_region = 0
				all_regions = 0
				end
		endcase
	endelse
	
;	Test for Line XANES case ...

	if xanes eq 0 then begin
		lxanes = ((*p).energy_proxy_axis ne 0)					; no proxy axis means not line XANES
	endif else begin
		lxanes = 0
	endelse

;	First branch for the special case of XY mode, not Line XANES and multiple regions. Output region files with element columns.

	if xy and (lxanes eq 0) then goto, special			; special case, will use element columns in region files.

;	For all other cases, use element files and possibly region columns (e.g. XANES).

	for jq=0L,nq-1 do begin			; ---------------- loop over element ---------------
	
		use_shape = 0
		nq2 = 0L
		major_y = 1
		major_x = 0
		
		if shape then begin
			if ((*pstate).corr_mode eq 0) then begin
				if (*pstate).analyze_mode eq 0 then begin		; include mode
					mark_vertices, pstate, x1,y1, n
					if n eq 0 then return
					x = round(x1)
					y = round(y1)
		
					q1 = where(x eq (*p).xsize-1)               ; Correct for flaw in polyfillv
					if q1[0] ne -1 then x[q1]=x[q1]+1           ; where it ignores the last column
					q1 = where(y eq (*p).ysize-1)               ; or row when all selected.
					if q1[0] ne -1 then y[q1]=y[q1]+1
		
					q2 = polyfillv( x,y, (*p).xsize,(*p).ysize)
					if q2[0] ne -1 then begin
						mask = bytarr((*p).xsize,(*p).ysize)
						mask[q2] = 1
						nq2 = n_elements(q2)
						q3 = where(mask ne 1, nq3)
						use_shape = 1
					endif else goto, bad_q2
					q_to_xy, q2, (*p).xsize, x,y
				endif
			endif
			if (use_shape ne 1) then goto, bad_region
		
			nxr = (*p).xsize
			nyr = (*p).ysize
			nt2 = long(nxr) * long(nyr)
		
			image = fltarr(nxr,nyr)
			norm = fltarr(nxr,nyr)
			image[q2] = (*(*p).image)[ q2 + nt2*q[jq] ]
			if ctype[jq] eq 0 then image[*,*] /= charge_per_pixel
			if clip_zero then image[*,*] >= 0.0
			norm[q2] = 1.
			collapse_done = 0
		
			if collapse then begin
				case (*p).energy_proxy_axis of
					1: begin										; X axis
						for j=0,nxr-1 do begin
							image[j,y[0]] = total( image[j,*])
							norm[j,y[0]] = total( norm[j,*])
							if y[0] gt 0 then begin
								image[j,0:y[0]-1] = 0.
								norm[j,0:y[0]-1] = 0.
							endif
							if y[0] lt nyr-1 then begin
								image[j,y[0]+1:nyr-1] = 0.
								norm[j,y[0]+1:nyr-1] = 0.
							endif
						endfor
						image[*,0] = image[*,0] / (norm[*,0] > 1.)
						collapse_done = 1
						major_x = 1
						q2 = where( image ne 0, nq2)
						q_to_xy, q2, nxr, x,y
						end
					2: begin										; Y axis
						for j=0,nyr-1 do begin
							image[x[0],j] = total( image[*,j])
							norm[x[0],j] = total( norm[*,j])
							if x[0] gt 0 then begin
								image[0:x[0]-1,j] = 0.
								norm[0:x[0]-1,j] = 0.
							endif
							if x[0] lt nxr-1 then begin
								image[x[0]+1:nxr-1,j] = 0.
								norm[x[0]+1:nxr-1,j] = 0.
							endif
						endfor
						image[x[0],*] = image[x[0],*] / (norm[x[0],*] > 1.)
						collapse_done = 1
						major_y = 1
						q2 = where( image ne 0, nq2)
						q_to_xy, q2, nxr, x,y
						end
					else:
				endcase
			endif

;			For Project X or Y, collapse image down onto that axis (even without collapse mode set) and export that line ...

			if ((*pstate).analyze_type[0] eq 8) or ((*pstate).analyze_type[0] eq 9) and (collapse_done eq 0) then begin
		
				case (*pstate).analyze_type[0] of
					9: begin									; project Y
						for j=0,nyr-1 do begin
							image[x[0],j] = total( image[*,j])
							norm[x[0],j] = total( norm[*,j])
							if x[0] gt 0 then begin
								image[0:x[0]-1,j] = 0.
								norm[0:x[0]-1,j] = 0.
							endif
							if x[0] lt nxr-1 then begin
								image[x[0]+1:nxr-1,j] = 0.
								norm[x[0]+1:nxr-1,j] = 0.
							endif
						endfor
						image[x[0],*] = image[x[0],*] / (norm[x[0],*] > 1.)
						q2 = where( image ne 0, nq2)
						q_to_xy, q2, nxr, x,y
						major_y = 1
						end
					8: begin									; project X
						for j=0,nxr-1 do begin
							image[j,y[0]] = total( image[j,*])
							norm[j,y[0]] = total( norm[j,*])
							if y[0] gt 0 then begin
								image[j,0:y[0]-1] = 0.
								norm[j,0:y[0]-1] = 0.
							endif
							if y[0] lt nyr-1 then begin
								image[j,y[0]+1:nyr-1] = 0.
								norm[j,y[0]+1:nyr-1] = 0.
							endif
						endfor
						image[*,0] = image[*,0] / (norm[*,0] > 1.)
						q2 = where( norm ne 0, nq2)
						q_to_xy, q2, nxr, x,y
						major_x = 1
						end
					else:
				endcase
		
				mask = bytarr(nxr,nyr)
				mask[q2] = 1
				nq2 = n_elements(q2)
				q3 = where(mask ne 1, nq3)
			endif
		
			if xy then begin
				for k=0,nq2-1 do begin
					if n_elements(list) eq 0 then begin
						list = {x:x[k], y:y[k], data:image[x[k],y[k]], col:0}
					endif else begin
						list = [list, {x:x[k], y:y[k], data:image[x[k],y[k]], col:0} ]
					endelse
				endfor
			endif
		
		endif else if (xy eq 0) then begin
			nxr = (*p).xsize
			nyr = (*p).ysize
			offx = 0
			offy = 0
			nt2 = long(nxr) * long(nyr)
			image = fltarr( nxr,nyr)
			image[*,*] = (*(*p).image)[*,*,q[jq]]
		    if clip_zero then image[*,*] >= 0.0
		    if ctype[jq] eq 0 then image[*,*] /= charge_per_pixel
			mask = bytarr(nxr,nyr)
			mask[*] = 1
			q2 = where(mask eq 1, nq2)
		    q_to_xy, q2, (*p).xsize, x,y
		endif
	
		if use_region then begin
			nreg = n_elements( *(*pstate).pregions)
			if ptr_good( (*pstate).pregions) eq 0 then goto, bad_region
			if all_regions then begin
				i1 = 0
				i2 = nreg-1
				reg = indgen(nreg)
				ncols = nreg
			endif else begin
				i1 = 0
				i2 = 0
				reg = (*pstate).region_id
				nreg = 1
				ncols = 1
			endelse
		
			for ir=i1,i2 do begin
				jr = reg[ir]
				pr = (*(*pstate).pregions)[jr]
				nxr = (*pr).nx
				nyr = (*pr).ny
				q2 = *(*pr).q
				q_to_xy, q2, nxr, x,y
				nq2 = n_elements(q2)
				nt2 = long(nxr) * long(nyr)
				xoff = min(x)
				yoff = min(y)
		
				image = fltarr(nxr,nyr)
				norm = fltarr(nxr,nyr)
				image[q2] = (*(*p).image)[ q2 + nt2*q[jq] ]
				if ctype[jq] eq 0 then image[*,*] /= charge_per_pixel
				if clip_zero then image[*,*] >= 0.0
				norm[q2] = 1.
				collapse_done = 0
		
				if collapse then begin
					case (*p).energy_proxy_axis of
						1: begin										; X axis
							for j=0,nxr-1 do begin
								image[j,y[0]] = total( image[j,*])
								norm[j,y[0]] = total( norm[j,*])
								if y[0] gt 0 then begin
									image[j,0:y[0]-1] = 0.
									norm[j,0:y[0]-1] = 0.
								endif
								if y[0] lt nyr-1 then begin
									image[j,y[0]+1:nyr-1] = 0.
									norm[j,y[0]+1:nyr-1] = 0.
								endif
							endfor
							image[*,0] = image[*,0] / (norm[*,0] > 1.)
							collapse_done = 1
							major_x = 1
							q2 = where( image ne 0, nq2)
							q_to_xy, q2, nxr, x,y
							end
						2: begin										; Y axis
							for j=0,nyr-1 do begin
								image[x[0],j] = total( image[*,j])
								norm[x[0],j] = total( norm[*,j])
								if x[0] gt 0 then begin
									image[0:x[0]-1,j] = 0.
									norm[0:x[0]-1,j] = 0.
								endif
								if x[0] lt nxr-1 then begin
									image[x[0]+1:nxr-1,j] = 0.
									norm[x[0]+1:nxr-1,j] = 0.
								endif
							endfor
							image[x[0],*] = image[x[0],*] / (norm[x[0],*] > 1.)
							collapse_done = 1
							major_y = 1
							q2 = where( image ne 0, nq2)
							q_to_xy, q2, nxr, x,y
							end
						else:
					endcase
				endif

;				For Project X or Y, collapse image down onto that axis (even without collapse mode set) and export that line ...

				if ((*pr).analyze_type[0] eq 8) or ((*pr).analyze_type[0] eq 9) and (collapse_done eq 0) then begin
			
					case (*pr).analyze_type[0] of
						9: begin									; project Y
							for j=0,nyr-1 do begin
								image[x[0],j] = total( image[*,j])
								norm[x[0],j] = total( norm[*,j])
								if x[0] gt 0 then begin
									image[0:x[0]-1,j] = 0.
									norm[0:x[0]-1,j] = 0.
								endif
								if x[0] lt nxr-1 then begin
									image[x[0]+1:nxr-1,j] = 0.
									norm[x[0]+1:nxr-1,j] = 0.
								endif
							endfor
							image[x[0],*] = image[x[0],*] / (norm[x[0],*] > 1.)
							q2 = where( image ne 0, nq2)
							q_to_xy, q2, nxr, x,y
							major_y = 1
							end
						8: begin									; project X
							for j=0,nxr-1 do begin
								image[j,y[0]] = total( image[j,*])
								norm[j,y[0]] = total( norm[j,*])
								if y[0] gt 0 then begin
									image[j,0:y[0]-1] = 0.
									norm[j,0:y[0]-1] = 0.
								endif
								if y[0] lt nyr-1 then begin
									image[j,y[0]+1:nyr-1] = 0.
									norm[j,y[0]+1:nyr-1] = 0.
								endif
							endfor
							image[*,0] = image[*,0] / (norm[*,0] > 1.)
							q2 = where( norm ne 0, nq2)
							q_to_xy, q2, nxr, x,y
							major_x = 1
							end
						else:
					endcase
			
					mask = bytarr(nxr,nyr)
					mask[q2] = 1
					nq2 = n_elements(q2)
					q3 = where(mask ne 1, nq3)
				endif
		
				if xy then begin
					for k=0,nq2-1 do begin
						if n_elements(list) eq 0 then begin
							list = {x:x[k], y:y[k], data:image[x[k],y[k]], col:ir}
						endif else begin
							list = [list, {x:x[k], y:y[k], data:image[x[k],y[k]], col:ir} ]
						endelse
					endfor
				endif
			endfor
		endif

;		For output, if collapse=1 then do not output non-E axis. Then can put all same energy in
;		columns on a row, one column for each region. If collapse=0, then will have many columns
;		per region, as well as many regions. Perhaps just have many columns and label columns
;		accordingly with both non-E axis index as well as region index. 

		if xy then begin

;			If an axis has coord labels, such as energy, then sort by the other axis first ...
;			But do not do this if multiple regions are listed.

			if (use_region and (all_regions eq 0)) or use_shape then begin
				if ptr_valid( (*p).px_coords) then begin
					top = double( max( (*(*p).px_coords)[x]))
					q4 = sort( y*top*2 + double((*(*p).px_coords)[x]) )
					q2 = q2[q4]
					list = list[q4]
				endif
				if ptr_valid( (*p).py_coords) then begin
					top = double( max( (*(*p).py_coords)[y]))
					q4 = sort( x*top*2 + double((*(*p).py_coords)[y]) )
					q2 = q2[q4]
					list = list[q4]
				endif
			endif
		
			if plot eq 0 then begin
			    F = strip_file_ext(file) + '-' + el[q[jq]]
				sx = 'X'
				if (*p).x_coord_units ne '' then sx = sx+' ('+(*p).x_coord_units+')'
				sy = 'Y'
				if (*p).y_coord_units ne '' then sy = sy+' ('+(*p).y_coord_units+')'
				title = sx + ctab + sy
				if use_region then begin
					title = title + ctab + strjoin( el[q[jq]]+' (#'+str_tidy(reg)+')', ctab)
				endif else begin
					title = title + ctab + strjoin( el[q[jq]])
				endelse
			    if use_region and (all_regions eq 0) then F = F + '-region' + str_tidy(jr)
			    if use_region and (all_regions eq 1) then F = F + '-regions'
			    if use_shape then F = F + '-shape'
			    if tab then F = F + '-xy.txt' else F = F + '-xy.csv'
			
			    on_ioerror, bad_open
			    close, 1
			    openw, 1, F
			    on_ioerror, bad_write
				printf, 1, title
			endif
	
			qk = where( list.col eq 0, nqk)				; use col=0 as master and find others ...
			if n_elements(nreg) eq 0 then nreg=1
			data = fltarr(nreg,nqk)
			ord = fltarr(nqk)
		
		    for j=0L,nqk-1 do begin
				k = qk[j]
				case (*p).energy_proxy_axis of
					1: begin												; X axis
						if ptr_valid( (*p).px_coords) then begin
							xp = (*(*p).px_coords)[list[k].x]
							sx = str_tidy( xp)
							sy = str_tidy( list[k].y)
		
							data[0,j] = list[k].data
							ord[j] = xp
							if nreg gt 1 then begin
								for ir=1,nreg-1 do begin
									jr = reg[ir]
									qr = where( (list.x eq list[k].x) and (list.col eq ir), nqr)
									if nqr gt 0 then begin
										data[ir,j] = list[qr[0]].data
									endif
								endfor
							endif
						endif
						end
					2: begin												; Y axis
						if ptr_valid( (*p).py_coords) then begin
							yp = (*(*p).py_coords)[list[k].y]
							sy = str_tidy( yp)
							sx = str_tidy( list[k].x)
		
							data[0,j] = list[k].data
							ord[j] = yp
							if nreg gt 1 then begin
								for ir=1,nreg-1 do begin
									jr = reg[ir]
									qr = where( (list.y eq list[k].y) and (list.col eq ir), nqr)
									if nqr gt 0 then begin
										data[ir,j] = list[qr[0]].data
									endif
								endfor
							endif
						endif
						end
					else: begin												; none
						if ptr_valid( (*p).px_coords) then begin
							xp = (*(*p).px_coords)[list[k].x]
							sx = str_tidy( xp)
							ord[j] = xp
						endif else begin
							xp = list[k].x
							sx = str_tidy( xp)
						endelse
						if ptr_valid( (*p).py_coords) then begin
							yp = (*(*p).py_coords)[list[k].y]
							sy = str_tidy( yp)
							ord[j] = yp
						endif else begin
							yp = list[k].y
							sy = str_tidy( yp)
						endelse
		
						data[0,j] = list[k].data
						if nreg gt 1 then begin
							for ir=1,nreg-1 do begin
								jr = reg[ir]
								if major_y then begin
									qr = where( (list.y eq list[k].y) and (list.col eq ir), nqr)
								endif else begin
									qr = where( (list.x eq list[k].x) and (list.col eq ir), nqr)
								endelse
								if nqr gt 0 then begin
									data[ir,j] = list[qr[0]].data
								endif
							endfor
						endif
						end
				endcase
	
				if (plot eq 0) then printf, 1, sx + ctab + sy + ctab +  strjoin( str_tidy( data[*,j]), ctab)
		    endfor	
			if (plot eq 0) then begin
				close_file, 1
			endif else begin
				if !version.os_family eq 'unix' then begin
					retain=2
				endif else begin
			    	retain=1
				endelse
			
				catch, ErrorNo
				if ErrorNo ne 0 then begin
					catch, /cancel
					on_error, 1
					window, 4, retain=retain
				endif
			
				wset, 4
				!p.title = 'XANES Spectrum for Region #'+str_tidy(reg)
				!x.title = 'Energy (keV)'
				!y.title = ''
				!p.thick = 1.0
				!p.charsize = 1.2
				!p.charthick = 1.0
				!p.background = 0
				!p.color = spec_colour('white')
				plot,ord,data,/nodata
				oplot,ord,data,color=spec_colour('green')			
			endelse
		
		endif else begin
;			for j=0L,nq-1 do begin
				i = q[jq]									; pick up from main element loop above
				F = strip_file_ext(file) + '-' + el[i]
				if use_shape then F = F + '-shape'
				if tab then F = F + '.txt' else F = F + '.csv'
				on_ioerror, bad_open
				close, 1
				openw, 1, F
				on_ioerror, bad_write
		
		    	array = (reform((*(*p).image)[*,*,i]) / charge_per_pixel) > 0.0
				if use_shape then begin
					if (nq3 gt 0) then array[q3] = 0.0
				endif
				for iy=0L,(*p).ysize-1 do begin
					printf, 1, strjoin( str_tidy( array[*,iy] ), ctab)
				endfor
			    close_file, 1
;			endfor
		endelse
	endfor	; ----------------------- loop over element planes --------------------------
    return

;....................................................................................................

;	Special case of not XANES and XY mode

special:
	use_shape = 0
	nq2 = 0L
	major_y = 1
	major_x = 0		

	if shape then begin
		if ((*pstate).corr_mode eq 0) then begin
			if (*pstate).analyze_mode eq 0 then begin		; include mode
				mark_vertices, pstate, x1,y1, n
				if n eq 0 then return
				x = round(x1)
				y = round(y1)
	
				q1 = where(x eq (*p).xsize-1)               ; Correct for flaw in polyfillv
				if q1[0] ne -1 then x[q1]=x[q1]+1           ; where it ignores the last column
				q1 = where(y eq (*p).ysize-1)               ; or row when all selected.
				if q1[0] ne -1 then y[q1]=y[q1]+1
	
				q2 = polyfillv( x,y, (*p).xsize,(*p).ysize)
				if q2[0] ne -1 then begin
					mask = bytarr((*p).xsize,(*p).ysize)
					mask[q2] = 1
					nq2 = n_elements(q2)
					q3 = where(mask ne 1, nq3)
					use_shape = 1
				endif else goto, bad_q2
				q_to_xy, q2, (*p).xsize, x,y
			endif
		endif
		if (use_shape ne 1) then goto, bad_region
	
		i1 = 0
		i2 = 0
		nxr = (*p).xsize
		nyr = (*p).ysize
		nt2 = long(nxr) * long(nyr)
		xoff = min(x)
		yoff = min(y)
	endif else begin
		if use_region then begin
			nreg = n_elements( *(*pstate).pregions)
			if ptr_good( (*pstate).pregions) eq 0 then goto, bad_region
			if all_regions then begin
				i1 = 0
				i2 = nreg-1
				reg = indgen(nreg)
;				ncols = nreg
			endif else begin
				i1 = 0
				i2 = 0
				reg = (*pstate).region_id
				nreg = 1
;				ncols = 1
			endelse
		endif else begin
			i1 = 0
			i2 = 0
			reg = 0
			nreg = 1
			nxr = (*p).xsize
			nyr = (*p).ysize
			nt2 = long(nxr) * long(nyr)
			q2 = lindgen(nt2)
			nq2 = nt2
			q_to_xy, q2, (*p).xsize, x,y
			xoff = 0L
			yoff = 0L
		endelse
	endelse
		
	for ir=i1,i2 do begin				; ---------------- loop over region ---------------
		if (use_shape eq 0) and use_region then begin
			jr = reg[ir]
			pr = (*(*pstate).pregions)[jr]
			nxr = (*pr).nx
			nyr = (*pr).ny
			q2 = *(*pr).q
			q_to_xy, q2, nxr, x,y
			nq2 = n_elements(q2)
			nt2 = long(nxr) * long(nyr)
			xoff = min(x)
			yoff = min(y)
		endif
		data = fltarr(nq,nq2)

		for jq=0L,nq-1 do begin			;  loop over element 
			image = fltarr(nxr,nyr)
			norm = fltarr(nxr,nyr)
			image[q2] = (*(*p).image)[ q2 + nt2*q[jq] ]
			if ctype[jq] eq 0 then image[*,*] /= charge_per_pixel
			if clip_zero then image[*,*] >= 0.0
;			norm[q2] = 1.
	
			for k=0,nq2-1 do begin
				data[jq,k] = image[x[k],y[k]]
			endfor
		endfor

		if ptr_valid( (*p).px_coords) then begin
			xp = (*(*p).px_coords)
			sx = str_tidy( xp)
		endif else begin
			sx = str_tidy(indgen(nxr))
		endelse
		if ptr_valid( (*p).py_coords) then begin
			yp = (*(*p).py_coords)
			sy = str_tidy( yp)
		endif else begin
			sy = str_tidy(indgen(nyr))
		endelse
	
		if plot eq 0 then begin
		    F = strip_file_ext(file)
			tx = 'X'
			if (*p).x_coord_units ne '' then tx = tx+' ('+(*p).x_coord_units+')'
			ty = 'Y'
			if (*p).y_coord_units ne '' then ty = ty+' ('+(*p).y_coord_units+')'
			title = tx + ctab + ty
			title = title + ctab + strjoin( el[q], ctab)
		    if use_shape then F = F + '-shape'
		    if use_region then F = F + '-region' + str_tidy(jr)
		    if tab then F = F + '-xy.txt' else F = F + '-xy.csv'
		
		    on_ioerror, bad_open
		    close, 1
		    openw, 1, F
		    on_ioerror, bad_write
			printf, 1, title
		endif
	
		if (plot eq 0) then begin
			for k=0,nq2-1 do begin
				printf, 1, sx[x[k]] + ctab + sy[y[k]] + ctab +  strjoin( str_tidy( data[*,k]), ctab)
			endfor
			close_file, 1
		endif
	endfor	; ----------------------- loop over  region --------------------------
	return

bad_open:
    warning,'export_images_csv','Failed to open file "' + F + '"'
    return
bad_region:
    warning,'export_images_csv',['Invalid region selected,','or clash of conflicting modes.','Try clearing Highlighted pixels.']
    return
bad_write:
    warning,'export_images_csv','Bad write to file "' + F + '"'
	close_file, 1
    return
bad_q2:
	warning,'export_images_csv', ['Must select a region for XY table output mode.', $
								'And integrate it with the "sigma" button.']
	close_file, 1
    return
end