;
;	Correct_Yield / Composition Matrix
;
;	Popup panel to set-up parameters for, and to perform either:
;
;	1.	Pixel-by-pixel yield variation correction of images
;	2.	Projection onto end-member mineral images.
;	3.	Absorption corrections.
;
;	If in DAM=0 mode, then it only acquires the R matrix and yield (instead of DAM) files and
;	does not do image corrections or mineral mapping. It's I/O is then to a .comat file rather than .correct.

pro correct_yield_event, event

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
		warning,'Correct_yield_event',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, kill
	endif
endif
  widget_control, hourglass=0

child = widget_info( event.top, /child)
widget_control, child, get_uvalue=pstate

if ptr_good(pstate, /struct) eq 0 then goto, bad_state
if ptr_good((*pstate).p, /struct) eq 0 then goto, bad_ptr

case tag_names( event,/structure) of
	'NOTIFY': begin
;		print,'****** Correct_Yields: notify: tag=',event.tag,' FROM=',event.from,' TO=',event.top
		case event.tag of
			'path': begin
				if ptr_valid( event.pointer) then begin
					print,'EVT: new path = ',(*event.pointer)
					*(*pstate).path = (*event.pointer)
				endif
				goto, finish
				end

			'image-region-select': begin

;	Save away locally the 'el' and 'conc' vectors (at least).
;	These are then available for the new "From Region" button to use.
;	Need to detect and save the OnCellSelect type events for 'table',
;	to save the currently selected row.

				end

			'images-changed': begin
				if (*pstate).damode eq 0 then goto, finish
				if ptr_valid( event.pointer) eq 0 then goto, finish
				if ptr_valid((*pstate).pimg) then begin
					if (*(*pstate).pimg).orphan eq 1 then begin
						(*pstate).local = 1
						(*(*pstate).pimg).orphan = 0
					endif
					if (event.pointer ne (*pstate).pimg) and (*pstate).local then free_images, (*pstate).pimg
				endif
				if ptr_valid((*(*pstate).p).pyield) then ptr_free, (*(*pstate).p).pyield
				if ptr_valid((*(*pstate).p).plast) then ptr_free, (*(*pstate).p).plast
				if ptr_valid((*(*pstate).p).pdensity) then ptr_free, (*(*pstate).p).pdensity
				if ptr_valid((*(*pstate).p).pscale) then ptr_free, (*(*pstate).p).pscale
				(*pstate).pimg = event.pointer
				*(*pstate).pel = strtrim(*(*(*pstate).pimg).el,2)
				(*pstate).local = 0
				p = (*pstate).p
				for i=0L,(*p).max_comp-1 do begin
					widget_control, (*pstate).element_drop[i], set_value=*(*pstate).pel
					if (i lt (*p).n_comp) then begin
						q = where( strtrim( *(*pstate).pel, 2) eq (*p).comp[i])
						if q[0] ge 0 then widget_control, (*pstate).element_drop[i], set_combobox_select=q[0]
					endif
				endfor
				dam = strlowcase(extract_extension( (*(*pstate).pimg).matrix.file))
				if (dam ne 'dam') and (dam ne 'damx') and (dam ne 'mpdam') then begin
					warning, 'Correct_Yield', ['Image Correction is not available for non-PIXE,SXRF data.','Abort Correct_Yield.']
					goto, kill
				endif
				(*(*pstate).p).original = (*(*pstate).pimg).matrix.file
;				*(*pstate).path = extract_path( (*(*pstate).p).original)
				widget_control, (*pstate).original, set_value=strip_path( (*(*pstate).p).original)
				goto, finish
				end
			else:
		endcase
		end
	'WIDGET_TRACKING': begin
		widget_control, event.id, get_uvalue=s
		if size(s,/tname) eq 'STRING' then begin
			if event.enter eq 1 then begin
				widget_control, (*pstate).help, set_value=s
			endif else begin
				s = 'Setup a composition correct matrix: Define end-members (and select their YIELD files), select elements to ' + $
					'act as a proxy for each phase and define the composition matrix for these end-member phases.'
				if ptr_good( (*pstate).pimg) then s = 'Image: ' + (*(*pstate).pimg).file
				widget_control, (*pstate).help, set_value=s
			endelse
		endif
		goto, finish
		end
	'WIDGET_TIMER': begin
		case widget_info( event.id, /uname) of
			'save': begin
				widget_control, (*pstate).table, set_table_view=[0,0]		; keep table view @ 0,0
				widget_control, event.id, timer=2.0
				end
			else:
		endcase
		goto, finish
		end
	'WIDGET_KILL_REQUEST': begin
		print,'Kill request correct_yield ...'
		goto, kill
		end
	else:
endcase

dame = (*pstate).damode ? extract_extension( (*(*pstate).p).original) : 'yield'

if (*pstate).damode then begin
	if ptr_good( (*pstate).pimg) eq 0 then goto, bad_image
	pimg = (*pstate).pimg
endif else pimg=0L
pel = (*pstate).pel

uname = widget_info( event.id, /uname)

case uname of
	'components': begin								; components droplist
		n2 = (event.index+1)>1
		widget_control, (*pstate).component_drop, set_combobox_select=n2-1
		p = (*pstate).p

		for i=0L,(*p).n_comp-1 do begin
			widget_control, (*pstate).mineral_text[i], get_value=s
			(*(*pstate).p).minerals[i] = s
		endfor
		widget_control, (*pstate).table, get_value=t
		n = n_elements( (*(*pstate).p).R[*,0])
		(*(*pstate).p).R = t[0:n-1,0:n-1]

		; NOTE: This struct also occurs in main Correct_Yield routine, and in read_correct.pro.

		pars = {	$
			n_comp:		long(n2), $				; number of components
			max_comp:	(*p).max_comp, $		; maximum number of components
			comp:		strarr(n2), $			; component elements array
			minerals:	strarr(n2), $			; mineral name strings array
			original:	(*p).original, $		; original DA matrix file name
			rest:		(*p).rest, $			; "rest" DA matrix file name
			files:		strarr(n2), $			; DA matrix file names for minerals
			R:			fltarr(n2,n2), $		; end-member conc table (wt%)
			current_mode:	0, $				; current correction mode
			pyield:		ptr_new(), $			; pointer to end-member yields
			plast:		ptr_new(), $			; pointer to pixel yields, last iteration
			pdensity:	ptr_new(), $			; pointer to density of end-members
			pscale:		ptr_new() $				; pointer to absorb scaling, last iteration
		}
		n = min([(*p).n_comp,n2])
		pars.comp[0:n-1] = (*p).comp[0:n-1]
		pars.minerals[0:n-1] = (*p).minerals[0:n-1]
		pars.files[0:n-1] = (*p).files[0:n-1]
		pars.R[0:n-1,0:n-1] = (*p).R[0:n-1,0:n-1]

		if ptr_valid((*p).pyield) then ptr_free, (*p).pyield
		if ptr_valid((*p).plast) then ptr_free, (*p).plast
		if ptr_valid((*p).pdensity) then ptr_free, (*p).pdensity
		if ptr_valid((*p).pscale) then ptr_free, (*p).pscale
;		(*p).corrected = 0
		*p = pars
		t = (*p).R
		if n2 eq 1 then begin
			t = fltarr(2,2)
			t[0,0] = (*p).R[0,0]
		endif
		widget_control, (*pstate).table, set_value=t, table_xsize=n2, table_ysize=n2, $
				scr_xsize=n2*(*pstate).column_width+20, scr_ysize=(n2)*(*pstate).row_height+20, $
				row_heights=(*pstate).row_height, column_widths=(*pstate).column_width
		n_comp = (*p).n_comp

		y = (*pstate).row_height
		for i=0L,(*p).max_comp-1 do begin
			if (i ge n_comp) then begin
				widget_control, (*pstate).mineral_text[i], scr_ysize=1
				widget_control, (*pstate).dam_file[i], scr_ysize=1
				widget_control, (*pstate).element_drop[i], scr_xsize=1

				widget_control, (*pstate).mineral_base[i], map=0
				widget_control, (*pstate).dam_base[i], map=0
				widget_control, (*pstate).element_base[i], map=0
			endif else begin
				widget_control, (*pstate).mineral_text[i], scr_ysize=y, set_value=(*p).minerals[i]
				widget_control, (*pstate).dam_file[i], scr_ysize=y, set_value=strip_path((*p).files[i])
				widget_control, (*pstate).element_drop[i], scr_xsize=(*pstate).column_width

				q = where( strtrim( *pel, 2) eq (*p).comp[i])
				if q[0] ge 0 then widget_control, (*pstate).element_drop[i], set_combobox_select=q[0]

				widget_control, (*pstate).mineral_base[i], map=1
				widget_control, (*pstate).dam_base[i], map=1
				widget_control, (*pstate).element_base[i], map=1
			endelse
		endfor
		width = (*pstate).wleft + (*pstate).wtable*n_comp+18 + (*pstate).wright + 2
		(*pstate).whelp = width + 5
		widget_control, (*pstate).help, scr_xsize=(*pstate).whelp
		end

	'correct': begin

		; This directly modifies the image arrays in Image program space.
		; Just send a 'image-display' notify when done.

		p = (*pstate).pimg
		ps = (*pstate).p
		if ptr_good(ps) eq 0 then goto, bad_ptr
		if ptr_good(p) eq 0 then goto, bad_image
		if ptr_valid( (*p).options ) eq 0 then fix_options, p

		n_comp = (*ps).n_comp
		for i=0L,n_comp-1 do begin
			widget_control, (*pstate).mineral_text[i], get_value=s
			(*ps).minerals[i] = s
		endfor
		widget_control, (*pstate).table, get_value=t
		n = n_elements( (*ps).R[*,0])
		(*ps).R = t[0:n-1,0:n-1]

		correct_yields_do_correct, ps, p, path=*(*pstate).path, /progress, error=err
		if err then goto, finish

		widget_control, (*pstate).original, set_value=strip_path((*ps).original)
		for j=0L,n_comp-1 do begin
			widget_control, (*pstate).dam_file[j], set_value=strip_path((*ps).files[j])
		endfor
		widget_control, (*pstate).rest, set_value=strip_path((*ps).rest)

		notify, 'image-display', from=event.top
		end

	'project': begin

		; Project elemental images onto end-member fractions.
		; Produce images of end-member fraction.

		p = (*pstate).pimg
		ps = (*pstate).p
		if ptr_good(ps) eq 0 then goto, bad_ptr
		if ptr_good(p) eq 0 then goto, bad_image

		n_comp = (*ps).n_comp
		for i=0L,n_comp-1 do begin
			widget_control, (*pstate).mineral_text[i], get_value=s
			(*ps).minerals[i] = s
		endfor
		widget_control, (*pstate).table, get_value=t
		n = n_elements( (*ps).R[*,0])
		(*ps).R = t[0:n-1,0:n-1]

		correct_yields_do_phases, ps, p, /progress, error=err
		if err then goto, finish

		widget_control, (*pstate).original, set_value=strip_path((*ps).original)
		for j=0L,n_comp-1 do begin
			widget_control, (*pstate).dam_file[j], set_value=strip_path((*ps).files[j])
		endfor
		widget_control, (*pstate).rest, set_value=strip_path((*ps).rest)

		if ptr_valid((*pstate).pimg) then begin
			if (*(*pstate).pimg).orphan eq 1 then begin
				(*pstate).local = 1
				(*(*pstate).pimg).orphan = 0
			endif
			if (*pstate).local then free_images, (*pstate).pimg
		endif
		(*pstate).pimg = p
		(*p).orphan = 1			; request 'image' master to "own me"
		(*pstate).local = 0
		notify, 'images', (*pstate).pimg, from=event.top
		end

	'absorb': begin

		; Correct just the currently displayed image for the effects of spatial
		; variation of mass absorption.
		;
		; This directly modifies the image arrays in Image program space.
		; Just send a 'image-display' notify when done.

		p = (*pstate).pimg
		ps = (*pstate).p
		if ptr_valid( (*p).image ) eq 0 then goto, bad_image
		if (*p).scan.x lt 1.0e-6 then goto, bad_scale
		mode = (*p).corrected
		if (*ps).current_mode ne 1 then begin
			(*p).corrected = 0
			mode = 0
			(*ps).current_mode = 1
		endif
		if mode ne 0 then begin
			if ptr_valid( (*ps).pyield) eq 0 then goto, bad_mode
			if ptr_valid( (*ps).plast) eq 0 then goto, bad_mode
			if ptr_valid( (*ps).pdensity) eq 0 then goto, bad_mode
			if ptr_valid( (*ps).pscale) eq 0 then goto, bad_mode
		endif
		if ptr_valid( (*p).options ) eq 0 then fix_options, p
		opt = (*p).options
		n_comp = (*ps).n_comp
	;	widget_control, /hourglass
		cancel = 0
		progress, tlb=progress_tlb, title='Correct Image for Spatial Variation of Absorption'

		for i=0L,n_comp-1 do begin
			widget_control, (*pstate).mineral_text[i], get_value=s
			(*ps).minerals[i] = s
		endfor
		widget_control, (*pstate).table, get_value=t
		n = n_elements( (*ps).R[*,0])
		(*ps).R = t[0:n-1,0:n-1]

		if n_comp gt 1 then begin
			R = 10000. * transpose( (*ps).R )
		endif else begin
			R = 10000. * (*ps).R
		endelse

		; Make transform matrix F, to transform elements to minerals

		if n_comp gt 1 then begin
			F = invert( R, status)
			if status ne 0 then begin
				print,'Bad matrix invert status = ',status
				warning,'Correct_Yield_Event','Bad matrix invert, status = '+string(status)
				goto, finish
			endif
		endif else begin
			F = 1.0 / R
		endelse

		; Assemble just those relevant elemental images

		C = fltarr( (*p).xsize * (*p).ysize, n_comp)

		el = strtrim(*(*pstate).pel,2)
		in = 'Source ='
		out = 'Dest ='
		for i=0L,n_comp-1 do begin
			q = where( strtrim((*ps).comp[i],2) eq el)
			if q[0] eq -1 then begin
				print,'Missing element: ',(*ps).comp[i]
				warning,'Correct_Yield_Event',['Missing element: ' + (*ps).comp[i], $
							'This element was not found in this image data-set.','','Abort correction.']
				goto, finish
			endif

			; Here we need to fill in the low pixels on the right end of a (X Step?) scan.

			t = (*(*p).image)[*,*,q[0]]
			nx = (*p).xsize
			t2 = smooth2(t[nx-10:nx-1,*],4)
			t[nx-2,*] = t2[4,*]
			t[nx-1,*] = t2[5,*]

			C[*,i] = t
			in = in + ' ' + (*ps).comp[i]
			out = out + ' ' + (*ps).minerals[i]
		endfor

		charge_per_pixel = (*p).charge / ((*p).xsize * (*p).ysize)
		C = C / charge_per_pixel

		; Transform the selected elemental images to minerals.
		; Ignore any negative components for now.

		img = (C # F) > 0.0

		; Check for any pixels with greater than 1.0 sum

		sum = fltarr( (*p).xsize * (*p).ysize)
		for i=0L,n_comp-1 do begin
			sum[*] = sum[*] + img[*,i]
		endfor
		q = where(sum gt 1.0)
		if q[0] ne -1 then begin
			for i=0L,n_comp-1 do begin
				img[q,i] = img[q,i] / sum[q]
			endfor
			sum[q] = 1.0
		endif
		rest = (1.0 - sum) > 0.0
		progress, /update, progress_tlb, {unit:0, value:0, current:1L, size:(*p).n_el}, cancel=cancel
		if cancel then goto, finish

		; Here we'll use the "yield" and "yield_last" arrays to store mass attenuation data
		; "yield" stores the mass-absorption coeffs. for each element, in each phase.
		;
		; "yield_last" stores the MU image of each element.
		; "density_image" the density image.

		if mode eq 0 then begin
			yield_orig = fltarr( (*p).n_el)
			yield = ptr_new( fltarr( (*p).n_el, n_comp+1), /no_copy)
			yield_last = ptr_new( fltarr((*p).xsize * (*p).ysize, (*p).n_el), /no_copy)
			density_orig = 0.0
			density = ptr_new( fltarr( n_comp+1), /no_copy)
			scale_last = ptr_new( fltarr((*p).xsize, (*p).ysize, (*p).n_el, /nozero), /no_copy)
			(*scale_last)[*] = 1.0

			da = read_da((*ps).original, error=error)
			if error then begin
				file = *(*pstate).path + strip_path( (*ps).original)
				da = read_da( file, error=error)
				if error eq 0 then begin
					(*ps).original = file
					widget_control, (*pstate).original, set_value=file
				endif
			endif
			if error then begin
				print,'Bad DA matrix read for - ',(*ps).original
				warning,'Correct_Yield_Event','Bad DA matrix read for - ' + (*ps).original
				goto, finish
			endif
			ok = 0
			if n_elements(da.mu_zero) le 1 then begin
				warning,'Correct_Yield_Event',['DA matrix contains no mass absorption data.', $
							'File name = ' + (*ps).original, $
							'This must be an old version DAM file.', $
							'Regenerate the DAM file in a new fit in "Fit Setup".']
				goto, finish
			endif
			del = strtrim(da.el,2)
			for i=0L,(*p).n_el-1 do begin
				q = where( strtrim((*(*p).el)[i],2) eq del)
				if q[0] ne -1 then begin
					yield_orig[i] = da.mu_zero[q[0]]
					(*yield_last)[*,i] = yield_orig[i]
					if ok eq 0 then begin
						density_orig = da.density0
						ok = 1
					endif
				endif
			endfor

			for j=0L,n_comp-1 do begin
				(*yield)[*,j] = yield_orig[*]			; default values
				(*density)[j] = density_orig
			endfor
			for j=0L,n_comp-1 do begin
				da = read_da((*ps).files[j], error=error)
				if error then begin
					file = *(*pstate).path + strip_path( (*ps).files[j])
					da = read_da( file, error=error)
					if error eq 0 then begin
						(*ps).files[j] = file
						widget_control, (*pstate).dam_file[j], set_value=strip_path(file)
					endif
				endif
				if error then begin
					print,'bad DA matrix read for - ',(*ps).files[j]
					warning,'Correct_Yield_Event','Bad DA matrix read for - ' + (*ps).files[j]
					goto, finish
				endif
				ok = 0
				if n_elements(da.mu_zero) le 1 then begin
					warning,'Correct_Yield_Event',['DA matrix contains no mass absorption data.', $
								'File name = ' + (*ps).files[j], $
								'This must be an old version DAM file.', $
								'Regenerate the DAM file in a new fit in "Fit Setup".']
					goto, finish
				endif
				del = strtrim(da.el,2)
				for i=0L,(*p).n_el-1 do begin
					q = where( strtrim((*(*p).el)[i],2) eq del)
					if q[0] ne -1 then begin
						(*yield)[i,j] = da.mu_zero[q[0]]
						if ok eq 0 then begin
							(*density)[j] = da.density0
							ok = 1
						endif
					endif
				endfor
			endfor

			da = read_da((*ps).rest, error=error)
			if error then begin
				file = *(*pstate).path + strip_path( (*ps).rest)
				da = read_da( file, error=error)
				if error eq 0 then begin
					(*ps).rest = file
					widget_control, (*pstate).rest, set_value=file
				endif
			endif
			if error then begin
				print,'Bad DA matrix read for - ',(*ps).rest
				warning,'Correct_Yield_Event','Bad DA matrix read for - ' + (*ps).rest
				goto, finish
			endif
			ok = 0
			if n_elements(da.mu_zero) le 1 then begin
				warning,'Correct_Yield_Event',['DA matrix contains no mass absorption data.', $
							'File name = ' + (*ps).rest, $
							'This must be an old version DAM file.', $
							'Regenerate the DAM file in a new fit in "Fit Setup".']
				goto, finish
			endif
			del = strtrim(da.el,2)
			for i=0L,(*p).n_el-1 do begin
				q = where( strtrim((*(*p).el)[i],2) eq del)
				if q[0] ne -1 then begin
					(*yield)[i,n_comp] = da.mu_zero[q[0]]
					if ok eq 0 then begin
						(*density)[n_comp] = da.density0
						ok = 1
					endif
				endif
			endfor

		endif else begin
			yield = (*ps).pyield
			yield_last = (*ps).plast
			density = (*ps).pdensity
			scale_last = (*ps).pscale
		endelse

		; Calculate density and mass-absorption images

		d = fltarr( (*p).xsize * (*p).ysize)
		for j=0L,n_comp-1 do begin
			d[*] = d[*] + img[*,j] * (*density)[j]						; combined density
		endfor
		d[*] = d[*] + rest * (*density)[n_comp]							; "rest" density component

		progress, /update, progress_tlb, {unit:0, value:0, current:0, size:(*p).n_el}, cancel=cancel
		if cancel then goto, finish

		mu = fltarr( (*p).xsize * (*p).ysize, (*p).n_el)
		(*p).undo.ok = 0
		for i=0L,(*p).n_el-1 do begin
			for j=0L,n_comp-1 do begin
				mu[*,i] = mu[*,i] + img[*,j] * (*yield)[i,j]			; combined mass-absorb
			endfor

			mu[*,i] = mu[*,i] + rest * (*yield)[i,n_comp]				; "rest" absorption component
			(*yield_last)[*,i] = mu[*,i]

			; To test, return the 'd' image, scaled by 1000.
;			if i eq 0 then begin
;				(*(*p).image)[*,*,0] = 1000. * d[*]
;				(*(*p).el)[0] = 'Den'
;			endif
		endfor

		(*(*p).image)[*,*,0] = 1000. * mu[*,4]
		(*(*pstate).pel)[0] = 'Abs'

		mu = reform( mu, (*p).xsize, (*p).ysize, (*p).n_el)
		d = reform( d, (*p).xsize, (*p).ysize)

		; Here we need to determine the 'scale' image for absorption correction.
		; This will correct all element images for absorption. Is this what we want?
		; This will depend on how fast it is ...

		pixel = ((*p).scan.x * 1000.) / (*p).xsize					; size of pixel (microns)
;		steps = [1,1,2,3,5]			; set "a"
;		ishft = [1,2,4,6,10]
;		n_steps = 5
;		steps = [1,1,1]				; set "b"
;		ishft = [1,2,3]
;		n_steps = 3
;		steps = [1,1,2,3]			; set "c"
;		ishft = [1,2,4,6]
;		n_steps = 4
		steps = [1,1,2,3,5,7]		; set "d"
		ishft = [1,2,4,6,9,16]
		n_steps = 6

		; This assumes a 45 degree take-off for now, and detector on right. Later use 'cos_detector'.
		; Then root2 is replaced by  1 / sqrt( 1 - cos_detector * cos_detector)

		root2 = sqrt(2.)

		elup = strupcase( *(*p).el)
		for i=0L,(*p).n_el-1 do begin
		  if ( elup[i] ne 'ABS') and ( elup[i] ne 'SUM') and ( elup[i] ne 'SUM') then begin

;		q = where( *(*p).el eq 'Cu')
;		if q[0] eq -1 then goto, bad_scale
;		i = q[0]

			progress, /update, progress_tlb, {unit:0, value:0, current:i+1, size:(*p).n_el}, cancel=cancel
			if cancel then goto, finish

			mux = mu[*,*,i] * pixel * d / 10.0					; mu-x image for el i

			; First with a shift of the mux image.
			; Negative image shift for detector on the right.

			y = exp( -mux * 0.5 * root2)
			sum = y

			for j=0L,n_steps-1 do begin
				y = y * exp( -shift(mux,-ishft[j],0) * steps[j] * root2)
				sum = sum + y
			endfor

			; Now with no shift of the mux image.
			; This corresponds to the uniform absorption approx.

			y = exp( -mux * 0.5 * root2)
			sum0 = y

			for j=0L,n_steps-1 do begin
				y = y * exp( -mux * steps[j] * root2)
				sum0 = sum0 + y
			endfor

			scale = sum0 / sum
			(*(*p).image)[*,*,i] = (*(*p).image)[*,*,i] * (scale / (*scale_last)[*,*,i])

			(*scale_last)[*,*,i] = scale
		  endif
		endfor

		progress, /complete, progress_tlb, 'Finished, notify Image ...'

		if mode eq 0 then begin
			if ptr_valid( (*ps).pyield) then ptr_free, (*ps).pyield
			if ptr_valid( (*ps).plast) then ptr_free, (*ps).plast
			if ptr_valid( (*ps).pdensity) then ptr_free, (*ps).pdensity
			if ptr_valid( (*ps).pscale) then ptr_free, (*ps).pscale
			(*ps).pyield = yield
			(*ps).plast = yield_last
			(*ps).pdensity = density
			(*ps).pscale = scale_last
		endif

		for i=0L,(*p).n_el-1 do begin
			(*opt)[i].min = min((*(*p).image)[*,*,i])
			(*opt)[i].max = max((*(*p).image)[*,*,i])
			add_history, (*p).history, i, 'Corrected absorption (iter ' + string(mode+1) + ') for ' + in + ' to ' + out
		endfor
		(*p).corrected = ((*p).corrected + 1) > 0

;		notify, 'images', (*pstate).pimg, from=event.top
		notify, 'image-display', from=event.top
		progress, /ending, progress_tlb
		end

	'original': begin									; original DA matrix file

		F = file_requester( /read, filter='*.'+dame, /must_exist, $
				path=*(*pstate).path, group=event.top, fix_filter=0, $
				title='Original image DA Matrix')
		if F ne '' then begin
			(*(*pstate).p).original = F
			*(*pstate).path = extract_path(F)
			widget_control, (*pstate).original, set_value=strip_path(F)
		endif
		end

	'rest': begin										; rest DA matrix file

		F = file_requester( /read, filter='*.'+dame, /must_exist, $
				path=*(*pstate).path, group=event.top, fix_filter=0, $
				title=(*pstate).damode ? 'Remainder end-member DA Matrix' : 'Remainder end-member yield')
		if F ne '' then begin
			(*(*pstate).p).rest = F
			*(*pstate).path = extract_path(F)
			widget_control, (*pstate).rest, set_value=strip_path(F)
		endif
		end

	'mineral': begin									; one of the minerals text
		q = where( event.id eq (*pstate).mineral_text)
		if q[0] eq -1 then begin
			print,'Unknown mineral text ID'
			goto, finish
		endif
		i = q[0]
		widget_control, event.id, get_value=s
		(*(*pstate).p).minerals[i] = s
		end

	'element': begin								; one of the element droplists
		q = where( event.id eq (*pstate).element_drop)
		if q[0] eq -1 then begin
			print,'Unknown element droplist ID'
			goto, finish
		endif
		i = q[0]
		n = n_elements(*(*pstate).pel)
		el = strtrim( (*(*pstate).pel)[event.index<(n-1)], 2)
		(*(*pstate).p).comp[i] = strtrim(el,2)
		end

	'file': begin									; DA matrix file buttons
		q = where( event.id eq (*pstate).dam_file)
		if q[0] eq -1 then begin
			print,'Unknown DA file button ID'
			goto, finish
		endif
		i = q[0]
		ttext = (*pstate).damode ? 'DA Matrix for end-member ' : 'Yield file for end-member '
		F = file_requester( /read, filter='*.'+dame, /must_exist, $
				path=*(*pstate).path, group=event.top, fix_filter=0, $
				title=ttext+string(i+1)+' - '+(*(*pstate).p).minerals[i])
		if F ne '' then begin
			(*(*pstate).p).files[i] = F
			*(*pstate).path = extract_path(F)
			widget_control, (*pstate).dam_file[i], set_value=strip_path(F)
		endif
		end

	'table': begin									; R matrix table
		print,'Correct_Yield_Event: got a "table" event.'
		widget_control, (*pstate).table, get_value=t
		n = n_elements( (*(*pstate).p).R[*,0])
		(*(*pstate).p).R = t[0:n-1,0:n-1]
		end

	'load': begin
		path = *(*pstate).path
		F = file_requester( /read, filter=((*pstate).damode ? '*.correct' : '*.comat'), /must_exist, $
				path=path, group=event.top, fix_filter=0, $
				title='Load yield correction parameters from file')
		if F ne '' then begin
			widget_control_update, event.top, update=0
			p = read_correct( F, mode=(*pstate).damode, error=err)
			if ptr_valid(p) and (err eq 0) then begin
				if (*p).n_comp gt n_elements((*pstate).mineral_base) then begin
					warning,'',['Number of loaded components exceeds GUI layout.', '', $
								'Close the "Correct Yields (N=6)" window and use the', $
								'Correct Yields (N=16)" window with more component capacity.']
					goto, finish
				endif
				*(*pstate).path = extract_path(F[0])
				(*p).pyield = (*(*pstate).p).pyield
				(*p).plast = (*(*pstate).p).plast
				(*p).pdensity = (*(*pstate).p).pdensity
				(*p).pscale = (*(*pstate).p).pscale
				(*p).max_comp = (*(*pstate).p).max_comp
				*(*pstate).p = *p
				p = (*pstate).p
				pimg = (*pstate).pimg
				n_comp = (*p).n_comp
				width = (*pstate).wleft + (*pstate).wtable*n_comp+18 + (*pstate).wright + 2
				(*pstate).whelp = width + 5
				t = (*p).R
				if n_comp eq 1 then begin
					t = fltarr(2,2)
					t[0,0] = (*p).R[0,0]
				endif
				widget_control, (*pstate).component_drop, set_combobox_select=(n_comp-1)>0
				widget_control, (*pstate).table, set_value=t, table_xsize=n_comp, table_ysize=n_comp, $
						scr_xsize=n_comp*(*pstate).column_width+20, $
						row_heights=(*pstate).row_height, column_widths=(*pstate).column_width, $
						scr_ysize=(n_comp)*(*pstate).row_height+20
				widget_control, (*pstate).rest, set_value=strip_path((*p).rest), scr_ysize=(*pstate).row_height

				if widget_info((*pstate).original,/valid) then begin
;					widget_control, (*pstate).original, set_value=strip_path((*p).original), scr_ysize=(*pstate).row_height
					(*(*pstate).p).original = (*(*pstate).pimg).matrix.file
					widget_control, (*pstate).original, set_value=strip_path( (*(*pstate).p).original), scr_ysize=(*pstate).row_height
				endif

				y = (*pstate).row_height
				for i=0L,(*p).max_comp-1 do begin
					if (i ge n_comp) then begin
						widget_control, (*pstate).mineral_text[i], scr_ysize=1
						widget_control, (*pstate).dam_file[i], scr_ysize=1
						widget_control, (*pstate).element_drop[i], scr_xsize=1

						widget_control, (*pstate).mineral_base[i], map=0
						widget_control, (*pstate).dam_base[i], map=0
						widget_control, (*pstate).element_base[i], map=0
					endif else begin
						widget_control, (*pstate).mineral_text[i], scr_ysize=y, set_value=(*p).minerals[i]
						widget_control, (*pstate).dam_file[i], scr_ysize=y, set_value=strip_path((*p).files[i])
						widget_control, (*pstate).element_drop[i], scr_xsize=(*pstate).column_width

						q = where(  *(*pstate).pel eq (*p).comp[i])
						if q[0] ge 0 then widget_control, (*pstate).element_drop[i], set_combobox_select=q[0]

						widget_control, (*pstate).mineral_base[i], map=1
						widget_control, (*pstate).dam_base[i], map=1
						widget_control, (*pstate).element_base[i], map=1
					endelse
				endfor
				widget_control, (*pstate).help, scr_xsize=(*pstate).whelp
				*(*pstate).ptemp = F
				notify, 'new-correct', (*pstate).ptemp, from=event.top
			endif
			widget_control_update, event.top, update=1
		endif
		end

		'import': begin
			path = *(*pstate).path
			F = file_requester( /read, filter=((*pstate).damode ? '*.comat' : '*.correct'), /must_exist, $
				path=path, group=event.top, fix_filter=0, $
				title='Import yield correction matrix only from file')
			if F ne '' then begin
				widget_control_update, event.top, update=0
				p = read_correct( F, mode=1-(*pstate).damode, error=err)
				if ptr_valid(p) and (err eq 0) then begin
					*(*pstate).path = extract_path(F[0])
					(*p).pyield = (*(*pstate).p).pyield
					(*p).plast = (*(*pstate).p).plast
					(*p).pdensity = (*(*pstate).p).pdensity
					(*p).pscale = (*(*pstate).p).pscale
					(*p).max_comp = (*(*pstate).p).max_comp
					*(*pstate).p = *p
					p = (*pstate).p
					pimg = (*pstate).pimg
					n_comp = (*p).n_comp
					width = (*pstate).wleft + (*pstate).wtable*n_comp+18 + (*pstate).wright + 2
					(*pstate).whelp = width + 5
					t = (*p).R
					if n_comp eq 1 then begin
						t = fltarr(2,2)
						t[0,0] = (*p).R[0,0]
					endif
					(*p).original = ''
					(*p).files[*] = ''
					(*p).rest = ''
					widget_control, (*pstate).component_drop, set_combobox_select=(n_comp-1)>0
					widget_control, (*pstate).table, set_value=t, table_xsize=n_comp, table_ysize=n_comp, $
						scr_xsize=n_comp*(*pstate).column_width+20, $
						row_heights=(*pstate).row_height, column_widths=(*pstate).column_width, $
						scr_ysize=(n_comp)*(*pstate).row_height+20
					if widget_info((*pstate).original,/valid) then widget_control, (*pstate).original, set_value=strip_path((*p).original), scr_ysize=(*pstate).row_height
					widget_control, (*pstate).rest, set_value=strip_path((*p).rest), scr_ysize=(*pstate).row_height
					y = (*pstate).row_height
					for i=0L,(*p).max_comp-1 do begin
						if (i ge n_comp) then begin
							widget_control, (*pstate).mineral_text[i], scr_ysize=1
							widget_control, (*pstate).dam_file[i], scr_ysize=1
							widget_control, (*pstate).element_drop[i], scr_xsize=1

							widget_control, (*pstate).mineral_base[i], map=0
							widget_control, (*pstate).dam_base[i], map=0
							widget_control, (*pstate).element_base[i], map=0
						endif else begin
							widget_control, (*pstate).mineral_text[i], scr_ysize=y, set_value=(*p).minerals[i]
							widget_control, (*pstate).dam_file[i], scr_ysize=y, set_value=strip_path((*p).files[i])
							widget_control, (*pstate).element_drop[i], scr_xsize=(*pstate).column_width

							q = where(  *(*pstate).pel eq (*p).comp[i])
							if q[0] ge 0 then widget_control, (*pstate).element_drop[i], set_combobox_select=q[0]

							widget_control, (*pstate).mineral_base[i], map=1
							widget_control, (*pstate).dam_base[i], map=1
							widget_control, (*pstate).element_base[i], map=1
						endelse
					endfor
					widget_control, (*pstate).help, scr_xsize=(*pstate).whelp
					*(*pstate).ptemp = F
					notify, 'new-correct', (*pstate).ptemp, from=event.top
				endif
				widget_control_update, event.top, update=1
			endif
		end

	'save': begin
		for i=0L,(*(*pstate).p).n_comp-1 do begin
			widget_control, (*pstate).mineral_text[i], get_value=s
			(*(*pstate).p).minerals[i] = s
		endfor
		widget_control, (*pstate).table, get_value=t
		n = n_elements( (*(*pstate).p).R[*,0])
		(*(*pstate).p).R = t[0:n-1,0:n-1]

		path = *(*pstate).path
		F = file_requester( /write, filter=((*pstate).damode ? '*.correct' : '*.comat'), $
				path=path, group=event.top, fix_filter=1, $
				title='Save yield correction parameters to file')
		if F ne '' then begin
			write_correct, (*pstate).p, F, mode=(*pstate).damode, error=err
			if (err eq 0) then *(*pstate).path = extract_path(F[0])
			*(*pstate).ptemp = F
			notify, 'new-correct', (*pstate).ptemp, from=event.top
		endif
		end

	'close': begin
		goto, kill
		end
	else:
endcase
goto, finish

finish:
;	if tag_names( event,/structure) ne 'WIDGET_TRACKING' then begin
;		help,(*(*pstate).p),/struct
;		print,'comp=',(*(*pstate).p).comp
;		print,'minerals=',(*(*pstate).p).minerals
;		print,'R=',(*(*pstate).p).R
;		print,'files=',(*(*pstate).p).files
;	endif
	widget_control, hourglass=0
	if n_elements(progress_tlb) gt 0 then begin
		if widget_info( progress_tlb, /valid) then progress, /ending, progress_tlb
	endif
	return

bad_scale:
	warning, 'Correct_Yield_Event', ['Cannot calculate absorption without micron scale.', $
					'Image must have an XY micron size scale specified in Sort EVT.']
	goto, finish
bad_image:
	warning, 'Correct_Yield_Event', 'Bad pointer to image data.'
	goto, finish
bad_mode:
	warning, 'Correct_Yield_Event', ['Images are already corrected.','','To perform more iterations, you will','need to reload the uncorrected images,','and start from there.']
	goto, finish

bad_state:
	warning,'Correct_Yield_Event',['STATE variable has become ill-defined.','Abort Correct_Yield_Event.'],/error
	goto, kill
bad_ptr:
	warning,'Correct_Yield_Event',['Parameter structure variable has become ill-defined.','Abort Correct_Yield_Event.'],/error
	goto, kill

kill:
	cancel_notify, event.top
	if n_elements(pstate) eq 0 then goto, die
	if ptr_valid(pstate) eq 0 then goto, die
	if size(*pstate,/tname) ne 'STRUCT' then goto, die

	if ptr_valid((*(*pstate).p).pyield) then ptr_free, (*(*pstate).p).pyield
	if ptr_valid((*(*pstate).p).plast) then ptr_free, (*(*pstate).p).plast
	if ptr_valid((*(*pstate).p).pdensity) then ptr_free, (*(*pstate).p).pdensity
	if ptr_valid((*(*pstate).p).pscale) then ptr_free, (*(*pstate).p).pscale
	if ptr_good((*pstate).pimg) then begin
		if (*(*pstate).pimg).orphan eq 1 then begin
			(*pstate).local = 1
			(*(*pstate).pimg).orphan = 0
		endif
		if (*pstate).local then free_images, (*pstate).pimg
	endif
die:
	widget_control, event.top, /destroy
	widget_control, hourglass=0
	return
end

;------------------------------------------------------------------------------------------

pro OnRealize_Correct_Components, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	widget_control, wWidget, set_combobox_select=(*(*pstate).p).n_comp-1
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_Correct_Mineral, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	q = where( (*pstate).mineral_text eq wWidget)
	if q[0] ge 0 then begin
		i = q[0]
		if (i ge (*(*pstate).p).n_comp) then begin
			y = 1
		endif else begin
			y = (*pstate).row_height
		endelse
		widget_control, wWidget, scr_ysize=y[0]
	endif
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_Correct_File, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	q = where( (*pstate).dam_file eq wWidget)
	if q[0] ge 0 then begin
		i = q[0]
		if (i ge (*(*pstate).p).n_comp) then begin
			y = 1
		endif else begin
			y = (*pstate).row_height
		endelse
		widget_control, wWidget, scr_ysize=y[0]
	endif
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_Correct_Element, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

if ptr_valid( (*pstate).p) then begin
	p = (*pstate).p
	q = where( (*pstate).element_drop eq wWidget)
	if q[0] ge 0 then begin
		i = q[0]
		if (i ge (*(*pstate).p).n_comp) then begin
			widget_control, wWidget, scr_xsize=1
		endif else begin
			q = where( strtrim( *(*pstate).pel, 2) eq (*p).comp[i])
			if q[0] ge 0 then widget_control, wWidget, set_combobox_select=q[0]
		endelse
	endif
endif
end

;------------------------------------------------------------------------------------------

pro OnRealize_Correct_Table, wWidget

top = tlb_id( wWidget)
child = widget_info( top, /child)
widget_control, child, get_uvalue=pstate

rows = widget_info( wWidget, /row_heights)
if !version.os_family ne 'Windows' then begin
	if (rows[0] gt 3) and (rows[0] lt 40) then begin
		(*pstate).row_height = rows[0] > 29
	endif else begin
		(*pstate).row_height = 29
	endelse
	widget_control, wWidget, row_height=(*pstate).row_height
endif else begin
	(*pstate).row_height = rows[0]
endelse
end

;------------------------------------------------------------------------------------------

pro correct_yield_rows, child

if !version.os_family eq 'Windows' then return
widget_control, child, get_uvalue=pstate

p = (*pstate).p
h = (*pstate).row_height
widget_control, (*pstate).table, scr_ysize=(*p).n_comp*h+20

for i=0L,(*p).n_comp-1 do begin
	widget_control, (*pstate).mineral_text[i], scr_ysize=h
	widget_control, (*pstate).dam_file[i], scr_ysize=h
endfor

return
end

;------------------------------------------------------------------------------------------
;
;	Correct_Yield / Composition Matrix
;
;	Popup panel to set-up parameters for, and to perform either:
;
;	1.	Pixel-by-pixel yield variation correction of images
;	2.	Projection onto end-member mineral images.
;	3.	Absorption corrections.
;
;	If in DAMODE=0 mode, then it only acquires the R matrix and yield (instead of DAM) files and
;	does not do image corrections or mineral mapping. It's I/O is then to a .comat file rather than .correct.

pro correct_yield, pimg, pars=p, group_leader=group, TLB=tlb, path=path, _extra=extra, $
					xoffset=xoffset, yoffset=yoffset, big=big, small=small, damode=damode, el=el

;	/damode		the full version using DA matrix fields, else yields only
;	/small		smaller window for N=6 at most
;	/big		N=16
;	el			element list, else use *(*pimg).el

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
		warning,'Correct_yield',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(group) lt 1 then group=0L
if n_elements(again) lt 1 then again=0
if n_elements(big) lt 1 then big=0
if n_elements(small) lt 1 then small=0
if n_elements(damode) lt 1 then damode=1
if n_elements(path) lt 1 then path=''

original_dam = ''
if damode eq 1 then begin
	if n_elements(pimg) eq 0 then goto, bad_pars
	if (ptr_valid(pimg) eq 1) then begin
		if ptr_valid( (*pimg).image ) eq 0 then goto, bad_pars
		if ptr_valid( (*pimg).options ) eq 0 then goto, bad_pars
		opt = (*pimg).options
		original_dam = (*pimg).matrix.file
		dam = strlowcase(extract_extension( original_dam))
		if (dam ne 'dam') and (dam ne 'damx') and (dam ne 'mpdam') then goto, bad_dam
		if n_elements(el) eq 0 then el = strtrim(*(*pimg).el,2)
	endif else goto, bad_pars
endif else begin
	pimg = 0L
endelse
if n_elements(el) eq 0 then goto, bad_el		; el=['Ca','Fe','Ni','Cu','Zn','BaL','UL']	;

w = 0
h = 0
xoff = 0
yoff = 0
if widget_info( Group, /valid) then begin
	geom = widget_info( Group, /geometry)
	w = geom.scr_xsize
	h = geom.scr_ysize
	xoff = geom.xoffset
	yoff = geom.yoffset
endif
screen = get_screen_size()
if n_elements(xoffset) lt 1 then begin
	xoffset = ((xoff+w/2) < (screen[0]-34 - 528)) > 0
endif
if n_elements(yoffset) lt 1 then begin
	yoffset = ((yoff+h) < (screen[1]-28 - 283)) > 0
endif
if n_elements(yoffset) lt 1 then yoffset = yoff

p = bad_pars_struct( p, make_pars=make_p)

init_comp = 6L
if big then init_comp = 16L

if make_p eq 0 then begin
	if ((*p).max_comp lt 10) and big then begin
		init_comp = 16L
		make_p = 1
	endif
	if ((*p).max_comp gt 10) and small then begin
		init_comp = 6L
		make_p = 1
	endif
endif

if make_p then begin

	; NOTE: This struct also occurs in the Correct_Yield_Event routine, and in read_correct.pro.

	pars = define( /correct_yields)
	pars.max_comp = init_comp
	pars.original = original_dam

;	pars = {	$
;		n_comp:		3L, $						; number of components
;		max_comp:	init_comp, $				; maximum number of components
;		comp:		replicate(el[1],3), $		; component elements array
;		minerals:	strarr(3), $				; mineral name strings array
;		rest:		'', $						; "rest" DA matrix file name
;		original:	original_dam, $				; original DA matrix file name
;		files:		strarr(3), $				; DA matrix file names for minerals
;		R:			fltarr(3,3), $				; end-member conc table (wt%)
;		current_mode:	0, $					; current correction mode
;		pyield:		ptr_new(), $				; pointer to end-mmeber yields
;		plast:		ptr_new(), $				; pointer to pixel yields, last iteration
;		pdensity:	ptr_new(), $				; pointer to end-mmeber densities
;		pscale:		ptr_new() $					; pointer to absorb scaling, last iteration
;	}
	*p = pars
endif

; 	top-level base

n_comp = (*p).n_comp
max_comp = (*p).max_comp

case !version.os_family of
	'MacOS': begin
		wleft = 100
		wtable = 51
		wright = 400
		xlong = 500
		end
	'unix': begin
		wleft = 100
		wtable = 70
		wright = 400
		xlong = 500
		end
	else: begin
		wleft = 100
		wtable = 51
		wright = 350
		xlong = 450
		end
endcase
yheight = 29

width = wleft + wtable*n_comp+yheight + wright + 2
xhelp = width + 5
title = damode ? 'Correct Images for Composition or Project Minerals' : 'Define Composition Correction matrix'

tlb = widget_base( /column, title=title, /TLB_KILL_REQUEST_EVENTS, $
				group_leader=group, _extra=extra, uname='correct_yields_TLB', $
				xpad=2, ypad=2, space=6, /base_align_center, xoffset=xoffset, yoffset=yoffset)
base1 = widget_base( tlb, /column, xpad=0, ypad=0, space=0, /base_align_center)

cbase = widget_base( base1, /row, /align_left, /base_align_center, ypad=0, space=15)
lab = widget_label( cbase, value='Components:')
component_drop = widget_combobox( cbase, value=string(indgen(max_comp)+1), uname='components', /tracking, $
				notify_realize='OnRealize_Correct_Components', $
				uvalue='Number of distinct end-member components.', xsize=100)

base2 = widget_base( tlb, /column, xpad=0, ypad=0, space=1, /base_align_left)
hbase = widget_base( base2, /row, /base_align_center, ypad=0, xpad=0, space=0)
lab = widget_label( hbase, value=' Mineral \ Element', scr_xsize=wleft+6)
element_base = lonarr(max_comp)
element_drop = lonarr(max_comp)
for i=0L,max_comp-1 do begin
	if i ge n_comp then begin
		on = 0
	endif else begin
		on = 1
	endelse
	element_base[i] = widget_base( hbase, /row, /base_align_center, xpad=0, ypad=0, map=on)
	element_drop[i] = widget_combobox( element_base[i], value=el, $
			uname='element', /tracking, notify_realize='OnRealize_Correct_Element', $
			uvalue='Select element for component.', xsize=wtable)
endfor

tbase = widget_base( base2, /row, /base_align_top, ypad=0, space=1)
lbase = widget_base( tbase, /column, /base_align_right, ypad=0, xpad=0, space=0)
mbase = widget_base( tbase, /column, /base_align_right, ypad=0, xpad=0, space=0)
rbase = widget_base( tbase, /column, /base_align_right, ypad=0, xpad=0, space=0)

mineral_base = lonarr(max_comp)
mineral_text = lonarr(max_comp)
for i=0L,max_comp-1 do begin
	if i ge n_comp then begin
		on = 0
		val = ''
	endif else begin
		on = 1
		val = (*p).minerals[i]
	endelse
	mineral_base[i] = widget_base( lbase, /row, /base_align_center, xpad=0, ypad=0, map=on)
	mineral_text[i] = widget_text( mineral_base[i], value=val, uname='mineral', $
			/edit, notify_realize='OnRealize_Correct_Mineral', scr_ysize=yheight, $
			/tracking, uvalue='Select mineral for component '+string(i+1), scr_xsize=wleft)
endfor

t = (*p).R
if n_comp eq 1 then begin
	t = fltarr(2,2)
	t[0,0] = (*p).R[0,0]
endif
;table = widget_table( mbase, value=t, column_widths=wtable, /no_header, uname='table', $
;			/tracking, uvalue='Edit the concentration (wt%) for each element in each component.', $
;			/editable, resizeable_columns=0, scroll=0, scr_xsize=(n_comp)*wtable+20, $
;			x_scroll_size=n_comp, y_scroll_size=n_comp, notify_realize='OnRealize_Correct_Table')
table = widget_table( mbase, value=t, column_widths=wtable, /no_header, uname='table', $
			/tracking, uvalue='Edit the concentration (wt%) for each element in each component.', $
			/editable, resizeable_columns=0, scroll=0, scr_xsize=(n_comp)*wtable+20, $
			xsize=n_comp, ysize=n_comp, notify_realize='OnRealize_Correct_Table')

dam_base = lonarr(max_comp)
dam_file = lonarr(max_comp)
dtype = damode ? 'DA matrix' : 'YIELD'
type = damode ? 'CORRECT' : 'COMAT'
for i=0L,max_comp-1 do begin
	if i ge n_comp then begin
		on = 0
		val = ''
	endif else begin
		on = 1
		val = strip_path((*p).files[i])
	endelse
	dam_base[i] = widget_base( rbase, /row, /base_align_center, xpad=0, ypad=0, map=on)
	dam_file[i] = widget_button( dam_base[i], value=val, scr_xsize=wright, scr_ysize=yheight, $
			notify_realize='OnRealize_Correct_File', uname='file', $
			/tracking, uvalue=dtype+' file for each mineral.')
endfor

restbase = widget_base( tlb, /row, /align_right, /base_align_center, xpad=0, ypad=0, space=10)
lab = widget_label( restbase, value=damode ? 'Remainder phase DA:' : 'Remainder Phase Yields:')
rest_button = widget_button( restbase, value=strip_path((*p).rest), scr_xsize=xlong, scr_ysize=yheight, uname='rest', $
			/tracking, uvalue=dtype+' file to use for remainder end-member component. ' + $
			'i.e. Missing end-member fractions not accounted for by the specified components.')

if damode then begin
	obase = widget_base( tlb, /row, /align_right, /base_align_center, xpad=0, ypad=0, space=10)
	lab = widget_label( obase, value='Original Image DA:')
	original_button = widget_button( obase, value=strip_path((*p).original), scr_xsize=xlong, scr_ysize=yheight, uname='original', $
				/tracking, uvalue='DA Matrix used to project original images from EVT data.')
endif else original_button=0L

bbase = widget_base( tlb, /row, /base_align_center, xpad=0, ypad=0, space=2)
type = damode ? 'CORRECT' : 'COMAT'
otype = (1-damode) ? 'CORRECT' : 'COMAT'
button = widget_button( bbase, value='Load', uname='load', /tracking, $
			uvalue='Load mineral end-member and yield file settings from a '+type+' file.', xsize=wtable-10)
button = widget_button( bbase, value='Import', uname='import', /tracking, $
			uvalue='Import mineral end-members and matrix from a '+otype+' file.', xsize=wtable-10)
spc = widget_label( bbase, value='', scr_xsize=10)
save_button = widget_button( bbase, value='Save', uname='save', /tracking, $
			uvalue='Save mineral end-member and yield file details to a '+type+' file.', xsize=wtable-10)
spc = widget_label( bbase, value='', scr_xsize=20)

if damode then begin
	button = widget_button( bbase, value='Correct', uname='correct', /tracking, $
				uvalue='Project onto end-member fractions and perform yield correction of all images based on end-member yields and the original DA matrix yields. ' + $
				'Afterwards, use the "Display->Reset display min/max" menu to reset display range min/max.', xsize=wtable-4)
	button = widget_button( bbase, value='Minerals', uname='project', /tracking, $
				uvalue='Project elemental images onto new images representing end-member mass fraction.', xsize=wtable-1)
	button = widget_button( bbase, value='Absorb', uname='absorb', /tracking, $
				uvalue='Project onto end-member fractions and correct the current image for the spatial variation of mass absorption. ' + $
				'At present assumes 45-degree takeoff angle geometry.', xsize=wtable-7)
endif

spc = widget_label( bbase, value='', scr_xsize=20)
button = widget_button( bbase, value='Close', uname='close', /tracking, $
			uvalue='Close the correction/mineral projection window.', xsize=wtable-10)

help = widget_text( tlb, scr_xsize=xhelp, ysize=2, /wrap, uname='HELP', /tracking, $
				uvalue='Help window. Displays context sensitive information about widgets.',frame=0)

state = {	component_drop:	component_drop, $		; components droplist ID
			element_base:	element_base, $			; element droplists bases ID array
			element_drop:	element_drop, $			; element droplists ID array
			mineral_base:	mineral_base, $			; mineral bases ID array
			mineral_text:	mineral_text, $			; mineral text ID array
			table:			table, $				; table ID
			row_height:		yheight, $					; row heights
			column_width:	wtable, $				; column widths
			dam_base:		dam_base, $				; DA matrix label bases ID array
			dam_file:		dam_file, $				; DA matrix labels ID array
			original:		original_button, $		; Original DA matrix file name button ID
			rest:			rest_button, $			; "Rest" DA matrix file name button ID
			help:			help, $					; help text widget ID
			damode:			damode, $				; flags DA matrix version
			path:			ptr_new(path), $		; current path
			pimg:			pimg, $					; pointer to image struct
			pel:			ptr_new(el), $			; element list
			local:			0, $					; flags locally created image
			ptemp:			ptr_new(/alloc), $		; pointer to temp storage for Notify
			wleft:			wleft, $
			wright:			wright, $
			wtable:			wtable, $
			whelp:			xhelp, $
			p:				p $						; pointer to local correction parameters
	}

child = widget_info( tlb, /child)
widget_control, child, set_uvalue=ptr_new(state, /no_copy)
widget_control, tlb, /realize
correct_yield_rows, child
widget_control, save_button, timer=2.0		; start timer to check table scroll

register_notify, tlb, ['path'], $			; new path
					from=group
if damode then begin
	register_notify, tlb, [ $			
			'image-region-select', $		; new region selected
			'images-changed'], $			; get notified whenever images changed
			from=group
endif

xmanager, 'correct_yield', tlb, /no_block
return

bad_pars:
	warning, 'Correct_Yield', ['Bad input pointers.','No valid image data found.']
	return
bad_dam:
	warning, 'Correct_Yield', ['Image Correction is not available for non-PIXE,SXRF data.','Abort Correct_Yield.']
	return
bad_el:
	warning, 'Correct_Yield', ['No valid element list supplied.','Abort Correct_Yield.']
	return
end
