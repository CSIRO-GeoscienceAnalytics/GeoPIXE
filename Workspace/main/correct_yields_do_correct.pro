pro correct_yields_do_correct, ps, p, progress=do_progress, path=path, error=err

; Do the yield correction of image 'p' using pars 'ps'
; If /progress, pop up a progress bar.

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
		warning,'correct_yields_do_correct',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
err = 1
if n_elements(do_progress) eq 0 then do_progress=0
if n_elements(path) eq 0 then path=''

	if ptr_good(ps) eq 0 then goto, bad_ptr
	if ptr_good(p) eq 0 then goto, bad_ptri

	if ptr_good( (*p).image ) eq 0 then goto, bad_image
	els = strtrim(*(*p).el,2)

	mode = (*p).corrected
	if (*ps).current_mode ne 0 then begin
		(*p).corrected = 0
		mode = 0
		(*ps).current_mode = 0
	endif
	if mode ne 0 then begin
		if ptr_valid( (*ps).pyield) eq 0 then goto, bad_mode
		if ptr_valid( (*ps).plast) eq 0 then goto, bad_mode
	endif
;	if ptr_valid( (*p).options ) eq 0 then fix_options, p
	opt = (*p).options
	n_comp = (*ps).n_comp
	cancel = 0
	if do_progress then progress, tlb=progress_tlb, title='Correct Images for Spatial Variation of Composition'

	if n_comp gt 1 then begin
		R = 10000. * transpose( (*ps).R )
	endif else begin
		R = 10000. * (*ps).R
	endelse

	; Make transform matrix F, to transform elements to minerals

	if n_comp gt 1 then begin
		F = invert( R, status)
		if status ne 0 then goto, bad_status
	endif else begin
		F = 1.0 / R
	endelse

	; Assemble just those relevant elemental images

	C = fltarr( (*p).xsize * (*p).ysize, n_comp)

	el = strtrim(els,2)
	in = 'Source ='
	out = 'Dest ='
	for i=0L,n_comp-1 do begin
		q = where( strtrim((*ps).comp[i],2) eq el)
		if q[0] eq -1 then goto, missing_element
		C[*,i] = (*(*p).image)[*,*,q[0]]
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
	q = where(sum gt 1.0, nq)
	if q[0] ne -1 then begin
		print, 'correct_yields_do_correct: renorm ',nq,' pixels greater than 1.'
		for i=0L,n_comp-1 do begin
			if max(img[q,i]) gt 1. then print,'  Comp ', i, ' max = ', max(img[q,i])
			img[q,i] = img[q,i] / sum[q]
		endfor
		sum[q] = 1.0
	endif
	
	rest = (1.0 - sum) > 0.0

	if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:1L, size:(*p).n_el}, cancel=cancel
	if cancel then goto, finish

	if mode eq 0 then begin
		yield_orig = fltarr( (*p).n_el)
		yield = ptr_new( fltarr( (*p).n_el, n_comp+1), /no_copy)
		yield_last = ptr_new( fltarr((*p).xsize * (*p).ysize, (*p).n_el), /no_copy)

		da = read_da((*ps).original, error=error)
		if error then begin
			file = path + strip_path( (*ps).original)
			da = read_da( file, error=error)
			if error eq 0 then begin
				(*ps).original = file
			endif
		endif
		if error then goto, bad_da_original
		del = strtrim(da.el,2)
		for i=0L,(*p).n_el-1 do begin
			q = where( strtrim((*(*p).el)[i],2) eq del)
			if q[0] ne -1 then begin
				yield_orig[i] = da.yield[q[0]]
				(*yield_last)[*,i] = yield_orig[i]
			endif
		endfor

		for j=0L,n_comp-1 do begin
			(*yield)[*,j] = yield_orig[*]			; default values
		endfor
		for j=0L,n_comp-1 do begin
			da = read_da((*ps).files[j], error=error)
			if error then begin
				file = path + strip_path( (*ps).files[j])
				da = read_da( file, error=error)
				if error eq 0 then begin
					(*ps).files[j] = file
				endif
			endif
			if error then goto, bad_da_file
			del = strtrim(da.el,2)
			for i=0L,(*p).n_el-1 do begin
				q = where( strtrim((*(*p).el)[i],2) eq del)
				if q[0] ne -1 then begin
					(*yield)[i,j] = da.yield[q[0]]
				endif
			endfor
		endfor

		da = read_da((*ps).rest, error=error)
		if error then begin
			file = path + strip_path( (*ps).rest)
			da = read_da( file, error=error)
			if error eq 0 then begin
				(*ps).rest = file
			endif
		endif
		if error then goto, bad_da_rest
		del = strtrim(da.el,2)
		for i=0L,(*p).n_el-1 do begin
			q = where( strtrim((*(*p).el)[i],2) eq del)
			if q[0] ne -1 then begin
				(*yield)[i,n_comp] = da.yield[q[0]]
			endif
		endfor

	endif else begin
		yield = (*ps).pyield
		yield_last = (*ps).plast
	endelse

	(*p).undo.ok = 0
	for i=0L,(*p).n_el-1 do begin
		sum[*] = 0.0
		for j=0L,n_comp-1 do begin
			sum[*] = sum[*] + img[*,j] / (*yield)[i,j]
		endfor

		temp = sum + rest / (*yield)[i,n_comp]			; 1/Y averaging
		scale = temp * (*yield_last)[*,i]
		(*yield_last)[*,i] = 1. / temp

		if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:(i>1), size:(*p).n_el}, cancel=cancel
		if cancel then goto, finish

		(*(*p).image)[*,*,i] = (*(*p).image)[*,*,i] * scale
	endfor
	if do_progress then progress, /complete, progress_tlb, 'Finished, notify Image ...'

	if mode eq 0 then begin
		if ptr_valid( (*ps).pyield) then ptr_free, (*ps).pyield
		if ptr_valid( (*ps).plast) then ptr_free, (*ps).plast
		(*ps).pyield = yield
		(*ps).plast = yield_last
	endif

	for i=0L,(*p).n_el-1 do begin
		(*opt)[i].min = min((*(*p).image)[*,*,i])
		(*opt)[i].max = max((*(*p).image)[*,*,i])
		add_history, (*p).history, i, 'Corrected yields (iter ' + string(mode+1) + ') for ' + in + ' to ' + out
	endfor
	(*p).corrected = ((*p).corrected + 1) > 0
	err = 0

finish:
	if n_elements(progress_tlb) gt 0 then begin
		if widget_info( progress_tlb, /valid) then progress, /ending, progress_tlb
	endif
	return

bad_ptr:
	warning,'correct_yields_do_correct',['Parameter pointer invalid.','Abort.'],/error
	goto, finish
bad_ptri:
	warning,'correct_yields_do_correct',['Bad pointer to image struct.','Abort.'],/error
	goto, finish
bad_image:
	warning,'correct_yields_do_correct',['Bad pointer to image data.','Abort.'],/error
	goto, finish
bad_mode:
	warning, 'correct_yields_do_correct', ['Images are already corrected.','','To perform more iterations, you will','need to reload the uncorrected images,','and start from there.']
	goto, finish
bad_status:
	warning,'correct_yields_do_correct','Bad matrix invert, status = '+string(status)
	goto, finish
missing_element:
	warning,'correct_yields_do_correct',['Missing element: ' + (*ps).comp[i], $
				'This element was not found in this image data-set.','','Abort correction.']
	goto, finish
bad_da_original:
	warning,'correct_yields_do_correct','Bad DA matrix read for - ' + (*ps).original
	goto, finish
bad_da_file:
	warning,'correct_yields_do_correct','Bad DA matrix read for - ' + (*ps).files[j]
	goto, finish
bad_da_rest:
	warning,'correct_yields_do_correct','Bad DA matrix read for - ' + (*ps).rest
	goto, finish
end
