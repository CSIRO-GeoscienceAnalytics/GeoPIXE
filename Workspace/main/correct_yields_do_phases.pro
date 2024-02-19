pro correct_yields_do_phases, ps, p, progress=do_progress, error=err

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
		warning,'correct_yields_do_phases',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
err = 1
if n_elements(do_progress) eq 0 then do_progress=0

	if ptr_good(ps) eq 0 then goto, bad_ptr
	if ptr_good(p) eq 0 then goto, bad_ptri
	cancel = 0

	if ptr_good( (*p).image ) eq 0 then goto, bad_image
	els = strtrim(*(*p).el,2)

	mode = (*p).corrected
	opt = (*p).options
	n_comp = (*ps).n_comp
	if do_progress then progress, tlb=progress_tlb, title='Project onto end-member Phase maps'

;	if n_comp gt 1 then begin
;		R = 10000. * transpose( (*ps).R )
;	endif else begin
;		R = 10000. * (*ps).R
;	endelse

	R = 10000. * transpose( (*ps).R )

	; Make transform matrix F, to transform elements to minerals

;	if n_comp gt 1 then begin
;		F = invert( R, status)
;		if status ne 0 then goto, bad_status
;	endif else begin
;		F = 1.0 / R
;	endelse

	if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:1L, size:4}, cancel=cancel
	if cancel then goto, finish

	F = invert( R, status)
	if status ne 0 then goto, bad_status

	; Assemble just those relevant elemental images

	C = fltarr( (*p).xsize * (*p).ysize, n_comp)
	if (*p).has_errors then begin
		xesize = n_elements( (*(*p).error)[*,0,0])
		yesize = n_elements( (*(*p).error)[0,*,0])
		V = fltarr( xesize*yesize, n_comp)
	endif

	el = strtrim(els,2)
	in = 'Source ='
	out = 'Dest ='
	for i=0L,n_comp-1 do begin
		q = where( strtrim((*ps).comp[i],2) eq el)
		if q[0] eq -1 then goto, missing_element
		; C[*,i] = (*(*p).image)[*,*,q[0]]
		C[*,i] = median( (*(*p).image)[*,*,q[0]], 3)
		
		; if (*p).has_errors then V[*,i] = (*(*p).error)[*,*,q[0]]
		if (*p).has_errors then V[*,i] = median( (*(*p).error)[*,*,q[0]], 3)
		
		in = in + ' ' + (*ps).comp[i]
		out = out + ' ' + (*ps).minerals[i]
	endfor

	if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:2, size:4}, cancel=cancel
	if cancel then goto, finish

	charge_per_pixel = (*p).charge / ((*p).xsize * (*p).ysize)
	C[*] = C[*] / charge_per_pixel

	; Transform the selected elemental images to minerals

	img = (C # F) > 0.0

	; Check for any pixels with greater than 1.0 sum

	C = 0															; free this local memory
	sum = fltarr( (*p).xsize * (*p).ysize)
	for i=0L,n_comp-1 do begin
		sum[*] = sum[*] + img[*,i]
	endfor
	q = where(sum gt 1.0)
	if q[0] ne -1 then begin
		for i=0L,n_comp-1 do begin
			if max(img[q,i]) gt 1. then print,'  Comp ', i, ' max = ', max(img[q,i])
			img[q,i] = img[q,i] / sum[q]
		endfor
		sum[q] = 1.0
	endif		

	if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:3, size:4}, cancel=cancel
	if cancel then goto, finish

	rest = (1.0 - sum) > 0.0

	sum = 0															; free this local memory
	img = reform( img, (*p).xsize, (*p).ysize, n_comp, /overwrite)
	img2 = fltarr( (*p).xsize, (*p).ysize, n_comp+1, /nozero)
	img2[*,*,0:n_comp-1] = img
	img2[*,*,n_comp] = rest
	img = 0															; free this local memory
	rest = 0

	if (*p).has_errors then begin
		var = ((V # (F*F)) > 0) / (16.0 * charge_per_pixel * charge_per_pixel)
		var = reform( var, xesize, yesize, n_comp, /overwrite)
		var2 = fltarr( xesize, yesize, n_comp+1, /nozero)
		var2[*,*,0:n_comp-1] = var
		var2[*,*,n_comp] = 0.02
		var = 0														; free this local memory
	endif

	if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:4, size:4}, cancel=cancel
	if cancel then goto, finish

	p = copy_image( p, /no_image)									; do not copy image, error data

	history = ptrarr( n_comp+1)
	if ptr_valid((*p).history) then begin
		for i=0L,n_elements(*(*p).history)-1 do begin
			if ptr_valid((*(*p).history)[i]) then begin
				ptr_free, (*(*p).history)[i]
			endif
		endfor
		ptr_free, (*p).history
	endif
	(*p).history = ptr_new( history, /no_copy)

	opt = define( /options_image)
	options = replicate( opt, n_comp+1)
	escale = options

	for i=0L,n_comp do begin
		options[i].min = min(img2[*,*,i])
		options[i].max = max(img2[*,*,i])
		add_history, (*p).history, i, 'Transformed from ' + in + ' to ' + out
	endfor
	if ptr_valid((*p).options) then ptr_free, (*p).options
	(*p).options = ptr_new( options, /no_copy)

	mdl = fltarr( n_comp+1)
	if ptr_valid((*p).matrix.mdl) then ptr_free, (*p).matrix.mdl
	(*p).matrix.mdl = ptr_new( mdl, /no_copy)

	if ptr_valid((*p).image) then ptr_free, (*p).image
	(*p).image = ptr_new( img2, /no_copy)							; save image data in *p
	(*p).undo.ok = 0

	if (*p).has_errors then begin
		for i=0L,n_comp do begin
			escale[i].min = min(var2[*,*,i])
			escale[i].max = max(var2[*,*,i])
		endfor
		if ptr_valid((*p).escale) then ptr_free, (*p).escale
		(*p).escale = ptr_new( escale, /no_copy)

		if ptr_valid((*p).error) then ptr_free, (*p).error
		(*p).error = ptr_new( var2, /no_copy)						; save error data in *p
	endif
	if do_progress then progress, /complete, progress_tlb, 'Finished, notify Image ...'

	if ptr_valid((*p).el) then ptr_free, (*p).el
	(*p).el = ptr_new( [(*ps).minerals,'Rest'])
	(*p).n_el = n_comp+1
	(*p).type = 1							; mineral fractions
	err = 0

finish:
	if n_elements(progress_tlb) gt 0 then begin
		if widget_info( progress_tlb, /valid) then progress, /ending, progress_tlb
	endif
	return

bad_ptr:
	warning,'correct_yields_do_phases',['Parameter pointer invalid.','Abort.'],/error
	goto, finish
bad_ptri:
	warning,'correct_yields_do_phases',['Bad pointer to image struct.','Abort.'],/error
	goto, finish
bad_image:
	warning,'correct_yields_do_phases',['Bad pointer to image data.','Abort.'],/error
	goto, finish
bad_status:
	warning,'correct_yields_do_phases','Bad matrix invert, status = '+string(status)
	goto, finish
missing_element:
	warning,'correct_yields_do_phases',['Missing element: ' + (*ps).comp[i], $
				'This element was not found in this image data-set.','','Abort correction.']
	goto, finish
end
