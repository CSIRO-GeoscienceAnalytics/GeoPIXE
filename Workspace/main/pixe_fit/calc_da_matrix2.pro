function calc_da_matrix2, p, pspec, pback, pars, peaks, mask, a, matrix, rGamma, pqoff, $
			array=array, gamma=gamma, pileup_mode=pileup_mode, ppileup=ppileup

;	Calculate the DA matrix, after one more linear-parameters-only iteration.
;	
; Input:
;	p			pointer to 'fit_setup' parameter struct
;	pspec		pointer to spectrum being fitted
;	pback		pointer to background spectrum data
;	pars		pointer to saved DA pars struct from fit
;	peaks		pointer to element lines/yields struct for reduced element list
;	ppileup		pointer to pileup spectrum data
;
;	mask		variable mask array (offset by 'org')
;	a			variable value array (offset by 'org')
;
;	/gamma		flags Gamma-rays (1) or X-rays/protons (0)
;	/pileup_mode we want to build a DA matrix without pileup row
;	
; Output:
;	mask		variable mask array (offset by 'org')
;	a			variable value array (offset by 'org')
;	matrix		DA matrix
;	array		array struct (from (*pars).array and rGamma)
;	rGamma		relative sensitivity factors (from (*pars).array.rGamma)
;	pqoff		index array into elements

COMPILE_OPT STRICTARR
	if n_elements(gamma) lt 1 then gamma=0
	if n_elements(pileup_mode) lt 1 then pileup_mode=0
	if ptr_good(p,/struct) eq 0 then goto, bad
	if ptr_good(pspec,/struct) eq 0 then goto, bad
	if ptr_good(pars,/struct) lt 1 then goto, bad
	if ptr_good(peaks,/struct) eq 0 then goto, bad
	if ptr_valid((*pspec).data) eq 0 then goto, bad_ptr
	if ptr_valid((*p).detector) eq 0 then goto, bad_ptr
	if pileup_mode eq 1 then begin
		if ptr_valid(ppileup) eq 0 then goto, bad_ptr
		if ptr_valid((*ppileup).data) eq 0 then goto, bad_ptr
		pileup = *(*ppileup).data
	endif else pileup = 0.0

	common c_debug_2, pdebug2

	if gamma eq 0 then begin
		if n_elements(pback) lt 1 then goto, bad
		if ptr_valid(pback) eq 0 then goto, bad_ptr
		if ptr_valid((*(*pback)[0]).data) eq 0 then goto, bad_ptr
		background1 = *(*(*pback)[0]).data								;@9-19
		background2 = background1
		background2[*] = 0.0
		if n_elements(*pback) gt 1 then begin
			if ptr_valid((*(*pback)[1]).data) then begin
				background2 = *(*(*pback)[1]).data
			endif
		endif
		background = background1 + background2
	endif else begin
		background = 0.0
	endelse
	back2_non_zero = (total(background2) gt 0.)							;@9-19
	error = 1
	
	n_channels = (*pspec).size
	if ptr_valid(peaks) eq 0 then goto, bad_peaks
	use_mu_zero = 0
	mu_zero = 0.0
	if n_elements( (*peaks).mu_zero) gt 1 then use_mu_zero=1

	name = (*pars).name
	org = (*pars).org
	rorg = (*pars).rorg
	do_tail = (*pars).do_tail
	e_low = (*pars).e_low
	e_high = (*pars).e_high
	cal_a = (*pars).cal_a
	cal_b = (*pars).cal_b
	x = (*pars).x
	y = (*(*pspec).data)[x]
	size = max(x)+1
	ni = n_elements(x)
;	n_els = n_elements((*pars).array.rGamma[0,*])
	n_els = (*peaks).n_els

	back_normalize = 0						; normalize background trial for multi-phase DA
		
	back_mask = 1							; (debug=0) debug background variation

	if back_mask ne 1 then begin
		warning,'calc_da_matrix2','zero "back_mask" set.'
	endif
	
;---- Build DA matrix, after additional linear iteration ---------------------------------

	mask[0:org-1] = 0
	mask[7] = (gamma eq 1) ? 0 : back_mask
	mask[10] = (gamma eq 1) ? 0 : (back_mask and back2_non_zero)					; @9-19
	back_offset = (gamma eq 1) ? 0 : (back_mask ? (back2_non_zero ? 2 : 1) : 0)		; @9-19

;	Normalize the background for unit total, only when mask[7]=1, for multi-phase DA trial

	if back_normalize and (mask[7] eq 1) then begin
		background = background / total(background)
	endif
	
;	In pileup_mode=1 we want to build a DA matrix without pileup row.

	if pileup_mode eq 1 then begin
		mask[4] = 0
		a[4] = 0.0
	endif

	if gamma then begin

;	Check that this is correct. Or, should they be intertwined?

		counts_per_ppm_uc = [(*pars).counts_per_ppm_uc,(*pars).counts_per_ppm_uc]
		cmdl = [(*pars).cmdl,(*pars).cmdl]
		rGamma = fltarr((*pars).array.n_det, 2*n_els)
		rGamma[*,0:n_els-1] = (*pars).array.rGamma
		rGamma[*,n_els:2*n_els-1] = (*pars).array.rGamma
	endif else if back_mask then begin
		if back2_non_zero then begin										;@9-19
			counts_per_ppm_uc = [1.0,1.0,(*pars).counts_per_ppm_uc]			; add one for 'Back1' and 'Back2'
			cmdl = [1.0,1.0,(*pars).cmdl]
			rGamma = fltarr((*pars).array.n_det, n_els+2)
			rGamma[*,0:1] = 1.0
			rGamma[*,2:*] = (*pars).array.rGamma
		endif else begin
			counts_per_ppm_uc = [1.0,(*pars).counts_per_ppm_uc]				; add one for 'Back'
			cmdl = [1.0,(*pars).cmdl]
			rGamma = fltarr((*pars).array.n_det, n_els+1)
			rGamma[*,0] = 1.0
			rGamma[*,1:*] = (*pars).array.rGamma
		endelse
	endif else begin
		counts_per_ppm_uc = [(*pars).counts_per_ppm_uc]
		cmdl = [(*pars).cmdl]
		rGamma = fltarr((*pars).array.n_det, n_els)
		rGamma[*,*] = (*pars).array.rGamma
	endelse

;print,'counts_per_ppm_uc*charge=',counts_per_ppm_uc*(*pspec).charge

	q = where( mask eq 1)
	nq = n_elements(q)

;print,'a=',a

; Test scenarios (some ideas tested)
; enable_DA3: 
;	To set a threshold (minimum) value for element peaks, to make sure 
;	other elements (and Back1, Back2) keep clear of them (i.e DA matrix rows show
;	low weights on all element peaks for Back1, Back2), need to set a minimum
;	for 'a' values (only for element peak area paramters) here.
;
; enable_DA4:
;	To use unity weights in formation of DA matrix.
;
; enable_DA5:
;	To use a 'weighting "master" spectrum' in weights in formation of DA matrix,
;	such as a "stripe" spectrum that samples most phases.
;	Test DA6 uses this too, with a 'MPDA master weights' spectrum from plugin.

	case (*p).weight_da_mode of
		0: begin									; normal weights
			enable_DA4 = 0
			enable_DA3 = 0
			enable_DA5 = 0
			end
		1: begin									; clipped minimum intensities
			enable_DA4 = 0
			enable_DA3 = 1
			enable_DA5 = 0
			end
		2: begin									; unity weights
			enable_DA4 = 1
			enable_DA3 = 0
			enable_DA5 = 0
			end
		3: begin									; use spectrum for 'f' weights
			enable_DA4 = 0							; this option set by "Single DA matrix (w/ Master Weights)"
			enable_DA3 = 0							; Test DA6 uses this too, with a 
			enable_DA5 = 1							; 'MPDA master weights' from plugin.
			end
	endcase

	a0 = a
	qe = [4,5,6,10,13,24,33,35]
	if enable_DA3 then begin
		a[org:*] = a[org:*] > max(a[org:*])/5.		; 20%
;		a[org+qe] = a[org+qe] > max(a[org+qe])/2.	; 50% for selected majors
	endif

	if gamma then begin
		fit = pige( a, org, peaks, n_channels, x, (*p).detector, $
			dfa=dfa, mask=mask, do_step=do_tail, e_low=e_low, e_high=e_high )
	endif else begin
		fit = pixe( a, org, rorg, peaks, n_channels, x, background1, background2, (*p).detector, $
			dfa=dfa, mask=mask, do_tail=do_tail, e_low=e_low, e_high=e_high, $
			pileup_mode=pileup_mode, pileup_spec=pileup, compton=(*p).compton )		;@9-19
	endelse

	a = a0
	t = dfa[x,*]
	df = t[*,q]
	f = fit[x]

;	This option set by "Single DA matrix (w/ Master Weights)" selection to 'Generate DA matrix'

	if enable_DA5 then begin
;		file = file_requester( /read, filter='*.spec', path=extract_path((*pspec).file), title='Load "master" DA weight spectrum')
		file = (*p).file
		if file ne '' then begin
			ps = read_spec( file, error=err)
			if err eq 0 then begin
				cal = (*ps).cal
				cal.poly[1] = cal_a
				cal.poly[0] = cal_b
				weight = map_spec( (*ps).data, (*ps).cal, cal, error=err)
				if err eq 0 then begin
					f2 = weight[x] > 1.
					f2 = f2 * max(f)/max(f2)
				endif else enable_DA5=0
			endif else enable_DA5=0
		endif else enable_DA5=0
	endif

	beta = fltarr(ni,nq)
	for k=0L,nq-1 do begin
		beta[*,k] = df[*,k] / ((enable_DA4 ? 1.0 : (enable_DA5 ? f2 : f)) > 1.0)
	endfor

	alpha = df ## transpose(beta)

	inverse = invert( alpha, status)
	if status eq 2 then print,'calc_da_matrix2: Possible poor accuracy in matrix inversion'

	gamma_matrix = float(inverse ## beta)

	show_back = 1					; in /gamma, show the Back rows

	if gamma then begin
		if show_back then begin
			pq = (q - org) > 0
			n_pure = nq

			matrix = fltarr(size,n_pure)
			for i=0L,n_pure-1 do begin
				matrix[x,i] = gamma_matrix[*,i] / (counts_per_ppm_uc[pq[i]] > 1.0e-10)
			endfor

			qe = where( mask[org:org+n_els-1] eq 1)
			qb = where( mask[org+n_els:org+2*n_els-1] eq 1)
			el = element_name( (*peaks).z[qe] ) + '*'
			el = [el,'Back ' + element_name( (*peaks).z[qb])]
			if use_mu_zero then mu_zero = [reform((*peaks).mu_zero[0,qe]),reform((*peaks).mu_zero[0,qe])]
		endif else begin
			qe = where( mask[org:org+n_els-1] eq 1)
			pq = qe
			n_pure = n_elements(pq)

			matrix = fltarr(size,n_pure)
			for i=0L,n_pure-1 do begin
				matrix[x,i] = gamma_matrix[*,i] / (counts_per_ppm_uc[pq[i]] > 1.0e-10)
			endfor

			el = element_name( (*peaks).z[qe] ) + '*'
			if use_mu_zero then mu_zero = reform((*peaks).mu_zero[0,qe])
		endelse
		q2 = where( pq ge 0, nq2)
	endif else begin
		pq1 = q - ORG
		qt = where( q eq 7, nqt)					; mask[7]
		if nqt gt 0 then pq1[qt] = -2
		qt = where( q eq 10, nqt)					; mask[10]		;@9-19
		if nqt gt 0 then pq1[qt] = -1
		qt = where( (q lt ORG) and (q ne 7) and (q ne 10), nqt)	; other non-linear pars masked on?
		if nqt gt 0 then begin
			print,'calc_da_matrix2: found "'+strjoin(str_tidy(q[qt]),' ')+'" bad non-linear enabled.'
		endif
		pq = pq1 > (-back_offset)					; -2 (back1), -1 (back2), 0:* elements
		n_pure = nq
		off = 1

; The 1.0e-10 limit on "counts_per_ppm_uc" here is the same as the one used to make the 
; scale finite in "build_da_fit".

		size = max(x)+1
		matrix = fltarr(size,n_pure)
		for i=0L,n_pure-1 do begin
			matrix[x,i] = gamma_matrix[*,i] / (counts_per_ppm_uc[pq[i]+back_offset] > 1.0e-10)
		endfor
		qt = where(finite(matrix) eq 0,cq)
		if qt[0] ne -1 then begin
			matrix[qt] = 0.0
			print,'calc_da_matrix2: fixed ',cq,' NaN matrix elements'
		endif

		q2 = where( pq ge 0, nq2)
		shell = ['','','L','M']
		el = element_name( (*peaks).z[pq[q2]] ) + shell[ (*peaks).shell[pq[q2]]]
		t = where( (*peaks).z[pq[q2]] le 0)									;@10-24
		if t[0] ne -1 then el[t] = name[org+pq[q2[t]]]
		if back_mask then begin
			if back2_non_zero then begin									;@9-19
				el = ['Back1','Back2',el]
				if use_mu_zero then mu_zero = [0.0,0.0, reform((*peaks).mu_zero[0,pq[q2]]) ]
			endif else begin
				el = ['Back',el]
				if use_mu_zero then mu_zero = [0.0, reform((*peaks).mu_zero[0,pq[q2]]) ]
			endelse
		endif else begin
			if use_mu_zero then mu_zero = [reform((*peaks).mu_zero[0,pq[q2]]) ]
		endelse

;		if pileup_mode eq 1 then begin
;			el = ['sum',el]
;			if use_mu_zero then mu_zero = [0.0, mu_zero]
;		endif
	endelse
	pqoff = pq + back_offset
	
	pure = fltarr(n_channels,n_pure)

	for i=0L,n_pure-1 do begin
		if gamma then begin
			pure[*,i] = pige( a, org, peaks, n_channels, x, (*p).detector, $
				mask=mask, do_step=do_tail, e_low=e_low, e_high=e_high, pure=pq1[i] )
		endif else begin
			pure[*,i] = pixe( a, org, rorg, peaks, n_channels, x, background1, background2, (*p).detector, $
				mask=mask, do_tail=do_tail, e_low=e_low, e_high=e_high, pure=pq1[i], $
				pileup_mode=pileup_mode, pileup_spec=pileup, compton=(*p).compton )
		endelse
	endfor

; Take care to save only the rGamma arrays needed for the elements in the DA images.

	pure = pure[0:size-1,*]
	cal = {a:cal_a, b:cal_b}
	array = {on:(*pars).array.on, n_det:(*pars).array.n_det, rGamma:rGamma[*,pqoff] }
	
	unknown = ((*(*pars).peaks).unknown-1) > 0
	thick = (*(*pars).peaks).thick[unknown]
	density = (*(*pars).peaks).density[unknown]
	if (*(*pars).peaks).microns[unknown] then thick = thick * density / 10.
	
	label = (*pspec).source
	if label eq '' then label = (*pspec).label
	if label eq '' then label = strip_path( (*pspec).file)
	
; N.B This has to match the form in 'calc_da_matrix2', 'read_da' and 'read_old_da'

	da_matrix = { label: label, $						; label, name of source data
			file:		'', $							; local file name
			cal_orig: 	cal, $							; original spectrum cal
			cal:		cal, $							; cal of DA matrix rows
			station:	(*pspec).station, $				; detector station number
			charge:		(*pspec).charge, $				; charge of DA matrix. for MDLs
			n_el:		long(n_pure), $					; number of element rows
			el:			el, $							; element names
			mu_zero: 	mu_zero, $						; major-line mass absorption coefficients
			density0: 	(*peaks).density[0], $			; density of top layer
			ecompress: 	(*pspec).ecompress, $			; spectrum compression factor
			mdl:		cmdl[pqoff], $					; mdls at this charge
			size:		long(size), $					; size of a row
			matrix:		matrix, $						; matrix
			yield:		float(counts_per_ppm_uc[pqoff]), $	; yield factors (central detector)
			thick:		thick, $						; mg/cm2 for unknown layer
			array:		array, $						; array, yield-ratios, ...
			n_pure:		long(n_pure), $					; # of pure element unit spectra
			pure:		pure, $ 						; pure spectra
			E_beam:		(*peaks).e_beam, $				; beam energy (keV Xray, meV ionbeam)
			pmore:		ptr_new() }						; pointer to more DA matrices

	error = 0
	return, ptr_new( da_matrix, /no_copy)

bad:
	warning, 'calc_da_matrix2', 'Missing pointer parameters', /error
	goto, bad_exit
bad_ptr:
	warning, 'calc_da_matrix2', 'Invalid pointers', /error
	goto, bad_exit
bad_peaks:
	warning, 'calc_da_matrix2', 'Invalid X-ray peaks data', /error
	goto, bad_exit

bad_exit:
	error = 1
	return, ptr_new()
	end
