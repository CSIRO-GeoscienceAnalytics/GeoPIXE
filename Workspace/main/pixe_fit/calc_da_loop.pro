pro calc_da_merge_scatter, pars, peaks

;	Merge "Compton" and "elastic", if low 'eb', return as "scatter"
;	
; Arguments:
; 	pars		pointer to saved DA pars struct from fit1
;	peaks		pointer to element lines/yields struct for reduced element list

;	Combine the following that depend on element indices or number of elements:
;	pars		A, mask, name, do_tail, counts_per_ppm_uc, cmdl
;	peaks		n_els, z, shell, n_lines, lines, e, intensity, yield, 
;				free, mu_zero, ratio_yield, ratio_intensity

COMPILE_OPT STRICTARR
	if ptr_good(pars,/struct) eq 0 then goto, bad
	if ptr_good(peaks,/struct) eq 0 then goto, bad

	qc = where( (*pars).name eq 'Compton', nqc)
	qe = where( (*pars).name eq 'elastic', nqe)
	if (nqc eq 0) or (nqe eq 0) then return
	
	org = (*pars).org
	qo = where( (*pars).name ne 'Compton', nq)		; index to all non-Compton elements
	q = qo - org
	qt = where(q ge 0, nqt)
	if nqt eq 0 then goto, bad
	q = q[qt]										; indices into A and Mask arrays (offset by 'org')
	ic = qc[0] - org								; Compton element index
	ie = qe[0] - org								; elastic element index
	r = (*pars).a[qc[0]] / (*pars).a[qe[0]]			; ratio of Compton to elastic intensity
	
	name = (*pars).name
	name[org+ie] = 'scatter'
	array = (*pars).array
	
	array = {	on:		array.on, $					; array detector mode
				n_det:	array.n_det, $				; # detectors in array
				rGamma:	array.rGamma[*,q] }			; rGamma, rel sensitivities

	lines = (*peaks).lines
	e = (*peaks).e
	intensity = (*peaks).intensity
	rIntensity = (*peaks).ratio_intensity
	n_lines = (*peaks).n_lines

;	energy 'e' and line indices 'lines' are inherited from 'Compton' and added to 'elastic' list.
;	# of lines 'n_lines' is increased accordingly.

	if (*peaks).n_lines[ic] gt 0 then begin
		lines[ (*peaks).n_lines[ie]:(*peaks).n_lines[ie]+(*peaks).n_lines[ic]-1, ie] = lines[0:(*peaks).n_lines[ic]-1,ic]
		e[ (*peaks).n_lines[ie]:(*peaks).n_lines[ie]+(*peaks).n_lines[ic]-1,ie] = e[0:(*peaks).n_lines[ic]-1,ic]
		n_lines[ie] = n_lines[ie] + n_lines[ic] 

;		relative intensities 'intensity' and 'ratio_intensity' inherit Compton, but scaled by 'r'.

		intensity[ (*peaks).n_lines[ie]:(*peaks).n_lines[ie]+(*peaks).n_lines[ic]-1,ie] = r * intensity[0:(*peaks).n_lines[ic]-1,ic]
		if array.on then rIntensity[ *,(*peaks).n_lines[ie]:(*peaks).n_lines[ie]+(*peaks).n_lines[ic]-1,ie] = r * rIntensity[*,0:(*peaks).n_lines[ic]-1,ic]
	endif
	
;	N.B. This has to match the form in 'calc_da_merge_scatter', 'correct_lines', 'make_peaks',
;	'read_yield', 'select_element_lines', 'sum_peaks', 'append_escapes', 'pixe_initial'

	t1 = make_peaks( $ 
			z2=					(*peaks).z[q], $				; atomic numbers
			shell=				(*peaks).shell[q], $			; shell indices
			n_lines=			n_lines[q], $					; # lines for each element
			lines=				lines[*,q], $					; line indices for each element
			e_lines=			e[*,q], $						; line energies
			intensity=			intensity[*,q], $				; line intensities
			yield=				(*peaks).yield[q], $			; yields
			free=				(*peaks).free[q], $				; free element in fit? 
			mu_zero=			(*peaks).mu_zero[*,q], $		; absorption coeff for ?
			ratio_yield=		(*peaks).ratio_yield[*,q], $	; yield ratios
			ratio_intensity=	rIntensity[*,*,q], $			; intensity ratios
			default=			peaks )

	*peaks = t1

;	N.B. This has to match the form in 'calc_da_merge_scatter' ('correct_da_loop'), 'pixe_fit'

	t2 = { $
		mask:				(*pars).mask[qo], $				; parameter mask
		a:					(*pars).a[qo], $				; A
		name:				name[qo], $						; names of parameters
		org:				(*pars).org, $					; org: origin of elements
		rorg:				(*pars).rorg, $					; rorg: origin of tweeks
		peaks:				ptr_new(t1,/no_copy), $			; peaks
		x:					(*pars).x, $					; x
		do_tail:			(*pars).do_tail[q], $			; do_tail / step
		e_low:				(*pars).e_low, $				; low energy range
		e_high:				(*pars).e_high, $				; high energy range
		cal_a:				(*pars).cal_a, $				; Cal A
		cal_b:				(*pars).cal_b, $				; Cal B
		counts_per_ppm_uc:	(*pars).counts_per_ppm_uc[q], $	; yield factors
		array: 				array, $						; array struct
		cmdl:				(*pars).cmdl[q] }				; MDL (ppm)

	*pars = t2
	return

bad:
	return
end

;---------------------------------------------------------------------------------

function calc_da_loop, p, pspec, pback, pars, gamma=gamma, $
			pileup_mode=pileup_mode, ppileup=ppileup, e_beam=eb, E_scatter_merge=E_scatter_merge

;	Calculate the DA matrix in 'cal_da_matrix2' called here
;	1.	In a loop if need a DA matrix that spans a XANES energy range
;	2.	Try some matrix tests on it at the end.
;	
; Input:
;	p			pointer to 'fit_setup' parameter struct
;	pspec		pointer to spectrum being fitted
;	pback		pointer to background spectrum data
;	ppileup		pointer to pileup spectrum data
;	pars		pointer to saved DA pars struct from fit
;	/gamma		flags Gamma-rays (1) or X-rays/protons (0)
;	/pileup_mode we want to build a DA matrix without pileup row
;
; optional:
;	e_beam		array of beam_energy (keV) to calculate DA matrix series
;	E_scatter_merge		energy below which Compton and elastic are merged
;	
; Returns:
;				DA matrix struct (optionally with tag 'pmore' pointing to
;				a series of more DA matrix structs for XANES)

COMPILE_OPT STRICTARR
	if n_elements(gamma) lt 1 then gamma=0
	if n_elements(pileup_mode) lt 1 then pileup_mode=0
	if n_elements(E_scatter_merge) lt 1 then E_scatter_merge=10.0
	if ptr_good(pars,/struct) lt 1 then goto, bad
	if ptr_good(p,/struct) eq 0 then goto, bad_ptr
	if ptr_good(pspec,/struct) eq 0 then goto, bad_ptr
	if ptr_valid((*pspec).data) eq 0 then goto, bad_ptr
	if ptr_valid((*p).detector) eq 0 then goto, bad_ptr
	if pileup_mode eq 1 then begin
		if ptr_valid(ppileup) eq 0 then goto, bad_ptr
		if ptr_valid((*ppileup).data) eq 0 then goto, bad_ptr
		pileup = *(*ppileup).data
	endif else pileup = 0.0

	peaks = (*pars).peaks
	if ptr_good(peaks,/struct) eq 0 then goto, bad_peaks

	mask = (*pars).mask
	a = (*pars).a
	name = (*pars).name
	org = (*pars).org
	rorg = (*pars).rorg
	x = (*pars).x
	y = (*(*pspec).data)[x]
	size = max(x)+1
	ni = n_elements(x)
;	n_els = n_elements((*pars).array.rGamma[0,*])
	n_els = (*peaks).n_els

	pda = calc_da_matrix2( p, pspec, pback, pars, peaks, mask, a, matrix, rGamma, pqoff, $
			array=array, gamma=gamma, pileup_mode=pileup_mode, ppileup=ppileup)

	q = where( mask eq 1, nq)

;........................................................................................
; Test the matrix by multiplication with source spectrum ...
; Here it is assumed that rGamma is in normal detector number order.

	if array.on then begin
		channel = ptr_valid((*pspec).pactive) ? *(*pspec).pactive : ((*pspec).station-1)
		rG = (mask[7] eq 1) ? fltarr(n_els+1) : fltarr(n_els)
		for i=0L,n_elements(channel)-1 do begin
			if channel[i] lt array.n_det then begin
				rG = rG + rGamma[channel[i],*]
			endif
		endfor
		xd = reform(1./rG)
		print,'calc_da_matrix: total rN active = ',n_elements(channel)
		print,'calc_da_matrix: total rG = ',reform(rG)
	endif else begin
		xd = (mask[7] eq 1) ? replicate(1./float(array.n_det), n_els+1) : replicate(1./float(array.n_det), n_els)
	endelse
	if n_elements(xd) eq 1 then xd=xd[0]

	t = reform( matrix ## transpose((*(*pspec).data)[0:size-1]) )		; DA matrix multiply
	
	if gamma eq 0 then begin
		if mask[7] eq 1 then begin
			if mask[10] eq 1 then begin									;@9-19
				t[2:*] = t[2:*] / (*pspec).charge
				t = t*xd[pqoff]
				print, ' Matrix multiply: Back=',t[0:1]
				for i=2L,nq-1 do print, ' Matrix multiply: ',name[q[i]],' conc=',t[i],' ppm (wt)'
			endif else begin
				t[1:*] = t[1:*] / (*pspec).charge
				t = t*xd[pqoff]
				print, ' Matrix multiply: Back=',t[0]
			endelse
			for i=1L,nq-1 do print, ' Matrix multiply: ',name[q[i]],' conc=',t[i],' ppm (wt)'
		endif else begin
			t[*] = t[*] / (*pspec).charge
			t = t*xd[pqoff]
			for i=0L,nq-1 do print, ' Matrix multiply: ',name[q[i]],' conc=',t[i],' ppm (wt)'
		endelse
	endif

;	print, el
;	pt_fit_index = 3
;	print, name[org:*]
;	pt_array_index = pq[q2[pt_fit_index]]
;	window,2
;	t2 = gamma_matrix[*,pt_fit_index] * y
;	plot, t2
;	print, 'Total = ',total(t2)
;	print,rg[pt_array_index],(*pspec).charge*counts_per_ppm_uc[pt_array_index]
;	print, 'Conc = ',total(t2)/ (rg[pt_array_index] * (*pspec).charge*counts_per_ppm_uc[pt_array_index] )
;........................................................................................

;	Code for multiple DA matrices, one for each XANES energy step ...
;	Generate XANES energies file using XANES Image window Export menu.
;	Later need to pass 'eb' vector of energies from 'fit_setup' somehow.

	if n_elements(eb) eq 0 then goto, done

;	xfile = 'C:\NMP\AS\Mar-2012\Janssens\analysis\33488_33613_b_new_XANES\33488-33613-Cr-x-energies.csv'
;	eb = get_xanes_energies(xfile, do_xanes=do_xanes)
;	if do_xanes eq 0 then goto, done
	
	pt = ptr_new( *peaks)					; don't want changes to effect
	part = ptr_new( *pars)					; *peaks or *pars
	
	if max(eb) lt E_scatter_merge then begin
		calc_da_merge_scatter, part, pt		; merge Compton and elastic, if low 'eb'
	endif
	mask = (*part).mask
	a = (*part).a
	name = (*part).name
	n_els = (*pt).n_els

	qe = where( ((*pt).lines eq line_index('Compton')) or ((*pt).lines eq line_index('elastic')) or ((*pt).lines eq line_index('scatter')), nqe)
	if nqe eq 0 then goto, done

	neb = n_elements(eb)	
	pdm = ptrarr(neb)
	eb0 = (*pt).e_beam
	e0 = (*pt).e
	
;	First redo main matrix at e_beam, to merge scatter there too ...

	ptr_free, pda
	pda = calc_da_matrix2( p, pspec, pback, part, pt, mask, a, matrix, rGamma, pqoff, $
				array=array, gamma=gamma, pileup_mode=pileup_mode, ppileup=ppileup)

	for i=0,neb-1 do begin					; move scatter peak energies
		de = eb[i] - eb0
		print,'i=',i,' E_beam=',eb[i],' de=',de
		(*pt).e[qe] = e0[qe] + de
		(*pt).e_beam = eb[i]
		
		pdm[i] = calc_da_matrix2( p, pspec, pback, part, pt, mask, a, matrix, rGamma, pqoff, $
					array=array, gamma=gamma, pileup_mode=pileup_mode, ppileup=ppileup)
	endfor
	(*pda).pmore = ptr_new( pdm)		
	ptr_free, pt
	
done:
	return, pda

bad:
	warning, 'calc_da_loop', 'Missing pointer parameters', /error
	goto, bad_exit
bad_ptr:
	warning, 'calc_da_loop', 'Invalid pointers', /error
	goto, bad_exit
bad_peaks:
	warning, 'calc_da_loop', 'Invalid X-ray peaks data', /error
	goto, bad_exit

bad_exit:
	return, ptr_new()
	end
