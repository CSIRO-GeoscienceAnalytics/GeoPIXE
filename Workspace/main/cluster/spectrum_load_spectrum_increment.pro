pro spectrum_load_spectrum_increment, pp1, pp2, free=free, region=region

;	Increment all spectra pointed to by 'pp2' into those pointed to be 'pp1'
;	Both are pointers to pointer arrays --> spectrum struct.
;	
;	Not all indices will be present, so we need to match those in pp1 and pp2.
;	Need to identify the 'index' for each by (i) region #, or (ii) ADC #.
;	This is done in 'spectrum_index'.
;	
;	/free		if p2 is added into p1 then can free p2 spectrum
;	/region		region mode is different, as charge, IC don't need to be added.

	COMPILE_OPT STRICTARR
	if n_elements(free) eq 0 then free=0
	if n_elements(region) eq 0 then region=0
	
	if (ptr_good(pp1) eq 0) or (ptr_good(pp2) eq 0) then return
	indx1 = spectrum_index(*pp1)
	indx2 = spectrum_index(*pp2)
	n = (max([indx1, indx2]) + 1) < 10000
	m1 = intarr(n)
	m2 = intarr(n)
	m1[indx1] = 1
	m2[indx2] = 1

	ref1 = intarr(n)
	ref2 = intarr(n)
	ref1[indx1] = indgen(n_elements(indx1))			; back index pointers
	ref2[indx2] = indgen(n_elements(indx2))

;	Add together the histograms that only appear for the first detector in a set ...
	
	p1 = (*pp1)[0]
	p2 = (*pp2)[0]
	if ptr_good(p1,/struct) and ptr_good(p2,/struct) then begin
		if ptr_good( (*p1).pileup_loss_det) and ptr_good( (*p2).pileup_loss_det) then begin
			*(*p1).pileup_loss_det = *(*p1).pileup_loss_det * (*p1).charge + *(*p2).pileup_loss_det * (*p2).charge
			*(*p1).pileup_loss_det = *(*p1).pileup_loss_det / ( (*p1).charge + (*p2).charge)
		endif else if (ptr_good((*p1).pileup_loss_det,/struct) eq 0) and ptr_good((*p2).pileup_loss_det,/struct) then begin
			(*p1).pileup_loss_det = ptr_new (*(*p2).pileup_loss_det)
			(*p1).had_pileup = 1
		endif
	endif
	if ptr_good(p1,/struct) and ptr_good(p2,/struct) then begin
		if ptr_good( (*p1).deadtime_det) and ptr_good( (*p2).deadtime_det) then begin
			*(*p1).deadtime_det = *(*p1).deadtime_det * (*p1).charge + *(*p2).deadtime_det * (*p2).charge
			*(*p1).deadtime_det = *(*p1).deadtime_det  / ( (*p1).charge + (*p2).charge)
		endif else if (ptr_good((*p1).pileup_loss_det,/struct) eq 0) and ptr_good((*p2).pileup_loss_det,/struct) then begin
			(*p1).deadtime_det = ptr_new (*(*p2).deadtime_det)
			(*p1).had_dead = 1
		endif
	endif
	
;	Add together spectra which are common in the two lists ...
;	This assumes that a detector index appears at most once in each list.

	q = where( (m1 eq 1) and (m2 eq 1), nq)
	if nq gt 0 then begin
		q1 = ref1[q]								; back pointers in indx2,pp2 array
		q2 = ref2[q]
		for i=0L,nq-1 do begin
			p1 = (*pp1)[q1[i]]
			p2 = (*pp2)[q2[i]]
			if (ptr_good(p1,/struct) eq 0) or (ptr_good(p2,/struct) eq 0) then break
			ns1 = (*p1).size < 8192	
			ns2 = (*p2).size < 8192	
			if ns2 le ns1 then begin				; add p2 data into p1
				if ns2 gt 0 then begin
					(*(*p1).data)[0:ns2-1] = (*(*p1).data)[0:ns2-1] + (*(*p2).data)[0:ns2-1]
					if (*p1).has_errors and (*p2).has_errors then begin
						(*(*p1).error)[0:ns2-1] = sqrt((*(*p1).error)[0:ns2-1]^2 + (*(*p2).error)[0:ns2-1]^2)
					endif
				endif
			endif else begin						; add p1 data into p2, then copy data back
				if ns1 gt 0 then begin
					(*(*p2).data)[0:ns1-1] = (*(*p2).data)[0:ns1-1] + (*(*p1).data)[0:ns1-1]
					if (*p1).has_errors and (*p2).has_errors then begin
						(*(*p2).error)[0:ns1-1] = sqrt((*(*p2).error)[0:ns1-1]^2 + (*(*p1).error)[0:ns1-1]^2)
					endif
				endif
				*(*p1).data = *(*p2).data			; copy back
				(*p1).size = (*p2).size
			endelse
			
;			Assumes that each spectrum has an increment to charge, IC_total.
;			Need a charge weighted sum for the DT correction factor.
;				
;			Except for region spectra from the processes, where each is assigned the total
;			"live" charge and flux for the region already (not just this range of EVT file).
;			Also, DT_corr is defined at 1.00 for these regions.

			if region eq 0 then begin
				(*p1).deadtime_correction = (*p1).charge*(*p1).deadtime_correction + (*p2).charge*(*p2).deadtime_correction 
				(*p1).charge = (*p1).charge + (*p2).charge 
				(*p1).IC_total = (*p1).IC_total + (*p2).IC_total 
				(*p1).deadtime_correction = (*p1).deadtime_correction / (*p1).charge

				(*p1).energy = (*p1).energy > (*p2).energy 						; these add lines also in routine:
			endif																; "spectrum_Add" in "spectrum_display_eventcb"

			(*p1).processed = (*p1).processed + (*p2).processed 
			(*p1).valid = (*p1).valid + (*p2).valid 
			(*p1).bad_xy = (*p1).bad_xy + (*p2).bad_xy 
			(*p1).clipped = (*p1).clipped + (*p2).clipped 
			
			if (*p1).n_fit gt 0 then begin			; free any overlays
				for j=0L,(*p1).n_fit-1 do begin
					free_spectrum, (*p1).fit[j] 
				endfor
				(*p1).n_fit = 0
			endif
			if free then free_spectrum, p2
		endfor
	endif
	
	q = where((m1 eq 0) and (m2 eq 1), nq)
	if nq gt 0 then begin
		q2 = ref2[q]								; back pointers in indx2,pp2 array
		*pp1 = [*pp1, (*pp2)[q2]]
		indx1 = [indx1, indx2[q2]]
		q = sort(indx1)
		*pp1 = (*pp1)[q]
		indx1 = indx1[q]
	endif
	return
end
