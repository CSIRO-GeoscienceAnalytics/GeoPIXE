pro sum_peaks, a, mask, name, note, do_tail, org, na, peaks, new=new, $
		pileup_ratio=pileup_ratio, e_high=e_high, sum_deficit=sum_deficit

;	If /new then build a new sum element, and append it to the list
;	If new=0 (default), then assume that last element is the sum element,
;	and just update the line list, intensities for it.
;	sum_deficit = % deficit in sum peak amplitudes due to finite time resolution.

	check_double = 30		; number of intense lines to combine for double PU		10
	check_triple = 6		; number of intense lines to combine for triple PU		4

	if n_elements(new) lt 1 then new=0
	if n_elements(e_high) lt 1 then e_high=100000.
	if n_elements(sum_deficit) lt 1 then sum_deficit=0.1

	n_els = 0
	if ptr_valid(peaks) eq 0 then goto, bad
	n_els = (*peaks).n_els
	if n_els lt 1 then goto, bad
	q = where( name eq 'sum')
	if (q[0] eq -1) and (new ne 1) then return

	if new then begin
		asum = 3.0												; for testing !!!
		area = [replicate(200.,(*peaks).n_els),3.0]
	endif else begin
		if mask[na-1] eq 0 then begin
			goto, bad
		endif else begin
			asum = a[na-1]
			i_sum = n_els-1
			area = a[org:*]
		endelse
	endelse
	use_mu_zero = 0
	mu_zero = 0.0
	if n_elements( (*peaks).mu_zero) gt 1 then use_mu_zero=1

	n_lines = (*peaks).n_lines
	nk = n_elements((*peaks).e[*,0])

	ar = fltarr(nk*n_els)
	el = strarr(nk*n_els)
	e = fltarr(nk*n_els)
	id = intarr(nk*n_els)

;	Form arrays of intensity and energy for all significant lines

	k = 0
	for i=0L,n_els-1 do begin
		if (n_lines[i] gt 0) and (area[i] gt 0.1) and (name[org+i] ne 'sum') then begin
			x = indgen(n_lines[i])
			ar[k+x] = area[i] * (*peaks).intensity[0:n_lines[i]-1,i]
			e[k+x] = (*peaks).e[0:n_lines[i]-1,i]
			el[k+x] = element_name( (*peaks).z[i]) + ' ' + line_id( (*peaks).lines[0:n_lines[i]-1,i])
			id[k+x] =(*peaks).lines[0:n_lines[i]-1,i]
			k = k + n_lines[i]
		endif
	endfor

	q = where( ar gt 100.)							; only use lines over 100 counts
	if q[0] eq -1 then goto, bad
	ar = ar[q]
	e = e[q]
	el = el[q]
	id = id[q]

;	Sort list by intensity

	q = reverse( sort(ar))							; descending order
	ar = ar[q]
	e = e[q]
	el = el[q]
	id = id[q]

	q = where( ar gt 0.0003*ar[0])					; select only most intense
	ar = ar[q]
	e = e[q]
	el = el[q]
	id = id[q]

	if new then begin
		p_factor = 0.0001							; triple / double ratio
	endif else begin
		p_factor = asum / ar[0]
	endelse

;	Build array of double and triple sums

	n2 = n_elements(ar) < check_double
	a2 = ar[0:n2-1]

	row2 = indgen(n2) # replicate(1,n2)
	col2 = replicate(1,n2) # indgen(n2)
	diagonal = indgen(n2)*(n2+1)

	d2 = a2#a2										; double intensity products
;	d2[diagonal] = d2[diagonal] / 2.0				; off-diagonal already counted twice? (NO)

	dm2 = d2
	if n2 gt 1 then dm2[1:*,*] = 0.0				; contain major line only

	q = reverse( sort(d2))

	pu_sum_factor = 1. - sum_deficit/100.			; pu sum scale factor reflecting deficit

	r2 = d2[q] / d2[0]								; not q[0] here - main peak is product of majors
	e2 = pu_sum_factor * (e[row2[q]] + e[col2[q]])
	el2 = el[row2[q]] + ' + ' + el[col2[q]]
	id2 = replicate( line_index('sum'), n_elements(q))
	qid = where( (id[row2[q]] eq line_index('Compton')) or  $
				(id[col2[q]] eq line_index('Compton')) )
	if qid[0] ne -1 then id2[qid] = line_index('Compton')
	qid = where( (id[row2[q]] eq line_index('Compton')) and  $
				(id[col2[q]] eq line_index('Compton')) )
	if qid[0] ne -1 then id2[qid] = line_index('Compton2')		; Compton+Compton pileup flagged as "Compton2" ID

	rmain = total(dm2/d2[0])						; total containing main line
	pileup_ratio = p_factor * rmain

	q = where( asum * r2 gt 0.3)
	if q[0] eq -1 then goto, bad
	nq = n_elements(q)
	r2 = reform( r2[q], nq)							; final significant doubles
	e2 = reform( e2[q], nq)
	el2 = reform( el2[q], nq)
	id2 = reform( id2[q], nq)
	n2 = n_elements(r2)

	if (new eq 0) and (asum gt 1000.0) and (p_factor gt 3.0e-4) then begin
		n3 = (n2 < check_triple) < n_elements(ar)
		a3 = ar[0:n3-1]

		row3 = indgen(n3) # replicate(1,n3)
		col3 = replicate(1,n3) # indgen(n3)
		diagonal = indgen(n3)*(n3+1)

		d3 = a3#a3
		s3 = replicate(2.0, n3,n3)
;		s3[diagonal] = s3[diagonal] / 2.0			; off-diagonal already counted twice? (NO)
		d3 = reform(d3,n3*n3) * reform(s3,n3*n3)

		q = reverse( sort(d3))

		d3 = d3[q]
		e3 = pu_sum_factor * (e[row3[q]] + e[col3[q]])
		el3 = el[row3[q]] + ' + ' + el[col3[q]]
		id3 = replicate( line_index('sum'), n_elements(q))
		qid = where( (id[row3[q]] eq line_index('Compton')) or  $
					(id[col3[q]] eq line_index('Compton')) )
		if qid[0] ne -1 then id3[qid] = line_index('Compton')
		qid = where( (id[row3[q]] eq line_index('Compton')) and  $
					(id[col3[q]] eq line_index('Compton')) )
		if qid[0] ne -1 then id3[qid] = line_index('Compton2')

		d4 = d3#a3									; triple intensity products
		s4 = replicate(3.0, n3*n3,n3)

		row4 = indgen(n3*n3) # replicate(1,n3)
		col4 = replicate(1,n3*n3) # indgen(n3)
		diagonal2 = indgen(n3)*(n3*(n3+1)+1)

;		s4[diagonal2] = s4[diagonal2] / 3.0			; off-diagonal already counted multiply? (NO)
		d4 = reform(d4,n3*n3*n3) * reform(s4,n3*n3*n3)

		q = reverse( sort(d4))

		r4 = p_factor * d4[q] / d4[0]
		e4 = pu_sum_factor * (e3[row4[q]] + e[col4[q]])
		el4 = el3[row4[q]] + ' + ' + el[col4[q]]
		id4 = replicate( line_index('sum'), n_elements(q))
		qid = where( ((id3[row4[q]] eq line_index('Compton')) or (id3[row4[q]] eq line_index('Compton2'))) or  $
					((id[col4[q]] eq line_index('Compton')) or (id[col4[q]] eq line_index('Compton2'))) )
		if qid[0] ne -1 then id4[qid] = line_index('Compton')
		qid = where( ((id3[row4[q]] eq line_index('Compton')) or (id3[row4[q]] eq line_index('Compton2'))) and  $
					((id[col4[q]] eq line_index('Compton')) or (id[col4[q]] eq line_index('Compton2'))) )
		if qid[0] ne -1 then id4[qid] = line_index('Compton2')

		q = where( asum * r4 gt 0.3)
		if q[0] ne -1 then begin
			nq = n_elements(q)
			r4 = reform( r4[q], nq)					; final significant triples
			e4 = reform( e4[q], nq)
			el4 = reform( el4[q], nq)
			id4 = reform( id4[q], nq)

			r2 = [r2, r4]
			e2 = [e2, e4]
			el2 = [el2, el4]
			id2 = [id2, id4]
			n2 = n_elements(r2)
		endif
	endif

;	Histogram on energy to merge close lines (within 60 eV)

	h = histogram( e2, binsize=0.06, reverse_indices=r)
	n = n_elements(h)
	if n gt 1 then begin
		q = where( (r[0:n-2] ne r[1:n-1]) and (r[1:n-1]-1 gt r[0:n-2]))
		if q[0] ne -1 then begin
			for i=0L,n_elements(q)-1 do begin
				ri1 = r[q[i]]
				ri2 = r[q[i]+1]-1
				if (ri1 ge n) and (ri2 ge ri1) then begin
					p = r[ri1:ri2]
					q2 = where( (p ge 0) and (p lt n_elements(e2)) and (id2[p] ne line_index('Compton')) and (id2[p] ne line_index('Compton2')), nq2)
					if nq2 gt 1 then begin
						rr = total(r2[p[q2]])
						er = total(e2[p[q2]] * r2[p[q2]]) / rr
						r2[p[q2[0]]] = rr
						e2[p[q2[0]]] = er
						r2[p[q2[1:*]]] = 0.0
						e2[p[q2[1:*]]] = 0.0
						t = id2[p[q2]]
						id2[p[q2[0]]] = line_index('sum')

;						qid = where( (t eq line_index('Compton')) )
;						if qid[0] ne -1 then begin
;							if total(r2[p[q2[qid]]]) gt 0.5*rr then begin
;								id2[p[q2[0]]] = line_index('Compton')
;							endif
;						endif
;						qid = where( (t eq line_index('Compton2')) )
;						if qid[0] ne -1 then begin
;							if total(r2[p[q2[qid]]]) gt 0.5*rr then begin
;								id2[p[q2[0]]] = line_index('Compton2')
;							endif
;						endif
					endif
				endif
			endfor
		endif
	endif
	q = where( asum*r2 gt 0.3, n2)
	if q[0] eq -1 then goto, bad
	r2 = r2[q]
	e2 = e2[q]
	el2 = el2[q]
	id2 = id2[q]

;	Remove all out of range lines

	q = where( e2 le e_high, n2)
	if q[0] eq -1 then goto, bad
	r2 = r2[q]
	e2 = e2[q]
	el2 = el2[q]
	id2 = id2[q]

;	Sort on intensity to form final list

	q = reverse( sort(r2))							; descending intensity order
	r2 = r2[q] / r2[q[0]]
	e2 = e2[q]
	el2 = el2[q]
	id2 = id2[q]

	if new and (n2 gt 0) then begin
		nk2 = nk > 40
		q = where( e2 le e_high, n2)
		if n2 eq 0 then return						; this leaves us with NO sum. Is this OK?
		n2 = n2 < nk2
		n_els = n_els+1
		mask = [mask,1]
		e = fltarr(nk2,n_els)
		intensity = fltarr(nk2,n_els)
		if use_mu_zero then mu_zero = fltarr(nk2,n_els)
		lines = intarr(nk2,n_els)
		n_lines = intarr(n_els)

		ratio_yield = 0.
		array = (*peaks).array
		ratio_intensity = 1.0
		if array then begin
			n_det = n_elements((*peaks).ratio_yield[*,0])
			ratio_yield = replicate( 1.0, n_det,n_els)
			ratio_yield[*,0:n_els-2] = (*peaks).ratio_yield
			if tag_present('ratio_intensity', *peaks) then begin
				ratio_intensity = replicate( 1.0, n_det, nk2, n_els)
				ratio_intensity[*,0:nk-1,0:n_els-2] = (*peaks).ratio_intensity
			endif
		endif

		e[0:nk-1,0:n_els-2] = (*peaks).e[0:nk-1,0:n_els-2]
		intensity[0:nk-1,0:n_els-2] = (*peaks).intensity[0:nk-1,0:n_els-2]
		if use_mu_zero then mu_zero[0:nk-1,0:n_els-2] = (*peaks).mu_zero[0:nk-1,0:n_els-2]
		lines[0:nk-1,0:n_els-2] = (*peaks).lines[0:nk-1,0:n_els-2]
		n_lines[0:n_els-2] = (*peaks).n_lines[0:n_els-2]

		i_sum = n_els-1
		e[0:n2-1,i_sum] = e2[q[0:n2-1]]
		intensity[0:n2-1,i_sum] = r2[q[0:n2-1]]
		lines[0:n2-1,i_sum] = id2[q[0:n2-1]]
		n_lines[i_sum] = n2

;	N.B. This has to match the form in 'calc_da_merge_scatter' ('correct_da_loop'), 'correct_lines',
;	'make_peaks', 'read_yield', 'select_element_lines', 'sum_peaks', 'append_escapes', 'pixe_initial'

;		xrays = {n_els:n_els, z:[(*peaks).z,0], shell:[(*peaks).shell,0], n_lines:n_lines, lines:lines, $
;				e:e, intensity:intensity, n_layers:(*peaks).n_layers, $
;				yield:[(*peaks).yield,replicate(1.0,1,(*peaks).n_layers)], $
;				layers:(*peaks).layers, unknown:(*peaks).unknown, $
;				e_beam:(*peaks).e_beam, theta:(*peaks).theta, phi:(*peaks).phi, $
;				alpha:(*peaks).alpha, beta:(*peaks).beta, $
;				title:(*peaks).title, file:(*peaks).file, free:mask[org:*], $
;				formula:(*peaks).formula, weight:(*peaks).weight, thick:(*peaks).thick, $
;				microns:(*peaks).microns, density:(*peaks).density, $
;				z1:(*peaks).z1, a1:(*peaks).a1, state:(*peaks).state, $
;				emin:(*peaks).emin, emax:(*peaks).emax, mu_zero:mu_zero, $
;				ratio_yield:ratio_yield, array:(*peaks).array, detector_file:(*peaks).detector_file, $
;				ratio_intensity:ratio_intensity  }

		xrays = make_peaks( z2=[(*peaks).z,0], shell=[(*peaks).shell,0], n_lines=n_lines, lines=lines, $
				e_lines=e, intensity=intensity, yield=[(*peaks).yield,replicate(1.0,1,(*peaks).n_layers)], $
				free=mask[org:*], mu_zero=mu_zero, ratio_yield=ratio_yield, ratio_intensity=ratio_intensity, $
				default=peaks )

		ptr_free, peaks
		peaks = ptr_new( xrays, /no_copy)

		a = [a,asum]
		na = na+1
		do_tail = [do_tail,1]									; was "0"
		name = [name,'sum']
		note = [note,'Pileup fraction = '+string(pileup_ratio)]
	endif else begin
		n2 = n2 < nk
		(*peaks).e[0:n2-1,i_sum] = e2[0:n2-1]
		(*peaks).intensity[0:n2-1,i_sum] = r2[0:n2-1]
		(*peaks).lines[0:n2-1,i_sum] = id2[0:n2-1]
		(*peaks).n_lines[i_sum] = n2
		note[org+i_sum] = 'Pileup fraction = '+string(pileup_ratio)
		q = where( e2[0:n2-1] le e_high, n2)
		mask[na-1] = (q[0] ne -1) ? 1 : 0
		(*peaks).free[i_sum] = mask[na-1]
	endelse

	return

bad:
	if (new eq 0) and (n_els gt 1) and (name[na-1] eq 'sum') then begin
		mask[na-1] = 0
		a[na-1] = 0.0
		note[na-1] = 'No significant pile-up'
	endif
	return
	end
