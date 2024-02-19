	function fx_spectrum, p, icr=icr, dtp=dtp
	
	; Spectrum for all TAG=6,7 (16,17) records in p array
	
@falconx_listmode.def

	if ptr_good(p) eq 0 then return,0.0
	if ptr_good( (*p)[0], /struct ) eq 0 then return,0.0
	
	n = n_elements(*p)
	m = lonarr(n)
	for i=0L,n-1 do m[i] = ((*(*p)[i]).tag eq 6) or ((*(*p)[i]).tag eq 7) 	; pulse1, pulse2
	q = where( m eq 1, nq)
	if nq eq 0 then return, 0L
	m2 = lonarr(n)
	for i=0L,n-1 do m2[i] = ((*(*p)[i]).tag eq 2) or ((*(*p)[i]).tag eq 3)	; stats1, stats2
	q2 = where( m2 eq 1, nq2)

	k16 = 16 * 1024L
	t = lonarr(k16)	
	for i=0L,nq-1 do begin
		pd = (*p)[q[i]]
		b = (*(*pd).b)
		valid = long(ishft( b[0] and pulse1_valid_mask, pulse1_valid_offset))
		energy = long(ishft( b[0] and pulse1_energy_mask, pulse1_energy_offset))
		if (energy gt 0) and (energy lt k16) then t[energy] += 1
	endfor
	q1 = where( t gt 0, nq1)
	mx = max(q1) > 256

	icr = 0.
	dtp = 0.
	if nq2 gt 0 then begin
		for i=0L,nq2-1 do begin
			pd = (*p)[q2[i]]
			b = (*(*pd).b)
			if (*pd).tag eq 2 then begin
				icr += (float( b[stats1_est_icr_index]) / 100.)
				dtp += float( b[stats1_dt_percent_index]) / 100.
			endif else begin
				icr += (float( b[stats2_est_icr_index]) / 100.)
				dtp += float( b[stats2_dt_percent_index]) / 100.
			endelse
		endfor
		icr = icr / float(nq2)
		dtp = dtp / float(nq2)
	endif
	return, t[0:mx]
end
