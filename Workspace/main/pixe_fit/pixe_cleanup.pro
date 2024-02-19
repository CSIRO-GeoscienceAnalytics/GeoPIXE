	pro pixe_cleanup, peaks, e_low, e_high, cal_a, cal_b, w0, w1, $
		dcal_a, dcal_b, dw0, dw1, $
		na, org, rorg, mask, a, err, name, note, $
		n_els, aname, area, aerr, mdl, pileup_ratio, background, $
		fit, x, f_chi, chi, rms, pileup_mode=pileup_mode, detector=detector, $
		compton=compton, silent=silent

;	Routine to cleanup results, scale errors, etc., determine detection limits.

COMPILE_OPT STRICTARR
common c_fit_memory_1, fit_memory_on
if n_elements(fit_memory_on) lt 1 then fit_memory_on=0

	if n_elements(pileup_mode) lt 1 then pileup_mode=0
	if n_elements(silent) lt 1 then silent=0
	if n_elements(peaks) lt 1 then goto, done
	if ptr_valid(peaks) eq 0 then goto, done
	if n_elements(background) lt 1 then goto, done
	if n_elements(f_chi) lt 1 then goto, done
	if n_elements(a) lt (org+n_els) then goto, done

	aw = 1.0645							; convert height*FWHM to Gaussian peak area
	x_low = (e_low - cal_b)/cal_a
	x_high = (e_high - cal_b)/cal_a
	e_origin = e_low + 0.3 * (e_high - e_low)
	fwhm_origin = e_low
	alpha_zero = -0.05
	gamma_zero = 0.1

;---- rms error --------------------------------------------------------------------------

	sum = total( fit[x])
	nq = n_elements( where(mask eq 1))
	nx = n_elements(x)
	nfree = nx - nq
	rms = sqrt( ((chi-1.) > 0.) * nfree / sum)

;---- Refined cal ------------------------------------------------------------------------

	cal_a = 1./a[3]
	cal_b = e_origin - a[2]/a[3]
	w1 = (a[1]/a[3]) * (a[1]/a[3])
	w0 = (a[0]*a[0] - a[1]*a[1]*fwhm_origin) / (a[3]*a[3])

;---- cal, width uncertainties -----------------------------------------------------------

	ca2 = cal_a*cal_a
	if mask[3] eq 1 then begin
		dcal_a = err[3]*ca2
	endif else begin
		dcal_a = 0.0
	endelse
	if mask[2] eq 1 then begin
		dcal_b = sqrt( ( err[2]*err[2] + err[3]*err[3]*cal_b*cal_b )*ca2 )
	endif else begin
		dcal_b = 0.0
	endelse

	if mask[0] eq 1 then begin
		d0 = ( a[0]*a[0] - fwhm_origin*a[1]*a[1] )
		dw0 = 2.*ca2* sqrt( a[0]*a[0]*err[0]*err[0] + fwhm_origin*fwhm_origin* $
				a[1]*a[1]*err[1]*err[1] + d0*d0*err[3]*err[3] )
	endif else begin
		dw0 = 0.0
	endelse
	if mask[1] eq 1 then begin
		d1 = a[1]*a[1]
		dw1 = 2.*ca2* sqrt( a[1]*a[1]*err[1]*err[1] + $
				d1*d1*err[3]*err[3] )
	endif else begin
		dw1 = 0.0
	endelse

;	if fit_memory_on then begin					;@29-3-16
		if (mask[0] eq 1) then begin
			(*detector).w0 = w0
		endif
		if (mask[1] eq 1) then begin
			(*detector).w1 = w1
		endif
;	endif
	
;---- update detector parameters ---------------------------------------------------------

	if (mask[5] eq 1) and fit_memory_on then begin
		(*detector).tail.amp = (*detector).tail.amp * (alpha_zero + a[5]*a[5])
		a[5] = sqrt(1. - alpha_zero)
	endif
	if (mask[6] eq 1) and fit_memory_on then begin
		(*detector).tail.L = (*detector).tail.L * (gamma_zero + a[6]*a[6])
		(*detector).tail.S = (*detector).tail.S * (gamma_zero + a[6]*a[6])
		a[6] = sqrt(1. - gamma_zero)
	endif

;---- update Compton tail parameters -----------------------------------------------------

	rho_zero = 0.2
	eta_zero = 0.1
	if (mask[8] eq 1) and fit_memory_on then begin
		(*compton).tail.amp = (*compton).tail.amp * (rho_zero + a[8]*a[8])
		a[8] = sqrt(1. - rho_zero)
	endif
	if (mask[9] eq 1) and fit_memory_on then begin
		(*compton).tail.len = (*compton).tail.len * (eta_zero + a[9]*a[9])
		a[9] = sqrt(1. - eta_zero)
	endif

;---- areas, errors, MDL -----------------------------------------------------------------

	isum = na-1
	if (pileup_mode eq 1) and (name[0] eq 'sum') then isum = 0

	if pileup_ratio gt 0.2 then begin
		pileup_ratio = 0.2
		if silent eq 0 then begin
			warning,'pixe_cleanup',['High pileup obtained in the fit.','','Check that the energy range enables', $
					'an effective fit of pile-up contributions.']
			silent = 1
		endif
		if name[isum] eq 'sum' then note[isum]='Unphysical pileup ratio constrained'
	endif else if pileup_ratio lt 0.0 then begin
		pileup_ratio = 0.0
		if name[isum] eq 'sum' then note[isum]='Negative pileup ratio constrained'
	endif else begin
		if name[isum] eq 'sum' then note[isum]='Pileup ratio = '+string(pileup_ratio)
	endelse
	print,' Pileup_ratio:', pileup_ratio

	n_channels = n_elements(fit)
	nk = n_elements((*peaks).e[*,0])

	on = intarr(nk,n_els)
	for k=0L,n_els-1 do begin
		if (*peaks).n_lines[k] gt 0 then on[0:(*peaks).n_lines[k]-1,k] = 1
	endfor
	row = replicate(1,nk) # indgen(n_els)
	area = A[org:*] * (1. + pileup_ratio)			; correct for pileup losses
	aerr = err[org:*] * (1. + pileup_ratio)
	mdl = fltarr(n_els)
	aname = name[org:*]

	for k=0L,n_els-1 do begin
		onk = on
		nl = (*peaks).n_lines[k]
		if nl gt 0 then begin
			rmajor = (*peaks).intensity[0,k]
			order = reverse( sort( (*peaks).intensity[0:nl-1,k] ))
			no = n_elements(order)
			e = (*peaks).e[order,k]
			onk[order,k] = 0
			c = (e-cal_b)/cal_a
			q = where( (c ge x_low) and (c le x_high) and ((*peaks).intensity[order,k] gt 0.03))
			if q[0] ne -1 then begin
				order = order[q]
				no = n_elements(order)
				sr = 0.0
				sb = 0.0
				serr = 0.0
				schi = 0.0

;				For every line of this element, determine background and reduced chi-squared summed
;				within 0.5 FWHM of each side of peak.
;
;				Also, look for all lines from other elements that fall within 2*FWHM.
;				Determine the overlap interference contribution to error from these, weighted by
;				Gaussian weighted energy difference.

				for i=0L,no-1 do begin
					e = (*peaks).e[order[i],k]
					rel = (*peaks).intensity[order[i],k]
					c = (e-cal_b)/cal_a
					d = sqrt( A[0]*A[0] + A[1]*A[1] * (e-fwhm_origin))		; FWHM channels
					w = d*cal_a
					j = fix(d/2.) > 1
					i1 = (c-j) > 0
					i2 = (c+j) < (n_channels-1)

;					tb = total( background[i1:i2] > 1.) * rel
;					sb = sb + tb*tb

;					Just use back under major line for MDL estimation ...
					if i eq 0 then sb = total( background[i1:i2] > 1.)

					schi = schi + sqrt(total( f_chi[i1:i2]) / (2.*j)) * rel
					sr = sr + rel

					de = abs((*peaks).e - e)
					q = where( (de le 2.*w) and (onk eq 1) )
					if q[0] ne -1 then begin
						lerr = 0.01 * area[row[q]] * (*peaks).intensity[q]
						serr = serr + rel * total( lerr * exp(-de[q]*de[q]/(2.*w*w)) )
					endif
				endfor
				sb = sb
;				sb = sb / (sr > 0.1)
				schi = schi / (sr > 0.1)
				serr = serr / (sr > 0.1)

				oerr = aerr[k]
				t = (schi > 1.) * aerr[k]
				aerr[k] = sqrt( (t*t + (serr + sb)/(rmajor*rmajor)) )
				mdl[k] = 3.29 * ( sqrt(serr + sb) > 1.) / rmajor
		;		print,aname[k],'sb=',sb,' serr=',serr,' oerr=',oerr,' schi=',schi,' aerr=',aerr[k],' mdl=',mdl[k]
			endif
		endif else begin
			note[org+k] = note[org+k] + '; no significant lines remain in fit'
		endelse
	endfor

done:
	return
	end
