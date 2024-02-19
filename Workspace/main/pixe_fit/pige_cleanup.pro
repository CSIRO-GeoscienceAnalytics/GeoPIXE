	pro pige_cleanup, peaks, e_low, e_high, cal_a, cal_b, w0, w1, $
		dcal_a, dcal_b, dw0, dw1, na, org, mask, a, err, name, note, $
		n_els, aname, area, aerr, mdl, fit, x, f_chi, chi, rms

;	Routine to cleanup results, scale errors, etc., determine detection limits.

COMPILE_OPT STRICTARR
common c_fit_memory_1, fit_memory_on
if n_elements(fit_memory_on) lt 1 then fit_memory_on=0

	if n_elements(peaks) lt 1 then goto, done
	if ptr_valid(peaks) eq 0 then goto, done
	if n_elements(f_chi) lt 1 then goto, done
	if n_elements(a) lt (org+2*n_els) then goto, done

	aw = 1.0645								; convert height*FWHM to Gaussian peak area
	x_low = (e_low - cal_b)/cal_a
	x_high = (e_high - cal_b)/cal_a
	e_origin = e_low + 0.3 * (e_high - e_low)
	fwhm_origin = e_low

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

	if (mask[0] eq 1) and fit_memory_on then begin
		(*detector).w0 = w0
	endif
	if (mask[1] eq 1) and fit_memory_on then begin
		(*detector).w1 = w1
	endif
	
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

;---- areas, errors, MDL -----------------------------------------------------------------

	area = A[org:org+n_els-1]
	aerr = err[org:org+n_els-1]
	mdl = fltarr(n_els)
	aname = name[org:org+n_els-1]
	n_channels = n_elements(fit)

	for k=0L,n_els-1 do begin
		nl = (*peaks).n_lines[k]
		if nl gt 0 then begin
			rmajor = (*peaks).intensity[0,k]
			order = reverse( sort( (*peaks).intensity[0:nl-1,k] ))
			no = n_elements(order)
			e = (*peaks).e[order,k]
			c = (e-cal_b)/cal_a
			q = where( (c ge x_low) and (c le x_high) and ((*peaks).intensity[order,k] gt 0.03))
			if q[0] ne -1 then begin
				order = order[q]
				no = n_elements(order)
				sr = 0.0
				sb = 0.0
				schi = 0.0

;				For every line of this element, determine background and reduced chi-squared summed
;				within 0.5 FWHM of each side of peak.

				for i=0L,no-1 do begin
					e = (*peaks).e[order[i],k]
					rel = (*peaks).intensity[order[i],k]
					c = (e-cal_b)/cal_a
					d = sqrt( A[0]*A[0] + A[1]*A[1] * (e-fwhm_origin))		; FWHM channels
					w = d*cal_a
					j = fix(d/2.) > 1
					i1 = (c-j) > 0
					i2 = (c+j) < (n_channels-1)
					sb = d * A[org+n_els+k]	* rel							; back level
					schi = schi + sqrt(total( f_chi[i1:i2]) / (2.*j)) * rel
					sr = sr + rel
				endfor
				sb = sb / (sr > 0.1)
				schi = schi / (sr > 0.1)

				oerr = aerr[k]
				t = (schi > 1.) * aerr[k]
				aerr[k] = sqrt( (t*t + (sb)/(rmajor*rmajor)) )
				mdl[k] = 3.29 * ( sqrt(sb) > 1.) / rmajor
		;		print,aname[k],'sb=',sb,' oerr=',oerr,' schi=',schi,' aerr=',aerr[k],' mdl=',mdl[k]
			endif
		endif else begin
			note[org+k] = note[org+k] + '; no significant lines remain in fit'
		endelse
	endfor

done:
	return
	end
