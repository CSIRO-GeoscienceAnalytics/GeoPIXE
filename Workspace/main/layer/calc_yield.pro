function calc_yield, state, cos_beam, cos_detector, slices, z2, shell, e_lines, xsect, cmux, $
				branch_ratio, fluoro, jump, n_lines, n_layers, lid, e_bind, intensity=intensity, $
				sec_fl=sec_fl, relmux=relmux, sec_yield=sec_yield, $
				yieldx=yieldx, photo=photo, flux=flux, zero=zero, beam=beam

;	Calculate PIXE/SXRF (or PIGE) yield values for all lines/elements at each slice
;	Lines are in descending intensity order, so major line is always index 0.
;
;	Note that 1 uC of charge is assumed. For photons this means 6.242e+12 photons.
;
;	Input:
;		state				charge state
;		cos_beam			cosine for beam incidence (made by 'experimental_angles')
;		cos_detector		cosine for outgoing X-rays ('experimental_angles', negative for transmission target)
;		slices[j]			struct array giving details of each target slice ('calc_slices')
;		z2[i]				atomic numbers of target elements  (from 'get_lines')
;		shell[i]			shell index (K=1, L=2, M=3) of target elements  (from 'get_lines')
;		e_bind[i]			binding energy / edge
;		xsect[i,n]			cross-section for each element, at half-slice steps 'n' ('calc_cross')
;		xsect[i,e]			or across continuum spectrum 'e' if continuum mode ('calc_cross')
;		cmux[k,i,n]			mass-atten coeff. for line 'k', element 'i', half-step 'n' ('calc_abs')
;		e_lines[k,i]		X-ray line energies, line 'k', element 'i' (only used in sec fl.)
;		branch_ratio[k,i]	branching ratios (from 'get_lines')
;		branch_ratio[k,i,n]	over [k,i,n] for half-slices 'n', for continuum spectrum source
;		fluoro[i]			fluorescent yields (from 'get_lines')
;		n_lines[i]			number of X-ray lines for each element 'i'
;		n_layers			number of layers
;		lid[j]				layer ID of each slice (from 'calc_slices')
;		relmux[k,i,i2,l]	relative mux for source line 'k', element 'i',
;							fluorescing destination element 'i2' in layer 'l'
;		/sec_fl				enables secondary fluorescence (requires relmux too).
;		/photo				photon induced ionization.
;		beam				continuum beam spec, if present
;		flux				relative flux decay from surface.
;
;	Note that 'cmux' and returned 'xsect' are both in half-slice steps (start at surface).
;		'xsect' is different for continuum case and ions/Mono Xrays.
;		Ions/mono Xrays:	xsect[ elements, slices]
;		Continuum:			xsect[ elements, spectrum]
;
;	Output:
;		intensity[k,i,l]	relative intensities (rel to major) for
;							line 'k', element 'i', for layer 'l'
;		yieldx[h,i]			yield of major line, element 'i', half-slice steps 'h'
;		sec_yield[]			yield due to secondary-fluorescence
;		zero[i]				flags zero xsect for this element
;
;	Returns:
;		major_yield[i,l]	yield of major line for element 'i', layer 'l'
;

	COMPILE_OPT STRICTARR
	if n_elements(sec_fl) lt 1 then sec_fl=0
	if n_elements(relmux) lt 1 then sec_fl=0
	
	avogadro = 6.02252D23			; avogadro's number
	ions_per_microC = 6.2418D12		; particles per uC charge
	four_pi = 12.5663706D0
	mgm = 1.0D-3					; grams to milligrams
	charge = 1.0D0					; charge/micro_coulomb				for yield in units of
	omega = 1.0D-3					; solid angle 1 milli-steradian		/uC.mstr

	if n_elements(beam) lt 1 then begin					; no beam spectrum flags ions/mono Xrays
		continuum = 0									; 'xsect' over slices
		n_hs = n_elements( xsect[0,*])
	endif else begin
		continuum = beam[0].continuum					; for a continuum source, e_beam is maximum energy
		n_hs = n_elements( branch_ratio[0,0,*])			; 'xsect' over spectrum, 'branch_ratio' over slices
	endelse
	if photo eq 0 then continuum = 0					; no "continuum" for ions
	fluence = ions_per_microC
	if continuum then fluence = 1.						; in this case 'beam' spectrum shows number of photons

	solid = mgm*omega * avogadro * fluence / (four_pi*cos_beam*double(state))

	linmax = n_elements( e_lines[*,0])
	n_els = n_elements( e_lines[0,*])
	n_slices = n_elements(slices)
	n_lay = max(lid)+1
	qline = indgen(linmax) # replicate(1, n_els)
	qel = replicate(1, linmax) # indgen(n_els)

	yield = fltarr(linmax,n_els,n_layers)
	yieldx = fltarr(2*n_slices+1,n_els)
	syield = fltarr(linmax,n_els,n_layers)
	a2 = mass(z2)
	zero = intarr(n_els)

	if cos_detector lt 0.0 then begin						; transmission detector geometry
		exp_cmux = fltarr(linmax,n_els,2*n_slices+1)		; use cmux relative to back of tgt
		cmax = double( cmux[*,*,2*n_slices])
		for k=0L,2*n_slices do begin
			exp_cmux[*,*,k] = exp( -(cmax-double(cmux[*,*,k])) / (-cos_detector))
		endfor
	endif else begin										; normal detector geometry
		exp_cmux = exp( -double(cmux) / cos_detector)
	endelse

	sec_found = 0
	if sec_fl then begin									; find all lines/element combinations
		sig = bytarr(linmax,n_els,n_els,n_layers)			; that have significant sec. fluor.
		for l=0L,n_lay-1 do begin
			q = where(lid eq l)
			sig[*,*,*,l] = sec_significant( slices[q[0]], e_lines, z2, e_bind, sec_found=t)
			sec_found = sec_found OR t
		endfor
	endif

;	'xsect' is different for continuum case and ions/Mono Xrays.
;	Ions/mono Xrays:	xsect[ elements, slices]
;	Continuum:			xsect[ elements, spectrum]
;
;	For ions reflects falling cross-sections as beam slow down through slices.
;	For mono XRF, it remains unchanged with slice, and 'flux' used to show falling flux.
;	For continuum, must total spectrum[e]*xsect[el,e] to weight yield.
;
;	'fel'	includes branch_ratio for ions/mono Xrays,
;			does not include branch_ratio for continuum, since they change with evolving beam in slices.

	for i=0L,n_els-1 do begin
		n = n_lines[i]
		conc_1ppm = 1.0d-6/a2[i]
		fel = solid  * fluoro[i]
		if continuum then begin
			lastbx = total( beam[0].spectrum.data * xsect[i,*])
			if max(xsect[i,*]) lt 1.0e-38 then zero[i]=1
		endif else begin
			fel = fel * branch_ratio[0:n-1,i]
			if xsect[i,0] lt 1.0e-38 then zero[i]=1
		endelse
		last = exp_cmux[0:n-1,i,0]
		
		for j=0L,n_slices-1 do begin
			k = 2*j+1									; half slide ID
			l = lid[j]									; layer ID

			next = exp_cmux[0:n-1,i,k+1]
			q = where(next gt 1.0e-8, count)
			if count eq 0 then begin
;				print, 'skip slices for z=',z2[i],'  at half-step=',k, '  slice=',j,'  layer=',l
				goto, cont
			endif

			if zero[i] then begin						; need this to still get rel ints right
				xsect_av = 1.0e-30						; but not too big - then sec. fluoresces things!
				bx = 1.0e-30
			endif else begin	
				if photo then begin						; form 4th order Runge Kutta integration of 'xsect_av' 
					if continuum then begin
						bx = total( beam[k+1].spectrum.data * xsect[i,*])
						xsect_av = (  lastbx +   $
							4.0d0 * total( beam[k].spectrum.data * xsect[i,*]) * exp_cmux[0:n-1,i,k]/last +  $
							      bx * next/last ) / 6.0d0
					endif else begin
						xsect_av = (  xsect[i,k-1] * flux[k-1] +   $
							4.0d0 * xsect[i,k] * flux[k]   * exp_cmux[0:n-1,i,k]/last +  $
							      xsect[i,k+1] * flux[k+1] * next/last ) / 6.0d0
					endelse
				endif else begin
					xsect_av = (  xsect[i,k-1] +   $
							4.0d0 * xsect[i,k] * exp_cmux[0:n-1,i,k]/last +  $
							      xsect[i,k+1] * next/last ) / 6.0d0
				endelse
			endelse

			dyield = fel * xsect_av * slices[j].thick
			if continuum then dyield = dyield * branch_ratio[0:n-1,i,k]						; branch changes with slice, not in 'fel'
			yield[0:n-1,i,l] = yield[0:n-1,i,l] + conc_1ppm * dyield * last					; yield calculation
																							; these for plots ...
			if continuum then begin
				yieldx[k-1,i] = conc_1ppm * fel[0] * lastbx * exp_cmux[0,i,k-1]				; * slices[j].thick
				yieldx[k,i] = conc_1ppm * fel[0] * bx * exp_cmux[0,i,k]						; * slices[j].thick
			endif else begin
				yieldx[k-1,i] = conc_1ppm * fel[0] * xsect[i,k-1] * exp_cmux[0,i,k-1]		; * slices[j].thick
				yieldx[k,i] = conc_1ppm * fel[0] * xsect[i,k] * exp_cmux[0,i,k]				; * slices[j].thick
			endelse

			if sec_fl and sec_found then begin
				q = where( z2[i] eq slices[j].Z[0:slices[j].N-1])
				if q[0] ne -1 then begin
					conc = slices[j].F[q[0]] / total( mass(slices[j].Z[0:slices[j].N-1]) * slices[j].F[0:slices[j].N-1] )

					q = where( sig[*,i,*,l] eq 1)				; col is 'this line', row is 'that element'
					if q[0] ne -1 then begin					; only for significant sec. fluor.

						if continuum then begin
							secondary_fluorescence, qline[q], i, qel[q], dyield*conc, yield, syield, $
									z2, n_lines, e_lines, fluoro, branch_ratio[*,*,k], jump, $
									slices, j, k-1, cmux, exp_cmux, relmux, lid
						endif else begin
							secondary_fluorescence, qline[q], i, qel[q], dyield*conc, yield, syield, $
									z2, n_lines, e_lines, fluoro, branch_ratio, jump, $
									slices, j, k-1, cmux, exp_cmux, relmux, lid
						endelse
					endif
				endif
			endif

			if k ge n_hs-2 then goto, cont
			last = next > 1.0e-30
			if continuum then lastbx = bx
		endfor
cont:
	endfor

	yield[0,*,*] = yield[0,*,*] > 1.0e-20							; was 1.0e-10

	major_yield = reform( yield[ 0, *,*], n_els, n_layers)
	sec_yield = reform( syield[ 0, *,*], n_els, n_layers)

	intensity = fltarr( linmax, n_els, n_layers)

	for k=0L,linmax-1 do begin
		intensity[k,*,0:n_lay-1] = yield[k,*,0:n_lay-1] / major_yield[*,0:n_lay-1]
	endfor

	return, major_yield
	end
