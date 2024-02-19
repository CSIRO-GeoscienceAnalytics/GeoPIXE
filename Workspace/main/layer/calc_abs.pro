function calc_abs, e_lines, z, slices, lid, relmux=relmux, sec_fl=sec_fl, $
					mu_zero=mu_zero, gamma=gamma, $
					photo=photo, e_beam=e_beam, flux=flux

;	Calculate cummulative mu-x (cmux) for all X-ray lines cummulated from surface.
;
;	This version can use slices of different thickness.
;	'cmux' returns in half-slice steps, starting at surface.
;
;	'mu_zero' returns the mu's for all lines for top slice=0.
;	'flux' return cummulative abs coeff for e_beam for photon case.

COMPILE_OPT STRICTARR
if n_elements(sec_fl) lt 1 then sec_fl=0
if n_elements(gamma) lt 1 then gamma=0
if n_elements(photo) lt 1 then photo=0

n_lines = n_elements(e_lines[*,0])
n_els = n_elements(e_lines[0,*])
n_slices = n_elements(slices)
n_layers = max(lid)+1 > 1

cmux = fltarr(n_lines*n_els, 2*n_slices+1)
if photo then flux = fltarr(2*n_slices+1)
do_relmux = 0
if arg_present(relmux) and sec_fl and (gamma eq 0) then begin
	do_relmux = 1
	relmux = fltarr(n_lines*n_els, n_els, n_layers)

	for i=0L,n_els-1 do begin
		t = 1.0E-9 * absco(z[i],e_lines)
		for l=0L,n_layers-1 do relmux[*,i,l] = t
	endfor
endif
do_mu_zero = 0
if arg_present(mu_zero) then begin
	do_mu_zero = 1
endif

llast = -1
cmux[*,0] = 0.0
if photo then flux[0] = 0.0
k = 1										; cmux zero at surface
for i=0L,n_slices-1 do begin
	if lid[i] ne llast then begin			; start of layer
		mu = atten( slices[i], e_lines, gamma=gamma)
		if do_mu_zero and (i eq 0) then begin
			mu_zero = mu
		endif

		if do_relmux then begin
			for j=0L,n_els-1 do relmux[*,j,lid[i]] = relmux[*,j,lid[i]] / mu
		endif
		if photo then mub = atten( slices[i], e_beam)
	endif
	mux = 0.5 * mu * slices[i].thick 		; half slice step

	if photo then begin
		mup = 0.5 * mub * slices[i].thick
		flux[k] = flux[k-1] + mup			; cummulate flux abs coeff.
		flux[k+1] = flux[k] + mup
	endif

	cmux[*,k] = cmux[*,k-1] + mux			; first half of slice
	k = k+1

	cmux[*,k] = cmux[*,k-1] + mux			; second half
	k = k+1
	llast = lid[i]
endfor

cmux = reform( cmux, n_lines, n_els, 2*n_slices+1, /overwrite)
if do_relmux then relmux = reform( relmux, n_lines, n_els, n_els, n_layers, /overwrite)
if do_mu_zero then mu_zero = reform( mu_zero, n_lines, n_els, /overwrite)

return, cmux
end

