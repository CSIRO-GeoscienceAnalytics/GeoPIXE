function convol_self, spec, ca, cb, cross=cross, low=low

;	Convolute a spectrum by itself for pileup estimation.
;	Spec		spectrum vector
;	ca, cb		calibration coefficients
;
;	low			low channel limit to consider
;
; To cross convolute two spectra use:
;	cross		cross convolute with 'cross' spec
;				this assumes equal calibrations.

	if n_elements(cb) lt 1 then cb=0.
	if n_elements(ca) lt 1 then ca=1.0
	if n_elements(low) lt 1 then low=1
	if n_elements(spec) lt 1 then return,0

	siz = n_elements(spec)
	if siz lt 3 then return, 0
	temp1 = spec[0:*]

	if n_elements(cross) lt 1 then begin
		temp = float(spec[0:siz-2])
	endif else begin
		siz2 = min([siz,n_elements(cross)])
		temp = float(cross[0:siz2-2])
	endelse
	temp1[0:low] = 0.0
	temp1[siz-2:*] = 0.0			; zero ends
	temp[0:low] = 0.0
	temp[siz-2:*] = 0.0

	pileup = convol( float(temp1[0:siz-1]), temp, center=0, /edge_truncate)

	xb = round(cb/ca)
	np = n_elements(pileup)
	if (xb lt 0) and (-xb lt np-1) then begin
		pileup[0:np+xb-1] = pileup[-xb:np-1]
		pileup[np+xb:*] = 0
	endif else if (xb gt 0) and (xb lt np-1) then begin
		pileup[xb:np-1] = pileup[0:np-xb-1]
		pileup[0:xb-1] = 0.0
	endif

	return, pileup
end
