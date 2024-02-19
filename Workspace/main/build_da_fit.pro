function build_da_fit, p, pmatrix, pregion, charge, multiplicity, back=bterm, show=show, error=err, $
			suppress=suppress, bscale=bscale

;	Estimate the total fit to extracted spectra 'p'
;	using the DA matrix 'pmatrix' and the concentration and charge from 'pregion'.
;
;	'p' is a pointer to a spectrum struct as in 'spectrum_display' or 'read_spec'.
;
;	'pmatrix' is a pointer to a matrix struct
;
;	'pregion' points to the region results
;
;	'bscale'	scales the background estimation (only for MPDA n_comp>1)
;	
;	/suppress	to suppress printing messages
;	
;	Use the passed 'charge' rather than the region's fractional charge
;	as in the spectrum header because the spectra may have been projected
;	without a valid charge.
;
;	return:
;		b		background overlay
;		t		"fit"
;		show	flags showing back

COMPILE_OPT STRICTARR
show_back = 1								; show background for PIXE/SXRF

err = 1
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'build_da_fit',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       return, 0
    endif
endif

pel = (*pregion).el
pconc = (*pregion).conc
pphase = (*pregion).phase
payield = (*pregion).ayield
if ptr_valid(p) eq 0 then return,0
if ptr_valid(pmatrix) eq 0 then return,0
if ptr_valid(pconc) eq 0 then return,0
if ptr_valid(pel) eq 0 then return,0
if charge lt 1.0e-10 then return,0
if n_elements(multiplicity) lt 1 then multiplicity=1
if multiplicity lt 1 then multiplicity=1
if n_elements(bscale) lt 1 then bscale=1.0
if n_elements(suppress) lt 1 then suppress=0

; First search any series matrix for closest energy match ...

matrix = *pmatrix
if ptr_good( matrix.pmore) then begin
	nda_extra = n_elements( *matrix.pmore) 
	pdm = ptrarr(nda_extra)
	e = fltarr(nda_extra)
	for i=0,nda_extra-1 do begin
		pdm[i] = (*matrix.pmore)[i]
		e[i] = (*pdm[i]).e_beam
	endfor
	q = sort( abs( e - (*p).energy))
	if abs(e[q[0]]-(*p).energy) gt 0.002 then begin
		warning,'build_da_fit',['Chosen stack DA matrix for XANES using poor energy match.', $
				'E requested ='+str_tidy(eb),'E match ='+str_tidy(e[q[0]])]
	endif
	matrix = *pdm[q[0]]
endif

; N.B. Offset seems to have been lost below (re-mapped in append_DA_fit?) ...
; Generally the difference between cal_b's is small these days, but not zero.

offset = fix((matrix.cal.b - (*p).cal.poly[0]) / (*p).cal.poly[1])
cal_ratio = matrix.cal.a / (*p).cal.poly[1]

;print,'append: matrix cal b,a =',matrix.cal.b, matrix.cal.a
;print,'append: spectr cal b,a =',(*p).cal.poly[0],(*p).cal.poly[1]
;print,'append: cal_ratio = ',cal_ratio
;print,'*pel=',*pel
;print,'matrix.el=',matrix.el
;print,'matrix.file=',matrix.file,'  extension=',strupcase(extract_extension( matrix.file))

pige_mode = 0
if strlowcase(extract_extension( matrix.file)) eq 'damg' then pige_mode=1

show = show_back or (*p).show_back or pige_mode

matrix.n_pure = min([matrix.n_pure,matrix.n_el])
tspec = fltarr( n_elements(matrix.pure[*,0]))
bterm = tspec
bterm[*] = 0.

rG = multiplicity_scale( (*p).pactive, matrix, multiplicity=multiplicity)		;@3-16

;----------------------------------------------------------------------------------------------
; Test for presence of new *poverlay spectra in *pregion structs ...

if ptr_good( (*pregion).poverlay) eq 0 then goto, old							;@3-16

;----------------------------------------------------------------------------------------------
;	New MPDA using poverlay's from Region (analyze_image)						;@3-16

	if suppress eq 0 then print,'Use new "poverlay" spectrum overlay approach ...'
	npure = n_elements( (*(*pregion).poverlay)[0,*]) < n_elements(rG)
;	if suppress eq 0 then print, '	cal_ratio = ',cal_ratio
	
	for j=0L,npure-1 do begin
		back_el = strlowcase( strmid( (*(*pregion).el)[j],0,4)) eq 'back'
		if show or (back_el eq 0) then begin
			bs = back_el ? bscale : 1.0
			scale = bs * rG[j]

			term = ((*(*pregion).poverlay)[*,j] / cal_ratio) * scale
			if back_el then bterm = bterm + term
			if finite(scale) then begin
				if back_el eq 0 then tspec = tspec + term
				if suppress eq 0 then print,'append add: el=',(*(*pregion).el)[j],' rG/n_det =',rG[j]/(multiplicity > 1),' multiplicity =',(multiplicity > 1),' scale =',scale
			endif else begin
				if suppress eq 0 then print,'append add: el=',(*(*pregion).el)[j],' not added; scale infinite'
			endelse			
		endif
	endfor
	goto, done

;----------------------------------------------------------------------------------------------
;	If no poverlay exists from Region (analyze_image)

old:
	n_comp = n_elements( matrix.matrix[0,0,*])
	if ptr_good( pphase) eq 0 then n_comp = 0 else if n_elements(*pphase) lt n_comp then n_comp=0
	if n_comp gt 0 then begin
		sum = 0.
		for i=0,n_comp-1 do begin
			sum = sum + (*pphase)[i]
		endfor
		if (n_comp le 1) or (sum lt 1.0e-10) then n_comp=0
	endif
	if n_comp gt 1 then goto, old_mpda
	
;----------------------------------------------------------------------------------------------
;	Single phase, normal overlays from Region (analyze_image)

	if suppress eq 0 then print,'Use single phase spectrum overlay approach ...'
	
	for j=0L,matrix.n_pure-1 do begin
		back_el = strlowcase( strmid(matrix.el[j],0,4)) eq 'back'
		if show or (back_el eq 0) then begin
			k = contains( (*pel), matrix.el[j])
			if (k ge 0) then begin
				bs = back_el ? bscale : 1.0
				s = matrix.pure[*,j]
				scale = (matrix.yield[j] > 1.0e-10) * (*pconc)[k] * charge * rG[j] * bs
				if back_el then begin
					bterm = bterm + (s / cal_ratio) * scale
				endif else begin
					term = (s / cal_ratio) * scale
				endelse

				if finite(scale) then begin
					if back_el eq 0 then tspec = tspec + term
					if suppress eq 0 then print,'append add: el=',matrix.el[j],' rG/n_det =',rG[j]/(multiplicity > 1),' multiplicity =',(multiplicity > 1),' scale =',scale
				endif else begin
					if suppress eq 0 then print,'append add: el=',matrix.el[j],' not added; scale infinite'
				endelse
			endif
		endif
	endfor
	goto, done

;----------------------------------------------------------------------------------------------
;	Multi-phase, old MPDA overlays from Region (analyze_image)

; The type of yield average to use depends on whether it's a compound mixture, or
; a spatially dispersed mixture. For a compound, we use the 1/Y average approach, reflecting
; the dominance of the more absorbing elements in a compound. For a spatially dispersed
; mixture, a simple average is appropriate.

old_mpda:
	if suppress eq 0 then print,'Use old MPDA spectrum overlay approach ...'

	cmixture = 0			; use 1=compound mixture, 0=spatially distributed mixture.

; For a multi-phase sample, this is now averaged both within and across pixels in
; 'analyze_image' in 'image_routines', and passed in results 'pregion' as 'ayield'.
; 
; The 1.0e-10 limit on "counts_per_ppm_uc" here is the same as the one used to make the 
; matrix rows finite in "calc_da_matrix2".

	for j=0L,matrix.n_pure-1 do begin
		back_el = strlowcase( strmid(matrix.el[j],0,4)) eq 'back'
		if show or (back_el eq 0) then begin
			k = contains( (*pel), matrix.el[j])
			if (k ge 0) then begin
				ry = 0.
				s = matrix.pure[*,j,0]
				s[*] = 0.
				q = where( matrix.yield[j,*] gt 1.0e-6, nq)
				if nq eq 0 then continue
				sum = 0.
				for i=0,nq-1 do begin
					if cmixture then begin
						ry = ry + (1./matrix.yield[j,q[i]]) * (*pphase)[q[i]]
					endif else begin
						ry = ry + matrix.yield[j,q[i]] * (*pphase)[q[i]]
					endelse
					mx = matrix.pure[*,j,q[i]] * (*pphase)[q[i]]
					if back_el then mx = mx / matrix.charge[q[i]]
					s = s + mx
					sum = sum + (*pphase)[q[i]]
				endfor
				
				if ptr_good( payield) then begin
					y = (*payield)[k]
					s = s / sum
				endif else begin
					print, matrix.el[j], '  phase yields=', reform(matrix.yield[j,*])
					if cmixture then begin
						y = sum / ry
					endif else begin
						y = ry / sum
					endelse
					s = s / sum
				endelse

				pconc_rG = back_el ? bscale : (*pconc)[k] * rG[j]
				scale = y * pconc_rG * charge

				if back_el then begin
					if suppress eq 0 then begin
						print,'Matrix charge = ', matrix.charge
						print,'Back: conc=',(*pconc)[k],', y=',y,', rG=',rG[j],', charge=',charge
					endif
					bterm = (s / cal_ratio) * scale
				endif else begin
					term = (s / cal_ratio) * scale
				endelse
	
				if finite(scale) then begin
					if back_el eq 0 then tspec = tspec + term
					if suppress eq 0 then print,'append add: el=',matrix.el[j],' rG/n_det =',rG[j]/(multiplicity > 1),' multiplicity =',(multiplicity > 1),' scale =',scale
				endif else begin
					if suppress eq 0 then print,'append add: el=',matrix.el[j],' not added; scale infinite'
				endelse
			endif
		endif
	endfor

done:
	tspec = (tspec > 0.) + bterm				; suppress negatives in the spec term

	err = 0
	return, tspec
end

