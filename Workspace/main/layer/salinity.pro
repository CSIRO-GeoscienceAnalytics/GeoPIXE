function salinity, p, error=error

;	Estimate the equivalent salinity using the formulas of
;	Heinrich, Ryan, Mernagh and Eadington, Econ. Geol. 87 (1992) 1566.

	n_cations = 18
	cation = ['K K','Ca K','Cs L','Ba L','Ti K','Mn K','Fe K','Co K','Ni K', $
     			'Cu K','Zn K','Rb K','Sr K','Mo K','Ag K','Te K','Pb L','Bi L']
	Na = 'Na K'
	Cl = 'Cl K'
	coord = [1,2,1,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2]

	error = 1
	if ptr_valid(p) eq 0 then goto, bad_ptr
	if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr

	q = where( (*p).el.name eq Cl)				; make sure Cl is in the list
	j = q[0]
	if j eq -1  then goto, bad_901
	MCl = mass(17)

	CCl = (*p).conc[j]
	if CCl lt 100. then goto, bad_901

	MNaCl = mass(11) + MCl

	sum = 0.0
	for m=0L,n_cations-1 do begin
		q = where( (*p).el.name eq cation[m])
		j = q[0]
		if j ge 0 then begin

		    Cm = (*p).conc[j] > 0.0
		    Mm = mass((*p).el.z[j])							; mass of metal 'm'
		    MmCl = Mm + float(coord[m])*MCl					; mass of metal chloride

		    r = (0.5*MmCl - float(coord[m])*MNaCl ) / Mm

		    sum = sum + r*Cm
		endif
	endfor

	sal = 0.0001 * ( (MNaCl * CCl / MCl + sum) > 0.0)		; wt %
	error = 0
	return, sal

bad_ptr:
	warning,'SALINITY','Bad results pointer',/error
	error = 1
	return, 0.0
bad_901:
	warning,'SALINITY','No Cl in region '+(*p).spectrum.label
	error = 1
	return, 0.0
end
