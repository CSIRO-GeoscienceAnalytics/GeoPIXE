	function relative_intensity, zi, ni, photo=photo
;
;	Return relative intensity of X-ray index 'n' for element 'z'
;
;	Index Line	   Index Line	  Index Line	 Index Line
;
;	  1   Ka1        2   Ka2        3   Ka3        4   Kb1
;	  5   Kb2        6   Kb3        7   Kb4        8   Kb5
;	  9   Ka_       10   Kb_       11   Kb11      12   Kb12
;	 13   Ll        14   Leta      15   La1       16   La2
;	 17   Lb1       18   Lb2       19   Lb3       20   Lb4
;	 21   Lb5       22   Lb6       23   Lg1       24   Lg2
;	 25   Lg3       26   Lg4       27   Lg5       28   Lg6
;	 29   La_       30   Lb_       31   Lg_	      32   Ma1
;	 33   Ma2       34   Mb_       35   Mg_       36   Mz_
;	 37   M-NO      38   K-LL      39   K-MM
;	 40   sum       41   esc       42   gamma     43   Compton
;	 44   F197      45   Na440     46   elastic   47   Compton2
;    48   M3N1      49   M2N1      50   M3O5      51   Lt
;    52   Lb9,10

;    M-NO = M2-N4 + M1-N3 + M1-N2 + M3-O4,5

	common c_xray, energy, relint, xray_OK
;
	if n_elements(photo) lt 1 then photo=0

	z = zi
	n = ni

	if photo then return, relative_intensity_xrf( zi,ni)

	nz = n_elements(z)
	nn = n_elements(n)
	nt = nz

	if (nz lt 1) or (nn lt 1) then return, 0.0
	if nz eq 1 then begin
		nt = nn
		z = replicate(Z,nt)
	endif else if nn eq 1 then begin
		nt = nz
		n = replicate(n,nt)
	endif else if nz ne nn then begin
		print,'relative_intensity: Z,N can only both be vectors if they have same length.'
		e = fltarr(nt)
		goto, done
	endif
	e = fltarr(nt)

	if( n_elements(xray_OK) eq 0) then xray_OK = 0
	if( xray_OK ne 1) then init_xray_lines
	if( xray_OK ne 1) then goto, done

	q = where( ((z ge 1) and (z lt n_elements(relint[*,0]))) and $
				((n ge 1) and (n lt n_elements(relint[0,*]))) )
	if (q[0] ne -1) then begin
		e[q] = relint[z[q],n[q]]
	endif

done:
	e = reform(e)
	if n_elements(e) eq 1 then e = e[0]
	return, e
	end
