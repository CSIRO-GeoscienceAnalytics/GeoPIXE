	pro chop_element, str, z, x, v=v, k=k, l=l, m=m

; Chop the 'str' into element name, and hence 'z', weighting 'x'
; and detect any qualifying valence number 'v', or K, L, M inidcators,
; between "\".

COMPILE_OPT STRICTARR
	k = 0
	l = 0
	m = 0
	z = 0
	x = 0.0
	v = 0

	ls = lenchr(str)
	if ls lt 1 then return
	z = atomic_number( extract(str,0,1))
	j = 0
	if z gt 0 then begin
		j = 2
	endif else begin
		z = atomic_number( extract(str,0,0))
		if z gt 0 then begin
			j = 1
		endif else begin
			z = 0
			if gnumeric(str) then begin
				j = 0
			endif
		endelse
	endelse

;	if z gt 56 then l=1
;	if z le 56 then k=1

	if extract(str,j,j) eq '\' then begin
		j = j+1
		s = extract(str,j,j)
		b = byte(s)
		b = b[0]
		if s eq 'K' then begin
			k = 1
		endif else if s eq 'L' then begin
			l = 1
		endif else if s eq 'M' then begin
			m = 1
		endif else if ((b ge 48) and (b le 57)) then begin
			v = fix(s)
		endif
		j = j+2
	endif

	if ls gt j then begin
		x = 0.0
		s = extract(str,j,ls-1)
		if gnumeric( s) then x = float( s)
	endif else begin
		x = 1.0
	endelse

	return
	end
