	function valence, str
;
;	Return valence of element string 'str
;
	common c_mass, mass2, valence, mass_OK
;
	n = n_elements(str)
	if n gt 1 then begin
		m = intarr(n)
	endif else begin
		m = 0
	endelse
	if( n_elements(mass_OK) eq 0) then mass_OK = 0
	if( mass_OK ne 1) then init_mass
	if( mass_OK ne 1) then return, m

	for i=0L,n-1 do begin
		z = atomic_number( str[i])
		if( z gt 0) then m = valence[z]
	endfor
	return, m
	end
