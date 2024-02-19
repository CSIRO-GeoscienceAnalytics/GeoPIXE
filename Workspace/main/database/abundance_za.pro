	function abundance_za, Z, A
;
;	Return abundance (%) for isotope of atomic number 'Z' and mass number 'A'
;
	common c_abundance, isotope_el, isotope_m, amount, element_start, abundance_OK
;
	if( n_elements(abundance_OK) eq 0) then abundance_OK = 0
	if( abundance_OK ne 1) then init_abundance
	if( abundance_OK ne 1) then return, 0.0

	if( Z lt 1) then return, 0.0
	if( A lt 1) then return, 0.0
;
;	start will be very large or zero for non-existing isotopes/elements
;
	n = n_elements(isotope_el)
	start = element_start[z]
	if( start gt n-1) then return, 0.0
	if( (start lt 1) and (z gt 1)) then return, 0.0

	for i=start,n-1 do begin
	    if(isotope_m[i] eq A) then return, amount[i]
	endfor

	return, 0.0
	end