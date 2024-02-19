	function abundance, str
;
;	Return abundance (%) for isotope string 'str' (e.g. "Mg 25").
;	Must call init_abundance sometime first.
;
	common c_abundance, isotope_el, isotope_m, amount, element_start, abundance_OK
;
	if( n_elements(abundance_OK) eq 0) then abundance_OK = 0
	if( abundance_OK ne 1) then init_abundance
	if( abundance_OK ne 1) then return, 0.0

;	Strip leading and trailing blanks
;
	temp = strtrim(strcompress(str),2)
;
;	Split into element name and mass number
;
	part = str_sep(temp,' ')
	if( n_elements(part) lt 2) then return, 0.0
	z = atomic_number(part[0])
	mass = fix(part[1])
	if( z lt 1) then return, 0.0
	if( mass lt 1) then return, 0.0
;
;	start will be very large or zero for non-existing isotopes/elements
;
	n = n_elements(isotope_el)
	start = element_start[z]
	if( start gt n-1) then return, 0.0
	if( (start lt 1) and (z gt 1)) then return, 0.0

	for i=start,n-1 do begin
	    if(isotope_el[i] ne part[0]) then return, 0.0
	    if(isotope_m[i] eq mass) then return, amount[i]
	endfor

	return, 0.0
	end
