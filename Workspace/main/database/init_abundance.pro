	pro init_abundance
;
;	Read in abundance info.
;
	common c_abundance, isotope_el, isotope_m, amount, element_start, abundance_OK
	common c_working_dir2, geopixe_database_path
;
	abundance_OK = 0
	on_ioerror, bad
	openr, lun, geopixe_database_path+'dat/isotope.txt', /get_lun
	n = 0
	readf, lun, n
	if( n lt 10) then goto, bad

	isotope_el = strarr(n)
	isotope_m = intarr(n)
	amount = fltarr(n)
	element_start = intarr(100)
	element_start[*] = 1000

	str = ' '
	for i=0L,n-1 do begin
	    readf,lun,str
		str = strtrim(strcompress(str),2)
	    part = str_sep(str,' ')
	    isotope_el[i] = part[0]
	    isotope_m[i] = fix(part[1])
	    amount[i] = float(part[2])

	    z = atomic_number(part[0])
	    if( z lt 1) then goto, bad
	    if( isotope_m[i] lt 1) then goto, bad
	    if( i lt element_start[z] ) then element_start[z] = i
	endfor
	abundance_OK = 1
	goto, finish

bad:
	warning,'init_abundance','Error reading "Isotope.txt" database.'
finish:
	close_file, lun
	return
	end
