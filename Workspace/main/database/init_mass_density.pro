	pro init_mass_density
;
;	Read in mass, density info.
;
	common c_density, mass, density, density_OK
	common c_working_dir2, geopixe_database_path
;
	density_OK = 0
	on_ioerror, bad
	openr, lun, geopixe_database_path+'dat/mass_density.txt', /get_lun

	mass = fltarr(95)
	density = fltarr(95)

	n = 0
	readf,lun,n
	if (n lt 1) or (n gt 110) then begin
		print,'init_mass_density: bad N'
		goto, finish
	endif

	str = ' '
	for i=0L,n-1 do begin
	    readf,lun,str
	    str = strtrim(strcompress(str),2)
		z = atomic_number( str)

	    readf,lun,str
	    str = strtrim(strcompress(str),2)
		s = str_sep( str, ' ',/trim)
		if (z gt 0) and (z lt n_elements(mass)) then begin
			if fix(s[0]) eq z then begin
				mass[z] = float( s[1])
				density[z] = float( s[2])
			endif else begin
				print,'init_mass_density: inconsistent Z ['+s[0]+']'
			endelse
		endif
	endfor
	density_OK = 1
	goto, finish

bad:
	print,'init_mass_density: Error reading "Mass_density.txt".'
finish:
	close_file, lun
	return
	end
