	pro init_mass
;
;	Read in mass, valence info.
;
	common c_mass, mass2, valence, mass_OK
	common c_working_dir2, geopixe_database_path

	mass_OK = 0
	on_ioerror, bad
	openr, lun, geopixe_database_path+'dat/mass.txt', /get_lun

	mass2 = fltarr(95)
	valence = intarr(95)

	str = ' '
	for i=0L,86 do begin
	    readf,lun,str
	    str = strtrim(strcompress(str),2)
		s = str_sep( str, ' ',/trim)
		z = fix(s[0])
		if( z gt 0) then begin
			mass2[z] = float( s[2])
			valence[z] = fix( s[3])
		endif
	endfor
	mass_OK = 1
	goto, finish

bad:
	print,'Error reading "Mass.txt".'
finish:
	close_file, lun
	return
	end
