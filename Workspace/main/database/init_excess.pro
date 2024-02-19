	pro init_excess
;
;	Read in mass excess info.
;
	common c_excess, ze, n_e, ae, ex, excess_OK
	common c_working_dir2, geopixe_database_path

	excess_OK = 0
	on_ioerror, bad
	openr, lun, geopixe_database_path+'dat/mass_excess.txt', /get_lun

	line = {z:0, n:0, a:0, e:0.0}
	temp = replicate( line, 5000)

	on_ioerror, more
	readf, lun, temp

more:
	q = where((temp.z gt 0) or (temp.n gt 0))
	if q[0] eq -1 then goto, bad
	n = n_elements(q)

	ze = temp[q].z
	n_e = temp[q].n
	ae = temp[q].a
	ex = temp[q].e

	excess_OK = 1
	goto, finish

bad:
	print,'init_excessa: Error reading "Mass_excess.txt".'
finish:
	close_file, lun
	return
	end
