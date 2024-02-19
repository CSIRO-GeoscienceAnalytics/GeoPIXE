	pro init_xsect_pige
;
;	Read in PIGE cross-section data for
;	19F(p,p'g)
;	23Na(p,p'g)
;
;	Convert all cross-section tables on read to MeV, cm^2.

; 19F:
;	F19 p,p' 197 keV gamma (file in keV, mB)
	common c_xsect_F19, e_F19, sig_F19, xsect_F19_OK, xsect_pige_done
	common c_xsect_Na23, e_Na23, sig_Na23, xsect_Na23_OK
	common c_working_dir2, geopixe_database_path

	xsect_pige_done = 0

	xsect_F19_OK = 0
	on_ioerror, bad_F19
	openr, lun, geopixe_database_path+'dat/xsect_F19_197.txt', /get_lun

	x = {energy:0.0, cross_section:0.0}
	t = REPLICATE(x, 8192)

	on_ioerror, cont19
	readf, lun, t

cont19:
	q = where(t.energy gt 0.1)
	if q[0] eq -1 then goto, bad_F19

	e_F19 = 0.001 * t[q].energy							; convert to MeV
	sig_F19 = 1.0E-27 * t[q].cross_section				; convert to cm^2
	xsect_F19_OK = 1

	goto, more_23

bad_F19:
	print,'init_xsect_pige: Error reading "xsect_F19_197.txt".'
	goto, more_23

more_23:
	close_file, lun

; 23Na:
;	Na23 p,p' 440 keV gamma (file in MeV, mB)

	xsect_Na23_OK = 0
	on_ioerror, bad_Na23
	openr, lun, geopixe_database_path+'dat/xsect_Na23_440.txt', /get_lun

	x = {energy:0.0, cross_section:0.0}
	t = REPLICATE(x, 8192)

	on_ioerror, cont23
	readf, lun, t

cont23:
	q = where(t.energy gt 0.1)
	if q[0] eq -1 then goto, bad_Na23

	e_Na23 = t[q].energy								; convert to MeV
	sig_Na23 = 1.0E-27 * t[q].cross_section				; convert to cm^2
	xsect_Na23_OK = 1

	goto, finish

bad_Na23:
	print,'init_xsect_pige: Error reading "xsect_Na23_440.txt".'
	goto, finish

finish:
	close_file, lun
	xsect_pige_done = 1
	return
	end
