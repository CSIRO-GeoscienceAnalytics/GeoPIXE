	pro init_proton
;
;	Read in Ziegler and Anderson proton dedx coefficients.
;
	common c_dedx, proton, alpha, proton_ok, alpha_ok
	common c_working_dir2, geopixe_database_path

	proton_OK = 0
	on_ioerror, bad
	openr, lun, geopixe_database_path+'dat/proton.txt', /get_lun

	proton = fltarr(14,93)			; z from 0 to 92
	p = fltarr(14,92)

	readf, lun, p
	proton[*,1:*] = p

	proton_OK = 1
	goto, finish

bad:
	print,'init_proton: Error reading "proton.txt".'
finish:
	close_file, lun
	return
	end
;
;-------------------------------------------------------------------
;
	pro init_alpha
;
;	Read in Ziegler and Anderson alpha dedx coefficients.
;
	common c_dedx, proton, alpha, proton_ok, alpha_ok
	common c_working_dir2, geopixe_database_path

	alpha_OK = 0
	on_ioerror, bad
	openr, lun, geopixe_database_path+'dat/alpha.txt', /get_lun

	alpha = fltarr(7,93)			; z from 0 to 92
	p = fltarr(7,92)

	readf, lun, p
	alpha[*,1:*] = p

	alpha_OK = 1
	goto, finish

bad:
	print,'init_alpha: Error reading "alpha.txt".'
finish:
	close_file, lun
	return
	end
;
;-------------------------------------------------------------------
;
pro init_dedx

common c_dedx, proton, alpha, proton_ok, alpha_ok
;
if n_elements(proton_ok) lt 1 then proton_ok = 0
if n_elements(alpha_ok) lt 1 then alpha_ok = 0

if proton_ok ne 1 then init_proton
if alpha_ok ne 1 then init_alpha

return
end