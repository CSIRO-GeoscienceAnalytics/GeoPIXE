pro init_hubbell_raw

common c_hubbell_2, hubbell_raw_ok, hubbell_data
common c_working_dir2, geopixe_database_path

	if n_elements(hubbell_raw_ok) lt 1 then hubbell_raw_ok=0

	hubbell_raw_ok = 0
	on_ioerror, bad_file
	openr, lun, geopixe_database_path+'dat/hubbell_data.dat',/get_lun, /xdr

	ntot = 0
	nmax = 0
	nsmax = 0
	readu,lun, ntot,nmax,nsmax

	data = { el:'', Z:0, n:0, e:fltarr(nmax), units:'', Rayleigh:fltarr(nmax), $
			Compton:fltarr(nmax), Photo:fltarr(nmax), Pair:fltarr(nmax), $
			Atten_All:fltarr(nmax), Atten:fltarr(nmax), ns:0, edge_e:fltarr(nsmax), $
			edge_shell:intarr(nsmax), edge_subshell:intarr(nsmax), edge_label:strarr(nsmax), edge_ratio:fltarr(nsmax) }
	hubbell_data = replicate( data, ntot)

	on_ioerror, bad_io
	readu,lun, hubbell_data
	hubbell_raw_ok = 1

finish:
	close_file, lun
	return

bad_file:
	warning,'init_hubbell_raw','bad file open'
	goto, finish
bad_io:
	warning,'init_hubbell_raw','bad file I/O'
	goto, finish
end
