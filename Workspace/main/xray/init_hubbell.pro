pro init_hubbell

	common c_hubbell_1, hubbell_ok, hubbell
	common c_working_dir2, geopixe_database_path

	if n_elements(hubbell_ok) lt 1 then hubbell_ok=0

	hubbell_ok = 0
	on_ioerror, bad_file
	openr, lun, geopixe_database_path+'dat/hubbell.dat',/get_lun, /xdr

	ntot = 0
	nmax = 0
	readu,lun, ntot,nmax

	data = { el:'', Z:0, n:0, e:fltarr(nmax), units:'', Rayleigh:fltarr(nmax), $
			Compton:fltarr(nmax), Photo:fltarr(nmax), Pair:fltarr(nmax), $
			Atten_All:fltarr(nmax), Atten:fltarr(nmax) }
	hubbell = replicate( data, ntot)

	on_ioerror, bad_io
	readu,lun, hubbell
	hubbell_ok = 1

finish:
	close_file, lun
	return

bad_file:
	warning,'init_hubbell','bad file open'
	goto, finish
bad_io:
	warning,'init_hubbell','bad file I/O'
	goto, finish
end
