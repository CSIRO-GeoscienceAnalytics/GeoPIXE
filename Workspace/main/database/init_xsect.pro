	pro init_xsect
;
;	Read in PIXE cross-section data
;
;	The cross-section tables are based on 1 AMU.
;	Need to divide by A1 before interpolating.
;
	common c_xsect_k1, z2_k, index_k, e_k, sig_k, xsect_K_OK
	common c_xsect_L1, z2_L, index_L, e_L, sig_L, xsect_L_OK
	common c_xsect_m1, z2_m, index_m, e_m, sig_m, xsect_m_OK
	common c_working_dir2, geopixe_database_path

	xsect_K_OK = 0
	on_ioerror, bad_K
	openr, lun, geopixe_database_path+'dat/xsect_K.txt', /get_lun

	nz = 0
	readf, lun, nz
	if nz lt 1 then goto, bad_nz

	str = ''
	readf, lun, str
	n = intarr(3)
	readf, lun, n
	nen = n[2]

	z2_K = intarr(nz)
	e_k = fltarr(nen)
	sig_k = fltarr(nen,nz)
	sig = fltarr(nen)

	readf, lun, e_k

	for j=0L,nz-1 do begin
		readf, lun, str
		readf, lun, n
		z2_k[j] = n[0]

	    readf, lun, sig
	    sig_k[*,j] = sig
	endfor

	index_k = intarr(100)
	index_k[*] = -1
	index_k[z2_K] = indgen(n_elements(z2_K))

	qe = sort(e_K)
	e_K = e_K[qe]
	sig_k = sig_k[qe,*]
	xsect_K_OK = 1

;---------------------------------------------------------------

	xsect_L_OK = 0
	on_ioerror, bad_L
	openr, lun, geopixe_database_path+'dat/xsect_L.txt', /get_lun

	nz = 0
	readf, lun, nz
	if nz lt 1 then goto, bad_nz

	str = ''
	readf, lun, str
	n = intarr(3)
	readf, lun, n
	nen = n[2]

	z2_L = intarr(nz)
	e_L = fltarr(nen)
	sig_L = fltarr(nen,nz)
	sig = fltarr(nen)

	readf, lun, e_L

	for j=0L,nz-1 do begin
		readf, lun, str
		readf, lun, n
		z2_L[j] = n[0]

	    readf, lun, sig
	    sig_L[*,j] = sig
	endfor

	index_L = intarr(100)
	index_L[*] = -1
	index_L[z2_L] = indgen(n_elements(z2_L))

	qe = sort(e_L)
	e_L = e_L[qe]
	sig_L = sig_L[qe,*]
	xsect_L_OK = 1

;---------------------------------------------------------------

	xsect_m_OK = 0
	on_ioerror, bad_m
	openr, lun, geopixe_database_path+'dat/xsect_m.txt', /get_lun

	nz = 0
	readf, lun, nz
	if nz lt 1 then goto, bad_nz

	str = ''
	readf, lun, str
	n = intarr(3)
	readf, lun, n
	nen = n[2]

	z2_m = intarr(nz)
	e_m = fltarr(nen)
	sig_m = fltarr(nen,nz)
	sig = fltarr(nen)

	readf, lun, e_m

	for j=0L,nz-1 do begin
		readf, lun, str
		readf, lun, n
		z2_m[j] = n[0]

	    readf, lun, sig
	    sig_m[*,j] = sig
	endfor

	index_m = intarr(100)
	index_m[*] = -1
	index_m[z2_m] = indgen(n_elements(z2_m))

	qe = sort(e_M)
	e_M = e_M[qe]
	sig_M = sig_M[qe,*]
	xsect_M_OK = 1

;---------------------------------------------------------------

	goto, finish

bad_K:
	print,'init_xsect: Error reading "xsect_k.txt".'
bad_L:
	print,'init_xsect: Error reading "xsect_L.txt".'
bad_M:
	print,'init_xsect: Error reading "xsect_m.txt".'
bad_nz:
	print,'init_xsect: Bad NZ.'
finish:
	close_file, lun
	return
	end