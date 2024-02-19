function read_old_da, file, error=error
;
;	Read the DA matrix from unit 1 (already opened by read_da).
;	This reads the old ascii (VAX compatible) version.
;
;	Return 'p', a pointer, pointing to
;	DA matrix struct, containing the DA details
;	and data.
;

error = 0

on_ioerror, bad_file
openr,lun, file, /get_lun

on_ioerror, bad_io
version = 0
readf, lun, version

OK_version = [-1,-2]
small = 1.0e-10
error = 0

q = where(version eq OK_version)
if q[0] eq -1 then goto, bad_version

label = ''
readf, lun,label

n = 0L
readf, lun, n
if n lt 1 then goto, error

x = fltarr(5)
readf, lun,x
cal_orig = { a:x[0], b:x[1] }
cal = { a:x[2], b:x[3] }
charge = x[4]

el = strarr(n)
readf, lun,el

mdl = fltarr(n)
readf, lun,mdl

counts_per_ppm_uC = fltarr(n)
if version le -2 then begin
	readf, lun,counts_per_ppm_uC
	if counts_per_ppm_uC[0] lt small then counts_per_ppm_uC[0] = 1.0
endif

size = 0L
readf, lun,size

matrix = fltarr(size,n)
readf, lun,matrix

station = 0L

n_pure = 0L
pure = 0
if version le -2 then begin
	readf, lun, n_pure
	if n_pure gt 0 then begin
		pure = fltarr(size,n_pure)
		readf, lun,pure
	endif
endif

ecompress = 1L

on = 0
n_det = 0
rGamma = 0.0
array = {on:on, n_det:n_det, rGamma:rGamma }

; N.B This has to match the form in 'calc_da_matrix2', 'read_da' and 'read_old_da'

da = { label:	label, $				; label, name of source data
		file:	file, $					; local file name
		cal_orig: cal_orig, $			; original spectrum cal
		cal:	cal, $					; cal of DA matrix rows
		station: 	station, $			; detector stattion number
		charge:	charge, $				; charge of DA matrix. for MDLs
		n_el:	n, $					; number of element rows
		el:		el, $					; element names
		mu_zero: 0.0, $					; major-line mass absorption coeffs
		density0: 0.0, $				; density of top layer
		ecompress: ecompress, $			; spectrum compression factor
		mdl:	mdl, $					; mdls at this charge
		size:	size, $					; size of a row
		matrix:	matrix, $				; matrix
		yield:	counts_per_ppm_uC, $	; yield factors
		thick:		0.0, $				; mg/cm2 for unknown layer
		array:		array, $			; array, yield-ratios, ...
		n_pure:	n_pure, $				; # of pure element unit spectra
		pure:	pure, $					; pure spectra
		E_beam:		0.0, $				; beam energy (keV Xray, meV ionbeam)
		pmore:		ptr_new() }			; pointer to more DA matrices

finish:
	close_file,lun
	return, da

bad_file:
	print,'read_da: error opening file '+file
	goto, error
bad_io:
	print,'read_da: I/O error'
	goto, error
bad_version:
	print,'read_da: bad version number'
	goto, error

error:
	da = 0
	error = 1
	goto, finish
end
