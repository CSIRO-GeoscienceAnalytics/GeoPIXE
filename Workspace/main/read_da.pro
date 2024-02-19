;
;	Read the DA matrix file 'file'
;
;	Return 'p', a pointer, pointing to
;	DA matrix struct, containing the DA details
;	and data.

pro read_da_matrix, lun, da, version, error=error

;	Read a single DA matrix from file (called from "read_da" below).

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'read_da_matrix',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		return
	endif
endif
error = 1
small = 1.0e-10

on_ioerror, bad_io
label = ''
readu,lun,label

n = 0L
readu,lun, n
if n lt 1 then goto, bad_io

cal_orig = { a:0.0, b:0.0 }
cal = { a:0.0, b:0.0 }
charge = 0.0
readu,lun, cal_orig, cal, charge

el = strarr(n)
readu,lun, el

ecompress = 1L
if version le -3 then readu,lun, ecompress

mdl = fltarr(n)
readu,lun, mdl

counts_per_ppm_uC = fltarr(n)
if version le -2 then begin
	readu,lun, counts_per_ppm_uC
	if counts_per_ppm_uC[0] lt small then counts_per_ppm_uC[0] = 1.0
endif
q = where(finite(counts_per_ppm_uC) eq 0)
if q[0] ne -1 then counts_per_ppm_uC[q]=0.0

size = 0L
readu,lun, size

matrix = fltarr(size,n)
readu,lun, matrix
q = where(finite(matrix) eq 0)
if q[0] ne -1 then matrix[q]=0.0

station = 0L
if version le -6 then readu,lun, station

density0 = 0.0
thick = 0.0
if version le -5 then readu,lun, density0
if version le -8 then readu,lun, thick

use_mu_zero = 0L
mu_zero = 0.0
if version le -4 then begin
	readu,lun, use_mu_zero
	if use_mu_zero then begin
		mu_zero = fltarr(n)
		readu,lun, mu_zero
	endif
endif

n_pure = 0
pure = 0
if version le -2 then begin
	readu,lun, n_pure
	if n_pure gt 0 then begin
		pure = fltarr(size,n_pure)
		readu,lun, pure
	endif
endif

on = 0
n_det = 0
rGamma = 0.0
if version le -7 then begin
	readu,lun, on
	if on gt 0 then begin
		readu,lun, n_det
		if n_det gt 0 then begin
			rGamma = fltarr(n_det,n)
			readu,lun, rGamma
		endif
	endif
endif
array = {on:on, n_det:n_det, rGamma:rGamma }

e_beam = 0.0
if version le -9 then begin
	readu,lun, e_beam
endif

; N.B This has to match the form in 'calc_da_matrix2', 'read_da' and 'read_old_da'

da = { label:		label, $			; label, name of source data
		file:		'', $				; local file name
		cal_orig: 	cal_orig, $			; original spectrum cal
		cal:		cal, $				; cal of DA matrix rows
		station: 	station, $			; detector stattion number
		charge:		charge, $			; charge of DA matrix. for MDLs
		n_el:		n, $				; number of element rows
		el:			el, $				; element names
		mu_zero: 	mu_zero, $			; major-line mass absorption coeffs
		density0: 	density0, $			; density of top layer
		ecompress: 	ecompress, $		; spectrum compression factor
		mdl:		mdl, $				; mdls at this charge
		size:		size, $				; size of a row
		matrix:		matrix, $			; matrix
		yield:		counts_per_ppm_uC, $	; yield factors
		thick:		thick, $			; mg/cm2 for unknown layer
		array:		array, $			; array, yield-ratios, ...
		n_pure:		n_pure, $			; # of pure element unit spectra
		pure:		pure, $ 			; pure spectra
		E_beam:		e_beam, $			; beam energy (keV Xray, meV ionbeam)
		pmore:		ptr_new() }			; pointer to more DA matrices

error = 0
return

bad_io:
	da = 0
	error = 1
	return
end

;----------------------------------------------------------------------------

function read_da, file, error=error, title=title, e_beam=eb, eDA=e, phases=phase_dai, pcorr=pcorr, mpda=mpda

;	Read the DA matrix file 'file'
;
;	Return 'p', a pointer, pointing to
;	DA matrix struct, containing the DA details and data.
;	
;	file	can be either a DA matrix file or a mpdam file.
;			For mpdam it can also be a stringify string "{}" containing the full mpdam struct as a string
;			
;	eb		Select just this member of a DA matrix series
;			if eb=0 then return just first matrix.
;			
;			If 'eb' absent, then return all DAM in 'da.pmore',
;	eDA		also returns E vector here.		
;
;	For file extension .mpdam only:
;	phases	returns file-name of phase maps.
;	pcorr	returns the correct setup struct.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'Read_DA',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad_io
	endif
endif

error = 1
da = 0L
phase_dai = '
mpda = 0
filename = file

;	Test whether 'file' is either the mpdam file-name, or a stringify string containing
;	the full mpdam struct (including pointers).

if strmid( file,0,1) eq '{' then begin
	mpdam = unstringify( file, error=error, context='read_DA')
	if error then goto, bad_io
	filename = mpdam.file
	mpda = 1
endif else if extract_extension( file) eq 'mpdam' then begin
	mpdam = read_mpdam( file, error=error)
	if error then goto, bad_io
	filename = file
	mpda = 1
endif

; Multi-phase DA read ...

if mpda then begin
	phase_dai = mpdam.phases 
	pcorr = mpdam.pcorr
	
	n_comp = (*mpdam.pcorr).n_comp + 1
	for i=0,n_comp-2 do begin
		da = read_da( (*mpdam.pcorr).files[i], title='read_da: multi-phase DA read of component', error=error)
		if error then goto, bad_mpdam
		if i eq 0 then begin
			da1 = da
			mp_matrix = fltarr( da.size, da.n_el, n_comp)
			mp_yield = fltarr( da.n_el, n_comp)
			mp_charge = fltarr( n_comp)
			if da.n_pure gt 0 then pure = fltarr( da.size, da.n_el, n_comp)
			sz1 = da.size
			n1 = da.n_el
			els1 = da.el
			cal1 = {order:1, poly:[da.cal.b,da.cal.a], units:'keV'}
			mp_matrix[*,*,i] = da.matrix										; first component matrix
			if da.n_pure gt 0 then pure[*,*,i] = da.pure
			mp_yield[*,i] = da.yield
			mp_charge[i] = da.charge
			print,'	First:('+(*mpdam.pcorr).minerals[i]+') ca,cb=',da.cal.a,da.cal.b,' size=',sz1
		endif else begin
			sz2 = da.size
			cal2 = {order:1, poly:[da.cal.b,da.cal.a], units:'keV'}
			print,'	Next:('+(*mpdam.pcorr).minerals[i]+') ca,cb=',da.cal.a,da.cal.b,' size=',sz2
			for j=0,da.n_el-1 do begin
				q = where( da.el[j] eq els1, nq)
				if (nq ge 2) and (j ge 1) then begin
					q2 = where( da.el[j] eq da.el[0:j-1], nq2)					; prior duplicates (e.g. "Compton")?
					if nq2 eq 0 then begin
						k = q[0]
					endif else begin
						k = q[nq-1]
					endelse					
				endif else k=q[0]
				if nq ne 0 then begin
					t = map_spec( da.matrix[*,j], cal2, cal1, error=error)		; more component matrices
					if error eq 0 then begin
						nt = n_elements(t) < sz1
						mp_matrix[0:nt-1,k,i] = t[0:nt-1]
					endif
					if da.n_pure gt 0 then begin
						t = map_spec( da.pure[*,j], cal2, cal1, error=error)
						if error eq 0 then begin
							nt = n_elements(t) < sz1
							pure[0:nt-1,k,i] = t[0:nt-1]
						endif
					endif
					mp_yield[k,i] = da.yield[j]
				endif
			endfor
			mp_charge[i] = da.charge
		endelse
	endfor

	da = read_da( (*mpdam.pcorr).rest, title='read_da: multi-phase DA read of Rest', error=error)
	if error then goto, bad_mpdam
	sz2 = da.size
	cal2 = {order:1, poly:[da.cal.b,da.cal.a], units:'keV'}
;	if da.n_el ne n1 then goto, bad_mpnum2
	print,'	Rest: ca,cb=',da.cal.a,da.cal.b,' size=',sz2
	for j=0,da.n_el-1 do begin
		q = where( da.el[j] eq els1, nq)
		if (nq ge 2) and (j ge 1) then begin
			q2 = where( da.el[j] eq da.el[0:j-1], nq2)						; prior duplicates (e.g. "Compton")?
			if nq2 eq 0 then begin
				k = q[0]
			endif else begin
				k = q[nq-1]
			endelse					
		endif else k=q[0]
		if nq ne 0 then begin
			t = map_spec( da.matrix[*,j], cal2, cal1, error=error)			; more component matrices
			if error eq 0 then begin
				nt = n_elements(t) < sz1
				mp_matrix[0:nt-1,k,n_comp-1] = t[0:nt-1]
			endif
			if da.n_pure gt 0 then begin
				t = map_spec( da.pure[*,j], cal2, cal1, error=error)		; more component matrices
				if error eq 0 then begin
					nt = n_elements(t) < sz1
					pure[0:nt-1,k,n_comp-1] = t[0:nt-1]
				endif
			endif
			mp_yield[k,n_comp-1] = da.yield[j]
		endif
	endfor
	mp_charge[n_comp-1] = da.charge

;	Need to normalize the Back rows of the DA matrix, as well as the 'pure' spectra.
;	Do this in complementary directions.
;
;	Need to correct Back DA matrix pure, which is still scaled by charge for each phases' DA matrix fit.
;	Normalize the matrix rows for Back to complement the scaling in the poverlay calculation (analyze_image) ...

	qb = where( strlowcase( strmid( da.el, 0,4)) eq 'back', nb)						;@9-19

	if n_comp gt 1 then begin
		for i=0,n_comp-1 do begin
			f = mp_charge[i] / mean(mp_charge)
			for j=0,nb-1 do begin													; all back rows ;@9-19
				mp_matrix[*,j,i] = mp_matrix[*,j,i] * f
				pure[*,j,i] = pure[*,j,i] / f
			endfor
		endfor
	endif
		
; N.B This has to match the form in 'calc_da_matrix2', 'read_da_matrix' and 'read_old_da'

	da = { label:	da1.label, $				; label, name of source data
		file:		filename, $					; local file name
		cal_orig: 	da1.cal_orig, $				; original spectrum cal
		cal:		da1.cal, $					; cal of DA matrix rows
		station: 	da1.station, $				; detector stattion number
		charge:		mp_charge, $				; charge of DA matrix (for each phase fit), for MDLs
		n_el:		n1, $						; number of element rows
		el:			da1.el, $					; element names
		mu_zero: 	da1.mu_zero, $				; major-line mass absorption coeffs
		density0: 	da1.density0, $				; density of top layer
		ecompress: 	da1.ecompress, $			; spectrum compression factor
		mdl:		da1.mdl, $					; mdls at this charge
		size:		sz1, $						; size of a row
		matrix:		mp_matrix, $				; matrix
		yield:		mp_yield, $					; yield factors
		thick:		da1.thick, $				; mg/cm2 for unknown layer
		array:		da1.array, $				; array, yield-ratios, ...
		n_pure:		da1.n_pure, $				; # of pure element unit spectra
		pure:		pure, $ 					; pure spectra
		E_beam:		da1.e_beam, $				; beam energy (keV Xray, meV ionbeam)
		pmore:		ptr_new() }					; pointer to more DA matrices
	
	error = 0
	return, da
endif

; Normal and array DA read ...

OK_version = [-1,-2,-3,-4,-5,-6,-7,-8,-9]
error = 0
if n_elements(title) lt 1 then title='Select DA Matrix file to read'

print,'read_da: file="',file,'"'
if strlen(file) lt 1 then goto, bad_file
last = file

on_ioerror, bad_io
da_file = file
openr, lun, da_file, /xdr, /get_lun

version = 0
readu,lun, version

q = where(version eq OK_version, nq)
if nq eq 0 then begin
	close_file, lun
	da = read_old_da( da_file, error=error)
	if error then goto, bad_version
	return, da
endif

nda_extra = 0L
if version le -9 then begin
	readu,lun, nda_extra
endif

read_da_matrix, lun, da, version, error=error
if error then goto, bad_io
if da.label eq '' then da.label = da_file
da.file = da_file
use_eb = 0
if n_elements(eb) eq 1 then begin
	use_eb = 1
	if eb eq 0.0 then goto, finish

;	Force looking in series, do not use first even if energy is right.
;	This will pick up merging of elastic and Compton if enabled.
endif

if nda_extra lt 1 then goto, finish

pdm = ptrarr(nda_extra)
e = fltarr(nda_extra)
for i=0,nda_extra-1 do begin
	read_da_matrix, lun, da1, version, error=error
	if error then goto, bad_io2
	da1.file = da_file
	e[i] = da1.e_beam
	pdm[i] = ptr_new( da1, /no_copy)
endfor

if use_eb then begin
	q = sort( abs( e - eb))
	if abs(e[q[0]]-eb) gt 0.001 then begin
		warning,'read_da',['Chosen stack DA matrix for XANES using poor energy match.', $
				'E requested ='+str_tidy(eb),'E match ='+str_tidy(e[q[0]])]
	endif
	da = *pdm[q[0]]
	free_da, pdm
endif else begin
	da.pmore = ptr_new( pdm)
endelse

finish:
	close_file,lun
	if n_elements(mpdam) gt 0 then begin
		if size(mpdam, /tname) eq 'STRUCT' then begin
			if tag_present('pcorr',mpdam) then begin
				if ptr_valid(mpdam.pcorr) then ptr_free, mpdam.pcorr
			endif
		endif
	endif
	return, da

bad_io:
	print,'read_da: I/O error'
	goto, error
bad_io2:
	print,'read_da: Extended DA matrux series I/O error'
	goto, error
bad_file:
	print,'read_da: no file name supplied'
	goto, usage
bad_version:
	print,'read_da: bad version number'
	goto, error

usage:
	print,'read_da: Usage: da = read_da(file)'
	print,'		where "da" is the DA matrix struct  returned'
	print,'		and "file" is the name of the input file'
	goto, error

bad_mpdam:
    warning, output=cluster_debug, 'read_da', 'Bad multiphase DA correction file read.'
    goto, error

bad_mpdai:
    warning, output=cluster_debug, 'read_da', 'Bad phase map DAI file read.'
    goto, error

bad_mpsub:
    warning, output=cluster_debug, 'read_da', ['Phase map sub-region size and offset', $
												'do not match the present settings.']
    goto, error

bad_mpnum:
    warning, output=cluster_debug, 'read_da', 'Number of phases in DAI does not match number of DA matrices.'
    goto, error

bad_mpnum2:
    warning, output=cluster_debug, 'read_da', 'Number of elements in one DA does not match number in first matrix.'
    goto, error

bad_mpsize:
    warning, output=cluster_debug, 'read_da', 'Inconsistent DA matrix sizes.'
    goto, error

error:
	da = 0
	error = 1
	goto, finish
end
