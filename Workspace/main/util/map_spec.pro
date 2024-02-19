function map_spec, spec, cal_old, cal_new, table=table, quiet=quiet, error=error

; Re map the spectrum *pspec from old_cal to new_cal
; replaces the hack 'map_to_cal' and can be used for 
; non-linear mappings too, via the table keyword.
; 
;	spec		pointer to spectrum data array, or array itself
;	cal_old		old cal struct
;	cal_new		new cal struct
;				Only treat linear Cals for now.
;
; if Table is used, it is the lookup table into the old cal spec
; to map into the new cal spectrum. 
; Note: Table must be monotonically increasing.
; 
; Calling:
; 1. map to new cal
; 	spec2 = map_spec( pspec, cal_old, cal_new)
; 	
; 2. map using table
; 	spec2 = map_spec( pspec, table=f)
; 	
; Perhaps we need general Energy() and Channel() functions later.

	COMPILE_OPT STRICTARR
	error = 1
	local_spec = 0
	if n_elements(spec) lt 1 then return, 0
	if n_elements(quiet) lt 1 then quiet=0
	f = 0
	if ptr_good(spec) then begin
		pspec = spec
	endif else begin
		if ptr_valid(spec[0]) then return, 0
		if n_elements(spec) gt 1 then begin
			pspec = ptr_new( spec)
			local_spec = 1
		endif else return, 0
	endelse
	siz= n_elements(*pspec)
	
	if n_elements(table) eq 0 then begin
		if n_params(0) lt 3 then goto, bad_cal_miss
		if size(cal_new, /tname) ne 'STRUCT' then goto, bad_cal
		if size(cal_old, /tname) ne 'STRUCT' then goto, bad_cal
		if cal_old.order ne 1 then goto, bad_calo
		if cal_new.order ne 1 then goto, bad_calo
	
		cal_ab, cal_old, ca1, cb1, cu1, error=err
		if err then goto, bad_calab
		cal_ab, cal_new, ca2, cb2, cu2, error=err
		if err then goto, bad_calab

		E1 = (siz-1)*ca1 + cb1
		xm = (E1 - cb2) / ca2
		siz = siz > xm
		x2 = indgen(siz)
		if n_elements(table) eq 0 then xL = ((ca2 * x2 + cb2) - cb1) / ca1
	endif else begin
		q = where(table gt 1., nq)
		if nq eq 0 then goto, bad_table
		xL = table
	endelse
	
	
	xH = shift(xL, -1)
;	left = xL < xH						; need to fix wrap-around at top
;	right = xL > xH						; for this to work properly
	left = xL
	right = xH
	i = clip(floor(left), 0, siz-1)
	j = clip(floor(right), i, siz-1)
	alpha = (float(i+1) - left) > 0.0
	beta = (right - float(j)) > 0.0
	q = where( i eq j, nq)
	if nq ge 1 then begin
		alpha[q] = (right[q] - left[q]) > 0.0
		beta[q] = 0.0
	endif
	f = fltarr(siz)
	
	n = i+1
	q = where(n lt j, nq)
	while nq ge 1 do begin
		f[q] = f[q] + (*pspec)[n[q]]
		n[q] = n[q]+1
		q = where(n lt j, nq)	
	endwhile
	f = f + alpha*(*pspec)[i] + beta*(*pspec)[j]

	error = 0
finish:
	if local_spec then ptr_free, pspec
	return, f

bad_cal_miss:
	if quiet eq 0 then warning,'map_spec','Missing Cal struct'
	goto, finish
bad_cal:
	if quiet eq 0 then warning,'map_spec','Bad Cal struct'
	goto, finish
bad_calab:
	if quiet eq 0 then warning,'map_spec','Bad Cal_ab, unknown units'
	goto, finish
bad_cala:
	if quiet eq 0 then warning,'map_spec','Bad Cal A parameter'
	goto, finish
bad_calo:
	if quiet eq 0 then warning,'map_spec','Bad Cal Order'
	goto, finish
bad_calu:
	if quiet eq 0 then warning,'map_spec','Bad Cal Units'
	goto, finish
bad_table:
	if quiet eq 0 then warning,'map_spec','Zero linearization table'
	goto, finish
end
