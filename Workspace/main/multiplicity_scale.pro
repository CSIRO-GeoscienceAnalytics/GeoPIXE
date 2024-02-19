function multiplicity_scale, pactive, pmatrix, multiplicity=multiplicity, select=select

; Calculate the multiplicity scaling factor "fk" for each element 'k'
;	pactive			(pointer to or) list of active channels
;	pmatrix			matrix struct or pointer to one
;	multiplicity	default multiplicity
;	select			select just one element index (XANES stacks)

COMPILE_OPT STRICTARR
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'multiplicity_scale',['IDL run-time error caught.', '', $
			'Error:  '+strtrim(!error_state.name,2), $
			!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
if n_elements(multiplicity) lt 1 then multiplicity=1
if multiplicity lt 1 then multiplicity=1
matrix = pmatrix
if ptr_good( pmatrix) then matrix = *pmatrix
good_array = 1
if size(pactive, /tname) eq 'POINTER' then begin
	if ptr_good( pactive) then active = *pactive else good_array=0
endif else begin
	active = pactive
endelse
if n_elements(select) eq 0 then begin
	select=indgen(matrix.n_el)
	rG = fltarr(matrix.n_el)
	rGdef = replicate(float(multiplicity > 1), matrix.n_el)
endif else begin
	rG = 0.
	rGdef = float(multiplicity > 1)
endelse

	if matrix.array.on and good_array then begin
		q = where(active lt matrix.array.n_det, nd)
		if nd ge 1 then begin
			for j=0L,nd-1 do begin
				rG = rG + matrix.array.rGamma[active[q[j]],select]
			endfor
		endif else begin
			warning,'multiplicity_scale','Array detector data, but no valid channels.'
			rG = 1.
		endelse
	endif else begin
		rG = rGdef
;		print,'multiplicity_scale: Bad Array detector data, use simple multiplicity.'
	endelse

	return, rG
end
