function flux_flatten, flux, base=basei, top=top, qbad=qc

;	Calculate scaling to flatten images according to variation from
;	the average of the flux, ignoring any missing (or wildly divergent) pixels.
;	'base' is lowest flux relative to average that's deemed valid. Ignore
;	(don't correct) pixels where flux is lower than this fraction of average,
;	or greater than 'top' times average.
;	'qc' returns pixels that are not corrected.

COMPILE_OPT STRICTARR
ErrorNo = 0
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
		warning,'flux_flatten',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 1.0
	endif
endif

;	help, flux, base
	if n_elements(basei) eq 0 then basei = 0.001
	if n_elements(topi) eq 0 then topi = 1.0e+10
	if n_elements(flux) eq 0 then return, 1.0
	base = basei
	top = topi
	qc = -1L

	base = clip( base, 0.001, 1.)
	top = clip( top, 1.0, 1.0e+30)

	q = where(flux gt 0.0, nq)
	if nq eq 0 then return, 1.0
	
	total_flux = float(total(flux[q]))
	avf = total_flux / float(nq)

	q = where( (flux ge base*avf) and (flux le top*avf), nq, complement=qc, ncomplement=nqc)
	if nq eq 0 then return, 1.0
	print,'Total pixels = ',n_elements(flux),', number in range [base,top] =',nq

	scale = flux
	scale[*] = 1.0
	avf = float( total(flux[q])) / float(nq)
	scale[q] = 1. / (( (flux[q] / avf) > base) < top)

	return, scale
end
