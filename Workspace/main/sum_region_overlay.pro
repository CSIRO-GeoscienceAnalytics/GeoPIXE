function sum_region_overlay, image, yield, phase, pure

;	These are each a vector over pixel vector 'qe':
;		image	conc*Q per pixel, for one element
;		yield	yield per pixel, for one element
;		phase	phase per pixel, for one phase
;
;	This is a single spectrum:
;		pure	spec per one element, one phase

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
		warning,'sum_region_overlay',['IDL run-time error caught.', '', $
			'Error:  '+strtrim(!error_state.name,2), $
			!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	pixeln = n_elements(image)
	size = n_elements(pure)
	spec = fltarr(size)
	
	sum = total( image * yield * phase, /NaN)
	spec = sum * pure

;	help, image , yield , phase, sum
;	print,'		sum_region_overlay: image - ', min(image),max(image),mean(image)
;	print,'		sum_region_overlay: yield - ', min(yield),max(yield),mean(yield)
;	print,'		sum_region_overlay: phase - ', min(phase),max(phase),mean(phase)
	return, spec
end
