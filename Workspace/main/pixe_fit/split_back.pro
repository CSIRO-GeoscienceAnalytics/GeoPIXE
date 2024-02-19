function split_back, pspec, pback, emid=emid, espan=espan, do_split=do_split

;	Split a background into two parts around an 'emid' dip,
;	only if /do_split. 'espan' controls the energy over which it transitions.
;	'pback' assumed to be already pointer to pointer(s) to back structs.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'split_back',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, pback
	endif
endif

	if n_elements(do_split) lt 1 then do_split=1
	if n_elements(emid) lt 1 then emid = 10.
	if n_elements(espan) lt 1 then espan = emid/8.
	if (ptr_good(*pback) eq 0) then return, ptr_new()
	if (ptr_good((*pback)[0]) eq 0) then return, ptr_new()
	if (ptr_good((*(*pback)[0]).data) eq 0) then return, ptr_new()

	two = (n_elements(*pback) gt 1)
	if two then if (ptr_good((*pback)[1]) eq 0) then two=0
	
	ca = (*pspec).cal.poly[1]
	cb = (*pspec).cal.poly[0]
	n = (*pspec).size
	spec = *(*pspec).data
	if two then begin
		back = *(*(*pback)[0]).data + *(*(*pback)[1]).data
	endif else begin
		back = *(*(*pback)[0]).data
	endelse
	e = cb + ca*findgen(n)
	mid = round((emid - cb)/ca)

;	window, 0
;	plot, e, back, /ylog, yrange=[500,max(spec)],xrange=[2,30]
;	oplot, e, spec, psym=10, color=spec_colour('green')

	if do_split then begin
		nspan = espan / ca
		g = -alog( 0.1) / float(nspan)
		f = 1./(1. + exp( -g*(findgen(n)-mid)))
	
		back2 = back * f
		back1 = back - back2
	endif else begin
		back1 = back
		back2 = back
		back2[*] = 0.0
	endelse

;	oplot, e, back1, color=spec_colour('red')
;	oplot, e, back2, color=spec_colour('blue')

	if two eq 0 then begin
		copy_pointer_data, (*pback)[0], pback2, /init
		*(*(*pback)[0]).data = back1
		*(*pback2).data = back2
		new_back = ptr_new([(*pback)[0],pback2])
	endif else begin
		*(*(*pback)[0]).data = back1
		*(*(*pback)[1]).data = back2
		new_back = pback
	endelse

	return, new_back
end
