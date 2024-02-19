function qsample, d, nq, veto_low=vlow, veto_high=vhigh, positive=positive, non_zero=non_zero, min=xmin

; Sample an array based on value. Report indices of representative values.
; Sort array and veto % of ends. Return indices of 'middle'.
;
;	vlow		% of low part of sorted distribution to veto
;	vhigh		% of high part of sorted distribution to veto
;	/positive	only consider positive values
;	/non_zero	only consider non-zero values
;	min=min		only consider values above 'min'

COMPILE_OPT STRICTARR
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
		warning,'qsample',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		print,'qsample: ql, qh, nq = ',ql,qh,nq
		nq = 0L
		return, 0L
	endif
endif

	if n_elements(vlow) lt 1 then vlow=0.
	if n_elements(vhigh) lt 1 then vhigh=0.
	if n_elements(positive) lt 1 then positive=0
	if n_elements(non_zero) lt 1 then non_zero=0
	if n_elements(xmin) lt 1 then xmin=-1.e+30
	ql = -1L
	qh = -1L
	nq = 0L

	n = n_elements(d)
	if n lt 1 then return, 0L
	if size(d,/tname) eq 'POINTER' then begin
		p = d
		if ptr_good(p) eq 0 then return, 0L
		n = n_elements(*p)
	endif else begin
		p = ptr_new(d)
	endelse
	if vlow + vhigh ge 100. then begin
		warning,'qsample','veto_low + veto_high > 100'
		return, 0L
	endif
	
	if positive then begin
		q1 = where( (*p gt 0.) and (*p gt xmin), nq)			; only consider positive
	endif else if non_zero then begin
		q1 = where( (*p ne 0.) and (*p gt xmin), nq)			; only consider non-zero
	endif else begin
		q1 = where( (*p gt xmin), nq)							; consider all above 'min'
	endelse
	if nq eq 0 then return, 0L
	
	q = sort((*p)[q1])
	
	ql = (long(( (vlow < 99.) * nq) / 100) > 0) < (nq-1)
	qh = (long((nq-1) - ((vhigh < 99.) * nq) / 100) > 0) < (nq-1)
	if ql gt qh then begin
		nq = 0
		q = -1
		goto, done
	endif
	q = q[ql:qh]
	nq = n_elements(q)
	q2 = q1[q]

done:
	return, q2
end
