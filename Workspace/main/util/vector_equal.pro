function vector_equal, v1, v2, order=order

;	Are the vectors equal (same lengths and contain same values)?
;	/order	and in same order

COMPILE_OPT STRICTARR
if n_elements(order) eq 0 then order=0

if n_elements(v1) eq 0 then return, 0
if n_elements(v2) eq 0 then return, 0
n = n_elements(v1)
if n ne n_elements(v2) then return, 0

real = 1
if size(v1,/tname) eq 'BYTE' then real=0
if size(v1,/tname) eq 'INT' then real=0
if size(v1,/tname) eq 'UINT' then real=0
if size(v1,/tname) eq 'LONG' then real=0
if size(v1,/tname) eq 'ULONG' then real=0
if size(v1,/tname) eq 'LONG64' then real=0
if size(v1,/tname) eq 'ULONG64' then real=0

if order then begin
	diff = v1 - v2
	v1q = v1
endif else begin
	q1 = sort(v1)
	q2 = sort(v2)
	diff = v1[q1] - v2[q2]
	v1q = v1[q1]
endelse

true = 1
if real eq 0 then begin
	q = where( diff ne 0, nq)
	if nq ne 0 then true = 0
endif else begin
	small = 0.0001 > (abs( v1q) /1000.)
	q = where( abs(diff) gt small, nq)
	if nq ne 0 then true = 0
endelse

return, true
end
