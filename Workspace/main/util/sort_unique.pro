function sort_unique, x, ns, last=last, first=first, descend=descend

; Sort an array in ascending order (unless /descend set)
; and eliminate duplicates. For /first show the first of duplicates,
; for /last show the last.
;
; 'x' can be string, in which case use alphabetical order
; or integer for numerical order.
; 
; return the new index array.

COMPILE_OPT STRICTARR
ns = n_elements(x)
if ns le 0 then return, -1L
if ns eq 1 then return, 0L
if n_elements(descend) lt 1 then descend=0
if n_elements(last) lt 1 then last=0
if n_elements(first) lt 1 then first = (last eq 0) ? 1 : 0
if first then last=0

;	Uniq() uses the LAST of duplicates, so need to fool it to return first
;	normally, unless /last passed.

if last XOR descend then begin
	offset = 100000L + indgen(ns,/long)
endif else begin
	offset = 999999L - indgen(ns,/long)
endelse

case size(x,/tname) of
	'STRING': begin
		t = x + str_tidy(offset)
		q = sort(t)
		if descend then q=reverse(q)
		q2 = uniq(x[q])
		q3 = q[q2]
		end
		
	'INT': goto, number
	'UINT': goto, number
	'LONG': goto, number
	'ULONG': goto, number
	'LONG64': goto, number
	'ULONG64': goto, number
	'BYTE': goto, number

	else: return, 0L
endcase

ns = n_elements(q3)
return, q3

number:		

;	Uniq() uses the LAST of duplicates, so need to fool it to return first
;	normally, unless /last passed.

	f = double(x) + double(offset)/1000000.
	q = sort(f)
	if descend then q=reverse(q)
	q2 = uniq(x[q])
	q3 = q[q2]

	ns = n_elements(q3)
	return, q3
end
