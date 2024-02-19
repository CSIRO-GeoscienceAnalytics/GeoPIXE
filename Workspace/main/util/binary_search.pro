function bin_search2, list, e, lo,hi
;
; Binary search routine (recursive)
;
l = lo
h = hi
nl = n_elements(list)
rev = (list[0] gt list[nl-1])
if l eq -1 then begin
	l = 0
	h = nl-1
endif
m = (l+h)/2
if m le l then begin
	em = 0.5*( list[l] + list[h])
	if (e ge em) xor rev then begin
		return, h
	endif else begin
		return, l
	endelse
endif

if (e lt list[m]) xor rev then begin
	return, bin_search2( list, e, l,m)
endif else begin
	return, bin_search2( list, e, m,h)
endelse
end

;----------------------------------------------------------------

function binary_search, list, e
;
; Binary search routine (recursive)
;
return, bin_search2( list, e, -1,-1)
end
