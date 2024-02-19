function search,  list, e, li,hi
;
; Binary search routine (recursive)
;
if n_elements(li) lt 1 then li=-1
n = n_elements(list)
if n_elements(hi) lt 1 then hi=n-1

l = li
h = hi
if l eq -1 then begin
	l = 0
	h = n-1
endif
m = (l+h)/2
if m le l then begin
	em = 0.5*( list[l] + list[h])
	if e ge em then begin
		return, h
	endif else begin
		return, l
	endelse
endif

if e lt list[m] then begin
	return, search( list, e, l,m)
endif else begin
	return, search( list, e, m,h)
endelse
end