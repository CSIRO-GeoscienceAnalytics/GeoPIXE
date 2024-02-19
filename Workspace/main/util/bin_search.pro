function bin_search,  e, li,hi
;
; Binary search routine (recursive)
;
common c_bin_search, bin_list_id

l = li
h = hi
if l eq -1 then begin
	l = 0
	h = n_elements(bin_list_id.e)-1
endif
m = (l+h)/2
if m le l then begin
	em = 0.5*( bin_list_id.e[l] + bin_list_id.e[h])
	if e ge em then begin
		return, h
	endif else begin
		return, l
	endelse
endif

if e lt bin_list_id.e[m] then begin
	return, bin_search( e, l,m)
endif else begin
	return, bin_search( e, m,h)
endelse
end