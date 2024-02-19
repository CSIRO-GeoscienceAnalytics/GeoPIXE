function cross_reference, name1, name2

; Form an array of indices that give the matching
; entry in array 'name2' for each in 'name1'.

n1 = n_elements(name1)
q = lonarr(n1)

for i=0L,n1-1 do begin
	q[i] = where(name1[i] eq name2)
endfor

return, q
end
