function compress2, x, n

; Compress matrix 'x' by factor 'n' in first dimension

m = n_elements(x[*,0])
if m lt n then return, x

m2 = m/n
t = replicate( x[0,0], m2,n_elements(x[0,*]))
q = n * indgen(m2)

for i=0L,n-1 do begin
	if i eq 0 then begin
		t[*,*] = x[q,*]
	endif else begin
		t[*,*] = t[*,*] + x[i+q,*]
	endelse
endfor

return,t
end

