function compress, x, n

; Compress vector 'x' by factor 'n'

m = n_elements(x)
if m lt n then return, x

m2 = m/n
t = replicate( x[0], m2)
q = n * indgen(m2)

for i=0L,n-1 do begin
	if i eq 0 then begin
		t[*] = x[q]
	endif else begin
		t[*] = t[*] + x[i+q]
	endelse
endfor

return,t
end

