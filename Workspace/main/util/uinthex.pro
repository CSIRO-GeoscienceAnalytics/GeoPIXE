function uinthex, s

; take a string in hex code and convert to UINT

b = byte(s)
n = n_elements(b)
if n_elements(b) lt 1 then return, 0US
if n gt 4 then begin
	print,'uinthex: too many digits.'
	return, 0.0
endif

lookup = bytarr(256)
for i=0L,9 do lookup[i+48] = i			; 0-9
for i=10,15 do lookup[i-10+65] = i		; A-F
for i=10,15 do lookup[i-10+97] = i		; e-f

f = 1US
v = 0US
for i=n-1,0,-1 do begin
	v = v + f*lookup(b[i])
;	print,i,b[i],f,v
	f = f*16US
endfor

return, v
end
