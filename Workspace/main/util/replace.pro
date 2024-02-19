function replace, c, new, str

; Replace character 'c' in 'str' with 'new'

if n_elements(str) lt 1 then return, ''

n = n_elements(str)
out = strarr(n)
bc = byte(c)

for i=0L,n-1 do begin
	b = byte(str[i])
	q = where(b eq bc[0])
	if q[0] ne -1 then b[q] = byte(new)
	
	q2 = where( b ne 0, nq2)
	out[i] = (nq2 gt 0) ? string(b[q2]) : ''
endfor
if n_elements(out) eq 1 then out=out[0]

return, out
end
