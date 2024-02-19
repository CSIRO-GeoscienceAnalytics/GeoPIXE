function strip_non_numeric, str

; Strip non-numeric characters from 'str'

n = n_elements(str)
if n lt 1 then return, ''
s = strarr(n)

for i=0L,n-1 do begin
	b = byte(str[i])
	q = where( ((b ge 48) and (b le 57)) )

	if q[0] ne -1 then s[i] = string(b[q])
endfor

if n eq 1 then s=s[0]
return, s
end
