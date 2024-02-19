function strip_char, str, chr

; Strip selected 'chr' from 'str'

COMPILE_OPT STRICTARR
if n_elements(str) lt 1 then return, ''
if n_elements(chr) lt 1 then return, str

ns = n_elements(str)
s = str
c = byte(chr)
nc = n_elements(c)

for j=0L,ns-1 do begin
	b = byte(str[j])
	nb = n_elements(b)
	ok = replicate(1,nb)

	for i=0L,nc-1 do begin
		q = where( b eq c[i] )
		if q[0] ne -1 then ok[q] = 0
	endfor

	q = where( ok eq 1)
	if q[0] ne -1 then s[j]= string(b[q]) else s[j]=''
endfor

if ns eq 1 then s=s[0]
return, s
end
