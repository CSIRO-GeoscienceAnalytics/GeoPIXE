function strip_non_alphanumeric, str, no_space=no_space

; Strip non-alphanumeric characters from 'str'

if n_elements(no_space) lt 1 then no_space=0

ns = n_elements(str)
if ns lt 1 then return, ''
if ns gt 1 then begin
	s2 = strarr(ns)
	for i=0,ns-1 do s2[i] = strip_non_alphanumeric( str[i], no_space=no_space)
	return, s2
endif

if n_elements(str) lt 1 then return, ''

b = byte(str)
q = where( ((no_space eq 0) and (b eq 32)) or ((b ge 48) and (b le 57)) or ((b ge 65) and (b le 90)) or $
					((b ge 97) and (b le 122)) )

if q[0] eq -1 then return, ''
s = string(b[q])

return, s
end
