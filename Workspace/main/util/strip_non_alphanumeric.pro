function strip_non_alphanumeric, str

; Strip non-alphanumeric characters from 'str'

if n_elements(str) lt 1 then return, ''

b = byte(str)
q = where( ((b ge 48) and (b le 57)) or ((b ge 65) and (b le 90)) or $
					((b ge 97) and (b le 122)) )

if q[0] eq -1 then return, ''
s = string(b[q])

return, s
end
