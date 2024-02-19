function float2, s, error=err

; Convert 's' into a float if it is a valid floating string
;
; Chris Ryan (CSIRO), 2008, revised 2012, 2015, 2020

err = 0
n = n_elements(s)
if n lt 1 then return, 0
if size(s,/tname) ne 'STRING' then return, float(s)

err = 1
t = s
t[*] = '0'
x = float(t)
ok = gnumeric(s)

err = 0
q = where( ok eq 0, nq)
if nq gt 0 then err=1

q = where( ok eq 1, nq)
if nq gt 0 then x[q] = float(s[q])

if n_elements(x) eq 1 then x=x[0]
return, x
end

