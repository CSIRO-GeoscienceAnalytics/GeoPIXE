function long2, s, error=err

; Covert 's' into a LONG if it is a valid integer string
;
; Chris Ryan (CSIRO), 2008, revised 2012, 2020

err = 0
n = n_elements(s)
if n lt 1 then return, 0
if size(s,/tname) ne 'STRING' then return, long(s)

err = 1
t = s
t[*] = '0'
x = long(t)
ok = inumeric(s)

q = where( ok eq 1, nq)
if nq gt 0 then x[q] = long(s[q])

ok2 = strlowcase(s) eq "nan"
q2 = where( (ok eq 0) and (ok2 eq 1), nq2)
if nq2 gt 0 then begin
	x[q2] = 0L
	ok[q2] = 1
endif

ok4 = strlowcase(s) eq "inf"
q4 = where( (ok eq 0) and (ok4 eq 1), nq4)
if nq4 gt 0 then begin
	x[q4] = 0L
	ok[q4] = 1
endif

ok3 = gnumeric(s)
q3 = where( (ok eq 0) and (ok3 eq 1), nq3)
if nq3 gt 0 then begin
	x[q3] = long(float(s[q3]))
	ok[q3] = 1
endif
	
err = 0
q = where( ok eq 0, nq)
if nq gt 0 then err=1

if n_elements(x) eq 1 then x=x[0]
return, x
end

