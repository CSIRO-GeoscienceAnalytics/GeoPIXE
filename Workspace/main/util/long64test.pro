function long64test, x, strict=strict

; Chris Ryan (CSIRO), revised 2021

if n_elements(strict) eq 0 then strict=0

n = n_elements(x)
if n lt 1 then return, 0L
r = lon64arr(n)

q = where( inumeric(x, strict=strict) eq 1, nq)
if nq gt 0 then r[q] = long64(x[q])

return, r
end
