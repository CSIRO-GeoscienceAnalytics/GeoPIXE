function count_steps, x, steps=steps

; Count the number of steps in 'x'
; i.e. Number of times it changes.

n = n_elements(x)
steps = 0
if n lt 1 then return, 0
if n eq 1 then return, 1

q = where( x ne shift(x,1))
if q[0] eq -1 then return, 1
steps = q

return, n_elements(q)
end


