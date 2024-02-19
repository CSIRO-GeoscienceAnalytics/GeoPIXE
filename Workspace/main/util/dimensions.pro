function dimensions, x, n=n, type=type

s = size(x)

d = s[0]
nd = n_elements(s)
n = s[nd-1]
type = s[nd-2]

return, d
end
