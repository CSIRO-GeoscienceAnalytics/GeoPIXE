function element_name, z

common c_atomic_1, periodic_table, n_periodic

if n_elements(n_periodic) lt 1 then t = atomic_number('He')

n = n_elements(z)
if n lt 1 then return, ''
el = strarr(n)

q = where( (z gt 0) and (z lt n_periodic))
if q[0] ne -1 then el[q] = periodic_table[z[q]]

el =  reform(el)
if n_elements(el) eq 1 then el = el[0]
return, el
end
