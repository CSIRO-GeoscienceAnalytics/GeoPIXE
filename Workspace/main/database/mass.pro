function mass, z

common c_density, mass, density, density_OK

n = n_elements(z)
if n gt 1 then begin
	m = fltarr(n)
endif else begin
	m = 0.0
endelse
if n_elements(density_ok) lt 1 then density_ok = 0
if density_ok ne 1 then init_mass_density

if density_ok ne 1 then return, m

q = where( (z ge 1) or (z lt n_elements(mass)))
if q[0] ne -1 then begin
	m[q] = mass[z[q]]
endif

return, m
end
