function angstroms, E

; convert E (keV) to wavelength in Angstroms

n = n_elements(E)
if n lt 1 then return, 0.0

W = fltarr(n)
q = where( E gt 0.1)
if q[0] eq -1 then return, W

W[q] = 12.398135 / E[q]

return, W
end
