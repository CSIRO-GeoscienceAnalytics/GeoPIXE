function weight, formula

; Return molecular weight of 'formula' (e.g. "(Fe2O3)7(Al2O3)2")

decode_formula, formula, n,z,f, atoms=t
if n eq 0 then return, 0.0

sum = 0.
for i=0L,n-1 do begin
	sum = sum + mass(z[i]) * f[i] * t
endfor

return, sum
end
