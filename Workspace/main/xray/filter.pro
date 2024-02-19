function filter, formula, thick, E, weight=weight, $
			microns=microns, density=dens, pinratio=pinratio

;	May want to add Bragg parameters later

if n_elements(weight) lt 1 then weight=0
if n_elements(microns) lt 1 then microns=0
if n_elements(dens) lt 1 then dens=-1.0
if n_elements(pinratio) lt 1 then pinratio=0.0
pinhole = 0
if pinratio gt 1.0 then pinhole=1
if (dens lt 0.0) and microns then begin
	dens = density(atomic_number(formula))
	if dens lt 1.0e-10 then begin
		print,'Error in filter: /microns but no density.'
		dens = 1.0
	endif
endif

t = thick
if microns then t = t * dens / 10.

decode_formula, formula, n,z,f, weight=weight

t = transmit( {N:n, Z:z, F:f, thick:t, pinhole:pinhole, pinratio:pinratio }, E)

return, t
end
