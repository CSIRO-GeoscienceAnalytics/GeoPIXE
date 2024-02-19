function make_layer, formula, thick, microns=microns, $
				density=dens, weight=weight, name=name, $
				error=error

;	Make a layer spec.

COMPILE_OPT STRICTARR
if n_elements(formula) lt 1 then formula='C'
if n_elements(thick) lt 1 then thick=1.0e-6
if n_elements(weight) lt 1 then weight=0
if n_elements(microns) lt 1 then microns=0
if n_elements(name) lt 1 then name=formula
if n_elements(dens) lt 1 then dens=-1.0
if (dens le 1.0e-10) and microns then begin
	dens = density(atomic_number(formula))
	if dens lt 1.0e-10 then begin
		if arg_present(error) eq 0 then begin
			warning, 'make_layer','Compound layer specified in microns but no density provided.',/error
		endif
		dens = 1.0
		error = 1
	endif
endif

t = thick
if microns then t = t * dens / 10.

decode_formula, formula, n,z,f, weight=weight

; N.B. decode_formula always returns 32 element vectors for 'z' and 'f'

lay = {layer, N:n, Z:z, F:f, thick:t, name:name }

error = 0
return, lay
end
