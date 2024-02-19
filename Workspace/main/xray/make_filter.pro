function make_filter, formula, thick, pinratio=pinratio, microns=microns, $
				density=dens, weight=weight, name=name, error=error

;	Make a filter absorber spec.
;	It now returns an expanded struct (see define(/filter) ).
;
;	Later - may want to expand this for Bragg parameters

error = 1
if n_elements(formula) lt 1 then formula='Be'
if n_elements(thick) lt 1 then thick=1.0e-6
if n_elements(weight) lt 1 then weight=0
if n_elements(microns) lt 1 then microns=0
if n_elements(dens) lt 1 then dens=-1.0
if n_elements(name) lt 1 then name=formula
if n_elements(pinratio) lt 1 then pinratio=0.0
pinhole = 0
if pinratio gt 1.0 then pinhole=1
if (dens lt 1.0e-10) and microns then begin
	dens = density(atomic_number(formula))
	if dens lt 1.0e-10 then begin
		warning, 'make_filter','/microns but no density.',/error
		dens = 1.0
	endif
endif
error = 0

t = thick
if microns then t = t * dens / 10.

decode_formula, formula, n,z,f, weight=weight

; N.B. decode_formula always returns 32 element vectors for
; 'z' and 'f', as in the filter struct.

filt = define(/filter)

filt.n = n
filt.z = z
filt.f = f
filt.thick = t				; always stored in mg/cm^2
filt.pinhole = pinhole
filt.pinratio = pinratio
filt.name = name
filt.microns = microns		; was entered as microns
filt.density = dens			; was used if /microns
filt.formula = formula
filt.weight = weight

return, filt
end
