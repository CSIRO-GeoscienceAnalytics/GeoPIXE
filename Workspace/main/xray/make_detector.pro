function make_detector, formula, thick=thick, diameter=diameter, $
				filters=filters, density=dens, distance=distance, source=source, $
				Aeff=aeff, Beff=beff, special1=special1, special2=special2, $
				name=name, tail=tail, resolution=resolution, tilt=tilt, $
				w1=w1, cohen=cohen, gamma=gamma, poly=poly, $
				e_low=e_low, e_high=e_high, layout=layout, array=array, $
				shape=shape, correct_solid_angle=correct_solid_angle, error=error

;	Make a detector struct.
;	The struct is now larger, due to the embedded longer filters struct.

if n_elements(thick) lt 1 then thick=10.0
if n_elements(diameter) lt 1 then diameter=10.0
if n_elements(filters) lt 1 then filters=0
if n_elements(distance) lt 1 then distance=25.0
if n_elements(tilt) lt 1 then tilt=0.0
if n_elements(source) lt 1 then source=0.01
if n_elements(aeff) lt 1 then aeff=0.0
if n_elements(beff) lt 1 then beff=0.0
if n_elements(cohen) lt 1 then cohen=0
if n_elements(gamma) lt 1 then gamma=0
if n_elements(formula) eq 0 then formula='Ge'
if n_elements(poly) lt 1 then poly=fltarr(6)
if n_elements(name) lt 1 then name=formula
if n_elements(e_low) lt 1 then e_low=0.1
if n_elements(e_high) lt 1 then e_high=4.0
if n_elements(layout) eq 0 then layout=''
if n_elements(array) eq 0 then array=0
if n_elements(shape) eq 0 then shape=0
if n_elements(correct_solid_angle) eq 0 then correct_solid_angle=1

if formula eq 'Si' then begin
	gamma_factor = 0.022
endif else if formula eq 'Ge' then begin
	gamma_factor = 0.24
endif else begin
	gamma_factor = 0.022
endelse
if n_elements(resolution) lt 1 then resolution = 0.16
if n_elements(tail) lt 1 then tail={F:12.0, B:0.9*thick/1000., amp:0.045, L:0.9, S:0.064}

use_special1=1
if (n_elements(special1) lt 1) then begin
	use_special1=0
	special1 = {Tb:0.01, db:0.01, Rest:0.1}
endif else begin
	if size(special1,/tname) ne 'STRUCT' then use_special1=0
endelse
use_special2=1
if (n_elements(special2) lt 1) then begin
	use_special2=0
	special2 = {c:1.0, w:0.1, h:0.1, c2:1.0, w2:0.1, h2:0.1}
endif else begin
	if size(special2,/tname) ne 'STRUCT' then use_special2=0
endelse

if n_elements(dens) lt 1 then dens=-1.0
if (dens lt 1.0e-10) then begin
	dens = density(atomic_number(formula))
	if dens lt 1.0e-10 then begin
		if arg_present(error) eq 0 then begin
			warning, 'make_detector','compound material but no density.',/error
		endif
		dens = 1.0
		error = 1
	endif
endif

t = 100.0 * thick * dens			; thick in mm, t in mg/cm^2

crystal = make_layer( formula, t, name=name)
crystal.thick = t

if n_elements(w1) lt 1 then begin
	if formula eq 'Si' then begin
		w1 = 0.0033
	endif else if formula eq 'Ge' then begin
		w1 = 0.002089
	endif else begin
		w1 = 0.0033
	endelse
endif
if gamma then begin
	w0 = (resolution*resolution - w1 * 1332.0) > 0.001
endif else begin
	w0 = (resolution*resolution - w1 * 5.895) > 0.001
endelse
error = 0

detector = {absorbers:filters, crystal:crystal, diameter:diameter, $
			density:dens, distance:distance, source:source, aeff:aeff, $
			beff:beff, gamma:gamma_factor, w0:w0, w1:w1, tail:tail, resolution:resolution, $
			tilt:tilt, use_special1:use_special1, use_special2:use_special2, $
			special1:special1, special2:special2, cohen:cohen, pige:gamma, a:poly, $
			e_low:e_low, e_high:e_high, file:'', layout:layout, array:array, shape:shape, $
			correct_solid_angle:correct_solid_angle }

return, detector
end
