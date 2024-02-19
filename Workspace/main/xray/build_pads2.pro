function build_pads2, T=T, pitch=pitch, gap=gap, no_tube=no_tube

; Build an array of pad 'boxes' for detector active volumes
; Only build a quadrant, and veto inner pads for Mo tube.
;
; This version knows about the new gap model, which symptotes to
; an affective gap of 30 microns on the detector face (target side),
; which represents photoelectron range distribution effects.
; On the pads side (upstream) the gap is set by the 'gap' parameter.
; 
; It approximates this using 4 layers gap boxes:
; 	3 of thickness T/6 and one of thickness T/2, starting with level=0
; 	on the pad side, away from the target.
; 
;	T			thickness of the detector wafer (mm)
;	pitch		pitch between detector pads (mm)
;	gap			gap between pads (mm)
;	/no_tube	do not veto central detectors for a Mo tube
;	
; Format of each element:
;	box = { x:[-3.,-1.], y:[-2.,2.], z:[-1.,0.], material:'Si', IDx:0, IDy:0, order:0.0 }
;
;	x,y,z		give dimensions of box on x,y,z axes
;	material	is the composition of the box
;	IDx, IDy	are the x,y indices for a detector in array
;	order		is a priority order, giving the radial distance from origin

compile_opt strictarr
if n_elements(T) lt 1 then T=0.4
if n_elements(pitch) lt 1 then pitch=1.0
if n_elements(gap) lt 1 then gap=0.075
if n_elements(no_tube) lt 1 then no_tube=0						; 1 for Maia 96

nx = 10
ny = 10
veto = no_tube ? -100 : 2									; index to veto for central pads
width = pitch - gap
dT = T/6.
z1a = -T					; layer a, close to pad surface, back face
z2a = -5.*dT
z1b = -5.*dT				; layer b
z2b = -4.*dT
z1c = -4.*dT				; layer c
z2c = -3.*dT
z1d = -3.*dT				; layer d, 1/2 of T nearest to front/tgt face
z2d = 0.0
first = 1
dg = 0.0					; test only!
;dg = 0.5*(gap - 0.03)/3. > 0.0

for i=0L,nx-1 do begin
	for j=0L,ny-1 do begin
	  if (i ge veto) or (j ge veto) then begin
		x1a = 0.5*gap + pitch*float(i)					; start of pad in X
		x2a = x1a + width								; end vertical, layer a
		y1a = 0.5*gap + pitch*float(j)					; 
		y2a = y1a + width								; horizontal
		
		x1b = x1a - dg									; start of pad in X
		x2b = x2a + dg									; end vertical, layer b
		y1b = y1a - dg									; 
		y2b = y2a + dg									; horizontal
		
		x1c = x1a - 2.*dg								; start of pad in X
		x2c = x2a + 2.*dg								; end vertical, layer c
		y1c = y1a - 2.*dg								; 
		y2c = y2a + 2.*dg								; horizontal
		
		x1d = x1a - 3.*dg								; start of pad in X
		x2d = x2a + 3.*dg								; end vertical, layer d
		y1d = y1a - 3.*dg								; 
		y2d = y2a + 3.*dg								; horizontal
		
		order = 1 + long(i)*long(i) + long(j)*long(j)
		boxa = { x:[x1a,x2a], y:[y1a,y2a], z:[z1a,z2a], material:'Si', IDx:i, IDy:j, order:order, level:0 }
		boxb = { x:[x1b,x2b], y:[y1b,y2b], z:[z1b,z2b], material:'Si', IDx:i, IDy:j, order:order, level:1 }
		boxc = { x:[x1c,x2c], y:[y1c,y2c], z:[z1c,z2c], material:'Si', IDx:i, IDy:j, order:order, level:2 }
		boxd = { x:[x1d,x2d], y:[y1d,y2d], z:[z1d,z2d], material:'Si', IDx:i, IDy:j, order:order, level:3 }
		if first then begin
			pads = [boxd,boxc,boxb,boxa]
			first = 0
		endif else pads = [pads, boxd,boxc,boxb,boxa]
	  endif
	endfor
endfor

return, pads
end
