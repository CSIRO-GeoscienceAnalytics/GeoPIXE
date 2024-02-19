function build_gaps2, T=T, pitch=pitch, gap=gap, no_tube=no_tube

; Build an array of gap 'boxes' for detector gap volumes
; Only build a quadrant, and veto inner pads for Mo tube.
;
; This version knows about the new gap model, which symptotes to
; an affective gap of 30 microns on the detector face (target side),
; which represents photoelectron range distribution effects.
; On the pads side (upstream) the gap is set by the 'gap' parameter.
; 
; It approximates this using 4 layers gap boxes:
; 	3 of thickness T/6 and one of thickness T/2.
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
if n_elements(no_tube) lt 1 then no_tube=0					; 1 for Maia 96

nx = 10
ny = 10
veto = no_tube ? -100 : 1									; index to veto for central pads
width = pitch - gap
dT = T/6.
z1a = -T					; layer a, close to pad surface, back face
z2a = -5.*dT
z1b = -5.*dT				; layer b
z2b = -4.*dT
z1c = -4.*dT				; layer c
z2c = -3.*dT
z1d = -3.*dT				; layer d, 1/2 of T nearest to front face
z2d = 0.0
first = 1
xymin = -0.5*gap
;dg = 0.0					; test only!
dg = 0.5*(gap - 0.03)/3. > 0.0

for i=-1L,nx-1 do begin
	for j=-1L,ny-1 do begin
	  if (i ge veto) or (j ge veto) then begin
	  	no_box2 = 0
	  
		if i eq -1 and j eq -1 then begin
			x1a = -0.5*gap									; start of gap in X
			x2a = 0.5*gap									; end of gap in X (layer a)
			x1b = -0.5*gap									; 
			x2b = 0.5*gap - dg								; layer b
			x1c = -0.5*gap									; 
			x2c = 0.5*gap - 2.*dg							; layer c
			x1d = -0.5*gap									; 
			x2d = 0.5*gap - 3.*dg							; layer d

			y1a = -0.5*gap									; start of gap in Y
			y2a = 0.5*gap									; end of gap in Y (layer a)
			y1b = -0.5*gap									; 
			y2b = 0.5*gap - dg								; layer b
			y1c = -0.5*gap									; 
			y2c = 0.5*gap - 2.*dg							; layer c
			y1d = -0.5*gap									; 
			y2d = 0.5*gap - 3.*dg							; layer d
			no_box2 = 1
			goto, box
		endif

		if i eq -1 and j ge 0 then begin
			x1a = -0.5*gap									; start of gap in X
			x2a = 0.5*gap									; end of gap in X (layer a)
			x1b = -0.5*gap									; 
			x2b = 0.5*gap - dg								; layer b
			x1c = -0.5*gap									; 
			x2c = 0.5*gap - 2.*dg							; layer c
			x1d = -0.5*gap									; 
			x2d = 0.5*gap - 3.*dg							; layer d
			no_box2 = no_box2 or 1		
		endif else begin
;			no_box2 = 0
			x1a = pitch*float(i+1)-0.5*gap					; start of gap in X
			x2a = x1a + gap									; end, vertical (layer a)
			x3a = pitch*float(i)+0.5*gap					; 
			x4a = x3a + pitch								; horizontal
			
			x1b = x1a + dg									; start of gap in X
			x2b = x2a - dg									; end, vertical (layer b)
			x3b = x3a - dg									; 
			x4b = x4a - dg									; horizontal
			
			x1c = x1a + 2.*dg								; start of gap in X
			x2c = x2a - 2.*dg								; end, vertical (layer c)
			x3c = x3a - 2.*dg								;	 
			x4c = x4a - 2.*dg								; horizontal
			
			x1d = x1a + 3.*dg								; start of gap in X
			x2d = x2a - 3.*dg								; end, vertical (layer d)
			x3d = x3a - 3.*dg								; 
			x4d = x4a - 3.*dg								; horizontal
		endelse

		if j eq -1 and i ge 0 then begin
			x1a = x3a
			x2a = x4a
			y1a = -0.5*gap									; start of pad in Y
			y2a = 0.5*gap 									; layer a

			x1b = x3b
			x2b = x4b
			y1b = y1a										; start of pad in Y
			y2b = y2a - dg									; layer b

			x1c = x3c
			x2c = x4c
			y1c = y1a										; start of pad in Y
			y2c = y2a - 2.*dg								; layer c

			x1d = x3d
			x2d = x4d
			y1d = -y1a										; start of pad in Y
			y2d = y2a - 3.*dg								; layer d
			no_box2 = no_box2 or 1
		endif else begin
;			no_box2 = 0
			y1a = pitch*float(j)+0.5*gap					; start of pad in Y
			y2a = y1a + pitch - gap							; end vertical, layer a
			y3a = y2a										; 
			y4a = y3a + gap									; horizontal
			
			y1b = y1a - dg									; start of pad in Y
			y2b = y2a + dg									; end vertical, layer b
			y3b = y2b										; 
			y4b = y4a - dg									; horizontal
			
			y1c = y1a - 2.*dg								; start of pad in Y
			y2c = y2a + 2.*dg								; end vertical, layer c
			y3c = y2c										; 
			y4c = y4a - 2.*dg								; horizontal
			
			y1d = y1a - 3.*dg								; start of pad in Y
			y2d = y2a + 3.*dg								; end vertical, layer d
			y3d = y2d										; 
			y4d = y4a - 3.*dg								; horizontal
		endelse

		if i eq veto and j lt veto then begin
			x1a = x1a > (float(i+1)*pitch-0.5*gap)
			x3a = x3a > (float(i+1)*pitch-0.5*gap)
			x1b = x1b > (float(i+1)*pitch-0.5*gap)
			x3b = x3b > (float(i+1)*pitch-0.5*gap)
			x1c = x1c > (float(i+1)*pitch-0.5*gap)
			x3c = x3c > (float(i+1)*pitch-0.5*gap)
			x1d = x1d > (float(i+1)*pitch-0.5*gap)
			x3d = x3d > (float(i+1)*pitch-0.5*gap)
		endif
		if j eq veto and i lt veto then begin
			no_box2 = 1
			x1a = x3a
			x2a = x4a
			y1a = y3a
			y2a = y4a

			x1b = x3b
			x2b = x4b
			y1b = y3b
			y2b = y4b

			x1c = x3c
			x2c = x4c
			y1c = y3c
			y2c = y4c

			x1d = x3d
			x2d = x4d
			y1d = y3d
			y2d = y4d
		endif
		
box:
		order = 1 + long(i)*long(i) + long(j)*long(j)
		box1a = { x:[x1a,x2a]>xymin, y:[y1a,y2a]>xymin, z:[z1a,z2a], material:'Si', IDx:i, IDy:j, order:order, level:0 }
		box1b = { x:[x1b,x2b]>xymin, y:[y1b,y2b]>xymin, z:[z1b,z2b], material:'Si', IDx:i, IDy:j, order:order, level:1 }
		box1c = { x:[x1c,x2c]>xymin, y:[y1c,y2c]>xymin, z:[z1c,z2c], material:'Si', IDx:i, IDy:j, order:order, level:2 }
		box1d = { x:[x1d,x2d]>xymin, y:[y1d,y2d]>xymin, z:[z1d,z2d], material:'Si', IDx:i, IDy:j, order:order, level:3 }
		if first then begin
			gaps = [box1a,box1b,box1c,box1d]
			first = 0
		endif else gaps = [gaps, box1a,box1b,box1c,box1d]
		if no_box2 eq 0 then begin
			box2a = { x:[x3a,x4a]>xymin, y:[y3a,y4a]>xymin, z:[z1a,z2a], material:'Si', IDx:i, IDy:j, order:order, level:0 }
			box2b = { x:[x3b,x4b]>xymin, y:[y3b,y4b]>xymin, z:[z1b,z2b], material:'Si', IDx:i, IDy:j, order:order, level:1 }
			box2c = { x:[x3c,x4c]>xymin, y:[y3c,y4c]>xymin, z:[z1c,z2c], material:'Si', IDx:i, IDy:j, order:order, level:2 }
			box2d = { x:[x3d,x4d]>xymin, y:[y3d,y4d]>xymin, z:[z1d,z2d], material:'Si', IDx:i, IDy:j, order:order, level:3 }
			gaps = [gaps, box2a,box2b,box2c,box2d]
		endif
;		if j le 0 and i eq 1 then begin
;			print,'test'
;		endif
	  endif
	endfor
endfor

return, gaps
end
