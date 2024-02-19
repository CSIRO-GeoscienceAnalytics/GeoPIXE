function detector_geometry, di, distance, theta, phi, tilt=gtilt, xtilt=xtilt, $
			reorient=reorient, symmetry=symmetry, mirrorX=mirrorX, $
			mirrorY=mirrorY, error=error

; Calculate detector pad geometry for yield calculation. Take care - this routine returns
; a struct array in CSV table order. May need to re-order to detector number elsewhere.
;
; Detector layout data 'd' gives local coordinates in the array, area, local tilt angle.
; From this, and 'theta', 'phi', 'tilt', 'distance' for the array as a whole, calculate
; the individual 'theta', 'phi', 'R', 'tilt' for all detector elements.
;
; Input:
;	d			detector layout data struct (see read_detector_layout.pro)
;	theta		rotation of detector as a whole (about Y/vertical axis)
;	phi			rotation of detector as a whole (about Z/beam axis)
;	xtilt		global tilt of the detector as a whole (about X axis)
;	gtilt		global tilt of the detector as a whole (about Y axis)
;				from pointing directly at target
;	distance	distance of detector plane from target
;	reorient	rotation in steps of 360/symmetry (default in 'd' struct)
;	symmetry	rotation symmetry steps for 360 degree (default in 'd' struct)
;	mirrorX		mirror X coordinates first before reorient (default in 'd' struct)
;	mirrorY		mirror Y coordinates first before reorient (default in 'd' struct)
;
; Return:
;	geometry	struct containing {index, theta, phi, R, tilt} for each element
;				index is just a copy of the index in 'd'.
;	error		1 means error.
;
; Remember: any changes here likely to affect "detector_build" too.
; Used in routines: geo_array_yield, array_yield, detector_efficiency, detector_solid_angle

error = 1
if n_elements(di) lt 1 then return, 0
if n_elements(distance) lt 1 then distance=50.
if n_elements(theta) lt 1 then theta=90.
if n_elements(phi) lt 1 then phi=0.0
if n_elements(gtilt) lt 1 then gtilt=0.0
if n_elements(xtilt) lt 1 then xtilt=0.0

; Detector array u,v coords:
; Using 'homogenous' cartesian 4D coordinate system (e.g. Foley and Van Dam)
; Without any scaling, the 4th dimension will remain 1, and can be ignored.

d = di
if size(d, /tname) eq 'POINTER' then d = *d
if n_elements(reorient) lt 1 then reorient=d.reorient
if n_elements(symmetry) lt 1 then symmetry=d.symmetry
if n_elements(mirrorX) lt 1 then mirrorX=d.mirrorX
if n_elements(mirrorY) lt 1 then mirrorY=d.mirrorY

;-----------------------------------------------------------------------------
; Translate, rotate the detector pad coordinates:

t3d, /reset, matrix=matrix

; re-orient detector (in 360/symmetry steps)

if symmetry eq 0 then symmetry=4
delta = (360./symmetry) * (reorient mod symmetry)

t3d, matrix, matrix=matrix, rotate=[0.,0.,-delta]

; global tilt

t3d, matrix, matrix=matrix, rotate=[xtilt,gtilt,0.]

; translate to Z distance

t3d, matrix, matrix=matrix, translate=[0.,0.,distance]

; rotate by theta about Y axis, and phi about the Z axis

t3d, matrix, matrix=matrix, rotate=[0.,theta,0.]
t3d, matrix, matrix=matrix, rotate=[0.,0.,phi]

u = -d.data.x											; -ve because facing in -ve Z direction initially
if mirrorX then u=-u
v = d.data.y
if mirrorY then v=-v
radial = sqrt( u*u + v*v)								; radial distance from centre of array
s = -d.data.z
w = replicate(1.,d.N)
h = [[u],[v],[s],[w]]									; 4D homogenous coordinates

h2 = h # matrix											; apply the transformations
r2 = transpose(h2[*,0:2])								; cartesian coords (ignore 'w' since no scaling)

; transform into spherical coords

p2 = cv_coord( from_rect=r2, /to_sphere, /degrees)		; spherical coords (lat, long, R)
p2[1,*] = 90. - p2[1,*]									; spherical (phi, theta, R)

g = replicate({index:0, theta:0.0, phi:0.0, R:0.0, tilt:0.0, pol:0.0, radial:0.0}, d.N)

g.index = d.data.index
g.phi = reform(p2[0,*])
g.theta = reform(p2[1,*])
g.R = reform(p2[2,*])
g.radial = reform(radial)

;-----------------------------------------------------------------------------
; Calculate dot product with the polarization vector.
; Dot product with 'r2' to form direction cosines

pol = [1.,0.,0.]										; vector @ 90 degrees along polarization
forward = [0.,0.,1.]									; vector in forward direction

cs  = fltarr(d.N)
fs  = fltarr(d.N)
ang  = fltarr(d.N)
sign  = fltarr(d.N)
for i=0L,d.N-1 do begin
	mod2 = sqrt(total(r2[*,i]*r2[*,i]))
	cs[i] = total( r2[*,i]*pol[*]) / mod2				; direction cosince along polarization
	fs[i] = total( r2[*,i]*forward[*]) / mod2			; direction cosine in forward direction
	ang[i] = acos( abs(cs[i])) * 180./!pi				; abs(angle) along polarization
	sign[i] = (fs[i] lt 0) ? 1. : -1.					; is it forward or back-scattered
endfor
g.pol = 90. + sign * ang

;-----------------------------------------------------------------------------
; Locate a point above each detector pad as a normal vector (relative to the pad position)
; Rotate, translate, etc. these as above and calculate normal vectors and hence tilts

t3d, /reset, matrix=matrix

; re-orient detector

t3d, matrix, matrix=matrix, rotate=[0.,0.,-delta]

; global tilt

t3d, matrix, matrix=matrix, rotate=[xtilt,gtilt,0.]

; translate to Z distance

t3d, matrix, matrix=matrix, translate=[0.,0.,distance]

; rotate by theta about Y axis, and phi about the Z axis

t3d, matrix, matrix=matrix, rotate=[0.,theta,0.]
t3d, matrix, matrix=matrix, rotate=[0.,0.,phi]

u = -d.data.x
if mirrorX then u=-u
v = d.data.y
if mirrorY then v=-v
s = -d.data.z

r = sqrt(u*u + v*v)										; tilt is defined as towards centre of array
rp = r - sin(d.data.tilt * !pi/180.)					; offset to end of unit normal vector line
u = u * rp/r
v = v * rp/r
s = s - cos(d.data.tilt * !pi/180.)						; offset to end of unit normal vector line
q = where(abs(r) lt 0.001)
if q[0] ne -1 then begin								; if on beam axis
	u[q] = -sin(d.data[q].tilt * !pi/180.)
	v[q] = 0.0
	s[q] = -d.data[q].z - cos(d.data[q].tilt * !pi/180.)
endif

w = replicate(1.,d.N)
h = [[u],[v],[s],[w]]									; 4D homogenous coordinates

h3 = h # matrix
r3 = transpose(h3[*,0:2])								; cartesian coords of normal vector ends
n3 = r2 - r3											; normal vectors

; Dot product with 'r2' to form direction cosine

cs  = fltarr(d.N)
for i=0L,d.N-1 do begin
	mod2 = sqrt(total(r2[*,i]*r2[*,i]))
	mod3 = sqrt(total(n3[*,i]*n3[*,i]))
	cs[i] = total( r2[*,i]*n3[*,i]) / (mod2 * mod3)
endfor
g.tilt = acos(cs) * 180./!pi

;-----------------------------------------------------------------------------

error = 0
return, g
end
