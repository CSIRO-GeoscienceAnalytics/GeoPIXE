pro makeCylinder, verts, connectivity, scale=scale, offset=offset, sX=width, sY=height, sZ=depth, $
			rX=rX, rY=rY, rZ=rZ

; Creates the vertices and faces on a unit cylinder.
; Rotate unit cylinder by rX, rY, rZ if needed.
; Then scale globally with 'scale' or on axes using sX, sY, sZ.
; Finally, offset using 'offset'

;vertices in unit coordinates

if n_elements(mirror) lt 1 then mirror=0

_points = [ $
		[0.000000, 0.000000, 0.500000], $
		[0.000000, 0.500000, 0.500000], $
		[0.000000, 0.500000, -0.500000], $
		[0.000000, 0.000000, -0.500000] ]

; NOTE: p4, p5 are very important for setting direction of polygons, and hence
;		which side is 'inside' versus 'outside'.

_nfacets = 72
	_p4 = 6.28319
	_p5 = 0.000000

MESH_OBJ, 6, verts, connectivity, _points, P1=_nfacets, P4=_p4, P5=_p5

if keyword_set(rX) or keyword_set(rY) or keyword_set(rZ) then begin
	t3d, /reset, matrix=matrix
	if keyword_set(rX) then t3d, matrix, matrix=matrix, rotate=[rX,0.,0.]
	if keyword_set(rY) then t3d, matrix, matrix=matrix, rotate=[0.,rY,0.]
	if keyword_set(rZ) then t3d, matrix, matrix=matrix, rotate=[0.,0.,rZ]

	n = n_elements(verts[0,*])
	h = fltarr(n,4)
	h[*,0:2] = transpose(verts)
	h[*,3] = 1.
	h2 = h # matrix
	verts = transpose( h2[*,0:2])
endif

;if the scale keyword is set then use it.
if keyword_set(scale) then verts = verts * scale

if keyword_set(width) then verts[0,*] = verts[0,*] * width
if keyword_set(height) then verts[1,*] = verts[1,*] * height
if keyword_set(depth) then verts[2,*] = verts[2,*] * depth

;if the offset keyword is present then apply it
if n_elements(offset) ne 0 then verts=verts + offset

return
end