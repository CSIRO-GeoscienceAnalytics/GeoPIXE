function calc_slices, layer, slice, layer_id=lid, zmajor=zmajor, photo=photo

;	calculate array of slices {N:n, Z:z, F:f, thick:t}
;	based on input array of layers and the base slice thickness.
;
;	'lid' returns the source layer array index for each slice.
;	'zmajor' returns all major elemnt Z.

	if n_elements(photo) lt 1 then photo=0
	n = n_elements(layer)
	if n lt 1 then return, 0

	njump = [1,2,3,4,5,6,8,10,13,14,18,20,22,26,30]
	nstep = [1,2,3,3,3,4,4,5,5,5,6,6,7,7,8]
	jump = intarr(8,15)
	jump[0,0] = 1
	jump[0:1,1] = [1,1]
	jump[0:2,2] = [1,1,1]
	jump[0:2,3] = [1,2,1]
	jump[0:2,4] = [1,3,1]
	jump[0:3,5] = [1,2,2,1]
	jump[0:3,6] = [1,3,3,1]
	jump[0:4,7] = [1,2,4,2,1]
	jump[0:4,8] = [1,3,5,3,1]
	jump[0:4,9] = [1,3,6,3,1]
	jump[0:5,10] = [1,3,5,5,3,1]
	jump[0:5,11] = [1,3,6,6,3,1]
	jump[0:6,12] = [1,2,4,8,4,2,1]
	jump[0:6,13] = [1,3,5,8,5,3,1]
	jump[0:7,14] = [1,2,4,8,8,4,2,1]

	tcum = 0.0							; current thickness
	tmax = photo ? 10000. : 200.		; maximum depth (200 mg/cm^2)

	for i=0L,n-1 do begin
;		thick = layer[i].thick < (tmax - tcum)
		thick = layer[i].thick
		ns = round( thick / slice) > 1
		q = where( njump gt ns)
		if q[0] ne -1 then begin
			ij = q[0]
			n8 = 0
		endif else begin
			ij = 14
			n8 = ceil( (ns-30)/8. )
			i8 = 3
		endelse
		nt = njump[ij] + 8*n8
		s = thick / nt
		ns = nstep[ij] + n8
		t = fltarr(ns)
		if n8 eq 0 then begin
			t[*] = s * float(jump[0:nstep[ij]-1,ij])
		endif else begin
			t[*] = s * 8.0
			t[0:i8] = s * float(jump[0:i8,ij])
			t[ns-i8-1:ns-1] = s * float( jump[nstep[ij]-i8-1:nstep[ij]-1,ij])
		endelse

		l = {layer, N:layer[i].n, Z:layer[i].z, F:layer[i].f, thick:s, name:'Layer '+str_tidy(i)}
		lay = replicate(l,ns)
		lay.thick = t

		if i eq 0 then begin
			slices = lay
			lid = intarr(ns)
		endif else begin
			slices = [slices,lay]
			lid = [lid,replicate(i,ns)]
		endelse
		tcum = tcum+thick
	endfor

	z = fltarr(100)
	z[layer.z] = 1
	z[0] = 0
	zmajor = where(z gt 0)

	return, slices
	end

