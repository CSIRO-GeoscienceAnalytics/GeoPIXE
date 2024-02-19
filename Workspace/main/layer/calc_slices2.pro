function calc_slices2, layer, slice, layer_id=lid, zmajor=zmajor, photo=photo, error=error

;	calculate array of slices {N:n, Z:z, F:f, thick:t}
;	based on input array of layers and the base slice thickness.
;
;	'lid' returns the source layer array index for each slice.
;	'zmajor' returns all major elemnt Z.

	COMPILE_OPT STRICTARR
	ErrorNo = 0
	common c_errors_1, catch_errors_on
	if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
	if catch_errors_on then begin
		Catch, ErrorNo
		if (ErrorNo ne 0) then begin
			Catch, /cancel
			on_error, 1
			help, calls = s
			n = n_elements(s)
			c = 'Call stack: '
			if n gt 2 then c = [c, s[1:n-2]]
			warning,'calc_slices2',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			error = 1
			return, 0
		endif
	endif
	error = 1
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

	tbig = (50. * slice) < 5.				; set largest slice for PIXE to 100*slice < 5 mg/cm^2

	tmax = photo ? 500. : tbig				; largest slice (5 mg/cm^2) for PIXE   *** 15/3/19 CGR
;	tmax = 200.								; was too coarse (calc_slice.pro) for PIXE 'slow_beam'
	slices = replicate(layer[0],10000)
	lid = intarr(10000)
	
	k = 0L
	for i=0L,n-1 do begin
		thick = layer[i].thick
		lay = layer[i]
		lay.name = 'Layer '+str_tidy(i)
		if thick le slice then begin						; thin layer
			slices[k] = lay
			lid[k] = i
			k = k+1
			continue			;break
		endif
		
		nf = floor( thick / slice)
		if nf lt 1 then begin
			print,'Debug ...'
		endif
		q = max( where( njump le nf, nq))
		if nq eq 0 then break

		nj = njump[q[0]]									; add sequence to slice list
		ns = nstep[q[0]]
		l = replicate(lay, ns)
		t = slice * float(jump[0:ns-1,q[0]])
		l.thick = t
		
		delta = thick - float(nj)*slice
		if delta le slice then begin						; sequence completes layer
			l[ns/2].thick = l[ns/2].thick + delta 			; add in any thickness left over
			slices[k:k+ns-1] = l
			lid[k:k+ns-1] = i
			k = k+ns
			continue			;break
 		endif
		
		lay1 = l[0:ns/2-1]
		lay2 = l[ns/2:*]
		nf = 2L * jump[ns/2,q[0]]
		if nf lt 1 then begin
			print,'Debug ...'
		endif
		while (delta ge 2*nf*slice) do begin
			l = lay											; continue to add (doubling) slices
			l.thick = nf*slice								; until not enough remains
			
			lay1 = [lay1,l]									; add new slices in middle
			lay2 = [l,lay2]
			delta = delta - 2*nf*slice
			if (2*nf*slice le tmax) then nf = nf*2L
		endwhile
		l = [lay,lay]												
		l.thick = delta/2.									; add any remnant
		ns = n_elements(lay1) + n_elements(lay2) + 2		
		slices[k:k+ns-1] = [lay1,l,lay2]
		lid[k:k+ns-1] = i
		k = k+ns
	endfor
	slices = slices[0:k-1]
	lid = lid[0:k-1]

	z = fltarr(100)
	z[layer.z] = 1
	z[0] = 0
	zmajor = where(z gt 0)

	error = 0
	return, slices
end


		 
