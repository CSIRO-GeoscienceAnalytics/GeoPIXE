function intersection, box, ray, hit=hit, entry=entry, exit=exit

; Input:
; 	Box		simple rectangular prism shape aligned with x,y,z axes.
;			{ x:[low,high], y:[low,high], z:[low,high] }
;
;	Ray		parametric equation for a line in terms of point p, a unit vector a 
;			and a variable scaler lambda: vector x = p + lambda*a
;			{ p:[x,y,z], a:[x,y,z] }
;
; Returns:
;		hit		1 if the ray hit the prism, 0 if not
;		len		length of intersection through prism, if hit=1
;		entry	entry point, if a hit is registered, if hit=1
;		exit	exit point, if a hit is registered, if hit=1

compile_opt strictarr

	small = 1.0e-6
	dfuzz = 1.0e-5
	if n_elements(box) gt 1 then goto, bad_box
	n = n_elements(ray)
	len = fltarr(n)
	entry = fltarr(3,n)
	exit = fltarr(3,n)
	hiti = bytarr(n)
	hito = bytarr(n)
	bad = bytarr(n)
	point = fltarr(3,n)

;	Need to expand each face a tiny amount to ensure that a ray does not
;	sneak through the corner, due to rounding errors. Based on a fraction
;	of largest dimension of box. e.g. for 1 mm max, get fuzz = 10 um

	fuzz = dfuzz * max([ abs(box.x[1]-box.x[0]), abs(box.y[1]-box.y[0]), abs(box.z[1]-box.z[0]) ])
	
; Look for an intersection on each of the 6 faces ...
; x plane low
	normal = [-1.,0.,0.]
	d = box.x[0]*normal[0]
	dot = ray.a[0]*normal[0] + ray.a[1]*normal[1] + ray.a[2]*normal[2]
	pn = ray.p[0]*normal[0] + ray.p[1]*normal[1] + ray.p[2]*normal[2]
	q = where( (abs(dot) lt small) and (abs(d-pn) lt small),nq)
	if nq gt 0 then bad[q]=1
	q = where( (abs(dot) ge small) ,nq)
	if nq gt 0 then begin
		lambda = (d - pn[q])/dot[q]
		for i=0L,2 do begin
			point[i,0:nq-1] = ray[q].p[i] + lambda * ray[q].a[i]			; intersection points
		endfor
		qinside = where(((point[1,0:nq-1] ge box.y[0]-fuzz) and (point[1,0:nq-1] le box.y[1]+fuzz)) and  $
					((point[2,0:nq-1] ge box.z[0]-fuzz) and (point[2,0:nq-1] le box.z[1]+fuzz)),nqinside)
		if nqinside gt 0 then begin
			q2 = where(dot[q[qinside]] gt 0.0, nq2)				; within plane, exit
			if nq2 gt 0 then begin
				exit[*,q[qinside[q2]]] = point[*,qinside[q2]]
				hito[q[qinside[q2]]] = 1
			endif
			q2 = where(dot[q[qinside]] le 0.0, nq2)				; within plane, entry
			if nq2 gt 0 then begin
				entry[*,q[qinside[q2]]] = point[*,qinside[q2]]
				hiti[q[qinside[q2]]] = 1
			endif
		endif
	endif

; x plane high
	normal = [1.,0.,0.]
	d = box.x[1]*normal[0]
	dot = ray.a[0]*normal[0] + ray.a[1]*normal[1] + ray.a[2]*normal[2]
	pn = ray.p[0]*normal[0] + ray.p[1]*normal[1] + ray.p[2]*normal[2]
	q = where( (abs(dot) lt small) and (abs(d-pn) lt small),nq)
	if nq gt 0 then bad[q]=1
	q = where( (abs(dot) ge small) ,nq)
	if nq gt 0 then begin
		lambda = (d - pn[q])/dot[q]
		for i=0L,2 do begin
			point[i,0:nq-1] = ray[q].p[i] + lambda * ray[q].a[i]			; intersection points
		endfor
		qinside = where(((point[1,0:nq-1] ge box.y[0]-fuzz) and (point[1,0:nq-1] le box.y[1]+fuzz)) and  $
					((point[2,0:nq-1] ge box.z[0]-fuzz) and (point[2,0:nq-1] le box.z[1]+fuzz)),nqinside)
		if nqinside gt 0 then begin
			q2 = where(dot[q[qinside]] gt 0.0, nq2)				; within plane, exit
			if nq2 gt 0 then begin
				exit[*,q[qinside[q2]]] = point[*,qinside[q2]]
				hito[q[qinside[q2]]] = 1
			endif
			q2 = where(dot[q[qinside]] le 0.0, nq2)				; within plane, entry
			if nq2 gt 0 then begin
				entry[*,q[qinside[q2]]] = point[*,qinside[q2]]
				hiti[q[qinside[q2]]] = 1
			endif
		endif
	endif

; y plane low
	normal = [0.,-1.,0.]
	d = box.y[0]*normal[1]
	dot = ray.a[0]*normal[0] + ray.a[1]*normal[1] + ray.a[2]*normal[2]
	pn = ray.p[0]*normal[0] + ray.p[1]*normal[1] + ray.p[2]*normal[2]
	q = where( (abs(dot) lt small) and (abs(d-pn) lt small),nq)
	if nq gt 0 then bad[q]=1
	q = where( (abs(dot) ge small) ,nq)
	if nq gt 0 then begin
		lambda = (d - pn[q])/dot[q]
		for i=0L,2 do begin
			point[i,0:nq-1] = ray[q].p[i] + lambda * ray[q].a[i]			; intersection points
		endfor
		qinside = where(((point[0,0:nq-1] ge box.x[0]-fuzz) and (point[0,0:nq-1] le box.x[1]+fuzz)) and  $
					((point[2,0:nq-1] ge box.z[0]-fuzz) and (point[2,0:nq-1] le box.z[1]+fuzz)),nqinside)
		if nqinside gt 0 then begin
			q2 = where(dot[q[qinside]] gt 0.0, nq2)				; within plane, exit
			if nq2 gt 0 then begin
				exit[*,q[qinside[q2]]] = point[*,qinside[q2]]
				hito[q[qinside[q2]]] = 1
			endif
			q2 = where(dot[q[qinside]] le 0.0, nq2)				; within plane, entry
			if nq2 gt 0 then begin
				entry[*,q[qinside[q2]]] = point[*,qinside[q2]]
				hiti[q[qinside[q2]]] = 1
			endif
		endif
	endif

; y plane high
	normal = [0.,1.,0.]
	d = box.y[1]*normal[1]
	dot = ray.a[0]*normal[0] + ray.a[1]*normal[1] + ray.a[2]*normal[2]
	pn = ray.p[0]*normal[0] + ray.p[1]*normal[1] + ray.p[2]*normal[2]
	q = where( (abs(dot) lt small) and (abs(d-pn) lt small),nq)
	if nq gt 0 then bad[q]=1
	q = where( (abs(dot) ge small) ,nq)
	if nq gt 0 then begin
		lambda = (d - pn[q])/dot[q]
		for i=0L,2 do begin
			point[i,0:nq-1] = ray[q].p[i] + lambda * ray[q].a[i]			; intersection points
		endfor
		qinside = where(((point[0,0:nq-1] ge box.x[0]-fuzz) and (point[0,0:nq-1] le box.x[1]+fuzz)) and  $
					((point[2,0:nq-1] ge box.z[0]-fuzz) and (point[2,0:nq-1] le box.z[1]+fuzz)),nqinside)
		if nqinside gt 0 then begin
			q2 = where(dot[q[qinside]] gt 0.0, nq2)				; within plane, exit
			if nq2 gt 0 then begin
				exit[*,q[qinside[q2]]] = point[*,qinside[q2]]
				hito[q[qinside[q2]]] = 1
			endif
			q2 = where(dot[q[qinside]] le 0.0, nq2)				; within plane, entry
			if nq2 gt 0 then begin
				entry[*,q[qinside[q2]]] = point[*,qinside[q2]]
				hiti[q[qinside[q2]]] = 1
			endif
		endif
	endif

; z plane low
	normal = [0.,0.,-1.]
	d = box.z[0]*normal[2]
	dot = ray.a[0]*normal[0] + ray.a[1]*normal[1] + ray.a[2]*normal[2]
	pn = ray.p[0]*normal[0] + ray.p[1]*normal[1] + ray.p[2]*normal[2]
	q = where( (abs(dot) lt small) and (abs(d-pn) lt small),nq)
	if nq gt 0 then bad[q]=1
	q = where( (abs(dot) ge small) ,nq)
	if nq gt 0 then begin
		lambda = (d - pn[q])/dot[q]
		for i=0L,2 do begin
			point[i,0:nq-1] = ray[q].p[i] + lambda * ray[q].a[i]			; intersection points
		endfor
		qinside = where(((point[0,0:nq-1] ge box.x[0]-fuzz) and (point[0,0:nq-1] le box.x[1]+fuzz)) and  $
					((point[1,0:nq-1] ge box.y[0]-fuzz) and (point[1,0:nq-1] le box.y[1]+fuzz)),nqinside)
		if nqinside gt 0 then begin
			q2 = where(dot[q[qinside]] gt 0.0, nq2)				; within plane, exit
			if nq2 gt 0 then begin
				exit[*,q[qinside[q2]]] = point[*,qinside[q2]]
				hito[q[qinside[q2]]] = 1
			endif
			q2 = where(dot[q[qinside]] le 0.0, nq2)				; within plane, entry
			if nq2 gt 0 then begin
				entry[*,q[qinside[q2]]] = point[*,qinside[q2]]
				hiti[q[qinside[q2]]] = 1
			endif
		endif
	endif

; z plane high
	normal = [0.,0.,1.]
	d = box.z[1]*normal[2]
	dot = ray.a[0]*normal[0] + ray.a[1]*normal[1] + ray.a[2]*normal[2]
	pn = ray.p[0]*normal[0] + ray.p[1]*normal[1] + ray.p[2]*normal[2]
	q = where( (abs(dot) lt small) and (abs(d-pn) lt small),nq)
	if nq gt 0 then bad[q]=1
	q = where( (abs(dot) ge small) ,nq)
	if nq gt 0 then begin
		lambda = (d - pn[q])/dot[q]
		for i=0L,2 do begin
			point[i,0:nq-1] = ray[q].p[i] + lambda * ray[q].a[i]			; intersection points
		endfor
		qinside = where(((point[0,0:nq-1] ge box.x[0]-fuzz) and (point[0,0:nq-1] le box.x[1]+fuzz)) and  $
					((point[1,0:nq-1] ge box.y[0]-fuzz) and (point[1,0:nq-1] le box.y[1]+fuzz)),nqinside)
		if nqinside gt 0 then begin
			q2 = where(dot[q[qinside]] gt 0.0, nq2)				; within plane, exit
			if nq2 gt 0 then begin
				exit[*,q[qinside[q2]]] = point[*,qinside[q2]]
				hito[q[qinside[q2]]] = 1
			endif
			q2 = where(dot[q[qinside]] le 0.0, nq2)				; within plane, entry
			if nq2 gt 0 then begin
				entry[*,q[qinside[q2]]] = point[*,qinside[q2]]
				hiti[q[qinside[q2]]] = 1
			endif
		endif
	endif

	bad_hit = hiti xor hito
;	q3 = where(bad_hit eq 1, nq3)
;	if nq3 gt 0 then begin
;		print,'intersection: ','Bad hit for ',nq3,' rays.'
;	endif
	q3 = where(bad eq 1, nq3)
	if nq3 gt 0 then begin
		print,'intersection: ','Bad parallel rays for ',nq3,' rays.'
	endif
	
	hit = hiti and hito
	q = where(hit eq 1, nq)
	if nq gt 0 then begin
		len[q] = sqrt((entry[0,q]-exit[0,q])^2 + (entry[1,q]-exit[1,q])^2 + (entry[2,q]-exit[2,q])^2)
	endif
	return, len

bad_box:
	warning,'intersection','Bad box: Can have multiple "rays", but only 1 "box".'
	return, len
end
