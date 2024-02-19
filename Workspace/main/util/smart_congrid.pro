function smart_congrid, d, xi,yi,zi, interp=interp, minus_one=minus_one, center=center, $
						factor=f, compress=compress, _extra=extra

; Resize the array 'd' to new dimensions x,y,z
; When shrinking, use rebin to average pixels
; When growing, use congrid to be more general /interp.
; Return 'f' as the final length/area/volume scale factor.
; Use this to scale integrated image quantities (e.g. counts).
;
; Congrid is good for enlargening an image, but tends to loose
; info (sample) on compression. 
; 
; If the original dimesions are multiples of the final dimensions,
; then we can use rebin, which averages the merged pixels together.
; 
; If the final dimensions are smaller than the originals, but the
; original dimesions are NOT multiples of the final dimensions, then
; we use rebin down to the next smaller size and congrid back up to
; the final size, unless the original size is close to a multiple of
; the final size, in which case we use rebin to a close size and fill.
;
; The defaults of center=1, minus_one=0 suit image interpolation to
; to preserve position of features to match the un-interp display.
;
; compress	vector of compression factors (integer) for each axis
;			if this is provided, then the x1,yi,zi are not needed.
; factor	returns the area factor - ratio of original to final area.

COMPILE_OPT STRICTARR
if n_elements(interp) lt 1 then interp=1
if n_elements(center) lt 1 then center=1
if n_elements(minus_one) lt 1 then minus_one=0
if n_elements(d) lt 1 then return, 0
s = size(d)
dims = s[0]
tol = 0.005
debug = 0
f = 1.
case dims of
	0: begin
		return, d
		end
	1: begin
		if n_elements(xi) lt 1 then begin
			if n_elements(compress) ge 1 then begin
				xi = n_elements(d[*]) / compress[0]
			endif else goto, bad
		endif
		nx = s[1]
		x = long(xi) > 1
		if (x le nx) then begin
			fx = long(nx/float(x))
			x2 = nx/fx
			if debug then print,'smart_congrid: original rx =',float(x-x2)/float(x),'   x2=',x2,'    tol=',tol 
			if (float(x2-x)/float(x) lt tol) then begin
				nx = fx*x2
				if debug then print,'smart_congrid 1: original ',s[1],'     rebin from ',nx,' to ',x2 
				out = replicate(d[0],x)
				t = rebin( d[0:nx-1], x2)
				out = t[0:x-1]
			endif else begin
				fx = ceil(nx/float(x))
				x2 = nx/fx
				nx = fx*x2
				if debug then print,'smart_congrid 2: original ',s[1],'     rebin from ',nx,' to ',x2 
				out = rebin( d[0:nx-1], x2)
				if (x2 ne x) then begin
					out = congrid(out, x, interp=interp, minus_one=minus_one, center=center, _extra=extra)
				endif
			endelse
		endif else begin
			out = congrid(d, x, interp=interp, minus_one=minus_one, center=center, _extra=extra)
		endelse
		f = float(nx)/float(x)
		end
	2: begin
		if n_elements(xi) lt 1 then begin
			if n_elements(compress) ge 1 then begin
				xi = n_elements(d[*,0]) / compress[0]
			endif else goto, bad
		endif
		if n_elements(yi) lt 1 then begin
			if n_elements(compress) ge 2 then begin
				yi = n_elements(d[0,*]) / compress[1]
			endif else goto, bad
		endif
		nx = s[1]
		ny = s[2]
		x = long(xi) > 1
		y = long(yi) > 1
		if (x le nx) and (y le ny) then begin
			fx = long(nx/float(x))
			x2 = nx/fx
			fy = long(ny/float(y))
			y2 = ny/fy
			if debug then print,'smart_congrid: original rx,ry =',float(x-x2)/float(x),float(y-y2)/float(y),'   x2,y2=',x2,y2,'    tol=',tol 
			if (float(x2-x)/float(x) lt tol) and (float(y2-y)/float(y) lt tol) then begin
				nx = fx*x2
				ny = fy*y2
				if debug then print,'smart_congrid 1: original ',s[1],s[2],'     rebin from ',nx,ny,' to ',x2,y2 
				out = replicate(d[0],x,y)
				t = rebin( d[0:nx-1,0:ny-1], x2,y2)
				out = t[0:x-1,0:y-1]
			endif else begin
				fx = ceil(nx/float(x))
				x2 = nx/fx
				nx = fx*x2
				fy = ceil(ny/float(y))
				y2 = ny/fy
				ny = fy*y2
				if debug then print,'smart_congrid 2: original ',s[1],s[2],'     rebin from ',nx,ny,' to ',x2,y2 
				out = rebin( d[0:nx-1,0:ny-1], x2,y2)
				if (x2 ne x) or (y2 ne y) then begin
					if debug then print,'smart_congrid 2: original ',s[1],s[2],'     congrid from ',x2,y2,' to ',x,y 
					out = congrid(out, x,y, interp=interp, minus_one=minus_one, center=center, _extra=extra)
				endif
			endelse
		endif else begin
			out = congrid(d, x,y, interp=interp, minus_one=minus_one, center=center, _extra=extra)
		endelse
		f = float(nx)*float(ny)/(float(x)*float(y))
		end
	3: begin
		if n_elements(xi) lt 1 then begin
			if n_elements(compress) ge 1 then begin
				xi = n_elements(d[*,0,0]) / compress[0]
			endif else goto, bad
		endif
		if n_elements(yi) lt 1 then begin
			if n_elements(compress) ge 2 then begin
				yi = n_elements(d[0,*,0]) / compress[1]
			endif else goto, bad
		endif
		if n_elements(zi) lt 1 then begin
			if n_elements(compress) ge 3 then begin
				zi = n_elements(d[0,0,*]) / compress[2]
			endif else goto, bad
		endif
		nx = s[1]
		ny = s[2]
		nz = s[3]
		x = long(xi) > 1
		y = long(yi) > 1
		z = long(zi) > 1
		if (x le nx) and (y le ny) and (z le nz) then begin
			fx = long(nx/float(x))
			x2 = nx/fx
			fy = long(ny/float(y))
			y2 = ny/fy
			fz = long(nz/float(z))
			z2 = nz/fz
			if debug then print,'smart_congrid: original rx,ry,rz =',float(x-x2)/float(x),float(y-y2)/float(y),float(z-z2)/float(z),'   x2,y2,z2=',x2,y2,z2,'    tol=',tol 
			if (float(x2-x)/float(x) lt tol) and (float(y2-y)/float(y) lt tol) and (float(z2-z)/float(z) lt tol) then begin
				nx = fx*x2
				ny = fy*y2
				nz = fz*z2
				if debug then print,'smart_congrid 1: original ',s[1],s[2],s[3],'     rebin from ',nx,ny,nz,' to ',x2,y2,z2
				out = replicate(d[0],x,y,z)
				t = rebin( d[0:nx-1,0:ny-1,0:nz-1], x2,y2,z2)
				out = t[0:x-1,0:y-1,0:z-1]
			endif else begin
				fx = ceil(nx/float(x))
				x2 = nx/fx
				nx = fx*x2
				fy = ceil(ny/float(y))
				y2 = ny/fy
				ny = fy*y2
				fz = ceil(nz/float(z))
				z2 = nz/fz
				nz = fz*z2
				if debug then print,'smart_congrid 2: original ',s[1],s[2],s[3],'     rebin from ',nx,ny,nz,' to ',x2,y2,z2
				out = rebin( d[0:nx-1,0:ny-1,0:nz-1], x2,y2,z2)
				if (x2 ne x) or (y2 ne y) or (z2 ne z) then begin
					if debug then print,'smart_congrid 2: original ',s[1],s[2],s[3],'     congrid from ',x2,y2,z2,' to ',x,y,z
					out = congrid(out, x,y,z, interp=interp, minus_one=minus_one, center=center, _extra=extra)
				endif
			endelse
		endif else begin
			out = congrid(d, x,y,z, interp=interp, minus_one=minus_one, center=center, _extra=extra)
		endelse
		f = float(nx)*float(ny)*float(nz)/(float(x)*float(y)*float(z))
		end
	else: return, d
endcase
return, out

bad:
	warning,'smart_congrid','bad or missing arguments.'
	return, d
	end
