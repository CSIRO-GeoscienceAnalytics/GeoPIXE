function finite_image, p, mean=av, zero=zero, norm=norm, ninf=ninf

; Test multi-dimensional image for non-finite pixels.
; Replace these with:
;	/mean	mean value in each plane
;	/zero	zero
;	/norm	normalize resulting images in each XY pixel (for dim>=3)
;	ninf	returns number of NaN pixels

COMPILE_OPT STRICTARR

	if n_elements(av) eq 0 then av=0
	if n_elements(zero) eq 0 then zero=1
	if n_elements(norm) eq 0 then norm=0
	if av then zero=0
	mode = 'mean'
	if zero then mode = 'zero'

	if ptr_good( p) then begin
		img = *p
	endif else img=p
	ninf = 0LL
	small = 1.0e-10
	
	nd = size( img, /n_dimensions)
	case nd of
		1: begin
			q = where( finite( img[*]) eq 0, nq)
			if nq gt 0 then begin
				case mode of
					'mean': img[q] = mean( img[*], /NaN)
					'zero': img[q] = 0.
					else:
				endcase
				ninf = ninf+nq
			endif
			end
		2: begin
			q = where( finite( img[*]) eq 0, nq)
			if nq gt 0 then begin
				case mode of
					'mean': img[q] = mean( img[*], /NaN)
					'zero': img[q] = 0.
					else:
				endcase
				ninf = ninf+nq
			endif
			end
		3: begin
			nxy = n_elements(img[*,0,0]) * n_elements(img[0,*,0])
			n = n_elements(img[0,0,*])
			if norm then begin
				tot = reform(img[*,*,0])
				tot[*] = 0.
			endif
		
			for i=0,n-1 do begin
				q = where( finite( img[*,*,i]) eq 0, nq)
				if nq gt 0 then begin
					case mode of
						'mean': img[q + i*nxy] = mean( img[*,*,i], /NaN)
						'zero': img[q + i*nxy] = 0.
						else:
					endcase
					ninf = ninf+nq
				endif
				if norm then tot = tot + img[*,*,i]
			endfor
			if norm then begin
				q1 = where( tot ge small, nq1, complement=q2, ncomplement=nq2)
				for i=0,n-1 do begin
					if nq1 ge 1 then begin
						img[q1 + i*nxy] = img[q1 + i*nxy] / tot[q1]
					endif
					if nq2 ge 1 then begin
						img[q2 + i*nxy] = 0.
						ninf = ninf+nq2
					endif
				endfor
			endif
			end
		4: begin
			nxy = n_elements(img[*,0,0,0]) * n_elements(img[0,*,0,0])
			nxyz = nxy * n_elements(img[0,0,*,0])
			n = n_elements(img[0,0,0,*])
			nz = n_elements(img[0,0,*,0])
			if norm then begin
				tot = reform(img[*,*,0,0])
				tot[*] = 0.
			endif
		
			for i=0,n-1 do begin
				for j=0,nz-1 do begin
					q = where( finite( img[*,*,j,i]) eq 0, nq)
					if nq gt 0 then begin
						case mode of
							'mean': img[q + j*nxy + i*nxyz] = mean( img[*,*,j,i], /NaN)
							'zero': img[q + j*nxy + i*nxyz] = 0.
							else:
						endcase
						ninf = ninf+nq
					endif
					if norm then tot = tot + img[*,*,i,j]
				endfor
			endfor
			if norm then begin
				q1 = where( tot ge small, nq1, complement=q2, ncomplement=nq2)
				for i=0,n-1 do begin
					for j=0,nz-1 do begin
						if nq1 ge 1 then begin
							img[q1 + j*nxy + i*nxyz] = img[q1 + j*nxy + i*nxyz] / tot[q1]
						endif
						if nq2 ge 1 then begin
							img[q1 + j*nxy + i*nxyz] = 0.
							ninf = ninf+nq2
						endif
					endfor
				endfor
			endif
			end
		else:
	endcase
	
	return, img
end

		
