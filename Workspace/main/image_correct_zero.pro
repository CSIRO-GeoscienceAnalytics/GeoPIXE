pro image_correct_zero, q, pimg, pflux, neighbours=neighbours, verbose=verbose, remain=qz

; Correct "zero" pixels, and replace witb local average.
; Can be applied repeatedly to bridge larger "zero" spaces.
;
;	q		vector of "zero" pixels to correct
;	pimg	pointer to image array
;	pflux	pointer to flux array (null ptr if not used)
;	neighbours number of good 'neighbours' to provide local average (default = 2)
;	qz		returns vector of zeroes that remain
;	/verbose pop up info afterwards

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
		warning,'image_correct_zero',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
nqz = 0L
if n_elements(neighbours) lt 1 then neighbours=2
if n_elements(verbose) lt 1 then verbose=0
if ptr_good( pimg ) eq 0 then return
if n_elements(q) lt 1 then return

	nq = n_elements(q)
	print, 'Before zero correct pass, zero pixels = ',nq
	sx = n_elements( (*pimg)[*,0,0])
	sy = n_elements( (*pimg)[0,*,0])
	n_el = n_elements( (*pimg)[0,0,*])
	mask = bytarr(sx,sy)
	mask[*] = 0
	mask[q] = 1
	
	a = fltarr(sx*sy,3,3)
	m = bytarr(sx*sy,3,3)
	for kx = 0,2 do begin								; shifted mask
		for ky = 0,2 do begin
			m[*,kx,ky] = reform( shift( mask, kx-1,ky-1), sx*sy)
		endfor
	endfor
	
	for j=0L,n_el-1 do begin
		img = (*pimg)[*,*,j]							; image data for this element
	
		for kx = 0,2 do begin							; shifted image
			for ky = 0,2 do begin
				a[*,kx,ky] = reform( shift( img, kx-1,ky-1), sx*sy)
			endfor
		endfor
		
		sum = fltarr(nq)								; for each dud pixel sum all good
		n = intarr(nq)									; nearby pixels to replace it
		for kx = 0,2 do begin
			for ky = 0,2 do begin
				qd = where( m[q,kx,ky] eq 0, nd)
				if nd gt 0 then begin
					sum[qd] = sum[qd] + a[q[qd],kx,ky]	; 
					n[qd] = n[qd] + 1					; 
				endif
			endfor
		endfor
		qn = where(n ge neighbours, nn)
		if nn gt 0 then begin
			img[q[qn]] = sum[qn]/float(n[qn])			; replace duds with average of neighbourhood
			mask[q[qn]] = 0								; pixel fixed
		endif
		
		(*pimg)[*,*,j] = img	
	endfor
	qz = where( mask eq 1, nqz)
	print, 'After zero correct pass, zero pixels remain = ',nqz
	
	if ptr_good( pflux) then begin
	
		img = (*pflux)[*,*]								; image data for flux map
		if (sx eq n_elements(img[*,0])) and (sy eq n_elements(img[0,*])) then begin
		
			for kx = 0,2 do begin						; shifted images and mask
				for ky = 0,2 do begin
					a[*,kx,ky] = reform( shift( img, kx-1,ky-1), sx*sy)
				endfor
			endfor
			
			sum = fltarr(nq)							; for each dud pixel sum all good
			n = intarr(nq)								; nearby pixels to replace it
			for kx = 0,2 do begin
				for ky = 0,2 do begin
					qd = where( m[q,kx,ky] eq 0, nd)
					if nd gt 0 then begin
						sum[qd] = sum[qd] + a[q[qd],kx,ky]
						n[qd] = n[qd] + 1
					endif
				endfor
			endfor
			qn = where(n ge 5, nn)
			if nn gt 0 then begin
				img[q[qn]] = sum[qn]/float(n[qn])		; replace duds with average of neighbourhood
			endif
		
			(*pflux)[*,*] = img
		endif
	endif
	
	if verbose then warning,/info,'image_correct_zero',['Number of "zeros" to correct = '+str_tidy(nq),'Number of "zeros" remaining = '+str_tidy(nqz)]
	return
end
