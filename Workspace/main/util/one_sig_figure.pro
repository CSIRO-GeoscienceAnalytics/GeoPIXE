function one_sig_fig2, x, nice=nice, positive=positive

; Return one significant figure only (positive or negative)
; If /nice, then return one of these figures 1,2,5
; If /positive, then only return positive values

	if n_elements(nice) lt 1 then nice=0
	if n_elements(positive) lt 1 then positive=0
	if abs(x) lt 1.0e-20 then return, 0.0
	neg = 0
	if x lt 0.0 then neg=1
	t = abs(x)
	xlast = 0.0
	n = 0
	it = round(t)
	up = 1
	if t lt 1.0 then begin
		up = 0
		t = t*10.
		n = 0
	endif

more:
	it = round(t)
	if up and (t ge 1.0) then begin
		t = t/10.
		n = n+1
		xlast = it
		goto, more
	endif else if (up eq 0) and (it eq 0) then begin
		t = t*10.
		n = n-1
		goto, more
	endif else if (up eq 0) and (it ne 0) then begin
		xlast = it
	endif

;	if nice then begin
;		xnice = [1,2,5,10,20]
;		q = where( xlast le xnice)
;		if q[0] ne -1 then xlast = xnice[q[0]]
;	endif
	if nice then begin
		xnice = [1,2,5,10,20]
		q = where( xnice le xlast, nq)
		if q[0] ne -1 then xlast = xnice[q[nq-1]]
	endif

	r = xlast * 10.^(n-1)
	if neg and (positive eq 0) then r=-r
	return, r
end

;-----------------------------------------------------

function one_sig_figure, x, nice=nice, positive=positive

; Return one significant figure only (positive or negative)
; If /nice, then return one of these figures 1,2,5
; If /positive, then only return positive values

	if n_elements(nice) lt 1 then nice=0
	if n_elements(positive) lt 1 then positive=0

	n = n_elements(x)
	if n lt 1 then return, 0.0
	result = fltarr(n)

	for i=0L,n-1 do begin
		result[i] = one_sig_fig2( x[i], nice=nice, positive=positive)
	endfor

	if n eq 1 then result=result[0]
	return, result
end
