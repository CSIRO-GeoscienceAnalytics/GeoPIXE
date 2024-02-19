function Elliptic_Ei, x

gamma = 0.5772156649015

n = n_elements(x)
if n lt 1 then return, 0.0

term = replicate(1.0D0,n)
sum = gamma + alog(x)

for n=1L,50 do begin
	term = term * x/n
	sum = sum + term/n
endfor

return, sum
end

;-------------------------------------------------------------------

function Elliptical_1, x, noneg=noneg

;	Elliptical integral of first kind.
;	Analytical approximation.

A = [-0.57721566, 0.99999193, -0.24991055, 0.05519968, -0.00976004, 0.00107857]
B = [0.2677737343, 8.6347608925, 18.0590169730, 8.5733287401]
C = [3.9584969228, 21.0996530827, 25.6329561486, 9.5733223454]

if n_elements(noneg) lt 1 then noneg=0

n = n_elements(x)
if n lt 1 then return, 0.0
e1 = dblarr(n)									; for x=0.0, return 0.0
												; not strickly true, but doesn't blow up.
if noneg eq 0 then begin
	q = where(x lt 0.0)
	if q[0] ne -1 then begin						; shouldn't be negative now.
		e1[q] = -Elliptic_Ei(-x[q])
	endif
endif

q = where( (x le 1.0) and (x gt 0.0))
if q[0] ne -1 then begin
	sum = 0.0D0
	for i=6,1,-1 do begin
		sum = x[q] * sum + A[i-1]
	endfor
	e1[q] = sum - alog(x[q])
endif

q = where(x gt 1.0)
if q[0] ne -1 then begin
	sum = 1.0D0
	sum1 = 1.0D0
	for i=4,1,-1 do begin
		sum = x[q] * sum + B[i-1]
		sum1 = x[q] * sum1 + C[i-1]
	endfor
	e1[q] = exp(-x[q]) * (sum/sum1)/x[q]
endif

return, e1
end

