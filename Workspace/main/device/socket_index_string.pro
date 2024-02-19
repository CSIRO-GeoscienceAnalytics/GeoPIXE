function socket_index_string, ki, nrun=nrun

; Encode a list of indices into a string for Miro type command line
;	1,2,3  3-7  *
;
; "-1" returns "*", unless nrun is present and then it returns "0-[nrun-1]"
; 
;  1,4,5,6,9
;  0 1 2 3 4 5 6 7 8 9 10
;  
;  0 1 0 0 1 1 1 0 0 1 0 	    mask --> x
;  0 0 1 0 0 1 1 1 0 0 1 		shift --> sx
;  0 1 1 0 1 0 0 1 0 1 1		x xor sx --> xsx
;
;  0 1 0 0 1 0 0 0 0 1 0 		xsx and x --> y
;  0 0 1 0 0 0 0 1 0 0 1 		xsx and sx --> z

if n_elements(ki) lt 1 then return,''
all = '*'
if n_elements(nrun) gt 0 then begin
	if nrun le 0 then begin
		warning,'socket_index_string','"nrun" is zero.'
	endif else if nrun eq 1 then begin
		all = '0'
	endif else begin
		all = '0-'+str_tidy(nrun-1)
	endelse
endif
if n_elements(ki) eq 1 then begin
	if ki eq -1 then begin
		return, all
	endif else begin
		return, str_tidy(ki)
	endelse
endif
q = where(ki eq -1, nq)
if nq ge 1 then return, all
;q = where((ki ge 0) and (ki lt 32), nq)
;if nq lt 1 then return,''
;k = ki[q]
k = ki

n = max(k)+2
mask = bytarr(n)
mask[k] = 1

x = mask
sx = shift(x,1)
xsx = x xor sx

y = xsx and x
z = xsx and sx
z[n-1] = 1

qy = where(y eq 1, ny)
;if ny ...
qz = where(z eq 1, nz)
;if nz ...

s = str_tidy(qy) + '-' + str_tidy(qz-1) 

q = where(qy eq qz-1, nq)
if nq ge 1 then s[q] = str_tidy(qy[q])
return, strjoin(s,',')
end