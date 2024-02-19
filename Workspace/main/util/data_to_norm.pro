function data_to_norm, v, x=x, y=y

; Convert data coordinates to norm coordinates
; /X for X axis, /Y for Y axis.

if n_elements(x) lt 1 then x=0
if n_elements(y) lt 1 then y=0

v2 = v
if y then begin
	if !y.type eq 1 then v2 = alog10(v)
	return, !y.s[0] + !y.s[1]*v2
endif

if !x.type eq 1 then v2 = alog10(v)
return, !x.s[0] + !x.s[1]*v2
end
