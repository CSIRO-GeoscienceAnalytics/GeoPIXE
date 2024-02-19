function norm_to_data, v, x=x, y=y

; Convert norm coordinates to data coordinates
; /X for X axis, /Y for Y axis.

if n_elements(x) lt 1 then x=0
if n_elements(y) lt 1 then y=0

v2 = float(v)
if y then begin
	v2 = (v2 - !y.s[0]) / !y.s[1]
	if !y.type eq 1 then v2 = 10.0^v2
	return, v2
endif

v2 = (v2 - !x.s[0]) / !x.s[1]
if !x.type eq 1 then v2 = 10.0^v2
return, v2
end
