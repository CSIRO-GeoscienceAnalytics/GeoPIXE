function bit_split, xi, reverse=rev

; Form a byte array of the bits in 'xi' starting from the LSB,
;	unless /reverse is set
;
; Complements 'bit_jam' function

COMPILE_OPT STRICTARR
if n_elements(rev) lt 1 then rev=0

bits = [0,8,16,32,32,64,64,0,0,128,0,0,16,32,64,64]
s = size(xi)
n = bits[s[n_elements(s)-2]]
if n eq 0 then return, 0

case n of
	8: begin
		x = byte(xi)
		b = bytarr(8)
		hi = -1B
		end
	16: begin
		x = uint(xi)
		b = bytarr(16)
		hi = -1US
		end
	32: begin
		x = ulong(xi)
		b = bytarr(32)
		hi = -1UL
		end
	64: begin
		x = ulong64(xi)
		b = bytarr(64)
		hi = -1ULL
		end
endcase
if x eq hi then begin
	b[*] = 1
	goto, done
endif

for i=0L,n-1 do begin
	b[i] = x and 1
	x = ishft(x,-1)
endfor

done:
	if rev then b=reverse(b)
	return, b
end

