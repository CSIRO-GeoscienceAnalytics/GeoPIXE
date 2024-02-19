function bit_jam, bi, reverse=reverse

; Reform the byte array of '0' and '1' values into a digital word.
; /reverse  reverse the bit order
;
; Complements 'bit_split' function

if n_elements(reverse) lt 1 then reverse=0

b = (reverse ? bi : reverse(bi))
n = n_elements(b)

x = ulong64( b[0])

for i=1L,n-1 do begin
	x = ishft(x,1) or (b[i] and 1)
endfor

case n of
	8: begin
		x = byte(x)
		end
	16: begin
		x = uint(x)
		end
	32: begin
		x = ulong(x)
		end
	else:
endcase
return, x
end
