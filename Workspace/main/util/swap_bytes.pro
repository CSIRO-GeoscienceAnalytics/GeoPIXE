pro swap_bytes, t, big_endian_data=big, vax_data=vax

; If big_endian_data=1, then swap bytes only if machine is little endian
; If big_endian_data=0, then swap bytes only if machine is big endian
;
; If vax_data=1 then the source data was VAX floating and needs conversion to IEEE.

; Big endian data applies for:		Unix, Mac, ...
; Little endian data applies for:	PC, Linux, VAX/VMS, ...

if n_elements(t) lt 1 then return
if n_elements(big) lt 1 then big=0
little = 1-big
if n_elements(vax) lt 1 then vax=0

case (size(t, /TYPE)) of
	1:  return			 															; BYTES require no swapping.
	2:  byteorder,swap_if_little_endian=big, swap_if_big_endian=little, t, /SSWAP   ; 16-bit signed integers
	12: byteorder,swap_if_little_endian=big, swap_if_big_endian=little, t, /SSWAP   ; Unsigned shorts
	3:  byteorder,swap_if_little_endian=big, swap_if_big_endian=little, t, /LSWAP   ; 32-bit signed long integers
	13: byteorder,swap_if_little_endian=big, swap_if_big_endian=little, t, /LSWAP   ; Unsigned Longs
	14: byteorder,swap_if_little_endian=big, swap_if_big_endian=little, t, /L64SWAP ; Signed 64-bit integers
	15: byteorder,swap_if_little_endian=big, swap_if_big_endian=little, t, /L64SWAP ; Unsigned 64-bit integers
	4:  begin
		byteorder,swap_if_little_endian=big, swap_if_big_endian=little, t, /LSWAP   ; Single floats
		if vax then byteorder, t, /VAXtoF
		end
	5:  begin
		byteorder,swap_if_little_endian=big, swap_if_big_endian=little, t, /L64SWAP ; Double floats
		if vax then byteorder, t, /VAXtoD
		end
	6:  begin
		byteorder,swap_if_little_endian=big, swap_if_big_endian=little, t, /LSWAP   ; Single complex
		if vax then byteorder, t, /VAXtoF
		end
	9:  begin
		byteorder,swap_if_little_endian=big, swap_if_big_endian=little, t, /L64SWAP ; Double complex
		if vax then byteorder, t, /VAXtoD
		end
	7:  return																		; Strings require no swapping
	8:  for i=0L, n_tags(t)-1 do begin												; Handle structures recursively.
		  temp = t.(i)
		  swap_bytes, temp, big_endian_data=big, vax_data=vax
		  t.(i) = temp
	    endfor
	10:  for i=0L, n_elements(t)-1 do begin											; Handle pointers recursively.
			if ptr_valid( t[i]) then begin
				if n_elements(*t[i]) gt 0 then begin
					temp = *t[i]
					swap_bytes, temp, big_endian_data=big, vax_data=vax
					*t[i] = temp
				endif
			endif
	    endfor
	else:
ENDCASE

return
end

