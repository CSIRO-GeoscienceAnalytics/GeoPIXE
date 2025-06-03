pro swap_bytes, t, big_endian_data=big, vax_data=vax

; If big_endian_data=1, then swap bytes only if machine is little endian
; If big_endian_data=0, then swap bytes only if machine is big endian
;
; If vax_data=1 then the source data was VAX floating and needs conversion to IEEE.

; Big endian data applies for:		Unix, old Mac, Amiga, ...
; Little endian data applies for:	PC, Linux, VAX/VMS, new x86 Mac, ...

if n_elements(big) lt 1 then big=0
little = 1-big
if n_elements(vax) lt 1 then vax=0

n = n_elements(t)
if (typename(t) eq 'HASH') or (typename(t) eq 'ORDEREDHASH') or (typename(t) eq 'DICTIONARY') then n=1										; parts of Hash treated below
if n lt 1 then return

case var_type(t) of
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
	8:  begin
			for i=0L, n_tags(t)-1 do begin											; Handle structures recursively.
			  temp = t.(i)
			  swap_bytes, temp, big_endian_data=big, vax_data=vax
			  t.(i) = temp
		    endfor
			end
	10: begin
			for i=0L, n-1 do begin													; Handle pointers recursively.
				if ptr_valid( t[i]) then begin
					if n_elements(*t[i]) gt 0 then begin
						temp = *t[i]
						swap_bytes, temp, big_endian_data=big, vax_data=vax
						*t[i] = temp
					endif
				endif
		    endfor
			end
	110: begin
			for i=0L, n-1 do begin													; Handle Lists recursively.
				temp = t[i]
				swap_bytes, temp, big_endian_data=big, vax_data=vax
				t[i] = temp
			endfor
			end	
	111: begin
			key = t.keys()															; Handle Hash recursively.
			for j=0L, n_elements(key)-1 do begin	
				temp = t[key[j]]
				swap_bytes, temp, big_endian_data=big, vax_data=vax
				t[key[j]] = temp
			endfor
			end
	112: begin
			key = t.keys()															; Handle ordered Hash recursively.
			for j=0L, n_elements(key)-1 do begin	
				temp = t[key[j]]
				swap_bytes, temp, big_endian_data=big, vax_data=vax
				t[key[j]] = temp
			endfor
			end
	113: begin
			key = t.keys()															; Handle Dictionary recursively.
			for j=0L, n_elements(key)-1 do begin	
				temp = t[key[j]]
				swap_bytes, temp, big_endian_data=big, vax_data=vax
				t[key[j]] = temp
			endfor
			end
	else:
ENDCASE

return
end

