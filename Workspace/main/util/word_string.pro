function word_string, w

; Convert word (int 16 bit) array to string.
; Any byte swapping has already been done, which
; means that it is in current platform order -
; i.e. first char in low byte (PC, VAX/VMS),
; or first char in high byte (MAC, UNix).

case !version.os_family of
	'Windows': low_first = 1
	'MacOS': low_first = 0
	'UNIX': low_first = 0
	else: low_first = 0
endcase

n = n_elements(w)
if n lt 1 then return, ''

b = bytarr(n*2)
j = 0
for i=0L,n-1 do begin

	if low_first then begin
		first = w[i] and '00FF'xu
		second = (w[i] and 'FF00'xu) / 256
	endif else begin
		second = w[i] and '00FF'xu
		first = (w[i] and 'FF00'xu) / 256
	endelse

	b[j] = byte(first)
	j = j+1
	b[j] = byte(second)
	j = j+1
endfor

return, string(b)
end


