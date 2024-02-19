	function pnumeric, str, hex=hex

; Is 'str' a pure integer numeral string with no sign?
; Can't have any sign, modifier (byte length), white-space, tabs, etc. either
; /hex	hexadecimal allowed
;
; Chris Ryan (CSIRO),  2012

if n_elements(hex) eq 0 then hex=0

	good = bytarr(256)
	good[48:57] = 1							; 0-9
	if hex then begin
		good[97:102] = 1					; a-f
		good[65:70] = 1						; A-F
	endif
	
	n = n_elements(str)
	if n eq 0 then return, 0
	bad = intarr(n)
	
	for i=0L,n-1 do begin
		b = byte(str[i])
		q = where( good[b] eq 0, nq)
		if nq gt 0 then bad[i]=1
	endfor

	return, 1 - bad
end
