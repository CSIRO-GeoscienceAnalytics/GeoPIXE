	function inumeric, stri, strict=strict

; Is 'str' a valid integer numeric string, with optional sign?
; If not /strict, then allow trailing modifier byte-length type characters.
;
; Chris Ryan (CSIRO), 2008, revised 2012
	
	if n_elements(strict) lt 1 then strict=0	
	good = bytarr(256)
	if strict eq 0 then begin
		good[85] = 1				; U
		good[117] = 1				; u
		good[76] = 1				; L
		good[108] = 1				; l
		good[83] = 1				; S
		good[115] = 1				; s
		good[66] = 1				; B
		good[98] = 1				; b
	endif
	
	n = n_elements(stri)
	if n eq 0 then return, 0

	bad = intarr(n)
	for i=0L,n-1 do begin
		str = strtrim(stri[i],2)
		ns = strlen(str)
		
		if ns ge 1 then begin											; ns>=1 for a numeral
			b = byte(str)
			
			if ns ge 2 then begin										; ns>1 for a sign (no lone signs)
				if (b[0] eq 43B) or (b[0] eq 45B) then j=1 else j=0		; sign
				k = ns-1
				
				if strict eq 0 then begin
					if k ge j+1 then begin								; ns>j+1 for a modifier
						if good[b[k]] then begin						; trailing modifier (last char)
							k--	
							if k ge j+1 then begin
								if good[b[k]] then k--					; trailing modifier (2nd last char)
							endif
						endif
					endif
				endif
				
				bad[i] = 1 - pnumeric( string(b[j:k]))
				
			endif else bad[i] = 1 - pnumeric( string(b[0]))
			
		endif else bad[i]=1
	endfor
	return, 1 - bad
end
