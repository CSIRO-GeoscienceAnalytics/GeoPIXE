function gnumeric, stri

; Is 'str' a valid G-floating numeric string?
;
; Chris Ryan (CSIRO), 2008, revised 2012

	n = n_elements(stri)
	if n eq 0 then return, 0
	
	bad = intarr(n)
	for i=0L,n-1 do begin
		str = strtrim(stri[i],2)
	
		if (strlowcase(str) ne 'nan') and (strlowcase(str) ne '-nan') and (strlowcase(str) ne 'infinity') $
									and (strlowcase(str) ne '-infinity') then begin 
			b = byte(str)
			ns = strlen(str)
			case ns of
				0: bad[i] = 1													; blank
				1: bad[i] = 1 - fnumeric(str)									; one char only
				else: begin
					q = where( (b eq 69B) or (b eq 101B) or $					; found any "E" or "e"
										(b eq 68B) or (b eq 100B), nq)			;  or any "D" or "d"
					case nq of
						0: bad[i] = 1 - fnumeric(str)							; simple fnumeric
						1: begin												; one "E" only
							case q[0] of	
								0: begin										; "E" at the start
									bad[i] = 1									; not legal
									end
								(ns-1): begin									; "E" at the end
									bad[i] = 1									; not legal
									end
								else: begin										; "E" within
									if ns ge 3 then begin
										bad[i] = (fnumeric(string(b[0:q[0]-1])) eq 0) or $
														(inumeric(string(b[q[0]+1:ns-1])) eq 0)
									endif else bad[i]=1
									end
							endcase
							end
						else: bad[i] = 1										; too many "E"
					endcase														; not legal
					end
			endcase
		endif
	endfor
	return, 1-bad
end
