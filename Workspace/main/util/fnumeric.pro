function fnumeric, stri

; Is 'str' a valid F-floating numeric string?
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
				0: bad[i] = 1														; blank
				1: bad[i] = 1 - inumeric(str)										; one char only
				else: begin
					q = where( b eq 46B, nq)										; found any "."
					case nq of
						0: bad[i] = 1 - inumeric(str)								; simple inumeric
						1: begin													; one "." only
							case q[0] of	
								0: begin											; "." at the start
									bad[i] = 1										; not legal
									end
								(ns-1): begin										; "." at the end
									bad[i] = 1 - inumeric(string(b[0:ns-2]))
									end
								else: begin											; "." within
									if ns ge 3 then begin
										bad[i] = (inumeric(string(b[0:q[0]-1])) eq 0) or $
														(inumeric(string(b[q[0]+1:ns-1])) eq 0)
									endif else bad[i]=1
									end
							endcase
							end
						else: bad[i] = 1											; too many "."
					endcase															; not legal
					end
			endcase
		endif
	endfor
	return, 1-bad
end
