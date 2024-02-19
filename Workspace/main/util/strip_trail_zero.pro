function strip_trail_zero, str, keep=keep, places=places

; Strip trailing and sundry zeroes in floating string values.
; Trim to significqnt digits.
; 
; Ignore strip for non floating value strings.
; Only strip after decimal point, or leading zeroes in value and exponent.
; /keep				keep one zero after decimal point
; places=places		only keep this many places maximum after "."
;					if 'places' is negative, relax this to keep significant digits
;
; Chris Ryan (CSIRO), revised 2012

COMPILE_OPT STRICTARR
n = n_elements(str)
if n lt 1 then return, ''
if n_elements(keep) eq 0 then keep=0
if n_elements(places) eq 0 then places=-6
s = strtrim( str,2)
nplace = abs(places)
relax = (places lt 0)

for i=0L,n-1 do begin
	if gnumeric(s[i]) then begin
		if inumeric(s[i]) then begin
			ns  =strlen(s[i])
			x = (ns ge 10) ? long64(s[i]) : long(s[i])
		endif else begin
			x = double(s[i])
		endelse
		s[i] = strtrim(string(x),2)											; standard form (what about rounding?)
		b = byte(s[i])
		nb = n_elements(b)
		me = strpos( strlowcase(s[i]),'e')									; gnumeric allows only one "." and "e"
		md = strpos( strlowcase(s[i]),'.')									; "e" not at start
		ns = strlen(s[i])
		if md ge 0 then begin
			if (me gt 0) then begin											; accept only between "." and "e"
				if me ge md+1 then begin
					s2 = strip_trail_zero( string( b[0:me-1]), /keep, places=places)
				endif else s2 = '0'
				if (b[me+1] eq 43B) or (b[me+1] eq 45B) then begin
					q = where( (b[*] ne 48B) and (b[*] ne 0B), nq)
					q1 = where( q gt me+1, nq1)
					if (nq1 gt 0) then mz = q[q1[0]] else mz = me+1
					s3 = string( b[me:me+1]) + string( b[mz:ns-1])
				endif else s3 = string( b[me:ns-1])
				s[i] = s2 + s3
			endif else if (me eq -1) then begin								; accept only after "."
				q = where( (b[*] ne 48B) and (b[*] ne 0B), nq)
				q1 = where( q gt md, nq1)
				if (nq1 gt 0) and relax then mz = q[q1[0]]-md-1 else mz = 0
				last = (max(q) > md)										; at least up to decimal point 
				if (relax eq 0) then last = last > (nplace+md)				; at least this many places, if specified
				s[i] = string( b[0:(last<(nb-1))])
				if keep and (max(q) eq md) then begin
					s[i] = s[i] + '0'
				endif
			endif
		endif else begin
			if (me gt 0) then begin											; "e" but no "."
				s2 = strip_trail_zero( string( b[0:me-1]), places=places)
				if (b[me+1] eq 43B) or (b[me+1] eq 45B) then begin			; strip leading zeroes in exponent
					q = where( (b[*] ne 48B) and (b[*] ne 0B), nq)			; after sign
					q1 = where( q gt me+1, nq1)
					if (nq1 gt 0) then mz = q[q1[0]] else mz = me+1
					s3 = string( b[me:me+1]) + string( b[mz:ns-1])
				endif else s3 = string( b[me:ns-1])
				s[i] = s2 + s3
			endif else begin												; no "e" either
				if (b[0] eq 43B) or (b[0] eq 45B) then begin				; strip leading zeroes after sign
					q = where( (b[*] ne 48B) and (b[*] ne 0B), nq)
					q1 = where( q gt 0, nq1)
					if (nq1 gt 0) then mz = q[q1[0]] else mz = 1
					s3 = string( b[0]) + string( b[mz:ns-1])
				endif else s3 = string( b[0:ns-1])
				s[i] = s3
			endelse
		endelse
	endif
endfor

return, s
end
