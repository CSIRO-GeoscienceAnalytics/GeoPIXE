function str_escape, str, twice=twice, strip=strip, join=join

; str_escape, str, twice=twice, strip=strip
; 
; Correct a string to insert escape sequences for special characters.
; Remove any leading or trailing '"'
; /twice	do this twice to allow for 2 levels of decoding.
; /strip	strip out escape sequence "\\" instead of insertion
; /join		join a vector of strings, with space between

n = n_elements(str)
if n_elements(str) lt 1 then return, ''
if n_elements(twice) lt 1 then twice=0
if n_elements(strip) lt 1 then strip=0
if n_elements(join) lt 1 then join=0
out = str
if size(str, /tname) ne 'STRING' then return, out

; Strip escaping for special characters ...

if strip then begin
	if twice then begin
		for i=0L,n-1 do begin
			s = str[i]
			ns = strlen(s)
			if strmid(s,0,1) eq '"' then s=strmid(s,1,ns-1)
			ns = strlen(s)
			if strmid(s,ns-1,1) eq '"' then s=strmid(s,0,ns-1)
			ns = strlen(s)
			b = byte(s)
			pos = 0
			done = 0
			repeat begin
				k = strpos( s, '\\\\', pos)
				if k ge 0 then b[k:k+2] = 0B else done=1
				pos = k+4
			endrep until (pos ge ns) or done
			pos = 0
			done = 0
			repeat begin
				k = strpos( s, '\\\"', pos)
				if k ge 0 then b[k:k+2] = 0B else done=1
				pos = k+4
			endrep until (pos ge ns) or done
			q = where( b gt 0, nq)
			if nq gt 0 then out[i]=string(b[q]) else out[i]=''
		endfor
	endif else begin
		for i=0L,n-1 do begin
			s = str[i]
			ns = strlen(s)
			if strmid(s,0,1) eq '"' then s=strmid(s,1,ns-1)
			ns = strlen(s)
			if strmid(s,ns-1,1) eq '"' then s=strmid(s,0,ns-1)
			ns = strlen(s)
			b = byte(s)
			pos = 0
			done = 0
			repeat begin
				k = strpos( s, '\\', pos)
				if k ge 0 then b[k] = 0B else done=1
				pos = k+2
			endrep until (pos ge ns) or done
			pos = 0
			done = 0
			repeat begin
				k = strpos( s, '\"', pos)
				if k ge 0 then b[k] = 0B else done=1
				pos = k+2
			endrep until (pos ge ns) or done
			q = where( b gt 0, nq)
			if nq gt 0 then out[i]=string(b[q]) else out[i]=''
		endfor
	endelse
	goto, done
endif

; Add escaping for special characters ...

if twice then begin
	for i=0L,n-1 do begin
		b = byte(str[i])
		nb = n_elements(b)
		s = ''
		for j=0L,nb-1 do begin
			case b[j] of
				9B: t = '\\t'				; tab
;				39B: t = "\\\'"				; '
;				34B: t = '\\\"'				; "
				92B: t = '\\\\'				; \
				else: begin
					if (b[j] ge 32) and (b[j] le 126) then begin
						t = string(b[j])
					endif else begin
						t = ''
					endelse
					end
			endcase
			s = s + t
		endfor
		out[i] = s
	endfor
endif else begin
	for i=0L,n-1 do begin
		b = byte(str[i])
		nb = n_elements(b)
		s = ''
		for j=0L,nb-1 do begin
			case b[j] of
				9B: t = '\t'				; tab
;				39B: t = "\'"				; '
;				34B: t = '\"'				; "
				92B: t = '\\'				; \
				else: begin
					if (b[j] ge 32) and (b[j] le 126) then begin
						t = string(b[j])
					endif else begin
						t = ''
					endelse
					end
			endcase
			s = s + t
		endfor
		out[i] = s
	endfor
endelse

done:
	if join then out = strjoin( out, ' ')
	if n_elements(out) eq 1 then out=out[0]
	return, out
end
