function hide_embedded, s, c, unhide=unhide

; Hide any 'c' (e.g. ',') that are embedded within containers '[]', '{}', '""', '()', etc.
; (use in unstringify)
;
; Chris Ryan (CSIRO), 2010, revised 2012
;							revised 2020 - treat levels of single and double quotes explicitly, separately

COMPILE_OPT STRICTARR
if n_elements(unhide) eq 0 then unhide=0

b = byte(s)
n = n_elements(b)
if n lt 1 then return, ''
if n_elements(c) lt 1 then c=','

bc = byte(c)
bv = byte('|')

if unhide then begin
	q = where(b eq bv[0], nq)
	if nq gt 0 then b[q]=bc
	t = string(b)
	return, t
endif

level = intarr(n)
bup = bytarr(n)
bdown = bytarr(n)
text1 = ['"']
text2 = ["'"]
up = ['{','[','(']
down = ['}',']',')']
text1 = byte(text1)
text2 = byte(text2)
up = byte(up)
down = byte(down)

lev = 0
in_text1 = 0
in_text2 = 0
for i=0L,n-1 do begin
	qtxt1 = where( b[i] eq text1, ntxt1)
	qtxt2 = where( b[i] eq text2, ntxt2)
	qup = where( b[i] eq up, nup)
	qdown = where( b[i] eq down, ndown)
	if ntxt1 ge 1 then begin
		if in_text1 then begin
			lev = lev-1
			in_text1 = 0
		endif else begin
			lev = lev+1
			in_text1 = 1
		endelse
	endif else if ntxt2 ge 1 then begin
		if in_text2 then begin
			lev = lev-1
			in_text2 = 0
		endif else begin
			lev = lev+1
			in_text2 = 1
		endelse
	endif else if (nup ge 1) and (in_text1 eq 0) and (in_text2 eq 0) then begin
		lev = lev+1
	endif else if (ndown ge 1) and (in_text1 eq 0) and (in_text2 eq 0) then begin
		lev = (lev-1) > 0
	endif
	level[i] = lev
endfor

q = where( (b eq bc[0]) and (level ne 0), nq)
if nq gt 0 then b[q] = bv[0]

t = string(b)
return, t
end
