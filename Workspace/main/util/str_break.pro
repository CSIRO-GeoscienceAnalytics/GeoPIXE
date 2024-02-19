function str_break, str, keep_double=keep_double, discard_single=discard_single, all=all

; Break up 'str' in array by white space.
; Don't break within bracketed strings enclosed within ' or "
; Discard the single and double quotes.
; /keep_double			keep the double quotes (normally removed)
; /discard_single		discard the single quotes (normally keep the single quotes)

COMPILE_OPT STRICTARR
if n_elements(str) lt 1 then return, ''
if n_elements(str) gt 1 then return, str
if n_elements(keep_double) lt 1 then keep_double=0
if n_elements(discard_single) lt 1 then discard_single=0

bs = byte("'")
bd = byte('"')
bm = byte('|')
space = 32B
tab = 9B

n = strlen(str)
b = byte(replace('|','-',str))

in_double = 0
in_single = 0
for i=0L,n-1 do begin
	if b[i] eq bs then in_single = 1-in_single
	if b[i] eq bd then in_double = 1-in_double
	if (in_single eq 0) and (in_double eq 0) then begin
		if (b[i] eq space) or (b[i] eq tab) then begin
			b[i] = bm
		endif 
	endif
endfor
t = string(b)
out = strsplit( t, '|', /extract)
all = out

if keep_double eq 0 then out=strip_char( out, '"')
if discard_single then out=strip_char( out, "'")
return, out
end
