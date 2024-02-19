function fix_file_name, file, all=all, blank=blank

; Remove illegal characters from a filename

if n_elements(all) lt 1 then all=0
if n_elements(blank) lt 1 then blank=0
if n_elements(file) lt 1 then return,''
if strlen(file) lt 1 then return,''

bad = bytarr(256)
bad[0:31] = 1			; control chars
bad[33:44] = 1			; specials (!"#$%&'()*+,)
bad[60] = 1				; <
bad[62] = 1				; >
bad[63] = 1				; ?
bad[64] = 1				; @
bad[94] = 1				; ^
bad[96] = 1				; `
bad[123:127] = 1		; { | } ~ rubout

if blank then bad[32] = 1
if all then begin
	bad[47] = 1			; /
	bad[58] = 1			; :
	bad[59] = 1			; ;
	bad[91] = 1			; [
	bad[92] = 1			; \
	bad[93] = 1			; ]
endif

underscore = 95B
hyphen = 45B

b = byte(file)
q = where( bad[b] eq 1)
if q[0] ne -1 then b[q] = underscore

return, string(b)
end
