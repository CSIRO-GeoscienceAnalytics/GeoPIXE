function strip_non_print, s, no_tab=no_tab, no_space=no_space

; Remove all non-printing characters
; /no_tab		remove tab as well
; /no_space	remove all spaces too

COMPILE_OPT STRICTARR
if n_elements(no_space) eq 0 then no_space=0
if n_elements(no_tab) eq 0 then no_tab=0

ns = n_elements(s)
if ns lt 1 then return, ''
if ns gt 1 then begin
	s2 = strarr(ns)
	for i=0,ns-1 do s2[i] = strip_non_print( s[i], no_tab=no_tab, no_space=no_space)
	return, s2
endif

b = byte(s)
if n_elements(b) lt 1 then return, ''

good = bytarr(256)
good[32:126] = 1
if no_space then good[32] = 0
if no_tab eq 0 then good[9] = 1

q = where( good[b] eq 1, nq)
if nq eq 0 then return, ''

return, string(b[q])
end
