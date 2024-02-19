pro chop_string, str, sub, n_sub, PRESERVE_NULL=PRESERVE_NULL

common c_separators, separators

if n_elements(PRESERVE_NULL) lt 1 then PRESERVE_NULL=0
n_sub = 0
ns = strlen(str)
np = n_elements(separators)
if np lt 1 then begin
;	separators = [' ','	',',']
	separators = ' 	,'
	np = 3
endif
if (ns lt 1) then return

sub = strsplit( str, separators, /extract, PRESERVE_NULL=PRESERVE_NULL)

;b = byte(str)
;bs = byte(separators)
;bt = byte('|')
;bt = bt[0]
;for j=0L,np-1 do begin
;	q = where( b eq bs[j])
;	if q[0] ne -1 then begin
;;		b[q] = bt
;	endif
;endfor
;s = string(b)
;sub = str_sep( s, '|', /trim)

n_sub = n_elements(sub)
return
end
