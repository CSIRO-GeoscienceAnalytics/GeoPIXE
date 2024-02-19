function sort_file_numeric, f, ext=ext, name=name, strict=strict

; Sort a file list with numeric extension or file-names, or both
; Chris Ryan (CSIRO), revised 2021

if n_elements(ext) lt 1 then ext=0
if n_elements(name) lt 1 then name=0
if name eq 0 then ext=1
if ext eq 0 then name=1
if n_elements(strict) eq 0 then strict=0

n = n_elements(f)
l = strlen(f)
i = strpos(f,'.')
d = (n+1)*indgen(n)
q = where(i eq -1, nq)
if nq gt 0 then i[q] = l[q]
sname = strarr(n)
sext = strarr(n)

for j=0L,n-1 do begin
	sname[j] = strupcase(strmid(f[j],0,i[j]))
	sext[j] = strupcase(strmid(f[j],i[j]+1,l[j]-i[j]-1))
endfor
m = max(strlen(sext))
scale = 10LL ^ m

if ext then begin										; sort extension numerically
	if name then begin									; name also
		j = long64test(sname, strict=strict) * scale + long64test(sext, strict=strict)
		q = sort(j)
		qz = where(j[q] eq 0, nqz)						; for non-numeric names, sort alphabetically
		if nqz gt 0 then begin
			q2 = sort(sname[q[qz]] + sext[q[qz]])
			q[qz] = q[qz[q2]]
		endif
	endif else begin
		j = long64test(sext, strict=strict)
		q = sort(j)
		qz = where(j[q] eq 0, nqz)						; for non-numeric extensions, sort alphabetically
		if nqz gt 0 then begin
			q2 = sort(sext[q[qz]])
			q[qz] = q[qz[q2]]
		endif
	endelse
endif else if name then begin							; sort name numerically
	j = long64test(sname, strict=strict)
	q = sort(j)
	qz = where(j[q] eq 0, nqz)							; for non-numeric names, sort alphabetically
	if nqz gt 0 then begin
		q2 = sort(sname[q[qz]])
		q[qz] = q[qz[q2]]
	endif
endif

return, q
end
