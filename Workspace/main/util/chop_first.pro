pro chop_first, str, first, rest
;
common c_separators, separators

first = ''
rest = ''
ns = strlen(str)
np = n_elements(separators)
if (ns lt 1) or (np lt 1) then return

s = str
n = intarr(4)
k = 0
found = 1
for i=0L,ns-1 do begin
	t = strmid(str,i,1)
	for j=0L,np-1 do begin
		if t eq separators[j] then begin
			if found eq 0 then begin	; first separator
				n[k] = i-1
				k = k+1
				if k ge 4 then goto, finish
			endif
			found = 1
			goto, more
		endif
	endfor
	if found eq 1 then begin			; first non-separator
		n[k] = i
		k = k+1
		if k ge 4 then goto, finish
		found = 0
	endif
  more:
endfor
n[k] = ns-1

finish:
first = strtrim( strmid( str, n[0], n[1]-n[0]+1), 2)
if k gt 1 then begin
	rest = strtrim( strmid( str, n[2], strlen(str)-n[2]), 2)
endif else begin
	rest = ''
endelse
return
end
