function all_lines, z, k=k, l=l, m=m, range=range, rel=rel
;
;	Return all X-ray line energies for 'z'
;	for selected shell(s), else all, and
;	for selected energy range, else all.
;
common c_line_list_1, element_start, line_list, n_line_list
common c_atomic_1, periodic_table, n_periodic

;print,'z=',z,'  k,l,m=',k,l,m
if n_elements(n_periodic) lt 1 then t = atomic_number('He')
if n_elements(n_line_list) lt 1 then n_line_list = 0
build_line_list, 0.0001

if (z[0] le 0) or (z[0] ge n_periodic) then return, 0.0
start = element_start(z[0])
if start eq -1 then return, 0.0

if n_elements(k) lt 1 then k=0
if n_elements(l) lt 1 then l=0
if n_elements(m) lt 1 then m=0
if n_elements(range) lt 1 then range=[0.0,1.0E+9]
if (k eq 0) and (l eq 0) and (m eq 0) then begin
	k = 1
	l = 1
	m = 1
endif

i = start
first = 1
e = 0.0
rel = 0.0
while ((line_list.z[i<(n_line_list-1)] eq z[0]) and (i lt n_line_list)) do begin
	shell = strmid( line_list.line[i], 0,1)
	if (((shell eq 'K') and (k eq 1)) or $
			((shell eq 'L') and (l eq 1)) or $
			((shell eq 'M') and (m eq 1))) and $
			((range[0] le line_list.e[i]) and $
				(line_list.e[i] le range[1])) then begin
		if first then begin
			e = line_list.e[i]
			rel = line_list.rel[i]
			first = 0
		endif else begin
			e = [e,line_list.e[i]]
			rel = [rel,line_list.rel[i]]
		endelse
	endif
	i = i+1
endwhile

;print,'e=',e
return, e
end
