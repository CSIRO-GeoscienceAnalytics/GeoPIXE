pro spec_back, file, n, q

; Ammend a spec file, spec 'n', show_back to 'q'

if n_elements(q) lt 1 then return
nn = n_elements(n)
if nn lt 1 then return

p = read_spec(file)
if ptr_valid(p[0]) eq 0 then return
np = n_elements(p)

for i=0L,nn-1 do begin
	if n[i] lt np then begin
		print,'Modify show ',n[i],' from ',(*p[n[i]]).show_back,'  to  ',long(q)
		(*p[n[i]]).show_back = long(q)
	endif
endfor

write_spec, p, file
return
end

