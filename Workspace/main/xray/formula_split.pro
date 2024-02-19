function formula_split, linei

; Split formula between element and weight groups

line = strcompress(linei,/remove_all)
n = lenchr(line)
if n lt 1 then return, ''
string = ''

j = 0
k = -1
for i=0L,n-1 do begin
	s = extract( line,i,i)
	if is_upper(s) and (sole_e(line,i) eq 0) and (slashed(line,i) eq 0) then begin
		if k ge j then begin
			string = [string,extract(line,j,k)]
			j = i
		endif
	endif

	k = i
endfor
if n-1 ge j then begin
	string = [string,extract(line,j,n-1)]
endif

if n_elements(string) gt 1 then string = string[1:*]
return, string
end
