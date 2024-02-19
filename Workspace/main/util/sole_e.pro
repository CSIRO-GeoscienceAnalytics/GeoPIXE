function sole_e, str, i

n = strlen(str)
if (i lt 0) or (i ge n) then return, 0

s1 = extract( str, i,i)
se = 0

if i eq n-1 then begin
	if (s1 eq 'E') then se=1
endif else begin
	s2 = extract( str, i+1,i+1)
	if (s1 eq 'E') and (is_lower(s2) eq 0) then se=1
endelse

return, se
end
