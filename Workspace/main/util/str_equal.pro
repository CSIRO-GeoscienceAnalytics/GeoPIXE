function str_equal_one, s1,s2, ignore_case=ignore_case

; Ignore trailing blanks

if n_elements(ignore_case) lt 1 then ignore_case=0
n1 = lenchr(s1)
n2 = lenchr(s2)
if n1 ne n2 then return, 0

t1 = strmid(s1,0,n1)
t2 = strmid(s2,0,n2)
if ignore_case then begin
	t1 = strlowcase(t1)
	t2 = strlowcase(t2)
endif
if t1 eq t2 then return, 1

return, 0
end

;---------------------------------------------------------------------------

function str_equal, s1,s2, ignore_case=ignore_case

; Ignore trailing blanks and blank strings

if n_elements(ignore_case) lt 1 then ignore_case=0
n1 = n_elements(s1)
n2 = n_elements(s2)

n = min([n1,n2])
for i=0L,n-1 do begin
	if str_equal_one( s1[i],s2[i], ignore_case=ignore_case) eq 0 then return, 0
endfor

nl = max([n1,n2])
for i=n,nl-1 do begin
	if i le n1-1 then begin
		if lenchr(s1[i]) ne 0 then return, 0
	endif
	if i le n2-1 then begin
		if lenchr(s2[i]) ne 0 then return, 0
	endif
endfor

return, 1
end
