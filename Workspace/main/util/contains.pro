function contains, list, s
;
; Test if 'list' contains 's'
; return index, else -1
;
if n_elements(list) lt 1 then return, -1
if n_elements(s) lt 1 then return, -1

for i=0L,n_elements(list)-1 do begin
	if str_equal( list[i], s, /ignore_case) then return, i
endfor

return, -1
end
