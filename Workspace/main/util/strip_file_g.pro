function strip_file_g, filei
;
; remove "-g" at end

if n_elements(filei) lt 1 then return, ''
file = filei[0]

n = strlen(file)
if n lt 2 then return, file

s = strmid(file,n-2,2)
f = file

if str_equal(s,'-g',/ignore_case) then f = strmid(file,0,n-2)

return, f
end
