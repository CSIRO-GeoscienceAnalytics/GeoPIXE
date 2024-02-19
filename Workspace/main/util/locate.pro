function locate, sub, str, nocase=nocase

;	Locate string 'sub' in string 'str'

COMPILE_OPT STRICTARR
if n_elements(nocase) eq 0 then nocase=0

s1 = nocase ? strlowcase(str) : str
s2 = nocase ? strlowcase(sub) : sub 

n = strpos( s1, s2)

return, n
end