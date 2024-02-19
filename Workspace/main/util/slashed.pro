function slashed, str, i

n = strlen(str)
if (i le 0) or (i ge n-1) then return, 0

s1 = extract( str, i-1,i-1)
s2 = extract( str, i+1,i+1)
se = 0
sl = slash()

if (s1 eq sl) and (s2 eq sl) then se=1

return, se
end
