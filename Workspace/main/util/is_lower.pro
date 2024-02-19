function is_lower, c

; Is 'c' a lower case alphabetic character

b = byte(c)
i = 0
if (b[0] ge 97) and (b[0] le 122) then i=1

return, i
end

