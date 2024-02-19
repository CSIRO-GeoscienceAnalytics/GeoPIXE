function is_upper, c

; Is 'c' a upper case alphabetic character

b = byte(c)
i = 0
if (b[0] ge 65) and (b[0] le 90) then i=1

return, i
end

