function big_endian

; Is the current platform big endian byte ordering?
; Use the offset argument to byte to test the first byte
; of a short 16-bit integer 1.

return, byte(1US, 0) eq 0
end