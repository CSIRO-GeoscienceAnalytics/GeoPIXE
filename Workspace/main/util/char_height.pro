function char_height, sz

; return charcter height in norm coords for size 'sz'
; or the current !p.charsize.

if n_elements(sz) lt 1 then sz = !p.charsize

y = (float(!d.y_ch_size) / float(!d.y_size)) * sz

return, y
end
