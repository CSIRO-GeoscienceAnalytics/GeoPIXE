pro q_to_xy, q, nx, x,y

; Convert the 1D index vector into x,y vector coordinates in 
; a 2D matrix.
; It assumes that 'q' is already in INT or LONG form.

y = q / nx
x = q - nx*y

return
end
