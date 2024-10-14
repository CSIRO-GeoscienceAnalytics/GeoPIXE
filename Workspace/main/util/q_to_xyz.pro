pro q_to_xyz, q, nx,ny, x,y,z

; Convert the 1D index vector q into x,y,z vector coordinates in 
; a 3D matrix.
; It assumes that 'q' is already in LONG form.

nxy = long(nx)*long(ny)
z = q / nxy
y = (q - nxy*z) / nx
x = q - nx*y - nxy*z

return
end
