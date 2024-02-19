function list_line_index_sort_e, z, shell

; Return line energies, sorted in ascending energy,
; for a list of line indices for this 'shell' and 'z',
; sorted in order of relative-intensity.
;
; Both arguments must be scalars
;
; Shells are:  K=1, L=2, M=3, all=0

if n_params() lt 2 then return, 0

list = list_line_index( z,shell,/nosort)

energy = e_line(z,list)
q = sort(energy)			; sort Energy

list = list[q]

return, list
end
