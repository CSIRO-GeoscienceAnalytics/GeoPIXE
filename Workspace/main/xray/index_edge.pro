function index_edge, n

; returns the xray edge/sub-shell index for the xray line index n

shell_list = [0, 1,1,1,1, 1,1,1,1, 1,1,1,1, $
				 4,3,4,4, 3,4,2,2, 4,4,3,2, 2,2,3,3, 4,0,0, $
				 9, 9,8,7,9, 0, 1,1, $
				 0,0,0,0, 0,0,0,0, $
				 7,6,7, 4,2]

nn = n_elements(n)
if nn lt 1 then return, 0

shell = intarr(nn)
q = where( (n ge 1) and (n lt n_elements(shell_list)))
shell[q] = shell_list[ n[q] ]

if nn eq 1 then shell = shell[0]
return, shell
end