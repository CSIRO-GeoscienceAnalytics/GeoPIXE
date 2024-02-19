function index_shell, n

; returns the xray shell index for the xray line index n

shell_list = [0, 1,1,1,1,1,1,1,1,1,1,1,1, $
				 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, $
				 3,3,3,3,3,3, $
				 1,1, $
				 0,0,0,0,0,0,0,0, $
				 3,3,3, 2,2]

nn = n_elements(n)
if nn lt 1 then return, 0

shell = intarr(nn)
q = where( (n ge 1) and (n lt n_elements(shell_list)))
shell[q] = shell_list[ n[q] ]

if nn eq 1 then shell = shell[0]
return, shell
end