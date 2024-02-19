function sub_shell, n

; returns the xray sub-shell index for the xray line index n

; M-NO = M2-N4 + M1-N3 + M1-N2 + M3-O4,5

sub_shell_list = [0, 1,1,1,1,1,1,1,1,1,1,1,1,   $
					3,2,3,3,2,3,1,1,3,3,2,1,1,2,2,2,3,2,2,   $
				 	5,5,4,3,4,2,  1,1,  0,0,0,0,0,0,0,0,   $
				 	3,2,3,  3,1]

nn = n_elements(n)
if nn lt 1 then return, 0

shell = intarr(nn)
q = where( (n ge 1) and (n lt n_elements(sub_shell_list)))
shell[q] = sub_shell_list[ n[q] ]

if nn eq 1 then shell = shell[0]
return, shell
end