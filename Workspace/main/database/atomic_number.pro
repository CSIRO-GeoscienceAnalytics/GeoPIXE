function atomic_number, el

common c_atomic_1, periodic_table, n_periodic

if n_elements( n_periodic) lt 1 then begin
	t1 =		 [ " ", "H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg", "Al", "Si", $
				"P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe", $
				"Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", $
				"Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", $
				"Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm"]
	t2 = 			[ "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", $
				"Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", $
				"Fr", "Ra", "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", $
				"Es", "Fm", "Md", "No", "Lr" ]
	periodic_table = [t1,t2]
	n_periodic = n_elements( periodic_table)
endif

s = strtrim( el, 2)
n = n_elements( s)
z = intarr( n)
k = 0
for i=0L, n_periodic-1 do begin
	q = where(s eq periodic_table[i])
	if q[0] ne -1 then begin
		z[q] = i
		k = k+n_elements(q)
		if k ge n then goto, done
	endif
endfor

done:
if n_elements(z) eq 1 then z = z[0]
return, z
end
