pro print_relative_intensities, Z, shell=shell

if n_elements(z) lt 1 then z=6+indgen(93-6)
if n_elements(shell) lt 1 then shell=0			; default to ALL

;	Index Line	   Index Line	  Index Line	 Index Line
;
;	  1   Ka1        2   Ka2        3   Ka3        4   Kb1
;	  5   Kb2        6   Kb3        7   Kb4        8   Kb5
;	  9   Ka_       10   Kb_       11   Kb11      12   Kb12
;	 13   Ll        14   Leta      15   La1       16   La2
;	 17   Lb1       18   Lb2       19   Lb3       20   Lb4
;	 21   Lb5       22   Lb6       23   Lg1       24   Lg2
;	 25   Lg3       26   Lg4       27   Lg5       28   Lg6
;	 29   La_       30   Lb_       31   Lg_	      32   Ma1
;	 33   Ma2       34   Mb_       35   Mg_       36   Mz_
;	 37   M-NO      38   K-LL      39   K-MM
;	 40   sum       41   esc       42   gamma     43   Compton
;	 44   F197      45   Na440     46   elastic   47   Compton2
;    48   M3N1      49   M2N1      50   M3O5      51   Lt
;    52   Lb9,10

for i=0L,n_elements(z)-1 do begin
	el = element_name(z[i])
	n = list_line_index( z[i], shell)
	name = line_id(n)
	iup = iupac( name)
	e = e_line( z[i], n)
	r = relative_intensity( z[i], n)
	print,z[i], '  ',el
	for j=0L,n_elements(n)-1 do begin
		print,n[j],name[j],iup[j],e[j],r[j], $
			format='(I4,T8,A6,T16,A8,T27,F7.3,T37,F9.6)'
	endfor
endfor
return
end
