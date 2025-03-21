function line_id, index

; Return the line mneumonic for index 'index'

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

ni = n_elements(index)
if ni lt 1 then return, ''

mneumonic = ['','Ka1','Ka2','Ka3','Kb1','Kb2','Kb3','Kb4', $
		'Kb5','Ka_','Kb_','Kb11','Kb12','Ll','Leta','La1', $
		'La2','Lb1','Lb2','Lb3','Lb4','Lb5','Lb6','Lg1', $
		'Lg2','Lg3','Lg4','Lg5','Lg6','La_','Lb_','Lg_', $
		'Ma1','Ma2','Mb_','Mg_','Mz_','M-NO','K-LL','K-MM', $
		'sum','esc','gamma','Compton','F197','Na440','elastic', $
		'Compton2','M3N1','M2N1','M3O5','Lt','Lb9,10']

nm = n_elements(mneumonic)
if ni eq 1 then begin
	if (index lt 1) or (index gt nm-1) then return, ''
	return, mneumonic[index]
endif else begin
	id = strarr(n_elements(index))
	q = where( (index ge 1) or (index le nm-1) )
	id[q] = mneumonic[index[q]]
endelse

return, id
end
