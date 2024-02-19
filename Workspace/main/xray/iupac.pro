function iupac, linei

; Return the IUPAC notation string for a given Siegbahn transition string
; Use lines as per line_index list

;Index Line  IUPAC   Index Line  IUPAC  Index Line  IUPAC  Index Line  IUPAC
;
;  1   Ka1   K-L3      2   Ka2   K-L2     3   Ka3   K-L1     4   Kb1   K-M3
;  5   Kb2   K-N2,3    6   Kb3   K-M2     7   Kb4   K-N4,5   8   Kb5   K-M4,5
;  9   Ka_   K-L2,3   10   Kb_   K-MN    11   Kb11  K-M2-5  12   Kb12  K-N2-7
;  
; 13   Ll    L3-M1    14   Leta  L2-M1   15   La1   L3-M5   16   La2   L3-M4
; 17   Lb1   L2-M4    18   Lb2   L3-N5   19   Lb3   L1-M3   20   Lb4   L1-M2
; 21   Lb5   L3-O4,5  22   Lb6   L3-N1   23   Lg1   L2-N4   24   Lg2   L1-N2
; 25   Lg3   L1-N3    26   Lg4   L1-O2   27   Lg5   L2-N1   28   Lg6   L2-O4
; 29   La_   L3-M4,5  30   Lb_   L-MNO   31   Lg_   L-NO   
; 
; 32   Ma1   M5-N7
; 33   Ma2   M5-N6    34   Mb_   M4-N6   35   Mg_   M3-N5   36   Mz_   M4,5-N2,3
; 37   M-NO  M-NO  
; 
; 38   K-LL  K-LL     39   K-MM  K-MM
; 
; 48   M3N1  M3-N1    49   M2N1  M2-N1   50   M3O5  M3-O5
; 51   Lt    L3-M2    52   Lb9,10 L1-M4,5

nl = n_elements(linei)
if nl lt 1 then return, 0
line = strtrim(linei,2)
index = line_index(line)

ni = n_elements(index)
if ni lt 1 then return, ''

IUPAC = ['','K-L3','K-L2','K-L1','K-M3','K-N2,3','K-M2','K-N4,5','K-M4,5', $
		'K-L2,3','K-MN','K-M2-5','K-N2-7','L3-M1','L2-M1','L3-M5','L3-M4','L2-M4', $
		'L3-N5','L1-M3','L1-M2','L3-O4,5','L3-N1','L2-N4','L1-N2','L1-N3', $
		'L1-O3','L2-N1','L2-O4','L3-M4,5','L-MNO','L-NO','M5-N7','M5-N6', $
		'M4-N6','M3-N5','M4,5-N2,3','M-NO','K-LL rA','K-MM rA','','','','', $
		'','','','','M3-N1','M2-N1','M3-O5','L3-M2','L1-M4,5']

nm = n_elements(IUPAC)
if ni eq 1 then begin
	if (index lt 1) or (index gt nm-1) then return, ''
	return, IUPAC[index]
endif else begin
	id = strarr(n_elements(index))
	q = where( (index ge 1) or (index le nm-1) )
	id[q] = IUPAC[index[q]]
endelse

return, id
end
