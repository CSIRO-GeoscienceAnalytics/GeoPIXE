pro show_lines, z, shell, e=e

if n_elements(shell) lt 1 then shell=2
if n_elements(z) lt 1 then z=79
if n_elements(e) lt 1 then e=30.

print,'============= Initialize XRF intensities using an energy of ',e,' keV ==================='
init_xrf_lines, e, /force

name = strarr(30)
iup = strarr(30)
e = fltarr(30)
rel = fltarr(30)
relx = fltarr(30)
index = intarr(30)
e1 = fltarr(30)
iup1 = strarr(30)
list = list_line_index(z,shell)
;print,'list=',list

case shell of
	1: begin
		name[0]='Ka1'	& iup[0]='K-L3'		& e[0]=edge(z,1)-edge(z,4)
		name[1]='Ka2'	& iup[1]='K-L2'		& e[1]=edge(z,1)-edge(z,3)
		name[2]='Ka3'	& iup[2]='K-L1'		& e[2]=edge(z,1)-edge(z,2)
		name[3]='Kb1'	& iup[3]='K-M3'		& e[3]=edge(z,1)-edge(z,7)
		name[4]='Kb2'	& iup[4]='K-N2,3'	& e[4]=edge(z,1)-0.5*(edge(z,11)+edge(z,12))
		name[5]='Kb3'	& iup[5]='K-M2'		& e[5]=edge(z,1)-edge(z,6)
		name[6]='Kb4'	& iup[6]='K-N4,5'	& e[6]=edge(z,1)-0.5*(edge(z,13)+edge(z,14))
		name[7]='Kb5'	& iup[7]='K-M4,5'	& e[7]=edge(z,1)-0.5*(edge(z,8)+edge(z,9))

		name[8]='Ka_'	& iup[8]='K-L2,3'
		name[9]='Kb_'	& iup[9]='K-M2,3'
		name[10]='Kb11'	& iup[10]='K-M_'
		name[11]='Kb12'	& iup[11]='K-N_'
		qt = [0,1,2]
		qt = [qt,replicate(-1,12-3)]
		end
	2: begin
		name[0]='La1'	& iup[0]='L3-M5'		& e[0]=edge(z,4)-edge(z,9)
		name[1]='La2'	& iup[1]='L3-M4'		& e[1]=edge(z,4)-edge(z,8)
		name[2]='Lb1'	& iup[2]='L2-M4'		& e[2]=edge(z,3)-edge(z,8)
		name[3]='Lb2'	& iup[3]='L3-N5'		& e[3]=edge(z,4)-edge(z,14)
		name[4]='Lb3'	& iup[4]='L1-M3'		& e[4]=edge(z,2)-edge(z,7)
		name[5]='Lb4'	& iup[5]='L1-M2'		& e[5]=edge(z,2)-edge(z,6)
		name[6]='Lb5'	& iup[6]='L3-O4,5'		& e[6]=edge(z,4)-0.5*(edge(z,20)+edge(z,21))
		name[7]='Lb6'	& iup[7]='L3-N1'		& e[7]=edge(z,4)-edge(z,10)
		name[8]='Lb7'	& iup[8]='L3-O1'		& e[8]=edge(z,4)-edge(z,17)
		name[9]="Lb7'"	& iup[9]='L3-N6,7'		& e[9]=edge(z,4)-0.5*(edge(z,15)+edge(z,16))
		name[10]='Lb9,10'	& iup[10]='L1-M4,5'		& e[10]=edge(z,2)-0.5*(edge(z,8)+edge(z,9))
		name[11]='Lb15'	& iup[11]='L3-N4'		& e[11]=edge(z,4)-edge(z,13)
		name[12]='Lb17'	& iup[12]='L2-M3'		& e[12]=edge(z,3)-edge(z,7)
		name[13]='Lg1'	& iup[13]='L2-N4'		& e[13]=edge(z,3)-edge(z,13)
		name[14]='Lg2'	& iup[14]='L1-N2'		& e[14]=edge(z,2)-edge(z,11)
		name[15]='Lg3'	& iup[15]='L1-N3'		& e[15]=edge(z,2)-edge(z,12)
		name[16]='Lg4'	& iup[16]='L1-O3'		& e[16]=edge(z,2)-edge(z,19)
		name[17]="Lg4'"	& iup[17]='L1-O2'		& e[17]=edge(z,2)-edge(z,18)
		name[18]='Lg5'	& iup[18]='L2-N1'		& e[18]=edge(z,3)-edge(z,10)
		name[19]='Lg6'	& iup[19]='L2-O4'		& e[19]=edge(z,3)-edge(z,20)
		name[20]='Lg8'	& iup[20]='L2-O1'		& e[20]=edge(z,3)-edge(z,17)
		name[21]="Lg8'"	& iup[21]='L2-N6,7'		& e[21]=edge(z,3)-0.5*(edge(z,15)+edge(z,16))
		name[22]='Leta'	& iup[22]='L2-M1'		& e[22]=edge(z,3)-edge(z,5)
		name[23]='Ll'	& iup[23]='L3-M1'		& e[23]=edge(z,4)-edge(z,5)
		name[24]='Ls'	& iup[24]='L3-M3'		& e[24]=edge(z,4)-edge(z,7)
		name[25]='Lt'	& iup[25]='L3-M2'		& e[25]=edge(z,4)-edge(z,6)
		name[26]='Lu'	& iup[26]='L3-N6,7'		& e[26]=edge(z,4)-0.5*(edge(z,15)+edge(z,16))
		name[27]='Lv'	& iup[27]='L2-N6,7'		& e[27]=edge(z,3)-0.5*(edge(z,15)+edge(z,16))
		qt = [0,1,3,6,7,8,11,23,24,25]
		qt = [qt,replicate(-1,28-10)]
		end
	3: begin
		name[0]='Ma1'	& iup[0]='M5-N7'		& e[0]=edge(z,9)-edge(z,16)
		name[1]='Ma2'	& iup[1]='M5-N6'		& e[1]=edge(z,9)-edge(z,15)
		name[2]='Mb_'	& iup[2]='M4-N6'		& e[2]=edge(z,8)-edge(z,15)
		name[3]='Mg_'	& iup[3]='M3-N5'		& e[3]=edge(z,7)-edge(z,14)
		name[4]='Mz_'	& iup[4]='M4,5-N2,3'	& e[4]=0.5*(edge(z,8)+edge(z,9))-0.5*(edge(z,11)+edge(z,12))
		name[5]='M-NO'	& iup[5]='M1-3-NO
		name[6]='M3N1'	& iup[6]='M3-N1'		& e[6]=edge(z,7)-edge(z,10)
		name[7]='M2N1'	& iup[7]='M2-N1'		& e[7]=edge(z,6)-edge(z,10)
		name[8]='M3O5'	& iup[8]='M3-O5'		& e[8]=edge(z,7)-edge(z,21)
		qt = [0,1]
		qt = [qt,replicate(-1,9-2)]
		end
	else:
endcase

q2 = where(e gt 0.0001)
q1 = sort(e[q2])
q3 = q2[q1]

q = where(name[q3] ne '', count)
if count ne 0 then begin
	index[q3[q]] = line_index(name[q3[q]])
	rel[q3[q]] = relative_intensity(z,index[q3[q]])
	relx[q3[q]] = relative_intensity_xrf(z,index[q3[q]])
	e1[q3[q]] = e_line(z,index[q3[q]])
	iup1[q3[q]] = iupac(name[q3[q]])
	qt1 = qt[q3[q]]
	q4 = where(qt1 ge 0)
	qt2 = qt1[q4]

	print,'                  from edge differences            from database'
	print,'                                              (* member of index list)'
	print,'Line','IUPAC','energy','index','e-line','int-PIXE','int-SXRF','IUPAC','difference', format='(4x,A6,1x,A8,2x,A9,11x,A5,3x,A7,2x,A9,1x,A9,3x,A6,9x,A10)'
	for i=0L,count-1 do begin
		OK = ' '
		q2 = where(list eq index[q3[q[i]]])
		if q2[0] ne -1 then OK='*'
		if rel[q3[q[i]]] gt 1.0e-6 then begin
			if e[q3[q[i]]] gt 1.0e-6 then begin
				print,name[q3[q[i]]],iup[q3[q[i]]],e[q3[q[i]]],OK,index[q3[q[i]]],e1[q3[q[i]]],rel[q3[q[i]]],relx[q3[q[i]]],iup1[q3[q[i]]],e1[q3[q[i]]]-e[q3[q[i]]], $
					format='(2x,A8,1x,A8,2x,F9.4,11x,A1,1x,I2,2x,F9.4,2x,F8.5,2x,F8.5,2x,A8,10x,F9.4)'
			endif else begin
				print,name[q3[q[i]]],iup[q3[q[i]]],OK,index[q3[q[i]]],e1[q3[q[i]]],rel[q3[q[i]]],relx[q3[q[i]]],iup1[q3[q[i]]], $
					format='(2x,A8,1x,A8,2x,9x,11x,A1,1x,I2,2x,F9.4,2x,F8.5,2x,F8.5,2x,A8)'
			endelse
		endif else begin
			if e[q3[q[i]]] gt 1.0e-6 then begin
				print,name[q3[q[i]]],iup[q3[q[i]]],e[q3[q[i]]],OK, format='(2x,A8,1x,A8,2x,F9.4,8x,A4)'
			endif else begin
				print,name[q3[q[i]]],iup[q3[q[i]]],OK, format='(2x,A8,1x,A8,2x,9x,8x,A4)'
			endelse
		endelse
	endfor
	print,total(rel[q3[q]]),total(relx[q3[q]]), format='(2x,8x,1x,"Total Rel",10x,11x,1x,1x,2x,12x, F8.4,2x,F8.4)'	
	print,total(relx[qt2]), format='(2x,8x,1x,"Total (last subshell)",9x,1x,1x,2x,12x, 10x,F8.4)'	
;	print,name[qt2]
;	print,iup[qt2]
;	print,iup1[qt2]
endif

return
end
