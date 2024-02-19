function decode_elam, file

; Decode the ElamDB12.txt file and return parameters ...
; W.T. Elam et al., Radiation Physics and Chemistry 63 (2002) 121-128.

; Use the GeoPIXE line indices ...
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
;    48   M3N1      49   M2N1      50   M3O5      51   Lt
;    52   Lb9,10

COMPILE_OPT STRICTARR
common c_working_dir2, geopixe_database_path

elam_line_index = [ 1,2,3,4,5,6,7,8,11,12, $				; K
					13,14,15,16,17,18,18,19,20, $			; L
					21,22,23,24,25,26,27,28, $				; L
					32,33,32,34,35,36,37,48,49,50, $		; M
					51,52]									; L
elam_line_subshell = [1,1,1,1,1,1,1,1,1,1, $
					3,2,3,3,2,3,3,1,1, $
					3,3,2,1,1,1,2,2, $
					5,5,5,4,3,4,2,3,2,3, $
					3,1]
elam_line_names = ['Ka1','Ka2','Ka3','Kb1','Kb2','Kb3','Kb4','Kb5','Kb11','Kb12', $
					'Ll','Ln','La1','La2','Lb1','Lb2','Lb2,15','Lb3','Lb4', $
					'Lb5','Lb6','Lg1','Lg2','Lg3','Lg4','Lg5','Lg6', $
					'Ma1','Ma2','Ma','Mb','Mg','Mz','M-NO','M3N1','M2N1','M3O5', $
					'Lt','Lb9,10']
; Note that Elam L eta is denoted as "Ln" in input table, and needs to be converted to "Leta" ...

geopixe_line_names = ['Ka1','Ka2','Ka3','Kb1','Kb2','Kb3','Kb4','Kb5','Kb11','Kb12', $
					'Ll','Leta','La1','La2','Lb1','Lb2','Lb2,15','Lb3','Lb4', $
					'Lb5','Lb6','Lg1','Lg2','Lg3','Lg4','Lg5','Lg6', $
					'Ma1','Ma2','Ma','Mb','Mg','Mz','M-NO','M3N1','M2N1','M3O5', $
					'Lt','Lb9,10']

; And edge definitions ...
;	1  K		6  Mii		11 Nii		16 Nvii		21 Ov
;	2  Li		7  Miii		12 Niii		17 Oi		22 Pi
;	3  Lii		8  Miv		13 Niv		18 Oii		23 Pii
;	4  Liii		9  Mv		14 Nv		19 Oiii		24 Piii
;	5  Mi		10 Ni		15 Nvi		20 Oiv

elam_edge_names = ['?','K','L1','L2','L3','M1','M2','M3','M4','M5','N1','N2','N3', $
					'N4','N5','N6','N7','O1','O2','O3','O4','O5','P1','P2','P3']

;	if n_elements(file) lt 1 then file=geopixe_database_path+'dat/ElamDB12-original.txt'
	if n_elements(file) lt 1 then file=geopixe_database_path+'dat/ElamDB12.txt'
;	if n_elements(file) lt 1 then file=file_requester()
	if n_elements(file) lt 1 then return, 0
	if lenchr(file) lt 1 then return, 0

	elam0 = { edge:fltarr(25), jump:fltarr(25), fluor:fltarr(25), $
				ck:{ L:{f12:0.0, F13:0.0, f23:0.0}, M:{f12:0.0, F13:0.0, F14:0.0, F15:0.0, f23:0.0, F24:0.0, F25:0.0, f34:0.0, F35:0.0, f45:0.0 }}, $
				e:fltarr(60), id:strarr(60), branch:fltarr(60), $
				subshell:intarr(60), intense:fltarr(60) }
	data = replicate(elam0, 100)

	z = 0
	tokens = ['Element','Edge','CK','CKtotal','EndElement','Photo','Scatter']

	on_ioerror, bad_file
	openr, lun, file, /get_lun

	line_mode = 0
	s = ''

	repeat begin
next:
		readf,lun, s

		set_separators, '	 '
		chop_string, s, sub, n_sub
		if n_sub ge 1 then begin
			if sub[0] eq '//' then goto, next

; Stay in 'line mode' unless a new token is encountered ...

			q = where(sub[0] eq tokens, count)
			if count ne 0 then line_mode=0

			if line_mode then begin

;				Elam line format:  Transition, mneumonic, energy (eV), branch

				q = where( sub[1] eq elam_line_names, count)
				if count ne 0 then begin
					i = elam_line_index[q[0]]
					elam.e[i] = float(sub[2])
					elam.branch[i] = float(sub[3])
					elam.id[i] = geopixe_line_names[q[0]]			; sub[1] - except for "Ln" --> "Leta"
					elam.subshell[i] = elam_line_subshell[q[0]]
				endif
				goto, next
			endif

			case sub[0] of
				'Element': begin

;					Elam element format: name, Z, atomic-weight, density (g/cm3)

					name = sub[1]
					z = fix(sub[2])
					elam = elam0
					end

				'EndElement': begin
					data[z] = elam
					end

				'Lines': line_mode=1

				'Edge': begin

;					Elam edge format: sub-shell, energy (eV), fluor-yield, jump

					q = where( sub[1] eq elam_edge_names, count)
					if count ne 0 then begin
						subshell = sub[1]
						elam.edge[q[0]] = float(sub[2])/1000.0
						elam.fluor[q[0]] = float(sub[3])
						elam.jump[q[0]] = float(sub[4])
					endif
					end

				'CKtotal': begin
					case subshell of
						'L1': begin
							for i=1L,n_sub-2,2 do begin
								case sub[i] of
									'L2': begin
										elam.ck.L.f12 = float(sub[i+1])
										end
									'L3': begin
										elam.ck.L.F13 = float(sub[i+1])
										end
									else:
								endcase
							endfor
							end
						'L2': begin
							for i=1L,n_sub-2,2 do begin
								case sub[i] of
									'L3': begin
										elam.ck.L.f23 = float(sub[i+1])
										end
									else:
								endcase
							endfor
							end
						'M1': begin
							for i=1L,n_sub-2,2 do begin
								case sub[i] of
									'M2': begin
										elam.ck.M.f12 = float(sub[i+1])
										end
									'M3': begin
										elam.ck.M.F13 = float(sub[i+1])
										end
									'M4': begin
										elam.ck.M.F14 = float(sub[i+1])
										end
									'M5': begin
										elam.ck.M.F15 = float(sub[i+1])
										end
									else:
								endcase
							endfor
							end
						'M2': begin
							for i=1L,n_sub-2,2 do begin
								case sub[i] of
									'M3': begin
										elam.ck.M.f23 = float(sub[i+1])
										end
									'M4': begin
										elam.ck.M.F24 = float(sub[i+1])
										end
									'M5': begin
										elam.ck.M.F25 = float(sub[i+1])
										end
									else:
								endcase
							endfor
							end
						'M3': begin
							for i=1L,n_sub-2,2 do begin
								case sub[i] of
									'M4': begin
										elam.ck.M.f34 = float(sub[i+1])
										end
									'M5': begin
										elam.ck.M.F35 = float(sub[i+1])
										end
									else:
								endcase
							endfor
							end
						'M4': begin
							for i=1L,n_sub-2,2 do begin
								case sub[i] of
									'M5': begin
										elam.ck.M.f45 = float(sub[i+1])
										end
									else:
								endcase
							endfor
							end
						else:
					endcase
					end
				else:
			endcase
		endif
	endrep until EOF(lun)
	goto, finish

bad_file:
	warning,'decode_elam','bad file open for '+file
	data = 0
	goto, finish

finish:
	close_file, lun
	return, data
end
