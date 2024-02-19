	pro init_edge
;
;	Read in electron binding energy table, for X-ray edges.
;
;	1  K		6  Mii		11 Nii		16 Nvii		21 Ov
;	2  Li		7  Miii		12 Niii		17 Oi		22 Pi
;	3  Lii		8  Miv		13 Niv		18 Oii		23 Pii
;	4  Liii		9  Mv		14 Nv		19 Oiii		24 Piii
;	5  Mi		10 Ni		15 Nvi		20 Oiv

	common c_edge, edge, edge_OK
	common c_working_dir2, geopixe_database_path
;
	edge_OK = 0
;	on_ioerror, bad
	openr, lun, geopixe_database_path+'dat/edge.txt', /get_lun

	edge = fltarr(95,24)			; z from 0 to 94
	str1 = ' '
	str2 = ' '
	str3 = ' '

	while EOF(lun) eq 0 do begin
	    readf,lun, str1
	    readf,lun, str2
	    readf,lun, str3

		z = fix( strmid( str1, 0, 4))
	    if (z ge 1) and (z le 94) then begin
			for i=0L,10 do begin
				s = strtrim( strmid( str1, 4+11*i, 11), 2)
				if strlen(s) ge 1 then begin
					edge[z,i] = float(s)
				endif
				s = strtrim( strmid( str2, 4+11*i, 11), 2)
				if strlen(s) ge 1 then begin
					edge[z,i+11] = float(s)
				endif
				s = strtrim( strmid( str3, 4+11*i, 11), 2)
				if strlen(s) ge 1 then begin
					edge[z,i+22] = float(s)
				endif
			endfor
	    endif
	endwhile
	edge_OK = 1
	edge = edge/1000.0
	goto, finish

bad:
	print,'init_edge: Error reading "edge.txt".'
finish:
	close_file, lun
	return
	end
