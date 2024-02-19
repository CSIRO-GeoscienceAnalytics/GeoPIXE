	pro init_xray_lines, build_sort=build_sort
;
;	Read in X-ray mneumonics, line energy and relative intensities.
;
	common c_xray, energy, relint, xray_OK
	common c_working_dir2, geopixe_database_path

	xray_OK = 0
	if n_elements(build_sort) eq 0 then build_sort=0
	on_ioerror, bad
	openr, lun, geopixe_database_path+'dat/xray_lines.txt', /get_lun

	energy = fltarr(95,53)			; z from 0 to 94
	relint = fltarr(95,53)			; 52 line mneumonics

	str = ' '
	z = 0
	codes = strarr(3,6)
	while EOF(lun) eq 0 do begin
	    readf,lun,str
	    str = strtrim(strcompress(str),2)
	    if strlen(str) gt 0 then begin
			s = str_sep( str, ' ',/trim)
			ns = n_elements(s)
			if ns eq 1 then begin
				z = atomic_number(s[0])
			endif else begin
				codes[0:ns-1] = s
				if ns lt 18 then codes[ns:17] = ''
				q = where( strlen(codes[0,*]) gt 0)
				for j=0L,n_elements(q)-1 do begin
					k = line_index( codes[0,q[j]] )
					if k eq -1 then begin
						print,'init_xray_lines: unknown mneumonic [' + codes[0,q[j]] + ']'
					endif else begin
						energy[z,k] = float( codes[1,q[j]] )
						relint[z,k] = float( codes[2,q[j]] )
					endelse
				endfor
			endelse
		endif
	endwhile
	xray_OK = 1
	goto, finish

bad:
	print,'init_xray_lines: Error reading "xray_lines.txt".'
finish:
	close_file, lun

	if xray_ok then begin
		zt = indgen(94)+1
		relint[zt,38] = auger_rel_intensity(zt,1)
		relint[zt,39] = auger_rel_intensity(zt,2)
	endif
	
	if build_sort then begin
		openw, lun, 'new_sorted_xray_lines.txt', /get_lun
		
		z = intarr(95,53)
		line = strarr(95,53)
		ok = intarr(95,53)
		for i=0L,94 do begin
			line[i,*] = line_id( indgen(53))
			z[i,*] = i
			list = list_line_index(i,0,/nosort)
			ok[i,list] = 1
		endfor
		q1 = where( ok eq 1, nq)
;		q1 = where(energy gt 0.2,nq)
		q = sort(energy[q1])
		printf, lun, nq
		for j=0L, nq-1 do begin
			k = q1[q[j]]
			printf, lun, energy[k], element_name(z[k]), line[k], relint[k], $
				format='(F10.4,", ",A2,", ",A6,", ",F9.4)'
		endfor
		close_file, lun
		print,'"new_sorted_xray_lines.txt" table generated.'
	endif
	return
	end
