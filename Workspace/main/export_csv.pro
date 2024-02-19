pro export_csv, pp, name, fits=fits, append=append, size=siz, all=all, offset=offset

;   Export all valid spectra in a comma-separated ascii file
;   /fits	only export first spectrum, but also all its overlays
;	siz		just export this many channels
;	/append	append the output to the file 'name' (without headings added).
;	/all	export all spectra
;	offset	distance, energy, time axis offset/origin

    if n_elements(fits) lt 1 then fits=0
    if n_elements(append) lt 1 then append=0
    if file_test(name) eq 0 then append=0
    if n_elements(all) lt 1 then all=0
    if n_elements(offset) lt 1 then offset=0.0d+0

    on_ioerror, finish
	openw, lun, name, /get_lun, append=append

    has_errors = 0
    np = 0
	for i=0L,n_elements(*pp)-1 do begin
		if ptr_valid( (*pp)[i]) then begin
			if (*(*pp)[i]).show or all then begin
				if np eq 0 then begin
					p = (*pp)[i]
					length = (*p).size
					if n_elements(siz) gt 0 then length = length < siz
					label = fix_file_name( strtrim((*p).label,2), /all,/blank)
					if (*p).has_errors then has_errors=1
					np = 1
					if fits then begin
						nf = (*(*pp)[i]).n_fit
						if nf lt 1 then goto, cont
						for j=0L,nf-1 do begin
							p = [p, (*(*pp)[i]).fit[j] ]
							np = np+1
							len = (*(*(*pp)[i]).fit[j]).size
							if n_elements(siz) gt 0 then len = len < siz
							length = [length, len]
							label = [label, fix_file_name( strtrim((*(*(*pp)[i]).fit[j]).label,2), /all,/blank) ]
						endfor
						goto, cont
					endif
				endif else begin
					p = [p, (*pp)[i] ]
					np = np+1
					len = (*(*pp)[i]).size
					if n_elements(siz) gt 0 then len = len < siz
					length = [length, len]
					label = [label, fix_file_name( strtrim((*(*pp)[i]).label,2), /all,/blank) ]
				endelse
			endif
		endif
	endfor

cont:
	if has_errors and (fits eq 0) then begin
		data = fltarr( 2,max(length),np)
	endif else begin
		data = fltarr( 1,max(length),np)
	endelse

	a = (*p[0]).cal.poly[1]
	b = (*p[0]).cal.poly[0]
	if ptr_good( (*p[0]).px_coords) then begin
		distance = *(*p[0]).px_coords
		units = (*p[0]).x_coord_units
	endif else begin
		distance = offset + double(b + indgen( max(length)) * a)
		units = (*p[0]).cal.units
	endelse
	for i=0L,np-1 do begin
;		t = strsplit((*p[i]).label, /extract)
;		n = n_elements(t)
;		label[i] = t[n-1]
;		if has_errors then label[i] = label[i] + ',e_' + t[n-1]

		if has_errors and (fits eq 0) then label[i] = label[i] + ',e_' + label[i]
		data[0,0:length[i]-1,i] = (*(*p[i]).data)[0:length[i]-1] 	     ;> 0.0
		
		if (*p[i]).has_errors and (fits eq 0) then begin
			data[1,0:length[i]-1,i] = (*(*p[i]).error)[0:length[i]-1]    ;> 0.0
		endif
	endfor

	if append eq 0 then printf, lun, units, label, format='(A,",",'+string(np)+'(A,:,","))'
	for i=0L,max(length)-1 do begin
		printf, lun, distance[i], data[*,i,*], format='(G,",",'+string(2*np)+'(G,:,","))'
	endfor

finish:
    close_file, lun
    return
end
