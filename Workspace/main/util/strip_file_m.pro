function strip_file_m, filei, ending=ending, element=element
;
; Remove "-m" (or specified ending) at end;
;
; If /element, then remove trailing element name "-el", where 'el'
; may contain a trailing "K", "L" or "M".

if n_elements(filei) lt 1 then return, ''
if n_elements(ending) lt 1 then ending='-m'
if n_elements(element) lt 1 then element=0
file = filei

add_ext = 0
if locate('.',file[0]) ge 0 then begin
	ext = extract_extension(file)
	file = strip_file_ext(file)
	if ext ne '' then add_ext = 1
endif

if element then begin
	n = strlen(file[0])
	f = file[0]
	n2 = locate_last('-',f)
	if (n2 ge 0) and (n2 lt n-1) then begin
		el = extract(f,n2+1,n-1)
		nel = strlen(el)
		sl = extract(el,nel-1,nel-1)
		if ((sl eq 'L') or (sl eq 'M') or (sl eq 'K')) and (nel ge 2) then begin
			el = extract(el,0,nel-2)
		endif
		if atomic_number(el) ge 1 then begin
			f = extract(f,0,n2-1)
		endif
	endif
endif else begin
	f = file
	for j=0,n_elements(f)-1 do begin
		for i=0L,n_elements(ending)-1 do begin
			n2 = strlen(ending[i])
			n = strlen(f[j])
			if n lt n2 then goto, finish
	
			done = 0
			while( done eq 0) do begin
				s = strmid(f[j],n-n2,n2)
	
				if str_equal(s,ending[i],/ignore_case) then begin
					f[j] = strmid(f[j],0,n-n2)
				endif else begin
					done = 1
				endelse
			endwhile
		endfor
	endfor
endelse

finish:
if add_ext then begin
	f = f + '.' + ext
endif
return, f
end
