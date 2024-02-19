pro maia_chart_add, pc, namei, value

; If (*pc).n=0 then
; Add values to the chart log record stored in structures
; {name:'name text',val:value} pointed to by
; an array of pointers pointed to by (*(*pc).p)[nlog] 
; where nlog is given by (*pc).n.
;
; else match name amongst names for the first log and save value
; in same position in value list. If new values are added (not in the first)
; then need to add these names to the first one (value=0).

n = (*pc).n
p = (*pc).p[n]					; p = p0 first time through
p0 = (*pc).p[0]
nn = n_elements(value)
file = (*pc).file
name = namei
if n_elements(name) lt nn then name=replicate(name,nn)

init = 0
if ((*pc).n eq 0) and ((*pc).init eq 0) then begin
	init=1
	if file_test(file) then begin
		on_ioerror, bad_open
		openr, lun, file, /get_lun
		on_ioerror, bad_read
		line = ''
		readf, lun, line
		str = strsplit(line, ',', /extract)
		ns = n_elements(str)
		if ns le 1 then goto, cont
		
		; This code to extend ptr array and initialize struct also in maia_update_parameters3
		; and Maia_launch after 'state' set-up.
		
		if ns-1 gt n_elements((*p0).p) then begin
			p1 = ptrarr(ns-1-n_elements((*p0).p), /allocate_heap)
			for k=0L,ns-1-n_elements((*p0).p)-1 do *p1[k] = {name:'', index:0, val:0.0}
			pnew = [(*p0).p, p1]
			*p0 = {n:(*p0).n, time:(*p0).time, p:pnew}
		endif

		for i=1L, ns-1 do begin
			s = str[i]
			index = 0
			if locate('[',s) ge 0 then begin
				s2 = strsplit(s,'[]',/extract)
				n2 = n_elements(s2)
				if n2 ge 2 then begin
					s = s2[0]
					index = fix(s2[n2-1])
				endif
			endif
			*(*p0).p[i-1] = {name:s, index:index, val:0.0}
		endfor	
		(*p0).n = ns-1
		init = 0
		(*pc).init = 1
		goto, cont
		
bad_open:
		goto, cont
bad_read:
		goto, cont
cont:	
		close_file, lun
	endif
endif		

if init then begin
	if (*p).n + nn gt n_elements((*p).p) then begin
		p1 = ptrarr((*p).n + nn - n_elements((*p).p),/allocate_heap)
		for k=0L,(*p).n + nn - n_elements((*p).p)-1 do *p1[k] = {name:'', index:0, val:0.0}
		pnew = [(*p).p, p1]
		*p = {n:(*p).n, time:(*p).time, p:pnew}
	endif
	for i=0L, nn-1 do begin
		*(*p).p[(*p).n] = {name:name[i], index:i, val:value[i]}
		(*p).n = (*p).n + 1
	endfor
endif else begin
	found = 0
	if n_elements((*p0).p) gt n_elements((*p).p) then begin
		p1 = ptrarr(n_elements((*p0).p) - n_elements((*p).p),/allocate_heap)
		for k=0L,n_elements((*p0).p)-n_elements((*p).p)-1 do *p1[k] = {name:'', index:0, val:0.0}
		pnew = [(*p).p, p1]
		*p = {n:(*p).n, time:(*p).time, p:pnew}
	endif
	(*p).n = (*p0).n
	for j=0L,(*p0).n-1 do begin
		if ((*(*p0).p[j]).index eq 0) and ((*(*p0).p[j]).name eq name[0]) then begin
			for i=0L, (nn < ((*p).n-j))-1 do begin
				*(*p).p[j+i] = {name:name[i], index:i, val:value[i]}
			endfor	
			found = 1
			break		
		endif
	endfor
	if found eq 0 then begin
		if (*p0).n + nn gt n_elements((*p0).p) then begin
			p1 = ptrarr((*p0).n + nn - n_elements((*p0).p),/allocate_heap)
			for k=0L,(*p0).n + nn - n_elements((*p0).p)-1 do *p1[k] = {name:'', index:0, val:0.0}
			pnew = [(*p0).p, p1]
			*p0 = {n:(*p0).n, time:(*p0).time, p:pnew}
		endif
		if n_elements((*p0).p) gt n_elements((*p).p) then begin
			p1 = ptrarr(n_elements((*p0).p) - n_elements((*p).p),/allocate_heap)
			for k=0L,n_elements((*p0).p)-n_elements((*p).p)-1 do *p1[k] = {name:'', index:0, val:0.0}
			pnew = [(*p).p, p1]
			*p = {n:(*p).n, time:(*p).time, p:pnew}
		endif
		for i=0L, nn-1 do begin
			*(*p).p[(*p).n] = {name:name[i], index:i, val:value[i]}
			(*(*p0).p[(*p).n]).name = name[i]
			(*(*p0).p[(*p).n]).index = i
			(*p0).n = (*p0).n + 1
			(*p).n = (*p0).n
		endfor
	endif
endelse
return
end
