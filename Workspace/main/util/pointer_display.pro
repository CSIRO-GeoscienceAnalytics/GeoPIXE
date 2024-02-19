pro pointer_display, p, unit=uniti, level=level, file=file, maxarray=maxarray, maxstruct=maxstruct

;	Display the contents of the pointer 'p' recursively.
;	Will display other data types as well.
;	
;	By default print to console.
;	If 'file' is specified, then open file for output, and close on exit.
;	If 'unit' is specified, print to this unit, and don't close it.
;	
;	'maxarray'	specifies the maximum number of elements to print for an array/vector.
;	'maxstruct'	specifies the maximum number of elements to print for a struct array.
;	'level'		is only for internal use (tab indent level).
;
;	C.G.Ryan (CSIRO), Sep, 2011

COMPILE_OPT STRICTARR
if n_elements(uniti) lt 1 then uniti=-1
unit = uniti
if unit le 0 then unit=-1
use_file = 0
if n_elements(file) gt 0 then begin
	openw, unit, file, /get_lun
	use_file = 1
endif
if n_elements(level) lt 1 then level=0
if n_elements(maxarray) lt 1 then maxarray=50
if n_elements(maxstruct) lt 1 then maxstruct=4

	space = ''
	if n_elements(p) lt 1 then begin
		if level gt 0 then space = strjoin( replicate('    ',level))
		printf, unit, space + 'Undefined'
		goto, done
	endif
	if level gt 0 then space = strjoin( replicate('    ',level))
	if size(p,/tname) eq 'POINTER'  then goto, ptr
	
	If size(p, /type) eq 8 then begin										; Handle structures
		for j=0L, (n_elements(p) < maxstruct)-1 do begin					; Deal with struct array
			tag = tag_names( p[j])
			if n_elements(p) gt 1 then begin
				printf, unit, space + 'struct ['+str_tidy(j)+' of '+str_tidy(n_elements(p))+'] {'
			endif else begin
				printf, unit, space + 'struct {'
			endelse
			for i=0L, n_tags( p[j] )-1 do begin								; for any members in this struct
				printf, unit, '    ' + space + strtrim(string(tag[i]),2)+':'
				if n_elements((p[j]).(i)) eq 0 then printf, unit, '        ' + space + 'struct member undefined'
				pointer_display, (p[j]).(i), unit=unit, level=level+2, $
					maxarray=maxarray, maxstruct=maxstruct					; call pointer_display recursively
		    endfor		    
			printf, unit, space + '}'
		endfor
	endif else if typename(p) eq 'HASH' then begin							; Handle Hash
		printf, unit, space + 'hash {'
		keys = p.keys()
		vals = p.values()
		for i=0L, n_elements(keys)-1 do begin								; for any members in this hash
			printf, unit, '    ' + space + strtrim(string(keys[i]),2)+':'
;			if n_elements(vals[i]) eq 0 then printf, unit, '        ' + space + 'hash member value undefined'
			pointer_display, vals[i], unit=unit, level=level+2, $
				maxarray=maxarray, maxstruct=maxstruct						; call pointer_display recursively
	    endfor		    
		printf, unit, space + '}'
	endif else if typename(p) eq 'ORDEREDHASH' then begin					; Handle Ordered Hash
		printf, unit, space + 'ordered hash {'
		keys = p.keys()
		vals = p.values()
		for i=0L, n_elements(keys)-1 do begin								; for any members in this hash
			printf, unit, '    ' + space + strtrim(string(keys[i]),2)+':'
;			if n_elements(vals[i]) eq 0 then printf, unit, '        ' + space + 'hash member value undefined'
			pointer_display, vals[i], unit=unit, level=level+2, $
				maxarray=maxarray, maxstruct=maxstruct						; call pointer_display recursively
	    endfor		    
		printf, unit, space + '}'
	endif else if typename(p) eq 'DICTIONARY' then begin					; Handle Dictionary
		printf, unit, space + 'dictionary {'
		keys = p.keys()
		vals = p.values()
		for i=0L, n_elements(keys)-1 do begin								; for any members in this hash
			printf, unit, '    ' + space + strtrim(string(keys[i]),2)+':'
;			if n_elements(vals[i]) eq 0 then printf, unit, '        ' + space + 'hash member value undefined'
			pointer_display, vals[i], unit=unit, level=level+2, $
				maxarray=maxarray, maxstruct=maxstruct						; call pointer_display recursively
	    endfor		    
		printf, unit, space + '}'
	endif else if typename(p) eq 'LIST' then begin							; Handle List
		printf, unit, space + 'list ['
		for i=0L, n_elements(p)-1 do begin									; for any members in this hash
;			if n_elements(p[i]) eq 0 then printf, unit, '        ' + space + 'list member value undefined'
			pointer_display, p[i], unit=unit, level=level+2, $
				maxarray=maxarray, maxstruct=maxstruct						; call pointer_display recursively
	    endfor		    
		printf, unit, space + ']'
	endif else begin

;		Take care with Python objects, as n_elements() tends to return the number of attributes, etc.
;		and not the number of Python objects.

		np = n_elements(p)
		if typename(p) eq 'PYTHON' then begin
			if np gt 1 then begin
				printf, unit, space + 'PYTHON object with ' + strtrim(string(np),2) + ' keys or attributes.'
			endif else begin
				printf, unit, space + 'PYTHON object.'
			endelse
		endif else begin
			help, output=s, p, /str
			str = strsplit(strjoin(s,' '),' 	',/extract)
			printf, unit, space + strjoin(str[1:*],' ')
			if (np gt 1) or (var_type(p) ge 110) then printf, unit, space+'   ', p[0:(np < maxarray)-1]
		endelse
	endelse
	goto, done

ptr:
	if level gt 0 then space = strjoin( replicate('    ',level))
	help, output=s, p, /str
	str = strsplit(strjoin(s,' '),' 	',/extract)
	printf, unit, space + strjoin(str[1:*],' ') + ' -->'
	level = level+1
	space = strjoin( replicate('    ',level))
	space1 = space

	for k=0L, (n_elements(p) < maxstruct)-1 do begin										; array of pointers
		if n_elements(p) gt 1 then begin
			printf, unit, space1 + 'pointer ['+strtrim(string(k),2)+'] -->'
			kup = 1
			space = strjoin( replicate('    ',level+kup))
		endif else begin
			kup = 0
		endelse
		if ptr_valid(p[k]) eq 0 then begin
			printf, unit, space + 'Null Pointer'											; null pointer
		endif else begin
			if n_elements(*p[k]) eq 0  then begin
				printf, unit, space + 'Pointer to empty heap'								; empty heap
			endif else begin
				pointer_display, *p[k], unit=unit, level=level+kup, $
								maxarray=maxarray, maxstruct=maxstruct						; call pointer_display recursively
			endelse
		endelse
	endfor

done:
	if use_file then close_file, unit
	return
end
