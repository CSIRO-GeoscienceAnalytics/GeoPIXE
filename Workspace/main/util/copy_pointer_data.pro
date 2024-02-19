pro copy_pointer_data, psrc, pdest, init=init, tag=tag, notag=notag

;	Copy *psrc data to *pdest. Recursively navigate and copy all embedded 
;	pointer data to new pointers in destination.
;	Assumes both are set-up already and have the same structure.
;
;	Does NOT handle Lists, Hash, OrderedHash or Dictionary data-types.
;	Assumes all /TYPE 11 data are ObjRef.
;		
;	/init	create destination as we go and copy in source
;			and create embedded pointers afresh using ptr_new(),
;			so no pointers in dest use the same heap as in src.
;			
;	tag		for pointers to structs, only copy the named tags.
;			'tag' supplies the name(s) of the tags as strings/array.
;			Cannot be used with /init.
;			
;	notag	do not copy these tags (at top level only). Can be used in /init.
;			
;	Use 'pointer_display' to display pointer data.
;
;	NOTE:	If you extend a struct, do it at the end, else the tests below
;			of datatype on same tag index as src in dest will fail.

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) eq 0 then catch_errors_on=1
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'copy_pointer_data',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(psrc) lt 1 then goto, bad
;if ptr_valid(psrc[0]) eq 0 then goto, bad
if n_elements(init) lt 1 then init=0

nsrc = n_elements(psrc)
if nsrc gt 1 then begin														; array of pointers
	if init then begin
		pdest = ptrarr(nsrc)
	endif else begin
		if n_elements(pdest) lt nsrc then goto, bad2
	endelse

	for i=0,nsrc-1 do begin
		if ptr_valid(psrc[i]) then begin
			if (init eq 0) and (ptr_valid(pdest[i]) eq 0) then goto, bad2
			t = pdest[i]
			copy_pointer_data, psrc[i], t, init=init, tag=tag, notag=notag				; call each separately
			pdest[i] = t
		endif
	endfor
	return
endif

if n_elements(tag) ge 1 then begin
	if init then goto, bad0
	if n_elements(*psrc) eq 0 then goto, bad1
	if size(*psrc, /TYPE) ne 8 then goto, bad1b
	if n_elements(pdest) lt 1 then goto, bad2
	if ptr_valid(pdest) eq 0 then goto, bad2
	if n_elements(*pdest) eq 0 then goto, bad1c
	if size(*pdest, /TYPE) ne 8 then goto, bad1d
	
	stags = tag_names( *psrc)
	dtags = tag_names( *pdest)
	for i=0L,n_elements(tag)-1 do begin
		jsrc = -1L
		jdest = -1L
		for j=0L, n_elements(stags)-1 do begin
			if strupcase(tag[i]) eq stags[j] then jsrc = j
		endfor

		for j=0L, n_elements(dtags)-1 do begin
			if strupcase(tag[i]) eq dtags[j] then jdest = j
		endfor
	
		if (jsrc ge 0) and (jdest ge 0) then begin
			ps = ptr_new( (*psrc).(jsrc))
			pd = ptr_new( (*pdest).(jdest))
			copy_pointer_data, ps, pd
			(*pdest).(jdest) = *pd
			ptr_free, ps
			ptr_free, pd
		endif
	endfor
	return
endif

if init then begin
	pdest = ptr_new()
	if ptr_valid(psrc) then begin
		pdest = ptr_new(/allocate_heap)
		if n_elements(*psrc) gt 0 then *pdest = *psrc						; clone pointer and data (still old pointers)
	endif else if obj_valid(psrc) then begin
		pdest = clone_object( psrc, error=err)								; clone object ref and data
		if err then goto, bad6b
	endif else return
endif else begin
	if n_elements(pdest) lt 1 then goto, bad2
	if ptr_valid(pdest) eq 0 then goto, bad2
endelse

case (size(*psrc, /TYPE)) of
	8:  begin																; Handle structures recursively.
		if (size(*pdest, /TYPE)) ne 8 then begin
			if n_elements(*psrc) gt 0 then *pdest = *psrc					; psrc must be new, copy it over
		endif
		for j=0L, n_elements(*psrc)-1 do begin								; Deal with struct array
		
			old = (*pdest)[j]												; save old dest struct (to keep old pointer components)
			new = old														; copy struct across (but this copies pointer						
			struct_assign, (*psrc)[j], new, /nozero, /verbose				; address literally, which is not desired)
			dtags = tag_names( (*pdest)[j])
			stags = tag_names( (*psrc)[j])									; tag names
			
			for i=0L, n_tags( (*psrc)[j] )-1 do begin						; loop to copy across pointer/obj contents
				k = where( stags[i] eq dtags, nk)							; find matching tag name in destination
				if n_elements(notag) gt 0 then begin						; skip tags in 'notag' list
					m = where( stags[i] eq strupcase(notag), nm)
				endif else nm=0
				if (nk gt 0) and (nm eq 0) then begin
					case size( ((*psrc)[j]).(i), /TYPE) of
						8: begin												; for any structs in this struct
							if size( old.(k[0]), /TYPE) ne 8 then goto, bad4b
							t1 = ptr_new(((*psrc)[j]).(i))
							t = ptr_new( old.(k[0]))							; copy pointer contents in struct to new pointer
							copy_pointer_data, t1, t, init=init					; and create new pointer if needed (/init)
							new.(k[0]) = *t
							ptr_free, t
							ptr_free, t1
							end
						10: begin												; for any pointers in this struct
							if size( old.(k[0]), /TYPE) ne 10 then goto, bad4
							t = old.(k[0])
							copy_pointer_data, ((*psrc)[j]).(i), t, init=init	; and create new pointer if needed (/init)
							new.(k[0]) = t
							end
						11: begin												; Handle object refs
							if (size( old.(k[0]), /TYPE)) ne 11 then goto, bad6
							t = old.(k[0])
							copy_object_data, ((*psrc)[j]).(i), t, init=init, error=err	; copy object data
							if err then goto, bad6b
							new.(k[0]) = t
						    end
						else:
					endcase
				endif
		    endfor		    
		    (*pdest)[j] = new												; copy correct new struct to dest
		endfor
	    end
	10: begin																; Handle pointers recursively.
		if (size(*pdest, /TYPE)) ne 10 then goto, bad5
		for i=0L, n_elements(*psrc)-1 do begin								; Deal with pointer array
			if ptr_valid( (*psrc)[i]) then begin
				if n_elements( *(*psrc)[i]) gt 0 then begin
					new = (*pdest)[i]
					copy_pointer_data, (*psrc)[i], new, init=init			; use copy_pointer_data recursively
					(*pdest)[i] = new
				endif
			endif
	    endfor
	    end
	11: begin																; Handle object refs
		if (size(*pdest, /TYPE)) ne 11 then goto, bad6
		for i=0L, n_elements(*psrc)-1 do begin								; Deal with object array
			if obj_valid( (*psrc)[i]) then begin
				new = (*pdest)[i]
				copy_object_data, (*psrc)[i], new, init=init, error=err		; copy object data
				if err then goto, bad6b
				(*pdest)[i] = new
			endif
	    endfor
	    end
	else: begin
		if n_elements(*psrc) ge 1 then *pdest = *psrc						; other data types
		end		
ENDCASE
return

bad:
	message = 'Source pointer data not found.'
	goto, done
bad0:
	message = 'Cannot use "/init" with "Tag" option.'
	goto, done
bad1:
	message = 'Source pointer to null only.'
	goto, done
bad1b:
	message = 'Source does not point to "struct"; incompatible with "TAG" option.'
	goto, done
bad1c:
	message = 'Dest pointer to null only.'
	goto, done
bad1d:
	message = 'Dest does not point to "struct"; incompatible with "TAG" option.'
	goto, done
bad2:
	message = 'Destination pointer not valid.'
	goto, done
bad3:
	message = 'Destination type does not match source "struct" type.'
	goto, done
bad4:
	message = 'Destination type in struct does not match source "pointer" type.'
	goto, done
bad4b:
	message = 'Destination type in struct does not match source "struct" type.'
	goto, done
bad5:
	message = 'Destination type does not match source "pointer" type.'
	goto, done
bad6:
	message = 'Destination type does not match source "object" type.'
	goto, done
bad6b:
	message = 'Error cloning "object".'
	goto, done
done:
	help, calls = s
	n = n_elements(s)
	c = 'Call stack: '
	if n gt 2 then c = [c, s[1:n-2]]
	warning,'copy_pointer_data',[message,'',c]
	return
end
