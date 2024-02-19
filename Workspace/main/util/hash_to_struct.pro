
;	This does work properly (as opposed to method in XML-to-Hash),
;	reentering to traverse a nested Hash

function hash_To_Struct, h
	COMPILE_OPT STRICTARR	

	if is_a_hash(h) eq 0 then return,h
	key = h.keys()
	s = 0
	if n_elements(key) eq 0 then return,s

	for i=0,n_elements(key)-1 do begin
		if is_a_hash(h[key[i]]) then begin
			d = hash_to_struct( h[key[i]])
		endif else if is_a_list(h[key[i]]) then begin
			d = list_to_normal( h[key[i]])
		endif else begin
			d = h[key[i]]
		endelse
		if d eq !NULL then d=0						; was a problem with key "KEY" equal to !NULL ?
		if n_elements(d) gt 0 then begin
			name = key[i]
			name = replace( '+', 'p', name)
			name = replace( '-', 'm', name)
			name = idl_validname( name, /convert_all)
			if i eq 0 then begin
				s = create_struct( name, d)
			endif else begin
				s = create_struct( s, name, d)
			endelse
		endif else print, 'hash_to_struct:  bad key ="' + key[i] + '"'
	endfor
	return, s
end
