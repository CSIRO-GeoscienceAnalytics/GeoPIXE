function struct_to_hash, s, lower=lower

	COMPILE_OPT STRICTARR	
	if n_elements(lower) eq 0 then lower=0
	if is_a_struct(s) eq 0 then return,s
	h = hash()
	tag = tag_names(s)

	for i=0,n_tags(s)-1 do begin
		tagi = lower ? strlowcase(tag[i]) : tag[i]
		case size(s.(i), /type) of
			8: begin
				h[ tagi] = struct_to_hash( s.(i), lower=lower)
				end
			else: begin
				h[ tagi] = s.(i)
				end
		endcase
	endfor
	return, h
end
