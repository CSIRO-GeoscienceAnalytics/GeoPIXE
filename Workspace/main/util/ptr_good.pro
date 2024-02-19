function ptr_good, p, struct=struct, all=all, depth=depth

; Test pointer 'p' for valid as well as containing data
; optionally test for it being a pointer to struct
; if /all test all members of an array, not just first
; Limit recurrrence depth to 10

	if n_elements(struct) eq 0 then struct=0
	if n_elements(all) eq 0 then all=0
	if n_elements(depth) eq 0 then depth=0

	np = n_elements(p)
	if np eq 0 then return, 0

	for i=0L,np-1 do begin
		if ptr_valid(p[i]) eq 0 then return, 0
		if n_elements( *p[i]) eq 0 then return, 0

		if struct then begin
			if size(*p[i], /tname) ne 'STRUCT' then return, 0
		endif

		if size(*p[i], /tname) eq 'POINTER' then begin
			if depth lt 10 then return, ptr_good(*p[i], depth=depth+1)
		endif
		if all eq 0 then break
	endfor

	return, 1
end
