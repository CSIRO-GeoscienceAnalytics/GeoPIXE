
; This detects a struct ...

function is_a_struct, h

	COMPILE_OPT STRICTARR	
	if size(h, /tname) eq 'STRUCT' then return,1
	return, 0
end
