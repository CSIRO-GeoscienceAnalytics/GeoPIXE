
; This detects a hash on return from Python ...

function is_a_list, h

	COMPILE_OPT STRICTARR	
	if typename(h) eq 'LIST' then return,1
	return, 0
end
