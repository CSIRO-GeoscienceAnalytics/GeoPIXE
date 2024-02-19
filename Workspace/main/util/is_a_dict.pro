
; This detects a hash on return from Python ...

function is_a_dict, h

	COMPILE_OPT STRICTARR	
	if typename(h) eq 'DICTIONARY' then return,1
	return, 0
end
