
; This detects a hash on return from Python ...

function is_a_hash, h

	COMPILE_OPT STRICTARR	
	if typename(h) eq 'ORDEREDHASH' then return,1
	if typename(h) eq 'HASH' then return,1
	return, 0
end
