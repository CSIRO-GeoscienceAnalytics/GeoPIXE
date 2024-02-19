function find_device, s, objects=objects, names=names

; Find an object called 's' 
; in a vector of device objects 'objects', or
; a list of device names 'names'

	if n_elements(objects) gt 0 then begin
		for i=0,n_elements(objects)-1 do begin
			if obj_valid(objects[i]) then begin
				if s eq objects[i]->name() then return, i
			endif
		endfor
	endif
	if n_elements(names) gt 0 then begin
		for i=0,n_elements(names)-1 do begin
			if s eq names[i] then return, i
		endfor
	endif
	
	return, -1
end
