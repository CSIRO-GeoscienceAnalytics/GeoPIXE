function sizeof, x, follow=follow

; Function to determine the size of 'x' in bytes,
; will recursively descend into structures.
; Set /follow to also follow pointers and add their sizes recursively.

if n_elements(follow) lt 1 then follow=0

	number = size(x)
	number = number[number[0]+2]	; number of elements of 'x'

	type = size(x, /type)			; data type
	case type of
		0: n = 0					; undefined
		1: n = 1					; byte
		2: n = 2					; int (what about if 32bit INT is active?)
		3: n = 4					; long
		4: n = 4					; float
		5: n = 8					; double
		6: n = 8					; complex
		7: n = strlen(x)			; string
		8: goto, more				; struct
		9: n = 16					; dcomplex
		10: begin
			if follow then goto, pmore
			n = 4					; pointer
			end
		11: n = 4					; objref
		12: n = 2					; uint
		13: n = 4					; ulong
		14: n = 8					; long64
		15: n = 8					; ulong64
		else: n = 0
	endcase
	return, n * number

more:								; struct(s)
	tags = tag_names(x)
	n = 0LL
	for i=0L,n_elements(x)-1 do begin
		for j=0L,n_elements(tags)-1 do begin
			n = n + sizeof(x[i].(j))
		endfor
	endfor
	return, n
	
pmore:								; pointer(s)
	n = 0LL
	for i=0L,n_elements(x)-1 do begin
		n = n + sizeof(*x[i])
	endfor
	return, n
end
