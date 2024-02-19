function vector_merge, xi, yi

; If x,y are compatible, then combine as new vector 'x'
; If necessary, upgrade type of 'x' to cope with 'y'.
; If incompatible, then convert to Lists first.
; If both structs, then use soft struct_assign.

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
       warning,'vector_merge',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return, xi
    endif
endif

	x = xi
	y = yi
	compat = [1,2,12,3,13,14,15,4,5,6,9,7]				; these can be combined in a vector
	rank =   [1,3,2,5,4,7,6,8,9,10,11,12]
	
	qx = where( var_type(x) eq compat, nqx)
	qy = where( var_type(y) eq compat, nqy)
	
	if (nqx gt 0) and (nqy gt 0) then begin				; vector
		r = max( [rank[qx[0]], rank[qy[0]]] )
		case r of
			12: x = strtrim(string(x),2)
			11: x = dcomplex(x)							; in order of decreasing precision precedence
			10: x = complex(x)
			9: x = double(x)
			8: x = float(x)
			7: x = long64(x)
			6: x = ulong64(x)
			5: x = long(x)
			4: x = ulong(x)
			3: x = fix(x)
			2: x = uint(x)
			7: x = long(x)
			1: x = byte(x)
			else:
		endcase
		x = [x,y]										; concatenate into vectors

	endif else if (var_type(x) eq 8) and (var_type(y) eq 8) then begin		; Struct arrays
		t = x[0]
		struct_assign, y, t								; soft/forgiving assignment
		y = t
		x = [x,y]										; concatenate struct vectors

	endif else if (var_type(x) eq 10) and (var_type(y) eq 10) then begin		; pointer arrays

		x = [x,y]										; concatenate pointers into vector

	endif else if (var_type(x) eq 114) and (var_type(y) eq 114) then begin		; object arrays

		x = [x,y]										; concatenate objects into vector

	endif else begin									; List
		if var_type(x) ne 110 then begin
			x = list(x)
		endif
		if var_type(y) ne 110 then begin				; if not a LIST make it a LIST
			y = list(y)
		endif
		x = x+y											; concatenate Lists
	endelse
		
	return, x
end
