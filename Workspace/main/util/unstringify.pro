function unstringify, si, transpose=transpose, did_transpose=did_transpose, $
							context=context, error=error

; Take a 'stringify(p, /embed_ptr)' string and convert it back into a complete structure.
;	error		error=1 return
;	context		a context label, where unstringify is being used.
;	other args are for internal (recursive) use only.
;
; Chris Ryan (CSIRO), 2009, revised 2012, 2018
;		Added List, Hash (and orderedhash, dictionary)  2019
;		Embedded Hash (and orderedhash, dictionary) decode as Struct.

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
       warning,'unstringify',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c,'','unstringify arg:',si], /error
       MESSAGE, /RESET
       return,'--ERROR('+si+')--'
    endif
endif

error = 1
if n_elements(transpose) eq 0 then transpose=0
if n_elements(context) eq 0 then context=''
if n_elements(did_transpose) eq 0 then did_transpose=0
if n_elements(si) lt 1 then return,''
if size(si, /tname) ne 'STRING' then goto, bad_string
s0 = si[0]
if strlen(s0) gt 30 then s0 = strmid(s0,0,20) + '...'
if n_elements(si) gt 1 then goto, bad_syntax2
n = strlen(si)
if n lt 1 then return,''
x = 0

s = strtrim(si,2)
n = strlen(s)
c1 = strupcase(strmid(s,0,1))					; first character
cn = strupcase(strmid(s,n-1,1))					; last character

; Ensure whole words are used to encode Complex and Double

if (c1 eq 'C') then begin
	if strupcase(strmid(s,0,7)) ne 'COMPLEX' then c1='?'
endif
if (c1 eq 'D') then begin
	if strupcase(strmid(s,0,6)) ne 'DOUBLE' then c1='?'
endif

case c1 of
	'{': begin									; struct and (Hash, OrderedHash, Dictionary)
		if cn ne '}' then goto, bad_syntax1
		if n gt 2 then begin
			i1 = 1
			i2 = n-2
			if strmid( s,i1,1) eq '{' then begin			; also accept double {}
				i1 = i1+1
				if strmid( s,i2,1) eq '}' then begin
					i2 = i2-1
				endif else goto, bad_syntax1
			endif
			n1 = i2-i1+1
			t0 = strmid( s, i1, n1)
			t = hide_embedded( t0, ',')
			str = strtrim( strsplit( t, ',', /extract),2)
			ns = n_elements(str)
			if ns lt 1 then return, ''
			tag = strarr(ns)
			first = 1
			for i=ns-1,0,-1 do begin
				t2 = hide_embedded( hide_embedded( str[i], ',', /unhide), ':')
				s2 = strsplit( t2, ':', /extract)
				ns2 = n_elements(s2)
				if ns2 eq 2 then begin
					xt = unstringify( hide_embedded( s2[1], ':', /unhide), context=context)
					s3 = s2[0]
					case strmid(s3,0,1) of
						'"': begin					 						; hash
							if first then begin
								x = hash( strip_char(s3,'"'), xt)
								first = 0
							endif else begin
								x = hash( strip_char(s3,'"'), xt) + x
							endelse	
							end
						'%': begin					 						; ordered hash
							if first then begin
								x = orderedhash( strip_char(s3,'%'), xt)
								first = 0
							endif else begin
								x = orderedhash( strip_char(s3,'%'), xt) + x
							endelse	
							end
						'$': begin					 						; dictionary
							if first then begin
								x = dictionary( strip_char(s3,'$'), xt)
								first = 0
							endif else begin
								x = dictionary( strip_char(s3,'$'), xt) + x
							endelse	
							end
						else: begin											; struct
							if first then begin
								x = create_struct(strcompress(s3,/remove_all),xt)
								first = 0
							endif else begin
								x = create_struct(strcompress(s3,/remove_all),xt,x)
							endelse
							end
					endcase
				endif
			endfor
		endif else goto, bad_syntax3
		end
	'[': begin									; vector (or List)
		if cn ne ']' then goto, bad_syntax1
		if n gt 2 then begin
			i1 = 1
			i2 = n-2
			n1 = i2-i1+1
			t1 = strmid( s, i1, n1)
			t = hide_embedded( t1, ',')
			str = strtrim(strsplit( t, ',', /extract),2)
			ns = n_elements(str)
			if ns lt 1 then return, ''
			first = 1
			for i=0L,ns-1 do begin
				did_transpose = 0
				xt = unstringify( hide_embedded( str[i], ',', /unhide), transpose=1-transpose, did_transpose=did_transpose, context=context)
				if first then begin
					x = xt
					first = 0
				endif else begin
					x = vector_merge( x, xt)					; vector or may become List
				endelse
			endfor
			if (size(x,/type) ne 11) and (size(x,/type) ne 8) then begin	; idl does not allow the transpose
				if did_transpose then x = transpose(x)						; of an array containing structs.
				did_transpose = transpose
				if transpose then x=transpose(x)
			endif
		endif else goto, bad_syntax3
		end
	'"': begin									; string
		if cn ne '"' then goto, bad_syntax1
		x = ''
		if n gt 2 then begin
			i1 = 1
			i2 = n-2
			n1 = i2-i1+1
			x = strmid( s, i1, n1)
		endif
		end
	"'": begin									; string
		if cn ne "'" then goto, bad_syntax1
		x = ''
		if n gt 2 then begin
			i1 = 1
			i2 = n-2
			n1 = i2-i1+1
			x = strmid( s, i1, n1)
		endif
		end
	'C': begin									; complex
		if cn ne ')' then goto, bad_syntax1
		i1 = strpos( s, '(')
		if i1 lt 0 then goto, bad_syntax1
		i1 = i1+1
		i2 = n-2
		n1 = i2-i1+1
		x = complex(0.,0.)
		if n1 ge 1 then begin
			t = strmid( s, i1, n1)
			str = strtrim(strsplit( t, ',', /extract),2)
			ns = n_elements(str)
			if ns eq 1 then x = complex( unstringify(str[0], context=context), 0.)
			if ns eq 2 then x = complex( unstringify(str[0], context=context), unstringify(str[1], context=context))
		endif
		end
	'D': begin									; Dcomplex
		if cn ne ')' then goto, bad_syntax1
		i1 = strpos( s, '(')
		if i1 lt 0 then goto, bad_syntax1
		i1 = i1+1
		i2 = n-2
		n1 = i2-i1+1
		x = dcomplex(0.,0.)
		if n1 ge 1 then begin
			t = strmid( s, i1, n1)
			str = strtrim(strsplit( t, ',', /extract),2)
			ns = n_elements(str)
			if ns eq 1 then x = dcomplex( unstringify(str[0], context=context), 0.)
			if ns eq 2 then x = dcomplex( unstringify(str[0], context=context), unstringify(str[1], context=context))
		endif
		end
	'P': begin									; pointer
		if cn ne ')' then goto, bad_syntax1
		i1 = strpos( s, '(')
		if i1 lt 0 then goto, bad_syntax1
		i1 = i1+1
		i2 = n-2
		n1 = i2-i1+1
		x = ptr_new()
		if n1 ge 1 then begin
			t = strmid( s, i1, n1)
			t = hide_embedded( t, ',')
			str = strtrim(strsplit( t, ',', /extract),2)
			ns = n_elements(str)
			no_copy = 0
			allocate_heap = 0
			for i=0L,ns-1 do begin
				s1 = hide_embedded( str[i], ',', /unhide)
				st = strupcase(strmid(s1,0,2))
				case st of
					'/N': no_copy = 1
					'/A': allocate_heap = 1
					else: 
				endcase
			endfor
			s1 = hide_embedded( str[0], ',', /unhide)
			if strmid(s1,0,1) ne '/' then begin
				x = ptr_new( unstringify(s1, context=context), no_copy=no_copy)
			endif else begin
				x = ptr_new( allocate_heap=allocate_heap)
			endelse
		endif
		end
	'O': begin									; object
		if cn ne ')' then goto, bad_syntax1
		i1 = strpos( s, '(')
		if i1 lt 0 then goto, bad_syntax1
		i1 = i1+1
		i2 = n-2
		n1 = i2-i1+1
		x = ptr_new()
		if n1 ge 1 then begin
			t = strmid( s, i1, n1)
			t = strip_char(t,'"')
			m = strlen(t)
			if m ge 1 then begin
				x = obj_new(t)
			endif else begin
				x = obj_new()
			endelse
		endif
		end
	else: begin
		if gnumeric(s) then begin					; number
			x = 0
			if inumeric(s) then begin
				x = long2(s)
			endif else if fnumeric(s) then begin
				y = float2(s)
				b = (y lt 0.) ? 1 : 0							; allow for sign
				y = (y eq 0.) ? 1.0 : 1./abs(y)
				m = strlen( strtrim(string(long2( y)),2)) > 1	; allow for zeroes after decimal points
				if n gt m+8+b then begin
					x = double2(s)
				endif else begin
					x = float2(s)
				endelse
			endif else if gnumeric(s) then begin
				y = float2(s)
				b = (y lt 0.) ? 1 : 0							; allow for sign
				if n gt 12+b then begin
					x = double2(s)
				endif else begin
					x = float2(s)
				endelse
;			endif else if s eq 'Inf' then begin
;				x = !values.f_infinity
			endif
		endif else begin						; just assume it's a simple string
			error = 0
			return, s
		endelse
		end
endcase

if n_elements(x) eq 1 then x=x[0]
error = 0
return, x

bad_syntax1:
	print,'unstringify: ','bad syntax, unbalanced container "'+s0+'", Context: '+context
	return, 0
bad_syntax2:
	print,'unstringify: ','bad syntax, non-scaler string supplied ["'+s0+'", ...], Context: '+context
	return, 0
bad_syntax3:
	print,'unstringify: ','bad syntax, missing tokens or values, Context: '+context
	return, 0
bad_string:
	print,'unstringify: ','bad string supplied, Context: '+context
	return, 0
end
