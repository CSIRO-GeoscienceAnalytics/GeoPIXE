; function stringify, t, embed_ptr=embed_ptr
;
; Expand the contents of 't' as a single string scaler.
; Use recursion to dive into components
; /embed_ptr	embed ptr_new() functions to create the pointers when used
;				else place pointer contents in-line.
;
; Chris Ryan (CSIRO), 2009, revised 2012
;		Added List, Hash (and orderedhash, dictionary)  2019

function stringify_part, t, embed_ptr=embed_ptr

; Used as part of the stringify function (see below) to decode a scaler/vector of single type
; Returns a scaler/vector string array/scaler

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
       warning,'stringify_part',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return,'--ERROR--'
    endif
endif

if embed_ptr lt 1 then embed_ptr=0
n = n_elements(t)
if (typename(t) eq 'HASH') or (typename(t) eq 'ORDEREDHASH') or (typename(t) eq 'DICTIONARY') then n=1										; parts of Hash treated below
if n lt 1 then return,''

case var_type(t) of
	1:	return, strtrim(string(t,format='(I)'),2)						; BYTES return value as string not char
	2:  return, strtrim(string(t,format='(I)'),2)						; 16-bit signed integers
	12: return, strtrim(string(t,format='(I)'),2)						; Unsigned shorts
	3:  return, strtrim(string(t,format='(I)'),2)						; 32-bit signed long integers
	13: return, strtrim(string(t,format='(I)'),2)						; Unsigned Longs
	14: return, strtrim(string(t,format='(I)'),2)						; Signed 64-bit integers
	15: return, strtrim(string(t,format='(I)'),2)						; Unsigned 64-bit integers
	4:  return, strip_trail_zero(string(t,format='(G)'),places=-6)		; Single floats
	5:  return, strip_trail_zero(string(t,format='(G)'),places=-14)		; Double floats
	6:  return, 'Complex' + strcompress(string(t),/remove_all)			; Single complex
	9:  return, 'DComplex' + strcompress(string(t),/remove_all)			; Double complex
	7:  return, '"' + t + '"'											; String
	8:  begin
			t1 = strarr(n)
			for i=0L, n-1 do begin
				t2 = '{'												; Handle structures recursively.
				tag = tag_names(t)
				for j=0L, n_tags(t)-1 do begin	
					t3 = tag[j] + ':' + stringify( t[i].(j),embed_ptr=embed_ptr)
					if j gt 0 then t2 = t2 + ','
					t2 = t2 + t3
				endfor
				t1[i] = t2 + '}'
			endfor
		    return, t1
			end
	10: begin
			t1 = strarr(n)
			for i=0L, n-1 do begin										; Handle pointers recursively.
				if ptr_valid(t[i]) then begin
					if n_elements(*t[i]) gt 0 then begin
						t2 = stringify( *t[i],embed_ptr=embed_ptr)
						t1[i] = (embed_ptr ? 'ptr_new('+t2+',/no_copy)': t2)
					endif else t1[i] = (embed_ptr ? 'ptr_new(/allocate_heap)' : '0L')
				endif else begin
					t1[i] = embed_ptr ? 'ptr_new()' : '0L'
				endelse
		    endfor
		    return, t1
		    end
	110:  begin
			t2 = ''
			for i=0L, n-1 do begin										; Handle Lists recursively.
				t3 = stringify( t[i],embed_ptr=embed_ptr)
				if i gt 0 then t2 = t2 + ','
				t2 = t2 + t3
			endfor
		    return, t2
			end
	111: begin
			t1 = strarr(n)
			for i=0L, n-1 do begin
				t2 = '{'												; Handle Hash recursively.
				key = t.keys()
				for j=0L, n_elements(key)-1 do begin	
					t3 = '"' + key[j] + '"' + ':' + stringify( t[key[j]],embed_ptr=embed_ptr)
					if j gt 0 then t2 = t2 + ','
					t2 = t2 + t3
				endfor
				t1[i] = t2 + '}'
			endfor
		    return, t1
			end
	112: begin
			t1 = strarr(n)
			for i=0L, n-1 do begin
				t2 = '{'												; Handle Ordrered Hash recursively.
				key = t.keys()
				for j=0L, n_elements(key)-1 do begin	
					t3 = '%' + key[j] + '%' + ':' + stringify( t[key[j]],embed_ptr=embed_ptr)
					if j gt 0 then t2 = t2 + ','
					t2 = t2 + t3
				endfor
				t1[i] = t2 + '}'
			endfor
		    return, t1
			end
	113: begin
			t1 = strarr(n)
			for i=0L, n-1 do begin
				t2 = '{'												; Handle Dictionary recursively.
				key = t.keys()
				for j=0L, n_elements(key)-1 do begin	
					t3 = '$' + key[j] + '$' + ':' + stringify( t[key[j]],embed_ptr=embed_ptr)
					if j gt 0 then t2 = t2 + ','
					t2 = t2 + t3
				endfor
				t1[i] = t2 + '}'
			endfor
		    return, t1
			end
	114: begin
			t1 = strarr(n)
			for i=0L, n-1 do begin										; Handle objects simply for now.
				if obj_valid(t[i]) then begin
					if obj_class(t[i]) ne '' then begin
						t1[i] = embed_ptr ? 'obj_new("'+obj_class(t[i])+'")' : '0L'
					endif else t1[i] = embed_ptr ? 'obj_new()' : '0L'
				endif else begin
					t1[i] = embed_ptr ? 'obj_new()' : '0L'
				endelse
		    endfor
		    return, t1
		    end
	else: return, ''
endcase
return, ''
end

;--------------------------------------------------------------------------------------------------
 
function stringify, t, embed_ptr=embed_ptr

; Expand the contents of 't' as a single string scaler.
; Use recursion to dive into components
; /embed_ptr	embed ptr_new() functions to create the pointers when used.

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
       warning,'stringify',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return,'--ERROR--'
    endif
endif

	if n_elements(embed_ptr) eq 0 then embed_ptr=0
	n = n_elements(t)
	if n lt 1 then return,''
	siz = size(t)
	dim = siz[0]													; dimensions of array (0=scaler)

	if (typename(t) eq 'HASH') or (typename(t) eq $
			'ORDEREDHASH') or (typename(t) eq 'DICTIONARY') then begin
		dim = 0														; siz[0] for HASH is number of elements
		n = 1
	endif

	case dim of
		0: begin													; scaler
			str = stringify_part(t,embed_ptr=embed_ptr)
			end
		1: begin													; vector
			if n eq 1 then begin
				str = stringify_part(t,embed_ptr=embed_ptr)
			endif else begin
				str = '[' + strjoin( stringify_part(t,embed_ptr=embed_ptr), ',') + ']'
			endelse
			end
		else: begin													; 2,3,4,5 dimension array
			str = '['
			for i=0L,siz[dim]-1 do begin
				if i gt 0 then str = str + ','
				case dim of
					2: begin
						str = str + stringify(t[*,i],embed_ptr=embed_ptr)
						end
					3: begin
						str = str + stringify(t[*,*,i],embed_ptr=embed_ptr)
						end
					4: begin
						str = str + stringify(t[*,*,*,i],embed_ptr=embed_ptr)
						end
					5: begin
						str = str + stringify(t[*,*,*,*,i],embed_ptr=embed_ptr)
						end
				endcase
			endfor
			str = str + ']'
			end
	endcase
	if n_elements(str) eq 1 then str=str[0]
	return, str
end
