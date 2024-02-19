function var_type, x

; Type Code Type Name	Data Type
;	0		UNDEFINED	Undefined
;	1		BYTE		Byte
;	2		INT			Integer
;	3		LONG		Longword integer
;	4		FLOAT		Floating point
;	5		DOUBLE		Double-precision floating
;	6		COMPLEX		Complex floating
;	7		STRING		String
;	8		STRUCT		Structure
;	9		DCOMPLEX	Double-precision complex
;	10		POINTER		Pointer
;			OBJREF		Object references expanded (see below ...)
;	12		UINT		Unsigned Integer
;	13		ULONG		Unsigned Longword Integer
;	14		LONG64		64-bit Integer
;	15		ULONG64		Unsigned 64-bit Integer
;
;	110		LIST		List
;	111		HASH		Hash
;	112		ORDEREDHASH	Ordered Hash
;	113		DICTIONARY	Dictionary
;	114		OBJREF		Object ref (not any of the above)	

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
       warning,'var_type',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return,0
    endif
endif

	type = size(x, /type)
	if type eq 11 then begin

		case typename(x) of
			'LIST': type = 110
			'HASH': type = 111
			'ORDEREDHASH': type = 112
			'DICTIONARY': type = 113
			'OBJREF': type = 114
			else: type = 114					; OBJREF Class name
		endcase
	endif
	return, type
end
