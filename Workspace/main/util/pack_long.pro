function pack_long, bi, swap=swap, long=long, short=short, long64=long64

; Pack bytes into words, long, long64 integers.
; Think it does this for Unix byte order, not PC.
; (need to use /swap for PC (Win, Linux) byte order)
;
; Not used anymore ...

b = bi
n = n_elements(b)
if n_elements(swap) eq 0 then swap=0
if n_elements(long) eq 0 then long=0
if n_elements(short) eq 0 then short=0
if n_elements(long64) eq 0 then long64=0
if (short eq 0) and (long64 eq 0) then long=1

if long then begin
	nl = n/4
	q0 = 4*lindgen(nl)
	q1 = q0+1
	q2 = q0+2
	q3 = q0+3

	if swap then begin
		t = b[q0]
		b[q0] = b[q3]
		b[q3] = t
		t = b[q1]
		b[q1] = b[q2]
		b[q2] = t
	endif
	r = ulong(b[q3])
	r = ishft(r,8) or ulong(b[q2])
	r = ishft(r,8) or ulong(b[q1])
	r = ishft(r,8) or ulong(b[q0])
endif else if long64 then begin
	nl = n/8
	q0 = 8*lindgen(nl)
	q1 = q0+1
	q2 = q0+2
	q3 = q0+3
	q4 = q0+4
	q5 = q0+5
	q6 = q0+6
	q7 = q0+7

	if swap then begin
		t = b[q0]
		b[q0] = b[q7]
		b[q7] = t
		t = b[q1]
		b[q1] = b[q6]
		b[q6] = t

		t = b[q2]
		b[q2] = b[q5]
		b[q5] = t
		t = b[q3]
		b[q3] = b[q4]
		b[q4] = t
	endif
	r = ulong64(b[q7])
	r = ishft(r,8) or ulong64(b[q6])
	r = ishft(r,8) or ulong64(b[q5])
	r = ishft(r,8) or ulong64(b[q4])
	r = ishft(r,8) or ulong64(b[q3])
	r = ishft(r,8) or ulong64(b[q2])
	r = ishft(r,8) or ulong64(b[q1])
	r = ishft(r,8) or ulong64(b[q0])
endif else if short then begin
	nl = n/2
	q0 = 2*lindgen(nl)
	q1 = q0+1

	if swap then begin
		t = b[q0]
		b[q0] = b[q1]
		b[q1] = t
	endif
	r = uint(b[q1])
	r = ishft(r,8) or uint(b[q0])
endif

return, r
end
