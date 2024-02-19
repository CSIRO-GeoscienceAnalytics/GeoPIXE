function non_linearity3, X, A

; Detector non-linearity function #3
; as a further polynomial correction to "non_linearity()"
; of the form: Y = X + A0 + A1*(X-c) + A2*(X-c)^2 + A3*(X-c)^3 ...
; where 'c' is the centre of the spectrum array

common c_non_linearity3, centre_spec

f = X + A[0]
dfda0 = replicate(1,n_elements(X))

xc = double(X - centre_spec)
f = f + A[1]*xc
dfda1 = xc

xc = xc*(X - centre_spec)
f = f + A[2]*xc
dfda2 = xc

xc = xc*(X - centre_spec)
f = f + A[3]*xc
dfda3 = xc

if n_elements(A) gt 4 then begin
	xc = xc*(X - centre_spec)
	f = f + A[4]*xc
	dfda4 = xc
endif
if n_elements(A) gt 5 then begin
	xc = xc*(X - centre_spec)
	f = f + A[5]*xc
	dfda5 = xc
endif
if n_elements(A) gt 6 then begin
	xc = xc*(X - centre_spec)
	f = f + A[6]*xc
	dfda6 = xc
endif
if n_elements(A) gt 7 then begin
	xc = xc*(X - centre_spec)
	f = f + A[7]*xc
	dfda7 = xc
endif
if n_elements(A) gt 8 then begin
	xc = xc*(X - centre_spec)
	f = f + A[8]*xc
	dfda8 = xc
endif
if n_elements(A) gt 9 then begin
	xc = xc*(X - centre_spec)
	f = f + A[9]*xc
	dfda9 = xc
endif
if n_elements(A) gt 10 then begin
	xc = xc*(X - centre_spec)
	f = f + A[10]*xc
	dfda10 = xc
endif
if n_elements(A) gt 11 then begin
	xc = xc*(X - centre_spec)
	f = f + A[11]*xc
	dfda11 = xc
endif
if n_elements(A) gt 12 then begin
	xc = xc*(X - centre_spec)
	f = f + A[12]*xc
	dfda12 = xc
endif
if n_elements(A) gt 13 then begin
	xc = xc*(X - centre_spec)
	f = f + A[13]*xc
	dfda13 = xc
endif
if n_elements(A) gt 14 then begin
	xc = xc*(X - centre_spec)
	f = f + A[14]*xc
	dfda14 = xc
endif
if n_elements(A) gt 15 then begin
	xc = xc*(X - centre_spec)
	f = f + A[15]*xc
	dfda15 = xc
endif
if n_elements(A) gt 16 then begin
	xc = xc*(X - centre_spec)
	f = f + A[16]*xc
	dfda16 = xc
endif
if n_elements(A) gt 17 then begin
	xc = xc*(X - centre_spec)
	f = f + A[17]*xc
	dfda17 = xc
endif

case n_elements(A) of
	17+1: begin
		r = [ [f], [dfda0], [dfda1], [dfda2], [dfda3], [dfda4], [dfda5], [dfda6], [dfda7], [dfda8], [dfda9], $
				[dfda10], [dfda11], [dfda12], [dfda13], [dfda14], [dfda15], [dfda16], [dfda17] ]
		end
	15+1: begin
		r = [ [f], [dfda0], [dfda1], [dfda2], [dfda3], [dfda4], [dfda5], [dfda6], [dfda7], [dfda8], [dfda9], $
				[dfda10], [dfda11], [dfda12], [dfda13], [dfda14], [dfda15] ]
		end
	13+1: begin
		r = [ [f], [dfda0], [dfda1], [dfda2], [dfda3], [dfda4], [dfda5], [dfda6], [dfda7], [dfda8], [dfda9], $
				[dfda10], [dfda11], [dfda12], [dfda13] ]
		end
	11+1: begin
		r = [ [f], [dfda0], [dfda1], [dfda2], [dfda3], [dfda4], [dfda5], [dfda6], [dfda7], [dfda8], [dfda9], $
				[dfda10], [dfda11] ]
		end
	9+1: begin
		r = [ [f], [dfda0], [dfda1], [dfda2], [dfda3], [dfda4], [dfda5], [dfda6], [dfda7], [dfda8], [dfda9] ]
		end
	7+1: begin
		r = [ [f], [dfda0], [dfda1], [dfda2], [dfda3], [dfda4], [dfda5], [dfda6], [dfda7] ]
		end
	5+1: begin
		r = [ [f], [dfda0], [dfda1], [dfda2], [dfda3], [dfda4], [dfda5] ]
		end
	4+1: begin
		r = [ [f], [dfda0], [dfda1], [dfda2], [dfda3], [dfda4] ]
		end
	else: begin
		r = [ [f], [dfda0], [dfda1], [dfda2], [dfda3] ]
		print,'Even order not catered for.'
		end
endcase
return, r
end
