function non_linearity, X, A

; Detector non-linearity function
; of the form: Y = X + A0*A1* exp( -A1 * X)

dex = exp( -A[1]*X)
f = X + A[0]*A[1]*dex
dfda0 = A[1]*dex
dfda1 = -A[0]*(A[1]*X - 1.0) * dex

return, [ [f], [dfda0], [dfda1] ]
end
