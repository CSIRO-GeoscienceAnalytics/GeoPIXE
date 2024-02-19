function non_linearity2, X, A

; Detector non-linearity function
; including energy calibration (offset = A2, gain = A3)
; of the form: Y = A2 + A3*[ X - A0*A1* exp( -A1 * X) ]

n = n_elements(x)
dex = exp( -A[1]*X)
f0 = X - A[0]*A[1]*dex
f = A[2] + A[3]*f0
dfda0 = -A[3]*A[1]*dex
dfda1 = A[3]*A[0]*(A[1]*X - 1.0) * dex
dfda2 = replicate(1.0,n)
dfda3 = f0

return, [ [f], [dfda0], [dfda1], [dfda2], [dfda3] ]
end
