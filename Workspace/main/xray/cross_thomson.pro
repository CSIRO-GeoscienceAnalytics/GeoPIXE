function cross_thomson, theta_degrees, E

; Thomson free electron scattering cross-section
; that Rayleigh is built on.
; theta in degrees, E in keV

re = 2.82e-13						; cm
theta = theta_degrees / !radeg

cost = cos(theta)

cross = 0.5*re*re * (1. + cost*cost)

return, cross
end
