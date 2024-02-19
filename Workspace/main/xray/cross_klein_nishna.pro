function cross_klein_nishna, E, theta_degrees

; Klein Nishna free electron scattering cross-section
; that Compton scattering is built on.
; theta in degrees, E in keV

me = 511.							; keV
re = 2.82e-13						; cm
theta = theta_degrees / !radeg

k0k = 1. + E * (1-cos(theta)) / me
sint = sin(theta)

cross = re*re * (1./k0k + k0k - sint*sint) / (2.*k0k*k0k)

return, cross
end
