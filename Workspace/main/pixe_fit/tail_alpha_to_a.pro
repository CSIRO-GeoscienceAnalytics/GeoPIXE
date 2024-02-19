function tail_alpha_to_a, detector, E, a

;	Return the parameter A[5] for the value of alpha 'a'

	alpha = tail_amplitude(detector,E)
	alpha_zero = -0.05

	tail_alpha_to_a = sqrt( abs( a/alpha - alpha_zero ) )

	return, tail_alpha_to_a
	end
