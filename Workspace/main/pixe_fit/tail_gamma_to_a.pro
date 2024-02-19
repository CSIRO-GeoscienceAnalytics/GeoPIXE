function tail_gamma_to_a, detector, E, a

;	Return the parameter A[6] for the value of gamma 'a'


	gamma = tail_length(detector,E)
	gamma_zero = 0.1

	tail_gamma_to_a = sqrt( abs( a/gamma - gamma_zero ) )

	return, tail_gamma_to_a
	end
