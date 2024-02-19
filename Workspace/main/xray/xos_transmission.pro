function xos_transmission, E

;	Empirical polycapillary transmission, supplied by XOS
;	versus energy (keV) as input

	trans = [0.0,10.12,6.62,8.56,9.7,10.69,11.42,12.02,12.49,12.8,12.95,12.82,12.42,11.73,10.91, $
			10.1,9.29,8.56,7.83,7.23,6.72,6.24,5.76,5.36,5,4.68,4.36,4.09,3.79,3.58,3.28,3.11,2.99, $
			2.84,2.67,2.58,2.37,2.23,2.08,1.91,1.77,1.69,1.61,1.52,1.47,1.39,1.32,1.23,1.17,1.1,1.03]
	energy = findgen( n_elements(trans))

	result = interpol( trans, energy, E)

;	Note that this is as quoted by XOS, without empirical modification, which looks like this.
;	The empirical factor is scaled in in 'geo_yield2' for 'XOS default' beam.poly.model
;
;	result = result * ((10./(E > 10.))^4)
;	result = result * exp(-0.3*( (E > 10.) - 10.))
;	result = result * exp(-0.2*( (E > 10.) - 10.))
;	result = result * exp(-0.2*( (E > 12.) - 12.))

	result = result * exp(-0.19*( (E > 10.) - 10.))

	return, result
end