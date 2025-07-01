	function experiment_angles, theta, phi, alpha, beta, cos_beam, cos_detector

;	Calculate the cosine factors

	RAD = !radeg

	cos_detector = sin(theta/rad) * sin(phi/rad) * sin(beta/rad) $
				- sin(theta/rad) * cos(phi/rad) * cos(beta/rad) * sin(alpha/rad) $
				- cos(theta/rad) * cos(beta/rad) * cos(alpha/rad)

	cos_beam = cos(beta/rad) * cos(alpha/rad)

	q = where( 	abs(cos_detector) LT 1.0E-6, count)
	if count gt 0 then begin
		warning, 'experiment_angles', ['Extreme glancing geometry for some detectors.', $
					'Detector view is along target surface.', $
					'','For detector back-angles, ensure theta-alpha greater ', $
					'than 90 degrees. For wide arrays, you may need more.']
		return, 1
	endif

	q = where( 	cos_detector LT -1.0E-6, count)
	if count gt 0 then begin
		warning, 'experiment_angles', ['Some detectors look at the "back" of the sample.', $
					'Is that what is desired?', '', $
					'For detector back-angles, ensure theta-alpha greater ', $
					'than 90 degrees. For wide arrays, you may need more.']
		return, 1
	endif

	q = where( cos_beam LT 1.0E-6, count)
	if count gt 0 then begin
		warning, 'experiment_angles', ['Beam hitting back of target?', $
					'Is that what is desired?', '', $
					'If, not, use target rotation "alpha" and','tilt "beta" less than 90 degrees.']
		return, 1
	endif

	return, 0
	end
