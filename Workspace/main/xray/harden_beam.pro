function harden_beam, beam, slices, lid			;, scale=scale

; Harden continuum (+lines) spectrum as it traverse the sample (half) slices
; Return spectrum at half slice steps.
; 'scale' returns the scaling factor for the result returned up to absolute spectrum.

COMPILE_OPT STRICTARR
n_slices = n_elements(slices)
n_layers = max(lid)+1 > 1
llast = -1

	E = beam.spectrum.E							; continuum energy vector
	spec = beam.spectrum.data > 0.
;	scale = total(spec)							; 'scale' factor for normalization
;	spec = spec / scale							; normalize starting 'spec'

	new = replicate(beam, 2*n_slices+1)
	new[0].spectrum.data = spec
	
	tlast = 0.0
	k = 1										; cmux zero at surface
	for i=0L,n_slices-1 do begin
		if slices[i].thick ne tlast then begin	
			t = transmit( slices[i], E)
			th = sqrt(t)						; transmission for half-slice
			tlast = slices[i].thick
		endif
	
		spec = spec * th
		new[k].spectrum.data = spec				; half step spectrum
		k = k+1
	
		spec = spec * th
		new[k].spectrum.data = spec				; half step spectrum
		k = k+1
		llast = lid[i]
	endfor

	return, new
end
