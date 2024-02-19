function incomplete_charge, spec, loss

;	Generate incomplete charge losses in 'spec' that fall at lower energies
;	uniformly across spectrum below each peak, witb 'loss' fraction lost to
;	lower energies.

	COMPILE_OPT STRICTARR

	if n_elements(loss) lt 1 then loss = 0.02
	
	spec2 = spec
	for i=n_elements(spec)-1,1,-1 do begin
		x = indgen(i)
		n = i
		nloss = spec[i] * loss
		spec2[x] = spec2[x] + float(nloss)/float(n)
	endfor
	
	return, spec2
end