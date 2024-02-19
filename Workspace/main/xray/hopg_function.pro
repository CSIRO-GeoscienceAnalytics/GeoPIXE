function hopg_function, hopg, energy

	arg = (energy - hopg.Eopt)
	gauss = exp(-arg^2/(2*(hopg.a2a*hopg.a2a)))
	q1 = where(energy gt hopg.Eopt)
	if q1[0] ne -1 then gauss[q1]=exp(-arg[q1]^2/(2*(hopg.a2b*hopg.a2b)))
	gauss = hopg.a3 + hopg.a4*energy + hopg.a0*gauss

return, gauss
end
