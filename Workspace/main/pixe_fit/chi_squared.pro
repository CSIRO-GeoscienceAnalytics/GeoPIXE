function chi_squared, fit, y, n, f_chi=fc

	fc = (fit-y)*(fit-y)/(fit > 1.0)

	chi = total( fc ) / (n > 1)

	return, chi
	end
