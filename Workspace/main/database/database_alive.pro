	function database_alive

;	Test whether the database has been initialized using "geopixe2.sav"

	common c_mass, mass2, valence, mass_OK

	if n_elements(mass_OK) lt 1 then mass_OK = 0

	return, mass_OK
	end
