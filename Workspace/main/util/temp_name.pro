function temp_name

; Generate a unique temporary filename string

COMPILE_OPT STRICTARR

	common C_temp_name_seed, seed

	name = str_tidy(long(randomu(seed,1)*10000000L)) + '.tmp'
	return, name
end

