	function read_maia_pulser, file, pulser=pulser, error=error
	
;	Read Maia pulser parameters from a CSV file.
;	from struct pointed to by 'p'.
;	detector indices in 'index'

	COMPILE_OPT STRICTARR
	error = 1
	if n_elements(file) lt 1 then file='Maia.pulser.csv'
	if lenchr(file) lt 1 then goto, bad_file

	on_ioerror, bad_file
	openr,unit, file, /get_lun
	on_ioerror,err

	mode = 0
	low = 0.0
	high = 0.0
	rate = 0
	time = 0.0
	count = 0
	
	s = ''
	readf, unit, s
	
	readf, unit, s
	str = strsplit( s, ', ', /extract)
	ns = n_elements(str)
	if ns ge 2 then mode = fix(str[1])

	readf, unit, s
	str = strsplit( s, ', ', /extract)
	ns = n_elements(str)
	if ns ge 2 then low = float(str[1])

	readf, unit, s
	str = strsplit( s, ', ', /extract)
	ns = n_elements(str)
	if ns ge 2 then high = float(str[1])

	readf, unit, s
	str = strsplit( s, ', ', /extract)
	ns = n_elements(str)
	if ns ge 2 then rate = fix(str[1])

	readf, unit, s
	str = strsplit( s, ', ', /extract)
	ns = n_elements(str)
	if ns ge 2 then count = fix(str[1])

	readf, unit, s
	str = strsplit( s, ', ', /extract)
	ns = n_elements(str)
	if ns ge 2 then time = float(str[1])

	readf, unit, s
	readf, unit, s
	data = replicate( {Indx:0, select:0}, 384)
	
	on_ioerror, short
	readf, unit, data
	goto, cont

short:
	q = where(data[1:*].indx gt 0, nq)
	n = nq+1
	data = data[0:n-1]
cont:
	close_file, unit
	
	pulser = { mode:		mode, $						; pulser mode
			on:				0, $						; pulser ON
			low:			low, $						; pulser low amp
			high:			high, $						; pulser high amp
			rate:			rate, $						; pulser rate
			count:			count, $					; pulser count
			time:			time}						; pulser dwell time

	error = 0
	return, data.select

err:
	warning, 'read_maia_pulser', 'Error reading file.'
	close_file, unit
	return, 0
bad_ptr:
	warning, 'read_maia_pulser', 'Bad detector data pointer.'
	close_file, unit
	return, 0
bad_file:
	warning, 'read_maia_pulser', 'Error opening file.'
	close_file, unit
	return, 0
	end

