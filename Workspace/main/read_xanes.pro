	function read_xanes, file, error=error
	
;	Read XANES data writtem by xanes_single_da_evt.pro into a .xanes.CSV file.
;	Return from struct pointed to by 'p'.
;	detector indices in 'index'

	COMPILE_OPT STRICTARR
	error = 1
;	if lenchr(file) lt 1 then goto, bad_file
	if lenchr(file) lt 1 then file='E:\data\Feb-2010\Brugger\508\508-cuts.xanes.csv'

	F = strip_file_ext(file,/double) + '.xanes.csv'
	on_ioerror, bad_file
	openr,unit, F, /get_lun
	on_ioerror,err

	s = ''
	n_rows = 0L
	n_el = 0
	readf, unit, n_rows, n_el	
	readf, unit, s
	
	data = replicate({index:0L, mon_index:0L, ET_time:0.0, energy:0.0, flux:0.0, mon_time:0.0, signal:fltarr(n_el)}, n_rows)
	readf, unit, data
	close_file, unit
	
	error = 0
	return, {n_records:n_rows, n_el:n_el, data:data}

err:
	warning, 'read_xanes', 'Error reading file.'
	close_file, unit
	return, error
bad_file:
	warning, 'read_xanes', 'Error opening file.'
	close_file, unit
	return, error
	end

