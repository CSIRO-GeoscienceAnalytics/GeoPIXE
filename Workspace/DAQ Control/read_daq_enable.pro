	function read_daq_enable, F, index=index, error=error
	
;	Read DAQ channel enable parameters (hermes ech) from a CSV file.
;	If the file is in detector index order, then pass the layout 'index' detector index
;	to redirect it to CSV number order.

	COMPILE_OPT STRICTARR
	error = 1
	if n_elements(F) lt 1 then file='DAQ.enable.csv'
	if lenchr(F) lt 1 then goto, bad_file

	on_ioerror, bad_file
	openr,unit, F, /get_lun
	on_ioerror,err
	s = ''

	if extract_extension(F) eq 'csv' then goto, old_enable
	ech = intarr(36)
	max_nc = 1
	
	while( EOF(unit) ne 1) do begin
		readf, unit, s
		if extract(s,0,0) ne '#' then begin
			str = strsplit(s,'[] 	',/extract, count=count)
			if count ge 3 then begin
				if (str[0] eq 'photon.chan') and (str[1] eq '.enable') then begin
					nval = (count - 2) < 36
					ech[0:nval-1] = 1-fix(str[2:2+nval-1])
				endif
			endif
		endif
	endwhile
	ech = ech[index]
	goto, cont

old_enable:
	n_detectors = 0
	readf, unit, s
	readf, unit, n_detectors
	readf, unit, s

	data = replicate( {Index:0, ech:0}, n_detectors)
	readf, unit, data
	ech = data.ech
	
cont:	
	close_file, unit
	error = 0
	return, ech

err:
	warning, 'read_daq_enable', 'Error reading file.'
	close_file, unit
	return, 0
bad_ptr:
	warning, 'read_daq_enable', 'Bad detector data pointer.'
	close_file, unit
	return, 0
bad_file:
	warning, 'read_daq_enable', 'Error opening file.'
	close_file, unit
	return, 0
	end

