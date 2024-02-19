	pro write_maia_pulser, file, select=sel, pulser=pulser, error=error
;
;	Write Maia pukser parameters to a CSV file.
;	pulser parameters in 'pulser'.
;	detector selection in 'sel'

	COMPILE_OPT STRICTARR
	error = 1
	if n_elements(file) lt 1 then goto, bad_file
	if lenchr(file) lt 1 then goto, bad_file

	on_ioerror, bad_file
	openw,unit, file, /get_lun
	on_ioerror,err

	printf, unit, 'Maia 384 pulser parameters'
	s1 = 'Mode:, ' + str_tidy(pulser.mode)
	printf, unit, s1	
	s1 = 'Low:, ' + str_tidy(pulser.low)
	printf, unit, s1	
	s1 = 'High:, ' + str_tidy(pulser.high)
	printf, unit, s1	
	s1 = 'Rate:, ' + str_tidy(pulser.rate)
	printf, unit, s1	
	s1 = 'Count:, ' + str_tidy(pulser.count)
	printf, unit, s1	
	s1 = 'Time:, ' + str_tidy(pulser.time)
	printf, unit, s1	

	printf, unit, 'Detector selection'
	printf, unit, 'Indx, select'
	f = "(I4,',',I4)"
	
	for i=0L,n_elements(sel)-1 do begin
		printf, unit, format=f, i, sel[i]
	endfor
	
	close_file, unit
	error = 0
	return

err:
	warning, 'write_maia_pulser', 'Error writing file.'
	close_file, unit
	return
bad_ptr:
	warning, 'write_maia_pulser', 'Bad detector data pointer.'
	close_file, unit
	return
bad_file:
	warning, 'write_maia_pulser', 'Error opening file.'
	close_file, unit
	return
	end

