	pro write_daq_enable, file, index=index, data=data, error=error
;
;	Write DAQ channel enable parameters (ech) to a var file,
;	in detector (chip-pad) order, from 'data'.
;	If data is in csv index order, then pass the layout 'ref' cross-reference index
;	to redirect it to detector number order.

	COMPILE_OPT STRICTARR
	error = 1
	if n_elements(data) lt 32 then goto, bad_ptr
	if n_elements(file) lt 1 then file='DAQ.enable.var'
	if lenchr(file) lt 1 then goto, bad_file

	F = file
	on_ioerror, bad_file
	openw, unit, F, /get_lun
	on_ioerror,err
	n_detectors = n_elements(data)
	if n_elements(index) eq 0 then index=indgen(n_detectors)
	ech = fix(data[index])				; convert to int in case data was byte only
	
	printf, unit, '# DAQ channel disable file'
	printf, unit, '# Generated by DAQ_Control, ' + systime()
	printf, unit, '#' 
	
	printf, unit, 'photon.chan[].enable ' + strjoin(str_tidy(1-ech),' ')
	printf, unit, '# end'
	
	close_file, unit
	error = 0
	return

err:
	warning, 'write_daq_enable', 'Error writing file.'
	close_file, unit
	return
bad_ptr:
	warning, 'write_daq_enable', 'Bad detector data pointer.'
	close_file, unit
	return
bad_file:
	warning, 'write_daq_enable', 'Error opening file.'
	close_file, unit
	return
	end