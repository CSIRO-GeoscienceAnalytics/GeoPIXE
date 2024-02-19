	pro write_maia_parameters, file, data=p, index=index, error=error
;
;	Write Maia parameters to s CSV file.
;	from struct pointed to by 'p'.
;	detector indices in 'index'

	COMPILE_OPT STRICTARR
	error = 1
	if n_elements(p) eq 0 then goto, bad_ptr
	if ptr_valid(p) eq 0 then goto, bad_ptr
	if size(*p,/tname) ne 'STRUCT' then goto, bad_ptr
	if n_elements(file) lt 1 then goto, bad_file
	if lenchr(file) lt 1 then goto, bad_file

	F = strip_file_ext(file) + '.csv'
	on_ioerror, bad_file
	openw,unit, F, /get_lun
	on_ioerror,err
	path = extract_path(F)

	printf, unit, 'Maia parameters'
	printf, unit, (*p).n_detectors
	t = (*p).linear.file
	if extract_path(t) eq path then t = strip_path(t)
	s1 = 'Linearization:, ' + str_tidy((*p).linear.on) + ', ' + t	
	printf, unit, s1	
	t = (*p).trim.file
	if extract_path(t) eq path then t = strip_path(t)
	s1 = 'Gain-Trimming:, ' + str_tidy((*p).trim.on) + ', ' + t	
	printf, unit, s1	
	if lenchr((*p).trim.file2) gt 0 then begin
		t = (*p).trim.file2
		if extract_path(t) eq path then t = strip_path(t)
		s1 = 'Gain-Trimming2:, ' + t	
		printf, unit, s1	
	endif
	t = (*p).pileup.file
	if extract_path(t) eq path then t = strip_path(t)
	s1 = 'Pileup:, ' + str_tidy((*p).pileup.on) + ', ' + t
	printf, unit, s1	
;	s1 = 'Throttle:, ' + str_tidy((*p).throttle.on) + ', ' + (*p).throttle.file	
;	printf, unit, s1	
	t = (*p).cal.file
	if extract_path(t) eq path then t = strip_path(t)
	s1 = 'Energy-Calibration2:, ' + str_tidy((*p).cal.mode) + ', ' + t	
	printf, unit, s1	
	s1 = 'Deadtime-Calibration:, ' + str_tidy((*p).deadtime.cal.b) + ', ' + str_tidy((*p).deadtime.cal.a)	
	printf, unit, s1	

	printf, unit, 'ASIC parameters'
	printf, unit, 'ID, time, gain, eblk, elk, tdm, tds, tos, trk, trke, trim, thresh, clock, thpd, tcm, filt'
	f = "(10(I4,','),4(F8.4,','),I4,',',I4)"
	
	for i=0L,(*p).n_detectors-1 do begin
		time = (*p).channel[i].hermes.time
		gain = (*p).channel[i].hermes.gain
		eblk = (*p).channel[i].hermes.eblk
		elk = (*p).channel[i].hermes.elk
		tdm = (*p).channel[i].scepter.tdm
		tds = (*p).channel[i].scepter.tds
		tos = (*p).channel[i].scepter.tos
		trk = (*p).channel[i].scepter.trk
		trke = (*p).channel[i].scepter.trke
		trim = (*p).channel[i].scepter.trim
		thresh = (*p).channel[i].scepter.thresh
		clock = (*p).channel[i].scepter.clock
		thpd = (*p).channel[i].scepter.thpd
		tcm = (*p).channel[i].scepter.tcm
		filt = (*p).channel[i].scepter.filt
		printf, unit, format=f, index[i], time, gain, eblk, elk, tdm, tds, tos, trk, trke, trim, thresh, clock, thpd, tcm, filt
	endfor
	
	close_file, unit
	error = 0
	return

err:
	warning, 'write_maia_parameters', 'Error writing file.'
	close_file, unit
	return
bad_ptr:
	warning, 'write_maia_parameters', 'Bad detector data pointer.'
	close_file, unit
	return
bad_file:
	warning, 'write_maia_parameters', 'Error opening file.'
	close_file, unit
	return
	end

