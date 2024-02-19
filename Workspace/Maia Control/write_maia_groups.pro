	pro write_maia_groups, file, data=groups, index=index, error=error
;
;	Write Maia "groups" parameters to s CSV file.
;	from 'data'.

	COMPILE_OPT STRICTARR
	error = 1
	if size(groups,/tname) ne 'STRUCT' then goto, bad_ptr
	if n_elements(file) lt 1 then file='Maia-groups.csv'
	if lenchr(file) lt 1 then goto, bad_file

	on_ioerror, bad_file
	openw,unit, file, /get_lun
	on_ioerror,err
	n_detectors = n_elements(groups.group[0].table)
	
	printf, unit, 'Maia detector "Group" definitions'
	printf, unit, n_detectors
	printf, unit, 'Index, ET-mode, Pileup, Throttle, title'
	f = "(4(I4,','),A)"
	
	for i=0L,15 do begin
		printf, unit, format=f, i, groups.group[i].et_mode, groups.group[i].pileup, groups.group[i].throttle, groups.group[i].title
	endfor
	
	start = 0
	for j=0L,(n_detectors/96)-1 do begin
		f3 = "('Quadrant,',I3)"
		printf, unit, format=f3, j

		printf, unit, 'Index, table'
		f2 = "(I3,96(',',I2))"
		
		for i=0L,15 do begin
			printf, unit, format=f2, i, groups.group[i].table[start:start+95]
		endfor
		start = start + 96
	endfor
	close_file, unit
	error = 0
	return

err:
	warning, 'write_maia_groups', 'Error writing file.'
	close_file, unit
	return
bad_ptr:
	warning, 'write_maia_groups', 'Bad detector data pointer.'
	close_file, unit
	return
bad_file:
	warning, 'write_maia_groups', 'Error opening file.'
	close_file, unit
	return
	end
