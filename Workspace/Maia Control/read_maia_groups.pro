	function read_maia_groups, file, error=error
	
;	Read Maia channel enable parameters (hermes ech) from a CSV file.

	COMPILE_OPT STRICTARR
	error = 1
	if n_elements(file) lt 1 then file='Maia-groups.csv'
	if lenchr(file) lt 1 then goto, bad_file

	F = strip_file_ext(file) + '.csv'
	on_ioerror, bad_file
	openr,unit, F, /get_lun
	on_ioerror,err

	s = ''
	n_detectors = 0
	readf, unit, s
	readf, unit, n_detectors
	readf, unit, s
	
	group_pars = {	title:	'', $					; Group title string
					table:	bytarr(n_detectors), $	; detectors assigned to a group (0, 1 values)
					et_mode: 0, $					; E or T mode
					pileup:	0, $					; enable pileup rejection for this group
					throttle: 0 }					; enable Throttling for this group
	groups = replicate(group_pars,16)

	s2 = strarr(16)
	readf, unit, s2
	for i=0L,15 do begin
		str = strsplit( s2[i], ',', /extract)
		if n_elements(str) lt 4 then goto, bad
		j = (fix(str[0]) > 0) < 15
		et = (fix(str[1]) > 0) < 1
		pu = (fix(str[2]) > 0) < 1
		thr = (fix(str[3]) > 0) < 1
		s = (n_elements(str) gt 4) ? strjoin(str[4:*],' ') : ''
		groups[j].et_mode = et
		groups[j].pileup = pu
		groups[j].throttle = thr
		groups[j].title = s
	endfor

	start = 0
	for k=0L,(n_detectors/96)-1 do begin
		readf, unit, s
		readf, unit, s

		data = replicate({Index:0, table:intarr(96)}, 16)
		readf, unit, data
;		j = (data.index > 0) < 15
		groups.table[start:start+95] = data.table
		start = start+96
	endfor
	close_file, unit
	
	error = 0
	return, groups

err:
	warning, 'read_maia_groups', 'Error reading file.'
	close_file, unit
	return, 0
bad_ptr:
	warning, 'read_maia_groups', 'Bad detector data pointer.'
	close_file, unit
	return, 0
bad_file:
	warning, 'read_maia_groups', 'Error opening file.'
	close_file, unit
	return, 0
bad:
	warning, 'read_maia_groups', 'Error reading file.'
	close_file, unit
	return, 0
	end

