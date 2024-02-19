	function read_maia_enable, F, index=index, error=error
	
;	Read Maia channel enable parameters (hermes ech) from a CSV file.
;	If the file is in detector index order, then pass the layout 'index' detector index
;	to redirect it to CSV number order.

	COMPILE_OPT STRICTARR
	error = 1
	if n_elements(F) lt 1 then file='Maia-enable.csv'
	if lenchr(F) lt 1 then goto, bad_file

	on_ioerror, bad_file
	openr,unit, F, /get_lun
	on_ioerror,err
	s = ''

	if extract_extension(F) eq 'csv' then goto, old_enable
	ech = intarr(32,384/32)
	max_nc = 1
	
	while( EOF(unit) ne 1) do begin
		readf, unit, s
		if extract(s,0,0) ne '#' then begin
			str = strsplit(s,'[] 	',/extract, count=count)
			if count ge 5 then begin
				if (str[0] eq 'hermes') and (str[2] eq '.ech') then begin
					i = fix(str[1])
					if (i ge 0) and (i lt 384/32) then begin
						nval = count - 3
						ech[0:nval-1,i] = fix(str[3:3+nval-1])
						max_nc = max_nc > (i+1)
					endif
				endif else if (str[0] eq 'hermes') and (str[1] eq '.ech') then begin
					nval = count - 2
					nc = fix(ceil(float(nval/32.)))
					ech[*,0:nc-1] = fix(str[2:2+nc*32-1])
					max_nc = max_nc > nc
				endif
			endif
		endif
	endwhile
	ech = reform( ech[*,0:max_nc-1], 32*max_nc )
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
	warning, 'read_maia_enable', 'Error reading file.'
	close_file, unit
	return, 0
bad_ptr:
	warning, 'read_maia_enable', 'Bad detector data pointer.'
	close_file, unit
	return, 0
bad_file:
	warning, 'read_maia_enable', 'Error opening file.'
	close_file, unit
	return, 0
	end

