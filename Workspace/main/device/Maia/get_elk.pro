function get_elk, csv, error=err

if n_elements(csv) eq 0 then goto, bad_file

on_ioerror, bad_file
openr, unit, csv, /get_lun

	factor = replicate(0.0,384)
	s = ''
	while( EOF(unit) ne 1) do begin
		readf, unit, s
		if extract(s,0,0) ne '#' then begin
			str = strsplit(s,' 	,',/extract,count=count)
			if count ge 2 then begin
				i = float2(str[0])
				if (i ge 0) and (i lt 384) then factor[i] = float2(str[1])
			endif
		endif
	endwhile

cont:
	close_file, unit
	err = 0
	return, factor

bad_file:
	close_file, unit
	err = 1
	return, 0
end
