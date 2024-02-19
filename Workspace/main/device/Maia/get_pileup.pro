function get_pileup, pileup, do_pileup=do_pileup

do_pileup = 0
if n_elements(pileup) eq 0 then begin
	pileup = ''
	goto, bad_file
endif

on_ioerror, bad_file
openr, unit, pileup, /get_lun

old = 0
if extract_extension(pileup) eq 'txt' then goto, old_pileup

	range = intarr(2,4096)
	s = ''
	while( EOF(unit) ne 1) do begin
		readf, unit, s
		if extract(s,0,0) ne '#' then begin
			str = strsplit(s,'[] 	',/extract, count=count)
			if count ge 5 then begin
				if (str[0] eq 'pileup.energy') and (str[2] eq '.trange') then begin
					i = fix(str[1])
					if (i ge 0) and (i lt 4096) then begin
						range[0,i] = fix(str[3])
						range[1,i] = fix(str[4])
					endif
				endif else if (str[0] eq 'pileup.energy') and (str[1] eq '.trange') then begin
					nval = count - 2
					nc = nval/2
					range[*,0:nc-1] = fix(str[2:2+nc*2-1])
				endif
			endif
		endif
	endwhile
	goto, cont


old_pileup:
	range = intarr(2,4096)
	s = ''
	while( EOF(unit) ne 1) do begin
		readf, unit, s
		if extract(s,0,0) ne '#' then begin
			str = strsplit(s,' 	,',/extract,count=count)
			if count ge 3 then begin
				i = fix(str[0])
				if (i ge 0) and (i lt 4096) then begin
					range[0,i] = fix(str[1])
					range[1,i] = fix(str[2])
				endif
			endif
		endif
	endwhile

cont:
	close_file, unit
	
	do_pileup = 1
	return, range

bad_file:
	close_file, unit
	return, 0
end
