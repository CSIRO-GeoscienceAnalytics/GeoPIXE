function get_throttle, throttle, do_throttle=do_throttle

do_throttle = 0
if n_elements(throttle) eq 0 then begin
	throttle = ''
	goto, bad_file
endif

on_ioerror, bad_file
openr, unit, throttle, /get_lun

old = 0
if extract_extension(throttle) eq 'txt' then goto, old_throttle

	factor = replicate(1US,4096)
	s = ''
	while( EOF(unit) ne 1) do begin
		readf, unit, s
		if extract(s,0,0) ne '#' then begin
			s = strlowcase( s)
			str = strsplit(s,' 	', /extract, count=count)
			if locate(']',str[1]) ne -1 then begin
				s = str[0] + str[1] + ' ' + str[2]
			endif
			str = strsplit(s,' 	', /extract, count=count)
			if count ge 2 then begin
				val = fix(str[1:*])
				nval = n_elements(val)
				str = strsplit(str[0],'.', /extract, count=count)
				if count ge 3 then begin
					if (str[0] eq 'throttle') and (str[2] eq 'factor') then begin
						str = strsplit(str[1],'[]', /preserve_null, /extract, count=count)
						if count ge 2 then begin
							if str[0] eq 'energy' then begin
								if locate('-',str[1]) ne -1 then begin
									str = strsplit(str[1],'-', /extract, count=count)
									if count ge 2 then begin
										i1 = fix(str[0]) > 0
										i2 = fix(str[1]) > 0
										if nval eq 1 then begin
											factor[i1:i2] = val
										endif else begin
											n = min([i2-i1+1,nval]) > 1
											factor[i1:i1+n-1] = val[0:n-1]
										endelse
									endif								
								endif else if str[1] eq '' then begin
									if nval eq 1 then begin
										factor[*] = val
									endif else begin
										factor[0:nval-1] = val
									endelse
								endif else begin
									i = fix(str[1])
									if (i ge 0) and (i lt 4096) then begin
										factor[i] = val
									endif
								endelse
							endif
						endif
					endif
				endif
			endif
		endif
	endwhile
	goto, cont

old_throttle:
	factor = replicate(1US,4096)
	s = ''
	while( EOF(unit) ne 1) do begin
		readf, unit, s
		if extract(s,0,0) ne '#' then begin
			str = strsplit(s,' 	,',/extract,count=count)
			if count ge 2 then begin
				i = fix(str[0])
				if (i ge 0) and (i lt 4096) then factor[i] = fix(str[1])
			endif
		endif
	endwhile

cont:
	close_file, unit

	do_throttle = 1
	return, factor

bad_file:
	close_file, unit
	return, 0
end
