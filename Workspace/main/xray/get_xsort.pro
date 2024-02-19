function get_xsort, file
;
;	Read sorted X-ray table
;
	on_ioerror, bad
	openr, lun, file, /get_lun

	n = 0
	readf, lun, n
	e = fltarr(n)
	el = strarr(n)
	line = strarr(n)
	rel = fltarr(n)
	b = bytarr(33)

	set_separators, [' ',',','	']
	s = ' '
	for i=0L, n-1 do begin
;		readf, lun, s
;		chop_string, s, sub, ns

		readf, lun, s
		b = byte(s)
		sub = strtrim([string(b[0:9]),string(b[12:13]),string(b[16:21]),string(b[23:32])],2)

		e[i] = float(sub[0])
		el[i] = sub[1]
		line[i] = sub[2]
		rel[i] = float(sub[3])
	endfor
	list = {e:e, el:el, line:line, rel:rel}
	goto, finish

bad:
	print,'Error reading "'+file+'".'
	list = {e:0.0, el:'', line:'', rel:0.0}
finish:
	close_file, lun
	return, list
end
