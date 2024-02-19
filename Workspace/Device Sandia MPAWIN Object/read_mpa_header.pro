function read_mpa_header, unit, error=error

common c_sandia_1, evt_data, evt_trailer
common c_mpsys_4b, event_array, buffer, n_buffer,i_buffer

	error = 0
	on_ioerror, bad_io

	stat = fstat(unit)
	file = stat.name
	hs = 0L
	skip = 0L
	readf,unit,hs
	readf,unit,skip
	head_block = bytarr(hs*512)

	stmp = ''
	for i=0L,3 do begin
		readf,unit,stmp
		if i eq 0 then begin
			date = strtrim(stmp,2)
;			tok1 = strsplit(stmp,' ',/extract)
;			dtok = fix(strsplit(tok1[0],'-',/extract))
;			ttok = fix(strsplit(tok1[1],':',/extract))
;			date = julday(dtok[0],dtok[1],dtok[2],ttok[0],ttok[1],ttok[2])
		endif
	endfor

	maxadc = 8
	adcgain = intarr(maxadc)
	counts = lonarr(maxadc)
	for i=0L,6 do readf,unit,skip

	j = 0
	x = 0L
	y = 0L
	for i=0L,maxadc-1 do begin
		readf,unit,x
		readf,unit,y
		if y ne 0 then begin
			adcgain[j] = x
			j = j+1
		endif
		readf,unit,skip
		readf,unit,skip
	endfor

	readf,unit,skip
	adcnum = 0L
	readf,unit,adcnum
	close,unit
	openr,unit,file

	n_buffer = long((stat.size-hs*512)/(adcnum*2)) < 50000L		; limit repeat buffer size
	if n_buffer le 0 then begin
		warning,'read_mpa_header',['Error reading file, or','zero data records in LST file.']
		error = 1
		return, 0
	endif
	readu,unit,head_block
	stat = fstat(unit)

	head = { max_adcs:		maxadc, $			; max number of adcs (8)
			n_adcs:			adcnum, $			; number of adcs
			adc_gain:		adcgain, $			; ADC ranges
			date:			date, $				; date
			file:			stat.name, $		; File name
			bytes:			stat.cur_ptr, $		; number of bytes in header
			size:			stat.size }			; Size of file

	return, head

bad_io:
	error = 1
	return, 0
	end