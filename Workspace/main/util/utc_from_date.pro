function utc_from_date, line

; Return a UTC seconds time 't' (double) from a date/time string 'line'
; Complements 'date_from_utc()'.

	COMPILE_OPT STRICTARR

	s = strsplit(line, ' ', /extract, count=ns)
	if ns lt 4 then begin
		warning,'utc_from_date','Bad time string: '+line
		return, 0.0d+0
	endif
	months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

	day = long2(s[0])
	year = long2(s[2])
	month = 1 + (where( s[1] eq months, nq))[0]

	s1 = strsplit(s[3], ':', /extract, count=ns)
	if ns lt 3 then begin
		warning,'utc_from_date','Bad time string: '+line
		sec = 0.0d+0
	endif else begin
		sec = double2(s1[2])
	endelse
	hour = float2(s1[0])
	minute = float2(s1[1])
	frac = sec - floor(sec)

	jultime = double(julday(month, day, year, hour, minute, floor(sec)))

	UTC = (jultime - double(julday(1,1,1970,0,0,0)) ) *24.D+0*60*60  + frac
	return, UTC
end
