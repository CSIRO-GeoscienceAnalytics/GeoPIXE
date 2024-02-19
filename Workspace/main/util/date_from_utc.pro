function date_from_utc, t

; Return a date/time string from a UTC seconds time 't'
; Need to pass time 't' as a DOUBLE
; Complements 'utc_from_date()'.

	COMPILE_OPT STRICTARR

	julTime =  (t / (24.D+0*60*60) ) + ( double(julday(1,1,1970,0,0,0)) )
;	print, t / (24.D+0*60*60), julday(1,1,1970,0,0,0), format='(F15.7,3x,F15.7)'
;	print, 'JulTime = ',julTime, format='(2x,A,1x,F15.7)'

	CalDat, julTime, month, day, year, hour, min, sec
;	help, month, day, year, hour, min, sec

	months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
	smin = (min lt 10. ? '0' : '') + str_tidy(min)
	ssec = (sec lt 10. ? '0' : '') + str_tidy(sec, places=3)

;	s = timestamp(year=year, month=month, day=day, hour=hour, minute=min, second=sec)
	s = String(day,months[month-1],year,hour,smin,ssec, FORMAT='(I2,1x,A3,1x,I4,1x,I2,":",A2,":",A6)')
	return, s
end
