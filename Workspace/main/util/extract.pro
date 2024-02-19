	function extract, str, starti, stopi

; Extract strings 'str' between 'start' and 'stop'

	n = n_elements(str)
	if n lt 1 then return, ''
	start = starti
	stop = stopi
	if n_elements(start) lt n then start = replicate(start[0],n)
	if n_elements(stop) lt n then stop = replicate(stop[0],n)

	q = where(stop eq -1)
	if q[0] ne -1 then begin
		stop[q] = strlen(str[q])-1 > 0
	endif

	e = ''
	for i=0L,n-1 do begin
		ns = strlen(str[i])
		if ns ge 1 then begin
			l = clip( start[i], 0,ns-1)
			m = clip( stop[i], 0,ns-1)
			nc = m-l+1
			if nc ge 1 then begin
				e = [e, strmid( str[i], l, nc)]
			endif else begin
				e = [e,'']
			endelse
		endif else begin
			e = [e,'']
		endelse
	endfor
	e = e[1:*]
	if n_elements(e) eq 1 then e=e[0]

	return, e
	end
