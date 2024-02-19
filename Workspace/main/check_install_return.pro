function check_install_return, file, silent=silent

COMPILE_OPT STRICTARR
if n_elements(silent) lt 1 then silent=0

	if silent eq 0 then warning,'check_install_return','must put "geopixe-request.txt" in "develop" dir first.'
	if n_elements(file) lt 1 then file='geopixe-request.txt'

	openr,1, file
	s = strarr(20)
	on_ioerror, cont
	readf,1, s
cont:
	close, 1
	q = where((lenchr(s) ge 1) and (extract(s,0,2) ne '***'), nq)
	if nq lt 1 then return, ''

	s = s[q]
	x = long(byte(s))
	x1 = x[*,0]
	for i=1L,n_elements(s)-1 do begin
		x1 = x1 + x[*,i]
	endfor
	s1 = '*** ' + string(64B + (byte( x1-73) mod 32B)) + ' ***'

	return,s1
end
