function xsort_list, thresh, list=x, table=table

if n_elements(table) lt 1 then table=0

x = get_xsort_threshold( thresh)
if n_elements(x.e) lt 2 then return,''

n = n_elements(x.e)
if table then s = strarr(5,n) else s = strarr(n)

for i=0L, n-1 do begin
	t = x.el[i]
	if lenchr(t) lt 2 then t = t + ' '
	tl = x.line[i]
	nl = lenchr(tl)
	if nl lt 6 then for k=0L,6-nl-1 do tl = tl + ' '
	ti = x.iupac[i]
	ni = lenchr(tl)
	if ni lt 8 then for k=0L,8-ni-1 do ti = ti + ' '

	if table then begin
		s[0,i] = str_tidy(x.e[i], length=6)
		s[1,i] = x.el[i]
		s[2,i] = x.line[i]
		s[3,i] = str_tidy(x.rel[i], length=6)
		s[4,i] = x.iupac[i]
	endif else begin
		s[i] = str_tidy(x.e[i], length=6) + '  ' + t + ' ' + $
		tl + ' ' + str_tidy(x.rel[i], length=6) + '  ' + ti
	endelse
endfor

return, s
end
