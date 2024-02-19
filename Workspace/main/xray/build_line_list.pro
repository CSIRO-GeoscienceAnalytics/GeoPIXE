pro build_line_list, thresh
;
;	Build an alphabetic list of X-ray lines
;
common c_line_list_1, element_start, line_list, n_line_list
common c_atomic_1, periodic_table, n_periodic

i = atomic_number('He')
n_line_list = 0
element_start = intarr( n_periodic)

x = get_xsort_threshold( thresh)
n = n_elements(x.e)
if n lt 2 then return

s = strarr( n)
for i=0L, n-1 do begin
	t = x.el[i]
	if lenchr(t) lt 2 then t = t + ' '
	s[i] = t + ' ' + x.line[i] + ' ' + str_tidy(x.e[i], length=6) $
		+ ' ' + str_tidy(x.rel[i], length=6)
endfor

q = sort( s)
z = atomic_number( x.el)
line_list = {Z:z[q] , el:x.el[q] , line:x.line[q] , e:x.e[q] , $
			rel:x.rel[q] , text:s[q] }
n_line_list = n_elements(q)

element_start[*] = -1
z = 0
for i=0L,n_line_list-1 do begin
	if line_list.z[i] ne z then begin
		z = line_list.z[i]
		element_start[z] = i
	endif
endfor

return
end







