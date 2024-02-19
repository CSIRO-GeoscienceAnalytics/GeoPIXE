pro show_yield, p

help,*p,/struct

n_els = (*p).n_els
el = element_name( (*p).z)
shells = [' ','K','L','M']

print,'Yields = '
for i=0L,n_els-1 do begin
	print,'	',el[i],' ',shells[(*p).shell[i]],'	', (*p).z[i],'	', (*p).shell[i],'	', (*p).yield[i,0]
endfor

return
end
