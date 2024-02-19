pro show_det_eff, p

help,*p,/struct

omega = !pi*(*p).diameter*(*p).diameter/(4.*(*p).distance*(*p).distance)
print,'Solid angle = ', omega

print,'     E	','Det_eff			','Intrins		','Total'

e = [1,2,4,8,11,16,30,100]
eff = det_eff(p,e)
intrin = eff/0.717
tot = eff*omega/(4.*!pi)

for i=0L,n_elements(e)-1 do begin
	print,e[i],' ',eff[i],'	',intrin[i],'	',tot[i]
endfor

return
end
