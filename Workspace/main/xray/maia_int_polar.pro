function maia_int_polar, Ri, N=nt, XYmax=XYmax, solid=solid

compile_opt strictarr
common c_maia_eff_polar_1, R, ox,dx, oy,dy
common c_maia_eff_polar_2, eff_2D
common c_maia_eff_polar_3, Xmax, Ymax
common c_maia_eff_polar_4, solid_switch

if n_elements(solid_switch) lt 1 then solid_switch=0
if n_elements(xymax) lt 1 then xymax=[10.0,10.0]
if n_elements(xymax) lt 2 then xymax=[xymax[0],xymax[0]]
if n_elements(nt) lt 1 then nt=1000
if n_elements(solid) lt 1 then solid=0

resolve_routine, 'maia_eff_polar', /is_function
R = Ri
Xmax = XYmax[0]
Ymax = XYmax[1]
solid_switch = solid

dphi = 90./nt
dtheta = atan(max([Xmax,Ymax])*1.414/R) / nt
sum = 0.0
theta = findgen(nt) * dtheta

for phid=0.0, 90.0, dphi do begin
	phi = phid * !dtor
	tmax = maia_eff_polar_PQ_limits(phi)
	q = where( theta le tmax[1])
	if q[0] ne -1 then begin
		fun = sin(theta[q]) * maia_eff_polar(phi,theta[q])
		sum = sum + total(fun)
	endif
endfor

return, sum * dtheta * dphi * !dtor
end
