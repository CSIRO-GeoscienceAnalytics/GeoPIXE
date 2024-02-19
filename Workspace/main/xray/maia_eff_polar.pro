Function maia_eff_polar_PQ_limits, phi

; PQ_limits(x) function for int_2D (where x=phi, y=theta)

compile_opt strictarr
common c_maia_eff_polar_1, R, ox,dx, oy,dy
common c_maia_eff_polar_3, Xmax, Ymax

Tmax = phi
Tmax[*] = 0.0
q = where( phi lt 45.*!dtor)
if q[0] ne -1 then Tmax[q] = Xmax / (R * cos(phi[q])) 

q = where( phi ge 45.*!dtor)
if q[0] ne -1 then Tmax[q] = Ymax / (R * sin(phi[q])) 

Tmax = atan(Tmax)

return, [0.,Tmax]
end

function maia_eff_polar, phi, theta

; Function for int_2D, where x=phi, y=theta
; To use:
;	initilize eff_2D with 2D eff image, and fill in ox,dx, oy,dy
;	result = int_2d( 'maia_eff_polar', [0.,90.]*!dtor, 'maia_eff_polar_PQ_limits')

compile_opt strictarr
common c_maia_eff_polar_1, R, ox,dx, oy,dy
common c_maia_eff_polar_2, eff_2D
common c_maia_eff_polar_4, solid_switch
common c_maia_eff_polar_6, thresh_2D
common c_maia_eff_polar_7, fill_2D
if n_elements(solid_switch) lt 1 then solid_switch=0

x = R * tan(theta) * cos(phi)
y = R * tan(theta) * sin(phi)
iX = (x - ox)/dx
iY = (y - oy)/dy

nx = n_elements(iX)
ny = n_elements(iY)
if nx eq 1 and nx lt ny then begin
	iX = replicate(iX[0], ny)
endif
if ny eq 1 and ny lt nx then begin
	iY = replicate(iY[0], nx)
endif

case solid_switch of
	0: begin
		z = interpolate( eff_2D, iX,iY)
		end
	1: begin
		z = interpolate( thresh_2D, iX,iY)
		end
	2: begin
		z = interpolate( fill_2D, iX,iY)
		end
endcase

return, z
end
