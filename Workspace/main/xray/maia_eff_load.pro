pro maia_eff_load, file, error=error

compile_opt strictarr
common c_maia_eff_polar_1, R, ox,dx, oy,dy
common c_maia_eff_polar_2, eff_2D
common c_maia_eff_polar_3, Xmax, Ymax
common c_maia_eff_polar_4, solid_switch
common c_maia_eff_polar_5, fun_count
common c_maia_eff_polar_6, thresh_2D
common c_maia_eff_polar_7, fill_2D

close, 1
error = 1
if lenchr(file) lt 1 then return
on_ioerror, bad

nx = 0L
ny = 0L
openr, 1, file
readu, 1, nx, ny
eff_2D = fltarr(nx,ny)
ox = 0.0
dx = 0.0
oy = 0.0
dy = 0.0
readu, 1, ox,dx, oy,dy
readu, 1, eff_2D
close, 1
error = 0

thresh_2D = eff_2D
thresh_2D[*] = 0.0
q = where( eff_2D gt max(eff_2D)/2.)
if q[0] ne -1 then thresh_2D[q] = 1.0

; Want dilate/erode structure to be just less than
; 0.5 mm for a 0.5 mm border around pads. The "23"
; assumes that a 0.5 mm border has been set-up elsewhere.

fill_2D = thresh_2D
structure = bytarr(nx/23,ny/23)
structure[*] = 1
fill_2D = dilate( fill_2D, structure)
fill_2D = erode( fill_2D, structure)
return

bad:
	close, 1
	return
end
