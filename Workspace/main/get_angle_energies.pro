function get_angle_energies, file, angles=angles, do_xanes=do_xanes

; Read a .csv (comma separated) file containing:
; Column 1	energy (keV)
;		2	angles (deg)

COMPILE_OPT STRICTARR
if lenchr(file) lt 1 then file='C:\NMP\Petra\May-2015\bragg_to_E.csv'

return, get_xanes_energies( file, do_xanes=do_xanes, x=angles)
end
