function get_xanes_energies, file, do_xanes=do_xanes, x=x, y=y, asis=asis

; Read a .csv (comma separated) file containing:
; Column 1	energy
;		2	X
;		3	Y

COMPILE_OPT STRICTARR
if n_elements(asis) lt 1 then asis=0
do_xanes = 0
energies = 0.0
x = 0.0
y = 0.0
;if lenchr(file) lt 1 then file='E:\data\Feb-2010\Brugger\Ni-Energies.txt'
;if lenchr(file) lt 1 then file='C:\software\Data\CSIRO\Maia-384\Apr-2010\Analysis\XANES-5873-5983\As-xanes-energies-111-steps.csv'

on_ioerror, bad_file
openr, unit, file, /get_lun

energies = fltarr(4096)
energies[*] = -1.
x = fltarr(4096)
y = fltarr(4096)
str = ''
i = 0
while( EOF(unit) ne 1) do begin
	readf, unit, str
	if extract(str,0,0) ne '#' then begin
		s = strsplit( str, ' 	,', /extract)
		ns = n_elements(s)
		energies[i] = float(s[0])
		if ns ge 2 then x[i] = float(s[1])
		if ns ge 3 then y[i] = float(s[2])
		i = i+1
	endif
endwhile
close_file, unit

if i gt 1 then begin
	q = where( energies[0:i-1] ge 0., nq)
endif else nq=0
if nq gt 0 then begin
	do_xanes = 1
	energies = energies[q]
	x = x[q]
	y = y[q]
endif else begin
	do_xanes = 0
	energies = 0.0
	x = 0.0
	y = 0.0
endelse
if (asis eq 0) and (energies[0] gt 500.) then energies = energies/1000.
return, energies

bad_file:
	warning,'get_xanes_energies','error reading file: '+file
	close_file, unit
	return, 0.0
end
