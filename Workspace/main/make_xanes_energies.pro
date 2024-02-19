pro make_xanes_energies

; Make boundaries in descending order ...
; 		Did Cleverley run use descending order? No. Then set 'descending = 0'
; 	
; As XANES (run #)
; Energies for As XANES

e_boundaries = double([	12010,	11910,	11850,	11800])
e_step      = double([	5.0,	0.75,	5.0		     ])
file	= 'C:\software\Data\CSIRO\Maia-384\Apr-2010\Analysis\XANES-5873-5983\As-xanes-energies-111-steps.csv'
descending = 0

n = n_elements(e_step)
e = dblarr(2000)
ei = e_boundaries[0]
k = 0
for i=0L,n-1 do begin
	repeat begin
		e[k] = ei
		ei = ei - e_step[i]
		k = k+1
	endrep until ei le e_boundaries[i+1]
endfor
e[k] = min(e_boundaries)

e = descending ? e[0:k] : reverse(e[0:k])

on_ioerror, bad_file
openw, unit, file, /get_lun
for i=0L,k do printf, unit, e[i]

fin:
	close_file, unit
	return

bad_file:
	warning,'make_xanes_energies','error writing energies file '+file
	goto, fin
end

