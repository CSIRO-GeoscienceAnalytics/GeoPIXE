function make_tube_spectrum

; Make a temporary .source file

COMPILE_OPT STRICTARR
startupp, /database

;goto, delta
goto, delta2

; Galinstan test case 100 W @ 70 keV
;
; alloys used in 'tube_spectrum2.pro':
;			0	off			
;			1	Ga = 95%, In = 5% 						(Alloy G1)
;			2	Ga = 68.5%, In = 21.5%, Sn = 10% 		(galinstan, Alloy I1)
;			3	Ga = 47%, In = 37%, Sn = 16% 			(Alloy I2)
;			4	Bi = 49%, Pb = 18%, Sn = 12%, In = 21%	(Ostalloy 136)
;			5	Bi = 32.5%, In = 51%, Sn = 16.5%		(Fields metal)

file = 'C:\Software\IDL\GeoPIXE-source2\Workspace\GeoPIXE\setup\sources\excillum-I2-70kV-100W.source'
sname = 'Excillum'
;alloy = 'Ga68.5In21.5Sn10'
;name = 'Alloy I1'
alloy = 'Ga47In37Sn16'
name = 'Alloy I2'
weight = 1

power = 100.										; power (W)
E0 = 70.											; electron energy (keV)
bin = 0.02											; energy bin (keV) for 0.1% bandwidth?
phi = 90.											; incident angle (to surface)
eps = 5.											; takeoff angle (to surface)
dia = 0.020											; source diameter (mm)
area = (dia/2)^2 * !pi								; area
;mono = [e_line(49,line_index('Ka_')),0.03,0.5]		; optional In Ka monochromator
mono = [0.0,0.03,0.5]

filter1 = make_filter( 'Al', 1000., /microns)		
filter2 = make_filter( 'La', 55., /microns)		
;filter = [filter1,filter2]
filter = [filter1]
n_filters = n_elements(filter)
goto, cont

;--------------------------------------------------------------------------------------------------------------
; Delta Rh (40 kV w/ 2 mm Al filter)

delta:
file = 'C:\Software\IDL\GeoPIXE-source2\Workspace\GeoPIXE\setup\sources\delta-Rh-40kV-4W.source'
sname = 'Delta'
alloy = 'Rh'
name = alloy
weight = 1

power = 4.0											; power (W)
E0 = 40.											; electron energy (keV)
bin = 0.02											; energy bin (keV) for 0.1% bandwidth?
phi = 90.											; incident angle (to surface)
eps = 90.											; takeoff angle (to surface)
trans_thick = 0.6 * density(45) / 10.				; transmission target (mg/cm2)
dia = 0.30											; source diameter (mm)
area = (dia/2)^2 * !pi								; area
;mono = [e_line(49,line_index('Ka_')),0.03,0.5]		; optional In Ka monochromator
mono = [0.0,0.03,0.5]

filter1 = make_filter( 'Be', 125., /microns)
filter3 = make_filter( 'Al', 2000., /microns)		
filter = [filter1,filter3]
n_filters = n_elements(filter)
goto, cont

;--------------------------------------------------------------------------------------------------------------
; Delta Rh (10 kV without filter)

delta2:
file = 'C:\Software\IDL\GeoPIXE-source2\Workspace\GeoPIXE\setup\sources\delta-Rh-10kV-4W.source'
sname = 'Delta'
alloy = 'Rh'
name = alloy
weight = 1

power = 4.0											; power (W)
E0 = 10.											; electron energy (keV)
bin = 0.02											; energy bin (keV) for 0.1% bandwidth?
phi = 90.											; incident angle (to surface)
eps = 90.											; takeoff angle (to surface)
trans_thick = 0.6 * density(45) / 10.				; transmission target (mg/cm2)
dia = 0.30											; source diameter (mm)
area = (dia/2)^2 * !pi								; area
;mono = [e_line(49,line_index('Ka_')),0.03,0.5]		; optional In Ka monochromator
mono = [0.0,0.03,0.5]

filter1 = make_filter( 'Be', 125., /microns)
filter = [filter1]
n_filters = n_elements(filter)
goto, cont

;--------------------------------------------------------------------------------------------------------------

; Build model brilliance spectrum (ph / (bin) keV / msr / s)

cont:
spec = source_tube_spectrum( E0, power, bin=bin, Energy=E, eps=eps, phi=phi, formula=alloy, $
				weight=weight, filter=filter, proportion=proportion, char=char, lines=lines, $  ; mono=mono, $
				transmission=trans_thick, error=error)
if error then return, 0.0
spec = 1.0e-3 * spec
char = 1.0e-3 * char

q = where( (E ge 1.0) and (E lt 100.), nq)
spec = spec[q]
char = char[q]
E = E[q]
proportion = proportion[q]

source = define(/source)
N1 = n_elements(spec)
N2 = n_elements(source.spectrum.data)
if N1 gt N2 then begin
	f = float(N1)/float(N2)
	spec = f * smart_congrid( spec, N2)
	proportion = smart_congrid( proportion, N2)
	char = f * smart_congrid( char, N2)
	E = smart_congrid( E, N2)
	N = N2
endif else N=N1

source.continuum = 1
source.energy = E0
source.model = 1
source.spectrum.N = N
source.spectrum.data[0:N-1] = spec[0:N-1] 
source.spectrum.E[0:N-1] = E[0:N-1] 
source.spectrum.proportion[0:N-1] = proportion[0:N-1]
source.spectrum.char[0:N-1] = char[0:N-1]
source.spectrum.cal.B = E[0]
source.spectrum.cal.A = E[1]-E[0]
source.file = file
source.title = sname+' ('+str_tidy(E0)+' kV @ '+str_tidy(power)+' W, '+name+')'
source.modata.volts = E0
source.modata.power = power
source.modata.anode.name = name
source.modata.anode.formula = alloy
source.modata.anode.weight = weight
source.modata.spot = dia
source.modata.phi = phi
source.modata.eps = eps
source.modata.bin = bin
source.modata.mono = mono
source.n_filters = n_filters
source.filters[0:n_filters-1] = filter

if size( lines, /tname) eq 'STRUCT' then begin
	nz = n_elements(lines.z) < n_elements(source.lines.z)
	nl = n_elements(lines.line[*,0]) < n_elements(source.lines.line[*,0])
	source.lines.n_lines[0:nz-1] = lines.n_lines[0:nz-1]
	source.lines.z[0:nz-1] = lines.z[0:nz-1]
	source.lines.shell[0:nz-1] = lines.shell[0:nz-1]
	source.lines.line[0:nl-1,0:nz-1] = lines.line[0:nl-1,0:nz-1]
	source.lines.e[0:nl-1,0:nz-1] = lines.e[0:nl-1,0:nz-1]
	source.lines.rel[0:nl-1,0:nz-1] = lines.rel[0:nl-1,0:nz-1]
endif

if n_elements(trans_thick) gt 0 then begin
	if trans_thick gt 0.0 then begin
		source.beam.mode = 1
		source.beam.thick = trans_thick
	endif
endif
return, source
end
